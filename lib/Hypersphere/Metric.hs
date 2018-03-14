-- |
--
-- Some metrics are discrete, some are continuous.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
module Hypersphere.Metric
    ( Metric()
    , fromList
    , fromVector
    , TimeStamp
    , findNearest
    , sampleMetric
    , metricToKdeTime
    , metricToKdeIndependent
    , Sample(..)
    )
    where


import Chrono.TimeStamp
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Inference
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Sequential
import Control.Monad.Reader
import Control.Monad.ST
import Data.Aeson
import Data.Bits
import Data.Int
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector.Algorithms.Heap as VA
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector as V
import Hypersphere.Density
import System.Random.MWC (initialize)

-- | A @Metric@ is just a collection of sample values (of type @Double@) and
-- their @TimeStamp@s. There has to be at least one sample in a @Metric@.
newtype Metric = Metric { getAscendingVector :: UV.Vector Sample }
    deriving (Eq, Ord, Show)

instance ToJSON Metric where
    toJSON = Array . G.map toJSON . G.convert . getAscendingVector

instance FromJSON Metric where
    parseJSON = withArray "Metric" $ \a -> do
        v <- G.mapM parseJSON a
        let
            uv = G.convert v
        case fromVector uv of
            Nothing -> fail "A Metric must contain at least one data point"
            Just m  -> pure m

data Sample = Sample !TimeStamp !Double
    deriving (Eq, Ord, Show)

instance ToJSON Sample where
    toJSON (Sample ts val) = Array (V.fromList [toJSON (unTimeStamp ts), toJSON val])

instance FromJSON Sample where
    parseJSON = withArray "Sample" $ \a -> do
        when (V.length a /= 2) $ fail "Sample array must have two elements"
        ts  <- TimeStamp <$> parseJSON (a V.! 0)
        val <- parseJSON (a V.! 1)
        pure $ Sample ts val

newtype instance UM.MVector s Sample = MV_Sample (UM.MVector s (TimeStamp, Double))
newtype instance UV.Vector Sample = V_Sample (UV.Vector (TimeStamp, Double))

instance G.Vector UV.Vector Sample where
    basicUnsafeFreeze (MV_Sample v) = V_Sample <$> G.basicUnsafeFreeze v
    basicUnsafeThaw (V_Sample v) = MV_Sample <$> G.basicUnsafeThaw v
    basicLength (V_Sample v) = G.basicLength v
    basicUnsafeSlice i j (V_Sample v) = V_Sample $ G.basicUnsafeSlice i j v
    basicUnsafeIndexM (V_Sample v) i = uncurry Sample <$> G.basicUnsafeIndexM v i

instance M.MVector UM.MVector Sample where
    basicLength (MV_Sample v) = M.basicLength v
    basicUnsafeSlice i j (MV_Sample v) = MV_Sample $ M.basicUnsafeSlice i j v
    basicOverlaps (MV_Sample a) (MV_Sample b) = M.basicOverlaps a b
    basicUnsafeNew i = MV_Sample <$> M.basicUnsafeNew i
    basicInitialize (MV_Sample v) = M.basicInitialize v
    basicUnsafeRead (MV_Sample v) i = uncurry Sample <$> M.basicUnsafeRead v i
    basicUnsafeWrite (MV_Sample v) i (Sample t d) = M.basicUnsafeWrite v i (t,d)

instance UV.Unbox Sample

-- | Convert a list to a @Metric@
fromList :: [Sample] -> Maybe Metric
fromList = fromVector . UV.fromList

-- | Convert an Unboxed @Vector@ to a @Metric@
fromVector :: UV.Vector Sample -> Maybe Metric
fromVector v
    | UV.null v = Nothing
    | otherwise = Just . Metric . UV.modify VA.sort $ v

-- | Finds the nearest sample in a @Metric@ to a given @TimeStamp@
--
-- NB: Because a @Metric@ always contains at least one value, this
-- always finds a result.
findNearest :: Metric -> TimeStamp -> Double
findNearest (Metric ms) t =
    let
        maxIx = pred $ UV.length ms
        sr = binarySearchBounds ms t 0 maxIx
        Sample prev _ = ms UV.! pred sr -- Don't force unless safe
        Sample curr _ = ms UV.! sr
        nearest
            | sr == 0 = 0
            | (t - prev) < (curr - t) = pred sr
            | otherwise = sr
    in getValue $ ms UV.! nearest

getTimeStamp :: Sample -> TimeStamp
getTimeStamp (Sample t _) = t

getValue :: Sample -> Double
getValue (Sample _ v) = v

binarySearchBounds :: UV.Vector Sample -> TimeStamp -> Int -> Int -> Int
binarySearchBounds v ts = go
    where
        go !l !u
            | u <= l = l
            | otherwise = case compare (getTimeStamp $ v UV.! k) ts of
                LT -> go (k + 1) u
                EQ -> k
                GT -> go l k
            where
                k = (u + l) `shiftR` 1

-- | Sample a @Metric@ uniformly over time. If you have large gaps in your
-- metric data, the values at the edges of those gaps will disproportionately
-- represented in the distribution.
--
-- This works well for things like disk usage.
--
-- It is fairly important that your @Metric@ data actually covers the time
-- range you specified.
sampleMetric :: MonadSample m => Metric -> TimeStamp -> TimeStamp -> m Double
sampleMetric m l h = do
    -- TODO: Precision loss a concern here? Probably not?
    -- > maxBound :: Int64
    -- 9223372036854775807
    -- > round (fromIntegral (maxBound :: Int64) :: Double)
    -- 9223372036854775808
    s <- round <$> uniform (fromIntegral l) (fromIntegral h)
    return $ findNearest m s

samplesBetween :: Metric -> TimeStamp -> TimeStamp -> Int
samplesBetween (Metric ms) t1 t2 = UV.length . fst $ (UV.partition (\(Sample t _) -> t >= t1 && t <= t2) ms)

-- | Creates a @Density@ from a @Metric@ by uniformly sampling the metric
-- over time.
metricToKdeTime :: Metric -> TimeStamp -> TimeStamp -> Density
metricToKdeTime m l h =
    let
        sample = sampleMetric m l h
        sim = runST $ do
            gen <- initialize (UV.fromList [0])
            -- TODO: This is a pretty rough estimate at the number of times we
            -- need to sample. Too many causes the bandwidth selector to go
            -- crazy.
            runReaderT (runSamplerST $ explicitPopulation $ spawn (samplesBetween m l h) >> sample) gen
        -- Safe because we always have a metric
        (r:rs) = map fst sim
    in kde (r :| rs)

-- | Creates a @Density@ from a @Metric@ by assuming all samples are
-- independent of time.
metricToKdeIndependent :: Metric -> Density
metricToKdeIndependent (Metric m) =
    let
        (r:rs) = UV.toList (UV.map getValue m)
    in kde (r :| rs)
