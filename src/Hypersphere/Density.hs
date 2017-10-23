-- |
-- This module defines the @Density@ type, which is the result of
-- doing a kernel density estimate on a sample of data. The idea
-- is that the distribution of metrics can be recorded using the @kde@
-- function, and then sampled in the model using @sampleDensity@.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Hypersphere.Density where

import Control.DeepSeq
import Control.Monad.Bayes.Class
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector.Unboxed as UV
import GHC.Generics
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy
import qualified Statistics.Sample.KernelDensity as S

-- | Represents a Kernel Density Estimate.
--
-- TODO: Should we store the cummulutive density to prevent recalculating
-- it all the time when sampling?
data Density = Density
    { dMesh :: !(UV.Vector Double)
    , dDensity :: !(UV.Vector Double)
    } deriving (Eq, Ord, Show, Generic, NFData)

-- | Find the area under the curve of the probability density. Should
-- be pretty close to 1.
--
-- Useful for debug.
integrate :: Density -> Double
integrate d@Density{..} =
    let
        spacing = UV.foldl1 subtract $ UV.take 2 dMesh
        zoids = getTrapezoids d
    in UV.sum $ UV.map (uncurry $ trapezoidalArea spacing) zoids

-- | The area of a trapezoid.
-- `trapezoid width height1 height2`
trapezoidalArea :: Double -> Double -> Double -> Double
trapezoidalArea w h1 h2 = (h1 + h2) * 0.5 * w

-- | Get the trapezoids that make up the area under the density curve.
getTrapezoids :: Density -> UV.Vector (Double,Double)
getTrapezoids Density{..} = UV.zip dDensity $ UV.tail dDensity

-- | Cumulative area under the density curve.
scanIntegral :: Density -> UV.Vector Double
scanIntegral d@Density{..} =
    let
        spacing = UV.foldl1 subtract $ UV.take 2 dMesh
        zoids = getTrapezoids d
    in 
        UV.scanl (\area trap -> area + uncurry (trapezoidalArea spacing) trap) 0 zoids

-- | Create a density from a series of observations. There must be at least
-- one observation.
kde :: NonEmpty Double -> Density
kde = uncurry Density . S.kde 512 . UV.fromList . toList

-- | Plot the @Density@ to a file with the given name and title.
plotDensity :: FilePath -> String -> Density -> IO ()
plotDensity file title Density{..} = toFile def file $ do
    let
        maxDensity = 1.1 * UV.maximum dDensity
    layout_title .= title
    layout_y_axis . laxis_generate .= scaledAxis def (0,maxDensity)
    plot $ line "" [UV.toList $ UV.zip dMesh dDensity]

-- | Sample a value from the @Density@.
sampleDensity :: MonadSample m => Density -> m Double
sampleDensity d@Density{..} = do
    s <- random
    let
        mi = UV.findIndex (>=s) $ scanIntegral d
        o = maybe UV.last (\i -> (UV.! i)) mi dMesh
    return o

