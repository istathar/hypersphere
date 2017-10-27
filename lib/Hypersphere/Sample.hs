-- | This module provides functions for sampling models to determine their
-- probability distributions.
module Hypersphere.Sample where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Helpers
import Control.Monad.Bayes.Inference
import Control.Monad.Bayes.Sampler
import Data.Default
import Data.List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Rendering.Chart.Backend.Diagrams as C
import qualified Graphics.Rendering.Chart.Easy as C
import Hypersphere.Check
import Hypersphere.Density
import Text.Printf

-- | Runs a *quick* Sequential Monte Carlo simulation on the provided
-- distribution. Useful for quickly checking results. The return values
-- are the probability distribution of the result.
quickSample :: Ord a => S (P SamplerIO) a -> IO [(a, Double)]
quickSample = sampleIO . smcMultinomial' 0 1000

-- | Similar to @quickSample@ but assumes the result is a @Status@ and
-- formats the output so you can easily check the highest risk items.
quickCheck :: S (P SamplerIO) Status -> IO (Double, [(Text, Double)])
quickCheck m = do
    r <- quickSample m
    let
        m = sortBy (comparing $ Down . snd)
            $ Map.toList
            $ Map.fromListWith (+)
                [(formatReason reason, prob) | (reasons, prob) <- r, reason <- Set.toList $ getReasons reasons]
    return (fromMaybe 0 $ lookup okStatus r, m)

-- | Does a @quickCheck@ and prints the results.
quickCheckPrint :: S (P SamplerIO) Status -> IO ()
quickCheckPrint m = do
    (ok,problems) <- quickCheck m
    printf "Service is up with probability: %0.3f\n\n" ok
    printf "Risk items:\n"
    mapM_ (uncurry . flip $ printf "  %6.3f\t%s\n") problems

-- | Like @quickSample@, but plots the *discrete* distribution as a historgram.
quickPlot :: C.PlotValue a => FilePath -> String -> S (P SamplerIO) a -> IO ()
quickPlot file title m = do
    dat <- quickSample m
    C.toFile C.def file $ do
        C.layout_title C..= title
        C.plot (C.plotBars <$> C.bars [""] [(x,[y]) | (x,y) <- dat])

-- | Like @quickSample@, but plots the kernel density estimation of the result.
--
-- NB: This function might not produce a plot if no data was collected.
quickPlotDensity :: FilePath -> String -> S (P SamplerIO) Double -> IO ()
quickPlotDensity file title m = do
    dat <- quickSample m
    let
        k = do
            nonZeros <- NonEmpty.nonEmpty $ filter ((/= 0) . snd) dat
            let
                (_,minProb) = minimumBy (comparing snd) nonZeros
            ds <- NonEmpty.nonEmpty
                [ x
                | (val,prob) <- NonEmpty.toList nonZeros
                , x <- replicate (round (prob / minProb)) val
                ]
            return $ kde ds
    case k of
        Nothing -> putStrLn "Not enough information to produce a KDE"
        Just k -> plotDensity file title k

-- | Does a simple bucketing procedure on the output by rounding it to the
-- nearest multiple of `n`.
--
-- Compare
--
-- >>> quickPlot "example.svg" "An Example" $ normal 0 100
-- 
-- and
--
-- >>> quickPlot "example.svg" "An Example" $ rounding 10 <$> normal 0 100
rounding :: Double -> Double -> Double
rounding n i = n * fromIntegral (round (i / n) :: Integer)
