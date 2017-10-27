{-# LANGUAGE OverloadedLists #-}
module Main where

import Criterion.Main
import qualified Data.Vector.Unboxed as UV
import Hypersphere.Density
import qualified Hypersphere.Metric as Metric


main :: IO ()
main = defaultMain
    [ bgroup "kde"
        [ bench "[1]" $ whnf kde [1]
        , bench "big" $ whnf kde [1,2,3,4,1,2,3,4,7,8,21,32,23,24,25]
        ]
    , env (return $ kde [1,2,3,4,5,21,23,24,25,26]) $ \env ->
        bgroup "integrate"
            [ bench "all" $ whnf integrate env
            , bench "scanned" $ nf scanIntegral env
            , bench "last scanned" $ nf (UV.last . scanIntegral) env
            ]
    , bgroup "findNearest"
        [ bench "first" $ nf (\(Just m) -> Metric.findNearest m 0) $ Metric.fromList $ zip [1..5000] [0.1,0.2..]
        , bench "last" $ nf (\(Just m) -> Metric.findNearest m 6000) $ Metric.fromList $ zip [1..5000] [0.1,0.2..]
        ]
    ]
