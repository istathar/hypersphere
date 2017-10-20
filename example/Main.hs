{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.Bayes.Class
import qualified Data.Map as Map
import qualified Data.Text as Text
import Hypersphere.Check
import Hypersphere.PhysicalInput
import Hypersphere.Sample
import Text.Printf


----------------------------------------------------------------
-- Defining Our Physical Structure
----------------------------------------------------------------

exampleDisk :: Disk
exampleDisk = Disk
    { diskSize = 1 -- TB
    , diskMaintenance = Maintenance {mttf = months 70, mttr = days 10}
    }

exampleWorkerNode :: Node
exampleWorkerNode = Node
    { nodeMaintenance = Maintenance {mttf = years 7, mttr = days 30}
    , disks = replicate 4 exampleDisk
    , role = ["Worker", "Storage"]
    }

exampleLeaderNode :: Node
exampleLeaderNode = Node
    { nodeMaintenance = Maintenance {mttf = years 7, mttr = days 10}
    , disks = replicate 4 exampleDisk { diskMaintenance = (diskMaintenance exampleDisk) {mttr = days 3}}
    , role = ["Leader"]
    }

exampleRack :: Rack
exampleRack = Rack
    { rackMaintenance = Maintenance {mttf = years 2, mttr = days 2}
    , nodes = Map.fromList $
        [ (Text.pack $ printf "worker%02d" n, exampleWorkerNode)
        | n <- [1..18 :: Int]
        ] ++
        [ ("leader01", exampleLeaderNode)
        ]
    }

cluster :: Cluster
cluster = Cluster
    { racks = Map.fromList
        [ (Text.pack $ printf "rack%02d" n, exampleRack)
        | n <- [1..8 :: Int]
        ]
    }

-- | Define a distilled version of the information found in the physical
-- description of the cluster.
data FixedInput = FixedInput
    { blockStorage :: Double
    , numberOfLeaders :: Int
    } deriving (Eq, Ord, Show)

-- | This function distills down our large unwieldy @Cluster@ into
-- some more palatable types.
clusterToFixedInput :: Cluster -> FixedInput
clusterToFixedInput c =
    let
        blockStorage = sum $ do
            (_,rack) <- Map.toList $ racks c
            (_,node) <- Map.toList $ nodes rack
            guard $ "Storage" `elem` role node
            disk <- disks node
            return $ diskSize disk

        numberOfLeaders = length $ do
            (_,rack) <- Map.toList $ racks c
            (_,node) <- Map.toList $ nodes rack
            guard $ "Leader" `elem` role node

    in FixedInput{..}

-- | Given our two inputs, we can run some checks to see if everything is OK.
-- Checks are simple booleans, @True@ if we are OK, and @False@ if we are not.
-- The checks must be given a name so that we can rank each check later on
-- as being high or low risk.
healthChecks :: FixedInput -> MetricInput -> Check ()
healthChecks FixedInput{..} MetricInput{..} = do

    check "Storage Space Low" $
        (usedStorage / blockStorage) < 0.9

    check "Average Request Latency High" $
        averageRequestLatency < 5.0

-- | The @MetricInput@ of our model corresponds to measurements we made. These
-- distributions *are* learnt, and fluctuate over time in ways we don't
-- understand or control directly.
data MetricInput = MetricInput
    { usedStorage :: Double
    , averageRequestLatency :: Double
    } deriving (Show, Eq, Ord)

main :: IO ()
main = do
    putStrLn "Plotting total Storage storage distribution to \"total_storage.svg\"..."
    quickPlot "total_storage.svg" "Total Available Storage (TB)" $ blockStorage . clusterToFixedInput <$> maintainCluster cluster

    quickCheckPrint $ do
        f <- clusterToFixedInput <$> maintainCluster cluster

        -- Fixed distributions for now.
        m <- do
            usedStorage <- normal 500 10
            averageRequestLatency <- normal 3 0.8
            return MetricInput{..}

        return . runChecks $ healthChecks f m

