-- |
-- Description: Generic hardware descriptions.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DefaultSignatures #-}
module Hypersphere.PhysicalInput
    ( Maintenance(..)
    , Rack(..)
    , Cluster(..)
    , Node(..)
    , Maintainable(..)
    , Disk(..)
    , years, months, days, minutes
    , maintainCluster
    ) where

import Control.Monad.Bayes.Class
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Type.Bool
import Data.Vector (Vector)
import GHC.Generics

-- TODO: Is this module actually generic enough to be useful, or is this
-- specific to each use case.

type Name = Text

data Maintenance = Maintenance
    { mttf :: Double
    , mttr :: Double
    } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Cluster = Cluster
    { racks :: Map Name Rack
    } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Rack = Rack
    { nodes :: Map Name Node
    , rackMaintenance :: Maintenance
    } deriving (Eq,Ord,Show,Generic,ToJSON,FromJSON)

data Node = Node
    { disks :: [Disk]
    , nodeMaintenance :: Maintenance
    , role :: [Text]
    } deriving (Eq,Ord,Show,Generic,ToJSON,FromJSON)

data Disk = Disk
    { diskMaintenance :: Maintenance
    , diskSize :: Double
    } deriving (Eq,Ord,Show,Generic,ToJSON,FromJSON)

instance Maintainable Disk
instance Maintainable Node
instance Maintainable Rack

maintainMap :: (MonadSample m, Maintainable b) => Map a b -> m (Map a b)
maintainMap m = do
    t <- mapM maintain m
    return $ Map.mapMaybe id t


maintainCluster :: MonadSample m => Cluster -> m Cluster
maintainCluster Cluster{..} = do
    racks <- mapM maintainRack racks
    return Cluster{..}

maintainRack :: MonadSample m => Rack -> m Rack
maintainRack Rack{..} = do
    nodes <- mapM maintainNode nodes
    return Rack{..}

maintainNode :: MonadSample m => Node -> m Node
maintainNode Node{..} = do
    disks <- catMaybes <$> mapM maintain disks
    return Node{..}

-- | A thing that is @Maintainable@ allows us to query for whether or not the
-- thing is currently undergoing maintenance. Usually this is defined by having
-- a Mean Time To Failure (MTTF) and a Mean Time To Repair (MTTR). The generic
-- instance checks your record time for a field of type @Maintenance@ and
-- uses that for the calculations.
class Maintainable a where

    -- | Returns @Nothing@ if the thing is currently out of order.
    -- Returns @Just@ the thing if it's not out of order.
    maintain :: MonadSample m => a -> m (Maybe a)
    default maintain :: (MonadSample m, Generic a, GMaintainable (Rep a)) => a -> m (Maybe a)
    maintain a = do
        let
            Maintenance{..} = gMaintain (from a)
            mtbf = mttf + mttr
        r <- categorical ([mttf/mtbf, mttr/mtbf] :: Vector Double)
        return $ if r == 0 then Just a else Nothing



-- | A number of years
years :: Double -> Double
years i = i * days 365

-- | A number of months (30 days)
months :: Double -> Double
months i = i * days 30

-- | A number of days
days :: Double -> Double
days i = i * hours 24

-- | A number of hours
hours :: Double -> Double
hours i = i * minutes 60

-- | A number of minutes
minutes :: Double -> Double
minutes i = i * 60

-- | A number of seconds
seconds :: Double -> Double
seconds = id

----------------------
-- Generics helpers --
----------------------

class GMaintainable f where
    gMaintain :: f p -> Maintenance

instance GMaintainable (Rec0 Maintenance) where
    gMaintain (K1 m) = m

instance GMaintainable c => GMaintainable (D1 m c) where
    gMaintain (M1 m) = gMaintain m

instance GMaintainable c => GMaintainable (C1 m c) where
    gMaintain (M1 m) = gMaintain m

instance GMaintainable (S1 m (Rec0 Maintenance)) where
    gMaintain (M1 (K1 m)) = m

instance GMaintainable b => GMaintainable (a :*: b) where
    gMaintain (_ :*: b) = gMaintain b

instance {-# OVERLAPS #-} GMaintainable ((S1 m (Rec0 Maintenance)) :*: b) where
    gMaintain (M1 (K1 r) :*: _) = r

