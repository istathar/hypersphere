module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Instances ()
import Hypersphere.Check
import qualified Data.Aeson as Aeson

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

roundTripJSON :: (Eq a, Aeson.FromJSON a, Aeson.ToJSON a) => a -> Bool
roundTripJSON a = Aeson.fromJSON (Aeson.toJSON a) == Aeson.Success a

qcProps :: TestTree
qcProps = testGroup "(QuickCheck)"
    [ QC.testProperty "Reason JSON" (roundTripJSON :: Reason -> Bool)
    ]

--------------------------------------------------
-- Arbitrary Instances
--------------------------------------------------

instance Arbitrary Reason where
    arbitrary = Reason <$> arbitrary <*> arbitrary
    shrink = genericShrink
