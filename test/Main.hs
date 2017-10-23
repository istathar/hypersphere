module Main where

import qualified Data.Aeson as Aeson
import Hypersphere.Check
import Hypersphere.Density
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [unitTests,qcProps]

roundTripJSON :: (Eq a, Aeson.FromJSON a, Aeson.ToJSON a) => a -> Bool
roundTripJSON a = Aeson.fromJSON (Aeson.toJSON a) == Aeson.Success a

nearlyEqual :: Double -> Double -> Bool
nearlyEqual a b
    | (a * 1.01 > b) && (b * 1.01 > a) = True
    | otherwise = False

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ testCase "Trapezoidal Area (square)" $ trapezoidalArea 1 1 1 @?= 1
    , testCase "Trapezoidal Area (wide rect)" $ trapezoidalArea 2 1 1 @?= 2
    , testCase "Trapezoidal Area (tall rect)" $ trapezoidalArea 1 2 2 @?= 2
    , testCase "Trapezoidal Area (trapezoid)" $ trapezoidalArea 2 1 0.5 @?= 1.5
    ]

qcProps :: TestTree
qcProps = testGroup "(QuickCheck)"
    [ QC.testProperty "Reason JSON" (roundTripJSON :: Reason -> Bool)
    , QC.testProperty "Probability distribution sums to 1" (nearlyEqual 1.0 . integrate . kde)
    ]

--------------------------------------------------
-- Arbitrary Instances
--------------------------------------------------

instance Arbitrary Reason where
    arbitrary = Reason <$> arbitrary <*> arbitrary
    shrink = genericShrink
