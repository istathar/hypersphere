module Main where

import Chrono.TimeStamp
import qualified Data.Aeson as Aeson
import qualified Data.Vector.Unboxed as UV
import Hypersphere.Check
import Hypersphere.Density
import Hypersphere.Metric as Metric
import Hypersphere.Sample
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties,unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

roundTripJSON :: (Eq a, Aeson.FromJSON a, Aeson.ToJSON a) => a -> Bool
roundTripJSON a = Aeson.fromJSON (Aeson.toJSON a) == Aeson.Success a

nearlyEqual :: Double -> Double -> Bool
nearlyEqual a b
    -- If they are within 1.01 of each other
    | (a * 1.01 >= b) && (b * 1.01 >= a) = True
    -- Or if they are both with 0.01 of 0.
    | (abs a < 0.01) && (abs b < 0.01) = True
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
    , QC.testProperty "Metric JSON" (roundTripJSON :: Metric -> Bool)
    , QC.testProperty "Density JSON" (roundTripJSON :: Density -> Bool)
    , QC.testProperty "Probability distribution sums to 1" $ nearlyEqual 1.0 . integrate . kde
    , QC.testProperty "Rounding is divisible by target" $
        \(NonZero r) t -> let n = (rounding r t)/r in nearlyEqual 0 (n - fromIntegral (round n))
    , QC.testProperty "Integration, scan = integrate" $ \x ->
        let v = kde x in nearlyEqual (UV.last . scanIntegral $ v) (integrate v)
    ]

--------------------------------------------------
-- Arbitrary Instances
--------------------------------------------------

instance Arbitrary Reason where
    arbitrary = Reason <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary Sample where
    arbitrary = Sample <$> (TimeStamp <$> arbitrary) <*> arbitrary

instance Arbitrary Metric where
    arbitrary = do
        ls <- getNonEmpty <$> arbitrary
        let Just m = Metric.fromList ls
        pure m

instance Arbitrary Density where
    arbitrary = metricToKdeIndependent <$> arbitrary
