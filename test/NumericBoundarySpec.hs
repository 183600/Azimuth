{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NumericBoundarySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Data.Text (pack)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Numeric Boundary Tests" $ do
  
  -- 测试零值处理
  describe "Zero Value Handling" $ do
    it "should handle positive zero" $ do
      let result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "zero-test" "count" 0.0
            recordMetric metric 0.0
            metricValue metric
      result `shouldBe` 0.0
    
    it "should handle negative zero" $ do
      let result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "negative-zero-test" "count" (-0.0)
            recordMetric metric (-0.0)
            metricValue metric
      result `shouldBe` 0.0
    
    it "should handle zero addition" $ property $
      \value ->
        let result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "zero-addition" "count" value
              recordMetric metric 0.0
              metricValue metric
        in result == value || isNaN result || isInfinite result
    
    it "should handle zero subtraction" $ property $
      \value ->
        let result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "zero-subtraction" "count" value
              recordMetric metric (-value)
              metricValue metric
            tolerance = 1.0e-9
        in abs result < tolerance || isNaN result || isInfinite result
  
  -- 测试极值处理
  describe "Extreme Value Handling" $ do
    it "should handle maximum finite double value" $ do
      let maxFinite = 1.7976931348623157e308 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "max-finite" "count" 0.0
            recordMetric metric maxFinite
            metricValue metric
      result `shouldSatisfy` (\v -> v == maxFinite || isInfinite v)
    
    it "should handle minimum finite double value" $ do
      let minFinite = -1.7976931348623157e308 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "min-finite" "count" 0.0
            recordMetric metric minFinite
            metricValue metric
      result `shouldSatisfy` (\v -> v == minFinite || isInfinite v)
    
    it "should handle smallest positive non-zero double" $ do
      let minPositive = 5.0e-324 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "min-positive" "count" 0.0
            recordMetric metric minPositive
            metricValue metric
      result `shouldSatisfy` (\v -> v == minPositive || v == 0.0)
    
    it "should handle largest negative non-zero double" $ do
      let maxNegative = -5.0e-324 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "max-negative" "count" 0.0
            recordMetric metric maxNegative
            metricValue metric
      result `shouldSatisfy` (\v -> v == maxNegative || v == 0.0)
  
  -- 测试无穷大值处理
  describe "Infinity Value Handling" $ do
    it "should handle positive infinity" $ do
      let posInf = 1.0/0.0 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "positive-infinity" "count" 0.0
            recordMetric metric posInf
            metricValue metric
      result `shouldSatisfy` isInfinite
      result > 0 `shouldBe` True
    
    it "should handle negative infinity" $ do
      let negInf = -1.0/0.0 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "negative-infinity" "count" 0.0
            recordMetric metric negInf
            metricValue metric
      result `shouldSatisfy` isInfinite
      result < 0 `shouldBe` True
    
    it "should handle infinity addition" $ do
      let posInf = 1.0/0.0 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "infinity-addition" "count" posInf
            recordMetric metric 1.0
            metricValue metric
      result `shouldSatisfy` isInfinite
      result > 0 `shouldBe` True
    
    it "should handle infinity subtraction" $ do
      let posInf = 1.0/0.0 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "infinity-subtraction" "count" posInf
            recordMetric metric (-1.0)
            metricValue metric
      result `shouldSatisfy` isInfinite
      result > 0 `shouldBe` True
    
    it "should handle infinity with infinity" $ do
      let posInf = 1.0/0.0 :: Double
          negInf = -1.0/0.0 :: Double
          result1 = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "pos-inf-pos-inf" "count" posInf
            recordMetric metric posInf
            metricValue metric
          result2 = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "pos-inf-neg-inf" "count" posInf
            recordMetric metric negInf
            metricValue metric
      result1 `shouldSatisfy` isInfinite
      result1 > 0 `shouldBe` True
      result2 `shouldSatisfy` (\v -> isInfinite v || isNaN v)
  
  -- 测试NaN值处理
  describe "NaN Value Handling" $ do
    it "should handle NaN" $ do
      let nan = 0.0/0.0 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "nan-test" "count" 0.0
            recordMetric metric nan
            metricValue metric
      isNaN result `shouldBe` True
    
    it "should handle NaN propagation" $ do
      let nan = 0.0/0.0 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "nan-propagation" "count" 1.0
            recordMetric metric nan
            recordMetric metric 2.0
            metricValue metric
      isNaN result `shouldBe` True
    
    it "should handle NaN with finite values" $ do
      let nan = 0.0/0.0 :: Double
          result1 = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "nan-finite-1" "count" nan
            recordMetric metric 1.0
            metricValue metric
          result2 = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "finite-nan-1" "count" 1.0
            recordMetric metric nan
            metricValue metric
      isNaN result1 `shouldBe` True
      isNaN result2 `shouldBe` True
    
    it "should handle NaN with infinity" $ do
      let nan = 0.0/0.0 :: Double
          posInf = 1.0/0.0 :: Double
          result1 = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "nan-pos-inf" "count" nan
            recordMetric metric posInf
            metricValue metric
          result2 = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "pos-inf-nan" "count" posInf
            recordMetric metric nan
            metricValue metric
      isNaN result1 `shouldBe` True
      isNaN result2 `shouldBe` True
  
  -- 测试数值精度边界
  describe "Numeric Precision Boundaries" $ do
    it "should handle very small positive numbers" $ property $
      \(exponent :: Int) ->
        let exp = abs exponent `mod` 300 + 1
            smallValue = 10.0 ** (-fromIntegral exp) :: Double
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "small-positive" "count" 0.0
              recordMetric metric smallValue
              metricValue metric
        in result == smallValue || result == 0.0 || isInfinite result
    
    it "should handle very small negative numbers" $ property $
      \(exponent :: Int) ->
        let exp = abs exponent `mod` 300 + 1
            smallValue = -(10.0 ** (-fromIntegral exp)) :: Double
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "small-negative" "count" 0.0
              recordMetric metric smallValue
              metricValue metric
        in result == smallValue || result == 0.0 || isInfinite result
    
    it "should handle very large positive numbers" $ property $
      \(exponent :: Int) ->
        let exp = abs exponent `mod` 300 + 1
            largeValue = 10.0 ** (fromIntegral exp) :: Double
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "large-positive" "count" 0.0
              recordMetric metric largeValue
              metricValue metric
        in result == largeValue || isInfinite result
    
    it "should handle very large negative numbers" $ property $
      \(exponent :: Int) ->
        let exp = abs exponent `mod` 300 + 1
            largeValue = -(10.0 ** (fromIntegral exp)) :: Double
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "large-negative" "count" 0.0
              recordMetric metric largeValue
              metricValue metric
        in result == largeValue || isInfinite result
  
  -- 测试数值溢出处理
  describe "Numeric Overflow Handling" $ do
    it "should handle overflow in addition" $ do
      let largeValue = 1.0e308 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "overflow-addition" "count" largeValue
            recordMetric metric largeValue
            metricValue metric
      result `shouldSatisfy` isInfinite
    
    it "should handle underflow in addition" $ do
      let smallValue = 1.0e-323 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "underflow-addition" "count" smallValue
            recordMetric metric smallValue
            metricValue metric
      result `shouldSatisfy` (\v -> v == 0.0 || v == smallValue * 2)
    
    it "should handle overflow in subtraction" $ do
      let largeValue = 1.0e308 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "overflow-subtraction" "count" (-largeValue)
            recordMetric metric (-largeValue)
            metricValue metric
      result `shouldSatisfy` isInfinite
      result < 0 `shouldBe` True
  
  -- 测试数值精度损失
  describe "Numeric Precision Loss" $ do
    it "should handle precision loss with large numbers" $ do
      let largeValue = 1.0e16 :: Double
          smallValue = 1.0 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "precision-loss" "count" largeValue
            recordMetric metric smallValue
            metricValue metric
      result `shouldSatisfy` (\v -> v >= largeValue)
    
    it "should handle precision loss with very small numbers" $ do
      let baseValue = 1.0e-16 :: Double
          smallValue = 1.0e-32 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "precision-loss-small" "count" baseValue
            recordMetric metric smallValue
            metricValue metric
      result `shouldSatisfy` (\v -> v >= baseValue)
  
  -- 测试特殊数值组合
  describe "Special Numeric Combinations" $ do
    it "should handle alternating sign values" $ property $
      \values ->
        let testValues = take 20 values :: [Double]
            alternatingValues = zipWith (\i v -> if even i then v else -v) [0..] testValues
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "alternating-signs" "count" 0.0
              sequence_ $ map (recordMetric metric) alternatingValues
              metricValue metric
            expectedValue = sum alternatingValues
        in result == expectedValue || isNaN result || isInfinite result
    
    it "should handle geometric progression" $ property $
      \base ratio ->
        let baseValue = if base == 0 then 1.0 else abs base
            ratioValue = if ratio == 0 then 2.0 else abs ratio
            geometricValues = take 10 $ iterate (* ratioValue) baseValue
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "geometric-progression" "count" 0.0
              sequence_ $ map (recordMetric metric) geometricValues
              metricValue metric
            expectedValue = sum geometricValues
        in result == expectedValue || isNaN result || isInfinite result
    
    it "should handle arithmetic progression" $ property $
      \start step ->
        let startValue = start
            stepValue = if step == 0 then 1.0 else step
            arithmeticValues = take 10 [startValue, startValue + stepValue ..]
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "arithmetic-progression" "count" 0.0
              sequence_ $ map (recordMetric metric) arithmeticValues
              metricValue metric
            expectedValue = sum arithmeticValues
        in result == expectedValue || isNaN result || isInfinite result
  
  -- 测试数值边界与特殊值的交互
  describe "Boundary and Special Value Interactions" $ do
    it "should handle zero with infinity" $ do
      let posInf = 1.0/0.0 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "zero-infinity" "count" posInf
            recordMetric metric 0.0
            metricValue metric
      result `shouldSatisfy` isInfinite
    
    it "should handle zero with NaN" $ do
      let nan = 0.0/0.0 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "zero-nan" "count" 0.0
            recordMetric metric nan
            recordMetric metric 0.0
            metricValue metric
      isNaN result `shouldBe` True
    
    it "should handle infinity with NaN" $ do
      let posInf = 1.0/0.0 :: Double
          nan = 0.0/0.0 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "infinity-nan" "count" posInf
            recordMetric metric nan
            metricValue metric
      isNaN result `shouldBe` True