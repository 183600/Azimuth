{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MetricPropertiesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (sort)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Metric Properties Tests" $ do
  
  -- 测试度量累加的交换律
  describe "Metric Addition Commutativity" $ do
    it "should satisfy commutative property: a + b = b + a" $ property $
      \a b ->
        let result1 = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "commutative-test" "count" 0.0
              recordMetric metric a
              recordMetric metric b
              metricValue metric
            result2 = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "commutative-test" "count" 0.0
              recordMetric metric b
              recordMetric metric a
              metricValue metric
        in result1 == result2
    
    it "should satisfy associative property: (a + b) + c = a + (b + c)" $ property $
      \a b c ->
        let result1 = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "associative-test" "count" 0.0
              recordMetric metric a
              recordMetric metric b
              recordMetric metric c
              metricValue metric
            result2 = unsafePerformIO $ do
              metric1 <- createMetricWithInitialValue "associative-test" "count" 0.0
              recordMetric metric1 a
              metric1Value <- metricValue metric1
              
              metric2 <- createMetricWithInitialValue "associative-test" "count" 0.0
              recordMetric metric2 b
              recordMetric metric2 c
              metric2Value <- metricValue metric2
              
              return (metric1Value + metric2Value)
        in result1 == result2
  
  -- 测试度量的恒等元素
  describe "Metric Identity Element" $ do
    it "should satisfy identity property: a + 0 = a" $ property $
      \a ->
        let result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "identity-test" "count" a
              recordMetric metric 0.0
              metricValue metric
        in result == a
    
    it "should satisfy identity property with initial zero: 0 + a = a" $ property $
      \a ->
        let result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "identity-test" "count" 0.0
              recordMetric metric a
              metricValue metric
        in result == a
  
  -- 测试度量的逆元
  describe "Metric Additive Inverse" $ do
    it "should satisfy additive inverse property: a + (-a) ≈ 0" $ property $
      \a ->
        let result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "inverse-test" "count" 0.0
              recordMetric metric a
              recordMetric metric (-a)
              metricValue metric
            -- 允许浮点数精度误差
            tolerance = 1.0e-9
        in abs result < tolerance || isNaN result || isInfinite result
    
    it "should handle multiple inverse operations" $ property $
      \a b ->
        let result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "multi-inverse-test" "count" 0.0
              recordMetric metric a
              recordMetric metric b
              recordMetric metric (-a)
              recordMetric metric (-b)
              metricValue metric
            -- 允许浮点数精度误差
            tolerance = 1.0e-9
        in abs result < tolerance || isNaN result || isInfinite result
  
  -- 测试度量的乘法缩放
  describe "Metric Multiplicative Scaling" $ do
    it "should satisfy distributive property: n * (a + b) = n*a + n*b" $ property $
      \(a :: Double) (b :: Double) (n :: Int) ->
        let multiplier = fromIntegral (abs n `mod` 10 + 1)  -- 避免过大的数
            result1 = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "distributive-test" "count" 0.0
              recordMetric metric a
              recordMetric metric b
              value <- metricValue metric
              return (multiplier * value)
            result2 = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "distributive-test" "count" 0.0
              sequence_ $ replicate (fromIntegral (abs n `mod` 10 + 1)) $ do
                recordMetric metric a
                recordMetric metric b
              metricValue metric
        in abs (result1 - result2) < 1.0e-9 || isNaN result1 || isNaN result2 || isInfinite result1 || isInfinite result2
  
  -- 测试特殊值处理
  describe "Special Value Handling" $ do
    it "should handle NaN values consistently" $ do
      let nan = 0.0/0.0 :: Double
          result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "nan-test" "count" 0.0
            recordMetric metric nan
            metricValue metric
      isNaN result `shouldBe` True
    
    it "should handle infinity values consistently" $ do
      let posInf = 1.0/0.0 :: Double
          negInf = -1.0/0.0 :: Double
          result1 = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "pos-inf-test" "count" 0.0
            recordMetric metric posInf
            metricValue metric
          result2 = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "neg-inf-test" "count" 0.0
            recordMetric metric negInf
            metricValue metric
      result1 `shouldSatisfy` isInfinite
      result2 `shouldSatisfy` isInfinite
      result1 > 0 `shouldBe` True
      result2 < 0 `shouldBe` True
    
    it "should handle zero values" $ property $
      \a ->
        let result1 = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "zero-test-1" "count" a
              recordMetric metric 0.0
              metricValue metric
            result2 = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "zero-test-2" "count" 0.0
              recordMetric metric a
              metricValue metric
        in result1 == result2
  
  -- 测试度量的持久性
  describe "Metric Persistence" $ do
    it "should maintain metric name and unit after operations" $ property $
      \name unit values ->
        let metricNameText = pack (take 50 name)  -- 限制名称长度
            metricUnitText = pack (take 20 unit)  -- 限制单位长度
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue metricNameText metricUnitText 0.0
              sequence_ $ map (recordMetric metric) (take 10 values)
              return (metricName metric, metricUnit metric)
        in fst result == metricNameText && snd result == metricUnitText
    
    it "should handle metric identity comparison" $ property $
      \name1 name2 unit1 unit2 ->
        let name1Text = pack (take 50 name1)
            name2Text = pack (take 50 name2)
            unit1Text = pack (take 20 unit1)
            unit2Text = pack (take 20 unit2)
            result = unsafePerformIO $ do
              metric1 <- createMetricWithInitialValue name1Text unit1Text 0.0
              metric2 <- createMetricWithInitialValue name2Text unit2Text 0.0
              return (metric1 == metric2, name1Text == name2Text, unit1Text == unit2Text)
        in case result of
            (isEqual, nameEq, unitEq) -> isEqual == (nameEq && unitEq)
  
  -- 测试度量的边界条件
  describe "Metric Boundary Conditions" $ do
    it "should handle very large values" $ property $
      \a ->
        let largeValue = if abs a > 1e100 then a else a * 1e100
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "large-value-test" "count" 0.0
              recordMetric metric largeValue
              metricValue metric
        in result == largeValue || isInfinite result || isNaN result
    
    it "should handle very small values" $ property $
      \a ->
        let smallValue = if abs a < 1e-100 && a /= 0 then a else a * 1e-100
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "small-value-test" "count" 0.0
              recordMetric metric smallValue
              metricValue metric
        in result == smallValue || result == 0.0 || isNaN result
    
    it "should handle alternating positive and negative values" $ property $
      \values ->
        let testValues = take 20 values :: [Double]
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "alternating-test" "count" 0.0
              sequence_ $ zipWith (\i v -> recordMetric metric (if even i then v else -v)) [0..] testValues
              metricValue metric
            expectedValue = sum $ zipWith (\i v -> if even i then v else -v) [0..] testValues
        in result == expectedValue || isNaN result || isInfinite result