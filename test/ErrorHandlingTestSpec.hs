{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErrorHandlingTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (pack, unpack)
import Control.Exception (try, SomeException, evaluate)
import Control.Monad (when)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Error Handling Tests" $ do
  
  -- 1. 测试特殊浮点值处理
  describe "Special Floating Point Values" $ do
    it "should handle NaN values correctly" $ do
      metric <- createMetric "nan-test" "special"
      recordMetric metric (0.0/0.0)  -- NaN
      
      value <- metricValue metric
      value `shouldSatisfy` isNaN
    
    it "should handle positive infinity correctly" $ do
      metric <- createMetric "positive-infinity-test" "special"
      recordMetric metric (1.0/0.0)  -- Positive infinity
      
      value <- metricValue metric
      value `shouldSatisfy` isInfinite
      value `shouldSatisfy` (> 0)
    
    it "should handle negative infinity correctly" $ do
      metric <- createMetric "negative-infinity-test" "special"
      recordMetric metric (-1.0/0.0)  -- Negative infinity
      
      value <- metricValue metric
      value `shouldSatisfy` isInfinite
      value `shouldSatisfy` (< 0)
    
    it "should handle mixed special values" $ do
      metric <- createMetric "mixed-special-test" "special"
      
      -- 记录正常值
      recordMetric metric 1.0
      value1 <- metricValue metric
      value1 `shouldBe` 1.0
      
      -- 记录NaN
      recordMetric metric (0.0/0.0)
      value2 <- metricValue metric
      value2 `shouldSatisfy` isNaN
      
      -- NaN应该保持
      recordMetric metric 2.0
      value3 <- metricValue metric
      value3 `shouldSatisfy` isNaN
  
  -- 2. 测试边界值处理
  describe "Boundary Value Handling" $ do
    it "should handle very large values" $ do
      metric <- createMetric "large-value-test" "count"
      let largeValue = 1.0e100
      
      recordMetric metric largeValue
      value <- metricValue metric
      value `shouldBe` largeValue
    
    it "should handle very small values" $ do
      metric <- createMetric "small-value-test" "count"
      let smallValue = 1.0e-100
      
      recordMetric metric smallValue
      value <- metricValue metric
      value `shouldBe` smallValue
    
    it "should handle zero values" $ do
      metric <- createMetric "zero-value-test" "count"
      
      recordMetric metric 0.0
      value <- metricValue metric
      value `shouldBe` 0.0
      
      recordMetric metric (-0.0)
      value2 <- metricValue metric
      value2 `shouldBe` (-0.0)
  
  -- 3. 测试字符串边界处理
  describe "String Boundary Handling" $ do
    it "should handle empty strings" $ do
      metric <- createMetric "" ""
      logger <- createLogger "" Debug
      span <- createSpan ""
      
      metricName metric `shouldBe` ""
      metricUnit metric `shouldBe` ""
      loggerName logger `shouldBe` ""
      spanName span `shouldBe` ""
    
    it "should handle very long strings" $ do
      let longString = pack $ replicate 10000 'x'
      
      metric <- createMetric longString longString
      logger <- createLogger longString Debug
      span <- createSpan longString
      
      metricName metric `shouldBe` longString
      metricUnit metric `shouldBe` longString
      loggerName logger `shouldBe` longString
      spanName span `shouldBe` longString
  
  -- 4. 测试异常安全性
  describe "Exception Safety" $ do
    it "should handle exceptions gracefully during metric operations" $ do
      metric <- createMetric "exception-test" "count"
      
      -- 正常操作
      recordMetric metric 1.0
      value <- metricValue metric
      value `shouldBe` 1.0
      
      -- 即使有异常，系统应该保持稳定
      result <- try $ do
        recordMetric metric 2.0
        recordMetric metric 3.0
        metricValue metric
      
      case result of
        Left (_ :: SomeException) -> expectationFailure "Unexpected exception"
        Right v -> v `shouldBe` 6.0
    
    it "should handle exceptions gracefully during span operations" $ do
      -- 创建span
      span <- createSpan "exception-span-test"
      
      -- 即使有异常，系统应该保持稳定
      result <- try $ do
        finishSpan span
        return True
      
      case result of
        Left (_ :: SomeException) -> expectationFailure "Unexpected exception"
        Right _ -> return ()
    
    it "should handle exceptions gracefully during logging operations" $ do
      logger <- createLogger "exception-logger-test" Info
      
      -- 即使有异常，系统应该保持稳定
      result <- try $ do
        logMessage logger Info "exception test message"
        return True
      
      case result of
        Left (_ :: SomeException) -> expectationFailure "Unexpected exception"
        Right _ -> return ()
  
  -- 5. 测试资源清理
  describe "Resource Cleanup" $ do
    it "should clean up resources after shutdown" $ do
      -- 初始化系统
      initTelemetry defaultConfig
      
      -- 创建资源
      metric <- createMetric "cleanup-test" "count"
      recordMetric metric 1.0
      
      logger <- createLogger "cleanup-test-logger" Info
      logMessage logger Info "cleanup test"
      
      span <- createSpan "cleanup-test-span"
      finishSpan span
      
      -- 关闭系统
      shutdownTelemetry
      
      -- 重新初始化
      initTelemetry defaultConfig
      
      -- 创建新资源应该正常工作
      newMetric <- createMetric "after-cleanup-test" "count"
      recordMetric newMetric 2.0
      value <- metricValue newMetric
      value `shouldBe` 2.0
  
  -- 6. 测试QuickCheck属性
  describe "QuickCheck Properties" $ do
    it "should handle arbitrary floating point values" $ property $
      \(value :: Double) ->
        unsafePerformIO $ do
          metric <- createMetric "arbitrary-float-test" "count"
          recordMetric metric value
          recordedValue <- metricValue metric
          
          -- 检查特殊值的处理
          if isNaN value
            then return (isNaN recordedValue)
            else if isInfinite value
              then return (isInfinite recordedValue && 
                          (value > 0 && recordedValue > 0 || 
                           value < 0 && recordedValue < 0))
              else return (recordedValue == value)
    
    it "should handle arbitrary string values" $ property $
      \(str :: String) ->
        let testString = take 1000 str  -- 限制长度以避免性能问题
        in unsafePerformIO $ do
          metric <- createMetric (pack testString) "test-unit"
          logger <- createLogger (pack testString) Info
          span <- createSpan (pack testString)
          
          return (unpack (metricName metric) == testString &&
                  unpack (loggerName logger) == testString &&
                  unpack (spanName span) == testString)
    
    it "should maintain NaN propagation property" $ property $
      \(values :: [Double]) ->
        let hasNaN = any isNaN values
        in if hasNaN
           then unsafePerformIO $ do
             metric <- createMetric "nan-propagation-test" "count"
             sequence_ $ map (recordMetric metric) values
             finalValue <- metricValue metric
             return (isNaN finalValue)
           else True
    
    it "should handle infinity persistence correctly" $ property $
      \(value :: Double) ->
        isInfinite value ==> unsafePerformIO $ do
          metric <- createMetric "infinity-test" "count"
          recordMetric metric value
          
          -- 尝试添加正常值
          recordMetric metric 1.0
          finalValue <- metricValue metric
          
          -- 无穷大应该保持
          return (isInfinite finalValue && 
                  signum finalValue == signum value)