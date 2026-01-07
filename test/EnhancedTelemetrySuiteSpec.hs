{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EnhancedTelemetrySuiteSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Control.Monad (replicateM_)
import qualified Data.Text as Text
import Data.Text (pack, unpack)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Azimuth.Telemetry

-- | 测试SimpleMetric的交换律属性
prop_simpleMetricCommutative :: Double -> Double -> Property
prop_simpleMetricCommutative x y =
  let metric1 = recordSimpleMetric (createSimpleMetric "test" "unit" 0.0) x
      metric2 = recordSimpleMetric metric1 y
      metric3 = recordSimpleMetric (createSimpleMetric "test" "unit" 0.0) y
      metric4 = recordSimpleMetric metric3 x
  in simpleMetricValue metric2 === simpleMetricValue metric4

-- | 测试Span ID的格式属性
prop_spanIdFormat :: String -> Property
prop_spanIdFormat name =
  let span = unsafePerformIO $ createSpan (pack name)
      spanId = spanSpanId span
      isHexChar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
  in property $ all isHexChar (unpack spanId) && length (unpack spanId) >= 1

-- | 测试Span名称的保持性
prop_spanNamePreservation :: String -> Property
prop_spanNamePreservation name =
  let span = unsafePerformIO $ createSpan (pack name)
  in unpack (spanName span) === name

-- | 测试Logger名称和级别的保持性
prop_loggerPropertiesPreservation :: String -> Int -> Property
prop_loggerPropertiesPreservation name levelInt =
  let levels = [Debug, Info, Warn, Error]
      level = levels !! (abs levelInt `mod` length levels)
      logger = unsafePerformIO $ createLogger (pack name) level
  in property $ unpack (loggerName logger) == name && loggerLevel logger == level

-- | 测试TelemetryConfig字段的保持性
prop_configPropertiesPreservation :: String -> String -> Bool -> Bool -> Bool -> Property
prop_configPropertiesPreservation name version metrics tracing logging =
  let config = TelemetryConfig (pack name) (pack version) metrics tracing logging False
  in property $ unpack (serviceName config) == name &&
     unpack (serviceVersion config) == version &&
     enableMetrics config == metrics &&
     enableTracing config == tracing &&
     enableLogging config == logging

-- | 测试SimpleMetric的零元素属性
prop_simpleMetricZeroElement :: Double -> Property
prop_simpleMetricZeroElement x =
  let metric = recordSimpleMetric (createSimpleMetric "zero-test" "unit" 0.0) x
      metric2 = recordSimpleMetric metric 0.0
  in simpleMetricValue metric2 === x

-- | 测试Trace ID在同一操作序列中的一致性
prop_traceIdConsistency :: String -> Property
prop_traceIdConsistency name =
  let span1 = unsafePerformIO $ createSpan (pack name)
      span2 = unsafePerformIO $ createSpan (pack (name ++ "-2"))
  in spanTraceId span1 === spanTraceId span2

spec :: Spec
spec = describe "Enhanced Telemetry Suite Tests" $ do
  
  -- Metrics 测试
  describe "Metrics Properties" $ do
    it "should handle NaN values correctly" $ do
      metric <- createMetric "nan-test" "unit"
      recordMetric metric (0/0)  -- NaN
      value <- metricValue metric
      isNaN value `shouldBe` True
    
    it "should handle infinity values correctly" $ do
      metric <- createMetric "infinity-test" "unit"
      recordMetric metric (1/0)  -- Infinity
      value <- metricValue metric
      isInfinite value `shouldBe` True
    
    it "should handle NaN values correctly" $ do
      metric <- createMetric "nan-test" "unit"
      recordMetric metric (0/0)  -- NaN
      value <- metricValue metric
      isNaN value `shouldBe` True
    
    it "should handle infinity values correctly" $ do
      metric <- createMetric "infinity-test" "unit"
      recordMetric metric (1/0)  -- Infinity
      value <- metricValue metric
      isInfinite value `shouldBe` True
  
  -- SimpleMetric 测试
  describe "SimpleMetric Properties" $ do
    it "should satisfy commutative property" $ property prop_simpleMetricCommutative
    it "should preserve zero element" $ property prop_simpleMetricZeroElement
  
  -- Span 测试
  describe "Span Properties" $ do
    it "should generate span IDs with valid hex format" $ property prop_spanIdFormat
    it "should preserve span name" $ property prop_spanNamePreservation
    it "should maintain trace ID consistency" $ property prop_traceIdConsistency
    
    it "should handle empty span names" $ do
      span <- createSpan ""
      spanName span `shouldBe` ""
    
    it "should handle special characters in span names" $ do
      let specialName = pack "span-with-特殊字符-!@#$%"
      span <- createSpan specialName
      spanName span `shouldBe` specialName
  
  -- Logger 测试
  describe "Logger Properties" $ do
    it "should preserve logger properties" $ property prop_loggerPropertiesPreservation
    
    it "should handle all log levels" $ do
      let levels = [Debug, Info, Warn, Error]
      loggers <- mapM (\level -> createLogger "test-logger" level) levels
      map loggerLevel loggers `shouldBe` levels
    
    it "should handle empty logger names" $ do
      logger <- createLogger "" Info
      loggerName logger `shouldBe` ""
  
  -- TelemetryConfig 测试
  describe "TelemetryConfig Properties" $ do
    it "should preserve config properties" $ property prop_configPropertiesPreservation
    
    it "should handle default config correctly" $ do
      let config = defaultConfig
      serviceName config `shouldBe` "azimuth-service"
      serviceVersion config `shouldBe` "0.1.0"
      enableMetrics config `shouldBe` True
      enableTracing config `shouldBe` True
      enableLogging config `shouldBe` True
    
    it "should handle production config correctly" $ do
      let config = productionConfig
      serviceName config `shouldBe` "azimuth-service"
      serviceVersion config `shouldBe` "0.1.0"
      enableMetrics config `shouldBe` True
      enableTracing config `shouldBe` True
      enableLogging config `shouldBe` True
  
  -- 集成测试
  describe "Integration Tests" $ do
    it "should handle complete telemetry workflow" $ do
      initTelemetry defaultConfig
      
      -- Create and use metric
      metric <- createMetric "workflow-metric" "count"
      recordMetric metric 42.0
      value <- metricValue metric
      value `shouldBe` 42.0
      
      -- Create and use span
      span <- createSpan "workflow-span"
      spanName span `shouldBe` "workflow-span"
      finishSpan span
      
      -- Create and use logger
      logger <- createLogger "workflow-logger" Info
      loggerName logger `shouldBe` "workflow-logger"
      logMessage logger Info "workflow message"
      
      shutdownTelemetry
    
    it "should handle multiple telemetry components" $ do
      initTelemetry defaultConfig
      
      -- Create multiple metrics with different names to avoid sharing
      metrics <- sequence $ map (\i -> createMetric (pack ("multi-metric-" ++ show i)) "unit") [1..5]
      sequence_ $ zipWith recordMetric metrics [1..5]
      values <- sequence $ map metricValue metrics
      values `shouldBe` [1..5]
      
      -- Create multiple spans
      spans <- sequence $ map (\i -> createSpan (pack ("multi-span-" ++ show i))) [1..3]
      length spans `shouldBe` 3
      sequence_ $ map finishSpan spans
      
      -- Create multiple loggers
      loggers <- sequence $ map (\i -> createLogger (pack ("multi-logger-" ++ show i)) Info) [1..2]
      length loggers `shouldBe` 2
      sequence_ $ flip map loggers $ \logger -> do
        logMessage logger Info "multi-test"
      
      shutdownTelemetry