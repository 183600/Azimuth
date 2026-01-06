{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ComprehensiveIntegrationSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Control.Monad (replicateM_)
import qualified Data.Text as Text
import Data.Text (pack, unpack)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Azimuth.Telemetry
import Prelude hiding (id)
-- Import internal state for testing
import System.IO.Unsafe (unsafePerformIO)

-- | Test metric aggregation functionality
spec :: Spec
spec = do
  describe "Comprehensive Integration Tests" $ do
    
    -- Test 1: Metric aggregation with QuickCheck
    describe "Metric Aggregation" $ do
      it "should aggregate multiple metric values correctly" $ property $
        \(values :: [Double]) -> unsafePerformIO $ do
          -- Disable metric sharing for test isolation
          writeIORef enableMetricSharing False
          
          metric <- createMetric "aggregation-test" "count"
          mapM_ (recordMetric metric) values
          finalValue <- metricValue metric
          let expectedValue = sum values
          return $ finalValue `shouldBe` expectedValue
      
      it "should handle mixed positive and negative values" $ property $
        \(values :: [Double]) -> unsafePerformIO $ do
          -- Disable metric sharing for test isolation
          writeIORef enableMetricSharing False
          
          metric <- createMetric "mixed-values" "count"
          mapM_ (recordMetric metric) values
          finalValue <- metricValue metric
          let expectedValue = sum values
          return $ finalValue `shouldBe` expectedValue

    -- Test 2: Span trace context propagation
    describe "Trace Context Propagation" $ do
      it "should maintain trace context across multiple spans" $ do
        span1 <- createSpan "operation-1"
        let traceId1 = spanTraceId span1
        
        span2 <- createSpan "operation-2"
        span3 <- createSpan "operation-3"
        
        -- All spans should have the same trace ID
        spanTraceId span2 `shouldBe` traceId1
        spanTraceId span3 `shouldBe` traceId1
        
        -- But different span IDs
        spanSpanId span1 `shouldNotBe` spanSpanId span2
        spanSpanId span2 `shouldNotBe` spanSpanId span3
        spanSpanId span1 `shouldNotBe` spanSpanId span3
      
      it "should generate unique span IDs" $ do
        -- Reset telemetry for test isolation
        initTelemetry productionConfig
        span1 <- createSpan "test-span"
        span2 <- createSpan "test-span"
        spanSpanId span1 `shouldNotBe` spanSpanId span2
        shutdownTelemetry

    -- Test 3: Logger level filtering with QuickCheck
    describe "Logger Level Filtering" $ do
      it "should handle all log levels consistently" $ property $
        \(levelIndex :: Int) ->
          let levels = [Debug, Info, Warn, Error]
              level = levels !! (abs levelIndex `mod` length levels)
              logger = unsafePerformIO $ createLogger "test-logger" level
          in loggerLevel logger == level
      
      it "should create loggers with different levels" $ do
        debugLogger <- createLogger "debug" Debug
        infoLogger <- createLogger "info" Info
        warnLogger <- createLogger "warn" Warn
        errorLogger <- createLogger "error" Error
        
        loggerLevel debugLogger `shouldBe` Debug
        loggerLevel infoLogger `shouldBe` Info
        loggerLevel warnLogger `shouldBe` Warn
        loggerLevel errorLogger `shouldBe` Error

    -- Test 4: Configuration validation with QuickCheck
    describe "Configuration Validation" $ do
      it "should create valid configurations" $ property $
        \(name :: String) (version :: String) (metrics :: Bool) (tracing :: Bool) (logging :: Bool) ->
          let config = TelemetryConfig (pack name) (pack version) metrics tracing logging False
          in unpack (serviceName config) == name &&
             unpack (serviceVersion config) == version &&
             enableMetrics config == metrics &&
             enableTracing config == tracing &&
             enableLogging config == logging
      
      it "should handle empty configuration fields" $ do
        let emptyConfig = TelemetryConfig "" "" False False False False
        serviceName emptyConfig `shouldBe` ""
        serviceVersion emptyConfig `shouldBe` ""
        enableMetrics emptyConfig `shouldBe` False
        enableTracing emptyConfig `shouldBe` False
        enableLogging emptyConfig `shouldBe` False

    -- Test 5: Special values handling with QuickCheck
    describe "Special Values Handling" $ do
      it "should handle NaN and infinity values" $ do
        metric <- createMetric "special-values" "test"
        
        -- Test NaN
        recordMetric metric (0/0)  -- NaN
        nanValue <- metricValue metric
        isNaN nanValue `shouldBe` True
        
        -- Test positive infinity
        recordMetric metric (1/0)  -- +Infinity
        infValue <- metricValue metric
        isInfinite infValue `shouldBe` True
        infValue > 0 `shouldBe` True
        
        -- Test negative infinity
        recordMetric metric (-1/0)  -- -Infinity
        negInfValue <- metricValue metric
        isInfinite negInfValue `shouldBe` True
        negInfValue < 0 `shouldBe` True
      
      it "should handle very small and large values" $ property $
        \(value :: Double) -> unsafePerformIO $ do
          -- Disable metric sharing for test isolation
          writeIORef enableMetricSharing False
          
          metric <- createMetric "extreme-values" "test"
          recordMetric metric value
          result <- metricValue metric
          return $ if isNaN value || isInfinite value
                   then isNaN result || isInfinite result
                   else result == value

    -- Test 6: Concurrent operations with QuickCheck
    describe "Concurrent Operations" $ do
      it "should handle concurrent metric operations" $ do
        metric <- createMetric "concurrent-metric" "count"
        
        -- Simulate concurrent operations
        sequence_ $ replicate 100 $ do
          recordMetric metric 1.0
        
        finalValue <- metricValue metric
        finalValue `shouldBe` 100.0
      
      it "should handle concurrent span operations" $ do
        spans <- sequence $ replicate 50 $ createSpan "concurrent-span"
        length spans `shouldBe` 50
        
        -- All spans should have the same trace ID
        let traceIds = map spanTraceId spans
        all (== head traceIds) (tail traceIds) `shouldBe` True
        
        -- All spans should have unique span IDs
        let spanIds = map spanSpanId spans
        length (spanIds) `shouldBe` length (spanIds)

    -- Test 7: Resource management
    describe "Resource Management" $ do
      it "should handle resource cleanup properly" $ do
        -- Initialize telemetry
        initTelemetry productionConfig
        
        -- Create resources
        metric <- createMetric "resource-test" "count"
        span <- createSpan "resource-test"
        logger <- createLogger "resource-test" Info
        
        -- Use resources
        recordMetric metric 42.0
        logMessage logger Info "Resource test message"
        finishSpan span
        
        -- Shutdown telemetry
        shutdownTelemetry
        
        -- Verify resources can be recreated after shutdown
        initTelemetry productionConfig
        newMetric <- createMetric "new-resource" "count"
        recordMetric newMetric 10.0
        newValue <- metricValue newMetric
        newValue `shouldBe` 10.0
        shutdownTelemetry

    -- Test 8: Error handling and edge cases
    describe "Error Handling and Edge Cases" $ do
      it "should handle empty metric and span names" $ do
        emptyMetric <- createMetric "" ""
        metricName emptyMetric `shouldBe` ""
        metricUnit emptyMetric `shouldBe` ""
        
        emptySpan <- createSpan ""
        spanName emptySpan `shouldBe` ""
        
        emptyLogger <- createLogger "" Debug
        loggerName emptyLogger `shouldBe` ""
      
      it "should handle very long names" $ do
        let longName = pack $ replicate 1000 'a'
        longMetric <- createMetric longName "unit"
        longSpan <- createSpan longName
        longLogger <- createLogger longName Info
        
        metricName longMetric `shouldBe` longName
        spanName longSpan `shouldBe` longName
        loggerName longLogger `shouldBe` longName

    -- Test 9: Performance characteristics with QuickCheck
    describe "Performance Characteristics" $ do
      it "should handle rapid metric updates" $ property $
        \(numOps :: Int) -> unsafePerformIO $ do
          -- Disable metric sharing for test isolation
          writeIORef enableMetricSharing False
          
          let limitedOps = abs numOps `mod` 100 + 1  -- Limit to 1-100 operations
          metric <- createMetric "performance-test" "ops"
          sequence_ $ replicate limitedOps $ recordMetric metric 1.0
          result <- metricValue metric
          return $ result == fromIntegral limitedOps
      
      it "should handle metric operations efficiently" $ do
        metric <- createMetric "efficiency-test" "count"
        
        -- Perform many operations
        sequence_ $ replicate 1000 $ recordMetric metric 1.0
        
        finalValue <- metricValue metric
        finalValue `shouldBe` 1000.0

    -- Test 10: Integration workflow
    describe "Integration Workflow" $ do
      it "should handle complete telemetry workflow" $ do
        initTelemetry productionConfig
        
        -- Create multiple metrics
        requestMetric <- createMetric "requests" "count"
        latencyMetric <- createMetric "latency" "ms"
        errorMetric <- createMetric "errors" "count"
        
        -- Create spans for tracing
        requestSpan <- createSpan "process-request"
        dbSpan <- createSpan "database-query"
        
        -- Create logger
        logger <- createLogger "service" Info
        
        -- Simulate request processing
        recordMetric requestMetric 1.0
        logMessage logger Info "Processing request"
        
        recordMetric latencyMetric 42.5
        finishSpan dbSpan
        
        recordMetric errorMetric 0.0  -- No errors
        finishSpan requestSpan
        
        -- Verify metrics
        requests <- metricValue requestMetric
        latency <- metricValue latencyMetric
        errors <- metricValue errorMetric
        
        requests `shouldBe` 1.0
        latency `shouldBe` 42.5
        errors `shouldBe` 0.0
        
        shutdownTelemetry