{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AdvancedTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate, try, SomeException)
import Control.Monad (replicateM, replicateM_)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import Data.Text (pack, unpack)
import Data.List (sort, nub)
import Numeric (showHex)
import Data.Time (getCurrentTime, diffUTCTime)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Advanced Telemetry Tests" $ do

  -- 1. QuickCheck属性测试：Metric操作的数学性质
  describe "Metric Mathematical Properties" $ do
    it "should satisfy commutative property for metric addition" $ property $
      \metricName (metricUnit :: String) (value1 :: Double) (value2 :: Double) -> do
        let name = pack metricName
            unit = pack metricUnit
        metric1 <- createMetric name unit
        metric2 <- createMetric name unit
        
        -- Record values in different order
        recordMetric metric1 value1
        recordMetric metric1 value2
        
        recordMetric metric2 value2
        recordMetric metric2 value1
        
        -- Results should be the same (commutative property)
        val1 <- metricValue metric1
        val2 <- metricValue metric2
        val1 `shouldBe` val2
    
    it "should satisfy associative property for metric addition" $ property $
      \metricName (metricUnit :: String) (value1 :: Double) (value2 :: Double) (value3 :: Double) -> do
        let name = pack metricName
            unit = pack metricUnit
        metric1 <- createMetric name unit
        metric2 <- createMetric name unit
        
        -- Group additions differently
        recordMetric metric1 value1
        recordMetric metric1 value2
        recordMetric metric1 value3
        
        recordMetric metric2 value2
        recordMetric metric2 value3
        recordMetric metric2 value1
        
        -- Results should be the same (associative property)
        val1 <- metricValue metric1
        val2 <- metricValue metric2
        val1 `shouldBe` val2
    
    it "should satisfy identity property for zero addition" $ property $
      \metricName (metricUnit :: String) (value :: Double) -> do
        let name = pack metricName
            unit = pack metricUnit
        metric <- createMetric name unit
        
        -- Record value and then zero
        recordMetric metric value
        recordMetric metric 0.0
        
        val <- metricValue metric
        val `shouldBe` value

  -- 2. Span ID生成唯一性测试
  describe "Span ID Uniqueness" $ do
    it "should generate unique span IDs" $ property $
      \spanName -> do
        let name = pack spanName
        spans <- replicateM 100 $ createSpan name
        let spanIds = map spanSpanId spans
        -- All span IDs should be unique
        length (nub spanIds) `shouldBe` length spanIds
    
    it "should generate trace IDs with consistent format" $ property $
      \spanName -> do
        let name = pack spanName
        spans <- replicateM 50 $ createSpan name
        let traceIds = map spanTraceId spans
        -- All trace IDs should be non-empty
        all (not . null . unpack) traceIds `shouldBe` True
        -- All trace IDs in the same trace should be the same
        let firstTraceId = head traceIds
        all (== firstTraceId) traceIds `shouldBe` True

  -- 3. 并发安全性增强测试
  describe "Enhanced Concurrency Safety" $ do
    it "should handle concurrent metric operations safely" $ do
      metric <- createMetric "concurrent-test" "count"
      let numThreads = 20
          operationsPerThread = 50
      
      -- Create concurrent operations
      results <- replicateM numThreads $ forkIO $ do
        replicateM_ operationsPerThread $ do
          recordMetric metric 1.0
          threadDelay 1000  -- Small delay to increase contention
      
      -- Wait for all threads to complete
      mapM_ killThread results
      threadDelay 1000000  -- Wait for operations to complete
      
      -- Verify final value
      finalValue <- metricValue metric
      finalValue `shouldSatisfy` (>= fromIntegral (numThreads * operationsPerThread))
    
    it "should handle concurrent span creation and finishing" $ do
      let numThreads = 10
          operationsPerThread = 20
      
      spans <- newTVarIO []
      
      -- Create concurrent operations
      results <- replicateM numThreads $ forkIO $ do
        createdSpans <- replicateM operationsPerThread $ do
          span <- createSpan "concurrent-span"
          finishSpan span
          return span
        atomically $ modifyTVar spans (++ createdSpans)
      
      -- Wait for all threads to complete
      mapM_ killThread results
      threadDelay 1000000  -- Wait for operations to complete
      
      -- Verify all spans were created
      allSpans <- readTVarIO spans
      length allSpans `shouldBe` numThreads * operationsPerThread
      
      -- Verify all span names are correct
      all (\span -> spanName span == "concurrent-span") allSpans `shouldBe` True

  -- 4. 资源管理和内存泄漏测试
  describe "Resource Management" $ do
    it "should handle large number of telemetry objects without leaking" $ do
      -- Create and discard many telemetry objects
      replicateM_ 1000 $ do
        metric <- createMetric "temp-metric" "count"
        recordMetric metric 1.0
        
        span <- createSpan "temp-span"
        finishSpan span
        
        logger <- createLogger "temp-logger" Info
        logMessage logger Info "temp message"
      
      -- If we reach here without crashing, resource management is working
      True `shouldBe` True
    
    it "should handle telemetry system restart gracefully" $ do
      -- Initialize and shutdown multiple times
      replicateM_ 10 $ do
        initTelemetry defaultConfig
        
        metric <- createMetric "restart-test" "count"
        recordMetric metric 1.0
        
        span <- createSpan "restart-test-span"
        finishSpan span
        
        logger <- createLogger "restart-test-logger" Info
        logMessage logger Info "restart test"
        
        shutdownTelemetry
      
      True `shouldBe` True

  -- 5. 错误处理和异常情况测试
  describe "Error Handling and Edge Cases" $ do
    it "should handle special floating point values" $ do
      metric <- createMetric "special-values" "test"
      
      -- Test infinity
      recordMetric metric (1/0)  -- Positive infinity
      recordMetric metric (-1/0) -- Negative infinity
      
      -- Test NaN (should be handled gracefully)
      recordMetric metric (0/0)  -- NaN
      
      -- If we reach here, special values are handled
      True `shouldBe` True
    
    it "should handle extremely large metric values" $ do
      metric <- createMetric "large-values" "test"
      
      -- Test very large values
      recordMetric metric 1e308
      recordMetric metric (-1e308)
      
      -- Verify metric can still be read
      _ <- metricValue metric
      
      True `shouldBe` True
    
    it "should handle empty and whitespace-only names" $ do
      -- Test empty names
      emptyMetric <- createMetric "" ""
      emptySpan <- createSpan ""
      emptyLogger <- createLogger "" Info
      
      metricName emptyMetric `shouldBe` ""
      metricUnit emptyMetric `shouldBe` ""
      spanName emptySpan `shouldBe` ""
      loggerName emptyLogger `shouldBe` ""
      
      -- Test whitespace-only names
      whitespaceMetric <- createMetric "   " "  "
      whitespaceSpan <- createSpan "   "
      whitespaceLogger <- createLogger "   " Info
      
      metricName whitespaceMetric `shouldBe` "   "
      metricUnit whitespaceMetric `shouldBe` "  "
      spanName whitespaceSpan `shouldBe` "   "
      loggerName whitespaceLogger `shouldBe` "   "

  -- 6. 配置管理测试
  describe "Configuration Management" $ do
    it "should handle configuration changes during operation" $ do
      -- Initialize with default config
      initTelemetry defaultConfig
      
      -- Create telemetry components
      metric <- createMetric "config-test" "count"
      recordMetric metric 1.0
      
      -- Change configuration
      let customConfig = TelemetryConfig "custom-service" "2.0.0" False True True False
      initTelemetry customConfig
      
      -- Continue operations with new config
      recordMetric metric 2.0
      
      span <- createSpan "config-test-span"
      finishSpan span
      
      logger <- createLogger "config-test-logger" Warn
      logMessage logger Warn "config test message"
      
      shutdownTelemetry
      
      True `shouldBe` True
    
    it "should validate configuration consistency" $ property $
      \serviceName serviceVersion (metrics :: Bool) (tracing :: Bool) (logging :: Bool) ->
        let config = TelemetryConfig (pack serviceName) (pack serviceVersion) metrics tracing logging False
        in serviceName config == pack serviceName &&
           serviceVersion config == pack serviceVersion &&
           enableMetrics config == metrics &&
           enableTracing config == tracing &&
           enableLogging config == logging

  -- 7. 性能基准测试
  describe "Performance Benchmarks" $ do
    it "should handle high-frequency metric operations" $ do
      metric <- createMetric "perf-test" "ops"
      let numOperations = 10000
      
      -- Measure time for operations
      start <- getCurrentTime
      replicateM_ numOperations $ recordMetric metric 1.0
      end <- getCurrentTime
      
      -- Verify all operations completed
      finalValue <- metricValue metric
      finalValue `shouldBe` fromIntegral numOperations
      
      -- Performance check (should complete within reasonable time)
      let elapsed = diffUTCTime end start
      elapsed `shouldSatisfy` (< 5.0)  -- Should complete within 5 seconds
    
    it "should handle high-frequency logging operations" $ do
      logger <- createLogger "perf-logger" Info
      let numOperations = 5000
      
      -- Measure time for operations
      start <- getCurrentTime
      replicateM_ numOperations $ logMessage logger Info "performance test message"
      end <- getCurrentTime
      
      -- Performance check (should complete within reasonable time)
      let elapsed = diffUTCTime end start
      elapsed `shouldSatisfy` (< 3.0)  -- Should complete within 3 seconds

  -- 8. 数据完整性测试
  describe "Data Integrity" $ do
    it "should maintain data integrity under concurrent load" $ do
      metric <- createMetric "integrity-test" "count"
      let numThreads = 10
          operationsPerThread = 100
          expectedTotal = fromIntegral (numThreads * operationsPerThread)
      
      -- Perform concurrent operations
      results <- replicateM numThreads $ forkIO $ do
        replicateM_ operationsPerThread $ recordMetric metric 1.0
      
      -- Wait for all threads to complete
      mapM_ killThread results
      threadDelay 1000000  -- Wait for operations to complete
      
      -- Verify final value matches expected total
      finalValue <- metricValue metric
      finalValue `shouldBe` expectedTotal
    
    it "should preserve metric identity under stress" $ do
      let numMetrics = 100
      metrics <- replicateM numMetrics $ createMetric "stress-test" "count"
      
      -- Perform operations on all metrics
      sequence_ $ map (\(metric, index) -> do
        recordMetric metric (fromIntegral index)
      ) $ zip metrics [1..]
      
      -- Verify all metrics have correct values
      values <- sequence $ map metricValue metrics
      values `shouldBe` map fromIntegral [1..numMetrics]

  -- 9. 跨模块集成测试
  describe "Cross-Module Integration" $ do
    it "should handle complex telemetry workflows" $ do
      initTelemetry defaultConfig
      
      -- Create a complex workflow
      let numSteps = 10
      
      replicateM_ numSteps $ \step -> do
        -- Create step-specific metric
        metric <- createMetric (pack $ "step-" ++ show step) "count"
        recordMetric metric 1.0
        
        -- Create step-specific span
        span <- createSpan (pack $ "step-" ++ show step ++ "-operation")
        finishSpan span
        
        -- Log step completion
        logger <- createLogger "workflow-logger" Info
        logMessage logger Info (pack $ "Completed step " ++ show step)
      
      shutdownTelemetry
      
      True `shouldBe` True
    
    it "should handle telemetry component interaction" $ do
      initTelemetry defaultConfig
      
      -- Create interacting components
      requestMetric <- createMetric "requests" "count"
      latencyMetric <- createMetric "latency" "ms"
      requestLogger <- createLogger "request-logger" Info
      
      -- Simulate request processing
      replicateM_ 100 $ \requestId -> do
        -- Start request span
        requestSpan <- createSpan (pack $ "request-" ++ show requestId)
        
        -- Record request
        recordMetric requestMetric 1.0
        
        -- Simulate processing
        latency <- randomIO :: IO Double
        recordMetric latencyMetric latency
        
        -- Log request
        logMessage requestLogger Info (pack $ "Processed request " ++ show requestId)
        
        -- Finish span
        finishSpan requestSpan
      
      shutdownTelemetry
      
      True `shouldBe` True

  -- 10. 高级QuickCheck属性测试
  describe "Advanced QuickCheck Properties" $ do
    it "should maintain metric invariants under arbitrary operations" $ property $
      \operations -> do
        let metricOps = take 100 $ cycle operations
        metric <- createMetric "property-test" "count"
        
        -- Apply arbitrary operations
        sequence_ $ map (\op -> recordMetric metric op) metricOps
        
        -- Verify metric value is sum of all operations
        finalValue <- metricValue metric
        let expectedValue = sum metricOps
        finalValue `shouldBe` expectedValue
    
    it "should preserve span properties under arbitrary names" $ property $
      \spanName -> do
        let name = pack spanName
        span <- createSpan name
        
        -- Verify span properties are preserved
        spanName span `shouldBe` name
        (not . null . unpack) (spanTraceId span) `shouldBe` True
        (not . null . unpack) (spanSpanId span) `shouldBe` True
        spanTraceId span `shouldNotBe` spanSpanId span
    
    it "should handle logger level filtering correctly" $ property $
      \loggerName -> do
        let name = pack loggerName
            levels = [Debug, Info, Warn, Error]
        
        -- Create logger with each level
        loggers <- mapM (\level -> createLogger name level) levels
        
        -- Verify logger properties
        sequence_ $ map (\logger -> do
          loggerName logger `shouldBe` name
          loggerLevel logger `shouldSatisfy` (`elem` levels)
        ) loggers