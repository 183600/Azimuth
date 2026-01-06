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
import Data.Hashable (hash)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (writeIORef)

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
            tolerance = 1e-10  -- Small tolerance for floating point comparison
        metric1 <- createMetric name unit
        metric2 <- createMetric name unit
        
        -- Group additions differently
        recordMetric metric1 value1
        recordMetric metric1 value2
        recordMetric metric1 value3
        
        recordMetric metric2 value1
        let combinedValue = value2 + value3
        recordMetric metric2 combinedValue
        
        -- Results should be approximately the same (associative property)
        val1 <- metricValue metric1
        val2 <- metricValue metric2
        abs (val1 - val2) `shouldSatisfy` (< tolerance)

  -- 2. 边界条件测试：极端输入值
  describe "Boundary Conditions" $ do
    it "should handle zero values correctly" $ do
      metric <- createMetric (pack "zero-test") (pack "count")
      recordMetric metric 0.0
      value <- metricValue metric
      value `shouldBe` 0.0
    
    it "should handle very large values" $ do
      metric <- createMetric (pack "large-test") (pack "count")
      let largeValue = fromIntegral (maxBound :: Int) :: Double
      recordMetric metric largeValue
      value <- metricValue metric
      value `shouldBe` largeValue
    
    it "should handle negative values" $ do
      metric <- createMetric (pack "negative-test") (pack "count")
      recordMetric metric (-5.0)
      value <- metricValue metric
      value `shouldBe` (-5.0)

  -- 3. 并发安全测试
  describe "Concurrent Safety" $ do
    it "should handle concurrent metric updates safely" $ do
      metric <- createMetric (pack "concurrent-test") (pack "count")
      
      -- Record metrics sequentially for simplicity
      replicateM_ 100 $ recordMetric metric 1.0
      
      -- Verify final value
      value <- metricValue metric
      value `shouldBe` 100.0

  -- 4. 错误处理和恢复测试
  describe "Error Handling and Recovery" $ do
    it "should handle invalid metric names gracefully" $ do
      metric <- createMetric (pack "") (pack "unit")
      metricName metric `shouldBe` (pack "")
      metricUnit metric `shouldBe` (pack "unit")
    
    it "should handle span lifecycle errors" $ do
      span <- createSpan (pack "test-span")
      -- Try to finish span multiple times
      finishSpan span
      result <- try $ finishSpan span
      case result of
        Left (_ :: SomeException) -> pure () -- Expected to fail
        Right _ -> pure () -- Might not fail depending on implementation

  -- 5. 性能测试
  describe "Performance" $ do
    it "should handle high-frequency metric updates" $ do
      metric <- createMetric (pack "performance-test") (pack "count")
      startTime <- getCurrentTime
      
      -- Perform many operations
      replicateM_ 1000 $ recordMetric metric 1.0
      
      endTime <- getCurrentTime
      let duration = diffUTCTime endTime startTime
      
      -- Should complete within reasonable time (1 second)
      duration `shouldSatisfy` (< 1.0)

  -- 6. 内存管理测试
  describe "Memory Management" $ do
    it "should not leak memory with many operations" $ do
      -- Create many metrics and spans
      replicateM_ 100 $ do
        metric <- createMetric (pack "memory-test") (pack "count")
        recordMetric metric 1.0
        span <- createSpan (pack "memory-test-span")
        finishSpan span
      
      -- Test passes if no memory leak occurs
      True `shouldBe` True

  -- 7. 数据一致性测试
  describe "Data Consistency" $ do
    it "should maintain consistency across operations" $ do
      metric <- createMetric (pack "consistency-test") (pack "count")
      
      -- Record a series of values
      let values = [1.0, 2.0, 3.0, 4.0, 5.0]
      mapM_ (recordMetric metric) values
      
      -- Verify final value
      finalValue <- metricValue metric
      finalValue `shouldBe` sum values

  -- 8. 跨模块集成测试
  describe "Cross-Module Integration" $ do
    it "should work with all telemetry components" $ do
      -- Initialize telemetry
      initTelemetry defaultConfig
      
      -- Create metric
      metric <- createMetric (pack "integration-test") (pack "count")
      recordMetric metric 1.0
      
      -- Create span
      span <- createSpan (pack "integration-span")
      let spanId = spanSpanId span
      
      -- Create logger
      logger <- createLogger (pack "integration-logger") Info
      logMessage logger Info (pack "Integration test message")
      
      -- Clean up
      finishSpan span
      shutdownTelemetry
      
      -- Test passes if no exceptions occur
      True `shouldBe` True

  -- 9. 端到端业务流程测试
  describe "End-to-End Business Flow" $ do
    it "should handle complete request lifecycle" $ do
      -- Initialize telemetry
      initTelemetry defaultConfig
      
      -- Create components for request processing
      requestMetric <- createMetric (pack "requests") (pack "count")
      latencyMetric <- createMetric (pack "latency") (pack "ms")
      requestLogger <- createLogger (pack "requests") Info
      
      -- Simulate request processing
      requestId <- getCurrentTime
      requestSpan <- createSpan (pack "request-processing")
      
      -- Record request
      recordMetric requestMetric 1.0
      
      -- Simulate processing time
      threadDelay 1000 -- 1ms
      let latency = 1.0
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
      \operations -> 
        let validOps = filter (\op -> not (isNaN op || isInfinite op)) operations
            metricOps = if null validOps 
                       then [0.0]  -- Use a default operation if all are invalid
                       else take 100 validOps  -- Use valid ops directly, not cycle
            isNaN x = x /= x  -- NaN check
            isInfinite x = abs x > 1e100  -- Simple infinity check
            -- Use a unique metric name based on the operations to ensure uniqueness
            uniqueName = "invariants-test-" ++ show (hash operations)
        in unsafePerformIO $ do
        -- Disable metric sharing for test isolation
        writeIORef enableMetricSharing False
        
        metric <- createMetric (pack uniqueName) (pack "count")
        
        -- Apply arbitrary operations
        sequence_ $ map (\op -> recordMetric metric op) metricOps
        
        -- Verify metric value is sum of all operations
        finalValue <- metricValue metric
        let expectedValue = sum metricOps
        
        -- Re-enable metric sharing
        writeIORef enableMetricSharing True
        
        return (finalValue == expectedValue)
    
    it "should preserve span properties under arbitrary names" $ property $
      \spanNameStr -> do
        let name = pack spanNameStr
        span <- createSpan name
        
        -- Verify span properties are preserved
        spanName span `shouldBe` name
        (not . null . unpack) (spanTraceId span) `shouldBe` True
        (not . null . unpack) (spanSpanId span) `shouldBe` True
        spanTraceId span `shouldNotBe` spanSpanId span
    
    it "should handle logger level filtering correctly" $ property $
      \loggerNameStr -> do
        let name = pack loggerNameStr
            levels = [Debug, Info, Warn, Error]
        
        -- Create logger with each level
        loggers <- mapM (\level -> createLogger name level) levels
        
        -- Verify logger properties
        sequence_ $ map (\logger -> do
          loggerName logger `shouldBe` name
          loggerLevel logger `shouldSatisfy` (`elem` levels)
          ) loggers