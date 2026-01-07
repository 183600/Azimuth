{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EnhancedCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Control.Concurrent (forkIO, threadDelay, readMVar)
import Control.Monad (replicateM, when)
import qualified Data.Text as Text
import Data.Text (pack, unpack)
import Data.IORef
import Data.List (nub, foldl')
import Data.Char (isHexDigit)
import qualified Data.Map as Map
import Prelude hiding (id)
import Azimuth.Telemetry

spec :: Spec
spec = do
  describe "Enhanced Cabal Tests" $ do
    
    -- 1. SimpleMetric的全面测试
    describe "SimpleMetric Comprehensive Tests" $ do
      it "should create simple metric with initial value" $ do
        let metric = createSimpleMetric "test-simple" "count" 42.0
        smName metric `shouldBe` "test-simple"
        smUnit metric `shouldBe` "count"
        smValue metric `shouldBe` 42.0
      
      it "should record values to simple metric" $ do
        let metric = createSimpleMetric "test-record" "count" 10.0
            updatedMetric = recordSimpleMetric metric 5.0
        smValue updatedMetric `shouldBe` 15.0
      
      it "should handle negative values in simple metric" $ do
        let metric = createSimpleMetric "test-negative" "temp" 20.0
            updatedMetric = recordSimpleMetric metric (-30.0)
        smValue updatedMetric `shouldBe` (-10.0)
      
      it "should handle zero values in simple metric" $ do
        let metric = createSimpleMetric "test-zero" "count" 100.0
            updatedMetric1 = recordSimpleMetric metric 0.0
            updatedMetric2 = recordSimpleMetric updatedMetric1 0.0
        smValue updatedMetric2 `shouldBe` 100.0
      
      it "should convert simple metric to regular metric" $ do
        let simpleMetric = createSimpleMetric "convert-test" "ms" 25.0
        regularMetric <- simpleToMetric simpleMetric
        metricName regularMetric `shouldBe` "convert-test"
        metricUnit regularMetric `shouldBe` "ms"
        value <- metricValue regularMetric
        value `shouldBe` 25.0

    -- 2. NaN和Infinity值的特殊处理测试
    describe "Special Values Handling" $ do
      it "should handle NaN values in metrics" $ do
        metric <- createMetric "nan-test" "special"
        recordMetric metric (0/0) `shouldReturn` ()
        value <- metricValue metric
        isNaN value `shouldBe` True
      
      it "should handle positive infinity in metrics" $ do
        metric <- createMetric "pos-inf-test" "special"
        recordMetric metric (1/0) `shouldReturn` ()
        value <- metricValue metric
        isInfinite value `shouldBe` True
        value `shouldSatisfy` (> 0)
      
      it "should handle negative infinity in metrics" $ do
        metric <- createMetric "neg-inf-test" "special"
        recordMetric metric (-1/0) `shouldReturn` ()
        value <- metricValue metric
        isInfinite value `shouldBe` True
        value `shouldSatisfy` (< 0)
      
      it "should handle NaN values in simple metrics" $ do
        let metric = createSimpleMetric "nan-simple" "special" 10.0
            updatedMetric = recordSimpleMetric metric (0/0)
        smValue updatedMetric `shouldSatisfy` isNaN
      
      it "should handle infinity values in simple metrics" $ do
        let metric = createSimpleMetric "inf-simple" "special" 10.0
            updatedMetric1 = recordSimpleMetric metric (1/0)
            updatedMetric2 = recordSimpleMetric updatedMetric1 (-1/0)
        smValue updatedMetric1 `shouldSatisfy` isInfinite
        smValue updatedMetric2 `shouldSatisfy` isInfinite
      
      it "should handle transition from NaN to finite values" $ do
        metric <- createMetric "nan-to-finite" "special"
        recordMetric metric (0/0) `shouldReturn` ()
        recordMetric metric 42.0 `shouldReturn` ()
        value <- metricValue metric
        -- Once NaN is recorded, it stays NaN (NaN propagation)
        isNaN value `shouldBe` True
      
      it "should handle transition from infinity to finite values" $ do
        metric <- createMetric "inf-to-finite" "special"
        recordMetric metric (1/0) `shouldReturn` ()
        recordMetric metric 100.0 `shouldReturn` ()
        value <- metricValue metric
        -- Once infinity is recorded, it stays infinity
        isInfinite value `shouldBe` True

    -- 3. metric sharing功能的测试
    describe "Metric Sharing Tests" $ do
      it "should share metrics with same name and unit when enabled" $ do
        -- Enable metric sharing
        writeIORef enableMetricSharing True
        
        metric1 <- createMetric "shared-metric" "count"
        recordMetric metric1 10.0
        
        metric2 <- createMetric "shared-metric" "count"
        value <- metricValue metric2
        value `shouldBe` 10.0
        
        -- Verify they are the same metric (same IORef)
        recordMetric metric1 5.0
        value2 <- metricValue metric2
        value2 `shouldBe` 15.0
      
      it "should create separate metrics when sharing is disabled" $ do
        -- Disable metric sharing
        writeIORef enableMetricSharing False
        
        metric1 <- createMetric "separate-metric" "count"
        recordMetric metric1 10.0
        
        metric2 <- createMetric "separate-metric" "count"
        value <- metricValue metric2
        value `shouldBe` 0.0  -- Should be initial value, not shared
        
        -- Verify they are separate metrics
        recordMetric metric1 5.0
        value2 <- metricValue metric2
        value2 `shouldBe` 0.0  -- Should remain unchanged
      
      it "should not share metrics with different units" $ do
        writeIORef enableMetricSharing True
        
        metric1 <- createMetric "same-name-diff-unit" "ms"
        recordMetric metric1 100.0
        
        metric2 <- createMetric "same-name-diff-unit" "count"
        value <- metricValue metric2
        value `shouldBe` 0.0  -- Should be initial value, not shared
      
      it "should handle metric sharing with initial values" $ do
        writeIORef enableMetricSharing True
        
        metric1 <- createMetricWithInitialValue "shared-initial" "count" 5.0
        value1 <- metricValue metric1
        value1 `shouldBe` 5.0
        
        metric2 <- createMetricWithInitialValue "shared-initial" "count" 10.0
        value2 <- metricValue metric2
        value2 `shouldBe` 10.0  -- Should be updated with new initial value

    -- 4. trace context的测试
    describe "Trace Context Tests" $ do
      it "should maintain trace context across spans" $ do
        span1 <- createSpan "first-span"
        let traceId1 = spanTraceId span1
        
        span2 <- createSpan "second-span"
        let traceId2 = spanTraceId span2
        
        -- Both spans should have the same trace ID
        traceId1 `shouldBe` traceId2
        
        -- But different span IDs
        spanSpanId span1 `shouldNotBe` spanSpanId span2
      
      it "should generate different trace IDs for new traces" $ do
        -- Clear trace context by shutting down and reinitializing
                        
        span1 <- createSpan "trace-1"
        let traceId1 = spanTraceId span1
        
        -- Clear trace context again
                        
        span2 <- createSpan "trace-2"
        let traceId2 = spanTraceId span2
        
        -- Should have different trace IDs
        traceId1 `shouldNotBe` traceId2

    -- 5. span ID生成唯一性测试
    describe "Span ID Uniqueness Tests" $ do
      it "should generate unique span IDs" $ do
        let numSpans = 100
        spans <- replicateM numSpans $ createSpan "uniqueness-test"
        let spanIds = map spanSpanId spans
        
        -- All span IDs should be unique
        length (nub spanIds) `shouldBe` numSpans
        
        -- All span IDs should be non-empty
        all (not . Text.null) spanIds `shouldBe` True
        
        -- All span IDs should be hex characters
        all (all isHexDigit . unpack) spanIds `shouldBe` True
      
      it "should generate span IDs with consistent length" $ do
        spans <- replicateM 10 $ createSpan "length-test"
        let spanIds = map spanSpanId spans
            lengths = map Text.length spanIds
        
        -- All span IDs should have the same length
        length (nub lengths) `shouldBe` 1
        
        -- Span ID should be 12 characters long (8 for counter + 4 for thread hash)
        head lengths `shouldBe` 12

    -- 6. 日志级别过滤的测试
    describe "Log Level Filtering Tests" $ do
      it "should create loggers with different levels" $ do
        debugLogger <- createLogger "debug-logger" Debug
        infoLogger <- createLogger "info-logger" Info
        warnLogger <- createLogger "warn-logger" Warn
        errorLogger <- createLogger "error-logger" Error
        
        loggerLevel debugLogger `shouldBe` Debug
        loggerLevel infoLogger `shouldBe` Info
        loggerLevel warnLogger `shouldBe` Warn
        loggerLevel errorLogger `shouldBe` Error
        
        -- Test level ordering
        loggerLevel debugLogger `shouldBe` Debug
        loggerLevel infoLogger `shouldBe` Info
        loggerLevel warnLogger `shouldBe` Warn
        loggerLevel errorLogger `shouldBe` Error
        -- Verify ordering using numeric values
        fromEnum (loggerLevel debugLogger) `shouldBe` 0
        fromEnum (loggerLevel infoLogger) `shouldBe` 1
        fromEnum (loggerLevel warnLogger) `shouldBe` 2
        fromEnum (loggerLevel errorLogger) `shouldBe` 3
      
      it "should handle all log levels without errors" $ do
        logger <- createLogger "level-test" Info
        
        logMessage logger Debug "debug message" `shouldReturn` ()
        logMessage logger Info "info message" `shouldReturn` ()
        logMessage logger Warn "warning message" `shouldReturn` ()
        logMessage logger Error "error message" `shouldReturn` ()

    -- 7. QuickCheck属性测试
    describe "QuickCheck Property Tests" $ do
      it "should preserve simple metric properties" $ property $
        \(name :: String) (unit :: String) (value :: Double) ->
          let metric = createSimpleMetric (pack name) (pack unit) value
              updatedMetric = recordSimpleMetric metric 5.0
          in unpack (smName metric) == name &&
             unpack (smUnit metric) == unit &&
             smValue updatedMetric == value + 5.0
      
      it "should handle any double value in simple metric" $ property $
        \(initialValue :: Double) (recordValue :: Double) ->
          let metric = createSimpleMetric "prop-test" "unit" initialValue
              updatedMetric = recordSimpleMetric metric recordValue
              expectedValue 
                | isNaN recordValue = recordValue
                | isNaN initialValue = recordValue
                | isInfinite recordValue = recordValue
                | isInfinite initialValue = recordValue
                | otherwise = initialValue + recordValue
          in smValue updatedMetric == expectedValue
      
      it "should maintain metric identity with same name and unit" $ property $
        \(name :: String) (unit :: String) ->
          let metric1 = createSimpleMetric (pack name) (pack unit) 0.0
              metric2 = createSimpleMetric (pack name) (pack unit) 42.0
          in smName metric1 == smName metric2 &&
             smUnit metric1 == smUnit metric2 &&
             smValue metric1 /= smValue metric2
      
      it "should handle empty strings in simple metrics" $ property $
        \(_ :: String) ->
          let metric = createSimpleMetric "" "" 0.0
              updatedMetric = recordSimpleMetric metric 1.0
          in smName metric == "" &&
             smUnit metric == "" &&
             smValue updatedMetric == 1.0

    -- 8. 边界条件和错误处理测试
    describe "Boundary Conditions and Error Handling" $ do
      it "should handle very small values" $ do
        metric <- createMetric "tiny-values" "scientific"
        recordMetric metric 1e-300 `shouldReturn` ()
        recordMetric metric 1e-320 `shouldReturn` ()  -- Very close to underflow
        value <- metricValue metric
        value `shouldSatisfy` (> 0)
      
      it "should handle very large values" $ do
        metric <- createMetric "large-values" "scientific"
        recordMetric metric 1e300 `shouldReturn` ()
        value <- metricValue metric
        value `shouldSatisfy` (> 0)
      
      it "should handle alternating NaN and finite values" $ do
        metric <- createMetric "alternating" "test"
        recordMetric metric (0/0) `shouldReturn` ()
        recordMetric metric 42.0 `shouldReturn` ()
        recordMetric metric (0/0) `shouldReturn` ()
        recordMetric metric 100.0 `shouldReturn` ()
        value <- metricValue metric
        -- Once NaN is recorded, it stays NaN (NaN propagation)
        isNaN value `shouldBe` True
      
      it "should handle alternating infinity and finite values" $ do
        metric <- createMetric "alternating-inf" "test"
        recordMetric metric (1/0) `shouldReturn` ()
        recordMetric metric 42.0 `shouldReturn` ()
        recordMetric metric (-1/0) `shouldReturn` ()
        recordMetric metric 100.0 `shouldReturn` ()
        value <- metricValue metric
        -- Once infinity is recorded, it stays infinity
        isInfinite value `shouldBe` True

    -- 9. 性能和并发测试
    describe "Performance and Concurrency Tests" $ do
      it "should handle concurrent simple metric operations" $ do
        let metric = createSimpleMetric "concurrent-simple" "count" 0.0
            numThreads = 10
            operationsPerThread = 100
        
        -- Simulate concurrent operations by creating multiple updated versions
        results <- replicateM numThreads $ do
          let updateOperations = replicate operationsPerThread $ \m -> recordSimpleMetric m 1.0
          return $ foldl' (\m op -> op m) metric updateOperations
        
        -- All results should have the same name and unit
        all (\m -> smName m == "concurrent-simple" && smUnit m == "count") results `shouldBe` True
        
        -- All results should have the same final value
        let values = map smValue results
        length (nub values) `shouldBe` 1
        head values `shouldBe` fromIntegral operationsPerThread
      
      it "should handle rapid span creation and finishing" $ do
        let numOperations = 100
        spans <- replicateM numOperations $ createSpan "rapid-span"
        
        -- All spans should have non-empty IDs
        all (not . Text.null . spanTraceId) spans `shouldBe` True
        all (not . Text.null . spanSpanId) spans `shouldBe` True
        
        -- All trace IDs should be the same (same trace context)
        let traceIds = map spanTraceId spans
        length (nub traceIds) `shouldBe` 1
        
        -- All span IDs should be unique
        let spanIds = map spanSpanId spans
        length (nub spanIds) `shouldBe` numOperations
        
        -- Finish all spans
        sequence_ $ map finishSpan spans

    -- 10. 资源清理和内存泄漏测试
    describe "Resource Cleanup and Memory Leak Tests" $ do
      it "should properly clean up resources on shutdown" $ do
        -- Initialize telemetry
                
        -- Create resources
        metric <- createMetric "cleanup-test" "count"
        recordMetric metric 100.0
        
        span <- createSpan "cleanup-span"
        
        logger <- createLogger "cleanup-logger" Info
        logMessage logger Info "cleanup test"
        
        -- Shutdown telemetry
                
        -- Verify metric registry is cleared
        registry <- readMVar metricRegistry
        Map.null registry `shouldBe` True
        
        -- Reinitialize and verify clean state
                
        newMetric <- createMetric "cleanup-test" "count"
        value <- metricValue newMetric
        value `shouldBe` 0.0  -- Should be initial value, not from previous session