{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AzimuthCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Control.Monad (replicateM_)
import qualified Data.Text as Text
import Data.Text (pack, unpack)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Control.Concurrent (forkIO, threadDelay, myThreadId)
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar, MVar)
import Data.List (sort, nub)
import Data.Char (isAscii, isControl)
import Prelude hiding (id)

import Azimuth.Telemetry

-- | Test metric with special values
testSpecialValues :: IO ()
testSpecialValues = do
  let positiveInfinity = 1/0 :: Double
      negativeInfinity = -1/0 :: Double
      nan = 0/0 :: Double
  
  metric <- createMetric "special-values" "test"
  
  recordMetric metric positiveInfinity
  val1 <- metricValue metric
  evaluate (val1 == positiveInfinity) `shouldReturn` True
  
  recordMetric metric negativeInfinity
  val2 <- metricValue metric
  evaluate (val2 == negativeInfinity) `shouldReturn` True
  
  recordMetric metric nan
  val3 <- metricValue metric
  evaluate (isNaN val3) `shouldReturn` True

-- | Test metric registry isolation
testMetricRegistryIsolation :: IO ()
testMetricRegistryIsolation = do
  -- Disable metric sharing for test isolation
  writeIORef enableMetricSharing False
  
  metric1 <- createMetric "isolation-test" "count"
  metric2 <- createMetric "isolation-test" "count"
  
  recordMetric metric1 10.0
  recordMetric metric2 20.0
  
  val1 <- metricValue metric1
  val2 <- metricValue metric2
  
  val1 `shouldSatisfy` (== 10.0)
  val2 `shouldSatisfy` (== 20.0)
  val1 `shouldNotBe` val2

-- | Test concurrent metric operations
testConcurrentMetricOperations :: IO ()
testConcurrentMetricOperations = do
  metric <- createMetric "concurrent-test" "count"
  
  let numThreads = 10
      operationsPerThread = 100
  
  -- Create synchronization barriers
  barrier <- newMVar ()
  completion <- newIORef 0
  
  -- Fork threads that will all increment the same metric
  threads <- sequence $ replicate numThreads $ forkIO $ do
    takeMVar barrier  -- Wait for all threads to be ready
    replicateM_ operationsPerThread $ recordMetric metric 1.0
    atomicModifyIORef' completion (\c -> (c + 1, ()))  -- Signal completion
  
  -- Release all threads
  replicateM_ numThreads $ putMVar barrier ()
  
  -- Wait for all threads to complete by checking completion counter
  let waitForCompletion = do
        count <- readIORef completion
        if count == numThreads 
          then return ()
          else do
            threadDelay 1000  -- 1ms
            waitForCompletion
  
  waitForCompletion
  
  -- Wait a bit for all operations to complete
  threadDelay 100000  -- 100ms
  
  finalValue <- metricValue metric
  finalValue `shouldSatisfy` (>= fromIntegral (numThreads * operationsPerThread))

-- | Test span ID uniqueness
testSpanIdUniqueness :: IO ()
testSpanIdUniqueness = do
  let numSpans = 1000
  
  spans <- sequence $ replicate numSpans $ createSpan "uniqueness-test"
  let spanIds = map spanSpanId spans
      uniqueSpanIds = nub spanIds
  
  length uniqueSpanIds `shouldBe` numSpans

-- | Test trace context propagation
testTraceContextPropagation :: IO ()
testTraceContextPropagation = do
  span1 <- createSpan "parent-span"
  let traceId1 = spanTraceId span1
  
  span2 <- createSpan "child-span"
  let traceId2 = spanTraceId span2
  
  -- Spans created in sequence should have the same trace ID
  traceId1 `shouldBe` traceId2
  traceId1 `shouldNotBe` spanSpanId span1
  traceId2 `shouldNotBe` spanSpanId span2

-- | Test logger level filtering
testLoggerLevelFiltering :: IO ()
testLoggerLevelFiltering = do
  logger <- createLogger "level-test" Warn
  
  -- These should all succeed without errors
  logMessage logger Debug "debug message" `shouldReturn` ()
  logMessage logger Info "info message" `shouldReturn` ()
  logMessage logger Warn "warning message" `shouldReturn` ()
  logMessage logger Error "error message" `shouldReturn` ()

-- | Test telemetry configuration validation
testTelemetryConfigurationValidation :: IO ()
testTelemetryConfigurationValidation = do
  let testConfig = TelemetryConfig "test-service" "1.0.0" True False True False
  
  initTelemetry testConfig
  shutdownTelemetry
  
  -- Test with empty service name
  let emptyConfig = TelemetryConfig "" "1.0.0" True True True False
  initTelemetry emptyConfig
  shutdownTelemetry
  
  -- Test with very long names
  let longName = pack $ replicate 1000 'a'
      longConfig = TelemetryConfig longName longName True True True False
  initTelemetry longConfig
  shutdownTelemetry

-- | Test metric aggregation patterns
testMetricAggregationPatterns :: IO ()
testMetricAggregationPatterns = do
  counter <- createMetric "counter" "count"
  gauge <- createMetric "gauge" "value"
  
  -- Counter pattern (only increments)
  recordMetric counter 1.0
  recordMetric counter 1.0
  recordMetric counter 1.0
  counterValue <- metricValue counter
  counterValue `shouldBe` 3.0
  
  -- Gauge pattern (can go up and down)
  recordMetric gauge 10.0
  recordMetric gauge (-5.0)
  recordMetric gauge 2.0
  gaugeValue <- metricValue gauge
  gaugeValue `shouldBe` 7.0

-- | Test resource cleanup
testResourceCleanup :: IO ()
testResourceCleanup = do
  initTelemetry productionConfig
  
  -- Create many resources
  metrics <- sequence $ replicate 100 $ createMetric "cleanup-test" "count"
  spans <- sequence $ replicate 50 $ createSpan "cleanup-test"
  loggers <- sequence $ replicate 25 $ createLogger "cleanup-test" Info
  
  -- Use the resources
  sequence_ $ map (`recordMetric` 1.0) metrics
  sequence_ $ map finishSpan spans
  sequence_ $ flip map loggers $ \logger -> logMessage logger Info "cleanup test"
  
  -- Shutdown and verify no errors
  shutdownTelemetry
  
  -- Should be able to reinitialize
  initTelemetry productionConfig
  shutdownTelemetry

-- | QuickCheck property for metric operations
prop_metricOperations :: [Double] -> Bool
prop_metricOperations values = unsafePerformIO $ do
  writeIORef enableMetricSharing False  -- Disable sharing for test isolation
  metric <- createMetric "quickcheck-test" "unit"
  
  -- Record all values
  sequence_ $ map (recordMetric metric) values
  
  -- Get final value
  finalValue <- metricValue metric
  
  -- Calculate expected sum (handling special values)
  let expectedSum = foldl (\acc val -> 
        if isNaN val then val
        else if isNaN acc then val
        else if isInfinite val && isInfinite acc && signum val /= signum acc then 0/0
        else if isInfinite val then val
        else if isInfinite acc then val
        else acc + val) 0.0 values
  
  return $ if isNaN expectedSum then isNaN finalValue else finalValue == expectedSum

-- | QuickCheck property for span creation
prop_spanCreation :: String -> Bool
prop_spanCreation name = unsafePerformIO $ do
  let nameText = pack name
  span <- createSpan nameText
  
  let hasValidName = spanName span == nameText
      hasValidTraceId = not $ Text.null $ spanTraceId span
      hasValidSpanId = not $ Text.null $ spanSpanId span
      idsAreDifferent = spanTraceId span /= spanSpanId span
  
  return $ hasValidName && hasValidTraceId && hasValidSpanId && idsAreDifferent

-- | QuickCheck property for logger creation
prop_loggerCreation :: String -> Int -> Bool
prop_loggerCreation name levelInt = unsafePerformIO $ do
  let nameText = pack name
      levels = [Debug, Info, Warn, Error]
      level = levels !! (abs levelInt `mod` length levels)
  
  logger <- createLogger nameText level
  
  let hasValidName = loggerName logger == nameText
      hasValidLevel = loggerLevel logger == level
  
  return $ hasValidName && hasValidLevel

spec :: Spec
spec = describe "AzimuthCabalTestSpec" $ do
  
  -- 1. 特殊值处理测试
  describe "Special Values Handling" $ do
    it "should handle infinity and NaN values correctly" $ do
      testSpecialValues
  
  -- 2. 指标注册表隔离测试
  describe "Metric Registry Isolation" $ do
    it "should isolate metrics when sharing is disabled" $ do
      testMetricRegistryIsolation
  
  -- 3. 并发操作测试
  describe "Concurrent Operations" $ do
    it "should handle concurrent metric operations safely" $ do
      testConcurrentMetricOperations
  
  -- 4. Span ID唯一性测试
  describe "Span ID Uniqueness" $ do
    it "should generate unique span IDs" $ do
      testSpanIdUniqueness
  
  -- 5. 追踪上下文传播测试
  describe "Trace Context Propagation" $ do
    it "should propagate trace context between spans" $ do
      testTraceContextPropagation
  
  -- 6. 日志级别过滤测试
  describe "Logger Level Filtering" $ do
    it "should handle all log levels without errors" $ do
      testLoggerLevelFiltering
  
  -- 7. 遥测配置验证测试
  describe "Telemetry Configuration Validation" $ do
    it "should validate various configuration scenarios" $ do
      testTelemetryConfigurationValidation
  
  -- 8. 指标聚合模式测试
  describe "Metric Aggregation Patterns" $ do
    it "should support counter and gauge patterns" $ do
      testMetricAggregationPatterns
  
  -- 9. 资源清理测试
  describe "Resource Cleanup" $ do
    it "should clean up resources properly" $ do
      testResourceCleanup
  
  -- 10. QuickCheck属性测试
  describe "QuickCheck Properties" $ do
    it "should satisfy metric operations property" $ property $
      prop_metricOperations
    
    it "should satisfy span creation property" $ property $
      prop_spanCreation
    
    it "should satisfy logger creation property" $ property $
      prop_loggerCreation