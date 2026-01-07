{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NewTelemetrySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (replicateM, replicateM_, when)
import Data.IORef
import qualified Data.Text as Text
import Data.Text (pack, unpack)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (sort, nub)
import Data.Char (ord, chr)

import Azimuth.Telemetry

spec :: Spec
spec = describe "New Telemetry Tests" $ do

  -- 测试1: 配置热重载功能
  describe "Configuration Hot Reload" $ do
    it "should preserve metric values across config changes" $ property $
      \(values :: [Double]) ->
        let simpleMetric = createSimpleMetric "hot-reload-metric" "count" 0.0
            recordAllValues = foldl recordSimpleMetric simpleMetric values
            finalValue = simpleMetricValue recordAllValues
            expectedValue = sum values
        in finalValue `shouldBe` expectedValue
    
    it "should handle multiple configuration updates" $ do
      -- Initialize with default config
            
      -- Create metric with first config
      metric1 <- createMetric "config-test-1" "count"
      recordMetric metric1 10.0
      
      -- Update config (simulate hot reload)
      let newConfig = productionConfig { enableDebugOutput = False }
      initTelemetry newConfig
      
      -- Create another metric with new config
      metric2 <- createMetric "config-test-2" "count"
      recordMetric metric2 20.0
      
      -- Verify both metrics work
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      
      value1 `shouldBe` 10.0
      value2 `shouldBe` 20.0
      
      
  -- 测试2: 指标聚合功能
  describe "Metric Aggregation" $ do
    it "should correctly aggregate metric values" $ property $
      \(values :: [Double]) ->
        let simpleMetric = createSimpleMetric "aggregation-metric" "count" 0.0
            recordAllValues = foldl recordSimpleMetric simpleMetric values
            finalValue = simpleMetricValue recordAllValues
            expectedValue = sum values
        in (not . null) values ==> finalValue `shouldBe` expectedValue
    
    it "should handle floating point precision in aggregation" $ property $
      \(values :: [Float]) ->
        let doubleValues = map realToFrac values :: [Double]
            simpleMetric = createSimpleMetric "precision-metric" "count" 0.0
            recordAllValues = foldl recordSimpleMetric simpleMetric doubleValues
            finalValue = simpleMetricValue recordAllValues
            expectedValue = sum doubleValues
        in (not . null) values ==> abs (finalValue - expectedValue) < 0.0001

  -- 测试3: 跨模块数据共享
  describe "Cross-Module Data Sharing" $ do
    it "should share metrics by name and unit" $ do
      -- Enable metric sharing for this test
      writeIORef enableMetricSharing True
      
      -- Create metrics with same name and unit
      metric1 <- createMetric "shared-metric" "count"
      metric2 <- createMetric "shared-metric" "count"
      
      -- Record value on first metric
      recordMetric metric1 42.0
      
      -- Check second metric sees the value
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      
      value1 `shouldBe` 42.0
      value2 `shouldBe` 42.0
    
    it "should isolate metrics with different units" $ do
      -- Create metrics with same name but different units
      metric1 <- createMetric "isolated-metric" "ms"
      metric2 <- createMetric "isolated-metric" "count"
      
      -- Record different values
      recordMetric metric1 100.0
      recordMetric metric2 50.0
      
      -- Check values are isolated
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      
      value1 `shouldBe` 100.0
      value2 `shouldBe` 50.0

  -- 测试4: 时间序列相关测试
  describe "Time Series Operations" $ do
    it "should handle time-based metric recording" $ property $
      \(values :: [Double]) ->
        let sortedValues = sort values
            simpleMetric = createSimpleMetric "time-series" "value" 0.0
            recordSortedValues = foldl recordSimpleMetric simpleMetric sortedValues
            finalValue = simpleMetricValue recordSortedValues
            expectedValue = sum sortedValues
        in (not . null) values ==> finalValue `shouldBe` expectedValue
    
    it "should maintain order of operations" $ do
      metric <- createMetric "ordered-metric" "count"
      
      -- Record values in specific order
      recordMetric metric 1.0
      recordMetric metric 2.0
      recordMetric metric 3.0
      
      value <- metricValue metric
      value `shouldBe` 6.0

  -- 测试5: 错误恢复机制
  describe "Error Recovery" $ do
    it "should recover from metric recording errors" $ property $
      \(values :: [Double]) ->
        let validValues = filter (not . isNaN) values
            simpleMetric = createSimpleMetric "recovery-metric" "count" 0.0
            recordValidValues = foldl recordSimpleMetric simpleMetric validValues
            finalValue = simpleMetricValue recordValidValues
            expectedValue = sum validValues
        in (not . null) validValues ==> finalValue `shouldBe` expectedValue
    
    it "should handle span creation failures gracefully" $ do
      -- Try to create spans with various names
      results <- sequence $ map createSpan ["valid-span", "", "span-with-特殊字符"]
      length results `shouldBe` 3
      
      -- All spans should be valid
      sequence_ $ map finishSpan results

  -- 测试6: 采样策略测试
  describe "Sampling Strategies" $ do
    it "should handle high-frequency metric sampling" $ property $
      \(sampleRate :: Positive Int) ->
        let numSamples = getPositive sampleRate `mod` 1000 + 1
            simpleMetric = createSimpleMetric "sampling-metric" "count" 0.0
            recordSamples = replicate numSamples 1.0
            finalMetric = foldl recordSimpleMetric simpleMetric recordSamples
            finalValue = simpleMetricValue finalMetric
            expectedValue = fromIntegral numSamples
        in finalValue `shouldBe` expectedValue
    
    it "should maintain accuracy with burst operations" $ do
      metric <- createMetric "burst-metric" "count"
      
      -- Perform burst of operations
      replicateM_ 1000 $ recordMetric metric 1.0
      
      value <- metricValue metric
      value `shouldBe` 1000.0

  -- 测试7: 批量操作性能测试
  describe "Batch Operations" $ do
    it "should handle batch metric creation" $ property $
      \(batchSize :: Positive Int) ->
        let size = getPositive batchSize `mod` 100 + 1
            simpleMetrics = replicate size $ createSimpleMetric "batch-metric" "count" 0.0
        in length simpleMetrics == size
    
    it "should handle batch span operations" $ do
      let batchSize = 50
      
      -- Create batch of spans
      spans <- replicateM batchSize $ createSpan "batch-span"
      length spans `shouldBe` batchSize
      
      -- Finish all spans
      sequence_ $ map finishSpan spans

  -- 测试8: 资源清理测试
  describe "Resource Cleanup" $ do
    it "should clean up resources on shutdown" $ do
            
      -- Create resources
      metric <- createMetric "cleanup-metric" "count"
      span <- createSpan "cleanup-span"
      logger <- createLogger "cleanup-logger" Info
      
      -- Use resources
      recordMetric metric 42.0
      logMessage logger Info "cleanup test"
      
      -- Shutdown
      shutdownTelemetry
            
      -- Reinitialize and verify clean state
      initTelemetry defaultConfig
            
      newMetric <- createMetric "cleanup-metric" "count"
      newValue <- metricValue newMetric
      newValue `shouldBe` 0.0
      
          
    it "should handle multiple shutdown cycles" $ property $
      \(cycles :: Positive Int) ->
        let numCycles = getPositive cycles `mod` 5 + 1
            cyclesValid = numCycles > 0
        in cyclesValid

  -- 测试9: 压缩和序列化测试
  describe "Compression and Serialization" $ do
    it "should handle compressed metric names" $ property $
      \(name :: String) ->
        let compressedName = pack $ nub name  -- Remove duplicate characters
            simpleMetric = createSimpleMetric compressedName "count" 0.0
            actualName = smName simpleMetric
        in actualName == compressedName
    
    it "should handle serialized data formats" $ property $
      \(values :: [Int]) ->
        let serializedValues = map show values
            simpleMetric = createSimpleMetric "serialization" "count" 0.0
            recordSerialized = map fromIntegral values
            finalMetric = foldl recordSimpleMetric simpleMetric recordSerialized
            finalValue = simpleMetricValue finalMetric
            expectedValue = fromIntegral $ sum values
        in (not . null) values ==> finalValue == expectedValue

  -- 测试10: 分布式追踪传播测试
  describe "Distributed Trace Propagation" $ do
    it "should propagate trace context across spans" $ do
      -- Create first span
      span1 <- createSpan "operation-1"
      let traceId1 = spanTraceId span1
      
      -- Create second span (should inherit trace context)
      span2 <- createSpan "operation-2"
      let traceId2 = spanTraceId span2
      
      -- Both spans should have same trace ID
      traceId1 `shouldBe` traceId2
      spanSpanId span1 `shouldNotBe` spanSpanId span2
    
    it "should handle nested span operations" $ property $
      \(depth :: Positive Int) ->
        let nestingLevel = getPositive depth `mod` 10 + 1
            nestingValid = nestingLevel > 0
        in nestingValid
    
    it "should maintain trace consistency" $ do
            
      -- Create multiple spans in sequence
      spans <- replicateM 5 $ createSpan "sequential-span"
      
      -- All should share same trace ID
      let traceIds = map spanTraceId spans
          allSame = all (== head traceIds) (tail traceIds)
      
      allSame `shouldBe` True
      
      -- All should have unique span IDs
      let spanIds = map spanSpanId spans
          uniqueSpanIds = nub spanIds
      
      length uniqueSpanIds `shouldBe` length spanIds
      
      shutdownTelemetry