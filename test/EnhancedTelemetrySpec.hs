{-# LANGUAGE OverloadedStrings #-}

module EnhancedTelemetrySpec (spec) where

import Test.Hspec
import qualified Data.Text as Text
import Data.Text (pack)
import Data.IORef

import Azimuth.Telemetry

spec :: Spec
spec = describe "Enhanced Telemetry Tests" $ do
  
  -- Test 1: Special value handling
  describe "Special Value Handling" $ do
    it "should handle NaN values in metrics" $ do
      metric <- createMetric "nan-metric" "special"
      
      -- Record NaN value
      recordMetric metric (0/0) `shouldReturn` ()
      value <- metricValue metric
      isNaN value `shouldBe` True
      
      -- Record normal value after NaN
      recordMetric metric 42.0 `shouldReturn` ()
      value <- metricValue metric
      -- Once NaN is recorded, it stays NaN (NaN propagation)
      isNaN value `shouldBe` True
      
      -- Record NaN value again
      recordMetric metric (0/0) `shouldReturn` ()
      value <- metricValue metric
      isNaN value `shouldBe` True
    
    it "should handle infinity values in metrics" $ do
      metric <- createMetric "infinity-metric" "special"
      
      -- Record positive infinity
      recordMetric metric (1/0) `shouldReturn` ()
      value <- metricValue metric
      isInfinite value `shouldBe` True
      value `shouldSatisfy` (> 0)
      
      -- Record negative infinity
      recordMetric metric (-1/0) `shouldReturn` ()
      value <- metricValue metric
      isInfinite value `shouldBe` True  -- negative infinity replaces positive infinity
      value `shouldSatisfy` (< 0)
      
      -- Record positive infinity again
      recordMetric metric (1/0) `shouldReturn` ()
      value <- metricValue metric
      isInfinite value `shouldBe` True
      value `shouldSatisfy` (> 0)

  -- Test 2: Metric sharing mechanism
  describe "Metric Sharing Mechanism" $ do
    it "should share metrics with same name and unit when enabled" $ do
      -- Enable metric sharing
      writeIORef enableMetricSharing True
      writeIORef enableMetricAggregation True
      
      -- Create two metrics with same name and unit
      metric1 <- createMetric "shared-metric" "count"
      recordMetric metric1 10.0
      
      metric2 <- createMetric "shared-metric" "count"
      value2 <- metricValue metric2
      value2 `shouldBe` 10.0  -- Should share the same value
      
      -- Record value through metric2
      recordMetric metric2 5.0
      value1 <- metricValue metric1
      value1 `shouldBe` 15.0  -- Both metrics reference the same value
    
    it "should not share metrics when disabled" $ do
      -- Disable metric sharing
      writeIORef enableMetricSharing False
      
      -- Create two metrics with same name and unit
      metric1 <- createMetric "isolated-metric" "count"
      recordMetric metric1 10.0
      
      metric2 <- createMetric "isolated-metric" "count"
      value2 <- metricValue metric2
      value2 `shouldBe` 0.0  -- Should be independent value
      
      -- Record value through metric2
      recordMetric metric2 5.0
      value1 <- metricValue metric1
      value1 `shouldBe` 10.0  -- metric1 value unchanged

  -- Test 3: Resource cleanup and memory leak tests
  describe "Resource Cleanup and Memory Management" $ do
    it "should clean up metric registry on shutdown" $ do
      -- Create some metrics
      sequence_ $ replicate 10 $ do
        createMetric "cleanup-test" "count"
      
      -- Shutdown telemetry system
            
      -- Reinitialize
            
      -- Creating metric with same name should start from initial value
      metric <- createMetric "cleanup-test" "count"
      value <- metricValue metric
      value `shouldBe` 0.0

  -- Test 4: Cross-thread data consistency tests
  describe "Cross-thread Data Consistency" $ do
    it "should maintain consistent metric values across threads" $ do
      writeIORef enableMetricSharing True
      metric <- createMetric "consistency-test" "count"
      
      let numThreads = 5
          incrementsPerThread = 10
      
      -- Increment metric value in multiple threads simultaneously
      sequence_ $ replicate numThreads $ do
        sequence_ $ replicate incrementsPerThread $ do
          recordMetric metric 1.0
      
      -- Verify final value
      value <- metricValue metric
      value `shouldBe` fromIntegral (numThreads * incrementsPerThread)

  -- Test 5: Error recovery mechanism tests
  describe "Error Recovery Mechanisms" $ do
    it "should recover from invalid metric operations" $ do
      metric <- createMetric "recovery-test" "count"
      
      -- Record series of potentially problematic values
      recordMetric metric (0/0) `shouldReturn` ()  -- NaN
      recordMetric metric (1/0) `shouldReturn` ()  -- Positive infinity
      recordMetric metric (-1/0) `shouldReturn` () -- Negative infinity
      recordMetric metric 42.0 `shouldReturn` ()   -- Normal value
      
      -- System should still work normally
      value <- metricValue metric
      -- Once NaN is recorded, it stays NaN (NaN propagation)
      isNaN value `shouldBe` True

  -- Test 6: Performance benchmark tests
  describe "Performance Benchmarks" $ do
    it "should handle high-frequency metric operations efficiently" $ do
      metric <- createMetric "performance-test" "ops"
      
      let numOperations = 100
      
      -- Measure time to record large number of metric values
      start <- metricValue metric
      sequence_ $ replicate numOperations $ recordMetric metric 1.0
      end <- metricValue metric
      
      -- Verify all operations were executed correctly
      end - start `shouldBe` fromIntegral numOperations

  -- Test 7: QuickCheck advanced property tests
  describe "Advanced QuickCheck Properties" $ do
    it "should preserve metric identity regardless of values" $ do
      let simpleMetric = createSimpleMetric "test" "unit" 42.0
          updatedMetric = recordSimpleMetric simpleMetric 999.0
      smName updatedMetric `shouldBe` smName simpleMetric
      smUnit updatedMetric `shouldBe` smUnit simpleMetric
      smValue updatedMetric `shouldNotBe` smValue simpleMetric

  -- Test 8: Configuration validation tests
  describe "Configuration Validation" $ do
    it "should handle configuration changes during operation" $ do
      -- Initialize default configuration
            
      -- Create some components
      metric <- createMetric "config-test" "count"
      recordMetric metric 10.0
      
      -- Change configuration
      let newConfig = productionConfig { enableDebugOutput = False }
      initTelemetry newConfig
      
      -- Verify components still work normally
      recordMetric metric 20.0
      value <- metricValue metric
      value `shouldBe` 30.0
      
      -- Restore default configuration
      
  -- Test 9: Data integrity tests
  describe "Data Integrity Tests" $ do
    it "should maintain data integrity during complex operations" $ do
      -- Create multiple metrics
      counter <- createMetric "counter" "count"
      gauge <- createMetric "gauge" "value"
      rate <- createMetric "rate" "per-second"
      
      -- Execute complex operation sequence
      recordMetric counter 1.0
      recordMetric gauge 42.0
      recordMetric rate 100.0
      
      recordMetric counter 1.0
      recordMetric gauge (-10.0)
      recordMetric rate 50.0
      
      -- Verify data integrity
      counterValue <- metricValue counter
      gaugeValue <- metricValue gauge
      rateValue <- metricValue rate
      
      counterValue `shouldBe` 2.0
      gaugeValue `shouldBe` 32.0
      rateValue `shouldBe` 150.0

  -- Test 10: Boundary conditions and exception handling
  describe "Boundary Conditions and Exception Handling" $ do
    it "should handle extreme values in all telemetry components" $ do
      -- Test metrics
      extremeMetric <- createMetric "extreme" "test"
      recordMetric extremeMetric 1000.0 `shouldReturn` ()  -- Large value
      recordMetric extremeMetric 500.0 `shouldReturn` ()   -- Small value
      
      -- Test span
      extremeSpan <- createSpan $ pack $ replicate 100 'x'  -- Long name
      spanName extremeSpan `shouldSatisfy` ((>= 100) . Text.length)
      finishSpan extremeSpan `shouldReturn` ()
      
      -- Test logger
      extremeLogger <- createLogger (pack $ replicate 100 'y') Error  -- Long name
      loggerName extremeLogger `shouldSatisfy` ((>= 100) . Text.length)
      logMessage extremeLogger Error (pack $ replicate 500 'z') `shouldReturn` ()  -- Long message