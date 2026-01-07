{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Azimuth.Telemetry
import Data.IORef

main :: IO ()
main = hspec $ do
  describe "Metric Sharing Tests" $ do
    it "should share metrics with same name and unit" $ do
      -- Save current sharing setting
      originalSharing <- readIORef enableMetricSharing
      
      -- Enable metric sharing
      writeIORef enableMetricSharing True
      
      -- Initialize telemetry to ensure clean state
      initTelemetry defaultConfig
      
      -- Create two metrics with same name and unit
      metric1 <- createMetric "shared-metric" "count"
      recordMetric metric1 10.0
      
      metric2 <- createMetric "shared-metric" "count"
      value <- metricValue metric2
      value `shouldBe` 10.0
      
      -- Record through second metric
      recordMetric metric2 5.0
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      value1 `shouldBe` 15.0
      value2 `shouldBe` 15.0
      
      -- Restore original sharing setting
      writeIORef enableMetricSharing originalSharing
      
      shutdownTelemetry
      
    it "should clear metric registry on shutdown" $ do
      -- Save current sharing setting
      originalSharing <- readIORef enableMetricSharing
      
      -- Enable metric sharing
      writeIORef enableMetricSharing True
      
      -- Initialize telemetry to ensure clean state
      initTelemetry defaultConfig
      
      -- Create many metrics to populate registry
      metrics <- sequence $ replicate 10 $ do
        createMetric "memory-test" "count"
      
      -- Check that registry is populated
      import qualified Data.Map as Map
      registryBefore <- readMVar metricRegistry
      Map.size registryBefore `shouldSatisfy` (> 0)
      
      -- Shutdown telemetry
      shutdownTelemetry
              
      -- Registry should be empty
      registry <- readMVar metricRegistry
      Map.null registry `shouldBe` True
      
      -- Restore original sharing setting
      writeIORef enableMetricSharing originalSharing
      
    it "should handle configuration changes without affecting operations" $ do
      let config1 = TelemetryConfig "test-service" "1.0.0" True True True False
          config2 = TelemetryConfig "test-service-updated" "2.0.0" False True False False
      
      -- Initialize with first config
      initTelemetry config1
      
      -- Create metric
      metric <- createMetric "config-test" "count"
      recordMetric metric 10.0
      
      -- Update configuration
      initTelemetry config2
      
      -- Continue using metric
      recordMetric metric 20.0
      value <- metricValue metric
      
      value `shouldBe` 30.0
      
      shutdownTelemetry