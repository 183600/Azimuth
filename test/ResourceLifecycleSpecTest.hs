{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResourceLifecycleSpecTest (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)
import Data.Text (pack)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Resource Lifecycle Tests" $ do
  
  -- 测试初始化生命周期
  describe "Initialization Lifecycle" $ do
    it "should handle single initialization cycle" $ property $
      \serviceName serviceVersion ->
        let config = TelemetryConfig (pack $ take 50 serviceName) (pack $ take 20 serviceVersion) True True True False
            result = unsafePerformIO $ do
              initTelemetry config
              -- 执行一些操作
              metric <- createMetric "init-test" "count"
              recordMetric metric 1.0
              shutdownTelemetry
              return True
        in result
    
    it "should handle multiple initialization cycles" $ property $
      \cycles ->
        let numCycles = max 1 (abs cycles `mod` 5 + 1)
            result = unsafePerformIO $ do
              sequence_ $ replicate numCycles $ do
                initTelemetry productionConfig
                -- 执行一些操作
                metric <- createMetric "multi-init-test" "count"
                recordMetric metric 1.0
                shutdownTelemetry
              return True
        in result
    
    it "should handle initialization with different configurations" $ property $
      \configs ->
        let testConfigs = take 3 $ cycle [
            productionConfig,
            TelemetryConfig "test-service-1" "1.0.0" True False True False,
            TelemetryConfig "test-service-2" "2.0.0" False True False True,
            TelemetryConfig "test-service-3" "3.0.0" True True True False
          ]
            result = unsafePerformIO $ do
              sequence_ $ map (\config -> do
                initTelemetry config
                -- 执行一些操作
                metric <- createMetric "config-test" "count"
                recordMetric metric 1.0
                shutdownTelemetry
              ) testConfigs
              return True
        in result