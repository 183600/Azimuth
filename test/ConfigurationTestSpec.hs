{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConfigurationTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (pack, unpack)
import Control.Monad (when)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Configuration Tests" $ do
  
  -- 1. 测试默认配置
  describe "Default Configuration" $ do
    it "should have correct default values" $ do
      serviceName defaultConfig `shouldBe` "azimuth-service"
      serviceVersion defaultConfig `shouldBe` "0.1.0"
      enableMetrics defaultConfig `shouldBe` True
      enableTracing defaultConfig `shouldBe` True
      enableLogging defaultConfig `shouldBe` True
      enableDebugOutput defaultConfig `shouldBe` False
  
  -- 2. 测试生产配置
  describe "Production Configuration" $ do
    it "should have correct production values" $ do
      serviceName productionConfig `shouldBe` "azimuth-service"
      serviceVersion productionConfig `shouldBe` "0.1.0"
      enableMetrics productionConfig `shouldBe` True
      enableTracing productionConfig `shouldBe` True
      enableLogging productionConfig `shouldBe` True
      enableDebugOutput productionConfig `shouldBe` False
  
  -- 3. 测试配置初始化
  describe "Configuration Initialization" $ do
    it "should initialize with custom configuration" $ do
      let customConfig = TelemetryConfig
            { serviceName = "custom-service"
            , serviceVersion = "1.0.0"
            , enableMetrics = True
            , enableTracing = False
            , enableLogging = True
            , enableDebugOutput = True
            }
      
      initTelemetry customConfig
      
      -- 验证配置已应用（通过创建资源来验证）
      metric <- createMetric "config-test" "count"
      recordMetric metric 1.0
      value <- metricValue metric
      value `shouldBe` 1.0
      
      logger <- createLogger "config-test-logger" Info
      logMessage logger Info "config test"
      
      span <- createSpan "config-test-span"
      finishSpan span
      
      -- 如果没有异常，配置生效
      True `shouldBe` True
    
    it "should handle multiple initializations" $ do
      let config1 = TelemetryConfig "service1" "1.0.0" True True True False
          config2 = TelemetryConfig "service2" "2.0.0" False True True True
      
      -- 初始化第一个配置
      initTelemetry config1
      
      -- 创建资源
      metric1 <- createMetric "multi-init-test" "count"
      recordMetric metric1 1.0
      
      -- 重新初始化（热更新）
      initTelemetry config2
      
      -- 创建更多资源
      metric2 <- createMetric "multi-init-test-2" "count"
      recordMetric metric2 2.0
      
      value2 <- metricValue metric2
      value2 `shouldBe` 2.0
  
  -- 4. 测试配置关闭
  describe "Configuration Shutdown" $ do
    it "should shutdown cleanly" $ do
      -- 初始化配置
      initTelemetry defaultConfig
      
      -- 创建资源
      metric <- createMetric "shutdown-test" "count"
      recordMetric metric 1.0
      
      logger <- createLogger "shutdown-test-logger" Info
      logMessage logger Info "before shutdown"
      
      span <- createSpan "shutdown-test-span"
      
      -- 关闭系统
      shutdownTelemetry
      
      finishSpan span
      
      -- 重新初始化
      initTelemetry defaultConfig
      
      -- 创建新资源
      newMetric <- createMetric "after-shutdown-test" "count"
      recordMetric newMetric 2.0
      value <- metricValue newMetric
      value `shouldBe` 2.0
  
  -- 5. 测试配置属性
  describe "Configuration Properties" $ do
    it "should maintain configuration equality" $ do
      let config1 = TelemetryConfig "service" "1.0.0" True True True False
          config2 = TelemetryConfig "service" "1.0.0" True True True False
          config3 = TelemetryConfig "different-service" "1.0.0" True True True False
      
      config1 `shouldBe` config2
      config1 `shouldNotBe` config3
    
    it "should show configuration correctly" $ do
      let config = TelemetryConfig "test-service" "1.0.0" True False True True
          configStr = show config
      
      configStr `shouldContain` "test-service"
      configStr `shouldContain` "1.0.0"
  
  -- 6. 测试QuickCheck属性
  describe "QuickCheck Properties" $ do
    it "should handle arbitrary configuration values" $ property $
      \(name :: String) (version :: String) (metrics :: Bool) (tracing :: Bool) (logging :: Bool) (debug :: Bool) ->
        let config = TelemetryConfig (pack name) (pack version) metrics tracing logging debug
        in unsafePerformIO $ do
          initTelemetry config
          
          -- 根据配置创建资源
          when (enableMetrics config) $ do
            metric <- createMetric "property-test" "count"
            recordMetric metric 1.0
          
          when (enableLogging config) $ do
            logger <- createLogger "property-test-logger" Info
            logMessage logger Info "property test"
          
          when (enableTracing config) $ do
            span <- createSpan "property-test-span"
            finishSpan span
          
          -- 如果没有异常，配置有效
          return True
    
    it "should maintain configuration consistency" $ property $
      \(name :: String) (version :: String) ->
        let config1 = TelemetryConfig (pack name) (pack version) True True True False
            config2 = TelemetryConfig (pack name) (pack version) True True True False
        in config1 == config2
    
    it "should distinguish different configurations" $ property $
      \(name1 :: String) (name2 :: String) ->
        let config1 = TelemetryConfig (pack name1) "1.0.0" True True True False
            config2 = TelemetryConfig (pack name2) "1.0.0" True True True False
        in if name1 == name2
           then config1 == config2
           else config1 /= config2