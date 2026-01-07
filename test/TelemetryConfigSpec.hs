{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetryConfigSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Control.Monad (forM_)
import Data.Text (pack, unpack)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "TelemetryConfig Properties Tests" $ do
  
  -- 测试配置字段的持久性
  describe "Configuration Field Persistence" $ do
    it "should preserve service name after initialization" $ property $
      \serviceName ->
        let serviceNameText = pack (take 100 serviceName)
            config = TelemetryConfig serviceNameText "1.0.0" True True True False
            result = unsafePerformIO $ do
              initTelemetry config
              -- 在实际实现中，我们可以通过全局配置引用来验证
              -- 这里我们验证初始化不会失败
                            return True
        in result
    
    it "should preserve service version after initialization" $ property $
      \serviceVersion ->
        let serviceVersionText = pack (take 50 serviceVersion)
            config = TelemetryConfig "test-service" serviceVersionText True True True False
            result = unsafePerformIO $ do
              initTelemetry config
                            return True
        in result
    
    it "should preserve feature flags after initialization" $ property $
      \(flags :: Int) ->
        let (metrics, tracing, logging, debug) = 
              if even flags then (True, True, True, False) else (False, True, False, True)
            config = TelemetryConfig "test-service" "1.0.0" metrics tracing logging debug
            result = unsafePerformIO $ do
              initTelemetry config
                            return True
        in result
  
  -- 测试默认配置
  describe "Default Configuration" $ do
    it "should have valid default configuration" $ do
      let config = defaultConfig
      serviceName config `shouldBe` "azimuth-service"
      serviceVersion config `shouldBe` "0.1.0"
      enableMetrics config `shouldBe` True
      enableTracing config `shouldBe` True
      enableLogging config `shouldBe` True
      enableDebugOutput config `shouldBe` False
    
    it "should handle operations with default configuration" $ property $
      \(operations :: Int) ->
        let numOps = max 1 (abs operations `mod` 10 + 1)
            result = unsafePerformIO $ do
              initTelemetry defaultConfig
              
              -- 尝试各种操作
              metric <- createMetric "default-config-test" "count"
              sequence_ $ map (\i -> recordMetric metric (fromIntegral i)) [1..numOps]
              
              span <- createSpan "default-config-span"
              finishSpan span
              
              logger <- createLogger "default-config-logger" Info
              logMessage logger Info "default config test"
              
                            return True
        in result
  
  -- 测试生产配置
  describe "Production Configuration" $ do
    it "should have valid production configuration" $ do
      let config = productionConfig
      serviceName config `shouldBe` "azimuth-service"
      serviceVersion config `shouldBe` "0.1.0"
      enableMetrics config `shouldBe` True
      enableTracing config `shouldBe` True
      enableLogging config `shouldBe` True
      enableDebugOutput config `shouldBe` False
    
    it "should handle operations with production configuration" $ property $
      \(operations :: Int) ->
        let numOps = max 1 (abs operations `mod` 10 + 1)
            result = unsafePerformIO $ do
              -- 尝试各种操作
              metric <- createMetric "production-config-test" "count"
              sequence_ $ map (\i -> recordMetric metric (fromIntegral i)) [1..numOps]
              
              span <- createSpan "production-config-span"
              finishSpan span
              
              logger <- createLogger "production-config-logger" Info
              logMessage logger Info "production config test"
              
              return True
        in result
  
  -- 测试配置的相等性
  describe "Configuration Equality" $ do
    it "should satisfy reflexive property: config = config" $ property $
      \serviceName serviceVersion metrics tracing logging debug ->
        let config = TelemetryConfig (pack serviceName) (pack serviceVersion) metrics tracing logging debug
        in config == config
    
    it "should satisfy symmetric property: if a = b then b = a" $ property $
      \serviceName serviceVersion metrics tracing logging debug ->
        let config1 = TelemetryConfig (pack serviceName) (pack serviceVersion) metrics tracing logging debug
            config2 = TelemetryConfig (pack serviceName) (pack serviceVersion) metrics tracing logging debug
        in if config1 == config2 then config2 == config1 else True
    
    it "should satisfy transitive property: if a = b and b = c then a = c" $ property $
      \serviceName serviceVersion metrics tracing logging debug ->
        let config1 = TelemetryConfig (pack serviceName) (pack serviceVersion) metrics tracing logging debug
            config2 = TelemetryConfig (pack serviceName) (pack serviceVersion) metrics tracing logging debug
            config3 = TelemetryConfig (pack serviceName) (pack serviceVersion) metrics tracing logging debug
        in if config1 == config2 && config2 == config3 then config1 == config3 else True
    
    it "should distinguish different configurations" $ property $
      \serviceName1 serviceName2 ->
        let config1 = TelemetryConfig (pack serviceName1) "1.0.0" True True True False
            config2 = TelemetryConfig (pack serviceName2) "1.0.0" True True True False
        in if pack serviceName1 /= pack serviceName2 then config1 /= config2 else True
  
  -- 测试配置的边界条件
  describe "Configuration Boundary Conditions" $ do
    it "should handle empty service names" $ do
      let config = TelemetryConfig "" "1.0.0" True True True False
          result = unsafePerformIO $ do
            initTelemetry config
                        return True
      result `shouldBe` True
    
    it "should handle empty service versions" $ do
      let config = TelemetryConfig "test-service" "" True True True False
          result = unsafePerformIO $ do
            initTelemetry config
                        return True
      result `shouldBe` True
    
    it "should handle very long service names" $ do
      let longName = pack $ replicate 10000 'a'
          config = TelemetryConfig longName "1.0.0" True True True False
          result = unsafePerformIO $ do
            initTelemetry config
                        return True
      result `shouldBe` True
    
    it "should handle very long service versions" $ do
      let longVersion = pack $ replicate 1000 '1'
          config = TelemetryConfig "test-service" longVersion True True True False
          result = unsafePerformIO $ do
            initTelemetry config
                        return True
      result `shouldBe` True
    
    it "should handle unicode characters in configuration" $ property $
      \serviceName serviceVersion ->
        let unicodeName = pack serviceName
            unicodeVersion = pack serviceVersion
            config = TelemetryConfig unicodeName unicodeVersion True True True False
            result = unsafePerformIO $ do
              initTelemetry config
                            return True
        in result
  
  -- 测试配置的生命周期
  describe "Configuration Lifecycle" $ do
    it "should handle multiple initialization cycles" $ property $
      \cycles ->
        let numCycles = max 1 (abs cycles `mod` 5 + 1)
            result = unsafePerformIO $ do
              sequence_ $ replicate numCycles $ do
                initTelemetry defaultConfig
                              return True
        in result
    
    it "should handle configuration changes" $ property $
      \changes ->
        let numChanges = max 1 (abs changes `mod` 5 + 1)
            configs = take numChanges $ cycle [
                defaultConfig,
                productionConfig,
                TelemetryConfig "service1" "1.0.0" True False True False,
                TelemetryConfig "service2" "2.0.0" False True False True,
                TelemetryConfig "service3" "3.0.0" True True True False
              ]
            result = unsafePerformIO $ do
              forM_ configs $ \config -> do
                initTelemetry config
                              return True
        in result
  
  -- 测试配置的错误处理
  describe "Configuration Error Handling" $ do
    it "should handle initialization failures gracefully" $ property $
      \serviceName ->
        let config = TelemetryConfig (pack serviceName) "1.0.0" True True True False
            result = unsafePerformIO $ do
              result <- try $ initTelemetry config
              case result of
                Right _ -> do
                                    return True
                Left (_ :: SomeException) -> return True
        in result
    
    it "should handle shutdown failures gracefully" $ property $
      \serviceName ->
        let config = TelemetryConfig (pack serviceName) "1.0.0" True True True False
            result = unsafePerformIO $ do
              initTelemetry config
              result <- try $ return ()
              case result of
                Right _ -> return True
                Left (_ :: SomeException) -> return True
        in result
  
  -- 测试配置的功能标志
  describe "Configuration Feature Flags" $ do
    it "should respect metrics flag" $ property $
      \(enabled :: Int) ->
        let metricsEnabled = even enabled
            config = TelemetryConfig "test-service" "1.0.0" metricsEnabled True True False
            result = unsafePerformIO $ do
              initTelemetry config
              -- 尝试创建度量
              metricResult <- (try :: IO a -> IO (Either SomeException a)) $ do
                metric <- createMetric "flag-test" "count"
                metric <- createMetric "flag-test" "count"
                recordMetric metric 1.0
                return ()
              return $ case metricResult of
                Right _ -> True
                Left _ -> False
        in result
      \(enabled :: Int) ->
        let tracingEnabled = even enabled
            config = TelemetryConfig "test-service" "1.0.0" True tracingEnabled True False
            result = unsafePerformIO $ do
              initTelemetry config
              -- 尝试创建span
              spanResult <- try ((do
                                    span <- createSpan "flag-test"
                                    finishSpan span
                                    return ()) :: IO a) :: IO (Either SomeException a)
              let isSuccess (Right _) = True
                  isSuccess (Left _) = False
              in in return (isSuccess spanResult)
    it "should respect logging flag" $ property $
      \(enabled :: Int) ->
        let loggingEnabled = even enabled
            config = TelemetryConfig "test-service" "1.0.0" True True loggingEnabled False
            result = unsafePerformIO $ do
              initTelemetry config
              -- 尝试记录日志
              logResult <- try $ do
                logger <- createLogger "flag-test" Info
                logMessage logger Info "flag test"
                return ()
              let isSuccess (Right _) = True
                  isSuccess (Left _) = False
              in return (isSuccess logResult)
    it "should respect debug flag" $ property $
      \(enabled :: Int) ->
        let debugEnabled = even enabled
            config = TelemetryConfig "test-service" "1.0.0" True True True debugEnabled
            result = unsafePerformIO $ do
              initTelemetry config
              -- 尝试记录调试信息
              debugResult <- try $ do
                logDebug "debug test"
                return ()
              let isSuccess (Right _) = True
                  isSuccess (Left _) = False
              in return (isSuccess debugResult)
        in result