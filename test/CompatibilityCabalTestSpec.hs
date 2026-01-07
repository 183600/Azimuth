{-# LANGUAGE OverloadedStrings #-}

module CompatibilityCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group, sortBy)
import Data.Ord (comparing)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, when, forM_, void, unless)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)
import Prelude hiding (id)
import Data.Version (Version, showVersion, parseVersion, makeVersion)
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP (readP_to_S)

import Azimuth.Telemetry

-- | 版本兼容性信息
data VersionCompatibility = VersionCompatibility
    { currentVersion :: Version
    , minCompatibleVersion :: Version
    , maxCompatibleVersion :: Version
    } deriving (Show, Eq)

-- | 默认版本兼容性
defaultVersionCompatibility :: VersionCompatibility
defaultVersionCompatibility = VersionCompatibility
    { currentVersion = makeVersion [0, 1, 0]
    , minCompatibleVersion = makeVersion [0, 1, 0]
    , maxCompatibleVersion = makeVersion [0, 2, 0]
    }

-- | 配置兼容性测试
data ConfigCompatibility = ConfigCompatibility
    { configName :: String
    , oldConfig :: TelemetryConfig
    , newConfig :: TelemetryConfig
    } deriving (Show, Eq)

-- | 创建配置兼容性测试用例
createConfigCompatibilityTests :: [ConfigCompatibility]
createConfigCompatibilityTests = 
    [ ConfigCompatibility
        { configName = "version-upgrade"
        , oldConfig = TelemetryConfig "service" "1.0.0" True True True False
        , newConfig = TelemetryConfig "service" "2.0.0" True True True False
        }
    , ConfigCompatibility
        { configName = "feature-enable"
        , oldConfig = TelemetryConfig "service" "1.0.0" False True True False
        , newConfig = TelemetryConfig "service" "1.0.0" True True True False
        }
    , ConfigCompatibility
        { configName = "feature-disable"
        , oldConfig = TelemetryConfig "service" "1.0.0" True True True False
        , newConfig = TelemetryConfig "service" "1.0.0" False True True False
        }
    , ConfigCompatibility
        { configName = "debug-toggle"
        , oldConfig = TelemetryConfig "service" "1.0.0" True True True False
        , newConfig = TelemetryConfig "service" "1.0.0" True True True True
        }
    ]

spec :: Spec
spec = describe "Compatibility Tests" $ do
  
  -- 1. 测试版本兼容性
  describe "Version Compatibility" $ do
    it "should handle version string parsing" $ do
      let versionStrings = 
            [ "0.1.0"
            , "1.0.0"
            , "1.2.3"
            , "10.20.30"
            ]
      
      forM_ versionStrings $ \versionStr -> do
        case [v | (v, "") <- readP_to_S parseVersion versionStr] of
          (version:_) -> do
            -- 验证版本字符串可以解析
            version `shouldSatisfy` (\v -> showVersion v == versionStr)
          [] -> do
            -- 如果解析失败，应该有明确的错误
            pendingWith $ "Could not parse version: " ++ versionStr
    
    it "should maintain backward compatibility" $ do
      -- 测试旧版本配置在新版本中的兼容性
      let oldVersionConfig = TelemetryConfig "legacy-service" "1.0.0" True True True False
      
      initTelemetry oldVersionConfig
      
      -- 使用旧版本API
      metric <- createMetric "legacy-metric" "count"
      recordMetric metric 10.0
      
      value <- metricValue metric
      value `shouldBe` 10.0
      
      -- 验证新版本API仍然可用
      span <- createSpan "legacy-span"
      finishSpan span `shouldReturn` ()
      
      logger <- createLogger "legacy-logger" Info
      logMessage logger Info (pack "legacy message") `shouldReturn` ()
      
      shutdownTelemetry
    
    it "should handle forward compatibility" $ do
      -- 测试新版本配置在旧版本中的兼容性
      let newVersionConfig = TelemetryConfig "future-service" "2.0.0" True True True True
      
      initTelemetry newVersionConfig
      
      -- 使用新版本功能
      metric <- createMetric "future-metric" "count"
      recordMetric metric 20.0
      
      value <- metricValue metric
      value `shouldBe` 20.0
      
      -- 验证基本功能仍然可用
      span <- createSpan "future-span"
      finishSpan span `shouldReturn` ()
      
      logger <- createLogger "future-logger" Info
      logMessage logger Info (pack "future message") `shouldReturn` ()
      
      shutdownTelemetry
  
  -- 2. QuickCheck属性测试：配置兼容性
  describe "Configuration Compatibility Properties" $ do
    it "should maintain compatibility across configuration changes" $ property $
      \seed ->
        let tests = createConfigCompatibilityTests
            testIndex = abs seed `mod` length tests
            ConfigCompatibility _ oldConfig newConfig = tests !! testIndex
        in unsafePerformIO $ do
          -- 使用旧配置
          initTelemetry oldConfig
          
          metric <- createMetric "compatibility-test" "count"
          recordMetric metric 5.0
          
          value1 <- metricValue metric
          
          -- 切换到新配置
          initTelemetry newConfig
          
          recordMetric metric 3.0
          value2 <- metricValue metric
          
          shutdownTelemetry
          return (value1 == 5.0 && value2 == 8.0)
    
    it "should handle configuration field additions" $ property $
      \(seed :: Int) ->
        let baseConfig = TelemetryConfig "test" "1.0.0" True True True False
            -- 模拟添加新字段（在实际系统中，这可能是新版本的功能）
            enhancedConfig = TelemetryConfig "test" "2.0.0" True True True True
        in unsafePerformIO $ do
          -- 使用基础配置
          initTelemetry baseConfig
          
          metric <- createMetric "field-addition-test" "count"
          recordMetric metric 1.0
          
          value1 <- metricValue metric
          
          -- 切换到增强配置
          initTelemetry enhancedConfig
          
          recordMetric metric 2.0
          value2 <- metricValue metric
          
          shutdownTelemetry
          return (value1 == 1.0 && value2 == 3.0)
  
  -- 3. 测试API兼容性
  describe "API Compatibility" $ do
    it "should maintain metric API compatibility" $ do
      initTelemetry defaultConfig
      
      -- 测试所有度量API
      metric1 <- createMetric "api-test-1" "count"
      metric2 <- createMetricWithInitialValue "api-test-2" "count" 5.0
      
      recordMetric metric1 10.0
      recordMetric metric2 15.0
      
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      
      value1 `shouldBe` 10.0
      value2 `shouldBe` 20.0
      
      -- 测试安全API
      let unsafeValue = unsafeMetricValue metric1
      unsafeValue `shouldBe` 10.0
      
      shutdownTelemetry
    
    it "should maintain span API compatibility" $ do
      initTelemetry defaultConfig
      
      -- 测试所有span API
      span1 <- createSpan "api-span-1"
      spanIds <- createSpanWithIds "api-span-2"
      
      finishSpan span1
      
      -- 验证span属性
      spanName span1 `shouldBe` "api-span-1"
      
      -- createSpanWithIds返回ID对，而不是Span
      let (traceId, spanId) = spanIds
      not (Text.null traceId) `shouldBe` True
      not (Text.null spanId) `shouldBe` True
      
      -- 验证ID格式
      not (Text.null (spanTraceId span1)) `shouldBe` True
      not (Text.null (spanSpanId span1)) `shouldBe` True
      
      shutdownTelemetry
    
    it "should maintain logger API compatibility" $ do
      initTelemetry defaultConfig
      
      -- 测试所有logger API
      logger <- createLogger "api-logger" Info
      
      -- 测试所有日志级别
      logMessage logger Debug (pack "debug message") `shouldReturn` ()
      logMessage logger Info (pack "info message") `shouldReturn` ()
      logMessage logger Warn (pack "warning message") `shouldReturn` ()
      logMessage logger Error (pack "error message") `shouldReturn` ()
      
      -- 验证logger属性
      loggerName logger `shouldBe` "api-logger"
      loggerLevel logger `shouldBe` Info
      
      shutdownTelemetry
  
  -- 4. 测试数据格式兼容性
  describe "Data Format Compatibility" $ do
    it "should handle text encoding compatibility" $ do
      initTelemetry defaultConfig
      
      -- 测试各种文本格式
      let textFormats = 
            [ "ascii"
            , "UTF-8"
            , "中文"
            , "emoji🚀"
            , "mixed🌟content"
            ]
      
      forM_ textFormats $ \text -> do
        let packedText = pack text
        
        -- 度量名称和单位
        metric <- createMetric packedText packedText
        recordMetric metric 1.0
        
        metricName metric `shouldBe` packedText
        metricUnit metric `shouldBe` packedText
        
        -- Span名称
        span <- createSpan packedText
        spanName span `shouldBe` packedText
        
        -- Logger名称和消息
        logger <- createLogger packedText Info
        logMessage logger Info packedText
      
      shutdownTelemetry
    
    it "should handle numeric format compatibility" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "numeric-compatibility" "count"
      
      -- 测试各种数值格式
      let numericValues = 
            [ 0.0
            , -1.0
            , 1.0
            , 3.14159
            , 1.0e10
            , -1.0e-10
            , 1.0/0.0  -- 正无穷
            , -1.0/0.0 -- 负无穷
            , 0.0/0.0  -- NaN
            ]
      
      forM_ numericValues $ \value -> do
        recordMetric metric value
        current <- metricValue metric
        
        -- 验证特殊值的处理
        when (isNaN value) $ do
          isNaN current `shouldBe` True
        
        when (isInfinite value && value > 0) $ do
          isInfinite current `shouldBe` True
          current `shouldSatisfy` (> 0)
        
        when (isInfinite value && value < 0) $ do
          isInfinite current `shouldBe` True
          current `shouldSatisfy` (< 0)
      
      shutdownTelemetry
  
  -- 5. 测试跨版本数据迁移
  describe "Cross-Version Data Migration" $ do
    it "should preserve data across version upgrades" $ do
      -- 模拟旧版本
      let oldConfig = TelemetryConfig "migration-test" "1.0.0" True True True False
      
      initTelemetry oldConfig
      
      metric <- createMetric "migration-metric" "count"
      
      -- 在旧版本中记录数据
      sequence_ $ replicate 10 $ recordMetric metric 1.0
      
      value1 <- metricValue metric
      value1 `shouldBe` 10.0
      
      -- 模拟版本升级
      let newConfig = TelemetryConfig "migration-test" "2.0.0" True True True False
      
      initTelemetry newConfig
      
      -- 验证数据仍然存在
      recordMetric metric 5.0
      value2 <- metricValue metric
      value2 `shouldBe` 15.0
      
      shutdownTelemetry
    
    it "should handle configuration migration" $ do
      -- 测试配置迁移
      let oldConfigs = 
            [ TelemetryConfig "service-v1" "1.0.0" True True True False
            , TelemetryConfig "service-v1" "1.0.0" False True True False
            ]
      
      forM_ oldConfigs $ \oldConfig -> do
        initTelemetry oldConfig
        
        metric <- createMetric "config-migration" "count"
        recordMetric metric 1.0
        
        value1 <- metricValue metric
        
        -- 迁移到新配置
        let newConfig = TelemetryConfig "service-v2" "2.0.0" True True True False
        
        initTelemetry newConfig
        
        recordMetric metric 2.0
        value2 <- metricValue metric
        
        value1 `shouldBe` 1.0
        value2 `shouldBe` 3.0
        
        shutdownTelemetry
  
  -- 6. 测试第三方集成兼容性
  describe "Third-Party Integration Compatibility" $ do
    it "should maintain compatibility with external systems" $ do
      initTelemetry defaultConfig
      
      -- 模拟外部系统集成
      metric <- createMetric "external-integration" "count"
      
      -- 外部系统可能使用不同的命名约定
      let externalNames = 
            [ "external.system.metric"
            , "external-system-metric"
            , "externalSystemMetric"
            , "external_system_metric"
            ]
      
      forM_ externalNames $ \name -> do
        let packedName = pack name
        
        externalMetric <- createMetric packedName "count"
        recordMetric externalMetric 1.0
        
        metricName externalMetric `shouldBe` packedName
      
      shutdownTelemetry
    
    it "should handle protocol compatibility" $ do
      initTelemetry defaultConfig
      
      -- 模拟不同协议的兼容性
      let protocols = ["http", "https", "grpc", "websocket"]
      
      forM_ protocols $ \protocol -> do
        let metricName = pack $ protocol ++ "-requests"
            spanName = pack $ protocol ++ "-operation"
            loggerName = pack $ protocol ++ "-logger"
        
        metric <- createMetric metricName "count"
        recordMetric metric 1.0
        
        span <- createSpan spanName
        finishSpan span
        
        logger <- createLogger loggerName Info
        logMessage logger Info (pack $ protocol ++ " message")
      
      shutdownTelemetry
  
  -- 7. 测试平台兼容性
  describe "Platform Compatibility" $ do
    it "should handle different runtime environments" $ do
      -- 测试不同运行时环境的兼容性
      let environments = ["development", "testing", "staging", "production"]
      
      forM_ environments $ \env -> do
        let config = case env of
              "development" -> TelemetryConfig "dev-service" "1.0.0" True True True True
              "testing" -> TelemetryConfig "test-service" "1.0.0" True True True False
              "staging" -> TelemetryConfig "staging-service" "1.0.0" True True True False
              "production" -> productionConfig
              _ -> defaultConfig
        
        initTelemetry config
        
        metric <- createMetric (pack $ env ++ "-metric") "count"
        recordMetric metric 1.0
        
        value <- metricValue metric
        value `shouldBe` 1.0
        
        shutdownTelemetry
    
    it "should handle resource constraint compatibility" $ do
      -- 测试不同资源约束下的兼容性
      let resourceConfigs = 
            [ TelemetryConfig "low-resource" "1.0.0" True False True False
            , TelemetryConfig "medium-resource" "1.0.0" True True True False
            , TelemetryConfig "high-resource" "1.0.0" True True True True
            ]
      
      forM_ resourceConfigs $ \config -> do
        initTelemetry config
        
        metric <- createMetric "resource-test" "count"
        recordMetric metric 1.0
        
        value <- metricValue metric
        value `shouldBe` 1.0
        
        shutdownTelemetry
  
  -- 8. 测试向后兼容性保证
  describe "Backward Compatibility Guarantees" $ do
    it "should guarantee API stability" $ do
      initTelemetry defaultConfig
      
      -- 验证所有核心API仍然可用
      metric <- createMetric "stability-test" "count"
      recordMetric metric 42.0
      value <- metricValue metric
      value `shouldBe` 42.0
      
      span <- createSpan "stability-span"
      finishSpan span `shouldReturn` ()
      
      logger <- createLogger "stability-logger" Info
      logMessage logger Info (pack "stability test") `shouldReturn` ()
      
      shutdownTelemetry
    
    it "should maintain data format stability" $ do
      writeIORef enableMetricAggregation False
      writeIORef enableMetricSharing False
      initTelemetry defaultConfig
      
      -- 验证数据格式稳定性
      metric <- createMetric "format-stability" "count"
      
      let testValues = [1.0, -1.0, 0.0, 3.14159, 1.0e6]
      
      -- Create separate metrics for each value to avoid aggregation
      metrics <- mapM (\_ -> createMetric "format-stability" "count") testValues
      
      forM_ (zip testValues metrics) $ \(value, m) -> do
        recordMetric m value
        current <- metricValue m
        
        -- 验证数值精度
        when (not (isNaN value) && not (isInfinite value)) $ do
          current `shouldSatisfy` (\v -> abs (v - value) < 1.0e-10)
      
      shutdownTelemetry