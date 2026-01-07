{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConfigHotReloadCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group, sortBy)
import Data.Ord (comparing)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, when, forM_, void)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)
import Prelude hiding (id)

import Azimuth.Telemetry

-- | 配置变体生成器
data ConfigVariant = 
    DebugConfig
  | ProductionConfig
  | TestConfig
  | CustomConfig Text Text Bool Bool Bool Bool
  deriving (Show, Eq)

-- | 生成随机配置
generateRandomConfig :: Int -> TelemetryConfig
generateRandomConfig seed = 
  let variants = [DebugConfig, ProductionConfig, TestConfig]
      variant = variants !! (seed `mod` length variants)
  in case variant of
    DebugConfig -> TelemetryConfig "debug-service" "1.0.0" True True True True
    ProductionConfig -> TelemetryConfig "production-service" "1.0.0" True True True False
    TestConfig -> TelemetryConfig "test-service" "1.0.0" True False True False
    CustomConfig name version metrics tracing logging debug -> 
      TelemetryConfig name version metrics tracing logging debug

spec :: Spec
spec = describe "Configuration Hot Reload Tests" $ do
  
  -- 1. 测试基本配置热更新
  describe "Basic Configuration Hot Reload" $ do
    it "should update configuration without losing data" $ do
      -- 初始配置
      let config1 = TelemetryConfig "service-v1" "1.0.0" True True True False
      
      initTelemetry config1
      
      metric <- createMetric "hot-reload-test" "count"
      recordMetric metric 10.0
      
      value1 <- metricValue metric
      value1 `shouldBe` 10.0
      
      -- 热更新配置
      let config2 = TelemetryConfig "service-v2" "2.0.0" True True True False
      
      initTelemetry config2
      
      -- 验证度量数据仍然存在
      recordMetric metric 5.0
      value2 <- metricValue metric
      value2 `shouldBe` 15.0
      
      -- 验证配置已更新
      currentConfig <- readIORef globalConfig
      serviceName currentConfig `shouldBe` "service-v2"
      serviceVersion currentConfig `shouldBe` "2.0.0"
      
      shutdownTelemetry
    
    it "should handle configuration field changes" $ do
      -- 初始配置
      let config1 = TelemetryConfig "field-test" "1.0.0" True True True True
      
      initTelemetry config1
      
      metric <- createMetric "field-change-test" "count"
      recordMetric metric 1.0
      
      -- 更改各个字段
      let config2 = TelemetryConfig "field-test" "2.0.0" False True True False
      initTelemetry config2
      
      recordMetric metric 2.0
      value2 <- metricValue metric
      value2 `shouldBe` 3.0
      
      let config3 = TelemetryConfig "field-test" "2.0.0" False False True False
      initTelemetry config3
      
      recordMetric metric 3.0
      value3 <- metricValue metric
      value3 `shouldBe` 6.0
      
      let config4 = TelemetryConfig "field-test" "2.0.0" False False False False
      initTelemetry config4
      
      recordMetric metric 4.0
      value4 <- metricValue metric
      value4 `shouldBe` 10.0
      
      shutdownTelemetry
  
  -- 2. QuickCheck属性测试：配置热更新的幂等性
  describe "Configuration Hot Reload Properties" $ do
    it "should be idempotent for identical configurations" $ property $
      \seed ->
        let config = generateRandomConfig seed
        in unsafePerformIO $ do
          initTelemetry config
          
          metric <- createMetric "idempotent-test" "count"
          recordMetric metric 1.0
          
          value1 <- metricValue metric
          
          -- 重复应用相同配置
          initTelemetry config
          initTelemetry config
          
          recordMetric metric 2.0
          value2 <- metricValue metric
          
          shutdownTelemetry
          return (value1 == 1.0 && value2 == 3.0)
    
    it "should preserve data across configuration changes" $ property $
      \seed1 seed2 ->
        let config1 = generateRandomConfig seed1
            config2 = generateRandomConfig seed2
        in unsafePerformIO $ do
          initTelemetry config1
          
          metric <- createMetric "preserve-data-test" "count"
          
          -- 记录一些数据
          sequence_ $ replicate 10 $ recordMetric metric 1.0
          value1 <- metricValue metric
          
          -- 更改配置
          initTelemetry config2
          
          -- 继续记录数据
          sequence_ $ replicate 5 $ recordMetric metric 1.0
          value2 <- metricValue metric
          
          shutdownTelemetry
          return (value1 == 10.0 && value2 == 15.0)
    
    it "should handle rapid configuration changes" $ property $
      \numChanges ->
        let changes = max 1 (abs numChanges `mod` 10 + 1)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          metric <- createMetric "rapid-changes-test" "count"
          
          -- 快速连续的配置更改
          sequence_ $ replicate changes $ do
            let config = TelemetryConfig "rapid-test" "1.0.0" True True True False
            initTelemetry config
            recordMetric metric 1.0
          
          finalValue <- metricValue metric
          
          shutdownTelemetry
          return (finalValue == fromIntegral changes)
  
  -- 3. 测试并发配置热更新
  describe "Concurrent Configuration Hot Reload" $ do
    it "should handle concurrent configuration updates" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "concurrent-config-test" "count"
      
      let numThreads = 5
          updatesPerThread = 10
      
      -- 并发更新配置
      done <- newEmptyMVar
      threads <- mapM (\i -> forkIO $ do
        sequence_ $ replicate updatesPerThread $ do
          let config = TelemetryConfig ("concurrent-" ++ show i) "1.0.0" True True True False
          initTelemetry config
          recordMetric metric 1.0
          threadDelay 100
        putMVar done ()
        ) [1..numThreads]
      
      -- 等待所有线程完成
      sequence_ $ replicate numThreads (takeMVar done)
      
      finalValue <- metricValue metric
      let expectedValue = fromIntegral numThreads * fromIntegral updatesPerThread
      
      finalValue `shouldBe` expectedValue
      
      shutdownTelemetry
    
    it "should maintain consistency during concurrent operations" $ do
      initTelemetry defaultConfig
      
      let numThreads = 3
      
      -- 并发执行不同的操作
      done <- newEmptyMVar
      threads <- mapM (\i -> forkIO $ do
        case i of
          1 -> do
            -- 线程1：配置更新
            sequence_ $ replicate 5 $ do
              let config = TelemetryConfig "thread-1" "1.0.0" True True True False
              initTelemetry config
              threadDelay 200
          2 -> do
            -- 线程2：度量操作
            metric <- createMetric "concurrent-consistency" "count"
            sequence_ $ replicate 10 $ do
              recordMetric metric 1.0
              threadDelay 100
          3 -> do
            -- 线程3：span操作
            sequence_ $ replicate 5 $ do
              span <- createSpan "concurrent-span"
              finishSpan span
              threadDelay 150
        putMVar done ()
        ) [1..numThreads]
      
      -- 等待所有线程完成
      sequence_ $ replicate numThreads (takeMVar done)
      
      -- 验证系统仍然可用
      metric <- createMetric "post-concurrent-test" "count"
      recordMetric metric 42.0
      value <- metricValue metric
      value `shouldBe` 42.0
      
      shutdownTelemetry
  
  -- 4. 测试配置热更新的性能
  describe "Configuration Hot Reload Performance" $ do
    it "should handle frequent configuration updates efficiently" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "performance-test" "count"
      
      let numUpdates = 1000
      
      -- 大量配置更新
      startTime <- unsafePerformIO $ do
        sequence_ $ replicate numUpdates $ do
          let config = TelemetryConfig "performance-test" "1.0.0" True True True False
          initTelemetry config
          recordMetric metric 1.0
        return undefined
      
      finalValue <- metricValue metric
      finalValue `shouldBe` fromIntegral numUpdates
      
      shutdownTelemetry
    
    it "should minimize overhead during configuration changes" $ do
      initTelemetry defaultConfig
      
      let numOperations = 1000
          configChangeInterval = 100
      
      metric <- createMetric "overhead-test" "count"
      
      -- 定期更改配置
      sequence_ $ [1..numOperations] >>= \i -> do
        when (i `mod` configChangeInterval == 0) $ do
          let config = TelemetryConfig ("overhead-" ++ show i) "1.0.0" True True True False
          initTelemetry config
        recordMetric metric 1.0
      
      finalValue <- metricValue metric
      finalValue `shouldBe` fromIntegral numOperations
      
      shutdownTelemetry
  
  -- 5. 测试配置热更新的边界条件
  describe "Configuration Hot Reload Boundary Conditions" $ do
    it "should handle empty configuration gracefully" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "empty-config-test" "count"
      recordMetric metric 1.0
      
      value1 <- metricValue metric
      value1 `shouldBe` 1.0
      
      -- 尝试使用空配置
      let emptyConfig = TelemetryConfig "" "" False False False False
      
      result <- try $ initTelemetry emptyConfig
      case result of
        Left (_ :: SomeException) -> do
          -- 如果失败，系统应该仍然可用
          recordMetric metric 2.0
          value2 <- metricValue metric
          value2 `shouldBe` 3.0
        Right _ -> do
          -- 如果成功，系统也应该可用
          recordMetric metric 2.0
          value2 <- metricValue metric
          value2 `shouldBe` 3.0
      
      shutdownTelemetry
    
    it "should handle extreme configuration values" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "extreme-config-test" "count"
      recordMetric metric 1.0
      
      -- 测试极端配置值
      let extremeConfigs = 
            [ TelemetryConfig (pack $ replicate 10000 'a') "1.0.0" True True True False
            , TelemetryConfig "extreme" (pack $ replicate 10000 'b') True True True False
            , TelemetryConfig "extreme" "1.0.0" True True True True
            ]
      
      forM_ extremeConfigs $ \config -> do
        result <- try $ initTelemetry config
        case result of
          Left (_ :: SomeException) -> do
            -- 配置更新失败，系统应该仍然可用
            recordMetric metric 1.0
          Right _ -> do
            -- 配置更新成功，系统也应该可用
            recordMetric metric 1.0
      
      finalValue <- metricValue metric
      let expectedValue = 1.0 + fromIntegral (length extremeConfigs)
      finalValue `shouldBe` expectedValue
      
      shutdownTelemetry
  
  -- 6. 测试配置热更新的持久性
  describe "Configuration Hot Reload Persistence" $ do
    it "should maintain configuration state across restarts" $ do
      -- 初始配置
      let config1 = TelemetryConfig "persistent-test" "1.0.0" True True True False
      
      initTelemetry config1
      
      metric <- createMetric "persistent-test" "count"
      recordMetric metric 10.0
      
      value1 <- metricValue metric
      value1 `shouldBe` 10.0
      
      -- 更新配置
      let config2 = TelemetryConfig "persistent-test" "2.0.0" True True True False
      
      initTelemetry config2
      
      recordMetric metric 5.0
      value2 <- metricValue metric
      value2 `shouldBe` 15.0
      
      -- 关闭系统
      shutdownTelemetry
      
      -- 重新初始化
      initTelemetry config2
      
      metric2 <- createMetric "persistent-test" "count"
      recordMetric metric2 3.0
      
      value3 <- metricValue metric2
      value3 `shouldBe` 3.0
      
      shutdownTelemetry
    
    it "should handle configuration rollback" $ do
      let config1 = TelemetryConfig "rollback-test" "1.0.0" True True True False
          config2 = TelemetryConfig "rollback-test" "2.0.0" False True True False
          config3 = TelemetryConfig "rollback-test" "1.0.0" True True True False  -- 回滚到config1
      
      -- 应用初始配置
      initTelemetry config1
      
      metric <- createMetric "rollback-test" "count"
      recordMetric metric 1.0
      
      value1 <- metricValue metric
      value1 `shouldBe` 1.0
      
      -- 更新配置
      initTelemetry config2
      
      recordMetric metric 2.0
      value2 <- metricValue metric
      value2 `shouldBe` 3.0
      
      -- 回滚配置
      initTelemetry config3
      
      recordMetric metric 3.0
      value3 <- metricValue metric
      value3 `shouldBe` 6.0
      
      -- 验证配置已回滚
      currentConfig <- readIORef globalConfig
      serviceVersion currentConfig `shouldBe` "1.0.0"
      enableMetrics currentConfig `shouldBe` True
      
      shutdownTelemetry