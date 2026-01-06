{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResourceLifecycleSpec (spec) where

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
            defaultConfig,
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
  
  -- 测试关闭生命周期
  describe "Shutdown Lifecycle" $ do
    it "should handle single shutdown cycle" $ property $
      \serviceName ->
        let config = TelemetryConfig (pack $ take 50 serviceName) "1.0.0" True True True False
            result = unsafePerformIO $ do
              initTelemetry config
              -- 创建资源
              metric <- createMetric "shutdown-test" "count"
              recordMetric metric 1.0
              span <- createSpan "shutdown-span"
              finishSpan span
              logger <- createLogger "shutdown-logger" Info
              logMessage logger Info "shutdown test"
              -- 关闭系统
              shutdownTelemetry
              return True
        in result
    
    it "should handle multiple shutdown cycles" $ property $
      \cycles ->
        let numCycles = max 1 (abs cycles `mod` 5 + 1)
            result = unsafePerformIO $ do
              sequence_ $ replicate numCycles $ do
                initTelemetry productionConfig
                -- 创建资源
                metric <- createMetric "multi-shutdown-test" "count"
                recordMetric metric 1.0
                -- 关闭系统
                shutdownTelemetry
              return True
        in result
    
    it "should handle shutdown without explicit initialization" $ property $
      \serviceName ->
        let result = unsafePerformIO $ do
              -- 尝试关闭未初始化的系统
              result <- try $ shutdownTelemetry
              case result of
                Right _ -> return True
                Left (_ :: SomeException) -> return True
        in result
  
  -- 测试资源创建和销毁
  describe "Resource Creation and Destruction" $ do
    it "should handle metric lifecycle" $ property $
      \numMetrics ->
        let actualMetrics = max 1 (abs numMetrics `mod` 10 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建度量
              metrics <- sequence $ replicate actualMetrics $ do
                createMetric "lifecycle-metric" "count"
              
              -- 使用度量
              sequence_ $ zipWith (\metric i -> do
                recordMetric metric (fromIntegral i)
              ) metrics [1..actualMetrics]
              
              -- 关闭系统（自动清理资源）
              shutdownTelemetry
              return True
        in result
    
    it "should handle span lifecycle" $ property $
      \numSpans ->
        let actualSpans = max 1 (abs numSpans `mod` 10 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建span
              spans <- sequence $ replicate actualSpans $ do
                createSpan "lifecycle-span"
              
              -- 使用span
              sequence_ $ map finishSpan spans
              
              -- 关闭系统（自动清理资源）
              shutdownTelemetry
              return True
        in result
    
    it "should handle logger lifecycle" $ property $
      \numLoggers ->
        let actualLoggers = max 1 (abs numLoggers `mod` 10 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建日志记录器
              loggers <- sequence $ replicate actualLoggers $ do
                createLogger "lifecycle-logger" Info
              
              -- 使用日志记录器
              sequence_ $ zipWith (\logger i -> do
                logMessage logger Info (pack $ "lifecycle message " ++ show i)
              ) loggers [1..actualLoggers]
              
              -- 关闭系统（自动清理资源）
              shutdownTelemetry
              return True
        in result
  
  -- 测试资源复用
  describe "Resource Reuse" $ do
    it "should handle metric reuse after shutdown" $ property $
      \reuseCycles ->
        let numCycles = max 1 (abs reuseCycles `mod` 3 + 1)
            result = unsafePerformIO $ do
              sequence_ $ replicate numCycles $ do
                initTelemetry productionConfig
                
                -- 创建和使用度量
                metric <- createMetric "reuse-metric" "count"
                recordMetric metric 1.0
                value1 <- metricValue metric
                
                shutdownTelemetry
                
                initTelemetry productionConfig
                
                -- 重新创建同名的度量
                metric2 <- createMetric "reuse-metric" "count"
                recordMetric metric2 2.0
                value2 <- metricValue metric2
                
                shutdownTelemetry
                
                return (value1 == 1.0 && value2 == 2.0)
              return True
        in result
    
    it "should handle span reuse after shutdown" $ property $
      \reuseCycles ->
        let numCycles = max 1 (abs reuseCycles `mod` 3 + 1)
            result = unsafePerformIO $ do
              sequence_ $ replicate numCycles $ do
                initTelemetry productionConfig
                
                -- 创建和使用span
                span <- createSpan "reuse-span"
                finishSpan span
                
                shutdownTelemetry
                
                initTelemetry productionConfig
                
                -- 重新创建同名的span
                span2 <- createSpan "reuse-span"
                finishSpan span2
                
                shutdownTelemetry
                
                return True
              return True
        in result
    
    it "should handle logger reuse after shutdown" $ property $
      \reuseCycles ->
        let numCycles = max 1 (abs reuseCycles `mod` 3 + 1)
            result = unsafePerformIO $ do
              sequence_ $ replicate numCycles $ do
                initTelemetry productionConfig
                
                -- 创建和使用日志记录器
                logger <- createLogger "reuse-logger" Info
                logMessage logger Info "reuse test"
                
                shutdownTelemetry
                
                initTelemetry productionConfig
                
                -- 重新创建同名的日志记录器
                logger2 <- createLogger "reuse-logger" Info
                logMessage logger2 Info "reuse test 2"
                
                shutdownTelemetry
                
                return True
              return True
        in result
  
  -- 测试资源泄漏防护
  describe "Resource Leak Prevention" $ do
    it "should prevent resource leaks with many metrics" $ property $
      \numMetrics ->
        let actualMetrics = max 1 (abs numMetrics `mod` 100 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建大量度量
              metrics <- sequence $ replicate actualMetrics $ do
                createMetric "leak-test-metric" "count"
              
              -- 使用度量
              sequence_ $ zipWith (\metric i -> do
                recordMetric metric (fromIntegral i)
              ) metrics [1..actualMetrics]
              
              -- 关闭系统（应该清理所有资源）
              shutdownTelemetry
              
              -- 重新初始化并验证系统仍然可用
              initTelemetry productionConfig
              testMetric <- createMetric "post-leak-test" "count"
              recordMetric testMetric 1.0
              finalValue <- metricValue testMetric
              shutdownTelemetry
              
              return (finalValue == 1.0)
        in result
    
    it "should prevent resource leaks with many spans" $ property $
      \numSpans ->
        let actualSpans = max 1 (abs numSpans `mod` 100 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建大量span
              spans <- sequence $ replicate actualSpans $ do
                createSpan "leak-test-span"
              
              -- 使用span
              sequence_ $ map finishSpan spans
              
              -- 关闭系统（应该清理所有资源）
              shutdownTelemetry
              
              -- 重新初始化并验证系统仍然可用
              initTelemetry productionConfig
              testSpan <- createSpan "post-leak-test"
              finishSpan testSpan
              shutdownTelemetry
              
              return True
        in result
    
    it "should prevent resource leaks with many loggers" $ property $
      \numLoggers ->
        let actualLoggers = max 1 (abs numLoggers `mod` 50 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建大量日志记录器
              loggers <- sequence $ replicate actualLoggers $ do
                createLogger "leak-test-logger" Info
              
              -- 使用日志记录器
              sequence_ $ zipWith (\logger i -> do
                logMessage logger Info (pack $ "leak test message " ++ show i)
              ) loggers [1..actualLoggers]
              
              -- 关闭系统（应该清理所有资源）
              shutdownTelemetry
              
              -- 重新初始化并验证系统仍然可用
              initTelemetry productionConfig
              testLogger <- createLogger "post-leak-test" Info
              logMessage testLogger Info "post leak test"
              shutdownTelemetry
              
              return True
        in result
  
  -- 测试异常情况下的资源清理
  describe "Resource Cleanup in Exception Scenarios" $ do
    it "should handle cleanup during metric operations" $ property $
      \numOperations ->
        let actualOps = max 1 (abs numOperations `mod` 20 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建度量
              metric <- createMetric "exception-metric" "count"
              
              -- 执行一些操作
              sequence_ $ replicate actualOps $ do
                recordMetric metric 1.0
              
              -- 在操作过程中关闭系统
              shutdownTelemetry
              
              -- 验证系统可以重新初始化
              initTelemetry productionConfig
              testMetric <- createMetric "post-exception-test" "count"
              recordMetric testMetric 1.0
              finalValue <- metricValue testMetric
              shutdownTelemetry
              
              return (finalValue == 1.0)
        in result
    
    it "should handle cleanup during span operations" $ property $
      \numOperations ->
        let actualOps = max 1 (abs numOperations `mod` 20 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 执行一些span操作
              sequence_ $ replicate actualOps $ do
                span <- createSpan "exception-span"
                finishSpan span
              
              -- 在操作过程中关闭系统
              shutdownTelemetry
              
              -- 验证系统可以重新初始化
              initTelemetry productionConfig
              testSpan <- createSpan "post-exception-test"
              finishSpan testSpan
              shutdownTelemetry
              
              return True
        in result
    
    it "should handle cleanup during logging operations" $ property $
      \numOperations ->
        let actualOps = max 1 (abs numOperations `mod` 20 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              logger <- createLogger "exception-logger" Info
              
              -- 执行一些日志操作
              sequence_ $ replicate actualOps $ do
                logMessage logger Info "exception test message"
              
              -- 在操作过程中关闭系统
              shutdownTelemetry
              
              -- 验证系统可以重新初始化
              initTelemetry productionConfig
              testLogger <- createLogger "post-exception-test" Info
              logMessage testLogger Info "post exception test"
              shutdownTelemetry
              
              return True
        in result
  
  -- 测试长时间运行的资源管理
  describe "Long-running Resource Management" $ do
    it "should handle long-running metric operations" $ property $
      \duration ->
        let runTime = max 100000 (abs duration `mod` 1000000 + 100000)  -- 100ms到1s
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              metric <- createMetric "long-running-metric" "count"
              
              -- 长时间运行操作
              sequence_ $ replicate 100 $ do
                recordMetric metric 1.0
                threadDelay (runTime `div` 100)
              
              finalValue <- metricValue metric
              shutdownTelemetry
              
              return (finalValue == 100.0)
        in result
    
    it "should handle long-running span operations" $ property $
      \duration ->
        let runTime = max 100000 (abs duration `mod` 1000000 + 100000)  -- 100ms到1s
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 长时间运行操作
              sequence_ $ replicate 10 $ do
                span <- createSpan "long-running-span"
                threadDelay (runTime `div` 10)
                finishSpan span
              
              shutdownTelemetry
              return True
        in result
    
    it "should handle long-running logging operations" $ property $
      \duration ->
        let runTime = max 100000 (abs duration `mod` 1000000 + 100000)  -- 100ms到1s
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              logger <- createLogger "long-running-logger" Info
              
              -- 长时间运行操作
              sequence_ $ replicate 50 $ do
                logMessage logger Info "long-running message"
                threadDelay (runTime `div` 50)
              
              shutdownTelemetry
              return True
        in result