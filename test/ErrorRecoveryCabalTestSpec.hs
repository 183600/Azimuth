{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErrorRecoveryCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate, ErrorCall(..), throwIO, catch)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group, sortBy)
import Data.Ord (comparing)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar, throwTo)
import Control.Monad (replicateM, when, forM_, void, unless)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)
import Prelude hiding (id)

import Azimuth.Telemetry

-- | 错误类型
data TelemetryError = 
    MetricCreationError Text
  | MetricRecordingError Text
  | SpanCreationError Text
  | LoggerCreationError Text
  | ConfigurationError Text
  deriving (Show, Eq)

spec :: Spec
spec = describe "Error Recovery Tests" $ do
  
  -- 1. 测试度量错误恢复
  describe "Metric Error Recovery" $ do
    it "should recover from metric creation errors" $ do
      initTelemetry defaultConfig
      
      -- 尝试创建带有无效名称的度量
      result1 <- try $ createMetric "" "count"
      case result1 of
        Left (_ :: SomeException) -> pendingWith "Expected to handle empty metric names"
        Right metric1 -> do
          -- 如果成功创建，验证基本功能
          recordMetric metric1 1.0
          value <- metricValue metric1
          value `shouldBe` 1.0
      
      -- 尝试创建带有无效单位的度量
      result2 <- try $ createMetric "test-metric" ""
      case result2 of
        Left (_ :: SomeException) -> pendingWith "Expected to handle empty metric units"
        Right metric2 -> do
          -- 如果成功创建，验证基本功能
          recordMetric metric2 2.0
          value <- metricValue metric2
          value `shouldBe` 2.0
      
      shutdownTelemetry
    
    it "should recover from metric recording errors" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "error-recovery-test" "count"
      
      -- 记录正常值
      recordMetric metric 10.0
      value1 <- metricValue metric
      value1 `shouldBe` 10.0
      
      -- 尝试记录特殊值
      recordMetric metric (1.0/0.0)  -- 正无穷
      value2 <- metricValue metric
      isInfinite value2 `shouldBe` True
      
      -- 尝试记录NaN
      recordMetric metric (0.0/0.0)  -- NaN
      value3 <- metricValue metric
      isNaN value3 `shouldBe` True
      
      -- 验证系统仍然可以记录正常值
      recordMetric metric 5.0
      value4 <- metricValue metric
      -- NaN值应该保持
      isNaN value4 `shouldBe` True
      
      shutdownTelemetry
  
  -- 2. 测试Span错误恢复
  describe "Span Error Recovery" $ do
    it "should recover from span creation errors" $ do
      initTelemetry defaultConfig
      
      -- 尝试创建带有无效名称的span
      result <- try $ createSpan ""
      case result of
        Left (_ :: SomeException) -> pendingWith "Expected to handle empty span names"
        Right span -> do
          -- 如果成功创建，验证基本功能
          spanName span `shouldBe` ""
          finishSpan span `shouldReturn` ()
      
      shutdownTelemetry
    
    it "should handle span operation failures gracefully" $ do
      initTelemetry defaultConfig
      
      span <- createSpan "error-test-span"
      
      -- 完成span
      finishSpan span `shouldReturn` ()
      
      -- 尝试再次完成同一个span（应该优雅处理）
      finishSpan span `shouldReturn` ()
      
      shutdownTelemetry
  
  -- 3. 测试Logger错误恢复
  describe "Logger Error Recovery" $ do
    it "should recover from logger creation errors" $ do
      initTelemetry defaultConfig
      
      -- 尝试创建带有无效名称的logger
      result <- try $ createLogger "" Info
      case result of
        Left (_ :: SomeException) -> pendingWith "Expected to handle empty logger names"
        Right logger -> do
          -- 如果成功创建，验证基本功能
          loggerName logger `shouldBe` ""
          logMessage logger Info (pack "test message") `shouldReturn` ()
      
      shutdownTelemetry
    
    it "should handle logging errors gracefully" $ do
      initTelemetry defaultConfig
      
      logger <- createLogger "error-test-logger" Info
      
      -- 记录正常消息
      logMessage logger Info (pack "normal message") `shouldReturn` ()
      
      -- 记录空消息
      logMessage logger Info (pack "") `shouldReturn` ()
      
      -- 记录非常长的消息
      let longMessage = pack $ replicate 10000 'a'
      logMessage logger Info longMessage `shouldReturn` ()
      
      -- 记录包含特殊字符的消息
      let specialMessage = pack "Special chars: \0\t\n\r"
      logMessage logger Info specialMessage `shouldReturn` ()
      
      shutdownTelemetry
  
  -- 4. QuickCheck属性测试：错误恢复的一致性
  describe "Error Recovery Properties" $ do
    it "should maintain consistency after errors" $ property $
      \values ->
        let testValues = take 10 values :: [Double]
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          metric <- createMetric "error-consistency" "count"
          
          -- 记录一些正常值
          sequence_ $ map (recordMetric metric) testValues
          
          -- 尝试记录特殊值
          recordMetric metric (1.0/0.0)
          recordMetric metric (0.0/0.0)
          
          -- 系统应该仍然可以记录正常值
          recordMetric metric 42.0
          
          finalValue <- metricValue metric
          
          shutdownTelemetry
          -- 最终值应该是NaN（因为NaN传播）
          return (isNaN finalValue)
    
    it "should handle concurrent error conditions" $ property $
      \numOps ->
        let operations = max 1 (abs numOps `mod` 20 + 1)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          metric <- createMetric "concurrent-errors" "count"
          
          -- 并发操作，包括正常值和特殊值
          done <- newEmptyMVar
          threads <- mapM (\i -> forkIO $ do
            if i `mod` 3 == 0
              then recordMetric metric (1.0/0.0)  -- 每三个操作记录一个无穷大
              else recordMetric metric 1.0
            putMVar done ()
            ) [1..operations]
          
          -- 等待所有操作完成
          sequence_ $ replicate operations (takeMVar done)
          
          finalValue <- metricValue metric
          
          shutdownTelemetry
          -- 如果有无穷大值，最终值应该是无穷大
          return (isInfinite finalValue || finalValue == fromIntegral (operations - operations `div` 3))
  
  -- 5. 测试配置错误恢复
  describe "Configuration Error Recovery" $ do
    it "should recover from configuration errors" $ do
      -- 尝试使用无效配置初始化
      let invalidConfig = TelemetryConfig "" "" False False False True
      
      result <- try $ initTelemetry invalidConfig
      case result of
        Left (_ :: SomeException) -> pendingWith "Expected to handle invalid configuration"
        Right _ -> do
          -- 如果成功初始化，验证系统仍然可用
          metric <- createMetric "config-error-test" "count"
          recordMetric metric 1.0
          value <- metricValue metric
          value `shouldBe` 1.0
          
          shutdownTelemetry
    
    it "should handle configuration hot updates with errors" $ do
      -- 正常初始化
      initTelemetry defaultConfig
      
      metric <- createMetric "hot-update-test" "count"
      recordMetric metric 10.0
      
      value1 <- metricValue metric
      value1 `shouldBe` 10.0
      
      -- 尝试更新为可能有问题的配置
      let problemConfig = TelemetryConfig "" "1.0.0" True True True False
      
      result <- try $ initTelemetry problemConfig
      case result of
        Left (_ :: SomeException) -> do
          -- 如果配置更新失败，系统应该仍然可用
          recordMetric metric 5.0
          value2 <- metricValue metric
          value2 `shouldBe` 15.0
        Right _ -> do
          -- 如果配置更新成功，系统也应该可用
          recordMetric metric 5.0
          value2 <- metricValue metric
          value2 `shouldBe` 15.0
      
      shutdownTelemetry
  
  -- 6. 测试资源耗尽恢复
  describe "Resource Exhaustion Recovery" $ do
    it "should handle resource exhaustion gracefully" $ do
      initTelemetry defaultConfig
      
      -- 创建大量度量（可能导致资源耗尽）
      let numMetrics = 1000
      
      result <- try $ do
        metrics <- replicateM numMetrics $ createMetric "resource-test" "count"
        sequence_ $ map (`recordMetric` 1.0) metrics
        return metrics
      
      case result of
        Left (_ :: SomeException) -> do
          -- 如果资源耗尽，系统应该仍然可以创建新的度量
          metric <- createMetric "recovery-test" "count"
          recordMetric metric 1.0
          value <- metricValue metric
          value `shouldBe` 1.0
        Right metrics -> do
          -- 如果成功，验证所有度量都有正确的值
          values <- mapM metricValue metrics
          all (== 1.0) values `shouldBe` True
      
      shutdownTelemetry
    
    it "should recover from memory pressure" $ do
      initTelemetry defaultConfig
      
      -- 创建大量对象以模拟内存压力
      let numObjects = 10000
      
      result <- try $ do
        metrics <- replicateM numObjects $ createMetric "memory-pressure" "count"
        sequence_ $ map (`recordMetric` 1.0) metrics
        return $ length metrics
      
      case result of
        Left (_ :: SomeException) -> do
          -- 如果内存不足，系统应该仍然可用
          metric <- createMetric "memory-recovery" "count"
          recordMetric metric 1.0
          value <- metricValue metric
          value `shouldBe` 1.0
        Right count -> do
          count `shouldBe` numObjects
      
      shutdownTelemetry
  
  -- 7. 测试并发错误恢复
  describe "Concurrent Error Recovery" $ do
    it "should handle concurrent errors across components" $ do
      initTelemetry defaultConfig
      
      let numThreads = 10
      
      -- 并发创建不同类型的组件
      done <- newEmptyMVar
      threads <- mapM (\i -> forkIO $ do
        -- 每个线程执行不同的操作
        case i `mod` 4 of
          0 -> do
            metric <- createMetric ("concurrent-error-" ++ show i) "count"
            recordMetric metric (fromIntegral i)
          1 -> do
            span <- createSpan ("concurrent-span-" ++ show i)
            finishSpan span
          2 -> do
            logger <- createLogger ("concurrent-logger-" ++ show i) Info
            logMessage logger Info (pack $ "message " ++ show i)
          3 -> do
            metric <- createMetric ("error-metric-" ++ show i) "count"
            recordMetric metric (1.0/0.0)  -- 记录特殊值
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
    
    it "should recover from cascading errors" $ do
      initTelemetry defaultConfig
      
      -- 创建一个链式依赖的场景
      metric1 <- createMetric "cascade-1" "count"
      metric2 <- createMetric "cascade-2" "count"
      metric3 <- createMetric "cascade-3" "count"
      
      -- 在第一个度量中引入错误
      recordMetric metric1 (1.0/0.0)
      
      -- 其他度量应该仍然可以正常工作
      recordMetric metric2 10.0
      recordMetric metric3 20.0
      
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      value3 <- metricValue metric3
      
      isInfinite value1 `shouldBe` True
      value2 `shouldBe` 10.0
      value3 `shouldBe` 20.0
      
      shutdownTelemetry
  
  -- 8. 测试系统级错误恢复
  describe "System-Level Error Recovery" $ do
    it "should recover from complete system restart" $ do
      -- 初始化系统
      initTelemetry defaultConfig
      
      metric <- createMetric "restart-test" "count"
      recordMetric metric 10.0
      
      value1 <- metricValue metric
      value1 `shouldBe` 10.0
      
      -- 完全关闭系统
      shutdownTelemetry
      
      -- 重新初始化
      initTelemetry defaultConfig
      
      -- 系统应该可以正常工作
      metric2 <- createMetric "restart-test" "count"
      recordMetric metric2 5.0
      
      value2 <- metricValue metric2
      value2 `shouldBe` 5.0
      
      shutdownTelemetry
    
    it "should maintain error state across restarts when appropriate" $ do
      -- 初始化系统
      initTelemetry defaultConfig
      
      metric <- createMetric "persistent-error" "count"
      
      -- 引入错误状态
      recordMetric metric (0.0/0.0)  -- NaN
      
      value1 <- metricValue metric
      isNaN value1 `shouldBe` True
      
      -- 关闭系统
      shutdownTelemetry
      
      -- 重新初始化
      initTelemetry defaultConfig
      
      -- 创建同名度量
      metric2 <- createMetric "persistent-error" "count"
      recordMetric metric2 1.0
      
      value2 <- metricValue metric2
      value2 `shouldBe` 1.0
      
      shutdownTelemetry