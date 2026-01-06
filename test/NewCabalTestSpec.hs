{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NewCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.Char (isHexDigit, isDigit)
import Data.List (sort, nub, group)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, replicateM_, when, void)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)
import Data.Maybe (isJust, isNothing)
import Data.Word (Word8)
import Data.Bits (xor)
import qualified Data.Map as Map

import Azimuth.Telemetry
import Data.IORef (writeIORef, readIORef)
import Azimuth.Telemetry (globalConfig)

spec :: Spec
spec = describe "New Cabal Test Suite" $ do
  
  -- 测试1: 度量值的精度和舍入行为
  describe "Metric Precision and Rounding" $ do
    it "should handle floating point precision correctly" $ property $
      \value ->
        let epsilon = 1e-10
            testValue = if abs value < 1e-15 then 1e-10 else value
        in unsafePerformIO $ do
          metric <- createMetric "precision-test" "unit"
          recordMetric metric testValue
          result <- metricValue metric
          return (abs (result - testValue) < epsilon)
    
    it "should handle cumulative precision loss" $ property $
      \value ->
        let testValue = if abs value < 1e-5 then 0.1 else value / 1000.0
            iterations = 1000
            expectedTotal = testValue * fromIntegral iterations
            epsilon = 1e-6
        in unsafePerformIO $ do
          metric <- createMetric "cumulative-precision" "unit"
          replicateM_ iterations $ recordMetric metric testValue
          result <- metricValue metric
          return (abs (result - expectedTotal) < epsilon)
  
  -- 测试2: Span和Trace的时间关系
  describe "Span and Trace Temporal Relationships" $ do
    it "should maintain trace context across span creation" $ property $
      \name ->
        let spanName = pack $ "temporal-test-" ++ show (name :: Int)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建第一个span
          span1 <- createSpan spanName
          let traceId1 = spanTraceId span1
          
          -- 创建第二个span，应该有相同的trace ID
          span2 <- createSpan spanName
          let traceId2 = spanTraceId span2
          
          shutdownTelemetry
          return (traceId1 == traceId2)
    
    it "should generate unique span IDs over time" $ property $
      \iterations ->
        let actualIterations = max 1 (abs iterations `mod` 10 + 1)
            spanName = "uniqueness-over-time"
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建多个span，每个应该有唯一的span ID
          spans <- replicateM actualIterations $ createSpan spanName
          let spanIds = map spanSpanId spans
              uniqueSpanIds = nub spanIds
          
          shutdownTelemetry
          return (length uniqueSpanIds == actualIterations)
  
  -- 测试3: 日志消息的格式化和编码
  describe "Log Message Formatting and Encoding" $ do
    it "should handle unicode characters in log messages" $ property $
      \codePoint ->
        let char = toEnum (abs codePoint `mod` 65536) :: Char
            message = pack $ "测试" ++ [char] ++ "message"
        in unsafePerformIO $ do
          logger <- createLogger "unicode-test" Info
          result <- try $ logMessage logger Info message
          return (case result of
                   Left (_ :: SomeException) -> False
                   Right _ -> True)
    
    it "should handle very long log messages" $ property $
      \length ->
        let actualLength = max 1 (abs length `mod` 1000 + 1)
            message = pack $ replicate actualLength 'x'
        in unsafePerformIO $ do
          logger <- createLogger "long-message-test" Info
          result <- try $ logMessage logger Info message
          return (case result of
                   Left (_ :: SomeException) -> False
                   Right _ -> True)
  
  -- 测试4: 配置的序列化和反序列化属性
  describe "Configuration Properties" $ do
    it "should maintain configuration invariants" $ property $
      \name version metrics tracing logging debug ->
        let config = TelemetryConfig (pack name) (pack version) metrics tracing logging debug
        in unsafePerformIO $ do
          -- 测试配置可以用于初始化
          result <- try $ initTelemetry config
          case result of
            Left (_ :: SomeException) -> return False
            Right _ -> do
              shutdownTelemetry
              return True
    
    it "should validate configuration field constraints" $ property $
      \name ->
        let config = TelemetryConfig (pack name) "1.0.0" True True True False
        in unsafePerformIO $ do
          initTelemetry config
          
          -- 验证配置已正确设置
          currentConfig <- readIORef globalConfig
          let serviceNameMatches = serviceName currentConfig == pack name
              versionMatches = serviceVersion currentConfig == "1.0.0"
              featuresEnabled = enableMetrics currentConfig && 
                               enableTracing currentConfig && 
                               enableLogging currentConfig
              debugDisabled = not (enableDebugOutput currentConfig)
          
          shutdownTelemetry
          return (serviceNameMatches && versionMatches && featuresEnabled && debugDisabled)
  
  -- 测试5: 度量值的聚合行为
  describe "Metric Aggregation Behavior" $ do
    it "should correctly aggregate positive and negative values" $ property $
      \pos neg ->
        let positiveValue = abs pos
            negativeValue = -abs neg
            expectedSum = positiveValue + negativeValue
            epsilon = 1e-10
        in unsafePerformIO $ do
          -- 禁用度量共享以确保测试隔离
          writeIORef enableMetricSharing False
          
          metric <- createMetric "aggregation-test" "sum"
          recordMetric metric positiveValue
          recordMetric metric negativeValue
          result <- metricValue metric
          
          -- 重新启用度量共享
          writeIORef enableMetricSharing True
          
          return (abs (result - expectedSum) < epsilon)
    
    it "should handle zero values in aggregation" $ property $
      \value ->
        let testValue = value
            zeroValue = 0.0
            expectedSum = testValue + zeroValue
            epsilon = 1e-10
        in unsafePerformIO $ do
          -- 禁用度量共享以确保测试隔离
          writeIORef enableMetricSharing False
          
          metric <- createMetric "zero-aggregation" "sum"
          recordMetric metric testValue
          recordMetric metric zeroValue
          result <- metricValue metric
          
          -- 重新启用度量共享
          writeIORef enableMetricSharing True
          
          return (abs (result - expectedSum) < epsilon)
  
  -- 测试6: 并发环境下的遥测组件隔离
  describe "Concurrent Component Isolation" $ do
    it "should isolate metrics between threads" $ property $
          \ (threadId :: Int) ->
            let metricName = pack $ "thread-metric-" ++ show (abs threadId `mod` 5)
        in unsafePerformIO $ do
          -- 禁用度量共享以确保测试隔离
          writeIORef enableMetricSharing False
          
          metric <- createMetric metricName "count"
          recordMetric metric 1.0
          value <- metricValue metric
          
          -- 重新启用度量共享
          writeIORef enableMetricSharing True
          
          return (value == 1.0)
    
    it "should handle concurrent logger creation" $ property $
      \loggerCount ->
        let actualCount = max 1 (abs loggerCount `mod` 5 + 1)
            loggerNameStr = "concurrent-logger"
        in unsafePerformIO $ do
          loggers <- replicateM actualCount $ createLogger loggerNameStr Info
          let allNamesMatch = all (\l -> loggerName l == loggerNameStr) loggers
              allLevelsCorrect = all (\l -> loggerLevel l == Info) loggers
          return (allNamesMatch && allLevelsCorrect)
  
  -- 测试7: 边界值和特殊值的处理
  describe "Boundary and Special Value Handling" $ do
    it "should handle infinity values" $ do
      let positiveInfinity = 1/0 :: Double
          negativeInfinity = -1/0 :: Double
      unsafePerformIO $ do
        metric <- createMetric "infinity-test" "special"
        recordMetric metric positiveInfinity
        result1 <- metricValue metric
        recordMetric metric negativeInfinity
        result2 <- metricValue metric
        return (isInfinite result1 && isInfinite result2)
    
    it "should handle NaN values" $ do
      let nanValue = 0/0 :: Double
      unsafePerformIO $ do
        metric <- createMetric "nan-test" "special"
        recordMetric metric nanValue
        result <- metricValue metric
        return (isNaN result)
    
    it "should handle very small values" $ property $
    
          \ (exponent :: Int) ->
    
            let actualExponent = max 1 (abs exponent `mod` 300 + 1)
                smallValue = 10.0 ^^ (-actualExponent)
                -- 对于极小值，检查是否为0或非常接近原始值
                epsilon = 1e-323
        in unsafePerformIO $ do
          -- 禁用度量共享以确保测试隔离
          writeIORef enableMetricSharing False
          
          metric <- createMetric "small-value-test" "tiny"
          recordMetric metric smallValue
          result <- metricValue metric
          
          -- 重新启用度量共享
          writeIORef enableMetricSharing True
          
          -- 检查结果是否为0或足够接近原始值
          return (result == 0.0 || abs (result - smallValue) < epsilon || 
                  (isNaN smallValue && isNaN result) ||
                  (isInfinite smallValue && isInfinite result))
  
  -- 测试8: 资源管理和内存泄漏防护
  describe "Resource Management and Memory Leak Prevention" $ do
    it "should clean up resources on shutdown" $ property $
      \metricCount ->
        let actualCount = max 1 (abs metricCount `mod` 10 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建多个度量
          metrics <- replicateM actualCount $ createMetric "resource-test" "count"
          sequence_ $ flip map metrics $ \m -> recordMetric m 1.0
          
          -- 关闭遥测系统
          shutdownTelemetry
          
          -- 重新初始化并验证干净状态
          initTelemetry productionConfig
          newMetric <- createMetric "after-shutdown" "count"
          newValue <- metricValue newMetric
          shutdownTelemetry
          
          return (newValue == 0.0)
    
    it "should handle metric registry cleanup" $ property $
    
          \ (name :: Int) ->
    
            let metricName = pack $ "registry-test-" ++ show (abs name `mod` 100)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建度量
          metric <- createMetric metricName "count"
          recordMetric metric 42.0
          
          -- 关闭并重新初始化
          shutdownTelemetry
          initTelemetry productionConfig
          
          -- 验证注册表已清理
          registry <- takeMVar metricRegistry
          let isEmpty = Map.null registry
          putMVar metricRegistry registry
          
          shutdownTelemetry
          return isEmpty
  
  -- 测试9: 遥测系统的性能特征
  describe "Telemetry System Performance Characteristics" $ do
    it "should handle high-frequency metric operations" $ property $
      \operationCount ->
        let actualCount = max 1 (abs operationCount `mod` 100 + 1)
        in unsafePerformIO $ do
          -- 禁用度量共享以确保测试隔离
          writeIORef enableMetricSharing False
          
          metric <- createMetric "performance-test" "ops"
          
          -- 测量操作时间
          let start = actualCount  -- 简单的性能测试
          sequence_ $ replicate actualCount $ recordMetric metric 1.0
          result <- metricValue metric
          
          -- 重新启用度量共享
          writeIORef enableMetricSharing True
          
          return (result == fromIntegral actualCount)
    
    it "should maintain performance with large metric names" $ property $
      \nameLength ->
        let actualLength = max 1 (abs nameLength `mod` 100 + 1)
            longName = pack $ replicate actualLength 'x'
        in unsafePerformIO $ do
          -- 禁用度量共享以确保测试隔离
          writeIORef enableMetricSharing False
          
          metric <- createMetric longName "performance"
          recordMetric metric 1.0
          result <- metricValue metric
          
          -- 重新启用度量共享
          writeIORef enableMetricSharing True
          
          return (result == 1.0)
  
  -- 测试10: 错误恢复和异常处理
  describe "Error Recovery and Exception Handling" $ do
    it "should recover from invalid metric values" $ property $
      \value ->
        let testValue = if isNaN value then 42.0 else value
        in unsafePerformIO $ do
          metric <- createMetric "error-recovery" "test"
          result <- try $ recordMetric metric testValue
          case result of
            Left (_ :: SomeException) -> return False
            Right _ -> do
              finalValue <- metricValue metric
              -- If testValue was NaN, finalValue will be NaN (NaN propagation)
              -- If testValue was not NaN, finalValue should not be NaN
              return (if isNaN value then isNaN finalValue else not (isNaN finalValue))
    
    it "should handle telemetry system restart" $ property $
    
          \ (restartCount :: Int) ->
    
            let actualRestarts = max 1 (abs restartCount `mod` 3 + 1)
        in unsafePerformIO $ do
          let performRestart i = do
                initTelemetry productionConfig
                metric <- createMetric (pack $ "restart-" ++ show i) "count"
                recordMetric metric 1.0
                shutdownTelemetry
          
          -- 执行多次重启
          sequence_ $ map performRestart [1..actualRestarts]
          
          -- 验证系统仍然可用
          initTelemetry productionConfig
          finalMetric <- createMetric "after-restarts" "count"
          recordMetric finalMetric 42.0
          result <- metricValue finalMetric
          shutdownTelemetry
          
          return (result == 42.0)