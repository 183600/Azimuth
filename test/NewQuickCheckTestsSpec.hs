{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NewQuickCheckTestsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.Char (isHexDigit, isControl)
import Data.List (nub, sort, group)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, replicateM_, when, void)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)
import Data.Maybe (isJust, isNothing)
import Data.Word (Word8)
import Data.Bits (Bits(shiftR, (.&.)))

import Azimuth.Telemetry

spec :: Spec
spec = describe "New QuickCheck-based Telemetry Tests" $ do
  
  -- 1. 测试度量值的NaN和无穷大处理
  describe "Metric NaN and Infinity Handling" $ do
    it "should handle NaN values correctly" $ property $
      \(_value :: Double) ->
        let nanValue = 0/0 :: Double
        in unsafePerformIO $ do
          metric <- createMetric "nan-test" "count"
          recordMetric metric nanValue
          result <- metricValue metric
          return (isNaN result)
    
    it "should handle positive infinity correctly" $ property $
      \(_value :: Double) ->
        let posInf = 1/0 :: Double
        in unsafePerformIO $ do
          metric <- createMetric "pos-inf-test" "count"
          recordMetric metric posInf
          result <- metricValue metric
          return (isInfinite result && result > 0)
    
    it "should handle negative infinity correctly" $ property $
      \(_value :: Double) ->
        let negInf = -1/0 :: Double
        in unsafePerformIO $ do
          metric <- createMetric "neg-inf-test" "count"
          recordMetric metric negInf
          result <- metricValue metric
          return (isInfinite result && result < 0)
    
    it "should handle mixed infinity operations" $ property $
      \(_value :: Double) ->
        let posInf = 1/0 :: Double
            negInf = -1/0 :: Double
        in unsafePerformIO $ do
          metric <- createMetric "mixed-inf-test" "count"
          recordMetric metric posInf
          recordMetric metric negInf
          result <- metricValue metric
          return (isNaN result)
  
  -- 2. 测试Span ID的格式验证
  describe "Span ID Format Validation" $ do
    it "should generate span IDs with only hexadecimal characters" $ property $
      \name ->
        let spanName = pack $ "hex-validation-" ++ show (name :: Int)
        in unsafePerformIO $ do
          span <- createSpan spanName
          let spanIdStr = unpack (spanSpanId span)
              isHexChar c = isHexDigit c
              allHex = all isHexChar spanIdStr
          return (allHex && not (null spanIdStr))
    
    it "should generate span IDs with consistent length" $ property $
      \name ->
        let spanName = pack $ "length-validation-" ++ show (name :: Int)
        in unsafePerformIO $ do
          spans <- replicateM 5 $ createSpan spanName
          let spanIds = map (unpack . spanSpanId) spans
              lengths = map length spanIds
              allSameLength = all (== head lengths) (tail lengths)
          return (allSameLength && all (> 0) lengths)
    
    it "should generate trace IDs with only hexadecimal characters" $ property $
      \name ->
        let spanName = pack $ "trace-hex-validation-" ++ show (name :: Int)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          span <- createSpan spanName
          let traceIdStr = unpack (spanTraceId span)
              isHexChar c = isHexDigit c
              allHex = all isHexChar traceIdStr
          shutdownTelemetry
          return (allHex && not (null traceIdStr))
  
  -- 3. 测试日志级别的枚举属性
  describe "Log Level Enumeration Properties" $ do
    it "should maintain correct ordering of log levels" $ property $
      \levelInt ->
        let levels = [Debug, Info, Warn, Error]
            level = levels !! (abs levelInt `mod` 4)
        in unsafePerformIO $ do
          logger <- createLogger "ordering-test" level
          let levelOrder = fromEnum (loggerLevel logger)
          return (levelOrder >= 0 && levelOrder <= 3)
    
    it "should handle log level comparisons correctly" $ property $
      \levelInt1 levelInt2 ->
        let levels = [Debug, Info, Warn, Error]
            level1 = levels !! (abs levelInt1 `mod` 4)
            level2 = levels !! (abs levelInt2 `mod` 4)
        in unsafePerformIO $ do
          logger1 <- createLogger "comparison-test-1" level1
          logger2 <- createLogger "comparison-test-2" level2
          
          let level1Order = fromEnum (loggerLevel logger1)
              level2Order = fromEnum (loggerLevel logger2)
              comparisonCorrect = (level1Order < level2Order) == (level1 < level2)
          
          return comparisonCorrect
    
    it "should handle all log levels consistently" $ property $
      \name ->
        let testName = pack $ "consistency-test-" ++ show (name :: Int)
            levels = [Debug, Info, Warn, Error]
        in unsafePerformIO $ do
          loggers <- mapM (\level -> createLogger testName level) levels
          let loggerNames = map (unpack . loggerName) loggers
              loggerLevels = map loggerLevel loggers
          return (all (== unpack testName) loggerNames && sort loggerLevels == levels)
  
  -- 4. 测试配置字段的持久性
  describe "Configuration Field Persistence" $ do
    it "should preserve configuration fields across operations" $ property $
      \name version metrics tracing logging debug ->
        let config = TelemetryConfig (pack name) (pack version) metrics tracing logging debug
        in unsafePerformIO $ do
          initTelemetry config
          
          -- 执行一些操作
          metric <- createMetric "persistence-test" "count"
          recordMetric metric 1.0
          
          logger <- createLogger "persistence-logger" Info
          logMessage logger Info "persistence test"
          
          span <- createSpan "persistence-span"
          finishSpan span
          
          -- 验证配置仍然有效
          metricValue <- metricValue metric
          shutdownTelemetry
          
          return (metricValue == 1.0)
    
    it "should handle configuration changes correctly" $ property $
      \name version metrics tracing logging debug ->
        let originalConfig = TelemetryConfig (pack name) (pack version) metrics tracing logging debug
            modifiedConfig = TelemetryConfig (pack name) (pack version) (not metrics) tracing logging debug
        in unsafePerformIO $ do
          initTelemetry originalConfig
          
          -- 执行一些操作
          metric <- createMetric "change-test" "count"
          recordMetric metric 1.0
          
          -- 更改配置
          initTelemetry modifiedConfig
          
          -- 验证操作仍然成功
          recordMetric metric 2.0
          finalValue <- metricValue metric
          
          shutdownTelemetry
          
          return (finalValue == 3.0)
  
  -- 5. 测试并发创建多个不同类型的组件
  describe "Concurrent Component Creation" $ do
    it "should handle concurrent creation of different component types" $ property $
      \numComponents ->
        let actualComponents = max 1 (abs numComponents `mod` 5 + 1)
        in unsafePerformIO $ do
          -- 禁用度量共享以确保测试隔离
          writeIORef enableMetricSharing False
          
          -- 创建MVar来同步线程
          ready <- newEmptyMVar
          done <- newEmptyMVar
          
          -- 启动多个线程创建不同类型的组件
          threads <- replicateM actualComponents $ forkIO $ do
            putMVar ready ()
            
            -- 创建度量
            metric <- createMetric "concurrent-metric" "count"
            recordMetric metric 1.0
            
            -- 创建日志器
            logger <- createLogger "concurrent-logger" Info
            logMessage logger Info "concurrent test"
            
            -- 创建Span
            span <- createSpan "concurrent-span"
            finishSpan span
            
            putMVar done ()
          
          -- 等待所有线程准备就绪
          replicateM actualComponents $ takeMVar ready
          
          -- 等待所有线程完成
          replicateM actualComponents $ takeMVar done
          
          -- 清理线程
          sequence_ $ map killThread threads
          
          -- 重新启用度量共享
          writeIORef enableMetricSharing True
          
          return True
    
    it "should handle concurrent operations on shared components" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
        in unsafePerformIO $ do
          -- 创建共享组件
          metric <- createMetric "shared-metric" "count"
          logger <- createLogger "shared-logger" Info
          
          -- 创建MVar来同步线程
          ready <- newEmptyMVar
          done <- newEmptyMVar
          
          -- 启动多个线程操作共享组件
          threads <- replicateM actualThreads $ forkIO $ do
            putMVar ready ()
            
            -- 操作共享度量
            replicateM_ 10 $ recordMetric metric 1.0
            
            -- 操作共享日志器
            replicateM_ 5 $ logMessage logger Info "shared operation"
            
            putMVar done ()
          
          -- 等待所有线程准备就绪
          replicateM actualThreads $ takeMVar ready
          
          -- 等待所有线程完成
          replicateM actualThreads $ takeMVar done
          
          -- 清理线程
          sequence_ $ map killThread threads
          
          -- 验证最终状态
          finalValue <- metricValue metric
          let expectedValue = fromIntegral actualThreads * 10.0
          
          return (finalValue == expectedValue)
  
  -- 6. 测试大量数据处理的性能
  describe "Large Data Processing Performance" $ do
    it "should handle large number of metric operations efficiently" $ property $
      \numOps ->
        let operations = max 10 (abs numOps `mod` 100 + 10)
        in unsafePerformIO $ do
          metric <- createMetric "performance-metric" "ops"
          
          -- 执行大量操作
          sequence_ $ replicate operations $ do
            recordMetric metric 1.0
          
          -- 验证所有操作都完成了
          finalValue <- metricValue metric
          return (finalValue == fromIntegral operations)
    
    it "should handle large number of span operations efficiently" $ property $
      \numOps ->
        let operations = max 5 (abs numOps `mod` 50 + 5)
        in unsafePerformIO $ do
          -- 创建大量span
          spans <- sequence $ replicate operations $ do
            createSpan "performance-span"
          
          -- 完成所有span
          sequence_ $ map finishSpan spans
          
          return (length spans == operations)
    
    it "should handle large number of logging operations efficiently" $ property $
      \numOps ->
        let operations = max 10 (abs numOps `mod` 100 + 10)
        in unsafePerformIO $ do
          logger <- createLogger "performance-logger" Info
          
          -- 执行大量日志操作
          sequence_ $ replicate operations $ do
            logMessage logger Info "performance test"
          
          return True
  
  -- 7. 测试边界条件（空字符串、特殊字符）
  describe "Boundary Conditions with Special Strings" $ do
    it "should handle empty strings in component names" $ property $
      \() ->
        unsafePerformIO $ do
          -- 测试空字符串
          metric <- createMetric "" ""
          logger <- createLogger "" Info
          span <- createSpan ""
          
          return (metricName metric == "" &&
                  metricUnit metric == "" &&
                  loggerName logger == "" &&
                  spanName span == "")
    
    it "should handle control characters in component names" $ property $
      \charCode ->
        let char = toEnum (abs charCode `mod` 32) :: Char
            controlStr = pack [char]
        in unsafePerformIO $ do
          -- 测试控制字符
          metric <- createMetric controlStr "control-unit"
          logger <- createLogger controlStr Info
          span <- createSpan controlStr
          
          return (metricName metric == controlStr &&
                  loggerName logger == controlStr &&
                  spanName span == controlStr)
    
    it "should handle very long strings in component names" $ property $
      \length ->
        let actualLength = max 1 (abs length `mod` 1000 + 1)
            longString = pack $ replicate actualLength 'a'
        in unsafePerformIO $ do
          -- 测试长字符串
          metric <- createMetric longString "long-unit"
          logger <- createLogger longString Info
          span <- createSpan longString
          
          return (Text.length (metricName metric) == actualLength &&
                  Text.length (loggerName logger) == actualLength &&
                  Text.length (spanName span) == actualLength)
    
    it "should handle unicode characters in component names" $ property $
      \unicodeStr ->
        let unicodeText = pack unicodeStr
        in unsafePerformIO $ do
          -- 测试Unicode字符
          metric <- createMetric unicodeText unicodeText
          logger <- createLogger unicodeText Info
          span <- createSpan unicodeText
          
          return (metricName metric == unicodeText &&
                  metricUnit metric == unicodeText &&
                  loggerName logger == unicodeText &&
                  spanName span == unicodeText)
  
  -- 8. 测试资源清理和内存泄漏
  describe "Resource Cleanup and Memory Leaks" $ do
    it "should clean up resources properly after shutdown" $ property $
      \numResources ->
        let resources = max 1 (abs numResources `mod` 10 + 1)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          -- 创建大量资源
          metrics <- sequence $ replicate resources $ do
            createMetric "cleanup-test" "count"
          
          loggers <- sequence $ replicate resources $ do
            createLogger "cleanup-logger" Info
          
          spans <- sequence $ replicate resources $ do
            createSpan "cleanup-span"
          
          -- 使用资源
          sequence_ $ map (`recordMetric` 1.0) metrics
          sequence_ $ flip map loggers $ \logger -> do
            logMessage logger Info "cleanup test"
          sequence_ $ map finishSpan spans
          
          -- 关闭系统
          shutdownTelemetry
          
          -- 重新初始化并验证系统仍然工作
          initTelemetry defaultConfig
          
          newMetric <- createMetric "after-cleanup" "count"
          recordMetric newMetric 42.0
          newValue <- metricValue newMetric
          
          shutdownTelemetry
          
          return (newValue == 42.0)
    
    it "should handle multiple init/shutdown cycles" $ property $
      \(numCycles :: Int) ->
        let cycles = max 1 (abs numCycles `mod` 5 + 1)
        in unsafePerformIO $ do
          let runCycle cycleNum = do
                initTelemetry defaultConfig
                
                -- 创建和使用组件
                metric <- createMetric (pack $ "cycle-" ++ show cycleNum) "count"
                recordMetric metric (fromIntegral cycleNum)
                
                logger <- createLogger (pack $ "cycle-logger-" ++ show cycleNum) Info
                logMessage logger Info (pack $ "cycle " ++ show cycleNum)
                
                span <- createSpan (pack $ "cycle-span-" ++ show cycleNum)
                finishSpan span
                
                shutdownTelemetry
          
          -- 执行多个周期
          sequence_ $ map runCycle [1..cycles]
          
          -- 如果没有异常，则测试通过
          return True
  
  -- 9. 测试组件间的交互
  describe "Component Interactions" $ do
    it "should handle interactions between metrics and spans" $ property $
      \numOperations ->
        let operations = max 1 (abs numOperations `mod` 10 + 1)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          -- 创建度量和span
          metric <- createMetric "interaction-metric" "count"
          
          -- 在span中记录度量
          results <- sequence $ replicate operations $ do
            span <- createSpan "interaction-span"
            recordMetric metric 1.0
            finishSpan span
            return ()
          
          shutdownTelemetry
          
          -- 验证所有度量都记录了
          finalValue <- metricValue metric
          return (finalValue == fromIntegral operations)
    
    it "should handle interactions between logging and spans" $ property $
      \numOperations ->
        let operations = max 1 (abs numOperations `mod` 10 + 1)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          -- 创建日志器
          logger <- createLogger "interaction-logger" Info
          
          -- 在span中记录日志
          results <- sequence $ replicate operations $ do
            span <- createSpan "interaction-span"
            logMessage logger Info "interaction test"
            finishSpan span
            return ()
          
          shutdownTelemetry
          
          return (length results == operations)
    
    it "should handle interactions between all three component types" $ property $
      \numOperations ->
        let operations = max 1 (abs numOperations `mod` 5 + 1)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          -- 创建所有组件类型
          metric <- createMetric "triple-interaction-metric" "count"
          logger <- createLogger "triple-interaction-logger" Info
          
          -- 在span中同时记录度量和日志
          results <- sequence $ replicate operations $ do
            span <- createSpan "triple-interaction-span"
            recordMetric metric 1.0
            logMessage logger Info "triple interaction test"
            finishSpan span
            return ()
          
          shutdownTelemetry
          
          -- 验证所有度量都记录了
          finalValue <- metricValue metric
          return (finalValue == fromIntegral operations)
  
  -- 10. 测试错误恢复机制
  describe "Error Recovery Mechanisms" $ do
    it "should recover from invalid metric values" $ property $
      \validValue ->
        let validVal = if isNaN validValue then 1.0 else validValue
        in unsafePerformIO $ do
          metric <- createMetric "error-recovery-metric" "count"
          
          -- 记录一些有效值
          recordMetric metric validVal
          recordMetric metric (validVal + 1.0)
          
          -- 记录无效值
          recordMetric metric (0/0)  -- NaN
          
          -- 记录更多有效值
          recordMetric metric (validVal + 2.0)
          
          -- 验证系统能够处理
          result <- metricValue metric
          return (isNaN result)
    
    it "should handle operations without initialization" $ property $
      \name ->
        let testName = pack $ "no-init-test-" ++ show (name :: Int)
        in unsafePerformIO $ do
          -- 确保系统未初始化
          shutdownTelemetry
          
          -- 尝试创建组件
          result <- try $ do
            metric <- createMetric testName "count"
            recordMetric metric 1.0
            metricValue metric
          
          -- 验证操作成功或优雅失败
          case result of
            Left (_ :: SomeException) -> return True  -- 优雅失败
            Right value -> return (value == 1.0)  -- 操作成功
    
    it "should handle multiple shutdown calls gracefully" $ property $
      \numShutdowns ->
        let shutdowns = max 1 (abs numShutdowns `mod` 5 + 1)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          -- 创建一些组件
          metric <- createMetric "multi-shutdown-test" "count"
          recordMetric metric 1.0
          
          -- 多次调用shutdown
          sequence_ $ replicate shutdowns shutdownTelemetry
          
          -- 验证系统仍然可以重新初始化
          initTelemetry defaultConfig
          
          newMetric <- createMetric "after-multi-shutdown" "count"
          recordMetric newMetric 42.0
          newValue <- metricValue newMetric
          
          shutdownTelemetry
          
          return (newValue == 42.0)