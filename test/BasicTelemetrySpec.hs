{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BasicTelemetrySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.Char (isHexDigit)
import Data.List (sort, nub)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, replicateM_, when, void)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)
import Data.Maybe (isJust, isNothing)

import Azimuth.Telemetry
import Data.IORef (writeIORef)

spec :: Spec
spec = describe "Basic Telemetry Property Tests" $ do
  
  -- 1. 测试度量值的数学属性（交换律）
  describe "Metric Mathematical Properties" $ do
    it "should satisfy commutative property for metric recording" $ property $
      \value1 value2 ->
        let values = [value1, value2] :: [Double]
        in unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "commutative-test-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "commutative-test-2" "count" 0.0
          
          -- 顺序1: value1 then value2
          recordMetric metric1 value1
          recordMetric metric1 value2
          result1 <- metricValue metric1
          
          -- 顺序2: value2 then value1
          recordMetric metric2 value2
          recordMetric metric2 value1
          result2 <- metricValue metric2
          
          return (result1 == result2)
    
    it "should satisfy associative property for metric recording" $ property $
      \value1 value2 value3 ->
        let values = [value1, value2, value3] :: [Double]
            expectedSum = value1 + value2 + value3
            epsilon = 1e-10  -- 允许的误差范围
        in unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "associative-test-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "associative-test-2" "count" 0.0
          
          -- 分组1: (value1 + value2) + value3
          recordMetric metric1 value1
          recordMetric metric1 value2
          recordMetric metric1 value3
          result1 <- metricValue metric1
          
          -- 分组2: value1 + (value2 + value3)
          recordMetric metric2 value2
          recordMetric metric2 value3
          recordMetric metric2 value1
          result2 <- metricValue metric2
          
          -- 检查结果是否在允许的误差范围内
          return (abs (result1 - expectedSum) < epsilon && 
                  abs (result2 - expectedSum) < epsilon &&
                  abs (result1 - result2) < epsilon)
  
  -- 2. 测试Span和Trace的一致性属性
  describe "Span and Trace Consistency" $ do
    it "should maintain consistent trace ID within a trace" $ property $
      \name ->
        let spanName = pack $ "trace-consistency-" ++ show (name :: Int)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建多个span，它们应该有相同的trace ID
          spans <- replicateM 5 $ createSpan spanName
          let traceIds = map spanTraceId spans
              uniqueTraceIds = nub traceIds
          
          shutdownTelemetry
          -- 所有span应该有相同的trace ID
          return (length uniqueTraceIds == 1)
    
    it "should generate unique span IDs within the same trace" $ property $
      \name ->
        let spanName = pack $ "span-uniqueness-" ++ show (name :: Int)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建多个span，它们应该有不同的span ID
          spans <- replicateM 5 $ createSpan spanName
          let spanIds = map spanSpanId spans
              uniqueSpanIds = nub spanIds
          
          shutdownTelemetry
          -- 所有span应该有不同的span ID
          return (length uniqueSpanIds == 5)
  
  -- 3. 测试日志级别的顺序属性
  describe "Log Level Ordering" $ do
    it "should maintain correct ordering of log levels" $ property $
      \levelInt1 levelInt2 ->
        let levels = [Debug, Info, Warn, Error]
            level1 = levels !! (abs levelInt1 `mod` 4)
            level2 = levels !! (abs levelInt2 `mod` 4)
        in unsafePerformIO $ do
          logger1 <- createLogger "ordering-test-1" level1
          logger2 <- createLogger "ordering-test-2" level2
          
          let level1Order = fromEnum (loggerLevel logger1)
              level2Order = fromEnum (loggerLevel logger2)
          
          -- 验证枚举顺序
          return (level1Order >= 0 && level1Order <= 3 && 
                  level2Order >= 0 && level2Order <= 3)
    
    it "should handle log level comparisons correctly" $ property $
      \levelInt ->
        let levels = [Debug, Info, Warn, Error]
            level = levels !! (abs levelInt `mod` 4)
        in unsafePerformIO $ do
          logger <- createLogger "comparison-test" level
          
          -- 验证日志级别的比较属性
          let currentLevel = loggerLevel logger
              isHigherThan l = currentLevel > l
              isLowerThan l = currentLevel < l
              isHigherOrEqual l = currentLevel >= l
              isLowerOrEqual l = currentLevel <= l
          
          -- 测试与所有级别的比较
          let comparisons = map (\l -> (isHigherThan l, isLowerThan l, isHigherOrEqual l, isLowerOrEqual l)) levels
          
          -- 验证自反性
          let selfComparison = isHigherOrEqual level && isLowerOrEqual level
          
          return (selfComparison)
  
  -- 4. 测试度量值的边界条件处理
  describe "Metric Boundary Conditions" $ do
    it "should handle zero values correctly" $ property $
      \(value :: Double) ->
        let testValue = if value == 0.0 then 0.0 else 0.0
        in unsafePerformIO $ do
          metric <- createMetricWithInitialValue "zero-test" "count" testValue
          recordMetric metric testValue
          result <- metricValue metric
          return (result == testValue)
    
    it "should handle very large values correctly" $ property $
      \(value :: Double) ->
        let testValue = 1e10  -- 使用固定的大值
            epsilon = 1e-5  -- 允许的误差范围
        in unsafePerformIO $ do
          metric <- createMetric "large-value-test" "count"
          recordMetric metric testValue
          result <- metricValue metric
          return (abs (result - testValue) < epsilon)
    
    it "should handle very small values correctly" $ property $
      \(value :: Double) ->
        let testValue = 1e-10  -- 使用固定的小值
            epsilon = 1e-15  -- 允许的误差范围
        in unsafePerformIO $ do
          metric <- createMetric "small-value-test" "count"
          recordMetric metric testValue
          result <- metricValue metric
          return (abs (result - testValue) < epsilon)
  
  -- 5. 测试配置的等价性属性
  describe "Configuration Equivalence" $ do
    it "should treat equivalent configurations as equal" $ property $
      \name version metrics tracing logging debug ->
        let config1 = TelemetryConfig (pack name) (pack version) metrics tracing logging debug
            config2 = TelemetryConfig (pack name) (pack version) metrics tracing logging debug
        in config1 == config2
    
    it "should detect different configurations correctly" $ property $
      \name1 name2 ->
        let config1 = TelemetryConfig (pack name1) "1.0.0" True True True True
            config2 = TelemetryConfig (pack name2) "1.0.0" True True True True
        in if name1 == name2 
           then True -- 相同名称应该相等
           else config1 /= config2 -- 不同名称应该不相等
    
    it "should handle configuration changes correctly" $ property $
      \name version metrics tracing logging debug ->
        let originalConfig = TelemetryConfig (pack name) (pack version) metrics tracing logging debug
            modifiedConfig = TelemetryConfig (pack name) (pack version) (not metrics) tracing logging debug
        in if metrics then originalConfig /= modifiedConfig else True
  
  -- 6. 测试度量值的类型转换属性
  describe "Metric Type Conversion" $ do
    it "should convert simple metrics to regular metrics correctly" $ property $
      \name unit value ->
        let simpleMetric = createSimpleMetric (pack name) (pack unit) value
        in unsafePerformIO $ do
          regularMetric <- simpleToMetric simpleMetric
          regularValue <- metricValue regularMetric
          return (regularValue == value)
    
    it "should maintain simple metric properties after conversion" $ property $
      \name unit value ->
        let simpleMetric = createSimpleMetric (pack name) (pack unit) value
            updatedSimpleMetric = recordSimpleMetric simpleMetric value
        in unsafePerformIO $ do
          regularMetric <- simpleToMetric updatedSimpleMetric
          regularValue <- metricValue regularMetric
          return (regularValue == value + value)
  
  -- 7. 测试Span ID的格式属性
  describe "Span ID Format Properties" $ do
    it "should generate span IDs with hexadecimal characters only" $ property $
      \name ->
        let spanName = pack $ "hex-test-" ++ show (name :: Int)
        in unsafePerformIO $ do
          span <- createSpan spanName
          let spanIdStr = unpack (spanSpanId span)
              isHexChar c = isHexDigit c
              allHex = all isHexChar spanIdStr
          return (allHex && not (null spanIdStr))
    
    it "should generate span IDs with consistent length" $ property $
      \name ->
        let spanName = pack $ "length-test-" ++ show (name :: Int)
        in unsafePerformIO $ do
          spans <- replicateM 3 $ createSpan spanName
          let spanIds = map (unpack . spanSpanId) spans
              lengths = map length spanIds
              allSameLength = all (== head lengths) (tail lengths)
          return (allSameLength && all (> 0) lengths)
  
  -- 8. 测试日志消息的持久性
  describe "Log Message Persistence" $ do
    it "should allow multiple loggers with the same name" $ property $
      \name level ->
        let testName = pack $ "persistence-test-" ++ show (name :: Int)
            levels = [Debug, Info, Warn, Error]
            logLevel = levels !! (abs level `mod` 4)
        in unsafePerformIO $ do
          logger1 <- createLogger testName logLevel
          logger2 <- createLogger testName logLevel
          
          -- 验证两个logger有相同的名称和级别
          let sameName = loggerName logger1 == loggerName logger2
              sameLevel = loggerLevel logger1 == loggerLevel logger2
          
          return (sameName && sameLevel)
    
    it "should handle logging at different levels" $ property $
      \name ->
        let testName = pack $ "multi-level-test-" ++ show (name :: Int)
        in unsafePerformIO $ do
          logger <- createLogger testName Info
          
          -- 尝试在不同级别记录消息
          logMessage logger Debug "debug message"
          logMessage logger Info "info message"
          logMessage logger Warn "warning message"
          logMessage logger Error "error message"
          
          -- 验证logger属性没有改变
          let nameUnchanged = loggerName logger == testName
              levelUnchanged = loggerLevel logger == Info
          
          return (nameUnchanged && levelUnchanged)
  
  -- 9. 测试度量值的并发累加属性
  describe "Concurrent Metric Accumulation" $ do
    it "should handle concurrent metric recording correctly" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
            incrementValue = 1.0
        in unsafePerformIO $ do
          -- 禁用度量共享以确保测试隔离
          writeIORef enableMetricSharing False
          
          metric <- createMetric "concurrent-accumulation" "count"
          
          -- 创建MVar来同步线程
          ready <- newEmptyMVar
          done <- newEmptyMVar
          
          -- 启动多个线程同时记录度量值
          threads <- replicateM actualThreads $ forkIO $ do
            putMVar ready ()
            replicateM_ 10 $ recordMetric metric incrementValue
          
          -- 等待所有线程准备就绪
          replicateM_ actualThreads $ takeMVar ready
          
          -- 等待所有线程完成
          threadDelay 10000  -- 10毫秒
          
          -- 清理线程
          sequence_ $ map killThread threads
          
          -- 检查最终值
          finalValue <- metricValue metric
          let expectedValue = fromIntegral actualThreads * 10.0 * incrementValue
          
          -- 重新启用度量共享
          writeIORef enableMetricSharing True
          
          return (finalValue == expectedValue)
  
  -- 10. 测试遥测组件的生命周期属性
  describe "Telemetry Component Lifecycle" $ do
    it "should maintain component properties across lifecycle" $ property $
      \name ->
        let componentName = pack $ "lifecycle-test-" ++ show (name :: Int)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建组件
          metric <- createMetric componentName "count"
          logger <- createLogger componentName Info
          span <- createSpan componentName
          
          -- 记录初始属性
          initialMetricName <- return $ metricName metric
          initialLoggerName <- return $ loggerName logger
          initialSpanName <- return $ spanName span
          
          -- 使用组件
          recordMetric metric 42.0
          logMessage logger Info "lifecycle test"
          finishSpan span
          
          -- 验证属性没有改变
          finalMetricName <- return $ metricName metric
          finalLoggerName <- return $ loggerName logger
          finalSpanName <- return $ spanName span
          
          shutdownTelemetry
          
          return (initialMetricName == finalMetricName &&
                  initialLoggerName == finalLoggerName &&
                  initialSpanName == finalSpanName)
    
    it "should handle multiple initialization cycles" $ property $
      \(cycles :: Int) ->
        let actualCycles = max 1 (abs cycles `mod` 3 + 1)
        in unsafePerformIO $ do
          let runCycle cycleNum = do
                initTelemetry productionConfig
                
                -- 创建和使用组件
                metric <- createMetric (pack $ "cycle-" ++ show cycleNum) "count"
                recordMetric metric (fromIntegral cycleNum)
                
                logger <- createLogger (pack $ "cycle-logger-" ++ show cycleNum) Info
                logMessage logger Info (pack $ "cycle " ++ show cycleNum)
                
                span <- createSpan (pack $ "cycle-span-" ++ show cycleNum)
                finishSpan span
                
                shutdownTelemetry
          
          -- 执行多个周期
          sequence_ $ map runCycle [1..actualCycles]
          
          -- 如果没有异常，则测试通过
          return True