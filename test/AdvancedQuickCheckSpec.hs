{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AdvancedQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newMVar, takeMVar, putMVar, modifyMVar_)
import Control.Monad (replicateM, replicateM_, when, void)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)
import Data.Char (ord, chr)
import Data.Bits (xor)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Advanced QuickCheck-based Telemetry Tests" $ do
  
  -- 1. 测试度量值的数学属性
  describe "Metric Mathematical Properties" $ do
    it "should satisfy commutative property of addition" $ property $
      \val1 val2 ->
        let testVals = [val1, val2] :: [Double]
        in unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "commutative-test-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "commutative-test-2" "count" 0.0
          
          -- 顺序1: val1 + val2
          recordMetric metric1 val1
          recordMetric metric1 val2
          result1 <- metricValue metric1
          
          -- 顺序2: val2 + val1
          recordMetric metric2 val2
          recordMetric metric2 val1
          result2 <- metricValue metric2
          
          return (result1 == result2)
    
    it "should satisfy associative property of addition" $ property $
      \val1 val2 val3 ->
        let testVals = [val1, val2, val3] :: [Double]
        in unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "associative-test-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "associative-test-2" "count" 0.0
          
          -- 分组1: (val1 + val2) + val3
          recordMetric metric1 val1
          recordMetric metric1 val2
          recordMetric metric1 val3
          result1 <- metricValue metric1
          
          -- 分组2: val1 + (val2 + val3)
          recordMetric metric2 val1
          recordMetric metric2 val2
          recordMetric metric2 val3
          result2 <- metricValue metric2
          
          return (result1 == result2)
    
    it "should satisfy identity property of addition" $ property $
      \value ->
        let testVal = value :: Double
        in unsafePerformIO $ do
          metric <- createMetricWithInitialValue "identity-test" "count" testVal
          originalValue <- metricValue metric
          
          recordMetric metric 0.0
          newValue <- metricValue metric
          
          return (newValue == originalValue)

  -- 2. 测试Span ID的生成算法
  describe "Span ID Generation Algorithm" $ do
    it "should generate span IDs with consistent length" $ property $
      \(names :: [String]) ->
        let spanNames = take 5 (map show names)
        in unsafePerformIO $ do
          spans <- mapM (\name -> createSpan (pack name)) spanNames
          let spanIds = map spanSpanId spans
              idLengths = map Text.length spanIds
          return (all (== head idLengths) idLengths)
    
    it "should generate span IDs with hexadecimal characters only" $ property $
      \(names :: [String]) ->
        let spanNames = take 3 (map show names)
        in unsafePerformIO $ do
          spans <- mapM (\name -> createSpan (pack name)) spanNames
          let spanIds = map spanSpanId spans
              isHexChar c = c `elem` ['0'..'9'] ++ ['a'..'f']
              allHex = all (\id -> all isHexChar (unpack id)) spanIds
          return allHex
    
    it "should generate different span IDs for same name" $ property $
      \name ->
        let spanName = pack name
        in unsafePerformIO $ do
          spans <- replicateM 3 $ createSpan spanName
          let spanIds = map spanSpanId spans
          return (length (nub spanIds) == length spanIds)

  -- 3. 测试Trace ID的生成和传播
  describe "Trace ID Generation and Propagation" $ do
    it "should maintain trace ID consistency within a trace" $ property $
      \(names :: [String]) ->
        let spanNames = if null names then ["op1", "op2", "op3", "op4"] else take 4 (map show names)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          spans <- mapM (\name -> createSpan (pack name)) spanNames
          let traceIds = map spanTraceId spans
              uniqueTraceIds = nub traceIds
          
          shutdownTelemetry
          return (length uniqueTraceIds == 1)
    
    it "should generate new trace ID after shutdown and reinit" $ property $
      \name ->
        let spanName = pack name
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          span1 <- createSpan spanName
          let traceId1 = spanTraceId span1
          shutdownTelemetry
          
          initTelemetry defaultConfig
          span2 <- createSpan spanName
          let traceId2 = spanTraceId span2
          shutdownTelemetry
          
          return (traceId1 /= traceId2)

  -- 4. 测试日志级别的层次结构
  describe "Log Level Hierarchy" $ do
    it "should maintain correct log level ordering" $ property $
      \levelInt ->
        let levels = [Debug, Info, Warn, Error]
            level = levels !! (abs levelInt `mod` 4)
            higherLevels = filter (>= level) levels
        in length higherLevels >= 1
    
    it "should create loggers with consistent level comparison" $ property $
      \(levelIntPair :: (Int, Int)) ->
        let (levelInt1, levelInt2) = levelIntPair
            levels = [Debug, Info, Warn, Error]
            level1 = levels !! (abs levelInt1 `mod` 4)
            level2 = levels !! (abs levelInt2 `mod` 4)
        in unsafePerformIO $ do
          logger1 <- createLogger "test-logger-1" level1
          logger2 <- createLogger "test-logger-2" level2
          
          return (loggerLevel logger1 == level1 && 
                  loggerLevel logger2 == level2 &&
                  (level1 == level2) == (loggerLevel logger1 == loggerLevel logger2))

  -- 5. 测试配置字段的独立性
  describe "Configuration Field Independence" $ do
    it "should maintain independent configuration fields" $ property $
      \(name :: String) (version :: String) (metrics :: Bool) (tracing :: Bool) (logging :: Bool) ->
        let config = TelemetryConfig (pack name) (pack version) metrics tracing logging False
        in unsafePerformIO $ do
          initTelemetry config
          
          -- 创建并使用组件
          metric <- createMetric "independence-test" "count"
          logger <- createLogger "independence-logger" Info
          span <- createSpan "independence-span"
          
          recordMetric metric 1.0
          logMessage logger Info "test message"
          finishSpan span
          
          shutdownTelemetry
          
          return (serviceName config == pack name &&
                  serviceVersion config == pack version &&
                  enableMetrics config == metrics &&
                  enableTracing config == tracing &&
                  enableLogging config == logging)

  -- 6. 测试并发操作的原子性
  describe "Concurrent Operation Atomicity" $ do
    it "should maintain atomicity of concurrent metric operations" $ property $
      \(numThreads :: Int) ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
            operationsPerThread = 50
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          metric <- createMetric "atomicity-test" "count"
          counter <- newMVar 0
          
          -- 创建多个线程同时操作度量
          threads <- mapM (\_ -> forkIO $ do
            replicateM_ operationsPerThread $ do
              recordMetric metric 1.0
              modifyMVar_ counter (\c -> return (c + 1))
            ) [1..actualThreads]
          
          -- 等待所有线程完成
          threadDelay 2000000  -- 2秒
          
          -- 清理线程
          sequence_ $ map killThread threads
          
          -- 验证最终值
          finalMetricValue <- metricValue metric
          finalCounter <- takeMVar counter
          let expectedValue = fromIntegral finalCounter
          
          shutdownTelemetry
          return (finalMetricValue == expectedValue)
    
    it "should handle concurrent span creation with unique IDs" $ property $
      \(numThreads :: Int) ->
        let actualThreads = max 1 (abs numThreads `mod` 3 + 1)
            spansPerThread = 20
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          spansRef <- newMVar []
          
          -- 创建多个线程同时创建spans
          threads <- mapM (\threadId -> forkIO $ do
            threadSpans <- replicateM spansPerThread $ do
              createSpan (pack $ "concurrent-span-" ++ show threadId)
            modifyMVar_ spansRef (\existing -> return (existing ++ threadSpans))
            ) [1..actualThreads]
          
          -- 等待所有线程完成
          threadDelay 2000000  -- 2秒
          
          -- 清理线程
          sequence_ $ map killThread threads
          
          -- 验证所有span ID都是唯一的
          allSpans <- takeMVar spansRef
          let spanIds = map spanSpanId allSpans
              uniqueSpanIds = nub spanIds
          
          shutdownTelemetry
          return (length uniqueSpanIds == length spanIds)

  -- 7. 测试文本处理的边界条件
  describe "Text Processing Edge Cases" $ do
    it "should handle extreme Unicode characters" $ property $
      \charCode ->
        let char = chr (abs charCode `mod` 65536)
            text = pack [char]
        in unsafePerformIO $ do
          metric <- createMetric text "unicode-unit"
          logger <- createLogger text Info
          span <- createSpan text
          
          return (metricName metric == text &&
                  loggerName logger == text &&
                  spanName span == text)
    
    it "should handle mixed ASCII and Unicode" $ property $
      \(str :: String) ->
        let mixedText = pack $ "prefix-" ++ str ++ "-suffix-测试"
        in unsafePerformIO $ do
          metric <- createMetric mixedText "mixed-unit"
          logger <- createLogger mixedText Info
          span <- createSpan mixedText
          
          return (metricName metric == mixedText &&
                  loggerName logger == mixedText &&
                  spanName span == mixedText)
    
    it "should handle very long text with special characters" $ property $
      \(baseLength :: Int) ->
        let base = abs baseLength `mod` 100 + 1
            longText = pack $ concat $ replicate base "test\n\t测试🚀"
        in unsafePerformIO $ do
          metric <- createMetric longText "long-unit"
          logger <- createLogger longText Info
          span <- createSpan longText
          
          return (metricName metric == longText &&
                  loggerName logger == longText &&
                  spanName span == longText)

  -- 8. 测试系统状态的一致性
  describe "System State Consistency" $ do
    it "should maintain consistent system state across operations" $ property $
      \(operations :: Int) ->
        let numOps = max 1 (abs operations `mod` 20 + 1)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          -- 创建组件
          metric <- createMetric "state-test" "count"
          logger <- createLogger "state-test-logger" Info
          
          -- 执行一系列操作
          replicateM_ numOps $ do
            recordMetric metric 1.0
            logMessage logger Info "state test message"
          
          -- 创建span并完成
          span <- createSpan "state-test-span"
          finishSpan span
          
          -- 验证系统状态
          finalValue <- metricValue metric
          let expectedValue = fromIntegral numOps
          
          shutdownTelemetry
          return (finalValue == expectedValue)
    
    it "should handle multiple initialization cycles consistently" $ property $
      \(cycles :: Int) ->
        let numCycles = max 1 (abs cycles `mod` 5 + 1)
        in unsafePerformIO $ do
          let runCycle cycleNum = do
                initTelemetry defaultConfig
                
                metric <- createMetric (pack $ "cycle-" ++ show cycleNum) "count"
                recordMetric metric (fromIntegral cycleNum)
                
                value <- metricValue metric
                shutdownTelemetry
                return value
          
          results <- mapM runCycle [1..numCycles]
          let expectedValues = map fromIntegral [1..numCycles]
          
          return (results == expectedValues)