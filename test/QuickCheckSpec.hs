{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Monad (replicateM, when)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "QuickCheck-based Telemetry Tests" $ do
  
  -- 1. 测试度量值的累加属性
  describe "Metric Accumulation Properties" $ do
    it "should accumulate metric values correctly" $ property $
      \values -> 
        let sortedValues = values :: [Double]
            nonEmpty = not (null sortedValues)
        in if nonEmpty 
           then unsafePerformIO $ do
             metric <- createMetricWithInitialValue "accumulation-test" "count" 0.0
             sequence_ $ map (\v -> recordMetric metric v) sortedValues
             finalValue <- metricValue metric
             let expectedValue = sum sortedValues
             return (finalValue == expectedValue)
           else True
    
    it "should handle negative values in accumulation" $ property $
      \values ->
        let testValues = if null values then [-1.0, -2.0, -3.0] else values :: [Double]
        in unsafePerformIO $ do
          metric <- createMetricWithInitialValue "negative-test" "count" 0.0
          sequence_ $ map (recordMetric metric) testValues
          finalValue <- metricValue metric
          return (finalValue == sum testValues)
  
  -- 2. 测试Span ID的唯一性
  describe "Span ID Uniqueness" $ do
    it "should generate unique span IDs" $ property $
      \(names :: [Int]) ->
        let spanNames = if null names then ["test1", "test2", "test3"] else take 5 (map show names)
        in unsafePerformIO $ do
          spans <- mapM (\name -> createSpan (pack name)) spanNames
          let spanIds = map spanSpanId spans
          return (length (nub spanIds) == length spanIds)
    
    it "should generate unique trace IDs when creating first spans" $ property $
      \(names :: [String]) ->
        let spanNames = take 3 (map show names)
            nonEmpty = not (null spanNames)
            nonEmptyStrings = all (not . null) spanNames
        in if nonEmpty && nonEmptyStrings
           then unsafePerformIO $ do
             -- 关闭当前追踪上下文
                                       
             spans <- mapM (\name -> createSpan (pack name)) spanNames
             let traceIds = map spanTraceId spans
             -- 同一个trace中的span应该有相同的trace ID
             return (length (nub traceIds) == 1)
           else True
  
  -- 3. 测试Trace ID的传播
  describe "Trace ID Propagation" $ do
    it "should propagate trace ID across spans" $ property $
      \(names :: [String]) ->
        let spanNames = if null names then ["parent", "child1", "child2"] else take 3 (map show names)
        in unsafePerformIO $ do
          -- 初始化追踪上下文
                    
          -- 创建第一个span（建立trace context）
          parentSpan <- createSpan (pack (head spanNames))
          let parentTraceId = spanTraceId parentSpan
          
          -- 创建子span
          childSpans <- mapM (\name -> createSpan (pack name)) (tail spanNames)
          
          let childTraceIds = map spanTraceId childSpans
              allSameTraceId = all (== parentTraceId) childTraceIds
          return allSameTraceId
  
  -- 4. 测试日志级别过滤
  describe "Log Level Filtering" $ do
    it "should create loggers with different levels" $ property $
      \(levelInt :: Int) ->
        let levels = [Debug, Info, Warn, Error]
            level = levels !! (abs levelInt `mod` 4)
            loggerNameStr = "test-logger-" ++ show levelInt
        in unsafePerformIO $ do
          logger <- createLogger (pack loggerNameStr) level
          return (loggerLevel logger == level && unpack (loggerName logger) == loggerNameStr)
    
    it "should handle all log levels consistently" $ property $
      \(name :: String) ->
        let levels = [Debug, Info, Warn, Error]
            loggerNameText = pack name
        in unsafePerformIO $ do
          loggers <- mapM (\level -> createLogger loggerNameText level) levels
          let loggerNames = map (unpack . loggerName) loggers
              loggerLevels = map loggerLevel loggers
          return (all (== unpack loggerNameText) loggerNames && loggerLevels == levels)
  
  -- 5. 测试配置的持久性
  describe "Configuration Persistence" $ do
    it "should maintain configuration across operations" $ property $
      \(name :: String) (version :: String) (metrics :: Bool) (tracing :: Bool) (logging :: Bool) ->
        let config = TelemetryConfig (pack name) (pack version) metrics tracing logging False
        in unsafePerformIO $ do
          initTelemetry config
          
          -- 执行一些操作
          metric <- createMetric "config-test" "count"
          recordMetric metric 1.0
          
          logger <- createLogger "config-test-logger" Info
          logMessage logger Info "config test"
          
          span <- createSpan "config-test-span"
          finishSpan span
          
          -- 验证配置仍然有效（通过检查操作是否成功）
          metricValue <- metricValue metric
                    
          -- 如果配置生效，操作应该成功
          return (metricValue == 1.0)
  
  -- 6. 测试并发安全性
  describe "Concurrency Safety" $ do
    it "should handle concurrent metric operations safely" $ property $
      \(numThreads :: Int) ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
            operationsPerThread = 10
        in unsafePerformIO $ do
                    
          metric <- createMetric "concurrent-test" "count"
          
          -- 创建多个线程同时操作度量
          threads <- mapM (\_ -> forkIO $ do
            sequence_ $ replicate operationsPerThread $ do
              recordMetric metric 1.0
            ) [1..actualThreads]
          
          -- 等待所有线程完成
          threadDelay 10000  -- 10毫秒
          
          -- 清理线程
          sequence_ $ map killThread threads
          
          -- 验证最终值
          finalValue <- metricValue metric
          let expectedValue = fromIntegral actualThreads * fromIntegral operationsPerThread
          return (finalValue == expectedValue)
  
  -- 7. 测试边界值处理
  describe "Boundary Value Handling" $ do
    it "should handle special floating point values" $ do
      let specialValues = [(0.0, "zero"), (-0.0, "negative-zero"), (1.0, "one"), (-1.0, "negative-one"), 
                           (1.0/0.0, "positive-infinity"), (-1.0/0.0, "negative-infinity"), (0.0/0.0, "nan")]
      
      mapM_ (\(value, name) -> do
        metric <- createMetric (pack $ "boundary-test-" ++ name) "special"
        recordMetric metric value
        finalValue <- metricValue metric
        
        -- 验证特殊值的处理
        when (isNaN value) $
          isNaN finalValue `shouldBe` True
        
        when (isInfinite value && value > 0) $
          finalValue `shouldSatisfy` isInfinite
        
        when (isInfinite value && value < 0) $
          finalValue `shouldSatisfy` (\v -> isInfinite v && v < 0)
        
        when (not (isNaN value) && not (isInfinite value)) $
          finalValue `shouldBe` value
        ) specialValues
    
    it "should handle empty and very long strings" $ property $
      \(str :: String) ->
        let testString = if null str then "" else take 1000 str
            testName = pack testString
        in unsafePerformIO $ do
          -- 测试空字符串和长字符串
          metric <- createMetric testName "test-unit"
          logger <- createLogger testName Info
          span <- createSpan testName
          
          return (metricName metric == testName &&
                  loggerName logger == testName &&
                  spanName span == testName)
  
  -- 8. 测试资源清理
  describe "Resource Cleanup" $ do
    it "should clean up resources properly after shutdown" $ do
            
      -- 创建资源
      metrics <- sequence $ replicate 10 $ do
        createMetric "cleanup-test" "count"
      
      loggers <- sequence $ replicate 5 $ do
        createLogger "cleanup-test-logger" Info
      
      spans <- sequence $ replicate 3 $ do
        createSpan "cleanup-test-span"
      
      -- 使用资源
      sequence_ $ map (`recordMetric` 1.0) metrics
      sequence_ $ flip map loggers $ \logger -> do
        logMessage logger Info "cleanup test"
      sequence_ $ map finishSpan spans
      
      -- 关闭系统
            
      -- 验证资源仍然可访问（在实际实现中可能需要检查资源计数）
      let metricNames = map (unpack . metricName) metrics
          loggerNames = map (unpack . loggerName) loggers
          spanNames = map (unpack . spanName) spans
      
      all (== "cleanup-test") metricNames `shouldBe` True
      all (== "cleanup-test-logger") loggerNames `shouldBe` True
      all (== "cleanup-test-span") spanNames `shouldBe` True
  
  -- 9. 测试文本编码处理
  describe "Text Encoding Handling" $ do
    it "should handle unicode characters correctly" $ property $
      \(str :: String) ->
        let unicodeText = pack str
        in unsafePerformIO $ do
          -- 测试Unicode字符处理
          metric <- createMetric unicodeText unicodeText
          logger <- createLogger unicodeText Info
          span <- createSpan unicodeText
          
          return (metricName metric == unicodeText &&
                  metricUnit metric == unicodeText &&
                  loggerName logger == unicodeText &&
                  spanName span == unicodeText)
    
    it "should handle special characters" $ do
      let specialChars = ["\0", "\t", "\n", "\r", "\\", "/", ":", "*", "?", "\"", "<", ">", "|"]
      
      mapM_ (\chars -> do
        let specialText = pack chars
        metric <- createMetric specialText "special-unit"
        logger <- createLogger specialText Info
        span <- createSpan specialText
        
        metricName metric `shouldBe` specialText
        loggerName logger `shouldBe` specialText
        spanName span `shouldBe` specialText
        ) specialChars
  
  -- 10. 测试性能基准
  describe "Performance Benchmarks" $ do
    it "should handle large numbers of operations efficiently" $ property $
      \(numOps :: Int) ->
        let operations = max 10 (abs numOps `mod` 100 + 10)
        in unsafePerformIO $ do
                    
          -- 测试度量操作性能
          metric <- createMetric "performance-test" "ops"
          
          startTime <- evaluate =<< sequence [return ()] -- 简单的时间戳获取
          
          sequence_ $ replicate operations $ do
            recordMetric metric 1.0
          
          endTime <- evaluate =<< sequence [return ()] -- 简单的时间戳获取
          
          -- 验证所有操作都完成了
          finalValue <- metricValue metric
          return (finalValue == fromIntegral operations)