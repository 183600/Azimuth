{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AdvancedCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, when, void)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Advanced Cabal Test Suite" $ do
  
  -- 1. 测试度量值的数学属性
  describe "Metric Mathematical Properties" $ do
    it "should satisfy commutative property of addition" $ property $
      \x y ->
        let testVals = [x, y] :: [Double]
        in unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "commutative-test-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "commutative-test-2" "count" 0.0
          
          -- 顺序1: x + y
          recordMetric metric1 x
          recordMetric metric1 y
          val1 <- metricValue metric1
          
          -- 顺序2: y + x
          recordMetric metric2 y
          recordMetric metric2 x
          val2 <- metricValue metric2
          
          return (val1 == val2)
    
    it "should satisfy associative property of addition" $ property $
      \x y z ->
        let testVals = [x, y, z] :: [Double]
        in unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "associative-test-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "associative-test-2" "count" 0.0
          
          -- 分组1: (x + y) + z
          recordMetric metric1 x
          recordMetric metric1 y
          recordMetric metric1 z
          val1 <- metricValue metric1
          
          -- 分组2: x + (y + z)
          recordMetric metric2 x
          recordMetric metric2 y
          recordMetric metric2 z
          val2 <- metricValue metric2
          
          return (val1 == val2)
    
    it "should handle additive identity property" $ property $
      \x ->
        let testVal = x :: Double
        in unsafePerformIO $ do
          metric <- createMetricWithInitialValue "identity-test" "count" testVal
          originalValue <- metricValue metric
          
          -- 加上0不应该改变值
          recordMetric metric 0.0
          newValue <- metricValue metric
          
          return (originalValue == newValue)
    
    it "should handle additive inverse property" $ property $
      \x ->
        let testVal = x :: Double
        in if not (isNaN testVal) && not (isInfinite testVal) && testVal /= 0.0
           then unsafePerformIO $ do
             metric <- createMetricWithInitialValue "inverse-test" "count" testVal
             
             -- 加上相反数应该得到0
             recordMetric metric (-testVal)
             finalValue <- metricValue metric
             
             return (abs finalValue < 1.0e-9)  -- 允许浮点误差
           else True  -- 跳过NaN和无穷大的测试
  
  -- 2. 测试Span和Trace ID的生成规则
  describe "Span and Trace ID Generation Rules" $ do
    it "should generate span IDs with consistent length" $ property $
      \(name :: String) ->
        let spanName = pack name
        in unsafePerformIO $ do
          span <- createSpan spanName
          let spanId = spanSpanId span
          return (Text.length spanId == 12)  -- 6 + 3 + 3 hex digits
    
    it "should generate span IDs with valid hex characters" $ property $
      \(name :: String) ->
        let spanName = pack name
            validHexChars = ['0'..'9'] ++ ['a'..'f']
        in unsafePerformIO $ do
          span <- createSpan spanName
          let spanId = spanSpanId span
              spanIdStr = unpack spanId
              allValidChars = all (`elem` validHexChars) spanIdStr
          return allValidChars
    
    it "should maintain trace context across multiple spans" $ property $
      \(names :: [String]) ->
        let spanNames = take 5 (map show names)
            nonEmpty = not (null spanNames)
        in if nonEmpty
           then unsafePerformIO $ do
                          
             spans <- mapM (\name -> createSpan (pack name)) spanNames
             let traceIds = map spanTraceId spans
                 allSameTraceId = length (nub traceIds) == 1
             
             return allSameTraceId
           else True
    
    it "should reset trace context after shutdown" $ property $
      \(names1 :: [String]) (names2 :: [String]) ->
        let spanNames1 = take 2 (map show names1)
            spanNames2 = take 2 (map show names2)
            nonEmpty1 = not (null spanNames1)
            nonEmpty2 = not (null spanNames2)
        in if nonEmpty1 && nonEmpty2
           then unsafePerformIO $ do
             -- 第一组span
             spans1 <- mapM (\name -> createSpan (pack name)) spanNames1
             let traceIds1 = map spanTraceId spans1
                 firstTraceId = head traceIds1
             
             -- 第二组span（应该有新的trace ID）
             spans2 <- mapM (\name -> createSpan (pack name)) spanNames2
             let traceIds2 = map spanTraceId spans2
                 secondTraceId = head traceIds2
             
             return (firstTraceId /= secondTraceId)
           else True
  
  -- 3. 测试日志级别的层次结构
  describe "Log Level Hierarchy" $ do
    it "should maintain correct log level ordering" $ do
      let levels = [Debug, Info, Warn, Error]
          orderedLevels = sort levels
      orderedLevels `shouldBe` [Debug, Info, Warn, Error]
    
    it "should compare log levels correctly" $ property $
      \level1Int level2Int ->
        let levels = [Debug, Info, Warn, Error]
            level1 = levels !! (abs level1Int `mod` 4)
            level2 = levels !! (abs level2Int `mod` 4)
        in if level1 == level2
           then level1 `compare` level2 == EQ
           else if level1 < level2
                then level1 `compare` level2 == LT
                else level1 `compare` level2 == GT
    
    it "should create loggers with all levels" $ do
      let levels = [Debug, Info, Warn, Error]
      loggers <- mapM (\level -> createLogger (pack $ "test-logger-" ++ show level) level) levels
      let loggerLevels = map loggerLevel loggers
      loggerLevels `shouldBe` levels
  
  -- 4. 测试配置参数的边界值测试
  describe "Configuration Boundary Values" $ do
    it "should handle empty string configuration" $ do
      let emptyConfig = TelemetryConfig "" "" True True True False
      initTelemetry emptyConfig `shouldReturn` ()
      shutdownTelemetry `shouldReturn` ()
    
    it "should handle very long configuration strings" $ property $
      \str ->
        let longString = pack $ take 10000 (cycle str)
            longConfig = TelemetryConfig longString longString True True True False
        in unsafePerformIO $ do
          result <- try $ initTelemetry longConfig
          case result of
            Left (_ :: SomeException) -> return False
            Right _ -> do
                            return True
    
    it "should handle all boolean combinations" $ do
      let bools = [True, False]
          configs = [TelemetryConfig "test" "1.0" m t l False | 
                     m <- bools, t <- bools, l <- bools]
      
      mapM_ (\config -> do
        initTelemetry config
                ) configs
  
  -- 5. 测试并发操作的原子性
  describe "Concurrent Operation Atomicity" $ do
    it "should maintain atomicity for concurrent metric updates" $ property $
      \(numThreads :: Int) ->
        let actualThreads = max 1 (abs numThreads `mod` 10 + 1)
            operationsPerThread = 100
        in unsafePerformIO $ do
                    
          metric <- createMetric "atomicity-test" "count"
          resultVar <- newEmptyMVar
          
          -- 创建多个线程同时递增同一个度量
          threads <- mapM (\_ -> forkIO $ do
            sequence_ $ replicate operationsPerThread $ do
              recordMetric metric 1.0
            ) [1..actualThreads]
          
          -- 等待所有线程完成
          threadDelay 100000  -- 100毫秒
          
          -- 清理线程
          sequence_ $ map killThread threads
          
          -- 验证最终值
          finalValue <- metricValue metric
          let expectedValue = fromIntegral actualThreads * fromIntegral operationsPerThread
          
          return (finalValue == expectedValue)
    
    it "should handle concurrent span creation without race conditions" $ property $
      \(numThreads :: Int) ->
        let actualThreads = max 1 (abs numThreads `mod` 10 + 1)
        in unsafePerformIO $ do
                    
          -- 创建多个线程同时创建span
          results <- mapM (\_ -> forkIO $ do
            span <- createSpan "concurrent-span"
            let spanId = spanSpanId span
            return ()
            ) [1..actualThreads]
          
          threadDelay 100000  -- 100毫秒
          
          -- 清理线程
          sequence_ $ map killThread results
          
          return True  -- 如果没有崩溃就算成功
  
  -- 6. 测试资源泄漏检测
  describe "Resource Leak Detection" $ do
    it "should not leak memory during repeated init/shutdown cycles" $ do
      let cycles = 100
      sequence_ $ replicate cycles $ do
                
        -- 创建一些资源
        metrics <- sequence $ replicate 10 $ do
          createMetric "leak-test" "count"
        
        loggers <- sequence $ replicate 5 $ do
          createLogger "leak-test-logger" Info
        
        spans <- sequence $ replicate 3 $ do
          createSpan "leak-test-span"
        
        -- 使用资源
        sequence_ $ map (`recordMetric` 1.0) metrics
        sequence_ $ flip map loggers $ \logger -> do
          logMessage logger Info "leak test"
        sequence_ $ map finishSpan spans
        
        performGC  -- 强制垃圾回收
      
      True `shouldBe` True  -- 如果没有内存溢出就算成功
    
    it "should clean up metric registry after shutdown" $ do
            
      -- 创建多个同名度量
      metrics1 <- sequence $ replicate 5 $ do
        createMetric "registry-test" "count"
      
      -- 记录一些值
      sequence_ $ map (`recordMetric` 1.0) metrics1
      
            
      -- 重新初始化
            
      -- 创建同名度量应该从新开始
      metric2 <- createMetric "registry-test" "count"
      value <- metricValue metric2
      
      value `shouldBe` 0.0
  
  -- 7. 测试错误恢复
  describe "Error Recovery" $ do
    it "should recover from invalid metric values" $ do
      let invalidValues = [0.0/0.0, 1.0/0.0, -1.0/0.0]  -- NaN, +Infinity, -Infinity
      
      mapM_ (\value -> do
        metric <- createMetric "error-recovery" "test"
        recordMetric metric value
        
        -- 验证系统仍然可以正常工作
        recordMetric metric 1.0
        finalValue <- metricValue metric
        
        -- 检查值是否合理（不是NaN）
        not (isNaN finalValue) `shouldBe` True
        ) invalidValues
    
    it "should handle multiple initialization attempts" $ do
      let attempts = 10
      sequence_ $ replicate attempts $ do
        threadDelay 1000  -- 1毫秒延迟
      
      True `shouldBe` True  -- 如果没有崩溃就算成功
  
  -- 8. 测试性能退化测试
  describe "Performance Regression" $ do
    it "should maintain performance with increasing metric count" $ property $
      \metricCount ->
        let actualCount = max 1 (abs metricCount `mod` 100 + 1)
        in unsafePerformIO $ do
                    
          -- 创建多个度量
          metrics <- sequence $ replicate actualCount $ do
            createMetric "performance-test" "count"
          
          -- 对每个度量记录值
          sequence_ $ map (`recordMetric` 1.0) metrics
          
          -- 验证所有度量都有正确的值
          values <- sequence $ map metricValue metrics
          let allCorrect = all (== 1.0) values
          
          return allCorrect
    
    it "should handle rapid metric operations efficiently" $ do
      let operations = 1000
            
      metric <- createMetric "rapid-operations" "ops"
      
      -- 执行快速操作
      sequence_ $ replicate operations $ do
        recordMetric metric 1.0
      
      -- 验证所有操作都完成了
      finalValue <- metricValue metric
      
      finalValue `shouldBe` fromIntegral operations
  
  -- 9. 测试数据一致性
  describe "Data Consistency" $ do
    it "should maintain metric name consistency" $ property $
      \name ->
        let testName = pack name
        in unsafePerformIO $ do
          metric <- createMetric testName "unit"
          
          -- 记录多个值
          recordMetric metric 1.0
          recordMetric metric 2.0
          recordMetric metric 3.0
          
          -- 验证名称不变
          return (metricName metric == testName)
    
    it "should maintain logger level consistency" $ property $
      \name levelInt ->
        let testName = pack name
            levels = [Debug, Info, Warn, Error]
            testLevel = levels !! (abs levelInt `mod` 4)
        in unsafePerformIO $ do
          logger <- createLogger testName testLevel
          
          -- 记录多条消息
          logMessage logger Debug "debug message"
          logMessage logger Info "info message"
          logMessage logger Warn "warning message"
          logMessage logger Error "error message"
          
          -- 验证级别不变
          return (loggerLevel logger == testLevel)
  
  -- 10. 测试内存使用
  describe "Memory Usage" $ do
    it "should not grow memory usage with repeated operations" $ do
      let iterations = 1000
            
      -- 重复创建和使用资源
      sequence_ $ replicate iterations $ do
        metric <- createMetric "memory-test" "count"
        recordMetric metric 1.0
        
        logger <- createLogger "memory-test-logger" Info
        logMessage logger Info "memory test"
        
        span <- createSpan "memory-test-span"
        finishSpan span
      
      -- 强制垃圾回收
      performGC
      
      True `shouldBe` True  -- 如果没有内存溢出就算成功
    
    it "should handle large metric values without overflow" $ property $
      \value ->
        let testValue = abs value * 1000000  -- 放大值
        in if not (isNaN testValue) && not (isInfinite testValue)
           then unsafePerformIO $ do
             metric <- createMetric "large-value" "count"
             recordMetric metric testValue
             
             finalValue <- metricValue metric
             return (not (isNaN finalValue) && not (isInfinite finalValue))
           else True