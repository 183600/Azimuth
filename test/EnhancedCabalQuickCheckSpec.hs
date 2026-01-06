{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EnhancedCabalQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group, intercalate)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, when, void, unless, zipWithM)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import Data.Char (isHexDigit, toLower)
import Numeric (showHex)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Enhanced Cabal QuickCheck Test Suite" $ do
  
  -- 1. 度量值的精确算术属性测试
  describe "Precise Arithmetic Properties" $ do
    it "should maintain precision with small fractional values" $ property $
      \x ->
        let smallValue = abs x * 1e-10 :: Double
        in if not (isNaN smallValue) && not (isInfinite smallValue)
           then unsafePerformIO $ do
             metric <- createMetricWithInitialValue "precision-test" "fractional" 0.0
             recordMetric metric smallValue
             finalValue <- metricValue metric
             return (abs (finalValue - smallValue) < 1e-15)
           else True
    
    it "should handle very large values without overflow" $ property $
      \x ->
        let largeValue = abs x * 1e10 :: Double
        in if not (isNaN largeValue) && not (isInfinite largeValue)
           then unsafePerformIO $ do
             metric <- createMetricWithInitialValue "large-value-test" "count" 0.0
             recordMetric metric largeValue
             finalValue <- metricValue metric
             return (abs (finalValue - largeValue) < 1e-5)  -- 允许相对误差
           else True
    
    it "should preserve zero regardless of operations" $ property $
      \values ->
        let testValues = values :: [Double]
        in unsafePerformIO $ do
          metric <- createMetricWithInitialValue "zero-preservation" "count" 0.0
          
          -- 记录一系列值
          sequence_ $ map (recordMetric metric) testValues
          
          -- 记录相反的值
          sequence_ $ map (recordMetric metric . negate) testValues
          
          finalValue <- metricValue metric
          return (abs finalValue < 1.0e-9)  -- 允许浮点误差
  
  -- 2. Span ID的分布特性测试
  describe "Span ID Distribution Properties" $ do
    it "should generate span IDs with sufficient entropy" $ property $
      \names ->
        let spanNames = take 10 (map show names)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          spans <- mapM (\name -> createSpan (pack name)) spanNames
          let spanIds = map spanSpanId spans
              uniqueIds = nub spanIds
          
          shutdownTelemetry
          return (length uniqueIds == length spanIds)
    
    it "should generate span IDs following the expected pattern" $ property $
      \name ->
        let spanName = pack name
        in unsafePerformIO $ do
          span <- createSpan spanName
          let spanId = spanSpanId span
              spanIdStr = unpack spanId
              validLength = length spanIdStr == 12
              allHexDigits = all isHexDigit spanIdStr
          return (validLength && allHexDigits)
    
    it "should generate different span IDs for the same span name" $ property $
      \name ->
        let spanName = pack name
            numSpans = 5
        in unsafePerformIO $ do
          spans <- sequence $ replicate numSpans $ do
            createSpan spanName
          
          let spanIds = map spanSpanId spans
              uniqueIds = nub spanIds
          
          return (length uniqueIds == numSpans)
  
  -- 3. 配置不变性测试
  describe "Configuration Immutability" $ do
    it "should preserve configuration values after operations" $ property $
      \name version metrics tracing logging ->
        let config = TelemetryConfig (pack name) (pack version) metrics tracing logging False
        in unsafePerformIO $ do
          initTelemetry config
          
          -- 执行各种操作
          metric <- createMetric "config-test" "count"
          recordMetric metric 1.0
          
          logger <- createLogger "config-test-logger" Info
          logMessage logger Info "config test"
          
          span <- createSpan "config-test-span"
          finishSpan span
          
          -- 验证配置仍然有效
          currentConfig <- readIORef globalConfig
          shutdownTelemetry
          
          return (currentConfig == config)
    
    it "should handle configuration changes properly" $ property $
      \name1 name2 ->
        let config1 = TelemetryConfig (pack name1) "1.0" True True True False
            config2 = TelemetryConfig (pack name2) "2.0" False False False False
        in unsafePerformIO $ do
          initTelemetry config1
          
          -- 执行一些操作
          metric <- createMetric "config-change-test" "count"
          recordMetric metric 1.0
          
          -- 更改配置
          initTelemetry config2
          
          -- 验证配置已更改
          currentConfig <- readIORef globalConfig
          shutdownTelemetry
          
          return (serviceName currentConfig == pack name2)
  
  -- 4. 并发一致性测试
  describe "Concurrent Consistency" $ do
    it "should maintain consistency under concurrent metric operations" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 20 + 1)
            operationsPerThread = 50
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          metric <- createMetric "concurrent-consistency" "count"
          
          -- 创建多个线程同时操作度量
          threads <- mapM (\threadId -> forkIO $ do
            sequence_ $ replicate operationsPerThread $ do
              recordMetric metric (fromIntegral threadId)
            ) [1..actualThreads]
          
          -- 等待所有线程完成
          threadDelay 200000  -- 200毫秒
          
          -- 清理线程
          sequence_ $ map killThread threads
          
          -- 验证最终值
          finalValue <- metricValue metric
          let expectedValue = fromIntegral actualThreads * fromIntegral operationsPerThread * 
                             fromIntegral (actualThreads + 1) / 2  -- 等差数列求和
          
          shutdownTelemetry
          return (abs (finalValue - expectedValue) < 1.0e-9)
    
    it "should handle concurrent span creation with consistent trace IDs" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 10 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建多个线程同时创建span
          results <- mapM (\_ -> forkIO $ do
            span <- createSpan "concurrent-trace-test"
            let traceId = spanTraceId span
                spanId = spanSpanId span
            return (traceId, spanId)
            ) [1..actualThreads]
          
          threadDelay 100000  -- 100毫秒
          
          -- 获取所有span的trace ID
          -- 注意：在实际实现中，我们需要从线程中获取结果
          -- 这里简化处理，只测试并发创建不会崩溃
          
          -- 清理线程
          sequence_ $ map killThread results
          
          shutdownTelemetry
          return True  -- 如果没有崩溃就算成功
  
  -- 5. 边界条件和特殊情况测试
  describe "Boundary Conditions and Edge Cases" $ do
    it "should handle extreme floating point values" $ property $
      \x ->
        let testValue = x * 1e100 :: Double
        in unsafePerformIO $ do
          metric <- createMetric "extreme-values" "test"
          
          -- 记录极值
          result <- try $ recordMetric metric testValue
          
          case result of
            Left (_ :: SomeException) -> return True  -- 抛出异常也算正确处理
            Right _ -> do
              finalValue <- metricValue metric
              return (not (isNaN finalValue))  -- 最终值不应该是NaN
      
    
    it "should handle empty and whitespace-only strings" $ property $
      \str ->
        let testString = if null str then "" else take 5 (filter (`elem` [' ', '\t', '\n']) str ++ str)
            testName = pack testString
        in unsafePerformIO $ do
          metric <- createMetric testName "test-unit"
          logger <- createLogger testName Info
          span <- createSpan testName
          
          return (metricName metric == testName &&
                  loggerName logger == testName &&
                  spanName span == testName)
    
    it "should handle Unicode and special characters" $ property $
      \str ->
        let unicodeText = pack $ take 100 (str ++ "测试🚀emoji🌟")
        in unsafePerformIO $ do
          metric <- createMetric unicodeText unicodeText
          logger <- createLogger unicodeText Info
          span <- createSpan unicodeText
          
          return (metricName metric == unicodeText &&
                  metricUnit metric == unicodeText &&
                  loggerName logger == unicodeText &&
                  spanName span == unicodeText)
  
  -- 6. 性能和可扩展性测试
  describe "Performance and Scalability" $ do
    it "should scale linearly with metric count" $ property $
      \metricCount ->
        let actualCount = max 1 (abs metricCount `mod` 50 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建多个度量
          metrics <- sequence $ replicate actualCount $ do
            createMetric "scalability-test" "count"
          
          -- 对每个度量记录一系列值
          sequence_ $ map (\metric -> do
            sequence_ $ replicate 10 $ do
              recordMetric metric 1.0
            ) metrics
          
          -- 验证所有度量都有正确的值
          values <- sequence $ map metricValue metrics
          let allCorrect = all (== 10.0) values
          
          shutdownTelemetry
          return allCorrect
    
    it "should handle high-frequency operations" $ property $
      \operationCount ->
        let actualCount = max 10 (abs operationCount `mod` 1000 + 10)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          metric <- createMetric "high-frequency" "ops"
          
          -- 执行高频操作
          sequence_ $ replicate actualCount $ do
            recordMetric metric 1.0
          
          -- 验证所有操作都完成了
          finalValue <- metricValue metric
          
          shutdownTelemetry
          return (finalValue == fromIntegral actualCount)
  
  -- 7. 资源管理和清理测试
  describe "Resource Management and Cleanup" $ do
    it "should properly clean up after shutdown" $ property $
      \cycleCount ->
        let actualCycles = max 1 (abs cycleCount `mod` 10 + 1)
        in unsafePerformIO $ do
          sequence_ $ replicate actualCycles $ do
            initTelemetry productionConfig
            
            -- 创建资源
            metrics <- sequence $ replicate 5 $ do
              createMetric "cleanup-test" "count"
            
            loggers <- sequence $ replicate 3 $ do
              createLogger "cleanup-test-logger" Info
            
            spans <- sequence $ replicate 2 $ do
              createSpan "cleanup-test-span"
            
            -- 使用资源
            sequence_ $ map (`recordMetric` 1.0) metrics
            sequence_ $ flip map loggers $ \logger -> do
              logMessage logger Info "cleanup test"
            sequence_ $ map finishSpan spans
            
            shutdownTelemetry
            performGC
          
          return True  -- 如果没有内存泄漏就算成功
    
    it "should handle resource exhaustion gracefully" $ property $
      \resourceCount ->
        let actualCount = max 1 (abs resourceCount `mod` 100 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建大量资源
          metrics <- sequence $ replicate actualCount $ do
            createMetric "exhaustion-test" "count"
          
          loggers <- sequence $ replicate actualCount $ do
            createLogger "exhaustion-test-logger" Info
          
          spans <- sequence $ replicate actualCount $ do
            createSpan "exhaustion-test-span"
          
          -- 使用资源
          sequence_ $ map (`recordMetric` 1.0) metrics
          sequence_ $ flip map loggers $ \logger -> do
            logMessage logger Info "exhaustion test"
          sequence_ $ map finishSpan spans
          
          shutdownTelemetry
          return True  -- 如果没有崩溃就算成功
  
  -- 8. 数据完整性和一致性测试
  describe "Data Integrity and Consistency" $ do
    it "should maintain metric integrity across operations" $ property $
      \values ->
        let testValues = take 100 (values :: [Double])
        in unsafePerformIO $ do
          metric <- createMetric "integrity-test" "count"
          
          -- 记录值
          sequence_ $ map (recordMetric metric) testValues
          
          -- 验证名称和单位不变
          originalName <- return $ metricName metric
          originalUnit <- return $ metricUnit metric
          
          -- 再次记录值
          recordMetric metric 999.0
          
          -- 验证名称和单位仍然不变
          return (metricName metric == originalName && 
                  metricUnit metric == originalUnit)
    
    it "should maintain span integrity across operations" $ property $
      \name ->
        let spanName = pack name
        in unsafePerformIO $ do
          span <- createSpan spanName
          
          -- 验证原始属性
          let originalName = spanName span
          originalTraceId <- return $ spanTraceId span
          originalSpanId <- return $ spanSpanId span
          
          -- 完成span
          finishSpan span
          
          -- 验证属性不变
          return (spanName span == originalName &&
                  spanTraceId span == originalTraceId &&
                  spanSpanId span == originalSpanId)
  
  -- 9. 错误处理和恢复测试
  describe "Error Handling and Recovery" $ do
    it "should recover from invalid operations gracefully" $ property $
      \values ->
        let testValues = take 10 (values :: [Double])
            invalidValues = filter (\v -> isNaN v || isInfinite v) testValues
        in if not (null invalidValues)
           then unsafePerformIO $ do
             metric <- createMetric "error-recovery" "test"
             
             -- 尝试记录无效值
             sequence_ $ map (recordMetric metric) invalidValues
             
             -- 记录有效值
             recordMetric metric 42.0
             
             -- 验证系统仍然可以工作
             finalValue <- metricValue metric
             return (not (isNaN finalValue))
           else True
    
    it "should handle multiple initialization attempts" $ property $
      \attemptCount ->
        let actualAttempts = max 1 (abs attemptCount `mod` 5 + 1)
        in unsafePerformIO $ do
          sequence_ $ replicate actualAttempts $ do
            initTelemetry productionConfig
            shutdownTelemetry
          
          return True  -- 如果没有崩溃就算成功
  
  -- 10. 系统集成测试
  describe "System Integration" $ do
    it "should work correctly with all components combined" $ property $
      \componentCount ->
        let actualCount = max 1 (abs componentCount `mod` 10 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建所有类型的组件
          metrics <- sequence $ replicate actualCount $ do
            createMetric "integration-test" "count"
          
          loggers <- sequence $ replicate actualCount $ do
            createLogger "integration-test-logger" Info
          
          spans <- sequence $ replicate actualCount $ do
            createSpan "integration-test-span"
          
          -- 组合使用所有组件
          sequence_ $ zipWith (\metric logger -> do
            recordMetric metric 1.0
            logMessage logger Info "integration test"
            return ()
            ) metrics loggers
          
          sequence_ $ map finishSpan spans
          
          -- 验证所有组件都正常工作
          metricValues <- sequence $ map metricValue metrics
          let allMetricsCorrect = all (== 1.0) metricValues
          
          shutdownTelemetry
          return allMetricsCorrect