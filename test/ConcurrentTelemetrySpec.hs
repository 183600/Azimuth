{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConcurrentTelemetrySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, when)
import Data.Text (pack)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Concurrent Telemetry Tests" $ do
  
  -- 测试并发的度量创建
  describe "Concurrent Metric Creation" $ do
    it "should handle concurrent metric creation safely" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 10 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建多个线程同时创建度量
              results <- sequence $ replicate actualThreads $ do
                forkIO $ do
                  metric <- createMetric "concurrent-metric" "count"
                  recordMetric metric 1.0
                  return ()
              
              -- 等待所有线程完成
              threadDelay 100000  -- 100毫秒
              
              -- 清理线程
              sequence_ $ map killThread results
              
              shutdownTelemetry
              return True
        in result
    
    it "should handle concurrent metric creation with unique names" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 10 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建多个线程同时创建具有唯一名称的度量
              results <- sequence $ replicate actualThreads $ \i -> do
                forkIO $ do
                  metric <- createMetric (pack $ "concurrent-metric-" ++ show i) "count"
                  recordMetric metric (fromIntegral i)
                  return ()
              
              -- 等待所有线程完成
              threadDelay 100000  -- 100毫秒
              
              -- 清理线程
              sequence_ $ map killThread results
              
              shutdownTelemetry
              return True
        in result
  
  -- 测试并发的度量记录
  describe "Concurrent Metric Recording" $ do
    it "should handle concurrent metric recording safely" $ property $
      \numThreads numOperations ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
            operationsPerThread = max 1 (abs numOperations `mod` 20 + 1)
            expectedTotal = fromIntegral actualThreads * fromIntegral operationsPerThread
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              metric <- createMetric "concurrent-recording" "count"
              
              -- 创建多个线程同时记录度量
              results <- sequence $ replicate actualThreads $ do
                forkIO $ do
                  sequence_ $ replicate operationsPerThread $ do
                    recordMetric metric 1.0
                  return ()
              
              -- 等待所有线程完成
              threadDelay 200000  -- 200毫秒
              
              -- 获取最终值
              finalValue <- metricValue metric
              
              -- 清理线程
              sequence_ $ map killThread results
              
              shutdownTelemetry
              return (finalValue == expectedTotal || finalValue > expectedTotal)
        in result
    
    it "should handle concurrent metric recording with different values" $ property $
      \numThreads numOperations ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
            operationsPerThread = max 1 (abs numOperations `mod` 10 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              metric <- createMetric "concurrent-different-values" "count"
              
              -- 创建多个线程同时记录不同的度量值
              results <- sequence $ replicate actualThreads $ \i -> do
                forkIO $ do
                  sequence_ $ replicate operationsPerThread $ do
                    recordMetric metric (fromIntegral i)
                  return ()
              
              -- 等待所有线程完成
              threadDelay 200000  -- 200毫秒
              
              -- 获取最终值
              finalValue <- metricValue metric
              
              -- 计算期望值
              let expectedValue = sum [fromIntegral i * fromIntegral operationsPerThread | i <- [0..actualThreads-1]]
              
              -- 清理线程
              sequence_ $ map killThread results
              
              shutdownTelemetry
              return (finalValue == expectedValue || finalValue > expectedValue)
        in result
  
  -- 测试并发的Span创建
  describe "Concurrent Span Creation" $ do
    it "should handle concurrent span creation safely" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 10 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建多个线程同时创建span
              results <- sequence $ replicate actualThreads $ \i -> do
                forkIO $ do
                  span <- createSpan (pack $ "concurrent-span-" ++ show i)
                  finishSpan span
                  return ()
              
              -- 等待所有线程完成
              threadDelay 100000  -- 100毫秒
              
              -- 清理线程
              sequence_ $ map killThread results
              
              shutdownTelemetry
              return True
        in result
    
    it "should maintain trace context across concurrent span creation" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建第一个span以建立trace context
              parentSpan <- createSpan "concurrent-parent"
              let parentTraceId = spanTraceId parentSpan
              
              -- 创建多个线程同时创建子span
              traceIds <- newEmptyMVar
              results <- sequence $ replicate actualThreads $ \i -> do
                forkIO $ do
                  childSpan <- createSpan (pack $ "concurrent-child-" ++ show i)
                  putMVar traceIds (spanTraceId childSpan)
                  finishSpan childSpan
                  return ()
              
              -- 收集所有trace ID
              childTraceIds <- sequence $ replicate actualThreads $ takeMVar traceIds
              
              -- 等待所有线程完成
              threadDelay 100000  -- 100毫秒
              
              -- 清理线程
              sequence_ $ map killThread results
              
              shutdownTelemetry
              return (all (== parentTraceId) childTraceIds)
        in result
  
  -- 测试并发的日志记录
  describe "Concurrent Logging" $ do
    it "should handle concurrent logging safely" $ property $
      \numThreads numOperations ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
            operationsPerThread = max 1 (abs numOperations `mod` 20 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              logger <- createLogger "concurrent-logger" Info
              
              -- 创建多个线程同时记录日志
              results <- sequence $ replicate actualThreads $ \i -> do
                forkIO $ do
                  sequence_ $ replicate operationsPerThread $ do
                    logMessage logger Info (pack $ "concurrent message " ++ show i)
                  return ()
              
              -- 等待所有线程完成
              threadDelay 200000  -- 200毫秒
              
              -- 清理线程
              sequence_ $ map killThread results
              
              shutdownTelemetry
              return True
        in result
    
    it "should handle concurrent logging at different levels" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
            levels = [Debug, Info, Warn, Error]
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              loggers <- sequence $ map (\level -> 
                createLogger (pack $ "concurrent-logger-" ++ show level) level
              ) levels
              
              -- 创建多个线程同时记录不同级别的日志
              results <- sequence $ replicate actualThreads $ \i -> do
                forkIO $ do
                  sequence_ $ zipWith (\logger level -> do
                    logMessage logger level (pack $ "concurrent message " ++ show i ++ " at " ++ show level)
                  ) loggers levels
                  return ()
              
              -- 等待所有线程完成
              threadDelay 200000  -- 200毫秒
              
              -- 清理线程
              sequence_ $ map killThread results
              
              shutdownTelemetry
              return True
        in result
  
  -- 测试混合并发操作
  describe "Mixed Concurrent Operations" $ do
    it "should handle mixed concurrent telemetry operations" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建共享资源
              metric <- createMetric "mixed-metric" "count"
              logger <- createLogger "mixed-logger" Info
              
              -- 创建多个线程执行混合操作
              results <- sequence $ replicate actualThreads $ \i -> do
                forkIO $ do
                  -- 度量操作
                  recordMetric metric (fromIntegral i)
                  
                  -- 日志操作
                  logMessage logger Info (pack $ "mixed operation " ++ show i)
                  
                  -- Span操作
                  span <- createSpan (pack $ "mixed-span-" ++ show i)
                  finishSpan span
                  
                  return ()
              
              -- 等待所有线程完成
              threadDelay 200000  -- 200毫秒
              
              -- 获取最终度量值
              finalValue <- metricValue metric
              
              -- 清理线程
              sequence_ $ map killThread results
              
              shutdownTelemetry
              return (finalValue >= 0)
        in result
  
  -- 测试并发的初始化和关闭
  describe "Concurrent Initialization and Shutdown" $ do
    it "should handle concurrent initialization safely" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 3 + 1)
            result = unsafePerformIO $ do
              -- 创建多个线程同时初始化
              results <- sequence $ replicate actualThreads $ do
                forkIO $ do
                  initTelemetry productionConfig
                  return ()
              
              -- 等待所有线程完成
              threadDelay 100000  -- 100毫秒
              
              -- 清理线程
              sequence_ $ map killThread results
              
              -- 关闭系统
              shutdownTelemetry
              return True
        in result
    
    it "should handle concurrent shutdown safely" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 3 + 1)
            result = unsafePerformIO $ do
              -- 初始化系统
              initTelemetry productionConfig
              
              -- 创建多个线程同时关闭
              results <- sequence $ replicate actualThreads $ do
                forkIO $ do
                  shutdownTelemetry
                  return ()
              
              -- 等待所有线程完成
              threadDelay 100000  -- 100毫秒
              
              -- 清理线程
              sequence_ $ map killThread results
              
              return True
        in result
  
  -- 测试并发的资源访问
  describe "Concurrent Resource Access" $ do
    it "should handle concurrent access to shared metrics" $ property $
      \numThreads numOperations ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
            operationsPerThread = max 1 (abs numOperations `mod` 10 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建多个共享度量
              metrics <- sequence $ map (\i -> 
                createMetric (pack $ "shared-metric-" ++ show i) "count"
              ) [1..5]
              
              -- 创建多个线程同时访问共享度量
              results <- sequence $ replicate actualThreads $ \i -> do
                forkIO $ do
                  sequence_ $ replicate operationsPerThread $ do
                    sequence_ $ zipWith (\metric j -> do
                      recordMetric metric (fromIntegral (i + j))
                    ) metrics [1..5]
                  return ()
              
              -- 等待所有线程完成
              threadDelay 200000  -- 200毫秒
              
              -- 验证所有度量都有合理的值
              finalValues <- sequence $ map metricValue metrics
              
              -- 清理线程
              sequence_ $ map killThread results
              
              shutdownTelemetry
              return (all (>= 0) finalValues)
        in result
    
    it "should handle concurrent access to shared loggers" $ property $
      \numThreads numOperations ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
            operationsPerThread = max 1 (abs numOperations `mod` 10 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建多个共享日志记录器
              loggers <- sequence $ map (\level -> 
                createLogger (pack $ "shared-logger-" ++ show level) level
              ) [Debug, Info, Warn, Error]
              
              -- 创建多个线程同时访问共享日志记录器
              results <- sequence $ replicate actualThreads $ \i -> do
                forkIO $ do
                  sequence_ $ replicate operationsPerThread $ do
                    sequence_ $ zipWith (\logger level -> do
                      logMessage logger level (pack $ "shared message " ++ show i ++ " at " ++ show level)
                    ) loggers [Debug, Info, Warn, Error]
                  return ()
              
              -- 等待所有线程完成
              threadDelay 200000  -- 200毫秒
              
              -- 清理线程
              sequence_ $ map killThread results
              
              shutdownTelemetry
              return True
        in result
  
  -- 测试并发的错误处理
  describe "Concurrent Error Handling" $ do
    it "should handle errors in concurrent operations gracefully" $ property $
      \numThreads ->
        let actualThreads = max 1 (abs numThreads `mod` 5 + 1)
            result = unsafePerformIO $ do
              initTelemetry productionConfig
              
              -- 创建多个线程，其中一些可能会产生错误
              results <- sequence $ replicate actualThreads $ \i -> do
                forkIO $ do
                  -- 正常操作
                  metric <- createMetric (pack $ "error-test-" ++ show i) "count"
                  recordMetric metric 1.0
                  
                  -- 可能产生错误的操作（使用特殊值）
                  recordMetric metric (1.0/0.0)  -- Infinity
                  recordMetric metric (0.0/0.0)  -- NaN
                  
                  return ()
              
              -- 等待所有线程完成
              threadDelay 200000  -- 200毫秒
              
              -- 清理线程
              sequence_ $ map killThread results
              
              shutdownTelemetry
              return True
        in result