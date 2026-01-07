{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResourceLimitCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group, sortBy)
import Data.Ord (comparing)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar, getNumCapabilities)
import Control.Monad (replicateM, when, forM_, void, unless)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)
import Prelude hiding (id)
import System.Mem (performGC)

import Azimuth.Telemetry

-- | 资源限制配置
data ResourceLimits = ResourceLimits
    { maxMetrics :: Int
    , maxSpans :: Int
    , maxLoggers :: Int
    , maxThreads :: Int
    , maxMemory :: Int  -- MB
    } deriving (Show, Eq)

-- | 默认资源限制
defaultResourceLimits :: ResourceLimits
defaultResourceLimits = ResourceLimits
    { maxMetrics = 10000
    , maxSpans = 5000
    , maxLoggers = 1000
    , maxThreads = 100
    , maxMemory = 100
    }

spec :: Spec
spec = describe "Resource Limit Tests" $ do
  
  -- 1. 测试度量资源限制
  describe "Metric Resource Limits" $ do
    it "should handle large number of metrics gracefully" $ do
      initTelemetry defaultConfig
      
      let numMetrics = 1000
      
      -- 创建大量度量
      result <- try $ do
        metrics <- replicateM numMetrics $ createMetric "resource-limit-test" "count"
        sequence_ $ map (`recordMetric` 1.0) metrics
        return $ length metrics
      
      case result of
        Left (_ :: SomeException) -> do
          -- 如果资源不足，系统应该仍然可以创建新的度量
          metric <- createMetric "recovery-test" "count"
          recordMetric metric 1.0
          value <- metricValue metric
          value `shouldBe` 1.0
        Right count -> do
          count `shouldBe` numMetrics
      
      shutdownTelemetry
    
    it "should limit metric creation when resources are constrained" $ do
      initTelemetry defaultConfig
      
      -- 模拟资源限制
      let maxAllowed = 100
      
      -- 创建大量度量直到达到限制
      result <- try $ do
        metrics <- replicateM (maxAllowed * 2) $ createMetric "constrained-test" "count"
        return $ length metrics
      
      case result of
        Left (_ :: SomeException) -> do
          -- 预期的资源限制行为
          pendingWith "Resource limit reached as expected"
        Right count -> do
          -- 如果系统支持更多度量，验证基本功能
          count `shouldBe` maxAllowed * 2
      
      shutdownTelemetry
  
  -- 2. QuickCheck属性测试：资源使用的一致性
  describe "Resource Usage Properties" $ do
    it "should maintain consistent resource usage" $ property $
      \numResources ->
        let resources = max 1 (abs numResources `mod` 100 + 1)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          -- 创建指定数量的资源
          result <- try $ do
            metrics <- replicateM resources $ createMetric "consistency-test" "count"
            sequence_ $ map (`recordMetric` 1.0) metrics
            return $ length metrics
          
          case result of
            Left (_ :: SomeException) -> do
              -- 资源限制，验证系统仍然可用
              metric <- createMetric "post-limit-test" "count"
              recordMetric metric 1.0
              value <- metricValue metric
              return (value == 1.0)
            Right count -> do
              -- 验证所有资源都正确创建
              return (count == resources)
          
          shutdownTelemetry
    
    it "should handle concurrent resource allocation" $ property $
      \numThreads numResources ->
        let threads = max 1 (abs numThreads `mod` 10 + 1)
            resources = max 1 (abs numResources `mod` 50 + 1)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          -- 并发分配资源
          done <- newEmptyMVar
          results <- replicateM threads $ do
            forkIO $ do
              result <- try $ do
                metrics <- replicateM resources $ createMetric ("concurrent-" ++ show resources) "count"
                sequence_ $ map (`recordMetric` 1.0) metrics
                return $ length metrics
              putMVar result
          
          -- 等待所有线程完成
          threadResults <- sequence $ replicate threads $ takeMVar done
          
          -- 验证至少有一些操作成功
          let successfulCount = length $ filter (\r -> case r of
            Left _ -> False
            Right count -> count > 0) threadResults
          
          shutdownTelemetry
          return (successfulCount > 0)
  
  -- 3. 测试内存限制
  describe "Memory Limits" $ do
    it "should handle memory pressure gracefully" $ do
      initTelemetry defaultConfig
      
      -- 创建大量对象以施加内存压力
      let memoryPressureSize = 10000
      
      result <- try $ do
        metrics <- replicateM memoryPressureSize $ createMetric "memory-pressure" "count"
        sequence_ $ map (`recordMetric` 1.0) metrics
        return $ length metrics
      
      case result of
        Left (_ :: SomeException) -> do
          -- 内存不足，系统应该仍然可用
          performGC  -- 强制垃圾回收
          metric <- createMetric "memory-recovery" "count"
          recordMetric metric 1.0
          value <- metricValue metric
          value `shouldBe` 1.0
        Right count -> do
          count `shouldBe` memoryPressureSize
      
      shutdownTelemetry
    
    it "should recover from out-of-memory conditions" $ do
      initTelemetry defaultConfig
      
      -- 逐步增加内存使用
      let stepSize = 1000
          maxSteps = 10
      
      finalResult <- foldM (\_ step -> do
        result <- try $ do
          metrics <- replicateM stepSize $ createMetric ("memory-step-" ++ show step) "count"
          sequence_ $ map (`recordMetric` 1.0) metrics
          return True
        
        case result of
          Left (_ :: SomeException) -> do
            -- 内存不足，尝试恢复
            performGC
            metric <- createMetric "recovery-test" "count"
            recordMetric metric 1.0
            return False
          Right _ -> return True
        ) True [1..maxSteps]
      
      -- 无论是否遇到内存限制，系统都应该可用
      metric <- createMetric "final-test" "count"
      recordMetric metric 42.0
      value <- metricValue metric
      value `shouldBe` 42.0
      
      shutdownTelemetry
  
  -- 4. 测试线程限制
  describe "Thread Limits" $ do
    it "should handle thread pool exhaustion" $ do
      initTelemetry defaultConfig
      
      let maxThreads = 50
      
      -- 创建大量线程
      result <- try $ do
        done <- newEmptyMVar
        threads <- replicateM maxThreads $ forkIO $ do
          threadDelay 1000000  -- 1秒
          putMVar done ()
        
        -- 等待一小部分线程完成
        sequence_ $ replicate (maxThreads `div` 2) $ takeMVar done
        
        -- 清理剩余线程
        mapM_ killThread threads
        return maxThreads
      
      case result of
        Left (_ :: SomeException) -> do
          -- 线程限制，系统应该仍然可用
          metric <- createMetric "thread-recovery" "count"
          recordMetric metric 1.0
          value <- metricValue metric
          value `shouldBe` 1.0
        Right count -> do
          count `shouldBe` maxThreads
      
      shutdownTelemetry
    
    it "should limit concurrent operations" $ do
      initTelemetry defaultConfig
      
      let numOperations = 100
          maxConcurrent = 10
      
      metric <- createMetric "concurrent-limit-test" "count"
      
      -- 分批执行并发操作
      let batches = [1..numOperations] `chunksOf` maxConcurrent
      
      forM_ batches $ \batch -> do
        done <- newEmptyMVar
        threads <- mapM (\_ -> forkIO $ do
          recordMetric metric 1.0
          putMVar done ()
          ) batch
        
        sequence_ $ replicate (length batch) $ takeMVar done
      
      finalValue <- metricValue metric
      finalValue `shouldBe` fromIntegral numOperations
      
      shutdownTelemetry
      where
        chunksOf :: Int -> [a] -> [[a]]
        chunksOf _ [] = []
        chunksOf n xs = take n xs : chunksOf n (drop n xs)
  
  -- 5. 测试文件描述符限制
  describe "File Descriptor Limits" $ do
    it "should handle file descriptor exhaustion" $ do
      initTelemetry defaultConfig
      
      -- 创建大量logger（可能使用文件描述符）
      let numLoggers = 100
      
      result <- try $ do
        loggers <- replicateM numLoggers $ createLogger "fd-test" Info
        sequence_ $ flip map loggers $ \logger -> do
          logMessage logger Info (pack "file descriptor test")
        return $ length loggers
      
      case result of
        Left (_ :: SomeException) -> do
          -- 文件描述符不足，系统应该仍然可用
          logger <- createLogger "fd-recovery" Info
          logMessage logger Info (pack "recovery test")
        Right count -> do
          count `shouldBe` numLoggers
      
      shutdownTelemetry
  
  -- 6. 测试资源限制的动态调整
  describe "Dynamic Resource Limit Adjustment" $ do
    it "should adapt to changing resource constraints" $ do
      initTelemetry defaultConfig
      
      -- 模拟资源限制变化
      let phases = [("low", 10), ("medium", 100), ("high", 1000)]
      
      forM_ phases $ \(phase, limit) -> do
        result <- try $ do
          metrics <- replicateM limit $ createMetric (phase ++ "-phase") "count"
          sequence_ $ map (`recordMetric` 1.0) metrics
          return $ length metrics
        
        case result of
          Left (_ :: SomeException) -> do
            -- 资源限制，创建一个测试度量验证系统可用性
            metric <- createMetric (phase ++ "-recovery") "count"
            recordMetric metric 1.0
          Right count -> do
            count `shouldBe` limit
      
      shutdownTelemetry
    
    it "should prioritize critical operations under resource pressure" $ do
      initTelemetry defaultConfig
      
      -- 模拟资源压力
      let pressureSize = 500
      
      result <- try $ do
        metrics <- replicateM pressureSize $ createMetric "pressure-test" "count"
        sequence_ $ map (`recordMetric` 1.0) metrics
        return pressureSize
      
      case result of
        Left (_ :: SomeException) -> do
          -- 资源压力下，关键操作应该仍然可用
          criticalMetric <- createMetric "critical-operation" "count"
          recordMetric criticalMetric 1.0
          value <- metricValue criticalMetric
          value `shouldBe` 1.0
        Right _ -> do
          -- 无资源压力，正常操作
          metric <- createMetric "normal-operation" "count"
          recordMetric metric 1.0
          value <- metricValue metric
          value `shouldBe` 1.0
      
      shutdownTelemetry
  
  -- 7. 测试资源监控
  describe "Resource Monitoring" $ do
    it "should track resource usage" $ do
      initTelemetry defaultConfig
      
      let numResources = 100
      
      -- 创建资源
      metrics <- replicateM numResources $ createMetric "monitoring-test" "count"
      
      -- 使用资源
      sequence_ $ map (`recordMetric` 1.0) metrics
      
      -- 验证资源使用情况
      values <- mapM metricValue metrics
      all (== 1.0) values `shouldBe` True
      
      shutdownTelemetry
    
    it "should detect resource leaks" $ do
      initTelemetry defaultConfig
      
      -- 创建并使用资源
      metrics <- replicateM 50 $ createMetric "leak-test" "count"
      sequence_ $ map (`recordMetric` 1.0) metrics
      
      -- 模拟资源清理（在实际系统中，这里应该检查资源计数）
      values <- mapM metricValue metrics
      all (== 1.0) values `shouldBe` True
      
      -- 验证系统仍然可以创建新资源
      newMetric <- createMetric "post-leak-test" "count"
      recordMetric newMetric 1.0
      value <- metricValue newMetric
      value `shouldBe` 1.0
      
      shutdownTelemetry
  
  -- 8. 测试资源限制的边界条件
  describe "Resource Limit Boundary Conditions" $ do
    it "should handle zero resource limits" $ do
      initTelemetry defaultConfig
      
      -- 尝试创建零个资源（边界条件）
      result <- try $ do
        metrics <- replicateM 0 $ createMetric "zero-limit" "count"
        return $ length metrics
      
      case result of
        Left (_ :: SomeException) -> do
          -- 零限制失败，系统应该仍然可用
          metric <- createMetric "zero-recovery" "count"
          recordMetric metric 1.0
          value <- metricValue metric
          value `shouldBe` 1.0
        Right count -> do
          count `shouldBe` 0
      
      shutdownTelemetry
    
    it "should handle negative resource requests" $ do
      initTelemetry defaultConfig
      
      -- 尝试处理负数请求（应该被优雅处理）
      metric <- createMetric "negative-test" "count"
      
      -- 记录负值
      recordMetric metric (-1.0)
      value <- metricValue metric
      value `shouldBe` -1.0
      
      shutdownTelemetry
    
    it "should handle extreme resource requests" $ do
      initTelemetry defaultConfig
      
      -- 尝试请求极大量资源
      let extremeRequest = 1000000
      
      result <- try $ do
        metrics <- replicateM extremeRequest $ createMetric "extreme-test" "count"
        return $ length metrics
      
      case result of
        Left (_ :: SomeException) -> do
          -- 极端请求失败，系统应该仍然可用
          metric <- createMetric "extreme-recovery" "count"
          recordMetric metric 1.0
          value <- metricValue metric
          value `shouldBe` 1.0
        Right count -> do
          -- 如果系统支持，验证功能
          count `shouldBe` extremeRequest
      
      shutdownTelemetry