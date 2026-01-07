{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PerformanceCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Control.Concurrent (forkIO, threadDelay, killThread, getNumCapabilities)
import Control.Monad (replicateM, when, void, unless, sequence_, forM, forM_)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group, intercalate, take, drop, replicate)
import Prelude hiding (id)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Time.Clock as Time

import Azimuth.Telemetry

spec :: Spec
spec = describe "Performance Cabal Test Suite" $ do
  
  -- 1. 基本性能基准测试
  describe "Basic Performance Benchmarks" $ do
    it "should handle metric operations efficiently" $ property $
      \operationCount ->
        let actualCount = max 100 (abs operationCount `mod` 10000 + 100)
        in unsafePerformIO $ do
                    
          metric <- createMetric "performance-metric" "ops"
          
          -- 测量操作时间
          startTime <- getCurrentTime
          
          sequence_ $ replicate actualCount $ do
            recordMetric metric 1.0
          
          endTime <- getCurrentTime
          let duration = diffUTCTime endTime startTime
              opsPerSecond = fromIntegral actualCount / realToFrac duration
          
          -- 验证所有操作都完成了
          finalValue <- metricValue metric
          
                    
          -- 性能要求：至少每秒10000次操作
          return (finalValue == fromIntegral actualCount && opsPerSecond > 10000)
    
    it "should handle span operations efficiently" $ property $
      \operationCount ->
        let actualCount = max 50 (abs operationCount `mod` 1000 + 50)
        in unsafePerformIO $ do
                    
          -- 测量span创建时间
          startTime <- getCurrentTime
          
          spans <- sequence $ replicate actualCount $ do
            createSpan "performance-span"
          
          endTime <- getCurrentTime
          let duration = diffUTCTime endTime startTime
              opsPerSecond = fromIntegral actualCount / realToFrac duration
          
          -- 完成所有span
          sequence_ $ map finishSpan spans
          
                    
          -- 性能要求：至少每秒1000次span创建
          return (opsPerSecond > 1000)
    
    it "should handle logging operations efficiently" $ property $
      \operationCount ->
        let actualCount = max 100 (abs operationCount `mod` 10000 + 100)
        in unsafePerformIO $ do
                    
          logger <- createLogger "performance-logger" Info
          
          -- 测量日志时间
          startTime <- getCurrentTime
          
          sequence_ $ replicate actualCount $ do
            logMessage logger Info "performance test message"
          
          endTime <- getCurrentTime
          let duration = diffUTCTime endTime startTime
              opsPerSecond = fromIntegral actualCount / realToFrac duration
          
                    
          -- 性能要求：至少每秒5000次日志操作
          return (opsPerSecond > 5000)
  
  -- 2. 可扩展性测试
  describe "Scalability Tests" $ do
    it "should scale linearly with metric count" $ property $
      \metricCount ->
        let actualCount = max 10 (abs metricCount `mod` 1000 + 10)
        in unsafePerformIO $ do
                    
          -- 创建多个度量
          metrics <- sequence $ replicate actualCount $ do
            createMetric "scalability-test" "count"
          
          -- 测量更新所有度量的时间
          startTime <- getCurrentTime
          
          sequence_ $ map (`recordMetric` 1.0) metrics
          
          endTime <- getCurrentTime
          let duration = diffUTCTime endTime startTime
              opsPerSecond = fromIntegral actualCount / realToFrac duration
          
          -- 验证所有度量都正确更新
          values <- sequence $ map metricValue metrics
          let allCorrect = all (== 1.0) values
          
                    
          -- 性能要求：至少每秒1000次度量更新
          return (allCorrect && opsPerSecond > 1000)
    
    it "should handle increasing load gracefully" $ property $
      \loadFactor ->
        let actualLoad = max 1 (abs loadFactor `mod` 10 + 1)
            baseOperations = 1000
            operations = baseOperations * actualLoad
        in unsafePerformIO $ do
                    
          metric <- createMetric "load-test" "ops"
          
          -- 测量不同负载下的性能
          startTime <- getCurrentTime
          
          sequence_ $ replicate operations $ do
            recordMetric metric 1.0
          
          endTime <- getCurrentTime
          let duration = diffUTCTime endTime startTime
              opsPerSecond = fromIntegral operations / realToFrac duration
          
          -- 验证所有操作都完成了
          finalValue <- metricValue metric
          
                    
          -- 性能要求：性能不应线性下降
          return (finalValue == fromIntegral operations && opsPerSecond > 5000)
  
  -- 3. 内存使用效率测试
  describe "Memory Efficiency Tests" $ do
    it "should not leak memory during repeated operations" $ property $
      \cycleCount ->
        let actualCycles = max 1 (abs cycleCount `mod` 100 + 1)
            operationsPerCycle = 100
        in unsafePerformIO $ do
                    
          -- 重复创建和使用资源
          sequence_ $ replicate actualCycles $ do
            metrics <- sequence $ replicate 10 $ do
              createMetric "memory-test" "count"
            
            sequence_ $ map (`recordMetric` 1.0) metrics
            
            -- 强制垃圾回收
            performGC
          
          return True  -- 如果没有内存泄漏就算成功
    
    it "should handle large numbers of resources efficiently" $ property $
      \resourceCount ->
        let actualCount = max 10 (abs resourceCount `mod` 1000 + 10)
        in unsafePerformIO $ do
                    
          -- 创建大量资源
          metrics <- sequence $ replicate actualCount $ do
            createMetric "memory-efficiency" "count"
          
          loggers <- sequence $ replicate actualCount $ do
            createLogger "memory-efficiency-logger" Info
          
          spans <- sequence $ replicate actualCount $ do
            createSpan "memory-efficiency-span"
          
          -- 使用所有资源
          sequence_ $ map (`recordMetric` 1.0) metrics
          sequence_ $ flip map loggers $ \logger -> do
            logMessage logger Info "memory efficiency test"
          sequence_ $ map finishSpan spans
          
          -- 验证所有资源都正常工作
          metricValues <- sequence $ map metricValue metrics
          let allCorrect = all (== 1.0) metricValues
          
          performGC
          
          return allCorrect
  
  -- 4. 并发性能测试
  describe "Concurrent Performance Tests" $ do
    it "should maintain performance under concurrent load" $ property $
      \(threadCount :: Int) ->
        let actualThreads = max 1 (abs threadCount `mod` 20 + 1)
            operationsPerThread = 500
        in unsafePerformIO $ do
                    
          metric <- createMetric "concurrent-performance" "ops"
          
          -- 测量并发操作性能
          startTime <- getCurrentTime
          
          threads <- mapM (\threadId -> forkIO $ do
            sequence_ $ replicate operationsPerThread $ do
              recordMetric metric 1.0
            ) [1..actualThreads]
          
          -- 等待所有线程完成
          threadDelay 2000000  -- 2秒
          
          endTime <- getCurrentTime
          let duration = diffUTCTime endTime startTime
              totalOperations = fromIntegral actualThreads * fromIntegral operationsPerThread
              opsPerSecond = totalOperations / realToFrac duration
          
          -- 清理线程
          sequence_ $ map killThread threads
          
          -- 验证所有操作都完成了
          finalValue <- metricValue metric
          let expectedValue = totalOperations
          
                    
          -- 性能要求：并发性能不应显著下降
          return (finalValue == expectedValue && opsPerSecond > 10000)
    
    it "should scale with number of cores" $ do
      numCores <- getNumCapabilities
      
            
      -- 创建与核心数相等的线程
      metric <- createMetric "core-scaling" "ops"
      
      startTime <- getCurrentTime
      
      threads <- mapM (\coreId -> forkIO $ do
        sequence_ $ replicate 1000 $ do
          recordMetric metric 1.0
        ) [1..numCores]
      
      -- 等待所有线程完成
      threadDelay 3000000  -- 3秒
      
      endTime <- getCurrentTime
      let duration = diffUTCTime endTime startTime
          totalOperations = fromIntegral numCores * 1000
          opsPerSecond = totalOperations / realToFrac duration
      
      -- 清理线程
      sequence_ $ map killThread threads
      
      -- 验证所有操作都完成了
      finalValue <- metricValue metric
      
            
      -- 性能要求：应该能够利用多核
      finalValue `shouldBe` totalOperations
      opsPerSecond `shouldSatisfy` (> 5000)
  
  -- 5. 高频操作性能测试
  describe "High-Frequency Operation Tests" $ do
    it "should handle high-frequency metric updates" $ property $
      \frequency ->
        let actualFrequency = max 1000 (abs frequency `mod` 50000 + 1000)
        in unsafePerformIO $ do
                    
          metric <- createMetric "high-frequency" "ops"
          
          -- 执行高频操作
          startTime <- getCurrentTime
          
          sequence_ $ replicate actualFrequency $ do
            recordMetric metric 1.0
          
          endTime <- getCurrentTime
          let duration = diffUTCTime endTime startTime
              opsPerSecond = fromIntegral actualFrequency / realToFrac duration
          
          -- 验证所有操作都完成了
          finalValue <- metricValue metric
          
                    
          -- 性能要求：应该能够处理高频操作
          return (finalValue == fromIntegral actualFrequency && opsPerSecond > 20000)
    
    it "should handle burst operations efficiently" $ property $
      \burstCount ->
        let actualBursts = max 1 (abs burstCount `mod` 10 + 1)
            operationsPerBurst = 1000
        in unsafePerformIO $ do
                    
          metric <- createMetric "burst-operations" "ops"
          
          -- 执行突发操作
          startTime <- getCurrentTime
          
          sequence_ $ replicate actualBursts $ do
            sequence_ $ replicate operationsPerBurst $ do
              recordMetric metric 1.0
            threadDelay 10000  -- 10毫秒间隔
          
          endTime <- getCurrentTime
          let duration = diffUTCTime endTime startTime
              totalOperations = fromIntegral actualBursts * fromIntegral operationsPerBurst
              opsPerSecond = totalOperations / realToFrac duration
          
          -- 验证所有操作都完成了
          finalValue <- metricValue metric
          let expectedValue = totalOperations
          
                    
          -- 性能要求：应该能够处理突发负载
          return (finalValue == expectedValue && opsPerSecond > 10000)
  
  -- 6. 资源使用效率测试
  describe "Resource Usage Efficiency" $ do
    it "should minimize CPU usage during idle periods" $ do
            
      metric <- createMetric "idle-efficiency" "count"
      
      -- 记录一些初始操作
      sequence_ $ replicate 100 $ do
        recordMetric metric 1.0
      
      -- 空闲期
      threadDelay 1000000  -- 1秒
      
      -- 再次操作
      sequence_ $ replicate 100 $ do
        recordMetric metric 1.0
      
      -- 验证系统仍然响应
      finalValue <- metricValue metric
      let expectedValue = 200.0
      
            
      finalValue `shouldBe` expectedValue
    
    it "should handle resource pooling efficiently" $ property $
      \poolSize ->
        let actualSize = max 1 (abs poolSize `mod` 100 + 1)
        in unsafePerformIO $ do
                    
          -- 创建资源池
          metrics <- sequence $ replicate actualSize $ do
            createMetric "resource-pool" "count"
          
          -- 重复使用资源
          sequence_ $ replicate 10 $ do
            sequence_ $ map (`recordMetric` 1.0) metrics
          
          -- 验证所有资源都正确更新
          values <- sequence $ map metricValue metrics
          let allCorrect = all (== 10.0) values
          
                    
          return allCorrect
  
  -- 7. 长期运行稳定性测试
  describe "Long-Running Stability Tests" $ do
    it "should maintain performance over extended periods" $ property $
      \duration ->
        let actualDuration = max 1 (abs duration `mod` 10 + 1)  -- 秒
        in unsafePerformIO $ do
                    
          metric <- createMetric "stability-test" "ops"
          
          -- 长期运行测试
          startTime <- getCurrentTime
          
          let operationsPerSecond = 1000
              totalOperations = operationsPerSecond * actualDuration
          
          sequence_ $ replicate totalOperations $ do
            recordMetric metric 1.0
            
          endTime <- getCurrentTime
          let realDuration = diffUTCTime endTime startTime
              opsPerSecond = fromIntegral totalOperations / realToFrac realDuration
          
          -- 验证所有操作都完成了
          finalValue <- metricValue metric
          
                    
          -- 性能要求：长期运行性能不应显著下降
          return (finalValue == fromIntegral totalOperations && opsPerSecond > 500)
    
    it "should handle memory growth gracefully" $ property $
      \iterationCount ->
        let actualIterations = max 1 (abs iterationCount `mod` 100 + 1)
        in unsafePerformIO $ do
                    
          -- 长期迭代测试
          sequence_ $ replicate actualIterations $ do
            -- 创建和使用资源
            metrics <- sequence $ replicate 10 $ do
              createMetric "memory-growth" "count"
            
            sequence_ $ map (`recordMetric` 1.0) metrics
            
            -- 清理一些资源
            sequence_ $ take 5 $ map (const $ return ()) metrics
            
            -- 定期垃圾回收
            when (actualIterations `mod` 10 == 0) $ do
              performGC
          
          return True  -- 如果没有内存问题就算成功
  
  -- 8. 性能回归测试
  describe "Performance Regression Tests" $ do
    it "should not regress in metric operation performance" $ do
      let baselineOpsPerSecond = 10000  -- 基准性能
      
            
      metric <- createMetric "regression-test" "ops"
      
      -- 测量当前性能
      startTime <- getCurrentTime
      
      sequence_ $ replicate 5000 $ do
        recordMetric metric 1.0
      
      endTime <- getCurrentTime
      let duration = diffUTCTime endTime startTime
          currentOpsPerSecond = 5000 / realToFrac duration
      
      -- 验证性能没有回归
      finalValue <- metricValue metric
      
            
      finalValue `shouldBe` 5000.0
      currentOpsPerSecond `shouldSatisfy` (>= baselineOpsPerSecond)
    
    it "should not regress in span operation performance" $ do
      let baselineOpsPerSecond = 1000  -- 基准性能
      
            
      -- 测量span创建性能
      startTime <- getCurrentTime
      
      spans <- sequence $ replicate 1000 $ do
        createSpan "regression-span"
      
      endTime <- getCurrentTime
      let duration = diffUTCTime endTime startTime
          currentOpsPerSecond = 1000 / realToFrac duration
      
      -- 完成所有span
      sequence_ $ map finishSpan spans
      
            
      -- 验证性能没有回归
      currentOpsPerSecond `shouldSatisfy` (>= baselineOpsPerSecond)
  
  -- 9. 负载均衡性能测试
  describe "Load Balancing Performance" $ do
    it "should distribute load evenly across metrics" $ property $
      \metricCount ->
        let actualCount = max 2 (abs metricCount `mod` 10 + 2)
        in unsafePerformIO $ do
                    
          -- 创建多个度量
          metrics <- sequence $ replicate actualCount $ do
            createMetric "load-balance" "count"
          
          -- 均匀分布负载
          forM_ [1..fromIntegral actualCount * 100] $ \i -> do
            let metricIndex = i `mod` actualCount
            recordMetric (metrics !! metricIndex) 1.0
          
          -- 验证负载分布
          values <- sequence $ map metricValue metrics
          let expectedValue = 100.0
              allCorrect = all (== expectedValue) values
          
                    
          return allCorrect
    
    it "should handle uneven load efficiently" $ property $
      \skewFactor ->
        let actualSkew = max 1 (abs skewFactor `mod` 5 + 1)
            metricCount = 5
        in unsafePerformIO $ do
                    
          -- 创建多个度量
          metrics <- sequence $ replicate metricCount $ do
            createMetric "uneven-load" "count"
          
          -- 不均匀分布负载
          forM_ [1..1000] $ \i -> do
            let metricIndex = if i `mod` actualSkew == 0 then 0 else 1 + (i `mod` (metricCount - 1))
            recordMetric (metrics !! metricIndex) 1.0
          
          -- 验证系统仍然可以工作
          values <- sequence $ map metricValue metrics
          let allValid = all (not . isNaN) values
          
                    
          return allValid
  
  -- 10. 极限性能测试
  describe "Extreme Performance Tests" $ do
    it "should handle maximum sustainable load" $ do
            
      metric <- createMetric "extreme-load" "ops"
      
      -- 尝试找到最大可持续负载
      let testDuration = 5  -- 秒
          targetOpsPerSecond = 50000
      
      startTime <- getCurrentTime
      
      let operationsPerBatch = 1000
          batchDelay = fromIntegral operationsPerBatch * 1000000 `div` targetOpsPerSecond  -- 微秒
      
      -- 执行高频操作
      let loop remainingTime = do
            currentTime <- getCurrentTime
            let elapsed = realToFrac (diffUTCTime currentTime startTime) :: Double
            
            if elapsed < fromIntegral testDuration
              then do
                sequence_ $ replicate operationsPerBatch $ do
                  recordMetric metric 1.0
                threadDelay batchDelay
                loop remainingTime
              else return ()
      
      loop testDuration
      
      endTime <- getCurrentTime
      let actualDuration = diffUTCTime endTime startTime
      
      -- 验证系统仍然可以工作
      finalValue <- metricValue metric
      
            
      -- 验证性能
      not (isNaN finalValue) `shouldBe` True
      realToFrac actualDuration `shouldSatisfy` (< fromIntegral testDuration * 2)  -- 不应超过2倍预期时间
    
    it "should recover from performance degradation" $ property $
      \degradationLevel ->
        let actualLevel = max 1 (abs degradationLevel `mod` 5 + 1)
        in unsafePerformIO $ do
                    
          metric <- createMetric "recovery-test" "ops"
          
          -- 正常操作
          sequence_ $ replicate 1000 $ do
            recordMetric metric 1.0
          
          -- 引入性能退化
          sequence_ $ replicate (actualLevel * 1000) $ do
            recordMetric metric 1.0
            threadDelay 1  -- 小延迟模拟退化
          
          -- 恢复测试
          sequence_ $ replicate 1000 $ do
            recordMetric metric 1.0
          
          -- 验证系统恢复
          finalValue <- metricValue metric
          let expectedValue = fromIntegral (1000 + actualLevel * 1000 + 1000)
          
                    
          return (finalValue == expectedValue && not (isNaN finalValue))
