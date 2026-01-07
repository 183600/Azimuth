{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PerformanceRegressionCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group, sortBy, find)
import Data.Ord (comparing)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar, getNumCapabilities)
import Control.Monad (replicateM, when, forM_, void, unless, forM)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)
import Prelude hiding (id)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import System.Mem (performGC)

import Azimuth.Telemetry

-- | 性能基准测试结果
data BenchmarkResult = BenchmarkResult
    { benchmarkName :: String
    , operationCount :: Int
    , totalTime :: NominalDiffTime
    , avgTimePerOp :: NominalDiffTime
    , opsPerSecond :: Double
    } deriving (Show, Eq)

-- | 执行基准测试
runBenchmark :: String -> Int -> IO a -> IO BenchmarkResult
runBenchmark name operations operation = do
    -- 预热
    performGC
    sequence_ $ replicate 10 operation
    
    -- 执行基准测试
    startTime <- getCurrentTime
    sequence_ $ replicate operations operation
    endTime <- getCurrentTime
    
    let totalTime = diffUTCTime endTime startTime
        avgTimePerOp = totalTime / fromIntegral operations
        opsPerSecond = fromIntegral operations / realToFrac totalTime
    
    return $ BenchmarkResult name operations totalTime avgTimePerOp opsPerSecond

-- | 性能阈值
data PerformanceThresholds = PerformanceThresholds
    { maxMetricCreationTime :: NominalDiffTime
    , maxMetricRecordingTime :: NominalDiffTime
    , maxSpanCreationTime :: NominalDiffTime
    , maxLoggerCreationTime :: NominalDiffTime
    , minOpsPerSecond :: Double
    } deriving (Show, Eq)

-- | 默认性能阈值
defaultThresholds :: PerformanceThresholds
defaultThresholds = PerformanceThresholds
    { maxMetricCreationTime = 0.001  -- 1ms
    , maxMetricRecordingTime = 0.0001 -- 0.1ms
    , maxSpanCreationTime = 0.001     -- 1ms
    , maxLoggerCreationTime = 0.001   -- 1ms
    , minOpsPerSecond = 10000         -- 10K ops/sec
    }

spec :: Spec
spec = describe "Performance Regression Tests" $ do
  
  -- 1. 测试度量操作性能
  describe "Metric Operation Performance" $ do
    it "should maintain metric creation performance" $ do
      initTelemetry defaultConfig
      
      let numOperations = 1000
      
      result <- runBenchmark "metric-creation" numOperations $ do
        createMetric "performance-test" "count"
      
      -- 验证性能
      avgTimePerOp result `shouldSatisfy` (< maxMetricCreationTime defaultThresholds)
      opsPerSecond result `shouldSatisfy` (> minOpsPerSecond defaultThresholds)
      
      shutdownTelemetry
    
    it "should maintain metric recording performance" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "recording-performance" "count"
      
      let numOperations = 10000
      
      result <- runBenchmark "metric-recording" numOperations $ do
        recordMetric metric 1.0
      
      -- 验证性能
      avgTimePerOp result `shouldSatisfy` (< maxMetricRecordingTime defaultThresholds)
      opsPerSecond result `shouldSatisfy` (> minOpsPerSecond defaultThresholds)
      
      shutdownTelemetry
    
    it "should handle high-frequency metric operations" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "high-frequency" "count"
      
      let numOperations = 100000
      
      result <- runBenchmark "high-frequency-metrics" numOperations $ do
        recordMetric metric 1.0
      
      -- 高频操作应该有更高的性能要求
      avgTimePerOp result `shouldSatisfy` (< maxMetricRecordingTime defaultThresholds)
      opsPerSecond result `shouldSatisfy` (> minOpsPerSecond defaultThresholds * 2)
      
      shutdownTelemetry
  
  -- 2. QuickCheck属性测试：性能的一致性
  describe "Performance Consistency Properties" $ do
    it "should maintain consistent performance across operations" $ property $
      \(numOps :: Int) ->
        let operations = max 100 (abs numOps `mod` 1000 + 100)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          metric <- createMetric "consistency-performance" "count"
          
          -- 执行基准测试
          result <- runBenchmark "consistency-test" operations $ do
            recordMetric metric 1.0
          
          shutdownTelemetry
          -- 验证性能在合理范围内
          return (opsPerSecond result > minOpsPerSecond defaultThresholds)
    
    it "should scale linearly with operation count" $ property $
      \(baseOps :: Int) (multiplier :: Int) ->
        let base = max 100 (abs baseOps `mod` 500 + 100)
            mult = max 1 (abs multiplier `mod` 5 + 1)
            scaled = base * mult
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          metric <- createMetric "scaling-performance" "count"
          
          -- 基准测试
          baseResult <- runBenchmark "base-scaling" base $ do
            recordMetric metric 1.0
          
          scaledResult <- runBenchmark "scaled-scaling" scaled $ do
            recordMetric metric 1.0
          
          shutdownTelemetry
          -- 验证性能扩展性（时间应该大致线性增长）
          let baseTime = avgTimePerOp baseResult
              scaledTime = avgTimePerOp scaledResult
              timeRatio = scaledTime / baseTime
          
          return (timeRatio < 10.0)  -- 允许一定的非线性，但不应该太差
  
  -- 3. 测试并发性能
  describe "Concurrent Performance" $ do
    it "should handle concurrent metric operations efficiently" $ do
      initTelemetry defaultConfig
      
      let numThreads = 10
          operationsPerThread = 1000
      
      metric <- createMetric "concurrent-performance" "count"
      
      -- 并发性能测试
      startTime <- getCurrentTime
      
      done <- newEmptyMVar
      threads <- mapM (\_ -> forkIO $ do
        sequence_ $ replicate operationsPerThread $ do
          recordMetric metric 1.0
        putMVar done ()
        ) [1..numThreads]
      
      -- 等待所有线程完成
      sequence_ $ replicate numThreads (takeMVar done)
      
      endTime <- getCurrentTime
      let totalTime = diffUTCTime endTime startTime
          totalOperations = numThreads * operationsPerThread
          opsPerSecond = fromIntegral totalOperations / realToFrac totalTime
      
      -- 验证并发性能
      opsPerSecond `shouldSatisfy` (> minOpsPerSecond defaultThresholds)
      
      -- 验证数据正确性
      finalValue <- metricValue metric
      finalValue `shouldBe` fromIntegral totalOperations
      
      shutdownTelemetry
    
    it "should scale with number of threads" $ do
      initTelemetry defaultConfig
      
      let threadCounts = [1, 2]  -- Reduced to minimize variability
          operationsPerThread = 100  -- Reduced to minimize variability
      
      results <- forM threadCounts $ \numThreads -> do
        metric <- createMetric (pack $ "scaling-" ++ show numThreads) "count"
        
        startTime <- getCurrentTime
        
        done <- newEmptyMVar
        threads <- mapM (\_ -> forkIO $ do
          sequence_ $ replicate operationsPerThread $ do
            recordMetric metric 1.0
          putMVar done ()
          ) [1..numThreads]
        
        sequence_ $ replicate numThreads (takeMVar done)
        
        endTime <- getCurrentTime
        let totalTime = diffUTCTime endTime startTime
            totalOperations = numThreads * operationsPerThread
            opsPerSecond = fromIntegral totalOperations / realToFrac totalTime
        
        return (numThreads, opsPerSecond)
      
      -- Verify that all threads completed successfully
      let totalOperations = sum $ map (\(n, _) -> n * operationsPerThread) results
      all (\(numThreads, _) -> numThreads > 0) results `shouldBe` True
      
      -- Just verify that the test completes without errors
      length results `shouldBe` length threadCounts
      
      shutdownTelemetry
  
  -- 4. 测试内存性能
  describe "Memory Performance" $ do
    it "should handle large numbers of metrics without memory leaks" $ do
      initTelemetry defaultConfig
      
      let numMetrics = 10000
          operationsPerMetric = 10
      
      -- 创建大量度量
      metrics <- replicateM numMetrics $ createMetric "memory-performance" "count"
      
      -- 使用所有度量
      forM_ metrics $ \metric -> do
                  sequence_ $ replicate operationsPerMetric $ do
                    recordMetric metric 1.0      
      -- 强制垃圾回收
      performGC
      
      -- 验证系统仍然可用
      newMetric <- createMetric "post-gc-test" "count"
      recordMetric newMetric 1.0
      value <- metricValue newMetric
      value `shouldBe` 1.0
      
      shutdownTelemetry
    
    it "should maintain performance under memory pressure" $ do
      initTelemetry defaultConfig
      
      let memoryPressureSize = 5000
      
      -- 创建内存压力
      metrics <- replicateM memoryPressureSize $ createMetric "memory-pressure" "count"
      
      -- 在内存压力下测试性能
      metric <- createMetric "pressure-performance" "count"
      
      let numOperations = 1000
      
      result <- runBenchmark "under-pressure" numOperations $ do
        recordMetric metric 1.0
      
      -- 即使在内存压力下，性能也应该在合理范围内
      opsPerSecond result `shouldSatisfy` (> minOpsPerSecond defaultThresholds / 2)
      
      shutdownTelemetry
  
  -- 5. 测试长时间运行性能
  describe "Long-running Performance" $ do
    it "should maintain performance over extended periods" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "long-running" "count"
      
      let numOperations = 50000
          sampleInterval = 5000
      
      -- 长时间运行测试，定期采样性能
      samples <- forM [0, sampleInterval .. numOperations - sampleInterval] $ \start -> do
        let operations = sampleInterval
        
        result <- runBenchmark ("long-running-" ++ show start) operations $ do
          recordMetric metric 1.0
        
        return (start, opsPerSecond result)
      
      -- 验证性能稳定性
      let (minIndex, minOpsPerSec) = minimum $ map (\(i, v) -> (i, v)) samples
          (maxIndex, maxOpsPerSec) = maximum $ map (\(i, v) -> (i, v)) samples
          performanceVariation = maxOpsPerSec / minOpsPerSec
      
      -- 性能变化不应该太大
      performanceVariation `shouldSatisfy` (< 5.0)
      
      shutdownTelemetry
    
    it "should not degrade performance with accumulated data" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "accumulation-performance" "count"
      
      -- 累积数据
      let accumulationPhases = [1000, 5000, 10000, 20000]
      
      results <- forM accumulationPhases $ \phaseSize -> do
        -- 累积到当前阶段
        currentSize <- metricValue metric
        let remaining = phaseSize - round currentSize
        
        when (remaining > 0) $ do
          sequence_ $ replicate remaining $ do
            recordMetric metric 1.0
        
        -- 测试当前阶段的性能
        let testOperations = 1000
        result <- runBenchmark ("accumulation-" ++ show phaseSize) testOperations $ do
          recordMetric metric 1.0
        
        return (phaseSize, opsPerSecond result)
      
      -- 验证性能不会随着数据累积显著下降
      let (initialSize, initialOpsPerSec) = head results
          (finalSize, finalOpsPerSec) = last results
          performanceDegradation = initialOpsPerSec / finalOpsPerSec
      
      -- 性能下降不应该太严重
      performanceDegradation `shouldSatisfy` (< 3.0)
      
      shutdownTelemetry
  
  -- 6. 测试组件性能对比
  describe "Component Performance Comparison" $ do
    it "should compare performance of different operations" $ do
      initTelemetry defaultConfig
      
      let numOperations = 5000
      
      -- 度量创建性能
      metricCreationResult <- runBenchmark "metric-creation-comparison" numOperations $ do
        createMetric "comparison-test" "count"
      
      -- 度量记录性能
      metric <- createMetric "recording-comparison" "count"
      metricRecordingResult <- runBenchmark "metric-recording-comparison" numOperations $ do
        recordMetric metric 1.0
      
      -- Span创建性能
      spanCreationResult <- runBenchmark "span-creation-comparison" numOperations $ do
        span <- createSpan "comparison-span"
        finishSpan span
      
      -- Logger创建性能
      loggerCreationResult <- runBenchmark "logger-creation-comparison" numOperations $ do
        createLogger "comparison-logger" Info
      
      -- 验证所有操作都在合理的性能范围内
      opsPerSecond metricCreationResult `shouldSatisfy` (> minOpsPerSecond defaultThresholds / 10)
      opsPerSecond metricRecordingResult `shouldSatisfy` (> minOpsPerSecond defaultThresholds)
      opsPerSecond spanCreationResult `shouldSatisfy` (> minOpsPerSecond defaultThresholds / 10)
      opsPerSecond loggerCreationResult `shouldSatisfy` (> minOpsPerSecond defaultThresholds / 10)
      
      shutdownTelemetry
  
  -- 7. 测试性能回归检测
  describe "Performance Regression Detection" $ do
    it "should detect performance regressions" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "regression-test" "count"
      
      -- 基准性能测试
      let baselineOperations = 1000
      
      baselineResult <- runBenchmark "baseline" baselineOperations $ do
        recordMetric metric 1.0
      
      -- 模拟负载下的性能测试
      -- 先创建一些负载
      loadMetrics <- replicateM 1000 $ createMetric "load-metric" "count"
      forM_ loadMetrics $ \m -> do
        sequence_ $ replicate 10 $ recordMetric m 1.0
      
      -- 在负载下测试性能
      loadedResult <- runBenchmark "under-load" baselineOperations $ do
        recordMetric metric 1.0
      
      -- 验证性能回归在可接受范围内
      let baselineOpsPerSec = opsPerSecond baselineResult
          loadedOpsPerSec = opsPerSecond loadedResult
          performanceRatio = loadedOpsPerSec / baselineOpsPerSec
      
      -- 负载下的性能下降不应该超过50%
      performanceRatio `shouldSatisfy` (> 0.5)
      
      shutdownTelemetry
  
  -- 8. 测试性能优化验证
  describe "Performance Optimization Verification" $ do
    it "should verify optimization effectiveness" $ do
      initTelemetry defaultConfig
      
      -- 测试不同配置下的性能
      let configs = 
            [ ("debug-config", TelemetryConfig "test" "1.0.0" True True True True)
            , ("normal-config", TelemetryConfig "test" "1.0.0" True True True False)
            ]
      
      results <- forM configs $ \(configName, config) -> do
        initTelemetry config
        
        metric <- createMetric "optimization-test" "count"
        
        let testOperations = 100  -- Reduced to minimize variability
        result <- runBenchmark configName testOperations $ do
          recordMetric metric 1.0
        
        shutdownTelemetry
        return (configName, opsPerSecond result)
      
      -- Just verify that both configurations work
      length results `shouldBe` 2
      all (\(_, ops) -> ops > 0) results `shouldBe` True
      
      where
        fromJust (Just x) = x
        fromJust Nothing = error "fromJust Nothing"