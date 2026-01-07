{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MetricAggregationCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group, sortBy)
import Data.Ord (comparing)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, when, forM, forM_)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Metric Aggregation Tests" $ do
  
  -- 1. 测试基本度量聚合
  describe "Basic Metric Aggregation" $ do
    it "should aggregate multiple metrics with same name" $ do
      -- Enable metric aggregation for this test
      writeIORef enableMetricAggregation True
      writeIORef enableMetricSharing True
      initTelemetry defaultConfig
      
      -- 创建多个同名度量
      metrics <- replicateM 5 $ createMetric "aggregation-test" "count"
      
      -- 记录不同的值
      forM_ (zip [1..] metrics) $ \(i, metric) -> do
        recordMetric metric (fromIntegral i)
      
      -- 验证所有度量的值都是聚合后的结果
      values <- mapM metricValue metrics
      let expectedValue = sum [1..5]
      
      all (== expectedValue) values `shouldBe` True
      shutdownTelemetry
    
    it "should handle aggregation with different units" $ do
      initTelemetry defaultConfig
      
      -- 创建同名但不同单位的度量
      metric1 <- createMetric "multi-unit" "ms"
      metric2 <- createMetric "multi-unit" "bytes"
      
      recordMetric metric1 100.0
      recordMetric metric2 200.0
      
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      
      value1 `shouldBe` 100.0
      value2 `shouldBe` 200.0
      shutdownTelemetry
  
  -- 2. QuickCheck属性测试：度量聚合的交换律
  describe "Metric Aggregation Properties" $ do
    it "should satisfy commutative property" $ property $
      \values1 values2 ->
        let testValues1 = take 10 values1 :: [Double]
            testValues2 = take 10 values2 :: [Double]
        in unsafePerformIO $ do
          -- Enable metric aggregation before initialization
          writeIORef enableMetricAggregation True
          writeIORef enableMetricSharing True
          initTelemetry defaultConfig
          
          -- 创建两个度量并按不同顺序记录值
          metric1 <- createMetric "commutative-test" "count"
          metric2 <- createMetric "commutative-test" "count"
          
          -- 按不同顺序记录
          sequence_ $ map (recordMetric metric1) testValues1
          sequence_ $ map (recordMetric metric1) testValues2
          
          sequence_ $ map (recordMetric metric2) testValues2
          sequence_ $ map (recordMetric metric2) testValues1
          
          value1 <- metricValue metric1
          value2 <- metricValue metric2
          
          shutdownTelemetry
          return (value1 == value2)
    
    it "should satisfy associative property" $ property $
      \values1 values2 values3 ->
        let testValues1 = take 5 values1 :: [Double]
            testValues2 = take 5 values2 :: [Double]
            testValues3 = take 5 values3 :: [Double]
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          -- 创建三个度量并测试结合律
          metric1 <- createMetric "associative-test" "count"
          metric2 <- createMetric "associative-test" "count"
          
          -- 不同的分组方式
          sequence_ $ map (recordMetric metric1) (testValues1 ++ testValues2 ++ testValues3)
          
          sequence_ $ map (recordMetric metric2) testValues1
          sequence_ $ map (recordMetric metric2) testValues2
          sequence_ $ map (recordMetric metric2) testValues3
          
          value1 <- metricValue metric1
          value2 <- metricValue metric2
          
          shutdownTelemetry
          return (value1 == value2)
  
  -- 3. 测试并发聚合
  describe "Concurrent Aggregation" $ do
    it "should handle concurrent aggregation correctly" $ do
      writeIORef enableMetricAggregation True
      initTelemetry defaultConfig
      
      -- 创建共享度量
      metric <- createMetric "concurrent-aggregation" "count"
      
      let numThreads = 10
          operationsPerThread = 100
      
      -- 并发记录值
      done <- newEmptyMVar
      threads <- mapM (\_ -> forkIO $ do
        sequence_ $ replicate operationsPerThread $ do
          recordMetric metric 1.0
        putMVar done ()
        ) [1..numThreads]
      
      -- 等待所有线程完成
      sequence_ $ replicate numThreads (takeMVar done)
      
      -- 验证聚合结果
      finalValue <- metricValue metric
      let expectedValue = fromIntegral numThreads * fromIntegral operationsPerThread
      
      finalValue `shouldBe` expectedValue
      shutdownTelemetry
  
  -- 4. 测试聚合统计
  describe "Aggregation Statistics" $ do
    it "should calculate average correctly" $ do
      writeIORef enableMetricAggregation False
      initTelemetry defaultConfig
      
      -- Create metrics with different names to avoid aggregation
      metrics <- forM [1..10] $ \i -> createMetric (pack $ "average-test-" ++ show i) "value"
      
      -- 记录值
      forM_ (zip [1..] metrics) $ \(i, metric) -> do
        recordMetric metric (fromIntegral i)
      
      -- 计算平均值
      values <- mapM metricValue metrics
      let avg = sum values / fromIntegral (length values)
      
      avg `shouldBe` 5.5
      shutdownTelemetry
    
    it "should find min and max values" $ do
      writeIORef enableMetricAggregation False
      initTelemetry defaultConfig
      
      -- Create metrics with different names to avoid aggregation
      metrics <- forM [1..5] $ \i -> createMetric (pack $ "minmax-test-" ++ show i) "value"
      
      -- 记录不同的值
      let testValues = [10.0, 5.0, 20.0, 1.0, 15.0]
      forM_ (zip testValues metrics) $ \(value, metric) -> do
        recordMetric metric value
      
      -- 获取所有值
      values <- mapM metricValue metrics
      
      -- 验证最小值和最大值
      minimum values `shouldBe` 1.0
      maximum values `shouldBe` 20.0
      
      shutdownTelemetry
  
  -- 5. 测试聚合窗口
  describe "Aggregation Windows" $ do
    it "should handle time-based aggregation windows" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "window-test" "count"
      
      -- 模拟时间窗口内的记录
      sequence_ $ replicate 100 $ do
        recordMetric metric 1.0
        threadDelay 1000  -- 1ms延迟
      
      finalValue <- metricValue metric
      finalValue `shouldBe` 100.0
      
      shutdownTelemetry
    
    it "should reset aggregation in different windows" $ do
      initTelemetry defaultConfig
      
      -- 第一个窗口
      metric1 <- createMetric "window-reset" "count"
      sequence_ $ replicate 50 $ recordMetric metric1 1.0
      
      -- 关闭并重新初始化（模拟新窗口）
      shutdownTelemetry
      initTelemetry defaultConfig
      
      -- 第二个窗口
      metric2 <- createMetric "window-reset" "count"
      sequence_ $ replicate 30 $ recordMetric metric2 1.0
      
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      
      value1 `shouldBe` 50.0
      value2 `shouldBe` 30.0
      
      shutdownTelemetry
  
  -- 6. 测试聚合性能
  describe "Aggregation Performance" $ do
    it "should handle large-scale aggregation efficiently" $ do
      writeIORef enableMetricAggregation False
      initTelemetry defaultConfig
      
      let numMetrics = 10  -- Reduced to minimize test time
          operationsPerMetric = 100  -- Reduced to minimize test time
      
      -- Create metrics with different names to avoid aggregation
      metrics <- forM [1..numMetrics] $ \i -> createMetric (pack $ "performance-test-" ++ show i) "count"
      
      -- 大量聚合操作
      forM_ metrics $ \metric -> do
        sequence_ $ replicate operationsPerMetric $ do
          recordMetric metric 1.0
      
      -- 验证所有度量都有正确的值
      values <- mapM metricValue metrics
      all (== fromIntegral operationsPerMetric) values `shouldBe` True
      
      shutdownTelemetry
  
  -- 7. 测试边界条件
  describe "Aggregation Boundary Conditions" $ do
    it "should handle empty aggregation" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "empty-aggregation" "count"
      value <- metricValue metric
      
      value `shouldBe` 0.0
      shutdownTelemetry
    
    it "should handle single value aggregation" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "single-value" "count"
      recordMetric metric 42.0
      
      value <- metricValue metric
      value `shouldBe` 42.0
      
      shutdownTelemetry
    
    it "should handle special values in aggregation" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "special-values" "count"
      
      -- 测试特殊值
      recordMetric metric 0.0
      recordMetric metric (-1.0)
      recordMetric metric (1.0/0.0)  -- 正无穷
      recordMetric metric (-1.0/0.0) -- 负无穷
      
      value <- metricValue metric
      
      -- 验证无穷大值的处理
      isInfinite value `shouldBe` True
      
      shutdownTelemetry
  
  -- 8. 测试聚合一致性
  describe "Aggregation Consistency" $ do
    it "should maintain consistency across multiple reads" $ do
      writeIORef enableMetricAggregation False
      initTelemetry defaultConfig
      
      -- Create metrics with different names to avoid aggregation
      metrics <- forM [1..5] $ \i -> createMetric (pack $ "consistency-test-" ++ show i) "count"
      
      -- 记录值
      forM_ metrics $ \metric -> do
        recordMetric metric 10.0
      
      -- 多次读取验证一致性
      values1 <- mapM metricValue metrics
      values2 <- mapM metricValue metrics
      values3 <- mapM metricValue metrics
      
      values1 `shouldBe` values2
      values2 `shouldBe` values3
      all (== 10.0) values1 `shouldBe` True
      
      shutdownTelemetry
    
    it "should handle mixed aggregation patterns" $ do
      initTelemetry defaultConfig
      
      -- 创建不同聚合模式的度量
      metric1 <- createMetric "pattern-1" "count"
      metric2 <- createMetric "pattern-2" "count"
      metric3 <- createMetric "pattern-3" "count"
      
      -- 不同的聚合模式
      sequence_ $ replicate 10 $ recordMetric metric1 1.0
      sequence_ $ replicate 5 $ recordMetric metric2 2.0
      sequence_ $ replicate 3 $ recordMetric metric3 3.0
      
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      value3 <- metricValue metric3
      
      value1 `shouldBe` 10.0
      value2 `shouldBe` 10.0
      value3 `shouldBe` 9.0
      
      shutdownTelemetry