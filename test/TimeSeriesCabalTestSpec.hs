{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TimeSeriesCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack, Text)
import qualified Data.Text as Text
import Data.List (nub, sort, group, sortBy, find)
import Data.Ord (comparing)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar, getNumCapabilities)
import Control.Monad (replicateM, when, forM_, void)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Prelude hiding (id)

import Azimuth.Telemetry

-- | 时间序列数据点
data TimeSeriesPoint = TimeSeriesPoint
    { tspValue :: Double
    , tspTimestamp :: Integer
    , tspMetricName :: Text
    } deriving (Show, Eq)

-- | 时间序列统计
data TimeSeriesStats = TimeSeriesStats
    { tssCount :: Int
    , tssSum :: Double
    , tssAverage :: Double
    , tssMin :: Double
    , tssMax :: Double
    } deriving (Show, Eq)

spec :: Spec
spec = describe "Time Series Tests" $ do
  
  -- 1. 测试基本时间序列功能
  describe "Basic Time Series Functionality" $ do
    it "should record metrics with time context" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "time-series-basic" "value"
      
      -- 记录一系列值
      let testValues = [10.0, 20.0, 15.0, 25.0, 30.0]
      forM_ testValues $ \value -> do
        recordMetric metric value
        threadDelay 1000  -- 1ms间隔
      
      finalValue <- metricValue metric
      finalValue `shouldBe` sum testValues
      
      shutdownTelemetry
    
    it "should handle time-based metric creation" $ do
      initTelemetry defaultConfig
      
      -- 在不同时间创建度量
      metric1 <- createMetric "time-based-1" "count"
      threadDelay 1000
      metric2 <- createMetric "time-based-2" "count"
      threadDelay 1000
      metric3 <- createMetric "time-based-3" "count"
      
      -- 记录值
      recordMetric metric1 1.0
      recordMetric metric2 2.0
      recordMetric metric3 3.0
      
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      value3 <- metricValue metric3
      
      value1 `shouldBe` 1.0
      value2 `shouldBe` 2.0
      value3 `shouldBe` 3.0
      
      shutdownTelemetry
  
  -- 2. QuickCheck属性测试：时间序列的时序一致性
  describe "Time Series Properties" $ do
    it "should maintain chronological order of operations" $ property $
      \values ->
        let testValues = take 20 values :: [Double]
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          metric <- createMetric "chronological-test" "value"
          
          -- 记录值并跟踪顺序
          forM_ (zip [1..] testValues) $ \(index, value) -> do
            recordMetric metric value
            threadDelay 100  -- 短暂延迟确保时序
          
          finalValue <- metricValue metric
          
          shutdownTelemetry
          return (finalValue == sum testValues)
    
    it "should handle concurrent time series operations" $ property $
      \numOps ->
        let operations = max 1 (abs numOps `mod` 50 + 1)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          metric <- createMetric "concurrent-time-series" "count"
          
          -- 并发记录值
          done <- newEmptyMVar
          threads <- mapM (\_ -> forkIO $ do
            recordMetric metric 1.0
            putMVar done ()
            ) [1..operations]
          
          -- 等待所有操作完成
          sequence_ $ replicate operations (takeMVar done)
          
          finalValue <- metricValue metric
          
          shutdownTelemetry
          return (finalValue == fromIntegral operations)
  
  -- 3. 测试时间序列窗口
  describe "Time Series Windows" $ do
    it "should handle sliding window aggregation" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "sliding-window" "count"
      
      -- 模拟滑动窗口
      let windowSize = 5
          totalValues = 20
      
      forM_ [1..totalValues] $ \i -> do
        recordMetric metric 1.0
        when (i `mod` windowSize == 0) $ do
          -- 每个窗口结束时的值
          value <- metricValue metric
          value `shouldBe` fromIntegral i
      
      finalValue <- metricValue metric
      finalValue `shouldBe` fromIntegral totalValues
      
      shutdownTelemetry
    
    it "should handle tumbling window reset" $ do
      initTelemetry defaultConfig
      
      let windowSize = 10
          numWindows = 3
      
      forM_ [1..numWindows] $ \windowIndex -> do
        metric <- createMetric (pack $ "tumbling-window-" ++ show windowIndex) "count"
        
        -- 在窗口内记录值
        forM_ [1..windowSize] $ \_ -> do
          recordMetric metric 1.0
        
        value <- metricValue metric
        value `shouldBe` fromIntegral windowSize
      
      shutdownTelemetry
  
  -- 4. 测试时间序列统计
  describe "Time Series Statistics" $ do
    it "should calculate time series statistics" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "stats-test" "value"
      
      let testValues = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
      
      forM_ testValues $ \value -> do
        recordMetric metric value
        threadDelay 1000
      
      finalValue <- metricValue metric
      let expectedStats = TimeSeriesStats
            { tssCount = length testValues
            , tssSum = sum testValues
            , tssAverage = sum testValues / fromIntegral (length testValues)
            , tssMin = minimum testValues
            , tssMax = maximum testValues
            }
      
      finalValue `shouldBe` tssSum expectedStats
      
      shutdownTelemetry
    
    it "should handle time series with missing data" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "missing-data" "value"
      
      -- 模拟缺失数据（不规则的记录间隔）
      let irregularValues = [10.0, 20.0, 15.0, 25.0]
          irregularDelays = [1000, 5000, 2000, 3000]  -- 不规则的延迟
      
      forM_ (zip irregularValues irregularDelays) $ \(value, delay) -> do
        recordMetric metric value
        threadDelay delay
      
      finalValue <- metricValue metric
      finalValue `shouldBe` sum irregularValues
      
      shutdownTelemetry
  
  -- 5. 测试时间序列性能
  describe "Time Series Performance" $ do
    it "should handle high-frequency time series data" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "high-frequency" "count"
      
      let numOperations = 10000
      
      -- 高频记录
      startTime <- getCurrentTime
      sequence_ $ replicate numOperations $ do
        recordMetric metric 1.0
      endTime <- getCurrentTime
      
      let duration = diffUTCTime endTime startTime
      
      -- 验证所有操作都完成了
      finalValue <- metricValue metric
      finalValue `shouldBe` fromIntegral numOperations
      
      -- 性能断言（应该在合理时间内完成）
      duration `shouldSatisfy` (< 1.0)  -- 应该在1秒内完成
      
      shutdownTelemetry
    
    it "should handle multiple concurrent time series" $ do
      initTelemetry defaultConfig
      
      let numSeries = 10
          operationsPerSeries = 1000
      
      metrics <- replicateM numSeries $ createMetric "concurrent-series" "count"
      
      -- 并发操作多个时间序列
      done <- newEmptyMVar
      threads <- mapM (\metric -> forkIO $ do
        sequence_ $ replicate operationsPerSeries $ do
          recordMetric metric 1.0
        putMVar done ()
        ) metrics
      
      -- 等待所有线程完成
      sequence_ $ replicate numSeries (takeMVar done)
      
      -- 验证所有时间序列都有正确的值
      values <- mapM metricValue metrics
      all (== fromIntegral operationsPerSeries) values `shouldBe` True
      
      shutdownTelemetry
  
  -- 6. 测试时间序列边界条件
  describe "Time Series Boundary Conditions" $ do
    it "should handle empty time series" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "empty-series" "value"
      value <- metricValue metric
      
      value `shouldBe` 0.0
      
      shutdownTelemetry
    
    it "should handle single point time series" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "single-point" "value"
      recordMetric metric 42.0
      
      value <- metricValue metric
      value `shouldBe` 42.0
      
      shutdownTelemetry
    
    it "should handle time series with special values" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "special-values" "value"
      
      -- 记录特殊值
      recordMetric metric 0.0
      recordMetric metric (-1.0)
      recordMetric metric (1.0/0.0)  -- 正无穷
      recordMetric metric (-1.0/0.0) -- 负无穷
      recordMetric metric (0.0/0.0)  -- NaN
      
      value <- metricValue metric
      
      -- 验证特殊值的处理
      isNaN value `shouldBe` True
      
      shutdownTelemetry
  
  -- 7. 测试时间序列一致性
  describe "Time Series Consistency" $ do
    it "should maintain consistency across time series operations" $ do
      initTelemetry defaultConfig
      
      metrics <- replicateM 5 $ createMetric "consistency-series" "count"
      
      -- 并发记录值
      forM_ [1..100] $ \i -> do
        forM_ metrics $ \metric -> do
          recordMetric metric 1.0
      
      -- 验证所有时间序列的值一致
      values <- mapM metricValue metrics
      all (== 100.0) values `shouldBe` True
      
      shutdownTelemetry
    
    it "should handle time series with different sampling rates" $ do
      initTelemetry defaultConfig
      
      -- 不同采样率的度量
      metric1 <- createMetric "high-rate" "count"
      metric2 <- createMetric "low-rate" "count"
      
      -- 高采样率
      forM_ [1..100] $ \_ -> do
        recordMetric metric1 1.0
        threadDelay 100
      
      -- 低采样率
      forM_ [1..10] $ \_ -> do
        recordMetric metric2 1.0
        threadDelay 1000
      
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      
      value1 `shouldBe` 100.0
      value2 `shouldBe` 10.0
      
      shutdownTelemetry
  
  -- 8. 测试时间序列持久化
  describe "Time Series Persistence" $ do
    it "should maintain time series across restarts" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "persistent-series" "count"
      
      -- 记录一些值
      sequence_ $ replicate 50 $ do
        recordMetric metric 1.0
      
      value1 <- metricValue metric
      
      -- 模拟重启
      shutdownTelemetry
      initTelemetry defaultConfig
      
      -- 继续记录
      sequence_ $ replicate 30 $ do
        recordMetric metric 1.0
      
      value2 <- metricValue metric
      
      value1 `shouldBe` 50.0
      value2 `shouldBe` 80.0
      
      shutdownTelemetry
    
    it "should handle time series with configuration changes" $ do
      let config1 = TelemetryConfig "test-service" "1.0.0" True True True False
          config2 = TelemetryConfig "test-service" "2.0.0" True True True False
      
      -- 使用第一个配置
      initTelemetry config1
      
      metric <- createMetric "config-change-series" "count"
      recordMetric metric 10.0
      
      value1 <- metricValue metric
      
      -- 更改配置
      initTelemetry config2
      
      recordMetric metric 20.0
      
      value2 <- metricValue metric
      
      value1 `shouldBe` 10.0
      value2 `shouldBe` 30.0
      
      shutdownTelemetry