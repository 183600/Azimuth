{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MetricAggregationTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Monad (replicateM, replicateM_, when)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (sum)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Metric Aggregation Tests" $ do
  
  -- 1. 测试指标值的累加
  describe "Metric Value Accumulation" $ do
    it "should accumulate positive values correctly" $ do
      metric <- createMetric "positive-test" "count"
      recordMetric metric 1.0
      recordMetric metric 2.0
      recordMetric metric 3.0
      value <- metricValue metric
      value `shouldBe` 6.0
    
    it "should accumulate negative values correctly" $ do
      metric <- createMetric "negative-test" "count"
      recordMetric metric 1.0
      recordMetric metric (-2.0)
      recordMetric metric 3.0
      value <- metricValue metric
      value `shouldBe` 2.0
    
    it "should handle zero values correctly" $ do
      metric <- createMetric "zero-test" "count"
      recordMetric metric 0.0
      recordMetric metric 1.0
      recordMetric metric 0.0
      recordMetric metric 2.0
      value <- metricValue metric
      value `shouldBe` 3.0
  
  -- 2. 测试指标重置
  describe "Metric Reset" $ do
    it "should reset metrics correctly" $ do
      metric <- createMetric "reset-test" "count"
      recordMetric metric 5.0
      recordMetric metric 3.0
      value1 <- metricValue metric
      value1 `shouldBe` 8.0
      
      -- 通过创建同名指标来"重置"
      writeIORef enableMetricSharing False
      newMetric <- createMetric "reset-test" "count"
      recordMetric newMetric 1.0
      value2 <- metricValue newMetric
      value2 `shouldBe` 1.0
      writeIORef enableMetricSharing True
  
  -- 3. 测试指标的并发更新
  describe "Concurrent Metric Updates" $ do
    it "should handle concurrent updates safely" $ do
      writeIORef enableMetricSharing False
      metric <- createMetric "concurrent-test" "count"
      
      let numThreads = 10
          operationsPerThread = 100
      
      threads <- mapM (\_ -> forkIO $ do
        replicateM_ operationsPerThread $ do
          recordMetric metric 1.0
        ) [1..numThreads]
      
      -- 等待所有线程完成
      threadDelay 1000000  -- 1秒
      mapM_ killThread threads
      
      finalValue <- metricValue metric
      finalValue `shouldBe` fromIntegral (numThreads * operationsPerThread)
      writeIORef enableMetricSharing True
  
  -- 4. 测试指标的初始值设置
  describe "Metric Initial Values" $ do
    it "should create metrics with custom initial values" $ do
      metric <- createMetricWithInitialValue "initial-test" "count" 10.0
      value <- metricValue metric
      value `shouldBe` 10.0
      
      recordMetric metric 5.0
      value2 <- metricValue metric
      value2 `shouldBe` 15.0
    
    it "should handle negative initial values" $ do
      metric <- createMetricWithInitialValue "negative-initial-test" "count" (-5.0)
      value <- metricValue metric
      value `shouldBe` (-5.0)
      
      recordMetric metric 3.0
      value2 <- metricValue metric
      value2 `shouldBe` (-2.0)
  
  -- 5. 测试QuickCheck属性：累加结合律
  describe "QuickCheck Properties" $ do
    it "should satisfy associativity of addition" $ property $
      \(x :: Double) (y :: Double) (z :: Double) ->
        not (isNaN x || isNaN y || isNaN z || isInfinite x || isInfinite y || isInfinite z) ==> 
        unsafePerformIO $ do
          metric1 <- createMetric "associativity-test-1" "count"
          metric2 <- createMetric "associativity-test-2" "count"
          metric3 <- createMetric "associativity-test-3" "count"
          
          -- (x + y) + z
          recordMetric metric1 x
          recordMetric metric1 y
          recordMetric metric1 z
          value1 <- metricValue metric1
          
          -- x + (y + z)
          recordMetric metric2 x
          recordMetric metric3 y
          recordMetric metric3 z
          value3 <- metricValue metric3
          recordMetric metric2 value3
          value2 <- metricValue metric2
          
          return (abs (value1 - value2) < 1.0e-9)
    
    it "should satisfy commutativity of addition" $ property $
      \(x :: Double) (y :: Double) ->
        not (isNaN x || isNaN y || isInfinite x || isInfinite y) ==>
        unsafePerformIO $ do
          metric1 <- createMetric "commutativity-test-1" "count"
          metric2 <- createMetric "commutativity-test-2" "count"
          
          -- x + y
          recordMetric metric1 x
          recordMetric metric1 y
          value1 <- metricValue metric1
          
          -- y + x
          recordMetric metric2 y
          recordMetric metric2 x
          value2 <- metricValue metric2
          
          return (value1 == value2)
    
    it "should satisfy identity element of addition" $ property $
      \(x :: Double) ->
        not (isNaN x || isInfinite x) ==>
        unsafePerformIO $ do
          metric <- createMetric "identity-test" "count"
          
          -- x + 0 = x
          recordMetric metric x
          recordMetric metric 0.0
          value <- metricValue metric
          
          return (value == x)