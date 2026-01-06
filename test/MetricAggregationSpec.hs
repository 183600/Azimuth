{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MetricAggregationSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Control.Monad (foldM)
import Data.Text (pack)
import Data.List (sort, group)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Metric Aggregation Tests" $ do
  
  -- 测试度量累加的聚合
  describe "Metric Sum Aggregation" $ do
    it "should correctly aggregate sum of values" $ property $
      \(values :: [Double]) ->
        let testValues = take 100 values :: [Double]
            expectedSum = sum testValues
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "sum-aggregation" "count" 0.0
              sequence_ $ map (recordMetric metric) testValues
              metricValue metric
        in result == expectedSum || isNaN result || isInfinite result
    
    it "should handle empty list aggregation" $ do
      let result = unsafePerformIO $ do
            metric <- createMetricWithInitialValue "empty-sum" "count" 0.0
            metricValue metric
      result `shouldBe` 0.0
    
    it "should handle single value aggregation" $ property $
      \(value :: Double) ->
        let result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "single-sum" "count" 0.0
              recordMetric metric value
              metricValue metric
        in result == value
    
    it "should handle aggregation of identical values" $ property $
      \(value :: Double) (count :: Int) ->
        let numValues = max 1 (abs count `mod` 20 + 1)
            expectedValue = value * fromIntegral numValues
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "identical-sum" "count" 0.0
              sequence_ $ replicate numValues $ recordMetric metric value
              metricValue metric
            -- Allow for floating point precision errors
            tolerance = abs expectedValue * 1.0e-9 + 1.0e-9
        in abs (result - expectedValue) < tolerance || isNaN result || isInfinite result
  
  -- 测试度量平均值的计算
  describe "Metric Average Aggregation" $ do
    it "should correctly calculate average of values" $ property $
      \(values :: [Double]) ->
        let nonEmptyValues = if null values then [1.0, 2.0, 3.0] else take 100 values :: [Double]
            expectedAverage = sum nonEmptyValues / fromIntegral (length nonEmptyValues)
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "average-aggregation" "count" 0.0
              sequence_ $ map (recordMetric metric) nonEmptyValues
              total <- metricValue metric
              return (total / fromIntegral (length nonEmptyValues))
        in result == expectedAverage || isNaN result || isInfinite result
    
    it "should handle average of identical values" $ property $
      \(value :: Double) (count :: Int) ->
        let numValues = max 1 (abs count `mod` 20 + 1)
            expectedAverage = value
            result = unsafePerformIO $ do
              metric <- createMetricWithInitialValue "identical-average" "count" 0.0
              sequence_ $ replicate numValues $ recordMetric metric value
              total <- metricValue metric
              return (total / fromIntegral numValues)
            -- Allow for floating point precision errors
            tolerance = abs expectedAverage * 1.0e-9 + 1.0e-9
        in abs (result - expectedAverage) < tolerance || isNaN result || isInfinite result
  
  -- 测试度量最大值的跟踪
  
    describe "Metric Maximum Tracking" $ do
  
      it "should track maximum value in aggregation" $ property $
  
        \(values :: [Double]) ->
  
          let nonEmptyValues = if null values then [1.0, 2.0, 3.0] else take 100 values :: [Double]
  
              expectedMax = maximum nonEmptyValues
  
              result = unsafePerformIO $ do
  
                -- 使用多个度量来跟踪最大值
  
                maxMetric <- createMetricWithInitialValue "max-tracking" "count" (head nonEmptyValues)
  
                sequence_ $ map (\v -> do
  
                  currentMax <- metricValue maxMetric
  
                  if v > currentMax then do
  
                    -- 更新最大值：先重置为0，然后设置为新的最大值
  
                    recordMetric maxMetric (-currentMax)
  
                    recordMetric maxMetric v
  
                  else return ()
  
                              ) (tail nonEmptyValues)
  
                metricValue maxMetric
  
          in result == expectedMax || isNaN result || isInfinite result
  
  -- 测试度量最小值的跟踪
  describe "Metric Minimum Tracking" $ do
    it "should track minimum value in aggregation" $ property $
      \(values :: [Double]) ->
        let nonEmptyValues = if null values then [1.0, 2.0, 3.0] else take 100 values :: [Double]
            expectedMin = minimum nonEmptyValues
            result = unsafePerformIO $ do
              -- 使用多个度量来跟踪最小值
              minMetric <- createMetricWithInitialValue "min-tracking" "count" (head nonEmptyValues)
              sequence_ $ map (\v -> do
                currentMin <- metricValue minMetric
                if v < currentMin then do
                  -- 更新最小值：先重置为0，然后设置为新的最小值
                  recordMetric minMetric (-currentMin)
                  recordMetric minMetric v
                else return ()
                            ) (tail nonEmptyValues)
              metricValue minMetric
        in result == expectedMin || isNaN result || isInfinite result
  
  -- 测试度量计数
  describe "Metric Count Aggregation" $ do
    it "should correctly count number of values" $ property $
      \(values :: [Double]) ->
        let testValues = take 100 values :: [Double]
            expectedCount = length testValues
            result = unsafePerformIO $ do
              countMetric <- createMetricWithInitialValue "count-aggregation" "count" 0.0
              sequence_ $ map (\_ -> recordMetric countMetric 1.0) testValues
              metricValue countMetric
        in result == fromIntegral expectedCount
    
    it "should handle count of empty list" $ do
      let result = unsafePerformIO $ do
            countMetric <- createMetricWithInitialValue "empty-count" "count" 0.0
            metricValue countMetric
      result `shouldBe` 0.0
  
  -- 测试度量的分组聚合
  describe "Metric Grouped Aggregation" $ do
    it "should handle aggregation of grouped values" $ property $
      \(values :: [Double]) ->
        let testValues = take 50 values :: [Double]
            groups = group $ sort testValues
            groupSums = map sum groups
            expectedTotalSum = sum groupSums
            result = unsafePerformIO $ do
              totalMetric <- createMetricWithInitialValue "grouped-total" "count" 0.0
              sequence_ $ map (\groupSum -> recordMetric totalMetric groupSum) groupSums
              metricValue totalMetric
        in result == expectedTotalSum || isNaN result || isInfinite result
    
    it "should handle aggregation of positive and negative groups" $ property $
      \(values :: [Double]) ->
        let testValues = take 50 values :: [Double]
            positiveValues = filter (> 0) testValues
            negativeValues = filter (< 0) testValues
            positiveSum = sum positiveValues
            negativeSum = sum negativeValues
            expectedTotalSum = positiveSum + negativeSum
            result = unsafePerformIO $ do
              posMetric <- createMetricWithInitialValue "positive-group" "count" 0.0
              negMetric <- createMetricWithInitialValue "negative-group" "count" 0.0
              totalMetric <- createMetricWithInitialValue "grouped-total" "count" 0.0
              
              sequence_ $ map (recordMetric posMetric) positiveValues
              sequence_ $ map (recordMetric negMetric) negativeValues
              
              posValue <- metricValue posMetric
              negValue <- metricValue negMetric
              recordMetric totalMetric posValue
              recordMetric totalMetric negValue
              
              metricValue totalMetric
        in result == expectedTotalSum || isNaN result || isInfinite result
  
  -- 测试度量的时间序列聚合
  describe "Metric Time Series Aggregation" $ do
    it "should handle time-based aggregation" $ property $
      \(values :: [Double]) ->
        let testValues = take 20 values :: [Double]
            result = unsafePerformIO $ do
              -- 模拟时间序列数据
              timeMetrics <- sequence $ map (\i -> 
                createMetricWithInitialValue (pack $ "time-series-" ++ show i) "count" 0.0
                            ) [1..length testValues]
              
              -- 记录每个时间点的值
              sequence_ $ zipWith (\metric value -> 
                recordMetric metric value
                            ) timeMetrics testValues
              
              -- 聚合所有时间点的值
              totalMetric <- createMetricWithInitialValue "time-series-total" "count" 0.0
              sequence_ $ map (\metric -> do
                value <- metricValue metric
                recordMetric totalMetric value
                            ) timeMetrics
              
              metricValue totalMetric
            expectedTotal = sum testValues
        in result == expectedTotal || isNaN result || isInfinite result
  
  -- 测试度量的层级聚合
  describe "Metric Hierarchical Aggregation" $ do
    it "should handle hierarchical aggregation" $ property $
      \(values :: [Double]) ->
        let testValues = take 30 values :: [Double]
            -- 将值分成三个层级
            level1Values = take 10 testValues
            level2Values = take 10 (drop 10 testValues)
            level3Values = drop 20 testValues
            level1Sum = sum level1Values
            level2Sum = sum level2Values
            level3Sum = sum level3Values
            expectedTotalSum = level1Sum + level2Sum + level3Sum
            result = unsafePerformIO $ do
              -- 创建层级度量
              level1Metric <- createMetricWithInitialValue "level-1" "count" 0.0
              level2Metric <- createMetricWithInitialValue "level-2" "count" 0.0
              level3Metric <- createMetricWithInitialValue "level-3" "count" 0.0
              totalMetric <- createMetricWithInitialValue "hierarchical-total" "count" 0.0
              
              -- 记录每个层级的值
              sequence_ $ map (recordMetric level1Metric) level1Values
              sequence_ $ map (recordMetric level2Metric) level2Values
              sequence_ $ map (recordMetric level3Metric) level3Values
              
              -- 聚合层级数据
              level1Value <- metricValue level1Metric
              level2Value <- metricValue level2Metric
              level3Value <- metricValue level3Metric
              
              recordMetric totalMetric level1Value
              recordMetric totalMetric level2Value
              recordMetric totalMetric level3Value
              
              metricValue totalMetric
        in result == expectedTotalSum || isNaN result || isInfinite result
  
  -- 测试度量的滑动窗口聚合
  describe "Metric Sliding Window Aggregation" $ do
    it "should handle sliding window aggregation" $ property $
      \(values :: [Double]) ->
        let testValues = take 20 values :: [Double]
            windowSize = 5
            result = unsafePerformIO $ do
              -- 创建滑动窗口度量
              windowMetrics <- sequence $ map (\i -> 
                createMetricWithInitialValue (pack $ "window-" ++ show i) "count" 0.0
                            ) [1..windowSize]
              
              -- 模拟滑动窗口
              finalValue <- foldM (\_ (index, value) -> do
                let windowIndex = (index `mod` windowSize) + 1
                windowMetric <- return $ windowMetrics !! (windowIndex - 1)
                
                -- 重置当前窗口度量
                currentValue <- metricValue windowMetric
                recordMetric windowMetric (-currentValue)
                
                -- 添加新值
                recordMetric windowMetric value
                
                -- 计算当前窗口总和
                windowSum <- foldM (\acc metric -> do
                  value <- metricValue metric
                  return (acc + value)
                                ) 0.0 windowMetrics
                
                return windowSum
                            ) 0.0 (zip [0..] testValues)
              
              return finalValue
        in not (isNaN result) && not (isInfinite result)
  
  -- 测试度量的加权聚合
  describe "Metric Weighted Aggregation" $ do
    it "should handle weighted aggregation" $ property $
      \(values :: [Double]) (weights :: [Int]) ->
        let testValues = take 10 values :: [Double]
            testWeights = take 10 $ map (\w -> max 0.1 (fromIntegral (abs w `mod` 10) + 1)) weights :: [Double]
            weightedSum = sum $ zipWith (*) testValues testWeights
            result = unsafePerformIO $ do
              weightedMetric <- createMetricWithInitialValue "weighted-aggregation" "count" 0.0
              sequence_ $ zipWith (\value weight -> 
                recordMetric weightedMetric (value * weight)
                            ) testValues testWeights
              metricValue weightedMetric
        in result == weightedSum || isNaN result || isInfinite result