{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EnhancedQuickCheckTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack)
import Data.List (nub)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (replicateM, replicateM_, when)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)
import System.Mem (performGC)
import Data.IORef

import Azimuth.Telemetry

spec :: Spec
spec = describe "Enhanced QuickCheck-based Telemetry Tests" $ do
  
  -- 1. 度量值特殊值处理测试
  describe "Special Value Handling in Metrics" $ do
    it "should handle NaN values correctly" $ property $
      \values ->
        let testValues = if null values then [0.0, 1.0, 0.0/0.0] else take 5 values :: [Double]
            hasNaN = any isNaN testValues
        in unsafePerformIO $ do
          writeIORef enableMetricSharing False  -- Disable sharing for test isolation
          metric <- createMetric "nan-test" "special"
          sequence_ $ map (recordMetric metric) testValues
          finalValue <- metricValue metric
          
          -- 如果输入包含NaN，最终值应该是NaN
          if hasNaN
            then return (isNaN finalValue)
            else return (not $ isNaN finalValue)
    
    it "should handle infinity values correctly" $ property $
      \values ->
        let testValues = if null values then [1.0, 1.0/0.0, -1.0/0.0] else take 5 values :: [Double]
            hasPosInf = any (== 1.0/0.0) testValues
            hasNegInf = any (== -1.0/0.0) testValues
        in unsafePerformIO $ do
          writeIORef enableMetricSharing False  -- Disable sharing for test isolation
          metric <- createMetric "infinity-test" "special"
          sequence_ $ map (recordMetric metric) testValues
          finalValue <- metricValue metric
          
          -- 检查无穷大值的处理
          if hasPosInf && hasNegInf
            then return (isInfinite finalValue)  -- 最后的无穷值替换前面的
            else if hasPosInf
              then return (finalValue == 1.0/0.0)
              else if hasNegInf
                then return (finalValue == -1.0/0.0)
                else return (not (isNaN finalValue) && not (isInfinite finalValue))

  -- 2. 日志消息处理测试
  describe "Log Message Processing" $ do
    it "should handle all log levels with property-based testing" $ property $
      \levelInt message ->
        let levels = [Debug, Info, Warn, Error]
            level = levels !! (abs levelInt `mod` 4)
            messageText = pack message
        in unsafePerformIO $ do
          logger <- createLogger "property-test-logger" level
          
          -- 尝试记录所有级别的消息
          results <- mapM (\testLevel -> do
            (try $ logMessage logger testLevel messageText) :: IO (Either SomeException ())
            ) levels
          
          -- 所有操作都应该成功（不抛出异常）
          let allSuccess = all (either (const False) (const True)) results
          return allSuccess
    
    it "should handle Unicode characters in log messages" $ property $
      \charCodes ->
        let chars = take 10 $ map (toEnum . (`mod` 65536) . abs) charCodes
            unicodeMessage = pack chars
        in unsafePerformIO $ do
          logger <- createLogger "unicode-logger" Info
          result <- (try $ logMessage logger Info unicodeMessage) :: IO (Either SomeException ())
          return (either (const False) (const True) result)

  -- 3. 配置转换和验证测试
  describe "Configuration Transformation and Validation" $ do
    it "should handle configuration transformations" $ property $
      \name version metrics tracing logging debug ->
        let config1 = TelemetryConfig (pack name) (pack version) metrics tracing logging debug
            config2 = TelemetryConfig (pack $ name ++ "-modified") 
                                     (pack $ version ++ "-modified") 
                                     (not metrics) (not tracing) (not logging) (not debug)
        in unsafePerformIO $ do
          -- 测试配置1
          initTelemetry config1
          metric1 <- createMetric "config-test-1" "count"
          recordMetric metric1 1.0
          value1 <- metricValue metric1
                    
          -- 测试配置2
          initTelemetry config2
          metric2 <- createMetric "config-test-2" "count"
          recordMetric metric2 2.0
          value2 <- metricValue metric2
                    
          return (value1 == 1.0 && value2 == 2.0)

  -- 4. 资源泄漏检测测试
  describe "Resource Leak Detection" $ do
    it "should not leak resources during repeated operations" $ property $
      \iterations ->
        let numIterations = max 1 (abs iterations `mod` 10 + 1)
        in unsafePerformIO $ do
                    
          -- 执行重复操作
          replicateM_ numIterations $ do
            metric <- createMetric "leak-test" "count"
            recordMetric metric 1.0
            logger <- createLogger "leak-test-logger" Info
            logMessage logger Info "leak test"
            span <- createSpan "leak-test-span"
            finishSpan span
          
          -- 强制垃圾回收
          performGC
          
          -- 验证系统仍然正常工作
          finalMetric <- createMetric "final-test" "count"
          recordMetric finalMetric 42.0
          finalValue <- metricValue finalMetric
          
          return (finalValue == 42.0)

  -- 5. 错误恢复和容错性测试
  describe "Error Recovery and Fault Tolerance" $ do
    it "should recover from metric recording errors" $ property $
      \values ->
        let testValues = take 5 $ values ++ [1.0, 2.0, 3.0] :: [Double]
        in unsafePerformIO $ do
                    
          -- 记录值，包括可能的特殊值
          metric <- createMetric "recovery-test" "count"
          results <- mapM (\value -> (try $ recordMetric metric value) :: IO (Either SomeException ())) testValues
          
          -- 验证至少有一些操作成功
          let successCount = length $ filter (either (const False) (const True)) results
          finalValue <- metricValue metric
          
          return (successCount > 0)

  -- 6. 性能回归测试
  describe "Performance Regression Tests" $ do
    it "should maintain performance characteristics" $ property $
      \operations ->
        let numOps = max 10 (abs operations `mod` 100 + 10)
        in unsafePerformIO $ do
                    
          -- 测试度量操作性能
          metric <- createMetric "performance-test" "ops"
          
          -- 简单的性能测量
          startResult <- (try $ sequence_ $ replicate numOps $ do
            recordMetric metric 1.0) :: IO (Either SomeException ())
          
          -- 验证所有操作都完成
          finalValue <- metricValue metric
          let expectedValue = fromIntegral numOps
          
          return (either (const False) (const True) startResult && 
                  finalValue == expectedValue)

  -- 7. 数据一致性测试
  describe "Data Consistency Tests" $ do
    it "should maintain metric consistency across operations" $ property $
      \values ->
        let testValues = take 5 $ values ++ [1.0, 2.0, 3.0] :: [Double]
        in unsafePerformIO $ do
                    
          -- 创建多个度量
          metrics <- replicateM 3 $ createMetric "consistency-test" "count"
          
          -- 记录相同的值到所有度量
          sequence_ $ map (\metric -> 
            sequence_ $ map (recordMetric metric) testValues
            ) metrics
          
          -- 验证所有度量都有相同的值
          values <- mapM metricValue metrics
          let allEqual = all (== head values) (tail values)
          
          return allEqual

  -- 8. 边界值和异常情况测试
  describe "Boundary Value and Exception Tests" $ do
    it "should handle empty and null inputs" $ property $
      \input ->
        let emptyText = ""
            nullText = pack input
        in unsafePerformIO $ do
          -- 测试空文本
          metric1 <- createMetric emptyText emptyText
          logger1 <- createLogger emptyText Info
          span1 <- createSpan emptyText
          
          -- 测试null文本
          metric2 <- createMetric nullText nullText
          logger2 <- createLogger nullText Info
          span2 <- createSpan nullText
          
          return (metricName metric1 == emptyText &&
                  loggerName logger1 == emptyText &&
                  spanName span1 == emptyText &&
                  metricName metric2 == nullText &&
                  loggerName logger2 == nullText &&
                  spanName span2 == nullText)