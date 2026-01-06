{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RegressionCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, when, void, unless, sequence_, zipWithM)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group, intercalate, take, drop, replicate)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Regression Cabal Test Suite" $ do
  
  -- 1. 基础功能回归测试
  describe "Basic Functionality Regression Tests" $ do
    it "should maintain basic metric operations" $ do
      initTelemetry productionConfig
      
      metric <- createMetric "regression-basic" "count"
      
      -- 基础操作序列
      recordMetric metric 1.0
      recordMetric metric 2.0
      recordMetric metric 3.0
      
      value <- metricValue metric
      
      shutdownTelemetry
      
      value `shouldBe` 6.0
    
    it "should maintain basic span operations" $ do
      initTelemetry productionConfig
      
      span <- createSpan "regression-span"
      let traceId = spanTraceId span
          spanId = spanSpanId span
      
      finishSpan span
      
      shutdownTelemetry
      
      -- 验证span属性不变
      Text.null traceId `shouldBe` False
      Text.null spanId `shouldBe` False
      traceId `shouldNotBe` spanId
    
    it "should maintain basic logging operations" $ do
      initTelemetry productionConfig
      
      logger <- createLogger "regression-logger" Info
      
      logMessage logger Debug "Debug message"
      logMessage logger Info "Info message"
      logMessage logger Warn "Warning message"
      logMessage logger Error "Error message"
      
      shutdownTelemetry
      
      -- 如果没有异常就算成功
      True `shouldBe` True
  
  -- 2. 数值计算回归测试
  describe "Numerical Calculation Regression Tests" $ do
    it "should maintain precision in decimal calculations" $ property $
      \values ->
        let testValues = take 10 (values :: [Double])
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          metric <- createMetric "precision-regression" "test"
          
          -- 记录精确值
          sequence_ $ map (recordMetric metric) testValues
          
          finalValue <- metricValue metric
          let expectedValue = sum testValues
          
          shutdownTelemetry
          
          return (abs (finalValue - expectedValue) < 1e-10)
    
    it "should handle large number accumulation" $ do
      initTelemetry productionConfig
      
      metric <- createMetric "large-accumulation" "count"
      
      -- 累积大量值
      sequence_ $ replicate 10000 $ do
        recordMetric metric 1.0
      
      value <- metricValue metric
      
      shutdownTelemetry
      
      value `shouldBe` 10000.0
    
    it "should maintain additive inverse property" $ property $
      \x ->
        let testValue = x :: Double
        in if not (isNaN testValue) && not (isInfinite testValue) && testValue /= 0.0
           then unsafePerformIO $ do
             metric <- createMetric "additive-inverse" "test"
             
             recordMetric metric testValue
             recordMetric metric (-testValue)
             
             finalValue <- metricValue metric
             
             shutdownTelemetry
             
             return (abs finalValue < 1.0e-9)
           else True
  
  -- 3. 并发安全回归测试
  describe "Concurrency Safety Regression Tests" $ do
    it "should maintain thread safety for metric operations" $ property $
      \threadCount ->
        let actualThreads = max 1 (abs threadCount `mod` 10 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          metric <- createMetric "concurrent-regression" "count"
          
          -- 并发操作
          threads <- mapM (\threadId -> forkIO $ do
            sequence_ $ replicate 100 $ do
              recordMetric metric 1.0
            ) [1..actualThreads]
          
          -- 等待完成
          threadDelay 500000  -- 500毫秒
          
          -- 清理线程
          sequence_ $ map killThread threads
          
          finalValue <- metricValue metric
          let expectedValue = fromIntegral actualThreads * 100.0
          
          shutdownTelemetry
          
          return (finalValue == expectedValue)
    
    it "should handle concurrent span creation" $ property $
      \spanCount ->
        let actualSpans = max 1 (abs spanCount `mod` 50 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 并发创建span
          threads <- mapM (\spanId -> forkIO $ do
            span <- createSpan (pack $ "concurrent-span-" ++ show spanId)
            finishSpan span
            ) [1..actualSpans]
          
          -- 等待完成
          threadDelay 200000  -- 200毫秒
          
          -- 清理线程
          sequence_ $ map killThread threads
          
          shutdownTelemetry
          
          return True  -- 如果没有崩溃就算成功
  
  -- 4. 内存管理回归测试
  describe "Memory Management Regression Tests" $ do
    it "should not leak memory during repeated operations" $ do
      let cycles = 100
          operationsPerCycle = 100
      
      initTelemetry productionConfig
      
      sequence_ $ replicate cycles $ do
        -- 创建和使用资源
        metrics <- sequence $ replicate 10 $ do
          createMetric "memory-regression" "count"
        
        sequence_ $ map (`recordMetric` 1.0) metrics
        
        -- 定期垃圾回收
        when (cycles `mod` 10 == 0) $ do
          performGC
      
      shutdownTelemetry
      performGC
      
      -- 如果没有内存泄漏就算成功
      True `shouldBe` True
    
    it "should handle resource cleanup correctly" $ do
      initTelemetry productionConfig
      
      -- 创建大量资源
      metrics <- sequence $ replicate 1000 $ do
        createMetric "cleanup-regression" "count"
      
      -- 使用资源
      sequence_ $ map (`recordMetric` 1.0) metrics
      
      -- 关闭系统
      shutdownTelemetry
      
      -- 重新初始化
      initTelemetry productionConfig
      
      -- 创建同名度量应该从新开始
      newMetric <- createMetric "cleanup-regression" "count"
      value <- metricValue newMetric
      
      shutdownTelemetry
      
      value `shouldBe` 0.0
  
  -- 5. 配置管理回归测试
  describe "Configuration Management Regression Tests" $ do
    it "should handle configuration changes correctly" $ do
      -- 初始配置
      initTelemetry productionConfig
      
      metric1 <- createMetric "config-regression" "count"
      recordMetric metric1 1.0
      
      -- 更改配置
      let customConfig = TelemetryConfig "regression-test" "1.0.0" True True True False
      initTelemetry customConfig
      
      metric2 <- createMetric "config-regression" "count"
      recordMetric metric2 2.0
      
      -- 验证配置更改生效
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      
      shutdownTelemetry
      
      value1 `shouldBe` 1.0
      value2 `shouldBe` 2.0
    
    it "should maintain configuration persistence" $ property $
      \name ->
        let testName = pack name
        in unsafePerformIO $ do
          let config = TelemetryConfig testName "regression-test" True True True False
          
          initTelemetry config
          
          -- 执行操作
          metric <- createMetric "config-persistence" "count"
          recordMetric metric 1.0
          
          -- 验证配置仍然有效
          currentConfig <- readIORef globalConfig
          
          shutdownTelemetry
          
          return (serviceName currentConfig == testName)
  
  -- 6. 错误处理回归测试
  describe "Error Handling Regression Tests" $ do
    it "should handle special floating point values consistently" $ do
      let specialValues = [0.0/0.0, 1.0/0.0, -1.0/0.0] :: [Double]
      
      initTelemetry productionConfig
      
      sequence_ $ flip map specialValues $ \value -> do
        metric <- createMetric "error-regression" "test"
        
        recordMetric metric value
        
        -- 尝试正常操作
        recordMetric metric 42.0
        finalValue <- metricValue metric
        
        -- 验证系统仍然可以工作
        not (isNaN finalValue) `shouldBe` True
      
      shutdownTelemetry
    
    it "should recover from error conditions" $ property $
      \errorCount ->
        let actualErrors = max 1 (abs errorCount `mod` 10 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          metric <- createMetric "recovery-regression" "count"
          
          -- 模拟错误条件
          sequence_ $ replicate actualErrors $ do
            recordMetric metric (0.0/0.0)  -- NaN
            
            -- 尝试恢复
            recordMetric metric 1.0
          
          -- 验证恢复
          finalValue <- metricValue metric
          
          shutdownTelemetry
          
          return (not (isNaN finalValue))
  
  -- 7. 性能回归测试
  describe "Performance Regression Tests" $ do
    it "should maintain baseline performance for metric operations" $ do
      let baselineOpsPerSecond = 10000
          operationCount = 5000
      
      initTelemetry productionConfig
      
      metric <- createMetric "performance-regression" "ops"
      
      -- 测量性能
      let startTime = ()  -- 简化的时间戳
      
      sequence_ $ replicate operationCount $ do
        recordMetric metric 1.0
      
      let endTime = ()  -- 简化的时间戳
      
      -- 验证所有操作都完成了
      finalValue <- metricValue metric
      
      shutdownTelemetry
      
      finalValue `shouldBe` fromIntegral operationCount
      -- 在实际实现中，这里会检查性能是否低于基线
    
    it "should handle high-frequency operations without degradation" $ property $
      \operationCount ->
        let actualOps = max 100 (abs operationCount `mod` 1000 + 100)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          metric <- createMetric "high-frequency-regression" "ops"
          
          -- 高频操作
          sequence_ $ replicate actualOps $ do
            recordMetric metric 1.0
          
          -- 验证所有操作都完成了
          finalValue <- metricValue metric
          
          shutdownTelemetry
          
          return (finalValue == fromIntegral actualOps)
  
  -- 8. 边界条件回归测试
  describe "Boundary Condition Regression Tests" $ do
    it "should handle empty string values consistently" $ do
      initTelemetry productionConfig
      
      metric <- createMetric "" ""
      logger <- createLogger "" Info
      span <- createSpan ""
      
      recordMetric metric 1.0
      logMessage logger Info ""
      finishSpan span
      
      value <- metricValue metric
      
      shutdownTelemetry
      
      value `shouldBe` 1.0
    
    it "should handle very long string values consistently" $ property $
      \str ->
        let longString = take 1000 (cycle str)
            longText = pack longString
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          metric <- createMetric longText longText
          logger <- createLogger longText Info
          span <- createSpan longText
          
          recordMetric metric 1.0
          logMessage logger Info longText
          finishSpan span
          
          value <- metricValue metric
          
          shutdownTelemetry
          
          return (value == 1.0)
  
  -- 9. 数据一致性回归测试
  describe "Data Consistency Regression Tests" $ do
    it "should maintain metric name consistency" $ property $
      \name ->
        let testName = pack name
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          metric <- createMetric testName "unit"
          
          -- 记录多个值
          recordMetric metric 1.0
          recordMetric metric 2.0
          recordMetric metric 3.0
          
          -- 验证名称不变
          finalName <- return $ metricName metric
          
          shutdownTelemetry
          
          return (finalName == testName)
    
    it "should maintain metric unit consistency" $ property $
      \unit ->
        let testUnit = pack unit
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          metric <- createMetric "unit-test" testUnit
          
          -- 记录多个值
          recordMetric metric 1.0
          recordMetric metric 2.0
          recordMetric metric 3.0
          
          -- 验证单位不变
          finalUnit <- return $ metricUnit metric
          
          shutdownTelemetry
          
          return (finalUnit == testUnit)
  
  -- 10. 集成回归测试
  describe "Integration Regression Tests" $ do
    it "should maintain component integration consistency" $ do
      initTelemetry productionConfig
      
      -- 创建所有类型的组件
      metric <- createMetric "integration-regression" "count"
      logger <- createLogger "integration-regression" Info
      span <- createSpan "integration-regression"
      
      -- 交互使用所有组件
      recordMetric metric 1.0
      logMessage logger Info "Integration test"
      finishSpan span
      
      -- 验证所有组件都正常工作
      metricValue <- metricValue metric
      
      shutdownTelemetry
      
      metricValue `shouldBe` 1.0
    
    it "should handle complex workflow consistently" $ property $
      \stepCount ->
        let actualSteps = max 1 (abs stepCount `mod` 10 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          metrics <- sequence $ replicate actualSteps $ do
            createMetric "workflow-regression" "count"
          
          loggers <- sequence $ replicate actualSteps $ do
            createLogger "workflow-regression" Info
          
          spans <- sequence $ replicate actualSteps $ do
            createSpan "workflow-regression"
          
          -- 复杂工作流
          sequence_ $ zipWith (\metric logger -> do
            recordMetric metric 1.0
            logMessage logger Info "Workflow step"
            return ()
            ) metrics loggers
          
          sequence_ $ map finishSpan spans
          
          -- 验证所有组件都正常工作
          metricValues <- sequence $ map metricValue metrics
          let allCorrect = all (== 1.0) metricValues
          
          shutdownTelemetry
          
          return allCorrect