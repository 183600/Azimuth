{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ComprehensiveTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate, bracket)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar, modifyMVar_)
import Control.Monad (replicateM, when, void, foldM)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group)
import Data.Char (isAscii, isControl)
import qualified Data.Map as Map
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Comprehensive Telemetry Tests" $ do
  
  -- 1. 测试度量注册表的共享机制
  describe "Metric Registry Sharing" $ do
    it "should share metrics with same name and unit" $ do
        -- Save current sharing setting
        originalSharing <- readIORef enableMetricSharing
        
        -- Enable metric sharing for this test
        writeIORef enableMetricSharing True
        
        let nameText = pack "test-metric"
            unitText = pack "count"
        
        metric1 <- createMetric nameText unitText
        metric2 <- createMetric nameText unitText
        
        -- 记录不同的值
        recordMetric metric1 10.0
        recordMetric metric2 20.0
        
        -- 验证它们共享相同的值
        value1 <- metricValue metric1
        value2 <- metricValue metric2
        
        value1 `shouldBe` value2
        value1 `shouldBe` 30.0
        
        -- Restore original sharing setting
        writeIORef enableMetricSharing originalSharing
    
    it "should not share metrics with different units" $ property $
      \(name :: String) (unit1 :: String) (unit2 :: String) ->
        let nameText = pack name
            unit1Text = pack unit1
            unit2Text = pack unit2
        in if unit1 /= unit2 && not (null name)
           then unsafePerformIO $ do
             -- 清理之前的度量注册表以确保测试隔离
             modifyMVar_ metricRegistry (\_ -> return Map.empty)
             
             metric1 <- createMetric nameText unit1Text
             metric2 <- createMetric nameText unit2Text
             
             -- 记录不同的值
             recordMetric metric1 10.0
             recordMetric metric2 20.0
             
             -- 验证它们不共享相同的值
             value1 <- metricValue metric1
             value2 <- metricValue metric2
             
             return (value1 == 10.0 && value2 == 20.0)
           else True

  -- 2. 测试跨线程的trace ID传播
  describe "Cross-thread Trace Propagation" $ do
    it "should propagate trace ID across threads" $ property $
      \(spanName :: String) ->
        let nameText = pack spanName
        in unsafePerformIO $ do
          -- 在主线程创建span
          mainSpan <- createSpan nameText
          let mainTraceId = spanTraceId mainSpan
          
          -- 创建MVar用于线程间通信
          traceIdVar <- newEmptyMVar
          
          -- 在子线程中创建span
          threadId <- forkIO $ do
            childSpan <- createSpan (nameText <> "-child")
            putMVar traceIdVar (spanTraceId childSpan)
          
          -- 等待子线程完成并获取trace ID
          childTraceId <- takeMVar traceIdVar
          killThread threadId
          
          -- 验证trace ID相同
          return (mainTraceId == childTraceId)
    
    it "should maintain trace context across multiple thread generations" $ property $
      \(spanName :: String) ->
        let nameText = pack spanName
        in unsafePerformIO $ do
                    
          -- 创建根span
          rootSpan <- createSpan nameText
          let rootTraceId = spanTraceId rootSpan
          
          -- 创建多级线程层次结构
          traceIdVar1 <- newEmptyMVar
          traceIdVar2 <- newEmptyMVar
          
          -- 第一级子线程
          threadId1 <- forkIO $ do
            childSpan1 <- createSpan (nameText <> "-child1")
            putMVar traceIdVar1 (spanTraceId childSpan1)
            
            -- 第二级子线程
            threadId2 <- forkIO $ do
              childSpan2 <- createSpan (nameText <> "-child2")
              putMVar traceIdVar2 (spanTraceId childSpan2)
            
            threadDelay 10000
            killThread threadId2
          
          -- 等待所有线程完成
          childTraceId1 <- takeMVar traceIdVar1
          childTraceId2 <- takeMVar traceIdVar2
          
          killThread threadId1
                    
          -- 验证所有span都有相同的trace ID
          return (rootTraceId == childTraceId1 && rootTraceId == childTraceId2)

  -- 3. 测试度量值的精度和数学属性
  describe "Metric Precision and Mathematical Properties" $ do
    it "should handle floating point precision correctly" $ property $
      \(values :: [Double]) ->
        let nonEmptyValues = if null values then [1.0, 2.0, 3.0] else take 10 values
        in unsafePerformIO $ do
          metric <- createMetricWithInitialValue "precision-test" "count" 0.0
          
          -- 记录值
          sequence_ $ map (recordMetric metric) nonEmptyValues
          
          -- 获取最终值
          finalValue <- metricValue metric
          let expectedValue = sum nonEmptyValues
          
          -- 对于浮点数，使用近似比较
          return (abs (finalValue - expectedValue) < 1e-10)
    
    it "should satisfy commutative property of addition" $ property $
      \(values :: [Double]) ->
        let nonEmptyValues = if null values then [1.0, 2.0, 3.0, 4.0, 5.0] else take 5 values
            forwardSum = sum nonEmptyValues
            reverseSum = sum (reverse nonEmptyValues)
        in unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "commutative-test-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "commutative-test-2" "count" 0.0
          
          -- 正向记录
          sequence_ $ map (recordMetric metric1) nonEmptyValues
          
          -- 反向记录
          sequence_ $ map (recordMetric metric2) (reverse nonEmptyValues)
          
          value1 <- metricValue metric1
          value2 <- metricValue metric2
          
          -- 验证交换律
          return (abs (value1 - value2) < 1e-10 && 
                  abs (value1 - forwardSum) < 1e-10 &&
                  abs (value2 - reverseSum) < 1e-10)
    
    it "should satisfy associative property of addition" $ property $
      \(values :: [Double]) ->
        let nonEmptyValues = if null values then [1.0, 2.0, 3.0, 4.0] else take 4 values
            (firstGroup, secondGroup) = splitAt (length nonEmptyValues `div` 2) nonEmptyValues
            group1Sum = sum firstGroup
            group2Sum = sum secondGroup
            totalSum = group1Sum + group2Sum
        in unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "associative-test-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "associative-test-2" "count" 0.0
          metric3 <- createMetricWithInitialValue "associative-test-3" "count" 0.0
          
          -- 分别记录两个组
          sequence_ $ map (recordMetric metric1) firstGroup
          sequence_ $ map (recordMetric metric2) secondGroup
          
          -- 记录所有值
          sequence_ $ map (recordMetric metric3) nonEmptyValues
          
          value1 <- metricValue metric1
          value2 <- metricValue metric2
          value3 <- metricValue metric3
          
          -- 验证结合律
          return (abs ((value1 + value2) - value3) < 1e-10 &&
                  abs (value3 - totalSum) < 1e-10)

  -- 4. 测试Span生命周期的状态管理
  describe "Span Lifecycle State Management" $ do
    it "should maintain span identity throughout lifecycle" $ property $
      \(spanNameParam :: String) ->
        let nameText = pack spanNameParam
        in unsafePerformIO $ do
          -- 创建span
          span <- createSpan nameText
          let originalName = spanName span
              originalTraceId = spanTraceId span
              originalSpanId = spanSpanId span
          
          -- 执行一些操作
          threadDelay 1000  -- 模拟一些工作
          
          -- 完成span
          finishSpan span
          
          -- 验证span属性保持不变
          return (originalName == nameText &&
                  not (Text.null originalTraceId) &&
                  not (Text.null originalSpanId) &&
                  originalTraceId == spanTraceId span &&
                  originalSpanId == spanSpanId span)
    
    it "should handle span creation in rapid succession" $ property $
      \(spanCount :: Int) ->
        let actualCount = max 1 (abs spanCount `mod` 20 + 1)
            baseName = "rapid-span"
        in unsafePerformIO $ do
          -- 快速创建多个span
          spans <- sequence $ replicate actualCount $ do
            createSpan (pack $ baseName ++ "-" ++ show (actualCount :: Int))
          
          -- 验证所有span都有有效的ID
          let allHaveValidIds = all (\span -> 
                not (Text.null (spanTraceId span)) && 
                not (Text.null (spanSpanId span))) spans
          
          -- 验证span ID的唯一性
          let spanIds = map spanSpanId spans
              uniqueSpanIds = nub spanIds
              allSpanIdsUnique = length spanIds == length uniqueSpanIds
          
          -- 完成所有span
          sequence_ $ map finishSpan spans
          
          return (allHaveValidIds && allSpanIdsUnique)

  -- 5. 测试配置热更新机制
  describe "Configuration Hot Updates" $ do
    it "should handle configuration changes without affecting operations" $ property $
      \(serviceName :: String) (enableDebug :: Bool) ->
        let config1 = TelemetryConfig (pack serviceName) "1.0.0" True True True enableDebug
            config2 = TelemetryConfig (pack $ serviceName ++ "-updated") "2.0.0" False True False (not enableDebug)
        in unsafePerformIO $ do
          -- 初始化第一个配置
          initTelemetry config1
          
          -- 创建一些组件
          metric <- createMetric "config-test" "count"
          logger <- createLogger "config-test-logger" Info
          span <- createSpan "config-test-span"
          
          -- 使用组件
          recordMetric metric 10.0
          logMessage logger Info "config test message"
          
          -- 更新配置
          initTelemetry config2
          
          -- 继续使用组件
          recordMetric metric 20.0
          logMessage logger Info "updated config test message"
          finishSpan span
          
          -- 验证操作仍然正常
          value <- metricValue metric
          
                    
          return (value == 30.0)

  -- 6. 测试错误恢复机制
  describe "Error Recovery Mechanisms" $ do
    it "should recover from metric operation failures" $ do
        let testValues = [1.0, 2.0, 3.0, 4.0, 5.0]
        metric <- createMetric "error-recovery-test" "count"
        
        -- 尝试记录可能引起问题的值
        results <- sequence $ map (\value -> do
          try $ recordMetric metric value
          ) testValues
        
        -- 验证所有操作都成功（或被正确处理）
        let allSuccess = all (\result -> case result of
              Left (_ :: SomeException) -> False
              Right _ -> True) results
        
        -- 验证最终状态
        finalValue <- metricValue metric
        let expectedValue = sum testValues
        
        allSuccess `shouldBe` True
        finalValue `shouldBe` expectedValue
    
    it "should handle span creation failures gracefully" $ property $
      \(spanNames :: [String]) ->
        let testNames = if null spanNames then ["test1", "test2", "test3"] else take 5 spanNames
        in unsafePerformIO $ do
          -- 尝试创建可能引起问题的span
          results <- sequence $ map (\name -> do
            try $ createSpan (pack name)
            ) testNames
          
          -- 验证所有操作都成功（或被正确处理）
          let allSuccess = all (\result -> case result of
                Left (_ :: SomeException) -> False
                Right _ -> True) results
          
          -- 如果成功，验证span属性
          if allSuccess
            then do
              let spans = map (\result -> case result of
                    Right span -> span
                    Left _ -> error "Impossible: allSuccess is True") results
              let allHaveValidIds = all (\span -> 
                    not (Text.null (spanTraceId span)) && 
                    not (Text.null (spanSpanId span))) spans
              return allHaveValidIds
            else return False

  -- 7. 测试资源泄漏检测
  describe "Resource Leak Detection" $ do
    it "should not leak resources during repeated operations" $ property $
      \(iterations :: Int) ->
        let actualIterations = max 1 (abs iterations `mod` 100 + 1)
        in unsafePerformIO $ do
                    
          -- 执行大量操作
          sequence_ $ replicate actualIterations $ do
            metric <- createMetric "leak-test" "count"
            recordMetric metric 1.0
            
            logger <- createLogger "leak-test-logger" Info
            logMessage logger Info "leak test message"
            
            span <- createSpan "leak-test-span"
            finishSpan span
          
          -- 验证系统仍然响应
          testMetric <- createMetric "final-test" "count"
          recordMetric testMetric 42.0
          
          finalValue <- metricValue testMetric
          
                    
          return (finalValue == 42.0)
    
    it "should clean up resources properly after shutdown" $ property $
    
          \(resourceCount :: Int) ->
    
            let actualCount = max 1 (abs resourceCount `mod` 50 + 1)
    
            in unsafePerformIO $ do
    
                        
    
              -- 创建大量资源
    
              metrics <- sequence $ replicate actualCount $ do
    
                createMetric "cleanup-test" "count"
    
              
    
              loggers <- sequence $ replicate actualCount $ do
    
                createLogger "cleanup-test-logger" Info
    
              
    
              spans <- sequence $ replicate actualCount $ do
    
                createSpan "cleanup-test-span"
    
              
    
              -- 使用资源
    
              sequence_ $ map (`recordMetric` 1.0) metrics
    
              sequence_ $ flip map loggers $ \logger -> do
    
                logMessage logger Info "cleanup test"
    
              sequence_ $ map finishSpan spans
    
              
    
              -- 关闭系统
    
              shutdownTelemetry
    
                        
    
              -- 重新初始化并验证系统正常工作
    
              initTelemetry defaultConfig
    
                        
    
              testMetric <- createMetric "post-cleanup-test" "count"
    
              recordMetric testMetric 100.0
    
              
    
              finalValue <- metricValue testMetric
    
                        
    
              return (finalValue == 100.0)

  -- 8. 测试系统健康检查
  describe "System Health Checks" $ do
    it "should maintain system health under stress" $ property $
      \(stressLevel :: Int) ->
        let operations = max 10 (abs stressLevel `mod` 1000 + 10)
        in unsafePerformIO $ do
                    
          -- 创建健康检查指标
          healthMetric <- createMetric "health-check" "ops"
          healthLogger <- createLogger "health-logger" Info
          
          -- 执行压力测试
          sequence_ $ replicate operations $ do
            recordMetric healthMetric 1.0
            logMessage healthLogger Info "health check message"
          
          -- 验证系统健康状态
          totalOps <- metricValue healthMetric
          
                    
          return (totalOps == fromIntegral operations)
    
    it "should handle mixed workload gracefully" $ do
        let operations = 50
            metricRatio = 0.4  -- 40% metric operations
            loggerRatio = 0.3  -- 30% logger operations
            spanRatio = 0.3    -- 30% span operations
            
            metricOps = max 1 $ round (fromIntegral operations * metricRatio)
            loggerOps = max 1 $ round (fromIntegral operations * loggerRatio)
            spanOps = max 1 $ operations - metricOps - loggerOps
        
                
        -- 执行混合工作负载
        metrics <- sequence $ zipWith (\index _ -> do
          createMetric (pack $ "mixed-workload-" ++ show index) "count") [1..] (replicate metricOps ())
        
        loggers <- sequence $ replicate loggerOps $ do
          createLogger "mixed-workload-logger" Info
        
        spans <- sequence $ replicate spanOps $ do
          createSpan "mixed-workload-span"
        
        -- 使用所有组件
        sequence_ $ zipWith (\metric index -> do
          recordMetric metric (fromIntegral index)) metrics [1..]
        
        sequence_ $ zipWith (\logger index -> do
          logMessage logger Info (pack $ "mixed workload message " ++ show index)) loggers [1..]
        
        sequence_ $ map finishSpan spans
        
        -- 验证系统状态
        totalMetricValue <- foldM (\acc metric -> do
          value <- metricValue metric
          return (acc + value)) 0.0 metrics
        
        let expectedMetricValue = fromIntegral $ sum [1..metricOps]
        
                
        totalMetricValue `shouldBe` expectedMetricValue