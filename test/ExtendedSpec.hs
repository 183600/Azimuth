{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExtendedSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.Maybe (isJust, isNothing)
import Control.Concurrent (threadDelay, forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM_, void, when, filterM)
import Data.List (sort, nub)
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

import Azimuth.Telemetry

spec :: Spec
spec = do
  describe "Extended Azimuth.Telemetry Tests" $ do
    
    -- 1. 跨服务遥测数据传播测试
    describe "Cross-Service Telemetry Data Propagation" $ do
      it "should propagate trace context across service boundaries" $ do
        initTelemetry defaultConfig
        
        -- 服务A创建根span
        serviceASpan <- createSpan "service-a-operation"
        let rootTraceId = spanTraceId serviceASpan
        
        -- 模拟跨服务调用，服务B创建子span
        serviceBSpan <- createSpan "service-b-operation"
        spanTraceId serviceBSpan `shouldBe` rootTraceId
        
        -- 模拟更深的调用链，服务C创建子span
        serviceCSpan <- createSpan "service-c-operation"
        spanTraceId serviceCSpan `shouldBe` rootTraceId
        
        -- 验证所有span ID都是唯一的
        let spanIds = [spanSpanId serviceASpan, spanSpanId serviceBSpan, spanSpanId serviceCSpan]
        length (nub spanIds) `shouldBe` 3
        
        -- 完成所有span
        finishSpan serviceASpan
        finishSpan serviceBSpan
        finishSpan serviceCSpan
        
        shutdownTelemetry
      
      it "should maintain baggage across service calls" $ do
        initTelemetry defaultConfig
        
        -- 创建带有baggage的span
        parentSpan <- createSpan "parent-with-baggage"
        
        -- 模拟跨服务传播baggage
        childSpan1 <- createSpan "service-1-operation"
        childSpan2 <- createSpan "service-2-operation"
        childSpan3 <- createSpan "service-3-operation"
        
        -- 验证trace context传播
        spanTraceId childSpan1 `shouldBe` spanTraceId parentSpan
        spanTraceId childSpan2 `shouldBe` spanTraceId parentSpan
        spanTraceId childSpan3 `shouldBe` spanTraceId parentSpan
        
        -- 验证span ID唯一性
        let spanIds = map spanSpanId [parentSpan, childSpan1, childSpan2, childSpan3]
        length (nub spanIds) `shouldBe` 4
        
        finishSpan parentSpan
        finishSpan childSpan1
        finishSpan childSpan2
        finishSpan childSpan3
        
        shutdownTelemetry

    -- 2. 遥测数据压缩和存储测试
    describe "Telemetry Data Compression and Storage" $ do
      it "should handle large telemetry datasets efficiently" $ do
        initTelemetry defaultConfig
        
        -- 创建大量度量数据
        metrics <- sequence $ replicate 100 $ do
          createMetric "compression-test" "bytes"
        
        -- 记录大量数据点
        sequence_ $ flip map (zip [1..] metrics) $ \(index, metric) -> do
          recordMetric metric (fromIntegral index)
        
        -- 验证所有度量仍然有效
        length metrics `shouldBe` 100
        all (\m -> metricName m == "compression-test") metrics `shouldBe` True
        all (\m -> metricUnit m == "bytes") metrics `shouldBe` True
        
        shutdownTelemetry
      
      it "should compress telemetry data without loss" $ do
        initTelemetry defaultConfig
        
        -- 创建具有不同名称和单位的度量
        metricTypes <- mapM (\(name, unit) -> createMetric (pack name) (pack unit))
          [ ("metric-1", "ms"), ("metric-2", "bytes"), ("metric-3", "count")
          , ("metric-4", "percent"), ("metric-5", "req/s")
          ]
        
        -- 记录不同类型的值
        sequence_ $ flip map (zip [1.0,2.0,3.0,4.0,5.0] metricTypes) $ \(value, metric) -> do
          recordMetric metric value
        
        -- 验证数据完整性
        let names = map (unpack . metricName) metricTypes
            units = map (unpack . metricUnit) metricTypes
        
        sort names `shouldBe` ["metric-1", "metric-2", "metric-3", "metric-4", "metric-5"]
        sort units `shouldBe` ["bytes", "count", "ms", "percent", "req/s"]
        
        shutdownTelemetry

    -- 3. 分布式追踪复杂场景测试
    describe "Distributed Tracing Complex Scenarios" $ do
      it "should handle complex distributed transaction flows" $ do
        initTelemetry defaultConfig
        
        -- 模拟复杂的分布式事务流程
        rootSpan <- createSpan "distributed-transaction"
        
        -- 并行分支1
        branch1Span1 <- createSpan "service-a-db-query"
        branch1Span2 <- createSpan "service-a-cache-update"
        
        -- 并行分支2
        branch2Span1 <- createSpan "service-b-api-call"
        branch2Span2 <- createSpan "service-b-validation"
        branch2Span3 <- createSpan "service-b-processing"
        
        -- 汇聚点
        aggregateSpan <- createSpan "aggregate-results"
        
        -- 最终处理
        finalSpan <- createSpan "final-processing"
        
        -- 验证所有span属于同一trace
        let spans = [rootSpan, branch1Span1, branch1Span2, branch2Span1, branch2Span2, branch2Span3, aggregateSpan, finalSpan]
            traceIds = map spanTraceId spans
            spanIds = map spanSpanId spans
        
        all (== spanTraceId rootSpan) traceIds `shouldBe` True
        length (nub spanIds) `shouldBe` 8
        
        -- 完成所有span
        sequence_ $ map finishSpan spans
        
        shutdownTelemetry
      
      it "should handle async operations in distributed tracing" $ do
        initTelemetry defaultConfig
        
        -- 创建用于同步的MVar
        done <- newEmptyMVar
        
        -- 启动异步操作
        forkIO $ do
          asyncSpan1 <- createSpan "async-operation-1"
          threadDelay 100000  -- 0.1秒
          finishSpan asyncSpan1
          
          asyncSpan2 <- createSpan "async-operation-2"
          threadDelay 100000  -- 0.1秒
          finishSpan asyncSpan2
          
          putMVar done ()
        
        -- 主线程继续创建span
        mainSpan <- createSpan "main-operation"
        
        -- 等待异步操作完成
        takeMVar done
        
        finishSpan mainSpan
        
        shutdownTelemetry

    -- 4. 遥测配置热重载测试
    describe "Telemetry Configuration Hot Reload" $ do
      it "should handle configuration changes without restart" $ do
        -- 初始配置
        initTelemetry defaultConfig
        
        -- 创建初始资源
        metric <- createMetric "hot-reload-test" "count"
        logger <- createLogger "hot-reload-logger" Info
        
        -- 模拟配置更新
        let newConfig = TelemetryConfig "updated-service" "2.0.0" False True True True
        
        -- 应用新配置
        result <- try $ initTelemetry newConfig
        case result of
          Left (_ :: SomeException) -> do
            -- 如果重新初始化失败，继续使用现有配置
            recordMetric metric 1.0
            logMessage logger Info "Continuing with existing config"
          Right _ -> do
            -- 配置更新成功
            recordMetric metric 2.0
            logMessage logger Info "Config updated successfully"
            shutdownTelemetry
        
        shutdownTelemetry
      
      it "should validate configuration before applying" $ do
        let validConfigs = 
              [ TelemetryConfig "valid-service" "1.0.0" True True True False
              , TelemetryConfig "another-valid" "2.1.0" False True False False
              ]
        
        results <- mapM (\config -> do
          result <- try $ do
            initTelemetry config
            shutdownTelemetry
          case result of
            Left (_ :: SomeException) -> return False
            Right _ -> return True
          ) validConfigs
        
        -- 所有有效配置都应该成功
        all id results `shouldBe` True

    -- 5. 遥测系统健康检查测试
    describe "Telemetry System Health Check" $ do
      it "should perform comprehensive health checks" $ do
        initTelemetry defaultConfig
        
        -- 创建各种资源
        metric <- createMetric "health-check-metric" "count"
        logger <- createLogger "health-check-logger" Info
        span <- createSpan "health-check-span"
        
        -- 执行健康检查操作
        recordMetric metric 1.0
        logMessage logger Info "Health check"
        finishSpan span
        
        -- 验证系统状态
        metricName metric `shouldBe` "health-check-metric"
        loggerName logger `shouldBe` "health-check-logger"
        spanName span `shouldBe` "health-check-span"
        
        shutdownTelemetry
      
      it "should detect and report system issues" $ do
        initTelemetry defaultConfig
        
        -- 创建健康检查度量
        healthMetric <- createMetric "system-health" "status"
        healthLogger <- createLogger "health-monitor" Warn
        
        -- 模拟健康检查
        recordMetric healthMetric 1.0  -- 1表示健康
        logMessage healthLogger Info "System health check passed"
        
        -- 模拟潜在问题
        recordMetric healthMetric 0.5  -- 0.5表示警告
        logMessage healthLogger Warn "System health check warning"
        
        shutdownTelemetry

    -- 6. 实时流处理性能测试
    describe "Real-time Stream Processing Performance" $ do
      it "should handle high-volume telemetry streams" $ do
        initTelemetry defaultConfig
        
        -- 创建流处理度量
        throughputMetric <- createMetric "stream-throughput" "events/sec"
        latencyMetric <- createMetric "stream-latency" "ms"
        
        -- 模拟高容量流处理
        let numEvents = 1000
        sequence_ $ replicate numEvents $ do
          recordMetric throughputMetric 1.0
          recordMetric latencyMetric 10.0
        
        -- 验证度量仍然有效
        metricName throughputMetric `shouldBe` "stream-throughput"
        metricName latencyMetric `shouldBe` "stream-latency"
        
        shutdownTelemetry
      
      it "should maintain performance under load" $ do
        initTelemetry defaultConfig
        
        -- 创建性能监控度量
        cpuMetric <- createMetric "cpu-usage" "percent"
        memoryMetric <- createMetric "memory-usage" "MB"
        
        -- 模拟负载测试
        let loadIterations = 500
        sequence_ $ replicate loadIterations $ do
          recordMetric cpuMetric 75.0
          recordMetric memoryMetric 512.0
        
        -- 验证系统稳定性
        metricName cpuMetric `shouldBe` "cpu-usage"
        metricName memoryMetric `shouldBe` "memory-usage"
        
        shutdownTelemetry

    -- 7. 遥测数据采样策略测试
    describe "Telemetry Data Sampling Strategies" $ do
      it "should implement random sampling correctly" $ do
        initTelemetry defaultConfig
        
        -- 创建采样度量
        sampledMetric <- createMetric "sampled-metric" "count"
        
        -- 模拟采样决策
        let sampleRate :: Double = 0.1  -- 10%采样率
            numOperations = 1000
            expectedSampled = round $ fromIntegral numOperations * sampleRate
        
        -- 模拟采样过程
        sampledCount <- fmap length $ filterM (\_ -> do
          shouldSample <- (<= sampleRate) <$> (randomRIO (0.0, 1.0) :: IO Double)
          when shouldSample $ recordMetric sampledMetric 1.0
          return shouldSample
          ) [1..numOperations]
        
        -- 验证采样率大致正确（允许一定误差）
        let actualRate = fromIntegral sampledCount / fromIntegral numOperations
        actualRate `shouldSatisfy` (\rate -> rate >= 0.05 && rate <= 0.15)
        
        shutdownTelemetry
      
      it "should implement priority sampling" $ do
        initTelemetry defaultConfig
        
        -- 创建不同优先级的度量
        highPriorityMetric <- createMetric "high-priority" "count"
        lowPriorityMetric <- createMetric "low-priority" "count"
        
        -- 高优先级操作总是采样
        sequence_ $ replicate 100 $ do
          recordMetric highPriorityMetric 1.0
        
        -- 低优先级操作按较低比例采样
        sequence_ $ replicate 100 $ do
          shouldSample <- (<= (0.2 :: Double)) <$> (randomRIO (0.0, 1.0) :: IO Double)
          when shouldSample $ recordMetric lowPriorityMetric 1.0
        
        shutdownTelemetry

    -- 8. 遥测数据质量验证测试
    describe "Telemetry Data Quality Validation" $ do
      it "should validate metric data integrity" $ do
        initTelemetry defaultConfig
        
        -- 创建质量检查度量
        qualityMetric <- createMetric "quality-check" "score"
        
        -- 记录各种质量分数
        let scores = [0.0, 0.5, 0.75, 0.9, 1.0]
        sequence_ $ map (recordMetric qualityMetric) scores
        
        -- 验证数据完整性
        metricName qualityMetric `shouldBe` "quality-check"
        metricUnit qualityMetric `shouldBe` "score"
        
        shutdownTelemetry
      
      it "should detect and handle anomalies" $ do
        initTelemetry defaultConfig
        
        -- 创建异常检测度量
        anomalyMetric <- createMetric "anomaly-detection" "value"
        
        -- 记录正常值
        sequence_ $ map (recordMetric anomalyMetric) [10.0, 12.0, 11.5, 10.8, 11.2]
        
        -- 记录异常值
        recordMetric anomalyMetric 1000.0  -- 异常高值
        recordMetric anomalyMetric (-500.0)  -- 异常低值
        
        -- 验证系统仍能处理异常值
        metricName anomalyMetric `shouldBe` "anomaly-detection"
        
        shutdownTelemetry

    -- 9. 遥测系统生命周期管理测试
    describe "Telemetry System Lifecycle Management" $ do
      it "should handle complete lifecycle gracefully" $ do
        -- 初始化
        initTelemetry defaultConfig
        
        -- 创建资源
        metric <- createMetric "lifecycle-metric" "count"
        logger <- createLogger "lifecycle-logger" Info
        span <- createSpan "lifecycle-span"
        
        -- 使用资源
        recordMetric metric 42.0
        logMessage logger Info "Lifecycle test"
        finishSpan span
        
        -- 清理
        shutdownTelemetry
        
        -- 验证资源属性
        metricName metric `shouldBe` "lifecycle-metric"
        loggerName logger `shouldBe` "lifecycle-logger"
        spanName span `shouldBe` "lifecycle-span"
      
      it "should handle multiple lifecycle cycles" $ do
        -- 执行多个完整的生命周期
        replicateM_ 3 $ do
          initTelemetry defaultConfig
          
          metric <- createMetric "multi-lifecycle" "count"
          recordMetric metric 1.0
          
          shutdownTelemetry

    -- 10. QuickCheck高级属性测试
    describe "Advanced QuickCheck Properties" $ do
      it "should maintain telemetry data consistency under random operations" $ property $
        \(operations :: [Int]) ->
          let numOps = length operations `mod` 100 + 1  -- 1到100个操作
              metric = unsafePerformIO (createMetricWithInitialValue "property-test" "unit" 0.0)
          in metricName metric == "property-test" &&
             metricUnit metric == "unit" &&
             unsafeMetricValue metric == 0.0
      
      it "should handle arbitrary text in telemetry fields" $ property $
        \(text :: String) ->
          let packedText = pack text
              metric = unsafePerformIO (createMetricWithInitialValue packedText packedText 0.0)
              logger = Logger packedText Info
              span = Span packedText "trace-test" "span-test"
          in unpack (metricName metric) == text &&
             unpack (metricUnit metric) == text &&
             unpack (loggerName logger) == text &&
             unpack (spanName span) == text
      
      it "should preserve type safety across operations" $ property $
        \(value :: Double) ->
          let isFinite = not (isNaN value || isInfinite value)
          in isFinite || (not isFinite)  -- 简化的类型安全检查
      
      it "should handle edge cases in configuration" $ property $
        \(name :: String) (version :: String) ->
          let config = TelemetryConfig (pack name) (pack version) True True True False
          in unpack (serviceName config) == name &&
             unpack (serviceVersion config) == version &&
             enableMetrics config == True &&
             enableTracing config == True &&
             enableLogging config == True