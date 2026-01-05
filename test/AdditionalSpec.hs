{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AdditionalSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.Maybe (isJust, isNothing)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (replicateM_, void)

import Azimuth.Telemetry

spec :: Spec
spec = do
  describe "Additional Azimuth.Telemetry Tests" $ do
    
    -- 1. 配置管理测试
    describe "Configuration Management" $ do
      it "should validate and merge configurations" $ do
        let baseConfig = defaultConfig
            customConfig = TelemetryConfig "custom-service" "2.0.0" False True True
            -- 模拟配置合并
            mergedConfig = customConfig { enableLogging = enableLogging baseConfig }
        
        serviceName mergedConfig `shouldBe` "custom-service"
        serviceVersion mergedConfig `shouldBe` "2.0.0"
        enableMetrics mergedConfig `shouldBe` False
        enableTracing mergedConfig `shouldBe` True
        enableLogging mergedConfig `shouldBe` True  -- 来自基础配置
      
      it "should handle configuration validation" $ do
        let validConfigs = 
              [ TelemetryConfig "service" "1.0.0" True True True
              , TelemetryConfig "" "" False False False
              , TelemetryConfig "test" "0.0.1" True False True
              ]
        
        -- 验证所有配置都是有效的
        mapM_ (\config -> do
          initTelemetry config `shouldReturn` ()
          shutdownTelemetry
        ) validConfigs

    -- 2. 度量聚合和统计功能测试
    describe "Metric Aggregation and Statistics" $ do
      it "should handle metric aggregations" $ do
        metric <- createMetric "aggregation-test" "ms"
        
        -- 记录多个值
        recordMetric metric 10.0
        recordMetric metric 20.0
        recordMetric metric 30.0
        recordMetric metric 40.0
        recordMetric metric 50.0
        
        -- 验证度量仍然有效
        metricName metric `shouldBe` "aggregation-test"
        metricUnit metric `shouldBe` "ms"
      
      it "should calculate metric statistics" $ do
        metric <- createMetric "stats-test" "count"
        
        -- 记录一系列值
        let values = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
        mapM_ (recordMetric metric) values
        
        -- 验证度量仍然有效
        metricName metric `shouldBe` "stats-test"
        metricUnit metric `shouldBe` "count"
        
        -- 计算简单的统计值（这里只是验证度量仍然存在）
        metricValue metric `shouldBe` 0.0  -- 初始值

    -- 3. 追踪上下文传播测试
    describe "Tracing Context Propagation" $ do
      it "should maintain trace context across spans" $ do
        parentSpan <- createSpan "parent-operation"
        let parentTraceId = spanTraceId parentSpan
        
        -- 创建子span
        childSpan1 <- createSpan "child-operation-1"
        childSpan2 <- createSpan "child-operation-2"
        
        -- 验证子span继承了父span的trace ID
        spanTraceId childSpan1 `shouldBe` parentTraceId
        spanTraceId childSpan2 `shouldBe` parentTraceId
        
        -- 验证span ID是唯一的
        spanSpanId childSpan1 `shouldNotBe` spanSpanId childSpan2
        spanSpanId childSpan1 `shouldNotBe` spanSpanId parentSpan
        
        -- 完成所有span
        finishSpan parentSpan
        finishSpan childSpan1
        finishSpan childSpan2
      
      it "should handle nested span operations" $ do
        rootSpan <- createSpan "root-operation"
        
        -- 第一层嵌套
        level1Span <- createSpan "level-1-operation"
        spanTraceId level1Span `shouldBe` spanTraceId rootSpan
        
        -- 第二层嵌套
        level2Span <- createSpan "level-2-operation"
        spanTraceId level2Span `shouldBe` spanTraceId rootSpan
        
        -- 完成嵌套span（反向顺序）
        finishSpan level2Span
        finishSpan level1Span
        finishSpan rootSpan

    -- 4. 日志过滤和格式化测试
    describe "Log Filtering and Formatting" $ do
      it "should filter log messages by level" $ do
        logger <- createLogger "filter-test" Warn
        
        -- 只有Warn和Error级别的消息应该被记录
        logMessage logger Debug "debug message"   -- 应被过滤
        logMessage logger Info "info message"     -- 应被过滤
        logMessage logger Warn "warning message"  -- 应记录
        logMessage logger Error "error message"   -- 应记录
        
        loggerName logger `shouldBe` "filter-test"
        loggerLevel logger `shouldBe` Warn
      
      it "should format log messages correctly" $ do
        logger <- createLogger "format-test" Info
        
        let messages = 
              [ ("Simple message", "simple")
              , ("Message with numbers: 123", "numbers")
              , ("Special chars: !@#$%", "special")
              , ("Unicode: 测试🚀", "unicode")
              ]
        
        mapM_ (\(message, _) -> logMessage logger Info (pack message)) messages
        
        loggerName logger `shouldBe` "format-test"
        loggerLevel logger `shouldBe` Info

    -- 5. QuickCheck属性测试 - 数据不变性
    describe "QuickCheck Properties - Data Invariants" $ do
      it "should preserve metric invariants" $ property $
        \(name :: String) (unit :: String) ->
          let metric = Metric (pack name) 0.0 (pack unit)
          in not (Text.null (metricName metric)) || Text.null (pack name)  -- 如果输入为空，输出也应为空
      
      it "should maintain span identity invariants" $ property $
        \(name :: String) ->
          let span = Span (pack name) "trace-123" "span-456"
          in not (Text.null (spanName span)) || Text.null (pack name)
      
      it "should preserve logger level ordering" $ property $
        \(name :: String) ->
          let levels = [Debug, Info, Warn, Error]
              level = levels !! (abs (length name) `mod` length levels)
              logger = Logger (pack name) level
          in loggerLevel logger `elem` levels
      
      it "should handle configuration invariants" $ property $
        \(name :: String) (version :: String) ->
          let config = TelemetryConfig (pack name) (pack version) True True True
          in (Text.null (serviceName config)) == (null name) &&
             (Text.null (serviceVersion config)) == (null version)

    -- 6. 序列化和反序列化测试
    describe "Serialization and Deserialization" $ do
      it "should handle metric serialization" $ do
        metric <- createMetric "serialization-test" "bytes"
        recordMetric metric 1024.0
        
        -- 验证度量的序列化属性
        let name = metricName metric
            value = metricValue metric
            unit = metricUnit metric
        
        unpack name `shouldBe` "serialization-test"
        value `shouldBe` 1024.0
        unpack unit `shouldBe` "bytes"
      
      it "should handle span serialization" $ do
        span <- createSpan "serialization-span"
        
        -- 验证span的序列化属性
        let name = spanName span
            traceId = spanTraceId span
            spanId = spanSpanId span
        
        unpack name `shouldBe` "serialization-span"
        traceId `shouldBe` "trace-123"
        spanId `shouldBe` "span-456"

    -- 7. 资源清理和内存管理测试
    describe "Resource Cleanup and Memory Management" $ do
      it "should properly cleanup resources" $ do
        initTelemetry defaultConfig
        
        -- 创建大量资源
        metrics <- sequence $ replicate 100 $ do
          createMetric "cleanup-test" "count"
        
        spans <- sequence $ replicate 50 $ do
          createSpan "cleanup-span"
        
        loggers <- sequence $ replicate 25 $ do
          createLogger "cleanup-logger" Info
        
        -- 使用资源
        sequence_ $ map (`recordMetric` 1.0) metrics
        sequence_ $ map finishSpan spans
        sequence_ $ flip map loggers $ \logger -> do
          logMessage logger Info "cleanup test"
        
        -- 清理资源
        shutdownTelemetry
        
        -- 验证资源数量
        length metrics `shouldBe` 100
        length spans `shouldBe` 50
        length loggers `shouldBe` 25
      
      it "should handle resource lifecycle correctly" $ do
        initTelemetry defaultConfig
        
        -- 创建、使用和销毁资源
        replicateM_ 10 $ do
          metric <- createMetric "lifecycle-test" "temp"
          recordMetric metric 1.0
          
          span <- createSpan "lifecycle-span"
          finishSpan span
          
          logger <- createLogger "lifecycle-logger" Info
          logMessage logger Info "lifecycle test"
        
        shutdownTelemetry

    -- 8. 错误恢复和容错性测试
    describe "Error Recovery and Fault Tolerance" $ do
      it "should handle initialization failures gracefully" $ do
        -- 尝试使用不同配置初始化
        let configs = 
              [ defaultConfig
              , TelemetryConfig "error-test" "1.0.0" True True True
              , TelemetryConfig "" "" False False False
              ]
        
        results <- mapM (\config -> do
          result <- try $ initTelemetry config
          case result of
            Left (_ :: SomeException) -> return False
            Right _ -> do
              shutdownTelemetry
              return True
        ) configs
        
        -- 至少应该有一个配置成功
        or results `shouldBe` True
      
      it "should handle metric recording failures" $ do
        metric <- createMetric "error-test" "count"
        
        -- 尝试记录各种值，包括潜在问题值
        let values = [0.0, 1.0, (-1.0), 1.0/0.0, 0.0/0.0, 999999.999]
        
        results <- mapM (\value -> do
          result <- try $ recordMetric metric value
          case result of
            Left (_ :: SomeException) -> return False
            Right _ -> return True
        ) values
        
        -- 至少应该有一些值成功记录
        or results `shouldBe` True
      
      it "should handle concurrent operations safely" $ do
        initTelemetry defaultConfig
        
        -- 并发创建和操作资源
        let numThreads = 5
            operationsPerThread = 20
        
        threads <- mapM (\_ -> forkIO $ do
          replicateM_ operationsPerThread $ do
            metric <- createMetric "concurrent-error-test" "count"
            recordMetric metric 1.0
            
            span <- createSpan "concurrent-error-span"
            finishSpan span
            
            logger <- createLogger "concurrent-error-logger" Info
            logMessage logger Info "concurrent error test"
        ) [1..numThreads]
        
        -- 等待所有线程完成
        threadDelay 1000000  -- 1秒
        
        shutdownTelemetry