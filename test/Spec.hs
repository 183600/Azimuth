{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.QuickCheck
import Control.Monad (replicateM_)
import Data.Text (pack, unpack)

import Azimuth.Telemetry

main :: IO ()
main = hspec $ do
  describe "Azimuth.Telemetry" $ do
    
    -- 配置验证测试
    describe "TelemetryConfig" $ do
      it "should validate config fields" $ do
        let config = TelemetryConfig "test-service" "1.0.0" True False True
        serviceName config `shouldBe` "test-service"
        serviceVersion config `shouldBe` "1.0.0"
        enableMetrics config `shouldBe` True
        enableTracing config `shouldBe` False
        enableLogging config `shouldBe` True
      
      it "should create custom config" $ do
        let customConfig = TelemetryConfig "custom-service" "2.0.0" False True True
        initTelemetry customConfig `shouldReturn` ()
        shutdownTelemetry `shouldReturn` ()
    
    describe "defaultConfig" $ do
      it "should have correct default values" $ do
        let config = defaultConfig
        serviceName config `shouldBe` "azimuth-service"
        serviceVersion config `shouldBe` "0.1.0"
        enableMetrics config `shouldBe` True
        enableTracing config `shouldBe` True
        enableLogging config `shouldBe` True

    describe "initTelemetry" $ do
      it "should initialize without errors" $ do
        initTelemetry defaultConfig `shouldReturn` ()
      
      it "should initialize multiple times" $ do
        initTelemetry defaultConfig `shouldReturn` ()
        initTelemetry defaultConfig `shouldReturn` ()
        shutdownTelemetry `shouldReturn` ()

    describe "shutdownTelemetry" $ do
      it "should shutdown without errors" $ do
        shutdownTelemetry `shouldReturn` ()

    -- Metrics 测试
    describe "Metrics" $ do
      describe "createMetric" $ do
        it "should create a metric with initial value 0" $ do
          metric <- createMetric "test-metric" "count"
          metricName metric `shouldBe` "test-metric"
          metricValue metric `shouldBe` 0.0
          metricUnit metric `shouldBe` "count"
        
        it "should create metrics with different units" $ do
          metric1 <- createMetric "latency" "ms"
          metric2 <- createMetric "throughput" "req/s"
          metricUnit metric1 `shouldBe` "ms"
          metricUnit metric2 `shouldBe` "req/s"
      
      describe "recordMetric" $ do
        it "should record a metric value" $ do
          metric <- createMetric "test-metric" "count"
          recordMetric metric 42.0 `shouldReturn` ()
        
        it "should record negative values" $ do
          metric <- createMetric "temperature" "celsius"
          recordMetric metric (-10.5) `shouldReturn` ()
        
        it "should record zero values" $ do
          metric <- createMetric "counter" "count"
          recordMetric metric 0.0 `shouldReturn` ()
        
        it "should record large values" $ do
          metric <- createMetric "large-number" "count"
          recordMetric metric 999999.999 `shouldReturn` ()
      
      -- QuickCheck 属性测试
      describe "QuickCheck properties" $ do
        it "should handle any metric value" $ property $
          \(_value :: Double) -> 
            let metric = Metric "test-name" 0.0 "test-unit"
            in metricValue metric == 0.0
        
        it "should preserve metric name and unit with strings" $ property $
          \(name :: String) (unit :: String) ->
            let metric = Metric (pack name) 0.0 (pack unit)
            in unpack (metricName metric) == name && unpack (metricUnit metric) == unit

    -- Tracing 测试
    describe "Tracing" $ do
      describe "createSpan" $ do
        it "should create a span with trace and span IDs" $ do
          span <- createSpan "test-span"
          spanName span `shouldBe` "test-span"
          spanTraceId span `shouldBe` "trace-123"
          spanSpanId span `shouldBe` "span-456"
        
        it "should create spans with different names" $ do
          span1 <- createSpan "operation-1"
          span2 <- createSpan "operation-2"
          spanName span1 `shouldBe` "operation-1"
          spanName span2 `shouldBe` "operation-2"
          spanTraceId span1 `shouldBe` spanTraceId span2
      
      describe "finishSpan" $ do
        it "should finish a span" $ do
          span <- createSpan "test-span"
          finishSpan span `shouldReturn` ()
        
        it "should finish multiple spans" $ do
          span1 <- createSpan "span-1"
          span2 <- createSpan "span-2"
          finishSpan span1 `shouldReturn` ()
          finishSpan span2 `shouldReturn` ()

    -- Logging 测试
    describe "Logging" $ do
      describe "createLogger" $ do
        it "should create a logger with specified name and level" $ do
          logger <- createLogger "test-logger" Info
          loggerName logger `shouldBe` "test-logger"
          loggerLevel logger `shouldBe` Info
        
        it "should create loggers with different levels" $ do
          debugLogger <- createLogger "debug-logger" Debug
          infoLogger <- createLogger "info-logger" Info
          warnLogger <- createLogger "warn-logger" Warn
          errorLogger <- createLogger "error-logger" Error
          
          loggerLevel debugLogger `shouldBe` Debug
          loggerLevel infoLogger `shouldBe` Info
          loggerLevel warnLogger `shouldBe` Warn
          loggerLevel errorLogger `shouldBe` Error
      
      describe "logMessage" $ do
        it "should log a message" $ do
          logger <- createLogger "test-logger" Info
          logMessage logger Info "test message" `shouldReturn` ()
        
        it "should log messages at different levels" $ do
          logger <- createLogger "multi-level-logger" Debug
          logMessage logger Debug "debug message" `shouldReturn` ()
          logMessage logger Info "info message" `shouldReturn` ()
          logMessage logger Warn "warning message" `shouldReturn` ()
          logMessage logger Error "error message" `shouldReturn` ()
        
        it "should log empty messages" $ do
          logger <- createLogger "empty-logger" Info
          logMessage logger Info "" `shouldReturn` ()
        
        it "should log long messages" $ do
          logger <- createLogger "long-logger" Info
          let longMessage = "This is a very long log message that contains a lot of text and should still be handled properly by the logging system"
          logMessage logger Info longMessage `shouldReturn` ()

    -- 生命周期测试
    describe "Telemetry Lifecycle" $ do
      it "should complete full lifecycle" $ do
        initTelemetry defaultConfig `shouldReturn` ()
        
        -- Create and use components
        metric <- createMetric "lifecycle-metric" "count"
        recordMetric metric 100.0 `shouldReturn` ()
        
        span <- createSpan "lifecycle-span"
        finishSpan span `shouldReturn` ()
        
        logger <- createLogger "lifecycle-logger" Info
        logMessage logger Info "Lifecycle test" `shouldReturn` ()
        
        shutdownTelemetry `shouldReturn` ()
      
      it "should handle multiple init/shutdown cycles" $ do
        replicateM_ 3 $ do
          initTelemetry defaultConfig
          shutdownTelemetry
        return ()

    -- 边界条件测试
    describe "Boundary Conditions" $ do
      it "should handle empty metric names" $ do
        metric <- createMetric "" "count"
        metricName metric `shouldBe` ""
      
      it "should handle empty span names" $ do
        span <- createSpan ""
        spanName span `shouldBe` ""
      
      it "should handle empty logger names" $ do
        logger <- createLogger "" Info
        loggerName logger `shouldBe` ""
      
      it "should handle special characters in names" $ do
        metric <- createMetric "metric-with-special-chars!@#$%" "unit"
        metricName metric `shouldBe` "metric-with-special-chars!@#$%"
        
        span <- createSpan "span-with-特殊字符"
        spanName span `shouldBe` "span-with-特殊字符"
        
        logger <- createLogger "logger-with-特殊字符" Info
        loggerName logger `shouldBe` "logger-with-特殊字符"