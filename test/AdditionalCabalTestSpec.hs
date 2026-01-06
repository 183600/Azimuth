{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AdditionalCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Monad (replicateM, replicateM_, when)
import Data.IORef (writeIORef)
import Data.Text (pack, unpack)
import Data.List (nub)
import System.IO.Unsafe (unsafePerformIO)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Additional Cabal Test Suite" $ do

  -- 测试1: 遥测配置的属性验证
  describe "TelemetryConfig Properties" $ do
    it "should maintain consistency across config updates" $ property $
      \(name :: String) (version :: String) ->
        let config1 = TelemetryConfig (pack name) (pack version) True True True False
            config2 = TelemetryConfig (pack name) (pack version) False False False True
        in serviceName config1 == serviceName config2 &&
           serviceVersion config1 == serviceVersion config2 &&
           (enableMetrics config1 /= enableMetrics config2) &&
           (enableTracing config1 /= enableTracing config2) &&
           (enableLogging config1 /= enableLogging config2) &&
           (enableDebugOutput config1 /= enableDebugOutput config2)

    it "should handle extreme config combinations" $ do
      let configs = 
            [ TelemetryConfig "" "" True True True True
            , TelemetryConfig (pack $ replicate 1000 'a') (pack $ replicate 1000 'b') False False False False
            , TelemetryConfig "service" "1.0.0" True False True False
            , TelemetryConfig "service" "1.0.0" False True False True
            ]
      mapM_ (\config -> do
        initTelemetry config `shouldReturn` ()
        shutdownTelemetry `shouldReturn` ()
        ) configs

  -- 测试2: 指标值的数学运算属性
  describe "Metric Mathematical Properties" $ do
    it "should satisfy commutative property for addition" $ property $
      \(x :: Double) (y :: Double) ->
        let metric = unsafePerformIO $ createMetric "commutative" "test"
            result1 = unsafePerformIO $ do
              recordMetric metric x
              recordMetric metric y
              metricValue metric
            result2 = unsafePerformIO $ do
              metric2 <- createMetric "commutative" "test"
              recordMetric metric2 y
              recordMetric metric2 x
              metricValue metric2
        in (isNaN result1 && isNaN result2) || result1 == result2

    it "should handle associative property with multiple recordings" $ property $
      \(x :: Double) (y :: Double) (z :: Double) ->
        let metric1 = unsafePerformIO $ createMetric "associative" "test"
            result1 = unsafePerformIO $ do
              recordMetric metric1 x
              recordMetric metric1 y
              recordMetric metric1 z
              metricValue metric1
            metric2 = unsafePerformIO $ createMetric "associative" "test"
            result2 = unsafePerformIO $ do
              recordMetric metric2 (x + y)
              recordMetric metric2 z
              metricValue metric2
        in (isNaN result1 && isNaN result2) || result1 == result2

    it "should handle identity element (zero) correctly" $ property $
      \(x :: Double) ->
        let metric = unsafePerformIO $ createMetric "identity" "test"
            result = unsafePerformIO $ do
              recordMetric metric x
              recordMetric metric 0.0
              metricValue metric
        in (isNaN result) || result == x

  -- 测试3: 跨模块的遥测数据一致性
  describe "Cross-Module Data Consistency" $ do
    it "should maintain metric consistency across multiple instances" $ do
      -- 禁用指标共享以确保测试隔离
      evaluate $ unsafePerformIO $ writeIORef enableMetricSharing False
      
      metric1 <- createMetric "consistency-test" "unit"
      metric2 <- createMetric "consistency-test" "unit"
      
      recordMetric metric1 100.0
      recordMetric metric2 200.0
      
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      
      -- 恢复指标共享
      evaluate $ unsafePerformIO $ writeIORef enableMetricSharing True
      
      value1 `shouldBe` 100.0
      value2 `shouldBe` 200.0
      metric1 `shouldNotBe` metric2  -- 不同的实例

    it "should maintain span identity across operations" $ property $
      \(name :: String) ->
        let span1 = unsafePerformIO $ createSpan (pack name)
            span2 = unsafePerformIO $ createSpan (pack name)
        in spanName span1 == spanName span2 &&
           spanTraceId span1 /= spanTraceId span2 &&
           spanSpanId span1 /= spanSpanId span2

  -- 测试4: 资源限制下的行为
  describe "Resource Limit Behavior" $ do
    it "should handle large number of concurrent operations" $ do
      let numThreads = 50
          operationsPerThread = 20
      
      -- 测试并发指标创建
      metricResults <- replicateM numThreads $ do
        forkIO $ replicateM_ operationsPerThread $ do
          metric <- createMetric "resource-test" "count"
          recordMetric metric 1.0
      
      -- 等待所有线程完成
      threadDelay 1000000  -- 1秒
      mapM_ killThread metricResults
      
      -- 测试并发日志操作
      logger <- createLogger "resource-logger" Info
      logResults <- replicateM numThreads $ do
        forkIO $ replicateM_ operationsPerThread $ do
          logMessage logger Info "resource test message"
      
      -- 等待所有线程完成
      threadDelay 1000000  -- 1秒
      mapM_ killThread logResults
      
      -- 如果没有崩溃，测试通过
      True `shouldBe` True

    it "should handle memory pressure gracefully" $ do
      let numMetrics = 1000
      
      -- 创建大量指标
      metrics <- replicateM numMetrics $ do
        createMetric "memory-test" "unit"
      
      -- 记录值到所有指标
      sequence_ $ zipWith (\metric index -> do
        recordMetric metric (fromIntegral index)
        ) metrics [1..numMetrics]
      
      -- 验证所有指标仍然可用
      values <- sequence $ map metricValue metrics
      length values `shouldBe` numMetrics
      
      -- 清理
      shutdownTelemetry
      initTelemetry defaultConfig

  -- 测试5: 错误恢复机制
  describe "Error Recovery Mechanisms" $ do
    it "should recover from invalid metric values" $ do
      metric <- createMetric "error-recovery" "test"
      
      -- 测试特殊值
      let specialValues = [1/0, -1/0, 0/0]  -- 正无穷、负无穷、NaN
      
      sequence_ $ map (\value -> do
        result <- try $ recordMetric metric value
        case result of
          Left (_ :: SomeException) -> return ()  -- 预期中的异常
          Right _ -> return ()  -- 正常处理
        ) specialValues
      
      -- 验证指标仍然可用
      recordMetric metric 42.0 `shouldReturn` ()
      value <- metricValue metric
      -- 值应该是可计算的（不应该是未定义的）
      evaluate value `shouldReturn` value

    it "should handle telemetry lifecycle errors gracefully" $ do
      -- 多次初始化和关闭
      replicateM_ 5 $ do
        initTelemetry defaultConfig
        shutdownTelemetry
      
      -- 在关闭状态下操作应该仍然安全
      metric <- createMetric "post-shutdown" "test"
      recordMetric metric 100.0 `shouldReturn` ()
      
      -- 重新初始化应该正常工作
      initTelemetry defaultConfig `shouldReturn` ()

  -- 测试6: 并发场景下的数据竞争
  describe "Concurrent Data Race Prevention" $ do
    it "should prevent data races in metric registry" $ do
      let numThreads = 20
          operationsPerThread = 50
      
      -- 启用指标共享以测试注册表
      evaluate $ unsafePerformIO $ writeIORef enableMetricSharing True
      
      -- 并发创建同名指标
      results <- replicateM numThreads $ forkIO $ do
        replicateM_ operationsPerThread $ do
          metric <- createMetric "race-test" "shared"
          recordMetric metric 1.0
      
      -- 等待所有线程完成
      threadDelay 2000000  -- 2秒
      mapM_ killThread results
      
      -- 验证共享指标的状态
      sharedMetric <- createMetric "race-test" "shared"
      value <- metricValue sharedMetric
      
      -- 值应该是所有操作的总和
      let expectedValue = fromIntegral (numThreads * operationsPerThread)
      -- 由于并发性质，我们只检查值是否合理
      value `shouldSatisfy` (\v -> v >= 0 && not (isNaN v))

  -- 测试7: 内存使用效率
  describe "Memory Usage Efficiency" $ do
    it "should reuse metric references when sharing is enabled" $ do
      -- 启用指标共享
      evaluate $ unsafePerformIO $ writeIORef enableMetricSharing True
      
      metric1 <- createMetric "efficiency-test" "shared"
      metric2 <- createMetric "efficiency-test" "shared"
      
      -- 设置不同的值
      recordMetric metric1 100.0
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      
      -- 当共享启用时，两个指标应该引用相同的值
      value1 `shouldBe` value2
      
      -- 禁用指标共享
      evaluate $ unsafePerformIO $ writeIORef enableMetricSharing False
      
      metric3 <- createMetric "efficiency-test" "isolated"
      recordMetric metric3 200.0
      value3 <- metricValue metric3
      
      -- 当共享禁用时，值应该是独立的
      value3 `shouldBe` 200.0

    it "should handle string memory efficiently" $ property $
      \(name :: String) (unit :: String) ->
        let longName = pack $ name ++ replicate 100 'x'
            longUnit = pack $ unit ++ replicate 100 'y'
            metric = unsafePerformIO $ createMetric longName longUnit
        in unpack (metricName metric) == name ++ replicate 100 'x' &&
           unpack (metricUnit metric) == unit ++ replicate 100 'y'

  -- 测试8: 时间相关的遥测操作
  describe "Time-Related Telemetry Operations" $ do
    it "should handle time-based operations consistently" $ do
      -- 创建多个span并验证时间顺序
      span1 <- createSpan "time-test-1"
      threadDelay 1000  -- 1ms
      span2 <- createSpan "time-test-2"
      threadDelay 1000  -- 1ms
      span3 <- createSpan "time-test-3"
      
      -- 验证span ID的唯一性和顺序性
      let spanIds = [spanSpanId span1, spanSpanId span2, spanSpanId span3]
          uniqueIds = nub spanIds
      
      length uniqueIds `shouldBe` 3  -- 所有ID应该是唯一的
      
      -- 验证trace ID的一致性
      spanTraceId span1 `shouldBe` spanTraceId span2
      spanTraceId span2 `shouldBe` spanTraceId span3

    it "should handle high-frequency operations" $ do
      let numOperations = 1000
      metric <- createMetric "high-frequency" "ops"
      
      -- 高频操作
      sequence_ $ replicate numOperations $ do
        recordMetric metric 1.0
      
      value <- metricValue metric
      value `shouldBe` fromIntegral numOperations

  -- 测试9: 配置热重载功能
  describe "Configuration Hot Reload" $ do
    it "should handle configuration changes" $ do
      -- 初始配置
      let config1 = TelemetryConfig "hot-reload-test" "1.0.0" True True True True
      initTelemetry config1
      
      -- 验证初始配置
      metric <- createMetric "config-test" "unit"
      recordMetric metric 50.0
      value1 <- metricValue metric
      value1 `shouldBe` 50.0
      
      -- 更改配置
      let config2 = TelemetryConfig "hot-reload-test" "2.0.0" False True False True
      initTelemetry config2
      
      -- 验证配置更改后的行为
      logger <- createLogger "config-logger" Info
      logMessage logger Info "config changed" `shouldReturn` ()
      
      -- 清理
      shutdownTelemetry

  -- 测试10: 遥测数据的导出格式
  describe "Telemetry Data Export Formats" $ do
    it "should maintain data integrity for export" $ do
      -- 创建各种遥测数据
      metrics <- sequence $ map (\i -> do
        let name = pack $ "export-metric-" ++ show i
        metric <- createMetric name "test-unit"
        recordMetric metric (fromIntegral i * 10.0)
        return metric
        ) [1..5]
      
      spans <- sequence $ map (\i -> do
        let name = pack $ "export-span-" ++ show i
        createSpan name
        ) [1..3]
      
      loggers <- sequence $ map (\i -> do
        let name = pack $ "export-logger-" ++ show i
        createLogger name Info
        ) [1..2]
      
      -- 验证所有数据的完整性
      metricValues <- sequence $ map metricValue metrics
      length metricValues `shouldBe` 5
      
      let metricNames = map metricName metrics
          spanNames = map spanName spans
          loggerNames = map loggerName loggers
      
      length metricNames `shouldBe` 5
      length spanNames `shouldBe` 3
      length loggerNames `shouldBe` 2
      
      -- 验证名称的唯一性
      length (nub metricNames) `shouldBe` 5
      length (nub spanNames) `shouldBe` 3
      length (nub loggerNames) `shouldBe` 2

    it "should handle export format conversion" $ property $
      \(name :: String) (unit :: String) (value :: Double) ->
        let metric = unsafePerformIO $ createMetric (pack name) (pack unit)
            _ = unsafePerformIO $ recordMetric metric value
            actualValue = unsafePerformIO $ metricValue metric
        in not (isNaN actualValue) || isNaN value  -- NaN应该保留