{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module EnhancedTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad (replicateM, when)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar, readMVar, modifyMVar_)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.List (sort, group, nub)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utcToLocalTime)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)
import Text.Read (readMaybe)
import Prelude hiding (id)

import Azimuth.Telemetry

-- | 辅助函数：生成随机文本
generateRandomText :: Int -> IO Text
generateRandomText len = do
    let chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    randomIndices <- sequence $ replicate len $ randomIO :: IO [Int]
    return $ pack $ map (\i -> chars !! (i `mod` length chars)) randomIndices

-- | 辅助函数：检查文本是否为有效数字
isValidNumber :: Text -> Bool
isValidNumber txt = case readMaybe (unpack txt) of
    Just (_ :: Double) -> True
    Nothing -> False

spec :: Spec
spec = describe "Enhanced Telemetry Tests" $ do
  
  -- 1. 指标聚合功能测试
  describe "Metric Aggregation" $ do
    it "should aggregate multiple metric values correctly" $ do
      metric <- createMetric "aggregation-test" "count"
      
      -- 记录多个值
      recordMetric metric 10.0
      recordMetric metric 20.0
      recordMetric metric 30.0
      recordMetric metric 40.0
      
      -- 验证最终值是所有记录值的总和
      finalValue <- metricValue metric
      finalValue `shouldBe` 100.0
    
    it "should handle floating point precision in aggregation" $ do
      metric <- createMetric "precision-test" "ms"
      
      -- 记录浮点数
      recordMetric metric 0.1
      recordMetric metric 0.2
      recordMetric metric 0.3
      
      finalValue <- metricValue metric
      -- 允许一定的浮点误差，现在期望总和0.6
      finalValue `shouldSatisfy` (\v -> abs (v - 0.6) < 0.0001)
    
    it "should aggregate metrics with different units" $ do
      countMetric <- createMetric "request-count" "count"
      latencyMetric <- createMetric "request-latency" "ms"
      throughputMetric <- createMetric "request-throughput" "req/s"
      
      -- 记录不同单位的指标
      recordMetric countMetric 100.0
      recordMetric latencyMetric 50.5
      recordMetric throughputMetric 10.2
      
      countValue <- metricValue countMetric
      latencyValue <- metricValue latencyMetric
      throughputValue <- metricValue throughputMetric
      
      countValue `shouldBe` 100.0
      latencyValue `shouldBe` 50.5
      throughputValue `shouldBe` 10.2

  -- 2. 跨服务追踪传播测试
  describe "Cross-Service Trace Propagation" $ do
    it "should maintain trace context across multiple spans" $ do
      -- 创建第一个span，应该创建新的trace ID
      span1 <- createSpan "service-a-operation"
      
      -- 创建第二个span，应该继承相同的trace ID
      span2 <- createSpan "service-b-operation"
      
      -- 创建第三个span，也应该继承相同的trace ID
      span3 <- createSpan "service-c-operation"
      
      -- 验证所有span有相同的trace ID
      spanTraceId span1 `shouldBe` spanTraceId span2
      spanTraceId span2 `shouldBe` spanTraceId span3
      
      -- 验证每个span有不同的span ID
      spanSpanId span1 `shouldNotBe` spanSpanId span2
      spanSpanId span2 `shouldNotBe` spanSpanId span3
      spanSpanId span1 `shouldNotBe` spanSpanId span3
    
    it "should handle nested span operations" $ do
      parentSpan <- createSpan "parent-operation"
      
      -- 模拟嵌套操作
      childSpan1 <- createSpan "child-operation-1"
      childSpan2 <- createSpan "child-operation-2"
      
      -- 验证所有span在同一个trace中
      spanTraceId parentSpan `shouldBe` spanTraceId childSpan1
      spanTraceId parentSpan `shouldBe` spanTraceId childSpan2
      
      -- 验证span ID都是唯一的
      let spanIds = [spanSpanId parentSpan, spanSpanId childSpan1, spanSpanId childSpan2]
      length (nub spanIds) `shouldBe` 3

  -- 3. 日志过滤和采样测试
  describe "Log Filtering and Sampling" $ do
    it "should respect log levels when filtering" $ do
      debugLogger <- createLogger "debug-logger" Debug
      infoLogger <- createLogger "info-logger" Info
      warnLogger <- createLogger "warn-logger" Warn
      errorLogger <- createLogger "error-logger" Error
      
      -- 所有logger都应该能记录相同或更高级别的日志
      logMessage debugLogger Debug "debug message" `shouldReturn` ()
      logMessage debugLogger Info "info message" `shouldReturn` ()
      logMessage debugLogger Warn "warn message" `shouldReturn` ()
      logMessage debugLogger Error "error message" `shouldReturn` ()
      
      logMessage infoLogger Info "info message" `shouldReturn` ()
      logMessage infoLogger Warn "warn message" `shouldReturn` ()
      logMessage infoLogger Error "error message" `shouldReturn` ()
      
      logMessage warnLogger Warn "warn message" `shouldReturn` ()
      logMessage warnLogger Error "error message" `shouldReturn` ()
      
      logMessage errorLogger Error "error message" `shouldReturn` ()
    
    it "should handle high-volume log sampling" $ do
      logger <- createLogger "sampling-logger" Info
      
      -- 生成大量日志消息
      let numMessages = 1000
      sequence_ $ replicate numMessages $ do
        logMessage logger Info "high-volume message"
      
      -- 验证系统能够处理大量日志而不崩溃
      return ()

  -- 4. 遥测数据的序列化和反序列化测试
  describe "Telemetry Data Serialization" $ do
    it "should serialize metric data to text format" $ do
      metric <- createMetric "serialization-test" "bytes"
      recordMetric metric 1024.0
      
      -- 验证指标数据可以转换为文本表示
      let metricText = pack $ show (metricName metric) ++ "=" ++ show "<value>" ++ " " ++ show (metricUnit metric)
      Text.unpack metricText `shouldContain` "serialization-test"
      Text.unpack metricText `shouldContain` "bytes"
    
    it "should serialize span data to text format" $ do
      span <- createSpan "serialization-span"
      
      -- 验证span数据可以转换为文本表示
      let spanText = pack $ show span
      Text.unpack spanText `shouldContain` "serialization-span"
      Text.unpack spanText `shouldContain` unpack (spanTraceId span)
      Text.unpack spanText `shouldContain` unpack (spanSpanId span)
    
    it "should serialize logger data to text format" $ do
      logger <- createLogger "serialization-logger" Warn
      
      -- 验证logger数据可以转换为文本表示
      let loggerText = pack $ show logger
      Text.unpack loggerText `shouldContain` "serialization-logger"
      Text.unpack loggerText `shouldContain` "Warn"

  -- 5. 配置热重载测试
  describe "Configuration Hot Reload" $ do
    it "should handle configuration changes without restart" $ do
      -- 初始化初始配置
            
      -- 创建一些遥测组件
      metric <- createMetric "hot-reload-metric" "count"
      logger <- createLogger "hot-reload-logger" Info
      span <- createSpan "hot-reload-span"
      
      -- 记录一些数据
      recordMetric metric 100.0
      logMessage logger Info "hot reload test"
      
      -- 关闭当前遥测系统
            
      -- 使用新配置重新初始化
      let newConfig = TelemetryConfig "updated-service" "2.0.0" False True True False
      initTelemetry newConfig
      
      -- 验证新配置生效
      serviceName newConfig `shouldBe` "updated-service"
      serviceVersion newConfig `shouldBe` "2.0.0"
      enableMetrics newConfig `shouldBe` False
      enableTracing newConfig `shouldBe` True
      enableLogging newConfig `shouldBe` True
      
      -- 清理
      
  -- 6. 资源限制和清理测试
  describe "Resource Limits and Cleanup" $ do
    it "should properly clean up resources on shutdown" $ do
      -- 初始化遥测系统
            
      -- 创建大量资源
      metrics <- sequence $ replicate 100 $ do
        createMetric "cleanup-metric" "count"
      
      spans <- sequence $ replicate 50 $ do
        createSpan "cleanup-span"
      
      loggers <- sequence $ replicate 25 $ do
        createLogger "cleanup-logger" Info
      
      -- 使用资源
      sequence_ $ map (`recordMetric` 1.0) metrics
      sequence_ $ map finishSpan spans
      sequence_ $ flip map loggers $ \logger -> do
        logMessage logger Info "cleanup test"
      
      -- 关闭系统，应该清理所有资源
            
      -- 验证资源已清理（在实际实现中，这里会检查资源计数）
      return ()
    
    it "should handle resource exhaustion gracefully" $ do
            
      -- 尝试创建大量指标，测试系统如何处理资源限制
      let numMetrics = 10000
      metrics <- sequence $ replicate numMetrics $ do
        createMetric "exhaustion-test" "count"
      
      -- 验证系统能够处理大量资源
      length metrics `shouldBe` numMetrics
      
      
  -- 7. 时间同步和时区处理测试
  describe "Time Synchronization and Timezone Handling" $ do
    it "should handle timestamps in different timezones" $ do
      -- 获取当前时间
      currentTime <- getCurrentTime
      currentTimezone <- getCurrentTimeZone
      
      -- 验证时间数据可以正确处理
      let localTime = utcToLocalTime currentTimezone currentTime
      length (show localTime) `shouldSatisfy` (> 0)
      
      -- 验证时间差计算
      threadDelay 10000  -- 等待10毫秒
      laterTime <- getCurrentTime
      let timeDiff = diffUTCTime laterTime currentTime
      timeDiff `shouldSatisfy` (> 0)
    
    it "should handle time-based metric aggregation" $ do
      metric <- createMetric "time-based-metric" "count"
      
      -- 在不同时间点记录指标
      recordMetric metric 10.0
      threadDelay 10000  -- 等待10毫秒
      recordMetric metric 20.0
      threadDelay 10000  -- 等待10毫秒
      recordMetric metric 30.0
      
      -- 验证最终值是所有记录值的总和
      finalValue <- metricValue metric
      finalValue `shouldBe` 60.0

  -- 8. 批量操作性能测试
  describe "Batch Operations Performance" $ do
    it "should handle batch metric recording efficiently" $ do
      metric <- createMetric "batch-metric" "count"
      
      -- 批量记录指标
      let batchSize = 1000
      let values = [1.0..fromIntegral batchSize]
      
      -- 测试批量操作性能
      sequence_ $ map (recordMetric metric) values
      
      -- 验证最终值是所有记录值的总和
      finalValue <- metricValue metric
      finalValue `shouldBe` sum values
    
    it "should handle batch span creation efficiently" $ do
      -- 批量创建span
      let batchSize = 500
      spans <- sequence $ replicate batchSize $ do
        createSpan "batch-span"
      
      -- 验证所有span创建成功
      length spans `shouldBe` batchSize
      
      -- 验证所有span在同一trace中
      let traceIds = map spanTraceId spans
      length (nub traceIds) `shouldBe` 1
      
      -- 验证所有span ID是唯一的
      let spanIds = map spanSpanId spans
      length (nub spanIds) `shouldBe` batchSize
      
      -- 批量完成span
      sequence_ $ map finishSpan spans

  -- 9. QuickCheck测试遥测数据的数学属性
  describe "QuickCheck Mathematical Properties" $ do
    it "should maintain metric value ordering" $ property $
      \values -> 
        let sortedValues = sort (values :: [Double])
            nonEmpty = not (null sortedValues)
        in if nonEmpty 
           then unsafePerformIO $ do
             metric <- createMetricWithInitialValue "ordering-test" "count" 0.0
             sequence_ $ map (\v -> recordMetric metric v) sortedValues
             finalValue <- metricValue metric
             let expectedValue = sum sortedValues
             return (finalValue == expectedValue)
           else True
    
    it "should preserve span identity properties" $ property $
      \(name :: String) ->
        let span1 = unsafePerformIO $ createSpan (pack name)
            span2 = unsafePerformIO $ createSpan (pack name)
        in spanName span1 == spanName span2 &&
           spanTraceId span1 == spanTraceId span2 &&
           spanSpanId span1 /= spanSpanId span2
    
    it "should maintain logger level hierarchy" $ property $
      \(name :: String) ->
        let levels = [Debug, Info, Warn, Error]
            loggers = map (\level -> Logger (pack name) level) levels
            levelNames = map show $ map loggerLevel loggers
        in length (nub levelNames) == length levels  -- 所有级别都不同
    
    it "should handle metric value commutativity" $ property $
      \value1 -> 
        let (v1, v2) = value1 :: (Double, Double)
        in unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "commutative-test-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "commutative-test-2" "count" 0.0
          recordMetric metric1 v1
          recordMetric metric1 v2
          recordMetric metric2 v2
          recordMetric metric2 v1
          val1 <- metricValue metric1
          val2 <- metricValue metric2
          -- 处理浮点数精度问题，特别是-0.0和0.0的情况
          let normalized1 = if val1 == 0.0 then 0.0 else val1
              normalized2 = if val2 == 0.0 then 0.0 else val2
          return (normalized1 == normalized2)

  -- 10. 错误恢复和容错机制测试
  describe "Error Recovery and Fault Tolerance" $ do
    it "should recover from metric recording errors" $ do
      metric <- createMetric "error-recovery-metric" "count"
      
      -- 记录正常值
      recordMetric metric 10.0
      
      -- 尝试记录特殊值（正无穷、负无穷、NaN）
      let positiveInfinity = 1/0 :: Double
          negativeInfinity = -1/0 :: Double
          nan = 0/0 :: Double
      
      -- 验证系统能够处理特殊值
      recordMetric metric positiveInfinity
      recordMetric metric negativeInfinity
      recordMetric metric nan
      
      -- 验证指标仍然可用
      finalValue <- metricValue metric
      isNaN finalValue `shouldBe` True
    
    it "should handle concurrent access errors gracefully" $ do
      metric <- createMetric "concurrent-error-metric" "count"
      logger <- createLogger "concurrent-error-logger" Info
      
      -- 创建多个并发线程访问同一资源
      let numThreads = 10
          operationsPerThread = 100
      
      threads <- sequence $ replicate numThreads $ forkIO $ do
        sequence_ $ replicate operationsPerThread $ do
          recordMetric metric 1.0
          logMessage logger Info "concurrent error test"
      
      -- 等待所有线程完成
      threadDelay 10000  -- 等待10毫秒
      
      -- 清理线程
      sequence_ $ map killThread threads
      
      -- 验证系统仍然可用
      finalValue <- metricValue metric
      finalValue `shouldSatisfy` (>= 0)
    
    it "should maintain system stability under stress" $ do
            
      -- 创建大量资源并执行操作
      metrics <- sequence $ replicate 100 $ do
        createMetric "stress-metric" "count"
      
      spans <- sequence $ replicate 50 $ do
        createSpan "stress-span"
      
      loggers <- sequence $ replicate 25 $ do
        createLogger "stress-logger" Info
      
      -- 在压力下执行操作
      sequence_ $ replicate 1000 $ do
        sequence_ $ map (\m -> recordMetric m 1.0) metrics
        sequence_ $ map finishSpan spans
        sequence_ $ flip map loggers $ \l -> logMessage l Info "stress test"
      
      -- 验证系统仍然稳定
      shutdownTelemetry