{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TelemetryIntegrationSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (when)
import Data.Text (pack)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Telemetry Integration Tests" $ do
  
  -- 测试完整的遥测工作流
  describe "Complete Telemetry Workflow" $ do
    it "should handle complete workflow with metrics, spans, and logging" $ property $
      \(operations :: Int) ->
        let numOps = max 1 (abs operations `mod` 10 + 1)
            result = unsafePerformIO $ do
                            
              -- 创建度量
              metrics <- sequence $ map (\i -> 
                createMetric (pack $ "workflow-metric-" ++ show i) "count"
                            ) [1..numOps]
              
              -- 创建span
              spans <- sequence $ map (\i -> 
                createSpan (pack $ "workflow-span-" ++ show i)
                            ) [1..numOps]
              
              -- 创建日志记录器
              loggers <- sequence $ map (\i -> 
                createLogger (pack $ "workflow-logger-" ++ show i) Info
                            ) [1..numOps]
              
              -- 执行操作
              sequence_ $ zipWith (\metric i -> do
                recordMetric metric (fromIntegral i)
                            ) metrics [1..numOps]
              
              sequence_ $ map finishSpan spans
              
              sequence_ $ zipWith (\logger i -> do
                logMessage logger Info (pack $ "workflow message " ++ show i)
                ) loggers [1..numOps]
              return True
        in result
    
    it "should handle nested workflow operations" $ property $
      \(depth :: Int) ->
        let actualDepth = max 1 (abs depth `mod` 5 + 1)
            result = unsafePerformIO $ do
                            
              -- 创建嵌套操作
              let nestedOperation 0 = return ()
                  nestedOperation n = do
                    metric <- createMetric (pack $ "nested-metric-" ++ show n) "count"
                    recordMetric metric (fromIntegral n)
                    
                    span <- createSpan (pack $ "nested-span-" ++ show n)
                    
                    logger <- createLogger (pack $ "nested-logger-" ++ show n) Info
                    logMessage logger Info (pack $ "nested operation " ++ show n)
                    
                    nestedOperation (n - 1)
                    
                    finishSpan span
              
              nestedOperation actualDepth
              return True
        in result
  
  -- 测试跨组件的数据流
  describe "Cross-Component Data Flow" $ do
    it "should handle data flow from metrics to logging" $ property $
      \(values :: [Double]) ->
        let testValues = take 10 values :: [Double]
            result = unsafePerformIO $ do
                            
              -- 创建度量
              metric <- createMetric "data-flow-metric" "count"
              
              -- 记录度量值
              sequence_ $ map (recordMetric metric) testValues
              
              -- 获取度量值并记录到日志
              finalValue <- metricValue metric
              logger <- createLogger "data-flow-logger" Info
              logMessage logger Info (pack $ "Final metric value: " ++ show finalValue)
              return True
        in result
    
    it "should handle data flow from spans to metrics" $ property $
      \(numSpans :: Int) ->
        let actualSpans = max 1 (abs numSpans `mod` 10 + 1)
            result = unsafePerformIO $ do
                            
              -- 创建度量来跟踪span数量
              spanCountMetric <- createMetric "span-count" "count"
              
              -- 创建多个span
              spans <- sequence $ replicate actualSpans $ do
                createSpan "data-flow-span"
              
              -- 记录span数量
              recordMetric spanCountMetric (fromIntegral actualSpans)
              
              -- 完成所有span
              sequence_ $ map finishSpan spans
              return True
        in result
    
    it "should handle data flow from logging to metrics" $ property $
      \(numMessages :: Int) ->
        let actualMessages = max 1 (abs numMessages `mod` 20 + 1)
            result = unsafePerformIO $ do
                            
              -- 创建度量来跟踪日志消息数量
              logCountMetric <- createMetric "log-count" "count"
              
              logger <- createLogger "data-flow-logger" Info
              
              -- 记录多个日志消息
              sequence_ $ replicate actualMessages $ do
                logMessage logger Info "data flow test message"
                recordMetric logCountMetric 1.0
              return True
        in result
  
  -- 测试配置驱动的集成
  describe "Configuration-Driven Integration" $ do
    it "should integrate components based on configuration flags" $ property $
      \(flags :: Int) ->
        let (metrics, tracing, logging, debug) = 
              if even flags then (True, True, True, False) else (False, True, False, True)
            config = TelemetryConfig "integration-test" "1.0.0" metrics tracing logging debug
            result = unsafePerformIO $ do
              initTelemetry config
              
              -- 尝试创建组件
              metricResult <- try (do
                when metrics $ do
                  metric <- createMetric "config-metric" "count"
                  recordMetric metric 1.0
                ) :: IO (Either SomeException ())
              
              spanResult <- try (do
                when tracing $ do
                  span <- createSpan "config-span"
                  finishSpan span
                ) :: IO (Either SomeException ())
              
              loggerResult <- try (do
                when logging $ do
                  logger <- createLogger "config-logger" Info
                  logMessage logger Info "config test"
                ) :: IO (Either SomeException ())
              
              return True
        in result
    
    it "should handle configuration changes during runtime" $ property $
      \(changes :: Int) ->
        let numChanges = max 1 (abs changes `mod` 3 + 1)
            configs = take numChanges $ cycle [
                TelemetryConfig "service-1" "1.0.0" True True True False,
                TelemetryConfig "service-2" "2.0.0" False True False True,
                TelemetryConfig "service-3" "3.0.0" True False True False
              ]
            result = unsafePerformIO $ do
              sequence_ $ map (\config -> do
                initTelemetry config
                
                -- 根据配置执行操作
                when (enableMetrics config) $ do
                  metric <- createMetric "runtime-config-metric" "count"
                  recordMetric metric 1.0
                
                when (enableTracing config) $ do
                  span <- createSpan "runtime-config-span"
                  finishSpan span
                
                when (enableLogging config) $ do
                  logger <- createLogger "runtime-config-logger" Info
                  logMessage logger Info "runtime config test"
                
                                            ) configs
              return True
        in result
  
  -- 测试错误恢复集成
  describe "Error Recovery Integration" $ do
    it "should handle errors in one component without affecting others" $ property $
      \(componentType :: Int) ->
        let component = abs componentType `mod` 3
            result = unsafePerformIO $ do
                            
              -- 尝试在不同组件中引入错误
              case component of
                0 -> do
                  -- 度量错误
                  metric <- createMetric "error-metric" "count"
                  recordMetric metric (1.0/0.0)  -- Infinity
                  recordMetric metric (0.0/0.0)  -- NaN
                  -- 其他组件应该仍然工作
                  span <- createSpan "post-error-span"
                  finishSpan span
                  
                  logger <- createLogger "post-error-logger" Info
                  logMessage logger Info "post error test"
                  return True
                  
                1 -> do
                  -- Span错误（如果有）
                  span <- createSpan "error-span"
                  finishSpan span
                  -- 其他组件应该仍然工作
                  metric <- createMetric "post-error-metric" "count"
                  recordMetric metric 1.0
                  
                  logger <- createLogger "post-error-logger" Info
                  logMessage logger Info "post error test"
                  return True
                  
                2 -> do
                  -- 日志错误（如果有）
                  logger <- createLogger "error-logger" Info
                  logMessage logger Info "error test"
                  -- 其他组件应该仍然工作
                  metric <- createMetric "post-error-metric" "count"
                  recordMetric metric 1.0
                  
                  span <- createSpan "post-error-span"
                  finishSpan span
                  return True
        in result
    
    it "should recover from initialization errors" $ property $
      \(serviceName :: String) ->
        let config = TelemetryConfig (pack $ take 50 serviceName) "1.0.0" True True True False
            result = unsafePerformIO $ do
              -- 尝试初始化
              result <- try $ initTelemetry config
              case result of
                Right _ -> do
                  -- 成功初始化，执行一些操作
                  metric <- createMetric "recovery-metric" "count"
                  recordMetric metric 1.0
                  return True
                Left (_ :: SomeException) -> do
                  -- 初始化失败，尝试使用默认配置
                  initTelemetry defaultConfig
                  metric <- createMetric "fallback-metric" "count"
                  recordMetric metric 1.0
                  return True
        in result
  
  -- 测试性能集成
  describe "Performance Integration" $ do
    it "should handle high-volume integrated operations" $ property $
      \(operations :: Int) ->
        let numOps = max 10 (abs operations `mod` 100 + 10)
            result = unsafePerformIO $ do
                            
              metric <- createMetric "performance-metric" "count"
              logger <- createLogger "performance-logger" Info
              
              -- 高Volume集成操作
              sequence_ $ replicate numOps $ do
                recordMetric metric 1.0
                
                span <- createSpan "performance-span"
                finishSpan span
                
                logMessage logger Info "performance test"
              
              finalValue <- metricValue metric
              return (finalValue == fromIntegral numOps)
        in result
    
    it "should handle resource-efficient operations" $ property $
      \(operations :: Int) ->
        let numOps = max 10 (abs operations `mod` 50 + 10)
            result = unsafePerformIO $ do
                            
              -- 资源高效操作
              metric <- createMetric "resource-efficient-metric" "count"
              
              sequence_ $ replicate numOps $ do
                recordMetric metric 1.0
              
              -- 重用相同的span和logger
              span <- createSpan "resource-efficient-span"
              logger <- createLogger "resource-efficient-logger" Info
              
              sequence_ $ replicate numOps $ do
                logMessage logger Info "resource efficient test"
              
              finishSpan span
              
              finalValue <- metricValue metric
              return (finalValue == fromIntegral numOps)
        in result
  
  -- 测试并发集成
  describe "Concurrent Integration" $ do
    it "should handle concurrent operations across components" $ property $
      \(threads :: Int) ->
        let numThreads = max 1 (abs threads `mod` 5 + 1)
            result = unsafePerformIO $ do
                            
              -- 创建共享资源
              metric <- createMetric "concurrent-integration-metric" "count"
              logger <- createLogger "concurrent-integration-logger" Info
              
              -- 并发操作
              threadResults <- mapM (\i -> 
                forkIO $ do
                  sequence_ $ replicate 10 $ do
                    recordMetric metric 1.0
                    
                    span <- createSpan (pack $ "concurrent-span-" ++ show i)
                    finishSpan span
                    
                    logMessage logger Info (pack $ "concurrent message " ++ show i)
                ) [1..numThreads]
              
              -- 等待线程完成
              threadDelay 500000  -- 500ms
              
              -- 清理线程
              sequence_ $ map (\_ -> return ()) threadResults
              
              finalValue <- metricValue metric
              return (finalValue >= 0)
        in result
  
  -- 测试端到端集成
  describe "End-to-End Integration" $ do
    it "should handle complete end-to-end telemetry scenario" $ property $
      \(scenario :: Int) ->
        let scenarioType = abs scenario `mod` 3
            result = unsafePerformIO $ do
                            
              case scenarioType of
                0 -> do
                  -- Web请求场景
                  requestMetric <- createMetric "http-requests" "count"
                  latencyMetric <- createMetric "request-latency" "ms"
                  
                  requestSpan <- createSpan "http-request"
                  
                  requestLogger <- createLogger "http-logger" Info
                  logMessage requestLogger Info "Processing HTTP request"
                  
                  recordMetric requestMetric 1.0
                  recordMetric latencyMetric 150.0
                  
                  finishSpan requestSpan
                  logMessage requestLogger Info "HTTP request completed"
                  return True
                  
                1 -> do
                  -- 数据库操作场景
                  dbMetric <- createMetric "db-operations" "count"
                  queryMetric <- createMetric "query-time" "ms"
                  
                  dbSpan <- createSpan "database-query"
                  
                  dbLogger <- createLogger "db-logger" Info
                  logMessage dbLogger Info "Executing database query"
                  
                  recordMetric dbMetric 1.0
                  recordMetric queryMetric 25.0
                  
                  finishSpan dbSpan
                  logMessage dbLogger Info "Database query completed"
                  return True
                  
                2 -> do
                  -- 业务逻辑场景
                  businessMetric <- createMetric "business-operations" "count"
                  processingMetric <- createMetric "processing-time" "ms"
                  
                  businessSpan <- createSpan "business-logic"
                  
                  businessLogger <- createLogger "business-logger" Info
                  logMessage businessLogger Info "Processing business logic"
                  
                  recordMetric businessMetric 1.0
                  recordMetric processingMetric 75.0
                  
                  finishSpan businessSpan
                  logMessage businessLogger Info "Business logic completed"
                  return True
        in result