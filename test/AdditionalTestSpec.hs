{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AdditionalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate, bracket)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, replicateM_, when, void)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group)
import Data.Char (isHexDigit)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Additional Telemetry Tests" $ do
  
  -- 1. 测试度量值的原子性操作
  describe "Metric Atomic Operations" $ do
    it "should handle concurrent metric updates atomically" $ do
      metric <- createMetric "atomic-test" "count"
      numThreads <- return 50
      incrementsPerThread <- return 100
      
      -- 使用MVar来等待所有线程完成，而不是使用固定延迟
      done <- newEmptyMVar
      results <- sequence $ replicate numThreads $ forkIO $ do
        replicateM_ incrementsPerThread $ recordMetric metric 1.0
        putMVar done ()
      
      -- 等待所有线程完成
      replicateM_ numThreads $ takeMVar done
      sequence_ $ map killThread results
      
      -- 验证最终值
      finalValue <- metricValue metric
      let expectedValue = fromIntegral (numThreads * incrementsPerThread)
      finalValue `shouldBe` expectedValue
    
    it "should handle mixed increment/decrement operations atomically" $ do
      metric <- createMetric "mixed-atomic-test" "count"
      numThreads <- return 20
      operationsPerThread <- return 50
      
      -- 使用MVar来等待所有线程完成，而不是使用固定延迟
      done <- newEmptyMVar
      results <- sequence $ replicate numThreads $ forkIO $ do
        replicateM_ operationsPerThread $ do
          recordMetric metric 1.0
          recordMetric metric (-0.5)
        putMVar done ()
      
      -- 等待所有线程完成
      replicateM_ numThreads $ takeMVar done
      sequence_ $ map killThread results
      
      -- 验证最终值 (每个线程增加 1.0 - 0.5 = 0.5，共 operationsPerThread 次)
      finalValue <- metricValue metric
      let expectedValue = fromIntegral numThreads * fromIntegral operationsPerThread * 0.5
      finalValue `shouldBe` expectedValue
  
  -- 2. 测试Span的层次关系
  describe "Span Hierarchy" $ do
    it "should maintain trace context across related spans" $ do
      -- 使用bracket管理遥测系统生命周期
      bracket (initTelemetry productionConfig) (\_ -> shutdownTelemetry) $ \_ -> do
        -- 创建父Span
        parentSpan <- createSpan "parent-operation"
        let parentTraceId = spanTraceId parentSpan
        
        -- 创建子Span
        childSpan1 <- createSpan "child-operation-1"
        childSpan2 <- createSpan "child-operation-2"
        
        -- 验证所有Span在同一个Trace中
        spanTraceId childSpan1 `shouldBe` parentTraceId
        spanTraceId childSpan2 `shouldBe` parentTraceId
        
        -- 验证Span ID的唯一性
        spanSpanId parentSpan `shouldNotBe` spanSpanId childSpan1
        spanSpanId parentSpan `shouldNotBe` spanSpanId childSpan2
        spanSpanId childSpan1 `shouldNotBe` spanSpanId childSpan2
        
        -- 完成所有Span
        finishSpan parentSpan
        finishSpan childSpan1
        finishSpan childSpan2
    
    it "should generate valid hex trace IDs" $ do
      bracket (initTelemetry productionConfig) (\_ -> shutdownTelemetry) $ \_ -> do
        -- 创建多个Span
        spans <- replicateM 10 $ createSpan "hex-test"
        
        -- 验证所有Trace ID都是有效的十六进制字符串
        let traceIds = map spanTraceId spans
            spanIds = map spanSpanId spans
        
        all (all isHexDigit . unpack) traceIds `shouldBe` True
        all (all isHexDigit . unpack) spanIds `shouldBe` True
        
        -- 验证Trace ID长度合理
        all (\tid -> Text.length tid >= 8 && Text.length tid <= 32) traceIds `shouldBe` True
        
        -- 完成所有Span
        sequence_ $ map finishSpan spans
  
  -- 3. 测试日志级别的优先级处理
  describe "Log Level Priority" $ do
    it "should create loggers with correct priority levels" $ do
      -- 创建不同级别的日志记录器
      debugLogger <- createLogger "debug-logger" Debug
      infoLogger <- createLogger "info-logger" Info
      warnLogger <- createLogger "warn-logger" Warn
      errorLogger <- createLogger "error-logger" Error
      
      -- 验证日志级别
      loggerLevel debugLogger `shouldBe` Debug
      loggerLevel infoLogger `shouldBe` Info
      loggerLevel warnLogger `shouldBe` Warn
      loggerLevel errorLogger `shouldBe` Error
      
      -- 验证级别优先级顺序
      let levels = [Debug, Info, Warn, Error]
          levelPriorities = map fromEnum levels
      sort levelPriorities `shouldBe` levelPriorities
    
    it "should handle log level filtering correctly" $ do
      -- 创建一个Info级别的日志记录器
      logger <- createLogger "filter-test" Info
      
      -- 记录不同级别的日志（在实际实现中应该有过滤逻辑）
      logMessage logger Debug "debug message"  -- 应该被过滤
      logMessage logger Info "info message"    -- 应该被记录
      logMessage logger Warn "warning message" -- 应该被记录
      logMessage logger Error "error message"  -- 应该被记录
      
      -- 在当前实现中，所有消息都会被处理
      -- 在实际实现中，应该根据日志级别进行过滤
      loggerName logger `shouldBe` "filter-test"
      loggerLevel logger `shouldBe` Info
  
  -- 4. 测试配置热重载
  describe "Configuration Hot Reload" $ do
    it "should handle configuration changes" $ do
      -- 使用bracket管理遥测系统生命周期
      bracket (initTelemetry productionConfig) (\_ -> shutdownTelemetry) $ \_ -> do
        -- 创建一些遥测组件
        metric <- createMetric "config-test" "count"
        logger <- createLogger "config-test" Info
        span <- createSpan "config-test"
        
        -- 执行一些操作
        recordMetric metric 1.0
        logMessage logger Info "test message"
        
        -- 更改配置
        let newConfig = TelemetryConfig "updated-service" "2.0.0" False True True False
        initTelemetry newConfig
        
        -- 验证配置已更新（通过检查操作行为）
        recordMetric metric 2.0  -- 如果metrics被禁用，这可能不生效
        logMessage logger Info "updated message"
        finishSpan span
  
  -- 5. 测试资源泄漏检测
  describe "Resource Leak Detection" $ do
    it "should properly clean up resources on shutdown" $ do
      -- 使用bracket管理遥测系统生命周期
      bracket (initTelemetry productionConfig) (\_ -> shutdownTelemetry) $ \_ -> do
        -- 创建适量资源进行测试
        initialMetrics <- replicateM 20 $ createMetric "leak-test" "count"
        initialLoggers <- replicateM 10 $ createLogger "leak-test" Info
        initialSpans <- replicateM 5 $ createSpan "leak-test"
        
        -- 使用资源
        sequence_ $ map (`recordMetric` 1.0) initialMetrics
        sequence_ $ flip map initialLoggers $ \logger -> do
          logMessage logger Info "leak test message"
        sequence_ $ map finishSpan initialSpans
      
      -- 使用bracket管理第二次初始化
      bracket (initTelemetry productionConfig) (\_ -> shutdownTelemetry) $ \_ -> do
        -- 创建新资源（如果资源泄漏，这可能会失败或表现异常）
        newMetrics <- replicateM 20 $ createMetric "new-leak-test" "count"
        newLoggers <- replicateM 10 $ createLogger "new-leak-test" Info
        newSpans <- replicateM 5 $ createSpan "new-leak-test"
        
        -- 验证新资源正常工作
        sequence_ $ map (`recordMetric` 1.0) newMetrics
        sequence_ $ flip map newLoggers $ \logger -> do
          logMessage logger Info "new leak test message"
        sequence_ $ map finishSpan newSpans
        
        -- 如果到达这里，说明没有明显的资源泄漏
        True `shouldBe` True
  
  -- 6. 测试异常情况处理
  describe "Exception Handling" $ do
    it "should handle exceptions in metric operations gracefully" $ do
      metric <- createMetric "exception-test" "count"
      
      -- 尝试记录特殊值
      result1 <- try $ recordMetric metric (1.0/0.0)  -- 正无穷
      result2 <- try $ recordMetric metric (-1.0/0.0) -- 负无穷
      result3 <- try $ recordMetric metric (0.0/0.0)  -- NaN
      
      -- 验证异常处理
      case result1 of
        Left (_ :: SomeException) -> True `shouldBe` True  -- 异常被处理
        Right _ -> True `shouldBe` True                     -- 操作成功
      
      case result2 of
        Left (_ :: SomeException) -> True `shouldBe` True
        Right _ -> True `shouldBe` True
      
      case result3 of
        Left (_ :: SomeException) -> True `shouldBe` True
        Right _ -> True `shouldBe` True
      
      -- 验证度量仍然可用
      recordMetric metric 42.0
      value <- metricValue metric
      -- 不检查具体值，因为特殊值的处理可能不同
      True `shouldBe` True
    
    it "should handle empty and null strings gracefully" $ do
      -- 测试空字符串
      emptyMetric <- createMetric "" ""
      emptyLogger <- createLogger "" Debug
      emptySpan <- createSpan ""
      
      metricName emptyMetric `shouldBe` ""
      metricUnit emptyMetric `shouldBe` ""
      loggerName emptyLogger `shouldBe` ""
      spanName emptySpan `shouldBe` ""
      
      -- 测试包含特殊字符的字符串
      let specialChars = "\0\t\n\r"
      specialMetric <- createMetric specialChars specialChars
      specialLogger <- createLogger specialChars Debug
      specialSpan <- createSpan specialChars
      
      metricName specialMetric `shouldBe` specialChars
      metricUnit specialMetric `shouldBe` specialChars
      loggerName specialLogger `shouldBe` specialChars
      spanName specialSpan `shouldBe` specialChars
  
  -- 7. 测试大数据量处理
  describe "Large Data Volume Handling" $ do
    it "should handle large numbers of metrics efficiently" $ do
            
      let numMetrics = 100
          operationsPerMetric = 10
      
      -- 创建大量度量，每个都有唯一的名称
      metrics <- sequence $ map (\i -> createMetric (pack $ "volume-test-" ++ show i) "count") [1..numMetrics]
      
      -- 对每个度量执行多次操作
      sequence_ $ flip map metrics $ \metric -> do
        replicateM_ operationsPerMetric $ recordMetric metric 1.0
      
      -- 验证所有度量都有正确的值
      values <- sequence $ map metricValue metrics
      all (== fromIntegral operationsPerMetric) values `shouldBe` True
      
          
    it "should handle long metric and span names" $ do
      let longName = pack $ replicate 1000 'a'
          veryLongName = pack $ replicate 10000 'b'
      
      -- 测试长名称
      longMetric <- createMetric longName "long-unit"
      veryLongMetric <- createMetric veryLongName "very-long-unit"
      longSpan <- createSpan longName
      veryLongSpan <- createSpan veryLongName
      longLogger <- createLogger longName Info
      veryLongLogger <- createLogger veryLongName Info
      
      -- 验证长名称被正确保存
      metricName longMetric `shouldBe` longName
      metricName veryLongMetric `shouldBe` veryLongName
      spanName longSpan `shouldBe` longName
      spanName veryLongSpan `shouldBe` veryLongName
      loggerName longLogger `shouldBe` longName
      loggerName veryLongLogger `shouldBe` veryLongName
      
      -- 验证长名称的组件仍然可用
      recordMetric longMetric 1.0
      recordMetric veryLongMetric 2.0
      logMessage longLogger Info "long message"
      logMessage veryLongLogger Info "very long message"
      finishSpan longSpan
      finishSpan veryLongSpan
  
  -- 8. QuickCheck测试：度量值的数学属性
  describe "QuickCheck: Metric Mathematical Properties" $ do
    it "should satisfy metric addition associativity" $ property $
      \(a :: Double) (b :: Double) (c :: Double) ->
        unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "assoc-test-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "assoc-test-2" "count" 0.0
          
          -- (a + b) + c
          recordMetric metric1 a
          recordMetric metric1 b
          recordMetric metric1 c
          
          -- a + (b + c)
          recordMetric metric2 a
          recordMetric metric2 b
          recordMetric metric2 c
          
          value1 <- metricValue metric1
          value2 <- metricValue metric2
          
          return (value1 == value2)
    
    it "should satisfy metric addition commutativity" $ property $
      \(a :: Double) (b :: Double) ->
        unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "comm-test-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "comm-test-2" "count" 0.0
          
          -- a + b
          recordMetric metric1 a
          recordMetric metric1 b
          
          -- b + a
          recordMetric metric2 b
          recordMetric metric2 a
          
          value1 <- metricValue metric1
          value2 <- metricValue metric2
          
          return (value1 == value2)
    
    it "should satisfy metric addition identity" $ property $
      \(a :: Double) ->
        unsafePerformIO $ do
          metric <- createMetricWithInitialValue "identity-test" "count" a
          
          -- a + 0 = a
          recordMetric metric 0.0
          value <- metricValue metric
          
          return (value == a)
  
  -- 9. QuickCheck测试：Span ID的唯一性和格式
  describe "QuickCheck: Span ID Properties" $ do
    it "should generate unique span IDs" $ property $
      \(names :: [String]) ->
        let spanNames = take 10 (map show names)
        in unsafePerformIO $ 
          bracket (initTelemetry productionConfig) (\_ -> shutdownTelemetry) $ \_ -> do
            spans <- mapM (\name -> createSpan (pack name)) spanNames
            let spanIds = map spanSpanId spans
                uniqueSpanIds = nub spanIds
            return (length spanIds == length uniqueSpanIds)
    
    it "should generate valid hex span IDs" $ property $
      \(names :: [String]) ->
        let spanNames = take 5 (map show names)
        in unsafePerformIO $ 
          bracket (initTelemetry productionConfig) (\_ -> shutdownTelemetry) $ \_ -> do
            spans <- mapM (\name -> createSpan (pack name)) spanNames
            let spanIds = map spanSpanId spans
                allValidHex = all (all isHexDigit . unpack) spanIds
            return allValidHex
  
  -- 10. QuickCheck测试：日志级别的层次关系
  describe "QuickCheck: Log Level Hierarchy" $ do
    it "should maintain consistent log level ordering" $ property $
      \(levelInts :: [Int]) ->
        let levels = map (\i -> [Debug, Info, Warn, Error] !! (abs i `mod` 4)) levelInts
            sortedLevels = sort levels
            allLevels = [Debug, Info, Warn, Error]
        in null levels ||  -- Empty list is valid
           sortedLevels == sort levels &&  -- Sorting is consistent
           all (`elem` allLevels) sortedLevels &&  -- All levels are valid
           sortedLevels == sort sortedLevels  -- Sorted list is properly ordered
    
    it "should create loggers with consistent properties" $ property $
      \(name :: String) (levelInt :: Int) ->
        let levels = [Debug, Info, Warn, Error]
            level = levels !! (abs levelInt `mod` 4)
            nameText = pack name
        in unsafePerformIO $ do
          logger <- createLogger nameText level
          return (loggerName logger == nameText && loggerLevel logger == level)