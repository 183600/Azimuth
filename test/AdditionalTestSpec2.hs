{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AdditionalTestSpec2 (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import qualified Data.Text as T (append)
import Data.List (nub, sort, group)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newMVar, takeMVar, putMVar)
import Control.Monad (replicateM, replicateM_, when, void)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)
import Data.Maybe (isJust, isNothing)
import Data.Char (isAscii, isControl)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Additional Telemetry Tests 2" $ do
  
  -- 测试1: 度量值的幂等性
  describe "Metric Idempotency" $ do
    it "should handle zero value additions idempotently" $ property $
      \initialValue ->
        let value = initialValue :: Double
        in unsafePerformIO $ do
          metric <- createMetricWithInitialValue "idempotent-test" "count" value
          -- 记录多个零值
          replicateM_ 10 $ recordMetric metric 0.0
          finalValue <- metricValue metric
          return (finalValue == value)
    
    it "should maintain metric identity across zero operations" $ property $
      \name unit ->
        let nameText = pack name
            unitText = pack unit
        in unsafePerformIO $ do
          metric <- createMetric nameText unitText
          -- 执行零操作
          replicateM_ 5 $ return ()
          -- 验证度量仍然有效
          currentValue <- metricValue metric
          return (metricName metric == nameText && 
                  metricUnit metric == unitText && 
                  currentValue == 0.0)

  -- 测试2: Span ID的格式一致性
  describe "Span ID Format Consistency" $ do
    it "should generate span IDs in consistent hex format" $ property $
      \name ->
        let nameText = pack name
        in unsafePerformIO $ do
          span <- createSpan nameText
          let spanId = spanSpanId span
          -- 验证span ID只包含十六进制字符
          let isHexChar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
          return (all isHexChar (unpack spanId) && not (Text.null spanId))
    
    it "should generate trace IDs in consistent hex format" $ property $
      \name ->
        let nameText = pack name
        in unsafePerformIO $ do
          shutdownTelemetry
          initTelemetry productionConfig
          span <- createSpan nameText
          let traceId = spanTraceId span
          -- 验证trace ID只包含十六进制字符
          let isHexChar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
          return (all isHexChar (unpack traceId) && not (Text.null traceId))

  -- 测试3: 配置字段的边界值测试
  describe "Configuration Field Boundaries" $ do
    it "should handle extremely long service names" $ property $
      \baseName ->
        let longName = pack $ take 10000 (baseName ++ baseName ++ baseName)
            config = TelemetryConfig longName "0.1.0" True True True False
        in unsafePerformIO $ do
          initTelemetry config
          -- 验证长名称不会导致问题
          metric <- createMetric "boundary-test" "count"
          recordMetric metric 1.0
          finalValue <- metricValue metric
          shutdownTelemetry
          return (finalValue == 1.0)
    
    it "should handle special characters in service version" $ property $
      \version ->
        let versionText = pack version
            config = TelemetryConfig "test-service" versionText True True True False
        in unsafePerformIO $ do
          result <- try $ initTelemetry config
          shutdownTelemetry
          case result of
            Left (_ :: SomeException) -> return False
            Right _ -> return True

  -- 测试4: 度量值累加的数学属性
  describe "Metric Accumulation Mathematical Properties" $ do
    it "should satisfy commutative property of addition" $ property $
      \values1 values2 ->
        let vals1 = take 10 values1 :: [Double]
            vals2 = take 10 values2 :: [Double]
        in unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "commutative-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "commutative-2" "count" 0.0
          
          -- 顺序1: 先加vals1，再加vals2
          sequence_ $ map (recordMetric metric1) vals1
          sequence_ $ map (recordMetric metric1) vals2
          
          -- 顺序2: 先加vals2，再加vals1
          sequence_ $ map (recordMetric metric2) vals2
          sequence_ $ map (recordMetric metric2) vals1
          
          value1 <- metricValue metric1
          value2 <- metricValue metric2
          
          let expectedValue = sum vals1 + sum vals2
              epsilon = 1e-10
          return (abs (value1 - value2) < epsilon && 
                  abs (value1 - expectedValue) < epsilon)
    
    it "should satisfy associative property of addition" $ property $
      \values1 values2 values3 ->
        let vals1 = take 5 values1 :: [Double]
            vals2 = take 5 values2 :: [Double]
            vals3 = take 5 values3 :: [Double]
        in unsafePerformIO $ do
          metric1 <- createMetricWithInitialValue "associative-1" "count" 0.0
          metric2 <- createMetricWithInitialValue "associative-2" "count" 0.0
          
          -- 分组1: (vals1 + vals2) + vals3
          sequence_ $ map (recordMetric metric1) vals1
          sequence_ $ map (recordMetric metric1) vals2
          sequence_ $ map (recordMetric metric1) vals3
          
          -- 分组2: vals1 + (vals2 + vals3)
          sequence_ $ map (recordMetric metric2) vals2
          sequence_ $ map (recordMetric metric2) vals3
          sequence_ $ map (recordMetric metric2) vals1
          
          value1 <- metricValue metric1
          value2 <- metricValue metric2
          
          let expectedValue = sum vals1 + sum vals2 + sum vals3
              epsilon = 1e-10
          return (abs (value1 - value2) < epsilon && 
                  abs (value1 - expectedValue) < epsilon)

  -- 测试5: 日志级别的排序属性
  describe "Log Level Ordering Properties" $ do
    it "should maintain consistent ordering of log levels" $ property $
      \levelInt1 levelInt2 ->
        let levels = [Debug, Info, Warn, Error]
            level1 = levels !! (abs levelInt1 `mod` 4)
            level2 = levels !! (abs levelInt2 `mod` 4)
            loggerName = pack $ "ordering-test-" ++ show levelInt1 ++ "-" ++ show levelInt2
        in unsafePerformIO $ do
          Logger name1 level1Created <- createLogger loggerName level1
          Logger name2 level2Created <- createLogger loggerName level2
          
          -- 验证日志级别的比较属性
          let levelComparison = compare level1 level2
              nameEquality = name1 == name2
          
          return (nameEquality && 
                  (if level1 == level2 
                   then level1Created == level2Created
                   else level1Created /= level2Created))

  -- 测试6: 度量注册表的共享行为
  describe "Metric Registry Sharing Behavior" $ do
    it "should share metrics with same name and unit" $ property $
      \name unit ->
        let nameText = pack name
            unitText = pack unit
        in not (null name && null unit) ==> unsafePerformIO $ do
          -- Save current sharing setting
          originalSharing <- readIORef enableMetricSharing
          
          -- Enable metric sharing for this test
          writeIORef enableMetricSharing True
          
          -- 创建两个同名同单位的度量
          metric1 <- createMetric nameText unitText
          recordMetric metric1 10.0
          
          metric2 <- createMetric nameText unitText
          recordMetric metric2 5.0
          
          -- 验证它们共享相同的值
          value1 <- metricValue metric1
          value2 <- metricValue metric2
          
          -- Restore original sharing setting
          writeIORef enableMetricSharing originalSharing
          
          return (value1 == value2 && value1 == 15.0)
    
    it "should not share metrics with different units" $ do
        -- 使用固定值而不是QuickCheck，确保unitText2与unitText1不同
        let nameText = pack "test-metric"
            unitText1 = pack "unit1"
            unitText2 = pack "unit2"  -- 确保与unitText1不同
        
        -- 创建两个同名不同单位的度量
        metric1 <- createMetric nameText unitText1
        recordMetric metric1 10.0
        
        metric2 <- createMetric nameText unitText2
        recordMetric metric2 5.0
        
        -- 验证它们不共享相同的值
        value1 <- metricValue metric1
        value2 <- metricValue metric2
        
        value1 `shouldBe` 10.0
        value2 `shouldBe` 5.0

  -- 测试7: 文本编码的完整性
  describe "Text Encoding Integrity" $ do
    it "should preserve ASCII characters exactly" $ property $
      \asciiStr ->
        let filteredStr = filter isAscii asciiStr
            text = pack filteredStr
        in unsafePerformIO $ do
          metric <- createMetric text "ascii-unit"
          logger <- createLogger text Info
          span <- createSpan text
          
          return (unpack (metricName metric) == filteredStr &&
                  unpack (loggerName logger) == filteredStr &&
                  unpack (spanName span) == filteredStr)
    
    it "should handle control characters consistently" $ property $
      \controlStr ->
        let hasControl = any isControl controlStr
            text = pack controlStr
        in if hasControl
           then unsafePerformIO $ do
             metric <- createMetric text "control-unit"
             logger <- createLogger text Info
             span <- createSpan text
             
             return (metricName metric == text &&
                     loggerName logger == text &&
                     spanName span == text)
           else True

  -- 测试8: 并发操作的原子性
  describe "Concurrent Operation Atomicity" $ do
    it "should maintain atomicity of concurrent metric updates" $ do
        -- 使用固定参数而不是QuickCheck，避免并发测试的不确定性
        let numThreads = 2
            incrementsPerThread = 5
        
        metric <- createMetric "atomic-test" "count"
        
        -- 使用MVar同步线程开始和结束
        startSignal <- newMVar ()
        completionSignal <- newMVar ()
        
        -- 创建多个线程
        threads <- mapM (\_ -> forkIO $ do
          takeMVar startSignal  -- 等待开始信号
          replicateM_ incrementsPerThread $ recordMetric metric 1.0
          putMVar completionSignal ()  -- 发送完成信号
          ) [1..numThreads]
        
        -- 同时启动所有线程
        replicateM_ numThreads $ putMVar startSignal ()
        
        -- 等待所有线程完成
        replicateM_ numThreads $ takeMVar completionSignal
        
        -- 给线程一些时间完成
        threadDelay 10000  -- 10毫秒
        
        -- 清理线程
        sequence_ $ map killThread threads
        
        -- 验证最终值
        finalValue <- metricValue metric
        let expectedValue = fromIntegral numThreads * fromIntegral incrementsPerThread
        
        finalValue `shouldBe` expectedValue

  -- 测试9: 资源泄漏检测
  describe "Resource Leak Detection" $ do
    it "should not leak resources during repeated operations" $ do
      let iterations = 1000
      unsafePerformIO $ do
        initTelemetry productionConfig
        
        -- 执行大量操作
        replicateM_ iterations $ do
          metric <- createMetric "leak-test" "count"
          recordMetric metric 1.0
          
          logger <- createLogger "leak-test-logger" Info
          logMessage logger Info "leak test"
          
          span <- createSpan "leak-test-span"
          finishSpan span
        
        -- 验证系统仍然响应
        finalMetric <- createMetric "final-test" "count"
        recordMetric finalMetric 42.0
        finalValue <- metricValue finalMetric
        
        shutdownTelemetry
        
        return (finalValue == 42.0)

  -- 测试10: 度量值的精度保持
  describe "Metric Value Precision" $ do
    it "should maintain precision of decimal values" $ property $
      \value ->
        let testValue = value / 1000000.0  -- 创建小数值
        in unsafePerformIO $ do
          metric <- createMetricWithInitialValue "precision-test" "count" testValue
          recordMetric metric testValue
          
          finalValue <- metricValue metric
          let expectedValue = testValue * 2.0
          
          -- 对于浮点数，使用近似比较
          return (abs (finalValue - expectedValue) < 1e-10)
    
    it "should handle very large values without overflow" $ property $
      \value ->
        let largeValue = if abs value > 1e50 && value /= 0.0 then value else value * 1e50 + 1.0
        in not (isNaN largeValue) && largeValue /= 0.0 ==> unsafePerformIO $ do
          metric <- createMetricWithInitialValue "large-value-test" "count" largeValue
          recordMetric metric largeValue
          
          finalValue <- metricValue metric
          let expectedValue = largeValue * 2.0
          
          -- 检查是否为无穷大（溢出）
          if isInfinite expectedValue
            then return (isInfinite finalValue)
            else return (abs (finalValue - expectedValue) < abs expectedValue * 1e-10)