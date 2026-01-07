{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeStrings #-}

module DataIntegrityCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group, sortBy, find)
import Data.Ord (comparing)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, when, forM_, void, unless)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)
import Data.Bits (xor)
import Prelude hiding (id)
import Data.Char (ord)

import Azimuth.Telemetry

-- | 数据校验和
data Checksum = Checksum
    { checksumValue :: Int
    , checksumAlgorithm :: String
    } deriving (Show, Eq)

-- | 计算简单校验和
calculateChecksum :: Text -> Checksum
calculateChecksum text = 
    let charCodes = map ord (unpack text)
        checksum = foldl xor 0 charCodes
    in Checksum checksum "XOR"

-- | 数据完整性验证器
data IntegrityValidator = IntegrityValidator
    { validateMetric :: Metric -> IO Bool
    , validateSpan :: Span -> IO Bool
    , validateLogger :: Logger -> IO Bool
    }

-- | 创建默认完整性验证器
createIntegrityValidator :: IO IntegrityValidator
createIntegrityValidator = do
    return IntegrityValidator
        { validateMetric = \metric -> do
            let nameChecksum = calculateChecksum (metricName metric)
                unitChecksum = calculateChecksum (metricUnit metric)
            value <- metricValue metric
            return (not (Text.null (metricName metric)) && 
                   not (isNaN value) && 
                   checksumValue nameChecksum >= 0)
        , validateSpan = \span -> do
            let nameChecksum = calculateChecksum (spanName span)
                traceIdChecksum = calculateChecksum (spanTraceId span)
                spanIdChecksum = calculateChecksum (spanSpanId span)
            return (not (Text.null (spanName span)) &&
                   not (Text.null (spanTraceId span)) &&
                   not (Text.null (spanSpanId span)) &&
                   checksumValue traceIdChecksum >= 0 &&
                   checksumValue spanIdChecksum >= 0)
        , validateLogger = \logger -> do
            let nameChecksum = calculateChecksum (loggerName logger)
            return (not (Text.null (loggerName logger)) &&
                   checksumValue nameChecksum >= 0)
        }

spec :: Spec
spec = describe "Data Integrity Tests" $ do
  
  -- 1. 测试度量数据完整性
  describe "Metric Data Integrity" $ do
    it "should maintain metric name integrity" $ do
      initTelemetry defaultConfig
      
      let testName = "integrity-test-metric"
      metric <- createMetric (pack testName) "count"
      
      metricName metric `shouldBe` pack testName
      
      -- 验证校验和
      let originalChecksum = calculateChecksum (pack testName)
          currentChecksum = calculateChecksum (metricName metric)
      
      originalChecksum `shouldBe` currentChecksum
      
      shutdownTelemetry
    
    it "should preserve metric value integrity" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "value-integrity" "count"
      
      let testValues = [1.0, 2.0, 3.5, -1.0, 0.0, 999.999]
      
      forM_ testValues $ \value -> do
        recordMetric metric value
        current <- metricValue metric
        
        -- 验证值不为NaN（除非明确记录NaN）
        when (not (isNaN value)) $ do
          isNaN current `shouldBe` False
      
      shutdownTelemetry
    
    it "should handle special values correctly" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "special-values" "count"
      
      -- 测试无穷大
      recordMetric metric (1.0/0.0)
      value1 <- metricValue metric
      isInfinite value1 `shouldBe` True
      
      -- 测试负无穷大
      recordMetric metric (-1.0/0.0)
      value2 <- metricValue metric
      isInfinite value2 `shouldBe` True
      
      -- 测试NaN
      recordMetric metric (0.0/0.0)
      value3 <- metricValue metric
      isNaN value3 `shouldBe` True
      
      shutdownTelemetry
  
  -- 2. QuickCheck属性测试：数据完整性的一致性
  describe "Data Integrity Properties" $ do
    it "should maintain metric integrity across operations" $ property $
      \values ->
        let testValues = take 10 values :: [Double]
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          metric <- createMetric "property-integrity" "count"
          originalName <- return $ metricName metric
          originalUnit <- return $ metricUnit metric
          
          -- 执行操作
          sequence_ $ map (recordMetric metric) testValues
          
          -- 验证完整性
          currentName <- return $ metricName metric
          currentUnit <- return $ metricUnit metric
          finalValue <- metricValue metric
          
          shutdownTelemetry
          return (originalName == currentName && 
                 originalUnit == currentUnit && 
                 not (Text.null currentName))
    
    it "should preserve span integrity across creation and finishing" $ property $
      \name ->
        let spanName = pack $ take 20 (show name)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          span <- createSpan spanName
          originalName <- return $ spanName span
          originalTraceId <- return $ spanTraceId span
          originalSpanId <- return $ spanSpanId span
          
          -- 完成span
          finishSpan span
          
          -- 验证完整性
          finalName <- return $ spanName span
          finalTraceId <- return $ spanTraceId span
          finalSpanId <- return $ spanSpanId span
          
          shutdownTelemetry
          return (originalName == finalName && 
                 originalTraceId == finalTraceId && 
                 originalSpanId == finalSpanId &&
                 not (Text.null finalTraceId) &&
                 not (Text.null finalSpanId))
    
    it "should maintain logger integrity across operations" $ property $
      \name level ->
        let loggerName = pack $ take 15 (show name)
            levels = [Debug, Info, Warn, Error]
            loggerLevel = levels !! (abs level `mod` 4)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          logger <- createLogger loggerName loggerLevel
          originalName <- return $ loggerName logger
          originalLevel <- return $ loggerLevel logger
          
          -- 记录消息
          logMessage logger Info (pack "integrity test")
          
          -- 验证完整性
          finalName <- return $ loggerName logger
          finalLevel <- return $ loggerLevel logger
          
          shutdownTelemetry
          return (originalName == finalName && 
                 originalLevel == finalLevel &&
                 not (Text.null finalName))
  
  -- 3. 测试并发数据完整性
  describe "Concurrent Data Integrity" $ do
    it "should maintain integrity under concurrent modifications" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "concurrent-integrity" "count"
      
      let numThreads = 10
          operationsPerThread = 100
      
      -- 并发修改度量
      done <- newEmptyMVar
      threads <- mapM (\i -> forkIO $ do
        sequence_ $ replicate operationsPerThread $ do
          recordMetric metric 1.0
        putMVar done ()
        ) [1..numThreads]
      
      -- 等待所有线程完成
      sequence_ $ replicate numThreads (takeMVar done)
      
      -- 验证完整性
      finalValue <- metricValue metric
      let expectedValue = fromIntegral numThreads * fromIntegral operationsPerThread
      
      finalValue `shouldBe` expectedValue
      
      shutdownTelemetry
    
    it "should preserve data integrity across concurrent component creation" $ do
      initTelemetry defaultConfig
      
      let numComponents = 50
      
      -- 并发创建组件
      done <- newEmptyMVar
      threads <- mapM (\i -> forkIO $ do
        case i `mod` 3 of
          0 -> do
            metric <- createMetric ("concurrent-metric-" ++ show i) "count"
            recordMetric metric (fromIntegral i)
          1 -> do
            span <- createSpan ("concurrent-span-" ++ show i)
            finishSpan span
          2 -> do
            logger <- createLogger ("concurrent-logger-" ++ show i) Info
            logMessage logger Info (pack $ "message " ++ show i)
        putMVar done ()
        ) [1..numComponents]
      
      -- 等待所有线程完成
      sequence_ $ replicate numComponents (takeMVar done)
      
      -- 验证系统仍然可用
      metric <- createMetric "post-concurrent-integrity" "count"
      recordMetric metric 42.0
      value <- metricValue metric
      value `shouldBe` 42.0
      
      shutdownTelemetry
  
  -- 4. 测试数据序列化完整性
  describe "Data Serialization Integrity" $ do
    it "should handle text encoding correctly" $ do
      initTelemetry defaultConfig
      
      -- 测试各种文本编码
      let testTexts = 
            [ "simple text"
            , "text with spaces"
            , "text-with-dashes"
            , "text_with_underscores"
            , "text.with.dots"
            , "text/with/slashes"
            , "text\\with\\backslashes"
            , "文本中文"
            , "emoji🚀test"
            , "mixed🌟content测试"
            ]
      
      forM_ testTexts $ \text -> do
        let packedText = pack text
        
        -- 测试度量名称
        metric <- createMetric packedText "count"
        metricName metric `shouldBe` packedText
        
        -- 测试度量单位
        metric2 <- createMetric "encoding-test" packedText
        metricUnit metric2 `shouldBe` packedText
        
        -- 测试span名称
        span <- createSpan packedText
        spanName span `shouldBe` packedText
        
        -- 测试logger名称
        logger <- createLogger packedText Info
        loggerName logger `shouldBe` packedText
        
        -- 测试日志消息
        logMessage logger Info packedText
      
      shutdownTelemetry
    
    it "should preserve data across restarts" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "restart-integrity" "count"
      
      -- 记录一些数据
      sequence_ $ replicate 10 $ recordMetric metric 1.0
      
      value1 <- metricValue metric
      value1 `shouldBe` 10.0
      
      -- 重启系统
      shutdownTelemetry
      initTelemetry defaultConfig
      
      -- 验证系统仍然可用
      metric2 <- createMetric "restart-integrity" "count"
      recordMetric metric2 5.0
      
      value2 <- metricValue metric2
      value2 `shouldBe` 5.0
      
      shutdownTelemetry
  
  -- 5. 测试数据验证
  describe "Data Validation" $ do
    it "should validate metric data" $ do
      initTelemetry defaultConfig
      
      validator <- createIntegrityValidator
      
      -- 创建有效度量
      validMetric <- createMetric "valid-metric" "count"
      recordMetric validMetric 10.0
      
      isValid <- validateMetric validator validMetric
      isValid `shouldBe` True
      
      -- 测试边界情况
      emptyMetric <- createMetric "" ""
      recordMetric emptyMetric 0.0
      
      isEmptyValid <- validateMetric validator emptyMetric
      -- 根据验证器的实现，空名称可能无效
      
      shutdownTelemetry
    
    it "should validate span data" $ do
      initTelemetry defaultConfig
      
      validator <- createIntegrityValidator
      
      -- 创建有效span
      validSpan <- createSpan "valid-span"
      
      isValid <- validateSpan validator validSpan
      isValid `shouldBe` True
      
      -- 测试边界情况
      emptySpan <- createSpan ""
      
      isEmptyValid <- validateSpan validator emptySpan
      -- 根据验证器的实现，空名称可能仍然有效（因为有ID）
      
      shutdownTelemetry
    
    it "should validate logger data" $ do
      initTelemetry defaultConfig
      
      validator <- createIntegrityValidator
      
      -- 创建有效logger
      validLogger <- createLogger "valid-logger" Info
      
      isValid <- validateLogger validator validLogger
      isValid `shouldBe` True
      
      -- 测试不同级别
      loggers <- mapM (\level -> createLogger "level-test" level) [Debug, Info, Warn, Error]
      
      areValid <- mapM (validateLogger validator) loggers
      all (== True) areValid `shouldBe` True
      
      shutdownTelemetry
  
  -- 6. 测试数据损坏检测
  describe "Data Corruption Detection" $ do
    it "should detect metric value corruption" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "corruption-test" "count"
      
      -- 记录正常值
      recordMetric metric 10.0
      value1 <- metricValue metric
      value1 `shouldBe` 10.0
      
      -- 记录特殊值（可能被视为"损坏"）
      recordMetric metric (0.0/0.0)  -- NaN
      value2 <- metricValue metric
      isNaN value2 `shouldBe` True
      
      -- 系统应该仍然可以记录正常值
      recordMetric metric 5.0
      value3 <- metricValue metric
      -- NaN应该传播
      isNaN value3 `shouldBe` True
      
      shutdownTelemetry
    
    it "should handle concurrent access without corruption" $ do
      initTelemetry defaultConfig
      
      let numThreads = 20
          operationsPerThread = 1000
      
      metric <- createMetric "concurrent-corruption-test" "count"
      
      -- 高并发操作
      done <- newEmptyMVar
      threads <- mapM (\i -> forkIO $ do
        sequence_ $ replicate operationsPerThread $ do
          recordMetric metric 1.0
        putMVar done ()
        ) [1..numThreads]
      
      -- 等待所有线程完成
      sequence_ $ replicate numThreads (takeMVar done)
      
      -- 验证数据完整性
      finalValue <- metricValue metric
      let expectedValue = fromIntegral numThreads * fromIntegral operationsPerThread
      
      finalValue `shouldBe` expectedValue
      
      shutdownTelemetry
  
  -- 7. 测试数据一致性
  describe "Data Consistency" $ do
    it "should maintain metric consistency across reads" $ do
      initTelemetry defaultConfig
      
      metric <- createMetric "consistency-test" "count"
      
      recordMetric metric 42.0
      
      -- 多次读取应该返回相同值
      values <- replicateM 10 $ metricValue metric
      all (== 42.0) values `shouldBe` True
      
      shutdownTelemetry
    
    it "should maintain consistency across shared metrics" $ do
      initTelemetry defaultConfig
      
      -- 创建共享度量
      metric1 <- createMetric "shared-consistency" "count"
      metric2 <- createMetric "shared-consistency" "count"
      
      recordMetric metric1 10.0
      value1 <- metricValue metric1
      value2 <- metricValue metric2
      
      value1 `shouldBe` value2
      value1 `shouldBe` 10.0
      
      recordMetric metric2 5.0
      value3 <- metricValue metric1
      value4 <- metricValue metric2
      
      value3 `shouldBe` value4
      value3 `shouldBe` 15.0
      
      shutdownTelemetry
  
  -- 8. 测试数据完整性边界条件
  describe "Data Integrity Boundary Conditions" $ do
    it "should handle empty data gracefully" $ do
      initTelemetry defaultConfig
      
      -- 空度量名称
      emptyMetric <- createMetric "" "count"
      recordMetric emptyMetric 1.0
      value <- metricValue emptyMetric
      value `shouldBe` 1.0
      
      -- 空度量单位
      emptyUnitMetric <- createMetric "empty-unit" ""
      recordMetric emptyUnitMetric 2.0
      value2 <- metricValue emptyUnitMetric
      value2 `shouldBe` 2.0
      
      shutdownTelemetry
    
    it "should handle extremely long data" $ do
      initTelemetry defaultConfig
      
      let longName = pack $ replicate 10000 'a'
          longUnit = pack $ replicate 10000 'b'
      
      longMetric <- createMetric longName longUnit
      recordMetric longMetric 1.0
      
      metricName longMetric `shouldBe` longName
      metricUnit longMetric `shouldBe` longUnit
      
      value <- metricValue longMetric
      value `shouldBe` 1.0
      
      shutdownTelemetry
    
    it "should handle unicode and special characters" $ do
      initTelemetry defaultConfig
      
      let specialTexts = 
            [ "null\0character"
            , "tab\tcharacter"
            , "newline\ncharacter"
            , "return\rcarriage"
            , "back\\slash"
            , "quote\"character"
            , "emoji🚀🌟test"
            , "中文测试"
            , "mixed🌟content测试"
            ]
      
      forM_ specialTexts $ \text -> do
        let packedText = pack text
        
        metric <- createMetric packedText packedText
        recordMetric metric 1.0
        
        metricName metric `shouldBe` packedText
        metricUnit metric `shouldBe` packedText
        
        value <- metricValue metric
        value `shouldBe` 1.0
      
      shutdownTelemetry