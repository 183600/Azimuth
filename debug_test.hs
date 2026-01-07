#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Azimuth.Telemetry

main :: IO ()
main = do
    -- Test 1: Configuration hot update
    putStrLn "Test 1: Configuration hot update"
    let config1 = TelemetryConfig "test-service" "1.0.0" True True True False
        config2 = TelemetryConfig "test-service" "2.0.0" False True True False
    
    initTelemetry config1
    
    -- Create metric
    metric <- createMetric "hot-update-test" "count"
    recordMetric metric 1.0
    value1 <- metricValue metric
    putStrLn $ "After first config: " ++ show value1
    
    -- Update configuration
    initTelemetry config2
    
    -- Continue using metric
    recordMetric metric 2.0
    value2 <- metricValue metric
    putStrLn $ "After config update: " ++ show value2
    
    if value2 == 3.0
        then putStrLn "Test 1 PASSED"
        else putStrLn $ "Test 1 FAILED: expected 3.0, got " ++ show value2
    
    shutdownTelemetry
    
    -- Test 2: Metric sharing
    putStrLn "\nTest 2: Metric sharing"
    writeIORef enableMetricSharing True
    initTelemetry defaultConfig
    
    metric1 <- createMetric "shared-test" "count"
    recordMetric metric1 10.0
    
    metric2 <- createMetric "shared-test" "count"
    value <- metricValue metric2
    putStrLn $ "Shared metric value: " ++ show value
    
    if value == 10.0
        then putStrLn "Test 2 PASSED"
        else putStrLn $ "Test 2 FAILED: expected 10.0, got " ++ show value
    
    shutdownTelemetry