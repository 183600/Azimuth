#!/usr/bin/env runhaskell

import Azimuth.Telemetry

main :: IO ()
main = do
    putStrLn "Testing basic telemetry functionality..."
    
    -- Test 1: Basic metric creation and recording
    putStrLn "Test 1: Basic metric creation and recording"
    initTelemetry defaultConfig
    metric1 <- createMetric "test-metric-1" "count"
    recordMetric metric1 1.0
    value1 <- metricValue metric1
    putStrLn $ "Metric 1 value: " ++ show value1
    if value1 == 1.0
        then putStrLn "✓ Test 1 passed"
        else putStrLn $ "✗ Test 1 failed: expected 1.0, got " ++ show value1
    
    -- Test 2: Multiple metrics with different values
    putStrLn "\nTest 2: Multiple metrics with different values"
    metric2 <- createMetric "test-metric-2" "count"
    recordMetric metric2 2.0
    value2 <- metricValue metric2
    putStrLn $ "Metric 2 value: " ++ show value2
    if value2 == 2.0
        then putStrLn "✓ Test 2 passed"
        else putStrLn $ "✗ Test 2 failed: expected 2.0, got " ++ show value2
    
    -- Test 3: Metric aggregation test
    putStrLn "\nTest 3: Metric aggregation test"
    metrics <- sequence $ replicate 10 $ createMetric "aggregation-test" "count"
    sequence_ $ zipWith recordMetric metrics [1..10]
    values <- mapM metricValue metrics
    putStrLn $ "Values: " ++ show values
    let expectedValues = [1..10]
    if values == expectedValues
        then putStrLn "✓ Test 3 passed"
        else putStrLn $ "✗ Test 3 failed: expected " ++ show expectedValues ++ ", got " ++ show values
    
    -- Test 4: Average calculation test
    putStrLn "\nTest 4: Average calculation test"
    let avg = sum values / fromIntegral (length values)
    putStrLn $ "Average: " ++ show avg
    if avg == 5.5
        then putStrLn "✓ Test 4 passed"
        else putStrLn $ "✗ Test 4 failed: expected 5.5, got " ++ show avg
    
    shutdownTelemetry
    putStrLn "\nAll tests completed."