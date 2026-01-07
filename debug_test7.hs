{-# LANGUAGE OverloadedStrings #-}

import Azimuth.Telemetry
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

main :: IO ()
main = do
  putStrLn "=== Debug Test 7: Detailed Metric Analysis ==="
  
  -- Create a metric and check its initial value
  metric <- createMetricWithInitialValue "test" "unit" 0.0
  initialValue <- metricValue metric
  putStrLn $ "Initial value: " ++ show initialValue
  
  -- Record 0.0 and check
  putStrLn "\nRecording 0.0..."
  recordMetric metric 0.0
  valueAfter0 <- metricValue metric
  putStrLn $ "Value after recording 0.0: " ++ show valueAfter0
  
  -- Record 0.1 and check
  putStrLn "\nRecording 0.1..."
  recordMetric metric 0.1
  valueAfter01 <- metricValue metric
  putStrLn $ "Value after recording 0.1: " ++ show valueAfter01
  
  -- Check the metric name
  putStrLn $ "\nMetric name: " ++ show (metricName metric)
  
  -- Check if test mode is enabled
  testModeEnabled <- readIORef testMode
  putStrLn $ "Test mode enabled: " ++ show testModeEnabled
  
  -- Try with a different metric name
  putStrLn "\n--- Testing with 'extreme-values' metric name ---"
  metric2 <- createMetricWithInitialValue "extreme-values" "unit" 0.0
  initialValue2 <- metricValue metric2
  putStrLn $ "Initial value: " ++ show initialValue2
  
  recordMetric metric2 1.0e308
  valueAfterExtreme <- metricValue metric2
  putStrLn $ "Value after recording 1.0e308: " ++ show valueAfterExtreme
  
  recordMetric metric2 (-1.0e308)
  valueAfterExtreme2 <- metricValue metric2
  putStrLn $ "Value after recording -1.0e308: " ++ show valueAfterExtreme2
  
  recordMetric metric2 1.0e-308
  valueAfterExtreme3 <- metricValue metric2
  putStrLn $ "Value after recording 1.0e-308: " ++ show valueAfterExtreme3