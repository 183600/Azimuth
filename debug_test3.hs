{-# LANGUAGE OverloadedStrings #-}

import Azimuth.Telemetry
import Text.Printf
import Control.Monad
import System.IO.Unsafe
import Data.IORef

main :: IO ()
main = do
  -- Set test mode
  writeIORef testMode True
  
  putStrLn "=== Debug Test 3: Understanding Test Expectations ==="
  
  -- Test case 1: Simple additive test
  putStrLn "\n--- Test 1: Simple additive test ---"
  metric1 <- createMetricWithInitialValue "test" "unit" 0.0
  recordMetric metric1 1.0
  recordMetric metric1 0.0
  result1 <- metricValue metric1
  putStrLn $ "After recording 1.0 then 0.0: " ++ show result1
  putStrLn $ "Expected (1.0 + 0.0): " ++ show (1.0 + 0.0)
  putStrLn $ "Difference: " ++ show (abs (result1 - (1.0 + 0.0)))
  putStrLn $ "Test passes: " ++ show (abs (result1 - (1.0 + 0.0)) < 1.0e-9)
  
  -- Test case 2: What if we try a different approach?
  putStrLn "\n--- Test 2: Alternative approach ---"
  metric2 <- createMetric "extreme-values" "test"
  
  -- Test very large positive number
  putStrLn $ "Recording 1.0e308..."
  recordMetric metric2 1.0e308
  value1 <- metricValue metric2
  putStrLn $ "Value after recording 1.0e308: " ++ show value1
  putStrLn $ "Is value1 > 1.0e307: " ++ show (value1 > 1.0e307)
  
  -- What if we reset before recording the negative value?
  putStrLn "\nResetting metric to 0.0..."
  writeIORef (metricValueRef metric2) 0.0
  
  putStrLn $ "Recording -1.0e308 after reset..."
  recordMetric metric2 (-1.0e308)
  value2 <- metricValue metric2
  putStrLn $ "Value after recording -1.0e308: " ++ show value2
  putStrLn $ "Is value2 < -1.0e307: " ++ show (value2 < -1.0e307)
  
  -- Test case 3: What if we don't allow cancellation?
  putStrLn "\n--- Test 3: Without cancellation ---"
  metric3 <- createMetric "extreme-values-2" "test"
  
  putStrLn $ "Recording 1.0e308..."
  recordMetric metric3 1.0e308
  val1 <- metricValue metric3
  putStrLn $ "Value after recording 1.0e308: " ++ show val1
  
  putStrLn $ "Recording -1.0e308 (no cancellation)..."
  -- Manually set to -1.0e308 to simulate no cancellation
  writeIORef (metricValueRef metric3) (-1.0e308)
  val2 <- metricValue metric3
  putStrLn $ "Value after recording -1.0e308: " ++ show val2
  putStrLn $ "Is val2 < -1.0e307: " ++ show (val2 < -1.0e307)
  
  putStrLn $ "Recording 1.0e-308..."
  recordMetric metric3 1.0e-308
  val3 <- metricValue metric3
  putStrLn $ "Value after recording 1.0e-308: " ++ show val3
  putStrLn $ "Is val3 > 0.0: " ++ show (val3 > 0.0)