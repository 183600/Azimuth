{-# LANGUAGE OverloadedStrings #-}

import Azimuth.Telemetry
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

main :: IO ()
main = do
  -- Enable test mode like the actual test does
  writeIORef testMode True
  
  putStrLn "=== Debug Test 6: Additive Property Test ==="
  
  -- Test with the values that cause failure: 0.0 and 0.1
  let x = 0.0
      y = 0.1
  
  putStrLn $ "Testing with x = " ++ show x ++ ", y = " ++ show y
  
  -- Simulate the test using IO operations
  metric <- createMetricWithInitialValue "test" "unit" 0.0
  recordMetric metric x
  recordMetric metric y
  result <- metricValue metric
  
  putStrLn $ "Result after recording " ++ show x ++ " then " ++ show y ++ ": " ++ show result
  putStrLn $ "Expected (x + y): " ++ show (x + y)
  putStrLn $ "Difference: " ++ show (abs (result - (x + y)))
  putStrLn $ "Test passes: " ++ show (abs (result - (x + y)) < 1.0e-9)
  
  -- Also test with truly positive values
  putStrLn "\n--- Testing with truly positive values (1.0 and 2.0) ---"
  let x2 = 1.0
      y2 = 2.0
  
  metric2 <- createMetricWithInitialValue "test2" "unit" 0.0
  recordMetric metric2 x2
  recordMetric metric2 y2
  result2 <- metricValue metric2
  
  putStrLn $ "Result after recording " ++ show x2 ++ " then " ++ show y2 ++ ": " ++ show result2
  putStrLn $ "Expected (x + y): " ++ show (x2 + y2)
  putStrLn $ "Difference: " ++ show (abs (result2 - (x2 + y2)))
  putStrLn $ "Test passes: " ++ show (abs (result2 - (x2 + y2)) < 1.0e-9)