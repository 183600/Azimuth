{-# LANGUAGE OverloadedStrings #-}

import Azimuth.Telemetry
import Text.Printf

main :: IO ()
main = do
  putStrLn "=== Debug Test 1: Additive Property ==="
  
  -- Test case 1: x=1.0, y=0.0 (from test failure)
  metric1 <- createMetricWithInitialValue "test" "unit" 0.0
  recordMetric metric1 1.0
  recordMetric metric1 0.0
  result1 <- metricValue metric1
  putStrLn $ "After recording 1.0 then 0.0: " ++ show result1
  putStrLn $ "Expected (1.0 + 0.0): " ++ show (1.0 + 0.0)
  putStrLn $ "Difference: " ++ show (abs (result1 - (1.0 + 0.0)))
  putStrLn $ "Test passes: " ++ show (abs (result1 - (1.0 + 0.0)) < 1.0e-9)
  
  putStrLn "\n=== Debug Test 2: Extreme Values ==="
  
  -- Test case 2: extreme values
  metric2 <- createMetric "extreme-values" "test"
  
  -- Test very large positive number
  putStrLn $ "Recording 1.0e308..."
  recordMetric metric2 1.0e308
  value1 <- metricValue metric2
  putStrLn $ "Value after recording 1.0e308: " ++ show value1
  putStrLn $ "Is value1 > 1.0e307: " ++ show (value1 > 1.0e307)
  
  -- Test very small negative number
  putStrLn $ "\nRecording -1.0e308..."
  recordMetric metric2 (-1.0e308)
  value2 <- metricValue metric2
  putStrLn $ "Value after recording -1.0e308: " ++ show value2
  putStrLn $ "Is value2 < -1.0e307: " ++ show (value2 < -1.0e307)
  
  -- Test very small positive number
  putStrLn $ "\nRecording 1.0e-308..."
  recordMetric metric2 1.0e-308
  value3 <- metricValue metric2
  putStrLn $ "Value after recording 1.0e-308: " ++ show value3
  putStrLn $ "Is value3 > 0.0: " ++ show (value3 > 0.0)
  
  putStrLn "\n=== Additional Debug Info ==="
  putStrLn $ "1.0/0.0 = " ++ show (1.0/0.0)
  putStrLn $ "isInfinite (1.0e308) = " ++ show (isInfinite 1.0e308)
  putStrLn $ "isNaN (1.0e308) = " ++ show (isNaN 1.0e308)