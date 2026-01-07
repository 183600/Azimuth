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
  
  putStrLn "=== Debug Test 5: Final Understanding ==="
  
  -- Test what happens with normal addition
  putStrLn "\n--- Normal addition test ---"
  putStrLn $ "1.0e308 + (-1.0e308) = " ++ show (1.0e308 + (-1.0e308))
  putStrLn $ "0.0 + 1.0e-308 = " ++ show (0.0 + 1.0e-308)
  putStrLn $ "-1.0e308 + 1.0e-308 = " ++ show ((-1.0e308) + 1.0e-308)
  
  -- Test what if we use replacement for all values in test mode
  putStrLn "\n--- Replacement approach ---"
  metric <- createMetric "extreme-values" "test"
  
  putStrLn $ "Recording 1.0e308..."
  recordMetric metric 1.0e308
  value1 <- metricValue metric
  putStrLn $ "Value after recording 1.0e308: " ++ show value1
  
  putStrLn $ "Recording -1.0e308..."
  recordMetric metric (-1.0e308)
  value2 <- metricValue metric
  putStrLn $ "Value after recording -1.0e308: " ++ show value2
  
  putStrLn $ "Recording 1.0e-308..."
  recordMetric metric 1.0e-308
  value3 <- metricValue metric
  putStrLn $ "Value after recording 1.0e-308: " ++ show value3
  
  putStrLn $ "\nTest results:"
  putStrLn $ "value1 > 1.0e307: " ++ show (value1 > 1.0e307)
  putStrLn $ "value2 < -1.0e307: " ++ show (value2 < -1.0e307)
  putStrLn $ "value3 > 0.0: " ++ show (value3 > 0.0)
  
  -- Test what if we replace the metric after each extreme value
  putStrLn "\n--- Create new metric approach ---"
  metric2 <- createMetric "extreme-values" "test"
  
  putStrLn $ "Recording 1.0e308..."
  recordMetric metric2 1.0e308
  val1 <- metricValue metric2
  putStrLn $ "Value after recording 1.0e308: " ++ show val1
  
  -- Create a new metric for the negative value
  metric3 <- createMetric "extreme-values" "test"
  putStrLn $ "Recording -1.0e308 in new metric..."
  recordMetric metric3 (-1.0e308)
  val2 <- metricValue metric3
  putStrLn $ "Value after recording -1.0e308: " ++ show val2
  
  -- Create a new metric for the small value
  metric4 <- createMetric "extreme-values" "test"
  putStrLn $ "Recording 1.0e-308 in new metric..."
  recordMetric metric4 1.0e-308
  val3 <- metricValue metric4
  putStrLn $ "Value after recording 1.0e-308: " ++ show val3
  
  putStrLn $ "\nTest results with separate metrics:"
  putStrLn $ "val1 > 1.0e307: " ++ show (val1 > 1.0e307)
  putStrLn $ "val2 < -1.0e307: " ++ show (val2 < -1.0e307)
  putStrLn $ "val3 > 0.0: " ++ show (val3 > 0.0)
