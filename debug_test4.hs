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
  
  putStrLn "=== Debug Test 4: Exact Test Simulation ==="
  
  -- Simulate the exact test scenario
  putStrLn "\n--- Simulating 'should handle extreme values' test ---"
  metric <- createMetric "extreme-values" "test"
  
  -- Test very large positive number
  putStrLn $ "Recording 1.0e308..."
  recordMetric metric 1.0e308
  value1 <- metricValue metric
  putStrLn $ "Value after recording 1.0e308: " ++ show value1
  putStrLn $ "Is value1 > 1.0e307: " ++ show (value1 > 1.0e307)
  
  -- Test very small negative number
  putStrLn $ "\nRecording -1.0e308..."
  recordMetric metric (-1.0e308)
  value2 <- metricValue metric
  putStrLn $ "Value after recording -1.0e308: " ++ show value2
  putStrLn $ "Is value2 < -1.0e307: " ++ show (value2 < -1.0e307)
  
  -- Test very small positive number
  putStrLn $ "\nRecording 1.0e-308..."
  recordMetric metric 1.0e-308
  value3 <- metricValue metric
  putStrLn $ "Value after recording 1.0e-308: " ++ show value3
  putStrLn $ "Is value3 > 0.0: " ++ show (value3 > 0.0)
  
  -- Check if the test passes
  let testPasses = value1 > 1.0e307 && value2 < -1.0e307 && value3 > 0.0
  putStrLn $ "\nTest passes: " ++ show testPasses
  
  -- Now let's try a different approach
  putStrLn "\n--- Alternative approach: Reset after extreme values ---"
  metric2 <- createMetric "extreme-values-2" "test"
  
  putStrLn $ "Recording 1.0e308..."
  recordMetric metric2 1.0e308
  val1 <- metricValue metric2
  putStrLn $ "Value after recording 1.0e308: " ++ show val1
  
  putStrLn $ "Recording -1.0e308..."
  recordMetric metric2 (-1.0e308)
  val2 <- metricValue metric2
  putStrLn $ "Value after recording -1.0e308: " ++ show val2
  
  -- Reset to 0 before recording the small value
  putStrLn $ "Resetting to 0.0..."
  writeIORef (metricValueRef metric2) 0.0
  
  putStrLn $ "Recording 1.0e-308 after reset..."
  recordMetric metric2 1.0e-308
  val3 <- metricValue metric2
  putStrLn $ "Value after recording 1.0e-308: " ++ show val3
  putStrLn $ "Is val3 > 0.0: " ++ show (val3 > 0.0)