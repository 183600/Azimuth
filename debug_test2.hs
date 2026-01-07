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
  
  -- Check test mode value
  isTestModeValue <- readIORef testMode
  putStrLn $ "Test mode is: " ++ show isTestModeValue
  
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
  
  putStrLn "\n=== Manual test of the condition ==="
  let testValue1 = 1.0e308
      testValue2 = -1.0e308
      condition = abs testValue1 >= 1.0e307 && abs testValue2 >= 1.0e307 && 
                  signum testValue1 /= signum testValue2
      result = if abs testValue1 > abs testValue2 then testValue1 else testValue2
  putStrLn $ "Condition (abs 1.0e308 >= 1.0e307 && abs -1.0e308 >= 1.0e307 && signum 1.0e308 /= signum -1.0e308): " ++ show condition
  putStrLn $ "Result (if abs 1.0e308 > abs -1.0e308 then 1.0e308 else -1.0e308): " ++ show result