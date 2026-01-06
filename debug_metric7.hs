{-# LANGUAGE OverloadedStrings #-}
import Azimuth.Telemetry
import Data.Text (pack)
import System.IO.Unsafe

main :: IO ()
main = do
  -- Disable metric sharing for test isolation
  writeIORef enableMetricSharing False
  
  -- Test with empty strings
  metric <- createMetricWithInitialValue "" "" 0.0
  putStrLn $ "Initial metric name: " ++ show (metricName metric)
  putStrLn $ "Initial metric unit: " ++ show (metricUnit metric)
  
  recordMetric metric 0.1
  value <- metricValue metric
  putStrLn $ "Metric value after recording 0.1: " ++ show value
  putStrLn $ "Is NaN: " ++ show (isNaN value)
  
  recordMetric metric 0.2
  value2 <- metricValue metric
  putStrLn $ "Metric value after recording 0.2: " ++ show value2
  putStrLn $ "Is NaN: " ++ show (isNaN value2)