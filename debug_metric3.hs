{-# LANGUAGE OverloadedStrings #-}

import Azimuth.Telemetry
import System.IO.Unsafe
import Data.Text (Text)

-- Test empty string handling
testEmptyStrings :: IO Bool
testEmptyStrings = do
  let metric = unsafePerformIO $ createMetricWithInitialValue "" "" 0.0
      _ = unsafePerformIO $ recordMetric metric 1.0
      actualValue = unsafePerformIO $ metricValue metric
  return $ metricName metric == "" && metricUnit metric == "" && actualValue == 1.0

-- Test simple metric values
testSimpleMetric :: IO Bool
testSimpleMetric = do
  let metric = unsafePerformIO $ createMetricWithInitialValue "test" "count" 0.0
      _ = unsafePerformIO $ recordMetric metric 1.0
      actualValue = unsafePerformIO $ metricValue metric
  return $ metricName metric == "test" && metricUnit metric == "count" && actualValue == 1.0

main :: IO ()
main = do
  result1 <- testEmptyStrings
  putStrLn $ "Empty string test: " ++ show result1
  
  result2 <- testSimpleMetric
  putStrLn $ "Simple metric test: " ++ show result2