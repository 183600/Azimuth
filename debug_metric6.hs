{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Azimuth.Telemetry

main :: IO ()
main = do
  -- Test with empty strings
  metric <- createMetricWithInitialValue "" "" 0.0
  recordMetric metric 1.0
  value <- metricValue metric
  putStrLn $ "Metric value: " ++ show value
  putStrLn $ "Is NaN: " ++ show (isNaN value)