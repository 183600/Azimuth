import Azimuth.Telemetry
import System.IO.Unsafe

main :: IO ()
main = do
  -- Test with empty strings
  metric <- createMetricWithInitialValue "" "" 0.0
  recordMetric metric 1.0
  value <- metricValue metric
  putStrLn $ "Metric name: " ++ show (metricName metric)
  putStrLn $ "Metric unit: " ++ show (metricUnit metric)
  putStrLn $ "Metric value: " ++ show value
  
  -- Test with non-empty strings
  metric2 <- createMetricWithInitialValue "test" "count" 0.0
  recordMetric metric2 2.0
  value2 <- metricValue metric2
  putStrLn $ "Metric2 name: " ++ show (metricName metric2)
  putStrLn $ "Metric2 unit: " ++ show (metricUnit metric2)
  putStrLn $ "Metric2 value: " ++ show value2