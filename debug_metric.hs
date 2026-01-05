import Azimuth.Telemetry
import Data.Text (pack)

main :: IO ()
main = do
  putStrLn "Testing metric recording..."
  metric <- createMetricWithInitialValue (pack "test-metric") (pack "count") 0.0
  putStrLn $ "Initial value: " ++ show (unsafeMetricValue metric)
  
  recordMetric metric 0.0
  putStrLn $ "After recording 0.0: " ++ show (unsafeMetricValue metric)
  
  metric2 <- createMetricWithInitialValue (pack "test-metric2") (pack "count") 0.0
  recordMetric metric2 0.0
  putStrLn $ "Metric2 after recording 0.0: " ++ show (unsafeMetricValue metric2)