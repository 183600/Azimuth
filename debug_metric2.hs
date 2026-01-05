import Azimuth.Telemetry
import Data.Text (pack)
import Data.List (sort)

main :: IO ()
main = do
  putStrLn "Testing metric ordering with [0.0]..."
  let values = [0.0] :: [Double]
  let sortedValues = sort values
  
  metric <- createMetricWithInitialValue (pack "ordering-test") (pack "count") 0.0
  sequence_ $ map (\v -> recordMetric metric v) sortedValues
  finalValue <- metricValue metric
  let expectedValue = sum sortedValues
  
  putStrLn $ "Values: " ++ show values
  putStrLn $ "Sorted values: " ++ show sortedValues
  putStrLn $ "Expected value: " ++ show expectedValue
  putStrLn $ "Actual value: " ++ show finalValue
  putStrLn $ "Test passed: " ++ show (finalValue == expectedValue)
  
  putStrLn "\nTesting metric commutativity with (0.1, 0.0)..."
  let v1 = 0.1
  let v2 = 0.0
  
  metric1 <- createMetricWithInitialValue (pack "commutative-test-1") (pack "count") 0.0
  metric2 <- createMetricWithInitialValue (pack "commutative-test-2") (pack "count") 0.0
  
  recordMetric metric1 v1
  recordMetric metric1 v2
  recordMetric metric2 v2
  recordMetric metric2 v1
  
  val1 <- metricValue metric1
  val2 <- metricValue metric2
  
  let normalized1 = if val1 == 0.0 then 0.0 else val1
  let normalized2 = if val2 == 0.0 then 0.0 else val2
  
  putStrLn $ "v1 = " ++ show v1 ++ ", v2 = " ++ show v2
  putStrLn $ "Metric1 (v1 then v2): " ++ show val1
  putStrLn $ "Metric2 (v2 then v1): " ++ show val2
  putStrLn $ "Normalized1: " ++ show normalized1
  putStrLn $ "Normalized2: " ++ show normalized2
  putStrLn $ "Test passed: " ++ show (normalized1 == normalized2)