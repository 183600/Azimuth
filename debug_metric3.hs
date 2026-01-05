import Azimuth.Telemetry
import Data.Text (pack)
import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
  putStrLn "Testing with global state changes..."
  
  -- 初始化遥测系统
  initTelemetry defaultConfig
  
  -- 测试1: should maintain metric value ordering
  putStrLn "\nTest 1: should maintain metric value ordering with [0.0]"
  let values = [0.0] :: [Double]
  let sortedValues = sort values
  
  metric1 <- createMetricWithInitialValue (pack "ordering-test") (pack "count") 0.0
  sequence_ $ map (\v -> recordMetric metric1 v) sortedValues
  finalValue1 <- metricValue metric1
  let expectedValue1 = sum sortedValues
  
  putStrLn $ "Values: " ++ show values
  putStrLn $ "Expected value: " ++ show expectedValue1
  putStrLn $ "Actual value: " ++ show finalValue1
  putStrLn $ "Test passed: " ++ show (finalValue1 == expectedValue1)
  
  -- 关闭并重新初始化遥测系统
  shutdownTelemetry
  initTelemetry defaultConfig
  
  -- 测试2: should handle metric value commutativity
  putStrLn "\nTest 2: should handle metric value commutativity with (0.1, 0.0)"
  let v1 = 0.1
  let v2 = 0.0
  
  metric2 <- createMetricWithInitialValue (pack "commutative-test-1") (pack "count") 0.0
  metric3 <- createMetricWithInitialValue (pack "commutative-test-2") (pack "count") 0.0
  
  recordMetric metric2 v1
  recordMetric metric2 v2
  recordMetric metric3 v2
  recordMetric metric3 v1
  
  val2 <- metricValue metric2
  val3 <- metricValue metric3
  
  let normalized2 = if val2 == 0.0 then 0.0 else val2
  let normalized3 = if val3 == 0.0 then 0.0 else val3
  
  putStrLn $ "v1 = " ++ show v1 ++ ", v2 = " ++ show v2
  putStrLn $ "Metric2 (v1 then v2): " ++ show val2
  putStrLn $ "Metric3 (v2 then v1): " ++ show val3
  putStrLn $ "Test passed: " ++ show (normalized2 == normalized3)
  
  shutdownTelemetry
