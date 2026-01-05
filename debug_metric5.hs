import Azimuth.Telemetry
import Data.Text (pack)
import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
  putStrLn "Testing metric creation and initial values..."
  
  -- 测试createMetricWithInitialValue是否正确设置初始值
  metric1 <- createMetricWithInitialValue (pack "test1") (pack "count") 0.0
  val1 <- metricValue metric1
  putStrLn $ "Metric1 initial value: " ++ show val1
  
  -- 测试createMetric是否正确设置初始值为0.0
  metric2 <- createMetric (pack "test2") (pack "count")
  val2 <- metricValue metric2
  putStrLn $ "Metric2 initial value: " ++ show val2
  
  -- 测试createMetricWithInitialValue是否正确设置非零初始值
  metric3 <- createMetricWithInitialValue (pack "test3") (pack "count") 2.0
  val3 <- metricValue metric3
  putStrLn $ "Metric3 initial value: " ++ show val3
  
  -- 测试记录0.0是否会改变值
  recordMetric metric1 0.0
  val1' <- metricValue metric1
  putStrLn $ "Metric1 after recording 0.0: " ++ show val1'
  
  -- 测试记录0.0到非零初始值
  recordMetric metric3 0.0
  val3' <- metricValue metric3
  putStrLn $ "Metric3 after recording 0.0: " ++ show val3'
  
  -- 测试创建多个相同名称的metric
  metric4 <- createMetricWithInitialValue (pack "test1") (pack "count") 0.0
  val4 <- metricValue metric4
  putStrLn $ "Metric4 (same name as metric1) initial value: " ++ show val4
  
  -- 测试记录值到metric4是否会影响metric1
  recordMetric metric4 1.0
  val4' <- metricValue metric4
  val1'' <- metricValue metric1
  putStrLn $ "Metric4 after recording 1.0: " ++ show val4'
  putStrLn $ "Metric1 after metric4 recorded 1.0: " ++ show val1''