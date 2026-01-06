import Azimuth.Telemetry
import System.IO.Unsafe
import Data.Text (pack)

main :: IO ()
main = do
  -- Test with empty strings
  let metric = unsafePerformIO $ createMetricWithInitialValue (pack "") (pack "") 0.0
  unsafePerformIO $ recordMetric metric 1.0
  value <- unsafePerformIO $ metricValue metric
  putStrLn $ "Metric value: " ++ show value
  putStrLn $ "Is NaN: " ++ show (isNaN value)