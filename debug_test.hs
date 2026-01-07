import System.IO.Unsafe
import Azimuth.Telemetry

main :: IO ()
main = do
    let metric = unsafePerformIO $ createMetricWithInitialValue "a" "a" 0.0
    unsafePerformIO $ recordMetric metric 0.0
    unsafePerformIO $ recordMetric metric 0.0
    unsafePerformIO $ recordMetric metric 1.0
    value <- unsafePerformIO $ metricValue metric
    putStrLn $ "Metric value: " ++ show value