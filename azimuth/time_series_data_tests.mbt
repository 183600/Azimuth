// Time Series Data Processing Tests
// This file contains test cases for time series data processing and analysis

// Test 1: Time series metrics collection and aggregation
pub test "time_series_metrics_collection_aggregation" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "time-series-metrics")
  
  // Create time series metrics
  let cpu_counter = azimuth::Meter::create_counter(meter, "cpu.usage.total", Some("Total CPU usage"), Some("percentage"))
  let memory_gauge = azimuth::Meter::create_gauge(meter, "memory.usage", Some("Current memory usage"), Some("bytes"))
  let request_histogram = azimuth::Meter::create_histogram(meter, "request.duration", Some("Request duration"), Some("ms"))
  
  // Simulate time series data collection over time
  let mut timestamps = []
  let mut cpu_values = []
  let mut memory_values = []
  let mut request_durations = []
  
  // Collect data points over 10 time intervals
  let mut i = 0
  while i < 10 {
    let timestamp = 1640995200 + (i * 60) // Every minute
    timestamps = timestamps + [timestamp]
    
    // Simulate CPU usage with some variation
    let cpu_usage = 50.0 + (10.0 * (i % 3).to_float()) + (5.0 * sin(i.to_float()))
    cpu_values = cpu_values + [cpu_usage]
    azimuth::Counter::add(cpu_counter, cpu_usage.to_int(), [("timestamp", timestamp.to_string())])
    
    // Simulate memory usage with gradual increase
    let memory_usage = 1024 * 1024 * 100 + (i * 1024 * 1024 * 10) // 100MB + 10MB per interval
    memory_values = memory_values + [memory_usage.to_float()]
    azimuth::Gauge::set(memory_gauge, memory_usage.to_float(), [("timestamp", timestamp.to_string())])
    
    // Simulate request durations with some outliers
    let base_duration = 100.0 + (20.0 * cos(i.to_float()))
    let request_duration = if i % 7 == 0 { base_duration * 3.0 } else { base_duration }
    request_durations = request_durations + [request_duration]
    azimuth::Histogram::record(request_histogram, request_duration, [("timestamp", timestamp.to_string())])
    
    i = i + 1
  }
  
  // Verify time series data collection
  assert_eq(length(timestamps), 10)
  assert_eq(length(cpu_values), 10)
  assert_eq(length(memory_values), 10)
  assert_eq(length(request_durations), 10)
  
  // Test aggregation functions
  let avg_cpu = azimuth::TimeSeries::average(cpu_values)
  let max_memory = azimuth::TimeSeries::max(memory_values)
  let min_request = azimuth::TimeSeries::min(request_durations)
  let sum_requests = azimuth::TimeSeries::sum(request_durations)
  
  // Verify aggregation results
  assert_true(avg_cpu > 50.0 && avg_cpu < 70.0)
  assert_true(max_memory > memory_values[0])
  assert_true(min_request < request_durations[0])
  assert_true(sum_requests > 0.0)
  
  // Test percentile calculations
  let p95_cpu = azimuth::TimeSeries::percentile(cpu_values, 95)
  let p50_memory = azimuth::TimeSeries::percentile(memory_values, 50)
  let p99_request = azimuth::TimeSeries::percentile(request_durations, 99)
  
  // Verify percentile calculations
  assert_true(p95_cpu > avg_cpu)
  assert_eq(p50_memory, memory_values[4]) // Median of 10 values
  assert_true(p99_request > p95_cpu)
}

// Test 2: Time series data windowing and sliding windows
pub test "time_series_windowing_sliding_windows" {
  // Create time series data
  let timestamps = [1640995200, 1640995260, 1640995320, 1640995380, 1640995440, 
                    1640995500, 1640995560, 1640995620, 1640995680, 1640995740]
  let values = [10.0, 15.0, 12.0, 18.0, 14.0, 20.0, 16.0, 22.0, 19.0, 25.0]
  
  // Test fixed windowing (window size = 3)
  let windows = azimuth::TimeSeries::fixed_window(timestamps, values, 3)
  
  // Verify window count
  assert_eq(length(windows), 3) // 10 values with window size 3 = 3 full windows (9 values) + 1 partial
  
  // Verify first window
  let first_window = windows[0]
  assert_eq(length(first_window.values), 3)
  assert_eq(first_window.values[0], 10.0)
  assert_eq(first_window.values[1], 15.0)
  assert_eq(first_window.values[2], 12.0)
  
  // Test sliding windowing (window size = 4, slide = 2)
  let sliding_windows = azimuth::TimeSeries::sliding_window(timestamps, values, 4, 2)
  
  // Verify sliding window count
  assert_eq(length(sliding_windows), 4) // (10-4)/2 + 1 = 4 windows
  
  // Verify first sliding window
  let first_sliding = sliding_windows[0]
  assert_eq(length(first_sliding.values), 4)
  assert_eq(first_sliding.values[0], 10.0)
  assert_eq(first_sliding.values[1], 15.0)
  assert_eq(first_sliding.values[2], 12.0)
  assert_eq(first_sliding.values[3], 18.0)
  
  // Verify second sliding window (slid by 2)
  let second_sliding = sliding_windows[1]
  assert_eq(length(second_sliding.values), 4)
  assert_eq(second_sliding.values[0], 12.0)
  assert_eq(second_sliding.values[1], 18.0)
  assert_eq(second_sliding.values[2], 14.0)
  assert_eq(second_sliding.values[3], 20.0)
  
  // Test time-based windowing (5-minute windows)
  let time_windows = azimuth::TimeSeries::time_window(timestamps, values, 300) // 5 minutes = 300 seconds
  
  // Verify time-based windows
  assert_eq(length(time_windows), 2) // 10 minutes of data with 5-minute windows = 2 windows
  
  // Verify first time window
  let first_time_window = time_windows[0]
  assert_eq(length(first_time_window.values), 5) // First 5 minutes
  assert_eq(first_time_window.values[0], 10.0)
  assert_eq(first_time_window.values[4], 14.0)
}

// Test 3: Time series data smoothing and trend analysis
pub test "time_series_smoothing_trend_analysis" {
  // Create time series with noise
  let base_values = [100.0, 102.0, 104.0, 106.0, 108.0, 110.0, 112.0, 114.0, 116.0, 118.0]
  let noise = [2.0, -1.5, 3.2, -2.8, 1.7, -1.2, 2.5, -3.1, 1.8, -2.2]
  
  // Add noise to base values
  let mut noisy_values = []
  let mut i = 0
  while i < length(base_values) {
    noisy_values = noisy_values + [base_values[i] + noise[i]]
    i = i + 1
  }
  
  // Test moving average smoothing
  let smoothed_ma = azimuth::TimeSeries::moving_average(noisy_values, 3)
  
  // Verify moving average reduces noise
  assert_eq(length(smoothed_ma), length(noisy_values) - 2) // Window size 3 reduces length by 2
  assert_true(abs(smoothed_ma[0] - ((noisy_values[0] + noisy_values[1] + noisy_values[2]) / 3.0)) < 0.001)
  
  // Test exponential smoothing
  let smoothed_exp = azimuth::TimeSeries::exponential_smoothing(noisy_values, 0.3)
  
  // Verify exponential smoothing
  assert_eq(length(smoothed_exp), length(noisy_values))
  assert_eq(smoothed_exp[0], noisy_values[0]) // First value remains the same
  
  // Test trend analysis
  let trend = azimuth::TimeSeries::linear_regression(base_values)
  
  // Verify trend is positive (increasing values)
  assert_true(trend.slope > 0.0)
  assert_true(trend.intercept < base_values[0])
  
  // Test trend prediction
  let predicted_values = azimuth::TimeSeries::predict(trend, 5) // Predict next 5 values
  
  // Verify predictions continue the trend
  assert_eq(length(predicted_values), 5)
  assert_true(predicted_values[0] > base_values[length(base_values) - 1])
  assert_true(predicted_values[4] > predicted_values[0])
  
  // Test seasonality detection
  let seasonal_data = [10.0, 20.0, 30.0, 20.0, 10.0, 20.0, 30.0, 20.0, 10.0, 20.0, 30.0, 20.0]
  let seasonality = azimuth::TimeSeries::detect_seasonality(seasonal_data, 4) // Look for period of 4
  
  // Verify seasonality detection
  assert_true(seasonality.is_seasonal)
  assert_eq(seasonality.period, 4)
}

// Test 4: Time series anomaly detection
pub test "time_series_anomaly_detection" {
  // Create time series with anomalies
  let normal_data = [100.0, 102.0, 98.0, 101.0, 99.0, 103.0, 97.0, 100.0, 98.0, 102.0]
  let data_with_anomalies = [100.0, 102.0, 98.0, 150.0, 99.0, 103.0, 50.0, 100.0, 98.0, 102.0] // 150 and 50 are anomalies
  
  // Test statistical anomaly detection (z-score)
  let anomalies_zscore = azimuth::TimeSeries::detect_anomalies_zscore(data_with_anomalies, 2.0) // 2 sigma threshold
  
  // Verify anomalies are detected
  assert_true(length(anomalies_zscore) > 0)
  assert_true(anomalies_zscore.contains(3)) // Index 3 (value 150) should be detected
  assert_true(anomalies_zscore.contains(6)) // Index 6 (value 50) should be detected
  
  // Test IQR-based anomaly detection
  let anomalies_iqr = azimuth::TimeSeries::detect_anomalies_iqr(data_with_anomalies, 1.5) // 1.5 * IQR threshold
  
  // Verify anomalies are detected
  assert_true(length(anomalies_iqr) > 0)
  
  // Test moving average-based anomaly detection
  let anomalies_ma = azimuth::TimeSeries::detect_anomalies_moving_average(data_with_anomalies, 3, 2.0)
  
  // Verify anomalies are detected
  assert_true(length(anomalies_ma) > 0)
  
  // Test anomaly severity calculation
  let anomaly_severity = azimuth::TimeSeries::calculate_anomaly_severity(data_with_anomalies, anomalies_zscore)
  
  // Verify severity calculation
  assert_eq(length(anomaly_severity), length(anomalies_zscore))
  for severity in anomaly_severity {
    assert_true(severity > 0.0)
  }
}

// Test 5: Time series data downsampling and upsampling
pub test "time_series_downsampling_upsampling" {
  // Create high-frequency time series data (every second)
  let high_freq_timestamps = [1640995200, 1640995201, 1640995202, 1640995203, 1640995204, 
                             1640995205, 1640995206, 1640995207, 1640995208, 1640995209]
  let high_freq_values = [10.0, 12.0, 11.0, 13.0, 15.0, 14.0, 16.0, 18.0, 17.0, 19.0]
  
  // Test downsampling with averaging (every 5 seconds)
  let downsampled = azimuth::TimeSeries::downsample(high_freq_timestamps, high_freq_values, 5, "average")
  
  // Verify downsampling
  assert_eq(length(downsampled.timestamps), 2) // 10 seconds / 5-second window = 2 windows
  assert_eq(length(downsampled.values), 2)
  assert_eq(downsampled.values[0], (10.0 + 12.0 + 11.0 + 13.0 + 15.0) / 5.0) // Average of first 5 values
  assert_eq(downsampled.values[1], (14.0 + 16.0 + 18.0 + 17.0 + 19.0) / 5.0) // Average of last 5 values
  
  // Test downsampling with max
  let downsampled_max = azimuth::TimeSeries::downsample(high_freq_timestamps, high_freq_values, 5, "max")
  
  // Verify max downsampling
  assert_eq(downsampled_max.values[0], 15.0) // Max of first 5 values
  assert_eq(downsampled_max.values[1], 19.0) // Max of last 5 values
  
  // Test downsampling with min
  let downsampled_min = azimuth::TimeSeries::downsample(high_freq_timestamps, high_freq_values, 5, "min")
  
  // Verify min downsampling
  assert_eq(downsampled_min.values[0], 10.0) // Min of first 5 values
  assert_eq(downsampled_min.values[1], 14.0) // Min of last 5 values
  
  // Test upsampling with linear interpolation
  let low_freq_timestamps = [1640995200, 1640995300, 1640995400] // Every 10 minutes
  let low_freq_values = [10.0, 20.0, 15.0]
  
  let upsampled = azimuth::TimeSeries::upsample(low_freq_timestamps, low_freq_values, 300) // Every 5 minutes
  
  // Verify upsampling
  assert_eq(length(upsampled.timestamps), 5) // 20 minutes / 5-minute interval = 5 points
  assert_eq(length(upsampled.values), 5)
  assert_eq(upsampled.values[0], 10.0) // First point remains the same
  assert_eq(upsampled.values[2], 20.0) // Middle point remains the same
  assert_eq(upsampled.values[4], 15.0) // Last point remains the same
  assert_eq(upsampled.values[1], 15.0) // Interpolated between 10 and 20
  assert_eq(upsampled.values[3], 17.5) // Interpolated between 20 and 15
}

// Test 6: Time series correlation and analysis
pub test "time_series_correlation_analysis" {
  // Create correlated time series
  let series1 = [10.0, 12.0, 14.0, 16.0, 18.0, 20.0, 22.0, 24.0, 26.0, 28.0]
  let series2 = [20.0, 24.0, 28.0, 32.0, 36.0, 40.0, 44.0, 48.0, 52.0, 56.0] // series2 = 2 * series1
  let series3 = [30.0, 25.0, 35.0, 20.0, 40.0, 15.0, 45.0, 10.0, 50.0, 5.0] // Not correlated with series1
  
  // Test correlation coefficient
  let corr_1_2 = azimuth::TimeSeries::correlation(series1, series2)
  let corr_1_3 = azimuth::TimeSeries::correlation(series1, series3)
  
  // Verify correlation
  assert_true(abs(corr_1_2 - 1.0) < 0.001) // Perfect positive correlation
  assert_true(abs(corr_1_3) < 0.5) // Low correlation
  
  // Test cross-correlation
  let lead_series = [10.0, 12.0, 14.0, 16.0, 18.0, 20.0, 22.0, 24.0, 26.0, 28.0]
  let lag_series = [0.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0, 22.0, 24.0, 26.0] // lag_series is lead_series shifted by 1
  
  let cross_corr = azimuth::TimeSeries::cross_correlation(lead_series, lag_series, 2) // Max lag of 2
  
  // Verify cross-correlation
  assert_true(length(cross_corr) == 5) // Lags from -2 to 2
  assert_true(cross_corr[2] > cross_corr[1]) // Lag 1 should have highest correlation
  assert_true(cross_corr[2] > cross_corr[3]) // Lag 1 should have highest correlation
  
  // Test autocorrelation
  let autocorr = azimuth::TimeSeries::autocorrelation(series1, 3) // Max lag of 3
  
  // Verify autocorrelation
  assert_true(length(autocorr) == 4) // Lags from 0 to 3
  assert_eq(autocorr[0], 1.0) // Autocorrelation at lag 0 is always 1
  assert_true(autocorr[1] < autocorr[0]) // Autocorrelation decreases with lag
  
  // Test cointegration
  let cointegrated1 = [10.0, 12.0, 14.0, 16.0, 18.0, 20.0, 22.0, 24.0, 26.0, 28.0]
  let cointegrated2 = [15.0, 17.0, 19.0, 21.0, 23.0, 25.0, 27.0, 29.0, 31.0, 33.0] // cointegrated2 = cointegrated1 + 5
  
  let is_cointegrated = azimuth::TimeSeries::test_cointegration(cointegrated1, cointegrated2)
  
  // Verify cointegration
  assert_true(is_cointegrated)
}