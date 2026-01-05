// Azimuth Telemetry System - Time Series Data Processing Tests
// This file contains test cases for time series data processing functionality

// Test 1: Basic Time Series Operations
test "basic time series operations" {
  // Create time series
  let ts = TimeSeries::new("cpu_usage")
  
  // Add data points
  TimeSeries::add_point(ts, 1000L, 25.5)
  TimeSeries::add_point(ts, 2000L, 30.2)
  TimeSeries::add_point(ts, 3000L, 28.7)
  TimeSeries::add_point(ts, 4000L, 32.1)
  TimeSeries::add_point(ts, 5000L, 29.8)
  
  // Test time series properties
  assert_eq(TimeSeries::name(ts), "cpu_usage")
  assert_eq(TimeSeries::length(ts), 5)
  assert_eq(TimeSeries::start_time(ts), 1000L)
  assert_eq(TimeSeries::end_time(ts), 5000L)
  
  // Test point retrieval
  let point = TimeSeries::get_point(ts, 3000L)
  match point {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), 28.7)
    None => assert_true(false)
  }
  
  // Test non-existent point
  let non_existent = TimeSeries::get_point(ts, 9999L)
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Time Series Aggregation
test "time series aggregation" {
  // Create time series with data
  let ts = TimeSeries::new("memory_usage")
  TimeSeries::add_point(ts, 1000L, 100.0)
  TimeSeries::add_point(ts, 2000L, 120.0)
  TimeSeries::add_point(ts, 3000L, 110.0)
  TimeSeries::add_point(ts, 4000L, 130.0)
  TimeSeries::add_point(ts, 5000L, 115.0)
  TimeSeries::add_point(ts, 6000L, 125.0)
  
  // Test sum aggregation
  let sum = TimeSeries::aggregate(ts, 1000L, 6000L, "sum")
  assert_eq(sum, 700.0)
  
  // Test average aggregation
  let avg = TimeSeries::aggregate(ts, 1000L, 6000L, "avg")
  assert_eq(avg, 700.0 / 6.0)
  
  // Test min aggregation
  let min = TimeSeries::aggregate(ts, 1000L, 6000L, "min")
  assert_eq(min, 100.0)
  
  // Test max aggregation
  let max = TimeSeries::aggregate(ts, 1000L, 6000L, "max")
  assert_eq(max, 130.0)
  
  // Test count aggregation
  let count = TimeSeries::aggregate(ts, 1000L, 6000L, "count")
  assert_eq(count, 6.0)
  
  // Test aggregation with time range
  let partial_sum = TimeSeries::aggregate(ts, 2000L, 5000L, "sum")
  assert_eq(partial_sum, 120.0 + 110.0 + 130.0 + 115.0)
}

// Test 3: Time Series Resampling
test "time series resampling" {
  // Create time series with irregular intervals
  let ts = TimeSeries::new("temperature")
  TimeSeries::add_point(ts, 1000L, 20.5)
  TimeSeries::add_point(ts, 1500L, 21.0)
  TimeSeries::add_point(ts, 2500L, 22.5)
  TimeSeries::add_point(ts, 3000L, 23.0)
  TimeSeries::add_point(ts, 4500L, 21.5)
  TimeSeries::add_point(ts, 5500L, 20.0)
  
  // Test downsampling with averaging
  let downsampled = TimeSeries::resample(ts, 1000L, 6000L, 2000L, "avg")
  
  assert_eq(TimeSeries::length(downsampled), 3)
  
  // First bucket (1000-3000): average of 20.5, 21.0, 22.5, 23.0
  let first_point = TimeSeries::get_point(downsampled, 2000L)
  match first_point {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), (20.5 + 21.0 + 22.5 + 23.0) / 4.0)
    None => assert_true(false)
  }
  
  // Second bucket (3000-5000): average of 23.0, 21.5
  let second_point = TimeSeries::get_point(downsampled, 4000L)
  match second_point {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), (23.0 + 21.5) / 2.0)
    None => assert_true(false)
  }
  
  // Third bucket (5000-6000): only 20.0
  let third_point = TimeSeries::get_point(downsampled, 6000L)
  match third_point {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), 20.0)
    None => assert_true(false)
  }
  
  // Test upsampling with linear interpolation
  let upsampled = TimeSeries::resample(ts, 1000L, 6000L, 500L, "linear")
  
  assert_eq(TimeSeries::length(upsampled), 11)
  
  // Test interpolated values
  let interpolated_point = TimeSeries::get_point(upsampled, 1250L)
  match interpolated_point {
    Some(p) => {
      // Should be between 20.5 and 21.0
      let value = TimeSeriesPoint::value(p)
      assert_true(value >= 20.5 && value <= 21.0)
    }
    None => assert_true(false)
  }
}

// Test 4: Time Series Windowing
test "time series windowing" {
  // Create time series
  let ts = TimeSeries::new("network_traffic")
  for i in range(1, 11) {
    let timestamp = (i * 1000).to_long()
    let value = (i * 10).to_float()
    TimeSeries::add_point(ts, timestamp, value)
  }
  
  // Test sliding window
  let windows = TimeSeries::sliding_window(ts, 3000L) // 3-second windows
  
  assert_eq(windows.length(), 8) // 10 points - window_size + 1
  
  // Test first window (points at 1000, 2000, 3000)
  let first_window = windows[0]
  assert_eq(TimeSeries::length(first_window), 3)
  assert_eq(TimeSeries::start_time(first_window), 1000L)
  assert_eq(TimeSeries::end_time(first_window), 3000L)
  
  // Test last window (points at 8000, 9000, 10000)
  let last_window = windows[7]
  assert_eq(TimeSeries::length(last_window), 3)
  assert_eq(TimeSeries::start_time(last_window), 8000L)
  assert_eq(TimeSeries::end_time(last_window), 10000L)
  
  // Test tumbling window
  let tumbling_windows = TimeSeries::tumbling_window(ts, 3000L)
  
  assert_eq(tumbling_windows.length(), 4)
  
  // Test first tumbling window (points at 1000, 2000, 3000)
  let first_tumbling = tumbling_windows[0]
  assert_eq(TimeSeries::length(first_tumbling), 3)
  
  // Test second tumbling window (points at 4000, 5000, 6000)
  let second_tumbling = tumbling_windows[1]
  assert_eq(TimeSeries::length(second_tumbling), 3)
  assert_eq(TimeSeries::start_time(second_tumbling), 4000L)
  assert_eq(TimeSeries::end_time(second_tumbling), 6000L)
}

// Test 5: Time Series Smoothing
test "time series smoothing" {
  // Create time series with noisy data
  let ts = TimeSeries::new("noisy_signal")
  TimeSeries::add_point(ts, 1000L, 10.0)
  TimeSeries::add_point(ts, 2000L, 12.5)
  TimeSeries::add_point(ts, 3000L, 9.8)
  TimeSeries::add_point(ts, 4000L, 11.2)
  TimeSeries::add_point(ts, 5000L, 10.5)
  TimeSeries::add_point(ts, 6000L, 13.1)
  TimeSeries::add_point(ts, 7000L, 9.5)
  
  // Test moving average smoothing
  let smoothed = TimeSeries::moving_average(ts, 3)
  
  assert_eq(TimeSeries::length(smoothed), 7)
  
  // First point should be average of first 3 points
  let first_smoothed = TimeSeries::get_point(smoothed, 1000L)
  match first_smoothed {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), (10.0 + 12.5 + 9.8) / 3.0)
    None => assert_true(false)
  }
  
  // Middle point should be average of 3 points around it
  let middle_smoothed = TimeSeries::get_point(smoothed, 4000L)
  match middle_smoothed {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), (9.8 + 11.2 + 10.5) / 3.0)
    None => assert_true(false)
  }
  
  // Test exponential smoothing
  let exp_smoothed = TimeSeries::exponential_smoothing(ts, 0.3)
  
  assert_eq(TimeSeries::length(exp_smoothed), 7)
  
  // First point should be same as original
  let first_exp = TimeSeries::get_point(exp_smoothed, 1000L)
  match first_exp {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), 10.0)
    None => assert_true(false)
  }
  
  // Second point should be weighted average
  let second_exp = TimeSeries::get_point(exp_smoothed, 2000L)
  match second_exp {
    Some(p) => {
      // 0.3 * 12.5 + 0.7 * 10.0 = 10.75
      assert_eq(TimeSeriesPoint::value(p), 10.75)
    }
    None => assert_true(false)
  }
}

// Test 6: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Create time series with normal data and some anomalies
  let ts = TimeSeries::new("server_response_time")
  TimeSeries::add_point(ts, 1000L, 100.0)
  TimeSeries::add_point(ts, 2000L, 105.0)
  TimeSeries::add_point(ts, 3000L, 98.0)
  TimeSeries::add_point(ts, 4000L, 102.0)
  TimeSeries::add_point(ts, 5000L, 500.0) // Anomaly
  TimeSeries::add_point(ts, 6000L, 95.0)
  TimeSeries::add_point(ts, 7000L, 110.0)
  TimeSeries::add_point(ts, 8000L, 50.0)  // Anomaly
  TimeSeries::add_point(ts, 9000L, 103.0)
  TimeSeries::add_point(ts, 10000L, 97.0)
  
  // Test statistical anomaly detection (z-score method)
  let anomalies = TimeSeries::detect_anomalies(ts, "zscore", 2.0)
  
  assert_eq(anomalies.length(), 2)
  
  // Check first anomaly
  let first_anomaly = anomalies[0]
  assert_eq(TimeSeriesPoint::timestamp(first_anomaly), 5000L)
  assert_eq(TimeSeriesPoint::value(first_anomaly), 500.0)
  
  // Check second anomaly
  let second_anomaly = anomalies[1]
  assert_eq(TimeSeriesPoint::timestamp(second_anomaly), 8000L)
  assert_eq(TimeSeriesPoint::value(second_anomaly), 50.0)
  
  // Test IQR method for anomaly detection
  let iqr_anomalies = TimeSeries::detect_anomalies(ts, "iqr", 1.5)
  
  // Should detect the same anomalies
  assert_eq(iqr_anomalies.length(), 2)
}

// Test 7: Time Series Forecasting
test "time series forecasting" {
  // Create time series with trend
  let ts = TimeSeries::new("sales_data")
  TimeSeries::add_point(ts, 1000L, 100.0)
  TimeSeries::add_point(ts, 2000L, 110.0)
  TimeSeries::add_point(ts, 3000L, 120.0)
  TimeSeries::add_point(ts, 4000L, 130.0)
  TimeSeries::add_point(ts, 5000L, 140.0)
  
  // Test linear regression forecasting
  let forecast = TimeSeries::forecast(ts, "linear", 3)
  
  assert_eq(TimeSeries::length(forecast), 3)
  
  // First forecast point (6000L)
  let first_forecast = TimeSeries::get_point(forecast, 6000L)
  match first_forecast {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), 150.0)
    None => assert_true(false)
  }
  
  // Second forecast point (7000L)
  let second_forecast = TimeSeries::get_point(forecast, 7000L)
  match second_forecast {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), 160.0)
    None => assert_true(false)
  }
  
  // Third forecast point (8000L)
  let third_forecast = TimeSeries::get_point(forecast, 8000L)
  match third_forecast {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), 170.0)
    None => assert_true(false)
  }
  
  // Test moving average forecasting
  let ma_forecast = TimeSeries::forecast(ts, "ma", 2)
  
  assert_eq(TimeSeries::length(ma_forecast), 2)
  
  // First MA forecast (6000L) - average of last 2 points
  let first_ma = TimeSeries::get_point(ma_forecast, 6000L)
  match first_ma {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), (130.0 + 140.0) / 2.0)
    None => assert_true(false)
  }
}

// Test 8: Time Series Correlation
test "time series correlation" {
  // Create two correlated time series
  let ts1 = TimeSeries::new("temperature")
  TimeSeries::add_point(ts1, 1000L, 20.0)
  TimeSeries::add_point(ts1, 2000L, 22.0)
  TimeSeries::add_point(ts1, 3000L, 24.0)
  TimeSeries::add_point(ts1, 4000L, 26.0)
  TimeSeries::add_point(ts1, 5000L, 28.0)
  
  let ts2 = TimeSeries::new("ice_cream_sales")
  TimeSeries::add_point(ts2, 1000L, 100.0)
  TimeSeries::add_point(ts2, 2000L, 120.0)
  TimeSeries::add_point(ts2, 3000L, 140.0)
  TimeSeries::add_point(ts2, 4000L, 160.0)
  TimeSeries::add_point(ts2, 5000L, 180.0)
  
  // Test correlation calculation
  let correlation = TimeSeries::correlation(ts1, ts2)
  
  // Should be perfectly correlated (correlation = 1.0)
  assert_eq(correlation, 1.0)
  
  // Create negatively correlated time series
  let ts3 = TimeSeries::new("hot_drink_sales")
  TimeSeries::add_point(ts3, 1000L, 180.0)
  TimeSeries::add_point(ts3, 2000L, 160.0)
  TimeSeries::add_point(ts3, 3000L, 140.0)
  TimeSeries::add_point(ts3, 4000L, 120.0)
  TimeSeries::add_point(ts3, 5000L, 100.0)
  
  // Test negative correlation
  let neg_correlation = TimeSeries::correlation(ts1, ts3)
  
  // Should be perfectly negatively correlated (correlation = -1.0)
  assert_eq(neg_correlation, -1.0)
  
  // Create uncorrelated time series
  let ts4 = TimeSeries::new("random_data")
  TimeSeries::add_point(ts4, 1000L, 50.0)
  TimeSeries::add_point(ts4, 2000L, 30.0)
  TimeSeries::add_point(ts4, 3000L, 70.0)
  TimeSeries::add_point(ts4, 4000L, 40.0)
  TimeSeries::add_point(ts4, 5000L, 60.0)
  
  // Test low correlation
  let low_correlation = TimeSeries::correlation(ts1, ts4)
  
  // Should be low correlation (close to 0)
  assert_true(low_correlation >= -0.5 && low_correlation <= 0.5)
}

// Test 9: Time Series Seasonality Detection
test "time series seasonality detection" {
  // Create time series with daily seasonality
  let ts = TimeSeries::new("hourly_traffic")
  for day in range(1, 8) { // 7 days
    for hour in range(0, 24) { // 24 hours
      let timestamp = ((day - 1) * 24 + hour) * 3600000L // Convert to milliseconds
      let value = if hour >= 9 && hour <= 17 { 100.0 } else { 20.0 } // Business hours
      TimeSeries::add_point(ts, timestamp, value)
    }
  }
  
  // Test seasonality detection
  let seasonality = TimeSeries::detect_seasonality(ts)
  
  assert_true(seasonality.is_seasonal)
  assert_eq(seasonality.period, 24) // Daily pattern
  
  // Create time series without seasonality
  let non_seasonal_ts = TimeSeries::new("random_walk")
  let mut value = 50.0
  for i in range(1, 169) { // 169 points (7 days * 24 hours + 1)
    let timestamp = i * 3600000L
    value = value + (Random::float() - 0.5) * 10.0 // Random walk
    TimeSeries::add_point(non_seasonal_ts, timestamp, value)
  }
  
  // Test no seasonality detection
  let no_seasonality = TimeSeries::detect_seasonality(non_seasonal_ts)
  
  assert_false(no_seasonality.is_seasonal)
}

// Test 10: Time Series Compression
test "time series compression" {
  // Create time series with many points
  let ts = TimeSeries::new("high_frequency_data")
  for i in range(1, 1001) {
    let timestamp = i * 1000L
    let value = 100.0 + (i % 10).to_float() // Repeating pattern
    TimeSeries::add_point(ts, timestamp, value)
  }
  
  assert_eq(TimeSeries::length(ts), 1000)
  
  // Test delta compression
  let compressed = TimeSeries::compress(ts, "delta")
  
  assert_true(compressed.size < ts.size) // Compressed data should be smaller
  
  // Test decompression
  let decompressed = TimeSeries::decompress(compressed, "delta")
  
  assert_eq(TimeSeries::length(decompressed), 1000)
  
  // Verify data integrity
  for i in range(1, 1001) {
    let timestamp = i * 1000L
    let original_point = TimeSeries::get_point(ts, timestamp)
    let decompressed_point = TimeSeries::get_point(decompressed, timestamp)
    
    match (original_point, decompressed_point) {
      (Some(orig), Some(decomp)) => {
        assert_eq(TimeSeriesPoint::value(orig), TimeSeriesPoint::value(decomp))
      }
      _ => assert_true(false)
    }
  }
  
  // Test gorilla compression (for floating point time series)
  let gorilla_compressed = TimeSeries::compress(ts, "gorilla")
  
  assert_true(gorilla_compressed.size < ts.size)
  
  // Test decompression
  let gorilla_decompressed = TimeSeries::decompress(gorilla_compressed, "gorilla")
  
  assert_eq(TimeSeries::length(gorilla_decompressed), 1000)
  
  // Verify data integrity for gorilla compression
  for i in range(1, 1001) {
    let timestamp = i * 1000L
    let original_point = TimeSeries::get_point(ts, timestamp)
    let gorilla_decompressed_point = TimeSeries::get_point(gorilla_decompressed, timestamp)
    
    match (original_point, gorilla_decompressed_point) {
      (Some(orig), Some(decomp)) => {
        // Gorilla compression might have small precision differences
        let diff = (TimeSeriesPoint::value(orig) - TimeSeriesPoint::value(decomp)).abs()
        assert_true(diff < 0.0001)
      }
      _ => assert_true(false)
    }
  }
}