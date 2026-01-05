// Azimuth Telemetry System - Time Series Data Processing Quality Tests
// This file contains comprehensive test cases for time series data processing functionality

// Test 1: Basic Time Series Data Creation
test "basic time series data creation" {
  let time_series = TimeSeries::new("cpu_usage")
  
  // Test initial state
  assert_eq(TimeSeries::name(time_series), "cpu_usage")
  assert_eq(TimeSeries::size(time_series), 0)
  assert_true(TimeSeries::is_empty(time_series))
  
  // Add data points
  let base_time = 1234567890L
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time, 25.5))
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + 60L, 30.2))
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + 120L, 28.7))
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + 180L, 32.1))
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + 240L, 29.8))
  
  // Verify data points
  assert_eq(TimeSeries::size(time_series), 5)
  assert_false(TimeSeries::is_empty(time_series))
  
  // Get first and last points
  let first_point = TimeSeries::first_point(time_series)
  match first_point {
    Some(point) => {
      assert_eq(TimeSeriesPoint::timestamp(point), base_time)
      assert_eq(TimeSeriesPoint::value(point), 25.5)
    }
    None => assert_true(false)
  }
  
  let last_point = TimeSeries::last_point(time_series)
  match last_point {
    Some(point) => {
      assert_eq(TimeSeriesPoint::timestamp(point), base_time + 240L)
      assert_eq(TimeSeriesPoint::value(point), 29.8)
    }
    None => assert_true(false)
  }
  
  // Get point at specific index
  let middle_point = TimeSeries::point_at(time_series, 2)
  match middle_point {
    Some(point) => {
      assert_eq(TimeSeriesPoint::timestamp(point), base_time + 120L)
      assert_eq(TimeSeriesPoint::value(point), 28.7)
    }
    None => assert_true(false)
  }
  
  // Get point at specific timestamp
  let specific_point = TimeSeries::point_at_time(time_series, base_time + 180L)
  match specific_point {
    Some(point) => {
      assert_eq(TimeSeriesPoint::timestamp(point), base_time + 180L)
      assert_eq(TimeSeriesPoint::value(point), 32.1)
    }
    None => assert_true(false)
  }
}

// Test 2: Time Series Aggregation
test "time series aggregation" {
  let time_series = TimeSeries::new("response_time")
  
  // Add data points
  let base_time = 1234567890L
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time, 100.0))
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + 60L, 150.0))
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + 120L, 200.0))
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + 180L, 120.0))
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + 240L, 180.0))
  
  // Test average aggregation
  let avg_value = TimeSeries::aggregate(time_series, AggregationType::Average)
  assert_eq(avg_value, 150.0)
  
  // Test sum aggregation
  let sum_value = TimeSeries::aggregate(time_series, AggregationType::Sum)
  assert_eq(sum_value, 750.0)
  
  // Test min aggregation
  let min_value = TimeSeries::aggregate(time_series, AggregationType::Min)
  assert_eq(min_value, 100.0)
  
  // Test max aggregation
  let max_value = TimeSeries::aggregate(time_series, AggregationType::Max)
  assert_eq(max_value, 200.0)
  
  // Test count aggregation
  let count_value = TimeSeries::aggregate(time_series, AggregationType::Count)
  assert_eq(count_value, 5.0)
  
  // Test aggregation on empty series
  let empty_series = TimeSeries::new("empty")
  let empty_avg = TimeSeries::aggregate(empty_series, AggregationType::Average)
  assert_eq(empty_avg, 0.0)
}

// Test 3: Time Series Resampling
test "time series resampling" {
  let time_series = TimeSeries::new("memory_usage")
  
  // Add data points at 30-second intervals
  let base_time = 1234567890L
  for i in 0..=11 {
    let timestamp = base_time + (i * 30L)
    let value = 50.0 + (i * 2.5)  // Increasing values
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Original series has 12 points at 30-second intervals
  assert_eq(TimeSeries::size(time_series), 12)
  
  // Resample to 1-minute intervals using average
  let resampled_avg = TimeSeries::resample(time_series, 60L, AggregationType::Average)
  assert_eq(TimeSeries::size(resampled_avg), 6)
  
  // Verify resampled points
  for i in 0..=5 {
    let point = TimeSeries::point_at(resampled_avg, i)
    match point {
      Some(p) => {
        let expected_timestamp = base_time + (i * 60L)
        assert_eq(TimeSeriesPoint::timestamp(p), expected_timestamp)
        
        // Average of two consecutive 30-second points
        let expected_value = 50.0 + ((i * 2) * 2.5) + 1.25
        assert_eq(TimeSeriesPoint::value(p), expected_value)
      }
      None => assert_true(false)
    }
  }
  
  // Resample to 2-minute intervals using max
  let resampled_max = TimeSeries::resample(time_series, 120L, AggregationType::Max)
  assert_eq(TimeSeries::size(resampled_max), 3)
  
  // Verify resampled points
  for i in 0..=2 {
    let point = TimeSeries::point_at(resampled_max, i)
    match point {
      Some(p) => {
        let expected_timestamp = base_time + (i * 120L)
        assert_eq(TimeSeriesPoint::timestamp(p), expected_timestamp)
        
        // Max of four consecutive 30-second points
        let expected_value = 50.0 + ((i * 4 + 3) * 2.5)
        assert_eq(TimeSeriesPoint::value(p), expected_value)
      }
      None => assert_true(false)
    }
  }
}

// Test 4: Time Series Windowing
test "time series windowing" {
  let time_series = TimeSeries::new("network_throughput")
  
  // Add data points
  let base_time = 1234567890L
  for i in 0..=23 {
    let timestamp = base_time + (i * 60L)  // 1-minute intervals
    let value = 100.0 + (i * 10.0)  // Increasing values
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Test time-based windowing
  let window_start = base_time + (5 * 60L)  // 5 minutes in
  let window_end = base_time + (15 * 60L)   // 15 minutes in
  
  let windowed_series = TimeSeries::time_window(time_series, window_start, window_end)
  assert_eq(TimeSeries::size(windowed_series), 11)  // Points from 5 to 15 inclusive
  
  // Verify first and last points in window
  let first_window_point = TimeSeries::first_point(windowed_series)
  match first_window_point {
    Some(point) => {
      assert_eq(TimeSeriesPoint::timestamp(point), window_start)
      assert_eq(TimeSeriesPoint::value(point), 100.0 + (5 * 10.0))
    }
    None => assert_true(false)
  }
  
  let last_window_point = TimeSeries::last_point(windowed_series)
  match last_window_point {
    Some(point) => {
      assert_eq(TimeSeriesPoint::timestamp(point), window_end)
      assert_eq(TimeSeriesPoint::value(point), 100.0 + (15 * 10.0))
    }
    None => assert_true(false)
  }
  
  // Test sliding window with aggregation
  let sliding_windows = TimeSeries::sliding_window(time_series, 5, AggregationType::Average)
  assert_eq(sliding_windows.length(), 19)  // 24 - 5 + 1
  
  // Verify first sliding window
  let first_window = sliding_windows[0]
  assert_eq(TimeSeries::size(first_window), 5)
  let first_window_avg = TimeSeries::aggregate(first_window, AggregationType::Average)
  assert_eq(first_window_avg, 120.0)  // Average of first 5 points
  
  // Verify last sliding window
  let last_window = sliding_windows[18]
  assert_eq(TimeSeries::size(last_window), 5)
  let last_window_avg = TimeSeries::aggregate(last_window, AggregationType::Average)
  assert_eq(last_window_avg, 310.0)  // Average of last 5 points
}

// Test 5: Time Series Filtering
test "time series filtering" {
  let time_series = TimeSeries::new("error_rate")
  
  // Add data points with varying values
  let base_time = 1234567890L
  for i in 0..=19 {
    let timestamp = base_time + (i * 60L)
    let value = if i % 4 == 0 { 0.0 } else if i % 4 == 1 { 0.1 } else if i % 4 == 2 { 0.5 } else { 1.0 }
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Filter by value (greater than 0.5)
  let filtered_high = TimeSeries::filter_by_value(time_series, func(value) { value > 0.5 })
  assert_eq(TimeSeries::size(filtered_high), 5)  // Points with value 1.0
  
  // Verify all filtered points have value > 0.5
  for i in 0..=4 {
    let point = TimeSeries::point_at(filtered_high, i)
    match point {
      Some(p) => assert_true(TimeSeriesPoint::value(p) > 0.5)
      None => assert_true(false)
    }
  }
  
  // Filter by value (less than 0.5)
  let filtered_low = TimeSeries::filter_by_value(time_series, func(value) { value < 0.5 })
  assert_eq(TimeSeries::size(filtered_low), 10)  // Points with value 0.0 or 0.1
  
  // Filter by time (first 10 minutes)
  let time_filtered = TimeSeries::filter_by_time(time_series, base_time, base_time + (9 * 60L))
  assert_eq(TimeSeries::size(time_filtered), 10)
  
  // Verify time range
  let first_filtered = TimeSeries::first_point(time_filtered)
  match first_filtered {
    Some(point) => assert_eq(TimeSeriesPoint::timestamp(point), base_time)
    None => assert_true(false)
  }
  
  let last_filtered = TimeSeries::last_point(time_filtered)
  match last_filtered {
    Some(point) => assert_eq(TimeSeriesPoint::timestamp(point), base_time + (9 * 60L))
    None => assert_true(false)
  }
}

// Test 6: Time Series Interpolation
test "time series interpolation" {
  let time_series = TimeSeries::new("temperature")
  
  // Add sparse data points
  let base_time = 1234567890L
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time, 20.0))
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + 300L, 25.0))  // 5 minutes later
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + 600L, 15.0))  // 10 minutes later
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + 900L, 30.0))  // 15 minutes later
  
  // Linear interpolation at intermediate points
  let interpolated_150 = TimeSeries::interpolate(time_series, base_time + 150L, InterpolationType::Linear)
  match interpolated_150 {
    Some(value) => assert_eq(value, 22.5)  // Midpoint between 20.0 and 25.0
    None => assert_true(false)
  }
  
  let interpolated_450 = TimeSeries::interpolate(time_series, base_time + 450L, InterpolationType::Linear)
  match interpolated_450 {
    Some(value) => assert_eq(value, 20.0)  // Midpoint between 25.0 and 15.0
    None => assert_true(false)
  }
  
  let interpolated_750 = TimeSeries::interpolate(time_series, base_time + 750L, InterpolationType::Linear)
  match interpolated_750 {
    Some(value) => assert_eq(value, 22.5)  // Midpoint between 15.0 and 30.0
    None => assert_true(false)
  }
  
  // Interpolation outside range should return None
  let before_range = TimeSeries::interpolate(time_series, base_time - 60L, InterpolationType::Linear)
  match before_range {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let after_range = TimeSeries::interpolate(time_series, base_time + 960L, InterpolationType::Linear)
  match after_range {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Fill missing values using interpolation
  let filled_series = TimeSeries::fill_missing(time_series, 60L, InterpolationType::Linear)
  assert_eq(TimeSeries::size(filled_series), 16)  // Original 4 points + 12 interpolated
  
  // Verify filled points at 1-minute intervals
  for i in 0..=15 {
    let point = TimeSeries::point_at(filled_series, i)
    match point {
      Some(p) => {
        let expected_timestamp = base_time + (i * 60L)
        assert_eq(TimeSeriesPoint::timestamp(p), expected_timestamp)
        
        // Verify interpolated values
        if i == 0 {
          assert_eq(TimeSeriesPoint::value(p), 20.0)
        } else if i == 5 {
          assert_eq(TimeSeriesPoint::value(p), 25.0)
        } else if i == 10 {
          assert_eq(TimeSeriesPoint::value(p), 15.0)
        } else if i == 15 {
          assert_eq(TimeSeriesPoint::value(p), 30.0)
        }
      }
      None => assert_true(false)
    }
  }
}

// Test 7: Time Series Anomaly Detection
test "time series anomaly detection" {
  let time_series = TimeSeries::new("server_load")
  
  // Add normal data points
  let base_time = 1234567890L
  for i in 0..=19 {
    let timestamp = base_time + (i * 60L)
    let value = 50.0 + (Math::sin(i.to_float() * 0.5) * 10.0)  // Normal fluctuation
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Add some anomalies
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + (20 * 60L), 150.0))  // Spike
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + (21 * 60L), 48.0))   // Normal
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + (22 * 60L), -20.0))  // Dip
  TimeSeries::add_point(time_series, TimeSeriesPoint::new(base_time + (23 * 60L), 52.0))   // Normal
  
  // Detect anomalies using standard deviation method
  let anomalies = TimeSeries::detect_anomalies(time_series, AnomalyDetectionMethod::StandardDeviation, 2.0)
  assert_eq(anomalies.length(), 2)  // Should detect the spike and the dip
  
  // Verify anomalies
  for anomaly in anomalies {
    let point = TimeSeries::point_at(time_series, anomaly.index)
    match point {
      Some(p) => {
        let value = TimeSeriesPoint::value(p)
        assert_true(value == 150.0 || value == -20.0)
      }
      None => assert_true(false)
    }
  }
  
  // Detect anomalies using percentile method
  let percentile_anomalies = TimeSeries::detect_anomalies(time_series, AnomalyDetectionMethod::Percentile, 95.0)
  assert_eq(percentile_anomalies.length(), 1)  // Should detect only the spike
  
  // Verify percentile anomaly
  let percentile_point = TimeSeries::point_at(time_series, percentile_anomalies[0].index)
  match percentile_point {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), 150.0)
    None => assert_true(false)
  }
}

// Test 8: Time Series Forecasting
test "time series forecasting" {
  let time_series = TimeSeries::new("sales_data")
  
  // Add trend data with some seasonality
  let base_time = 1234567890L
  for i in 0..=29 {
    let timestamp = base_time + (i * 86400L)  // Daily data
    let trend = 100.0 + (i * 2.0)  // Linear trend
    let seasonality = Math::sin(i.to_float() * 0.5) * 20.0  // Seasonal component
    let value = trend + seasonality
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Simple linear regression forecasting
  let forecast = TimeSeries::forecast(time_series, ForecastingMethod::LinearRegression, 5)
  assert_eq(forecast.length(), 5)
  
  // Verify forecast values are in reasonable range
  for i in 0..=4 {
    let forecast_point = forecast[i]
    let forecast_value = TimeSeriesPoint::value(forecast_point)
    
    // Forecast should continue the trend
    let expected_min = 100.0 + ((30 + i) * 2.0) - 20.0  // Trend - seasonality
    let expected_max = 100.0 + ((30 + i) * 2.0) + 20.0  // Trend + seasonality
    assert_true(forecast_value >= expected_min && forecast_value <= expected_max)
  }
  
  // Moving average forecasting
  let ma_forecast = TimeSeries::forecast(time_series, ForecastingMethod::MovingAverage, 3)
  assert_eq(ma_forecast.length(), 3)
  
  // Verify moving average forecast
  for i in 0..=2 {
    let ma_point = ma_forecast[i]
    let ma_value = TimeSeriesPoint::value(ma_point)
    
    // Moving average should be close to recent values
    assert_true(ma_value >= 140.0 && ma_value <= 180.0)
  }
}

// Test 9: Time Series Correlation
test "time series correlation" {
  let series1 = TimeSeries::new("cpu_usage")
  let series2 = TimeSeries::new("memory_usage")
  
  // Add correlated data
  let base_time = 1234567890L
  for i in 0..=19 {
    let timestamp = base_time + (i * 60L)
    let base_value = 50.0 + (i * 2.0)  // Base trend
    
    // Series 1: CPU usage
    let cpu_value = base_value + (Math::random() * 5.0 - 2.5)  // Add some noise
    TimeSeries::add_point(series1, TimeSeriesPoint::new(timestamp, cpu_value))
    
    // Series 2: Memory usage (correlated with CPU)
    let memory_value = base_value * 1.5 + (Math::random() * 5.0 - 2.5)  // Correlated with some noise
    TimeSeries::add_point(series2, TimeSeriesPoint::new(timestamp, memory_value))
  }
  
  // Calculate correlation
  let correlation = TimeSeries::correlation(series1, series2)
  
  // Should be positive correlation (close to 1.0)
  assert_true(correlation > 0.7)
  
  // Create uncorrelated series
  let series3 = TimeSeries::new("network_io")
  for i in 0..=19 {
    let timestamp = base_time + (i * 60L)
    let value = Math::random() * 100.0  // Random values
    TimeSeries::add_point(series3, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Calculate correlation with uncorrelated series
  let uncorrelated_correlation = TimeSeries::correlation(series1, series3)
  
  // Should be low correlation (close to 0.0)
  assert_true(Math::abs(uncorrelated_correlation) < 0.3)
}

// Test 10: Time Series Compression
test "time series compression" {
  let time_series = TimeSeries::new("sensor_data")
  
  // Add data points
  let base_time = 1234567890L
  for i in 0..=99 {
    let timestamp = base_time + (i * 60L)
    let value = 20.0 + (Math::sin(i.to_float() * 0.1) * 5.0)  // Smooth sinusoidal data
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Original size
  let original_size = TimeSeries::size(time_series)
  assert_eq(original_size, 100)
  
  // Compress using Douglas-Peucker algorithm
  let compressed_series = TimeSeries::compress(time_series, CompressionMethod::DouglasPeucker, 0.5)
  let compressed_size = TimeSeries::size(compressed_series)
  
  // Should reduce number of points
  assert_true(compressed_size < original_size)
  assert_true(compressed_size > 10)  // But not too aggressive
  
  // Verify compression preserves key features
  let original_min = TimeSeries::aggregate(time_series, AggregationType::Min)
  let original_max = TimeSeries::aggregate(time_series, AggregationType::Max)
  let compressed_min = TimeSeries::aggregate(compressed_series, AggregationType::Min)
  let compressed_max = TimeSeries::aggregate(compressed_series, AggregationType::Max)
  
  // Min and max should be close
  assert_true(Math::abs(original_min - compressed_min) < 1.0)
  assert_true(Math::abs(original_max - compressed_max) < 1.0)
  
  // Compress using largest triangle three buckets (LTTB) algorithm
  let lttb_series = TimeSeries::compress(time_series, CompressionMethod::LTTB, 20)  // Target 20 points
  let lttb_size = TimeSeries::size(lttb_series)
  
  // Should have approximately the target number of points
  assert_eq(lttb_size, 20)
  
  // Verify LTTB preserves overall shape
  let lttb_min = TimeSeries::aggregate(lttb_series, AggregationType::Min)
  let lttb_max = TimeSeries::aggregate(lttb_series, AggregationType::Max)
  
  assert_true(Math::abs(original_min - lttb_min) < 1.0)
  assert_true(Math::abs(original_max - lttb_max) < 1.0)
}