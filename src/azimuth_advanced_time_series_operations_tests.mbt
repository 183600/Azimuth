// Advanced Time Series Operations Tests for Azimuth Telemetry System
// This file contains test cases for advanced time series operations

test "time series data aggregation" {
  // Test time series data aggregation operations
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  // Create time series data points
  let data_points = []
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_time + (i * 1000000L)  // 1ms intervals
    let value = i.to_double() * 1.5  // Linear growth
    data_points.push((timestamp, value))
  }
  
  // Aggregate data points into time windows (e.g., 10ms windows)
  let window_size = 10000000L  // 10ms in nanoseconds
  let aggregated_windows = aggregate_time_windows(data_points, window_size)
  
  // Verify that we have the expected number of windows
  assert_eq(aggregated_windows.length(), 10)
  
  // Verify the first window
  let first_window = aggregated_windows[0]
  assert_eq(first_window.start_time, base_time)
  assert_eq(first_window.end_time, base_time + window_size)
  assert_eq(first_window.min_value, 0.0)
  assert_eq(first_window.max_value, 13.5)  // 9 * 1.5
  assert_eq(first_window.avg_value, (0.0 + 1.5 + 3.0 + 4.5 + 6.0 + 7.5 + 9.0 + 10.5 + 12.0 + 13.5) / 10.0)
  assert_eq(first_window.count, 10.0)
}

test "time series data interpolation" {
  // Test time series data interpolation
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  // Create sparse time series data points
  let sparse_data = [
    (base_time, 10.0),
    (base_time + 10000000L, 20.0),  // 10ms later
    (base_time + 30000000L, 40.0),  // 30ms later
    (base_time + 60000000L, 70.0),  // 60ms later
    (base_time + 100000000L, 100.0)  // 100ms later
  ]
  
  // Interpolate values at regular intervals
  let interpolation_interval = 5000000L  // 5ms
  let interpolated_data = interpolate_time_series(sparse_data, interpolation_interval)
  
  // Verify that we have the expected number of interpolated points
  assert_eq(interpolated_data.length(), 21)  // 0ms to 100ms at 5ms intervals
  
  // Verify specific interpolated values
  assert_eq(interpolated_data[0], (base_time, 10.0))  // Exact match
  assert_eq(interpolated_data[2], (base_time + 10000000L, 20.0))  // Exact match
  assert_eq(interpolated_data[6], (base_time + 30000000L, 40.0))  // Exact match
  
  // Check interpolated value at 50ms
  let interpolated_50ms = interpolated_data.find(fn((ts, _) { ts == base_time + 50000000L })
  match interpolated_50ms {
    Some((_, value)) => {
      // Should be interpolated between 40.0 at 30ms and 70.0 at 60ms
      // (50-30)/(60-30) = 20/30 = 2/3
      // 40 + (70-40) * 2/3 = 40 + 30 * 2/3 = 40 + 20 = 60
      assert_eq(value, 60.0)
    }
    None => assert_true(false)
  }
}

test "time series data downsampling" {
  // Test time series data downsampling
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  // Create high-frequency time series data
  let high_freq_data = []
  for i = 0; i < 1000; i = i + 1 {
    let timestamp = base_time + (i * 100000L)  // 0.1ms intervals
    let value = (i % 100).to_double()  // Repeating pattern 0-99
    high_freq_data.push((timestamp, value))
  }
  
  // Downsample to lower frequency
  let downsample_interval = 10000000L  // 10ms
  let downsampled_data = downsample_time_series(high_freq_data, downsample_interval)
  
  // Verify that we have the expected number of downsampled points
  assert_eq(downsampled_data.length(), 10)
  
  // Verify the first downsampled point
  let first_point = downsampled_data[0]
  assert_eq(first_point.timestamp, base_time)
  
  // The first window should contain 100 points (0-99)
  // Average of 0-99 is (0+99)/2 = 49.5
  assert_eq(first_point.value, 49.5)
}

test "time series data rate of change" {
  // Test time series data rate of change calculation
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  // Create time series data with quadratic growth
  let quadratic_data = []
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_time + (i * 1000000L)  // 1ms intervals
    let value = (i * i).to_double()  // Quadratic growth: 0, 1, 4, 9, 16, ...
    quadratic_data.push((timestamp, value))
  }
  
  // Calculate rate of change (derivative)
  let rate_of_change_data = calculate_rate_of_change(quadratic_data)
  
  // Verify that we have the expected number of rate of change points
  assert_eq(rate_of_change_data.length(), 99)
  
  // For quadratic growth f(x) = x^2, the derivative is f'(x) = 2x
  // At i=1: rate should be approximately 2*1 = 2
  assert_eq(rate_of_change_data[0], (base_time + 1000000L, 2.0))
  
  // At i=2: rate should be approximately 2*2 = 4
  assert_eq(rate_of_change_data[1], (base_time + 2000000L, 4.0))
  
  // At i=3: rate should be approximately 2*3 = 6
  assert_eq(rate_of_change_data[2], (base_time + 3000000L, 6.0))
}

test "time series data smoothing" {
  // Test time series data smoothing
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  // Create time series data with noise
  let noisy_data = []
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_time + (i * 1000000L)  // 1ms intervals
    let trend = i.to_double()  // Linear trend
    let noise = (if i % 2 == 0 { 5.0 } else { -5.0 })  // Alternating noise
    let value = trend + noise
    noisy_data.push((timestamp, value))
  }
  
  // Apply moving average smoothing
  let window_size = 5
  let smoothed_data = smooth_time_series(noisy_data, window_size)
  
  // Verify that we have the expected number of smoothed points
  assert_eq(smoothed_data.length(), 100)
  
  // Verify that the smoothed data has less variation than the noisy data
  let noisy_variation = calculate_variation(noisy_data)
  let smoothed_variation = calculate_variation(smoothed_data)
  
  // The smoothed variation should be less than the noisy variation
  assert_true(smoothed_variation < noisy_variation)
  
  // Verify specific smoothed values
  // First point should be the same as the original (window of size 1)
  assert_eq(smoothed_data[0], (base_time, 0.0 + 5.0))  // trend=0, noise=5
  
  // Fifth point should be the average of the first 5 points
  // Values: 5, -4, 7, -6, 9
  // Sum: 5 + (-4) + 7 + (-6) + 9 = 11
  // Average: 11 / 5 = 2.2
  assert_eq(smoothed_data[4], (base_time + 4000000L, 2.2))
}

test "time series data anomaly detection" {
  // Test time series data anomaly detection
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  // Create time series data with anomalies
  let anomaly_data = []
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_time + (i * 1000000L)  // 1ms intervals
    let value = match i {
      10 => 100.0  // Spike anomaly
      50 => -50.0  // Dip anomaly
      90 => 150.0  // Extreme spike anomaly
      _ => i.to_double() * 0.5  // Normal values
    }
    anomaly_data.push((timestamp, value))
  }
  
  // Detect anomalies using statistical method (e.g., z-score)
  let anomalies = detect_anomalies(anomaly_data, 3.0)  // 3 standard deviations threshold
  
  // Verify that we detected the expected anomalies
  assert_eq(anomalies.length(), 3)
  
  // Verify the specific anomalies
  let anomaly_10ms = anomalies.find(fn((ts, _, _) { ts == base_time + 10000000L }))
  let anomaly_50ms = anomalies.find(fn((ts, _, _) { ts == base_time + 50000000L }))
  let anomaly_90ms = anomalies.find(fn((ts, _, _) { ts == base_time + 90000000L }))
  
  match anomaly_10ms {
    Some((_, value, z_score)) => {
      assert_eq(value, 100.0)
      assert_true(z_score > 3.0)
    }
    None => assert_true(false)
  }
  
  match anomaly_50ms {
    Some((_, value, z_score)) => {
      assert_eq(value, -50.0)
      assert_true(z_score > 3.0)
    }
    None => assert_true(false)
  }
  
  match anomaly_90ms {
    Some((_, value, z_score)) => {
      assert_eq(value, 150.0)
      assert_true(z_score > 3.0)
    }
    None => assert_true(false)
  }
}

test "time series data forecasting" {
  // Test time series data forecasting
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  // Create time series data with linear trend
  let trend_data = []
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_time + (i * 1000000L)  // 1ms intervals
    let value = 10.0 + i.to_double() * 0.5  // Linear trend starting at 10
    trend_data.push((timestamp, value))
  }
  
  // Simple linear regression for forecasting
  let forecast_horizon = 10  // Forecast 10 points into the future
  let forecast_data = forecast_time_series(trend_data, forecast_horizon)
  
  // Verify that we have the expected number of forecast points
  assert_eq(forecast_data.length(), 10)
  
  // Verify that the forecast continues the linear trend
  // The last actual value should be at timestamp base_time + 99ms with value 10 + 99 * 0.5 = 59.5
  let last_actual_value = 10.0 + 99.0 * 0.5
  let first_forecast_value = forecast_data[0].value
  
  // The first forecast should be approximately last_actual_value + 0.5
  assert_true((first_forecast_value - last_actual_value - 0.5).abs() < 0.001)
  
  // The forecast should increase by approximately 0.5 for each step
  for i = 1; i < forecast_data.length(); i = i + 1 {
    let prev_value = forecast_data[i-1].value
    let curr_value = forecast_data[i].value
    let diff = curr_value - prev_value
    
    // The difference should be approximately 0.5 (the slope)
    assert_true((diff - 0.5).abs() < 0.001)
  }
}

// Helper functions for time series operations

type TimeSeriesPoint = (Int64, Double)

type AggregatedWindow = {
  start_time : Int64
  end_time : Int64
  min_value : Double
  max_value : Double
  avg_value : Double
  count : Double
}

fn aggregate_time_windows(data_points : Array[TimeSeriesPoint], window_size : Int64) -> Array[AggregatedWindow] {
  let windows = []
  let window_start = data_points[0].0
  let window_end = window_start + window_size
  
  while window_start < data_points[data_points.length() - 1].0 {
    let window_points = data_points.filter(fn((ts, _) { ts >= window_start && ts < window_end }))
    
    if window_points.length() > 0 {
      // Calculate aggregation metrics for the window
      let sum = window_points.fold(0.0, fn(acc, (_, v) { acc + v }))
      let count = window_points.length().to_double()
      let avg = sum / count
      
      let min_val = window_points.fold(Double::max_value(), fn(acc, (_, v) { if v < acc { v } else { acc } }))
      let max_val = window_points.fold(Double::min_value(), fn(acc, (_, v) { if v > acc { v } else { acc } }))
      
      windows.push({
        start_time: window_start,
        end_time: window_end,
        min_value: min_val,
        max_value: max_val,
        avg_value: avg,
        count: count
      })
    }
    
    window_start = window_end
    window_end = window_end + window_size
  }
  
  windows
}

fn interpolate_time_series(sparse_data : Array[TimeSeriesPoint], interval : Int64) -> Array[TimeSeriesPoint] {
  let interpolated = []
  let start_time = sparse_data[0].0
  let end_time = sparse_data[sparse_data.length() - 1].0
  let current_time = start_time
  
  while current_time <= end_time {
    // Find the two data points to interpolate between
    let lower_point = sparse_data.filter(fn((ts, _) { ts <= current_time })).last()
    let upper_point = sparse_data.filter(fn((ts, _) { ts >= current_time })).first()
    
    match (lower_point, upper_point) {
      (Some((lower_ts, lower_val)), Some((upper_ts, upper_val))) => {
        if lower_ts == upper_ts {
          // Exact match
          interpolated.push((current_time, lower_val))
        } else {
          // Linear interpolation
          let ratio = (current_time - lower_ts).to_double() / (upper_ts - lower_ts).to_double()
          let interpolated_value = lower_val + (upper_val - lower_val) * ratio
          interpolated.push((current_time, interpolated_value))
        }
      }
      _ => ()  // Skip if we can't interpolate
    }
    
    current_time = current_time + interval
  }
  
  interpolated
}

type DownsampledPoint = {
  timestamp : Int64
  value : Double
}

fn downsample_time_series(high_freq_data : Array[TimeSeriesPoint], interval : Int64) -> Array[DownsampledPoint] {
  let downsampled = []
  let start_time = high_freq_data[0].0
  let end_time = high_freq_data[high_freq_data.length() - 1].0
  let current_time = start_time
  
  while current_time < end_time {
    // Collect all points within the downsample window
    let window_data = high_freq_data.filter(fn((ts, _) { 
      ts >= current_time && ts < current_time + interval 
    }))
    
    if window_data.length() > 0 {
      // Calculate downsampled value (using average)
      let sum = window_data.fold(0.0, fn(acc, (_, v) { acc + v }))
      let count = window_data.length().to_double()
      let avg = sum / count
      
      downsampled.push({
        timestamp: current_time,
        value: avg
      })
    }
    
    current_time = current_time + interval
  }
  
  downsampled
}

fn calculate_rate_of_change(data : Array[TimeSeriesPoint]) -> Array[TimeSeriesPoint] {
  let rate_of_change = []
  
  for i = 1; i < data.length(); i = i + 1 {
    let (prev_time, prev_value) = data[i-1]
    let (curr_time, curr_value) = data[i]
    
    let time_diff = (curr_time - prev_time).to_double()  // in nanoseconds
    let value_diff = curr_value - prev_value
    let rate = value_diff / time_diff * 1000000000.0  // Convert to per-second rate
    
    rate_of_change.push((curr_time, rate))
  }
  
  rate_of_change
}

fn smooth_time_series(data : Array[TimeSeriesPoint], window_size : Int) -> Array[TimeSeriesPoint] {
  let smoothed = []
  
  for i = 0; i < data.length(); i = i + 1 {
    let start_idx = if i >= window_size { i - window_size + 1 } else { 0 }
    let end_idx = i + 1
    
    let window_values = []
    for j = start_idx; j < end_idx; j = j + 1 {
      let (_, value) = data[j]
      window_values.push(value)
    }
    
    let sum = window_values.fold(0.0, fn(acc, v) { acc + v })
    let avg = sum / window_values.length().to_double()
    
    smoothed.push((data[i].0, avg))
  }
  
  smoothed
}

fn calculate_variation(data : Array[TimeSeriesPoint]) -> Double {
  data.fold(0.0, fn(acc, (_, v) { acc + v.abs() }))
}

type Anomaly = (Int64, Double, Double)

fn detect_anomalies(data : Array[TimeSeriesPoint], threshold : Double) -> Array[Anomaly] {
  let anomalies = []
  
  // Calculate mean and standard deviation
  let values = data.map(fn((_, v) { v }))
  let sum = values.fold(0.0, fn(acc, v) { acc + v })
  let count = values.length().to_double()
  let mean = sum / count
  
  let squared_diffs = values.map(fn(v) { (v - mean) * (v - mean) })
  let variance = squared_diffs.fold(0.0, fn(acc, v) { acc + v }) / count
  let std_dev = variance.sqrt()
  
  // Detect anomalies (values more than threshold standard deviations from the mean)
  for (timestamp, value) in data {
    let z_score = (value - mean).abs() / std_dev
    
    if z_score > threshold {
      anomalies.push((timestamp, value, z_score))
    }
  }
  
  anomalies
}

fn forecast_time_series(data : Array[TimeSeriesPoint], horizon : Int) -> Array[TimeSeriesPoint] {
  // Simple linear regression for forecasting
  let n = data.length().to_double()
  let sum_x = data.fold(0.0, fn(acc, (ts, _) { acc + ts.to_double() }))
  let sum_y = data.fold(0.0, fn(acc, (_, v) { acc + v }))
  let sum_xy = data.fold(0.0, fn(acc, (ts, v) { acc + ts.to_double() * v }))
  let sum_x2 = data.fold(0.0, fn(acc, (ts, _) { acc + ts.to_double() * ts.to_double() }))
  
  let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
  let intercept = (sum_y - slope * sum_x) / n
  
  // Forecast future values
  let forecast = []
  let last_timestamp = data[data.length() - 1].0
  
  for i = 1; i <= horizon; i = i + 1 {
    let future_timestamp = last_timestamp + (i * 1000000L)  // 1ms intervals
    let forecast_value = slope * future_timestamp.to_double() + intercept
    forecast.push((future_timestamp, forecast_value))
  }
  
  forecast
}