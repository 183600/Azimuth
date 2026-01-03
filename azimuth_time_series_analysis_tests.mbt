// Time Series Analysis Tests for Azimuth Telemetry System
// This file contains test cases for time series data analysis and operations

// Test 1: Basic Time Series Operations
test "basic time series operations" {
  // Create time series data
  let time_series = [
    (1000, 10.5),
    (2000, 12.3),
    (3000, 11.8),
    (4000, 13.2),
    (5000, 14.1)
  ]
  
  // Test time series length
  assert_eq(time_series.length(), 5)
  
  // Test time range
  let time_range = time_series[time_series.length() - 1].0 - time_series[0].0
  assert_eq(time_range, 4000)
  
  // Test value range
  let values = time_series.map(fn(point) { point.1 })
  let min_value = values.reduce(fn(acc, val) { if val < acc { val } else { acc } }, values[0])
  let max_value = values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, values[0])
  
  assert_eq(min_value, 10.5)
  assert_eq(max_value, 14.1)
  
  // Test time series slicing
  let sliced_series = time_series.slice(1, 4)
  assert_eq(sliced_series.length(), 3)
  assert_eq(sliced_series[0], (2000, 12.3))
  assert_eq(sliced_series[2], (4000, 13.2))
}

// Test 2: Time Series Resampling
test "time series resampling" {
  // High frequency time series
  let high_freq_series = [
    (1000, 10.0),
    (1500, 10.5),
    (2000, 11.0),
    (2500, 11.5),
    (3000, 12.0),
    (3500, 12.5),
    (4000, 13.0)
  ]
  
  // Downsample to 2-second intervals
  let downsample_interval = 2000
  let downsampled_series = []
  
  let mut current_time = high_freq_series[0].0
  while current_time <= high_freq_series[high_freq_series.length() - 1].0 {
    let window_end = current_time + downsample_interval
    let window_points = high_freq_series.filter(fn(point) { 
      point.0 >= current_time && point.0 < window_end 
    })
    
    if window_points.length() > 0 {
      let avg_value = window_points.reduce(fn(acc, point) { acc + point.1 }, 0.0) / window_points.length().to_float()
      downsampled_series = downsampled_series.push((current_time, avg_value))
    }
    
    current_time = current_time + downsample_interval
  }
  
  assert_eq(downsampled_series.length(), 3)
  assert_eq(downsampled_series[0], (1000, 10.25))  // Average of 10.0 and 10.5
  assert_eq(downsampled_series[1], (3000, 12.25))  // Average of 12.0 and 12.5
  assert_eq(downsampled_series[2], (4000, 13.0))   // Single point
  
  // Upsample using linear interpolation
  let upsample_interval = 1000
  let upsampled_series = []
  
  for i in 0..<(downsampled_series.length() - 1) {
    let current_point = downsampled_series[i]
    let next_point = downsampled_series[i + 1]
    
    upsampled_series = upsampled_series.push(current_point)
    
    let time_diff = next_point.0 - current_point.0
    let value_diff = next_point.1 - current_point.1
    
    let interpolated_time = current_point.0 + upsample_interval
    if interpolated_time < next_point.0 {
      let ratio = (interpolated_time - current_point.0).to_float() / time_diff.to_float()
      let interpolated_value = current_point.1 + value_diff * ratio
      upsampled_series = upsampled_series.push((interpolated_time, interpolated_value))
    }
  }
  
  // Add the last point
  upsampled_series = upsampled_series.push(downsampled_series[downsampled_series.length() - 1])
  
  assert_eq(upsampled_series.length(), 5)
  assert_eq(upsampled_series[0], (1000, 10.25))
  assert_eq(upsampled_series[1], (2000, 11.25))  // Interpolated
  assert_eq(upsampled_series[2], (3000, 12.25))
  assert_eq(upsampled_series[3], (3500, 12.625))  // Interpolated
  assert_eq(upsampled_series[4], (4000, 13.0))
}

// Test 3: Moving Average Calculation
test "moving average calculation" {
  let time_series = []
  for i in 0..<20 {
    time_series = time_series.push((i * 1000, 10.0 + i.to_float() * 0.5))
  }
  
  // Simple moving average
  let window_size = 5
  let moving_averages = []
  
  for i in window_size - 1..<time_series.length() {
    let window_start = i - (window_size - 1)
    let window = time_series.slice(window_start, i + 1)
    
    let sum = window.reduce(fn(acc, point) { acc + point.1 }, 0.0)
    let avg = sum / window_size.to_float()
    
    moving_averages = moving_averages.push((time_series[i].0, avg))
  }
  
  assert_eq(moving_averages.length(), 16)
  assert_eq(moving_averages[0], (4000, 12.0))  // Average of first 5 points
  assert_eq(moving_averages[15], (19000, 17.25))  // Average of last 5 points
  
  // Weighted moving average
  let weighted_moving_averages = []
  let weights = [0.1, 0.15, 0.2, 0.25, 0.3]  // More weight to recent values
  
  for i in window_size - 1..<time_series.length() {
    let window_start = i - (window_size - 1)
    let window = time_series.slice(window_start, i + 1)
    
    let mut weighted_sum = 0.0
    for j in 0..<window_size {
      weighted_sum = weighted_sum + window[j].1 * weights[j]
    }
    
    weighted_moving_averages = weighted_moving_averages.push((time_series[i].0, weighted_sum))
  }
  
  assert_eq(weighted_moving_averages.length(), 16)
  assert_eq(weighted_moving_averages[0], (4000, 12.375))  // Weighted average of first 5 points
}

// Test 4: Time Series Trend Analysis
test "time series trend analysis" {
  // Linear trend time series
  let trend_series = [
    (1000, 10.0),
    (2000, 12.0),
    (3000, 14.0),
    (4000, 16.0),
    (5000, 18.0),
    (6000, 20.0)
  ]
  
  // Calculate linear regression
  let n = trend_series.length().to_float()
  let sum_x = trend_series.reduce(fn(acc, point) { acc + point.0.to_float() }, 0.0)
  let sum_y = trend_series.reduce(fn(acc, point) { acc + point.1 }, 0.0)
  let sum_xy = trend_series.reduce(fn(acc, point) { acc + point.0.to_float() * point.1 }, 0.0)
  let sum_x2 = trend_series.reduce(fn(acc, point) { acc + point.0.to_float() * point.0.to_float() }, 0.0)
  
  let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
  let intercept = (sum_y - slope * sum_x) / n
  
  assert_eq(slope, 0.002)  // 2 units per 1000 time units
  assert_eq(intercept, 8.0)
  
  // Test trend prediction
  let predict_value = fn(time) {
    slope * time.to_float() + intercept
  }
  
  let predicted_7000 = predict_value(7000)
  assert_eq(predicted_7000, 22.0)
  
  let predicted_8000 = predict_value(8000)
  assert_eq(predicted_8000, 24.0)
  
  // Calculate R-squared
  let y_mean = sum_y / n
  let total_sum_squares = trend_series.reduce(fn(acc, point) { 
    acc + (point.1 - y_mean) * (point.1 - y_mean) 
  }, 0.0)
  
  let residual_sum_squares = trend_series.reduce(fn(acc, point) { 
    let predicted = predict_value(point.0)
    acc + (point.1 - predicted) * (point.1 - predicted) 
  }, 0.0)
  
  let r_squared = 1.0 - (residual_sum_squares / total_sum_squares)
  assert_eq(r_squared, 1.0)  // Perfect linear relationship
}

// Test 5: Seasonality Detection
test "seasonality detection" {
  // Seasonal time series (daily pattern)
  let seasonal_series = []
  for day in 0..<7 {
    for hour in 0..<24 {
      let time = day * 24 * 3600 + hour * 3600  // Convert to seconds
      let base_value = 50.0
      let seasonal_component = 20.0 * (2.0 * 3.14159 * hour.to_float() / 24.0).cos()
      let value = base_value + seasonal_component
      seasonal_series = seasonal_series.push((time, value))
    }
  }
  
  // Detect daily seasonality
  let daily_period = 24 * 3600  // 24 hours in seconds
  let autocorrelations = []
  
  for lag in [3600, 7200, 10800, 14400, 18000, 21600, 43200, 86400] {  // Various lags
    let lagged_correlation = 0.0
    let count = 0
    
    for i in lag..<seasonal_series.length() {
      let current_value = seasonal_series[i].1
      let lagged_value = seasonal_series[i - lag].1
      lagged_correlation = lagged_correlation + current_value * lagged_value
      count = count + 1
    }
    
    if count > 0 {
      autocorrelations = autocorrelations.push((lag, lagged_correlation / count.to_float()))
    }
  }
  
  // Find highest autocorrelation (should be at 24-hour lag)
  let max_correlation = autocorrelations.reduce(fn(acc, corr) { 
    if corr.1 > acc.1 { corr } else { acc } 
  }, autocorrelations[0])
  
  assert_eq(max_correlation.0, 86400)  // 24 hours lag should have highest correlation
  
  // Extract seasonal pattern
  let hourly_pattern = []
  for hour in 0..<24 {
    let hour_values = seasonal_series.filter(fn(point) { 
      (point.0 / 3600) % 24 == hour 
    })
    
    let avg_value = hour_values.reduce(fn(acc, point) { acc + point.1 }, 0.0) / hour_values.length().to_float()
    hourly_pattern = hourly_pattern.push(avg_value)
  }
  
  assert_eq(hourly_pattern.length(), 24)
  assert_eq(hourly_pattern[0], 70.0)  // Peak at hour 0
  assert_eq(hourly_pattern[12], 30.0)  // Trough at hour 12
}

// Test 6: Anomaly Detection
test "anomaly detection in time series" {
  // Time series with anomalies
  let anomaly_series = [
    (1000, 10.0),
    (2000, 10.5),
    (3000, 10.2),
    (4000, 50.0),  // Anomaly
    (5000, 10.8),
    (6000, 10.3),
    (7000, 10.7),
    (8000, -20.0), // Anomaly
    (9000, 10.4),
    (10000, 10.6)
  ]
  
  // Statistical anomaly detection using z-score
  let values = anomaly_series.map(fn(point) { point.1 })
  let mean = values.reduce(fn(acc, val) { acc + val }, 0.0) / values.length().to_float()
  
  let variance = values.reduce(fn(acc, val) { 
    acc + (val - mean) * (val - mean) 
  }, 0.0) / values.length().to_float()
  
  let std_dev = variance.sqrt()
  
  // Detect anomalies (z-score > 2)
  let anomalies = []
  let threshold = 2.0
  
  for point in anomaly_series {
    let z_score = (point.1 - mean) / std_dev
    if z_score.abs() > threshold {
      anomalies = anomalies.push((point, z_score))
    }
  }
  
  assert_eq(anomalies.length(), 2)
  assert_eq(anomalies[0].0, (4000, 50.0))
  assert_eq(anomalies[1].0, (8000, -20.0))
  
  // Moving average based anomaly detection
  let window_size = 3
  let moving_avg_anomalies = []
  
  for i in window_size..<anomaly_series.length() - window_size {
    let window_start = i - window_size
    let window_end = i + window_size + 1
    let window = anomaly_series.slice(window_start, window_end)
    
    let window_values = window.map(fn(point) { point.1 })
    let window_mean = window_values.reduce(fn(acc, val) { acc + val }, 0.0) / window_values.length().to_float()
    let window_std = (window_values.reduce(fn(acc, val) { 
      acc + (val - window_mean) * (val - window_mean) 
    }, 0.0) / window_values.length().to_float()).sqrt()
    
    let current_point = anomaly_series[i]
    let z_score = (current_point.1 - window_mean) / window_std
    
    if z_score.abs() > threshold {
      moving_avg_anomalies = moving_avg_anomalies.push((current_point, z_score))
    }
  }
  
  assert_eq(moving_avg_anomalies.length(), 2)
}

// Test 7: Time Series Forecasting
test "time series forecasting" {
  // Historical data for forecasting
  let historical_data = [
    (1000, 10.0),
    (2000, 12.0),
    (3000, 11.0),
    (4000, 13.0),
    (4000, 15.0),
    (5000, 14.0),
    (6000, 16.0),
    (7000, 15.0),
    (8000, 17.0),
    (9000, 16.0)
  ]
  
  // Simple exponential smoothing
  let alpha = 0.3
  let smoothed_values = []
  
  // Initialize with first value
  smoothed_values = smoothed_values.push(historical_data[0].1)
  
  for i in 1..<historical_data.length() {
    let current_value = historical_data[i].1
    let previous_smoothed = smoothed_values[i - 1]
    let smoothed = alpha * current_value + (1.0 - alpha) * previous_smoothed
    smoothed_values = smoothed_values.push(smoothed)
  }
  
  assert_eq(smoothed_values.length(), 10)
  assert_eq(smoothed_values[0], 10.0)
  assert_eq(smoothed_values[1], 10.6)  // 0.3 * 12 + 0.7 * 10
  
  // Forecast next values
  let forecast_horizon = 3
  let forecasts = []
  
  for i in 0..<forecast_horizon {
    let forecast_time = historical_data[historical_data.length() - 1].0 + (i + 1) * 1000
    let forecast_value = smoothed_values[smoothed_values.length() - 1]  // Use last smoothed value
    forecasts = forecasts.push((forecast_time, forecast_value))
  }
  
  assert_eq(forecasts.length(), 3)
  assert_eq(forecasts[0], (10000, smoothed_values[9]))
  assert_eq(forecasts[1], (11000, smoothed_values[9]))
  assert_eq(forecasts[2], (12000, smoothed_values[9]))
  
  // Trend-based forecasting
  let trend_forecasts = []
  let recent_data = historical_data.slice(historical_data.length() - 3, historical_data.length())
  
  let trend_slope = (recent_data[2].1 - recent_data[0].1) / (recent_data[2].0 - recent_data[0].0).to_float()
  let last_value = historical_data[historical_data.length() - 1].1
  
  for i in 0..<forecast_horizon {
    let forecast_time = historical_data[historical_data.length() - 1].0 + (i + 1) * 1000
    let forecast_value = last_value + trend_slope * ((i + 1) * 1000).to_float()
    trend_forecasts = trend_forecasts.push((forecast_time, forecast_value))
  }
  
  assert_eq(trend_forecasts.length(), 3)
}

// Test 8: Time Series Decomposition
test "time series decomposition" {
  // Time series with trend, seasonality, and noise
  let decomposition_series = []
  for i in 0..<24 {
    let time = i * 3600  // Hourly data
    let trend = 10.0 + i.to_float() * 0.5
    let seasonal = 5.0 * (2.0 * 3.14159 * i.to_float() / 12.0).sin()  // 12-hour seasonality
    let noise = (i % 3 - 1).to_float() * 0.5  // Simple noise pattern
    let value = trend + seasonal + noise
    decomposition_series = decomposition_series.push((time, value))
  }
  
  // Extract trend using moving average
  let trend_window = 6
  let extracted_trend = []
  
  for i in trend_window - 1..<decomposition_series.length() {
    let window_start = i - (trend_window - 1)
    let window = decomposition_series.slice(window_start, i + 1)
    let avg = window.reduce(fn(acc, point) { acc + point.1 }, 0.0) / window.length().to_float()
    extracted_trend = extracted_trend.push((decomposition_series[i].0, avg))
  }
  
  // Extract seasonality
  let seasonal_period = 12
  let seasonal_components = []
  
  for i in 0..<seasonal_period {
    let period_values = []
    for j in i..<decomposition_series.length() {
      if (j - i) % seasonal_period == 0 {
        period_values = period_values.push(decomposition_series[j].1)
      }
    }
    
    if period_values.length() > 0 {
      let avg = period_values.reduce(fn(acc, val) { acc + val }, 0.0) / period_values.length().to_float()
      seasonal_components = seasonal_components.push(avg)
    }
  }
  
  assert_eq(seasonal_components.length(), 12)
  
  // Extract remainder (noise)
  let remainder = []
  for i in 0..<decomposition_series.length() {
    let original_value = decomposition_series[i].1
    let trend_value = if i < trend_window - 1 { 
      extracted_trend[0].1 
    } else if i >= extracted_trend.length() + trend_window - 1 { 
      extracted_trend[extracted_trend.length() - 1].1 
    } else { 
      extracted_trend[i - (trend_window - 1)].1 
    }
    
    let seasonal_value = seasonal_components[i % seasonal_period]
    let remainder_value = original_value - trend_value - seasonal_value
    remainder = remainder.push(remainder_value)
  }
  
  assert_eq(remainder.length(), 24)
}

// Test 9: Time Series Correlation Analysis
test "time series correlation analysis" {
  // Two correlated time series
  let series1 = [
    (1000, 10.0), (2000, 12.0), (3000, 11.0), (4000, 13.0), (5000, 15.0),
    (6000, 14.0), (7000, 16.0), (8000, 18.0), (9000, 17.0), (10000, 19.0)
  ]
  
  let series2 = [
    (1000, 20.0), (2000, 22.0), (3000, 21.0), (4000, 23.0), (5000, 25.0),
    (6000, 24.0), (7000, 26.0), (8000, 28.0), (9000, 27.0), (10000, 29.0)
  ]
  
  // Calculate correlation coefficient
  let values1 = series1.map(fn(point) { point.1 })
  let values2 = series2.map(fn(point) { point.1 })
  
  let mean1 = values1.reduce(fn(acc, val) { acc + val }, 0.0) / values1.length().to_float()
  let mean2 = values2.reduce(fn(acc, val) { acc + val }, 0.0) / values2.length().to_float()
  
  let covariance = values1.reduce(fn(acc, val1) { 
    let i = values1.index_of(val1).or_else(0)
    let val2 = values2[i]
    acc + (val1 - mean1) * (val2 - mean2)
  }, 0.0) / values1.length().to_float()
  
  let std1 = (values1.reduce(fn(acc, val) { acc + (val - mean1) * (val - mean1) }, 0.0) / values1.length().to_float()).sqrt()
  let std2 = (values2.reduce(fn(acc, val) { acc + (val - mean2) * (val - mean2) }, 0.0) / values2.length().to_float()).sqrt()
  
  let correlation = covariance / (std1 * std2)
  
  assert_eq(correlation, 1.0)  // Perfect positive correlation
  
  // Cross-correlation with lag
  let cross_correlations = []
  
  for lag in 0..<5 {
    let lagged_correlation = 0.0
    let count = 0
    
    for i in lag..<values1.length() {
      let val1 = values1[i - lag]
      let val2 = values2[i]
      lagged_correlation = lagged_correlation + (val1 - mean1) * (val2 - mean2)
      count = count + 1
    }
    
    if count > 0 {
      cross_correlations = cross_correlations.push((lag, lagged_correlation / (count.to_float() * std1 * std2)))
    }
  }
  
  assert_eq(cross_correlations.length(), 5)
  assert_eq(cross_correlations[0].1, 1.0)  // Zero lag correlation
}

// Test 10: Time Series Compression
test "time series compression" {
  // High-resolution time series
  let high_res_series = []
  for i in 0..<100 {
    let time = i * 100  // 100ms intervals
    let value = 10.0 + i.to_float() * 0.1 + (i % 10).to_float() * 0.05
    high_res_series = high_res_series.push((time, value))
  }
  
  // Piecewise linear approximation compression
  let compression_threshold = 0.1
  let compressed_points = []
  
  // Start with first point
  compressed_points = compressed_points.push(high_res_series[0])
  
  let mut start_idx = 0
  let mut end_idx = 1
  
  while end_idx < high_res_series.length() {
    let start_point = high_res_series[start_idx]
    let end_point = high_res_series[end_idx]
    
    // Check if intermediate points are within threshold of line
    let mut within_threshold = true
    
    for i in start_idx + 1..<end_idx {
      let intermediate_point = high_res_series[i]
      
      // Linear interpolation
      let ratio = (intermediate_point.0 - start_point.0).to_float() / (end_point.0 - start_point.0).to_float()
      let interpolated_value = start_point.1 + ratio * (end_point.1 - start_point.1)
      
      let error = (intermediate_point.1 - interpolated_value).abs()
      
      if error > compression_threshold {
        within_threshold = false
        break
      }
    }
    
    if within_threshold {
      end_idx = end_idx + 1
    } else {
      // Add the last valid point and start new segment
      compressed_points = compressed_points.push(high_res_series[end_idx - 1])
      start_idx = end_idx - 1
      end_idx = start_idx + 1
    }
  }
  
  // Add the last point
  compressed_points = compressed_points.push(high_res_series[high_res_series.length() - 1])
  
  assert_eq(compressed_points.length() < high_res_series.length())
  
  // Calculate compression ratio
  let compression_ratio = compressed_points.length().to_float() / high_res_series.length().to_float()
  assert_true(compression_ratio < 0.5)  // Should compress to less than 50%
  
  // Test reconstruction accuracy
  let mut max_error = 0.0
  
  for original_point in high_res_series {
    // Find corresponding segment
    let mut closest_compressed = compressed_points[0]
    let mut min_distance = (original_point.0 - closest_compressed.0).abs().to_float()
    
    for compressed_point in compressed_points {
      let distance = (original_point.0 - compressed_point.0).abs().to_float()
      if distance < min_distance {
        min_distance = distance
        closest_compressed = compressed_point
      }
    }
    
    let error = (original_point.1 - closest_compressed.1).abs()
    if error > max_error {
      max_error = error
    }
  }
  
  assert_true(max_error < 1.0)  // Maximum reconstruction error should be small
}