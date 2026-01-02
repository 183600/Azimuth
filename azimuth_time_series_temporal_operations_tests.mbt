// Azimuth Telemetry System - Time Series Operations Test Suite
// This file contains comprehensive test cases for time series operations

// Test 1: Basic Time Series Data Generation
test "basic time series data generation" {
  // Generate time series data with regular intervals
  let base_timestamp = 1640995200000  // 2022-01-01 00:00:00 UTC
  let interval_ms = 60000  // 1 minute interval
  let data_points = 24  // 24 hours of data
  
  let mut time_series = []
  
  for i = 0; i < data_points; i = i + 1 {
    let timestamp = base_timestamp + (i * interval_ms)
    let value = 50.0 + (10.0 * (i % 12).to_float()).sin()  // Sinusoidal pattern
    let data_point = (timestamp, value)
    time_series = time_series.concat([data_point])
  }
  
  assert_eq(time_series.length(), 24)
  
  // Verify first and last timestamps
  assert_eq(time_series[0][0], base_timestamp)
  assert_eq(time_series[23][0], base_timestamp + (23 * interval_ms))
  
  // Verify time intervals
  for i = 1; i < time_series.length(); i = i + 1 {
    let interval = time_series[i][0] - time_series[i-1][0]
    assert_eq(interval, interval_ms)
  }
  
  // Verify value range
  let values = time_series.map(|(_, value)| value)
  let min_value = values.reduce(|acc, val| if val < acc { val } else { acc }, values[0])
  let max_value = values.reduce(|acc, val| if val > acc { val } else { acc }, values[0])
  
  assert_true(min_value >= 40.0)  // 50 - 10
  assert_true(max_value <= 60.0)  // 50 + 10
}

// Test 2: Time Series Aggregation Operations
test "time series aggregation operations" {
  // Generate time series with 1-minute intervals for 1 hour
  let base_timestamp = 1640995200000
  let interval_ms = 60000  // 1 minute
  let data_points = 60     // 1 hour
  
  let mut time_series = []
  for i = 0; i < data_points; i = i + 1 {
    let timestamp = base_timestamp + (i * interval_ms)
    let value = 100.0 + (i.to_float() * 0.5)  // Linear increase
    time_series = time_series.concat([(timestamp, value)])
  }
  
  // Test time window aggregation (5-minute windows)
  let window_size_ms = 300000  // 5 minutes
  let mut aggregated_series = []
  
  let start_time = time_series[0][0]
  let end_time = time_series[time_series.length() - 1][0]
  
  let mut window_start = start_time
  while window_start < end_time {
    let window_end = window_start + window_size_ms
    
    // Filter data points in window
    let window_data = time_series.filter(|(timestamp, _)| {
      timestamp >= window_start && timestamp < window_end
    })
    
    if window_data.length() > 0 {
      let values = window_data.map(|(_, value)| value)
      let sum = values.reduce(|acc, val| acc + val, 0.0)
      let count = values.length().to_float()
      let avg = sum / count
      let min = values.reduce(|acc, val| if val < acc { val } else { acc }, values[0])
      let max = values.reduce(|acc, val| if val > acc { val } else { acc }, values[0])
      
      aggregated_series = aggregated_series.concat([(
        window_start,
        ("sum", sum),
        ("avg", avg),
        ("min", min),
        ("max", max),
        ("count", count)
      )])
    }
    
    window_start = window_end
  }
  
  // Should have 12 windows (60 minutes / 5 minutes)
  assert_eq(aggregated_series.length(), 12)
  
  // Verify aggregation calculations for first window
  let first_window = aggregated_series[0]
  let first_window_sum = first_window[1][1]  // ("sum", sum)
  let first_window_avg = first_window[2][1]  // ("avg", avg)
  let first_window_count = first_window[5][1]  // ("count", count)
  
  assert_eq(first_window_count, 5.0)  // 5 data points in 5-minute window
  assert_eq(first_window_sum, 100.0 + 100.5 + 101.0 + 101.5 + 102.0)  // Sum of first 5 values
  assert_eq(first_window_avg, first_window_sum / first_window_count)
}

// Test 3: Time Series Resampling and Interpolation
test "time series resampling and interpolation" {
  // Generate high-frequency time series (30-second intervals)
  let base_timestamp = 1640995200000
  let high_freq_interval = 30000  // 30 seconds
  let high_freq_points = 12       // 6 minutes of data
  
  let mut high_freq_series = []
  for i = 0; i < high_freq_points; i = i + 1 {
    let timestamp = base_timestamp + (i * high_freq_interval)
    let value = 50.0 + (i.to_float() * 2.0)  // Linear increase
    high_freq_series = high_freq_series.concat([(timestamp, value)])
  }
  
  // Resample to low frequency (1-minute intervals) using linear interpolation
  let low_freq_interval = 60000  // 1 minute
  let mut low_freq_series = []
  
  let start_time = high_freq_series[0][0]
  let end_time = high_freq_series[high_freq_series.length() - 1][0]
  
  let mut target_time = start_time
  while target_time <= end_time {
    if target_time == high_freq_series[0][0] {
      // First point, use exact value
      low_freq_series = low_freq_series.concat([(target_time, high_freq_series[0][1])])
    } else {
      // Find surrounding points for interpolation
      let mut lower_point = (0, 0.0)
      let mut upper_point = (0, 0.0)
      let mut found_lower = false
      let mut found_upper = false
      
      for point in high_freq_series {
        if point[0] <= target_time {
          lower_point = point
          found_lower = true
        }
        if point[0] >= target_time && !found_upper {
          upper_point = point
          found_upper = true
        }
      }
      
      if found_lower && found_upper {
        // Linear interpolation
        let time_diff = upper_point[0] - lower_point[0]
        let value_diff = upper_point[1] - lower_point[1]
        let target_offset = target_time - lower_point[0]
        let interpolated_value = lower_point[1] + (value_diff * target_offset.to_float() / time_diff.to_float())
        
        low_freq_series = low_freq_series.concat([(target_time, interpolated_value)])
      }
    }
    
    target_time = target_time + low_freq_interval
  }
  
  // Should have 7 points (6 minutes + 1)
  assert_eq(low_freq_series.length(), 7)
  
  // Verify interpolation accuracy
  assert_eq(low_freq_series[0][1], 50.0)   // Exact match
  assert_eq(low_freq_series[1][1], 52.0)   // Interpolated
  assert_eq(low_freq_series[2][1], 54.0)   // Interpolated
  assert_eq(low_freq_series[3][1], 56.0)   // Interpolated
  assert_eq(low_freq_series[4][1], 58.0)   // Interpolated
  assert_eq(low_freq_series[5][1], 60.0)   // Interpolated
  assert_eq(low_freq_series[6][1], 62.0)   // Exact match (last point)
}

// Test 4: Time Series Trend Analysis
test "time series trend analysis" {
  // Generate time series with different trends
  let base_timestamp = 1640995200000
  let interval_ms = 3600000  // 1 hour interval
  let data_points = 24       // 24 hours
  
  let mut trend_series = []
  
  for i = 0; i < data_points; i = i + 1 {
    let timestamp = base_timestamp + (i * interval_ms)
    // Combine multiple trends: linear + seasonal + noise
    let linear_trend = 100.0 + (i.to_float() * 2.0)  // Linear increase
    let seasonal_trend = 10.0 * ((i.to_float() * 2.0 * 3.14159) / 24.0).sin()  // Daily cycle
    let noise = (i % 7).to_float() - 3.0  // Small noise
    let value = linear_trend + seasonal_trend + noise
    
    trend_series = trend_series.concat([(timestamp, value)])
  }
  
  // Calculate simple moving average (SMA)
  let sma_period = 4  // 4-hour SMA
  let mut sma_series = []
  
  for i = sma_period - 1; i < trend_series.length(); i = i + 1 {
    let mut sum = 0.0
    for j = i - (sma_period - 1); j <= i; j = j + 1 {
      sum = sum + trend_series[j][1]
    }
    let sma_value = sum / sma_period.to_float()
    sma_series = sma_series.concat([(trend_series[i][0], sma_value)])
  }
  
  // Should have 20 SMA points (24 - 4 + 1)
  assert_eq(sma_series.length(), 20)
  
  // Calculate exponential moving average (EMA)
  let ema_period = 4
  let ema_alpha = 2.0 / (ema_period.to_float() + 1.0)
  let mut ema_series = []
  
  // Initialize EMA with first value
  let mut ema_value = trend_series[0][1]
  ema_series = ema_series.concat([(trend_series[0][0], ema_value)])
  
  for i = 1; i < trend_series.length(); i = i + 1 {
    ema_value = ema_alpha * trend_series[i][1] + (1.0 - ema_alpha) * ema_value
    ema_series = ema_series.concat([(trend_series[i][0], ema_value)])
  }
  
  // Should have 24 EMA points
  assert_eq(ema_series.length(), 24)
  
  // Calculate trend direction
  let mut trend_directions = []
  for i = 1; i < trend_series.length(); i = i + 1 {
    let current_value = trend_series[i][1]
    let previous_value = trend_series[i-1][1]
    let direction = if current_value > previous_value { "up" } else if current_value < previous_value { "down" } else { "flat" }
    trend_directions = trend_directions.concat([direction])
  }
  
  // Should have 23 trend directions
  assert_eq(trend_directions.length(), 23)
  
  // Count trend directions
  let up_count = trend_directions.filter(|dir| dir == "up").length()
  let down_count = trend_directions.filter(|dir| dir == "down").length()
  let flat_count = trend_directions.filter(|dir| dir == "flat").length()
  
  assert_eq(up_count + down_count + flat_count, 23)
  assert_true(up_count > down_count)  // Should be mostly upward due to linear trend
}

// Test 5: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Generate time series with some anomalies
  let base_timestamp = 1640995200000
  let interval_ms = 3600000  // 1 hour interval
  let data_points = 24       // 24 hours
  
  let mut anomaly_series = []
  
  for i = 0; i < data_points; i = i + 1 {
    let timestamp = base_timestamp + (i * interval_ms)
    let mut value = 100.0 + (5.0 * (i % 6).to_float()).sin()  // Regular pattern
    
    // Inject anomalies at specific points
    if i == 6 {
      value = value + 50.0  // Spike anomaly
    } else if i == 12 {
      value = value - 30.0  // Drop anomaly
    } else if i == 18 {
      value = value * 2.0   // Scale anomaly
    }
    
    anomaly_series = anomaly_series.concat([(timestamp, value)])
  }
  
  // Detect anomalies using statistical method (z-score)
  let values = anomaly_series.map(|(_, value)| value)
  let mean = values.reduce(|acc, val| acc + val, 0.0) / values.length().to_float()
  
  let variance = values.reduce(|acc, val| acc + (val - mean) * (val - mean), 0.0) / values.length().to_float()
  let std_dev = variance.sqrt()
  
  let z_score_threshold = 2.0  // 2 standard deviations
  let mut detected_anomalies = []
  
  for i = 0; i < anomaly_series.length(); i = i + 1 {
    let value = anomaly_series[i][1]
    let z_score = (value - mean).abs() / std_dev
    
    if z_score > z_score_threshold {
      detected_anomalies = detected_anomalies.concat([(
        anomaly_series[i][0],
        value,
        z_score
      )])
    }
  }
  
  // Should detect at least the 3 injected anomalies
  assert_true(detected_anomalies.length() >= 3)
  
  // Verify anomaly detection accuracy
  let anomaly_indices = detected_anomalies.map(|(timestamp, _, _)| {
    for i = 0; i < anomaly_series.length(); i = i + 1 {
      if anomaly_series[i][0] == timestamp {
        return i
      }
    }
    -1
  })
  
  let has_spike = anomaly_indices.any(|idx| idx == 6)
  let has_drop = anomaly_indices.any(|idx| idx == 12)
  let has_scale = anomaly_indices.any(|idx| idx == 18)
  
  assert_true(has_spike)
  assert_true(has_drop)
  assert_true(has_scale)
  
  // Test moving window anomaly detection
  let window_size = 6  // 6-hour window
  let mut window_anomalies = []
  
  for i = window_size; i < anomaly_series.length(); i = i + 1 {
    let window_values = []
    for j = i - window_size; j < i; j = j + 1 {
      window_values = window_values.concat([anomaly_series[j][1]])
    }
    
    let window_mean = window_values.reduce(|acc, val| acc + val, 0.0) / window_values.length().to_float()
    let window_variance = window_values.reduce(|acc, val| acc + (val - window_mean) * (val - window_mean), 0.0) / window_values.length().to_float()
    let window_std_dev = window_variance.sqrt()
    
    let current_value = anomaly_series[i][1]
    let z_score = (current_value - window_mean).abs() / window_std_dev
    
    if z_score > 2.5 {  // Higher threshold for window method
      window_anomalies = window_anomalies.concat([(
        anomaly_series[i][0],
        current_value,
        z_score
      )])
    }
  }
  
  // Window method should also detect anomalies
  assert_true(window_anomalies.length() >= 2)
}

// Test 6: Time Series Seasonality Analysis
test "time series seasonality analysis" {
  // Generate time series with multiple seasonal patterns
  let base_timestamp = 1640995200000
  let interval_ms = 3600000  // 1 hour interval
  let data_points = 168      // 1 week of hourly data
  
  let mut seasonal_series = []
  
  for i = 0; i < data_points; i = i + 1 {
    let timestamp = base_timestamp + (i * interval_ms)
    
    // Daily seasonality (24-hour cycle)
    let daily_seasonal = 20.0 * ((i.to_float() * 2.0 * 3.14159) / 24.0).sin()
    
    // Weekly seasonality (7-day cycle)
    let weekly_seasonal = 10.0 * ((i.to_float() * 2.0 * 3.14159) / (24.0 * 7.0)).sin()
    
    // Base value with trend
    let base_value = 100.0 + (i.to_float() * 0.1)
    
    let value = base_value + daily_seasonal + weekly_seasonal
    seasonal_series = seasonal_series.concat([(timestamp, value)])
  }
  
  // Autocorrelation analysis for seasonality detection
  let values = seasonal_series.map(|(_, value)| value)
  let mean = values.reduce(|acc, val| acc + val, 0.0) / values.length().to_float()
  
  // Calculate autocorrelation for different lags
  let test_lags = [24, 48, 72, 168]  // 1 day, 2 days, 3 days, 1 week
  let mut autocorrelations = []
  
  for lag in test_lags {
    let mut numerator = 0.0
    let mut denominator = 0.0
    
    for i = lag; i < values.length(); i = i + 1 {
      numerator = numerator + (values[i] - mean) * (values[i - lag] - mean)
    }
    
    for i = 0; i < values.length(); i = i + 1 {
      denominator = denominator + (values[i] - mean) * (values[i] - mean)
    }
    
    let autocorr = numerator / denominator
    autocorrelations = autocorrelations.concat([(lag, autocorr)])
  }
  
  // Should have 4 autocorrelation values
  assert_eq(autocorrelations.length(), 4)
  
  // Daily seasonality should be strong (lag 24, 48, 72)
  let lag_24 = autocorrelations.filter(|(lag, _)| lag == 24)[0][1]
  let lag_48 = autocorrelations.filter(|(lag, _)| lag == 48)[0][1]
  let lag_72 = autocorrelations.filter(|(lag, _)| lag == 72)[0][1]
  
  assert_true(lag_24.abs() > 0.5)  // Strong daily correlation
  assert_true(lag_48.abs() > 0.5)  // Strong 2-day correlation
  assert_true(lag_72.abs() > 0.5)  // Strong 3-day correlation
  
  // Weekly seasonality should also be present
  let lag_168 = autocorrelations.filter(|(lag, _)| lag == 168)[0][1]
  assert_true(lag_168.abs() > 0.3)  // Weekly correlation
  
  // Seasonal decomposition (simplified)
  let trend_period = 168  // Use full period for trend
  let mut trend_component = []
  
  // Simple moving average for trend
  for i = 0; i < seasonal_series.length(); i = i + 1 {
    let start = if i < 12 { 0 } else { i - 12 }
    let end = if i > seasonal_series.length() - 13 { seasonal_series.length() - 1 } else { i + 12 }
    
    let mut sum = 0.0
    let mut count = 0
    for j = start; j <= end; j = j + 1 {
      sum = sum + seasonal_series[j][1]
      count = count + 1
    }
    
    let trend_value = sum / count.to_float()
    trend_component = trend_component.concat([trend_value])
  }
  
  assert_eq(trend_component.length(), seasonal_series.length())
  
  // Detrended series
  let mut detrended_series = []
  for i = 0; i < seasonal_series.length(); i = i + 1 {
    let detrended_value = seasonal_series[i][1] - trend_component[i]
    detrended_series = detrended_series.concat([detrended_value])
  }
  
  assert_eq(detrended_series.length(), seasonal_series.length())
  
  // Seasonal component (average pattern)
  let mut seasonal_pattern = []
  for hour = 0; hour < 24; hour = hour + 1 {
    let mut hour_sum = 0.0
    let mut hour_count = 0
    
    for i = hour; i < detrended_series.length(); i = i + 24 {
      hour_sum = hour_sum + detrended_series[i]
      hour_count = hour_count + 1
    }
    
    let seasonal_avg = hour_sum / hour_count.to_float()
    seasonal_pattern = seasonal_pattern.concat([seasonal_avg])
  }
  
  assert_eq(seasonal_pattern.length(), 24)
  
  // Verify seasonal pattern has expected characteristics
  let pattern_min = seasonal_pattern.reduce(|acc, val| if val < acc { val } else { acc }, seasonal_pattern[0])
  let pattern_max = seasonal_pattern.reduce(|acc, val| if val > acc { val } else { acc }, seasonal_pattern[0])
  
  assert_true(pattern_max > pattern_min)  // Should have variation
  assert_true(pattern_max - pattern_min > 10.0)  // Significant variation
}

// Test 7: Time Series Forecasting
test "time series forecasting" {
  // Generate historical time series data
  let base_timestamp = 1640995200000
  let interval_ms = 3600000  // 1 hour interval
  let historical_points = 120  // 5 days of historical data
  
  let mut historical_series = []
  
  for i = 0; i < historical_points; i = i + 1 {
    let timestamp = base_timestamp + (i * interval_ms)
    // Trend + seasonality + noise
    let trend = 100.0 + (i.to_float() * 0.5)
    let seasonal = 10.0 * ((i.to_float() * 2.0 * 3.14159) / 24.0).sin()
    let noise = (i % 7).to_float() - 3.0
    let value = trend + seasonal + noise
    
    historical_series = historical_series.concat([(timestamp, value)])
  }
  
  // Simple linear regression forecasting
  let n = historical_series.length().to_float()
  let x_values = []
  for i = 0; i < historical_series.length(); i = i + 1 {
    x_values = x_values.concat([i.to_float()])
  }
  let y_values = historical_series.map(|(_, value)| value)
  
  let x_sum = x_values.reduce(|acc, val| acc + val, 0.0)
  let y_sum = y_values.reduce(|acc, val| acc + val, 0.0)
  let xy_sum = x_values.reduce(|acc, x| acc + x * y_values[x.to_int()], 0.0)
  let x2_sum = x_values.reduce(|acc, x| acc + x * x, 0.0)
  
  let slope = (n * xy_sum - x_sum * y_sum) / (n * x2_sum - x_sum * x_sum)
  let intercept = (y_sum - slope * x_sum) / n
  
  // Generate forecasts for next 24 hours
  let forecast_points = 24
  let mut forecast_series = []
  
  for i = 0; i < forecast_points; i = i + 1 {
    let forecast_timestamp = base_timestamp + ((historical_points + i) * interval_ms)
    let forecast_x = (historical_points + i).to_float()
    let forecast_value = slope * forecast_x + intercept
    
    forecast_series = forecast_series.concat([(forecast_timestamp, forecast_value)])
  }
  
  assert_eq(forecast_series.length(), 24)
  
  // Verify forecast timestamps are sequential
  for i = 1; i < forecast_series.length(); i = i + 1 {
    let interval = forecast_series[i][0] - forecast_series[i-1][0]
    assert_eq(interval, interval_ms)
  }
  
  // Verify forecast values show trend
  let first_forecast = forecast_series[0][1]
  let last_forecast = forecast_series[forecast_series.length() - 1][1]
  
  assert_true(last_forecast > first_forecast)  // Should be increasing due to positive trend
  
  // Seasonal adjustment forecasting
  let seasonal_period = 24
  let mut seasonal_forecasts = []
  
  for i = 0; i < forecast_points; i = i + 1 {
    let forecast_timestamp = base_timestamp + ((historical_points + i) * interval_ms)
    let forecast_x = (historical_points + i).to_float()
    let trend_forecast = slope * forecast_x + intercept
    
    // Add seasonal component
    let seasonal_index = (historical_points + i) % seasonal_period
    let seasonal_component = 10.0 * ((seasonal_index.to_float() * 2.0 * 3.14159) / 24.0).sin()
    
    let seasonal_forecast = trend_forecast + seasonal_component
    seasonal_forecasts = seasonal_forecasts.concat([(forecast_timestamp, seasonal_forecast)])
  }
  
  assert_eq(seasonal_forecasts.length(), 24)
  
  // Compare simple vs seasonal forecasts
  let simple_first = forecast_series[0][1]
  let seasonal_first = seasonal_forecasts[0][1]
  let simple_last = forecast_series[forecast_series.length() - 1][1]
  let seasonal_last = seasonal_forecasts[seasonal_forecasts.length() - 1][1]
  
  // Seasonal forecasts should have more variation
  let simple_range = simple_last - simple_first
  let seasonal_range = seasonal_last - seasonal_first
  
  assert_true(seasonal_range.abs() >= simple_range.abs())
}

// Test 8: Time Series Compression and Storage
test "time series compression and storage" {
  // Generate high-frequency time series data
  let base_timestamp = 1640995200000
  let interval_ms = 1000  // 1 second interval
  let data_points = 3600  // 1 hour of second-by-second data
  
  let mut high_freq_series = []
  
  for i = 0; i < data_points; i = i + 1 {
    let timestamp = base_timestamp + (i * interval_ms)
    // Generate realistic data with periods of stability and change
    let mut value = 100.0
    
    if i % 60 == 0 {
      value = value + (i % 300).to_float()  // Step changes every minute
    }
    
    value = value + (2.0 * ((i % 10).to_float() - 5.0))  // Small oscillations
    high_freq_series = high_freq_series.concat([(timestamp, value)])
  }
  
  assert_eq(high_freq_series.length(), 3600)
  
  // Delta encoding compression
  let mut delta_encoded = []
  let mut prev_timestamp = high_freq_series[0][0]
  let mut prev_value = high_freq_series[0][1]
  
  // First point stored as-is
  delta_encoded = delta_encoded.concat([high_freq_series[0]])
  
  // Subsequent points stored as deltas
  for i = 1; i < high_freq_series.length(); i = i + 1 {
    let timestamp_delta = high_freq_series[i][0] - prev_timestamp
    let value_delta = high_freq_series[i][1] - prev_value
    
    delta_encoded = delta_encoded.concat([(timestamp_delta, value_delta)])
    prev_timestamp = high_freq_series[i][0]
    prev_value = high_freq_series[i][1]
  }
  
  assert_eq(delta_encoded.length(), 3600)
  
  // Verify delta encoding can be reconstructed
  let mut reconstructed_series = []
  reconstructed_series = reconstructed_series.concat([delta_encoded[0]])
  
  for i = 1; i < delta_encoded.length(); i = i + 1 {
    let prev_timestamp = reconstructed_series[i-1][0]
    let prev_value = reconstructed_series[i-1][1]
    let timestamp = prev_timestamp + delta_encoded[i][0]
    let value = prev_value + delta_encoded[i][1]
    
    reconstructed_series = reconstructed_series.concat([(timestamp, value)])
  }
  
  assert_eq(reconstructed_series.length(), high_freq_series.length())
  
  // Verify reconstruction accuracy
  for i = 0; i < high_freq_series.length(); i = i + 1 {
    assert_eq(high_freq_series[i][0], reconstructed_series[i][0])
    assert_eq(high_freq_series[i][1], reconstructed_series[i][1])
  }
  
  // Simple run-length encoding for constant values
  let mut rle_encoded = []
  let mut current_run = (high_freq_series[0], 1)
  
  for i = 1; i < high_freq_series.length(); i = i + 1 {
    if high_freq_series[i][1] == current_run[0][1] && 
       high_freq_series[i][0] - current_run[0][0] == interval_ms {
      // Continue current run
      current_run = (current_run[0], current_run[1] + 1)
    } else {
      // End current run and start new one
      rle_encoded = rle_encoded.concat([current_run])
      current_run = (high_freq_series[i], 1)
    }
  }
  
  // Add final run
  rle_encoded = rle_encoded.concat([current_run])
  
  // RLE should significantly reduce data size for data with constant periods
  assert_true(rle_encoded.length() < high_freq_series.length())
  
  // Time-based downsampling
  let downsample_interval = 60000  // 1 minute
  let mut downsampled_series = []
  
  let mut window_start = high_freq_series[0][0]
  while window_start < high_freq_series[high_freq_series.length() - 1][0] {
    let window_end = window_start + downsample_interval
    
    // Aggregate data in window
    let window_data = high_freq_series.filter(|(timestamp, _)| {
      timestamp >= window_start && timestamp < window_end
    })
    
    if window_data.length() > 0 {
      let values = window_data.map(|(_, value)| value)
      let avg = values.reduce(|acc, val| acc + val, 0.0) / values.length().to_float()
      let min = values.reduce(|acc, val| if val < acc { val } else { acc }, values[0])
      let max = values.reduce(|acc, val| if val > acc { val } else { acc }, values[0])
      
      downsampled_series = downsampled_series.concat([(
        window_start,
        ("avg", avg),
        ("min", min),
        ("max", max),
        ("count", values.length())
      )])
    }
    
    window_start = window_end
  }
  
  // Should have 60 downsampled points (3600 seconds / 60 seconds)
  assert_eq(downsampled_series.length(), 60)
  
  // Verify downsampling preserves key characteristics
  let original_values = high_freq_series.map(|(_, value)| value)
  let original_min = original_values.reduce(|acc, val| if val < acc { val } else { acc }, original_values[0])
  let original_max = original_values.reduce(|acc, val| if val > acc { val } else { acc }, original_values[0])
  
  let downsampled_mins = downsampled_series.map(|(_, mins)| mins.filter(|(k, _)| k == "min")[0][1])
  let downsampled_maxs = downsampled_series.map(|(_, maxs)| maxs.filter(|(k, _)| k == "max")[0][1])
  
  let downsampled_min = downsampled_mins.reduce(|acc, val| if val < acc { val } else { acc }, downsampled_mins[0])
  let downsampled_max = downsampled_maxs.reduce(|acc, val| if val > acc { val } else { acc }, downsampled_maxs[0])
  
  assert_eq(original_min, downsampled_min)
  assert_eq(original_max, downsampled_max)
}