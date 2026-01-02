// Azimuth Time Series Data Processing Tests
// This file contains test cases for time series data processing and analysis

// Test 1: Time Series Basic Operations
test "time series basic operations" {
  // Test time series creation and basic operations
  let timestamps = [1000, 2000, 3000, 4000, 5000]
  let values = [10.5, 15.2, 12.8, 18.3, 20.1]
  
  // Verify time series length
  assert_eq(timestamps.length(), values.length())
  assert_eq(timestamps.length(), 5)
  
  // Test time series value access
  assert_eq(values[0], 10.5)
  assert_eq(values[4], 20.1)
  
  // Test timestamp ordering
  for i in 1..<timestamps.length() {
    assert_true(timestamps[i] > timestamps[i-1])
  }
  
  // Test time series range extraction
  let start_index = 1
  let end_index = 3
  let mut extracted_values = []
  let mut extracted_timestamps = []
  
  for i in start_index..=end_index {
    extracted_values = extracted_values.push(values[i])
    extracted_timestamps = extracted_timestamps.push(timestamps[i])
  }
  
  assert_eq(extracted_values.length(), 3)
  assert_eq(extracted_values[0], 15.2)
  assert_eq(extracted_values[2], 18.3)
}

// Test 2: Time Series Resampling
test "time series resampling" {
  // Test upsampling (interpolation)
  let original_timestamps = [0, 10, 20, 30]
  let original_values = [10.0, 20.0, 30.0, 40.0]
  
  // Upsample to every 5 units
  let new_timestamps = [0, 5, 10, 15, 20, 25, 30]
  let mut upsampled_values = []
  
  for new_ts in new_timestamps {
    // Linear interpolation
    if new_ts == 0 {
      upsampled_values = upsampled_values.push(original_values[0])
    } else if new_ts == 30 {
      upsampled_values = upsampled_values.push(original_values[3])
    } else {
      // Find surrounding points for interpolation
      let mut lower_index = 0
      let mut upper_index = original_timestamps.length() - 1
      
      for i in 0..<original_timestamps.length() {
        if original_timestamps[i] <= new_ts {
          lower_index = i
        }
        if original_timestamps[i] >= new_ts && upper_index == original_timestamps.length() - 1 {
          upper_index = i
        }
      }
      
      if original_timestamps[lower_index] == new_ts {
        upsampled_values = upsampled_values.push(original_values[lower_index])
      } else {
        let t = (new_ts - original_timestamps[lower_index]).to_float() / 
                (original_timestamps[upper_index] - original_timestamps[lower_index]).to_float()
        let interpolated_value = original_values[lower_index] + 
                                t * (original_values[upper_index] - original_values[lower_index])
        upsampled_values = upsampled_values.push(interpolated_value)
      }
    }
  }
  
  assert_eq(upsampled_values.length(), 7)
  assert_eq(upsampled_values[0], 10.0)
  assert_eq(upsampled_values[1], 15.0)  // Interpolated
  assert_eq(upsampled_values[2], 20.0)
  assert_eq(upsampled_values[3], 25.0)  // Interpolated
  
  // Test downsampling (aggregation)
  let downsample_interval = 20
  let mut downsampled_timestamps = []
  let mut downsampled_values = []
  
  for start_ts in range_step(0, 30, downsample_interval) {
    let end_ts = start_ts + downsample_interval
    
    // Find values in this interval
    let mut interval_values = []
    for i in 0..<original_timestamps.length() {
      if original_timestamps[i] >= start_ts && original_timestamps[i] < end_ts {
        interval_values = interval_values.push(original_values[i])
      }
    }
    
    // Calculate average for this interval
    let mut sum = 0.0
    for value in interval_values {
      sum = sum + value
    }
    let average = sum / interval_values.length().to_float()
    
    downsampled_timestamps = downsampled_timestamps.push(start_ts)
    downsampled_values = downsampled_values.push(average)
  }
  
  assert_eq(downsampled_values.length(), 2)
  assert_eq(downsampled_values[0], 15.0)  // Average of [10.0, 20.0]
  assert_eq(downsampled_values[1], 35.0)  // Average of [30.0, 40.0]
}

// Test 3: Time Series Moving Average
test "time series moving average" {
  // Test simple moving average
  let values = [10, 20, 30, 40, 50, 60, 70, 80]
  let window_size = 3
  let mut moving_averages = []
  
  for i in window_size-1..<values.length() {
    let mut sum = 0
    for j in 0..<window_size {
      sum = sum + values[i - j]
    }
    let average = sum / window_size
    moving_averages = moving_averages.push(average)
  }
  
  assert_eq(moving_averages.length(), 6)
  assert_eq(moving_averages[0], 20)  // (10+20+30)/3
  assert_eq(moving_averages[1], 30)  // (20+30+40)/3
  assert_eq(moving_averages[5], 70)  // (60+70+80)/3
  
  // Test weighted moving average
  let weights = [0.5, 0.3, 0.2]
  let mut weighted_averages = []
  
  for i in 2..<values.length() {
    let weighted_sum = values[i] * weights[0] + 
                      values[i-1] * weights[1] + 
                      values[i-2] * weights[2]
    weighted_averages = weighted_averages.push(weighted_sum)
  }
  
  assert_eq(weighted_averages.length(), 6)
  assert_eq(weighted_averages[0], 30.0)  // 30*0.5 + 20*0.3 + 10*0.2
  assert_eq(weighted_averages[1], 40.0)  // 40*0.5 + 30*0.3 + 20*0.2
}

// Test 4: Time Series Trend Analysis
test "time series trend analysis" {
  // Test linear trend detection
  let timestamps = [1, 2, 3, 4, 5]
  let upward_trend_values = [10, 20, 30, 40, 50]
  let downward_trend_values = [50, 40, 30, 20, 10]
  let flat_trend_values = [25, 25, 25, 25, 25]
  
  // Calculate trend direction
  let mut upward_increments = 0
  let mut upward_decrements = 0
  
  for i in 1..<upward_trend_values.length() {
    if upward_trend_values[i] > upward_trend_values[i-1] {
      upward_increments = upward_increments + 1
    } else if upward_trend_values[i] < upward_trend_values[i-1] {
      upward_decrements = upward_decrements + 1
    }
  }
  
  assert_true(upward_increments > upward_decrements)
  assert_eq(upward_increments, 4)
  assert_eq(upward_decrements, 0)
  
  // Downward trend
  let mut downward_increments = 0
  let mut downward_decrements = 0
  
  for i in 1..<downward_trend_values.length() {
    if downward_trend_values[i] > downward_trend_values[i-1] {
      downward_increments = downward_increments + 1
    } else if downward_trend_values[i] < downward_trend_values[i-1] {
      downward_decrements = downward_decrements + 1
    }
  }
  
  assert_true(downward_decrements > downward_increments)
  assert_eq(downward_increments, 0)
  assert_eq(downward_decrements, 4)
  
  // Flat trend
  let mut flat_increments = 0
  let mut flat_decrements = 0
  let mut flat_equal = 0
  
  for i in 1..<flat_trend_values.length() {
    if flat_trend_values[i] > flat_trend_values[i-1] {
      flat_increments = flat_increments + 1
    } else if flat_trend_values[i] < flat_trend_values[i-1] {
      flat_decrements = flat_decrements + 1
    } else {
      flat_equal = flat_equal + 1
    }
  }
  
  assert_eq(flat_increments, 0)
  assert_eq(flat_decrements, 0)
  assert_eq(flat_equal, 4)
}

// Test 5: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Test statistical anomaly detection
  let normal_values = [10.0, 12.0, 11.0, 13.0, 10.5, 11.5, 12.5, 10.8]
  let anomalous_values = [10.0, 12.0, 50.0, 11.0, 13.0, 0.5, 11.5, 100.0]
  
  // Calculate mean and standard deviation for normal values
  let mut sum = 0.0
  for value in normal_values {
    sum = sum + value
  }
  let mean = sum / normal_values.length().to_float()
  
  let mut variance_sum = 0.0
  for value in normal_values {
    let diff = value - mean
    variance_sum = variance_sum + diff * diff
  }
  let variance = variance_sum / normal_values.length().to_float()
  let std_dev = variance.sqrt()
  
  // Detect anomalies in anomalous_values (values beyond 2 standard deviations)
  let threshold = 2.0 * std_dev
  let mut detected_anomalies = []
  
  for value in anomalous_values {
    let diff = (value - mean).abs()
    if diff > threshold {
      detected_anomalies = detected_anomalies.push(value)
    }
  }
  
  // Should detect the extreme values (50.0, 0.5, 100.0)
  assert_true(detected_anomalies.length() >= 2)
  assert_true(detected_anomalies.contains(50.0))
  assert_true(detected_anomalies.contains(100.0))
}

// Test 6: Time Series Seasonality Detection
test "time series seasonality detection" {
  // Test seasonal pattern detection
  let seasonal_data = [10, 20, 30, 20, 10, 20, 30, 20, 10, 20, 30, 20]
  let season_length = 4
  
  // Extract seasonal patterns
  let mut seasonal_patterns = []
  
  for season_start in range_step(0, seasonal_data.length(), season_length) {
    let mut pattern = []
    for i in 0..<season_length {
      if season_start + i < seasonal_data.length() {
        pattern = pattern.push(seasonal_data[season_start + i])
      }
    }
    if pattern.length() == season_length {
      seasonal_patterns = seasonal_patterns.push(pattern)
    }
  }
  
  assert_eq(seasonal_patterns.length(), 3)
  
  // Verify patterns are similar
  let first_pattern = seasonal_patterns[0]
  let second_pattern = seasonal_patterns[1]
  let third_pattern = seasonal_patterns[2]
  
  assert_eq(first_pattern, [10, 20, 30, 20])
  assert_eq(second_pattern, [10, 20, 30, 20])
  assert_eq(third_pattern, [10, 20, 30, 20])
  
  // Test seasonality strength
  let mut pattern_matches = 0
  for i in 0..<first_pattern.length() {
    if first_pattern[i] == second_pattern[i] && 
       second_pattern[i] == third_pattern[i] {
      pattern_matches = pattern_matches + 1
    }
  }
  
  let seasonality_strength = pattern_matches.to_float() / first_pattern.length().to_float()
  assert_eq(seasonality_strength, 1.0)
}

// Test 7: Time Series Forecasting
test "time series forecasting" {
  // Test simple exponential smoothing forecasting
  let historical_data = [10.0, 12.0, 11.0, 13.0, 14.0, 12.0, 15.0, 16.0]
  let alpha = 0.3  // Smoothing factor
  let forecast_horizon = 3
  
  // Initialize with first value
  let mut smoothed_values = [historical_data[0]]
  
  // Apply exponential smoothing
  for i in 1..<historical_data.length() {
    let smoothed = alpha * historical_data[i] + (1.0 - alpha) * smoothed_values[i-1]
    smoothed_values = smoothed_values.push(smoothed)
  }
  
  // Generate forecasts
  let mut forecasts = []
  let last_smoothed = smoothed_values[smoothed_values.length() - 1]
  
  for i in 0..<forecast_horizon {
    // For simple exponential smoothing, forecast is constant
    forecasts = forecasts.push(last_smoothed)
  }
  
  assert_eq(forecasts.length(), 3)
  
  // Test trend-based forecasting (simple linear extrapolation)
  let trend_data = [10, 15, 20, 25, 30]
  let mut trend_forecasts = []
  
  // Calculate trend (slope)
  let slope = (trend_data[trend_data.length() - 1] - trend_data[0]).to_float() / 
              (trend_data.length() - 1).to_float()
  
  // Generate trend-based forecasts
  let last_value = trend_data[trend_data.length() - 1].to_float()
  for i in 1..=forecast_horizon {
    let forecast = last_value + slope * i.to_float()
    trend_forecasts = trend_forecasts.push(forecast)
  }
  
  assert_eq(trend_forecasts.length(), 3)
  assert_eq(trend_forecasts[0], 35.0)
  assert_eq(trend_forecasts[1], 40.0)
  assert_eq(trend_forecasts[2], 45.0)
}

// Test 8: Time Series Correlation Analysis
test "time series correlation analysis" {
  // Test cross-correlation between two time series
  let series1 = [10, 20, 30, 40, 50]
  let series2 = [15, 25, 35, 45, 55]  // Perfect positive correlation with offset
  
  // Calculate Pearson correlation coefficient
  let n = series1.length().to_float()
  
  // Calculate means
  let mut sum1 = 0.0
  let mut sum2 = 0.0
  for i in 0..<series1.length() {
    sum1 = sum1 + series1[i].to_float()
    sum2 = sum2 + series2[i].to_float()
  }
  let mean1 = sum1 / n
  let mean2 = sum2 / n
  
  // Calculate covariance and variances
  let mut covariance = 0.0
  let mut variance1 = 0.0
  let mut variance2 = 0.0
  
  for i in 0..<series1.length() {
    let diff1 = series1[i].to_float() - mean1
    let diff2 = series2[i].to_float() - mean2
    
    covariance = covariance + diff1 * diff2
    variance1 = variance1 + diff1 * diff1
    variance2 = variance2 + diff2 * diff2
  }
  
  covariance = covariance / n
  variance1 = variance1 / n
  variance2 = variance2 / n
  
  // Calculate correlation coefficient
  let correlation = covariance / (variance1.sqrt() * variance2.sqrt())
  
  // Should be close to 1.0 (perfect positive correlation)
  assert_true(correlation > 0.99)
  
  // Test negative correlation
  let series3 = [50, 40, 30, 20, 10]  // Perfect negative correlation with series1
  
  let mut sum3 = 0.0
  for value in series3 {
    sum3 = sum3 + value.to_float()
  }
  let mean3 = sum3 / n
  
  let mut covariance_neg = 0.0
  let mut variance3 = 0.0
  
  for i in 0..<series1.length() {
    let diff1 = series1[i].to_float() - mean1
    let diff3 = series3[i].to_float() - mean3
    
    covariance_neg = covariance_neg + diff1 * diff3
    variance3 = variance3 + diff3 * diff3
  }
  
  covariance_neg = covariance_neg / n
  variance3 = variance3 / n
  
  let correlation_neg = covariance_neg / (variance1.sqrt() * variance3.sqrt())
  
  // Should be close to -1.0 (perfect negative correlation)
  assert_true(correlation_neg < -0.99)
}

// Test 9: Time Series Compression
test "time series compression" {
  // Test piecewise constant approximation (PAA)
  let original_series = [10, 12, 11, 13, 15, 17, 16, 18, 20, 22]
  let compressed_length = 5
  
  let segment_size = original_series.length() / compressed_length
  let mut compressed_series = []
  
  for i in 0..<compressed_length {
    let start_index = i * segment_size
    let end_index = (i + 1) * segment_size
    
    let mut segment_sum = 0.0
    for j in start_index..<end_index {
      segment_sum = segment_sum + original_series[j].to_float()
    }
    let segment_average = segment_sum / segment_size.to_float()
    compressed_series = compressed_series.push(segment_average)
  }
  
  assert_eq(compressed_series.length(), 5)
  
  // Verify compression preserves general trend
  assert_true(compressed_series[0] < compressed_series[1])
  assert_true(compressed_series[1] < compressed_series[2])
  assert_true(compressed_series[2] < compressed_series[3])
  assert_true(compressed_series[3] < compressed_series[4])
  
  // Test symbolic aggregate approximation (SAX) - simplified version
  let breakpoints = [15.0, 20.0]  // Three regions: <15, 15-20, >20
  let mut sax_symbols = []
  
  for value in compressed_series {
    if value < breakpoints[0] {
      sax_symbols = sax_symbols.push("A")
    } else if value < breakpoints[1] {
      sax_symbols = sax_symbols.push("B")
    } else {
      sax_symbols = sax_symbols.push("C")
    }
  }
  
  assert_eq(sax_symbols.length(), 5)
  assert_eq(sax_symbols[0], "A")
  assert_eq(sax_symbols[2], "B")
  assert_eq(sax_symbols[4], "C")
}

// Test 10: Time Series Alignment
test "time series alignment" {
  // Test dynamic time warping (DTW) - simplified version
  let series1 = [1, 2, 3, 4, 5]
  let series2 = [1, 1, 2, 3, 3, 4, 5, 5]
  
  // Create cost matrix
  let n = series1.length()
  let m = series2.length()
  let mut cost_matrix = []
  
  for i in 0..<n {
    let mut row = []
    for j in 0..<m {
      let cost = (series1[i] - series2[j]).abs().to_float()
      row = row.push(cost)
    }
    cost_matrix = cost_matrix.push(row)
  }
  
  // Calculate cumulative cost matrix
  let mut dtw_matrix = []
  for i in 0..<n {
    let mut row = []
    for j in 0..<m {
      if i == 0 && j == 0 {
        row = row.push(cost_matrix[0][0])
      } else if i == 0 {
        row = row.push(cost_matrix[0][j] + row[j-1])
      } else if j == 0 {
        row = row.push(cost_matrix[i][0] + dtw_matrix[i-1][0])
      } else {
        let min_cost = [dtw_matrix[i-1][j], dtw_matrix[i][j-1], dtw_matrix[i-1][j-1]].min()
        row = row.push(cost_matrix[i][j] + min_cost)
      }
    }
    dtw_matrix = dtw_matrix.push(row)
  }
  
  // DTW distance is the bottom-right element
  let dtw_distance = dtw_matrix[n-1][m-1]
  
  // Should be relatively small since series are similar
  assert_true(dtw_distance < 10.0)
  
  // Test simple alignment with different sampling rates
  let high_freq = [1, 2, 3, 4, 5, 6, 7, 8]
  let low_freq = [1, 3, 5, 7]
  
  // Downsample high frequency to match low frequency
  let downsample_factor = 2
  let mut aligned_high = []
  
  for i in range_step(0, high_freq.length(), downsample_factor) {
    aligned_high = aligned_high.push(high_freq[i])
  }
  
  assert_eq(aligned_high.length(), low_freq.length())
  assert_eq(aligned_high[0], 1)
  assert_eq(aligned_high[1], 3)
  assert_eq(aligned_high[2], 5)
  assert_eq(aligned_high[3], 7)
  
  // Verify alignment
  for i in 0..<aligned_high.length() {
    assert_eq(aligned_high[i], low_freq[i])
  }
}