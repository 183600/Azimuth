// Azimuth Advanced Time Series Operations Tests
// This file contains comprehensive test cases for advanced time series operations in the telemetry system

// Test 1: Time Series Data Aggregation
test "time series data aggregation with multiple windows" {
  // Create time series data points
  let base_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  
  let time_series_data = [
    (base_timestamp, 100.0),
    (base_timestamp + 60000, 120.0),    // +1 minute
    (base_timestamp + 120000, 110.0),   // +2 minutes
    (base_timestamp + 180000, 130.0),   // +3 minutes
    (base_timestamp + 240000, 125.0),   // +4 minutes
    (base_timestamp + 300000, 140.0),   // +5 minutes
    (base_timestamp + 360000, 135.0),   // +6 minutes
    (base_timestamp + 420000, 150.0),   // +7 minutes
    (base_timestamp + 480000, 145.0),   // +8 minutes
    (base_timestamp + 540000, 160.0),   // +9 minutes
    (base_timestamp + 600000, 155.0),   // +10 minutes
    (base_timestamp + 660000, 170.0),   // +11 minutes
  ]
  
  // Test 1-minute window aggregation
  let one_minute_window = 60000 // 1 minute in milliseconds
  let aggregated_1min = azimuth::TimeSeries::aggregate(time_series_data, one_minute_window)
  
  // Verify aggregation results
  assert_eq(aggregated_1min.length(), 12)
  
  // Test 5-minute window aggregation
  let five_minute_window = 300000 // 5 minutes in milliseconds
  let aggregated_5min = azimuth::TimeSeries::aggregate(time_series_data, five_minute_window)
  
  // Verify 5-minute aggregation
  assert_eq(aggregated_5min.length(), 3)
  
  // Verify aggregation calculations
  // First window: [100.0, 120.0, 110.0, 130.0, 125.0] -> avg = 117.0
  assert_eq(aggregated_5min[0].value, 117.0)
  
  // Second window: [140.0, 135.0, 150.0, 145.0, 160.0] -> avg = 146.0
  assert_eq(aggregated_5min[1].value, 146.0)
  
  // Third window: [155.0, 170.0] -> avg = 162.5
  assert_eq(aggregated_5min[2].value, 162.5)
}

// Test 2: Time Series Resampling and Interpolation
test "time series resampling and interpolation" {
  // Create sparse time series data
  let base_timestamp = 1640995200000
  let sparse_data = [
    (base_timestamp, 100.0),
    (base_timestamp + 300000, 150.0),   // +5 minutes
    (base_timestamp + 900000, 200.0),   // +15 minutes
    (base_timestamp + 1200000, 180.0),  // +20 minutes
  ]
  
  // Resample to 1-minute intervals with linear interpolation
  let target_interval = 60000 // 1 minute
  let resampled_data = azimuth::TimeSeries::resample_linear(sparse_data, target_interval)
  
  // Verify resampling
  assert_eq(resampled_data.length(), 21) // 20 minutes + 1 = 21 points
  
  // Verify interpolated values
  // At 2 minutes: should be between 100.0 and 150.0
  let value_at_2min = resampled_data[2].value
  assert_true(value_at_2min > 100.0 && value_at_2min < 150.0)
  
  // At 10 minutes: should be between 150.0 and 200.0
  let value_at_10min = resampled_data[10].value
  assert_true(value_at_10min > 150.0 && value_at_10min < 200.0)
  
  // Test cubic spline interpolation
  let spline_resampled = azimuth::TimeSeries::resample_cubic(sparse_data, target_interval)
  assert_eq(spline_resampled.length(), 21)
  
  // Verify spline smoothness (second derivative should be continuous)
  for i = 2; i < spline_resampled.length() - 2; i = i + 1 {
    let prev_diff = spline_resampled[i].value - spline_resampled[i-1].value
    let curr_diff = spline_resampled[i+1].value - spline_resampled[i].value
    let next_diff = spline_resampled[i+2].value - spline_resampled[i+1].value
    
    // Check for smooth transitions (differences should not vary wildly)
    let diff_change = (curr_diff - prev_diff).abs()
    let next_diff_change = (next_diff - curr_diff).abs()
    assert_true(diff_change < 50.0) // Reasonable change threshold
    assert_true(next_diff_change < 50.0)
  }
}

// Test 3: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Create time series with anomalies
  let base_timestamp = 1640995200000
  let normal_data = [
    (base_timestamp, 100.0),
    (base_timestamp + 60000, 102.0),
    (base_timestamp + 120000, 98.0),
    (base_timestamp + 180000, 101.0),
    (base_timestamp + 240000, 99.0),
    (base_timestamp + 300000, 103.0),
    (base_timestamp + 360000, 97.0),
    (base_timestamp + 420000, 500.0),   // Anomaly: spike
    (base_timestamp + 480000, 102.0),
    (base_timestamp + 540000, 98.0),
    (base_timestamp + 600000, 101.0),
    (base_timestamp + 660000, 10.0),    // Anomaly: dip
    (base_timestamp + 720000, 99.0),
  ]
  
  // Detect anomalies using statistical method
  let anomalies = azimuth::TimeSeries::detect_anomalies_statistical(normal_data, 2.0) // 2 sigma threshold
  
  // Verify anomaly detection
  assert_eq(anomalies.length(), 2)
  
  // Verify anomaly points
  assert_eq(anomalies[0].timestamp, base_timestamp + 420000) // Spike
  assert_eq(anomalies[1].timestamp, base_timestamp + 660000) // Dip
  
  // Test anomaly severity scoring
  let anomaly_scores = azimuth::TimeSeries::calculate_anomaly_scores(normal_data)
  assert_eq(anomaly_scores.length(), normal_data.length())
  
  // Highest scores should be at anomaly points
  let max_score_index = 0
  let max_score = anomaly_scores[0]
  for i = 1; i < anomaly_scores.length(); i = i + 1 {
    if anomaly_scores[i] > max_score {
      max_score = anomaly_scores[i]
      max_score_index = i
    }
  }
  
  // The highest score should be at one of the anomaly points
  assert_true(max_score_index == 7 || max_score_index == 11) // Indices of anomalies
}

// Test 4: Time Series Forecasting
test "time series forecasting with multiple models" {
  // Create historical data
  let base_timestamp = 1640995200000
  let historical_data = [
    (base_timestamp, 100.0),
    (base_timestamp + 86400000, 105.0),  // +1 day
    (base_timestamp + 172800000, 110.0), // +2 days
    (base_timestamp + 259200000, 108.0), // +3 days
    (base_timestamp + 345600000, 115.0), // +4 days
    (base_timestamp + 432000000, 112.0), // +5 days
    (base_timestamp + 518400000, 118.0), // +6 days
    (base_timestamp + 604800000, 120.0), // +7 days
  ]
  
  // Test linear regression forecasting
  let linear_forecast = azimuth::TimeSeries::forecast_linear(historical_data, 3) // 3 days ahead
  assert_eq(linear_forecast.length(), 3)
  
  // Verify trend (should be increasing)
  assert_true(linear_forecast[0].value > 120.0)
  assert_true(linear_forecast[1].value > linear_forecast[0].value)
  assert_true(linear_forecast[2].value > linear_forecast[1].value)
  
  // Test exponential smoothing forecasting
  let ets_forecast = azimuth::TimeSeries::forecast_exponential_smoothing(historical_data, 3, 0.3)
  assert_eq(ets_forecast.length(), 3)
  
  // Test ARIMA forecasting
  let arima_forecast = azimuth::TimeSeries::forecast_arima(historical_data, 3, [1, 1, 1], [1, 1, 1])
  assert_eq(arima_forecast.length(), 3)
  
  // Compare forecast accuracy using historical data
  let train_data = historical_data.slice(0, 6) // First 6 days
  let test_data = historical_data.slice(6, 2)  // Last 2 days
  
  let linear_model = azimuth::TimeSeries::fit_linear(train_data)
  let ets_model = azimuth::TimeSeries::fit_exponential_smoothing(train_data, 0.3)
  let arima_model = azimuth::TimeSeries::fit_arima(train_data, [1, 1, 1], [1, 1, 1])
  
  // Calculate forecast errors
  let linear_error = azimuth::TimeSeries::calculate_mae(linear_model, test_data)
  let ets_error = azimuth::TimeSeries::calculate_mae(ets_model, test_data)
  let arima_error = azimuth::TimeSeries::calculate_mae(arima_model, test_data)
  
  // All models should have reasonable error rates
  assert_true(linear_error < 20.0)
  assert_true(ets_error < 20.0)
  assert_true(arima_error < 20.0)
}

// Test 5: Time Series Seasonality Analysis
test "time series seasonality analysis" {
  // Create data with weekly seasonality
  let base_timestamp = 1640995200000 // Monday
  let seasonal_data = [
    // Week 1
    (base_timestamp, 80.0),               // Monday
    (base_timestamp + 86400000, 85.0),    // Tuesday
    (base_timestamp + 172800000, 90.0),   // Wednesday
    (base_timestamp + 259200000, 95.0),   // Thursday
    (base_timestamp + 345600000, 100.0),  // Friday
    (base_timestamp + 432000000, 60.0),   // Saturday
    (base_timestamp + 518400000, 50.0),   // Sunday
    // Week 2
    (base_timestamp + 604800000, 82.0),   // Monday
    (base_timestamp + 691200000, 87.0),   // Tuesday
    (base_timestamp + 777600000, 92.0),   // Wednesday
    (base_timestamp + 864000000, 97.0),   // Thursday
    (base_timestamp + 950400000, 102.0),  // Friday
    (base_timestamp + 1036800000, 62.0),  // Saturday
    (base_timestamp + 1123200000, 52.0),  // Sunday
  ]
  
  // Detect seasonality
  let seasonality_result = azimuth::TimeSeries::detect_seasonality(seasonal_data, 604800000) // 1 week period
  assert_true(seasonality_result.has_seasonality)
  assert_eq(seasonality_result.period, 604800000)
  assert_true(seasonality_result.strength > 0.5) // Strong seasonality
  
  // Extract seasonal pattern
  let seasonal_pattern = azimuth::TimeSeries::extract_seasonal_pattern(seasonal_data, 604800000)
  assert_eq(seasonal_pattern.length(), 7) // 7 days in a week
  
  // Verify pattern (weekend should have lower values)
  assert_true(seasonal_pattern[5] < seasonal_pattern[4]) // Saturday < Friday
  assert_true(seasonal_pattern[6] < seasonal_pattern[5]) // Sunday < Saturday
  
  // Remove seasonality (deseasonalize)
  let deseasonalized_data = azimuth::TimeSeries::remove_seasonality(seasonal_data, 604800000)
  assert_eq(deseasonalized_data.length(), seasonal_data.length())
  
  // Verify deseasonalized data has less variation
  let original_variance = azimuth::TimeSeries::calculate_variance(seasonal_data)
  let deseasonalized_variance = azimuth::TimeSeries::calculate_variance(deseasonalized_data)
  assert_true(deseasonalized_variance < original_variance)
  
  // Test multiple seasonality detection (daily and weekly)
  let multi_seasonal_data = azimuth::TimeSeries::generate_multi_seasonal_data(
    base_timestamp, 
    14, // 2 weeks
    [86400000, 604800000], // daily and weekly patterns
    [10.0, 20.0] // daily and seasonal amplitudes
  )
  
  let multi_seasonality = azimuth::TimeSeries::detect_multiple_seasonality(multi_seasonal_data)
  assert_eq(multi_seasonality.periods.length(), 2)
  assert_true(multi_seasonality.periods.contains(86400000))  // Daily
  assert_true(multi_seasonality.periods.contains(604800000)) // Weekly
}

// Test 6: Time Series Compression and Decompression
test "time series compression and decompression" {
  // Create high-frequency time series data
  let base_timestamp = 1640995200000
  let high_freq_data = []
  
  // Generate 1440 data points (1 per minute for 24 hours)
  for i = 0; i < 1440; i = i + 1 {
    let timestamp = base_timestamp + i * 60000 // 1 minute intervals
    let value = 100.0 + 10.0 * (i % 60).to_double() / 60.0 + 5.0 * ((i / 60) % 24).to_double() / 24.0
    high_freq_data.push((timestamp, value))
  }
  
  // Test delta compression
  let delta_compressed = azimuth::TimeSeries::compress_delta(high_freq_data)
  let compression_ratio = delta_compressed.length().to_double() / high_freq_data.length().to_double()
  assert_true(compression_ratio < 0.8) // Should achieve at least 20% compression
  
  // Decompress and verify accuracy
  let delta_decompressed = azimuth::TimeSeries::decompress_delta(delta_compressed)
  assert_eq(delta_decompressed.length(), high_freq_data.length())
  
  for i = 0; i < high_freq_data.length(); i = i + 1 {
    assert_eq(delta_decompressed[i].timestamp, high_freq_data[i].timestamp)
    assert_true((delta_decompressed[i].value - high_freq_data[i].value).abs() < 0.001)
  }
  
  // Test swing compression
  let swing_compressed = azimuth::TimeSeries::compress_swing(high_freq_data, 0.1) // 0.1 tolerance
  let swing_compression_ratio = swing_compressed.length().to_double() / high_freq_data.length().to_double()
  assert_true(swing_compression_ratio < 0.5) // Should achieve better compression with tolerance
  
  // Decompress and verify within tolerance
  let swing_decompressed = azimuth::TimeSeries::decompress_swing(swing_compressed)
  assert_eq(swing_decompressed.length(), high_freq_data.length())
  
  for i = 0; i < high_freq_data.length(); i = i + 1 {
    assert_true((swing_decompressed[i].value - high_freq_data[i].value).abs() < 0.1)
  }
  
  // Test adaptive compression (chooses best method based on data characteristics)
  let adaptive_compressed = azimuth::TimeSeries::compress_adaptive(high_freq_data)
  let adaptive_decompressed = azimuth::TimeSeries::decompress_adaptive(adaptive_compressed)
  
  assert_eq(adaptive_decompressed.length(), high_freq_data.length())
  
  // Verify adaptive compression quality
  let adaptive_error = azimuth::TimeSeries::calculate_mse(adaptive_decompressed, high_freq_data)
  assert_true(adaptive_error < 1.0) // Low reconstruction error
}

// Test 7: Time Series Pattern Recognition
test "time series pattern recognition" {
  // Create time series with known patterns
  let base_timestamp = 1640995200000
  let pattern_data = []
  
  // Generate data with multiple patterns
  for i = 0; i < 1000; i = i + 1 {
    let timestamp = base_timestamp + i * 60000 // 1 minute intervals
    let mut value = 100.0
    
    // Add trend pattern
    value = value + 0.01 * i.to_double()
    
    // Add seasonal pattern
    value = value + 10.0 * ((2 * 3.14159 * i.to_double()) / 144.0).sin() // Daily pattern
    
    // Add occasional spikes every 100 points
    if i % 100 == 0 {
      value = value + 50.0
    }
    
    // Add noise
    value = value + 5.0 * ((2 * 3.14159 * i.to_double() / 10.0).sin() * 0.5)
    
    pattern_data.push((timestamp, value))
  }
  
  // Detect trend patterns
  let trend_result = azimuth::TimeSeries::detect_trend(pattern_data)
  assert_eq(trend_result.direction, "increasing")
  assert_true(trend_result.slope > 0.0)
  assert_true(trend_result.confidence > 0.8)
  
  // Detect seasonal patterns
  let seasonal_patterns = azimuth::TimeSeries::detect_seasonal_patterns(pattern_data)
  assert_true(seasonal_patterns.length() > 0)
  
  // Find daily pattern
  let daily_pattern = seasonal_patterns.find(|p| p.period == 86400000) // 1 day
  assert_true(daily_pattern.is_some)
  assert_true(daily_pattern.unwrap.amplitude > 5.0)
  
  // Detect spike patterns
  let spike_patterns = azimuth::TimeSeries::detect_spike_patterns(pattern_data, 3.0) // 3 sigma threshold
  assert_eq(spike_patterns.length(), 10) // Should detect 10 spikes (every 100 points)
  
  // Verify spike positions
  for i = 0; i < 10; i = i + 1 {
    let expected_position = i * 100
    assert_true(spike_patterns[i].position >= expected_position - 5)
    assert_true(spike_patterns[i].position <= expected_position + 5)
    assert_true(spike_patterns[i].magnitude > 40.0)
  }
  
  // Test pattern matching against template
  let template = [
    (0.0, 100.0),
    (1.0, 110.0),
    (2.0, 105.0),
    (3.0, 115.0),
    (4.0, 110.0)
  ]
  
  let matches = azimuth::TimeSeries::match_pattern(pattern_data, template, 0.8) // 80% similarity threshold
  assert_true(matches.length() > 0)
  
  // Verify match quality
  for match_result in matches {
    assert_true(match_result.similarity >= 0.8)
    assert_true(match_result.start_position < pattern_data.length())
    assert_true(match_result.start_position + template.length() <= pattern_data.length())
  }
}

// Test 8: Time Series Downsampling and Upsampling
test "time series downsampling and upsampling" {
  // Create high-frequency time series (1-second intervals)
  let base_timestamp = 1640995200000
  let high_freq_data = []
  
  for i = 0; i < 3600; i = i + 1 { // 1 hour of data
    let timestamp = base_timestamp + i * 1000 // 1 second intervals
    let value = 100.0 + 10.0 * ((2 * 3.14159 * i.to_double()) / 60.0).sin() // 1-minute pattern
    high_freq_data.push((timestamp, value))
  }
  
  // Downsample to 1-minute intervals using averaging
  let downsampled_1min = azimuth::TimeSeries::downsample_average(high_freq_data, 60000) // 1 minute
  assert_eq(downsampled_1min.length(), 60) // 60 minutes in 1 hour
  
  // Verify downsampling accuracy
  for i = 0; i < downsampled_1min.length(); i = i + 1 {
    let start_idx = i * 60
    let end_idx = (i + 1) * 60
    let expected_avg = 0.0
    
    for j = start_idx; j < end_idx; j = j + 1 {
      expected_avg = expected_avg + high_freq_data[j].value
    }
    expected_avg = expected_avg / 60.0
    
    assert_true((downsampled_1min[i].value - expected_avg).abs() < 0.001)
  }
  
  // Downsample using different methods
  let downsampled_max = azimuth::TimeSeries::downsample_max(high_freq_data, 60000)
  let downsampled_min = azimuth::TimeSeries::downsample_min(high_freq_data, 60000)
  let downsampled_first = azimuth::TimeSeries::downsample_first(high_freq_data, 60000)
  let downsampled_last = azimuth::TimeSeries::downsample_last(high_freq_data, 60000)
  
  assert_eq(downsampled_max.length(), 60)
  assert_eq(downsampled_min.length(), 60)
  assert_eq(downsampled_first.length(), 60)
  assert_eq(downsampled_last.length(), 60)
  
  // Verify max/min relationships
  for i = 0; i < 60; i = i + 1 {
    assert_true(downsampled_max[i].value >= downsampled_1min[i].value)
    assert_true(downsampled_min[i].value <= downsampled_1min[i].value)
  }
  
  // Upsample back to 1-second intervals using linear interpolation
  let upsampled = azimuth::TimeSeries::upsample_linear(downsampled_1min, 1000) // 1 second
  assert_eq(upsampled.length(), 3600) // Should match original length
  
  // Calculate upsampling error
  let upsampling_error = azimuth::TimeSeries::calculate_mse(upsampled, high_freq_data)
  assert_true(upsampling_error < 5.0) // Reasonable reconstruction error
  
  // Test upsampling with different interpolation methods
  let upsampled_cubic = azimuth::TimeSeries::upsample_cubic(downsampled_1min, 1000)
  let upsampled_step = azimuth::TimeSeries::upsample_step(downsampled_1min, 1000)
  
  assert_eq(upsampled_cubic.length(), 3600)
  assert_eq(upsampled_step.length(), 3600)
  
  // Compare interpolation quality
  let cubic_error = azimuth::TimeSeries::calculate_mse(upsampled_cubic, high_freq_data)
  let step_error = azimuth::TimeSeries::calculate_mse(upsampled_step, high_freq_data)
  
  // Cubic should be better than step for smooth data
  assert_true(cubic_error < step_error)
}

// Test 9: Time Series Statistical Analysis
test "time series statistical analysis" {
  // Create time series with known statistical properties
  let base_timestamp = 1640995200000
  let statistical_data = []
  
  // Generate data with normal distribution
  let mut sum = 0.0
  let mut sum_sq = 0.0
  
  for i = 0; i < 1000; i = i + 1 {
    let timestamp = base_timestamp + i * 60000 // 1 minute intervals
    // Generate normally distributed data using Box-Muller transform
    let u1 = (i + 1).to_double() / 1001.0
    let u2 = (i * 2 + 1).to_double() / 2001.0
    let z0 = ((-2.0 * u1.ln()).sqrt() * (2.0 * 3.14159 * u2).cos())
    let value = 100.0 + 15.0 * z0 // Mean=100, Std=15
    
    statistical_data.push((timestamp, value))
    sum = sum + value
    sum_sq = sum_sq + value * value
  }
  
  // Calculate basic statistics
  let mean = sum / 1000.0
  let variance = (sum_sq / 1000.0) - (mean * mean)
  let std_dev = variance.sqrt()
  
  // Verify calculated statistics
  let calculated_stats = azimuth::TimeSeries::calculate_statistics(statistical_data)
  assert_true((calculated_stats.mean - mean).abs() < 0.1)
  assert_true((calculated_stats.variance - variance).abs() < 1.0)
  assert_true((calculated_stats.std_dev - std_dev).abs() < 0.5)
  
  // Calculate percentiles
  let percentiles = azimuth::TimeSeries::calculate_percentiles(statistical_data, [25, 50, 75, 90, 95, 99])
  assert_eq(percentiles.length(), 6)
  
  // Verify percentile properties
  assert_true(percentiles[1] >= percentiles[0]) // 50th >= 25th
  assert_true(percentiles[2] >= percentiles[1]) // 75th >= 50th
  assert_true(percentiles[3] >= percentiles[2]) // 90th >= 75th
  
  // Test skewness and kurtosis
  let skewness = azimuth::TimeSeries::calculate_skewness(statistical_data)
  let kurtosis = azimuth::TimeSeries::calculate_kurtosis(statistical_data)
  
  // For normal distribution, skewness should be near 0 and kurtosis near 3
  assert_true(skewness.abs() < 0.5)
  assert_true((kurtosis - 3.0).abs() < 1.0)
  
  // Test autocorrelation
  let autocorr_1 = azimuth::TimeSeries::calculate_autocorrelation(statistical_data, 1) // Lag 1
  let autocorr_10 = azimuth::TimeSeries::calculate_autocorrelation(statistical_data, 10) // Lag 10
  let autocorr_100 = azimuth::TimeSeries::calculate_autocorrelation(statistical_data, 100) // Lag 100
  
  // For random data, autocorrelation should be near 0
  assert_true(autocorr_1.abs() < 0.2)
  assert_true(autocorr_10.abs() < 0.2)
  assert_true(autocorr_100.abs() < 0.2)
  
  // Test stationarity (Augmented Dickey-Fuller test)
  let stationarity_result = azimuth::TimeSeries::test_stationarity(statistical_data)
  assert_true(stationarity_result.is_stationary)
  assert_true(stationarity_result.p_value < 0.05)
}

// Test 10: Time Series Window Functions and Operations
test "time series window functions and operations" {
  // Create time series data
  let base_timestamp = 1640995200000
  let window_data = []
  
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_timestamp + i * 60000 // 1 minute intervals
    let value = 100.0 + i.to_double() + 10.0 * ((2 * 3.14159 * i.to_double()) / 20.0).sin()
    window_data.push((timestamp, value))
  }
  
  // Test moving average with different window sizes
  let ma_5 = azimuth::TimeSeries::moving_average(window_data, 5)
  let ma_10 = azimuth::TimeSeries::moving_average(window_data, 10)
  let ma_20 = azimuth::TimeSeries::moving_average(window_data, 20)
  
  assert_eq(ma_5.length(), window_data.length())
  assert_eq(ma_10.length(), window_data.length())
  assert_eq(ma_20.length(), window_data.length())
  
  // Verify moving average smooths data
  let original_variance = azimuth::TimeSeries::calculate_variance(window_data.map(|(_, v)| v))
  let ma_5_variance = azimuth::TimeSeries::calculate_variance(ma_5.map(|(_, v)| v))
  let ma_10_variance = azimuth::TimeSeries::calculate_variance(ma_10.map(|(_, v)| v))
  let ma_20_variance = azimuth::TimeSeries::calculate_variance(ma_20.map(|(_, v)| v))
  
  assert_true(ma_5_variance < original_variance)
  assert_true(ma_10_variance < ma_5_variance)
  assert_true(ma_20_variance < ma_10_variance)
  
  // Test exponential moving average
  let ema_05 = azimuth::TimeSeries::exponential_moving_average(window_data, 0.5)
  let ema_02 = azimuth::TimeSeries::exponential_moving_average(window_data, 0.2)
  
  assert_eq(ema_05.length(), window_data.length())
  assert_eq(ema_02.length(), window_data.length())
  
  // EMA with smaller alpha should be smoother
  let ema_05_variance = azimuth::TimeSeries::calculate_variance(ema_05.map(|(_, v)| v))
  let ema_02_variance = azimuth::TimeSeries::calculate_variance(ema_02.map(|(_, v)| v))
  assert_true(ema_02_variance < ema_05_variance)
  
  // Test moving standard deviation
  let moving_std = azimuth::TimeSeries::moving_standard_deviation(window_data, 10)
  assert_eq(moving_std.length(), window_data.length())
  
  // Verify all standard deviations are positive
  for std in moving_std.map(|(_, v)| v) {
    assert_true(std >= 0.0)
  }
  
  // Test rate of change (derivative)
  let rate_of_change = azimuth::TimeSeries::rate_of_change(window_data)
  assert_eq(rate_of_change.length(), window_data.length())
  
  // First element should have zero rate of change (no previous point)
  assert_eq(rate_of_change[0].value, 0.0)
  
  // Test cumulative sum
  let cumulative_sum = azimuth::TimeSeries::cumulative_sum(window_data)
  assert_eq(cumulative_sum.length(), window_data.length())
  
  // Verify cumulative sum properties
  for i = 1; i < cumulative_sum.length(); i = i + 1 {
    assert_true(cumulative_sum[i].value > cumulative_sum[i-1].value)
    let expected_diff = window_data[i].value
    let actual_diff = cumulative_sum[i].value - cumulative_sum[i-1].value
    assert_true((actual_diff - expected_diff).abs() < 0.001)
  }
  
  // Test rolling window functions
  let rolling_max = azimuth::TimeSeries::rolling_max(window_data, 5)
  let rolling_min = azimuth::TimeSeries::rolling_min(window_data, 5)
  let rolling_sum = azimuth::TimeSeries::rolling_sum(window_data, 5)
  
  assert_eq(rolling_max.length(), window_data.length())
  assert_eq(rolling_min.length(), window_data.length())
  assert_eq(rolling_sum.length(), window_data.length())
  
  // Verify rolling window relationships
  for i = 0; i < window_data.length(); i = i + 1 {
    assert_true(rolling_max[i].value >= rolling_min[i].value)
    assert_true(rolling_sum[i].value >= rolling_min[i].value * 5.0)
    assert_true(rolling_sum[i].value <= rolling_max[i].value * 5.0)
  }
}