// Azimuth Telemetry System - Time Series Data Processing Tests
// This file contains comprehensive test cases for time series data processing functionality

// Test 1: Basic Time Series Data Operations
test "basic time series data operations" {
  // Test time series manager
  let ts_manager = TimeSeriesManager::new()
  
  // Create time series data
  let timestamps = [
    "2023-01-01T00:00:00Z",
    "2023-01-01T01:00:00Z",
    "2023-01-01T02:00:00Z",
    "2023-01-01T03:00:00Z",
    "2023-01-01T04:00:00Z"
  ]
  
  let values = [10.5, 12.3, 11.8, 13.2, 14.1]
  
  // Create time series
  let ts = TimeSeriesManager::create(ts_manager, timestamps, values, "cpu_usage")
  
  assert_eq(TimeSeriesManager::get_name(ts), "cpu_usage")
  assert_eq(TimeSeriesManager::get_length(ts), 5)
  
  // Test data retrieval
  let first_point = TimeSeriesManager::get_point(ts, 0)
  assert_eq(first_point.timestamp, "2023-01-01T00:00:00Z")
  assert_eq(first_point.value, 10.5)
  
  let last_point = TimeSeriesManager::get_point(ts, 4)
  assert_eq(last_point.timestamp, "2023-01-01T04:00:00Z")
  assert_eq(last_point.value, 14.1)
  
  // Test value range
  let range = TimeSeriesManager::get_value_range(ts)
  assert_eq(range.min, 10.5)
  assert_eq(range.max, 14.1)
  
  // Test time range
  let time_range = TimeSeriesManager::get_time_range(ts)
  assert_eq(time_range.start, "2023-01-01T00:00:00Z")
  assert_eq(time_range.end, "2023-01-01T04:00:00Z")
  
  // Test statistics
  let stats = TimeSeriesManager::get_statistics(ts)
  assert_eq(stats.count, 5)
  assert_eq(stats.mean, 12.38)
  assert_eq(stats.median, 12.3)
  assert_eq(stats.sum, 61.9)
  
  // Test adding points
  TimeSeriesManager::add_point(ts, "2023-01-01T05:00:00Z", 15.2)
  assert_eq(TimeSeriesManager::get_length(ts), 6)
  
  let new_last_point = TimeSeriesManager::get_point(ts, 5)
  assert_eq(new_last_point.timestamp, "2023-01-01T05:00:00Z")
  assert_eq(new_last_point.value, 15.2)
  
  // Test removing points
  TimeSeriesManager::remove_point(ts, 2)  // Remove third point
  assert_eq(TimeSeriesManager::get_length(ts), 5)
  
  let point_after_removal = TimeSeriesManager::get_point(ts, 2)
  assert_eq(point_after_removal.timestamp, "2023-01-01T03:00:00Z")
  assert_eq(point_after_removal.value, 13.2)
  
  // Test clearing time series
  TimeSeriesManager::clear(ts)
  assert_eq(TimeSeriesManager::get_length(ts), 0)
}

// Test 2: Time Series Resampling and Aggregation Tests
test "time series resampling and aggregation operations" {
  // Test resampling manager
  let resample_manager = ResamplingManager::new()
  
  // Create hourly data
  let hourly_timestamps = [
    "2023-01-01T00:00:00Z",
    "2023-01-01T01:00:00Z",
    "2023-01-01T02:00:00Z",
    "2023-01-01T03:00:00Z",
    "2023-01-01T04:00:00Z",
    "2023-01-01T05:00:00Z",
    "2023-01-01T06:00:00Z",
    "2023-01-01T07:00:00Z"
  ]
  
  let hourly_values = [10.0, 12.0, 11.0, 13.0, 14.0, 15.0, 16.0, 18.0]
  
  let hourly_ts = TimeSeriesManager::create(TimeSeriesManager::new(), hourly_timestamps, hourly_values, "hourly_data")
  
  // Test upsampling (hourly to 30-minute)
  let upsampled = ResamplingManager::upsample(resample_manager, hourly_ts, "30m")
  assert_eq(TimeSeriesManager::get_length(upsampled), 15)  // 8 hours * 2 points per hour - 1
  
  // Test downsampling (hourly to 2-hour)
  let downsampled_mean = ResamplingManager::downsample(resample_manager, hourly_ts, "2h", "mean")
  assert_eq(TimeSeriesManager::get_length(downsampled_mean), 4)  // 8 hours / 2 hours per point
  
  // Check downsampled values
  let downsampled_point1 = TimeSeriesManager::get_point(downsampled_mean, 0)
  assert_eq(downsampled_point1.value, 11.0)  // Mean of 10.0 and 12.0
  
  let downsampled_point2 = TimeSeriesManager::get_point(downsampled_mean, 1)
  assert_eq(downsampled_point2.value, 12.0)  // Mean of 11.0 and 13.0
  
  // Test downsampling with different aggregation methods
  let downsampled_sum = ResamplingManager::downsample(resample_manager, hourly_ts, "2h", "sum")
  let downsampled_max = ResamplingManager::downsample(resample_manager, hourly_ts, "2h", "max")
  let downsampled_min = ResamplingManager::downsample(resample_manager, hourly_ts, "2h", "min")
  
  let sum_point1 = TimeSeriesManager::get_point(downsampled_sum, 0)
  assert_eq(sum_point1.value, 22.0)  // Sum of 10.0 and 12.0
  
  let max_point1 = TimeSeriesManager::get_point(downsampled_max, 0)
  assert_eq(max_point1.value, 12.0)  // Max of 10.0 and 12.0
  
  let min_point1 = TimeSeriesManager::get_point(downsampled_min, 0)
  assert_eq(min_point1.value, 10.0)  // Min of 10.0 and 12.0
  
  // Test irregular time series resampling
  let irregular_timestamps = [
    "2023-01-01T00:00:00Z",
    "2023-01-01T00:35:00Z",
    "2023-01-01T01:10:00Z",
    "2023-01-01T01:45:00Z",
    "2023-01-01T02:20:00Z"
  ]
  
  let irregular_values = [10.0, 12.0, 11.0, 13.0, 14.0]
  
  let irregular_ts = TimeSeriesManager::create(TimeSeriesManager::new(), irregular_timestamps, irregular_values, "irregular_data")
  
  // Resample irregular data to regular hourly intervals
  let regularized = ResamplingManager::regularize(resample_manager, irregular_ts, "1h", "linear")
  assert_eq(TimeSeriesManager::get_length(regularized), 3)  // 3 hours
  
  // Test aggregation over sliding windows
  let sliding_window = ResamplingManager::sliding_window(resample_manager, hourly_ts, "3h", "mean")
  assert_eq(TimeSeriesManager::get_length(sliding_window), 6)  // 8 - 3 + 1
  
  let window_point1 = TimeSeriesManager::get_point(sliding_window, 0)
  assert_eq(window_point1.value, 11.0)  // Mean of first 3 values
  
  // Test custom aggregation function
  let custom_agg = ResamplingManager::downsample_with_function(resample_manager, hourly_ts, "2h", (values) => {
    // Custom function: return the difference between max and min
    let max_val = ArrayUtil::max(values)
    let min_val = ArrayUtil::min(values)
    return max_val - min_val
  })
  
  let custom_point1 = TimeSeriesManager::get_point(custom_agg, 0)
  assert_eq(custom_point1.value, 2.0)  // 12.0 - 10.0
}

// Test 3: Time Series Filtering and Smoothing Tests
test "time series filtering and smoothing operations" {
  // Test filter manager
  let filter_manager = FilterManager::new()
  
  // Create noisy time series data
  let timestamps = []
  let noisy_values = []
  
  for i in 0..100 {
    let timestamp = "2023-01-01T" + (i / 60).to_string() + ":" + (i % 60).to_string() + ":00Z"
    ArrayUtil::push(timestamps, timestamp)
    
    // Create a sine wave with noise
    let base_value = 50.0 + 10.0 * MathUtil::sin(i * 0.1)
    let noise = (MathUtil::random() - 0.5) * 5.0  // Random noise between -2.5 and 2.5
    let noisy_value = base_value + noise
    ArrayUtil::push(noisy_values, noisy_value)
  }
  
  let noisy_ts = TimeSeriesManager::create(TimeSeriesManager::new(), timestamps, noisy_values, "noisy_data")
  
  // Test moving average filter
  let ma_filtered = FilterManager::moving_average(filter_manager, noisy_ts, 5)
  assert_eq(TimeSeriesManager::get_length(ma_filtered), 96)  // 100 - 5 + 1
  
  // Check that the filtered data is smoother (lower variance)
  let original_variance = TimeSeriesManager::get_variance(noisy_ts)
  let filtered_variance = TimeSeriesManager::get_variance(ma_filtered)
  assert_true(filtered_variance < original_variance)
  
  // Test exponential smoothing
  let exp_smoothed = FilterManager::exponential_smoothing(filter_manager, noisy_ts, 0.3)
  assert_eq(TimeSeriesManager::get_length(exp_smoothed), 100)
  
  // Check that exponential smoothing reduces noise
  let exp_variance = TimeSeriesManager::get_variance(exp_smoothed)
  assert_true(exp_variance < original_variance)
  
  // Test median filter
  let median_filtered = FilterManager::median_filter(filter_manager, noisy_ts, 3)
  assert_eq(TimeSeriesManager::get_length(median_filtered), 98)  // 100 - 3 + 1
  
  // Median filter should be effective against outliers
  let median_variance = TimeSeriesManager::get_variance(median_filtered)
  assert_true(median_variance < original_variance)
  
  // Test band-pass filter
  let band_passed = FilterManager::band_pass(filter_manager, noisy_ts, 0.05, 0.15)
  assert_eq(TimeSeriesManager::get_length(band_passed), 100)
  
  // Test high-pass filter
  let high_passed = FilterManager::high_pass(filter_manager, noisy_ts, 0.1)
  assert_eq(TimeSeriesManager::get_length(high_passed), 100)
  
  // Test low-pass filter
  let low_passed = FilterManager::low_pass(filter_manager, noisy_ts, 0.1)
  assert_eq(TimeSeriesManager::get_length(low_passed), 100)
  
  // Test time-based filtering
  let time_filtered = FilterManager::filter_by_time_range(
    filter_manager,
    noisy_ts,
    "2023-01-01T00:10:00Z",
    "2023-01-01T00:50:00Z"
  )
  
  // Should contain points from 00:10 to 00:50
  assert_eq(TimeSeriesManager::get_length(time_filtered), 41)
  
  let first_filtered = TimeSeriesManager::get_point(time_filtered, 0)
  assert_eq(first_filtered.timestamp, "2023-01-01T00:10:00Z")
  
  let last_filtered = TimeSeriesManager::get_point(time_filtered, 40)
  assert_eq(last_filtered.timestamp, "2023-01-01T00:50:00Z")
  
  // Test value-based filtering
  let value_filtered = FilterManager::filter_by_value_range(filter_manager, noisy_ts, 45.0, 55.0)
  
  // Should contain only values between 45.0 and 55.0
  for i in 0..TimeSeriesManager::get_length(value_filtered) {
    let point = TimeSeriesManager::get_point(value_filtered, i)
    assert_true(point.value >= 45.0 && point.value <= 55.0)
  }
  
  // Test outlier detection and removal
  let outliers_removed = FilterManager::remove_outliers(filter_manager, noisy_ts, 2.0)  // 2 standard deviations
  
  // The filtered series should have fewer points if outliers were removed
  assert_eq(TimeSeriesManager::get_length(outliers_removed) <= 100)
  
  // Test custom filter function
  let custom_filtered = FilterManager::custom_filter(filter_manager, noisy_ts, (point) => {
    // Keep only points where the value is an integer
    return point.value == MathUtil::floor(point.value)
  })
  
  // Should contain only points with integer values
  for i in 0..TimeSeriesManager::get_length(custom_filtered) {
    let point = TimeSeriesManager::get_point(custom_filtered, i)
    assert_eq(point.value, MathUtil::floor(point.value))
  }
}

// Test 4: Time Series Analysis and Patterns Tests
test "time series analysis and patterns operations" {
  // Test analysis manager
  let analysis_manager = AnalysisManager::new()
  
  // Create seasonal data (daily pattern)
  let seasonal_timestamps = []
  let seasonal_values = []
  
  for day in 0..7 {
    for hour in 0..24 {
      let timestamp = "2023-01-0" + (day + 1).to_string() + "T" + hour.to_string() + ":00:00Z"
      ArrayUtil::push(seasonal_timestamps, timestamp)
      
      // Create a pattern: higher values during business hours (9-17)
      let base_value = 10.0
      if hour >= 9 && hour <= 17 {
        base_value = 20.0 + (hour - 9) * 2.0  // Increasing during business hours
      } else {
        base_value = 5.0  // Lower outside business hours
      }
      
      // Add some random variation
      let noise = (MathUtil::random() - 0.5) * 2.0
      ArrayUtil::push(seasonal_values, base_value + noise)
    }
  }
  
  let seasonal_ts = TimeSeriesManager::create(TimeSeriesManager::new(), seasonal_timestamps, seasonal_values, "seasonal_data")
  
  // Test seasonality detection
  let seasonality = AnalysisManager::detect_seasonality(analysis_manager, seasonal_ts, 24)  // 24-hour seasonality
  assert_true(seasonality.detected)
  assert_eq(seasonality.period, 24)
  assert_true(seasonality.strength > 0.5)  // Strong seasonality
  
  // Test trend detection
  let trend_data = []
  let trend_timestamps = []
  
  for i in 0..100 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(trend_timestamps, actual_timestamp)
    
    // Create an upward trend with some noise
    let trend_value = 10.0 + i * 0.5 + (MathUtil::random() - 0.5) * 2.0
    ArrayUtil::push(trend_data, trend_value)
  }
  
  let trend_ts = TimeSeriesManager::create(TimeSeriesManager::new(), trend_timestamps, trend_data, "trend_data")
  
  let trend = AnalysisManager::detect_trend(analysis_manager, trend_ts)
  assert_true(trend.direction == "increasing")
  assert_true(trend.slope > 0.4)  // Should be close to 0.5
  assert_true(trend.confidence > 0.8)  // High confidence
  
  // Test change point detection
  let change_point_data = []
  let change_point_timestamps = []
  
  // First 50 points: stable around 10
  for i in 0..50 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(change_point_timestamps, actual_timestamp)
    ArrayUtil::push(change_point_data, 10.0 + (MathUtil::random() - 0.5))
  }
  
  // Next 50 points: stable around 20
  for i in 50..100 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(change_point_timestamps, actual_timestamp)
    ArrayUtil::push(change_point_data, 20.0 + (MathUtil::random() - 0.5))
  }
  
  let change_point_ts = TimeSeriesManager::create(TimeSeriesManager::new(), change_point_timestamps, change_point_data, "change_point_data")
  
  let change_points = AnalysisManager::detect_change_points(analysis_manager, change_point_ts)
  assert_eq(change_points.length(), 1)
  assert_true(change_points[0].index >= 45 && change_points[0].index <= 55)  // Around index 50
  
  // Test anomaly detection
  let anomaly_data = []
  let anomaly_timestamps = []
  
  for i in 0..100 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(anomaly_timestamps, actual_timestamp)
    
    // Normal data around 10, with some anomalies
    let value = 10.0 + (MathUtil::random() - 0.5) * 2.0
    
    // Add anomalies at specific points
    if i == 25 {
      value = 25.0  // Spike anomaly
    } else if i == 50 {
      value = -5.0  # Dip anomaly
    } else if i == 75 {
      value = 10.0  # Normal but followed by anomaly
    } else if i == 76 {
      value = 10.0  # Pattern break anomaly
    }
    
    ArrayUtil::push(anomaly_data, value)
  }
  
  let anomaly_ts = TimeSeriesManager::create(TimeSeriesManager::new(), anomaly_timestamps, anomaly_data, "anomaly_data")
  
  let anomalies = AnalysisManager::detect_anomalies(analysis_manager, anomaly_ts, "zscore", 2.0)
  assert_true(anomalies.length() >= 2)  // Should detect at least the spike and dip
  
  // Test autocorrelation
  let autocorr = AnalysisManager::autocorrelation(analysis_manager, seasonal_ts, 24)  // 24-hour lag
  assert_true(autocorr > 0.5)  # High autocorrelation at 24-hour lag
  
  // Test periodicity detection
  let periodicity = AnalysisManager::detect_periodicity(analysis_manager, seasonal_ts)
  assert_eq(periodicity.period, 24)  # Should detect 24-hour periodicity
  assert_true(periodicity.confidence > 0.8)
  
  // Test pattern matching
  let pattern = [15.0, 17.0, 19.0, 21.0, 23.0]  # Increasing pattern
  let matches = AnalysisManager::find_pattern(analysis_manager, seasonal_ts, pattern, 0.8)  # 80% similarity threshold
  assert_true(matches.length() > 0)  # Should find matches in the seasonal data
}

// Test 5: Time Series Forecasting Tests
test "time series forecasting operations" {
  // Test forecasting manager
  let forecast_manager = ForecastingManager::new()
  
  // Create historical data for forecasting
  let historical_timestamps = []
  let historical_values = []
  
  for i in 0..100 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(historical_timestamps, actual_timestamp)
    
    # Create a trend with seasonal pattern
    let trend = 10.0 + i * 0.1
    let seasonal = 5.0 * MathUtil::sin(i * 0.2)  # Seasonal component
    let noise = (MathUtil::random() - 0.5) * 1.0
    
    ArrayUtil::push(historical_values, trend + seasonal + noise)
  }
  
  let historical_ts = TimeSeriesManager::create(TimeSeriesManager::new(), historical_timestamps, historical_values, "historical_data")
  
  # Test simple moving average forecast
  let ma_forecast = ForecastingManager::moving_average_forecast(forecast_manager, historical_ts, 10, 10)
  assert_eq(TimeSeriesManager::get_length(ma_forecast), 10)
  
  # Test exponential smoothing forecast
  let exp_forecast = ForecastingManager::exponential_smoothing_forecast(forecast_manager, historical_ts, 0.3, 10)
  assert_eq(TimeSeriesManager::get_length(exp_forecast), 10)
  
  # Test linear regression forecast
  let linear_forecast = ForecastingManager::linear_regression_forecast(forecast_manager, historical_ts, 10)
  assert_eq(TimeSeriesManager::get_length(linear_forecast), 10)
  
  # Check that the forecast continues the trend
  let last_historical = TimeSeriesManager::get_point(historical_ts, 99)
  let first_forecast = TimeSeriesManager::get_point(linear_forecast, 0)
  assert_true(first_forecast.value > last_historical.value)  # Should continue upward trend
  
  # Test ARIMA forecast
  let arima_forecast = ForecastingManager::arima_forecast(forecast_manager, historical_ts, [1, 1, 1], 10)
  assert_eq(TimeSeriesManager::get_length(arima_forecast), 10)
  
  # Test seasonal forecast
  let seasonal_forecast = ForecastingManager::seasonal_forecast(forecast_manager, historical_ts, 24, 10)
  assert_eq(TimeSeriesManager::get_length(seasonal_forecast), 10)
  
  # Test forecast accuracy evaluation
  # Create test data with known values
  let test_timestamps = []
  let test_values = []
  
  for i in 0..50 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(test_timestamps, actual_timestamp)
    ArrayUtil::push(test_values, 10.0 + i * 0.2)  # Simple linear trend
  }
  
  let test_ts = TimeSeriesManager::create(TimeSeriesManager::new(), test_timestamps, test_values, "test_data")
  
  # Forecast last 10 points using first 40 points
  let training_ts = TimeSeriesManager::slice(test_ts, 0, 40)
  let test_forecast = ForecastingManager::linear_regression_forecast(forecast_manager, training_ts, 10)
  let actual_values = TimeSeriesManager::slice(test_ts, 40, 50)
  
  # Evaluate forecast accuracy
  let accuracy = ForecastingManager::evaluate_forecast(forecast_manager, test_forecast, actual_values)
  assert_true(accuracy.mae < 1.0)  # Mean absolute error should be small
  assert_true(accuracy.rmse < 1.0)  # Root mean square error should be small
  assert_true(accuracy.mape < 10.0)  # Mean absolute percentage error should be small
  
  # Test forecast confidence intervals
  let forecast_with_ci = ForecastingManager::forecast_with_confidence_intervals(
    forecast_manager,
    historical_ts,
    "linear_regression",
    10,
    0.95  # 95% confidence interval
  )
  
  assert_eq(TimeSeriesManager::get_length(forecast_with_ci.forecast), 10)
  assert_eq(forecast_with_ci.lower_bounds.length(), 10)
  assert_eq(forecast_with_ci.upper_bounds.length(), 10)
  
  # Check that confidence intervals make sense
  for i in 0..10 {
    let forecast_point = TimeSeriesManager::get_point(forecast_with_ci.forecast, i)
    let lower_bound = forecast_with_ci.lower_bounds[i]
    let upper_bound = forecast_with_ci.upper_bounds[i]
    
    assert_true(lower_bound <= forecast_point.value)
    assert_true(forecast_point.value <= upper_bound)
  }
  
  # Test ensemble forecasting
  let ensemble_methods = ["moving_average", "exponential_smoothing", "linear_regression"]
  let ensemble_forecast = ForecastingManager::ensemble_forecast(forecast_manager, historical_ts, ensemble_methods, 10)
  assert_eq(TimeSeriesManager::get_length(ensemble_forecast), 10)
  
  # Test forecast combination weights
  let weights = [0.3, 0.4, 0.3]
  let weighted_forecast = ForecastingManager::weighted_forecast(forecast_manager, historical_ts, ensemble_methods, weights, 10)
  assert_eq(TimeSeriesManager::get_length(weighted_forecast), 10)
}

// Test 6: Time Series Decomposition Tests
test "time series decomposition operations" {
  # Test decomposition manager
  let decomposition_manager = DecompositionManager::new()
  
  # Create time series with trend, seasonal, and residual components
  let decomp_timestamps = []
  let decomp_values = []
  
  for i in 0..100 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(decomp_timestamps, actual_timestamp)
    
    # Trend component (increasing)
    let trend = 10.0 + i * 0.1
    
    # Seasonal component (24-hour cycle)
    let seasonal = 5.0 * MathUtil::sin(i * 0.26)  # 2π/24 ≈ 0.26
    
    # Residual component (noise)
    let residual = (MathUtil::random() - 0.5) * 2.0
    
    ArrayUtil::push(decomp_values, trend + seasonal + residual)
  }
  
  let decomp_ts = TimeSeriesManager::create(TimeSeriesManager::new(), decomp_timestamps, decomp_values, "decomp_data")
  
  # Test additive decomposition
  let additive_decomp = DecompositionManager::additive_decomposition(decomposition_manager, decomp_ts, 24)
  
  assert_eq(TimeSeriesManager::get_length(additive_decomp.trend), 100)
  assert_eq(TimeSeriesManager::get_length(additive_decomp.seasonal), 100)
  assert_eq(TimeSeriesManager::get_length(additive_decomp.residual), 100)
  
  # Check that components add up to original
  for i in 0..100 {
    let original = TimeSeriesManager::get_point(decomp_ts, i).value
    let trend = TimeSeriesManager::get_point(additive_decomp.trend, i).value
    let seasonal = TimeSeriesManager::get_point(additive_decomp.seasonal, i).value
    let residual = TimeSeriesManager::get_point(additive_decomp.residual, i).value
    
    assert_true(MathUtil::abs((trend + seasonal + residual) - original) < 0.001)
  }
  
  # Test multiplicative decomposition
  let mult_decomp = DecompositionManager::multiplicative_decomposition(decomposition_manager, decomp_ts, 24)
  
  assert_eq(TimeSeriesManager::get_length(mult_decomp.trend), 100)
  assert_eq(TimeSeriesManager::get_length(mult_decomp.seasonal), 100)
  assert_eq(TimeSeriesManager::get_length(mult_decomp.residual), 100)
  
  # Check that components multiply to original
  for i in 0..100 {
    let original = TimeSeriesManager::get_point(decomp_ts, i).value
    let trend = TimeSeriesManager::get_point(mult_decomp.trend, i).value
    let seasonal = TimeSeriesManager::get_point(mult_decomp.seasonal, i).value
    let residual = TimeSeriesManager::get_point(mult_decomp.residual, i).value
    
    assert_true(MathUtil::abs((trend * seasonal * residual) - original) < 0.001)
  }
  
  # Test STL decomposition (Seasonal and Trend decomposition using Loess)
  let stl_decomp = DecompositionManager::stl_decomposition(decomposition_manager, decomp_ts, 24)
  
  assert_eq(TimeSeriesManager::get_length(stl_decomp.trend), 100)
  assert_eq(TimeSeriesManager::get_length(stl_decomp.seasonal), 100)
  assert_eq(TimeSeriesManager::get_length(stl_decomp.residual), 100)
  
  # Test seasonal strength
  let seasonal_strength = DecompositionManager::seasonal_strength(decomposition_manager, additive_decomp)
  assert_true(seasonal_strength > 0.5)  # Should have strong seasonality
  
  # Test trend strength
  let trend_strength = DecompositionManager::trend_strength(decomposition_manager, additive_decomp)
  assert_true(trend_strength > 0.5)  # Should have strong trend
  
  # Test reconstruction from components
  let reconstructed = DecompositionManager::reconstruct_from_components(
    decomposition_manager,
    additive_decomp.trend,
    additive_decomp.seasonal,
    additive_decomp.residual,
    "additive"
  )
  
  assert_eq(TimeSeriesManager::get_length(reconstructed), 100)
  
  # Check that reconstruction matches original
  for i in 0..100 {
    let original = TimeSeriesManager::get_point(decomp_ts, i).value
    let reconstructed_value = TimeSeriesManager::get_point(reconstructed, i).value
    
    assert_true(MathUtil::abs(reconstructed_value - original) < 0.001)
  }
  
  # Test component analysis
  let trend_analysis = DecompositionManager::analyze_component(decomposition_manager, additive_decomp.trend)
  assert_eq(trend_analysis.direction, "increasing")
  assert_true(trend_analysis.slope > 0.05)
  
  let seasonal_analysis = DecompositionManager::analyze_component(decomposition_manager, additive_decomp.seasonal)
  assert_eq(seasonal_analysis.period, 24)
  assert_true(seasonal_analysis.amplitude > 4.0)  # Should be close to 5.0
  
  let residual_analysis = DecompositionManager::analyze_component(decomposition_manager, additive_decomp.residual)
  assert_true(residual_analysis.mean < 0.5)  # Should be close to 0
  assert_true(residual_analysis.std_dev < 1.5)  # Should be close to original noise level
}

// Test 7: Time Series Transformation Tests
test "time series transformation operations" {
  # Test transformation manager
  let transform_manager = TransformationManager::new()
  
  # Create time series data
  let transform_timestamps = []
  let transform_values = []
  
  for i in 0..100 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(transform_timestamps, actual_timestamp)
    
    # Create data with exponential growth
    let value = 10.0 * MathUtil::exp(i * 0.01)
    ArrayUtil::push(transform_values, value)
  }
  
  let transform_ts = TimeSeriesManager::create(TimeSeriesManager::new(), transform_timestamps, transform_values, "transform_data")
  
  # Test logarithmic transformation
  let log_transformed = TransformationManager::log_transform(transform_manager, transform_ts)
  assert_eq(TimeSeriesManager::get_length(log_transformed), 100)
  
  # Check that log transformation makes the data more linear
  let original_correlation = TransformationManager::calculate_linearity(transform_manager, transform_ts)
  let log_correlation = TransformationManager::calculate_linearity(transform_manager, log_transformed)
  assert_true(log_correlation > original_correlation)
  
  # Test square root transformation
  let sqrt_transformed = TransformationManager::sqrt_transform(transform_manager, transform_ts)
  assert_eq(TimeSeriesManager::get_length(sqrt_transformed), 100)
  
  # Test Box-Cox transformation
  let box_cox_transformed = TransformationManager::box_cox_transform(transform_manager, transform_ts, 0.5)
  assert_eq(TimeSeriesManager::get_length(box_cox_transformed), 100)
  
  # Test differencing
  let differenced = TransformationManager::difference(transform_manager, transform_ts, 1)
  assert_eq(TimeSeriesManager::get_length(differenced), 99)  # One less point after differencing
  
  # Test seasonal differencing
  let seasonal_differenced = TransformationManager::seasonal_difference(transform_manager, transform_ts, 24)
  assert_eq(TimeSeriesManager::get_length(seasonal_differenced), 76)  # 100 - 24
  
  # Test normalization
  let normalized = TransformationManager::normalize(transform_manager, transform_ts)
  assert_eq(TimeSeriesManager::get_length(normalized), 100)
  
  # Check that normalized data has mean 0 and std dev 1
  let normalized_stats = TimeSeriesManager::get_statistics(normalized)
  assert_true(MathUtil::abs(normalized_stats.mean) < 0.001)
  assert_true(MathUtil::abs(normalized_stats.std_dev - 1.0) < 0.001)
  
  # Test min-max scaling
  let min_max_scaled = TransformationManager::min_max_scale(transform_manager, transform_ts, 0.0, 1.0)
  assert_eq(TimeSeriesManager::get_length(min_max_scaled), 100)
  
  # Check that scaled data is in [0, 1] range
  let min_max_stats = TimeSeriesManager::get_statistics(min_max_scaled)
  assert_true(min_max_stats.min >= 0.0 && min_max_stats.min <= 0.001)
  assert_true(min_max_stats.max <= 1.0 && min_max_stats.max >= 0.999)
  
  # Test robust scaling (using median and IQR)
  let robust_scaled = TransformationManager::robust_scale(transform_manager, transform_ts)
  assert_eq(TimeSeriesManager::get_length(robust_scaled), 100)
  
  # Test unit root test
  let unit_root_result = TransformationManager::unit_root_test(transform_manager, transform_ts)
  assert_true(unit_root_result.test_statistic != null)
  assert_true(unit_root_result.p_value != null)
  
  # Test inverse transformations
  let log_restored = TransformationManager::inverse_log_transform(transform_manager, log_transformed)
  assert_eq(TimeSeriesManager::get_length(log_restored), 100)
  
  # Check that inverse transformation restores original values
  for i in 0..100 {
    let original = TimeSeriesManager::get_point(transform_ts, i).value
    let restored = TimeSeriesManager::get_point(log_restored, i).value
    assert_true(MathUtil::abs(restored - original) < 0.001)
  }
  
  # Test custom transformation
  let custom_transformed = TransformationManager::custom_transform(
    transform_manager,
    transform_ts,
    (value) => { MathUtil::pow(value, 2) }  # Square transformation
  )
  
  assert_eq(TimeSeriesManager::get_length(custom_transformed), 100)
  
  # Check that custom transformation works correctly
  for i in 0..100 {
    let original = TimeSeriesManager::get_point(transform_ts, i).value
    let transformed = TimeSeriesManager::get_point(custom_transformed, i).value
    assert_true(MathUtil::abs(transformed - MathUtil::pow(original, 2)) < 0.001)
  }
}

// Test 8: Time Series Windowing and Segmentation Tests
test "time series windowing and segmentation operations" {
  # Test windowing manager
  let windowing_manager = WindowingManager::new()
  
  # Create time series data
  let window_timestamps = []
  let window_values = []
  
  for i in 0..100 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(window_timestamps, actual_timestamp)
    ArrayUtil::push(window_values, 10.0 + i * 0.1 + (MathUtil::random() - 0.5))
  }
  
  let window_ts = TimeSeriesManager::create(TimeSeriesManager::new(), window_timestamps, window_values, "window_data")
  
  # Test fixed-size windowing
  let fixed_windows = WindowingManager::fixed_size_windows(windowing_manager, window_ts, 10)
  assert_eq(fixed_windows.length(), 10)  # 100 / 10
  
  # Check that each window has the correct size
  for window in fixed_windows {
    assert_eq(TimeSeriesManager::get_length(window), 10)
  }
  
  # Test sliding windows
  let sliding_windows = WindowingManager::sliding_windows(windowing_manager, window_ts, 10, 5)
  assert_eq(sliding_windows.length(), 19)  # (100 - 10) / 5 + 1
  
  # Check that each window has the correct size
  for window in sliding_windows {
    assert_eq(TimeSeriesManager::get_length(window), 10)
  }
  
  # Test time-based windows
  let time_windows = WindowingManager::time_based_windows(windowing_manager, window_ts, "24h")
  assert_eq(time_windows.length(), 5)  # 100 hours / 24 hours per window (rounded)
  
  # Test variable-length windows based on conditions
  let variable_windows = WindowingManager::variable_length_windows(
    windowing_manager,
    window_ts,
    (window) => {
      # Create windows until the cumulative value exceeds 50
      let sum = TimeSeriesManager::sum(window)
      return sum < 50.0
    }
  )
  
  assert_true(variable_windows.length() > 0)
  
  # Test overlapping windows
  let overlapping_windows = WindowingManager::overlapping_windows(windowing_manager, window_ts, 10, 5)
  assert_eq(overlapping_windows.length(), 19)  # Same as sliding windows
  
  # Check that windows overlap correctly
  for i in 1..overlapping_windows.length() - 1 {
    let prev_window = overlapping_windows[i - 1]
    let curr_window = overlapping_windows[i]
    
    # Last 5 points of previous window should be first 5 points of current window
    for j in 0..5 {
      let prev_point = TimeSeriesManager::get_point(prev_window, 10 - 5 + j)
      let curr_point = TimeSeriesManager::get_point(curr_window, j)
      assert_eq(prev_point.timestamp, curr_point.timestamp)
      assert_eq(prev_point.value, curr_point.value)
    }
  }
  
  # Test segmentation by change points
  let change_point_data = []
  let change_point_timestamps = []
  
  # First segment: values around 10
  for i in 0..30 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(change_point_timestamps, actual_timestamp)
    ArrayUtil::push(change_point_data, 10.0 + (MathUtil::random() - 0.5))
  }
  
  # Second segment: values around 20
  for i in 30..60 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(change_point_timestamps, actual_timestamp)
    ArrayUtil::push(change_point_data, 20.0 + (MathUtil::random() - 0.5))
  }
  
  # Third segment: values around 15
  for i in 60..100 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(change_point_timestamps, actual_timestamp)
    ArrayUtil::push(change_point_data, 15.0 + (MathUtil::random() - 0.5))
  }
  
  let change_point_ts = TimeSeriesManager::create(TimeSeriesManager::new(), change_point_timestamps, change_point_data, "change_point_data")
  
  let segments = WindowingManager::segment_by_change_points(windowing_manager, change_point_ts, 2.0)
  assert_eq(segments.length(), 3)  # Should detect 3 segments
  
  # Check that each segment has the correct mean
  let segment1_mean = TimeSeriesManager::mean(segments[0])
  let segment2_mean = TimeSeriesManager::mean(segments[1])
  let segment3_mean = TimeSeriesManager::mean(segments[2])
  
  assert_true(segment1_mean > 9.0 && segment1_mean < 11.0)
  assert_true(segment2_mean > 19.0 && segment2_mean < 21.0)
  assert_true(segment3_mean > 14.0 && segment3_mean < 16.0)
  
  # Test window statistics
  let window_stats = WindowingManager::window_statistics(windowing_manager, fixed_windows)
  assert_eq(window_stats.length(), fixed_windows.length())
  
  # Check that each window has statistics
  for stats in window_stats {
    assert_true(stats.count == 10)
    assert_true(stats.mean != null)
    assert_true(stats.std_dev != null)
    assert_true(stats.min != null)
    assert_true(stats.max != null)
  }
}

// Test 9: Time Series Compression Tests
test "time series compression operations" {
  # Test compression manager
  let compression_manager = CompressionManager::new()
  
  # Create time series data
  let compression_timestamps = []
  let compression_values = []
  
  for i in 0..1000 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_minutes(timestamp, i)
    ArrayUtil::push(compression_timestamps, actual_timestamp)
    
    # Create data with some patterns that can be compressed
    if i % 10 < 8 {
      # Mostly linear with small variations
      ArrayUtil::push(compression_values, 10.0 + i * 0.01 + (MathUtil::random() - 0.5) * 0.1)
    } else {
      # Occasional outliers
      ArrayUtil::push(compression_values, 10.0 + i * 0.01 + (MathUtil::random() - 0.5) * 2.0)
    }
  }
  
  let compression_ts = TimeSeriesManager::create(TimeSeriesManager::new(), compression_timestamps, compression_values, "compression_data")
  
  # Test piecewise constant approximation (PAA)
  let paa_compressed = CompressionManager::piecewise_constant_approximation(compression_manager, compression_ts, 100)
  assert_eq(TimeSeriesManager::get_length(paa_compressed), 100)
  
  # Test piecewise aggregate approximation (PAA)
  let paa_agg_compressed = CompressionManager::piecewise_aggregate_approximation(compression_manager, compression_ts, 100)
  assert_eq(TimeSeriesManager::get_length(paa_agg_compressed), 100)
  
  # Test symbolic aggregate approximation (SAX)
  let sax_compressed = CompressionManager::symbolic_aggregate_approximation(compression_manager, compression_ts, 100, 8)
  assert_eq(TimeShellManager::get_length(sax_compressed), 100)
  
  # Check that SAX produces symbols
  for i in 0..TimeSeriesManager::get_length(sax_compressed) {
    let point = TimeSeriesManager::get_point(sax_compressed, i)
    assert_true(point.value is String)  # Should be a symbol, not a number
  }
  
  # Test adaptive piecewise constant approximation
  let adaptive_compressed = CompressionManager::adaptive_piecewise_constant(compression_manager, compression_ts, 0.5)
  assert_true(TimeSeriesManager::get_length(adaptive_compressed) < 1000)
  assert_true(TimeSeriesManager::get_length(adaptive_compressed) > 100)
  
  # Test compression ratio
  let paa_ratio = CompressionManager::compression_ratio(compression_manager, compression_ts, paa_compressed)
  assert_eq(paa_ratio, 0.1)  # 100 / 1000
  
  # Test compression error
  let paa_error = CompressionManager::compression_error(compression_manager, compression_ts, paa_compressed)
  assert_true(paa_error > 0.0)  # Should have some error
  assert_true(paa_error < 1.0)  # But not too much
  
  # Test dynamic time warping (DTW) based compression
  let dtw_compressed = CompressionManager::dtw_compression(compression_manager, compression_ts, 0.1)
  assert_true(TimeSeriesManager::get_length(dtw_compressed) < 1000)
  
  # Test perceptually important points (PIP) compression
  let pip_compressed = CompressionManager::perceptually_important_points(compression_manager, compression_ts, 100)
  assert_eq(TimeSeriesManager::get_length(pip_compressed), 100)
  
  # Test compression with error bound
  let error_bound_compressed = CompressionManager::compress_with_error_bound(compression_manager, compression_ts, 0.1)
  assert_true(TimeSeriesManager::get_length(error_bound_compressed) < 1000)
  
  # Verify that error bound is respected
  let max_error = CompressionManager::max_compression_error(compression_manager, compression_ts, error_bound_compressed)
  assert_true(max_error <= 0.1)
  
  # Test decompression
  let paa_decompressed = CompressionManager::decompress_paa(compression_manager, paa_compressed, compression_ts)
  assert_eq(TimeSeriesManager::get_length(paa_decompressed), 1000)
  
  # Test compression quality metrics
  let quality_metrics = CompressionManager::compression_quality_metrics(compression_manager, compression_ts, paa_compressed)
  assert_true(quality_metrics.compression_ratio > 0.0)
  assert_true(quality_metrics.reconstruction_error >= 0.0)
  assert_true(quality_metrics.signal_to_noise_ratio >= 0.0)
  assert_true(quality_metrics.mean_squared_error >= 0.0)
  
  # Test multi-resolution compression
  let multi_res_compressed = CompressionManager::multi_resolution_compression(compression_manager, compression_ts, [100, 50, 25])
  assert_eq(multi_res_compressed.length(), 3)
  assert_eq(TimeSeriesManager::get_length(multi_res_compressed[0]), 100)
  assert_eq(TimeSeriesManager::get_length(multi_res_compressed[1]), 50)
  assert_eq(TimeSeriesManager::get_length(multi_res_compressed[2]), 25)
}

// Test 10: Time Series Visualization Tests
test "time series visualization operations" {
  # Test visualization manager
  let viz_manager = VisualizationManager::new()
  
  # Create time series data for visualization
  let viz_timestamps = []
  let viz_values = []
  
  for i in 0..100 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(viz_timestamps, actual_timestamp)
    ArrayUtil::push(viz_values, 10.0 + 5.0 * MathUtil::sin(i * 0.1) + (MathUtil::random() - 0.5))
  }
  
  let viz_ts = TimeSeriesManager::create(TimeSeriesManager::new(), viz_timestamps, viz_values, "viz_data")
  
  # Test line chart generation
  let line_chart = VisualizationManager::line_chart(viz_manager, viz_ts, {
    "title": "Time Series Line Chart",
    "x_label": "Time",
    "y_label": "Value",
    "width": 800,
    "height": 400
  })
  
  assert_true(line_chart.contains("title"))
  assert_true(line_chart.contains("Time Series Line Chart"))
  
  # Test scatter plot generation
  let scatter_plot = VisualizationManager::scatter_plot(viz_manager, viz_ts, {
    "title": "Time Series Scatter Plot",
    "x_label": "Time",
    "y_label": "Value",
    "width": 800,
    "height": 400
  })
  
  assert_true(scatter_plot.contains("title"))
  assert_true(scatter_plot.contains("Time Series Scatter Plot"))
  
  # Test histogram generation
  let histogram = VisualizationManager::histogram(viz_manager, viz_ts, {
    "title": "Value Distribution",
    "x_label": "Value",
    "y_label": "Frequency",
    "bins": 20,
    "width": 600,
    "height": 400
  })
  
  assert_true(histogram.contains("title"))
  assert_true(histogram.contains("Value Distribution"))
  
  # Test box plot generation
  let box_plot = VisualizationManager::box_plot(viz_manager, viz_ts, {
    "title": "Value Box Plot",
    "y_label": "Value",
    "width": 400,
    "height": 600
  })
  
  assert_true(box_plot.contains("title"))
  assert_true(box_plot.contains("Value Box Plot"))
  
  # Test heat map generation
  let heat_map = VisualizationManager::heat_map(viz_manager, viz_ts, {
    "title": "Time Series Heat Map",
    "x_label": "Time",
    "y_label": "Value",
    "width": 800,
    "height": 400
  })
  
  assert_true(heat_map.contains("title"))
  assert_true(heat_map.contains("Time Series Heat Map"))
  
  # Test multiple series visualization
  let viz2_timestamps = []
  let viz2_values = []
  
  for i in 0..100 {
    let timestamp = "2023-01-01T00:00:00Z"
    let actual_timestamp = TimeUtil::add_hours(timestamp, i)
    ArrayUtil::push(viz2_timestamps, actual_timestamp)
    ArrayUtil::push(viz2_values, 15.0 + 3.0 * MathUtil::cos(i * 0.1) + (MathUtil::random() - 0.5))
  }
  
  let viz2_ts = TimeSeriesManager::create(TimeSeriesManager::new(), viz2_timestamps, viz2_values, "viz2_data")
  
  let multi_series_chart = VisualizationManager::multi_series_line_chart(viz_manager, [viz_ts, viz2_ts], {
    "title": "Multiple Time Series",
    "x_label": "Time",
    "y_label": "Value",
    "series_names": ["Series 1", "Series 2"],
    "width": 800,
    "height": 400
  })
  
  assert_true(multi_series_chart.contains("title"))
  assert_true(multi_series_chart.contains("Multiple Time Series"))
  assert_true(multi_series_chart.contains("Series 1"))
  assert_true(multi_series_chart.contains("Series 2"))
  
  # Test decomposition visualization
  let decomp_ts = TimeSeriesManager::create(TimeSeriesManager::new(), viz_timestamps, viz_values, "decomp_viz_data")
  let decomp = DecompositionManager::additive_decomposition(DecompositionManager::new(), decomp_ts, 24)
  
  let decomp_chart = VisualizationManager::decomposition_chart(viz_manager, decomp, {
    "title": "Time Series Decomposition",
    "width": 800,
    "height": 600
  })
  
  assert_true(decomp_chart.contains("title"))
  assert_true(decomp_chart.contains("Time Series Decomposition"))
  
  # Test forecast visualization
  let forecast_ts = TimeSeriesManager::create(TimeSeriesManager::new(), viz_timestamps, viz_values, "forecast_viz_data")
  let forecast = ForecastingManager::linear_regression_forecast(ForecastingManager::new(), forecast_ts, 10)
  
  let forecast_chart = VisualizationManager::forecast_chart(viz_manager, forecast_ts, forecast, {
    "title": "Time Series Forecast",
    "x_label": "Time",
    "y_label": "Value",
    "width": 800,
    "height": 400
  })
  
  assert_true(forecast_chart.contains("title"))
  assert_true(forecast_chart.contains("Time Series Forecast"))
  
  # Test anomaly visualization
  let anomaly_ts = TimeSeriesManager::create(TimeSeriesManager::new(), viz_timestamps, viz_values, "anomaly_viz_data")
  
  # Add some anomalies
  TimeSeriesManager::set_point(anomaly_ts, 25, "2023-01-02T01:00:00Z", 25.0)  # Spike
  TimeSeriesManager::set_point(anomaly_ts, 50, "2023-01-03T02:00:00Z", -5.0)  # Dip
  
  let anomalies = AnalysisManager::detect_anomalies(AnalysisManager::new(), anomaly_ts, "zscore", 2.0)
  
  let anomaly_chart = VisualizationManager::anomaly_chart(viz_manager, anomaly_ts, anomalies, {
    "title": "Anomaly Detection",
    "x_label": "Time",
    "y_label": "Value",
    "width": 800,
    "height": 400
  })
  
  assert_true(anomaly_chart.contains("title"))
  assert_true(anomaly_chart.contains("Anomaly Detection"))
  
  # Test interactive visualization
  let interactive_chart = VisualizationManager::interactive_chart(viz_manager, viz_ts, {
    "title": "Interactive Time Series",
    "x_label": "Time",
    "y_label": "Value",
    "width": 800,
    "height": 400,
    "zoom": true,
    "pan": true,
    "tooltip": true
  })
  
  assert_true(interactive_chart.contains("title"))
  assert_true(interactive_chart.contains("Interactive Time Series"))
  assert_true(interactive_chart.contains("zoom"))
  assert_true(interactive_chart.contains("pan"))
  assert_true(interactive_chart.contains("tooltip"))
  
  # Test dashboard generation
  let dashboard = VisualizationManager::dashboard(viz_manager, {
    "title": "Time Series Dashboard",
    "width": 1200,
    "height": 800,
    "charts": [
      {
        "type": "line",
        "data": viz_ts,
        "position": {"x": 0, "y": 0, "width": 600, "height": 300}
      },
      {
        "type": "histogram",
        "data": viz_ts,
        "position": {"x": 600, "y": 0, "width": 600, "height": 300}
      },
      {
        "type": "box",
        "data": viz_ts,
        "position": {"x": 0, "y": 300, "width": 600, "height": 250}
      },
      {
        "type": "heatmap",
        "data": viz_ts,
        "position": {"x": 600, "y": 300, "width": 600, "height": 250}
      }
    ]
  })
  
  assert_true(dashboard.contains("title"))
  assert_true(dashboard.contains("Time Series Dashboard"))
}