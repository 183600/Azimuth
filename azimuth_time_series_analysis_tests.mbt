// Azimuth Telemetry System - Time Series Analysis Tests
// This file contains test cases for time series analysis functionality

// Test 1: Basic Time Series Operations
test "basic time series operations" {
  let timestamps = [1000L, 2000L, 3000L, 4000L, 5000L]
  let values = [10.0, 20.0, 15.0, 25.0, 30.0]
  
  let time_series = TimeSeries::new(timestamps, values)
  
  assert_eq(TimeSeries::length(time_series), 5)
  assert_eq(TimeSeries::get_timestamp(time_series, 0), 1000L)
  assert_eq(TimeSeries::get_value(time_series, 2), 15.0)
  assert_eq(TimeSeries::get_value_at_timestamp(time_series, 3000L), Some(15.0))
  assert_eq(TimeSeries::get_value_at_timestamp(time_series, 3500L), None)
}

// Test 2: Time Series Aggregation
test "time series aggregation" {
  let timestamps = [1000L, 2000L, 3000L, 4000L, 5000L, 6000L]
  let values = [10.0, 20.0, 15.0, 25.0, 30.0, 35.0]
  let time_series = TimeSeries::new(timestamps, values)
  
  // Test sum aggregation
  let sum = TimeSeries::aggregate(time_series, Sum)
  assert_eq(sum, 135.0)
  
  // Test average aggregation
  let avg = TimeSeries::aggregate(time_series, Average)
  assert_eq(avg, 22.5)
  
  // Test min aggregation
  let min = TimeSeries::aggregate(time_series, Min)
  assert_eq(min, 10.0)
  
  // Test max aggregation
  let max = TimeSeries::aggregate(time_series, Max)
  assert_eq(max, 35.0)
}

// Test 3: Time Series Windowing
test "time series windowing" {
  let timestamps = [1000L, 2000L, 3000L, 4000L, 5000L, 6000L, 7000L, 8000L]
  let values = [10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0]
  let time_series = TimeSeries::new(timestamps, values)
  
  // Test fixed-size windowing
  let windows = TimeSeries::window_fixed_size(time_series, 3)
  assert_eq(windows.length(), 6)  // 8-3+1 = 6 windows
  
  let first_window = windows[0]
  assert_eq(TimeSeries::length(first_window), 3)
  assert_eq(TimeSeries::get_value(first_window, 0), 10.0)
  assert_eq(TimeSeries::get_value(first_window, 2), 20.0)
  
  // Test time-based windowing
  let time_windows = TimeSeries::window_by_time(time_series, 3000L)  // 3-second windows
  assert_true(time_windows.length() >= 2)
}

// Test 4: Time Series Resampling
test "time series resampling" {
  let timestamps = [1000L, 1500L, 2000L, 2500L, 3000L, 3500L]
  let values = [10.0, 12.0, 20.0, 22.0, 30.0, 32.0]
  let time_series = TimeSeries::new(timestamps, values)
  
  // Upsample to 1000ms intervals
  let upsampled = TimeSeries::resample(time_series, 1000L, Linear)
  assert_true(TimeSeries::length(upsampled) >= TimeSeries::length(time_series))
  
  // Downsample to 2000ms intervals
  let downsampled = TimeSeries::resample(time_series, 2000L, Average)
  assert_true(TimeSeries::length(downsampled) <= TimeSeries::length(time_series))
}

// Test 5: Moving Average Calculation
test "moving average calculation" {
  let timestamps = [1000L, 2000L, 3000L, 4000L, 5000L]
  let values = [10.0, 20.0, 30.0, 40.0, 50.0]
  let time_series = TimeSeries::new(timestamps, values)
  
  // Test simple moving average with window size 3
  let sma = TimeSeries::simple_moving_average(time_series, 3)
  assert_eq(sma.length(), 3)  // First (window_size-1) values are None
  
  assert_eq(sma[2], Some(20.0))  // (10+20+30)/3
  assert_eq(sma[3], Some(30.0))  // (20+30+40)/3
  assert_eq(sma[4], Some(40.0))  // (30+40+50)/3
  
  // Test exponential moving average
  let ema = TimeSeries::exponential_moving_average(time_series, 0.5)
  assert_eq(ema.length(), 5)
  assert_true(ema[0] != None)  // EMA starts from first value
}

// Test 6: Time Series Trend Analysis
test "time series trend analysis" {
  // Increasing trend
  let increasing_timestamps = [1000L, 2000L, 3000L, 4000L, 5000L]
  let increasing_values = [10.0, 20.0, 30.0, 40.0, 50.0]
  let increasing_series = TimeSeries::new(increasing_timestamps, increasing_values)
  
  let increasing_trend = TimeSeries::calculate_trend(increasing_series)
  assert_true(increasing_trend > 0.0)
  
  // Decreasing trend
  let decreasing_values = [50.0, 40.0, 30.0, 20.0, 10.0]
  let decreasing_series = TimeSeries::new(increasing_timestamps, decreasing_values)
  
  let decreasing_trend = TimeSeries::calculate_trend(decreasing_series)
  assert_true(decreasing_trend < 0.0)
  
  // Flat trend
  let flat_values = [25.0, 25.0, 25.0, 25.0, 25.0]
  let flat_series = TimeSeries::new(increasing_timestamps, flat_values)
  
  let flat_trend = TimeSeries::calculate_trend(flat_series)
  assert_true(flat_trend.abs() < 0.1)
}

// Test 7: Time Series Anomaly Detection
test "time series anomaly detection" {
  let timestamps = [1000L, 2000L, 3000L, 4000L, 5000L, 6000L]
  let values = [10.0, 12.0, 11.0, 13.0, 100.0, 12.0]  // 100.0 is an anomaly
  let time_series = TimeSeries::new(timestamps, values)
  
  let anomalies = TimeSeries::detect_anomalies(time_series, StandardDeviation, 2.0)
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0], 4)  // Index of the anomaly (100.0)
  
  // Test with no anomalies
  let normal_values = [10.0, 12.0, 11.0, 13.0, 12.0, 11.0]
  let normal_series = TimeSeries::new(timestamps, normal_values)
  
  let normal_anomalies = TimeSeries::detect_anomalies(normal_series, StandardDeviation, 2.0)
  assert_eq(normal_anomalies.length(), 0)
}

// Test 8: Time Series Forecasting
test "time series forecasting" {
  let timestamps = [1000L, 2000L, 3000L, 4000L, 5000L]
  let values = [10.0, 20.0, 30.0, 40.0, 50.0]
  let time_series = TimeSeries::new(timestamps, values)
  
  // Test linear forecasting
  let forecast = TimeSeries::forecast(time_series, Linear, 3)
  assert_eq(forecast.length(), 3)
  assert_eq(forecast[0], 60.0)  // Next value should be 60
  assert_eq(forecast[1], 70.0)  // Then 70
  assert_eq(forecast[2], 80.0)  // Then 80
  
  // Test exponential smoothing forecasting
  let exp_forecast = TimeSeries::forecast(time_series, ExponentialSmoothing, 2)
  assert_eq(exp_forecast.length(), 2)
}

// Test 9: Time Series Correlation Analysis
test "time series correlation analysis" {
  let timestamps = [1000L, 2000L, 3000L, 4000L, 5000L]
  
  // Perfect positive correlation
  let values1 = [10.0, 20.0, 30.0, 40.0, 50.0]
  let values2 = [20.0, 40.0, 60.0, 80.0, 100.0]  // Exactly 2x values1
  
  let series1 = TimeSeries::new(timestamps, values1)
  let series2 = TimeSeries::new(timestamps, values2)
  
  let correlation = TimeSeries::correlation(series1, series2)
  assert_true(correlation.abs() > 0.99)  // Should be very close to 1.0
  
  // Perfect negative correlation
  let values3 = [50.0, 40.0, 30.0, 20.0, 10.0]  // Reverse of values1
  let series3 = TimeSeries::new(timestamps, values3)
  
  let negative_correlation = TimeSeries::correlation(series1, series3)
  assert_true(negative_correlation < -0.99)  // Should be very close to -1.0
  
  // No correlation
  let values4 = [25.0, 25.0, 25.0, 25.0, 25.0]  // Constant
  let series4 = TimeSeries::new(timestamps, values4)
  
  let no_correlation = TimeSeries::correlation(series1, series4)
  assert_true(no_correlation.abs() < 0.1)  // Should be close to 0.0
}

// Test 10: Time Series Seasonality Detection
test "time series seasonality detection" {
  // Create seasonal data (daily pattern over 3 days)
  let timestamps = []
  let values = []
  
  for day in 0..=2 {
    for hour in 0..=23 {
      timestamps.push((day * 24 * 60 * 60 * 1000 + hour * 60 * 60 * 1000) as Int64)
      // Simulate daily pattern: higher during day, lower at night
      let value = if hour >= 8 && hour <= 18 { 80.0 + (hour - 8) * 2.0 } else { 40.0 }
      values.push(value)
    }
  }
  
  let seasonal_series = TimeSeries::new(timestamps, values)
  
  // Detect daily seasonality (24-hour period)
  let seasonality = TimeSeries::detect_seasonality(seasonal_series, 24 * 60 * 60 * 1000)
  assert_true(seasonality > 0.5)  // Should detect strong seasonality
  
  // Test with non-seasonal data
  let non_seasonal_values = [10.0, 20.0, 30.0, 40.0, 50.0]
  let non_seasonal_timestamps = [1000L, 2000L, 3000L, 4000L, 5000L]
  let non_seasonal_series = TimeSeries::new(non_seasonal_timestamps, non_seasonal_values)
  
  let non_seasonal = TimeSeries::detect_seasonality(non_seasonal_series, 1000L)
  assert_true(non_seasonal < 0.3)  // Should detect weak or no seasonality
}