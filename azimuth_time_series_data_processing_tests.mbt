// Azimuth Telemetry System - Time Series Data Processing Tests
// This file contains comprehensive test cases for time series data processing functionality

// Test 1: Time Series Point Creation
test "time series point creation" {
  // Create time series point with timestamp and value
  let timestamp = 1234567890L
  let value = 42.5
  let point = TimeSeriesPoint::new(timestamp, value)
  
  assert_eq(TimeSeriesPoint::timestamp(point), timestamp)
  assert_eq(TimeSeriesPoint::value(point), value)
  
  // Create time series point with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "unit", StringValue("celsius"))
  Attributes::set(attrs, "location", StringValue("server-room-1"))
  
  let point_with_attrs = TimeSeriesPoint::new_with_attributes(timestamp, value, attrs)
  assert_eq(TimeSeriesPoint::timestamp(point_with_attrs), timestamp)
  assert_eq(TimeSeriesPoint::value(point_with_attrs), value)
  
  let point_attrs = TimeSeriesPoint::attributes(point_with_attrs)
  match point_attrs {
    Some(a) => {
      match Attributes::get(a, "unit") {
        Some(StringValue(unit)) => assert_eq(unit, "celsius")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 2: Time Series Creation and Management
test "time series creation and management" {
  // Create a new time series
  let series_name = "cpu.temperature"
  let series = TimeSeries::new(series_name)
  
  assert_eq(TimeSeries::name(series), series_name)
  assert_eq(TimeSeries::length(series), 0)
  
  // Add points to the time series
  let point1 = TimeSeriesPoint::new(1000L, 25.5)
  let point2 = TimeSeriesPoint::new(2000L, 26.0)
  let point3 = TimeSeriesPoint::new(3000L, 25.8)
  
  let series_with_points = TimeSeries::add_point(TimeSeries::add_point(
    TimeSeries::add_point(series, point1), point2), point3)
  
  assert_eq(TimeSeries::length(series_with_points), 3)
  
  // Get points by index
  let retrieved_point1 = TimeSeries::get_point(series_with_points, 0)
  match retrieved_point1 {
    Some(p) => {
      assert_eq(TimeSeriesPoint::timestamp(p), 1000L)
      assert_eq(TimeSeriesPoint::value(p), 25.5)
    }
    None => assert_true(false)
  }
  
  // Test getting non-existent point
  let non_existent = TimeSeries::get_point(series_with_points, 10)
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Time Series Aggregation Operations
test "time series aggregation operations" {
  // Create time series with test data
  let series = TimeSeries::new("test.series")
  let points = [
    TimeSeriesPoint::new(1000L, 10.0),
    TimeSeriesPoint::new(2000L, 20.0),
    TimeSeriesPoint::new(3000L, 30.0),
    TimeSeriesPoint::new(4000L, 40.0),
    TimeSeriesPoint::new(5000L, 50.0)
  ]
  
  let populated_series = TimeSeries::from_points("test.series", points)
  
  // Test average calculation
  let average = TimeSeries::average(populated_series)
  match average {
    Some(avg) => assert_eq(avg, 30.0)
    None => assert_true(false)
  }
  
  // Test sum calculation
  let sum = TimeSeries::sum(populated_series)
  match sum {
    Some(s) => assert_eq(s, 150.0)
    None => assert_true(false)
  }
  
  // Test min calculation
  let min = TimeSeries::min(populated_series)
  match min {
    Some(m) => assert_eq(m, 10.0)
    None => assert_true(false)
  }
  
  // Test max calculation
  let max = TimeSeries::max(populated_series)
  match max {
    Some(m) => assert_eq(m, 50.0)
    None => assert_true(false)
  }
  
  // Test with empty series
  let empty_series = TimeSeries::new("empty.series")
  let empty_average = TimeSeries::average(empty_series)
  match empty_average {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: Time Series Windowing Operations
test "time series windowing operations" {
  // Create time series with test data
  let points = [
    TimeSeriesPoint::new(1000L, 10.0),
    TimeSeriesPoint::new(1500L, 15.0),
    TimeSeriesPoint::new(2000L, 20.0),
    TimeSeriesPoint::new(2500L, 25.0),
    TimeSeriesPoint::new(3000L, 30.0),
    TimeSeriesPoint::new(3500L, 35.0),
    TimeSeriesPoint::new(4000L, 40.0)
  ]
  
  let series = TimeSeries::from_points("window.test", points)
  
  // Test time-based windowing
  let windows = TimeSeries::time_windows(series, 1000L) // 1 second windows
  assert_eq(windows.length(), 4)
  
  // First window should contain points at 1000L and 1500L
  let first_window = windows[0]
  assert_eq(TimeSeries::length(first_window), 2)
  
  // Test sliding window
  let sliding_windows = TimeSeries::sliding_windows(series, 3)
  assert_eq(sliding_windows.length(), 5) // 7 points - 3 size + 1
  
  // First sliding window should contain first 3 points
  let first_sliding = sliding_windows[0]
  assert_eq(TimeSeries::length(first_sliding), 3)
  
  let first_point = TimeSeries::get_point(first_sliding, 0)
  match first_point {
    Some(p) => assert_eq(TimeSeriesPoint::timestamp(p), 1000L)
    None => assert_true(false)
  }
}

// Test 5: Time Series Resampling Operations
test "time series resampling operations" {
  // Create time series with irregular intervals
  let points = [
    TimeSeriesPoint::new(1000L, 10.0),
    TimeSeriesPoint::new(1300L, 13.0),
    TimeSeriesPoint::new(1800L, 18.0),
    TimeSeriesPoint::new(2200L, 22.0),
    TimeSeriesPoint::new(2800L, 28.0)
  ]
  
  let series = TimeSeries::from_points("resample.test", points)
  
  // Test upsampling (linear interpolation)
  let upsampled = TimeSeries::upsample_linear(series, 500L)
  assert_eq(TimeSeries::length(upsampled), 7)
  
  // Check interpolated point at 1500L
  let interpolated = TimeSeries::point_at_time(upsampled, 1500L)
  match interpolated {
    Some(p) => {
      // Should be approximately 15.0 (between 13.0 at 1300L and 18.0 at 1800L)
      let value = TimeSeriesPoint::value(p)
      assert_true(value > 14.0 && value < 16.0)
    }
    None => assert_true(false)
  }
  
  // Test downsampling (averaging)
  let downsampled = TimeSeries::downsample_average(series, 1000L)
  assert_eq(TimeSeries::length(downsampled), 3)
  
  // First downsampled point should average points at 1000L and 1300L
  let first_downsampled = TimeSeries::get_point(downsampled, 0)
  match first_downsampled {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), 11.5) // (10.0 + 13.0) / 2
    None => assert_true(false)
  }
}

// Test 6: Time Series Filtering Operations
test "time series filtering operations" {
  // Create time series with test data
  let points = [
    TimeSeriesPoint::new(1000L, 10.0),
    TimeSeriesPoint::new(2000L, 20.0),
    TimeSeriesPoint::new(3000L, 30.0),
    TimeSeriesPoint::new(4000L, 40.0),
    TimeSeriesPoint::new(5000L, 50.0)
  ]
  
  let series = TimeSeries::from_points("filter.test", points)
  
  // Test time range filtering
  let filtered = TimeSeries::filter_by_time_range(series, 2000L, 4000L)
  assert_eq(TimeSeries::length(filtered), 3)
  
  let first_filtered = TimeSeries::get_point(filtered, 0)
  match first_filtered {
    Some(p) => assert_eq(TimeSeriesPoint::timestamp(p), 2000L)
    None => assert_true(false)
  }
  
  // Test value range filtering
  let value_filtered = TimeSeries::filter_by_value_range(series, 15.0, 45.0)
  assert_eq(TimeSeries::length(value_filtered), 3)
  
  let first_value_filtered = TimeSeries::get_point(value_filtered, 0)
  match first_value_filtered {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), 20.0)
    None => assert_true(false)
  }
  
  // Test custom predicate filtering
  let predicate_filtered = TimeSeries::filter_by_predicate(series, fn(point) {
    TimeSeriesPoint::value(point) > 25.0
  })
  assert_eq(TimeSeries::length(predicate_filtered), 3)
}

// Test 7: Time Series Derivative Operations
test "time series derivative operations" {
  // Create time series with test data
  let points = [
    TimeSeriesPoint::new(1000L, 10.0),
    TimeSeriesPoint::new(2000L, 20.0),
    TimeSeriesPoint::new(3000L, 30.0),
    TimeSeriesPoint::new(4000L, 40.0),
    TimeSeriesPoint::new(5000L, 50.0)
  ]
  
  let series = TimeSeries::from_points("derivative.test", points)
  
  // Test first derivative (rate of change)
  let derivative = TimeSeries::first_derivative(series)
  assert_eq(TimeSeries::length(derivative), 4)
  
  // First derivative point should be (20.0 - 10.0) / (2000L - 1000L) = 0.01
  let first_derivative_point = TimeSeries::get_point(derivative, 0)
  match first_derivative_point {
    Some(p) => assert_eq(TimeSeriesPoint::value(p), 0.01)
    None => assert_true(false)
  }
  
  // Test second derivative (acceleration)
  let second_derivative = TimeSeries::second_derivative(series)
  assert_eq(TimeSeries::length(second_derivative), 3)
  
  // For linear data, second derivative should be approximately 0
  let second_derivative_point = TimeSeries::get_point(second_derivative, 0)
  match second_derivative_point {
    Some(p) => {
      let value = TimeSeriesPoint::value(p)
      assert_true(value > -0.001 && value < 0.001)
    }
    None => assert_true(false)
  }
}

// Test 8: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Create time series with normal data and one anomaly
  let points = [
    TimeSeriesPoint::new(1000L, 10.0),
    TimeSeriesPoint::new(2000L, 12.0),
    TimeSeriesPoint::new(3000L, 11.0),
    TimeSeriesPoint::new(4000L, 100.0), // Anomaly
    TimeSeriesPoint::new(5000L, 13.0),
    TimeSeriesPoint::new(6000L, 10.5)
  ]
  
  let series = TimeSeries::from_points("anomaly.test", points)
  
  // Test statistical anomaly detection (z-score)
  let anomalies = TimeSeries::detect_anomalies_zscore(series, 2.0)
  assert_eq(anomalies.length(), 1)
  
  let anomaly_point = anomalies[0]
  assert_eq(TimeSeriesPoint::timestamp(anomaly_point), 4000L)
  assert_eq(TimeSeriesPoint::value(anomaly_point), 100.0)
  
  // Test moving average anomaly detection
  let ma_anomalies = TimeSeries::detect_anomalies_moving_average(series, 2, 3.0)
  assert_eq(ma_anomalies.length(), 1)
  
  // Test with series without anomalies
  let normal_points = [
    TimeSeriesPoint::new(1000L, 10.0),
    TimeSeriesPoint::new(2000L, 12.0),
    TimeSeriesPoint::new(3000L, 11.0),
    TimeSeriesPoint::new(4000L, 13.0),
    TimeSeriesPoint::new(5000L, 12.5)
  ]
  
  let normal_series = TimeSeries::from_points("normal.test", normal_points)
  let normal_anomalies = TimeSeries::detect_anomalies_zscore(normal_series, 2.0)
  assert_eq(normal_anomalies.length(), 0)
}

// Test 9: Time Series Forecasting
test "time series forecasting" {
  // Create time series with trend data
  let points = [
    TimeSeriesPoint::new(1000L, 10.0),
    TimeSeriesPoint::new(2000L, 12.0),
    TimeSeriesPoint::new(3000L, 14.0),
    TimeSeriesPoint::new(4000L, 16.0),
    TimeSeriesPoint::new(5000L, 18.0)
  ]
  
  let series = TimeSeries::from_points("forecast.test", points)
  
  // Test linear regression forecasting
  let forecast = TimeSeries::forecast_linear(series, 2)
  assert_eq(forecast.length(), 2)
  
  // First forecast point should be at 6000L with value approximately 20.0
  let first_forecast = forecast[0]
  assert_eq(TimeSeriesPoint::timestamp(first_forecast), 6000L)
  let forecast_value = TimeSeriesPoint::value(first_forecast)
  assert_true(forecast_value > 19.0 && forecast_value < 21.0)
  
  // Second forecast point should be at 7000L with value approximately 22.0
  let second_forecast = forecast[1]
  assert_eq(TimeSeriesPoint::timestamp(second_forecast), 7000L)
  let second_forecast_value = TimeSeriesPoint::value(second_forecast)
  assert_true(second_forecast_value > 21.0 && second_forecast_value < 23.0)
  
  // Test moving average forecasting
  let ma_forecast = TimeSeries::forecast_moving_average(series, 3, 1)
  assert_eq(ma_forecast.length(), 1)
  
  let ma_forecast_point = ma_forecast[0]
  assert_eq(TimeSeriesPoint::timestamp(ma_forecast_point), 6000L)
  let ma_forecast_value = TimeSeriesPoint::value(ma_forecast_point)
  assert_true(ma_forecast_value > 15.0 && ma_forecast_value < 17.0)
}

// Test 10: Time Series Correlation Analysis
test "time series correlation analysis" {
  // Create two correlated time series
  let points1 = [
    TimeSeriesPoint::new(1000L, 10.0),
    TimeSeriesPoint::new(2000L, 20.0),
    TimeSeriesPoint::new(3000L, 30.0),
    TimeSeriesPoint::new(4000L, 40.0),
    TimeSeriesPoint::new(5000L, 50.0)
  ]
  
  let points2 = [
    TimeSeriesPoint::new(1000L, 15.0),
    TimeSeriesPoint::new(2000L, 25.0),
    TimeSeriesPoint::new(3000L, 35.0),
    TimeSeriesPoint::new(4000L, 45.0),
    TimeSeriesPoint::new(5000L, 55.0)
  ]
  
  let series1 = TimeSeries::from_points("correlation.test1", points1)
  let series2 = TimeSeries::from_points("correlation.test2", points2)
  
  // Test correlation coefficient calculation
  let correlation = TimeSeries::correlation(series1, series2)
  match correlation {
    Some(corr) => {
      // Should be close to 1.0 for perfectly correlated series
      assert_true(corr > 0.99)
    }
    None => assert_true(false)
  }
  
  // Test with negatively correlated series
  let points3 = [
    TimeSeriesPoint::new(1000L, 50.0),
    TimeSeriesPoint::new(2000L, 40.0),
    TimeSeriesPoint::new(3000L, 30.0),
    TimeSeriesPoint::new(4000L, 20.0),
    TimeSeriesPoint::new(5000L, 10.0)
  ]
  
  let series3 = TimeSeries::from_points("correlation.test3", points3)
  let negative_correlation = TimeSeries::correlation(series1, series3)
  match negative_correlation {
    Some(corr) => {
      // Should be close to -1.0 for perfectly negatively correlated series
      assert_true(corr < -0.99)
    }
    None => assert_true(false)
  }
  
  // Test with uncorrelated series
  let points4 = [
    TimeSeriesPoint::new(1000L, 10.0),
    TimeSeriesPoint::new(2000L, 50.0),
    TimeSeriesPoint::new(3000L, 20.0),
    TimeSeriesPoint::new(4000L, 60.0),
    TimeSeriesPoint::new(5000L, 30.0)
  ]
  
  let series4 = TimeSeries::from_points("correlation.test4", points4)
  let uncorrelated = TimeSeries::correlation(series1, series4)
  match uncorrelated {
    Some(corr) => {
      // Should be close to 0 for uncorrelated series
      assert_true(corr > -0.5 && corr < 0.5)
    }
    None => assert_true(false)
  }
  
  // Test with series of different lengths
  let short_points = [
    TimeSeriesPoint::new(1000L, 10.0),
    TimeSeriesPoint::new(2000L, 20.0)
  ]
  
  let short_series = TimeSeries::from_points("correlation.short", short_points)
  let different_length = TimeSeries::correlation(series1, short_series)
  match different_length {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}