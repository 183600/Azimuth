// Azimuth Telemetry System - Time Series Data Processing Tests
// This file contains test cases for time series data processing functionality

// Test 1: Time Series Data Point Creation
test "time series data point creation" {
  let timestamp = 1609459200L // 2021-01-01 00:00:00 UTC
  let value = 42.5
  let data_point = TimeSeriesDataPoint::new(timestamp, value)
  
  assert_eq(TimeSeriesDataPoint::timestamp(data_point), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point), value)
}

// Test 2: Time Series Data Point with Attributes
test "time series data point with attributes" {
  let timestamp = 1609459200L
  let value = 42.5
  let attributes = Attributes::new()
  Attributes::set(attributes, "unit", StringValue("celsius"))
  Attributes::set(attributes, "location", StringValue("server-room-1"))
  
  let data_point = TimeSeriesDataPoint::new_with_attributes(timestamp, value, attributes)
  
  assert_eq(TimeSeriesDataPoint::timestamp(data_point), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point), value)
  
  let unit_attr = TimeSeriesDataPoint::get_attribute(data_point, "unit")
  match unit_attr {
    Some(StringValue(unit)) => assert_eq(unit, "celsius")
    _ => assert_true(false)
  }
  
  let location_attr = TimeSeriesDataPoint::get_attribute(data_point, "location")
  match location_attr {
    Some(StringValue(location)) => assert_eq(location, "server-room-1")
    _ => assert_true(false)
  }
}

// Test 3: Time Series Creation and Management
test "time series creation and management" {
  let series_name = "cpu_temperature"
  let time_series = TimeSeries::new(series_name)
  
  assert_eq(TimeSeries::name(time_series), series_name)
  assert_eq(TimeSeries::length(time_series), 0)
  
  // Add data points
  let timestamp1 = 1609459200L
  let timestamp2 = 1609459260L // 1 minute later
  let timestamp3 = 1609459320L // 2 minutes later
  
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(timestamp1, 42.5))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(timestamp2, 43.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(timestamp3, 42.8))
  
  assert_eq(TimeSeries::length(time_series), 3)
}

// Test 4: Time Series Data Retrieval
test "time series data retrieval" {
  let time_series = TimeSeries::new("test_series")
  
  // Add data points
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1609459200L, 42.5))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1609459260L, 43.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1609459320L, 42.8))
  
  // Get all points
  let all_points = TimeSeries::get_all_points(time_series)
  assert_eq(all_points.length(), 3)
  
  // Get point by index
  let first_point = TimeSeries::get_point(time_series, 0)
  match first_point {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 1609459200L)
      assert_eq(TimeSeriesDataPoint::value(point), 42.5)
    }
    None => assert_true(false)
  }
  
  // Get point by timestamp range
  let points_in_range = TimeSeries::get_points_in_range(time_series, 1609459200L, 1609459260L)
  assert_eq(points_in_range.length(), 2)
}

// Test 5: Time Series Aggregation - Average
test "time series aggregation - average" {
  let time_series = TimeSeries::new("test_series")
  
  // Add data points
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1609459200L, 42.5))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1609459260L, 43.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1609459320L, 42.8))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1609459380L, 43.2))
  
  // Calculate average
  let average = TimeSeries::average(time_series)
  assert_eq(average, 42.875)
  
  // Calculate average in time range
  let range_average = TimeSeries::average_in_range(time_series, 1609459200L, 1609459260L)
  assert_eq(range_average, 42.75)
}

// Test 6: Time Series Aggregation - Min/Max
test "time series aggregation - min/max" {
  let time_series = TimeSeries::new("test_series")
  
  // Add data points
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1609459200L, 42.5))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1609459260L, 43.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1609459320L, 42.8))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1609459380L, 43.2))
  
  // Calculate min and max
  let min_value = TimeSeries::min(time_series)
  let max_value = TimeSeries::max(time_series)
  
  assert_eq(min_value, 42.5)
  assert_eq(max_value, 43.2)
  
  // Calculate min and max in time range
  let range_min = TimeSeries::min_in_range(time_series, 1609459200L, 1609459260L)
  let range_max = TimeSeries::max_in_range(time_series, 1609459200L, 1609459260L)
  
  assert_eq(range_min, 42.5)
  assert_eq(range_max, 43.0)
}

// Test 7: Time Series Resampling
test "time series resampling" {
  let time_series = TimeSeries::new("test_series")
  
  // Add data points every 30 seconds
  for i in 0..=5 {
    let timestamp = 1609459200L + (i * 30L)
    let value = 42.0 + (i * 0.1)
    TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Resample to 1-minute intervals using average
  let resampled = TimeSeries::resample(time_series, 60L, Average)
  assert_eq(TimeSeries::length(resampled), 3)
  
  // Check first resampled point (average of first 2 points)
  let first_resampled = TimeSeries::get_point(resampled, 0)
  match first_resampled {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 1609459200L)
      assert_eq(TimeSeriesDataPoint::value(point), 42.05)
    }
    None => assert_true(false)
  }
}

// Test 8: Time Series Trend Analysis
test "time series trend analysis" {
  let time_series = TimeSeries::new("test_series")
  
  // Add increasing data points
  for i in 0..=9 {
    let timestamp = 1609459200L + (i * 60L)
    let value = 42.0 + (i * 0.5)
    TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Calculate trend
  let trend = TimeSeries::calculate_trend(time_series)
  match trend {
    Increasing => assert_true(true)
    _ => assert_true(false)
  }
  
  // Add decreasing data points to a new series
  let decreasing_series = TimeSeries::new("decreasing_series")
  for i in 0..=9 {
    let timestamp = 1609459200L + (i * 60L)
    let value = 50.0 - (i * 0.5)
    TimeSeries::add_point(decreasing_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  let decreasing_trend = TimeSeries::calculate_trend(decreasing_series)
  match decreasing_trend {
    Decreasing => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 9: Time Series Anomaly Detection
test "time series anomaly detection" {
  let time_series = TimeSeries::new("test_series")
  
  // Add normal data points
  for i in 0..=9 {
    let timestamp = 1609459200L + (i * 60L)
    let value = 42.0 + (i * 0.1)
    TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Add an anomaly
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1609459860L, 100.0))
  
  // Detect anomalies
  let anomalies = TimeSeries::detect_anomalies(time_series, 2.0) // 2 standard deviations
  assert_eq(anomalies.length(), 1)
  
  let anomaly = anomalies[0]
  assert_eq(TimeSeriesDataPoint::timestamp(anomaly), 1609459860L)
  assert_eq(TimeSeriesDataPoint::value(anomaly), 100.0)
}

// Test 10: Time Series Forecasting
test "time series forecasting" {
  let time_series = TimeSeries::new("test_series")
  
  // Add historical data points
  for i in 0..=23 {
    let timestamp = 1609459200L + (i * 3600L) // Every hour
    let value = 42.0 + (i * 0.1)
    TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Forecast next 3 points
  let forecast = TimeSeries::forecast(time_series, 3)
  assert_eq(forecast.length(), 3)
  
  // Check first forecast point
  let first_forecast = forecast[0]
  let expected_timestamp = 1609459200L + (24 * 3600L)
  assert_eq(TimeSeriesDataPoint::timestamp(first_forecast), expected_timestamp)
  
  // Value should be close to the trend
  let forecast_value = TimeSeriesDataPoint::value(first_forecast)
  assert_true(forecast_value > 44.0 && forecast_value < 45.0)
}