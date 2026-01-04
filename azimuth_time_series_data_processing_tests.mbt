// Azimuth Telemetry System - Time Series Data Processing Tests
// This file contains test cases for time series data processing functionality

// Test 1: Time Series Data Point Creation
test "time series data point creation" {
  // Create a basic time series data point
  let timestamp = 1640995200000L  // 2022-01-01 00:00:00 UTC
  let value = 42.5
  let data_point = TimeSeriesDataPoint::new(timestamp, value)
  
  assert_eq(TimeSeriesDataPoint::timestamp(data_point), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point), value)
  
  // Create a data point with attributes
  let attrs = [
    ("source", StringValue("sensor-1")),
    ("unit", StringValue("celsius"))
  ]
  let data_point_with_attrs = TimeSeriesDataPoint::with_attributes(timestamp, value, attrs)
  
  assert_eq(TimeSeriesDataPoint::timestamp(data_point_with_attrs), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point_with_attrs), value)
  
  let source_attr = TimeSeriesDataPoint::get_attribute(data_point_with_attrs, "source")
  match source_attr {
    Some(StringValue(source)) => assert_eq(source, "sensor-1")
    _ => assert_true(false)
  }
}

// Test 2: Time Series Creation and Basic Operations
test "time series creation and basic operations" {
  // Create a new time series
  let series_name = "temperature-readings"
  let time_series = TimeSeries::new(series_name)
  
  assert_eq(TimeSeries::name(time_series), series_name)
  assert_eq(TimeSeries::length(time_series), 0)
  
  // Add data points to the time series
  let point1 = TimeSeriesDataPoint::new(1640995200000L, 20.5)
  let point2 = TimeSeriesDataPoint::new(1640995260000L, 21.0)
  let point3 = TimeSeriesDataPoint::new(1640995320000L, 20.8)
  
  let updated_series = TimeSeries::add_point(time_series, point1)
  let updated_series = TimeSeries::add_point(updated_series, point2)
  let updated_series = TimeSeries::add_point(updated_series, point3)
  
  assert_eq(TimeSeries::length(updated_series), 3)
  
  // Test getting data points by index
  let retrieved_point1 = TimeSeries::get_point(updated_series, 0)
  match retrieved_point1 {
    Some(point) => assert_eq(TimeSeriesDataPoint::value(point), 20.5)
    None => assert_true(false)
  }
  
  let retrieved_point3 = TimeSeries::get_point(updated_series, 2)
  match retrieved_point3 {
    Some(point) => assert_eq(TimeSeriesDataPoint::value(point), 20.8)
    None => assert_true(false)
  }
  
  // Test getting non-existent point
  let non_existent = TimeSeries::get_point(updated_series, 10)
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Time Series Aggregation Operations
test "time series aggregation operations" {
  // Create a time series with multiple data points
  let time_series = TimeSeries::new("test-series")
  
  // Add data points with known values for aggregation testing
  let points = [
    TimeSeriesDataPoint::new(1640995200000L, 10.0),
    TimeSeriesDataPoint::new(1640995260000L, 20.0),
    TimeSeriesDataPoint::new(1640995320000L, 30.0),
    TimeSeriesDataPoint::new(1640995380000L, 40.0),
    TimeSeriesDataPoint::new(1640995440000L, 50.0)
  ]
  
  let updated_series = TimeSeries::from_array(points)
  
  // Test average calculation
  let average = TimeSeries::average(updated_series)
  match average {
    Some(avg) => assert_eq(avg, 30.0)
    None => assert_true(false)
  }
  
  // Test min calculation
  let min = TimeSeries::min(updated_series)
  match min {
    Some(min_val) => assert_eq(min_val, 10.0)
    None => assert_true(false)
  }
  
  // Test max calculation
  let max = TimeSeries::max(updated_series)
  match max {
    Some(max_val) => assert_eq(max_val, 50.0)
    None => assert_true(false)
  }
  
  // Test sum calculation
  let sum = TimeSeries::sum(updated_series)
  match sum {
    Some(sum_val) => assert_eq(sum_val, 150.0)
    None => assert_true(false)
  }
  
  // Test empty series aggregation
  let empty_series = TimeSeries::new("empty-series")
  let empty_average = TimeSeries::average(empty_series)
  match empty_average {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: Time Series Range Queries
test "time series range queries" {
  // Create a time series with data points spanning a time range
  let points = [
    TimeSeriesDataPoint::new(1640995200000L, 10.0),  // 2022-01-01 00:00:00
    TimeSeriesDataPoint::new(1640995260000L, 20.0),  // 2022-01-01 00:01:00
    TimeSeriesDataPoint::new(1640995320000L, 30.0),  // 2022-01-01 00:02:00
    TimeSeriesDataPoint::new(1640995380000L, 40.0),  // 2022-01-01 00:03:00
    TimeSeriesDataPoint::new(1640995440000L, 50.0)   // 2022-01-01 00:04:00
  ]
  
  let time_series = TimeSeries::from_array(points)
  
  // Query for a specific time range
  let start_time = 1640995260000L  // 2022-01-01 00:01:00
  let end_time = 1640995380000L    // 2022-01-01 00:03:00
  
  let range_series = TimeSeries::get_range(time_series, start_time, end_time)
  
  // Should include points at 00:01:00, 00:02:00, and 00:03:00
  assert_eq(TimeSeries::length(range_series), 3)
  
  let first_point = TimeSeries::get_point(range_series, 0)
  match first_point {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 1640995260000L)
      assert_eq(TimeSeriesDataPoint::value(point), 20.0)
    }
    None => assert_true(false)
  }
  
  let last_point = TimeSeries::get_point(range_series, 2)
  match last_point {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 1640995380000L)
      assert_eq(TimeSeriesDataPoint::value(point), 40.0)
    }
    None => assert_true(false)
  }
  
  // Test empty range query
  let future_start = 1640996000000L  // Future time
  let future_end = 1640996100000L
  let empty_range = TimeSeries::get_range(time_series, future_start, future_end)
  assert_eq(TimeSeries::length(empty_range), 0)
}

// Test 5: Time Series Resampling
test "time series resampling" {
  // Create a time series with data points at 1-minute intervals
  let points = [
    TimeSeriesDataPoint::new(1640995200000L, 10.0),  // 00:00:00
    TimeSeriesDataPoint::new(1640995260000L, 20.0),  // 00:01:00
    TimeSeriesDataPoint::new(1640995320000L, 30.0),  // 00:02:00
    TimeSeriesDataPoint::new(1640995380000L, 40.0),  // 00:03:00
    TimeSeriesDataPoint::new(1640995440000L, 50.0),  // 00:04:00
    TimeSeriesDataPoint::new(1640995500000L, 60.0)   // 00:05:00
  ]
  
  let time_series = TimeSeries::from_array(points)
  
  // Resample to 2-minute intervals using average
  let resampled_series = TimeSeries::resample(time_series, 120000L, AggregationMethod::Average)
  
  // Should have points at 00:00:00, 00:02:00, 00:04:00
  assert_eq(TimeSeries::length(resampled_series), 3)
  
  // Check first resampled point (average of 00:00:00 and 00:01:00)
  let first_point = TimeSeries::get_point(resampled_series, 0)
  match first_point {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 1640995200000L)
      assert_eq(TimeSeriesDataPoint::value(point), 15.0)  // (10 + 20) / 2
    }
    None => assert_true(false)
  }
  
  // Check second resampled point (average of 00:02:00 and 00:03:00)
  let second_point = TimeSeries::get_point(resampled_series, 1)
  match second_point {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 1640995320000L)
      assert_eq(TimeSeriesDataPoint::value(point), 35.0)  // (30 + 40) / 2
    }
    None => assert_true(false)
  }
  
  // Check third resampled point (average of 00:04:00 and 00:05:00)
  let third_point = TimeSeries::get_point(resampled_series, 2)
  match third_point {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 1640995440000L)
      assert_eq(TimeSeriesDataPoint::value(point), 55.0)  // (50 + 60) / 2
    }
    None => assert_true(false)
  }
}

// Test 6: Time Series Window Operations
test "time series window operations" {
  // Create a time series with data points
  let points = [
    TimeSeriesDataPoint::new(1640995200000L, 10.0),
    TimeSeriesDataPoint::new(1640995260000L, 20.0),
    TimeSeriesDataPoint::new(1640995320000L, 30.0),
    TimeSeriesDataPoint::new(1640995380000L, 40.0),
    TimeSeriesDataPoint::new(1640995440000L, 50.0)
  ]
  
  let time_series = TimeSeries::from_array(points)
  
  // Apply a moving average with window size of 3
  let smoothed_series = TimeSeries::moving_average(time_series, 3)
  
  // The first two points should be None (not enough data for window)
  assert_eq(TimeSeries::length(smoothed_series), 5)
  
  // Check third point (average of first 3 points)
  let third_point = TimeSeries::get_point(smoothed_series, 2)
  match third_point {
    Some(point) => assert_eq(TimeSeriesDataPoint::value(point), 20.0)  // (10 + 20 + 30) / 3
    None => assert_true(false)
  }
  
  // Check fourth point (average of points 2-4)
  let fourth_point = TimeSeries::get_point(smoothed_series, 3)
  match fourth_point {
    Some(point) => assert_eq(TimeSeriesDataPoint::value(point), 30.0)  // (20 + 30 + 40) / 3
    None => assert_true(false)
  }
  
  // Check fifth point (average of points 3-5)
  let fifth_point = TimeSeries::get_point(smoothed_series, 4)
  match fifth_point {
    Some(point) => assert_eq(TimeSeriesDataPoint::value(point), 40.0)  // (30 + 40 + 50) / 3
    None => assert_true(false)
  }
}

// Test 7: Time Series Metadata Operations
test "time series metadata operations" {
  // Create a time series with metadata
  let time_series = TimeSeries::new("test-series")
  
  // Set metadata
  let metadata = [
    ("description", StringValue("Test time series for unit testing")),
    ("unit", StringValue("celsius")),
    ("source", StringValue("sensor-1"))
  ]
  
  let series_with_metadata = TimeSeries::with_metadata(time_series, metadata)
  
  // Test getting metadata
  let description = TimeSeries::get_metadata(series_with_metadata, "description")
  match description {
    Some(StringValue(desc)) => assert_eq(desc, "Test time series for unit testing")
    _ => assert_true(false)
  }
  
  let unit = TimeSeries::get_metadata(series_with_metadata, "unit")
  match unit {
    Some(StringValue(u)) => assert_eq(u, "celsius")
    _ => assert_true(false)
  }
  
  // Test updating metadata
  let updated_series = TimeSeries::set_metadata(series_with_metadata, "unit", StringValue("fahrenheit"))
  let updated_unit = TimeSeries::get_metadata(updated_series, "unit")
  match updated_unit {
    Some(StringValue(u)) => assert_eq(u, "fahrenheit")
    _ => assert_true(false)
  }
  
  // Test getting non-existent metadata
  let non_existent = TimeSeries::get_metadata(updated_series, "non-existent")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 8: Time Series Serialization and Deserialization
test "time series serialization and deserialization" {
  // Create a time series with data points and metadata
  let points = [
    TimeSeriesDataPoint::new(1640995200000L, 10.0),
    TimeSeriesDataPoint::new(1640995260000L, 20.0),
    TimeSeriesDataPoint::new(1640995320000L, 30.0)
  ]
  
  let metadata = [
    ("description", StringValue("Test time series")),
    ("unit", StringValue("celsius"))
  ]
  
  let original_series = TimeSeries::with_metadata(TimeSeries::from_array(points), metadata)
  
  // Serialize to JSON
  let json_data = TimeSeries::to_json(original_series)
  assert_true(json_data.length() > 0)
  
  // Deserialize from JSON
  let deserialized_series = TimeSeries::from_json(json_data)
  
  // Verify the deserialized series matches the original
  assert_eq(TimeSeries::name(deserialized_series), TimeSeries::name(original_series))
  assert_eq(TimeSeries::length(deserialized_series), TimeSeries::length(original_series))
  
  // Check data points
  for i in 0..TimeSeries::length(deserialized_series) {
    let original_point = TimeSeries::get_point(original_series, i)
    let deserialized_point = TimeSeries::get_point(deserialized_series, i)
    
    match (original_point, deserialized_point) {
      (Some(orig), Some(deser)) => {
        assert_eq(TimeSeriesDataPoint::timestamp(orig), TimeSeriesDataPoint::timestamp(deser))
        assert_eq(TimeSeriesDataPoint::value(orig), TimeSeriesDataPoint::value(deser))
      }
      _ => assert_true(false)
    }
  }
  
  // Check metadata
  let original_desc = TimeSeries::get_metadata(original_series, "description")
  let deserialized_desc = TimeSeries::get_metadata(deserialized_series, "description")
  
  match (original_desc, deserialized_desc) {
    (Some(StringValue(orig)), Some(StringValue(deser))) => assert_eq(orig, deser)
    _ => assert_true(false)
  }
}