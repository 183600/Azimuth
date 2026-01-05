// Azimuth Telemetry System - Time Series Data Processing Tests
// This file contains test cases for time series data processing functionality

// Test 1: Time Series Data Point Creation
test "time series data point creation" {
  // Test creating a data point with timestamp and value
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  let value = 42.5
  let data_point = TimeSeriesDataPoint::new(timestamp, value)
  
  assert_eq(TimeSeriesDataPoint::timestamp(data_point), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point), value)
  
  // Test creating a data point with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "unit", StringValue("celsius"))
  Attributes::set(attrs, "sensor_id", StringValue("temp-001"))
  
  let data_point_with_attrs = TimeSeriesDataPoint::with_attributes(
    timestamp, 
    value, 
    attrs
  )
  
  assert_eq(TimeSeriesDataPoint::timestamp(data_point_with_attrs), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point_with_attrs), value)
  
  let unit_attr = TimeSeriesDataPoint::get_attribute(data_point_with_attrs, "unit")
  match unit_attr {
    Some(StringValue(unit)) => assert_eq(unit, "celsius")
    _ => assert_true(false)
  }
}

// Test 2: Time Series Creation and Management
test "time series creation and management" {
  // Test creating a time series
  let series_name = "temperature_sensor"
  let time_series = TimeSeries::new(series_name)
  
  assert_eq(TimeSeries::name(time_series), series_name)
  assert_eq(TimeSeries::length(time_series), 0)
  
  // Test adding data points
  let timestamp1 = 1609459200000L
  let timestamp2 = 1609459260000L
  let timestamp3 = 1609459320000L
  
  let data_point1 = TimeSeriesDataPoint::new(timestamp1, 22.5)
  let data_point2 = TimeSeriesDataPoint::new(timestamp2, 23.1)
  let data_point3 = TimeSeriesDataPoint::new(timestamp3, 22.8)
  
  TimeSeries::add_point(time_series, data_point1)
  assert_eq(TimeSeries::length(time_series), 1)
  
  TimeSeries::add_point(time_series, data_point2)
  TimeSeries::add_point(time_series, data_point3)
  assert_eq(TimeSeries::length(time_series), 3)
  
  // Test retrieving data points
  let first_point = TimeSeries::get_point(time_series, 0)
  match first_point {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), timestamp1)
      assert_eq(TimeSeriesDataPoint::value(point), 22.5)
    }
    None => assert_true(false)
  }
  
  let last_point = TimeSeries::get_point(time_series, 2)
  match last_point {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), timestamp3)
      assert_eq(TimeSeriesDataPoint::value(point), 22.8)
    }
    None => assert_true(false)
  }
  
  // Test getting non-existent point
  let non_existent = TimeSeries::get_point(time_series, 10)
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Time Series Aggregation Operations
test "time series aggregation operations" {
  let time_series = TimeSeries::new("test_series")
  
  // Add test data points
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1000L, 10.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(2000L, 20.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(3000L, 30.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(4000L, 40.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(5000L, 50.0))
  
  // Test average calculation
  let average = TimeSeries::average(time_series)
  match average {
    Some(avg) => assert_eq(avg, 30.0)
    None => assert_true(false)
  }
  
  // Test sum calculation
  let sum = TimeSeries::sum(time_series)
  match sum {
    Some(s) => assert_eq(s, 150.0)
    None => assert_true(false)
  }
  
  // Test min calculation
  let min = TimeSeries::min(time_series)
  match min {
    Some(m) => assert_eq(m, 10.0)
    None => assert_true(false)
  }
  
  // Test max calculation
  let max = TimeSeries::max(time_series)
  match max {
    Some(m) => assert_eq(m, 50.0)
    None => assert_true(false)
  }
  
  // Test empty series
  let empty_series = TimeSeries::new("empty_series")
  let empty_avg = TimeSeries::average(empty_series)
  match empty_avg {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: Time Series Range Operations
test "time series range operations" {
  let time_series = TimeSeries::new("range_test_series")
  
  // Add test data points
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1000L, 10.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(2000L, 20.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(3000L, 30.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(4000L, 40.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(5000L, 50.0))
  
  // Test range query
  let start_time = 2000L
  let end_time = 4000L
  let range_series = TimeSeries::get_range(time_series, start_time, end_time)
  
  assert_eq(TimeSeries::length(range_series), 3)
  
  let first_range_point = TimeSeries::get_point(range_series, 0)
  match first_range_point {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 2000L)
      assert_eq(TimeSeriesDataPoint::value(point), 20.0)
    }
    None => assert_true(false)
  }
  
  // Test range with no matches
  let no_match_series = TimeSeries::get_range(time_series, 6000L, 7000L)
  assert_eq(TimeSeries::length(no_match_series), 0)
}

// Test 5: Time Series Resampling
test "time series resampling" {
  let time_series = TimeSeries::new("resample_test_series")
  
  // Add test data points with irregular intervals
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1000L, 10.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1500L, 15.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(2000L, 20.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(2500L, 25.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(3000L, 30.0))
  
  // Test downsampling with averaging
  let interval = 1000L
  let resampled_series = TimeSeries::resample(time_series, interval, Average)
  
  assert_eq(TimeSeries::length(resampled_series), 3)
  
  let first_resampled = TimeSeries::get_point(resampled_series, 0)
  match first_resampled {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 1000L)
      assert_eq(TimeSeriesDataPoint::value(point), 12.5) // Average of 10.0 and 15.0
    }
    None => assert_true(false)
  }
  
  // Test upsampling with interpolation
  let upsampled_series = TimeSeries::resample(time_series, 500L, Linear)
  assert_eq(TimeSeries::length(upsampled_series), 5)
}

// Test 6: Time Series Window Operations
test "time series window operations" {
  let time_series = TimeSeries::new("window_test_series")
  
  // Add test data points
  for i in 0..=9 {
    let timestamp = 1000L + (i.to_int64() * 100L)
    let value = i.to_float()
    TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Test moving average with window size 3
  let window_size = 3
  let moving_avg_series = TimeSeries::moving_average(time_series, window_size)
  
  assert_eq(TimeSeries::length(moving_avg_series), 8) // Reduced by window_size-1
  
  let first_avg = TimeSeries::get_point(moving_avg_series, 0)
  match first_avg {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::value(point), 1.0) // Average of 0, 1, 2
    }
    None => assert_true(false)
  }
  
  // Test moving sum with window size 2
  let moving_sum_series = TimeSeries::moving_sum(time_series, 2)
  assert_eq(TimeSeries::length(moving_sum_series), 9) // Reduced by window_size-1
  
  let first_sum = TimeSeries::get_point(moving_sum_series, 0)
  match first_sum {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::value(point), 1.0) // Sum of 0 and 1
    }
    None => assert_true(false)
  }
}

// Test 7: Time Series Difference Operations
test "time series difference operations" {
  let time_series = TimeSeries::new("difference_test_series")
  
  // Add test data points
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(1000L, 10.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(2000L, 20.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(3000L, 25.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(4000L, 40.0))
  TimeSeries::add_point(time_series, TimeSeriesDataPoint::new(5000L, 30.0))
  
  // Test first order difference
  let diff_series = TimeSeries::difference(time_series, 1)
  assert_eq(TimeSeries::length(diff_series), 4) // Reduced by order
  
  let first_diff = TimeSeries::get_point(diff_series, 0)
  match first_diff {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 2000L)
      assert_eq(TimeSeriesDataPoint::value(point), 10.0) // 20.0 - 10.0
    }
    None => assert_true(false)
  }
  
  // Test percentage change
  let pct_change_series = TimeSeries::percentage_change(time_series, 1)
  assert_eq(TimeSeries::length(pct_change_series), 4) // Reduced by order
  
  let first_pct = TimeSeries::get_point(pct_change_series, 0)
  match first_pct {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 2000L)
      assert_eq(TimeSeriesDataPoint::value(point), 100.0) // (20.0 - 10.0) / 10.0 * 100
    }
    None => assert_true(false)
  }
}

// Test 8: Time Series Merge Operations
test "time series merge operations" {
  let series1 = TimeSeries::new("merge_series_1")
  let series2 = TimeSeries::new("merge_series_2")
  
  // Add data to series 1
  TimeSeries::add_point(series1, TimeSeriesDataPoint::new(1000L, 10.0))
  TimeSeries::add_point(series1, TimeSeriesDataPoint::new(2000L, 20.0))
  TimeSeries::add_point(series1, TimeSeriesDataPoint::new(3000L, 30.0))
  
  // Add data to series 2
  TimeSeries::add_point(series2, TimeSeriesDataPoint::new(1500L, 15.0))
  TimeSeries::add_point(series2, TimeSeriesDataPoint::new(2500L, 25.0))
  TimeSeries::add_point(series2, TimeSeriesDataPoint::new(3500L, 35.0))
  
  // Test merge by timestamp
  let merged_series = TimeSeries::merge_by_timestamp(series1, series2)
  assert_eq(TimeSeries::length(merged_series), 6)
  
  // Verify sorted order by timestamp
  let first_point = TimeSeries::get_point(merged_series, 0)
  match first_point {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 1000L)
      assert_eq(TimeSeriesDataPoint::value(point), 10.0)
    }
    None => assert_true(false)
  }
  
  let second_point = TimeSeries::get_point(merged_series, 1)
  match second_point {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), 1500L)
      assert_eq(TimeSeriesDataPoint::value(point), 15.0)
    }
    None => assert_true(false)
  }
}