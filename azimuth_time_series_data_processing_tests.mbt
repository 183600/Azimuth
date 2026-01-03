// Azimuth Telemetry System - Time Series Data Processing Tests
// This file contains test cases for time series data processing functionality

// Test 1: Time Series Point Creation and Validation
test "time series point creation and validation" {
  // Test basic time series point creation
  let timestamp = 1640995200000L // January 1, 2022 00:00:00 UTC
  let value = 42.5
  let point = TimeSeriesPoint::new(timestamp, value, Some(Attributes::new()))
  
  assert_eq(TimeSeriesPoint::timestamp(point), timestamp)
  assert_eq(TimeSeriesPoint::value(point), value)
  match TimeSeriesPoint::attributes(point) {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  // Test time series point with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "metric.name", StringValue("cpu_usage"))
  Attributes::set(attrs, "host.name", StringValue("server-01"))
  
  let point_with_attrs = TimeSeriesPoint::new(timestamp, 75.2, Some(attrs))
  let retrieved_attrs = TimeSeriesPoint::attributes(point_with_attrs)
  
  match retrieved_attrs {
    Some(attr) => {
      match Attributes::get(attr, "metric.name") {
        Some(StringValue(name)) => assert_eq(name, "cpu_usage")
        _ => assert_true(false)
      }
      match Attributes::get(attr, "host.name") {
        Some(StringValue(host)) => assert_eq(host, "server-01")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 2: Time Series Creation and Operations
test "time series creation and operations" {
  // Create a new time series
  let series = TimeSeries::new("cpu.usage", "percentage", "CPU usage percentage")
  
  assert_eq(TimeSeries::name(series), "cpu.usage")
  assert_eq(TimeSeries::unit(series), "percentage")
  match TimeSeries::description(series) {
    Some(desc) => assert_eq(desc, "CPU usage percentage")
    None => assert_true(false)
  }
  
  // Test adding points to time series
  let point1 = TimeSeriesPoint::new(1640995200000L, 25.5, None)
  let point2 = TimeSeriesPoint::new(1640995260000L, 30.2, None)
  let point3 = TimeSeriesPoint::new(1640995320000L, 45.8, None)
  
  let series_with_points = TimeSeries::add_point(TimeSeries::add_point(
    TimeSeries::add_point(series, point1), point2), point3)
  
  // Test point count
  assert_eq(TimeSeries::point_count(series_with_points), 3)
  
  // Test getting points
  let points = TimeSeries::points(series_with_points)
  assert_eq(points.length(), 3)
  
  // Test point order (should be in chronological order)
  assert_eq(TimeSeriesPoint::timestamp(points[0]), 1640995200000L)
  assert_eq(TimeSeriesPoint::timestamp(points[1]), 1640995260000L)
  assert_eq(TimeSeriesPoint::timestamp(points[2]), 1640995320000L)
  
  // Test getting point by index
  match TimeSeries::get_point(series_with_points, 1) {
    Some(point) => {
      assert_eq(TimeSeriesPoint::timestamp(point), 1640995260000L)
      assert_eq(TimeSeriesPoint::value(point), 30.2)
    }
    None => assert_true(false)
  }
  
  // Test getting point by invalid index
  match TimeSeries::get_point(series_with_points, 10) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Time Series Aggregation Operations
test "time series aggregation operations" {
  // Create time series with multiple points
  let series = TimeSeries::new("memory.usage", "percentage", "Memory usage percentage")
  
  let points = [
    TimeSeriesPoint::new(1640995200000L, 25.5, None),
    TimeSeriesPoint::new(1640995260000L, 30.2, None),
    TimeSeriesPoint::new(1640995320000L, 45.8, None),
    TimeSeriesPoint::new(1640995380000L, 50.1, None),
    TimeSeriesPoint::new(1640995440000L, 35.7, None)
  ]
  
  let mut series_with_points = series
  for point in points {
    series_with_points = TimeSeries::add_point(series_with_points, point)
  }
  
  // Test average aggregation
  let avg_value = TimeSeries::aggregate(series_with_points, Average)
  assert_true(avg_value > 37.0 && avg_value < 38.0) // Approximately 37.46
  
  // Test sum aggregation
  let sum_value = TimeSeries::aggregate(series_with_points, Sum)
  assert_true(sum_value > 187.0 && sum_value < 188.0) // Approximately 187.3
  
  // Test min aggregation
  let min_value = TimeSeries::aggregate(series_with_points, Min)
  assert_eq(min_value, 25.5)
  
  // Test max aggregation
  let max_value = TimeSeries::aggregate(series_with_points, Max)
  assert_eq(max_value, 50.1)
  
  // Test count aggregation
  let count_value = TimeSeries::aggregate(series_with_points, Count)
  assert_eq(count_value, 5.0)
}

// Test 4: Time Series Windowing Operations
test "time series windowing operations" {
  // Create time series with points spanning multiple hours
  let series = TimeSeries::new("request.count", "count", "Request count per minute")
  
  // Add points for 3 hours (180 minutes)
  let mut series_with_points = series
  for i in 0..180 {
    let timestamp = 1640995200000L + (i * 60000L) // 1 minute intervals
    let value = (i % 10).to_float() + 10.0 // Values between 10.0 and 19.0
    let point = TimeSeriesPoint::new(timestamp, value, None)
    series_with_points = TimeSeries::add_point(series_with_points, point)
  }
  
  // Test time-based windowing (1-hour windows)
  let windows = TimeSeries::time_windows(series_with_points, 3600000L) // 1 hour in milliseconds
  assert_eq(windows.length(), 3)
  
  // Test first window
  let first_window = windows[0]
  assert_eq(TimeSeries::point_count(first_window), 60)
  
  // Test sliding window operation
  let sliding_windows = TimeSeries::sliding_windows(series_with_points, 300000L, 60000L) // 5min window, 1min slide
  assert_eq(sliding_windows.length(), 175) // 180 - 5 + 1
  
  // Test first sliding window
  let first_sliding_window = sliding_windows[0]
  assert_eq(TimeSeries::point_count(first_sliding_window), 5)
  
  // Test aggregation on windows
  let window_averages = []
  for window in windows {
    let avg = TimeSeries::aggregate(window, Average)
    window_averages = Array::push(window_averages, avg)
  }
  assert_eq(window_averages.length(), 3)
}

// Test 5: Time Series Resampling Operations
test "time series resampling operations" {
  // Create time series with minute-level data
  let series = TimeSeries::new("temperature", "celsius", "Temperature readings")
  
  // Add 60 points (1 hour of minute-level data)
  let mut series_with_points = series
  for i in 0..60 {
    let timestamp = 1640995200000L + (i * 60000L) // 1 minute intervals
    let value = 20.0 + (i % 10).to_float() * 0.5 // Values between 20.0 and 24.5
    let point = TimeSeriesPoint::new(timestamp, value, None)
    series_with_points = TimeSeries::add_point(series_with_points, point)
  }
  
  // Test upsampling (from minute to 30-second)
  let upsampled = TimeSeries::resample(series_with_points, 30000L, LinearInterpolation)
  assert_eq(TimeSeries::point_count(upsampled), 119) // (60-1)*2 + 1
  
  // Test downsampling (from minute to 5-minute)
  let downsampled = TimeSeries::resample(series_with_points, 300000L, Average)
  assert_eq(TimeSeries::point_count(downsampled), 12)
  
  // Test downsampling with different strategies
  let downsampled_max = TimeSeries::resample(series_with_points, 300000L, Max)
  let downsampled_min = TimeSeries::resample(series_with_points, 300000L, Min)
  
  assert_eq(TimeSeries::point_count(downsampled_max), 12)
  assert_eq(TimeSeries::point_count(downsampled_min), 12)
  
  // Verify first downsampled point (average of first 5 minutes)
  let first_downsampled_point = TimeSeries::get_point(downsampled, 0)
  match first_downsampled_point {
    Some(point) => {
      assert_eq(TimeSeriesPoint::timestamp(point), 1640995200000L)
      assert_true(TimeSeriesPoint::value(point) > 21.0 && TimeSeriesPoint::value(point) < 22.0)
    }
    None => assert_true(false)
  }
}

// Test 6: Time Series Filtering Operations
test "time series filtering operations" {
  // Create time series with varying values
  let series = TimeSeries::new("latency", "ms", "Request latency")
  
  let mut series_with_points = series
  for i in 0..100 {
    let timestamp = 1640995200000L + (i * 60000L) // 1 minute intervals
    let value = 50.0 + (i % 20).to_float() * 5.0 // Values between 50.0 and 145.0
    let point = TimeSeriesPoint::new(timestamp, value, None)
    series_with_points = TimeSeries::add_point(series_with_points, point)
  }
  
  // Test value-based filtering
  let filtered_gt_100 = TimeSeries::filter_by_value(series_with_points, \v => v > 100.0)
  assert_eq(TimeSeries::point_count(filtered_gt_100), 45) // Values 105, 110, 115, 120, 125, 130, 135, 140, 145 (5 * 9 = 45)
  
  // Test time-based filtering
  let start_time = 1640995200000L + (30 * 60000L) // 30 minutes in
  let end_time = 1640995200000L + (60 * 60000L)   // 60 minutes in
  let filtered_by_time = TimeSeries::filter_by_time(series_with_points, start_time, end_time)
  assert_eq(TimeSeries::point_count(filtered_by_time), 30) // 30 minutes of data
  
  // Test attribute-based filtering
  let attrs = Attributes::new()
  Attributes::set(attrs, "region", StringValue("us-west"))
  
  let mut series_with_attrs = series
  for i in 0..50 {
    let timestamp = 1640995200000L + (i * 60000L)
    let value = 100.0 + i.to_float()
    
    let point_attrs = Attributes::new()
    if i % 2 == 0 {
      Attributes::set(point_attrs, "region", StringValue("us-west"))
    } else {
      Attributes::set(point_attrs, "region", StringValue("us-east"))
    }
    
    let point = TimeSeriesPoint::new(timestamp, value, Some(point_attrs))
    series_with_attrs = TimeSeries::add_point(series_with_attrs, point)
  }
  
  let filtered_by_region = TimeSeries::filter_by_attribute(series_with_attrs, "region", StringValue("us-west"))
  assert_eq(TimeSeries::point_count(filtered_by_region), 25)
}

// Test 7: Time Series Compression Operations
test "time series compression operations" {
  // Create time series with predictable patterns
  let series = TimeSeries::new("network.traffic", "mbps", "Network traffic")
  
  let mut series_with_points = series
  for i in 0..1000 {
    let timestamp = 1640995200000L + (i * 60000L) // 1 minute intervals
    let value = 100.0 + (i % 10).to_float() * 2.0 // Repeating pattern: 100, 102, 104, ..., 118
    let point = TimeSeriesPoint::new(timestamp, value, None)
    series_with_points = TimeSeries::add_point(series_with_points, point)
  }
  
  // Test delta compression
  let compressed_delta = TimeSeries::compress(series_with_points, DeltaCompression)
  assert_true(compressed_delta.size() < series_with_points.size())
  
  // Test gorilla compression (for time series data)
  let compressed_gorilla = TimeSeries::compress(series_with_points, GorillaCompression)
  assert_true(compressed_gorilla.size() < compressed_delta.size())
  
  // Test decompression
  let decompressed = TimeSeries::decompress(compressed_gorilla)
  assert_eq(TimeSeries::point_count(decompressed), TimeSeries::point_count(series_with_points))
  
  // Verify data integrity after decompression
  let original_points = TimeSeries::points(series_with_points)
  let decompressed_points = TimeSeries::points(decompressed)
  
  for i in 0..original_points.length() {
    assert_eq(TimeSeriesPoint::timestamp(original_points[i]), TimeSeriesPoint::timestamp(decompressed_points[i]))
    assert_eq(TimeSeriesPoint::value(original_points[i]), TimeSeriesPoint::value(decompressed_points[i]))
  }
}

// Test 8: Time Series Query Operations
test "time series query operations" {
  // Create multiple time series
  let cpu_series = TimeSeries::new("cpu.usage", "percentage", "CPU usage")
  let memory_series = TimeSeries::new("memory.usage", "percentage", "Memory usage")
  let network_series = TimeSeries::new("network.usage", "mbps", "Network usage")
  
  // Add data to CPU series
  let mut cpu_with_data = cpu_series
  for i in 0..60 {
    let timestamp = 1640995200000L + (i * 60000L)
    let value = 50.0 + (i % 20).to_float() * 2.0
    let point = TimeSeriesPoint::new(timestamp, value, None)
    cpu_with_data = TimeSeries::add_point(cpu_with_data, point)
  }
  
  // Add data to Memory series
  let mut memory_with_data = memory_series
  for i in 0..60 {
    let timestamp = 1640995200000L + (i * 60000L)
    let value = 60.0 + (i % 15).to_float() * 2.0
    let point = TimeSeriesPoint::new(timestamp, value, None)
    memory_with_data = TimeSeries::add_point(memory_with_data, point)
  }
  
  // Add data to Network series
  let mut network_with_data = network_series
  for i in 0..60 {
    let timestamp = 1640995200000L + (i * 60000L)
    let value = 10.0 + (i % 30).to_float() * 1.0
    let point = TimeSeriesPoint::new(timestamp, value, None)
    network_with_data = TimeSeries::add_point(network_with_data, point)
  }
  
  // Create time series collection
  let collection = TimeSeriesCollection::new()
  let collection_with_series = TimeSeriesCollection::add_series(
    TimeSeriesCollection::add_series(
      TimeSeriesCollection::add_series(collection, cpu_with_data),
      memory_with_data),
    network_with_data)
  
  // Test query by name
  let cpu_query_result = TimeSeriesCollection::query_by_name(collection_with_series, "cpu.usage")
  match cpu_query_result {
    Some(series) => assert_eq(TimeSeries::name(series), "cpu.usage")
    None => assert_true(false)
  }
  
  // Test query by pattern
  let usage_series = TimeSeriesCollection::query_by_pattern(collection_with_series, "*.usage")
  assert_eq(usage_series.length(), 3)
  
  // Test query by time range
  let start_time = 1640995200000L + (15 * 60000L) // 15 minutes in
  let end_time = 1640995200000L + (45 * 60000L)   // 45 minutes in
  let time_filtered = TimeSeriesCollection::query_by_time_range(collection_with_series, start_time, end_time)
  assert_eq(time_filtered.length(), 3)
  
  // Verify time filtering result
  let filtered_cpu = time_filtered[0]
  assert_eq(TimeSeries::point_count(filtered_cpu), 31) // 45 - 15 + 1 = 31 points
}