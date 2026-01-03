// Azimuth Time Series Data Processing Tests
// This file contains test cases for time series data processing functionality

// Test 1: Time Series Data Point Creation
test "time series data point creation" {
  let timestamp = 1234567890L
  let value = 42.5
  let attributes = [
    ("service.name", StringValue("test-service")),
    ("operation.name", StringValue("test-operation"))
  ]
  
  let data_point = TimeSeriesDataPoint::new(timestamp, value, attributes)
  
  // Test data point properties
  assert_eq(TimeSeriesDataPoint::timestamp(data_point), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point), value)
  assert_eq(TimeSeriesDataPoint::attributes_count(data_point), 2)
  
  // Test attribute retrieval
  let service_name = TimeSeriesDataPoint::get_attribute(data_point, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false)
  }
}

// Test 2: Time Series Collection Operations
test "time series collection operations" {
  let collection = TimeSeriesCollection::new("cpu.usage", "percentage")
  
  // Test collection properties
  assert_eq(TimeSeriesCollection::name(collection), "cpu.usage")
  assert_eq(TimeSeriesCollection::unit(collection), "percentage")
  assert_eq(TimeSeriesCollection::size(collection), 0)
  
  // Add data points
  let point1 = TimeSeriesDataPoint::new(1234567890L, 25.5, [])
  let point2 = TimeSeriesDataPoint::new(1234567891L, 30.0, [])
  let point3 = TimeSeriesDataPoint::new(1234567892L, 27.8, [])
  
  TimeSeriesCollection::add_point(collection, point1)
  TimeSeriesCollection::add_point(collection, point2)
  TimeSeriesCollection::add_point(collection, point3)
  
  assert_eq(TimeSeriesCollection::size(collection), 3)
  
  // Test data point retrieval
  let retrieved_point = TimeSeriesCollection::get_point(collection, 1)
  match retrieved_point {
    Some(point) => assert_eq(TimeSeriesDataPoint::value(point), 30.0)
    None => assert_true(false)
  }
  
  // Test range queries
  let points_in_range = TimeSeriesCollection::get_points_in_range(
    collection, 
    1234567890L, 
    1234567891L
  )
  assert_eq(points_in_range.length(), 2)
}

// Test 3: Time Series Aggregation Operations
test "time series aggregation operations" {
  let collection = TimeSeriesCollection::new("response.time", "milliseconds")
  
  // Add test data
  for i = 0; i < 10; i = i + 1 {
    let timestamp = 1234567890L + i as Int64
    let value = 100.0 + (i as Float) * 10.0
    let point = TimeSeriesDataPoint::new(timestamp, value, [])
    TimeSeriesCollection::add_point(collection, point)
  }
  
  // Test average calculation
  let avg = TimeSeriesAggregator::average(collection)
  assert_eq(avg, 145.0)
  
  // Test min calculation
  let min = TimeSeriesAggregator::min(collection)
  assert_eq(min, 100.0)
  
  // Test max calculation
  let max = TimeSeriesAggregator::max(collection)
  assert_eq(max, 190.0)
  
  // Test sum calculation
  let sum = TimeSeriesAggregator::sum(collection)
  assert_eq(sum, 1450.0)
  
  // Test count calculation
  let count = TimeSeriesAggregator::count(collection)
  assert_eq(count, 10)
}

// Test 4: Time Series Windowing Operations
test "time series windowing operations" {
  let collection = TimeSeriesCollection::new("memory.usage", "megabytes")
  
  // Add data points with 1-second intervals
  for i = 0; i < 60; i = i + 1 {
    let timestamp = 1234567890L + (i as Int64)
    let value = 100.0 + (i as Float) * 0.5
    let point = TimeSeriesDataPoint::new(timestamp, value, [])
    TimeSeriesCollection::add_point(collection, point)
  }
  
  // Test time-based windowing (10-second windows)
  let windows = TimeSeriesWindower::time_windows(collection, 10L)
  assert_eq(windows.length(), 6)
  
  // Test first window (should contain 10 points)
  let first_window = windows[0]
  assert_eq(TimeSeriesWindow::size(first_window), 10)
  
  // Test window aggregation
  let window_avg = TimeSeriesWindow::average(first_window)
  assert_eq(window_avg, 102.25) // Average of values 100.0 to 104.5
}

// Test 5: Time Series Downsampling
test "time series downsampling" {
  let collection = TimeSeriesCollection::new("network.throughput", "bytes_per_second")
  
  // Add high-resolution data (1 point per second for 5 minutes)
  for i = 0; i < 300; i = i + 1 {
    let timestamp = 1234567890L + (i as Int64)
    let value = 1000.0 + (i as Float) * 10.0
    let point = TimeSeriesDataPoint::new(timestamp, value, [])
    TimeSeriesCollection::add_point(collection, point)
  }
  
  // Downsample to 1-minute resolution (60 points)
  let downsampled = TimeSeriesDownsampler::downsample(collection, 60L, Average)
  assert_eq(TimeSeriesCollection::size(downsampled), 5)
  
  // Verify downsampled values
  let first_point = TimeSeriesCollection::get_point(downsampled, 0).unwrap()
  let first_value = TimeSeriesDataPoint::value(first_point)
  assert_eq(first_value, 1245.0) // Average of first 60 points
}

// Test 6: Time Series Interpolation
test "time series interpolation" {
  let collection = TimeSeriesCollection::new("temperature", "celsius")
  
  // Add sparse data points
  TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(1234567890L, 20.0, []))
  TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(1234567950L, 30.0, [])) // 60 seconds later
  TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(1234568010L, 25.0, [])) // 60 seconds later
  
  // Test linear interpolation
  let interpolated_1 = TimeSeriesInterpolator::linear(collection, 1234567920L) // 30 seconds between first and second
  match interpolated_1 {
    Some(value) => assert_eq(value, 25.0) // Midpoint between 20.0 and 30.0
    None => assert_true(false)
  }
  
  let interpolated_2 = TimeSeriesInterpolator::linear(collection, 1234567980L) // 30 seconds between second and third
  match interpolated_2 {
    Some(value) => assert_eq(value, 27.5) // Midpoint between 30.0 and 25.0
    None => assert_true(false)
  }
  
  // Test interpolation outside range
  let outside_before = TimeSeriesInterpolator::linear(collection, 1234567880L)
  let outside_after = TimeSeriesInterpolator::linear(collection, 1234568020L)
  
  match outside_before {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  match outside_after {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 7: Time Series Anomaly Detection
test "time series anomaly detection" {
  let collection = TimeSeriesCollection::new("error.rate", "percentage")
  
  // Add normal data points
  for i = 0; i < 50; i = i + 1 {
    let timestamp = 1234567890L + (i as Int64)
    let value = 5.0 + (Random::float() * 2.0) // Normal range: 5.0-7.0
    let point = TimeSeriesDataPoint::new(timestamp, value, [])
    TimeSeriesCollection::add_point(collection, point)
  }
  
  // Add anomaly data points
  TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(1234567940L, 25.0, [])) // Spike
  TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(1234567950L, 0.5, []))  // Dip
  
  // Test anomaly detection using statistical method
  let anomalies = TimeSeriesAnomalyDetector::statistical(collection, 2.0) // 2 standard deviations
  assert_eq(anomalies.length(), 2)
  
  // Verify detected anomalies
  let first_anomaly = anomalies[0]
  assert_eq(TimeSeriesDataPoint::value(first_anomaly), 25.0)
  
  let second_anomaly = anomalies[1]
  assert_eq(TimeSeriesDataPoint::value(second_anomaly), 0.5)
}

// Test 8: Time Series Forecasting
test "time series forecasting" {
  let collection = TimeSeriesCollection::new("request.count", "requests_per_second")
  
  // Add seasonal data (simple sine wave pattern)
  for i = 0; i < 100; i = i + 1 {
    let timestamp = 1234567890L + (i as Int64)
    let value = 50.0 + 20.0 * ((i as Float * 0.1).sin()) // Sine wave with amplitude 20
    let point = TimeSeriesDataPoint::new(timestamp, value, [])
    TimeSeriesCollection::add_point(collection, point)
  }
  
  // Test simple moving average forecast
  let forecast = TimeSeriesForecaster::moving_average(collection, 10, 5) // 10-point window, 5 steps ahead
  assert_eq(forecast.length(), 5)
  
  // Test exponential smoothing forecast
  let exp_forecast = TimeSeriesForecaster::exponential_smoothing(collection, 0.3, 5)
  assert_eq(exp_forecast.length(), 5)
  
  // Verify forecast values are reasonable
  for value in forecast {
    assert_true(value >= 30.0 && value <= 70.0) // Should be within expected range
  }
}

// Test 9: Time Series Correlation Analysis
test "time series correlation analysis" {
  let collection1 = TimeSeriesCollection::new("cpu.usage", "percentage")
  let collection2 = TimeSeriesCollection::new("memory.usage", "percentage")
  
  // Add correlated data (when CPU goes up, memory goes up)
  for i = 0; i < 50; i = i + 1 {
    let timestamp = 1234567890L + (i as Int64)
    let base_value = 20.0 + (i as Float) * 0.5
    let cpu_value = base_value + Random::float() * 2.0
    let memory_value = base_value * 1.2 + Random::float() * 2.0
    
    TimeSeriesCollection::add_point(collection1, TimeSeriesDataPoint::new(timestamp, cpu_value, []))
    TimeSeriesCollection::add_point(collection2, TimeSeriesDataPoint::new(timestamp, memory_value, []))
  }
  
  // Test correlation calculation
  let correlation = TimeSeriesAnalyzer::correlation(collection1, collection2)
  assert_true(correlation > 0.7) // Should be strongly positive correlation
  
  // Test with uncorrelated data
  let collection3 = TimeSeriesCollection::new("network.io", "megabytes_per_second")
  
  // Add random data
  for i = 0; i < 50; i = i + 1 {
    let timestamp = 1234567890L + (i as Int64)
    let random_value = Random::float() * 100.0
    TimeSeriesCollection::add_point(collection3, TimeSeriesDataPoint::new(timestamp, random_value, []))
  }
  
  let uncorrelated = TimeSeriesAnalyzer::correlation(collection1, collection3)
  assert_true(uncorrelated.abs() < 0.3) // Should be weak or no correlation
}

// Test 10: Time Series Trend Analysis
test "time series trend analysis" {
  let collection = TimeSeriesCollection::new("user.growth", "users_per_day")
  
  // Add data with clear upward trend
  for i = 0; i < 100; i = i + 1 {
    let timestamp = 1234567890L + (i as Int64 * 86400L) // Daily data
    let value = 1000.0 + (i as Float) * 10.0 + Random::float() * 50.0 // Linear trend with noise
    let point = TimeSeriesDataPoint::new(timestamp, value, [])
    TimeSeriesCollection::add_point(collection, point)
  }
  
  // Test trend detection
  let trend = TimeSeriesAnalyzer::trend(collection)
  match trend {
    Upward => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test trend strength
  let strength = TimeSeriesAnalyzer::trend_strength(collection)
  assert_true(strength > 0.5) // Should be strong trend
  
  // Test linear regression
  let regression = TimeSeriesAnalyzer::linear_regression(collection)
  assert_true(regression.slope > 8.0) // Should be close to 10.0
  assert_true(regression.slope < 12.0)
  assert_true(regression.intercept > 950.0) // Should be close to 1000.0
  assert_true(regression.intercept < 1050.0)
  
  // Test R-squared
  assert_true(regression.r_squared > 0.8) // Should be good fit
}