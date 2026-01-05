// Azimuth Time Series Data Processing Tests
// This file contains test cases for time series data processing functionality

// Test 1: Time Series Data Point Creation
test "time series data point creation" {
  // Create a time series data point with timestamp and value
  let timestamp = 1609459200000L  // 2021-01-01 00:00:00 UTC
  let value = 42.5
  let data_point = TimeSeriesDataPoint::new(timestamp, value)
  
  assert_eq(TimeSeriesDataPoint::timestamp(data_point), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point), value)
}

// Test 2: Time Series Buffer Operations
test "time series buffer operations" {
  // Create a time series buffer with max capacity
  let buffer = TimeSeriesBuffer::new(100)
  
  // Add data points
  let timestamp1 = 1609459200000L
  let timestamp2 = 1609459260000L
  let timestamp3 = 1609459320000L
  
  TimeSeriesBuffer::add_point(buffer, TimeSeriesDataPoint::new(timestamp1, 10.5))
  TimeSeriesBuffer::add_point(buffer, TimeSeriesDataPoint::new(timestamp2, 15.3))
  TimeSeriesBuffer::add_point(buffer, TimeSeriesDataPoint::new(timestamp3, 12.7))
  
  // Test buffer size
  assert_eq(TimeSeriesBuffer::size(buffer), 3)
  
  // Test get point by index
  let point = TimeSeriesBuffer::get_point(buffer, 1)
  assert_eq(TimeSeriesDataPoint::timestamp(point), timestamp2)
  assert_eq(TimeSeriesDataPoint::value(point), 15.3)
  
  // Test get points in time range
  let start_time = 1609459230000L
  let end_time = 1609459350000L
  let points_in_range = TimeSeriesBuffer::get_points_in_range(buffer, start_time, end_time)
  assert_eq(points_in_range.length(), 2)
}

// Test 3: Time Series Aggregation Operations
test "time series aggregation operations" {
  let buffer = TimeSeriesBuffer::new(100)
  
  // Add data points
  for i in 0..=10 {
    let timestamp = 1609459200000L + (i * 60000L)  // 1 minute intervals
    let value = 10.0 + (i.to_float() * 1.5)
    TimeSeriesBuffer::add_point(buffer, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Test average calculation
  let avg = TimeSeriesAggregator::average(buffer)
  assert_true(avg > 15.0 && avg < 20.0)
  
  // Test min calculation
  let min = TimeSeriesAggregator::min(buffer)
  assert_eq(min, 10.0)
  
  // Test max calculation
  let max = TimeSeriesAggregator::max(buffer)
  assert_eq(max, 25.0)
  
  // Test sum calculation
  let sum = TimeSeriesAggregator::sum(buffer)
  assert_true(sum > 150.0 && sum < 200.0)
}

// Test 4: Time Series Resampling
test "time series resampling" {
  let buffer = TimeSeriesBuffer::new(100)
  
  // Add data points with 30-second intervals
  for i in 0..=20 {
    let timestamp = 1609459200000L + (i * 30000L)  // 30 second intervals
    let value = 10.0 + (i.to_float() * 0.5)
    TimeSeriesBuffer::add_point(buffer, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Resample to 1-minute intervals using average
  let resampled = TimeSeriesResampler::resample(buffer, 60000L, Average)
  
  // Should have approximately half the number of points
  assert_true(resampled.length() > 8 && resampled.length() < 12)
  
  // Verify first resampled point
  let first_point = TimeSeriesBuffer::get_point(resampled, 0)
  assert_eq(TimeSeriesDataPoint::timestamp(first_point), 1609459200000L)
  assert_true(TimeSeriesDataPoint::value(first_point) > 10.0 && TimeSeriesDataPoint::value(first_point) < 11.0)
}

// Test 5: Time Series Trend Analysis
test "time series trend analysis" {
  let buffer = TimeSeriesBuffer::new(100)
  
  // Add increasing trend data points
  for i in 0..=10 {
    let timestamp = 1609459200000L + (i * 60000L)
    let value = 10.0 + (i.to_float() * 2.0)  // Linear increase
    TimeSeriesBuffer::add_point(buffer, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Test trend detection
  let trend = TimeSeriesAnalyzer::detect_trend(buffer)
  match trend {
    Increasing => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test slope calculation
  let slope = TimeSeriesAnalyzer::calculate_slope(buffer)
  assert_true(slope > 1.5 && slope < 2.5)
  
  // Create decreasing trend data points
  let decreasing_buffer = TimeSeriesBuffer::new(100)
  for i in 0..=10 {
    let timestamp = 1609459200000L + (i * 60000L)
    let value = 30.0 - (i.to_float() * 2.0)  // Linear decrease
    TimeSeriesBuffer::add_point(decreasing_buffer, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  let decreasing_trend = TimeSeriesAnalyzer::detect_trend(decreasing_buffer)
  match decreasing_trend {
    Decreasing => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 6: Time Series Anomaly Detection
test "time series anomaly detection" {
  let buffer = TimeSeriesBuffer::new(100)
  
  // Add normal data points
  for i in 0..=20 {
    let timestamp = 1609459200000L + (i * 60000L)
    let value = 15.0 + (i.to_float() * 0.2)  // Slight increase
    TimeSeriesBuffer::add_point(buffer, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Add an anomaly
  let anomaly_timestamp = 1609459200000L + (21 * 60000L)
  TimeSeriesBuffer::add_point(buffer, TimeSeriesDataPoint::new(anomaly_timestamp, 50.0))  // Outlier
  
  // Test anomaly detection
  let anomalies = TimeSeriesAnomalyDetector::detect_anomalies(buffer, 2.0)  // 2 sigma threshold
  assert_eq(anomalies.length(), 1)
  
  let anomaly = anomalies[0]
  assert_eq(TimeSeriesDataPoint::timestamp(anomaly), anomaly_timestamp)
  assert_eq(TimeSeriesDataPoint::value(anomaly), 50.0)
}

// Test 7: Time Series Window Operations
test "time series window operations" {
  let buffer = TimeSeriesBuffer::new(100)
  
  // Add data points
  for i in 0..=30 {
    let timestamp = 1609459200000L + (i * 60000L)
    let value = 10.0 + (i.to_float() * 0.5)
    TimeSeriesBuffer::add_point(buffer, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Test sliding window average
  let window_size = 5
  let moving_avg = TimeSeriesWindow::moving_average(buffer, window_size)
  
  // Should have (original_size - window_size + 1) points
  assert_eq(moving_avg.length(), 27)
  
  // Test first moving average value
  let first_avg = TimeSeriesBuffer::get_point(moving_avg, 0)
  let expected_first_avg = (10.0 + 10.5 + 11.0 + 11.5 + 12.0) / 5.0
  assert_true(TimeSeriesDataPoint::value(first_avg) > expected_first_avg - 0.01 && 
              TimeSeriesDataPoint::value(first_avg) < expected_first_avg + 0.01)
}

// Test 8: Time Series Forecasting
test "time series forecasting" {
  let buffer = TimeSeriesBuffer::new(100)
  
  // Add seasonal data points (simple sine wave pattern)
  for i in 0..=50 {
    let timestamp = 1609459200000L + (i * 3600000L)  // 1 hour intervals
    let value = 20.0 + (10.0 * ((i % 24).to_float() / 24.0 * 3.14159).sin())  // Daily pattern
    TimeSeriesBuffer::add_point(buffer, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Test simple linear forecasting
  let forecast_points = 10
  let forecast = TimeSeriesForecaster::linear_forecast(buffer, forecast_points)
  
  assert_eq(forecast.length(), forecast_points)
  
  // Verify forecast timestamps are sequential
  for i in 1..=forecast_points - 1 {
    let prev_point = forecast[i - 1]
    let curr_point = forecast[i]
    let time_diff = TimeSeriesDataPoint::timestamp(curr_point) - TimeSeriesDataPoint::timestamp(prev_point)
    assert_eq(time_diff, 3600000L)  // 1 hour interval
  }
}

// Test 9: Time Series Compression
test "time series compression" {
  let buffer = TimeSeriesBuffer::new(100)
  
  // Add data points
  for i in 0..=100 {
    let timestamp = 1609459200000L + (i * 60000L)
    let value = 10.0 + (i.to_float() * 0.1)
    TimeSeriesBuffer::add_point(buffer, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Test time series compression
  let compressed = TimeSeriesCompressor::compress(buffer, 0.1)  // 0.1 tolerance
  
  // Compressed data should have fewer points
  assert_true(compressed.length() < buffer.length())
  assert_true(compressed.length() > 0)
  
  // Verify compression preserves data within tolerance
  for i in 0..=compressed.length() - 1 {
    let compressed_point = TimeSeriesBuffer::get_point(compressed, i)
    let original_timestamp = TimeSeriesDataPoint::timestamp(compressed_point)
    
    // Find corresponding point in original
    let original_point = TimeSeriesBuffer::get_point(buffer, i * (buffer.length() / compressed.length()))
    let value_diff = TimeSeriesDataPoint::value(compressed_point) - TimeSeriesDataPoint::value(original_point)
    assert_true(value_diff.abs() < 0.1)
  }
}

// Test 10: Time Series Metadata
test "time series metadata" {
  // Create a time series with metadata
  let metadata = TimeSeriesMetadata::new(
    "temperature_sensor",
    "Temperature readings from sensor A",
    "celsius",
    Some("sensor-a-001"),
    Some(["temperature", "sensor", "room-1"])
  )
  
  assert_eq(TimeSeriesMetadata::name(metadata), "temperature_sensor")
  assert_eq(TimeSeriesMetadata::description(metadata), "Temperature readings from sensor A")
  assert_eq(TimeSeriesMetadata::unit(metadata), "celsius")
  
  match TimeSeriesMetadata::source(metadata) {
    Some(source) => assert_eq(source, "sensor-a-001")
    None => assert_true(false)
  }
  
  let tags = TimeSeriesMetadata::tags(metadata)
  match tags {
    Some(tag_list) => {
      assert_eq(tag_list.length(), 3)
      assert_eq(tag_list[0], "temperature")
      assert_eq(tag_list[1], "sensor")
      assert_eq(tag_list[2], "room-1")
    }
    None => assert_true(false)
  }
}