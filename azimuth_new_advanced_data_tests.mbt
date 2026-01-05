// Azimuth Telemetry System - New Advanced Data Processing Tests
// This file contains new test cases for advanced data processing functionality

// Test 1: Complex Attribute Filtering and Querying
test "complex attribute filtering and querying" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "attribute_filter_test_meter")
  
  // Create counter with complex attributes
  let counter = Meter::create_counter(meter, "complex_requests", Some("Complex requests"), Some("count"))
  
  // Define complex attribute sets
  let attrs1 = Attributes::new()
  Attributes::set(attrs1, "service", StringValue("user-service"))
  Attributes::set(attrs1, "endpoint", StringValue("/api/users/profile"))
  Attributes::set(attrs1, "method", StringValue("GET"))
  Attributes::set(attrs1, "status_code", IntValue(200))
  Attributes::set(attrs1, "latency_ms", IntValue(150))
  Attributes::set(attrs1, "user_tier", StringValue("premium"))
  
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "service", StringValue("user-service"))
  Attributes::set(attrs2, "endpoint", StringValue("/api/users/profile"))
  Attributes::set(attrs2, "method", StringValue("GET"))
  Attributes::set(attrs2, "status_code", IntValue(404))
  Attributes::set(attrs2, "latency_ms", IntValue(25))
  Attributes::set(attrs2, "user_tier", StringValue("basic"))
  
  let attrs3 = Attributes::new()
  Attributes::set(attrs3, "service", StringValue("order-service"))
  Attributes::set(attrs3, "endpoint", StringValue("/api/orders/create"))
  Attributes::set(attrs3, "method", StringValue("POST"))
  Attributes::set(attrs3, "status_code", IntValue(201))
  Attributes::set(attrs3, "latency_ms", IntValue(300))
  Attributes::set(attrs3, "user_tier", StringValue("premium"))
  
  // Record metrics with different attributes
  for i in 0..=10 {
    Counter::add(counter, 1.0, Some(attrs1))
  }
  
  for i in 0..=5 {
    Counter::add(counter, 1.0, Some(attrs2))
  }
  
  for i in 0..=8 {
    Counter::add(counter, 1.0, Some(attrs3))
  }
  
  // Test complex filtering
  let filter = AttributeFilter::new()
  AttributeFilter::add_condition(filter, "service", Equals("user-service"))
  AttributeFilter::add_condition(filter, "latency_ms", GreaterThan(100))
  
  let filtered_results = MetricsFilter::apply(counter, filter)
  
  // Verify filtering results
  match filtered_results {
    Some(results) => {
      // Should only include attrs1 (user-service with latency > 100ms)
      assert_eq(results.length(), 1)
      assert_eq(results[0].count, 11) // 10 + 1
    }
    None => assert_true(false)
  }
  
  // Test multi-condition filtering
  let multi_filter = AttributeFilter::new()
  AttributeFilter::add_condition(multi_filter, "user_tier", Equals("premium"))
  AttributeFilter::add_condition(multi_filter, "status_code", InRange([200, 201]))
  
  let multi_filtered_results = MetricsFilter::apply(counter, multi_filter)
  
  // Verify multi-condition filtering
  match multi_filtered_results {
    Some(results) => {
      // Should include attrs1 and attrs3 (both premium with status 200/201)
      assert_eq(results.length(), 2)
      assert_eq(results[0].count, 11) // attrs1
      assert_eq(results[1].count, 9)  // attrs3 (8 + 1)
    }
    None => assert_true(false)
  }
}

// Test 2: Time-based Data Aggregation with Sliding Windows
test "time-based data aggregation with sliding windows" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "sliding_window_test_meter")
  
  // Create histogram for time-based aggregation
  let histogram = Meter::create_histogram(meter, "response_times", Some("Response times"), Some("ms"))
  
  // Simulate time-based data points with timestamps
  let time_series_data = [
    (1000L, 50.0),   // timestamp, value
    (2000L, 75.0),
    (3000L, 60.0),
    (4000L, 90.0),
    (5000L, 85.0),
    (6000L, 70.0),
    (7000L, 95.0),
    (8000L, 80.0),
    (9000L, 65.0),
    (10000L, 100.0)
  ]
  
  // Record time series data
  for (timestamp, value) in time_series_data {
    let attrs = Attributes::new()
    Attributes::set(attrs, "timestamp", IntValue(Int::from_long(timestamp)))
    Histogram::record(histogram, value, Some(attrs))
  }
  
  // Test sliding window aggregation
  let window_size = 3000L // 3 seconds window
  let step_size = 1000L   // 1 second step
  
  let sliding_aggregator = SlidingWindowAggregator::new(histogram, window_size, step_size)
  let aggregated_windows = SlidingWindowAggregator::aggregate(sliding_aggregator)
  
  // Verify sliding window results
  assert_true(aggregated_windows.length() > 0)
  
  // Check first window (1000-4000)
  let first_window = aggregated_windows[0]
  assert_eq(first_window.start_time, 1000L)
  assert_eq(first_window.end_time, 4000L)
  assert_true(first_window.mean >= 60.0 && first_window.mean <= 65.0) // Average of 50, 75, 60
  assert_eq(first_window.count, 3)
  
  // Check middle window (4000-7000)
  let middle_window = aggregated_windows[3]
  assert_eq(middle_window.start_time, 4000L)
  assert_eq(middle_window.end_time, 7000L)
  assert_true(middle_window.mean >= 80.0 && middle_window.mean <= 85.0) // Average of 90, 85, 70
  assert_eq(middle_window.count, 3)
  
  // Check last window (7000-10000)
  let last_window = aggregated_windows[6]
  assert_eq(last_window.start_time, 7000L)
  assert_eq(last_window.end_time, 10000L)
  assert_true(last_window.mean >= 80.0 && last_window.mean <= 85.0) // Average of 95, 80, 65, 100
  assert_eq(last_window.count, 4)
}