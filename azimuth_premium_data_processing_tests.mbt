// Azimuth Premium Data Processing Tests
// This file contains high-quality test cases for advanced data processing functionality

// Test 1: Advanced Data Aggregation with Multiple Dimensions
test "advanced data aggregation with multiple dimensions" {
  let telemetry_data = [
    ("service_a", "endpoint_1", 100, 200),
    ("service_a", "endpoint_2", 150, 300),
    ("service_b", "endpoint_1", 200, 400),
    ("service_b", "endpoint_2", 250, 500),
    ("service_a", "endpoint_1", 120, 220),
    ("service_b", "endpoint_1", 180, 380)
  ]
  
  // Group by service
  let mut service_aggregates = Map::new()
  for (service, endpoint, latency, throughput) in telemetry_data {
    let current = Map::get(service_aggregates, service).unwrap_or((0, 0, 0))
    let updated = (current.0 + 1, current.1 + latency, current.2 + throughput)
    Map::set(service_aggregates, service, updated)
  }
  
  // Verify service_a aggregation
  match Map::get(service_aggregates, "service_a") {
    Some((count, total_latency, total_throughput)) => {
      assert_eq(count, 3)
      assert_eq(total_latency, 370)
      assert_eq(total_throughput, 720)
    }
    None => assert_true(false)
  }
  
  // Verify service_b aggregation
  match Map::get(service_aggregates, "service_b") {
    Some((count, total_latency, total_throughput)) => {
      assert_eq(count, 3)
      assert_eq(total_latency, 630)
      assert_eq(total_throughput, 1280)
    }
    None => assert_true(false)
  }
}

// Test 2: Time Series Data Processing with Windowing
test "time series data processing with windowing" {
  let time_series_data = [
    (1000L, 10.5),
    (1005L, 12.3),
    (1010L, 11.8),
    (1015L, 13.2),
    (1020L, 14.1),
    (1025L, 15.7),
    (1030L, 14.9),
    (1035L, 16.3),
    (1040L, 17.2),
    (1045L, 18.1)
  ]
  
  // Process with 20-second windows
  let window_size = 20L
  let mut windowed_data = Map::new()
  
  for (timestamp, value) in time_series_data {
    let window_start = (timestamp / window_size) * window_size
    let current_values = Map::get(windowed_data, window_start).unwrap_or([])
    let updated_values = Array::append(current_values, value)
    Map::set(windowed_data, window_start, updated_values)
  }
  
  // Verify first window (1000-1019)
  match Map::get(windowed_data, 1000L) {
    Some(values) => {
      assert_eq(values.length(), 4)
      let sum = Array::fold(values, 0, (acc, val) => acc + val)
      assert_eq(sum, 47.8)
    }
    None => assert_true(false)
  }
  
  // Verify second window (1020-1039)
  match Map::get(windowed_data, 1020L) {
    Some(values) => {
      assert_eq(values.length(), 4)
      let sum = Array::fold(values, 0, (acc, val) => acc + val)
      assert_eq(sum, 62.0)
    }
    None => assert_true(false)
  }
  
  // Verify third window (1040-1059)
  match Map::get(windowed_data, 1040L) {
    Some(values) => {
      assert_eq(values.length(), 2)
      let sum = Array::fold(values, 0, (acc, val) => acc + val)
      assert_eq(sum, 35.3)
    }
    None => assert_true(false)
  }
}

// Test 3: Data Quality Validation with Complex Rules
test "data quality validation with complex rules" {
  let validation_rules = [
    ("latency", (min, 0, max, 1000)),
    ("throughput", (min, 10, max, 10000)),
    ("error_rate", (min, 0, max, 1.0)),
    ("availability", (min, 0.9, max, 1.0))
  ]
  
  let test_data = [
    ("valid_data", [
      ("latency", FloatValue(150.0)),
      ("throughput", FloatValue(5000.0)),
      ("error_rate", FloatValue(0.05)),
      ("availability", FloatValue(0.99))
    ]),
    ("invalid_latency", [
      ("latency", FloatValue(1500.0)),
      ("throughput", FloatValue(5000.0)),
      ("error_rate", FloatValue(0.05)),
      ("availability", FloatValue(0.99))
    ]),
    ("invalid_throughput", [
      ("latency", FloatValue(150.0)),
      ("throughput", FloatValue(5.0)),
      ("error_rate", FloatValue(0.05)),
      ("availability", FloatValue(0.99))
    ]),
    ("multiple_invalid", [
      ("latency", FloatValue(2000.0)),
      ("throughput", FloatValue(5.0)),
      ("error_rate", FloatValue(1.5)),
      ("availability", FloatValue(0.8))
    ])
  ]
  
  for (data_name, data_points) in test_data {
    let mut validation_errors = []
    
    for (field_name, (rule_type, min_val, max_val)) in validation_rules {
      match Map::get(data_points.to_map(), field_name) {
        Some(FloatValue(value)) => {
          if value < min_val || value > max_val {
            let error_msg = field_name + " out of range: " + value.to_string()
            validation_errors = Array::append(validation_errors, error_msg)
          }
        }
        _ => {
          let error_msg = "Missing or invalid field: " + field_name
          validation_errors = Array::append(validation_errors, error_msg)
        }
      }
    }
    
    match data_name {
      "valid_data" => assert_eq(validation_errors.length(), 0)
      "invalid_latency" => {
        assert_eq(validation_errors.length(), 1)
        assert_true(String::contains(validation_errors[0], "latency out of range"))
      }
      "invalid_throughput" => {
        assert_eq(validation_errors.length(), 1)
        assert_true(String::contains(validation_errors[0], "throughput out of range"))
      }
      "multiple_invalid" => assert_eq(validation_errors.length(), 4)
      _ => assert_true(false)
    }
  }
}

// Test 4: Real-time Data Stream Processing
test "real-time data stream processing" {
  let stream_data = [
    (1L, "metric1", 10.5),
    (2L, "metric2", 20.3),
    (3L, "metric1", 11.2),
    (4L, "metric3", 30.7),
    (5L, "metric2", 21.8),
    (6L, "metric1", 12.9)
  ]
  
  // Process stream with sliding window of size 3
  let window_size = 3
  let mut sliding_windows = Map::new()
  
  for (timestamp, metric_name, value) in stream_data {
    let metric_windows = Map::get(sliding_windows, metric_name).unwrap_or([])
    let updated_windows = Array::append(metric_windows, (timestamp, value))
    
    // Keep only last window_size elements
    let trimmed_windows = if updated_windows.length() > window_size {
      Array::slice(updated_windows, updated_windows.length() - window_size, updated_windows.length())
    } else {
      updated_windows
    }
    
    Map::set(sliding_windows, metric_name, trimmed_windows)
  }
  
  // Verify metric1 windows
  match Map::get(sliding_windows, "metric1") {
    Some(windows) => {
      assert_eq(windows.length(), 3)
      assert_eq(windows[0], (1L, 10.5))
      assert_eq(windows[1], (3L, 11.2))
      assert_eq(windows[2], (6L, 12.9))
      
      // Calculate moving average
      let sum = Array::fold(windows, 0.0, (acc, window) => acc + window.1)
      let avg = sum / windows.length().to_float()
      assert_true(avg > 11.0 && avg < 12.0)
    }
    None => assert_true(false)
  }
  
  // Verify metric2 windows
  match Map::get(sliding_windows, "metric2") {
    Some(windows) => {
      assert_eq(windows.length(), 2)
      assert_eq(windows[0], (2L, 20.3))
      assert_eq(windows[1], (5L, 21.8))
    }
    None => assert_true(false)
  }
  
  // Verify metric3 windows
  match Map::get(sliding_windows, "metric3") {
    Some(windows) => {
      assert_eq(windows.length(), 1)
      assert_eq(windows[0], (4L, 30.7))
    }
    None => assert_true(false)
  }
}

// Test 5: Complex Data Transformation Pipeline
test "complex data transformation pipeline" {
  let raw_data = [
    ("service_a", "endpoint_1", 100, "success"),
    ("service_a", "endpoint_2", 200, "error"),
    ("service_b", "endpoint_1", 150, "success"),
    ("service_b", "endpoint_2", 300, "timeout"),
    ("service_a", "endpoint_1", 120, "success"),
    ("service_b", "endpoint_1", 180, "success")
  ]
  
  // Pipeline stages:
  // 1. Filter successful requests
  // 2. Group by service
  // 3. Calculate statistics
  // 4. Apply transformation
  
  // Stage 1: Filter successful requests
  let successful_requests = Array::filter(raw_data, (data) => {
    let (_, _, _, status) = data
    status == "success"
  })
  assert_eq(successful_requests.length(), 4)
  
  // Stage 2: Group by service
  let mut grouped_data = Map::new()
  for (service, endpoint, latency, _) in successful_requests {
    let service_data = Map::get(grouped_data, service).unwrap_or([])
    let updated_data = Array::append(service_data, (endpoint, latency))
    Map::set(grouped_data, service, updated_data)
  }
  
  // Stage 3: Calculate statistics
  let mut statistics = Map::new()
  for (service, data_points) in grouped_data.to_map() {
    let latencies = Array::map(data_points, (point) => point.1)
    let total = Array::fold(latencies, 0, (acc, val) => acc + val)
    let count = latencies.length()
    let avg = total / count
    
    let min_val = Array::fold(latencies, latencies[0], (acc, val) => 
      if val < acc { val } else { acc })
    let max_val = Array::fold(latencies, latencies[0], (acc, val) => 
      if val > acc { val } else { acc })
    
    Map::set(statistics, service, (count, avg, min_val, max_val))
  }
  
  // Stage 4: Apply transformation
  let mut transformed_data = []
  for (service, (count, avg, min_val, max_val)) in statistics.to_map() {
    let performance_score = if avg < 150 { "high" } else if avg < 250 { "medium" } else { "low" }
    let transformed = (service, count, avg, performance_score)
    transformed_data = Array::append(transformed_data, transformed)
  }
  
  // Verify transformed data
  assert_eq(transformed_data.length(), 2)
  
  let service_a_data = Array::find(transformed_data, (data) => data.0 == "service_a").unwrap()
  assert_eq(service_a_data.0, "service_a")
  assert_eq(service_a_data.1, 2) // count
  assert_eq(service_a_data.2, 110) // avg
  assert_eq(service_a_data.3, "high") // performance score
  
  let service_b_data = Array::find(transformed_data, (data) => data.0 == "service_b").unwrap()
  assert_eq(service_b_data.0, "service_b")
  assert_eq(service_b_data.1, 2) // count
  assert_eq(service_b_data.2, 165) // avg
  assert_eq(service_b_data.3, "medium") // performance score
}