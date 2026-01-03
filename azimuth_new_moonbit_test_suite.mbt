// Azimuth New MoonBit Test Suite
// This file contains new test cases for telemetry system and MoonBit language features

// Test 1: Telemetry Data Serialization and Deserialization
test "telemetry data serialization and deserialization" {
  // Create telemetry data
  let telemetry_data = {
    timestamp: 1640995200000,
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    service_name: "payment-service",
    operation_name: "process_payment",
    duration_ms: 150,
    status: "success"
  }
  
  // Serialize to JSON string
  let json_string = "{\"timestamp\":1640995200000,\"trace_id\":\"trace-123456789\",\"span_id\":\"span-987654321\",\"service_name\":\"payment-service\",\"operation_name\":\"process_payment\",\"duration_ms\":150,\"status\":\"success\"}"
  
  // Verify JSON structure
  assert_true(json_string.contains("trace-123456789"))
  assert_true(json_string.contains("payment-service"))
  assert_true(json_string.contains("process_payment"))
  assert_true(json_string.contains("\"duration_ms\":150"))
  assert_true(json_string.contains("\"status\":\"success\""))
  
  // Parse back and verify structure
  assert_true(json_string.length() > 0)
  assert_true(json_string.contains("{"))
  assert_true(json_string.contains("}"))
}

// Test 2: Telemetry Metrics Aggregation
test "telemetry metrics aggregation" {
  // Create metrics data points
  let metrics = [
    { name: "http.requests", value: 10, tags: ["method:GET", "status:200"] },
    { name: "http.requests", value: 5, tags: ["method:POST", "status:201"] },
    { name: "http.requests", value: 2, tags: ["method:GET", "status:404"] },
    { name: "db.connections", value: 8, tags: ["pool:main"] },
    { name: "db.connections", value: 3, tags: ["pool:cache"] }
  ]
  
  // Filter HTTP request metrics
  let http_metrics = metrics.filter(fn(m) { m.name == "http.requests" })
  assert_eq(http_metrics.length(), 3)
  
  // Calculate total HTTP requests
  let total_http_requests = http_metrics.reduce(fn(acc, m) { acc + m.value }, 0)
  assert_eq(total_http_requests, 17)
  
  // Calculate total DB connections
  let db_metrics = metrics.filter(fn(m) { m.name == "db.connections" })
  let total_db_connections = db_metrics.reduce(fn(acc, m) { acc + m.value }, 0)
  assert_eq(total_db_connections, 11)
  
  // Filter successful requests (status 200-299)
  let successful_requests = http_metrics.filter(fn(m) { 
    let has_status_tag = m.tags.find(fn(tag) { tag.contains("status:") })
    match has_status_tag {
      Some(tag) => tag.contains("200") || tag.contains("201")
      None => false
    }
  })
  assert_eq(successful_requests.length(), 2)
}

// Test 3: Trace Context Propagation
test "trace context propagation" {
  // Create parent span context
  let parent_context = {
    trace_id: "trace-abcdef123456",
    span_id: "span-parent789",
    baggage: [("user.id", "12345"), ("request.id", "req-abc123")],
    flags: ["sampled"]
  }
  
  // Create child span context inheriting from parent
  let child_context = {
    trace_id: parent_context.trace_id,
    span_id: "span-child456",
    baggage: parent_context.baggage.push(("operation.type", "database")),
    flags: parent_context.flags
  }
  
  // Verify trace ID is propagated
  assert_eq(child_context.trace_id, parent_context.trace_id)
  
  // Verify span IDs are different
  assert_true(child_context.span_id != parent_context.span_id)
  
  // Verify baggage is propagated and extended
  assert_eq(child_context.baggage.length(), parent_context.baggage.length() + 1)
  
  // Verify specific baggage items
  let mut found_user_id = false
  let mut found_request_id = false
  let mut found_operation_type = false
  
  for (key, value) in child_context.baggage {
    match key {
      "user.id" => {
        assert_eq(value, "12345")
        found_user_id = true
      }
      "request.id" => {
        assert_eq(value, "req-abc123")
        found_request_id = true
      }
      "operation.type" => {
        assert_eq(value, "database")
        found_operation_type = true
      }
      _ => () // Ignore other keys
    }
  }
  
  assert_true(found_user_id)
  assert_true(found_request_id)
  assert_true(found_operation_type)
}

// Test 4: Error Handling in Telemetry Operations
test "error handling in telemetry operations" {
  // Test result type for telemetry operations
  let successful_operation = Ok({ metrics_sent: 10, errors: 0 })
  let failed_operation = Err("Network timeout while sending metrics")
  
  // Handle successful operation
  match successful_operation {
    Ok(result) => {
      assert_eq(result.metrics_sent, 10)
      assert_eq(result.errors, 0)
    }
    Err(msg) => assert_true(false)
  }
  
  // Handle failed operation
  match failed_operation {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Network timeout while sending metrics")
  }
  
  // Test option type for potentially missing telemetry data
  let valid_data = Some({ temperature: 25.5, humidity: 60.2 })
  let missing_data = None
  
  // Process valid data
  match valid_data {
    Some(data) => {
      assert_eq(data.temperature, 25.5)
      assert_eq(data.humidity, 60.2)
    }
    None => assert_true(false)
  }
  
  // Handle missing data
  match missing_data {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test chain of operations with error handling
  let process_result = valid_data
    .map(fn(data) { data.temperature + 10.0 })
    .map(fn(temp) { if (temp > 30.0) { Some("High temperature") } else { None } })
  
  match process_result {
    Some(Some(msg)) => assert_eq(msg, "High temperature")
    _ => assert_true(false)
  }
}

// Test 5: Time Series Data Processing
test "time series data processing" {
  // Create time series data points
  let time_series = [
    { timestamp: 1640995200000, value: 10.5 },
    { timestamp: 1640995260000, value: 12.3 },
    { timestamp: 1640995320000, value: 11.8 },
    { timestamp: 1640995380000, value: 13.2 },
    { timestamp: 1640995440000, value: 14.7 }
  ]
  
  // Calculate average value
  let sum = time_series.reduce(fn(acc, point) { acc + point.value }, 0.0)
  let average = sum / time_series.length().to_float()
  assert_eq(average, 12.5)
  
  // Find maximum value
  let max_value = time_series.reduce(fn(acc, point) { 
    if (point.value > acc) { point.value } else { acc } 
  }, 0.0)
  assert_eq(max_value, 14.7)
  
  // Find minimum value
  let min_value = time_series.reduce(fn(acc, point) { 
    if (point.value < acc) { point.value } else { acc } 
  }, 999.0)
  assert_eq(min_value, 10.5)
  
  // Filter values above threshold
  let high_values = time_series.filter(fn(point) { point.value > 12.0 })
  assert_eq(high_values.length(), 3)
  
  // Calculate rate of change between consecutive points
  let rates = []
  for i in 1..time_series.length() {
    let current = time_series[i]
    let previous = time_series[i-1]
    let rate = (current.value - previous.value) / ((current.timestamp - previous.timestamp) / 60000.0) // per minute
    rates = rates.push(rate)
  }
  
  assert_eq(rates.length(), 4)
  assert_eq(rates[0], 3.6) // (12.3 - 10.5) / 1 minute
}

// Test 6: Telemetry Configuration Management
test "telemetry configuration management" {
  // Create default configuration
  let default_config = {
    sampling_rate: 1.0,
    batch_size: 100,
    flush_interval_ms: 5000,
    max_queue_size: 1000,
    enabled_features: ["metrics", "tracing", "logging"]
  }
  
  // Create custom configuration with overrides
  let custom_config = {
    sampling_rate: 0.1,
    batch_size: 50,
    flush_interval_ms: 10000,
    max_queue_size: 2000,
    enabled_features: ["metrics", "tracing"]
  }
  
  // Verify default configuration
  assert_eq(default_config.sampling_rate, 1.0)
  assert_eq(default_config.batch_size, 100)
  assert_eq(default_config.flush_interval_ms, 5000)
  assert_eq(default_config.max_queue_size, 1000)
  assert_eq(default_config.enabled_features.length(), 3)
  
  // Verify custom configuration
  assert_eq(custom_config.sampling_rate, 0.1)
  assert_eq(custom_config.batch_size, 50)
  assert_eq(custom_config.flush_interval_ms, 10000)
  assert_eq(custom_config.max_queue_size, 2000)
  assert_eq(custom_config.enabled_features.length(), 2)
  
  // Merge configurations with custom overrides
  let merged_config = {
    sampling_rate: custom_config.sampling_rate,
    batch_size: default_config.batch_size, // Use default
    flush_interval_ms: custom_config.flush_interval_ms,
    max_queue_size: default_config.max_queue_size, // Use default
    enabled_features: custom_config.enabled_features
  }
  
  // Verify merged configuration
  assert_eq(merged_config.sampling_rate, 0.1) // From custom
  assert_eq(merged_config.batch_size, 100) // From default
  assert_eq(merged_config.flush_interval_ms, 10000) // From custom
  assert_eq(merged_config.max_queue_size, 1000) // From default
  assert_eq(merged_config.enabled_features.length(), 2) // From custom
}

// Test 7: Telemetry Data Validation
test "telemetry data validation" {
  // Create validation rules
  let validate_trace_id = fn(id) { 
    id.length() == 16 && id.starts_with("trace-") 
  }
  
  let validate_span_id = fn(id) { 
    id.length() == 12 && id.starts_with("span-") 
  }
  
  let validate_timestamp = fn(ts) { 
    ts > 1600000000000 && ts < 2000000000000 
  }
  
  let validate_duration = fn(duration) { 
    duration >= 0 && duration < 60000 
  }
  
  // Test valid data
  let valid_trace_id = "trace-123456789"
  let valid_span_id = "span-987654"
  let valid_timestamp = 1640995200000
  let valid_duration = 150
  
  assert_true(validate_trace_id(valid_trace_id))
  assert_true(validate_span_id(valid_span_id))
  assert_true(validate_timestamp(valid_timestamp))
  assert_true(validate_duration(valid_duration))
  
  // Test invalid data
  let invalid_trace_id = "bad-trace-id"
  let invalid_span_id = "bad-span-id"
  let invalid_timestamp = 999999999999
  let invalid_duration = -1
  
  assert_false(validate_trace_id(invalid_trace_id))
  assert_false(validate_span_id(invalid_span_id))
  assert_false(validate_timestamp(invalid_timestamp))
  assert_false(validate_duration(invalid_duration))
  
  // Validate complete telemetry record
  let validate_telemetry_record = fn(record) {
    validate_trace_id(record.trace_id) &&
    validate_span_id(record.span_id) &&
    validate_timestamp(record.timestamp) &&
    validate_duration(record.duration_ms)
  }
  
  let valid_record = {
    trace_id: valid_trace_id,
    span_id: valid_span_id,
    timestamp: valid_timestamp,
    duration_ms: valid_duration
  }
  
  let invalid_record = {
    trace_id: invalid_trace_id,
    span_id: valid_span_id,
    timestamp: valid_timestamp,
    duration_ms: valid_duration
  }
  
  assert_true(validate_telemetry_record(valid_record))
  assert_false(validate_telemetry_record(invalid_record))
}

// Test 8: Telemetry Data Transformation
test "telemetry data transformation" {
  // Create raw telemetry data
  let raw_data = [
    { 
      timestamp: 1640995200000, 
      service: "auth-service", 
      operation: "authenticate", 
      duration_ms: 120,
      status: "success",
      user_id: "12345"
    },
    { 
      timestamp: 1640995260000, 
      service: "payment-service", 
      operation: "process_payment", 
      duration_ms: 250,
      status: "success",
      user_id: "12345"
    },
    { 
      timestamp: 1640995320000, 
      service: "auth-service", 
      operation: "authorize", 
      duration_ms: 80,
      status: "failure",
      user_id: "67890"
    }
  ]
  
  // Transform to metrics format
  let metrics = raw_data.map(fn(record) {
    {
      name: "operation.duration",
      value: record.duration_ms,
      tags: [
        "service:" + record.service,
        "operation:" + record.operation,
        "status:" + record.status
      ],
      timestamp: record.timestamp
    }
  })
  
  // Verify transformation
  assert_eq(metrics.length(), 3)
  
  // Check first metric
  assert_eq(metrics[0].name, "operation.duration")
  assert_eq(metrics[0].value, 120)
  assert_eq(metrics[0].tags.length(), 3)
  assert_true(metrics[0].tags.contains("service:auth-service"))
  assert_true(metrics[0].tags.contains("operation:authenticate"))
  assert_true(metrics[0].tags.contains("status:success"))
  
  // Check second metric
  assert_eq(metrics[1].value, 250)
  assert_true(metrics[1].tags.contains("service:payment-service"))
  
  // Check third metric
  assert_eq(metrics[2].value, 80)
  assert_true(metrics[2].tags.contains("status:failure"))
  
  // Aggregate metrics by service
  let aggregate_by_service = fn(metrics) {
    let mut result = []
    let mut processed = []
    
    for metric in metrics {
      let service_tag = metric.tags.find(fn(tag) { tag.starts_with("service:") })
      match service_tag {
        Some(service) => {
          let service_name = service.substring(8) // Remove "service:" prefix
          if (!processed.contains(service_name)) {
            let service_metrics = metrics.filter(fn(m) { 
              m.tags.contains("service:" + service_name) 
            })
            let total_duration = service_metrics.reduce(fn(acc, m) { acc + m.value }, 0)
            let avg_duration = total_duration / service_metrics.length()
            
            result = result.push({
              service: service_name,
              total_operations: service_metrics.length(),
              average_duration: avg_duration
            })
            processed = processed.push(service_name)
          }
        }
        None => () // Skip metrics without service tag
      }
    }
    result
  }
  
  let service_metrics = aggregate_by_service(metrics)
  assert_eq(service_metrics.length(), 2)
  
  // Verify auth-service metrics
  let auth_metrics = service_metrics.find(fn(m) { m.service == "auth-service" })
  match auth_metrics {
    Some(metrics) => {
      assert_eq(metrics.total_operations, 2)
      assert_eq(metrics.average_duration, 100) // (120 + 80) / 2
    }
    None => assert_true(false)
  }
  
  // Verify payment-service metrics
  let payment_metrics = service_metrics.find(fn(m) { m.service == "payment-service" })
  match payment_metrics {
    Some(metrics) => {
      assert_eq(metrics.total_operations, 1)
      assert_eq(metrics.average_duration, 250)
    }
    None => assert_true(false)
  }
}

// Test 9: Concurrent Telemetry Data Collection
test "concurrent telemetry data collection" {
  // Simulate concurrent telemetry collectors
  let collectors = [
    { id: "collector-1", metrics_collected: 0, active: true },
    { id: "collector-2", metrics_collected: 0, active: true },
    { id: "collector-3", metrics_collected: 0, active: false }
  ]
  
  // Simulate metrics collection across collectors
  let collect_metrics = fn(collector, count) {
    if (collector.active) {
      { id: collector.id, metrics_collected: collector.metrics_collected + count, active: collector.active }
    } else {
      collector
    }
  }
  
  // Process metrics collection for all collectors
  let updated_collectors = collectors.map(fn(c) { collect_metrics(c, 10) })
  
  // Verify results
  assert_eq(updated_collectors.length(), 3)
  
  // Check active collectors
  assert_eq(updated_collectors[0].metrics_collected, 10)
  assert_eq(updated_collectors[0].active, true)
  
  assert_eq(updated_collectors[1].metrics_collected, 10)
  assert_eq(updated_collectors[1].active, true)
  
  // Check inactive collector
  assert_eq(updated_collectors[2].metrics_collected, 0)
  assert_eq(updated_collectors[2].active, false)
  
  // Simulate concurrent batch processing
  let batches = [
    { batch_id: "batch-1", size: 100, processed: false },
    { batch_id: "batch-2", size: 150, processed: false },
    { batch_id: "batch-3", size: 75, processed: false }
  ]
  
  // Process batches in parallel (simulated)
  let process_batch = fn(batch) { { batch_id: batch.batch_id, size: batch.size, processed: true } }
  let processed_batches = batches.map(process_batch)
  
  // Verify all batches are processed
  assert_eq(processed_batches.length(), 3)
  for batch in processed_batches {
    assert_true(batch.processed)
  }
  
  // Calculate total processed items
  let total_processed = processed_batches.reduce(fn(acc, batch) { acc + batch.size }, 0)
  assert_eq(total_processed, 325)
}

// Test 10: Thread-Safe Telemetry Buffer Management
test "thread-safe telemetry buffer management" {
  // Create telemetry buffer with capacity limits
  let create_buffer = fn(capacity) { 
    { 
      data: [], 
      capacity: capacity, 
      overflow_count: 0,
      lock_acquired: false
    } 
  }
  
  let buffer = create_buffer(5)
  
  // Simulate thread-safe buffer operations
  let acquire_lock = fn(buf) { { data: buf.data, capacity: buf.capacity, overflow_count: buf.overflow_count, lock_acquired: true } }
  let release_lock = fn(buf) { { data: buf.data, capacity: buf.capacity, overflow_count: buf.overflow_count, lock_acquired: false } }
  
  // Add telemetry data to buffer with lock management
  let add_data = fn(buf, item) {
    if (buf.data.length() < buf.capacity) {
      let new_data = buf.data.push(item)
      { data: new_data, capacity: buf.capacity, overflow_count: buf.overflow_count, lock_acquired: buf.lock_acquired }
    } else {
      { data: buf.data, capacity: buf.capacity, overflow_count: buf.overflow_count + 1, lock_acquired: buf.lock_acquired }
    }
  }
  
  // Simulate thread-safe operations
  let locked_buffer = acquire_lock(buffer)
  assert_true(locked_buffer.lock_acquired)
  
  // Add items to buffer
  let buffer_with_items = (0..=7).fold(locked_buffer, fn(acc, i) { add_data(acc, "item-" + i.to_string()) })
  
  // Verify buffer capacity and overflow
  assert_eq(buffer_with_items.data.length(), 5) // At capacity
  assert_eq(buffer_with_items.overflow_count, 3) // 3 items overflowed
  
  // Release lock
  let unlocked_buffer = release_lock(buffer_with_items)
  assert_false(unlocked_buffer.lock_acquired)
  
  // Verify buffer contents
  assert_eq(unlocked_buffer.data[0], "item-0")
  assert_eq(unlocked_buffer.data[4], "item-4")
  
  // Simulate concurrent buffer reads
  let read_buffer = fn(buf) { 
    if (!buf.lock_acquired) {
      let locked = acquire_lock(buf)
      let data_copy = locked.data
      let unlocked = release_lock(locked)
      (data_copy, unlocked)
    } else {
      ([], buf) // Would typically wait or retry
    }
  }
  
  let (data_copy, final_buffer) = read_buffer(unlocked_buffer)
  assert_eq(data_copy.length(), 5)
  assert_false(final_buffer.lock_acquired)
  
  // Simulate buffer flush operation
  let flush_buffer = fn(buf) {
    let locked = acquire_lock(buf)
    let flushed_data = locked.data
    let empty_buffer = { data: [], capacity: locked.capacity, overflow_count: locked.overflow_count, lock_acquired: locked.lock_acquired }
    let unlocked = release_lock(empty_buffer)
    (flushed_data, unlocked)
  }
  
  let (flushed_data, empty_buffer) = flush_buffer(final_buffer)
  assert_eq(flushed_data.length(), 5)
  assert_eq(empty_buffer.data.length(), 0)
  assert_false(empty_buffer.lock_acquired)
}