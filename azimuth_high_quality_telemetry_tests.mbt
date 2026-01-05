// Azimuth High-Quality Telemetry Test Suite
// This file contains comprehensive test cases focusing on advanced telemetry scenarios

// Test 1: Telemetry Data Aggregation
test "telemetry data aggregation across multiple spans" {
  // Create a telemetry aggregation context
  let aggregation_context = {
    trace_id: "trace-aggregation-123",
    spans: [],
    metrics: [],
    start_time: 1640995200,
    end_time: 0
  }
  
  // Simulate multiple spans with different operations
  let span1 = {
    span_id: "span-001",
    operation: "database_query",
    start_time: 1640995200,
    end_time: 1640995210,
    status: "success",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users"),
      ("db.rows", IntValue(42))
    ]
  }
  
  let span2 = {
    span_id: "span-002", 
    operation: "cache_lookup",
    start_time: 1640995211,
    end_time: 1640995213,
    status: "hit",
    attributes: [
      ("cache.key", "user:123"),
      ("cache.size", IntValue(1024))
    ]
  }
  
  let span3 = {
    span_id: "span-003",
    operation: "api_call",
    start_time: 1640995214,
    end_time: 1640995220,
    status: "success",
    attributes: [
      ("http.method", "GET"),
      ("http.url", "https://api.example.com/data"),
      ("http.status_code", IntValue(200))
    ]
  }
  
  // Aggregate spans
  let updated_context = { aggregation_context | 
    spans: aggregation_context.spans.push(span1).push(span2).push(span3),
    end_time: 1640995220
  }
  
  // Calculate total duration
  let total_duration = updated_context.end_time - updated_context.start_time
  assert_eq(total_duration, 20)
  
  // Calculate individual span durations
  let span1_duration = span1.end_time - span1.start_time
  let span2_duration = span2.end_time - span2.start_time
  let span3_duration = span3.end_time - span3.start_time
  
  assert_eq(span1_duration, 10)
  assert_eq(span2_duration, 2)
  assert_eq(span3_duration, 6)
  
  // Calculate operation success rate
  let successful_spans = updated_context.spans.filter(fn(s) { s.status == "success" or s.status == "hit" })
  let success_rate = (successful_spans.length().to_float() / updated_context.spans.length().to_float()) * 100.0
  assert_eq(success_rate, 100.0)
  
  // Extract database metrics
  let db_spans = updated_context.spans.filter(fn(s) { s.operation == "database_query" })
  assert_eq(db_spans.length(), 1)
  
  let db_span = db_spans[0]
  let db_rows_attr = db_span.attributes.find(fn(attr) { attr.0 == "db.rows" })
  match db_rows_attr {
    Some((_, IntValue(rows))) => assert_eq(rows, 42)
    _ => assert_true(false)
  }
}

// Test 2: Cross-Service Context Propagation
test "cross-service context propagation and correlation" {
  // Create initial context in service A
  let service_a_context = {
    trace_id: "trace-cross-service-456",
    span_id: "span-service-a-001",
    baggage: [
      ("user.id", "user-123"),
      ("request.id", "req-789"),
      ("tenant.id", "tenant-001")
    ],
    flags: ["sampled", "debug"]
  }
  
  // Simulate context propagation to service B
  let propagate_to_service_b = fn(context) {
    let new_span_id = "span-service-b-" + (Random::next_u64(Random::system()) % 1000).to_string()
    {
      trace_id: context.trace_id,
      span_id: new_span_id,
      baggage: context.baggage,
      flags: context.flags
    }
  }
  
  let service_b_context = propagate_to_service_b(service_a_context)
  assert_eq(service_b_context.trace_id, service_a_context.trace_id)
  assert_not_eq(service_b_context.span_id, service_a_context.span_id)
  assert_eq(service_b_context.baggage, service_a_context.baggage)
  assert_eq(service_b_context.flags, service_a_context.flags)
  
  // Simulate context propagation to service C
  let propagate_to_service_c = fn(context, additional_baggage) {
    let new_span_id = "span-service-c-" + (Random::next_u64(Random::system()) % 1000).to_string()
    let merged_baggage = context.baggage + additional_baggage
    
    {
      trace_id: context.trace_id,
      span_id: new_span_id,
      baggage: merged_baggage,
      flags: context.flags
    }
  }
  
  let service_c_context = propagate_to_service_c(
    service_b_context,
    [("operation.type", "async_processing"), ("priority", "high")]
  )
  
  assert_eq(service_c_context.trace_id, service_a_context.trace_id)
  assert_eq(service_c_context.baggage.length(), 5)
  assert_true(service_c_context.baggage.contains(("user.id", "user-123")))
  assert_true(service_c_context.baggage.contains(("operation.type", "async_processing")))
  
  // Test context correlation
  let correlate_trace = fn(contexts) {
    let trace_id = contexts[0].trace_id
    let all_match = contexts.all(fn(ctx) { ctx.trace_id == trace_id })
    let unique_spans = contexts.map(fn(ctx) { ctx.span_id }).unique()
    let merged_baggage = contexts.reduce(fn(acc, ctx) { acc + ctx.baggage }, [])
    let unique_baggage = merged_baggage.unique()
    
    {
      trace_id,
      consistent: all_match,
      span_count: unique_spans.length(),
      baggage_count: unique_baggage.length()
    }
  }
  
  let correlation_result = correlate_trace([
    service_a_context, 
    service_b_context, 
    service_c_context
  ])
  
  assert_eq(correlation_result.trace_id, "trace-cross-service-456")
  assert_true(correlation_result.consistent)
  assert_eq(correlation_result.span_count, 3)
  assert_eq(correlation_result.baggage_count, 5)
}

// Test 3: Performance Metrics Collection and Analysis
test "performance metrics collection and analysis" {
  // Create metrics collector
  let metrics_collector = {
    counters: [],
    gauges: [],
    histograms: [],
    timers: []
  }
  
  // Record various performance metrics
  let record_counter = fn(collector, name, value, tags) {
    let updated_counters = collector.counters.push({
      name,
      value,
      tags,
      timestamp: 1640995200
    })
    { collector | counters: updated_counters }
  }
  
  let record_gauge = fn(collector, name, value, tags) {
    let updated_gauges = collector.gauges.push({
      name,
      value,
      tags,
      timestamp: 1640995200
    })
    { collector | gauges: updated_gauges }
  }
  
  let record_histogram = fn(collector, name, value, buckets, tags) {
    let updated_histograms = collector.histograms.push({
      name,
      value,
      buckets,
      tags,
      timestamp: 1640995200
    })
    { collector | histograms: updated_histograms }
  }
  
  let record_timer = fn(collector, name, duration_ms, tags) {
    let updated_timers = collector.timers.push({
      name,
      duration_ms,
      tags,
      timestamp: 1640995200
    })
    { collector | timers: updated_timers }
  }
  
  // Record metrics
  let collector_with_metrics = metrics_collector
    |> record_counter("requests.total", 1000, [("service", "api"), ("method", "GET")])
    |> record_counter("errors.total", 50, [("service", "api"), ("method", "GET")])
    |> record_gauge("memory.usage", 512.0, [("service", "api")])
    |> record_gauge("cpu.usage", 75.5, [("service", "api")])
    |> record_histogram("response.time", 150.0, [10.0, 50.0, 100.0, 200.0, 500.0], [("service", "api")])
    |> record_timer("database.query", 25, [("db.type", "postgresql")])
    |> record_timer("database.query", 15, [("db.type", "postgresql")])
    |> record_timer("database.query", 35, [("db.type", "postgresql")])
    |> record_timer("cache.lookup", 2, [("cache.type", "redis")])
    |> record_timer("cache.lookup", 1, [("cache.type", "redis")])
  
  // Analyze metrics
  let calculate_error_rate = fn(collector) {
    let total_requests = collector.counters
      .filter(fn(c) { c.name == "requests.total" })
      .reduce(fn(acc, c) { acc + c.value }, 0)
    
    let total_errors = collector.counters
      .filter(fn(c) { c.name == "errors.total" })
      .reduce(fn(acc, c) { acc + c.value }, 0)
    
    if total_requests > 0 {
      (total_errors.to_float() / total_requests.to_float()) * 100.0
    } else {
      0.0
    }
  }
  
  let calculate_average_timer = fn(collector, timer_name) {
    let timers = collector.timers.filter(fn(t) { t.name == timer_name })
    if timers.length() > 0 {
      let total = timers.reduce(fn(acc, t) { acc + t.duration_ms }, 0)
      total.to_float() / timers.length().to_float()
    } else {
      0.0
    }
  }
  
  let error_rate = calculate_error_rate(collector_with_metrics)
  assert_eq(error_rate, 5.0)  // 50 errors / 1000 requests * 100
  
  let avg_db_query_time = calculate_average_timer(collector_with_metrics, "database.query")
  assert_eq(avg_db_query_time, 25.0)  // (25 + 15 + 35) / 3
  
  let avg_cache_lookup_time = calculate_average_timer(collector_with_metrics, "cache.lookup")
  assert_eq(avg_cache_lookup_time, 1.5)  // (2 + 1) / 2
  
  // Test gauge values
  let memory_gauge = collector_with_metrics.gauges.find(fn(g) { g.name == "memory.usage" })
  match memory_gauge {
    Some(gauge) => assert_eq(gauge.value, 512.0)
    None => assert_true(false)
  }
  
  let cpu_gauge = collector_with_metrics.gauges.find(fn(g) { g.name == "cpu.usage" })
  match cpu_gauge {
    Some(gauge) => assert_eq(gauge.value, 75.5)
    None => assert_true(false)
  }
  
  // Test histogram bucket placement
  let response_histogram = collector_with_metrics.histograms.find(fn(h) { h.name == "response.time" })
  match response_histogram {
    Some(histogram) => {
      assert_eq(histogram.value, 150.0)
      assert_eq(histogram.buckets.length(), 5)
      assert_true(histogram.value > histogram.buckets[2])  // 150 > 100
      assert_true(histogram.value <= histogram.buckets[3])  // 150 <= 200
    }
    None => assert_true(false)
  }
}

// Test 4: Error Handling and Recovery
test "error handling and recovery mechanisms" {
  // Define error types
  enum TelemetryError {
    NetworkTimeout(String)
    SerializationError(String)
    InvalidSpanId(String)
    BufferOverflow(Int)
    ConfigurationError(String)
  }
  
  // Define error recovery strategies
  enum RecoveryStrategy {
    Retry(Int)  // Number of retries
    Fallback(String)  // Fallback value
    CircuitBreaker  // Stop trying for a while
    Ignore  // Continue without the failed component
  }
  
  // Create error handler with recovery strategies
  let error_handler = {
    error_count: 0,
    max_retries: 3,
    circuit_breaker_threshold: 5,
    circuit_breaker_open: false,
    last_error: None
  }
  
  // Simulate error handling with retry
  let handle_with_retry = fn(handler, operation, error_type, max_retries) {
    let mut attempts = 0
    let mut last_error = None
    
    while attempts < max_retries {
      attempts = attempts + 1
      
      // Simulate operation success on retry
      if attempts >= 2 {
        return { success: true, result: "Operation succeeded", attempts }
      }
      
      last_error = Some(error_type)
    }
    
    { success: false, result: "Operation failed", attempts, error: last_error }
  }
  
  let retry_result = handle_with_retry(
    error_handler,
    "send_telemetry_data",
    TelemetryError::NetworkTimeout("Connection timeout"),
    3
  )
  
  assert_true(retry_result.success)
  assert_eq(retry_result.result, "Operation succeeded")
  assert_eq(retry_result.attempts, 2)
  
  // Test circuit breaker pattern
  let update_circuit_breaker = fn(handler, error_occurred) {
    let new_error_count = if error_occurred { handler.error_count + 1 } else { 0 }
    let circuit_open = new_error_count >= handler.circuit_breaker_threshold
    
    {
      error_count: new_error_count,
      max_retries: handler.max_retries,
      circuit_breaker_threshold: handler.circuit_breaker_threshold,
      circuit_breaker_open: circuit_open,
      last_error: if error_occurred { Some(TelemetryError::NetworkTimeout("Test error")) } else { None }
    }
  }
  
  // Simulate multiple errors to trigger circuit breaker
  let handler_after_errors = (0..=5).reduce(fn(acc, _) {
    update_circuit_breaker(acc, true)
  }, error_handler)
  
  assert_true(handler_after_errors.circuit_breaker_open)
  assert_eq(handler_after_errors.error_count, 6)
  
  // Test circuit breaker reset after success
  let handler_after_success = update_circuit_breaker(handler_after_errors, false)
  assert_false(handler_after_success.circuit_breaker_open)
  assert_eq(handler_after_success.error_count, 0)
  
  // Test error classification and recovery strategy selection
  let select_recovery_strategy = fn(error) {
    match error {
      TelemetryError::NetworkTimeout(_) => Retry(3)
      TelemetryError::SerializationError(_) => Fallback("default_serialized_data")
      TelemetryError::InvalidSpanId(_) => Fallback("default_span_id")
      TelemetryError::BufferOverflow(_) => CircuitBreaker
      TelemetryError::ConfigurationError(_) => Ignore
    }
  }
  
  let network_strategy = select_recovery_strategy(TelemetryError::NetworkTimeout("Timeout"))
  match network_strategy {
    Retry(count) => assert_eq(count, 3)
    _ => assert_true(false)
  }
  
  let serialization_strategy = select_recovery_strategy(TelemetryError::SerializationError("Invalid JSON"))
  match serialization_strategy {
    Fallback(value) => assert_eq(value, "default_serialized_data")
    _ => assert_true(false)
  }
  
  let buffer_strategy = select_recovery_strategy(TelemetryError::BufferOverflow(1024))
  match buffer_strategy {
    CircuitBreaker => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 5: Resource Management and Cleanup
test "resource management and cleanup operations" {
  // Define resource types
  enum ResourceType {
    MemoryBuffer(Int)  // Size in bytes
    FileHandle(String)  // File path
    NetworkConnection(String)  // Connection endpoint
    DatabaseConnection(String)  // Connection string
  }
  
  // Create resource manager
  let resource_manager = {
    active_resources: [],
    max_memory: 1048576,  // 1MB
    max_file_handles: 100,
    max_connections: 50,
    cleanup_threshold: 0.8  // 80% utilization
  }
  
  // Simulate resource allocation
  let allocate_resource = fn(manager, resource) {
    let updated_resources = manager.active_resources.push(resource)
    { manager | active_resources: updated_resources }
  }
  
  let allocate_memory = fn(manager, size) {
    allocate_resource(manager, ResourceType::MemoryBuffer(size))
  }
  
  let allocate_file_handle = fn(manager, path) {
    allocate_resource(manager, ResourceType::FileHandle(path))
  }
  
  let allocate_connection = fn(manager, endpoint) {
    allocate_resource(manager, ResourceType::NetworkConnection(endpoint))
  }
  
  // Allocate resources
  let manager_with_resources = resource_manager
    |> allocate_memory(1024)      // 1KB
    |> allocate_memory(2048)      // 2KB
    |> allocate_file_handle("/tmp/telemetry.log")
    |> allocate_file_handle("/tmp/metrics.log")
    |> allocate_connection("api.example.com:443")
    |> allocate_connection("db.example.com:5432")
  
  assert_eq(manager_with_resources.active_resources.length(), 6)
  
  // Calculate resource utilization
  let calculate_memory_usage = fn(manager) {
    manager.active_resources
      .filter(fn(r) {
        match r {
          ResourceType::MemoryBuffer(_) => true
          _ => false
        }
      })
      .reduce(fn(acc, r) {
        match r {
          ResourceType::MemoryBuffer(size) => acc + size
          _ => acc
        }
      }, 0)
  }
  
  let calculate_file_handle_usage = fn(manager) {
    manager.active_resources
      .filter(fn(r) {
        match r {
          ResourceType::FileHandle(_) => true
          _ => false
        }
      })
      .length()
  }
  
  let calculate_connection_usage = fn(manager) {
    manager.active_resources
      .filter(fn(r) {
        match r {
          ResourceType::NetworkConnection(_) => true
          _ => false
        }
      })
      .length()
  }
  
  let memory_usage = calculate_memory_usage(manager_with_resources)
  let file_handle_usage = calculate_file_handle_usage(manager_with_resources)
  let connection_usage = calculate_connection_usage(manager_with_resources)
  
  assert_eq(memory_usage, 3072)  // 1024 + 2048
  assert_eq(file_handle_usage, 2)
  assert_eq(connection_usage, 2)
  
  // Test cleanup strategies
  let cleanup_least_recently_used = fn(manager, count) {
    let sorted_resources = manager.active_resources.sort(fn(a, b) {
      // Simple comparison for test purposes
      match a {
        ResourceType::MemoryBuffer(size_a) => {
          match b {
            ResourceType::MemoryBuffer(size_b) => size_a > size_b
            _ => false
          }
        }
        _ => false
      }
    })
    
    let resources_to_keep = sorted_resources.drop(count)
    { manager | active_resources: resources_to_keep }
  }
  
  let cleanup_by_type = fn(manager, resource_type) {
    let remaining = manager.active_resources.filter(fn(r) {
      match r {
        ResourceType::MemoryBuffer(_) => resource_type != "MemoryBuffer"
        ResourceType::FileHandle(_) => resource_type != "FileHandle"
        ResourceType::NetworkConnection(_) => resource_type != "NetworkConnection"
        ResourceType::DatabaseConnection(_) => resource_type != "DatabaseConnection"
      }
    })
    { manager | active_resources: remaining }
  }
  
  // Cleanup least recently used (in this case, largest memory buffers)
  let manager_after_lru_cleanup = cleanup_least_recently_used(manager_with_resources, 1)
  assert_eq(manager_after_lru_cleanup.active_resources.length(), 5)
  
  let memory_after_lru = calculate_memory_usage(manager_after_lru_cleanup)
  assert_eq(memory_after_lru, 1024)  // Only the 1KB buffer remains
  
  // Cleanup by type
  let manager_after_type_cleanup = cleanup_by_type(manager_after_lru_cleanup, "FileHandle")
  assert_eq(manager_after_type_cleanup.active_resources.length(), 3)
  
  let file_handles_after_cleanup = calculate_file_handle_usage(manager_after_type_cleanup)
  assert_eq(file_handles_after_cleanup, 0)
  
  // Test resource leak detection
  let detect_resource_leaks = fn(manager, threshold_time) {
    let current_time = 1640995200
    let leaked_resources = manager.active_resources.filter(fn(r) {
      // In a real implementation, we'd check creation time
      // For test purposes, we'll simulate by checking resource type
      match r {
        ResourceType::MemoryBuffer(size) => size > 1500  // Simulate old large buffers
        _ => false
      }
    })
    leaked_resources
  }
  
  let leaked_resources = detect_resource_leaks(manager_after_type_cleanup, 3600)
  assert_eq(leaked_resources.length(), 1)
  
  match leaked_resources[0] {
    ResourceType::MemoryBuffer(size) => assert_eq(size, 1024)
    _ => assert_true(false)
  }
}

// Test 6: Concurrent Safety Operations
test "concurrent safety and thread synchronization" {
  // Simulate concurrent operations using atomic-like operations
  let atomic_counter = {
    value: 0,
    lock: false
  }
  
  let atomic_increment = fn(counter) {
    // Simulate atomic increment with lock
    if not(counter.lock) {
      { counter | value: counter.value + 1, lock: false }
    } else {
      counter  // Would wait in real implementation
    }
  }
  
  let atomic_add = fn(counter, addend) {
    // Simulate atomic add with lock
    if not(counter.lock) {
      { counter | value: counter.value + addend, lock: false }
    } else {
      counter  // Would wait in real implementation
    }
  }
  
  // Test concurrent increments
  let counter_after_increments = (0..=100).reduce(fn(acc, _) {
    atomic_increment(acc)
  }, atomic_counter)
  
  assert_eq(counter_after_increments.value, 101)
  
  // Test concurrent adds
  let counter_after_adds = (0..=10).reduce(fn(acc, i) {
    atomic_add(acc, i * 2)
  }, counter_after_increments)
  
  assert_eq(counter_after_adds.value, 101 + (0..=10).reduce(fn(sum, i) { sum + i * 2 }, 0))
  
  // Test concurrent data structure operations
  let concurrent_buffer = {
    data: [],
    read_index: 0,
    write_index: 0,
    capacity: 10,
    count: 0
  }
  
  let buffer_write = fn(buffer, item) {
    if buffer.count < buffer.capacity {
      let updated_data = buffer.data.push(item)
      {
        data: updated_data,
        read_index: buffer.read_index,
        write_index: buffer.write_index + 1,
        capacity: buffer.capacity,
        count: buffer.count + 1
      }
    } else {
      buffer  // Buffer full
    }
  }
  
  let buffer_read = fn(buffer) {
    if buffer.count > 0 {
      let item = buffer.data[buffer.read_index]
      let remaining = buffer.data.drop(1)
      {
        data: remaining,
        read_index: buffer.read_index + 1,
        write_index: buffer.write_index,
        capacity: buffer.capacity,
        count: buffer.count - 1
      }
    } else {
      buffer  // Buffer empty
    }
  }
  
  // Fill buffer
  let filled_buffer = (0..=9).reduce(fn(acc, i) {
    buffer_write(acc, "item-" + i.to_string())
  }, concurrent_buffer)
  
  assert_eq(filled_buffer.count, 10)
  assert_eq(filled_buffer.capacity, 10)
  
  // Try to write to full buffer
  let still_full_buffer = buffer_write(filled_buffer, "overflow-item")
  assert_eq(still_full_buffer.count, 10)  // Should remain unchanged
  
  // Read from buffer
  let buffer_after_read = buffer_read(filled_buffer)
  assert_eq(buffer_after_read.count, 9)
  
  // Write to buffer after read
  let buffer_after_write = buffer_write(buffer_after_read, "new-item")
  assert_eq(buffer_after_write.count, 10)
  
  // Test concurrent map operations
  let concurrent_map = {
    entries: [],
    lock_count: 0
  }
  
  let map_put = fn(map, key, value) {
    let updated_entries = map.entries.filter(fn(e) { e.0 != key }).push((key, value))
    { map | entries: updated_entries, lock_count: map.lock_count + 1 }
  }
  
  let map_get = fn(map, key) {
    map.entries.find(fn(e) { e.0 == key })
  }
  
  let map_remove = fn(map, key) {
    let updated_entries = map.entries.filter(fn(e) { e.0 != key })
    { map | entries: updated_entries, lock_count: map.lock_count + 1 }
  }
  
  // Concurrent map operations
  let map_with_data = concurrent_map
    |> map_put("key1", "value1")
    |> map_put("key2", "value2")
    |> map_put("key3", "value3")
  
  assert_eq(map_with_data.entries.length(), 3)
  
  let value1 = map_get(map_with_data, "key1")
  match value1 {
    Some((_, v)) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  let map_after_removal = map_remove(map_with_data, "key2")
  assert_eq(map_after_removal.entries.length(), 2)
  
  let removed_value = map_get(map_after_removal, "key2")
  assert_eq(removed_value, None)
  
  // Test lock count tracking
  assert_eq(map_after_removal.lock_count, 4)  // 3 puts + 1 remove
}

// Test 7: Configuration Management
test "configuration management and validation" {
  // Define configuration schema
  let config_schema = {
    telemetry: {
      enabled: { type: "boolean", required: true, default: true },
      sampling_rate: { type: "float", required: false, default: 1.0, min: 0.0, max: 1.0 },
      batch_size: { type: "integer", required: false, default: 100, min: 1, max: 1000 },
      export_interval: { type: "integer", required: false, default: 60, min: 1, max: 3600 }
    },
    service: {
      name: { type: "string", required: true },
      version: { type: "string", required: false, default: "1.0.0" },
      environment: { type: "string", required: false, default: "production" }
    },
    endpoints: {
      collector: { type: "string", required: false },
      api_key: { type: "string", required: false, sensitive: true }
    }
  }
  
  // Create configuration validator
  let validate_config = fn(schema, config) {
    let mut errors = []
    let mut warnings = []
    
    // Validate telemetry section
    match config.get("telemetry.enabled") {
      Some(value) => {
        if value.type != "boolean" {
          errors = errors.push("telemetry.enabled must be a boolean")
        }
      }
      None => {
        if schema.telemetry.enabled.required {
          errors = errors.push("telemetry.enabled is required")
        }
      }
    }
    
    match config.get("telemetry.sampling_rate") {
      Some(value) => {
        if value.type == "float" {
          if value.value < 0.0 or value.value > 1.0 {
            errors = errors.push("telemetry.sampling_rate must be between 0.0 and 1.0")
          }
        } else {
          errors = errors.push("telemetry.sampling_rate must be a float")
        }
      }
      None => {}  // Use default
    }
    
    match config.get("service.name") {
      Some(value) => {
        if value.type != "string" or value.value == "" {
          errors = errors.push("service.name must be a non-empty string")
        }
      }
      None => {
        if schema.service.name.required {
          errors = errors.push("service.name is required")
        }
      }
    }
    
    { valid: errors.length() == 0, errors, warnings }
  }
  
  // Test valid configuration
  let valid_config = {
    "telemetry.enabled": { type: "boolean", value: true },
    "telemetry.sampling_rate": { type: "float", value: 0.5 },
    "service.name": { type: "string", value: "test-service" },
    "service.version": { type: "string", value: "2.0.0" }
  }
  
  let valid_result = validate_config(config_schema, valid_config)
  assert_true(valid_result.valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid configuration
  let invalid_config = {
    "telemetry.enabled": { type: "string", value: "true" },  // Wrong type
    "telemetry.sampling_rate": { type: "float", value: 1.5 },  // Out of range
    // Missing required service.name
  }
  
  let invalid_result = validate_config(config_schema, invalid_config)
  assert_false(invalid_result.valid)
  assert_true(invalid_result.errors.length() >= 2)
  assert_true(invalid_result.errors.contains("telemetry.enabled must be a boolean"))
  assert_true(invalid_result.errors.contains("telemetry.sampling_rate must be between 0.0 and 1.0"))
  assert_true(invalid_result.errors.contains("service.name is required"))
  
  // Test configuration merging and defaults
  let apply_defaults = fn(schema, config) {
    let mut result = config
    
    // Apply telemetry defaults
    if not(result.contains("telemetry.enabled")) {
      result = result.set("telemetry.enabled", { type: "boolean", value: schema.telemetry.enabled.default })
    }
    
    if not(result.contains("telemetry.sampling_rate")) {
      result = result.set("telemetry.sampling_rate", { type: "float", value: schema.telemetry.sampling_rate.default })
    }
    
    if not(result.contains("telemetry.batch_size")) {
      result = result.set("telemetry.batch_size", { type: "integer", value: schema.telemetry.batch_size.default })
    }
    
    if not(result.contains("service.version")) {
      result = result.set("service.version", { type: "string", value: schema.service.version.default })
    }
    
    if not(result.contains("service.environment")) {
      result = result.set("service.environment", { type: "string", value: schema.service.environment.default })
    }
    
    result
  }
  
  let partial_config = {
    "telemetry.enabled": { type: "boolean", value: true },
    "service.name": { type: "string", value: "partial-service" }
  }
  
  let config_with_defaults = apply_defaults(config_schema, partial_config)
  assert_eq(config_with_defaults.get("telemetry.sampling_rate"), Some({ type: "float", value: 1.0 }))
  assert_eq(config_with_defaults.get("telemetry.batch_size"), Some({ type: "integer", value: 100 }))
  assert_eq(config_with_defaults.get("service.version"), Some({ type: "string", value: "1.0.0" }))
  assert_eq(config_with_defaults.get("service.environment"), Some({ type: "string", value: "production" }))
  
  // Test configuration hot reload
  let config_change_detector = fn(old_config, new_config) {
    let mut changes = []
    
    for (key, new_value) in new_config {
      match old_config.get(key) {
        Some(old_value) => {
          if old_value.value != new_value.value {
            changes = changes.push({ key, old_value: old_value.value, new_value: new_value.value })
          }
        }
        None => {
          changes = changes.push({ key, old_value: None, new_value: new_value.value })
        }
      }
    }
    
    for (key, old_value) in old_config {
      if not(new_config.contains(key)) {
        changes = changes.push({ key, old_value: old_value.value, new_value: None })
      }
    }
    
    changes
  }
  
  let original_config = {
    "telemetry.sampling_rate": { type: "float", value: 0.5 },
    "service.version": { type: "string", value: "1.0.0" }
  }
  
  let updated_config = {
    "telemetry.sampling_rate": { type: "float", value: 0.8 },  // Changed
    "service.version": { type: "string", value: "1.0.0" },     // Unchanged
    "service.environment": { type: "string", value: "staging" }  // Added
  }
  
  let changes = config_change_detector(original_config, updated_config)
  assert_eq(changes.length(), 2)
  
  let sampling_change = changes.find(fn(c) { c.key == "telemetry.sampling_rate" })
  match sampling_change {
    Some(change) => {
      assert_eq(change.old_value, 0.5)
      assert_eq(change.new_value, 0.8)
    }
    None => assert_true(false)
  }
  
  let env_change = changes.find(fn(c) { c.key == "service.environment" })
  match env_change {
    Some(change) => {
      assert_eq(change.old_value, None)
      assert_eq(change.new_value, "staging")
    }
    None => assert_true(false)
  }
}

// Test 8: Data Serialization and Deserialization
test "data serialization and deserialization operations" {
  // Define serializable telemetry data structure
  let telemetry_span = {
    trace_id: "trace-serialization-123",
    span_id: "span-456",
    parent_span_id: Some("span-789"),
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users"),
      ("db.rows", IntValue(42)),
      ("cache.hit", BoolValue(true))
    ],
    events: [
      {
        name: "query.start",
        timestamp: 1640995200,
        attributes: [("query.type", "select")]
      },
      {
        name: "query.end",
        timestamp: 1640995250,
        attributes: [("rows.returned", IntValue(42))]
      }
    ]
  }
  
  // Simulate JSON serialization
  let serialize_to_json = fn(span) {
    // Simulate JSON string construction
    let json = "{"
      + "\"trace_id\":\"" + span.trace_id + "\"," 
      + "\"span_id\":\"" + span.span_id + "\"," 
      + "\"parent_span_id\":\"" + span.parent_span_id.or_else("") + "\"," 
      + "\"operation_name\":\"" + span.operation_name + "\"," 
      + "\"start_time\":" + span.start_time.to_string() + "," 
      + "\"end_time\":" + span.end_time.to_string() + "," 
      + "\"status\":\"" + span.status + "\"," 
      + "\"attributes\":{"
      + span.attributes.map(fn(attr) {
        match attr.1 {
          StringValue(s) => "\"" + attr.0 + "\":\"" + s + "\""
          IntValue(i) => "\"" + attr.0 + "\":" + i.to_string()
          BoolValue(b) => "\"" + attr.0 + "\":" + b.to_string()
          _ => "\"" + attr.0 + "\":\"unsupported\""
        }
      }).join(",") + "},"
      + "\"events\":["
      + span.events.map(fn(event) {
        "{"
        + "\"name\":\"" + event.name + "\"," 
        + "\"timestamp\":" + event.timestamp.to_string() + "," 
        + "\"attributes\":{"
        + event.attributes.map(fn(attr) {
          match attr.1 {
            StringValue(s) => "\"" + attr.0 + "\":\"" + s + "\""
            IntValue(i) => "\"" + attr.0 + "\":" + i.to_string()
            BoolValue(b) => "\"" + attr.0 + "\":" + b.to_string()
            _ => "\"" + attr.0 + "\":\"unsupported\""
          }
        }).join(",")
        + "}"
        + "}"
      }).join(",")
      + "]"
      + "}"
    json
  }
  
  let json_string = serialize_to_json(telemetry_span)
  assert_true(json_string.contains("\"trace_id\":\"trace-serialization-123\""))
  assert_true(json_string.contains("\"operation_name\":\"database_query\""))
  assert_true(json_string.contains("\"db.type\":\"postgresql\""))
  assert_true(json_string.contains("\"db.rows\":42"))
  assert_true(json_string.contains("\"cache.hit\":true"))
  
  // Simulate JSON deserialization
  let parse_json_value = fn(json_str, key) {
    let key_pattern = "\"" + key + "\":" 
    let key_index = json_str.index_of(key_pattern)
    
    match key_index {
      Some(index) => {
        let value_start = index + key_pattern.length()
        let mut value_end = value_start
        
        if json_str[value_start] == '"' {
          // String value
          value_start = value_start + 1
          value_end = json_str.index_of("\"", value_start).or_else(json_str.length() - 1)
          Some(StringValue(json_str.substring(value_start, value_end - value_start)))
        } else if json_str[value_start] == 't' or json_str[value_start] == 'f' {
          // Boolean value
          value_end = value_start + (if json_str[value_start] == 't' { 4 } else { 5 })
          let bool_value = json_str.substring(value_start, value_end - value_start) == "true"
          Some(BoolValue(bool_value))
        } else {
          // Numeric value
          while value_end < json_str.length() and 
                (json_str[value_end] >= '0' and json_str[value_end] <= '9') {
            value_end = value_end + 1
          }
          let int_value = json_str.substring(value_start, value_end - value_start).to_int()
          Some(IntValue(int_value))
        }
      }
      None => None
    }
  }
  
  let trace_id_value = parse_json_value(json_string, "trace_id")
  match trace_id_value {
    Some(StringValue(id)) => assert_eq(id, "trace-serialization-123")
    _ => assert_true(false)
  }
  
  let operation_name_value = parse_json_value(json_string, "operation_name")
  match operation_name_value {
    Some(StringValue(name)) => assert_eq(name, "database_query")
    _ => assert_true(false)
  }
  
  let start_time_value = parse_json_value(json_string, "start_time")
  match start_time_value {
    Some(IntValue(time)) => assert_eq(time, 1640995200)
    _ => assert_true(false)
  }
  
  // Test binary serialization simulation
  let serialize_to_binary = fn(span) {
    // Simulate binary format with length-prefixed strings
    let mut binary = []
    
    // Helper to write string with length prefix
    let write_string = fn(data, str) {
      let length_bytes = [(str.length() / 256) % 256, str.length() % 256]
      let str_bytes = str.to_byte_array()
      data + length_bytes + str_bytes
    }
    
    // Helper to write integer (4 bytes, big-endian)
    let write_int = fn(data, value) {
      let bytes = [
        (value / 16777216) % 256,
        (value / 65536) % 256,
        (value / 256) % 256,
        value % 256
      ]
      data + bytes
    }
    
    // Write trace_id
    binary = write_string(binary, span.trace_id)
    
    // Write span_id
    binary = write_string(binary, span.span_id)
    
    // Write parent_span_id (1 byte flag + string if present)
    match span.parent_span_id {
      Some(parent_id) => {
        binary = binary.push(1)  // Flag indicating present
        binary = write_string(binary, parent_id)
      }
      None => {
        binary = binary.push(0)  // Flag indicating absent
      }
    }
    
    // Write operation_name
    binary = write_string(binary, span.operation_name)
    
    // Write timestamps
    binary = write_int(binary, span.start_time)
    binary = write_int(binary, span.end_time)
    
    // Write status
    binary = write_string(binary, span.status)
    
    binary
  }
  
  let binary_data = serialize_to_binary(telemetry_span)
  assert_true(binary_data.length() > 50)  // Should contain significant data
  
  // Test binary deserialization simulation
  let read_string_from_binary = fn(data, offset) {
    if offset + 1 < data.length() {
      let length = data[offset] * 256 + data[offset + 1]
      let str_start = offset + 2
      let str_end = str_start + length
      
      if str_end <= data.length() {
        let str_bytes = data.slice(str_start, str_end - str_start)
        let str_value = String::from_byte_array(str_bytes)
        Some((str_value, str_end))
      } else {
        None
      }
    } else {
      None
    }
  }
  
  let read_int_from_binary = fn(data, offset) {
    if offset + 3 < data.length() {
      let value = data[offset] * 16777216 + 
                  data[offset + 1] * 65536 + 
                  data[offset + 2] * 256 + 
                  data[offset + 3]
      Some((value, offset + 4))
    } else {
      None
    }
  }
  
  // Test reading trace_id from binary
  let trace_id_from_binary = read_string_from_binary(binary_data, 0)
  match trace_id_from_binary {
    Some((id, next_offset)) => {
      assert_eq(id, "trace-serialization-123")
      assert_true(next_offset > 0)
    }
    None => assert_true(false)
  }
  
  // Test compression simulation
  let compress_data = fn(data) {
    // Simple run-length encoding for demonstration
    let mut compressed = []
    let mut i = 0
    
    while i < data.length() {
      let current_byte = data[i]
      let mut count = 1
      
      while i + count < data.length() and data[i + count] == current_byte and count < 255 {
        count = count + 1
      }
      
      compressed = compressed.push(count)
      compressed = compressed.push(current_byte)
      i = i + count
    }
    
    compressed
  }
  
  // Test decompression simulation
  let decompress_data = fn(compressed) {
    let mut decompressed = []
    let mut i = 0
    
    while i + 1 < compressed.length() {
      let count = compressed[i]
      let byte_value = compressed[i + 1]
      
      let mut j = 0
      while j < count {
        decompressed = decompressed.push(byte_value)
        j = j + 1
      }
      
      i = i + 2
    }
    
    decompressed
  }
  
  // Create data with repeated patterns for compression
  let test_data = [1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 4, 5, 5, 5]
  let compressed = compress_data(test_data)
  let decompressed = decompress_data(compressed)
  
  assert_eq(test_data, decompressed)
  assert_true(compressed.length() < test_data.length())  // Should be smaller after compression
}

// Test 9: Sampling Strategy Implementation
test "sampling strategy implementation and evaluation" {
  // Define sampling strategies
  enum SamplingStrategy {
    AlwaysOn
    AlwaysOff
    Probability(Float)  // Sample rate between 0.0 and 1.0
    Ratio(Int, Int)     // Sample 1 out of N
    AttributeBased(String, String)  // Sample based on attribute value
    Adaptive            // Adaptive sampling based on system load
  }
  
  // Create sampling decision engine
  let sampling_engine = {
    current_strategy: SamplingStrategy::Probability(0.1),
    sample_count: 0,
    total_count: 0,
    adaptive_threshold: 1000  // Max samples per minute
  }
  
  // Implement sampling decision functions
  let should_sample = fn(engine, trace_id, attributes) {
    engine.total_count = engine.total_count + 1
    
    let decision = match engine.current_strategy {
      SamplingStrategy::AlwaysOn => true,
      SamplingStrategy::AlwaysOff => false,
      SamplingStrategy::Probability(rate) => {
        // Simple hash-based deterministic sampling
        let hash = trace_id.to_char_array().reduce(fn(acc, c) { acc + c.to_int() }, 0) % 100
        (hash.to_float() / 100.0) < rate
      },
      SamplingStrategy::Ratio(_, denominator) => {
        let hash = trace_id.to_char_array().reduce(fn(acc, c) { acc + c.to_int() }, 0) % denominator
        hash == 0
      },
      SamplingStrategy::AttributeBased(key, expected_value) => {
        match attributes.find(fn(attr) { attr.0 == key }) {
          Some((_, StringValue(value))) => value == expected_value,
          _ => false
        }
      },
      SamplingStrategy::Adaptive => {
        // Simple adaptive logic: sample if under threshold
        engine.sample_count < engine.adaptive_threshold
      }
    }
    
    if decision {
      engine.sample_count = engine.sample_count + 1
    }
    
    decision
  }
  
  // Test AlwaysOn strategy
  let always_on_engine = { sampling_engine | current_strategy: SamplingStrategy::AlwaysOn }
  let always_on_decision = should_sample(always_on_engine, "trace-123", [])
  assert_true(always_on_decision)
  
  // Test AlwaysOff strategy
  let always_off_engine = { sampling_engine | current_strategy: SamplingStrategy::AlwaysOff }
  let always_off_decision = should_sample(always_off_engine, "trace-123", [])
  assert_false(always_off_decision)
  
  // Test Probability strategy
  let probability_engine = { sampling_engine | current_strategy: SamplingStrategy::Probability(0.0) }
  let zero_prob_decision = should_sample(probability_engine, "trace-123", [])
  assert_false(zero_prob_decision)
  
  let full_prob_engine = { sampling_engine | current_strategy: SamplingStrategy::Probability(1.0) }
  let full_prob_decision = should_sample(full_prob_engine, "trace-123", [])
  assert_true(full_prob_decision)
  
  // Test Ratio strategy
  let ratio_engine = { sampling_engine | current_strategy: SamplingStrategy::Ratio(1, 10) }
  let ratio_decisions = (0..=100).map(fn(i) {
    should_sample(ratio_engine, "trace-" + i.to_string(), [])
  })
  
  // Should have approximately 1/10 sampling rate
  let sampled_count = ratio_decisions.filter(fn(d) { d }).length()
  assert_true(sampled_count >= 5 and sampled_count <= 15)  // Allow some variance
  
  // Test AttributeBased strategy
  let attr_engine = { sampling_engine | current_strategy: SamplingStrategy::AttributeBased("service.name", "critical-service") }
  
  let critical_attrs = [("service.name", StringValue("critical-service"))]
  let critical_decision = should_sample(attr_engine, "trace-123", critical_attrs)
  assert_true(critical_decision)
  
  let normal_attrs = [("service.name", StringValue("normal-service"))]
  let normal_decision = should_sample(attr_engine, "trace-456", normal_attrs)
  assert_false(normal_decision)
  
  // Test Adaptive strategy
  let adaptive_engine = { sampling_engine | current_strategy: SamplingStrategy::Adaptive, sample_count: 999 }
  let adaptive_decision_1 = should_sample(adaptive_engine, "trace-789", [])
  assert_true(adaptive_decision_1)  // Should sample (999 < 1000)
  
  let adaptive_engine_full = { adaptive_engine | sample_count: 1000 }
  let adaptive_decision_2 = should_sample(adaptive_engine_full, "trace-999", [])
  assert_false(adaptive_decision_2)  // Should not sample (1000 >= 1000)
  
  // Test sampling rate calculation
  let calculate_sampling_rate = fn(engine) {
    if engine.total_count > 0 {
      (engine.sample_count.to_float() / engine.total_count.to_float()) * 100.0
    } else {
      0.0
    }
  }
  
  let sampling_rate = calculate_sampling_rate(ratio_engine)
  assert_true(sampling_rate >= 5.0 and sampling_rate <= 15.0)  // Should be around 10%
  
  // Test sampling strategy switching
  let switch_strategy = fn(engine, new_strategy) {
    { engine | current_strategy: new_strategy, sample_count: 0, total_count: 0 }
  }
  
  let switched_engine = switch_strategy(ratio_engine, SamplingStrategy::AlwaysOn)
  match switched_engine.current_strategy {
    SamplingStrategy::AlwaysOn => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(switched_engine.sample_count, 0)
  assert_eq(switched_engine.total_count, 0)
  
  // Test sampling budget management
  let manage_sampling_budget = fn(engine, budget, time_window) {
    let current_time = 1640995200
    let window_start = current_time - time_window
    
    // In a real implementation, we'd filter samples by timestamp
    // For simulation, we'll just check against the budget
    if engine.sample_count >= budget {
      { engine | current_strategy: SamplingStrategy::AlwaysOff }
    } else {
      engine
    }
  }
  
  let budget_engine = { adaptive_engine | sample_count: 500 }
  let budget_limited_engine = manage_sampling_budget(budget_engine, 400, 60)
  
  match budget_limited_engine.current_strategy {
    SamplingStrategy::AlwaysOff => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 10: Batch Processing Operations
test "batch processing operations and optimization" {
  // Define batch processor configuration
  let batch_config = {
    max_batch_size: 100,
    max_batch_wait_ms: 5000,
    max_memory_mb: 10,
    compression_enabled: true,
    retry_attempts: 3
  }
  
  // Create batch processor
  let batch_processor = {
    config: batch_config,
    current_batch: [],
    batch_start_time: 0,
    total_items_processed: 0,
    total_batches_processed: 0,
    failed_batches: 0
  }
  
  // Define telemetry item structure
  let telemetry_item = {
    id: String,
    timestamp: Int,
    data: String,
    size_bytes: Int,
    priority: String  // "high", "medium", "low"
  }
  
  // Create test items
  let create_test_item = fn(id, timestamp, data, priority) {
    {
      id,
      timestamp,
      data,
      size_bytes: data.length(),
      priority
    }
  }
  
  let test_items = [
    create_test_item("item-1", 1640995200, "telemetry data 1", "high"),
    create_test_item("item-2", 1640995201, "telemetry data 2", "medium"),
    create_test_item("item-3", 1640995202, "telemetry data 3", "low"),
    create_test_item("item-4", 1640995203, "telemetry data 4", "high"),
    create_test_item("item-5", 1640995204, "telemetry data 5", "medium")
  ]
  
  // Implement batch addition logic
  let add_to_batch = fn(processor, item, current_time) {
    let batch_size_bytes = processor.current_batch.reduce(fn(acc, i) { acc + i.size_bytes }, 0)
    let new_batch_size = batch_size_bytes + item.size_bytes
    let max_memory_bytes = processor.config.max_memory_mb * 1024 * 1024
    
    let should_flush = 
      processor.current_batch.length() >= processor.config.max_batch_size or
      new_batch_size > max_memory_bytes or
      (current_time - processor.batch_start_time) >= processor.config.max_batch_wait_ms
    
    if should_flush and processor.current_batch.length() > 0 {
      // Flush current batch and start new one
      let updated_processor = { processor |
        current_batch: [item],
        batch_start_time: current_time,
        total_items_processed: processor.total_items_processed + processor.current_batch.length(),
        total_batches_processed: processor.total_batches_processed + 1
      }
      (updated_processor, true)  // (processor, batch_flushed)
    } else {
      // Add to current batch
      let updated_processor = { processor |
        current_batch: processor.current_batch.push(item)
      }
      (updated_processor, false)  // (processor, batch_flushed)
    }
  }
  
  // Add items to batch processor
  let mut processor = batch_processor
  let mut flush_count = 0
  
  for i in 0..test_items.length() {
    let item = test_items[i]
    let (updated_processor, flushed) = add_to_batch(processor, item, 1640995250 + i)
    processor = updated_processor
    
    if flushed {
      flush_count = flush_count + 1
    }
  }
  
  // Manually flush remaining items
  if processor.current_batch.length() > 0 {
    processor = { processor |
      total_items_processed: processor.total_items_processed + processor.current_batch.length(),
      total_batches_processed: processor.total_batches_processed + 1,
      current_batch: []
    }
    flush_count = flush_count + 1
  }
  
  assert_eq(processor.total_items_processed, 5)
  assert_eq(processor.total_batches_processed, 1)  // All items in one batch
  assert_eq(processor.current_batch.length(), 0)
  
  // Test batch priority sorting
  let sort_by_priority = fn(items) {
    items.sort(fn(a, b) {
      match a.priority {
        "high" => {
          match b.priority {
            "high" => a.timestamp <= b.timestamp
            _ => true
          }
        }
        "medium" => {
          match b.priority {
            "high" => false
            "medium" => a.timestamp <= b.timestamp
            "low" => true
          }
        }
        "low" => {
          match b.priority {
            "low" => a.timestamp <= b.timestamp
            _ => false
          }
        }
        _ => a.timestamp <= b.timestamp
      }
    })
  }
  
  let sorted_items = sort_by_priority(test_items)
  assert_eq(sorted_items[0].priority, "high")  // First item should be high priority
  assert_eq(sorted_items[0].id, "item-1")
  assert_eq(sorted_items[1].priority, "high")  // Second item should also be high priority
  assert_eq(sorted_items[1].id, "item-4")
  assert_eq(sorted_items[2].priority, "medium")  // Then medium priority
  assert_eq(sorted_items[2].id, "item-2")
  assert_eq(sorted_items[3].priority, "medium")  // Then medium priority
  assert_eq(sorted_items[3].id, "item-5")
  assert_eq(sorted_items[4].priority, "low")  // Finally low priority
  assert_eq(sorted_items[4].id, "item-3")
  
  // Test batch compression
  let compress_batch = fn(batch, enabled) {
    if enabled {
      // Simple compression simulation
      let total_size = batch.reduce(fn(acc, item) { acc + item.size_bytes }, 0)
      let compressed_size = (total_size.to_float() * 0.7).to_int()  // 30% compression
      compressed_size
    } else {
      batch.reduce(fn(acc, item) { acc + item.size_bytes }, 0)
    }
  }
  
  let uncompressed_size = compress_batch(test_items, false)
  let compressed_size = compress_batch(test_items, true)
  
  assert_true(compressed_size < uncompressed_size)
  assert_eq(compressed_size, (uncompressed_size.to_float() * 0.7).to_int())
  
  // Test batch retry mechanism
  let process_batch_with_retry = fn(batch, config) {
    let mut attempts = 0
    let mut success = false
    
    while attempts < config.retry_attempts and not(success) {
      attempts = attempts + 1
      
      // Simulate processing success on second attempt
      if attempts >= 2 {
        success = true
      }
    }
    
    { success, attempts }
  }
  
  let batch_result = process_batch_with_retry(test_items, batch_config)
  assert_true(batch_result.success)
  assert_eq(batch_result.attempts, 2)
  
  // Test batch with all retries exhausted
  let failing_batch_result = process_batch_with_retry(test_items, { batch_config | retry_attempts: 1 })
  assert_false(failing_batch_result.success)
  assert_eq(failing_batch_result.attempts, 1)
  
  // Test batch performance metrics
  let calculate_batch_metrics = fn(processor) {
    if processor.total_batches_processed > 0 {
      let avg_items_per_batch = processor.total_items_processed.to_float() / processor.total_batches_processed.to_float()
      let failure_rate = (processor.failed_batches.to_float() / processor.total_batches_processed.to_float()) * 100.0
      
      {
        avg_items_per_batch,
        failure_rate,
        total_batches: processor.total_batches_processed,
        total_items: processor.total_items_processed
      }
    } else {
      {
        avg_items_per_batch: 0.0,
        failure_rate: 0.0,
        total_batches: 0,
        total_items: 0
      }
    }
  }
  
  let metrics = calculate_batch_metrics(processor)
  assert_eq(metrics.avg_items_per_batch, 5.0)  // 5 items in 1 batch
  assert_eq(metrics.failure_rate, 0.0)  // No failures
  assert_eq(metrics.total_batches, 1)
  assert_eq(metrics.total_items, 5)
  
  // Test batch optimization based on item size
  let optimize_batch_size = fn(items, target_size_mb) {
    let target_size_bytes = target_size_mb * 1024 * 1024
    let mut current_batch = []
    let mut batches = []
    let mut current_size = 0
    
    for item in items {
      if current_size + item.size_bytes > target_size_bytes and current_batch.length() > 0 {
        batches = batches.push(current_batch)
        current_batch = [item]
        current_size = item.size_bytes
      } else {
        current_batch = current_batch.push(item)
        current_size = current_size + item.size_bytes
      }
    }
    
    if current_batch.length() > 0 {
      batches = batches.push(current_batch)
    }
    
    batches
  }
  
  let large_items = [
    create_test_item("large-1", 1640995200, "x".repeat(1024 * 1024), "high"),  // 1MB
    create_test_item("large-2", 1640995201, "x".repeat(2 * 1024 * 1024), "medium"),  // 2MB
    create_test_item("large-3", 1640995202, "x".repeat(512 * 1024), "low"),  // 0.5MB
    create_test_item("large-4", 1640995203, "x".repeat(1024 * 1024), "high"),  // 1MB
  ]
  
  let optimized_batches = optimize_batch_size(large_items, 2)  // 2MB target
  
  assert_eq(optimized_batches.length(), 3)
  assert_eq(optimized_batches[0].length(), 1)  // 2MB item
  assert_eq(optimized_batches[1].length(), 2)  // 1MB + 0.5MB = 1.5MB
  assert_eq(optimized_batches[2].length(), 1)  // 1MB item
}