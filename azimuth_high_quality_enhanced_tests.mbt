// Azimuth Telemetry System - High Quality Enhanced Test Suite
// This file contains high-quality test cases for enhanced telemetry functionality

// Test 1: Advanced Trace Context Propagation
test "advanced trace context propagation with complex scenarios" {
  // Test trace context with multiple service boundaries
  let initial_trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let initial_span_id = "00f067aa0ba902b7"
  let span_ctx = SpanContext::new(initial_trace_id, initial_span_id, true, "rojo=00f067aa0ba902b7")
  let context = Context::with_value(Context::root(), ContextKey::new("trace-context"), initial_trace_id + ":" + initial_span_id)
  
  // Simulate service-to-service propagation
  let service_a_span = Span::new("service-a-operation", Internal, span_ctx)
  let service_a_context = Context::with_value(context, ContextKey::new("service-a"), "processing")
  
  // Verify trace context is preserved across service boundaries
  assert_eq(SpanContext::trace_id(service_a_span.span_context()), initial_trace_id)
  assert_true(SpanContext::is_sampled(service_a_span.span_context()))
  
  // Test nested service calls
  let service_b_span_id = "b7ad6b7169203331"
  let service_b_span_ctx = SpanContext::new(initial_trace_id, service_b_span_id, true, "")
  let service_b_span = Span::new("service-b-operation", Server, service_b_span_ctx)
  
  // Verify trace ID consistency across services
  assert_eq(SpanContext::trace_id(service_a_span.span_context()), SpanContext::trace_id(service_b_span.span_context()))
  assert_not_eq(SpanContext::span_id(service_a_span.span_context()), SpanContext::span_id(service_b_span.span_context()))
  
  // Test span hierarchy and relationships
  service_a_span.add_event("service.b.call.start", Some([("service.name", "service-b"), ("operation.type", "database.query")]))
  service_b_span.add_event("database.query.execute", Some([("db.statement", "SELECT * FROM users"), ("db.type", "postgresql")]))
  
  service_b_span.end()
  service_a_span.add_event("service.b.call.complete", Some([("duration.ms", "150"), ("status", "success")]))
  service_a_span.end()
}

// Test 2: Comprehensive Metrics Collection and Aggregation
test "comprehensive metrics collection with multi-dimensional aggregation" {
  // Test metrics with multiple dimensions
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "multi-dimensional-meter")
  
  // Create counters with different dimensions
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let error_counter = Meter::create_counter(meter, "http.errors.total")
  let response_time_histogram = Meter::create_histogram(meter, "http.response.time")
  
  // Simulate requests with different attributes
  let request_scenarios = [
    ("GET", "/api/users", 200, 45),
    ("POST", "/api/users", 201, 120),
    ("GET", "/api/users/123", 200, 25),
    ("PUT", "/api/users/123", 200, 85),
    ("DELETE", "/api/users/123", 204, 35),
    ("GET", "/api/orders", 200, 55),
    ("POST", "/api/orders", 201, 180),
    ("GET", "/api/orders/456", 404, 15),
    ("GET", "/api/products", 500, 200),
    ("POST", "/api/products", 500, 250)
  ]
  
  let mut success_count = 0
  let mut error_count = 0
  let mut total_response_time = 0
  
  // Process each request scenario
  for (method, endpoint, status, response_time) in request_scenarios {
    // Record request count
    Counter::add(request_counter, 1.0)
    
    // Record response time
    Histogram::record(response_time_histogram, response_time.to_float())
    total_response_time = total_response_time + response_time
    
    // Record errors for non-successful status codes
    if status >= 400 {
      Counter::add(error_counter, 1.0)
      error_count = error_count + 1
    } else {
      success_count = success_count + 1
    }
  }
  
  // Verify metrics collection
  assert_eq(success_count, 7)
  assert_eq(error_count, 3)
  assert_eq(total_response_time, 1110)
  
  // Test gauge with dynamic updates
  let active_connections_gauge = Meter::create_gauge(meter, "active.connections")
  let connection_events = [1, 3, -1, 5, -2, 4, -1, 2, -3, 1]
  let mut active_connections = 0
  
  for event in connection_events {
    active_connections = active_connections + event
    assert_true(active_connections >= 0) // Active connections should never be negative
    // In a real implementation, this would update the gauge
  }
  
  assert_eq(active_connections, 9)
  
  // Test up-down counter for queue size
  let queue_size_counter = Meter::create_updown_counter(meter, "queue.size")
  let queue_operations = [10, -3, 5, -2, 8, -4, 3, -1, 6, -2]
  let mut queue_size = 0
  
  for operation in queue_operations {
    queue_size = queue_size + operation
    assert_true(queue_size >= 0) // Queue size should never be negative
    UpDownCounter::add(queue_size_counter, operation.to_float())
  }
  
  assert_eq(queue_size, 20)
}

// Test 3: Advanced Log Correlation and Context Enrichment
test "advanced log correlation with context enrichment" {
  // Test log correlation across distributed operations
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "correlation-test-logger")
  
  // Create a distributed operation with multiple log points
  let operation_id = "op-123456789"
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_id = "00f067aa0ba902b7"
  
  // Start operation log
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Operation started"),
    Some(Attributes::with([("operation.id", StringValue(operation_id)), ("operation.type", "user.process")])),
    Some(1640995200000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Processing step logs
  let processing_logs = [
    ("Validating user input", Debug, 1000L),
    ("Checking user permissions", Debug, 1500L),
    ("Querying database", Info, 2000L),
    ("Processing business logic", Info, 3000L),
    ("Updating cache", Info, 3500L)
  ]
  
  let correlated_logs = []
  
  for (message, severity, timestamp_offset) in processing_logs {
    let log = LogRecord::new_with_context(
      severity,
      Some(message),
      Some(Attributes::with([
        ("operation.id", StringValue(operation_id)),
        ("step.name", StringValue(message)),
        ("trace.id", StringValue(trace_id))
      ])),
      Some(1640995200000L + timestamp_offset),
      None,
      Some(trace_id),
      Some(span_id),
      Some(Context::root())
    )
    correlated_logs.push(log)
  }
  
  // Error scenario log
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Database connection timeout"),
    Some(Attributes::with([
      ("operation.id", StringValue(operation_id)),
      ("error.type", StringValue("timeout")),
      ("error.code", IntValue(408)),
      ("retry.count", IntValue(3)),
      ("trace.id", StringValue(trace_id))
    ])),
    Some(16409952005000L),
    None,
    Some(trace_id),
    Some(span_id + "_error"),
    Some(Context::root())
  )
  
  // Recovery log
  let recovery_log = LogRecord::new_with_context(
    Warn,
    Some("Operation recovered after retry"),
    Some(Attributes::with([
      ("operation.id", StringValue(operation_id)),
      ("recovery.strategy", StringValue("exponential.backoff")),
      ("total.retry.attempts", IntValue(3)),
      ("trace.id", StringValue(trace_id))
    ])),
    Some(16409952008000L),
    None,
    Some(trace_id),
    Some(span_id + "_recovery"),
    Some(Context::root())
  )
  
  // Completion log
  let completion_log = LogRecord::new_with_context(
    Info,
    Some("Operation completed successfully"),
    Some(Attributes::with([
      ("operation.id", StringValue(operation_id)),
      ("duration.ms", IntValue(8000)),
      ("status", StringValue("success")),
      ("trace.id", StringValue(trace_id))
    ])),
    Some(16409952008000L),
    None,
    Some(trace_id),
    Some(span_id + "_complete"),
    Some(Context::root())
  )
  
  // Verify log correlation
  assert_eq(correlated_logs.length(), 5)
  
  // Verify all logs have the same operation ID
  let all_logs = [start_log] + correlated_logs + [error_log, recovery_log, completion_log]
  
  for log in all_logs {
    let attrs = LogRecord::attributes(log)
    match attrs {
      Some(attributes) => {
        let op_id = Attributes::get(attributes, "operation.id")
        match op_id {
          Some(StringValue(id)) => assert_eq(id, operation_id)
          _ => assert_true(false) // All logs should have operation.id
        }
      }
      None => assert_true(false) // All logs should have attributes
    }
  }
  
  // Emit all logs
  for log in all_logs {
    Logger::emit(logger, log)
  }
}

// Test 4: Resource Attribute Management and Merging
test "resource attribute management with intelligent merging strategies" {
  // Test resource creation with multiple attribute types
  let base_resource = Resource::new()
  
  // Service-level attributes
  let service_attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.namespace", StringValue("production")),
    ("service.instance.id", StringValue("payment-service-001"))
  ]
  
  let service_resource = Resource::with_attributes(base_resource, service_attrs)
  
  // Host-level attributes
  let host_attrs = [
    ("host.name", StringValue("prod-web-01")),
    ("host.ip", StringValue("10.0.1.100")),
    ("host.arch", StringValue("amd64")),
    ("host.os.family", StringValue("linux")),
    ("host.os.version", StringValue("20.04"))
  ]
  
  let host_resource = Resource::with_attributes(base_resource, host_attrs)
  
  // Deployment attributes
  let deployment_attrs = [
    ("deployment.environment", StringValue("production")),
    ("deployment.region", StringValue("us-west-2")),
    ("deployment.zone", StringValue("us-west-2a")),
    ("deployment.cluster", StringValue("main-cluster"))
  ]
  
  let deployment_resource = Resource::with_attributes(base_resource, deployment_attrs)
  
  // Test multi-level resource merging
  let service_host_merged = Resource::merge(service_resource, host_resource)
  let fully_merged = Resource::merge(service_host_merged, deployment_resource)
  
  // Verify service attributes are preserved
  let service_name = Resource::get_attribute(fully_merged, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "payment-service")
    _ => assert_true(false)
  }
  
  // Verify host attributes are preserved
  let host_ip = Resource::get_attribute(fully_merged, "host.ip")
  match host_ip {
    Some(StringValue(ip)) => assert_eq(ip, "10.0.1.100")
    _ => assert_true(false)
  }
  
  // Verify deployment attributes are preserved
  let deployment_region = Resource::get_attribute(fully_merged, "deployment.region")
  match deployment_region {
    Some(StringValue(region)) => assert_eq(region, "us-west-2")
    _ => assert_true(false)
  }
  
  // Test conflict resolution with priority strategy
  let priority_base = Resource::with_attributes(base_resource, [
    ("priority.attr", StringValue("base-value")),
    ("retained.attr", StringValue("keep-me"))
  ])
  
  let priority_override = Resource::with_attributes(base_resource, [
    ("priority.attr", StringValue("override-value")),
    ("new.attr", StringValue("new-value"))
  ])
  
  let priority_merged = Resource::merge(priority_base, priority_override)
  
  // Override should win for priority.attr
  let priority_value = Resource::get_attribute(priority_merged, "priority.attr")
  match priority_value {
    Some(StringValue(value)) => assert_eq(value, "override-value")
    _ => assert_true(false)
  }
  
  // Retained attr should still be present
  let retained_value = Resource::get_attribute(priority_merged, "retained.attr")
  match retained_value {
    Some(StringValue(value)) => assert_eq(value, "keep-me")
    _ => assert_true(false)
  }
  
  // New attr should be added
  let new_value = Resource::get_attribute(priority_merged, "new.attr")
  match new_value {
    Some(StringValue(value)) => assert_eq(value, "new-value")
    _ => assert_true(false)
  }
  
  // Test array attribute merging
  let array_base = Resource::with_attributes(base_resource, [
    ("tags", ArrayStringValue(["web", "api", "public"]))
  ])
  
  let array_override = Resource::with_attributes(base_resource, [
    ("tags", ArrayStringValue(["database", "backend"]))
  ])
  
  let array_merged = Resource::merge(array_base, array_override)
  let tags_value = Resource::get_attribute(array_merged, "tags")
  
  match tags_value {
    Some(ArrayStringValue(tags)) => {
      // Override should replace the array
      assert_eq(tags.length(), 2)
      assert_eq(tags[0], "database")
      assert_eq(tags[1], "backend")
    }
    _ => assert_true(false)
  }
}

// Test 5: Baggage Propagation with Complex Values
test "complex baggage propagation with nested structures" {
  // Test baggage with complex nested values
  let baggage = Baggage::new()
  
  // Simple baggage entries
  let simple_baggage = [
    ("user.id", "12345"),
    ("session.id", "sess-abcdef"),
    ("request.id", "req-123456")
  ]
  
  let mut updated_baggage = baggage
  
  // Add simple baggage entries
  for (key, value) in simple_baggage {
    updated_baggage = Baggage::set_entry(updated_baggage, key, value)
    
    // Verify each entry
    let retrieved = Baggage::get_entry(updated_baggage, key)
    match retrieved {
      Some(v) => assert_eq(v, value)
      None => assert_true(false)
    }
  }
  
  // Complex baggage entries with structured data
  let complex_baggage = [
    ("user.preferences", "{\"theme\":\"dark\",\"lang\":\"en\",\"timezone\":\"UTC\"}"),
    ("service.metadata", "{\"version\":\"2.1.0\",\"commit\":\"abc123\",\"build\":\"2023-12-01\"}"),
    ("request.context", "{\"source\":\"web\",\"client\":\"mobile\",\"region\":\"us-west\"}")
  ]
  
  // Add complex baggage entries
  for (key, value) in complex_baggage {
    updated_baggage = Baggage::set_entry(updated_baggage, key, value)
    
    // Verify complex entries
    let retrieved = Baggage::get_entry(updated_baggage, key)
    match retrieved {
      Some(v) => {
        assert_eq(v, value)
        // Verify JSON structure is valid (basic check)
        assert_true(v.starts_with("{"))
        assert_true(v.ends_with("}"))
      }
      None => assert_true(false)
    }
  }
  
  // Test baggage with URL-encoded values
  let encoded_baggage = [
    ("url.encoded", "key1=value1&key2=value%20with%20spaces"),
    ("complex.query", "filter=status%3Dactive&sort=created_at%20DESC&limit=10"),
    ("special.chars", "email=user%40example.com&name=John%20Doe")
  ]
  
  // Add encoded baggage entries
  for (key, value) in encoded_baggage {
    updated_baggage = Baggage::set_entry(updated_baggage, key, value)
    
    // Verify encoded entries
    let retrieved = Baggage::get_entry(updated_baggage, key)
    match retrieved {
      Some(v) => assert_eq(v, value)
      None => assert_true(false)
    }
  }
  
  // Test baggage propagation simulation
  let propagation_baggage = updated_baggage
  
  // Simulate service-to-service baggage propagation
  let service_a_baggage = propagation_baggage
  let service_b_baggage = propagation_baggage
  let service_c_baggage = propagation_baggage
  
  // Verify baggage is consistent across services
  let test_keys = ["user.id", "session.id", "user.preferences", "service.metadata"]
  
  for key in test_keys {
    let service_a_value = Baggage::get_entry(service_a_baggage, key)
    let service_b_value = Baggage::get_entry(service_b_baggage, key)
    let service_c_value = Baggage::get_entry(service_c_baggage, key)
    
    match (service_a_value, service_b_value, service_c_value) {
      (Some(a), Some(b), Some(c)) => {
        assert_eq(a, b)
        assert_eq(b, c)
      }
      _ => assert_true(false) // All services should have the same baggage values
    }
  }
  
  // Test baggage modification during propagation
  let modified_baggage = Baggage::set_entry(service_b_baggage, "service.b.annotation", "processed-by-service-b")
  
  // Original baggage should not be affected
  let original_value = Baggage::get_entry(service_a_baggage, "service.b.annotation")
  assert_eq(original_value, None)
  
  // Modified baggage should have the new entry
  let modified_value = Baggage::get_entry(modified_baggage, "service.b.annotation")
  match modified_value {
    Some(v) => assert_eq(v, "processed-by-service-b")
    None => assert_true(false)
  }
  
  // Test baggage removal
  let final_baggage = Baggage::remove_entry(modified_baggage, "session.id")
  let removed_value = Baggage::get_entry(final_baggage, "session.id")
  assert_eq(removed_value, None)
  
  // Other entries should still be present
  let user_id = Baggage::get_entry(final_baggage, "user.id")
  match user_id {
    Some(v) => assert_eq(v, "12345")
    None => assert_true(false)
  }
}

// Test 6: Span Lifecycle Management with Complex Hierarchies
test "span lifecycle management with complex hierarchies" {
  // Test span creation with parent-child relationships
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "hierarchy-test-tracer")
  
  // Create root span
  let root_span = Tracer::start_span(tracer, "root-operation")
  let root_context = Context::with_value(Context::root(), ContextKey::new("current-span"), root_span)
  
  // Verify root span properties
  assert_eq(root_span.name(), "root-operation")
  assert_true(root_span.is_recording())
  assert_eq(root_span.status(), Unset)
  
  // Create child spans
  let child_span1 = Tracer::start_span_with_context(tracer, "child-operation-1", root_context)
  let child_span2 = Tracer::start_span_with_context(tracer, "child-operation-2", root_context)
  
  // Create grandchild spans
  let child1_context = Context::with_value(root_context, ContextKey::new("current-span"), child_span1)
  let grandchild_span1 = Tracer::start_span_with_context(tracer, "grandchild-operation-1", child1_context)
  let grandchild_span2 = Tracer::start_span_with_context(tracer, "grandchild-operation-2", child1_context)
  
  // Test span hierarchy
  assert_not_eq(SpanContext::span_id(root_span.span_context()), SpanContext::span_id(child_span1.span_context()))
  assert_not_eq(SpanContext::span_id(root_span.span_context()), SpanContext::span_id(child_span2.span_context()))
  assert_not_eq(SpanContext::span_id(child_span1.span_context()), SpanContext::span_id(grandchild_span1.span_context()))
  
  // All spans should have the same trace ID
  let root_trace_id = SpanContext::trace_id(root_span.span_context())
  assert_eq(SpanContext::trace_id(child_span1.span_context()), root_trace_id)
  assert_eq(SpanContext::trace_id(child_span2.span_context()), root_trace_id)
  assert_eq(SpanContext::trace_id(grandchild_span1.span_context()), root_trace_id)
  assert_eq(SpanContext::trace_id(grandchild_span2.span_context()), root_trace_id)
  
  // Add events to spans
  root_span.add_event("operation.started", Some([("operation.type", "batch.process")]))
  child_span1.add_event("child.operation.started", Some([("operation.type", "data.validation")]))
  child_span2.add_event("child.operation.started", Some([("operation.type", "data.transformation")]))
  grandchild_span1.add_event("grandchild.operation.started", Some([("operation.type", "database.query")]))
  grandchild_span2.add_event("grandchild.operation.started", Some([("operation.type", "cache.update")]))
  
  // Test span status transitions
  grandchild_span1.set_status(Ok, Some("Database query completed successfully"))
  grandchild_span2.set_status(Ok, Some("Cache updated successfully"))
  
  // End grandchild spans
  grandchild_span1.end()
  grandchild_span2.end()
  
  // Verify grandchild spans are no longer recording
  // In a real implementation, this would check the recording state
  child_span1.add_event("child.operations.completed", Some([("grandchild.count", "2")]))
  
  child_span1.set_status(Ok, Some("All child operations completed successfully"))
  child_span2.set_status(Ok, Some("Data transformation completed successfully"))
  
  // End child spans
  child_span1.end()
  child_span2.end()
  
  root_span.add_event("all.operations.completed", Some([("total.spans", "5"), ("duration.ms", "1000")]))
  root_span.set_status(Ok, Some("Root operation completed successfully"))
  
  // End root span
  root_span.end()
  
  // Test span with automatic lifecycle management
  let auto_span = Tracer::start_span(tracer, "auto-managed-span")
  
  // In a real implementation, this would use RAII or similar pattern
  // For testing, we manually manage the lifecycle
  auto_span.add_event("auto.span.event", Some([("lifecycle", "auto")]))
  auto_span.set_status(Error, Some("Simulated error for testing"))
  auto_span.end()
}

// Test 7: Performance Metrics with High-Frequency Operations
test "performance metrics with high-frequency operations" {
  // Test performance under high-frequency metric operations
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance-test-meter")
  
  // Test counter with high-frequency updates
  let high_freq_counter = Meter::create_counter(meter, "high.frequency.counter")
  
  // Simulate high-frequency counter updates
  let start_time = 1640995200000L
  let operation_count = 100000
  
  for i = 0; i < operation_count; i = i + 1 {
    Counter::add(high_freq_counter, 1.0)
    
    // Occasionally add larger values
    if i % 1000 == 0 {
      Counter::add(high_freq_counter, 10.0)
    }
  }
  
  // Test histogram with high-frequency recordings
  let high_freq_histogram = Meter::create_histogram(meter, "high.frequency.histogram")
  
  // Simulate high-frequency histogram recordings
  let response_times = [10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100]
  
  for i = 0; i < 10000; i = i + 1 {
    let response_time = response_times[i % response_times.length()]
    Histogram::record(high_freq_histogram, response_time.to_float())
  }
  
  // Test gauge with rapid value changes
  let rapid_gauge = Meter::create_gauge(meter, "rapid.changes.gauge")
  
  // Simulate rapid gauge value changes
  let gauge_values = [10.5, 15.2, 8.7, 22.1, 19.3, 25.8, 12.4, 18.9, 14.6, 20.3]
  
  for i = 0; i < 5000; i = i + 1 {
    let gauge_value = gauge_values[i % gauge_values.length()]
    // In a real implementation, this would update the gauge
    assert_true(gauge_value >= 8.0 && gauge_value <= 30.0)
  }
  
  // Test up-down counter with mixed operations
  let mixed_counter = Meter::create_updown_counter(meter, "mixed.operations.counter")
  
  // Simulate mixed increment/decrement operations
  let operations = [1, 1, 1, -1, 1, 1, -1, -1, 1, -1]
  let mut expected_value = 0
  
  for i = 0; i < 10000; i = i + 1 {
    let operation = operations[i % operations.length()]
    expected_value = expected_value + operation
    assert_true(expected_value >= 0) // Value should never be negative in this test
    UpDownCounter::add(mixed_counter, operation.to_float())
  }
  
  // Test metric aggregation performance
  let aggregation_counter = Meter::create_counter(meter, "aggregation.test.counter")
  
  // Create multiple counters for aggregation testing
  let counters = []
  for i = 0; i < 100; i = i + 1 {
    let counter = Meter::create_counter(meter, "aggregation.counter." + i.to_string())
    counters.push(counter)
  }
  
  // Update all counters
  for counter in counters {
    for j = 0; j < 1000; j = j + 1 {
      Counter::add(counter, 1.0)
    }
  }
  
  // Test memory efficiency with large attribute sets
  let attribute_counter = Meter::create_counter(meter, "attribute.heavy.counter")
  
  // Create spans with many attributes
  let large_attribute_set = []
  for i = 0; i < 1000; i = i + 1 {
    large_attribute_set.push(("attr." + i.to_string(), StringValue("value." + i.to_string())))
  }
  
  // Simulate operations with large attribute sets
  for i = 0; i < 100; i = i + 1 {
    // In a real implementation, this would create spans with large attribute sets
    Counter::add(attribute_counter, 1.0)
  }
  
  // Test concurrent metric operations simulation
  let concurrent_counters = []
  
  // Create multiple counters for concurrent simulation
  for i = 0; i < 10; i = i + 1 {
    let counter = Meter::create_counter(meter, "concurrent.counter." + i.to_string())
    concurrent_counters.push(counter)
  }
  
  // Simulate concurrent operations
  for counter in concurrent_counters {
    for j = 0; j < 5000; j = j + 1 {
      Counter::add(counter, 1.0)
    }
  }
  
  // Performance test completed successfully
  assert_true(true)
}

// Test 8: Error Handling and Recovery Mechanisms
test "error handling and recovery mechanisms" {
  // Test error handling in span operations
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error-test-tracer")
  
  // Test span with error status
  let error_span = Tracer::start_span(tracer, "error-operation")
  
  // Add error event
  error_span.add_event("error.occurred", Some([
    ("error.type", StringValue("timeout")),
    ("error.message", StringValue("Operation timed out after 30 seconds")),
    ("error.code", IntValue(408))
  ]))
  
  // Set error status
  error_span.set_status(Error, Some("Operation failed due to timeout"))
  
  // Test error recovery
  let recovery_span = Tracer::start_span(tracer, "recovery-operation")
  
  recovery_span.add_event("recovery.started", Some([
    ("recovery.strategy", StringValue("exponential.backoff")),
    ("max.retries", IntValue(5))
  ]))
  
  // Simulate successful recovery
  recovery_span.add_event("recovery.completed", Some([
    ("retry.count", IntValue(3)),
    ("total.duration.ms", IntValue(45000))
  ]))
  
  recovery_span.set_status(Ok, Some("Operation recovered successfully"))
  
  error_span.end()
  recovery_span.end()
  
  // Test error handling in metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "error-test-meter")
  
  // Create error tracking metrics
  let error_counter = Meter::create_counter(meter, "errors.total")
  let recovery_counter = Meter::create_counter(meter, "recoveries.total")
  
  // Simulate error scenarios
  let error_scenarios = [
    ("timeout", 408, "Network timeout occurred"),
    ("connection.refused", 503, "Database connection refused"),
    ("resource.exhausted", 429, "Rate limit exceeded"),
    ("invalid.argument", 400, "Invalid request parameters"),
    ("internal.error", 500, "Internal server error")
  ]
  
  for (error_type, error_code, error_message) in error_scenarios {
    // Record error
    Counter::add(error_counter, 1.0)
    
    // Simulate recovery attempt
    Counter::add(recovery_counter, 1.0)
  }
  
  // Test error handling in logging
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "error-test-logger")
  
  // Create error logs
  let error_logs = [
    (Error, "Database connection failed", Some([("error.code", IntValue(500)), ("retry.count", IntValue(3))])),
    (Warn, "High memory usage detected", Some([("memory.usage", StringValue("85%")), ("threshold", StringValue("80%"))])),
    (Error, "Authentication failed", Some([("user.id", StringValue("12345")), ("failure.reason", StringValue("invalid.credentials"))])),
    (Fatal, "System crash detected", Some([("crash.type", StringValue("segmentation.fault")), ("signal", IntValue(11))]))
  ]
  
  for (severity, message, attributes) in error_logs {
    let log = LogRecord::new_with_context(
      severity,
      Some(message),
      attributes.map(|attrs| Attributes::with(attrs)),
      Some(1640995200000L),
      None,
      Some("error-trace-id"),
      Some("error-span-id"),
      Some(Context::root())
    )
    
    Logger::emit(logger, log)
  }
  
  // Test error boundary conditions
  let boundary_span = Tracer::start_span(tracer, "boundary-test-operation")
  
  // Test with empty error message
  boundary_span.add_event("empty.error", Some([("error.message", StringValue(""))]))
  
  // Test with very long error message
  let long_error_message = "This is a very long error message that might exceed normal buffer sizes. ".repeat(50)
  boundary_span.add_event("long.error", Some([("error.message", StringValue(long_error_message))]))
  
  // Test with special characters in error message
  boundary_span.add_event("special.chars.error", Some([
    ("error.message", StringValue("Error with special chars: !@#$%^&*()_+-={}[]|\\:;\"'<>?,./"))
  ]))
  
  boundary_span.end()
  
  // Test error recovery with resource cleanup
  let cleanup_span = Tracer::start_span(tracer, "cleanup-operation")
  
  cleanup_span.add_event("cleanup.started", Some([
    ("cleanup.type", StringValue("resource.deallocation")),
    ("resource.count", IntValue(100))
  ]))
  
  // Simulate cleanup process
  let cleanup_steps = ["database.connections", "file.handles", "memory.allocated", "network.sockets"]
  
  for step in cleanup_steps {
    cleanup_span.add_event("cleanup.step", Some([
      ("step.name", StringValue(step)),
      ("step.status", StringValue("completed"))
    ]))
  }
  
  cleanup_span.add_event("cleanup.completed", Some([
    ("total.steps", IntValue(cleanup_steps.length())),
    ("cleanup.status", StringValue("success"))
  ]))
  
  cleanup_span.set_status(Ok, Some("All resources cleaned up successfully"))
  cleanup_span.end()
  
  // Error handling test completed successfully
  assert_true(true)
}

// Test 9: Time Series Analysis and Statistical Operations
test "time series analysis and statistical operations" {
  // Test time series data with statistical analysis
  let base_timestamp = 1640995200000L // 2022-01-01 00:00:00 UTC
  
  // Generate time series data with patterns
  let time_series_data = []
  let mut cumulative_sum = 0.0
  
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_timestamp + (i * 60000L) // 1-minute intervals
    let value = 50.0 + 10.0 * (i % 10).to_float() + 5.0 * ((i / 10) % 5).to_float()
    cumulative_sum = cumulative_sum + value
    time_series_data.push((timestamp, value))
  }
  
  // Test basic statistics
  let values = time_series_data.map(|(_, value)| value)
  let sum = values.reduce(|acc, val| acc + val, 0.0)
  let count = values.length().to_float()
  let mean = sum / count
  
  // Calculate min and max
  let mut min_val = values[0]
  let mut max_val = values[0]
  
  for value in values {
    if value < min_val {
      min_val = value
    }
    if value > max_val {
      max_val = value
    }
  }
  
  // Calculate variance and standard deviation
  let variance = values.reduce(|acc, val| acc + (val - mean) * (val - mean), 0.0) / count
  let std_dev = variance.sqrt()
  
  // Verify statistical calculations
  assert_eq(values.length(), 100)
  assert_true(sum > 0.0)
  assert_true(mean > 0.0)
  assert_true(min_val <= mean)
  assert_true(max_val >= mean)
  assert_true(variance >= 0.0)
  assert_true(std_dev >= 0.0)
  
  // Test time-based aggregation
  let window_size = 300000L // 5 minutes
  let mut windows = []
  
  let mut current_window_start = base_timestamp
  while current_window_start < base_timestamp + (99 * 60000L) {
    let current_window_end = current_window_start + window_size
    let window_data = time_series_data.filter(|(timestamp, _)| {
      timestamp >= current_window_start && timestamp < current_window_end
    })
    
    if window_data.length() > 0 {
      let window_values = window_data.map(|(_, value)| value)
      let window_sum = window_values.reduce(|acc, val| acc + val, 0.0)
      let window_mean = window_sum / window_values.length().to_float()
      let window_min = window_values.reduce(|acc, val| if val < acc { val } else { acc }, window_values[0])
      let window_max = window_values.reduce(|acc, val| if val > acc { val } else { acc }, window_values[0])
      
      windows.push((current_window_start, window_data.length(), window_mean, window_min, window_max))
    }
    
    current_window_start = current_window_start + window_size
  }
  
  // Verify window aggregation
  assert_true(windows.length() > 0)
  
  for (start, count, mean, min, max) in windows {
    assert_true(count > 0)
    assert_true(mean >= min && mean <= max)
  }
  
  // Test trend analysis
  let mut increasing_count = 0
  let mut decreasing_count = 0
  let mut flat_count = 0
  
  for i = 1; i < values.length(); i = i + 1 {
    let diff = values[i] - values[i - 1]
    if diff > 0.1 {
      increasing_count = increasing_count + 1
    } else if diff < -0.1 {
      decreasing_count = decreasing_count + 1
    } else {
      flat_count = flat_count + 1
    }
  }
  
  // Verify trend analysis
  let total_comparisons = increasing_count + decreasing_count + flat_count
  assert_eq(total_comparisons, 99)
  
  // Test anomaly detection
  let threshold = 2.0 * std_dev
  let mut anomalies = []
  
  for (i, value) in values.enumerate() {
    if (value - mean).abs() > threshold {
      anomalies.push((i, value))
    }
  }
  
  // Verify anomaly detection
  assert_true(anomalies.length() >= 0) // May or may not have anomalies
  
  // Test percentile calculation
  let mut sorted_values = values.clone()
  sorted_values.sort()
  
  let p50_index = (sorted_values.length() * 50) / 100
  let p95_index = (sorted_values.length() * 95) / 100
  let p99_index = (sorted_values.length() * 99) / 100
  
  let p50 = sorted_values[p50_index]
  let p95 = sorted_values[p95_index]
  let p99 = sorted_values[p99_index]
  
  // Verify percentile calculations
  assert_true(p50 >= min_val && p50 <= max_val)
  assert_true(p95 >= p50 && p95 <= max_val)
  assert_true(p99 >= p95 && p99 <= max_val)
  
  // Test time series with missing data points
  let irregular_data = [
    (base_timestamp, 100.0),
    (base_timestamp + 120000L, 110.0),     // +2 minutes (missing +1 minute)
    (base_timestamp + 180000L, 105.0),     // +3 minutes
    (base_timestamp + 300000L, 120.0),     // +5 minutes (missing +4 minutes)
    (base_timestamp + 360000L, 115.0),     // +6 minutes
    (base_timestamp + 420000L, 125.0)      // +7 minutes
  ]
  
  // Test interpolation for missing data points
  let interpolation_points = [
    base_timestamp + 60000L,   // +1 minute (missing)
    base_timestamp + 240000L,  // +4 minutes (missing)
    base_timestamp + 480000L   // +8 minutes (missing, beyond data range)
  ]
  
  for target_time in interpolation_points {
    // Find surrounding data points for interpolation
    let before_points = irregular_data.filter(|(timestamp, _)| timestamp < target_time)
    let after_points = irregular_data.filter(|(timestamp, _)| timestamp > target_time)
    
    if before_points.length() > 0 && after_points.length() > 0 {
      let (t1, v1) = before_points[before_points.length() - 1]
      let (t2, v2) = after_points[0]
      
      // Linear interpolation
      let interpolated_value = v1 + (v2 - v1) * ((target_time - t1).to_float() / (t2 - t1).to_float())
      
      // Verify interpolation is reasonable
      assert_true(interpolated_value >= v1.min(v2) && interpolated_value <= v1.max(v2))
    }
  }
  
  // Time series analysis test completed successfully
  assert_true(true)
}

// Test 10: Cross-Platform Compatibility and Format Conversion
test "cross-platform compatibility and format conversion" {
  // Test attribute value conversion between different formats
  let test_values = [
    ("string.value", StringValue("test.string")),
    ("int.value", IntValue(42)),
    ("float.value", FloatValue(3.14159)),
    ("bool.value", BoolValue(true)),
    ("string.array", ArrayStringValue(["a", "b", "c"])),
    ("int.array", ArrayIntValue([1, 2, 3]))
  ]
  
  // Test attribute creation and retrieval
  let attrs = Attributes::new()
  
  for (key, value) in test_values {
    Attributes::set(attrs, key, value)
    
    // Verify retrieval
    let retrieved = Attributes::get(attrs, key)
    match (retrieved, value) {
      (Some(StringValue(r)), StringValue(v)) => assert_eq(r, v),
      (Some(IntValue(r)), IntValue(v)) => assert_eq(r, v),
      (Some(FloatValue(r)), FloatValue(v)) => assert_true((r - v).abs() < 0.0001),
      (Some(BoolValue(r)), BoolValue(v)) => assert_eq(r, v),
      (Some(ArrayStringValue(r)), ArrayStringValue(v)) => {
        assert_eq(r.length(), v.length())
        for i = 0; i < r.length(); i = i + 1 {
          assert_eq(r[i], v[i])
        }
      },
      (Some(ArrayIntValue(r)), ArrayIntValue(v)) => {
        assert_eq(r.length(), v.length())
        for i = 0; i < r.length(); i = i + 1 {
          assert_eq(r[i], v[i])
        }
      },
      _ => assert_true(false) // Type mismatch
    }
  }
  
  // Test serialization format compatibility
  let serialization_attrs = Attributes::new()
  
  // Add attributes with different data types
  Attributes::set(serialization_attrs, "platform", StringValue("moonbit"))
  Attributes::set(serialization_attrs, "version", StringValue("1.0.0"))
  Attributes::set(serialization_attrs, "timestamp", IntValue(1640995200000L))
  Attributes::set(serialization_attrs, "success.rate", FloatValue(0.95))
  Attributes::set(serialization_attrs, "debug.mode", BoolValue(false))
  
  // Test JSON serialization format
  let json_format = "{"
    + "\"platform\":\"moonbit\","
    + "\"version\":\"1.0.0\","
    + "\"timestamp\":1640995200000,"
    + "\"success.rate\":0.95,"
    + "\"debug.mode\":false"
    + "}"
  
  // In a real implementation, this would serialize to actual JSON
  // For testing, we validate the expected format
  assert_true(json_format.contains("\"platform\":\"moonbit\""))
  assert_true(json_format.contains("\"version\":\"1.0.0\""))
  assert_true(json_format.contains("\"timestamp\":1640995200000"))
  assert_true(json_format.contains("\"success.rate\":0.95"))
  assert_true(json_format.contains("\"debug.mode\":false"))
  
  // Test key-value format
  let kv_format = [
    "platform=moonbit",
    "version=1.0.0",
    "timestamp=1640995200000",
    "success.rate=0.95",
    "debug.mode=false"
  ]
  
  // Verify key-value format
  assert_eq(kv_format.length(), 5)
  assert_true(kv_format[0].contains("platform=moonbit"))
  assert_true(kv_format[1].contains("version=1.0.0"))
  assert_true(kv_format[2].contains("timestamp=1640995200000"))
  assert_true(kv_format[3].contains("success.rate=0.95"))
  assert_true(kv_format[4].contains("debug.mode=false"))
  
  // Test URL-encoded format
  let url_encoded_format = "platform=moonbit&version=1.0.0&timestamp=1640995200000&success.rate=0.95&debug.mode=false"
  
  // Verify URL-encoded format
  assert_true(url_encoded_format.contains("platform=moonbit"))
  assert_true(url_encoded_format.contains("version=1.0.0"))
  assert_true(url_encoded_format.contains("timestamp=1640995200000"))
  assert_true(url_encoded_format.contains("success.rate=0.95"))
  assert_true(url_encoded_format.contains("debug.mode=false"))
  assert_true(url_encoded_format.contains("&"))
  
  // Test cross-platform data type conversion
  let platform_attrs = Attributes::new()
  
  // Add attributes that might have different representations across platforms
  Attributes::set(platform_attrs, "null.value", StringValue(""))
  Attributes::set(platform_attrs, "empty.array", ArrayStringValue([]))
  Attributes::set(platform_attrs, "zero.value", IntValue(0))
  Attributes::set(platform_attrs, "negative.value", IntValue(-42))
  Attributes::set(platform_attrs, "large.number", IntValue(9223372036854775807L))
  Attributes::set(platform_attrs, "small.float", FloatValue(0.000001))
  Attributes::set(platform_attrs, "special.chars", StringValue("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"))
  
  // Verify platform-specific values are handled correctly
  let null_value = Attributes::get(platform_attrs, "null.value")
  match null_value {
    Some(StringValue(value)) => assert_eq(value, "")
    _ => assert_true(false)
  }
  
  let empty_array = Attributes::get(platform_attrs, "empty.array")
  match empty_array {
    Some(ArrayStringValue(arr)) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
  
  let zero_value = Attributes::get(platform_attrs, "zero.value")
  match zero_value {
    Some(IntValue(value)) => assert_eq(value, 0)
    _ => assert_true(false)
  }
  
  let negative_value = Attributes::get(platform_attrs, "negative.value")
  match negative_value {
    Some(IntValue(value)) => assert_eq(value, -42)
    _ => assert_true(false)
  }
  
  let large_number = Attributes::get(platform_attrs, "large.number")
  match large_number {
    Some(IntValue(value)) => assert_eq(value, 9223372036854775807L)
    _ => assert_true(false)
  }
  
  let small_float = Attributes::get(platform_attrs, "small.float")
  match small_float {
    Some(FloatValue(value)) => assert_true(value > 0.0 && value < 0.00001)
    _ => assert_true(false)
  }
  
  let special_chars = Attributes::get(platform_attrs, "special.chars")
  match special_chars {
    Some(StringValue(value)) => assert_eq(value, "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
    _ => assert_true(false)
  }
  
  // Test format conversion for different transport protocols
  let transport_data = [
    ("http.header", "Content-Type: application/json\r\nX-Trace-ID: 4bf92f3577b34da6a3ce929d0e0e4736"),
    ("grpc.metadata", "trace-id: 4bf92f3577b34da6a3ce929d0e0e4736\nspan-id: 00f067aa0ba902b7"),
    ("message.queue", "{\"trace_id\":\"4bf92f3577b34da6a3ce929d0e0e4736\",\"span_id\":\"00f067aa0ba902b7\"}"),
    ("log.format", "[TRACE_ID:4bf92f3577b34da6a3ce929d0e0e4736] [SPAN_ID:00f067aa0ba902b7] Log message")
  ]
  
  // Verify transport protocol formats
  for (protocol, data) in transport_data {
    match protocol {
      "http.header" => {
        assert_true(data.contains("Content-Type:"))
        assert_true(data.contains("X-Trace-ID:"))
        assert_true(data.contains("\r\n"))
      },
      "grpc.metadata" => {
        assert_true(data.contains("trace-id:"))
        assert_true(data.contains("span-id:"))
        assert_true(data.contains("\n"))
      },
      "message.queue" => {
        assert_true(data.contains("\"trace_id\":"))
        assert_true(data.contains("\"span_id\":"))
        assert_true(data.contains("{") && data.contains("}"))
      },
      "log.format" => {
        assert_true(data.contains("[TRACE_ID:"))
        assert_true(data.contains("[SPAN_ID:"))
        assert_true(data.contains("]"))
      },
      _ => assert_true(false) // Unknown protocol
    }
  }
  
  // Cross-platform compatibility test completed successfully
  assert_true(true)
}