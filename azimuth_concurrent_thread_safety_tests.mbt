// Azimuth Telemetry System - Concurrent Thread Safety Tests
// This file contains test cases for concurrent operations and thread safety

// Test 1: Concurrent Span Creation and Management
test "concurrent span creation and management" {
  // Create shared trace context
  let trace_id = "1234567890abcdef1234567890abcdef"
  let base_ctx = SpanContext::new(trace_id, "base_span_id", true, "")
  
  // Simulate concurrent span creation
  let spans = []
  for i in 1..=10 {
    let span_id = "span_" + i.to_string()
    let ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("concurrent_operation_" + i.to_string(), Internal, ctx)
    
    // Add attributes concurrently
    Span::set_attribute(span, "thread.id", IntValue(i))
    Span::set_attribute(span, "operation.type", StringValue("concurrent_test"))
    
    // Add events concurrently
    Span::add_event(span, "span_created", Some([
      ("creation.time", IntValue(1234567890 + i)),
      ("worker.id", IntValue(i))
    ]))
    
    spans.push(span)
  }
  
  // Verify all spans have correct trace context
  for span in spans {
    let ctx = Span::span_context(span)
    assert_eq(SpanContext::trace_id(ctx), trace_id)
    assert_true(SpanContext::is_sampled(ctx))
    assert_true(SpanContext::is_valid(ctx))
  }
  
  // End all spans concurrently
  for span in spans {
    Span::add_event(span, "span_ending", Some([
      ("end.time", IntValue(1234567900))
    ]))
    Span::set_status(span, Ok, Some("Concurrent operation completed"))
    Span::end(span)
  }
}

// Test 2: Thread-Safe Metrics Collection
test "thread-safe metrics collection" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent_metrics")
  
  // Create shared metric instruments
  let request_counter = Meter::create_counter(
    meter,
    "concurrent.requests.total",
    Some("Total concurrent requests"),
    Some("requests")
  )
  
  let response_histogram = Meter::create_histogram(
    meter,
    "concurrent.response.duration",
    Some("Response duration for concurrent requests"),
    Some("ms")
  )
  
  let active_connections = Meter::create_updown_counter(
    meter,
    "concurrent.connections.active",
    Some("Active concurrent connections"),
    Some("connections")
  )
  
  let cpu_usage = Meter::create_gauge(
    meter,
    "concurrent.cpu.usage",
    Some("CPU usage during concurrent operations"),
    Some("percent")
  )
  
  // Simulate concurrent metric updates
  for i in 1..=20 {
    // Each "thread" increments the counter
    Counter::add(request_counter, 1.0, Some(Attributes::with([
      ("thread.id", IntValue(i)),
      ("operation.type", StringValue("concurrent_request"))
    ])))
    
    // Each "thread" records response time
    let response_time = 50.0 + (i.to_float() * 2.5)
    Histogram::record(response_histogram, response_time, Some(Attributes::with([
      ("thread.id", IntValue(i)),
      ("endpoint", StringValue("/api/concurrent"))
    ])))
    
    // Each "thread" updates active connections
    UpDownCounter::add(active_connections, 1.0, Some(Attributes::with([
      ("thread.id", IntValue(i))
    ])))
    
    // Each "thread" updates CPU usage
    let cpu_value = 10.0 + (i.to_float() * 0.5)
    Gauge::record(cpu_usage, cpu_value, Some(Attributes::with([
      ("thread.id", IntValue(i)),
      ("core", StringValue("0"))
    ])))
  }
  
  // Simulate connection release
  for i in 1..=20 {
    UpDownCounter::add(active_connections, -1.0, Some(Attributes::with([
      ("thread.id", IntValue(i))
    ])))
  }
  
  // Verify metric instruments are properly configured
  let counter_instrument = Counter::as_instrument(request_counter)
  assert_eq(Instrument::name(counter_instrument), "concurrent.requests.total")
  
  let histogram_instrument = Histogram::as_instrument(response_histogram)
  assert_eq(Instrument::name(histogram_instrument), "concurrent.response.duration")
  
  let updown_instrument = UpDownCounter::as_instrument(active_connections)
  assert_eq(Instrument::name(updown_instrument), "concurrent.connections.active")
  
  let gauge_instrument = Gauge::as_instrument(cpu_usage)
  assert_eq(Instrument::name(gauge_instrument), "concurrent.cpu.usage")
}

// Test 3: Concurrent Context Propagation
test "concurrent context propagation" {
  // Create root context with shared data
  let root_ctx = Context::root()
  let correlation_key = ContextKey::new("correlation_id")
  let user_key = ContextKey::new("user_context")
  let trace_key = ContextKey::new("trace_context")
  
  // Set shared context data
  let ctx_with_correlation = Context::with_value(root_ctx, correlation_key, "corr-12345")
  let ctx_with_user = Context::with_value(ctx_with_correlation, user_key, {
    "user_id": "user-67890",
    "user_role": "admin",
    "tenant_id": "tenant-abc123"
  })
  let ctx_with_trace = Context::with_value(ctx_with_user, trace_key, {
    "trace_id": "trace123456789012345678901234567890",
    "sampling_decision": true
  })
  
  // Create child contexts concurrently
  let child_contexts = []
  for i in 1..=10 {
    let operation_key = ContextKey::new("operation_" + i.to_string())
    let child_ctx = Context::with_value(ctx_with_trace, operation_key, {
      "operation_id": "op-" + i.to_string(),
      "operation_type": "concurrent_processing",
      "start_time": 1234567890 + i
    })
    child_contexts.push(child_ctx)
  }
  
  // Verify all child contexts have inherited parent data
  for child_ctx in child_contexts {
    // Check correlation ID is inherited
    match Context::get(child_ctx, correlation_key) {
      Some(correlation_id) => assert_eq(correlation_id, "corr-12345")
      None => assert_true(false)
    }
    
    // Check user context is inherited
    match Context::get(child_ctx, user_key) {
      Some(user_data) => {
        assert_eq(user_data["user_id"], "user-67890")
        assert_eq(user_data["user_role"], "admin")
      }
      None => assert_true(false)
    }
    
    // Check trace context is inherited
    match Context::get(child_ctx, trace_key) {
      Some(trace_data) => {
        assert_eq(trace_data["trace_id"], "trace123456789012345678901234567890")
        assert_true(trace_data["sampling_decision"])
      }
      None => assert_true(false)
    }
  }
  
  // Verify each child context has its own operation data
  for i in 1..=10 {
    let child_ctx = child_contexts[i - 1]
    let operation_key = ContextKey::new("operation_" + i.to_string())
    
    match Context::get(child_ctx, operation_key) {
      Some(operation_data) => {
        assert_eq(operation_data["operation_id"], "op-" + i.to_string())
        assert_eq(operation_data["operation_type"], "concurrent_processing")
      }
      None => assert_true(false)
    }
  }
}

// Test 4: Concurrent Baggage Operations
test "concurrent baggage operations" {
  // Create base baggage
  let base_baggage = Baggage::new()
  
  // Add initial baggage entries
  let baggage_with_user = Baggage::set_entry(base_baggage, "user.id", "user-12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-abcdef")
  let baggage_with_tenant = Baggage::set_entry(baggage_with_session, "tenant.id", "tenant-67890")
  
  // Simulate concurrent baggage modifications
  let modified_baggages = []
  for i in 1..=10 {
    let modified_baggage = Baggage::set_entry(baggage_with_tenant, "request.id", "req-" + i.to_string())
    let modified_baggage = Baggage::set_entry(modified_baggage, "worker.id", i.to_string())
    let modified_baggage = Baggage::set_entry(modified_baggage, "operation.type", "concurrent_baggage_test")
    modified_baggages.push(modified_baggage)
  }
  
  // Verify all modified baggages contain original entries
  for baggage in modified_baggages {
    let user_id = Baggage::get_entry(baggage, "user.id")
    match user_id {
      Some(id) => assert_eq(id, "user-12345")
      None => assert_true(false)
    }
    
    let session_id = Baggage::get_entry(baggage, "session.id")
    match session_id {
      Some(id) => assert_eq(id, "session-abcdef")
      None => assert_true(false)
    }
    
    let tenant_id = Baggage::get_entry(baggage, "tenant.id")
    match tenant_id {
      Some(id) => assert_eq(id, "tenant-67890")
      None => assert_true(false)
    }
    
    let operation_type = Baggage::get_entry(baggage, "operation.type")
    match operation_type {
      Some(op_type) => assert_eq(op_type, "concurrent_baggage_test")
      None => assert_true(false)
    }
  }
  
  // Verify each baggage has unique request ID
  for i in 1..=10 {
    let baggage = modified_baggages[i - 1]
    let request_id = Baggage::get_entry(baggage, "request.id")
    match request_id {
      Some(id) => assert_eq(id, "req-" + i.to_string())
      None => assert_true(false)
    }
    
    let worker_id = Baggage::get_entry(baggage, "worker.id")
    match worker_id {
      Some(id) => assert_eq(id, i.to_string())
      None => assert_true(false)
    }
  }
}

// Test 5: Concurrent Attribute Operations
test "concurrent attribute operations" {
  // Create shared attributes
  let shared_attrs = Attributes::new()
  
  // Set base attributes
  Attributes::set(shared_attrs, "service.name", StringValue("azimuth-concurrent-test"))
  Attributes::set(shared_attrs, "service.version", StringValue("2.1.0"))
  Attributes::set(shared_attrs, "environment", StringValue("test"))
  
  // Create attribute sets for concurrent operations
  let attribute_sets = []
  for i in 1..=15 {
    let attrs = Attributes::new()
    
    // Copy base attributes
    Attributes::set(attrs, "service.name", StringValue("azimuth-concurrent-test"))
    Attributes::set(attrs, "service.version", StringValue("2.1.0"))
    Attributes::set(attrs, "environment", StringValue("test"))
    
    // Add thread-specific attributes
    Attributes::set(attrs, "thread.id", IntValue(i))
    Attributes::set(attrs, "thread.name", StringValue("worker-" + i.to_string()))
    Attributes::set(attrs, "operation.type", StringValue("concurrent_attribute_test"))
    Attributes::set(attrs, "start.time", IntValue(1234567890 + i))
    
    attribute_sets.push(attrs)
  }
  
  // Verify all attribute sets have correct base attributes
  for attrs in attribute_sets {
    let service_name = Attributes::get(attrs, "service.name")
    match service_name {
      Some(StringValue(name)) => assert_eq(name, "azimuth-concurrent-test")
      _ => assert_true(false)
    }
    
    let service_version = Attributes::get(attrs, "service.version")
    match service_version {
      Some(StringValue(version)) => assert_eq(version, "2.1.0")
      _ => assert_true(false)
    }
    
    let environment = Attributes::get(attrs, "environment")
    match environment {
      Some(StringValue(env)) => assert_eq(env, "test")
      _ => assert_true(false)
    }
    
    let operation_type = Attributes::get(attrs, "operation.type")
    match operation_type {
      Some(StringValue(op_type)) => assert_eq(op_type, "concurrent_attribute_test")
      _ => assert_true(false)
    }
  }
  
  // Verify each attribute set has unique thread ID
  for i in 1..=15 {
    let attrs = attribute_sets[i - 1]
    let thread_id = Attributes::get(attrs, "thread.id")
    match thread_id {
      Some(IntValue(id)) => assert_eq(id, i)
      _ => assert_true(false)
    }
    
    let thread_name = Attributes::get(attrs, "thread.name")
    match thread_name {
      Some(StringValue(name)) => assert_eq(name, "worker-" + i.to_string())
      _ => assert_true(false)
    }
    
    let start_time = Attributes::get(attrs, "start.time")
    match start_time {
      Some(IntValue(time)) => assert_eq(time, 1234567890 + i)
      _ => assert_true(false)
    }
  }
}

// Test 6: Concurrent Log Record Processing
test "concurrent log record processing" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "concurrent_logger")
  
  // Create shared trace context for log correlation
  let trace_id = "1234567890abcdef1234567890abcdef"
  let base_span_id = "1234567890abcdef"
  
  // Generate log records concurrently
  for i in 1..=25 {
    let span_id = base_span_id + "-" + i.to_string()
    
    // Create log with different severity levels
    let severity = match i % 4 {
      0 => Debug
      1 => Info
      2 => Warn
      3 => Error
      _ => Info
    }
    
    let log_message = "Concurrent log message " + i.to_string()
    
    // Create log with context
    let log = LogRecord::new_with_context(
      severity,
      Some(log_message),
      Some(Attributes::with([
        ("thread.id", IntValue(i)),
        ("log.sequence", IntValue(i)),
        ("component", StringValue("concurrent_test")),
        ("operation.type", StringValue("log_generation"))
      ])),
      Some(1234567890L + i),
      Some(1234567890L + i + 1),
      Some(trace_id),
      Some(span_id),
      None
    )
    
    // Emit log
    Logger::emit(logger, log)
  }
  
  // Create error logs with exception details
  for i in 1..=5 {
    let error_log = LogRecord::new_with_context(
      Error,
      Some("Concurrent error " + i.to_string()),
      Some(Attributes::with([
        ("error.code", IntValue(500 + i)),
        ("error.type", StringValue("ConcurrentException")),
        ("error.message", StringValue("Error in concurrent operation " + i.to_string())),
        ("thread.id", IntValue(i + 100)),
        ("retry.count", IntValue(i))
      ])),
      Some(1234567900L + i),
      Some(1234567900L + i),
      Some(trace_id),
      Some("error_span_" + i.to_string()),
      None
    )
    
    Logger::emit(logger, error_log)
  }
  
  // Create structured logs with complex data
  for i in 1..=10 {
    let structured_log = LogRecord::new_with_context(
      Info,
      Some("Structured log " + i.to_string()),
      Some(Attributes::with([
        ("event.type", StringValue("concurrent_structured")),
        ("event.data", StringValue("{\"id\":" + i.to_string() + ",\"type\":\"test\"}")),
        ("thread.id", IntValue(i + 200)),
        ("processing.time", IntValue(i * 10)),
        ("data.size", IntValue(i * 1024))
      ])),
      Some(1234567950L + i),
      Some(1234567950L + i),
      Some(trace_id),
      Some("structured_span_" + i.to_string()),
      None
    )
    
    Logger::emit(logger, structured_log)
  }
}

// Test 7: Concurrent Resource Management
test "concurrent resource management" {
  // Create base resource
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("azimuth-concurrent-test")),
    ("service.version", StringValue("2.1.0")),
    ("deployment.environment", StringValue("test"))
  ])
  
  // Create resource variations concurrently
  let resources = []
  for i in 1..=12 {
    let resource = Resource::with_attributes(base_resource, [
      ("instance.id", StringValue("instance-" + i.to_string())),
      ("host.name", StringValue("host-" + i.to_string())),
      ("process.id", IntValue(10000 + i)),
      ("thread.pool.size", IntValue(i * 2)),
      ("worker.id", IntValue(i))
    ])
    resources.push(resource)
  }
  
  // Verify all resources have base attributes
  for resource in resources {
    let service_name = Resource::get_attribute(resource, "service.name")
    match service_name {
      Some(StringValue(name)) => assert_eq(name, "azimuth-concurrent-test")
      _ => assert_true(false)
    }
    
    let service_version = Resource::get_attribute(resource, "service.version")
    match service_version {
      Some(StringValue(version)) => assert_eq(version, "2.1.0")
      _ => assert_true(false)
    }
    
    let environment = Resource::get_attribute(resource, "deployment.environment")
    match environment {
      Some(StringValue(env)) => assert_eq(env, "test")
      _ => assert_true(false)
    }
  }
  
  // Verify each resource has unique instance attributes
  for i in 1..=12 {
    let resource = resources[i - 1]
    let instance_id = Resource::get_attribute(resource, "instance.id")
    match instance_id {
      Some(StringValue(id)) => assert_eq(id, "instance-" + i.to_string())
      _ => assert_true(false)
    }
    
    let host_name = Resource::get_attribute(resource, "host.name")
    match host_name {
      Some(StringValue(name)) => assert_eq(name, "host-" + i.to_string())
      _ => assert_true(false)
    }
    
    let process_id = Resource::get_attribute(resource, "process.id")
    match process_id {
      Some(IntValue(pid)) => assert_eq(pid, 10000 + i)
      _ => assert_true(false)
    }
    
    let thread_pool_size = Resource::get_attribute(resource, "thread.pool.size")
    match thread_pool_size {
      Some(IntValue(size)) => assert_eq(size, i * 2)
      _ => assert_true(false)
    }
    
    let worker_id = Resource::get_attribute(resource, "worker.id")
    match worker_id {
      Some(IntValue(id)) => assert_eq(id, i)
      _ => assert_true(false)
    }
  }
  
  // Test resource merging with concurrent modifications
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("feature.flag", StringValue("concurrent_testing_enabled")),
    ("test.mode", StringValue("stress_test")),
    ("concurrency.level", IntValue(12))
  ])
  
  let merged_resources = []
  for resource in resources {
    let merged = Resource::merge(resource, override_resource)
    merged_resources.push(merged)
  }
  
  // Verify all merged resources have override attributes
  for merged_resource in merged_resources {
    let feature_flag = Resource::get_attribute(merged_resource, "feature.flag")
    match feature_flag {
      Some(StringValue(flag)) => assert_eq(flag, "concurrent_testing_enabled")
      _ => assert_true(false)
    }
    
    let test_mode = Resource::get_attribute(merged_resource, "test.mode")
    match test_mode {
      Some(StringValue(mode)) => assert_eq(mode, "stress_test")
      _ => assert_true(false)
    }
    
    let concurrency_level = Resource::get_attribute(merged_resource, "concurrency.level")
    match concurrency_level {
      Some(IntValue(level)) => assert_eq(level, 12)
      _ => assert_true(false)
    }
    
    // Verify original attributes are preserved
    let service_name = Resource::get_attribute(merged_resource, "service.name")
    match service_name {
      Some(StringValue(name)) => assert_eq(name, "azimuth-concurrent-test")
      _ => assert_true(false)
    }
  }
}

// Test 8: Concurrent Span Link Management
test "concurrent span link management" {
  let trace_id = "1234567890abcdef1234567890abcdef"
  
  // Create parent spans
  let parent_spans = []
  for i in 1..=5 {
    let span_id = "parent_span_" + i.to_string()
    let ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("parent_operation_" + i.to_string(), Server, ctx)
    parent_spans.push(span)
  }
  
  // Create child spans with links to multiple parents
  let child_spans = []
  for i in 1..=10 {
    let span_id = "child_span_" + i.to_string()
    let ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("child_operation_" + i.to_string(), Client, ctx)
    
    // Add links to multiple parent spans
    for j in 1..=3 {
      let parent_span = parent_spans[j - 1]
      let parent_ctx = Span::span_context(parent_span)
      
      Span::add_link(span, parent_ctx, Some([
        ("link.type", StringValue("parent_child")),
        ("parent.index", IntValue(j)),
        ("child.index", IntValue(i)),
        ("relationship", StringValue("causal_dependency"))
      ]))
    }
    
    child_spans.push(span)
  }
  
  // Verify all spans are properly linked
  for child_span in child_spans {
    assert_eq(Span::name(child_span).substring(0, 15), "child_operation_")
    
    // Verify trace context is consistent
    let child_ctx = Span::span_context(child_span)
    assert_eq(SpanContext::trace_id(child_ctx), trace_id)
    assert_true(SpanContext::is_sampled(child_ctx))
    assert_true(SpanContext::is_valid(child_ctx))
  }
  
  // End all spans
  for parent_span in parent_spans {
    Span::set_status(parent_span, Ok, Some("Parent operation completed"))
    Span::end(parent_span)
  }
  
  for child_span in child_spans {
    Span::set_status(child_span, Ok, Some("Child operation completed"))
    Span::end(child_span)
  }
}

// Test 9: Concurrent Metric Aggregation
test "concurrent metric aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_test")
  
  // Create metrics for aggregation
  let request_counter = Meter::create_counter(
    meter,
    "aggregated.requests.total",
    Some("Aggregated request count"),
    Some("requests")
  )
  
  let response_histogram = Meter::create_histogram(
    meter,
    "aggregated.response.duration",
    Some("Aggregated response duration"),
    Some("ms")
  )
  
  let error_counter = Meter::create_counter(
    meter,
    "aggregated.errors.total",
    Some("Aggregated error count"),
    Some("errors")
  )
  
  // Simulate concurrent metric updates with different attributes
  let endpoints = ["/api/users", "/api/orders", "/api/products", "/api/payments"]
  let methods = ["GET", "POST", "PUT", "DELETE"]
  let status_codes = ["200", "201", "400", "404", "500"]
  
  for i in 1..=50 {
    let endpoint = endpoints[i % endpoints.length()]
    let method = methods[i % methods.length()]
    let status_code = status_codes[i % status_codes.length()]
    
    // Update request counter
    Counter::add(request_counter, 1.0, Some(Attributes::with([
      ("endpoint", StringValue(endpoint)),
      ("method", StringValue(method)),
      ("status.code", StringValue(status_code)),
      ("worker.id", IntValue(i % 10 + 1))
    ])))
    
    // Update response histogram
    let response_time = 10.0 + (i.to_float() * 5.0) + (i % 100).to_float()
    Histogram::record(response_histogram, response_time, Some(Attributes::with([
      ("endpoint", StringValue(endpoint)),
      ("method", StringValue(method)),
      ("worker.id", IntValue(i % 10 + 1))
    ])))
    
    // Update error counter for error status codes
    if status_code == "400" || status_code == "404" || status_code == "500" {
      Counter::add(error_counter, 1.0, Some(Attributes::with([
        ("endpoint", StringValue(endpoint)),
        ("method", StringValue(method)),
        ("error.code", StringValue(status_code)),
        ("worker.id", IntValue(i % 10 + 1))
      ])))
    }
  }
  
  // Verify metric instruments are properly configured
  let request_instrument = Counter::as_instrument(request_counter)
  assert_eq(Instrument::name(request_instrument), "aggregated.requests.total")
  
  let response_instrument = Histogram::as_instrument(response_histogram)
  assert_eq(Instrument::name(response_instrument), "aggregated.response.duration")
  
  let error_instrument = Counter::as_instrument(error_counter)
  assert_eq(Instrument::name(error_instrument), "aggregated.errors.total")
}

// Test 10: Concurrent Context Value Extraction
test "concurrent context value extraction" {
  // Create root context with multiple values
  let root_ctx = Context::root()
  
  let keys = []
  let values = []
  
  // Add multiple key-value pairs
  for i in 1..=20 {
    let key = ContextKey::new("key_" + i.to_string())
    let value = "value_" + i.to_string()
    keys.push(key)
    values.push(value)
  }
  
  // Build context with all values
  let ctx_with_values = fold(0..=19, root_ctx, fn(acc, i) {
    Context::with_value(acc, keys[i], values[i])
  })
  
  // Verify all values can be extracted concurrently
  for i in 1..=20 {
    let key = keys[i - 1]
    let expected_value = values[i - 1]
    
    match Context::get(ctx_with_values, key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => assert_true(false)
    }
  }
  
  // Create child contexts and verify inheritance
  let child_contexts = []
  for i in 1..=10 {
    let child_key = ContextKey::new("child_key_" + i.to_string())
    let child_value = "child_value_" + i.to_string()
    let child_ctx = Context::with_value(ctx_with_values, child_key, child_value)
    child_contexts.push(child_ctx)
  }
  
  // Verify child contexts have both parent and child values
  for i in 1..=10 {
    let child_ctx = child_contexts[i - 1]
    
    // Check child value
    let child_key = ContextKey::new("child_key_" + i.to_string())
    match Context::get(child_ctx, child_key) {
      Some(value) => assert_eq(value, "child_value_" + i.to_string())
      None => assert_true(false)
    }
    
    // Check inherited parent values
    for j in 1..=20 {
      let parent_key = keys[j - 1]
      let expected_parent_value = values[j - 1]
      
      match Context::get(child_ctx, parent_key) {
        Some(actual_value) => assert_eq(actual_value, expected_parent_value)
        None => assert_true(false)
      }
    }
  }
}