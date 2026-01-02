// Azimuth Advanced Quality Test Suite
// This file contains 8 high-quality test cases focusing on advanced telemetry scenarios

// Test 1: Complex Attribute Value Operations
test "complex attribute value nested operations" {
  // Test nested attribute structures
  let nested_attrs = Attributes::new()
  
  // Set string attribute
  Attributes::set(nested_attrs, "service.name", StringValue("azimuth-telemetry"))
  
  // Set integer attribute
  Attributes::set(nested_attrs, "service.instance.id", IntValue(12345))
  
  // Set float attribute
  Attributes::set(nested_attrs, "service.cpu.usage", FloatValue(75.5))
  
  // Set boolean attribute
  Attributes::set(nested_attrs, "service.healthy", BoolValue(true))
  
  // Set array attributes
  Attributes::set(nested_attrs, "service.tags", ArrayStringValue(["telemetry", "monitoring", "observability"]))
  Attributes::set(nested_attrs, "service.port.range", ArrayIntValue([8080, 8081, 8082]))
  
  // Verify string attribute retrieval
  match Attributes::get(nested_attrs, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "azimuth-telemetry")
    _ => assert_true(false)
  }
  
  // Verify integer attribute retrieval
  match Attributes::get(nested_attrs, "service.instance.id") {
    Some(IntValue(id)) => assert_eq(id, 12345)
    _ => assert_true(false)
  }
  
  // Verify float attribute retrieval
  match Attributes::get(nested_attrs, "service.cpu.usage") {
    Some(FloatValue(usage)) => assert_true(usage >= 75.0 && usage <= 76.0)
    _ => assert_true(false)
  }
  
  // Verify boolean attribute retrieval
  match Attributes::get(nested_attrs, "service.healthy") {
    Some(BoolValue(healthy)) => assert_true(healthy)
    _ => assert_true(false)
  }
  
  // Verify array attribute retrieval
  match Attributes::get(nested_attrs, "service.tags") {
    Some(ArrayStringValue(tags)) => {
      assert_eq(tags.length(), 3)
      assert_true(tags.contains("telemetry"))
      assert_true(tags.contains("monitoring"))
      assert_true(tags.contains("observability"))
    }
    _ => assert_true(false)
  }
}

// Test 2: Cross-Service Tracing Consistency
test "cross-service tracing consistency validation" {
  // Create trace context for service A
  let trace_id_a = "trace_1234567890abcdef"
  let span_id_a = "span_1111111111111111"
  let span_context_a = SpanContext::new(trace_id_a, span_id_a, true, "key1=value1,key2=value2")
  
  // Verify trace context properties
  assert_eq(SpanContext::trace_id(span_context_a), trace_id_a)
  assert_eq(SpanContext::span_id(span_context_a), span_id_a)
  assert_true(SpanContext::is_valid(span_context_a))
  assert_true(SpanContext::is_sampled(span_context_a))
  
  // Create child span for service B
  let span_id_b = "span_2222222222222222"
  let span_context_b = SpanContext::new(trace_id_a, span_id_b, true, "key1=value1,key2=value2,key3=value3")
  
  // Verify trace consistency across services
  assert_eq(SpanContext::trace_id(span_context_a), SpanContext::trace_id(span_context_b))
  assert_not_eq(SpanContext::span_id(span_context_a), SpanContext::span_id(span_context_b))
  assert_true(SpanContext::is_valid(span_context_b))
  assert_true(SpanContext::is_sampled(span_context_b))
  
  // Create grandchild span for service C
  let span_id_c = "span_3333333333333333"
  let span_context_c = SpanContext::new(trace_id_a, span_id_c, false, "key1=value1,key2=value2")
  
  // Verify trace consistency with different sampling
  assert_eq(SpanContext::trace_id(span_context_a), SpanContext::trace_id(span_context_c))
  assert_not_eq(SpanContext::span_id(span_context_b), SpanContext::span_id(span_context_c))
  assert_true(SpanContext::is_valid(span_context_c))
  assert_false(SpanContext::is_sampled(span_context_c)) // Different sampling decision
}

// Test 3: Error Recovery and Resilience
test "error recovery and resilience mechanisms" {
  // Test span lifecycle with error scenarios
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error-recovery-test", Some("1.0.0"))
  
  // Create a span that will encounter errors
  let error_span = Tracer::start_span(tracer, "error-prone-operation")
  assert_eq(Span::name(error_span), "error-prone-operation")
  assert_eq(Span::kind(error_span), Internal)
  assert_true(Span::is_recording(error_span))
  
  // Set error status on span
  Span::set_status(error_span, Error, Some("Operation failed due to network timeout"))
  assert_eq(Span::status(error_span), Error)
  
  // Add error event
  let error_attributes = [
    ("error.type", StringValue("network_timeout")),
    ("error.message", StringValue("Connection timeout after 30 seconds")),
    ("error.retry_count", IntValue(3))
  ]
  Span::add_event(error_span, "error_occurred", Some(error_attributes))
  
  // End the span despite errors
  Span::end(error_span)
  
  // Create recovery span
  let recovery_span = Tracer::start_span(tracer, "recovery-operation")
  Span::set_status(recovery_span, Ok, Some("Operation recovered successfully"))
  Span::end(recovery_span)
  
  // Verify both spans are properly handled
  assert_eq(Span::status(recovery_span), Ok)
}

// Test 4: Resource Merge Strategy
test "resource merge strategy validation" {
  // Create base resource with service attributes
  let base_attrs = [
    ("service.name", StringValue("azimuth-core")),
    ("service.version", StringValue("1.0.0")),
    ("service.namespace", StringValue("production"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Create override resource with deployment attributes
  let override_attrs = [
    ("service.name", StringValue("azimuth-core-v2")), // This should override
    ("deployment.environment", StringValue("staging")),
    ("deployment.region", StringValue("us-west-2"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Merge resources
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Verify merge results - override should win
  match Resource::get_attribute(merged_resource, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "azimuth-core-v2") // Override value
    _ => assert_true(false)
  }
  
  // Verify override attributes are present
  match Resource::get_attribute(merged_resource, "deployment.environment") {
    Some(StringValue(env)) => assert_eq(env, "staging")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "deployment.region") {
    Some(StringValue(region)) => assert_eq(region, "us-west-2")
    _ => assert_true(false)
  }
}

// Test 5: Advanced Timestamp Operations
test "advanced timestamp operations and validation" {
  // Create clock instance
  let clock = Clock::system()
  
  // Get current timestamp
  let timestamp1 = Clock::now_unix_nanos(clock)
  assert_true(timestamp1 > 0L)
  
  // Create log record with timestamp
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Test log message with timestamp"),
    None,
    Some(timestamp1),
    None,
    Some("trace_1234567890abcdef"),
    Some("span_1111111111111111"),
    None
  )
  
  // Verify log record timestamp
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Test log message with timestamp")
    None => assert_true(false)
  }
  assert_eq(LogRecord::trace_id(log_record), Some("trace_1234567890abcdef"))
  assert_eq(LogRecord::span_id(log_record), Some("span_1111111111111111"))
  assert_eq(log_record.timestamp, Some(timestamp1))
  
  // Get another timestamp (should be later)
  let timestamp2 = Clock::now_unix_nanos(clock)
  assert_true(timestamp2 >= timestamp1) // Allow equality in test environment
  
  // Create another log record with different timestamp
  let log_record2 = LogRecord::new_with_context(
    Warn,
    Some("Warning message with later timestamp"),
    None,
    Some(timestamp2),
    None,
    Some("trace_1234567890abcdef"),
    Some("span_2222222222222222"),
    None
  )
  
  // Verify timestamp ordering
  match (log_record.timestamp, log_record2.timestamp) {
    (Some(ts1), Some(ts2)) => assert_true(ts2 >= ts1)
    _ => assert_true(false)
  }
}

// Test 6: Metrics Instrument Creation and Operations
test "metrics instrument creation and operations" {
  // Create meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-metrics", Some("1.0.0"))
  
  // Create counter instrument
  let counter = Meter::create_counter(meter, "request_count", Some("Total number of requests"), Some("requests"))
  assert_eq(counter.name, "request_count")
  assert_eq(counter.description, Some("Total number of requests"))
  assert_eq(counter.unit, Some("requests"))
  
  // Add values to counter
  Counter::add(counter, 1.0, None)
  Counter::add(counter, 5.0, None)
  Counter::add(counter, 10.0, None)
  
  // Create histogram instrument
  let histogram = Meter::create_histogram(meter, "request_duration", Some("Request duration in seconds"), Some("seconds"))
  assert_eq(histogram.name, "request_duration")
  assert_eq(histogram.description, Some("Request duration in seconds"))
  assert_eq(histogram.unit, Some("seconds"))
  
  // Record values to histogram
  Histogram::record(histogram, 0.1, None)
  Histogram::record(histogram, 0.5, None)
  Histogram::record(histogram, 1.2, None)
  
  // Create up-down counter instrument
  let updown_counter = Meter::create_updown_counter(meter, "active_connections", Some("Current active connections"), Some("connections"))
  assert_eq(updown_counter.name, "active_connections")
  assert_eq(updown_counter.description, Some("Current active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  // Add and subtract values
  UpDownCounter::add(updown_counter, 10.0, None) // Add connections
  UpDownCounter::add(updown_counter, -3.0, None) // Remove connections
  
  // Create gauge instrument
  let gauge = Meter::create_gauge(meter, "memory_usage", Some("Current memory usage"), Some("bytes"))
  assert_eq(gauge.name, "memory_usage")
  assert_eq(gauge.description, Some("Current memory usage"))
  assert_eq(gauge.unit, Some("bytes"))
  
  // Test instrument type conversions
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(counter_instrument), "request_count")
  assert_eq(Instrument::description(counter_instrument), Some("Total number of requests"))
  assert_eq(Instrument::unit(counter_instrument), Some("requests"))
}

// Test 7: Context Propagation with Baggage
test "context propagation with baggage operations" {
  // Create root context
  let root_context = Context::root()
  
  // Create context key
  let user_id_key = ContextKey::new("user.id")
  let session_id_key = ContextKey::new("session.id")
  
  // Add values to context
  let context_with_user = Context::with_value(root_context, user_id_key, "user_12345")
  let context_with_session = Context::with_value(context_with_user, session_id_key, "session_abcdef")
  
  // Verify context values
  match Context::get(context_with_session, user_id_key) {
    Some(user_id) => assert_eq(user_id, "user_12345")
    None => assert_true(false)
  }
  
  match Context::get(context_with_session, session_id_key) {
    Some(session_id) => assert_eq(session_id, "session_abcdef")
    None => assert_true(false)
  }
  
  // Create baggage and add entries
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.role", "admin")
  let baggage_with_permissions = Baggage::set_entry(baggage_with_user, "user.permissions", "read,write,execute")
  
  // Verify baggage entries
  match Baggage::get_entry(baggage_with_permissions, "user.role") {
    Some(role) => assert_eq(role, "admin")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage_with_permissions, "user.permissions") {
    Some(permissions) => assert_eq(permissions, "read,write,execute")
    None => assert_true(false)
  }
  
  // Test baggage entry removal
  let baggage_without_permissions = Baggage::remove_entry(baggage_with_permissions, "user.permissions")
  match Baggage::get_entry(baggage_without_permissions, "user.permissions") {
    Some(_) => assert_true(false) // Should be removed
    None => assert_true(true) // Should be None
  }
  
  // Verify other entries still exist
  match Baggage::get_entry(baggage_without_permissions, "user.role") {
    Some(role) => assert_eq(role, "admin")
    None => assert_true(false)
  }
}

// Test 8: HTTP Client Integration with Telemetry
test "http client integration with telemetry" {
  // Create HTTP client
  let http_client = HttpClient::new()
  
  // Create HTTP request
  let request_headers = [
    ("Content-Type", "application/json"),
    ("User-Agent", "azimuth-telemetry/1.0.0"),
    ("X-Trace-ID", "trace_1234567890abcdef")
  ]
  let request = HttpRequest::new("GET", "https://api.example.com/metrics", request_headers, Some("{\"query\": \"cpu_usage\"}"))
  
  // Verify request properties
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/metrics")
  match HttpRequest::body(request) {
    Some(body) => assert_eq(body, "{\"query\": \"cpu_usage\"}")
    None => assert_true(false)
  }
  
  // Create HTTP response
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Request-ID", "req_abcdef123456")
  ]
  let response = HttpResponse::new(200, response_headers, Some("{\"cpu_usage\": 75.5, \"memory_usage\": 60.2}"))
  
  // Verify response properties
  assert_eq(HttpResponse::status_code(response), 200)
  match HttpResponse::body(response) {
    Some(body) => {
      assert_true(body.contains("cpu_usage"))
      assert_true(body.contains("memory_usage"))
      assert_true(body.contains("75.5"))
      assert_true(body.contains("60.2"))
    }
    None => assert_true(false)
  }
  
  // Test telemetry integration with HTTP
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "http-client")
  let http_span = Tracer::start_span(tracer, "HTTP GET /metrics")
  
  // Add HTTP-specific attributes
  let http_attributes = [
    ("http.method", StringValue("GET")),
    ("http.url", StringValue("https://api.example.com/metrics")),
    ("http.status_code", IntValue(200)),
    ("http.user_agent", StringValue("azimuth-telemetry/1.0.0"))
  ]
  Span::add_event(http_span, "http_request_completed", Some(http_attributes))
  
  // Set span status based on HTTP response
  if HttpResponse::status_code(response) >= 200 && HttpResponse::status_code(response) < 300 {
    Span::set_status(http_span, Ok, Some("HTTP request successful"))
  } else {
    Span::set_status(http_span, Error, Some("HTTP request failed"))
  }
  
  Span::end(http_span)
}