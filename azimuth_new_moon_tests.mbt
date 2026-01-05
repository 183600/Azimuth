// Azimuth New MoonBit Test Suite
// This file contains new test cases for the Azimuth telemetry system

// Test 1: Attribute value type conversions and edge cases
test "attribute value type conversions" {
  // Test string attribute value
  let string_attr = StringValue("test string")
  assert_eq(string_attr, StringValue("test string"))
  
  // Test integer attribute value
  let int_attr = IntValue(42)
  assert_eq(int_attr, IntValue(42))
  
  // Test float attribute value
  let float_attr = FloatValue(3.14159)
  assert_eq(float_attr, FloatValue(3.14159))
  
  // Test boolean attribute value
  let bool_attr = BoolValue(true)
  assert_eq(bool_attr, BoolValue(true))
  
  // Test array string attribute value
  let string_array_attr = ArrayStringValue(["a", "b", "c"])
  assert_eq(string_array_attr, ArrayStringValue(["a", "b", "c"]))
  
  // Test array int attribute value
  let int_array_attr = ArrayIntValue([1, 2, 3])
  assert_eq(int_array_attr, ArrayIntValue([1, 2, 3]))
  
  // Test empty arrays
  let empty_string_array = ArrayStringValue([])
  assert_eq(empty_string_array, ArrayStringValue([]))
  
  let empty_int_array = ArrayIntValue([])
  assert_eq(empty_int_array, ArrayIntValue([]))
}

// Test 2: Span status and event handling
test "span status and event handling" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "key1=value1")
  let span = Span::new("test-span", Server, span_ctx)
  
  // Test initial span state
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  assert_eq(Span::span_context(span), span_ctx)
  
  // Test span status operations
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)
  
  Span::set_status(span, Error, Some("Operation failed"))
  assert_eq(Span::status(span), Error)
  
  // Test span event operations
  Span::add_event(span, "event1", Some([("key1", StringValue("value1")), ("key2", IntValue(42))]))
  Span::add_event(span, "event2", None)
  
  // Test span ending
  Span::end(span)
  // After ending, span should still be accessible but might not be recording
  assert_eq(Span::name(span), "test-span")
}

// Test 3: Tracer and instrumentation scope
test "tracer and instrumentation scope" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer", Some("1.0.0"))
  let scope = Tracer::instrumentation_scope(tracer)
  
  // Test instrumentation scope
  assert_eq(scope.name, "test-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)
  
  // Test tracer with different configurations
  let tracer_no_version = TracerProvider::get_tracer(provider, "no-version-tracer")
  let scope_no_version = Tracer::instrumentation_scope(tracer_no_version)
  assert_eq(scope_no_version.name, "no-version-tracer")
  assert_eq(scope_no_version.version, None)
  
  // Test span creation with tracer
  let span = Tracer::start_span(tracer, "tracer-created-span", Some([("attr1", StringValue("value1"))]))
  assert_eq(Span::name(span), "tracer-created-span")
  assert_eq(Span::kind(span), Internal)
}

// Test 4: Advanced baggage operations
test "advanced baggage operations" {
  let empty_baggage = Baggage::new()
  
  // Test baggage with multiple entries
  let baggage1 = Baggage::set_entry(empty_baggage, "user.id", "12345")
  let baggage2 = Baggage::set_entry(baggage1, "request.id", "req-67890")
  let baggage3 = Baggage::set_entry(baggage2, "session.id", "session-abc")
  
  // Test retrieval of all entries
  assert_eq(Baggage::get_entry(baggage3, "user.id"), Some("12345"))
  assert_eq(Baggage::get_entry(baggage3, "request.id"), Some("req-67890"))
  assert_eq(Baggage::get_entry(baggage3, "session.id"), Some("session-abc"))
  assert_eq(Baggage::get_entry(baggage3, "nonexistent"), None)
  
  // Test baggage removal
  let baggage_after_removal = Baggage::remove_entry(baggage3, "request.id")
  assert_eq(Baggage::get_entry(baggage_after_removal, "user.id"), Some("12345"))
  assert_eq(Baggage::get_entry(baggage_after_removal, "request.id"), None)
  assert_eq(Baggage::get_entry(baggage_after_removal, "session.id"), Some("session-abc"))
  
  // Test baggage with special characters
  let special_baggage = Baggage::set_entry(empty_baggage, "special.key", "special=value&with=chars")
  assert_eq(Baggage::get_entry(special_baggage, "special.key"), Some("special=value&with=chars"))
}

// Test 5: Logger provider and operations
test "logger provider and operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger", Some("2.0.0"))
  let scope = logger.scope
  
  // Test logger scope
  assert_eq(scope.name, "test-logger")
  assert_eq(scope.version, Some("2.0.0"))
  assert_eq(scope.schema_url, None)
  
  // Test logger with different configurations
  let logger_minimal = LoggerProvider::get_logger(provider, "minimal-logger")
  assert_eq(logger_minimal.scope.name, "minimal-logger")
  assert_eq(logger_minimal.scope.version, None)
  
  // Test log record emission
  let info_log = LogRecord::new(Info, "Information message")
  let error_log = LogRecord::new(Error, "Error message")
  
  Logger::emit(logger, info_log)
  Logger::emit(logger, error_log)
  
  // Test log record with full context
  let detailed_log = LogRecord::new_with_context(
    Warn,
    Some("Warning with context"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(Context::root())
  )
  
  Logger::emit(logger, detailed_log)
  assert_eq(LogRecord::severity_number(detailed_log), Warn)
  assert_eq(LogRecord::body(detailed_log), Some("Warning with context"))
}

// Test 6: Complex resource attribute operations
test "complex resource attribute operations" {
  let base_resource = Resource::new()
  
  // Test resource with various attribute types
  let resource_with_attrs = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("complex-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-98765")),
    ("deployment.environment", StringValue("staging")),
    ("host.name", StringValue("staging-host")),
    ("process.pid", IntValue(54321)),
    ("process.start.time", StringValue("2025-01-01T00:00:00Z")),
    ("telemetry.auto.enabled", BoolValue(true)),
    ("feature.flags", ArrayStringValue(["feature1", "feature2", "feature3"])),
    ("server.ports", ArrayIntValue([8080, 8443, 9090]))
  ])
  
  // Test retrieval of different attribute types
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.name"), Some(StringValue("complex-service")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "process.pid"), Some(IntValue(54321)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "telemetry.auto.enabled"), Some(BoolValue(true)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "feature.flags"), Some(ArrayStringValue(["feature1", "feature2", "feature3"])))
  assert_eq(Resource::get_attribute(resource_with_attrs, "server.ports"), Some(ArrayIntValue([8080, 8443, 9090])))
  
  // Test nonexistent attribute
  assert_eq(Resource::get_attribute(resource_with_attrs, "nonexistent.attribute"), None)
  
  // Test resource merging with complex scenarios
  let override_resource = Resource::with_attributes(base_resource, [
    ("service.version", StringValue("2.0.0")),  // Override existing
    ("deployment.environment", StringValue("production")),  // Override existing
    ("new.attribute", StringValue("new-value")),  // New attribute
    ("process.pid", IntValue(99999))  // Override existing with different type
  ])
  
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("complex-service")))
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(merged_resource, "deployment.environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(merged_resource, "new.attribute"), Some(StringValue("new-value")))
  assert_eq(Resource::get_attribute(merged_resource, "process.pid"), Some(IntValue(99999)))
}

// Test 7: Context propagation with nested values
test "context propagation with nested values" {
  let root_ctx = Context::root()
  
  // Create nested context with multiple values
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  let key3 = ContextKey::new("key3")
  
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  let ctx3 = Context::with_value(ctx2, key3, "value3")
  
  // Test that all values are accessible
  assert_eq(Context::get(ctx3, key1), Some("value1"))
  assert_eq(Context::get(ctx3, key2), Some("value2"))
  assert_eq(Context::get(ctx3, key3), Some("value3"))
  
  // Test that earlier contexts still have their values
  assert_eq(Context::get(ctx2, key1), Some("value1"))
  assert_eq(Context::get(ctx2, key2), Some("value2"))
  assert_eq(Context::get(ctx2, key3), None)
  
  // Test context with special keys
  let special_key = ContextKey::new("special.key.with.dots_and_numbers_123")
  let ctx_special = Context::with_value(root_ctx, special_key, "special.value")
  assert_eq(Context::get(ctx_special, special_key), Some("special.value"))
  
  // Test context with empty key
  let empty_key = ContextKey::new("")
  let ctx_empty = Context::with_value(root_ctx, empty_key, "empty.key.value")
  assert_eq(Context::get(ctx_empty, empty_key), Some("empty.key.value"))
}

// Test 8: Advanced metrics with different instrument types
test "advanced metrics with different instrument types" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "advanced-metrics")
  
  // Test counter with various operations
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  Counter::add(request_counter, 1.0)
  Counter::add(request_counter, 5.0)
  Counter::add(request_counter, 10.0)
  
  // Test histogram with various operations
  let response_histogram = Meter::create_histogram(meter, "http.response.duration", Some("HTTP response duration"), Some("ms"))
  Histogram::record(response_histogram, 100.0)
  Histogram::record(response_histogram, 250.0)
  Histogram::record(response_histogram, 500.0)
  
  // Test up-down counter with various operations
  let active_connections = Meter::create_updown_counter(meter, "http.active_connections", Some("Active HTTP connections"), Some("connections"))
  UpDownCounter::add(active_connections, 1.0)  // Connection opened
  UpDownCounter::add(active_connections, 1.0)  // Another connection opened
  UpDownCounter::add(active_connections, -1.0)  // Connection closed
  
  // Test gauge with various operations
  let memory_usage = Meter::create_gauge(meter, "process.memory.usage", Some("Process memory usage"), Some("bytes"))
  // In a real implementation, gauge values would be set directly
  // For testing purposes, we just verify the gauge creation
  
  // Test instrument properties
  let counter_instrument = Counter(request_counter.name, request_counter.description, request_counter.unit)
  assert_eq(Instrument::name(counter_instrument), "http.requests.total")
  assert_eq(Instrument::description(counter_instrument), Some("Total HTTP requests"))
  assert_eq(Instrument::unit(counter_instrument), Some("requests"))
  
  // Test metrics with edge case names
  let empty_name_metric = Meter::create_counter(meter, "", Some("Empty name metric"), None)
  let very_long_name_metric = Meter::create_counter(meter, "this.is.a.very.long.metric.name.that.tests.edge.conditions.in.the.system", Some("Very long name"), None)
  let special_chars_metric = Meter::create_counter(meter, "metric.with-special_chars.and.numbers_123", Some("Special chars"), None)
  
  assert_eq(empty_name_metric.name, "")
  assert_eq(very_long_name_metric.name, "this.is.a.very.long.metric.name.that.tests.edge.conditions.in.the.system")
  assert_eq(special_chars_metric.name, "metric.with-special_chars.and.numbers_123")
}

// Test 9: Span context validation and edge cases
test "span context validation and edge cases" {
  // Test valid span context
  let valid_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key1=value1,key2=value2")
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
  assert_eq(SpanContext::trace_id(valid_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(valid_ctx), "b7ad6b7169203331")
  
  // Test invalid span contexts
  let empty_trace_id = SpanContext::new("", "b7ad6b7169203331", true, "")
  let empty_span_id = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", true, "")
  let both_empty = SpanContext::new("", "", false, "")
  
  assert_false(SpanContext::is_valid(empty_trace_id))
  assert_false(SpanContext::is_valid(empty_span_id))
  assert_false(SpanContext::is_valid(both_empty))
  
  // Test sampling scenarios
  let sampled_ctx = SpanContext::new("trace123", "span123", true, "")
  let not_sampled_ctx = SpanContext::new("trace123", "span123", false, "")
  
  assert_true(SpanContext::is_sampled(sampled_ctx))
  assert_false(SpanContext::is_sampled(not_sampled_ctx))
  
  // Test trace state handling
  let ctx_with_trace_state = SpanContext::new("trace123", "span123", true, "key1=value1,key2=value2,key3=value3")
  assert_eq(SpanContext::trace_state(ctx_with_trace_state), "key1=value1,key2=value2,key3=value3")
  
  let ctx_with_empty_trace_state = SpanContext::new("trace123", "span123", true, "")
  assert_eq(SpanContext::trace_state(ctx_with_empty_trace_state), "")
  
  // Test span context with special characters
  let special_ctx = SpanContext::new("trace-with-special-chars_123", "span-with-special-chars_456", true, "special.key=special.value&with=chars")
  assert_true(SpanContext::is_valid(special_ctx))
  assert_eq(SpanContext::trace_id(special_ctx), "trace-with-special-chars_123")
  assert_eq(SpanContext::span_id(special_ctx), "span-with-special-chars_456")
}

// Test 10: HTTP client and response handling
test "http client and response handling" {
  let client = HttpClient::new()
  
  // Test HTTP request with complex headers
  let complex_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Custom-Header", "custom-value"),
    ("X-Request-ID", "req-12345"),
    ("Accept", "application/json,text/plain")
  ]
  
  let request = HttpRequest::new(
    "POST",
    "https://api.example.com/v1/resource",
    complex_headers,
    Some("{\"name\": \"test\", \"value\": 123}")
  )
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/v1/resource")
  assert_eq(HttpRequest::body(request), Some("{\"name\": \"test\", \"value\": 123}"))
  
  // Test HTTP response with complex scenarios
  let success_response = HttpResponse::new(
    200,
    [("Content-Type", "application/json"), ("X-Response-ID", "resp-67890")],
    Some("{\"status\": \"success\", \"data\": {\"id\": 123, \"name\": \"test\"}}")
  )
  
  let error_response = HttpResponse::new(
    404,
    [("Content-Type", "application/json"), ("X-Error-Code", "NOT_FOUND")],
    Some("{\"error\": \"Resource not found\", \"code\": 404}")
  )
  
  let empty_response = HttpResponse::new(204, [], None)
  
  // Verify response properties
  assert_eq(HttpResponse::status_code(success_response), 200)
  assert_eq(HttpResponse::body(success_response), Some("{\"status\": \"success\", \"data\": {\"id\": 123, \"name\": \"test\"}}"))
  
  assert_eq(HttpResponse::status_code(error_response), 404)
  assert_eq(HttpResponse::body(error_response), Some("{\"error\": \"Resource not found\", \"code\": 404}"))
  
  assert_eq(HttpResponse::status_code(empty_response), 204)
  assert_eq(HttpResponse::body(empty_response), None)
  
  // Test edge case scenarios
  let request_without_body = HttpRequest::new("GET", "https://api.example.com/health", [], None)
  assert_eq(HttpRequest::body(request_without_body), None)
  
  let response_with_empty_body = HttpResponse::new(200, [("Content-Type", "application/json")], Some(""))
  assert_eq(HttpResponse::body(response_with_empty_body), Some(""))
}