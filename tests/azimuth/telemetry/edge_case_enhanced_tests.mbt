// Azimuth Telemetry System - Additional Edge Case Tests
// 专注于边界情况、错误处理和高级场景的测试用例

// Test 1: Attributes edge cases with special characters and empty values
test "attributes edge cases with special characters" {
  let attrs = Attributes::new()
  
  // Test with empty key
  Attributes::set(attrs, "", StringValue("empty_key"))
  let empty_key_val = Attributes::get(attrs, "")
  assert_eq(empty_key_val, None)  // Should handle empty key gracefully
  
  // Test with special characters in key
  Attributes::set(attrs, "special.key.with.dots", StringValue("special"))
  Attributes::set(attrs, "key-with-dashes", IntValue(123))
  Attributes::set(attrs, "key_with_underscores", BoolValue(true))
  
  // Test with different attribute value types
  Attributes::set(attrs, "array.string", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([1, 2, 3]))
  Attributes::set(attrs, "float.value", FloatValue(3.14159))
  
  // Verify array operations
  let array_str = Attributes::get(attrs, "array.string")
  let array_int = Attributes::get(attrs, "array.int")
  let float_val = Attributes::get(attrs, "float.value")
  
  assert_eq(array_str, Some(ArrayStringValue(["a", "b", "c"])))
  assert_eq(array_int, Some(ArrayIntValue([1, 2, 3])))
  assert_eq(float_val, Some(FloatValue(3.14159)))
}

// Test 2: Span lifecycle management with status changes
test "span lifecycle management" {
  let span_ctx = SpanContext::new("trace-789", "span-123", true, "state=value")
  let span = Span::new("lifecycle-test", Server, span_ctx)
  
  // Test initial state
  assert_true(Span::is_recording(span))
  assert_eq(Span::status(span), Unset)
  
  // Test status changes
  Span::set_status(span, Ok, "Operation completed")
  assert_eq(Span::status(span), Ok)
  
  // Test event addition
  Span::add_event(span, "event1", [])
  Span::add_event(span, "event2", [("key1", StringValue("value1"))])
  
  // Test span ending
  Span::end(span)
  // After ending, span should no longer be recording (in a real implementation)
  // This is a simplified test - actual behavior depends on implementation
}

// Test 3: Context chain propagation
test "context chain propagation" {
  let root_ctx = Context::root()
  
  // Create a chain of context values
  let key1 = ContextKey::new("request.id")
  let key2 = ContextKey::new("user.id")
  let key3 = ContextKey::new("session.id")
  
  let ctx1 = Context::with_value(root_ctx, key1, "req-123")
  let ctx2 = Context::with_value(ctx1, key2, "user-456")
  let ctx3 = Context::with_value(ctx2, key3, "session-789")
  
  // Verify all values are accessible
  assert_eq(Context::get(ctx3, key1), Some("req-123"))
  assert_eq(Context::get(ctx3, key2), Some("user-456"))
  assert_eq(Context::get(ctx3, key3), Some("session-789"))
  
  // Verify earlier contexts don't have later values
  assert_eq(Context::get(ctx1, key2), None)
  assert_eq(Context::get(ctx2, key3), None)
}

// Test 4: Resource attribute merging strategies
test "resource attribute merging" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("development"))
  ]
  let resource_with_base = Resource::with_attributes(base_resource, base_attrs)
  
  // Create override resource
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),  // This should override
    ("region", StringValue("us-west-2")),              // New attribute
    ("environment", StringValue("production"))         // This should override
  ]
  let resource_with_override = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = Resource::merge(resource_with_base, resource_with_override)
  
  // Verify merging behavior
  let service_name = Resource::get_attribute(merged, "service.name")
  let service_version = Resource::get_attribute(merged, "service.version")
  let environment = Resource::get_attribute(merged, "environment")
  let region = Resource::get_attribute(merged, "region")
  
  assert_eq(service_name, Some(StringValue("override-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))  // Should preserve
  assert_eq(environment, Some(StringValue("production")))
  assert_eq(region, Some(StringValue("us-west-2")))
}

// Test 5: Advanced metrics scenarios with detailed configuration
test "advanced metrics scenarios" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "advanced-meter")
  
  // Create instruments with detailed configuration
  let counter = Meter::create_counter(
    meter, 
    "http.requests.total",
    Some("Total number of HTTP requests"),
    Some("requests")
  )
  
  let histogram = Meter::create_histogram(
    meter,
    "http.request.duration",
    Some("HTTP request duration in seconds"),
    Some("s")
  )
  
  let updown_counter = Meter::create_updown_counter(
    meter,
    "active.connections",
    Some("Number of active connections"),
    Some("connections")
  )
  
  let gauge = Meter::create_gauge(
    meter,
    "memory.usage.bytes",
    Some("Current memory usage in bytes"),
    Some("bytes")
  )
  
  // Verify instrument properties
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, Some("Total number of HTTP requests"))
  assert_eq(counter.unit, Some("requests"))
  
  assert_eq(histogram.name, "http.request.duration")
  assert_eq(histogram.description, Some("HTTP request duration in seconds"))
  assert_eq(histogram.unit, Some("s"))
  
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Number of active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  assert_eq(gauge.name, "memory.usage.bytes")
  assert_eq(gauge.description, Some("Current memory usage in bytes"))
  assert_eq(gauge.unit, Some("bytes"))
  
  // Test metric operations
  Counter::add(counter, 10.0)
  Histogram::record(histogram, 0.150)  // 150ms
  UpDownCounter::add(updown_counter, 5.0)
  
  // Convert to generic instrument and verify
  let histogram_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(histogram_instrument), "http.request.duration")
  assert_eq(Instrument::description(histogram_instrument), Some("HTTP request duration in seconds"))
  assert_eq(Instrument::unit(histogram_instrument), Some("s"))
}

// Test 6: LogRecord with all fields populated
test "comprehensive log record" {
  // Create attributes for log record
  let log_attrs = Attributes::new()
  Attributes::set(log_attrs, "error.code", IntValue(500))
  Attributes::set(log_attrs, "error.type", StringValue("InternalError"))
  
  // Create comprehensive log record
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(log_attrs),
    Some(1735689600000000000L),  // Fixed timestamp
    Some(1735689600000000001L),  // Observed timestamp
    Some("trace-123456789"),
    Some("span-987654321"),
    Some(Context::root())
  )
  
  // Verify all fields
  assert_eq(LogRecord::severity_number(log_record), Error)
  assert_eq(LogRecord::body(log_record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(log_record), Some("trace-123456789"))
  assert_eq(LogRecord::span_id(log_record), Some("span-987654321"))
  
  // Test with different severity levels
  let debug_record = LogRecord::new(Debug, "Debug message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let fatal_record = LogRecord::new(Fatal, "Fatal error")
  
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  assert_eq(LogRecord::body(debug_record), Some("Debug message"))
  assert_eq(LogRecord::body(warn_record), Some("Warning message"))
  assert_eq(LogRecord::body(fatal_record), Some("Fatal error"))
}

// Test 7: HTTP client request/response cycle
test "http client request response cycle" {
  let client = HttpClient::new()
  
  // Create complex request with multiple headers
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"),
    ("X-Request-ID", "req-123456"),
    ("Accept", "application/json"),
    ("User-Agent", "Azimuth-Telemetry/1.0.0")
  ]
  
  let request_body = "{\"operation\": \"test\", \"data\": {\"key\": \"value\"}}"
  let request = HttpRequest::new(
    "POST",
    "https://api.example.com/telemetry",
    headers,
    Some(request_body)
  )
  
  // Verify request properties
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/telemetry")
  assert_eq(HttpRequest::body(request), Some(request_body))
  
  // Create response
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-Time", "150ms"),
    ("Cache-Control", "no-cache")
  ]
  
  let response_body = "{\"status\": \"success\", \"data\": {\"processed\": true}}"
  let response = HttpResponse::new(200, response_headers, Some(response_body))
  
  // Verify response properties
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some(response_body))
}

// Test 8: System utilities integration
test "system utilities integration" {
  // Test clock operations
  let clock = Clock::system()
  let timestamp1 = Clock::now_unix_nanos(clock)
  
  // Verify timestamp is in reasonable range for 2025
  assert_true(timestamp1 > 1700000000000000000L)  // Start of 2023
  assert_true(timestamp1 < 1800000000000000000L)  // End of 2027
  
  // Test random operations
  let random = Random::system()
  
  // Test byte array generation
  let bytes8 = Random::next_bytes(random, 8)
  let bytes16 = Random::next_bytes(random, 16)
  let bytes32 = Random::next_bytes(random, 32)
  
  assert_eq(bytes8.length(), 8)
  assert_eq(bytes16.length(), 16)
  assert_eq(bytes32.length(), 32)
  
  // Test random number generation
  let random_num1 = Random::next_u64(random)
  let random_num2 = Random::next_u64(random)
  
  // Verify numbers are in valid range
  assert_true(random_num1.to_int() >= 0)
  assert_true(random_num2.to_int() >= 0)
  
  // Numbers should be different (though this is probabilistic)
  // In a real test, you might want to run multiple iterations
}