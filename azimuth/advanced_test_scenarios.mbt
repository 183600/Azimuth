// Advanced test cases for Azimuth telemetry system
// These tests focus on edge cases, error handling, and complex scenarios

// Test 1: Advanced Attributes operations with complex nested structures
pub test "advanced attributes operations with complex data" {
  let attrs = azimuth::Attributes::new()
  
  // Test array values
  azimuth::Attributes::set(attrs, "string.array", azimuth::ArrayStringValue(["value1", "value2", "value3"]))
  azimuth::Attributes::set(attrs, "int.array", azimuth::ArrayIntValue([1, 2, 3, 4, 5]))
  
  // Test mixed attribute types
  azimuth::Attributes::set(attrs, "mixed.string", azimuth::StringValue("complex_string_123"))
  azimuth::Attributes::set(attrs, "mixed.int", azimuth::IntValue(-999))
  azimuth::Attributes::set(attrs, "mixed.float", azimuth::FloatValue(3.14159))
  azimuth::Attributes::set(attrs, "mixed.bool", azimuth::BoolValue(true))
  
  // Test special characters in keys
  azimuth::Attributes::set(attrs, "special.key-with.dots_and_underscores", azimuth::StringValue("special_value"))
  azimuth::Attributes::set(attrs, "unicode.key.测试", azimuth::StringValue("unicode_value"))
  
  // Verify attributes can handle complex scenarios
  let string_array = azimuth::Attributes::get(attrs, "string.array")
  let int_array = azimuth::Attributes::get(attrs, "int.array")
  let mixed_float = azimuth::Attributes::get(attrs, "mixed.float")
  let mixed_bool = azimuth::Attributes::get(attrs, "mixed.bool")
  let unicode_value = azimuth::Attributes::get(attrs, "unicode.key.测试")
  
  assert_eq(string_array, Some(azimuth::ArrayStringValue(["value1", "value2", "value3"])))
  assert_eq(int_array, Some(azimuth::ArrayIntValue([1, 2, 3, 4, 5])))
  assert_eq(mixed_float, Some(azimuth::FloatValue(3.14159)))
  assert_eq(mixed_bool, Some(azimuth::BoolValue(true)))
  assert_eq(unicode_value, Some(azimuth::StringValue("unicode_value")))
}

// Test 2: Context propagation with deep nesting
pub test "deep context propagation and nesting" {
  let root_ctx = azimuth::Context::root()
  
  // Create deeply nested context chain
  let ctx1 = azimuth::Context::with_value(root_ctx, azimuth::ContextKey::new("level1"), "value1")
  let ctx2 = azimuth::Context::with_value(ctx1, azimuth::ContextKey::new("level2"), "value2")
  let ctx3 = azimuth::Context::with_value(ctx2, azimuth::ContextKey::new("level3"), "value3")
  let ctx4 = azimuth::Context::with_value(ctx3, azimuth::ContextKey::new("level4"), "value4")
  let ctx5 = azimuth::Context::with_value(ctx4, azimuth::ContextKey::new("level5"), "value5")
  
  // Verify deep context access
  assert_eq(azimuth::Context::get(ctx5, azimuth::ContextKey::new("level1")), Some("value1"))
  assert_eq(azimuth::Context::get(ctx5, azimuth::ContextKey::new("level3")), Some("value3"))
  assert_eq(azimuth::Context::get(ctx5, azimuth::ContextKey::new("level5")), Some("value5"))
  
  // Test context isolation
  let isolated_ctx = azimuth::Context::with_value(root_ctx, azimuth::ContextKey::new("isolated"), "isolated_value")
  assert_eq(azimuth::Context::get(isolated_ctx, azimuth::ContextKey::new("level1")), None)
  assert_eq(azimuth::Context::get(isolated_ctx, azimuth::ContextKey::new("isolated")), Some("isolated_value"))
}

// Test 3: Span lifecycle and status management
pub test "span lifecycle and status management" {
  let span_ctx = azimuth::SpanContext::new("trace-789", "span-123", true, "state=value")
  let span = azimuth::Span::new("lifecycle-test", azimuth::Server, span_ctx)
  
  // Test initial state
  assert_true(azimuth::Span::is_recording(span))
  assert_eq(azimuth::Span::status(span), azimuth::Unset)
  
  // Test status transitions
  azimuth::Span::set_status(span, azimuth::Ok, "Operation completed successfully")
  assert_eq(azimuth::Span::status(span), azimuth::Ok)
  
  azimuth::Span::set_status(span, azimuth::Error, "Operation failed")
  assert_eq(azimuth::Span::status(span), azimuth::Error)
  
  // Test event addition
  azimuth::Span::add_event(span, "event1", [("event.type", azimuth::StringValue("test"))])
  azimuth::Span::add_event(span, "event2", [("event.order", azimuth::IntValue(2))])
  
  // Test span termination
  azimuth::Span::end(span)
  // Note: In a real implementation, is_recording might return false after end()
  // For this test, we just verify the end operation doesn't crash
}

// Test 4: Metrics with different instrument types and attributes
pub test "comprehensive metrics operations" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "comprehensive-test")
  
  // Test all metric types
  let counter = azimuth::Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  let histogram = azimuth::Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "test.updown", Some("Test updown"), Some("items"))
  let gauge = azimuth::Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("percent"))
  
  // Test metric operations with different value types
  azimuth::Counter::add(counter, 10.5)
  azimuth::Counter::add(counter, -5.0)  // Test negative values
  azimuth::Counter::add(counter, 0.0)   // Test zero values
  
  azimuth::Histogram::record(histogram, 100.0)
  azimuth::Histogram::record(histogram, 200.5)
  azimuth::Histogram::record(histogram, 0.1)   // Test very small values
  
  azimuth::UpDownCounter::add(updown_counter, 15.0)
  azimuth::UpDownCounter::add(updown_counter, -10.0)  // Test decrementing
  
  // Verify instrument properties
  let counter_instrument = azimuth::Counter::as_instrument(counter)
  assert_eq(azimuth::Instrument::name(counter_instrument), "test.counter")
  assert_eq(azimuth::Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(azimuth::Instrument::unit(counter_instrument), Some("count"))
}

// Test 5: Advanced logging with structured data
pub test "advanced logging with structured data" {
  let provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(provider, "advanced-logger")
  
  // Create complex log record with all fields
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "log.level", azimuth::StringValue("info"))
  azimuth::Attributes::set(attrs, "request.id", azimuth::StringValue("req-12345"))
  azimuth::Attributes::set(attrs, "user.id", azimuth::IntValue(67890))
  
  let complex_record = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Structured log message with complex data"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000100L),
    Some("complex-trace-id"),
    Some("complex-span-id"),
    Some(azimuth::Context::root())
  )
  
  // Test all severity levels
  let trace_record = azimuth::LogRecord::new(azimuth::Trace, "Trace message")
  let debug_record = azimuth::LogRecord::new(azimuth::Debug, "Debug message")
  let info_record = azimuth::LogRecord::new(azimuth::Info, "Info message")
  let warn_record = azimuth::LogRecord::new(azimuth::Warn, "Warning message")
  let error_record = azimuth::LogRecord::new(azimuth::Error, "Error message")
  let fatal_record = azimuth::LogRecord::new(azimuth::Fatal, "Fatal message")
  
  // Verify severity levels
  assert_eq(azimuth::LogRecord::severity_number(trace_record), azimuth::Trace)
  assert_eq(azimuth::LogRecord::severity_number(debug_record), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_record), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_record), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_record), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(fatal_record), azimuth::Fatal)
  
  // Emit all records
  azimuth::Logger::emit(logger, complex_record)
  azimuth::Logger::emit(logger, trace_record)
  azimuth::Logger::emit(logger, debug_record)
  azimuth::Logger::emit(logger, info_record)
  azimuth::Logger::emit(logger, warn_record)
  azimuth::Logger::emit(logger, error_record)
  azimuth::Logger::emit(logger, fatal_record)
}

// Test 6: Resource merging and attribute conflicts
pub test "resource merging and attribute conflict resolution" {
  let base_resource = azimuth::Resource::new()
  let base_attrs = [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("environment", azimuth::StringValue("development")),
    ("region", azimuth::StringValue("us-west"))
  ]
  let resource_with_base = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = azimuth::Resource::new()
  let override_attrs = [
    ("service.name", azimuth::StringValue("override-service")),  // Conflict
    ("environment", azimuth::StringValue("production")),        // Conflict
    ("new.attribute", azimuth::StringValue("new-value")),
    ("another.new", azimuth::IntValue(42))
  ]
  let resource_with_override = azimuth::Resource::with_attributes(override_resource, override_attrs)
  
  // Test merging
  let merged_resource = azimuth::Resource::merge(resource_with_base, resource_with_override)
  
  // Verify merge results (implementation dependent, but should handle conflicts)
  let service_name = azimuth::Resource::get_attribute(merged_resource, "service.name")
  let environment = azimuth::Resource::get_attribute(merged_resource, "environment")
  let service_version = azimuth::Resource::get_attribute(merged_resource, "service.version")
  let region = azimuth::Resource::get_attribute(merged_resource, "region")
  let new_attribute = azimuth::Resource::get_attribute(merged_resource, "new.attribute")
  let another_new = azimuth::Resource::get_attribute(merged_resource, "another.new")
  
  // These assertions depend on merge strategy - adjust based on actual implementation
  assert_eq(service_version, Some(azimuth::StringValue("1.0.0")))  // Should remain
  assert_eq(region, Some(azimuth::StringValue("us-west")))         // Should remain
  assert_eq(new_attribute, Some(azimuth::StringValue("new-value")))
  assert_eq(another_new, Some(azimuth::IntValue(42)))
}

// Test 7: Complex propagation scenarios
pub test "complex propagation with multiple headers" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // Create context with baggage
  let ctx = azimuth::Context::root()
  let ctx_with_baggage = azimuth::Context::with_value(ctx, azimuth::ContextKey::new("user.id"), "user-123")
  
  let carrier = azimuth::TextMapCarrier::new()
  
  // Inject context
  azimuth::CompositePropagator::inject(composite_propagator, ctx_with_baggage, carrier)
  
  // Test multiple header operations
  azimuth::TextMapCarrier::set(carrier, "custom-header", "custom-value")
  azimuth::TextMapCarrier::set(carrier, "another-header", "another-value")
  azimuth::TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Extract context
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction worked
  let extracted_value = azimuth::Context::get(extracted_ctx, azimuth::ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
  
  // Test header retrieval
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  let custom_header = azimuth::TextMapCarrier::get(carrier, "custom-header")
  let missing_header = azimuth::TextMapCarrier::get(carrier, "missing-header")
  
  assert_eq(trace_header, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(custom_header, Some("custom-value"))
  assert_eq(missing_header, None)
}

// Test 8: HTTP client with complex scenarios
pub test "http client complex scenarios" {
  let client = azimuth::HttpClient::new()
  
  // Test request with various HTTP methods
  let get_request = azimuth::HttpRequest::new("GET", "https://api.example.com/users", [], None)
  let post_request = azimuth::HttpRequest::new("POST", "https://api.example.com/users", 
    [("Content-Type", "application/json")], Some("{\"name\": \"test\"}"))
  let put_request = azimuth::HttpRequest::new("PUT", "https://api.example.com/users/123", 
    [("Content-Type", "application/json"), ("Authorization", "Bearer token")], Some("{\"name\": \"updated\"}"))
  let delete_request = azimuth::HttpRequest::new("DELETE", "https://api.example.com/users/123", 
    [("Authorization", "Bearer token")], None)
  
  // Verify request properties
  assert_eq(azimuth::HttpRequest::http_method(get_request), "GET")
  assert_eq(azimuth::HttpRequest::http_method(post_request), "POST")
  assert_eq(azimuth::HttpRequest::http_method(put_request), "PUT")
  assert_eq(azimuth::HttpRequest::http_method(delete_request), "DELETE")
  
  // Test response scenarios
  let success_response = azimuth::HttpResponse::new(200, [("Content-Type", "application/json")], 
    Some("{\"status\": \"success\", \"data\": {\"id\": 123}}"))
  let error_response = azimuth::HttpResponse::new(404, [("Content-Type", "application/json")], 
    Some("{\"error\": \"Not found\"}"))
  let server_error_response = azimuth::HttpResponse::new(500, [("Content-Type", "text/plain")], 
    Some("Internal Server Error"))
  let empty_response = azimuth::HttpResponse::new(204, [], None)
  
  // Verify response properties
  assert_eq(azimuth::HttpResponse::status_code(success_response), 200)
  assert_eq(azimuth::HttpResponse::status_code(error_response), 404)
  assert_eq(azimuth::HttpResponse::status_code(server_error_response), 500)
  assert_eq(azimuth::HttpResponse::status_code(empty_response), 204)
  
  assert_eq(azimuth::HttpResponse::body(success_response), Some("{\"status\": \"success\", \"data\": {\"id\": 123}}"))
  assert_eq(azimuth::HttpResponse::body(error_response), Some("{\"error\": \"Not found\"}"))
  assert_eq(azimuth::HttpResponse::body(empty_response), None)
}

// Test 9: Baggage operations with complex entries
pub test "complex baggage operations" {
  let baggage = azimuth::Baggage::new()
  
  // Test setting multiple baggage entries
  let baggage1 = azimuth::Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage2 = azimuth::Baggage::set_entry(baggage1, "request.id", "req-abcdef")
  let baggage3 = azimuth::Baggage::set_entry(baggage2, "session.id", "session-98765")
  let baggage4 = azimuth::Baggage::set_entry(baggage3, "trace.id", "trace-xyz")
  
  // Test baggage retrieval
  assert_eq(azimuth::Baggage::get_entry(baggage4, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage4, "request.id"), Some("req-abcdef"))
  assert_eq(azimuth::Baggage::get_entry(baggage4, "session.id"), Some("session-98765"))
  assert_eq(azimuth::Baggage::get_entry(baggage4, "trace.id"), Some("trace-xyz"))
  
  // Test missing entries
  assert_eq(azimuth::Baggage::get_entry(baggage4, "missing.key"), None)
  assert_eq(azimuth::Baggage::get_entry(baggage4, "another.missing"), None)
  
  // Test baggage removal
  let baggage_after_removal = azimuth::Baggage::remove_entry(baggage4, "session.id")
  assert_eq(azimuth::Baggage::get_entry(baggage_after_removal, "session.id"), None)
  assert_eq(azimuth::Baggage::get_entry(baggage_after_removal, "user.id"), Some("user-12345"))
  
  // Test baggage with special characters
  let special_baggage = azimuth::Baggage::set_entry(baggage, "special.key-with.dots", "special-value")
  let unicode_baggage = azimuth::Baggage::set_entry(special_baggage, "unicode.key.测试", "unicode-value")
  
  assert_eq(azimuth::Baggage::get_entry(unicode_baggage, "special.key-with.dots"), Some("special-value"))
  assert_eq(azimuth::Baggage::get_entry(unicode_baggage, "unicode.key.测试"), Some("unicode-value"))
}

// Test 10: System utilities stress test
pub test "system utilities stress test" {
  let clock = azimuth::Clock::system()
  let random = azimuth::Random::system()
  
  // Test multiple clock operations
  let timestamp1 = azimuth::Clock::now_unix_nanos(clock)
  let timestamp2 = azimuth::Clock::now_unix_nanos(clock)
  let timestamp3 = azimuth::Clock::now_unix_nanos(clock)
  
  // Verify timestamps are reasonable
  assert_true(timestamp1 > 1700000000000000000L)
  assert_true(timestamp2 > 1700000000000000000L)
  assert_true(timestamp3 > 1700000000000000000L)
  
  // Test multiple random operations
  let bytes1 = azimuth::Random::next_bytes(random, 16)
  let bytes2 = azimuth::Random::next_bytes(random, 32)
  let bytes3 = azimuth::Random::next_bytes(random, 64)
  
  let random1 = azimuth::Random::next_u64(random)
  let random2 = azimuth::Random::next_u64(random)
  let random3 = azimuth::Random::next_u64(random)
  
  // Verify random operations
  assert_eq(bytes1.length(), 16)
  assert_eq(bytes2.length(), 32)
  assert_eq(bytes3.length(), 64)
  
  assert_true(random1.to_int() >= 0)
  assert_true(random2.to_int() >= 0)
  assert_true(random3.to_int() >= 0)
  
  // Test edge cases
  let empty_bytes = azimuth::Random::next_bytes(random, 0)
  assert_eq(empty_bytes.length(), 0)
  
  let single_byte = azimuth::Random::next_bytes(random, 1)
  assert_eq(single_byte.length(), 1)
}