// Azimuth Telemetry System - Specialized Comprehensive Test Suite
// This file contains specialized test cases covering advanced telemetry functionality

// Test 1: Attribute Value Type Conversion and Validation
test "attribute value type conversion and validation" {
  // Test string attribute value
  let string_attr = AttributeValue::StringValue("test_string")
  match string_attr {
    StringValue(value) => assert_eq(value, "test_string")
    _ => assert_true(false)
  }
  
  // Test integer attribute value
  let int_attr = AttributeValue::IntValue(42)
  match int_attr {
    IntValue(value) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  // Test float attribute value
  let float_attr = AttributeValue::FloatValue(3.14159)
  match float_attr {
    FloatValue(value) => assert_eq(value, 3.14159)
    _ => assert_true(false)
  }
  
  // Test boolean attribute value
  let bool_attr = AttributeValue::BoolValue(true)
  match bool_attr {
    BoolValue(value) => assert_true(value)
    _ => assert_true(false)
  }
  
  // Test array string attribute value
  let array_string_attr = AttributeValue::ArrayStringValue(["item1", "item2", "item3"])
  match array_string_attr {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "item1")
      assert_eq(values[1], "item2")
      assert_eq(values[2], "item3")
    }
    _ => assert_true(false)
  }
  
  // Test array integer attribute value
  let array_int_attr = AttributeValue::ArrayIntValue([1, 2, 3, 4, 5])
  match array_int_attr {
    ArrayIntValue(values) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], 1)
      assert_eq(values[4], 5)
    }
    _ => assert_true(false)
  }
}

// Test 2: Span Kind and Status Code Boundary Tests
test "span kind and status code boundary tests" {
  // Test all span kinds
  let internal_span = Span::new("internal", Internal, SpanContext::new("trace1", "span1", true, ""))
  let server_span = Span::new("server", Server, SpanContext::new("trace2", "span2", true, ""))
  let client_span = Span::new("client", Client, SpanContext::new("trace3", "span3", true, ""))
  let producer_span = Span::new("producer", Producer, SpanContext::new("trace4", "span4", true, ""))
  let consumer_span = Span::new("consumer", Consumer, SpanContext::new("trace5", "span5", true, ""))
  
  // Verify span kinds
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
  
  // Test all status codes
  Span::set_status(internal_span, Unset)
  assert_eq(Span::status(internal_span), Unset)
  
  Span::set_status(server_span, Ok, Some("Operation successful"))
  assert_eq(Span::status(server_span), Ok)
  
  Span::set_status(client_span, Error, Some("Connection failed"))
  assert_eq(Span::status(client_span), Error)
  
  // Test span with empty name
  let empty_name_span = Span::new("", Internal, SpanContext::new("trace6", "span6", true, ""))
  assert_eq(Span::name(empty_name_span), "")
}

// Test 3: Log Severity Number and Timestamp Validation
test "log severity number and timestamp validation" {
  // Test all severity numbers
  let trace_record = LogRecord::new(Trace, "Trace message")
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  
  let debug_record = LogRecord::new(Debug, "Debug message")
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  
  let info_record = LogRecord::new(Info, "Info message")
  assert_eq(LogRecord::severity_number(info_record), Info)
  
  let warn_record = LogRecord::new(Warn, "Warning message")
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  
  let error_record = LogRecord::new(Error, "Error message")
  assert_eq(LogRecord::severity_number(error_record), Error)
  
  let fatal_record = LogRecord::new(Fatal, "Fatal error message")
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Test log record with timestamps
  let current_time = Clock::now_unix_nanos(Clock::system())
  let record_with_timestamp = LogRecord::new_with_context(
    Info,
    Some("Message with timestamp"),
    None,
    Some(current_time),
    Some(current_time + 1000L),
    Some("trace123"),
    Some("span456"),
    None
  )
  
  assert_eq(LogRecord::severity_number(record_with_timestamp), Info)
  assert_eq(LogRecord::trace_id(record_with_timestamp), Some("trace123"))
  assert_eq(LogRecord::span_id(record_with_timestamp), Some("span456"))
}

// Test 4: Context Operations with Complex Keys
test "context operations with complex keys" {
  // Test context with special characters in keys
  let ctx = Context::root()
  
  let key_with_dots = ContextKey::new("key.with.dots")
  let ctx_with_dots = Context::with_value(ctx, key_with_dots, "value_with_dots")
  assert_eq(Context::get(ctx_with_dots, key_with_dots), Some("value_with_dots"))
  
  let key_with_underscores = ContextKey::new("key_with_underscores")
  let ctx_with_underscores = Context::with_value(ctx, key_with_underscores, "value_with_underscores")
  assert_eq(Context::get(ctx_with_underscores, key_with_underscores), Some("value_with_underscores"))
  
  let key_with_hyphens = ContextKey::new("key-with-hyphens")
  let ctx_with_hyphens = Context::with_value(ctx, key_with_hyphens, "value_with_hyphens")
  assert_eq(Context::get(ctx_with_hyphens, key_with_hyphens), Some("value_with_hyphens"))
  
  // Test context chaining
  let chained_ctx = ctx
    |> Context::with_value(ContextKey::new("first"), "first_value")
    |> Context::with_value(ContextKey::new("second"), "second_value")
    |> Context::with_value(ContextKey::new("third"), "third_value")
  
  assert_eq(Context::get(chained_ctx, ContextKey::new("first")), Some("first_value"))
  assert_eq(Context::get(chained_ctx, ContextKey::new("second")), Some("second_value"))
  assert_eq(Context::get(chained_ctx, ContextKey::new("third")), Some("third_value"))
}

// Test 5: Text Map Carrier Operations with Multiple Headers
test "text map carrier operations with multiple headers" {
  // Create carrier
  let carrier = TextMapCarrier::new()
  
  // Set multiple headers
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  TextMapCarrier::set(carrier, "baggage", "userId=alice,serverNode=af0e7b9c")
  TextMapCarrier::set(carrier, "x-correlation-id", "correlation-12345")
  TextMapCarrier::set(carrier, "x-request-id", "request-67890")
  
  // Verify headers
  match TextMapCarrier::get(carrier, "traceparent") {
    Some(value) => assert_eq(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    None => assert_true(false)
  }
  
  match TextMapCarrier::get(carrier, "tracestate") {
    Some(value) => assert_eq(value, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
    None => assert_true(false)
  }
  
  match TextMapCarrier::get(carrier, "baggage") {
    Some(value) => assert_eq(value, "userId=alice,serverNode=af0e7b9c")
    None => assert_true(false)
  }
  
  match TextMapCarrier::get(carrier, "x-correlation-id") {
    Some(value) => assert_eq(value, "correlation-12345")
    None => assert_true(false)
  }
  
  match TextMapCarrier::get(carrier, "x-request-id") {
    Some(value) => assert_eq(value, "request-67890")
    None => assert_true(false)
  }
  
  // Test non-existent header
  match TextMapCarrier::get(carrier, "non-existent-header") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 6: Instrumentation Scope with Schema URL
test "instrumentation scope with schema url" {
  // Test instrumentation scope with all fields
  let scope_with_all = InstrumentationScope::{
    name: "test.instrument",
    version: Some("1.2.3"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  assert_eq(scope_with_all.name, "test.instrument")
  match scope_with_all.version {
    Some(version) => assert_eq(version, "1.2.3")
    None => assert_true(false)
  }
  match scope_with_all.schema_url {
    Some(schema_url) => assert_eq(schema_url, "https://example.com/schema/v1")
    None => assert_true(false)
  }
  
  // Test instrumentation scope with only name
  let scope_name_only = InstrumentationScope::{
    name: "minimal.instrument",
    version: None,
    schema_url: None
  }
  
  assert_eq(scope_name_only.name, "minimal.instrument")
  assert_eq(scope_name_only.version, None)
  assert_eq(scope_name_only.schema_url, None)
  
  // Test tracer with instrumentation scope
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "full.scope.tracer", Some("2.0.0"))
  let scope = Tracer::instrumentation_scope(tracer)
  
  assert_eq(scope.name, "full.scope.tracer")
  match scope.version {
    Some(version) => assert_eq(version, "2.0.0")
    None => assert_true(false)
  }
}

// Test 7: Resource with Complex Attributes
test "resource with complex attributes" {
  // Create resource with complex attributes
  let complex_attrs = [
    ("service.name", AttributeValue::StringValue("complex-service")),
    ("service.instance.id", AttributeValue::StringValue("instance-12345")),
    ("service.version", AttributeValue::StringValue("3.2.1")),
    ("deployment.environment", AttributeValue::StringValue("staging")),
    ("host.name", AttributeValue::StringValue("host-abc-123")),
    ("host.id", AttributeValue::StringValue("host-id-67890")),
    ("process.pid", AttributeValue::IntValue(9876)),
    ("process.executable.name", AttributeValue::StringValue("azimuth-service")),
    ("process.command_args", AttributeValue::ArrayStringValue(["--port", "8080", "--config", "/etc/config.yaml"])),
    ("telemetry.sdk.name", AttributeValue::StringValue("azimuth")),
    ("telemetry.sdk.language", AttributeValue::StringValue("moonbit")),
    ("telemetry.sdk.version", AttributeValue::StringValue("0.1.0"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), complex_attrs)
  
  // Test attribute retrieval
  match Resource::get_attribute(resource, "service.name") {
    Some(AttributeValue::StringValue(name)) => assert_eq(name, "complex-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource, "process.pid") {
    Some(AttributeValue::IntValue(pid)) => assert_eq(pid, 9876)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource, "process.command_args") {
    Some(AttributeValue::ArrayStringValue(args)) => {
      assert_eq(args.length(), 4)
      assert_eq(args[0], "--port")
      assert_eq(args[1], "8080")
      assert_eq(args[2], "--config")
      assert_eq(args[3], "/etc/config.yaml")
    }
    _ => assert_true(false)
  }
  
  // Test resource merge with complex attributes
  let override_attrs = [
    ("service.version", AttributeValue::StringValue("4.0.0")),
    ("deployment.environment", AttributeValue::StringValue("production")),
    ("new.attribute", AttributeValue::StringValue("new-value"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource, override_resource)
  
  // Verify merged attributes
  match Resource::get_attribute(merged_resource, "service.version") {
    Some(AttributeValue::StringValue(version)) => assert_eq(version, "4.0.0")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "deployment.environment") {
    Some(AttributeValue::StringValue(env)) => assert_eq(env, "production")
    _ => assert_true(false)
  }
}

// Test 8: Clock and Random Operations
test "clock and random operations" {
  // Test system clock
  let clock = Clock::system()
  let timestamp1 = Clock::now_unix_nanos(clock)
  let timestamp2 = Clock::now_unix_nanos(clock)
  
  // Verify timestamps are reasonable
  assert_true(timestamp1 > 0L)
  assert_true(timestamp2 > 0L)
  
  // Test system random
  let random = Random::system()
  
  // Test random bytes generation
  let bytes1 = Random::next_bytes(random, 8)
  let bytes2 = Random::next_bytes(random, 16)
  
  assert_eq(bytes1.length(), 8)
  assert_eq(bytes2.length(), 16)
  
  // Test random u64 generation
  let random_value1 = Random::next_u64(random)
  let random_value2 = Random::next_u64(random)
  
  assert_true(random_value1 > 0UL)
  assert_true(random_value2 > 0UL)
}

// Test 9: HTTP Client with Complex Request/Response
test "http client with complex request response" {
  // Create HTTP client
  let client = HttpClient::new()
  
  // Create complex HTTP request
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"),
    ("X-Request-ID", "req-12345"),
    ("X-Correlation-ID", "corr-67890"),
    ("Accept", "application/json"),
    ("User-Agent", "Azimuth-Telemetry/0.1.0")
  ]
  
  let body = "{\"operation\": \"telemetry_export\", \"data\": {\"metrics\": 42, \"traces\": 100}}"
  let request = HttpRequest::new("POST", "https://telemetry.example.com/api/v1/export", headers, Some(body))
  
  // Verify request properties
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://telemetry.example.com/api/v1/export")
  match HttpRequest::body(request) {
    Some(req_body) => assert_eq(req_body, "{\"operation\": \"telemetry_export\", \"data\": {\"metrics\": 42, \"traces\": 100}}")
    None => assert_true(false)
  }
  
  // Create complex HTTP response
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-Time", "150ms"),
    ("X-Request-ID", "req-12345"),
    ("X-Correlation-ID", "corr-67890"),
    ("Cache-Control", "no-cache, no-store, must-revalidate")
  ]
  
  let response_body = "{\"status\": \"success\", \"exported\": {\"metrics\": 42, \"traces\": 100}, \"timestamp\": \"2025-01-02T12:00:00Z\"}"
  let response = HttpResponse::new(200, response_headers, Some(response_body))
  
  // Verify response properties
  assert_eq(HttpResponse::status_code(response), 200)
  match HttpResponse::body(response) {
    Some(resp_body) => assert_eq(resp_body, "{\"status\": \"success\", \"exported\": {\"metrics\": 42, \"traces\": 100}, \"timestamp\": \"2025-01-02T12:00:00Z\"}")
    None => assert_true(false)
  }
}

// Test 10: Baggage with Complex Entry Values
test "baggage with complex entry values" {
  // Create baggage
  let baggage = Baggage::new()
  
  // Set complex baggage entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session-abcdef-123456")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "req-7890-1234-5678")
  let baggage4 = Baggage::set_entry(baggage3, "trace.state", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  let baggage5 = Baggage::set_entry(baggage4, "custom.context", "{\"key1\": \"value1\", \"key2\": 42}")
  
  // Verify baggage entries
  match Baggage::get_entry(baggage5, "user.id") {
    Some(value) => assert_eq(value, "user-12345")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage5, "session.id") {
    Some(value) => assert_eq(value, "session-abcdef-123456")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage5, "request.id") {
    Some(value) => assert_eq(value, "req-7890-1234-5678")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage5, "trace.state") {
    Some(value) => assert_eq(value, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage5, "custom.context") {
    Some(value) => assert_eq(value, "{\"key1\": \"value1\", \"key2\": 42}")
    None => assert_true(false)
  }
  
  // Remove entries and verify
  let baggage6 = Baggage::remove_entry(baggage5, "session.id")
  let baggage7 = Baggage::remove_entry(baggage6, "trace.state")
  
  match Baggage::get_entry(baggage7, "session.id") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  match Baggage::get_entry(baggage7, "trace.state") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Verify other entries still exist
  match Baggage::get_entry(baggage7, "user.id") {
    Some(value) => assert_eq(value, "user-12345")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage7, "custom.context") {
    Some(value) => assert_eq(value, "{\"key1\": \"value1\", \"key2\": 42}")
    None => assert_true(false)
  }
}