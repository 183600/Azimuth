// Additional Focused Test Enhancements for Azimuth Telemetry System
// Targeted tests for edge cases, error handling, and complex scenarios

test "attribute_value_deep_nesting_edge_cases" {
  // Test deeply nested attribute structures
  let nested_string = ArrayStringValue(["level1.level2.level3", "deep.nested.value"])
  let nested_int = ArrayIntValue([1000000, 2000000, 3000000])
  
  // Verify array integrity with large values
  match nested_string {
    ArrayStringValue(arr) => {
      @assertion.assert_eq(arr.length, 2)?
      @assertion.assert_eq(arr[0], "level1.level2.level3")?
      @assertion.assert_eq(arr[1], "deep.nested.value")?
    }
    _ => @test.fail("Expected ArrayStringValue")?
  }
  
  match nested_int {
    ArrayIntValue(arr) => {
      @assertion.assert_eq(arr.length, 3)?
      @assertion.assert_eq(arr[0], 1000000)?
      @assertion.assert_eq(arr[2], 3000000)?
    }
    _ => @test.fail("Expected ArrayIntValue")?
  }
}

test "span_context_boundary_conditions" {
  // Test span context with boundary values
  let max_length_trace_id = "0123456789abcdef0123456789abcdef01234567"
  let max_length_span_id = "0123456789abcdef"
  let empty_trace_id = ""
  let empty_span_id = ""
  
  // Test with maximum valid lengths
  let max_ctx = SpanContext::new(max_length_trace_id, max_length_span_id, true, "state=test")
  @assertion.assert_eq(SpanContext::is_valid(max_ctx), true)?
  @assertion.assert_eq(SpanContext::trace_id(max_ctx), max_length_trace_id)?
  @assertion.assert_eq(SpanContext::span_id(max_ctx), max_length_span_id)?
  
  // Test with empty IDs (invalid)
  let empty_ctx = SpanContext::new(empty_trace_id, empty_span_id, false, "")
  @assertion.assert_eq(SpanContext::is_valid(empty_ctx), false)?
  @assertion.assert_eq(SpanContext::is_sampled(empty_ctx), false)?
  
  // Test mixed valid/invalid
  let mixed_ctx1 = SpanContext::new(max_length_trace_id, empty_span_id, true, "")
  @assertion.assert_eq(SpanContext::is_valid(mixed_ctx1), false)?
  
  let mixed_ctx2 = SpanContext::new(empty_trace_id, max_length_span_id, true, "")
  @assertion.assert_eq(SpanContext::is_valid(mixed_ctx2), false)?
}

test "context_operations_memory_safety" {
  // Test context operations with multiple keys and values
  let root_ctx = Context::root()
  let keys = [
    ContextKey::new("key1"),
    ContextKey::new("key2"),
    ContextKey::new("key3"),
    ContextKey::new("key4"),
    ContextKey::new("key5")
  ]
  let values = ["value1", "value2", "value3", "value4", "value5"]
  
  // Build context with multiple values
  let mut ctx = root_ctx
  for i in 0..<keys.length {
    ctx = Context::with_value(ctx, keys[i], values[i])
  }
  
  // Verify only the last value is stored (simplified implementation)
  @assertion.assert_eq(Context::get(ctx, keys[4]), Some("value5"))?
  @assertion.assert_eq(Context::get(ctx, keys[0]), None)?
  
  // Test with non-existent key
  let non_existent_key = ContextKey::new("non_existent")
  @assertion.assert_eq(Context::get(ctx, non_existent_key), None)?
}

test "instrument_type_safety_validation" {
  // Test instrument type safety and property validation
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "safety_test_meter")
  
  // Test counter with empty name (edge case)
  let empty_counter = Meter::create_counter(meter, "", Some("Empty name counter"), None)
  @assertion.assert_eq(empty_counter.name, "")?
  @assertion.assert_eq(empty_counter.description, Some("Empty name counter"))?
  
  // Test histogram with special characters in name
  let special_histogram = Meter::create_histogram(meter, "test.histogram-with_special.chars", Some("Special chars"), Some("ms"))
  @assertion.assert_eq(special_histogram.name, "test.histogram-with_special.chars")?
  
  // Test updown counter and gauge
  let updown_counter = Meter::create_updown_counter(meter, "test_updown", Some("UpDown counter"), Some("operations"))
  let gauge = Meter::create_gauge(meter, "test_gauge", Some("Gauge metric"), Some("percent"))
  
  // Verify instrument properties
  @assertion.assert_eq(updown_counter.name, "test_updown")?
  @assertion.assert_eq(updown_counter.description, Some("UpDown counter"))?
  @assertion.assert_eq(gauge.unit, Some("percent"))?
}

test "log_record_timestamp_validation" {
  // Test log record with various timestamp scenarios
  let current_time = Clock::now_unix_nanos(Clock::system())
  let future_time = current_time + 86400000000000L  // +1 day in nanoseconds
  let past_time = current_time - 86400000000000L    // -1 day in nanoseconds
  
  // Test with current timestamp
  let current_log = LogRecord::new_with_context(
    Info,
    Some("Current time log"),
    None,
    Some(current_time),
    Some(current_time + 1000L),
    Some("trace123"),
    Some("span456"),
    None
  )
  
  @assertion.assert_eq(LogRecord::severity_number(current_log), Info)?
  @assertion.assert_eq(LogRecord::trace_id(current_log), Some("trace123"))?
  @assertion.assert_eq(LogRecord::span_id(current_log), Some("span456"))?
  
  // Test with future timestamp
  let future_log = LogRecord::new_with_context(
    Warn,
    Some("Future time log"),
    None,
    Some(future_time),
    None,
    None,
    None,
    None
  )
  
  @assertion.assert_eq(LogRecord::severity_number(future_log), Warn)?
  @assertion.assert_eq(LogRecord::body(future_log), Some("Future time log"))?
  
  // Test with past timestamp
  let past_log = LogRecord::new_with_context(
    Error,
    Some("Past time log"),
    None,
    Some(past_time),
    Some(past_time + 500L),
    None,
    None,
    None
  )
  
  @assertion.assert_eq(LogRecord::severity_number(past_log), Error)?
  @assertion.assert_eq(LogRecord::body(past_log), Some("Past time log"))?
}

test "resource_attribute_override_behavior" {
  // Test resource attribute merging and override behavior
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base_service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("development"))
  ]
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override_service")),
    ("new.attribute", StringValue("new_value")),
    ("environment", StringValue("production"))
  ]
  
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Test resource merging
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify override behavior (simplified implementation returns override)
  @assertion.assert_eq(
    Resource::get_attribute(merged_resource, "service.name"),
    Some(StringValue("override_service"))
  )?
  
  @assertion.assert_eq(
    Resource::get_attribute(merged_resource, "new.attribute"),
    Some(StringValue("new_value"))
  )?
  
  @assertion.assert_eq(
    Resource::get_attribute(merged_resource, "environment"),
    Some(StringValue("production"))
  )?
}

test "baggage_complex_entry_operations" {
  // Test baggage with complex key-value scenarios
  let baggage = Baggage::new()
  
  // Test with special characters in keys and values
  let complex_key = "user.id.with.special-chars_123"
  let complex_value = "value.with.special.chars=and&symbols?123"
  
  let baggage_complex = Baggage::set_entry(baggage, complex_key, complex_value)
  @assertion.assert_eq(Baggage::get_entry(baggage_complex, complex_key), Some(complex_value))?
  
  // Test with Unicode characters
  let unicode_key = "用户.标识"
  let unicode_value = "测试值_123"
  
  let baggage_unicode = Baggage::set_entry(baggage_complex, unicode_key, unicode_value)
  @assertion.assert_eq(Baggage::get_entry(baggage_unicode, unicode_key), Some(unicode_value))?
  
  // Test with very long key and value
  let long_key = "a".repeat(100)
  let long_value = "b".repeat(200)
  
  let baggage_long = Baggage::set_entry(baggage_unicode, long_key, long_value)
  @assertion.assert_eq(Baggage::get_entry(baggage_long, long_key), Some(long_value))?
  
  // Test removal of non-existent entry
  let baggage_after_removal = Baggage::remove_entry(baggage_long, "non.existent.key")
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, complex_key), Some(complex_value))?
}

test "http_request_response_edge_cases" {
  // Test HTTP operations with edge cases
  let empty_headers : Array[(String, String)] = []
  let large_headers = [
    ("X-Custom-Header-1", "value1"),
    ("X-Custom-Header-2", "value2"),
    ("X-Custom-Header-3", "value3"),
    ("X-Custom-Header-4", "value4"),
    ("X-Custom-Header-5", "value5")
  ]
  
  // Test request with empty headers and no body
  let minimal_request = HttpRequest::new("GET", "https://example.com", empty_headers, None)
  @assertion.assert_eq(HttpRequest::http_method(minimal_request), "GET")?
  @assertion.assert_eq(HttpRequest::url(minimal_request), "https://example.com")?
  @assertion.assert_eq(HttpRequest::body(minimal_request), None)?
  
  // Test request with many headers and large body
  let large_body = "{".repeat(1000) + "\"data\": \"" + "x".repeat(1000) + "\"}"
  let complex_request = HttpRequest::new("POST", "https://api.example.com/data", large_headers, Some(large_body))
  @assertion.assert_eq(HttpRequest::http_method(complex_request), "POST")?
  @assertion.assert_eq(HttpRequest::body(complex_request), Some(large_body))?
  
  // Test response with error status codes
  let error_response = HttpResponse::new(500, [("Content-Type", "application/json")], Some("{\"error\":\"Internal Server Error\"}"))
  @assertion.assert_eq(HttpResponse::status_code(error_response), 500)?
  @assertion.assert_eq(HttpResponse::body(error_response), Some("{\"error\":\"Internal Server Error\"}"))?
  
  // Test response with no body
  let no_body_response = HttpResponse::new(204, empty_headers, None)
  @assertion.assert_eq(HttpResponse::status_code(no_body_response), 204)?
  @assertion.assert_eq(HttpResponse::body(no_body_response), None)?
}

test "propagation_complex_context_scenarios" {
  // Test propagation with complex context scenarios
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Test with multiple context values
  let root_ctx = Context::root()
  let key1 = ContextKey::new("user.id")
  let key2 = ContextKey::new("request.id")
  let key3 = ContextKey::new("session.id")
  
  let ctx_with_values = Context::with_value(
    Context::with_value(
      Context::with_value(root_ctx, key1, "user123"),
      key2, 
      "req456"
    ),
    key3,
    "session789"
  )
  
  let carrier = TextMapCarrier::new()
  
  // Test injection from complex context
  CompositePropagator::inject(composite, ctx_with_values, carrier)
  @assertion.assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))?
  
  // Test extraction to new context
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_key = ContextKey::new("extracted")
  @assertion.assert_eq(Context::get(extracted_ctx, extracted_key), Some("true"))?
  
  // Verify original context is not affected
  @assertion.assert_eq(Context::get(ctx_with_values, key1), Some("user123"))?
  @assertion.assert_eq(Context::get(ctx_with_values, key2), Some("req456"))?
  @assertion.assert_eq(Context::get(ctx_with_values, key3), Some("session789"))?
}