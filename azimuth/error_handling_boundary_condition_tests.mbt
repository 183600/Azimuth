// Error Handling and Boundary Condition Tests for Azimuth Telemetry System
// This file contains comprehensive test cases for error handling and boundary conditions

test "span context boundary conditions" {
  // Test span context with empty trace ID and span ID
  let empty_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(empty_ctx))
  assert_false(SpanContext::is_sampled(empty_ctx))
  
  // Test span context with invalid trace ID but valid span ID
  let invalid_trace_ctx = SpanContext::new("", "valid-span-id", true, "")
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  
  // Test span context with valid trace ID but invalid span ID
  let invalid_span_ctx = SpanContext::new("valid-trace-id", "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // Test span context with very long trace ID and span ID
  let long_trace_id = "a".repeat(1000)  // Very long trace ID
  let long_span_id = "b".repeat(1000)   // Very long span ID
  let long_ctx = SpanContext::new(long_trace_id, long_span_id, true, "")
  assert_eq(SpanContext::trace_id(long_ctx), long_trace_id)
  assert_eq(SpanContext::span_id(long_ctx), long_span_id)
  
  // Test span context with special characters
  let special_ctx = SpanContext::new("trace!@#$%", "span^&*()", true, "key=value!@#$")
  assert_eq(SpanContext::trace_id(special_ctx), "trace!@#$%")
  assert_eq(SpanContext::span_id(special_ctx), "span^&*()")
}

test "attributes error handling" {
  let attrs = Attributes::new()
  
  // Test setting attributes with empty keys
  Attributes::set(attrs, "", StringValue("empty key test"))
  let empty_key_result = Attributes::get(attrs, "")
  
  // Test setting attributes with very long keys
  let long_key = "k".repeat(10000)  // Very long key
  Attributes::set(attrs, long_key, StringValue("long key test"))
  let long_key_result = Attributes::get(attrs, long_key)
  
  // Test setting attributes with special character keys
  Attributes::set(attrs, "key!@#$%^&*()", StringValue("special chars"))
  let special_chars_result = Attributes::get(attrs, "key!@#$%^&*()")
  
  // Test setting attributes with Unicode keys
  Attributes::set(attrs, "键测试", StringValue("Unicode key test"))
  let unicode_result = Attributes::get(attrs, "键测试")
  
  // Test getting non-existent attributes
  let non_existent = Attributes::get(attrs, "this.key.does.not.exist")
  assert_eq(non_existent, None)
  
  // Test overwriting with different types
  Attributes::set(attrs, "type.change", StringValue("string"))
  Attributes::set(attrs, "type.change", IntValue(42))
  Attributes::set(attrs, "type.change", BoolValue(true))
  let type_change_result = Attributes::get(attrs, "type.change")
}

test "baggage error conditions" {
  let baggage = Baggage::new()
  
  // Test baggage with empty keys and values
  let baggage1 = Baggage::set_entry(baggage, "", "")
  assert_eq(Baggage::get_entry(baggage1, ""), Some(""))
  
  // Test baggage with very long keys and values
  let long_key = "k".repeat(1000)
  let long_value = "v".repeat(1000)
  let baggage2 = Baggage::set_entry(baggage1, long_key, long_value)
  assert_eq(Baggage::get_entry(baggage2, long_key), Some(long_value))
  
  // Test baggage with special characters
  let baggage3 = Baggage::set_entry(baggage2, "special!@#$%", "value!@#$%^&*()")
  assert_eq(Baggage::get_entry(baggage3, "special!@#$%"), Some("value!@#$%^&*()"))
  
  // Test baggage with Unicode
  let baggage4 = Baggage::set_entry(baggage3, "用户键", "用户值")
  assert_eq(Baggage::get_entry(baggage4, "用户键"), Some("用户值"))
  
  // Test baggage removal of non-existent entry
  let baggage5 = Baggage::remove_entry(baggage4, "non.existent.key")
  
  // Test baggage with null-like values
  let baggage6 = Baggage::set_entry(baggage5, "null.value", "")
  let baggage7 = Baggage::set_entry(baggage6, "whitespace.value", "   ")
  assert_eq(Baggage::get_entry(baggage7, "null.value"), Some(""))
  assert_eq(Baggage::get_entry(baggage7, "whitespace.value"), Some("   "))
}

test "context error scenarios" {
  let ctx = Context::root()
  
  // Test context with empty key
  let empty_key = ContextKey::new("")
  let ctx_with_empty = Context::with_value(ctx, empty_key, "empty key value")
  let empty_retrieved = Context::get(ctx_with_empty, empty_key)
  
  // Test context with very long key
  let long_key_str = "k".repeat(1000)
  let long_key = ContextKey::new(long_key_str)
  let ctx_with_long = Context::with_value(ctx_with_empty, long_key, "long key value")
  let long_retrieved = Context::get(ctx_with_long, long_key)
  
  // Test context with special character key
  let special_key = ContextKey::new("key!@#$%^&*()")
  let ctx_with_special = Context::with_value(ctx_with_long, special_key, "special value")
  let special_retrieved = Context::get(ctx_with_special, special_key)
  
  // Test getting from non-existent key
  let non_existent_key = ContextKey::new("non.existent.key")
  let non_existent_value = Context::get(ctx_with_special, non_existent_key)
  assert_eq(non_existent_value, None)
  
  // Test context value overwriting
  let overwrite_key = ContextKey::new("overwrite.test")
  let ctx1 = Context::with_value(ctx, overwrite_key, "original")
  let ctx2 = Context::with_value(ctx1, overwrite_key, "overwritten")
  let final_value = Context::get(ctx2, overwrite_key)
  assert_eq(final_value, Some("overwritten"))
}

test "log record error handling" {
  // Test log record with all None values
  let empty_record = LogRecord::new_with_context(
    Info,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::severity_number(empty_record), Info)
  assert_eq(LogRecord::body(empty_record), None)
  
  // Test log record with very long body
  let very_long_body = "x".repeat(100000)  // Very long message
  let long_record = LogRecord::new(Error, very_long_body)
  assert_eq(LogRecord::body(long_record), Some(very_long_body))
  
  // Test log record with special characters in body
  let special_body = "Special chars: !@#$%^&*()_+-={}[]|\\:;\"'<>?,./ Unicode: 测试 Emoji: 🚀🌟"
  let special_record = LogRecord::new(Warn, special_body)
  assert_eq(LogRecord::body(special_record), Some(special_body))
  
  // Test log record with invalid timestamps
  let negative_timestamp_record = LogRecord::new_with_context(
    Debug,
    Some("Negative timestamp"),
    None,
    Some(-1000L),
    None,
    None,
    None,
    None
  )
  
  let zero_timestamp_record = LogRecord::new_with_context(
    Debug,
    Some("Zero timestamp"),
    None,
    Some(0L),
    None,
    None,
    None,
    None
  )
  
  // Test log record with invalid trace/span IDs
  let invalid_ids_record = LogRecord::new_with_context(
    Fatal,
    Some("Invalid IDs"),
    None,
    None,
    None,
    Some(""),
    Some(""),
    None
  )
  assert_eq(LogRecord::trace_id(invalid_ids_record), Some(""))
  assert_eq(LogRecord::span_id(invalid_ids_record), Some(""))
}

test "resource error conditions" {
  let resource = Resource::new()
  
  // Test resource with empty attribute names
  let empty_attrs = [("", StringValue("empty name"))]
  let resource_empty = Resource::with_attributes(resource, empty_attrs)
  
  // Test resource with very long attribute names and values
  let very_long_name = "n".repeat(1000)
  let very_long_value = "v".repeat(1000)
  let long_attrs = [(very_long_name, StringValue(very_long_value))]
  let resource_long = Resource::with_attributes(resource_empty, long_attrs)
  
  // Test resource with special character attributes
  let special_attrs = [
    ("special!@#$%", StringValue("special!@#$%")),
    ("unicode.键", StringValue("unicode值")),
    ("emoji.🚀", StringValue("rocket🚀"))
  ]
  let resource_special = Resource::with_attributes(resource_long, special_attrs)
  
  // Test getting non-existent attributes
  let missing = Resource::get_attribute(resource_special, "completely.missing.attribute")
  assert_eq(missing, None)
  
  // Test resource merging with empty resources
  let empty_resource1 = Resource::new()
  let empty_resource2 = Resource::new()
  let merged_empty = Resource::merge(empty_resource1, empty_resource2)
  
  // Test resource merging with conflicting types
  let resource1 = Resource::with_attributes(Resource::new(), [("conflict", StringValue("string"))])
  let resource2 = Resource::with_attributes(Resource::new(), [("conflict", IntValue(42))])
  let merged_conflict = Resource::merge(resource1, resource2)
}

test "propagator error scenarios" {
  let composite = CompositePropagator::new([])
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test inject/extract with empty propagator list
  CompositePropagator::inject(composite, ctx, carrier)
  let extracted_empty = CompositePropagator::extract(composite, carrier)
  
  // Test with malformed carrier headers
  TextMapCarrier::set(carrier, "traceparent", "malformed-header-format")
  TextMapCarrier::set(carrier, "baggage", "invalid=baggage=format")
  
  let malformed_extracted = CompositePropagator::extract(
    CompositePropagator::new([W3CTraceContextPropagator::new()]),
    carrier
  )
  
  // Test with empty carrier
  let empty_carrier = TextMapCarrier::new()
  let from_empty = CompositePropagator::extract(
    CompositePropagator::new([W3CTraceContextPropagator::new()]),
    empty_carrier
  )
  
  // Test with very long header values
  let very_long_traceparent = "00-" + "a".repeat(100) + "-" + "b".repeat(100) + "-01"
  TextMapCarrier::set(carrier, "traceparent", very_long_traceparent)
  let long_header_extracted = CompositePropagator::extract(
    CompositePropagator::new([W3CTraceContextPropagator::new()]),
    carrier
  )
}

test "metrics error handling" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  // Test creating instruments with empty names
  let empty_counter = Meter::create_counter(meter, "")
  let empty_histogram = Meter::create_histogram(meter, "")
  let empty_updown = Meter::create_updown_counter(meter, "")
  let empty_gauge = Meter::create_gauge(meter, "")
  
  // Test creating instruments with very long names
  let very_long_name = "n".repeat(1000)
  let long_counter = Meter::create_counter(meter, very_long_name)
  let long_histogram = Meter::create_histogram(meter, very_long_name)
  let long_updown = Meter::create_updown_counter(meter, very_long_name)
  let long_gauge = Meter::create_gauge(meter, very_long_name)
  
  // Test creating instruments with special characters
  let special_counter = Meter::create_counter(meter, "counter!@#$%^&*()")
  let special_histogram = Meter::create_histogram(meter, "histogram!@#$%^&*()")
  let special_updown = Meter::create_updown_counter(meter, "updown!@#$%^&*()")
  let special_gauge = Meter::create_gauge(meter, "gauge!@#$%^&*()")
  
  // Test instrument operations with edge case values
  let normal_counter = Meter::create_counter(meter, "normal.counter")
  Counter::add(normal_counter, 0.0)  // Zero value
  Counter::add(normal_counter, -1.0)  // Negative value
  Counter::add(normal_counter, 999999.999999)  // Large decimal value
  
  let normal_histogram = Meter::create_histogram(meter, "normal.histogram")
  Histogram::record(normal_histogram, 0.0)  // Zero value
  Histogram::record(normal_histogram, -1.0)  // Negative value
  Histogram::record(normal_histogram, 999999.999999)  // Large decimal value
}

test "http client error conditions" {
  // Test HTTP request with empty method and URL
  let empty_request = HttpRequest::new("", "", [])
  assert_eq(HttpRequest::http_method(empty_request), "")
  assert_eq(HttpRequest::url(empty_request), "")
  
  // Test HTTP request with very long method and URL
  let very_long_method = "GET".repeat(100)
  let very_long_url = "https://example.com/" + "path".repeat(1000)
  let long_request = HttpRequest::new(very_long_method, very_long_url, [])
  
  // Test HTTP request with special characters
  let special_request = HttpRequest::new(
    "!@#$%",
    "https://example.com/!@#$%^&*()_+-={}[]|\\:;\"'<>?,./",
    [("header!@#$%", "value!@#$%")]
  )
  
  // Test HTTP response with edge case status codes
  let min_response = HttpResponse::new(0, [])
  let max_response = HttpResponse::new(999, [])
  let negative_response = HttpResponse::new(-1, [])
  
  // Test HTTP response with very long headers and body
  let very_long_body = "x".repeat(100000)
  let long_body_response = HttpResponse::new(200, [], Some(very_long_body))
  assert_eq(HttpResponse::body(long_body_response), Some(very_long_body))
}

test "clock and random error scenarios" {
  let clock = Clock::system()
  let random = Random::system()
  
  // Test clock operations (should handle gracefully)
  let timestamp = Clock::now_unix_nanos(clock)
  assert_true(timestamp >= 0L)
  
  // Test random operations with edge cases
  let empty_bytes = Random::next_bytes(random, 0)
  assert_eq(length(empty_bytes), 0)
  
  let negative_bytes = Random::next_bytes(random, -1)
  // Should handle negative length gracefully
  
  let large_bytes = Random::next_bytes(random, 1000000)
  // Should handle large requests gracefully
  
  let random_u64 = Random::next_u64(random)
  assert_true(random_u64 >= 0UL)
  
  // Multiple calls should not crash
  for i in range(0, 10) {
    let ts = Clock::now_unix_nanos(clock)
    let bytes = Random::next_bytes(random, 100)
    let u64 = Random::next_u64(random)
    assert_true(ts >= 0L)
    assert_true(u64 >= 0UL)
  }
}

test "concurrent operations stress test" {
  // Test creating multiple resources concurrently (simulated)
  let resources = []
  for i in range(0, 100) {
    let resource = Resource::new()
    let attrs = [("test.id", IntValue(i))]
    let resource_with_attrs = Resource::with_attributes(resource, attrs)
    resources.push(resource_with_attrs)
  }
  assert_eq(length(resources), 100)
  
  // Test creating multiple spans concurrently (simulated)
  let spans = []
  for i in range(0, 100) {
    let ctx = SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, "")
    let span = Span::new("operation-" + i.to_string(), Internal, ctx)
    spans.push(span)
  }
  assert_eq(length(spans), 100)
  
  // Test creating multiple log records concurrently (simulated)
  let records = []
  for i in range(0, 100) {
    let record = LogRecord::new(Info, "Log message " + i.to_string())
    records.push(record)
  }
  assert_eq(length(records), 100)
}