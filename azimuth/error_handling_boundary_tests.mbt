// Error Handling and Boundary Conditions Test Suite for Azimuth Telemetry System
// Tests edge cases, error scenarios, and boundary conditions across all components

test "span context with invalid values" {
  // Test with empty trace ID
  let empty_trace_ctx = SpanContext::new("", "span-123", true, "")
  assert_false(SpanContext::is_valid(empty_trace_ctx))
  assert_eq(SpanContext::trace_id(empty_trace_ctx), "")
  
  // Test with empty span ID
  let empty_span_ctx = SpanContext::new("trace-123", "", true, "")
  assert_false(SpanContext::is_valid(empty_span_ctx))
  assert_eq(SpanContext::span_id(empty_span_ctx), "")
  
  // Test with both empty
  let both_empty_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(both_empty_ctx))
  
  // Test with valid values
  let valid_ctx = SpanContext::new("trace-123", "span-456", true, "key=value")
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
}

test "attributes with boundary values" {
  let attrs = Attributes::new()
  
  // Test with maximum integer values
  Attributes::set(attrs, "max.int", IntValue(9223372036854775807))
  let max_int = Attributes::get(attrs, "max.int")
  assert_eq(max_int, Some(IntValue(9223372036854775807)))
  
  // Test with minimum integer values
  Attributes::set(attrs, "min.int", IntValue(-9223372036854775808))
  let min_int = Attributes::get(attrs, "min.int")
  assert_eq(min_int, Some(IntValue(-9223372036854775808)))
  
  // Test with very large float values
  Attributes::set(attrs, "max.float", FloatValue(1.7976931348623157e+308))
  let max_float = Attributes::get(attrs, "max.float")
  assert_eq(max_float, Some(FloatValue(1.7976931348623157e+308)))
  
  // Test with very small float values
  Attributes::set(attrs, "min.float", FloatValue(2.2250738585072014e-308))
  let min_float = Attributes::get(attrs, "min.float")
  assert_eq(min_float, Some(FloatValue(2.2250738585072014e-308)))
  
  // Test with infinity
  Attributes::set(attrs, "infinity", FloatValue(1.0/0.0))
  let infinity = Attributes::get(attrs, "infinity")
  assert_eq(infinity, Some(FloatValue(1.0/0.0)))
}

test "context operations with null and empty values" {
  let ctx = Context::root()
  
  // Test with empty string key
  let empty_key = ContextKey::new("")
  let ctx_with_empty = Context::with_value(ctx, empty_key, "empty.key.value")
  let empty_value = Context::get(ctx_with_empty, empty_key)
  assert_eq(empty_value, Some("empty.key.value"))
  
  // Test with empty string value
  let normal_key = ContextKey::new("normal.key")
  let ctx_with_empty_value = Context::with_value(ctx, normal_key, "")
  let empty_value_result = Context::get(ctx_with_empty_value, normal_key)
  assert_eq(empty_value_result, Some(""))
  
  // Test with very long key
  let long_key = ContextKey::new("a".repeat(1000))
  let ctx_with_long = Context::with_value(ctx, long_key, "long.key.value")
  let long_value = Context::get(ctx_with_long, long_key)
  assert_eq(long_value, Some("long.key.value"))
  
  // Test with very long value
  let normal_key2 = ContextKey::new("normal.key2")
  let long_value_str = "x".repeat(10000)
  let ctx_with_long_value = Context::with_value(ctx, normal_key2, long_value_str)
  let long_value_result = Context::get(ctx_with_long_value, normal_key2)
  assert_eq(long_value_result, Some(long_value_str))
}

test "baggage operations with boundary conditions" {
  let baggage = Baggage::new()
  
  // Test with empty key
  let empty_key_baggage = Baggage::set_entry(baggage, "", "empty.key.value")
  let empty_key_value = Baggage::get_entry(empty_key_baggage, "")
  assert_eq(empty_key_value, Some("empty.key.value"))
  
  // Test with empty value
  let empty_value_baggage = Baggage::set_entry(baggage, "empty.value.key", "")
  let empty_value_result = Baggage::get_entry(empty_value_baggage, "empty.value.key")
  assert_eq(empty_value_result, Some(""))
  
  // Test with very long key
  let long_key = "a".repeat(500)
  let long_key_baggage = Baggage::set_entry(baggage, long_key, "long.key.value")
  let long_key_value = Baggage::get_entry(long_key_baggage, long_key)
  assert_eq(long_key_value, Some("long.key.value"))
  
  // Test with very long value
  let long_value = "x".repeat(5000)
  let long_value_baggage = Baggage::set_entry(baggage, "long.value.key", long_value)
  let long_value_result = Baggage::get_entry(long_value_baggage, "long.value.key")
  assert_eq(long_value_result, Some(long_value))
  
  // Test with special characters
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_baggage = Baggage::set_entry(baggage, "special.chars.key", special_chars)
  let special_result = Baggage::get_entry(special_baggage, "special.chars.key")
  assert_eq(special_result, Some(special_chars))
}

test "text map carrier with boundary conditions" {
  let carrier = TextMapCarrier::new()
  
  // Test with empty key
  TextMapCarrier::set(carrier, "", "empty.key.value")
  let empty_key_value = TextMapCarrier::get(carrier, "")
  assert_eq(empty_key_value, Some("empty.key.value"))
  
  // Test with empty value
  TextMapCarrier::set(carrier, "empty.value.key", "")
  let empty_value_result = TextMapCarrier::get(carrier, "empty.value.key")
  assert_eq(empty_value_result, Some(""))
  
  // Test with very long key
  let long_key = "a".repeat(1000)
  TextMapCarrier::set(carrier, long_key, "long.key.value")
  let long_key_value = TextMapCarrier::get(carrier, long_key)
  assert_eq(long_key_value, Some("long.key.value"))
  
  // Test with very long value
  let long_value = "x".repeat(10000)
  TextMapCarrier::set(carrier, "long.value.key", long_value)
  let long_value_result = TextMapCarrier::get(carrier, "long.value.key")
  assert_eq(long_value_result, Some(long_value))
  
  // Test with special characters in key
  let special_key = "special.key.with.dots-and_underscores-and-hyphens"
  TextMapCarrier::set(carrier, special_key, "special.key.value")
  let special_key_value = TextMapCarrier::get(carrier, special_key)
  assert_eq(special_key_value, Some("special.key.value"))
}

test "http operations with invalid data" {
  // Test with empty URL
  let empty_url_headers = [("Content-Type", "application/json")]
  let empty_url_request = HttpRequest::new("GET", "", empty_url_headers)
  assert_eq(HttpRequest::url(empty_url_request), "")
  
  // Test with empty HTTP method
  let empty_method_request = HttpRequest::new("", "https://example.com", empty_url_headers)
  assert_eq(HttpRequest::http_method(empty_method_request), "")
  
  // Test with invalid status codes
  let negative_status = HttpResponse::new(-1, [])
  assert_eq(HttpResponse::status_code(negative_status), -1)
  
  let very_large_status = HttpResponse::new(999999, [])
  assert_eq(HttpResponse::status_code(very_large_status), 999999)
  
  // Test with empty headers
  let no_headers_request = HttpRequest::new("GET", "https://example.com", [])
  assert_eq(HttpRequest::http_method(no_headers_request), "GET")
  assert_eq(HttpRequest::url(no_headers_request), "https://example.com")
}

test "metrics operations with extreme values" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "extreme-metrics-test")
  
  // Test counter with extreme values
  let counter = Meter::create_counter(meter, "extreme.counter")
  
  // Add very large values
  Counter::add(counter, 1.7976931348623157e+308)  // Max double
  Counter::add(counter, -1.7976931348623157e+308) // Min double
  
  // Add very small values
  Counter::add(counter, 2.2250738585072014e-308)  // Min positive double
  
  // Test histogram with extreme values
  let histogram = Meter::create_histogram(meter, "extreme.histogram")
  
  // Record extreme values
  Histogram::record(histogram, 1.7976931348623157e+308)  // Max double
  Histogram::record(histogram, -1.7976931348623157e+308) // Min double
  Histogram::record(histogram, 2.2250738585072014e-308)  // Min positive double
  Histogram::record(histogram, 0.0)  // Zero
  
  // Test updown counter with extreme values
  let updown = Meter::create_updown_counter(meter, "extreme.updown")
  
  UpDownCounter::add(updown, 1.7976931348623157e+308)   // Max double
  UpDownCounter::add(updown, -1.7976931348623157e+308)  // Min double
  UpDownCounter::add(updown, 2.2250738585072014e-308)   // Min positive double
  
  // Verify instruments are still functional
  assert_eq(counter.name, "extreme.counter")
  assert_eq(histogram.name, "extreme.histogram")
  assert_eq(updown.name, "extreme.updown")
}

test "timestamp boundary conditions" {
  let clock = Clock::system()
  
  // Get current timestamp
  let current = Clock::now_unix_nanos(clock)
  assert_true(current > 0L)
  
  // Test with minimum timestamp
  let min_timestamp = -9223372036854775808L  // Min Int64
  let record_with_min = LogRecord::new_with_context(
    Info,
    Some("Min timestamp"),
    None,
    Some(min_timestamp),
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::timestamp(record_with_min), Some(min_timestamp))
  
  // Test with maximum timestamp
  let max_timestamp = 9223372036854775807L  // Max Int64
  let record_with_max = LogRecord::new_with_context(
    Info,
    Some("Max timestamp"),
    None,
    Some(max_timestamp),
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::timestamp(record_with_max), Some(max_timestamp))
  
  // Test with zero timestamp
  let record_with_zero = LogRecord::new_with_context(
    Info,
    Some("Zero timestamp"),
    None,
    Some(0L),
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::timestamp(record_with_zero), Some(0L))
}

test "resource operations with boundary conditions" {
  let resource = Resource::new()
  
  // Test with empty attribute name
  let empty_name_attrs = [("", StringValue("empty.name.value"))]
  let with_empty_name = Resource::with_attributes(resource, empty_name_attrs)
  let empty_name_value = Resource::get_attribute(with_empty_name, "")
  assert_eq(empty_name_value, Some(StringValue("empty.name.value")))
  
  // Test with very long attribute name
  let long_name = "a".repeat(1000)
  let long_name_attrs = [(long_name, StringValue("long.name.value"))]
  let with_long_name = Resource::with_attributes(resource, long_name_attrs)
  let long_name_value = Resource::get_attribute(with_long_name, long_name)
  assert_eq(long_name_value, Some(StringValue("long.name.value")))
  
  // Test with empty string value
  let empty_value_attrs = [("empty.value", StringValue(""))]
  let with_empty_value = Resource::with_attributes(resource, empty_value_attrs)
  let empty_value_result = Resource::get_attribute(with_empty_value, "empty.value")
  assert_eq(empty_value_result, Some(StringValue("")))
  
  // Test with very long string value
  let long_value = "x".repeat(10000)
  let long_value_attrs = [("long.value", StringValue(long_value))]
  let with_long_value = Resource::with_attributes(resource, long_value_attrs)
  let long_value_result = Resource::get_attribute(with_long_value, "long.value")
  assert_eq(long_value_result, Some(StringValue(long_value)))
}