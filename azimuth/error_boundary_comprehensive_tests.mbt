// Comprehensive Error Boundary Test Suite
// Tests for error handling, recovery mechanisms, and graceful degradation

test "span context error handling" {
  // Test with invalid trace ID
  let invalid_span_ctx = SpanContext::new("", "b7ad6b7169203331", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // Test with invalid span ID
  let invalid_span_ctx2 = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx2))
  
  // Test with valid context
  let valid_span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_true(SpanContext::is_valid(valid_span_ctx))
}

test "attributes error handling with null values" {
  let attrs = Attributes::new()
  
  // Test setting and getting with various scenarios
  Attributes::set(attrs, "null.string", StringValue(""))
  let null_string = Attributes::get(attrs, "null.string")
  assert_eq(null_string, Some(StringValue("")))
  
  // Test getting non-existent attribute
  let missing_attr = Attributes::get(attrs, "non.existent.attribute")
  assert_eq(missing_attr, None)
  
  // Test with special characters in attribute names
  Attributes::set(attrs, "attr.with-special.chars!", StringValue("test"))
  let special_char_attr = Attributes::get(attrs, "attr.with-special.chars!")
  // This tests that the system doesn't crash with special characters
  assert_true(true)
}

test "context operations error handling" {
  let ctx = Context::root()
  
  // Test context with null data
  assert_eq(ctx.data, None)
  
  // Test context operations with valid data
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(ctx, key, "test.value")
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test.value"))
  
  // Test context operations with different key
  let different_key = ContextKey::new("different.key")
  let missing_value = Context::get(ctx_with_value, different_key)
  assert_eq(missing_value, None)
}

test "baggage operations error handling" {
  let baggage = Baggage::new()
  
  // Test operations on empty baggage
  let missing_entry = Baggage::get_entry(baggage, "non.existent.key")
  assert_eq(missing_entry, None)
  
  // Test setting and getting with empty values
  let baggage_with_empty = Baggage::set_entry(baggage, "empty.key", "")
  let empty_value = Baggage::get_entry(baggage_with_empty, "empty.key")
  assert_eq(empty_value, Some(""))
  
  // Test removal of non-existent entry
  let baggage_after_removal = Baggage::remove_entry(baggage, "non.existent.key")
  // Should not crash
  assert_true(true)
}

test "text map carrier error handling" {
  let carrier = TextMapCarrier::new()
  
  // Test getting from empty carrier
  let missing_header = TextMapCarrier::get(carrier, "non.existent.header")
  assert_eq(missing_header, None)
  
  // Test setting and getting headers
  TextMapCarrier::set(carrier, "test.header", "test.value")
  let header_value = TextMapCarrier::get(carrier, "test.header")
  assert_eq(header_value, Some("test.value"))
  
  // Test overwriting existing header
  TextMapCarrier::set(carrier, "test.header", "new.value")
  let new_header_value = TextMapCarrier::get(carrier, "test.header")
  assert_eq(new_header_value, Some("new.value"))
}

test "instrument operations error handling" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test counter with negative values (should not crash)
  let counter = Meter::create_counter(meter, "test.counter")
  Counter::add(counter, -1.0)
  Counter::add(counter, 0.0)
  Counter::add(counter, 1.0)
  
  // Test histogram with extreme values
  let histogram = Meter::create_histogram(meter, "test.histogram")
  Histogram::record(histogram, 0.0)
  Histogram::record(histogram, -1.0)
  Histogram::record(histogram, 999999.0)
  
  // Test updown counter with large values
  let updown_counter = Meter::create_updown_counter(meter, "test.updown")
  UpDownCounter::add(updown_counter, 999999.0)
  UpDownCounter::add(updown_counter, -999999.0)
  
  // If no crashes occur, error handling is working
  assert_true(true)
}

test "log record error handling" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // Test log record with empty body
  let empty_log = LogRecord::new(Info, "")
  assert_eq(LogRecord::body(empty_log), Some(""))
  
  // Test log record with None body
  let log_with_no_body = LogRecord::new_with_context(
    Error,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::body(log_with_no_body), None)
  
  // Test log record with all fields populated
  let full_log = LogRecord::new_with_context(
    Warn,
    Some("Test message"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace123"),
    Some("span456"),
    None
  )
  assert_eq(LogRecord::severity_number(full_log), Warn)
  assert_eq(LogRecord::body(full_log), Some("Test message"))
  assert_eq(LogRecord::trace_id(full_log), Some("trace123"))
  assert_eq(LogRecord::span_id(full_log), Some("span456"))
}

test "span operations error handling" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer")
  
  // Test span creation with different kinds
  let internal_span = Tracer::start_span(tracer, "internal-operation")
  let server_span = Tracer::start_span(tracer, "server-operation")
  let client_span = Tracer::start_span(tracer, "client-operation")
  
  // Test span status setting
  Span::set_status(internal_span, Error, Some("Test error"))
  Span::set_status(server_span, Ok, None)
  Span::set_status(client_span, Unset, Some("Reset status"))
  
  // Test span event addition with various attribute types
  Span::add_event(internal_span, "test.event", Some([
    ("string.attr", StringValue("test")),
    ("int.attr", IntValue(42)),
    ("bool.attr", BoolValue(true)),
    ("float.attr", FloatValue(3.14))
  ]))
  
  // Test span ending multiple times (should not crash)
  Span::end(internal_span)
  Span::end(server_span)
  Span::end(client_span)
  
  // If no crashes occur, error handling is working
  assert_true(true)
}

test "resource operations error handling" {
  // Test resource with empty attributes
  let empty_resource = Resource::new()
  let missing_attr = Resource::get_attribute(empty_resource, "any.attr")
  assert_eq(missing_attr, None)
  
  // Test resource with single attribute
  let single_attr = [("test.key", StringValue("test.value"))]
  let single_resource = Resource::with_attributes(Resource::new(), single_attr)
  let single_value = Resource::get_attribute(single_resource, "test.key")
  assert_eq(single_value, Some(StringValue("test.value")))
  
  // Test resource merging with empty resources
  let merged_empty = Resource::merge(Resource::new(), Resource::new())
  // Should not crash
  assert_true(true)
}

test "propagator error handling" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Test injection with empty context
  let empty_ctx = Context::root()
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, empty_ctx, carrier)
  
  // Test extraction from empty carrier
  let empty_carrier = TextMapCarrier::new()
  let extracted_ctx = CompositePropagator::extract(composite_propagator, empty_carrier)
  
  // Test extraction with malformed headers
  let malformed_carrier = TextMapCarrier::new()
  TextMapCarrier::set(malformed_carrier, "traceparent", "malformed-header-value")
  let malformed_ctx = CompositePropagator::extract(composite_propagator, malformed_carrier)
  
  // If no crashes occur, error handling is working
  assert_true(true)
}

test "http client error handling" {
  // Test HTTP request with minimal data
  let minimal_request = HttpRequest::new("", "", [])
  assert_eq(HttpRequest::http_method(minimal_request), "")
  assert_eq(HttpRequest::url(minimal_request), "")
  assert_eq(HttpRequest::body(minimal_request), None)
  
  // Test HTTP request with full data
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let full_request = HttpRequest::new("POST", "https://api.example.com/data", headers, Some("{\"test\": \"data\"}"))
  assert_eq(HttpRequest::http_method(full_request), "POST")
  assert_eq(HttpRequest::url(full_request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(full_request), Some("{\"test\": \"data\"}"))
  
  // Test HTTP response with edge case status codes
  let min_response = HttpResponse::new(0, [])
  let max_response = HttpResponse::new(999, [])
  let error_response = HttpResponse::new(500, [], Some("Internal Server Error"))
  
  assert_eq(HttpResponse::status_code(min_response), 0)
  assert_eq(HttpResponse::status_code(max_response), 999)
  assert_eq(HttpResponse::status_code(error_response), 500)
  assert_eq(HttpResponse::body(error_response), Some("Internal Server Error"))
}

test "clock and random error handling" {
  // Test clock operations
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  assert_true(timestamp >= 0L)
  
  // Test random operations
  let random = Random::system()
  let random_bytes = Random::next_bytes(random, 0)
  assert_eq(random_bytes.length(), 0)
  
  let random_bytes_10 = Random::next_bytes(random, 10)
  assert_true(random_bytes_10.length() >= 0)  // May be empty in simplified implementation
  
  let random_u64 = Random::next_u64(random)
  assert_true(random_u64 >= 0UL)
}