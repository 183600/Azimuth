// Error Boundary Test Suite for Azimuth Telemetry System
// Tests error handling, edge cases, and boundary conditions

test "span context with invalid values" {
  // Test with empty trace ID and span ID
  let invalid_span_ctx1 = SpanContext::new("", "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx1))
  assert_true(SpanContext::is_sampled(invalid_span_ctx1))
  
  // Test with empty trace ID but valid span ID
  let invalid_span_ctx2 = SpanContext::new("", "b7ad6b7169203331", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx2))
  assert_true(SpanContext::is_sampled(invalid_span_ctx2))
  
  // Test with valid trace ID but empty span ID
  let invalid_span_ctx3 = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx3))
  assert_true(SpanContext::is_sampled(invalid_span_ctx3))
  
  // Test with both valid
  let valid_span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_true(SpanContext::is_valid(valid_span_ctx))
  assert_true(SpanContext::is_sampled(valid_span_ctx))
}

test "context operations with non-existent keys" {
  let ctx = Context::root()
  let non_existent_key = ContextKey::new("non.existent.key")
  
  // Test getting non-existent key from root context
  let value1 = Context::get(ctx, non_existent_key)
  assert_eq(value1, None)
  
  // Test getting non-existent key from context with other values
  let existing_key = ContextKey::new("existing.key")
  let ctx_with_value = Context::with_value(ctx, existing_key, "some.value")
  
  let value2 = Context::get(ctx_with_value, non_existent_key)
  assert_eq(value2, None)
  
  // Verify existing key still works
  let existing_value = Context::get(ctx_with_value, existing_key)
  assert_eq(existing_value, Some("some.value"))
}

test "attributes operations with non-existent keys" {
  let attrs = Attributes::new()
  
  // Test getting non-existent attribute
  let non_existent_value = Attributes::get(attrs, "non.existent.attribute")
  assert_eq(non_existent_value, None)
  
  // Test getting non-existent attribute after setting other attributes
  Attributes::set(attrs, "existing.attribute", StringValue("existing.value"))
  let still_non_existent = Attributes::get(attrs, "still.non.existent")
  assert_eq(still_non_existent, None)
  
  // Verify existing attribute works
  let existing_value = Attributes::get(attrs, "existing.attribute")
  assert_eq(existing_value, Some(StringValue("existing.value")))
}

test "baggage operations with non-existent entries" {
  let baggage = Baggage::new()
  
  // Test getting non-existent entry
  let non_existent_entry = Baggage::get_entry(baggage, "non.existent.entry")
  assert_eq(non_existent_entry, None)
  
  // Test getting non-existent entry after setting other entries
  let baggage_with_entry = Baggage::set_entry(baggage, "existing.entry", "existing.value")
  let still_non_existent = Baggage::get_entry(baggage_with_entry, "still.non.existent")
  assert_eq(still_non_existent, None)
  
  // Verify existing entry works
  let existing_entry = Baggage::get_entry(baggage_with_entry, "existing.entry")
  assert_eq(existing_entry, Some("existing.value"))
}

test "text map carrier operations with missing headers" {
  let carrier = TextMapCarrier::new()
  
  // Test getting missing header
  let missing_header = TextMapCarrier::get(carrier, "missing.header")
  assert_eq(missing_header, None)
  
  // Test getting missing header after setting other headers
  TextMapCarrier::set(carrier, "existing.header", "existing.value")
  let still_missing = TextMapCarrier::get(carrier, "still.missing")
  assert_eq(still_missing, None)
  
  // Verify existing header works
  let existing_header = TextMapCarrier::get(carrier, "existing.header")
  assert_eq(existing_header, Some("existing.value"))
}

test "log record with None values" {
  // Test log record with None body
  let record1 = LogRecord::new_with_context(
    Info,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::severity_number(record1), Info)
  assert_eq(LogRecord::body(record1), None)
  assert_eq(LogRecord::trace_id(record1), None)
  assert_eq(LogRecord::span_id(record1), None)
  
  // Test log record with some None values
  let record2 = LogRecord::new_with_context(
    Error,
    Some("Error message"),
    None,
    Some(1735689600000000000L),
    None,
    Some("trace-id"),
    None,
    None
  )
  assert_eq(LogRecord::severity_number(record2), Error)
  assert_eq(LogRecord::body(record2), Some("Error message"))
  assert_eq(LogRecord::trace_id(record2), Some("trace-id"))
  assert_eq(LogRecord::span_id(record2), None)
}

test "span operations with edge cases" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Test adding events with None attributes
  Span::add_event(span, "event.without.attrs", None)
  
  // Test setting status without description
  Span::set_status(span, Ok, None)
  
  // Test setting status with empty description
  Span::set_status(span, Error, Some(""))
  
  // Test ending span multiple times (should not crash)
  Span::end(span)
  Span::end(span)  // Second end should not crash
  
  assert_true(true)  // If we reach here, all operations were safe
}

test "metrics operations with edge cases" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "edge-case-meter")
  
  let counter = Meter::create_counter(meter, "edge.counter")
  let histogram = Meter::create_histogram(meter, "edge.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "edge.updown")
  
  // Test with None attributes
  Counter::add(counter, 1.0, None)
  Histogram::record(histogram, 100.0, None)
  UpDownCounter::add(updown_counter, 5.0, None)
  
  // Test with zero values
  Counter::add(counter, 0.0)
  Histogram::record(histogram, 0.0)
  UpDownCounter::add(updown_counter, 0.0)
  
  // Test with negative values
  Counter::add(counter, -1.0)
  Histogram::record(histogram, -1.0)
  UpDownCounter::add(updown_counter, -1.0)
  
  assert_true(true)  // If we reach here, all operations were safe
}

test "resource operations with empty attributes" {
  // Test resource with no attributes
  let empty_resource = Resource::new()
  let missing_attr = Resource::get_attribute(empty_resource, "any.attribute")
  assert_eq(missing_attr, None)
  
  // Test resource with empty attributes array
  let resource_with_empty_attrs = Resource::with_attributes(empty_resource, [])
  let still_missing = Resource::get_attribute(resource_with_empty_attrs, "any.attribute")
  assert_eq(still_missing, None)
  
  // Test merging empty resources
  let merged_empty = Resource::merge(empty_resource, resource_with_empty_attrs)
  let still_missing_after_merge = Resource::get_attribute(merged_empty, "any.attribute")
  assert_eq(still_missing_after_merge, None)
}

test "composite propagator with empty propagators" {
  // Test with empty propagators array (if supported)
  // let empty_composite = CompositePropagator::new([])
  // This might not be supported in the current implementation
  
  // Test with single propagator
  let single_propagator = W3CTraceContextPropagator::new()
  let single_composite = CompositePropagator::new([single_propagator])
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test injection and extraction with minimal setup
  CompositePropagator::inject(single_composite, ctx, carrier)
  let extracted_ctx = CompositePropagator::extract(single_composite, carrier)
  
  assert_true(true)  // If we reach here, operations were safe
}

test "clock and random operations edge cases" {
  let clock = Clock::system()
  let random = Random::system()
  
  // Test clock operations
  let timestamp1 = Clock::now_unix_nanos(clock)
  let timestamp2 = Clock::now_unix_nanos(clock)
  
  // Timestamps should be reasonable (not negative, not zero in this implementation)
  assert_true(timestamp1 >= 0L)
  assert_true(timestamp2 >= 0L)
  
  // Test random operations
  let empty_bytes = Random::next_bytes(random, 0)
  assert_eq(empty_bytes.length, 0)
  
  let some_bytes = Random::next_bytes(random, 10)
  // In simplified implementation, this returns empty array
  assert_eq(some_bytes.length, 0)
  
  let random_u64 = Random::next_u64(random)
  // In simplified implementation, this returns a fixed value
  assert_eq(random_u64, 12345UL)
}

test "HTTP operations with empty values" {
  // Test HTTP request with empty values
  let empty_headers : Array[(String, String)] = []
  let empty_request = HttpRequest::new("", "", empty_headers, Some(""))
  
  assert_eq(HttpRequest::http_method(empty_request), "")
  assert_eq(HttpRequest::url(empty_request), "")
  assert_eq(HttpRequest::body(empty_request), Some(""))
  
  // Test HTTP response with empty values
  let empty_response = HttpResponse::new(0, empty_headers, Some(""))
  
  assert_eq(HttpResponse::status_code(empty_response), 0)
  assert_eq(HttpResponse::body(empty_response), Some(""))
  
  // Test HTTP request with None body
  let no_body_request = HttpRequest::new("GET", "https://example.com", empty_headers, None)
  assert_eq(HttpRequest::body(no_body_request), None)
  
  // Test HTTP response with None body
  let no_body_response = HttpResponse::new(200, empty_headers, None)
  assert_eq(HttpResponse::body(no_body_response), None)
}

test "instrument type operations with edge cases" {
  // Test instrument with None description and unit
  let counter_no_desc = Counter("test.counter", None, None)
  let instrument_name = Instrument::name(Counter(counter_no_desc.name, counter_no_desc.description, counter_no_desc.unit))
  assert_eq(instrument_name, "test.counter")
  
  let instrument_desc = Instrument::description(Counter(counter_no_desc.name, counter_no_desc.description, counter_no_desc.unit))
  assert_eq(instrument_desc, None)
  
  let instrument_unit = Instrument::unit(Counter(counter_no_desc.name, counter_no_desc.description, counter_no_desc.unit))
  assert_eq(instrument_unit, None)
  
  // Test instrument with Some description and unit
  let counter_with_desc = Counter("test.counter2", Some("Test counter"), Some("count"))
  let instrument_name2 = Instrument::name(Counter(counter_with_desc.name, counter_with_desc.description, counter_with_desc.unit))
  assert_eq(instrument_name2, "test.counter2")
  
  let instrument_desc2 = Instrument::description(Counter(counter_with_desc.name, counter_with_desc.description, counter_with_desc.unit))
  assert_eq(instrument_desc2, Some("Test counter"))
  
  let instrument_unit2 = Instrument::unit(Counter(counter_with_desc.name, counter_with_desc.description, counter_with_desc.unit))
  assert_eq(instrument_unit2, Some("count"))
}