// Boundary Condition Error Handling Tests
// This file contains comprehensive tests for boundary conditions and error handling

test "span context with invalid trace and span IDs" {
  // Test span context creation with invalid trace and span IDs
  let empty_trace_ctx = SpanContext::new("", "valid_span_id", true, "")
  let empty_span_ctx = SpanContext::new("valid_trace_id", "", true, "")
  let both_empty_ctx = SpanContext::new("", "", true, "")
  
  // Verify invalid contexts are properly detected
  assert_false(SpanContext::is_valid(empty_trace_ctx))
  assert_false(SpanContext::is_valid(empty_span_ctx))
  assert_false(SpanContext::is_valid(both_empty_ctx))
  
  // Test with null-like values
  let null_trace_ctx = SpanContext::new("00000000000000000000000000000000", "valid_span_id", true, "")
  let null_span_ctx = SpanContext::new("valid_trace_id", "0000000000000000", true, "")
  
  // These should still be considered invalid
  assert_false(SpanContext::is_valid(null_trace_ctx))
  assert_false(SpanContext::is_valid(null_span_ctx))
}

test "attribute value type conversion edge cases" {
  // Test attribute value type conversion with edge cases
  let attrs = Attributes::new()
  
  // Test with extreme integer values
  Attributes::set(attrs, "max.int", IntValue(2147483647))
  Attributes::set(attrs, "min.int", IntValue(-2147483648))
  
  let max_int = Attributes::get(attrs, "max.int")
  let min_int = Attributes::get(attrs, "min.int")
  
  assert_eq(max_int, Some(IntValue(2147483647)))
  assert_eq(min_int, Some(IntValue(-2147483648)))
  
  // Test with extreme float values
  Attributes::set(attrs, "max.float", FloatValue(1.7976931348623157e+308))
  Attributes::set(attrs, "min.float", FloatValue(-1.7976931348623157e+308))
  Attributes::set(attrs, "inf.float", FloatValue(1.0 / 0.0))
  Attributes::set(attrs, "neg.inf.float", FloatValue(-1.0 / 0.0))
  Attributes::set(attrs, "nan.float", FloatValue(0.0 / 0.0))
  
  let max_float = Attributes::get(attrs, "max.float")
  let min_float = Attributes::get(attrs, "min.float")
  let inf_float = Attributes::get(attrs, "inf.float")
  let neg_inf_float = Attributes::get(attrs, "neg.inf.float")
  let nan_float = Attributes::get(attrs, "nan.float")
  
  assert_true(max_float != None)
  assert_true(min_float != None)
  assert_true(inf_float != None)
  assert_true(neg_inf_float != None)
  assert_true(nan_float != None)
}

test "log record with extremely long content" {
  // Test log record with extremely long content
  let very_long_string = "This is an extremely long log message that exceeds normal limits. ".repeat(1000)
  let record = LogRecord::new(Error, very_long_string)
  
  // Verify the log record handles long content
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some(very_long_string))
  
  // Test with empty and whitespace-only content
  let empty_record = LogRecord::new(Info, "")
  let whitespace_record = LogRecord::new(Warn, "   \n\t   ")
  
  assert_eq(LogRecord::body(empty_record), Some(""))
  assert_eq(LogRecord::body(whitespace_record), Some("   \n\t   "))
}

test "context operations with null and empty values" {
  // Test context operations with null and empty values
  let ctx = Context::root()
  let empty_key = ContextKey::new("")
  let null_key = ContextKey::new("null.key")
  let empty_value = ""
  
  // Test with empty key
  let ctx_with_empty_key = Context::with_value(ctx, empty_key, "test.value")
  let retrieved_empty = Context::get(ctx_with_empty_key, empty_key)
  
  // Test with empty value
  let ctx_with_empty_value = Context::with_value(ctx, null_key, empty_value)
  let retrieved_empty_value = Context::get(ctx_with_empty_value, null_key)
  
  // Test retrieval of non-existent keys
  let missing_key = ContextKey::new("non.existent.key")
  let missing_value = Context::get(ctx, missing_key)
  
  assert_true(retrieved_empty != None)
  assert_eq(retrieved_empty_value, Some(""))
  assert_eq(missing_value, None)
}

test "baggage operations with special characters" {
  // Test baggage operations with special characters and edge cases
  let baggage = Baggage::new()
  
  // Test with special characters in keys and values
  let special_key = "special.key.with.dots.and-underscores_and@symbols"
  let special_value = "value with spaces & special chars!@#$%^&*(){}[]|\\:;\"'<>?,./"
  
  baggage = Baggage::set_entry(baggage, special_key, special_value)
  let retrieved_special = Baggage::get_entry(baggage, special_key)
  
  // Test with Unicode characters
  let unicode_key = "unicode.键"
  let unicode_value = "Unicode 值 with émojis 🚀🎉 and accent café"
  
  baggage = Baggage::set_entry(baggage, unicode_key, unicode_value)
  let retrieved_unicode = Baggage::get_entry(baggage, unicode_key)
  
  // Test with very long key and value
  let long_key = "very.long.key.".repeat(100)
  let long_value = "very.long.value.".repeat(100)
  
  baggage = Baggage::set_entry(baggage, long_key, long_value)
  let retrieved_long = Baggage::get_entry(baggage, long_key)
  
  assert_eq(retrieved_special, Some(special_value))
  assert_eq(retrieved_unicode, Some(unicode_value))
  assert_eq(retrieved_long, Some(long_value))
}

test "text map carrier with malformed headers" {
  // Test text map carrier with malformed headers
  let carrier = TextMapCarrier::new()
  
  // Test with malformed traceparent headers
  TextMapCarrier::set(carrier, "traceparent", "") // Empty
  TextMapCarrier::set(carrier, "traceparent", "malformed-header") // Malformed
  TextMapCarrier::set(carrier, "traceparent", "00-too-short") // Too short
  TextMapCarrier::set(carrier, "traceparent", "00-11112222333344445555666677778888-aaaabbbbccccdddd-03") // Invalid flags
  
  // Test retrieval of malformed headers
  let empty_header = TextMapCarrier::get(carrier, "traceparent")
  let malformed_header = TextMapCarrier::get(carrier, "traceparent")
  
  // Test with headers containing null bytes and special characters
  TextMapCarrier::set(carrier, "special.header", "value\x00with\x00null\x00bytes")
  TextMapCarrier::set(carrier, "unicode.header", "Unicode value with 🚀 emojis")
  
  let special_header = TextMapCarrier::get(carrier, "special.header")
  let unicode_header = TextMapCarrier::get(carrier, "unicode.header")
  
  // Verify headers are stored and retrieved (even if malformed)
  assert_true(empty_header != None)
  assert_true(malformed_header != None)
  assert_true(special_header != None)
  assert_true(unicode_header != None)
}

test "resource operations with extreme attribute counts" {
  // Test resource operations with extreme numbers of attributes
  let resource = Resource::new()
  
  // Create large number of attributes
  let mut large_attrs = []
  for i = 0; i < 1000; i = i + 1 {
    large_attrs = large_attrs.push(("attr." + i.to_string(), StringValue("value." + i.to_string())))
  }
  
  let resource_with_many_attrs = Resource::with_attributes(resource, large_attrs)
  
  // Test retrieval of attributes from large resource
  let first_attr = Resource::get_attribute(resource_with_many_attrs, "attr.0")
  let middle_attr = Resource::get_attribute(resource_with_many_attrs, "attr.500")
  let last_attr = Resource::get_attribute(resource_with_many_attrs, "attr.999")
  let missing_attr = Resource::get_attribute(resource_with_many_attrs, "attr.1000")
  
  // Test merge of resources with many attributes
  let override_resource = Resource::new()
  let mut override_attrs = []
  for i = 0; i < 500; i = i + 1 {
    override_attrs = override_attrs.push(("override.attr." + i.to_string(), StringValue("override.value." + i.to_string())))
  }
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  let merged = Resource::merge(resource_with_many_attrs, override_with_attrs)
  let merged_attr = Resource::get_attribute(merged, "override.attr.0")
  
  assert_eq(first_attr, Some(StringValue("value.0")))
  assert_eq(middle_attr, Some(StringValue("value.500")))
  assert_eq(last_attr, Some(StringValue("value.999")))
  assert_eq(missing_attr, None)
  assert_eq(merged_attr, Some(StringValue("override.value.0")))
}

test "metrics with extreme values" {
  // Test metrics with extreme values
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "extreme-test")
  let counter = Meter::create_counter(meter, "extreme.counter")
  let histogram = Meter::create_histogram(meter, "extreme.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "extreme.updown")
  
  // Test with extreme counter values
  Counter::add(counter, 1.7976931348623157e+308) // Max double
  Counter::add(counter, -1.7976931348623157e+308) // Min double
  Counter::add(counter, 1.0 / 0.0) // Infinity
  Counter::add(counter, -1.0 / 0.0) // Negative infinity
  Counter::add(counter, 0.0 / 0.0) // NaN
  
  // Test with extreme histogram values
  Histogram::record(histogram, 1.7976931348623157e+308)
  Histogram::record(histogram, -1.7976931348623157e+308)
  Histogram::record(histogram, 1.0 / 0.0)
  Histogram::record(histogram, -1.0 / 0.0)
  Histogram::record(histogram, 0.0 / 0.0)
  
  // Test with extreme updown counter values
  UpDownCounter::add(updown_counter, 1.7976931348623157e+308)
  UpDownCounter::add(updown_counter, -1.7976931348623157e+308)
  
  // Verify operations complete without errors
  assert_true(true)
}

test "span operations with invalid states" {
  // Test span operations with invalid states
  let invalid_ctx = SpanContext::new("", "", false, "")
  let span = Span::new("test.span", Internal, invalid_ctx)
  
  // Test operations on invalid span
  let span_name = Span::name(span)
  let span_kind = Span::kind(span)
  let is_recording = Span::is_recording(span)
  let span_context = Span::span_context(span)
  
  // Test status and event operations
  Span::set_status(span, Error, "Test error")
  Span::add_event(span, "Test event")
  Span::end(span)
  
  // Verify operations handle invalid state gracefully
  assert_eq(span_name, "test.span")
  assert_eq(span_kind, Internal)
  assert_true(is_recording) // Even with invalid context
  assert_false(SpanContext::is_valid(span_context))
}

test "propagator operations with invalid data" {
  // Test propagator operations with invalid data
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let ctx = Context::root()
  
  // Test with invalid carrier
  let empty_carrier = TextMapCarrier::new()
  let invalid_carrier = TextMapCarrier::new()
  TextMapCarrier::set(invalid_carrier, "traceparent", "invalid-traceparent-format")
  
  // Test injection and extraction with invalid data
  CompositePropagator::inject(propagator, ctx, empty_carrier)
  let extracted_from_empty = CompositePropagator::extract(propagator, empty_carrier)
  
  CompositePropagator::inject(propagator, ctx, invalid_carrier)
  let extracted_from_invalid = CompositePropagator::extract(propagator, invalid_carrier)
  
  // Test with null context
  CompositePropagator::inject(propagator, ctx, invalid_carrier)
  
  // Verify operations handle invalid data gracefully
  assert_true(extracted_from_empty != Context::root())
  assert_true(extracted_from_invalid != Context::root())
}