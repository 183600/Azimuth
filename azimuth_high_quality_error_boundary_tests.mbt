// High-Quality Test Suite for Error Boundary Handling
// This file contains comprehensive test cases for error boundary handling functionality

test "span with invalid trace context" {
  // Test with empty trace ID
  let invalid_trace_id = ""
  let span_id = "b7ad6b7169203331"
  let invalid_ctx = SpanContext::new(invalid_trace_id, span_id, true, "")
  let invalid_span = Span::new("invalid-span", Internal, invalid_ctx)
  
  // Verify invalid context is detected
  assert_false(SpanContext::is_valid(Span::span_context(invalid_span)))
  
  // Test with empty span ID
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let invalid_span_id = ""
  let invalid_ctx2 = SpanContext::new(trace_id, invalid_span_id, true, "")
  let invalid_span2 = Span::new("invalid-span2", Internal, invalid_ctx2)
  
  // Verify invalid context is detected
  assert_false(SpanContext::is_valid(Span::span_context(invalid_span2)))
}

test "span error status handling" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("error-span", Internal, span_ctx)
  
  // Initially unset status
  assert_eq(Span::status(span), Unset)
  
  // Set error status
  Span::set_status(span, Error, Some("Database connection failed"))
  assert_eq(Span::status(span), Error)
  
  // Add error event
  let error_attrs = [("error.type", StringValue("ConnectionError")), ("error.message", StringValue("Timeout after 30 seconds"))]
  Span::add_event(span, "exception", Some(error_attrs))
  
  // End span with error
  Span::end(span)
  
  // Verify error status is preserved
  assert_eq(Span::status(span), Error)
}

test "context with missing values" {
  let root_ctx = Context::root()
  let nonexistent_key = ContextKey::new("nonexistent.key")
  
  // Try to get nonexistent value
  let missing_value = Context::get(root_ctx, nonexistent_key)
  assert_eq(missing_value, None)
  
  // Create context with value and try to get different key
  let key = ContextKey::new("existing.key")
  let ctx_with_value = Context::with_value(root_ctx, key, "value")
  
  let different_key = ContextKey::new("different.key")
  let missing_value2 = Context::get(ctx_with_value, different_key)
  assert_eq(missing_value2, None)
}

test "baggage with missing entries" {
  let baggage = Baggage::new()
  
  // Try to get nonexistent entry
  let missing_entry = Baggage::get_entry(baggage, "nonexistent.key")
  assert_eq(missing_entry, None)
  
  // Add entry and try to get different key
  let updated_baggage = Baggage::set_entry(baggage, "existing.key", "value")
  let missing_entry2 = Baggage::get_entry(updated_baggage, "different.key")
  assert_eq(missing_entry2, None)
}

test "metrics with zero and negative values" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test counter with zero value
  let counter = Meter::create_counter(meter, "test.counter")
  Counter::add(counter, 0.0)
  
  // Test counter with negative value (should be allowed for some implementations)
  Counter::add(counter, -10.0)
  
  // Test histogram with zero value
  let histogram = Meter::create_histogram(meter, "test.histogram")
  Histogram::record(histogram, 0.0)
  
  // Test histogram with negative value (should be allowed for some implementations)
  Histogram::record(histogram, -5.0)
  
  // Test updown counter with negative value
  let updown_counter = Meter::create_updown_counter(meter, "test.updown")
  UpDownCounter::add(updown_counter, -100.0)
  
  // Test gauge with negative value
  let gauge = Meter::create_gauge(meter, "test.gauge")
  UpDownCounter::add(gauge, -50.0)
  
  assert_true(true) // If we reach here, operations were successful
}

test "log record with empty body" {
  // Test log record with empty body
  let record_with_empty_body = LogRecord::new(Info, "")
  assert_eq(LogRecord::body(record_with_empty_body), Some(""))
  
  // Test log record with None body (using new_with_context)
  let record_with_none_body = LogRecord::new_with_context(
    Warn,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::body(record_with_none_body), None)
}

test "resource with empty attributes" {
  let resource = Resource::new()
  let empty_attrs = [] : Array[(String, AttributeValue)]
  let resource_with_empty_attrs = Resource::with_attributes(resource, empty_attrs)
  
  // Try to get attribute from resource with empty attributes
  let missing_attr = Resource::get_attribute(resource_with_empty_attrs, "any.key")
  assert_eq(missing_attr, None)
}

test "propagator with empty carrier" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let empty_carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // Try to extract from empty carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, empty_carrier)
  
  // Verify extraction worked (returns a different context)
  let key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, key), Some("true"))
  
  // Inject into empty carrier
  CompositePropagator::inject(composite_propagator, ctx, empty_carrier)
  
  // Verify injection worked
  assert_eq(TextMapCarrier::get(empty_carrier, "traceparent"), Some("00-test-trace-id-test-span-id-01"))
}

test "span with extremely long names" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Create span with very long name
  let long_name = "this.is.a.very.long.span.name.that.exceeds.normal.lengths.and.tests.boundary.conditions"
  let span_with_long_name = Span::new(long_name, Internal, span_ctx)
  
  // Verify name is preserved
  assert_eq(Span::name(span_with_long_name), long_name)
  
  // Add event with long name
  Span::add_event(span_with_long_name, "this.is.a.very.long.event.name.that.exceeds.normal.lengths", None)
  
  // End span
  Span::end(span_with_long_name)
  
  // Verify span is still accessible
  assert_eq(Span::name(span_with_long_name), long_name)
}

test "context with special characters" {
  let root_ctx = Context::root()
  
  // Test key with special characters
  let special_key = ContextKey::new("key.with.special.chars!@#$%^&*()")
  let ctx_with_special = Context::with_value(root_ctx, special_key, "value.with.special.chars!@#$%^&*()")
  
  // Verify special characters are preserved
  assert_eq(Context::get(ctx_with_special, special_key), Some("value.with.special.chars!@#$%^&*()"))
  
  // Test key with unicode characters
  let unicode_key = ContextKey::new("键.值.测试")
  let ctx_with_unicode = Context::with_value(ctx_with_special, unicode_key, "测试值")
  
  // Verify unicode characters are preserved
  assert_eq(Context::get(ctx_with_unicode, unicode_key), Some("测试值"))
}

test "baggage with special characters" {
  let baggage = Baggage::new()
  
  // Test entry with special characters
  let baggage_with_special = Baggage::set_entry(baggage, "key.with.special.chars!@#$%^&*()", "value.with.special.chars!@#$%^&*()")
  
  // Verify special characters are preserved
  assert_eq(Baggage::get_entry(baggage_with_special, "key.with.special.chars!@#$%^&*()"), Some("value.with.special.chars!@#$%^&*()"))
  
  // Test entry with unicode characters
  let baggage_with_unicode = Baggage::set_entry(baggage_with_special, "键.值.测试", "测试值")
  
  // Verify unicode characters are preserved
  assert_eq(Baggage::get_entry(baggage_with_unicode, "键.值.测试"), Some("测试值"))
}

test "attributes with special characters and types" {
  let attrs = [
    ("string.special", StringValue("value.with.special.chars!@#$%^&*()")),
    ("string.unicode", StringValue("测试值")),
    ("int.large", IntValue(2147483647)),
    ("int.negative", IntValue(-2147483648)),
    ("float.large", FloatValue(3.4028235e38)),
    ("float.small", FloatValue(1.17549435e-38)),
    ("float.negative", FloatValue(-3.4028235e38)),
    ("bool.true", BoolValue(true)),
    ("bool.false", BoolValue(false)),
    ("array.empty", ArrayStringValue([])),
    ("array.single", ArrayStringValue(["single"])),
    ("array.multiple", ArrayStringValue(["value1", "value2", "value3"])),
    ("array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  let resource = Resource::new()
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Verify all attributes are accessible
  assert_eq(Resource::get_attribute(resource_with_attrs, "string.special"), Some(StringValue("value.with.special.chars!@#$%^&*()")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "string.unicode"), Some(StringValue("测试值")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "int.large"), Some(IntValue(2147483647)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "int.negative"), Some(IntValue(-2147483648)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "float.large"), Some(FloatValue(3.4028235e38)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "float.small"), Some(FloatValue(1.17549435e-38)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "float.negative"), Some(FloatValue(-3.4028235e38)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "bool.true"), Some(BoolValue(true)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "bool.false"), Some(BoolValue(false)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "array.empty"), Some(ArrayStringValue([])))
  assert_eq(Resource::get_attribute(resource_with_attrs, "array.single"), Some(ArrayStringValue(["single"])))
  assert_eq(Resource::get_attribute(resource_with_attrs, "array.multiple"), Some(ArrayStringValue(["value1", "value2", "value3"])))
  assert_eq(Resource::get_attribute(resource_with_attrs, "array.int"), Some(ArrayIntValue([1, 2, 3, 4, 5])))
}

test "error recovery scenario" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("error-recovery-span", Internal, span_ctx)
  
  // Simulate error condition
  Span::set_status(span, Error, Some("Initial error"))
  
  // Add error event
  Span::add_event(span, "error.occurred", None)
  
  // Simulate recovery
  Span::set_status(span, Ok, Some("Error recovered"))
  
  // Add recovery event
  Span::add_event(span, "error.recovered", None)
  
  // End span
  Span::end(span)
  
  // Verify final state
  assert_eq(Span::status(span), Ok)
}

test "complex error boundary scenario" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("complex-error-span", Internal, span_ctx)
  
  // Create context with error information
  let ctx = Context::root()
  let error_key = ContextKey::new("error.type")
  let ctx_with_error = Context::with_value(ctx, error_key, "DatabaseError")
  
  // Create baggage with error information
  let baggage = Baggage::new()
  let baggage_with_error = Baggage::set_entry(baggage, "error.code", "500")
  
  // Create log record with error
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_error)
  )
  
  // Create metrics for error tracking
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "error-tracking")
  let error_counter = Meter::create_counter(meter, "errors.total")
  
  // Increment error counter
  Counter::add(error_counter, 1.0)
  
  // Add error event to span
  let error_attrs = [
    ("error.type", StringValue("DatabaseError")),
    ("error.code", IntValue(500)),
    ("error.message", StringValue("Connection timeout"))
  ]
  Span::add_event(span, "database.error", Some(error_attrs))
  
  // Set error status
  Span::set_status(span, Error, Some("Database operation failed"))
  
  // End span
  Span::end(span)
  
  // Verify error state
  assert_eq(Span::status(span), Error)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(Context::get(ctx_with_error, error_key), Some("DatabaseError"))
  assert_eq(Baggage::get_entry(baggage_with_error, "error.code"), Some("500"))
}