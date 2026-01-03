// Azimuth Error Boundary and Recovery Tests
// This file contains comprehensive test cases for error boundary and recovery functionality

// Test 1: Span Error Status Handling
test "span error status handling" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "error_test_tracer")
  let span = Tracer::start_span(tracer, "error_operation")
  
  // Test initial status
  assert_eq(Span::status(span), Unset)
  
  // Test error status setting
  Span::set_status(span, Error, Some("Database connection failed"))
  assert_eq(Span::status(span), Error)
  
  // Test error event addition
  Span::add_event(span, "error_occurred", Some([
    ("error.type", StringValue("database")),
    ("error.code", IntValue(500)),
    ("error.message", StringValue("Connection timeout"))
  ]))
  
  // Test span ending with error
  Span::end(span)
  
  // Test span with different error types
  let timeout_span = Tracer::start_span(tracer, "timeout_operation")
  Span::set_status(timeout_span, Error, Some("Operation timeout"))
  Span::add_event(timeout_span, "timeout", Some([
    ("timeout.duration.ms", IntValue(30000)),
    ("timeout.type", StringValue("connection"))
  ]))
  Span::end(timeout_span)
}

// Test 2: Context Propagation Error Handling
test "context propagation error handling" {
  let root_ctx = Context::root()
  let key = ContextKey::new("test_key")
  
  // Test normal context propagation
  let normal_ctx = Context::with_value(root_ctx, key, "normal_value")
  let normal_value = Context::get(normal_ctx, key)
  assert_eq(normal_value, Some("normal_value"))
  
  // Test context with empty key
  let empty_key = ContextKey::new("")
  let empty_ctx = Context::with_value(root_ctx, empty_key, "empty_key_value")
  let empty_value = Context::get(empty_ctx, empty_key)
  assert_eq(empty_value, Some("empty_key_value"))
  
  // Test context with special characters
  let special_key = ContextKey::new("special.key.with.dots")
  let special_ctx = Context::with_value(root_ctx, special_key, "special_value")
  let special_value = Context::get(special_ctx, special_key)
  assert_eq(special_value, Some("special_value"))
  
  // Test context with unicode
  let unicode_key = ContextKey::new("unicode.测试")
  let unicode_ctx = Context::with_value(root_ctx, unicode_key, "unicode.value.测试")
  let unicode_value = Context::get(unicode_ctx, unicode_key)
  assert_eq(unicode_value, Some("unicode.value.测试"))
}

// Test 3: Attribute Error Handling
test "attribute error handling" {
  let attrs = Attributes::new()
  
  // Test setting and getting normal attributes
  Attributes::set(attrs, "normal.key", StringValue("normal_value"))
  let normal_value = Attributes::get(attrs, "normal.key")
  match normal_value {
    Some(StringValue(s)) => assert_eq(s, "normal_value")
    _ => assert_true(false)
  }
  
  // Test setting and getting special character keys
  Attributes::set(attrs, "special.key.with.dots", StringValue("special_value"))
  let special_value = Attributes::get(attrs, "special.key.with.dots")
  match special_value {
    Some(StringValue(s)) => assert_eq(s, "special_value")
    _ => assert_true(false)
  }
  
  // Test setting and getting empty key
  Attributes::set(attrs, "", StringValue("empty_key_value"))
  let empty_key_value = Attributes::get(attrs, "")
  match empty_key_value {
    Some(StringValue(s)) => assert_eq(s, "empty_key_value")
    _ => assert_true(false)
  }
  
  // Test getting non-existent key
  let non_existent = Attributes::get(attrs, "non.existent.key")
  assert_eq(non_existent, None)
  
  // Test overwriting existing attribute
  Attributes::set(attrs, "normal.key", StringValue("overwritten_value"))
  let overwritten_value = Attributes::get(attrs, "normal.key")
  match overwritten_value {
    Some(StringValue(s)) => assert_eq(s, "overwritten_value")
    _ => assert_true(false)
  }
}

// Test 4: SpanContext Error Handling
test "span context error handling" {
  // Test valid span context
  let valid_ctx = SpanContext::new("valid_trace_id", "valid_span_id", true, "")
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
  
  // Test invalid span contexts
  let empty_trace_ctx = SpanContext::new("", "valid_span_id", true, "")
  assert_false(SpanContext::is_valid(empty_trace_ctx))
  
  let empty_span_ctx = SpanContext::new("valid_trace_id", "", true, "")
  assert_false(SpanContext::is_valid(empty_span_ctx))
  
  let empty_both_ctx = SpanContext::new("", "", true, "")
  assert_false(SpanContext::is_valid(empty_both_ctx))
  
  // Test unsampled context
  let unsampled_ctx = SpanContext::new("valid_trace_id", "valid_span_id", false, "")
  assert_true(SpanContext::is_valid(unsampled_ctx))
  assert_false(SpanContext::is_sampled(unsampled_ctx))
  
  // Test context with special characters
  let special_ctx = SpanContext::new("trace-with-dash", "span_with_underscore", true, "state=value")
  assert_true(SpanContext::is_valid(special_ctx))
  assert_true(SpanContext::is_sampled(special_ctx))
  assert_eq(SpanContext::trace_id(special_ctx), "trace-with-dash")
  assert_eq(SpanContext::span_id(special_ctx), "span_with_underscore")
}

// Test 5: Logger Error Handling
test "logger error handling" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "error_test_logger")
  
  // Test logging with different severity levels
  let trace_record = LogRecord::new(Trace, "Trace message")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Info message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  
  // Emit all records
  Logger::emit(logger, trace_record)
  Logger::emit(logger, debug_record)
  Logger::emit(logger, info_record)
  Logger::emit(logger, warn_record)
  Logger::emit(logger, error_record)
  Logger::emit(logger, fatal_record)
  
  // Test logging with empty message
  let empty_record = LogRecord::new(Info, "")
  assert_eq(LogRecord::body(empty_record), Some(""))
  Logger::emit(logger, empty_record)
  
  // Test logging with special characters
  let special_record = LogRecord::new(Error, "Error with special chars: !@#$%^&*()")
  assert_eq(LogRecord::body(special_record), Some("Error with special chars: !@#$%^&*()"))
  Logger::emit(logger, special_record)
  
  // Test logging with unicode
  let unicode_record = LogRecord::new(Error, "错误信息 with émojis 🚨")
  assert_eq(LogRecord::body(unicode_record), Some("错误信息 with émojis 🚨"))
  Logger::emit(logger, unicode_record)
}

// Test 6: Metrics Error Handling
test "metrics error handling" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "error_test_meter")
  
  // Test counter with negative values
  let counter = Meter::create_counter(meter, "error_counter", Some("Error counter"), Some("errors"))
  Counter::add(counter, -1.0, None)
  Counter::add(counter, -100.5, None)
  
  // Test counter with very large values
  Counter::add(counter, 999999999.0, None)
  Counter::add(counter, 0.0000001, None)
  
  // Test histogram with negative values
  let histogram = Meter::create_histogram(meter, "error_histogram", Some("Error histogram"), Some("ms"))
  Histogram::record(histogram, -100.0, None)
  Histogram::record(histogram, 0.0, None)
  Histogram::record(histogram, 999999999.0, None)
  
  // Test updown counter with large values
  let updown_counter = Meter::create_updown_counter(meter, "error_updown", Some("Error updown"), Some("count"))
  UpDownCounter::add(updown_counter, 999999999.0, None)
  UpDownCounter::add(updown_counter, -999999999.0, None)
  
  // Test gauge with extreme values
  let gauge = Meter::create_gauge(meter, "error_gauge", Some("Error gauge"), Some("%"))
  UpDownCounter::add(gauge, 999.99, None)
  UpDownCounter::add(gauge, -999.99, None)
  UpDownCounter::add(gauge, 0.0, None)
}

// Test 7: Baggage Error Handling
test "baggage error handling" {
  let baggage = Baggage::new()
  
  // Test baggage with empty key
  let empty_key_baggage = Baggage::set_entry(baggage, "", "empty_key_value")
  let empty_key_value = Baggage::get_entry(empty_key_baggage, "")
  assert_eq(empty_key_value, Some("empty_key_value"))
  
  // Test baggage with empty value
  let empty_value_baggage = Baggage::set_entry(baggage, "empty_value_key", "")
  let empty_value = Baggage::get_entry(empty_value_baggage, "empty_value_key")
  assert_eq(empty_value, Some(""))
  
  // Test baggage with special characters
  let special_baggage = Baggage::set_entry(baggage, "special.key.with.dots", "special.value.with.dots")
  let special_value = Baggage::get_entry(special_baggage, "special.key.with.dots")
  assert_eq(special_value, Some("special.value.with.dots"))
  
  // Test baggage with unicode
  let unicode_baggage = Baggage::set_entry(baggage, "unicode.测试", "unicode.value.测试")
  let unicode_value = Baggage::get_entry(unicode_baggage, "unicode.测试")
  assert_eq(unicode_value, Some("unicode.value.测试"))
  
  // Test removing non-existent entry
  let non_existent_removal = Baggage::remove_entry(baggage, "non.existent.key")
  let non_existent_value = Baggage::get_entry(non_existent_removal, "non.existent.key")
  assert_eq(non_existent_value, None)
  
  // Test removing existing entry
  let with_entry = Baggage::set_entry(baggage, "to.remove", "value.to.remove")
  let before_removal = Baggage::get_entry(with_entry, "to.remove")
  assert_eq(before_removal, Some("value.to.remove"))
  
  let after_removal = Baggage::remove_entry(with_entry, "to.remove")
  let removed_value = Baggage::get_entry(after_removal, "to.remove")
  assert_eq(removed_value, None)
}

// Test 8: TextMapCarrier Error Handling
test "text map carrier error handling" {
  let carrier = TextMapCarrier::new()
  
  // Test setting and getting normal headers
  TextMapCarrier::set(carrier, "normal-header", "normal-value")
  let normal_value = TextMapCarrier::get(carrier, "normal-header")
  assert_eq(normal_value, Some("normal-value"))
  
  // Test setting and getting empty header value
  TextMapCarrier::set(carrier, "empty-value-header", "")
  let empty_value = TextMapCarrier::get(carrier, "empty-value-header")
  assert_eq(empty_value, Some(""))
  
  // Test setting and getting header with special characters
  TextMapCarrier::set(carrier, "special-header!@#$%", "special-value!@#$%")
  let special_value = TextMapCarrier::get(carrier, "special-header!@#$%")
  assert_eq(special_value, Some("special-value!@#$%"))
  
  // Test setting and getting header with unicode
  TextMapCarrier::set(carrier, "unicode-header-测试", "unicode-value-测试")
  let unicode_value = TextMapCarrier::get(carrier, "unicode-header-测试")
  assert_eq(unicode_value, Some("unicode-value-测试"))
  
  // Test getting non-existent header
  let non_existent = TextMapCarrier::get(carrier, "non-existent-header")
  assert_eq(non_existent, None)
  
  // Test case sensitivity
  TextMapCarrier::set(carrier, "case-sensitive", "value")
  let lowercase = TextMapCarrier::get(carrier, "case-sensitive")
  let uppercase = TextMapCarrier::get(carrier, "CASE-SENSITIVE")
  assert_eq(lowercase, Some("value"))
  assert_eq(uppercase, None)
}

// Test 9: Resource Error Handling
test "resource error handling" {
  let resource = Resource::new()
  
  // Test resource with empty attribute key
  let empty_key_attrs = [("", StringValue("empty-key-value"))]
  let empty_key_resource = Resource::with_attributes(resource, empty_key_attrs)
  let empty_key_value = Resource::get_attribute(empty_key_resource, "")
  match empty_key_value {
    Some(StringValue(s)) => assert_eq(s, "empty-key-value")
    _ => assert_true(false)
  }
  
  // Test resource with empty attribute value
  let empty_value_attrs = [("empty-value-key", StringValue(""))]
  let empty_value_resource = Resource::with_attributes(resource, empty_value_attrs)
  let empty_value = Resource::get_attribute(empty_value_resource, "empty-value-key")
  match empty_value {
    Some(StringValue(s)) => assert_eq(s, "")
    _ => assert_true(false)
  }
  
  // Test resource with special characters
  let special_attrs = [
    ("special.key.with.dots", StringValue("special.value.with.dots")),
    ("special-key-with-dashes", StringValue("special-value-with-dashes")),
    ("special_key_with_underscores", StringValue("special_value_with_underscores"))
  ]
  let special_resource = Resource::with_attributes(resource, special_attrs)
  
  let dots_value = Resource::get_attribute(special_resource, "special.key.with.dots")
  match dots_value {
    Some(StringValue(s)) => assert_eq(s, "special.value.with.dots")
    _ => assert_true(false)
  }
  
  // Test resource with unicode
  let unicode_attrs = [("unicode.key.测试", StringValue("unicode.value.测试"))]
  let unicode_resource = Resource::with_attributes(resource, unicode_attrs)
  let unicode_value = Resource::get_attribute(unicode_resource, "unicode.key.测试")
  match unicode_value {
    Some(StringValue(s)) => assert_eq(s, "unicode.value.测试")
    _ => assert_true(false)
  }
  
  // Test merging empty resources
  let empty_resource1 = Resource::new()
  let empty_resource2 = Resource::new()
  let merged_empty = Resource::merge(empty_resource1, empty_resource2)
  assert_eq(merged_empty.attributes.length(), 0)
}

// Test 10: Composite Recovery Scenarios
test "composite recovery scenarios" {
  // Test scenario: Span with error context and logging
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "recovery_tracer")
  let span = Tracer::start_span(tracer, "recovery_operation")
  
  // Set error status
  Span::set_status(span, Error, Some("Composite error scenario"))
  
  // Add error event
  Span::add_event(span, "composite_error", Some([
    ("error.type", StringValue("composite")),
    ("error.code", IntValue(500)),
    ("error.recovery.attempted", BoolValue(true))
  ]))
  
  // Create logger for error context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "recovery_logger")
  
  // Log error with context
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Composite error occurred in recovery scenario"),
    Some([
      ("error.type", StringValue("composite")),
      ("error.code", IntValue(500)),
      ("recovery.attempted", BoolValue(true)),
      ("recovery.success", BoolValue(false))
    ]),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    Some(Context::root())
  )
  
  Logger::emit(logger, error_record)
  
  // Create metrics for error tracking
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "recovery_meter")
  
  let error_counter = Meter::create_counter(meter, "recovery_errors", Some("Recovery errors"), Some("errors"))
  let recovery_histogram = Meter::create_histogram(meter, "recovery_time", Some("Recovery time"), Some("ms"))
  
  Counter::add(error_counter, 1.0, Some([
    ("error.type", StringValue("composite")),
    ("recovery.attempted", StringValue("true"))
  ]))
  
  Histogram::record(recovery_histogram, 5000.0, Some([
    ("error.type", StringValue("composite")),
    ("recovery.success", StringValue("false"))
  ]))
  
  // End the span
  Span::end(span)
  
  // Verify all components handled the error scenario
  assert_eq(Span::status(span), Error)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(error_counter.name, "recovery_errors")
  assert_eq(recovery_histogram.name, "recovery_time")
}