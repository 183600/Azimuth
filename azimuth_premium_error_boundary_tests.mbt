// Azimuth Premium Error Boundary Tests
// This file contains high-quality test cases for error boundary handling and recovery

// Test 1: Attribute operation error boundaries
pub test "premium attribute error boundaries" {
  // Test attribute operations with null/undefined values
  let attrs = Attributes::new()
  
  // Test setting null string values
  Attributes::set(attrs, "null.string", StringValue(""))
  let null_string_result = Attributes::get(attrs, "null.string")
  assert_eq(null_string_result, Some(StringValue("")))
  
  // Test setting extreme numeric values
  Attributes::set(attrs, "max.int", IntValue(9223372036854775807))  // Max Int64
  Attributes::set(attrs, "min.int", IntValue(-9223372036854775808))  // Min Int64
  Attributes::set(attrs, "max.float", FloatValue(1.7976931348623157e+308))  // Max Double
  Attributes::set(attrs, "min.float", FloatValue(-1.7976931348623157e+308))  // Min Double
  Attributes::set(attrs, "infinity", FloatValue(1.0/0.0))  // Infinity
  Attributes::set(attrs, "neg.infinity", FloatValue(-1.0/0.0))  // Negative Infinity
  Attributes::set(attrs, "nan", FloatValue(0.0/0.0))  // NaN
  
  // Verify extreme values are handled
  let max_int_result = Attributes::get(attrs, "max.int")
  let min_int_result = Attributes::get(attrs, "min.int")
  let max_float_result = Attributes::get(attrs, "max.float")
  let min_float_result = Attributes::get(attrs, "min.float")
  let infinity_result = Attributes::get(attrs, "infinity")
  let neg_infinity_result = Attributes::get(attrs, "neg.infinity")
  let nan_result = Attributes::get(attrs, "nan")
  
  assert_true(max_int_result.is_some())
  assert_true(min_int_result.is_some())
  assert_true(max_float_result.is_some())
  assert_true(min_float_result.is_some())
  assert_true(infinity_result.is_some())
  assert_true(neg_infinity_result.is_some())
  assert_true(nan_result.is_some())
  
  // Test attribute operations with extremely long keys
  let very_long_key = "this.is.a.very.long.key.name.that.exceeds.normal.limits.and.tests.error.boundaries." + 
                     "it.contains.many.characters.to.stress.test.the.system.1234567890"
  Attributes::set(attrs, very_long_key, StringValue("long.key.value"))
  let long_key_result = Attributes::get(attrs, very_long_key)
  assert_true(long_key_result.is_some())
  
  // Test attribute operations with special characters
  let special_keys = [
    "key.with.dots",
    "key-with-dashes",
    "key_with_underscores",
    "key.with.symbols!@#$%^&*()",
    "key.with.unicode.测试.中文",
    "key.with.emoji.😀🎉🔥",
    "key.with.newline\ncharacter",
    "key.with.tab\tcharacter",
    "key.with.null\0character",
    "key.with.quotes\"'character"
  ]
  
  for i in 0...special_keys.length() - 1 {
    let key = special_keys[i]
    let value = StringValue("value.for." + key)
    
    // Test setting special character keys
    Attributes::set(attrs, key, value)
    
    // Test getting special character keys
    let result = Attributes::get(attrs, key)
    assert_true(result.is_some())
  }
  
  // Test array attributes with edge cases
  Attributes::set(attrs, "empty.string.array", ArrayStringValue([]))
  Attributes::set(attrs, "empty.int.array", ArrayIntValue([]))
  Attributes::set(attrs, "large.string.array", ArrayStringValue(["item"] * 1000))
  Attributes::set(attrs, "large.int.array", ArrayIntValue([1] * 1000))
  Attributes::set(attrs, "mixed.array", ArrayStringValue(["", "null", "undefined", "123", "true", "false"]))
  
  // Verify array attributes
  let empty_string_array = Attributes::get(attrs, "empty.string.array")
  let empty_int_array = Attributes::get(attrs, "empty.int.array")
  let large_string_array = Attributes::get(attrs, "large.string.array")
  let large_int_array = Attributes::get(attrs, "large.int.array")
  let mixed_array = Attributes::get(attrs, "mixed.array")
  
  assert_true(empty_string_array.is_some())
  assert_true(empty_int_array.is_some())
  assert_true(large_string_array.is_some())
  assert_true(large_int_array.is_some())
  assert_true(mixed_array.is_some())
}

// Test 2: Span operation error boundaries
pub test "premium span error boundaries" {
  // Test span creation with edge cases
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error-boundary-test-tracer")
  
  // Test span with empty name
  let empty_name_span = Tracer::start_span(tracer, "")
  assert_eq(Span::name(empty_name_span), "")
  
  // Test span with very long name
  let very_long_name = "this.is.a.very.long.span.name.that.exceeds.normal.limits.and.tests.error.boundaries." +
                      "it.contains.many.characters.to.stress.test.the.system.1234567890"
  let long_name_span = Tracer::start_span(tracer, very_long_name)
  assert_eq(Span::name(long_name_span), very_long_name)
  
  // Test span with special characters in name
  let special_name_span = Tracer::start_span(tracer, "span.with.special.chars!@#$%^&*()测试😀")
  assert_eq(Span::name(special_name_span), "span.with.special.chars!@#$%^&*()测试😀")
  
  // Test span context with invalid values
  let invalid_trace_ctx = SpanContext::new("", "span-123", true, "")
  let invalid_span_ctx = SpanContext::new("trace-123", "", true, "")
  let invalid_both_ctx = SpanContext::new("", "", false, "")
  
  // Verify invalid contexts are handled
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  assert_false(SpanContext::is_valid(invalid_both_ctx))
  
  // Test span with invalid context
  let invalid_span = Span::new("invalid.span", Internal, invalid_both_ctx)
  assert_eq(Span::name(invalid_span), "invalid.span")
  assert_eq(Span::span_context(invalid_span), invalid_both_ctx)
  
  // Test span status with error conditions
  let error_span = Tracer::start_span(tracer, "error.span")
  
  // Test setting error status
  Span::set_status(error_span, Error, Some("This is a test error message"))
  assert_eq(Span::status(error_span), Error)
  
  // Test setting status with very long description
  let very_long_description = "This.is.a.very.long.error.description.that.exceeds.normal.limits." +
                             "it.contains.many.characters.to.stress.test.the.system." +
                             "the.purpose.is.to.test.error.boundaries.with.extremely.long.messages." +
                             "1234567890".repeat(100)
  Span::set_status(error_span, Error, Some(very_long_description))
  assert_eq(Span::status(error_span), Error)
  
  // Test adding events with edge cases
  Span::add_event(error_span, "")
  Span::add_event(error_span, "event.with.special.chars!@#$%^&*()测试😀")
  Span::add_event(error_span, "very.long.event.name." + "1234567890".repeat(100))
  
  // Test ending already ended span
  Span::end(error_span)
  Span::end(error_span)  // Should not cause error
  
  // Test operations on ended span
  Span::set_status(error_span, Ok, Some("Status after end"))
  Span::add_event(error_span, "Event after end")
  Span::end(error_span)  // Should not cause error
}

// Test 3: Metrics operation error boundaries
pub test "premium metrics error boundaries" {
  // Test metrics with edge cases
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "error-boundary-test-meter")
  
  // Test counter with empty name
  let empty_counter = Meter::create_counter(meter, "")
  assert_eq(empty_counter.name, "")
  
  // Test counter with very long name
  let very_long_name = "this.is.a.very.long.metric.name.that.exceeds.normal.limits.and.tests.error.boundaries." +
                      "it.contains.many.characters.to.stress.test.the.system.1234567890"
  let long_name_counter = Meter::create_counter(meter, very_long_name)
  assert_eq(long_name_counter.name, very_long_name)
  
  // Test counter with special characters in name
  let special_counter = Meter::create_counter(meter, "metric.with.special.chars!@#$%^&*()测试😀")
  assert_eq(special_counter.name, "metric.with.special.chars!@#$%^&*()测试😀")
  
  // Test counter operations with extreme values
  Counter::add(empty_counter, 0.0)
  Counter::add(empty_counter, 1.7976931348623157e+308)  // Max Double
  Counter::add(empty_counter, -1.7976931348623157e+308)  // Min Double
  Counter::add(empty_counter, 1.0/0.0)  // Infinity
  Counter::add(empty_counter, -1.0/0.0)  // Negative Infinity
  Counter::add(empty_counter, 0.0/0.0)  // NaN
  
  // Test histogram with extreme values
  let histogram = Meter::create_histogram(meter, "error.boundary.histogram")
  Histogram::record(histogram, 0.0)
  Histogram::record(histogram, 1.7976931348623157e+308)  // Max Double
  Histogram::record(histogram, -1.7976931348623157e+308)  // Min Double
  Histogram::record(histogram, 1.0/0.0)  // Infinity
  Histogram::record(histogram, -1.0/0.0)  // Negative Infinity
  Histogram::record(histogram, 0.0/0.0)  // NaN
  
  // Test up-down counter with extreme values
  let updown_counter = Meter::create_updown_counter(meter, "error.boundary.updown")
  UpDownCounter::add(updown_counter, 0.0)
  UpDownCounter::add(updown_counter, 1.7976931348623157e+308)  // Max Double
  UpDownCounter::add(updown_counter, -1.7976931348623157e+308)  // Min Double
  UpDownCounter::add(updown_counter, 1.0/0.0)  // Infinity
  UpDownCounter::add(updown_counter, -1.0/0.0)  // Negative Infinity
  UpDownCounter::add(updown_counter, 0.0/0.0)  // NaN
  
  // Test gauge operations
  let gauge = Meter::create_gauge(meter, "error.boundary.gauge")
  // Note: Gauge doesn't have a set method in current implementation
  // This would be extended in a real implementation
  
  // Test metrics with very long descriptions and units
  let very_long_description = "This.is.a.very.long.metric.description.that.exceeds.normal.limits." +
                              "it.contains.many.characters.to.stress.test.the.system." +
                              "1234567890".repeat(100)
  let very_long_unit = "very.long.unit.name.that.exceeds.normal.limits." + "1234567890".repeat(50)
  
  let long_desc_counter = Meter::create_counter(meter, "long.desc.counter", Some(very_long_description), Some(very_long_unit))
  assert_eq(long_desc_counter.description, Some(very_long_description))
  assert_eq(long_desc_counter.unit, Some(very_long_unit))
}

// Test 4: Logging operation error boundaries
pub test "premium logging error boundaries" {
  // Test logging with edge cases
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "error-boundary-test-logger")
  
  // Test log record with empty body
  let empty_body_log = LogRecord::new(Info, "")
  assert_eq(LogRecord::body(empty_body_log), Some(""))
  
  // Test log record with very long body
  let very_long_body = "This.is.a.very.long.log.message.that.exceeds.normal.limits.and.tests.error.boundaries." +
                      "it.contains.many.characters.to.stress.test.the.system." +
                      "1234567890".repeat(1000)
  let long_body_log = LogRecord::new(Error, very_long_body)
  assert_eq(LogRecord::body(long_body_log), Some(very_long_body))
  
  // Test log record with special characters
  let special_body_log = LogRecord::new(Warn, "Log message with special chars!@#$%^&*()测试😀\n\t\0")
  assert_eq(LogRecord::body(special_body_log), Some("Log message with special chars!@#$%^&*()测试😀\n\t\0"))
  
  // Test log record with all severity levels
  let trace_log = LogRecord::new(Trace, "Trace message")
  let debug_log = LogRecord::new(Debug, "Debug message")
  let info_log = LogRecord::new(Info, "Info message")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error message")
  let fatal_log = LogRecord::new(Fatal, "Fatal message")
  
  // Verify severity levels
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // Test detailed log record with edge cases
  let invalid_trace_id = ""
  let invalid_span_id = ""
  let invalid_timestamp = -1L
  let invalid_observed_timestamp = -1L
  
  let invalid_log = LogRecord::new_with_context(
    Error,
    Some("Invalid log record"),
    Some(Attributes::new()),
    Some(invalid_timestamp),
    Some(invalid_observed_timestamp),
    Some(invalid_trace_id),
    Some(invalid_span_id),
    Some(Context::root())
  )
  
  // Verify invalid values are handled
  assert_eq(LogRecord::trace_id(invalid_log), Some(invalid_trace_id))
  assert_eq(LogRecord::span_id(invalid_log), Some(invalid_span_id))
  
  // Test log emission with various edge cases
  Logger::emit(logger, empty_body_log)
  Logger::emit(logger, long_body_log)
  Logger::emit(logger, special_body_log)
  Logger::emit(logger, invalid_log)
  
  // Test logger with empty name
  let empty_logger = LoggerProvider::get_logger(logger_provider, "")
  assert_eq(empty_logger.scope.name, "")
  
  // Test logger with very long name
  let very_long_logger_name = "this.is.a.very.long.logger.name.that.exceeds.normal.limits.and.tests.error.boundaries." +
                              "it.contains.many.characters.to.stress.test.the.system.1234567890"
  let long_logger = LoggerProvider::get_logger(logger_provider, very_long_logger_name)
  assert_eq(long_logger.scope.name, very_long_logger_name)
  
  // Test logger with special characters in name
  let special_logger = LoggerProvider::get_logger(logger_provider, "logger.with.special.chars!@#$%^&*()测试😀")
  assert_eq(special_logger.scope.name, "logger.with.special.chars!@#$%^&*()测试😀")
}

// Test 5: Context and baggage error boundaries
pub test "premium context baggage error boundaries" {
  // Test context operations with edge cases
  let root_ctx = Context::root()
  
  // Test context with empty key
  let empty_key = ContextKey::new("")
  let ctx_with_empty_key = Context::with_value(root_ctx, empty_key, "empty.key.value")
  let empty_key_value = Context::get(ctx_with_empty_key, empty_key)
  assert_eq(empty_key_value, Some("empty.key.value"))
  
  // Test context with very long key
  let very_long_key = "this.is.a.very.long.context.key.that.exceeds.normal.limits.and.tests.error.boundaries." +
                     "it.contains.many.characters.to.stress.test.the.system.1234567890"
  let long_key = ContextKey::new(very_long_key)
  let ctx_with_long_key = Context::with_value(root_ctx, long_key, "long.key.value")
  let long_key_value = Context::get(ctx_with_long_key, long_key)
  assert_eq(long_key_value, Some("long.key.value"))
  
  // Test context with special characters in key
  let special_key = ContextKey::new("context.key.with.special.chars!@#$%^&*()测试😀")
  let ctx_with_special_key = Context::with_value(root_ctx, special_key, "special.key.value")
  let special_key_value = Context::get(ctx_with_special_key, special_key)
  assert_eq(special_key_value, Some("special.key.value"))
  
  // Test context with empty value
  let empty_value_key = ContextKey::new("empty.value.key")
  let ctx_with_empty_value = Context::with_value(root_ctx, empty_value_key, "")
  let empty_value_result = Context::get(ctx_with_empty_value, empty_value_key)
  assert_eq(empty_value_result, Some(""))
  
  // Test context with very long value
  let very_long_value = "This.is.a.very.long.context.value.that.exceeds.normal.limits.and.tests.error.boundaries." +
                       "it.contains.many.characters.to.stress.test.the.system." +
                       "1234567890".repeat(1000)
  let long_value_key = ContextKey::new("long.value.key")
  let ctx_with_long_value = Context::with_value(root_ctx, long_value_key, very_long_value)
  let long_value_result = Context::get(ctx_with_long_value, long_value_key)
  assert_eq(long_value_result, Some(very_long_value))
  
  // Test context with special characters in value
  let special_value_key = ContextKey::new("special.value.key")
  let ctx_with_special_value = Context::with_value(root_ctx, special_value_key, "special.value!@#$%^&*()测试😀\n\t\0")
  let special_value_result = Context::get(ctx_with_special_value, special_value_key)
  assert_eq(special_value_result, Some("special.value!@#$%^&*()测试😀\n\t\0"))
  
  // Test baggage operations with edge cases
  let baggage = Baggage::new()
  
  // Test baggage with empty key
  let baggage_with_empty_key = Baggage::set_entry(baggage, "", "empty.key.value")
  let empty_key_baggage_value = Baggage::get_entry(baggage_with_empty_key, "")
  assert_eq(empty_key_baggage_value, Some("empty.key.value"))
  
  // Test baggage with very long key
  let baggage_with_long_key = Baggage::set_entry(baggage, very_long_key, "long.key.value")
  let long_key_baggage_value = Baggage::get_entry(baggage_with_long_key, very_long_key)
  assert_eq(long_key_baggage_value, Some("long.key.value"))
  
  // Test baggage with special characters in key
  let baggage_with_special_key = Baggage::set_entry(baggage, "context.key.with.special.chars!@#$%^&*()测试😀", "special.key.value")
  let special_key_baggage_value = Baggage::get_entry(baggage_with_special_key, "context.key.with.special.chars!@#$%^&*()测试😀")
  assert_eq(special_key_baggage_value, Some("special.key.value"))
  
  // Test baggage with empty value
  let baggage_with_empty_value = Baggage::set_entry(baggage, "empty.value.key", "")
  let empty_value_baggage_result = Baggage::get_entry(baggage_with_empty_value, "empty.value.key")
  assert_eq(empty_value_baggage_result, Some(""))
  
  // Test baggage with very long value
  let baggage_with_long_value = Baggage::set_entry(baggage, "long.value.key", very_long_value)
  let long_value_baggage_result = Baggage::get_entry(baggage_with_long_value, "long.value.key")
  assert_eq(long_value_baggage_result, Some(very_long_value))
  
  // Test baggage with special characters in value
  let baggage_with_special_value = Baggage::set_entry(baggage, "special.value.key", "special.value!@#$%^&*()测试😀\n\t\0")
  let special_value_baggage_result = Baggage::get_entry(baggage_with_special_value, "special.value.key")
  assert_eq(special_value_baggage_result, Some("special.value!@#$%^&*()测试😀\n\t\0"))
  
  // Test baggage removal with non-existent key
  let baggage_after_remove = Baggage::remove_entry(baggage, "non.existent.key")
  let non_existent_value = Baggage::get_entry(baggage_after_remove, "non.existent.key")
  assert_eq(non_existent_value, None)
}

// Test 6: Propagator operation error boundaries
pub test "premium propagator error boundaries" {
  // Test propagator operations with edge cases
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Test injection with empty carrier
  let empty_carrier = TextMapCarrier::new()
  let ctx = Context::root()
  CompositePropagator::inject(composite_propagator, ctx, empty_carrier)
  
  // Test extraction from empty carrier
  let extracted_from_empty = CompositePropagator::extract(composite_propagator, empty_carrier)
  let key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_from_empty, key)
  assert_eq(extracted_value, Some("true"))
  
  // Test carrier operations with edge cases
  let carrier = TextMapCarrier::new()
  
  // Test setting empty key
  TextMapCarrier::set(carrier, "", "empty.key.value")
  let empty_key_result = TextMapCarrier::get(carrier, "")
  assert_eq(empty_key_result, Some("empty.key.value"))
  
  // Test setting very long key
  let very_long_key = "this.is.a.very.long.carrier.key.that.exceeds.normal.limits.and.tests.error.boundaries." +
                     "it.contains.many.characters.to.stress.test.the.system.1234567890"
  TextMapCarrier::set(carrier, very_long_key, "long.key.value")
  let long_key_result = TextMapCarrier::get(carrier, very_long_key)
  assert_eq(long_key_result, Some("long.key.value"))
  
  // Test setting special characters in key
  TextMapCarrier::set(carrier, "carrier.key.with.special.chars!@#$%^&*()测试😀", "special.key.value")
  let special_key_result = TextMapCarrier::get(carrier, "carrier.key.with.special.chars!@#$%^&*()测试😀")
  assert_eq(special_key_result, Some("special.key.value"))
  
  // Test setting empty value
  TextMapCarrier::set(carrier, "empty.value.key", "")
  let empty_value_result = TextMapCarrier::get(carrier, "empty.value.key")
  assert_eq(empty_value_result, Some(""))
  
  // Test setting very long value
  let very_long_value = "This.is.a.very.long.carrier.value.that.exceeds.normal.limits.and.tests.error.boundaries." +
                       "it.contains.many.characters.to.stress.test.the.system." +
                       "1234567890".repeat(1000)
  TextMapCarrier::set(carrier, "long.value.key", very_long_value)
  let long_value_result = TextMapCarrier::get(carrier, "long.value.key")
  assert_eq(long_value_result, Some(very_long_value))
  
  // Test setting special characters in value
  TextMapCarrier::set(carrier, "special.value.key", "special.value!@#$%^&*()测试😀\n\t\0")
  let special_value_result = TextMapCarrier::get(carrier, "special.value.key")
  assert_eq(special_value_result, Some("special.value!@#$%^&*()测试😀\n\t\0"))
  
  // Test getting non-existent key
  let non_existent_result = TextMapCarrier::get(carrier, "non.existent.key")
  assert_eq(non_existent_result, None)
  
  // Test composite propagator with empty propagators
  let empty_composite = CompositePropagator::new([])
  CompositePropagator::inject(empty_composite, ctx, carrier)
  let extracted_from_empty_composite = CompositePropagator::extract(empty_composite, carrier)
  let empty_composite_value = Context::get(extracted_from_empty_composite, key)
  assert_eq(empty_composite_value, Some("true"))
}

// Test 7: Resource operation error boundaries
pub test "premium resource error boundaries" {
  // Test resource operations with edge cases
  let resource = Resource::new()
  
  // Test setting attributes with empty key
  let resource_with_empty_key = Resource::with_attributes(resource, [("", StringValue("empty.key.value"))])
  let empty_key_resource_value = Resource::get_attribute(resource_with_empty_key, "")
  assert_eq(empty_key_resource_value, Some(StringValue("empty.key.value")))
  
  // Test setting attributes with very long key
  let very_long_key = "this.is.a.very.long.resource.key.that.exceeds.normal.limits.and.tests.error.boundaries." +
                     "it.contains.many.characters.to.stress.test.the.system.1234567890"
  let resource_with_long_key = Resource::with_attributes(resource, [(very_long_key, StringValue("long.key.value"))])
  let long_key_resource_value = Resource::get_attribute(resource_with_long_key, very_long_key)
  assert_eq(long_key_resource_value, Some(StringValue("long.key.value")))
  
  // Test setting attributes with special characters in key
  let resource_with_special_key = Resource::with_attributes(resource, [("resource.key.with.special.chars!@#$%^&*()测试😀", StringValue("special.key.value"))])
  let special_key_resource_value = Resource::get_attribute(resource_with_special_key, "resource.key.with.special.chars!@#$%^&*()测试😀")
  assert_eq(special_key_resource_value, Some(StringValue("special.key.value")))
  
  // Test setting attributes with extreme values
  let resource_with_extreme_values = Resource::with_attributes(resource, [
    ("max.int", IntValue(9223372036854775807)),  // Max Int64
    ("min.int", IntValue(-9223372036854775808)),  // Min Int64
    ("max.float", FloatValue(1.7976931348623157e+308)),  // Max Double
    ("min.float", FloatValue(-1.7976931348623157e+308)),  // Min Double
    ("infinity", FloatValue(1.0/0.0)),  // Infinity
    ("neg.infinity", FloatValue(-1.0/0.0)),  // Negative Infinity
    ("nan", FloatValue(0.0/0.0)),  // NaN
    ("empty.string", StringValue("")),
    ("very.long.string", StringValue("1234567890".repeat(1000))),
    ("special.string", StringValue("!@#$%^&*()测试😀\n\t\0"))
  ])
  
  // Verify extreme values are handled
  assert_true(Resource::get_attribute(resource_with_extreme_values, "max.int").is_some())
  assert_true(Resource::get_attribute(resource_with_extreme_values, "min.int").is_some())
  assert_true(Resource::get_attribute(resource_with_extreme_values, "max.float").is_some())
  assert_true(Resource::get_attribute(resource_with_extreme_values, "min.float").is_some())
  assert_true(Resource::get_attribute(resource_with_extreme_values, "infinity").is_some())
  assert_true(Resource::get_attribute(resource_with_extreme_values, "neg.infinity").is_some())
  assert_true(Resource::get_attribute(resource_with_extreme_values, "nan").is_some())
  assert_eq(Resource::get_attribute(resource_with_extreme_values, "empty.string"), Some(StringValue("")))
  assert_eq(Resource::get_attribute(resource_with_extreme_values, "very.long.string"), Some(StringValue("1234567890".repeat(1000))))
  assert_eq(Resource::get_attribute(resource_with_extreme_values, "special.string"), Some(StringValue("!@#$%^&*()测试😀\n\t\0")))
  
  // Test resource merging with edge cases
  let empty_resource1 = Resource::new()
  let empty_resource2 = Resource::new()
  let merged_empty = Resource::merge(empty_resource1, empty_resource2)
  
  let resource_with_attrs = Resource::with_attributes(resource, [("key1", StringValue("value1"))])
  let empty_override = Resource::new()
  let merged_with_empty = Resource::merge(resource_with_attrs, empty_override)
  
  let attrs_resource = Resource::with_attributes(resource, [("key1", StringValue("value1")), ("key2", StringValue("value2"))])
  let override_resource = Resource::with_attributes(resource, [("key2", StringValue("overridden")), ("key3", StringValue("new"))])
  let merged_attrs = Resource::merge(attrs_resource, override_resource)
  
  // Verify merge results
  assert_eq(Resource::get_attribute(merged_with_empty, "key1"), Some(StringValue("value1")))
  assert_eq(Resource::get_attribute(merged_attrs, "key1"), Some(StringValue("value1")))
  assert_eq(Resource::get_attribute(merged_attrs, "key2"), Some(StringValue("overridden")))
  assert_eq(Resource::get_attribute(merged_attrs, "key3"), Some(StringValue("new")))
  
  // Test getting non-existent attribute
  let non_existent_attr = Resource::get_attribute(resource, "non.existent.key")
  assert_eq(non_existent_attr, None)
}