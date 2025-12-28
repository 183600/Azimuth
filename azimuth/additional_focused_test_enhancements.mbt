// Additional Focused Test Enhancements for Azimuth Telemetry System
// High-quality test cases covering error handling, performance, and integration scenarios

test "attribute_value_edge_cases" {
  // Test empty string attribute
  let empty_string = StringValue("")
  match empty_string {
    StringValue(s) => assert_eq(s, "")
    _ => panic("Expected empty StringValue")
  }
  
  // Test zero values
  let zero_int = IntValue(0)
  let zero_float = FloatValue(0.0)
  let false_bool = BoolValue(false)
  
  match zero_int {
    IntValue(i) => assert_eq(i, 0)
    _ => panic("Expected zero IntValue")
  }
  
  match zero_float {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => panic("Expected zero FloatValue")
  }
  
  match false_bool {
    BoolValue(b) => assert_eq(b, false)
    _ => panic("Expected false BoolValue")
  }
  
  // Test empty arrays
  let empty_string_array = ArrayStringValue([])
  let empty_int_array = ArrayIntValue([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length, 0)
    _ => panic("Expected empty ArrayStringValue")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length, 0)
    _ => panic("Expected empty ArrayIntValue")
  }
}

test "span_context_character_validation" {
  // Test span context with special characters
  let special_chars_ctx = SpanContext::new("trace-123_456.789", "span-abc_def.123", true, "state=value;key=value2")
  assert_eq(SpanContext::is_valid(special_chars_ctx), true)
  assert_eq(SpanContext::trace_id(special_chars_ctx), "trace-123_456.789")
  assert_eq(SpanContext::span_id(special_chars_ctx), "span-abc_def.123")
  
  // Test span context with hexadecimal characters (common in real implementations)
  let hex_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_eq(SpanContext::is_valid(hex_ctx), true)
  assert_eq(SpanContext::trace_id(hex_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(hex_ctx), "b7ad6b7169203331")
}

test "context_nested_operations" {
  let root_ctx = Context::root()
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  let key3 = ContextKey::new("key3")
  
  // Test nested context operations
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  let ctx3 = Context::with_value(ctx2, key3, "value3")
  
  assert_eq(Context::get(ctx3, key1), Some("value1"))
  assert_eq(Context::get(ctx3, key2), Some("value2"))
  assert_eq(Context::get(ctx3, key3), Some("value3"))
  
  // Test that earlier contexts are unaffected
  assert_eq(Context::get(ctx2, key3), None)
  assert_eq(Context::get(ctx1, key2), None)
}

test "instrument_type_variations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_instruments")
  
  // Test all instrument types with different configurations
  let counter = Meter::create_counter(meter, "request_count", Some("Total number of requests"), Some("requests"))
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response time distribution"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "active_connections", Some("Currently active connections"), Some("connections"))
  let gauge = Meter::create_gauge(meter, "memory_usage", Some("Current memory usage"), Some("bytes"))
  
  // Test instrument operations
  Counter::add(counter, 1.0, None)
  Counter::add(counter, 5.0, None)
  
  Histogram::record(histogram, 100.5, None)
  Histogram::record(histogram, 250.75, None)
  
  UpDownCounter::add(updown_counter, 10.0, None)
  UpDownCounter::add(updown_counter, -3.0, None)
  
  Gauge::add(gauge, 1024.0, None)
  
  // Verify instrument properties
  assert_eq(counter.name, "request_count")
  assert_eq(histogram.name, "response_time")
  assert_eq(updown_counter.name, "active_connections")
  assert_eq(gauge.name, "memory_usage")
}

test "log_record_severity_levels" {
  // Test all severity levels
  let trace_log = LogRecord::new(Trace, "Debug trace message")
  let debug_log = LogRecord::new(Debug, "Debug information")
  let info_log = LogRecord::new(Info, "General information")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error occurred")
  let fatal_log = LogRecord::new(Fatal, "Fatal error")
  
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // Test log with empty body
  let empty_body_log = LogRecord::new(Info, "")
  assert_eq(LogRecord::body(empty_body_log), Some(""))
}

test "span_kind_and_status_transitions" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test_span_types")
  
  // Test different span kinds
  let internal_span = Tracer::start_span(tracer, "internal_operation")
  let server_span = Tracer::start_span(tracer, "server_request")
  let client_span = Tracer::start_span(tracer, "client_request")
  let producer_span = Tracer::start_span(tracer, "message_producer")
  let consumer_span = Tracer::start_span(tracer, "message_consumer")
  
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Internal)  // Default is Internal
  assert_eq(Span::kind(client_span), Internal)
  assert_eq(Span::kind(producer_span), Internal)
  assert_eq(Span::kind(consumer_span), Internal)
  
  // Test status transitions
  Span::set_status(internal_span, Unset, None)
  Span::set_status(server_span, Ok, Some("Request processed successfully"))
  Span::set_status(client_span, Error, Some("Connection failed"))
  
  // Test event addition with attributes
  let event_attrs = [("event.type", StringValue("error")), ("error.code", IntValue(500))]
  Span::add_event(client_span, "connection_error", Some(event_attrs))
  
  // End all spans
  Span::end(internal_span)
  Span::end(server_span)
  Span::end(client_span)
  Span::end(producer_span)
  Span::end(consumer_span)
}

test "resource_attribute_complex_operations" {
  let base_resource = Resource::new()
  
  // Test resource with mixed attribute types
  let mixed_attrs = [
    ("string.attr", StringValue("string_value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.string", ArrayStringValue(["item1", "item2", "item3"])),
    ("array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  let resource_with_mixed = Resource::with_attributes(base_resource, mixed_attrs)
  
  // Test retrieval of all attribute types
  assert_eq(Resource::get_attribute(resource_with_mixed, "string.attr"), Some(StringValue("string_value")))
  assert_eq(Resource::get_attribute(resource_with_mixed, "int.attr"), Some(IntValue(42)))
  assert_eq(Resource::get_attribute(resource_with_mixed, "float.attr"), Some(FloatValue(3.14159)))
  assert_eq(Resource::get_attribute(resource_with_mixed, "bool.attr"), Some(BoolValue(true)))
  assert_eq(Resource::get_attribute(resource_with_mixed, "array.string"), Some(ArrayStringValue(["item1", "item2", "item3"])))
  assert_eq(Resource::get_attribute(resource_with_mixed, "array.int"), Some(ArrayIntValue([1, 2, 3, 4, 5])))
  
  // Test resource merging
  let override_attrs = [
    ("string.attr", StringValue("overridden_value")),
    ("new.attr", StringValue("new_value"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource_with_mixed, override_resource)
  
  assert_eq(Resource::get_attribute(merged_resource, "string.attr"), Some(StringValue("overridden_value")))
  assert_eq(Resource::get_attribute(merged_resource, "new.attr"), Some(StringValue("new_value")))
}

test "telemetry_system_integration" {
  // Create a complete telemetry stack
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "integration_test", Some("1.0.0"))
  let meter = MeterProvider::get_meter(meter_provider, "integration_test", Some("1.0.0"))
  let logger = LoggerProvider::get_logger(logger_provider, "integration_test", Some("1.0.0"))
  
  // Create instrumentation scope
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "integration_test")
  assert_eq(scope.version, Some("1.0.0"))
  
  // Simulate a complete operation with tracing, metrics, and logging
  let span = Tracer::start_span(tracer, "complete_operation")
  let counter = Meter::create_counter(meter, "operations_total", Some("Total operations"), Some("operations"))
  let histogram = Meter::create_histogram(meter, "operation_duration", Some("Operation duration"), Some("ms"))
  
  // Record metrics
  Counter::add(counter, 1.0, None)
  Histogram::record(histogram, 150.5, None)
  
  // Add span events
  Span::add_event(span, "operation_started", None)
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Emit logs
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Operation started"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    None
  )
  
  let end_log = LogRecord::new_with_context(
    Info,
    Some("Operation completed"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    None
  )
  
  Logger::emit(logger, start_log)
  Logger::emit(logger, end_log)
  
  // End the span
  Span::end(span)
  
  // Verify all components are working
  assert_eq(Span::name(span), "complete_operation")
  assert_eq(counter.name, "operations_total")
  assert_eq(histogram.name, "operation_duration")
  assert_eq(LogRecord::body(start_log), Some("Operation started"))
  assert_eq(LogRecord::body(end_log), Some("Operation completed"))
}