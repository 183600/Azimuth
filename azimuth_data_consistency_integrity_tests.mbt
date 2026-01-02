test "azimuth_data_consistency_integrity_tests" {
  // Test 1: Trace ID and Span ID consistency across telemetry signals
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "consistency_test")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "consistency_test")
  
  // Create a span and verify trace/span ID consistency
  let test_span = Tracer::start_span(tracer, "consistency_check")
  let span_ctx = Span::span_context(test_span)
  let original_trace_id = SpanContext::trace_id(span_ctx)
  let original_span_id = SpanContext::span_id(span_ctx)
  
  // Create log records with same trace/span IDs
  let log1 = LogRecord::new_with_context(
    Info,
    Some("Log 1"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(original_trace_id),
    Some(original_span_id),
    None
  )
  
  let log2 = LogRecord::new_with_context(
    Warn,
    Some("Log 2"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(original_trace_id),
    Some(original_span_id),
    None
  )
  
  // Verify trace and span IDs are consistent
  @assert(original_trace_id == SpanContext::trace_id(span_ctx))
  @assert(original_span_id == SpanContext::span_id(span_ctx))
  @assert(log1.trace_id == Some(original_trace_id))
  @assert(log1.span_id == Some(original_span_id))
  @assert(log2.trace_id == Some(original_trace_id))
  @assert(log2.span_id == Some(original_span_id))
  
  Span::end(test_span)
  
  // Test 2: Attribute value type consistency
  let attrs = Attributes::new()
  
  // Set attributes with different types
  Attributes::set(attrs, "string_key", StringValue("string_value"))
  Attributes::set(attrs, "int_key", IntValue(42))
  Attributes::set(attrs, "float_key", FloatValue(3.14159))
  Attributes::set(attrs, "bool_key", BoolValue(true))
  
  // Retrieve and verify type consistency
  let string_value = Attributes::get(attrs, "string_key")
  let int_value = Attributes::get(attrs, "int_key")
  let float_value = Attributes::get(attrs, "float_key")
  let bool_value = Attributes::get(attrs, "bool_key")
  
  @assert(match string_value {
    Some(StringValue(v)) => v == "string_value"
    _ => false
  })
  
  @assert(match int_value {
    Some(IntValue(v)) => v == 42
    _ => false
  })
  
  @assert(match float_value {
    Some(FloatValue(v)) => v == 3.14159
    _ => false
  })
  
  @assert(match bool_value {
    Some(BoolValue(v)) => v == true
    _ => false
  })
  
  // Test 3: Resource attribute merge consistency
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base_service")),
    ("service.version", StringValue("1.0.0")),
    ("common.attr", StringValue("base_value"))
  ]
  let populated_base = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override_service")),
    ("new.attr", StringValue("new_value")),
    ("common.attr", StringValue("override_value"))
  ]
  let populated_override = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged_resource = Resource::merge(populated_base, populated_override)
  
  // Verify merge consistency
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let new_attr = Resource::get_attribute(merged_resource, "new.attr")
  let common_attr = Resource::get_attribute(merged_resource, "common.attr")
  
  @assert(match service_name {
    Some(StringValue(v)) => v == "override_service"
    _ => false
  })
  
  @assert(match new_attr {
    Some(StringValue(v)) => v == "new_value"
    _ => false
  })
  
  @assert(match common_attr {
    Some(StringValue(v)) => v == "override_value"
    _ => false
  })
  
  // Test 4: Context propagation consistency
  let root_ctx = Context::root()
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  
  // Build context chain
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  
  // Verify context consistency
  let value1_from_ctx2 = Context::get(ctx2, key1)
  let value2_from_ctx2 = Context::get(ctx2, key2)
  
  @assert(value1_from_ctx2 == Some("value1"))
  @assert(value2_from_ctx2 == Some("value2"))
  
  // Test 5: Baggage entry consistency
  let baggage = Baggage::new()
  
  // Add baggage entries
  let baggage1 = Baggage::set_entry(baggage, "user_id", "12345")
  let baggage2 = Baggage::set_entry(baggage1, "session_id", "session-67890")
  let baggage3 = Baggage::set_entry(baggage2, "request_id", "req-abcdef")
  
  // Verify baggage consistency
  let user_id = Baggage::get_entry(baggage3, "user_id")
  let session_id = Baggage::get_entry(baggage3, "session_id")
  let request_id = Baggage::get_entry(baggage3, "request_id")
  
  @assert(user_id == Some("12345"))
  @assert(session_id == Some("session-67890"))
  @assert(request_id == Some("req-abcdef"))
  
  // Test 6: Instrument name consistency across metric types
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "consistency_test")
  
  let instrument_name = "test_instrument"
  let description = Some("Test instrument for consistency")
  let unit = Some("unit")
  
  // Create different instrument types with same name
  let counter = Meter::create_counter(meter, instrument_name, description, unit)
  let histogram = Meter::create_histogram(meter, instrument_name, description, unit)
  let updown_counter = Meter::create_updown_counter(meter, instrument_name, description, unit)
  let gauge = Meter::create_gauge(meter, instrument_name, description, unit)
  
  // Convert to Instrument type and verify name consistency
  let counter_instrument = Counter(instrument_name, description, unit)
  let histogram_instrument = Histogram::as_instrument(histogram)
  
  @assert(Instrument::name(counter_instrument) == instrument_name)
  @assert(Instrument::name(histogram_instrument) == instrument_name)
  @assert(Instrument::description(counter_instrument) == description)
  @assert(Instrument::description(histogram_instrument) == description)
  @assert(Instrument::unit(counter_instrument) == unit)
  @assert(Instrument::unit(histogram_instrument) == unit)
  
  // Test 7: Log record severity consistency
  let base_timestamp = Clock::now_unix_nanos(Clock::system())
  
  // Create log records with different severity levels
  let trace_log = LogRecord::new_with_context(
    Trace,
    Some("Trace message"),
    None,
    Some(base_timestamp),
    None,
    None,
    None,
    None
  )
  
  let debug_log = LogRecord::new_with_context(
    Debug,
    Some("Debug message"),
    None,
    Some(base_timestamp + 1000L),
    None,
    None,
    None,
    None
  )
  
  let info_log = LogRecord::new_with_context(
    Info,
    Some("Info message"),
    None,
    Some(base_timestamp + 2000L),
    None,
    None,
    None,
    None
  )
  
  let warn_log = LogRecord::new_with_context(
    Warn,
    Some("Warning message"),
    None,
    Some(base_timestamp + 3000L),
    None,
    None,
    None,
    None
  )
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Error message"),
    None,
    Some(base_timestamp + 4000L),
    None,
    None,
    None,
    None
  )
  
  let fatal_log = LogRecord::new_with_context(
    Fatal,
    Some("Fatal message"),
    None,
    Some(base_timestamp + 5000L),
    None,
    None,
    None,
    None
  )
  
  // Verify severity consistency
  @assert(match LogRecord::severity_number(trace_log) { Trace => true _ => false })
  @assert(match LogRecord::severity_number(debug_log) { Debug => true _ => false })
  @assert(match LogRecord::severity_number(info_log) { Info => true _ => false })
  @assert(match LogRecord::severity_number(warn_log) { Warn => true _ => false })
  @assert(match LogRecord::severity_number(error_log) { Error => true _ => false })
  @assert(match LogRecord::severity_number(fatal_log) { Fatal => true _ => false })
  
  // Verify timestamp consistency
  @assert(trace_log.timestamp == Some(base_timestamp))
  @assert(debug_log.timestamp == Some(base_timestamp + 1000L))
  @assert(info_log.timestamp == Some(base_timestamp + 2000L))
  @assert(warn_log.timestamp == Some(base_timestamp + 3000L))
  @assert(error_log.timestamp == Some(base_timestamp + 4000L))
  @assert(fatal_log.timestamp == Some(base_timestamp + 5000L))
  
  // Test 8: Span status and event consistency
  let status_span = Tracer::start_span(tracer, "status_consistency_test")
  
  // Set status and add events
  Span::set_status(status_span, Ok, Some("Operation started"))
  Span::add_event(status_span, "Event 1", Some([("key1", StringValue("value1"))]))
  Span::add_event(status_span, "Event 2", Some([("key2", StringValue("value2"))]))
  Span::set_status(status_span, Error, Some("Operation failed"))
  
  // Verify status consistency
  @assert(match Span::status(status_span) {
    Error => true
    _ => false
  })
  
  Span::end(status_span)
  
  // Test 9: Instrumentation scope consistency across providers
  let scope_name = "consistency.test.scope"
  let scope_version = Some("1.0.0")
  
  let consistent_tracer = TracerProvider::get_tracer(tracer_provider, scope_name, scope_version)
  let consistent_meter = MeterProvider::get_meter(meter_provider, scope_name)
  let consistent_logger = LoggerProvider::get_logger(logger_provider, scope_name)
  
  // Verify scope consistency
  let tracer_scope = Tracer::instrumentation_scope(consistent_tracer)
  let meter_scope = Meter::scope(consistent_meter)
  let logger_scope = Logger::scope(consistent_logger)
  
  @assert(tracer_scope.name == scope_name)
  @assert(meter_scope.name == scope_name)
  @assert(logger_scope.name == scope_name)
  @assert(tracer_scope.version == scope_version)
}