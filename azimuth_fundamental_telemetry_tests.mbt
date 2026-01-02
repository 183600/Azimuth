// Fundamental Telemetry Tests for Azimuth
// This file contains essential test cases for core telemetry functionality

test "attribute value type conversion" {
  // Test string attribute value
  let string_attr = StringValue("test_value")
  match string_attr {
    StringValue(v) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  // Test integer attribute value
  let int_attr = IntValue(42)
  match int_attr {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test float attribute value
  let float_attr = FloatValue(3.14)
  match float_attr {
    FloatValue(v) => assert_eq(v, 3.14)
    _ => assert_true(false)
  }
  
  // Test boolean attribute value
  let bool_attr = BoolValue(true)
  match bool_attr {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
}

test "attributes basic operations" {
  let attrs = Attributes::new()
  
  // Test setting and getting attributes
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  
  match Attributes::get(attrs, "string.key") {
    Some(StringValue(v)) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "int.key") {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test getting non-existent attribute
  match Attributes::get(attrs, "nonexistent.key") {
    None => assert_true(true)
    _ => assert_true(false)
  }
}

test "span context lifecycle" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Test trace ID
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  
  // Test span ID
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  
  // Test validity
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Test sampling
  assert_true(SpanContext::is_sampled(span_ctx))
  
  // Test invalid context
  let invalid_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_ctx))
}

test "span basic operations" {
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Test span name
  assert_eq(Span::name(span), "test_span")
  
  // Test span kind
  match Span::kind(span) {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test recording state
  assert_true(Span::is_recording(span))
  
  // Test span context
  let ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(ctx), "trace_id")
  assert_eq(SpanContext::span_id(ctx), "span_id")
  
  // Test status operations
  assert_eq(Span::status(span), Unset)
  Span::set_status(span, Ok)
  // Note: simplified implementation doesn't actually change status
  
  // Test event operations
  Span::add_event(span, "test_event", None)
  
  // Test span ending
  Span::end(span)
}

test "tracer provider functionality" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test_tracer", Some("1.0.0"))
  
  // Test instrumentation scope
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test_tracer")
  assert_eq(scope.version, Some("1.0.0"))
  
  // Test span creation
  let span = Tracer::start_span(tracer, "test_span", None)
  assert_eq(Span::name(span), "test_span")
  
  // Test span context
  let ctx = Span::span_context(span)
  assert_true(SpanContext::is_valid(ctx))
}

test "meter and counter operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // Test counter creation
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  
  // Test counter properties
  assert_eq(counter.name, "test_counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  // Test counter operations
  Counter::add(counter, 1.0, None)
  Counter::add(counter, 2.5, Some(Attributes::new()))
  
  // Test instrument type operations
  let instrument = Counter("test_instrument", Some("Test instrument"), Some("unit"))
  assert_eq(Instrument::name(instrument), "test_instrument")
  assert_eq(Instrument::description(instrument), Some("Test instrument"))
  assert_eq(Instrument::unit(instrument), Some("unit"))
}

test "histogram operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // Test histogram creation
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  
  // Test histogram properties
  assert_eq(histogram.name, "test_histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test histogram operations
  Histogram::record(histogram, 100.0, None)
  Histogram::record(histogram, 200.5, Some(Attributes::new()))
  
  // Test histogram as instrument
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "test_histogram")
}

test "logger and log record operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test_logger")
  
  // Test log record creation
  let record = LogRecord::new(Info, "Test log message")
  
  // Test log record properties
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  
  // Test log record with context
  let record_with_ctx = LogRecord::new_with_context(
    Warn,
    Some("Warning message"),
    Some(Attributes::new()),
    Some(1640995200000000000L),
    Some(1640995201000000000L),
    Some("trace_id"),
    Some("span_id"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(record_with_ctx), Warn)
  assert_eq(LogRecord::body(record_with_ctx), Some("Warning message"))
  assert_eq(LogRecord::trace_id(record_with_ctx), Some("trace_id"))
  assert_eq(LogRecord::span_id(record_with_ctx), Some("span_id"))
  
  // Test logger emit
  Logger::emit(logger, record)
  Logger::emit(logger, record_with_ctx)
}

test "context operations" {
  let root_ctx = Context::root()
  let key = ContextKey::new("test_key")
  
  // Test context with value
  let ctx_with_value = Context::with_value(root_ctx, key, "test_value")
  
  // Test context get
  match Context::get(ctx_with_value, key) {
    Some(v) => assert_eq(v, "test_value")
    None => assert_true(false)
  }
  
  // Test context get with non-existent key
  let non_existent_key = ContextKey::new("non_existent")
  match Context::get(ctx_with_value, non_existent_key) {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
}

test "baggage operations" {
  let baggage = Baggage::new()
  
  // Test baggage set and get
  let updated_baggage = Baggage::set_entry(baggage, "key1", "value1")
  
  match Baggage::get_entry(updated_baggage, "key1") {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // Test baggage get non-existent entry
  match Baggage::get_entry(updated_baggage, "non_existent") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test baggage remove
  let final_baggage = Baggage::remove_entry(updated_baggage, "key1")
  match Baggage::get_entry(final_baggage, "key1") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
}

test "resource operations" {
  let resource = Resource::new()
  
  // Test resource with attributes
  let attrs = [("service.name", StringValue("test_service")), ("service.version", StringValue("1.0.0"))]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Test resource get attribute
  match Resource::get_attribute(resource_with_attrs, "service.name") {
    Some(StringValue(v)) => assert_eq(v, "test_service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "service.version") {
    Some(StringValue(v)) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
  
  // Test resource merge
  let override_resource = Resource::with_attributes(Resource::new(), [("env", StringValue("test"))])
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  match Resource::get_attribute(merged_resource, "env") {
    Some(StringValue(v)) => assert_eq(v, "test")
    _ => assert_true(false)
  }
}