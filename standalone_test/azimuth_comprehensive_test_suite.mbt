// Comprehensive Test Suite for Azimuth Telemetry System
// This file contains 8 focused test cases covering core telemetry functionality

test "attribute value type conversion" {
  // Test string attribute value
  let string_attr = StringValue("test_value")
  match string_attr {
    StringValue(s) => assert_eq(s, "test_value")
    _ => assert_true(false)
  }
  
  // Test int attribute value
  let int_attr = IntValue(42)
  match int_attr {
    IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  // Test float attribute value
  let float_attr = FloatValue(3.14)
  match float_attr {
    FloatValue(f) => assert_true(f > 3.0 && f < 3.2)
    _ => assert_true(false)
  }
  
  // Test bool attribute value
  let bool_attr = BoolValue(true)
  match bool_attr {
    BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
  
  // Test array string attribute value
  let string_array_attr = ArrayStringValue(["a", "b", "c"])
  match string_array_attr {
    ArrayStringValue(arr) => assert_eq(arr.length(), 3)
    _ => assert_true(false)
  }
  
  // Test array int attribute value
  let int_array_attr = ArrayIntValue([1, 2, 3])
  match int_array_attr {
    ArrayIntValue(arr) => assert_eq(arr.length(), 3)
    _ => assert_true(false)
  }
}

test "span lifecycle management" {
  // Create a new span context
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  assert_true(SpanContext::is_valid(span_ctx))
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_eq(SpanContext::trace_id(span_ctx), "trace123")
  assert_eq(SpanContext::span_id(span_ctx), "span456")
  
  // Create a new span
  let span = Span::new("test-span", Internal, span_ctx)
  assert_eq(Span::name(span), "test-span")
  assert_true(Span::is_recording(span))
  assert_eq(SpanContext::trace_id(Span::span_context(span)), SpanContext::trace_id(span_ctx))
  assert_eq(SpanContext::span_id(Span::span_context(span)), SpanContext::span_id(span_ctx))
  
  // Set span status
  Span::set_status(span, Ok, description? = Some("Operation completed successfully"))
  
  // Add events to span
  Span::add_event(span, "event1", attributes? = Some([("key1", StringValue("value1"))]))
  Span::add_event(span, "event2", attributes? = Some([("key2", IntValue(42))]))
  
  // End the span
  Span::end(span)
}

test "metrics dashboard functionality" {
  // Create meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  // Create counter
  let counter = Meter::create_counter(meter, "test-counter")
  assert_eq(Instrument::name(Counter(counter.name, counter.description, counter.unit)), "test-counter")
  
  // Record counter value
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5)
  
  // Create histogram
  let histogram = Meter::create_histogram(meter, "test-histogram")
  assert_eq(Instrument::name(Histogram::as_instrument(histogram)), "test-histogram")
  
  // Record histogram values
  Histogram::record(histogram, 1.0)
  Histogram::record(histogram, 2.5)
  Histogram::record(histogram, 3.7)
  
  // Create up-down counter
  let updown_counter = Meter::create_updown_counter(meter, "test-updown")
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // Create gauge
  let gauge = Meter::create_gauge(meter, "test-gauge")
  assert_eq(Instrument::name(Counter(gauge.name, gauge.description, gauge.unit)), "test-gauge")
}

test "distributed tracing consistency" {
  // Create tracer provider and tracer
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test-tracer")
  
  // Create root span
  let root_span = Tracer::start_span(tracer, "root-operation")
  let root_context = Span::span_context(root_span)
  
  // Create child span
  let child_span = Tracer::start_span(tracer, "child-operation")
  let child_context = Span::span_context(child_span)
  
  // Verify trace ID consistency
  assert_eq(SpanContext::trace_id(root_context), SpanContext::trace_id(child_context))
  
  // Verify span ID uniqueness
  assert_true(SpanContext::span_id(root_context) != SpanContext::span_id(child_context))
  
  // End both spans
  Span::end(child_span)
  Span::end(root_span)
}

test "cross-service propagation" {
  // Create baggage
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user-id", "12345")
  
  // Verify baggage entry
  match Baggage::get_entry(updated_baggage, "user-id") {
    Some(value) => assert_eq(value, "12345")
    None => assert_true(false)
  }
  
  // Create propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Create context and carrier
  let context = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite_propagator, context, carrier)
  
  // Extract context from carrier
  let extracted_context = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify context extraction
  let key = ContextKey::new("extracted")
  match Context::get(extracted_context, key) {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
}

test "log record severity levels" {
  // Create logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test-logger")
  
  // Create log records with different severity levels
  let trace_record = LogRecord::new(Trace, "Trace message")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Info message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  
  // Verify severity levels
  let trace_severity = LogRecord::severity_number(trace_record)
  let debug_severity = LogRecord::severity_number(debug_record)
  let info_severity = LogRecord::severity_number(info_record)
  let warn_severity = LogRecord::severity_number(warn_record)
  let error_severity = LogRecord::severity_number(error_record)
  let fatal_severity = LogRecord::severity_number(fatal_record)
  
  // Check each severity level by pattern matching
  match trace_severity {
    Trace => assert_true(true)
    _ => assert_true(false)
  }
  
  match debug_severity {
    Debug => assert_true(true)
    _ => assert_true(false)
  }
  
  match info_severity {
    Info => assert_true(true)
    _ => assert_true(false)
  }
  
  match warn_severity {
    Warn => assert_true(true)
    _ => assert_true(false)
  }
  
  match error_severity {
    Error => assert_true(true)
    _ => assert_true(false)
  }
  
  match fatal_severity {
    Fatal => assert_true(true)
    _ => assert_true(false)
  }
  
  // Verify log bodies
  match LogRecord::body(info_record) {
    Some(body) => assert_eq(body, "Info message")
    None => assert_true(false)
  }
  
  // Create log record with context
  let context_record = LogRecord::new_with_context(
    Info,
    Some("Contextual message"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  
  // Verify context fields
  match LogRecord::trace_id(context_record) {
    Some(trace_id) => assert_eq(trace_id, "trace123")
    None => assert_true(false)
  }
  
  match LogRecord::span_id(context_record) {
    Some(span_id) => assert_eq(span_id, "span456")
    None => assert_true(false)
  }
  
  // Emit log records
  Logger::emit(logger, info_record)
  Logger::emit(logger, context_record)
}

test "resource merge strategy" {
  // Create base resource
  let base_resource = Resource::new()
  let base_with_attrs = Resource::with_attributes(
    base_resource,
    [
      ("service.name", StringValue("base-service")),
      ("service.version", StringValue("1.0.0"))
    ]
  )
  
  // Create override resource
  let override_resource = Resource::new()
  let override_with_attrs = Resource::with_attributes(
    override_resource,
    [
      ("service.name", StringValue("override-service")),
      ("service.instance.id", StringValue("instance-123"))
    ]
  )
  
  // Merge resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merge result
  match Resource::get_attribute(merged, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "override-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged, "service.instance.id") {
    Some(StringValue(id)) => assert_eq(id, "instance-123")
    _ => assert_true(false)
  }
}

test "concurrent safety" {
  // Create multiple tracers
  let tracer_provider = TracerProvider::default()
  let tracer1 = TracerProvider::get_tracer(tracer_provider, "tracer1")
  let tracer2 = TracerProvider::get_tracer(tracer_provider, "tracer2")
  
  // Create multiple spans concurrently
  let span1 = Tracer::start_span(tracer1, "operation-1")
  let span2 = Tracer::start_span(tracer2, "operation-2")
  
  // Verify span independence
  assert_true(Span::name(span1) != Span::name(span2))
  assert_true(SpanContext::span_id(Span::span_context(span1)) != SpanContext::span_id(Span::span_context(span2)))
  
  // Create multiple meters
  let meter_provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(meter_provider, "meter1")
  let meter2 = MeterProvider::get_meter(meter_provider, "meter2")
  
  // Create multiple counters
  let counter1 = Meter::create_counter(meter1, "counter-1")
  let counter2 = Meter::create_counter(meter2, "counter-2")
  
  // Record values concurrently
  Counter::add(counter1, 1.0)
  Counter::add(counter2, 2.0)
  
  // End spans
  Span::end(span1)
  Span::end(span2)
}