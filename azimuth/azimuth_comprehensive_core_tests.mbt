// Azimuth Telemetry System - Core Functionality Tests
// This file contains comprehensive test cases for the Azimuth telemetry system

// Test 1: Span Context Operations
test "span context creation and validation" {
  let span_ctx = SpanContext::new("trace_123", "span_456", true, "state=value")
  
  assert_eq(SpanContext::trace_id(span_ctx), "trace_123")
  assert_eq(SpanContext::span_id(span_ctx), "span_456")
  assert_true(SpanContext::is_valid(span_ctx))
  assert_true(SpanContext::is_sampled(span_ctx))
  
  let empty_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(empty_ctx))
  assert_false(SpanContext::is_sampled(empty_ctx))
}

// Test 2: Span Lifecycle Management
test "span lifecycle operations" {
  let span_ctx = SpanContext::new("trace_789", "span_101", true, "")
  let span = Span::new("test_span", Server, span_ctx)
  
  assert_eq(Span::name(span), "test_span")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  assert_eq(SpanContext::trace_id(Span::span_context(span)), "trace_789")
  
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::add_event(span, "test_event", Some([("key1", StringValue("value1"))]))
  Span::end(span)
  
  assert_eq(Span::status(span), Ok)
}

// Test 3: Tracer and Span Creation
test "tracer and span creation workflow" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test_tracer", Some("1.0.0"))
  let scope = Tracer::instrumentation_scope(tracer)
  
  assert_eq(scope.name, "test_tracer")
  assert_eq(scope.version, Some("1.0.0"))
  
  let span = Tracer::start_span(tracer, "operation_span", Some([("operation", StringValue("test"))]))
  assert_eq(Span::name(span), "operation_span")
  assert_eq(Span::kind(span), Internal)
}

// Test 4: Context Operations
test "context value operations" {
  let root_ctx = Context::root()
  let key = ContextKey::new("test_key")
  
  let ctx_with_value = Context::with_value(root_ctx, key, "test_value")
  let retrieved_value = Context::get(ctx_with_value, key)
  
  match retrieved_value {
    Some(value) => assert_eq(value, "test_value")
    None => assert_true(false)
  }
  
  let empty_key = ContextKey::new("nonexistent")
  let empty_value = Context::get(root_ctx, empty_key)
  assert_eq(empty_value, None)
}

// Test 5: Metrics Counter Operations
test "metrics counter operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  
  assert_eq(counter.name, "test_counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  Counter::add(counter, 1.0, None)
  Counter::add(counter, 5.0, Some(Attributes::new()))
  
  let instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(instrument), "test_counter")
  assert_eq(Instrument::description(instrument), Some("Test counter"))
  assert_eq(Instrument::unit(instrument), Some("count"))
}

// Test 6: Histogram and Gauge Operations
test "histogram and gauge operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram_meter")
  
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response time histogram"), Some("ms"))
  Histogram::record(histogram, 100.5)
  Histogram::record(histogram, 200.0, Some(Attributes::new()))
  
  let gauge = Meter::create_gauge(meter, "cpu_usage", Some("CPU usage gauge"), Some("percent"))
  assert_eq(gauge.name, "cpu_usage")
  assert_eq(gauge.description, Some("CPU usage gauge"))
  assert_eq(gauge.unit, Some("percent"))
  
  let updown_counter = Meter::create_updown_counter(meter, "active_connections", Some("Active connections"), Some("connections"))
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -5.0, Some(Attributes::new()))
}

// Test 7: Log Record Operations
test "log record creation and operations" {
  let log_record = LogRecord::new(Info, "Test log message")
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("Test log message"))
  
  let detailed_log = LogRecord::new_with_context(
    Error,
    Some("Detailed error message"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace_123"),
    Some("span_456"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(detailed_log), Error)
  assert_eq(LogRecord::trace_id(detailed_log), Some("trace_123"))
  assert_eq(LogRecord::span_id(detailed_log), Some("span_456"))
}

// Test 8: Logger Provider and Logger Operations
test "logger provider and logger operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test_logger")
  
  assert_eq(logger.scope.name, "test_logger")
  assert_eq(logger.scope.version, None)
  assert_eq(logger.scope.schema_url, None)
  
  let log_record = LogRecord::new(Warn, "Warning message")
  Logger::emit(logger, log_record)
  
  let error_log = LogRecord::new_with_context(
    Fatal,
    Some("Fatal error occurred"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    None,
    None,
    None
  )
  Logger::emit(logger, error_log)
}

// Test 9: Attributes and Resource Operations
test "attributes and resource operations" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  let string_value = Attributes::get(attrs, "string.key")
  match string_value {
    Some(StringValue(value)) => assert_eq(value, "test_value")
    _ => assert_true(false)
  }
  
  let int_value = Attributes::get(attrs, "int.key")
  match int_value {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  let resource = Resource::new()
  let resource_with_attrs = Resource::with_attributes(resource, [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ])
  
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(StringValue(value)) => assert_eq(value, "test_service")
    _ => assert_true(false)
  }
  
  let base_resource = Resource::with_attributes(resource, [
    ("base.attribute", StringValue("base_value"))
  ])
  let merged_resource = Resource::merge(base_resource, resource_with_attrs)
  let merged_attr = Resource::get_attribute(merged_resource, "service.name")
  match merged_attr {
    Some(StringValue(value)) => assert_eq(value, "test_service")
    _ => assert_true(false)
  }
}

// Test 10: Propagation and Baggage Operations
test "propagation and baggage operations" {
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "custom_header", "custom_value")
  
  let header_value = TextMapCarrier::get(carrier, "traceparent")
  match header_value {
    Some(value) => assert_true(value.contains("trace"))
    None => assert_true(false)
  }
  
  let baggage = Baggage::new()
  let baggage_with_entry = Baggage::set_entry(baggage, "user.id", "user123")
  
  let user_id = Baggage::get_entry(baggage_with_entry, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  let non_existent = Baggage::get_entry(baggage, "non.existent")
  assert_eq(non_existent, None)
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let root_ctx = Context::root()
  CompositePropagator::inject(composite, root_ctx, carrier)
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
}