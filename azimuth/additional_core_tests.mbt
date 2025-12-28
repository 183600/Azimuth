// Azimuth Additional Core Tests - Testing less covered functionality

test "histogram_metric_operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response time in milliseconds"), Some("ms"))
  
  Histogram::record(histogram, 150.5)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 75.25)
  
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "response_time")
  
  match Instrument::description(instrument) {
    Some(desc) => assert_eq(desc, "Response time in milliseconds")
    _ => @expect(false)
  }
  
  match Instrument::unit(instrument) {
    Some(unit) => assert_eq(unit, "ms")
    _ => @expect(false)
  }
}

test "gauge_and_updown_counter_creation" {
  let provider = MeterProvider::noop()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  let gauge = Meter::create_gauge(meter, "cpu_usage", Some("CPU usage percentage"), Some("%"))
  assert_eq(gauge.name, "cpu_usage")
  
  match gauge.description {
    Some(desc) => assert_eq(desc, "CPU usage percentage")
    _ => @expect(false)
  }
  
  let updown_counter = Meter::create_up_down_counter(meter, "active_connections", Some("Number of active connections"), Some("connections"))
  assert_eq(updown_counter.name, "active_connections")
  
  match updown_counter.unit {
    Some(unit) => assert_eq(unit, "connections")
    _ => @expect(false)
  }
}

test "logger_and_log_record_emission" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test_logger")
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Warning: Service degradation detected"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some("trace_abc123"),
    Some("span_def456"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  
  match LogRecord::body(record) {
    Some(body) => assert_eq(body, "Warning: Service degradation detected")
    _ => @expect(false)
  }
  
  match LogRecord::trace_id(record) {
    Some(trace_id) => assert_eq(trace_id, "trace_abc123")
    _ => @expect(false)
  }
  
  match LogRecord::span_id(record) {
    Some(span_id) => assert_eq(span_id, "span_def456")
    _ => @expect(false)
  }
  
  Logger::emit(logger, record)
}

test "composite_propagator_inject_extract" {
  let propagators = [W3CTraceContextPropagator::new()]
  let composite = CompositePropagator::new(propagators)
  let carrier = TextMapCarrier::new()
  let ctx = Context::with_value(Context::root(), ContextKey::new("user.id"), "12345")
  
  CompositePropagator::inject(composite, ctx, carrier)
  
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  match injected_traceparent {
    Some(trace) => assert_eq(trace, "00-test-trace-id-test-span-id-01")
    _ => @expect(false)
  }
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    _ => @expect(false)
  }
}

test "clock_and_random_functionality" {
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  assert_eq(timestamp, 1735689600000000000L)
  
  let random = Random::system()
  let bytes = Random::next_bytes(random, 8)
  assert_eq(bytes.length, 0)
  
  let u64_value = Random::next_u64(random)
  assert_eq(u64_value, 12345UL)
}

test "attribute_value_types_comprehensive" {
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "string.value", StringValue("hello world"))
  Attributes::set(attrs, "int.value", IntValue(42))
  Attributes::set(attrs, "float.value", FloatValue(3.14159))
  Attributes::set(attrs, "bool.value", BoolValue(true))
  Attributes::set(attrs, "array.string", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([1, 2, 3]))
  
  let string_val = Attributes::get(attrs, "string.value")
  match string_val {
    Some(StringValue(v)) => assert_eq(v, "hello world")
    _ => @expect(false)
  }
  
  let int_val = Attributes::get(attrs, "int.key")
  match int_val {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => @expect(false)
  }
  
  let missing_val = Attributes::get(attrs, "nonexistent.key")
  match missing_val {
    None => @expect(true)
    _ => @expect(false)
  }
}

test "span_event_and_status_management" {
  let span_ctx = SpanContext::new("trace_789", "span_101", true, "test=state")
  let span = Span::new("operation_span", Server, span_ctx)
  
  assert_eq(Span::name(span), "operation_span")
  assert_eq(Span::kind(span), Server)
  assert_eq(Span::is_recording(span), true)
  
  Span::set_status(span, Error, Some("Operation failed due to timeout"))
  Span::add_event(span, "timeout_event", Some([
    ("timeout.ms", IntValue(5000)),
    ("error.type", StringValue("timeout"))
  ]))
  
  let status = Span::status(span)
  assert_eq(status, Error)
  
  Span::end(span)
}

test "resource_merge_operations" {
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0"))
  ])
  
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("override-service")),
    ("environment", StringValue("production"))
  ])
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // After merge, should have override's attributes
  let service_name = Resource::get_attribute(merged, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "override-service")
    _ => @expect(false)
  }
  
  let environment = Resource::get_attribute(merged, "environment")
  match environment {
    Some(StringValue(env)) => assert_eq(env, "production")
    _ => @expect(false)
  }
}

test "span_kinds_comprehensive" {
  let span_ctx = SpanContext::new("trace_123", "span_456", true, "")
  
  let internal_span = Span::new("internal_operation", Internal, span_ctx)
  let server_span = Span::new("server_request", Server, span_ctx)
  let client_span = Span::new("client_request", Client, span_ctx)
  let producer_span = Span::new("message_producer", Producer, span_ctx)
  let consumer_span = Span::new("message_consumer", Consumer, span_ctx)
  
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
  
  assert_eq(Span::name(internal_span), "internal_operation")
  assert_eq(Span::name(server_span), "server_request")
  assert_eq(Span::name(client_span), "client_request")
  assert_eq(Span::name(producer_span), "message_producer")
  assert_eq(Span::name(consumer_span), "message_consumer")
}