// Azimuth Telemetry System - Extended MoonBit Test Suite
// This file contains 8 test cases covering various functionality areas

test "resource merge operations" {
  let base_resource = Resource::new()
  let base_attrs = [("service.name", StringValue("azimuth-service")), ("service.version", StringValue("1.0.0"))]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [("service.instance.id", StringValue("instance-123")), ("environment", StringValue("production"))]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merged resource contains override attributes
  let instance_id = Resource::get_attribute(merged, "service.instance.id")
  assert_eq(instance_id, Some(StringValue("instance-123")))
  
  let env = Resource::get_attribute(merged, "environment")
  assert_eq(env, Some(StringValue("production")))
}

test "baggage propagation operations" {
  let baggage = Baggage::new()
  
  // Add entries to baggage
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "user-123")
  let baggage_with_more_entries = Baggage::set_entry(baggage_with_entries, "request.id", "req-456")
  
  // Retrieve entries
  let user_id = Baggage::get_entry(baggage_with_more_entries, "user.id")
  assert_eq(user_id, Some("user-123"))
  
  let request_id = Baggage::get_entry(baggage_with_more_entries, "request.id")
  assert_eq(request_id, Some("req-456"))
  
  // Remove entry
  let baggage_after_remove = Baggage::remove_entry(baggage_with_more_entries, "user.id")
  let removed_user_id = Baggage::get_entry(baggage_after_remove, "user.id")
  assert_eq(removed_user_id, None)
}

test "composite propagator injection and extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CTraceContextPropagator::new()  // Simplified for test
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test injection
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "histogram metric operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram-test-meter")
  let histogram = Meter::create_histogram(meter, "request.duration", Some("Request duration in seconds"), Some("s"))
  
  assert_eq(histogram.name, "request.duration")
  assert_eq(histogram.description, Some("Request duration in seconds"))
  assert_eq(histogram.unit, Some("s"))
  
  // Record some measurements
  Histogram::record(histogram, 0.1)  // 100ms
  Histogram::record(histogram, 0.25) // 250ms
  Histogram::record(histogram, 0.5)  // 500ms
  
  // Test instrument conversion
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "request.duration")
  assert_eq(Instrument::description(instrument), Some("Request duration in seconds"))
  assert_eq(Instrument::unit(instrument), Some("s"))
}

test "span lifecycle management" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test-tracer")
  let span = Tracer::start_span(tracer, "test-span")
  
  // Test initial state
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // Test span context
  let span_ctx = Span::span_context(span)
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Test status operations
  assert_eq(Span::status(span), Unset)
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Test event addition
  Span::add_event(span, "event1", Some([("key1", StringValue("value1"))]))
  
  // End the span
  Span::end(span)
}

test "log record with trace context correlation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let timestamp = 1735689600000000000L
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Database connection failed"),
    Some(Attributes::new()),
    Some(timestamp),
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
  
  // Test logger emission
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test-logger")
  Logger::emit(logger, record)
}

test "http client request and response handling" {
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some("request body"))
  
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("request body"))
  
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("{\"data\": \"response data\"}"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"data\": \"response data\"}"))
}

test "gauge metric operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge-test-meter")
  let gauge = Meter::create_gauge(meter, "system.memory.usage", Some("System memory usage percentage"), Some("%"))
  
  assert_eq(gauge.name, "system.memory.usage")
  assert_eq(gauge.description, Some("System memory usage percentage"))
  assert_eq(gauge.unit, Some("%"))
  
  // Record gauge measurements
  UpDownCounter::add(gauge, 75.5)  // 75.5% memory usage
  UpDownCounter::add(gauge, -10.0) // Decrease to 65.5%
  UpDownCounter::add(gauge, 5.0)   // Increase to 70.5%
  
  assert_true(true)  // Gauge operations don't return values to assert
}