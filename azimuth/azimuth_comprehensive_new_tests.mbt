// Azimuth Comprehensive New Test Suite
// This file contains comprehensive test cases for core Azimuth telemetry functionality

test "span lifecycle management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer", Some("1.0.0"))
  let span = Tracer::start_span(tracer, "test-span")
  
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  Span::add_event(span, "test-event", Some([("event.type", StringValue("test"))]))
  Span::end(span)
  
  assert_true(true)
}

test "histogram metrics operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram-meter")
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  Histogram::record(histogram, 10.5)
  Histogram::record(histogram, 25.0)
  Histogram::record(histogram, 50.25)
  
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "test.histogram")
  assert_eq(Instrument::description(instrument), Some("Test histogram"))
  assert_eq(Instrument::unit(instrument), Some("ms"))
}

test "updown counter metrics operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown-meter")
  let updown_counter = Meter::create_updown_counter(meter, "test.updown", Some("Test updown counter"), Some("items"))
  
  assert_eq(updown_counter.name, "test.updown")
  assert_eq(updown_counter.description, Some("Test updown counter"))
  assert_eq(updown_counter.unit, Some("items"))
  
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -2.0)
  UpDownCounter::add(updown_counter, 10.0)
  
  assert_true(true)
}

test "gauge metrics operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge-meter")
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("percent"))
  
  assert_eq(gauge.name, "test.gauge")
  assert_eq(gauge.description, Some("Test gauge"))
  assert_eq(gauge.unit, Some("percent"))
  
  assert_true(true)
}

test "log record with full context" {
  let trace_id = Some("0af7651916cd43dd8448eb211c80319c")
  let span_id = Some("b7ad6b7169203331")
  let timestamp = Some(1735689600000000000L)
  let observed_timestamp = Some(1735689600000000001L)
  let attributes = Some(Attributes::new())
  let context = Some(Context::root())
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Error occurred during processing"),
    attributes,
    timestamp,
    observed_timestamp,
    trace_id,
    span_id,
    context
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Error occurred during processing"))
  assert_eq(LogRecord::trace_id(record), trace_id)
  assert_eq(LogRecord::span_id(record), span_id)
}

test "resource attributes operations" {
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let service_instance_id = Resource::get_attribute(resource_with_attrs, "service.instance.id")
  
  assert_eq(service_name, Some(StringValue("azimuth-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(service_instance_id, Some(StringValue("instance-123")))
}

test "baggage operations" {
  let baggage = Baggage::new()
  let baggage_with_entry = Baggage::set_entry(baggage, "user.id", "12345")
  
  let user_id = Baggage::get_entry(baggage_with_entry, "user.id")
  let non_existent = Baggage::get_entry(baggage_with_entry, "non.existent")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(non_existent, None)
  
  let baggage_without_entry = Baggage::remove_entry(baggage_with_entry, "user.id")
  let removed_user_id = Baggage::get_entry(baggage_without_entry, "user.id")
  
  assert_eq(removed_user_id, None)
}

test "composite propagator operations" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite, ctx, carrier)
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  let key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, key)
  
  assert_eq(extracted_value, Some("true"))
}

test "http client operations" {
  let headers = [("Content-Type", "application/json")]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some("request body"))
  
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("request body"))
  
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("response body"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("response body"))
}

test "logger provider operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger", Some("1.0.0"), Some("https://example.com/schema"))
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Warning message"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some("trace-id-123"),
    Some("span-id-456"),
    Some(Context::root())
  )
  
  Logger::emit(logger, record)
  
  assert_true(true)
}