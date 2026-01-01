// Enhanced Core Telemetry Tests for Azimuth System
// This file contains comprehensive test cases for core telemetry functionality

test "span lifecycle management" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test-tracer")
  let span = Tracer::start_span(tracer, "test-span")
  
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  assert_eq(Span::span_context(span), span_ctx)
  
  Span::set_status(span, Ok, "Operation completed successfully")
  Span::add_event(span, "test-event", [("event.type", StringValue("test"))])
  Span::end(span)
}

test "histogram metric operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram metric"), Some("ms"))
  
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, Some("Test histogram metric"))
  assert_eq(histogram.unit, Some("ms"))
  
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "test.histogram")
}

test "updown counter metric operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_updown_counter(meter, "test.updown_counter", Some("Test updown counter"), Some("items"))
  
  assert_eq(counter.name, "test.updown_counter")
  assert_eq(counter.description, Some("Test updown counter"))
  assert_eq(counter.unit, Some("items"))
  
  UpDownCounter::add(counter, 10.0)
  UpDownCounter::add(counter, -5.0)
  UpDownCounter::add(counter, 3.0)
}

test "gauge metric operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge metric"), Some("percent"))
  
  assert_eq(gauge.name, "test.gauge")
  assert_eq(gauge.description, Some("Test gauge metric"))
  assert_eq(gauge.unit, Some("percent"))
}

test "log record with full context" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let timestamp = Clock::now_unix_nanos(Clock::system())
  let attributes = Attributes::new()
  Attributes::set(attributes, "log.level", StringValue("info"))
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Warning message with context"),
    Some(attributes),
    Some(timestamp),
    Some(timestamp + 1000L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::body(record), Some("Warning message with context"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
}

test "baggage propagation operations" {
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let final_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  
  let user_id = Baggage::get_entry(final_baggage, "user.id")
  let request_id = Baggage::get_entry(final_baggage, "request.id")
  let missing = Baggage::get_entry(final_baggage, "missing.key")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(missing, None)
  
  let removed_baggage = Baggage::remove_entry(final_baggage, "user.id")
  let removed_user_id = Baggage::get_entry(removed_baggage, "user.id")
  assert_eq(removed_user_id, None)
}

test "resource attributes management" {
  let resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let missing_attr = Resource::get_attribute(resource_with_attrs, "missing.attr")
  
  assert_eq(service_name, Some(StringValue("test-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(missing_attr, None)
  
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("environment", StringValue("test"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let environment = Resource::get_attribute(merged_resource, "environment")
  
  assert_eq(merged_service_name, Some(StringValue("override-service")))
  assert_eq(environment, Some(StringValue("test")))
}

test "composite propagator injection and extraction" {
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
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123")
  ]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some("{\"test\": \"data\"}"))
  
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("{\"test\": \"data\"}"))
  
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Request-ID", "req-12345")
  ]
  let response = HttpResponse::new(200, response_headers, Some("{\"result\": \"success\"}"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"result\": \"success\"}"))
}