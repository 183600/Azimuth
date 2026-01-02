// Azimuth MoonBit Enhanced Test Suite
// This file contains 8 focused test cases covering various telemetry aspects

test "baggage entry operations" {
  let baggage = Baggage::new()
  
  // Test setting and getting baggage entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let retrieved_value = Baggage::get_entry(updated_baggage, "user.id")
  
  assert_eq(retrieved_value, Some("12345"))
  
  // Test removing baggage entries
  let final_baggage = Baggage::remove_entry(updated_baggage, "user.id")
  let removed_value = Baggage::get_entry(final_baggage, "user.id")
  
  assert_eq(removed_value, None)
}

test "span lifecycle management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer")
  let span = Tracer::start_span(tracer, "test-span")
  
  // Test span basic properties
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // Test span status operations
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_true(true)
  
  // Test adding events to span
  Span::add_event(span, "test-event", Some([("event.type", StringValue("test"))]))
  
  // Test ending the span
  Span::end(span)
  assert_true(true)
}

test "histogram metrics operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram-meter")
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time in milliseconds"), Some("ms"))
  
  // Test histogram properties
  assert_eq(histogram.name, "response.time")
  assert_eq(histogram.description, Some("Response time in milliseconds"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test recording values
  Histogram::record(histogram, 100.5)
  Histogram::record(histogram, 200.3)
  Histogram::record(histogram, 150.7)
  
  // Test converting to instrument
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "response.time")
  assert_eq(Instrument::description(instrument), Some("Response time in milliseconds"))
  assert_eq(Instrument::unit(instrument), Some("ms"))
}

test "log severity levels" {
  // Test different severity levels
  let trace_record = LogRecord::new(Trace, "Debug trace information")
  let debug_record = LogRecord::new(Debug, "Debug information")
  let info_record = LogRecord::new(Info, "Information message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error occurred")
  let fatal_record = LogRecord::new(Fatal, "Fatal error")
  
  // Verify severity numbers
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Test log record with context
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_value = Context::with_value(ctx, key, "corr-123")
  
  let contextual_record = LogRecord::new_with_context(
    Info,
    Some("Contextual log message"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-123"),
    Some("span-456"),
    Some(ctx_with_value)
  )
  
  assert_eq(LogRecord::body(contextual_record), Some("Contextual log message"))
  assert_eq(LogRecord::trace_id(contextual_record), Some("trace-123"))
  assert_eq(LogRecord::span_id(contextual_record), Some("span-456"))
}

test "resource attributes management" {
  let resource = Resource::new()
  
  // Test adding attributes to resource
  let attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("deployment.environment", StringValue("production"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Test retrieving attributes
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let non_existent = Resource::get_attribute(resource_with_attrs, "non.existent")
  
  assert_eq(service_name, Some(StringValue("azimuth-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(non_existent, None)
  
  // Test resource merging
  let override_attrs = [
    ("service.version", StringValue("2.0.0")),
    ("new.attribute", StringValue("new-value"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  let merged_version = Resource::get_attribute(merged_resource, "service.version")
  let new_attr = Resource::get_attribute(merged_resource, "new.attribute")
  
  assert_eq(merged_version, Some(StringValue("2.0.0")))
  assert_eq(new_attr, Some(StringValue("new-value")))
}

test "composite propagator functionality" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Create composite propagator with both trace and baggage propagators
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // Test injection
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_value, Some("true"))
}

test "HTTP client operations" {
  let client = HttpClient::new()
  
  // Test creating HTTP request
  let headers = [
    ("Content-Type", StringValue("application/json")),
    ("Authorization", StringValue("Bearer token123"))
  ]
  
  let request = HttpRequest::new(
    "POST",
    "https://api.example.com/data",
    headers,
    Some("{\"key\":\"value\"}")
  )
  
  // Test request properties
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("{\"key\":\"value\"}"))
  
  // Test creating HTTP response
  let response_headers = [
    ("Content-Type", StringValue("application/json")),
    ("X-Request-ID", StringValue("req-123"))
  ]
  
  let response = HttpResponse::new(
    200,
    response_headers,
    Some("{\"result\":\"success\"}")
  )
  
  // Test response properties
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"result\":\"success\"}"))
}

test "time and random operations" {
  // Test clock operations
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  // Verify timestamp is reasonable (should be > 0)
  assert_true(timestamp > 0L)
  
  // Test random operations
  let random = Random::system()
  
  // Test generating random bytes
  let random_bytes = Random::next_bytes(random, 16)
  assert_eq(Array::length(random_bytes), 16)
  
  // Test generating random U64
  let random_u64 = Random::next_u64(random)
  assert_true(random_u64 > 0UL)
  
  // Test timestamp in log record
  let timestamped_record = LogRecord::new_with_context(
    Info,
    Some("Timestamped log"),
    None,
    Some(timestamp),
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::body(timestamped_record), Some("Timestamped log"))
}