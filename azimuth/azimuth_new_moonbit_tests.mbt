// Azimuth Telemetry System - New MoonBit Test Cases
// This file contains new test cases covering various telemetry functionality

// Test 1: Advanced Span Operations with Status Codes
test "span operations with status codes" {
  // Create a span context
  let span_ctx = SpanContext::new("trace123456789", "span987654321", true, "key1=value1,key2=value2")
  
  // Create a span
  let span = Span::new("test_span", Server, span_ctx)
  
  // Verify span properties
  assert_eq(Span::name(span), "test_span")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  
  // Set span status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)
  
  // Add events to span
  Span::add_event(span, "event1", Some([("event_type", AttributeValue::StringValue("custom"))]))
  Span::add_event(span, "event2", Some([("event_type", AttributeValue::StringValue("system"))]))
  
  // End the span
  Span::end(span)
}

// Test 2: Tracer Provider Integration
test "tracer provider integration" {
  // Create a tracer provider
  let provider = TracerProvider::default()
  
  // Get a tracer with version
  let tracer = TracerProvider::get_tracer(provider, "test_tracer", Some("1.0.0"))
  
  // Verify tracer scope
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test_tracer")
  match scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false)
  }
  
  // Start a span with the tracer
  let span = Tracer::start_span(tracer, "test_operation", Some([("operation.type", AttributeValue::StringValue("test"))]))
  
  // Verify span properties
  assert_eq(Span::name(span), "test_operation")
  assert_eq(Span::kind(span), Internal)
}

// Test 3: Histogram Metrics Operations
test "histogram metrics operations" {
  // Create a meter provider and meter
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // Create a histogram
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response time in milliseconds"), Some("ms"))
  
  // Verify histogram properties
  assert_eq(histogram.name, "response_time")
  match histogram.description {
    Some(desc) => assert_eq(desc, "Response time in milliseconds")
    None => assert_true(false)
  }
  match histogram.unit {
    Some(unit) => assert_eq(unit, "ms")
    None => assert_true(false)
  }
  
  // Record values with attributes
  let attrs = Attributes { values: [("endpoint", AttributeValue::StringValue("/api/test"))] }
  Histogram::record(histogram, 100.5, Some(attrs))
  Histogram::record(histogram, 200.3)
  Histogram::record(histogram, 150.7)
  
  // Test histogram as instrument
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "response_time")
  assert_eq(Instrument::description(instrument), Some("Response time in milliseconds"))
  assert_eq(Instrument::unit(instrument), Some("ms"))
}

// Test 4: Baggage Entry Management
test "baggage entry management" {
  // Create new baggage
  let baggage = Baggage::new()
  assert_eq(baggage.entries.length(), 0)
  
  // Set entries
  let baggage1 = Baggage::set_entry(baggage, "user_id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "session_id", "session456")
  let baggage3 = Baggage::set_entry(baggage2, "request_id", "request789")
  
  // Get entries
  match Baggage::get_entry(baggage3, "user_id") {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage3, "session_id") {
    Some(value) => assert_eq(value, "session456")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage3, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Remove entry
  let baggage4 = Baggage::remove_entry(baggage3, "session_id")
  match Baggage::get_entry(baggage4, "session_id") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Verify other entries still exist
  match Baggage::get_entry(baggage4, "user_id") {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
}

// Test 5: Composite Propagator Injection and Extraction
test "composite propagator injection and extraction" {
  // Create propagators
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CTraceContextPropagator::new()
  
  // Create composite propagator
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // Create context and carrier
  let ctx = Context::with_value(Context::root(), ContextKey::new("test_key"), "test_value")
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Verify traceparent header
  match TextMapCarrier::get(carrier, "traceparent") {
    Some(value) => assert_true(value.length() > 0)
    None => assert_true(false)
  }
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  match Context::get(extracted_ctx, ContextKey::new("extracted")) {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
}

// Test 6: HTTP Client Operations
test "http client operations" {
  // Create HTTP client
  let client = HttpClient::new()
  
  // Create HTTP request
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some("{\"test\": \"data\"}"))
  
  // Verify request properties
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  match HttpRequest::body(request) {
    Some(body) => assert_eq(body, "{\"test\": \"data\"}")
    None => assert_true(false)
  }
  
  // Create HTTP response
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("{\"result\": \"success\"}"))
  
  // Verify response properties
  assert_eq(HttpResponse::status_code(response), 200)
  match HttpResponse::body(response) {
    Some(body) => assert_eq(body, "{\"result\": \"success\"}")
    None => assert_true(false)
  }
}

// Test 7: Log Record with Context
test "log record with context" {
  // Create context
  let ctx = Context::with_value(Context::root(), ContextKey::new("correlation_id"), "corr123456")
  
  // Create attributes
  let attrs = Attributes { values: [
    ("service.name", AttributeValue::StringValue("azimuth_service")),
    ("service.version", AttributeValue::StringValue("2.0.0")),
    ("user.id", AttributeValue::IntValue(12345))
  ]}
  
  // Create log record with full context
  let record = LogRecord::new_with_context(
    Warn,
    Some("Operation completed with warnings"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace789012345"),
    Some("span543210987"),
    Some(ctx)
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(record), Warn)
  match LogRecord::body(record) {
    Some(body) => assert_eq(body, "Operation completed with warnings")
    None => assert_true(false)
  }
  match LogRecord::trace_id(record) {
    Some(trace_id) => assert_eq(trace_id, "trace789012345")
    None => assert_true(false)
  }
  match LogRecord::span_id(record) {
    Some(span_id) => assert_eq(span_id, "span543210987")
    None => assert_true(false)
  }
}

// Test 8: Resource Attributes Management
test "resource attributes management" {
  // Create resource
  let resource = Resource::new()
  
  // Add attributes
  let attrs = [
    ("service.name", AttributeValue::StringValue("payment_service")),
    ("service.version", AttributeValue::StringValue("1.2.3")),
    ("deployment.environment", AttributeValue::StringValue("production")),
    ("host.name", AttributeValue::StringValue("web-server-01")),
    ("process.pid", AttributeValue::IntValue(12345))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Verify attributes
  match Resource::get_attribute(resource_with_attrs, "service.name") {
    Some(AttributeValue::StringValue(name)) => assert_eq(name, "payment_service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "service.version") {
    Some(AttributeValue::StringValue(version)) => assert_eq(version, "1.2.3")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "process.pid") {
    Some(AttributeValue::IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test resource merging
  let override_attrs = [
    ("service.version", AttributeValue::StringValue("2.0.0")),
    ("deployment.region", AttributeValue::StringValue("us-west-2"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  // Verify merged attributes
  match Resource::get_attribute(merged_resource, "service.version") {
    Some(AttributeValue::StringValue(version)) => assert_eq(version, "2.0.0")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "deployment.region") {
    Some(AttributeValue::StringValue(region)) => assert_eq(region, "us-west-2")
    _ => assert_true(false)
  }
}

// Test 9: UpDownCounter and Gauge Metrics
test "updown_counter and gauge metrics" {
  // Create a meter provider and meter
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "metrics_meter")
  
  // Create an UpDownCounter
  let counter = Meter::create_updown_counter(meter, "active_connections", Some("Number of active connections"), Some("connections"))
  
  // Verify counter properties
  assert_eq(counter.name, "active_connections")
  match counter.description {
    Some(desc) => assert_eq(desc, "Number of active connections")
    None => assert_true(false)
  }
  
  // Add values to counter
  let attrs = Attributes { values: [("server.id", AttributeValue::StringValue("server-01"))] }
  UpDownCounter::add(counter, 10.0, Some(attrs))
  UpDownCounter::add(counter, -5.0)
  UpDownCounter::add(counter, 3.0)
  
  // Create a Gauge
  let gauge = Meter::create_gauge(meter, "memory_usage", Some("Memory usage in MB"), Some("MB"))
  
  // Verify gauge properties
  assert_eq(gauge.name, "memory_usage")
  match gauge.description {
    Some(desc) => assert_eq(desc, "Memory usage in MB")
    None => assert_true(false)
  }
  match gauge.unit {
    Some(unit) => assert_eq(unit, "MB")
    None => assert_true(false)
  }
}

// Test 10: Logger Provider with Log Emission
test "logger provider with log emission" {
  // Create a logger provider
  let provider = LoggerProvider::default()
  
  // Get a logger
  let logger = LoggerProvider::get_logger(provider, "test_logger")
  
  // Verify logger scope
  let scope = logger.scope
  assert_eq(scope.name, "test_logger")
  
  // Create and emit log records
  let info_record = LogRecord::new(Info, "Application started successfully")
  Logger::emit(logger, info_record)
  
  let warn_record = LogRecord::new_with_context(
    Warn,
    Some("Configuration using default values"),
    None,
    Some(1735689600000000000L),
    None,
    None,
    None,
    None
  )
  Logger::emit(logger, warn_record)
  
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes { values: [
      ("error.code", AttributeValue::IntValue(500)),
      ("error.message", AttributeValue::StringValue("Connection timeout"))
    ]}),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace123456789"),
    Some("span987654321"),
    Some(Context::with_value(Context::root(), ContextKey::new("request_id"), "req123456"))
  )
  Logger::emit(logger, error_record)
}