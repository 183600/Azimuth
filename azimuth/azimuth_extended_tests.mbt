// Azimuth Extended MoonBit Test Suite
// This file contains additional test cases for various Azimuth telemetry system features

// Test 1: Resource Attribute Operations
test "resource attribute operations" {
  let resource = Resource::new()
  let attrs = [("service.name", StringValue("azimuth-service")), 
               ("service.version", StringValue("1.0.0")),
               ("service.instance.id", StringValue("instance-123"))]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  match Resource::get_attribute(resource_with_attrs, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "service.version") {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Span Context Validation
test "span context validation" {
  let valid_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  let invalid_ctx = SpanContext::new("", "", false, "")
  
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_false(SpanContext::is_valid(invalid_ctx))
  
  assert_eq(SpanContext::trace_id(valid_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(valid_ctx), "b7ad6b7169203331")
  assert_true(SpanContext::is_sampled(valid_ctx))
}

// Test 3: Meter and Counter Operations
test "meter and counter operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "test-counter", 
                                              Some("Test counter for operations"), 
                                              Some("operations"))
  
  assert_eq(counter.name, "test-counter")
  assert_eq(counter.description, Some("Test counter for operations"))
  assert_eq(counter.unit, Some("operations"))
  
  // Test adding values to counter
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 10.0)
  
  // In a real implementation, we would verify the counter value
  // For this test, we just ensure the operations don't fail
  assert_true(true)
}

// Test 4: Histogram Operations
test "histogram operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let histogram = Meter::create_histogram(meter, "request.duration", 
                                                  Some("Request duration histogram"), 
                                                  Some("ms"))
  
  assert_eq(histogram.name, "request.duration")
  assert_eq(histogram.description, Some("Request duration histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test recording values
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 250.0)
  Histogram::record(histogram, 500.0)
  Histogram::record(histogram, 1000.0)
  
  // Test histogram as instrument
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "request.duration")
  assert_eq(Instrument::description(instrument), Some("Request duration histogram"))
  assert_eq(Instrument::unit(instrument), Some("ms"))
}

// Test 5: Logger and LogRecord Operations
test "logger and log record operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  let info_record = LogRecord::new(SeverityNumber::Info, "Test info message")
  let error_record = LogRecord::new(SeverityNumber::Error, "Test error message")
  
  assert_eq(LogRecord::severity_number(info_record), SeverityNumber::Info)
  assert_eq(LogRecord::severity_number(error_record), SeverityNumber::Error)
  
  match LogRecord::body(info_record) {
    Some(body) => assert_eq(body, "Test info message")
    None => assert_true(false)
  }
  
  match LogRecord::body(error_record) {
    Some(body) => assert_eq(body, "Test error message")
    None => assert_true(false)
  }
  
  // Test emitting log records
  Logger::emit(logger, info_record)
  Logger::emit(logger, error_record)
  
  // In a real implementation, we would verify the log records were emitted
  // For this test, we just ensure the operations don't fail
  assert_true(true)
}

// Test 6: Baggage Operations
test "baggage operations" {
  let baggage = Baggage::new()
  
  // Test setting and getting entries
  let baggage_with_entry = Baggage::set_entry(baggage, "user.id", "user-123")
  
  match Baggage::get_entry(baggage_with_entry, "user.id") {
    Some(value) => assert_eq(value, "user-123")
    None => assert_true(false)
  }
  
  // Test non-existent entry
  match Baggage::get_entry(baggage_with_entry, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test removing entries
  let baggage_without_entry = Baggage::remove_entry(baggage_with_entry, "user.id")
  
  match Baggage::get_entry(baggage_without_entry, "user.id") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 7: Context Operations
test "context operations" {
  let root_ctx = Context::root()
  let key = ContextKey::new("test.key")
  
  // Test setting and getting values
  let ctx_with_value = Context::with_value(root_ctx, key, "test.value")
  
  match Context::get(ctx_with_value, key) {
    Some(value) => assert_eq(value, "test.value")
    None => assert_true(false)
  }
  
  // Test getting non-existent key
  let non_existent_key = ContextKey::new("nonexistent")
  match Context::get(ctx_with_value, non_existent_key) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 8: Span Operations
test "span operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer")
  let span = Tracer::start_span(tracer, "test-span")
  
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), SpanKind::Internal)
  assert_true(Span::is_recording(span))
  
  let span_ctx = Span::span_context(span)
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Test setting status
  Span::set_status(span, StatusCode::Ok, Some("Operation completed"))
  
  // Test adding events
  Span::add_event(span, "event1", Some([("key1", StringValue("value1"))]))
  Span::add_event(span, "event2")
  
  // Test ending span
  Span::end(span)
  
  // In a real implementation, we would verify the span was properly ended
  // For this test, we just ensure the operations don't fail
  assert_true(true)
}

// Test 9: HTTP Client Operations
test "http client operations" {
  let client = HttpClient::new()
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some(""))
  
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  
  match HttpRequest::body(request) {
    Some(body) => assert_eq(body, "")
    None => assert_true(false)
  }
  
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("{\"data\": \"value\"}"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  
  match HttpResponse::body(response) {
    Some(body) => assert_eq(body, "{\"data\": \"value\"}")
    None => assert_true(false)
  }
}

// Test 10: Resource Merge Operations
test "resource merge operations" {
  let base_resource = Resource::new()
  let base_attrs = [("service.name", StringValue("base-service"))]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [("service.version", StringValue("2.0.0")),
                        ("service.instance.id", StringValue("instance-456"))]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Test merging resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merged resource has override attributes
  match Resource::get_attribute(merged, "service.version") {
    Some(StringValue(version)) => assert_eq(version, "2.0.0")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged, "service.instance.id") {
    Some(StringValue(instance_id)) => assert_eq(instance_id, "instance-456")
    _ => assert_true(false)
  }
}