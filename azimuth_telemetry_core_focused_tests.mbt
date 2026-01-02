// Azimuth Telemetry Core Focused Test Suite
// This file contains focused test cases for the core telemetry functionality

// Test 1: Span Context Validation
test "span context creation and validation" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "key1=value1,key2=value2")
  
  assert_eq(SpanContext::trace_id(span_ctx), "trace123")
  assert_eq(SpanContext::span_id(span_ctx), "span456")
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
  
  let invalid_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_ctx))
}

// Test 2: Attributes Operations
test "attributes set and get operations" {
  let attrs = Attributes::new()
  
  let string_value = Attributes::get(attrs, "string.key")
  match string_value {
    Some(StringValue(v)) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  let int_value = Attributes::get(attrs, "int.key")
  match int_value {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  let non_existent = Attributes::get(attrs, "non.existent.key")
  assert_true(non_existent.is_none())
}

// Test 3: Context Value Management
test "context value management" {
  let root_ctx = Context::root()
  let key = ContextKey::new("test.key")
  
  let ctx_with_value = Context::with_value(root_ctx, key, "test_value")
  let retrieved_value = Context::get(ctx_with_value, key)
  
  match retrieved_value {
    Some(v) => assert_eq(v, "test_value")
    None => assert_true(false)
  }
  
  let empty_ctx_value = Context::get(root_ctx, key)
  assert_true(empty_ctx_value.is_none())
}

// Test 4: Meter and Counter Operations
test "meter and counter operations" {
  let provider = MeterProvider::noop()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  
  assert_eq(counter.name, "test.counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  // Test adding value (simplified implementation)
  Counter::add(counter, 10.0)
  Counter::add(counter, 5.0)
}

// Test 5: Histogram Operations
test "histogram creation and recording" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test recording values
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  
  // Test as_instrument conversion
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "test.histogram")
  assert_eq(Instrument::description(instrument), Some("Test histogram"))
  assert_eq(Instrument::unit(instrument), Some("ms"))
}

// Test 6: Log Record Operations
test "log record creation and severity handling" {
  let log_record = LogRecord::new(SeverityNumber::Error, "Test error message")
  
  assert_eq(LogRecord::severity_number(log_record), SeverityNumber::Error)
  
  match LogRecord::body(log_record) {
    Some(message) => assert_eq(message, "Test error message")
    None => assert_true(false)
  }
  
  // Test with context
  let detailed_record = LogRecord::new_with_context(
    SeverityNumber::Warn,
    Some("Warning message"),
    None,
    Some(1735689600000000000L),
    None,
    Some("trace123"),
    Some("span456"),
    None
  )
  
  assert_eq(LogRecord::severity_number(detailed_record), SeverityNumber::Warn)
  assert_eq(LogRecord::trace_id(detailed_record), Some("trace123"))
  assert_eq(LogRecord::span_id(detailed_record), Some("span456"))
}

// Test 7: Span Lifecycle Management
test "span lifecycle management" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test.tracer", Some("1.0.0"))
  
  let span = Tracer::start_span(tracer, "test.span")
  
  assert_eq(Span::name(span), "test.span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // Test span context
  let span_ctx = Span::span_context(span)
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Add event and set status
  Span::add_event(span, "test.event")
  Span::set_status(span, Ok, Some("Operation completed"))
  
  // End span
  Span::end(span)
}

// Test 8: Baggage Operations
test "baggage entry management" {
  let baggage = Baggage::new()
  
  // Set and get entry
  let updated_baggage = Baggage::set_entry(baggage, "key1", "value1")
  let retrieved_value = Baggage::get_entry(updated_baggage, "key1")
  
  match retrieved_value {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // Remove entry
  let final_baggage = Baggage::remove_entry(updated_baggage, "key1")
  let removed_value = Baggage::get_entry(final_baggage, "key1")
  assert_true(removed_value.is_none())
}

// Test 9: HTTP Client Operations
test "http client request and response handling" {
  let client = HttpClient::new()
  
  // Create request
  let headers = [("Content-Type", "application/json"), ("User-Agent", "azimuth/1.0")]
  let request = HttpRequest::new("POST", "https://api.example.com/data", headers, Some("{\"test\": \"data\"}"))
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  
  match HttpRequest::body(request) {
    Some(body) => assert_eq(body, "{\"test\": \"data\"}")
    None => assert_true(false)
  }
  
  // Create response
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("{\"result\": \"success\"}"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  
  match HttpResponse::body(response) {
    Some(body) => assert_eq(body, "{\"result\": \"success\"}")
    None => assert_true(false)
  }
}

// Test 10: Resource Management
test "resource attribute management" {
  let resource = Resource::new()
  
  // Add attributes
  let attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // Get attributes
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  // Test resource merge
  let override_attrs = [
    ("service.name", StringValue("updated-service")),
    ("environment", StringValue("production"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  let merged_name = Resource::get_attribute(merged_resource, "service.name")
  match merged_name {
    Some(StringValue(name)) => assert_eq(name, "updated-service")
    _ => assert_true(false)
  }
}