// Focused Enhanced Test Suite for Azimuth Telemetry System
// This file contains targeted test cases for specific scenarios

// Test 1: Basic span context validation
pub test "basic span context validation" {
  // Test with valid IDs
  let valid_ctx = SpanContext::new("trace123", "span456", true, "")
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_eq(SpanContext::trace_id(valid_ctx), "trace123")
  assert_eq(SpanContext::span_id(valid_ctx), "span456")
  assert_true(SpanContext::is_sampled(valid_ctx))
  
  // Test with invalid IDs
  let invalid_ctx = SpanContext::new("", "", true, "")
  assert_false(SpanContext::is_valid(invalid_ctx))
}

// Test 2: Context operations
pub test "context operations" {
  let root_ctx = Context::root()
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(root_ctx, key, "test_value")
  
  // Test value retrieval
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test_value"))
  
  // Test missing key
  let missing_key = ContextKey::new("missing.key")
  let missing_value = Context::get(root_ctx, missing_key)
  assert_eq(missing_value, None)
}

// Test 3: Basic metrics operations
pub test "basic metrics operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  let counter = Meter::create_counter(meter, "test.counter")
  assert_eq(counter.name, "test.counter")
  
  let histogram = Meter::create_histogram(meter, "test.histogram")
  assert_eq(histogram.name, "test.histogram")
  
  let updown_counter = Meter::create_updown_counter(meter, "test.updown")
  assert_eq(updown_counter.name, "test.updown")
  
  let gauge = Meter::create_gauge(meter, "test.gauge")
  assert_eq(gauge.name, "test.gauge")
  
  // Test metric operations
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 100.0)
  UpDownCounter::add(updown_counter, 5.0)
  
  assert_true(true) // If we reach here, all operations completed
}

// Test 4: Log record operations
pub test "log record operations" {
  let record = LogRecord::new(azimuth::Info, "Test log message")
  
  assert_eq(LogRecord::severity_number(record), azimuth::Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  
  // Test with different severity levels
  let error_record = LogRecord::new(azimuth::Error, "Error message")
  assert_eq(LogRecord::severity_number(error_record), azimuth::Error)
  assert_eq(LogRecord::body(error_record), Some("Error message"))
  
  let warn_record = LogRecord::new(azimuth::Warn, "Warning message")
  assert_eq(LogRecord::severity_number(warn_record), azimuth::Warn)
  assert_eq(LogRecord::body(warn_record), Some("Warning message"))
}

// Test 5: Resource operations
pub test "resource operations" {
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // Test attribute retrieval
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let missing_attr = Resource::get_attribute(resource_with_attrs, "missing.attr")
  
  assert_eq(service_name, Some(StringValue("test-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(missing_attr, None)
}

// Test 6: HTTP operations
pub test "http operations" {
  let headers = [("Content-Type", "application/json")]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some("request body"))
  
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("request body"))
  
  let response = HttpResponse::new(200, headers, Some("{\"status\": \"success\"}"))
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"status\": \"success\"}"))
}

// Test 7: Span operations
pub test "span operations" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  assert_eq(Span::span_context(span), span_ctx)
  
  // Test span operations
  Span::add_event(span, "test-event")
  Span::set_status(span, Ok, Some("Test completed"))
  Span::end(span)
  
  assert_true(true) // If we reach here, all operations completed
}

// Test 8: Baggage operations
pub test "baggage operations" {
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "test.key", "test.value")
  
  // Test baggage retrieval
  let baggage_value = Baggage::get_entry(updated_baggage, "test.key")
  assert_eq(baggage_value, Some("test.value"))
  
  // Test missing entry
  let missing_value = Baggage::get_entry(updated_baggage, "missing.key")
  assert_eq(missing_value, None)
  
  // Test removal
  let baggage_without_key = Baggage::remove_entry(updated_baggage, "test.key")
  let removed_value = Baggage::get_entry(baggage_without_key, "test.key")
  assert_eq(removed_value, None)
}

// Test 9: Propagator operations
pub test "propagator operations" {
  let propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([propagator])
  
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // Test injection
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  let key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, key)
  assert_eq(extracted_value, Some("true"))
}

// Test 10: Text map carrier operations
pub test "text map carrier operations" {
  let carrier = TextMapCarrier::new()
  
  // Test setting headers
  TextMapCarrier::set(carrier, "test-header", "test-value")
  
  // Test getting headers
  let header_value = TextMapCarrier::get(carrier, "test-header")
  let missing_value = TextMapCarrier::get(carrier, "missing-header")
  
  assert_eq(header_value, Some("test-value"))
  assert_eq(missing_value, None)
}