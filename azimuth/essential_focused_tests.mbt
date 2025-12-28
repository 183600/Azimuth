// Essential Focused Tests for Azimuth Telemetry System

test "attribute_value_type_conversion" {
  // Test different AttributeValue types
  let string_val = StringValue("test")
  let int_val = IntValue(42)
  let float_val = FloatValue(3.14)
  let bool_val = BoolValue(true)
  let array_string_val = ArrayStringValue(["a", "b", "c"])
  let array_int_val = ArrayIntValue([1, 2, 3])
  
  // Verify type preservation
  match string_val {
    StringValue(s) => assert_eq(s, "test")
    _ => @panic("Expected StringValue")
  }
  
  match int_val {
    IntValue(i) => assert_eq(i, 42)
    _ => @panic("Expected IntValue")
  }
  
  match float_val {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @panic("Expected FloatValue")
  }
  
  match bool_val {
    BoolValue(b) => assert_eq(b, true)
    _ => @panic("Expected BoolValue")
  }
}

test "context_lifecycle_operations" {
  let root_ctx = Context::root()
  let key = ContextKey::new("test_key")
  let ctx_with_value = Context::with_value(root_ctx, key, "test_value")
  
  // Test value retrieval
  match Context::get(ctx_with_value, key) {
    Some(value) => assert_eq(value, "test_value")
    None => @panic("Expected value to be found")
  }
  
  // Test non-existent key
  let non_existent_key = ContextKey::new("non_existent")
  match Context::get(root_ctx, non_existent_key) {
    None => () // Expected
    Some(_) => @panic("Expected None for non-existent key")
  }
}

test "span_context_validation" {
  let valid_ctx = SpanContext::new("trace123", "span456", true, "test_state")
  let invalid_ctx = SpanContext::new("", "", false, "")
  
  // Test valid context
  assert_eq(SpanContext::trace_id(valid_ctx), "trace123")
  assert_eq(SpanContext::span_id(valid_ctx), "span456")
  assert_eq(SpanContext::is_sampled(valid_ctx), true)
  assert_eq(SpanContext::is_valid(valid_ctx), true)
  
  // Test invalid context
  assert_eq(SpanContext::is_valid(invalid_ctx), false)
}

test "metric_instrument_creation" {
  let provider = MeterProvider::noop()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // Test Counter creation
  let counter = Meter::create_counter(meter, "test_counter")
  assert_eq(counter.name, "test_counter")
  assert_eq(counter.description, None)
  assert_eq(counter.unit, None)
  
  // Test Histogram creation
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test Histogram"), Some("ms"))
  assert_eq(histogram.name, "test_histogram")
  assert_eq(histogram.description, Some("Test Histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test UpDownCounter creation
  let updown_counter = Meter::create_updown_counter(meter, "test_updown")
  assert_eq(updown_counter.name, "test_updown")
  
  // Test Gauge creation
  let gauge = Meter::create_gauge(meter, "test_gauge")
  assert_eq(gauge.name, "test_gauge")
}

test "log_record_severity_levels" {
  let trace_record = LogRecord::new(Trace, "Trace message")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Info message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  
  // Test severity levels
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Test body retrieval
  match LogRecord::body(info_record) {
    Some(body) => assert_eq(body, "Info message")
    None => @panic("Expected body to be present")
  }
}

test "span_lifecycle_management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test_tracer")
  let span = Tracer::start_span(tracer, "test_span")
  
  // Test span properties
  assert_eq(Span::name(span), "test_span")
  assert_eq(Span::kind(span), Internal)
  assert_eq(Span::is_recording(span), true)
  
  // Test span context
  let span_ctx = Span::span_context(span)
  assert_eq(SpanContext::is_valid(span_ctx), true)
  
  // Test status operations
  Span::set_status(span, Ok, Some("Operation completed"))
  
  // Test event addition
  Span::add_event(span, "test_event", Some([("event_key", StringValue("event_value"))]))
  
  // Test span ending
  Span::end(span)
}

test "resource_attribute_operations" {
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // Test attribute retrieval
  match Resource::get_attribute(resource_with_attrs, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "test_service")
    _ => @panic("Expected service.name attribute")
  }
  
  match Resource::get_attribute(resource_with_attrs, "nonexistent") {
    None => () // Expected
    Some(_) => @panic("Expected None for nonexistent attribute")
  }
}

test "baggage_operations" {
  let baggage = Baggage::new()
  
  // Test setting and getting entries
  let baggage_with_entry = Baggage::set_entry(baggage, "user.id", "12345")
  
  match Baggage::get_entry(baggage_with_entry, "user.id") {
    Some(value) => assert_eq(value, "12345")
    None => @panic("Expected baggage entry to be found")
  }
  
  // Test non-existent entry
  match Baggage::get_entry(baggage, "nonexistent") {
    None => () // Expected
    Some(_) => @panic("Expected None for non-existent entry")
  }
  
  // Test entry removal
  let baggage_without_entry = Baggage::remove_entry(baggage_with_entry, "user.id")
  match Baggage::get_entry(baggage_without_entry, "user.id") {
    None => () // Expected after removal
    Some(_) => @panic("Expected entry to be removed")
  }
}

test "http_client_operations" {
  let client = HttpClient::new()
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token")]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some("{\"test\": true}"))
  
  // Test request properties
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  
  match HttpRequest::body(request) {
    Some(body) => assert_eq(body, "{\"test\": true}")
    None => @panic("Expected request body to be present")
  }
  
  // Test response creation
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("{\"success\": true}"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  
  match HttpResponse::body(response) {
    Some(body) => assert_eq(body, "{\"success\": true}")
    None => @panic("Expected response body to be present")
  }
}

test "platform_utilities" {
  // Test Clock
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  assert_eq(timestamp > 0L, true)
  
  // Test Random
  let random = Random::system()
  let bytes = Random::next_bytes(random, 16)
  assert_eq(Array::length(bytes), 16)
  
  let random_u64 = Random::next_u64(random)
  assert_eq(random_u64 > 0UL, true)
}