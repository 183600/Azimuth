// Comprehensive Enhanced Test Suite for Azimuth Telemetry System
// Advanced test coverage for edge cases, performance, and reliability

test "attribute_value_deep_nesting" {
  // Test deeply nested attribute values
  let nested_string = ArrayStringValue(["level1", "level2", "level3", "level4", "level5"])
  let nested_int = ArrayIntValue([1, 10, 100, 1000, 10000])
  
  // Verify nested array preservation
  match nested_string {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], "level1")
      assert_eq(arr[4], "level5")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match nested_int {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 10000)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
}

test "span_lifecycle_events" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "lifecycle_test")
  let span = Tracer::start_span(tracer, "lifecycle_span")
  
  // Test initial state
  assert_eq(Span::is_recording(span), true)
  assert_eq(Span::status(span), Unset)
  
  // Test status transitions
  Span::set_status(span, Ok, Some("Started successfully"))
  Span::add_event(span, "operation_started", Some([("operation.type", StringValue("test"))]))
  
  // Test error scenario
  Span::set_status(span, Error, Some("Simulated error"))
  Span::add_event(span, "error_occurred", Some([("error.code", IntValue(500))]))
  
  // Test span completion
  Span::end(span)
}

test "context_advanced_operations" {
  let root_ctx = Context::root()
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  let key3 = ContextKey::new("key3")
  
  // Test context chaining
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  let ctx3 = Context::with_value(ctx2, key3, "value3")
  
  // Verify all values are accessible
  assert_eq(Context::get(ctx3, key1), Some("value1"))
  assert_eq(Context::get(ctx3, key2), Some("value2"))
  assert_eq(Context::get(ctx3, key3), Some("value3"))
  
  // Test non-existent key
  let non_existent = ContextKey::new("non_existent")
  assert_eq(Context::get(ctx3, non_existent), None)
}

test "clock_random_advanced_tests" {
  let clock = Clock::system()
  let random = Random::system()
  
  // Test clock consistency
  let timestamp1 = Clock::now_unix_nanos(clock)
  let timestamp2 = Clock::now_unix_nanos(clock)
  
  // Timestamps should be reasonable (in nanoseconds since epoch)
  assert_eq(timestamp1 > 0L, true)
  assert_eq(timestamp2 >= timestamp1, true)
  
  // Test random generation
  let random_bytes = Random::next_bytes(random, 16)
  let random_u64 = Random::next_u64(random)
  
  assert_eq(random_bytes.length(), 16)
  assert_eq(random_u64 > 0UL, true)
}

test "instrumentation_scope_detailed" {
  // Test instrumentation scope with full details
  let scope1 = InstrumentationScope::{ 
    name: "test.instrument", 
    version: Some("1.2.3"), 
    schema_url: Some("https://example.com/schema") 
  }
  
  assert_eq(scope1.name, "test.instrument")
  assert_eq(scope1.version, Some("1.2.3"))
  assert_eq(scope1.schema_url, Some("https://example.com/schema"))
  
  // Test instrumentation scope with minimal details
  let scope2 = InstrumentationScope::{ 
    name: "minimal.instrument", 
    version: None, 
    schema_url: None 
  }
  
  assert_eq(scope2.name, "minimal.instrument")
  assert_eq(scope2.version, None)
  assert_eq(scope2.schema_url, None)
}

test "resource_advanced_tests" {
  let base_resource = Resource::new()
  
  // Test resource with various attribute types
  let mixed_attributes = [
    ("string.attr", StringValue("string_value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.string", ArrayStringValue(["a", "b", "c"])),
    ("array.int", ArrayIntValue([1, 2, 3]))
  ]
  
  let resource_with_mixed = Resource::with_attributes(base_resource, mixed_attributes)
  
  // Test retrieval of different attribute types
  assert_eq(
    Resource::get_attribute(resource_with_mixed, "string.attr"),
    Some(StringValue("string_value"))
  )
  
  assert_eq(
    Resource::get_attribute(resource_with_mixed, "int.attr"),
    Some(IntValue(42))
  )
  
  assert_eq(
    Resource::get_attribute(resource_with_mixed, "float.attr"),
    Some(FloatValue(3.14159))
  )
  
  // Test resource merging
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("new.attr", StringValue("new_value")),
    ("string.attr", StringValue("overridden_value"))
  ])
  
  let merged = Resource::merge(resource_with_mixed, override_resource)
  assert_eq(
    Resource::get_attribute(merged, "new.attr"),
    Some(StringValue("new_value"))
  )
}

test "log_severity_context_tests" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "severity_test")
  
  // Test all severity levels
  let severities = [Trace, Debug, Info, Warn, Error, Fatal]
  let severity_names = ["Trace", "Debug", "Info", "Warn", "Error", "Fatal"]
  
  for i in 0..<severities.length() {
    let severity = severities[i]
    let expected_name = severity_names[i]
    
    let log_record = LogRecord::new(severity, "Test message for {expected_name}")
    assert_eq(LogRecord::severity_number(log_record), severity)
    
    Logger::emit(logger, log_record)
  }
  
  // Test log record with full context
  let ctx = Context::with_value(Context::root(), ContextKey::new("request_id"), "req-123")
  let contextual_log = LogRecord::new_with_context(
    Error,
    Some("Contextual error message"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-789"),
    Some("span-456"),
    Some(ctx)
  )
  
  assert_eq(LogRecord::trace_id(contextual_log), Some("trace-789"))
  assert_eq(LogRecord::span_id(contextual_log), Some("span-456"))
}

test "meter_instruments_comprehensive" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "comprehensive_test")
  
  // Test all instrument types
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "test_updown", Some("Test up/down"), Some("items"))
  let gauge = Meter::create_gauge(meter, "test_gauge", Some("Test gauge"), Some("percent"))
  
  // Test instrument operations
  Counter::add(counter, 1.0, None)
  Counter::add(counter, 5.5, Some(Attributes::new()))
  
  Histogram::record(histogram, 100.0, None)
  Histogram::record(histogram, 200.5, Some(Attributes::new()))
  
  UpDownCounter::add(updown_counter, 10.0, None)
  UpDownCounter::add(updown_counter, -5.0, Some(Attributes::new()))
  
  // Test instrument properties
  assert_eq(counter.name, "test_counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  assert_eq(histogram.name, "test_histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
}

test "composite_propagator_advanced" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let ctx = Context::with_value(
    Context::with_value(Context::root(), ContextKey::new("key1"), "value1"),
    ContextKey::new("key2"),
    "value2"
  )
  
  let carrier = TextMapCarrier::new()
  
  // Test injection with multiple context values
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Test extraction back to context
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction preserves context structure
  let extracted_key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, extracted_key), Some("true"))
}

test "http_client_network_tests" {
  let client = HttpClient::new()
  
  // Test various HTTP methods
  let methods = ["GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS"]
  
  for method in methods {
    let headers = [("User-Agent", "azimuth-test/1.0")]
    let request = HttpRequest::new(method, "https://api.example.com/test", headers, None)
    
    assert_eq(HttpRequest::http_method(request), method)
    assert_eq(HttpRequest::url(request), "https://api.example.com/test")
    assert_eq(HttpRequest::body(request), None)
  }
  
  // Test request/response with bodies
  let json_body = "{\"test\": \"data\", \"number\": 123}"
  let post_request = HttpRequest::new(
    "POST", 
    "https://api.example.com/data", 
    [("Content-Type", "application/json"), ("Content-Length", "28")],
    Some(json_body)
  )
  
  let success_response = HttpResponse::new(
    200,
    [("Content-Type", "application/json"), ("Content-Length", "15")],
    Some("{\"success\": true}")
  )
  
  let error_response = HttpResponse::new(
    404,
    [("Content-Type", "text/plain"), ("Content-Length", "9")],
    Some("Not Found")
  )
  
  assert_eq(HttpResponse::status_code(success_response), 200)
  assert_eq(HttpResponse::body(success_response), Some("{\"success\": true}"))
  
  assert_eq(HttpResponse::status_code(error_response), 404)
  assert_eq(HttpResponse::body(error_response), Some("Not Found"))
}