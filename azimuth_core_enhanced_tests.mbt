// Azimuth Core Enhanced Test Suite
// Enhanced test cases for Azimuth telemetry system core functionality

// Test 1: Attribute Value Type Conversion and Validation
test "attribute value type conversion and validation" {
  let string_attr = AttributeValue::StringValue("test_value")
  let int_attr = AttributeValue::IntValue(42)
  let float_attr = AttributeValue::FloatValue(3.14)
  let bool_attr = AttributeValue::BoolValue(true)
  let array_string_attr = AttributeValue::ArrayStringValue(["a", "b", "c"])
  let array_int_attr = AttributeValue::ArrayIntValue([1, 2, 3])
  
  // Test attribute creation and type safety
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", string_attr)
  Attributes::set(attrs, "int.key", int_attr)
  Attributes::set(attrs, "float.key", float_attr)
  Attributes::set(attrs, "bool.key", bool_attr)
  
  // Test attribute retrieval
  let retrieved_string = Attributes::get(attrs, "string.key")
  let retrieved_int = Attributes::get(attrs, "int.key")
  let retrieved_nonexistent = Attributes::get(attrs, "nonexistent.key")
  
  match retrieved_string {
    Some(AttributeValue::StringValue(v)) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  match retrieved_int {
    Some(AttributeValue::IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  match retrieved_nonexistent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Resource Management and Merging
test "resource management and merging operations" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", AttributeValue::StringValue("azimuth-service")),
    ("service.version", AttributeValue::StringValue("1.0.0")),
    ("host.name", AttributeValue::StringValue("localhost"))
  ]
  let resource_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.version", AttributeValue::StringValue("2.0.0")),
    ("environment", AttributeValue::StringValue("production"))
  ]
  let resource_with_override = Resource::with_attributes(override_resource, override_attrs)
  
  // Test resource attribute retrieval
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  
  match service_name {
    Some(AttributeValue::StringValue(v)) => assert_eq(v, "azimuth-service")
    _ => assert_true(false)
  }
  
  match service_version {
    Some(AttributeValue::StringValue(v)) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
  
  // Test resource merging
  let merged_resource = Resource::merge(resource_with_attrs, resource_with_override)
  let merged_version = Resource::get_attribute(merged_resource, "service.version")
  let merged_env = Resource::get_attribute(merged_resource, "environment")
  
  match merged_version {
    Some(AttributeValue::StringValue(v)) => assert_eq(v, "2.0.0")
    _ => assert_true(false)
  }
  
  match merged_env {
    Some(AttributeValue::StringValue(v)) => assert_eq(v, "production")
    _ => assert_true(false)
  }
}

// Test 3: Context Propagation and Baggage Operations
test "context propagation and baggage operations" {
  let root_ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  
  // Test context with values
  let ctx_with_user = Context::with_value(root_ctx, user_key, "user123")
  let ctx_with_session = Context::with_value(ctx_with_user, session_key, "session456")
  
  let retrieved_user = Context::get(ctx_with_session, user_key)
  let retrieved_session = Context::get(ctx_with_session, session_key)
  let non_existent = Context::get(ctx_with_session, ContextKey::new("nonexistent"))
  
  match retrieved_user {
    Some(v) => assert_eq(v, "user123")
    None => assert_true(false)
  }
  
  match retrieved_session {
    Some(v) => assert_eq(v, "session456")
    None => assert_true(false)
  }
  
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test baggage operations
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session456")
  
  let retrieved_baggage_user = Baggage::get_entry(baggage_with_session, "user.id")
  let retrieved_baggage_session = Baggage::get_entry(baggage_with_session, "session.id")
  
  match retrieved_baggage_user {
    Some(v) => assert_eq(v, "user123")
    None => assert_true(false)
  }
  
  match retrieved_baggage_session {
    Some(v) => assert_eq(v, "session456")
    None => assert_true(false)
  }
}

// Test 4: Span Lifecycle and Context Management
test "span lifecycle and context management" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_context = SpanContext::new(trace_id, span_id, true, "test-state")
  
  // Test span context properties
  assert_eq(SpanContext::trace_id(span_context), trace_id)
  assert_eq(SpanContext::span_id(span_context), span_id)
  assert_true(SpanContext::is_valid(span_context))
  assert_true(SpanContext::is_sampled(span_context))
  
  // Test span creation and lifecycle
  let span = Span::new("test-span", Server, span_context)
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  
  // Test span operations
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  Span::add_event(span, "test-event", Some([("event.type", AttributeValue::StringValue("test"))]))
  Span::end(span)
  
  // Test tracer operations
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test-tracer", Some("1.0.0"))
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  
  let new_span = Tracer::start_span(tracer, "new-test-span")
  assert_eq(Span::name(new_span), "new-test-span")
  assert_eq(Span::kind(new_span), Internal)
}

// Test 5: Metrics Instruments and Operations
test "metrics instruments and operations" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  // Test counter creation and operations
  let counter = Meter::create_counter(meter, "test-counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5)
  
  // Test histogram creation and operations
  let histogram = Meter::create_histogram(meter, "test-histogram", Some("Test histogram"), Some("ms"))
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  
  // Test up-down counter creation and operations
  let updown_counter = Meter::create_updown_counter(meter, "test-updown", Some("Test up-down counter"), Some("value"))
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -5.0)
  
  // Test gauge creation and operations
  let gauge = Meter::create_gauge(meter, "test-gauge", Some("Test gauge"), Some("value"))
  
  // Test instrument type conversions
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(counter_instrument), "test-counter")
  assert_eq(Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  
  let histogram_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(histogram_instrument), "test-histogram")
}

// Test 6: Logging System and Severity Levels
test "logging system and severity levels" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test-logger")
  
  // Test log record creation with different severity levels
  let trace_log = LogRecord::new(Trace, "Trace message")
  let debug_log = LogRecord::new(Debug, "Debug message")
  let info_log = LogRecord::new(Info, "Info message")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error message")
  let fatal_log = LogRecord::new(Fatal, "Fatal message")
  
  // Test severity number retrieval
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // Test log record body retrieval
  match LogRecord::body(info_log) {
    Some(msg) => assert_eq(msg, "Info message")
    None => assert_true(false)
  }
  
  // Test enhanced log record creation with context
  let attrs = Attributes::new()
  let enhanced_log = LogRecord::new_with_context(
    Info,
    Some("Enhanced log message"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("test_trace_id"),
    Some("test_span_id"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::trace_id(enhanced_log), Some("test_trace_id"))
  assert_eq(LogRecord::span_id(enhanced_log), Some("test_span_id"))
  
  // Test log emission
  Logger::emit(logger, enhanced_log)
}

// Test 7: HTTP Client and Network Operations
test "http client and network operations" {
  let http_client = HttpClient::new()
  
  // Test HTTP request creation
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some("{\"test\": \"data\"}"))
  
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  
  match HttpRequest::body(request) {
    Some(body) => assert_eq(body, "{\"test\": \"data\"}")
    None => assert_true(false)
  }
  
  // Test HTTP response creation
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("{\"result\": \"success\"}"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  
  match HttpResponse::body(response) {
    Some(body) => assert_eq(body, "{\"result\": \"success\"}")
    None => assert_true(false)
  }
}

// Test 8: Time Operations and Clock Functionality
test "time operations and clock functionality" {
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  // Test timestamp is reasonable (should be > 0)
  assert_true(timestamp > 0L)
  
  // Test timestamp is in expected range (around 2025)
  assert_true(timestamp > 1700000000000000000L)
  assert_true(timestamp < 1800000000000000000L)
  
  // Test random number generation
  let random = Random::system()
  let random_bytes = Random::next_bytes(random, 16)
  assert_eq(random_bytes.length(), 16)
  
  let random_u64 = Random::next_u64(random)
  assert_true(random_u64 > 0UL)
}

// Test 9: Composite Propagator and Context Injection/Extraction
test "composite propagator and context injection/extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  let ctx = Context::with_value(Context::root(), ContextKey::new("test.key"), "test.value")
  
  // Test injection
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  match extracted_value {
    Some(v) => assert_eq(v, "true")
    None => assert_true(false)
  }
  
  // Test carrier operations
  let trace_value = TextMapCarrier::get(carrier, "traceparent")
  match trace_value {
    Some(v) => assert_true(v.length() > 0)
    None => assert_true(false)
  }
}

// Test 10: Edge Cases and Error Handling
test "edge cases and error handling" {
  // Test empty span context
  let empty_span_context = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(empty_span_context))
  assert_false(SpanContext::is_sampled(empty_span_context))
  
  // Test empty attributes
  let empty_attrs = Attributes::new()
  let non_existent_attr = Attributes::get(empty_attrs, "nonexistent")
  match non_existent_attr {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test empty baggage
  let empty_baggage = Baggage::new()
  let non_existent_entry = Baggage::get_entry(empty_baggage, "nonexistent")
  match non_existent_entry {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test empty resource
  let empty_resource = Resource::new()
  let non_existent_resource_attr = Resource::get_attribute(empty_resource, "nonexistent")
  match non_existent_resource_attr {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test context with no data
  let empty_ctx = Context::root()
  let non_existent_ctx_value = Context::get(empty_ctx, ContextKey::new("nonexistent"))
  match non_existent_ctx_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}