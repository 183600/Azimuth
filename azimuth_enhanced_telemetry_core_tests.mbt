// Azimuth Enhanced Telemetry Test Suite
// This file contains comprehensive test cases for Azimuth telemetry system

// Test 1: Attribute System Operations
test "attribute system operations" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  let string_val = Attributes::get(attrs, "string.key")
  let int_val = Attributes::get(attrs, "int.key")
  let float_val = Attributes::get(attrs, "float.key")
  let bool_val = Attributes::get(attrs, "bool.key")
  let missing_val = Attributes::get(attrs, "missing.key")
  
  match string_val {
    Some(StringValue(v)) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  match int_val {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  match float_val {
    Some(FloatValue(v)) => assert_true(v > 3.0 && v < 3.2)
    _ => assert_true(false)
  }
  
  match bool_val {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
  
  assert_eq(missing_val, None)
}

// Test 2: Context Propagation
test "context propagation operations" {
  let root_ctx = Context::root()
  let user_key = ContextKey::new("user_id")
  let session_key = ContextKey::new("session_id")
  
  let ctx_with_user = Context::with_value(root_ctx, user_key, "user123")
  let ctx_with_session = Context::with_value(ctx_with_user, session_key, "session456")
  
  let user_id = Context::get(ctx_with_session, user_key)
  let session_id = Context::get(ctx_with_session, session_key)
  let missing_value = Context::get(ctx_with_session, ContextKey::new("missing"))
  
  match user_id {
    Some(v) => assert_eq(v, "user123")
    None => assert_true(false)
  }
  
  match session_id {
    Some(v) => assert_eq(v, "session456")
    None => assert_true(false)
  }
  
  assert_eq(missing_value, None)
}

// Test 3: Span Context Operations
test "span context operations" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "state=active")
  
  assert_eq(SpanContext::trace_id(span_ctx), "trace123")
  assert_eq(SpanContext::span_id(span_ctx), "span456")
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
  
  let invalid_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_ctx))
  assert_false(SpanContext::is_sampled(invalid_ctx))
}

// Test 4: Span Lifecycle Management
test "span lifecycle management" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test_tracer")
  let span = Tracer::start_span(tracer, "test_operation")
  
  assert_eq(Span::name(span), "test_operation")
  
  match Span::kind(span) {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_true(Span::is_recording(span))
  
  Span::set_status(span, Ok, "Operation completed successfully")
  Span::add_event(span, "event1", Some([("key1", StringValue("value1"))]))
  Span::end(span)
  
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test_tracer")
}

// Test 5: Metrics System Operations
test "metrics system operations" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test_meter")
  
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "test_updown", Some("Test updown"), Some("value"))
  let gauge = Meter::create_gauge(meter, "test_gauge", Some("Test gauge"), Some("percent"))
  
  assert_eq(counter.name, "test_counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5)
  
  Histogram::record(histogram, 100.5)
  Histogram::record(histogram, 200.3)
  
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -5.0)
  
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(counter_instrument), "test_counter")
  assert_eq(Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
}

// Test 6: Log Record Operations
test "log record operations" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test_logger")
  
  let simple_record = LogRecord::new(Info, "Test log message")
  assert_eq(LogRecord::severity_number(simple_record), Info)
  
  match LogRecord::body(simple_record) {
    Some(msg) => assert_eq(msg, "Test log message")
    None => assert_true(false)
  }
  
  let detailed_record = LogRecord::new_with_context(
    Error,
    Some("Error occurred"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(detailed_record), Error)
  assert_eq(LogRecord::trace_id(detailed_record), Some("trace123"))
  assert_eq(LogRecord::span_id(detailed_record), Some("span456"))
  
  Logger::emit(logger, detailed_record)
  
  let scope = Logger::instrumentation_scope(logger)
  assert_eq(scope.name, "test_logger")
}

// Test 7: Resource Management
test "resource management operations" {
  let resource = Resource::new()
  let attrs = [("service.name", StringValue("azimuth")), ("service.version", StringValue("1.0.0"))]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let missing_attr = Resource::get_attribute(resource_with_attrs, "missing.attr")
  
  match service_name {
    Some(StringValue(v)) => assert_eq(v, "azimuth")
    _ => assert_true(false)
  }
  
  match service_version {
    Some(StringValue(v)) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
  
  assert_eq(missing_attr, None)
  
  let override_resource = Resource::with_attributes(resource, [("environment", StringValue("production"))])
  let merged = Resource::merge(resource_with_attrs, override_resource)
  
  let env = Resource::get_attribute(merged, "environment")
  match env {
    Some(StringValue(v)) => assert_eq(v, "production")
    _ => assert_true(false)
  }
}

// Test 8: HTTP Client Operations
test "http client operations" {
  let request = HttpRequest::new(
    "GET",
    "https://api.example.com/data",
    [("Content-Type", "application/json"), ("Authorization", "Bearer token123")],
    Some("{\"key\":\"value\"}")
  )
  
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  
  match HttpRequest::body(request) {
    Some(body) => assert_true(body.contains("key"))
    None => assert_true(false)
  }
  
  let response = HttpResponse::new(
    200,
    [("Content-Type", "application/json"), ("X-Request-ID", "req-123")],
    Some("{\"result\":\"success\"}")
  )
  
  assert_eq(HttpResponse::status_code(response), 200)
  
  match HttpResponse::body(response) {
    Some(body) => assert_true(body.contains("success"))
    None => assert_true(false)
  }
}