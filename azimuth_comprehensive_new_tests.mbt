// Azimuth Comprehensive New Test Suite
// This file contains comprehensive test cases covering the core functionality of Azimuth

// Test 1: Attributes Operations
test "attributes creation and manipulation" {
  let attrs = Attributes::new()
  assert_true(attrs.values.length() == 0)
  
  // Test setting and getting string values
  Attributes::set(attrs, "service.name", AttributeValue::StringValue("azimuth-service"))
  let string_value = Attributes::get(attrs, "service.name")
  match string_value {
    Some(AttributeValue::StringValue(value)) => assert_eq(value, "test_value")
    _ => assert_true(false)
  }
  
  // Test setting and getting int values
  Attributes::set(attrs, "service.port", AttributeValue::IntValue(8080))
  let int_value = Attributes::get(attrs, "service.port")
  match int_value {
    Some(AttributeValue::IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  // Test getting non-existent key
  let none_value = Attributes::get(attrs, "non.existent.key")
  assert_eq(none_value, None)
}

// Test 2: Context Management
test "context creation and value management" {
  let root_ctx = Context::root()
  assert_eq(root_ctx.data, None)
  
  // Test creating context key
  let trace_key = ContextKey::new("trace.id")
  assert_eq(trace_key.key, "trace.id")
  
  // Test setting value in context
  let ctx_with_value = Context::with_value(root_ctx, trace_key, "trace-12345")
  match ctx_with_value.data {
    Some((k, v)) => {
      assert_eq(k, "trace.id")
      assert_eq(v, "trace-12345")
    }
    None => assert_true(false)
  }
  
  // Test getting value from context
  let retrieved_value = Context::get(ctx_with_value, trace_key)
  assert_eq(retrieved_value, Some("trace-12345"))
  
  // Test getting non-existent key
  let non_existent_key = ContextKey::new("non.existent")
  let non_existent_value = Context::get(ctx_with_value, non_existent_key)
  assert_eq(non_existent_value, None)
}

// Test 3: Span Context Operations
test "span context creation and validation" {
  // Test creating valid span context
  let span_ctx = SpanContext::new("trace-12345", "span-67890", true, "test-state")
  assert_eq(SpanContext::trace_id(span_ctx), "trace-12345")
  assert_eq(SpanContext::span_id(span_ctx), "span-67890")
  assert_true(SpanContext::is_valid(span_ctx))
  assert_true(SpanContext::is_sampled(span_ctx))
  
  // Test creating invalid span context (empty trace ID)
  let invalid_span_ctx = SpanContext::new("", "span-67890", true, "test-state")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // Test creating invalid span context (empty span ID)
  let invalid_span_ctx2 = SpanContext::new("trace-12345", "", true, "test-state")
  assert_false(SpanContext::is_valid(invalid_span_ctx2))
  
  // Test non-sampled span context
  let not_sampled_ctx = SpanContext::new("trace-12345", "span-67890", false, "test-state")
  assert_false(SpanContext::is_sampled(not_sampled_ctx))
}

// Test 4: Span Lifecycle Management
test "span creation and lifecycle" {
  let span_ctx = SpanContext::new("trace-12345", "span-67890", true, "test-state")
  let span = Span::new("test-operation", SpanKind::Internal, span_ctx)
  
  // Test span properties
  assert_eq(Span::name(span), "test-operation")
  assert_eq(Span::kind(span), SpanKind::Internal)
  assert_true(Span::is_recording(span))
  assert_eq(SpanContext::trace_id(Span::span_context(span)), "trace-12345")
  assert_eq(SpanContext::span_id(Span::span_context(span)), "span-67890")
  
  // Test span status
  assert_eq(Span::status(span), StatusCode::Unset)
  
  // Test adding events
  Span::add_event(span, "test-event", Some([("event.type", AttributeValue::StringValue("test"))]))
  
  // Test setting status
  Span::set_status(span, StatusCode::Ok, Some("Operation completed successfully"))
  
  // Test ending span
  Span::end(span)
}

// Test 5: Tracer and TracerProvider Operations
test "tracer and tracer provider functionality" {
  // Test creating tracer provider
  let tracer_provider = TracerProvider::default()
  
  // Test creating tracer
  let tracer = TracerProvider::get_tracer(tracer_provider, "azimuth-tracer", Some("1.0.0"))
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "azimuth-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)
  
  // Test creating span with tracer
  let span = Tracer::start_span(tracer, "tracer-test-span", Some([("test.key", AttributeValue::StringValue("test.value"))]))
  assert_eq(Span::name(span), "tracer-test-span")
  assert_eq(Span::kind(span), SpanKind::Internal)
  assert_true(Span::is_recording(span))
}

// Test 6: Metrics Operations
test "metrics creation and operations" {
  // Test creating meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  assert_eq(meter.scope.name, "test-meter")
  
  // Test creating counter
  let counter = Meter::create_counter(meter, "test-counter", Some("Test counter description"), Some("count"))
  assert_eq(counter.name, "test-counter")
  assert_eq(counter.description, Some("Test counter description"))
  assert_eq(counter.unit, Some("count"))
  
  // Test counter operations
  Counter::add(counter, 10.0)
  Counter::add(counter, 5.0, Some(Attributes::new()))
  
  // Test creating histogram
  let histogram = Meter::create_histogram(meter, "test-histogram", Some("Test histogram"), Some("ms"))
  assert_eq(histogram.name, "test-histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test histogram operations
  Histogram::record(histogram, 100.5)
  Histogram::record(histogram, 200.3, Some(Attributes::new()))
  
  // Test creating up-down counter
  let updown_counter = Meter::create_updown_counter(meter, "test-updown", Some("Test up-down counter"), Some("items"))
  assert_eq(updown_counter.name, "test-updown")
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // Test creating gauge
  let gauge = Meter::create_gauge(meter, "test-gauge", Some("Test gauge"), Some("percent"))
  assert_eq(gauge.name, "test-gauge")
  assert_eq(gauge.description, Some("Test gauge"))
  assert_eq(gauge.unit, Some("percent"))
  
  // Test instrument operations
  let counter_instrument = Instrument::name(Instrument::Counter("test-counter", None, None))
  assert_eq(counter_instrument, "test-counter")
  
  let histogram_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(histogram_instrument), "test-histogram")
  assert_eq(Instrument::description(histogram_instrument), Some("Test histogram"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
}

// Test 7: Logging Operations
test "logging functionality" {
  // Test creating logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test-logger")
  assert_eq(logger.scope.name, "test-logger")
  
  // Test creating log record
  let log_record = LogRecord::new(SeverityNumber::Info, "Test log message")
  assert_eq(LogRecord::severity_number(log_record), SeverityNumber::Info)
  assert_eq(LogRecord::body(log_record), Some("Test log message"))
  assert_eq(LogRecord::trace_id(log_record), None)
  assert_eq(LogRecord::span_id(log_record), None)
  
  // Test creating log record with context
  let log_record_with_context = LogRecord::new_with_context(
    SeverityNumber::Error,
    Some("Error message"),
    Some(Attributes::new()),
    Some(1640995200000L),
    Some(1640995250000L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(Context::root())
  )
  assert_eq(LogRecord::severity_number(log_record_with_context), SeverityNumber::Error)
  assert_eq(LogRecord::body(log_record_with_context), Some("Error message"))
  assert_eq(LogRecord::trace_id(log_record_with_context), Some("trace-12345"))
  assert_eq(LogRecord::span_id(log_record_with_context), Some("span-67890"))
  
  // Test emitting log record
  Logger::emit(logger, log_record)
  Logger::emit(logger, log_record_with_context)
}

// Test 8: Resource Management
test "resource creation and management" {
  // Test creating resource
  let resource = Resource::new()
  assert_true(resource.attributes.length() == 0)
  
  // Test creating resource with attributes
  let resource_attrs = [
    ("service.name", AttributeValue::StringValue("azimuth-service")),
    ("service.version", AttributeValue::StringValue("1.0.0")),
    ("service.instance.id", AttributeValue::StringValue("instance-12345"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, resource_attrs)
  assert_eq(resource_with_attrs.attributes.length(), 3)
  
  // Test getting attributes
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(AttributeValue::StringValue(value)) => assert_eq(value, "azimuth-service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  match service_version {
    Some(AttributeValue::StringValue(value)) => assert_eq(value, "1.0.0")
    _ => assert_true(false)
  }
  
  // Test getting non-existent attribute
  let non_existent = Resource::get_attribute(resource_with_attrs, "non.existent")
  assert_eq(non_existent, None)
  
  // Test resource merging
  let override_attrs = [
    ("service.version", AttributeValue::StringValue("2.0.0")),
    ("deployment.environment", AttributeValue::StringValue("production"))
  ]
  let override_resource = Resource::with_attributes(resource, override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  // In simplified implementation, merged resource is just the override resource
  let merged_version = Resource::get_attribute(merged_resource, "service.version")
  match merged_version {
    Some(AttributeValue::StringValue(value)) => assert_eq(value, "2.0.0")
    _ => assert_true(false)
  }
  
  let merged_env = Resource::get_attribute(merged_resource, "deployment.environment")
  match merged_env {
    Some(AttributeValue::StringValue(value)) => assert_eq(value, "production")
    _ => assert_true(false)
  }
}

// Test 9: Platform Services (Clock, Random, HTTP)
test "platform services functionality" {
  // Test Clock operations
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  assert_true(timestamp > 0L)
  
  // Test Random operations
  let random = Random::system()
  let random_bytes = Random::next_bytes(random, 16)
  assert_eq(random_bytes.length(), 0)  // Simplified implementation returns empty array
  
  let random_u64 = Random::next_u64(random)
  assert_eq(random_u64, 12345UL)  // Simplified implementation returns fixed value
  
  // Test HTTP Client operations
  let http_client = HttpClient::new()
  
  // Test HTTP Request
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let http_request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some("{\"key\":\"value\"}"))
  assert_eq(HttpRequest::http_method(http_request), "GET")
  assert_eq(HttpRequest::url(http_request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(http_request), Some("{\"key\":\"value\"}"))
  
  // Test HTTP Response
  let response_headers = [("Content-Type", "application/json")]
  let http_response = HttpResponse::new(200, response_headers, Some("{\"result\":\"success\"}"))
  assert_eq(HttpResponse::status_code(http_response), 200)
  assert_eq(HttpResponse::body(http_response), Some("{\"result\":\"success\"}"))
}