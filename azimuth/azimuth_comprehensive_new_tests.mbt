// Comprehensive New Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases covering core functionality

// Test 1: Attributes operations
test "attributes creation and manipulation" {
  let attrs = Attributes::new()
  
  // Test setting and getting string attributes
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  let string_value = Attributes::get(attrs, "string.key")
  assert_eq(string_value, Some(StringValue("test_value")))
  
  // Test setting and getting int attributes
  Attributes::set(attrs, "int.key", IntValue(42))
  let int_value = Attributes::get(attrs, "int.key")
  assert_eq(int_value, Some(IntValue(42)))
  
  // Test getting non-existent attribute
  let missing_value = Attributes::get(attrs, "missing.key")
  assert_eq(missing_value, None)
}

// Test 2: Span lifecycle management
test "span lifecycle operations" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  let span = Span::new("test-span", Server, span_ctx)
  
  // Test span properties
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  
  // Test span context
  let retrieved_ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(retrieved_ctx), trace_id)
  assert_eq(SpanContext::span_id(retrieved_ctx), span_id)
  assert_true(SpanContext::is_sampled(retrieved_ctx))
  
  // Test span status
  assert_eq(Span::status(span), Unset)
  
  // Test adding events
  Span::add_event(span, "test-event", Some([("event.key", StringValue("event-value"))]))
  
  // Test ending span
  Span::end(span)
}

// Test 3: Metrics operations
test "metrics creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test counter
  let counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, Some("Total HTTP requests"))
  assert_eq(counter.unit, Some("requests"))
  
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  
  // Test histogram
  let histogram = Meter::create_histogram(meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  assert_eq(histogram.name, "http.request.duration")
  assert_eq(histogram.description, Some("HTTP request duration"))
  assert_eq(histogram.unit, Some("ms"))
  
  Histogram::record(histogram, 100.5)
  Histogram::record(histogram, 200.0)
  
  // Test updown counter
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  assert_eq(updown_counter.name, "active.connections")
  
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -5.0)
  
  // Test gauge
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  assert_eq(gauge.name, "memory.usage")
  
  // Note: Gauge::set is not implemented in azimuth.mbt, so we can't test it here
  
  // Test instrument type operations
  let counter_instrument = Counter("test.counter", Some("Test counter"), Some("count"))
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
}

// Test 4: Log record operations
test "log record creation and operations" {
  // Test basic log record
  let record = LogRecord::new(Info, "Test log message")
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  assert_eq(LogRecord::trace_id(record), None)
  assert_eq(LogRecord::span_id(record), None)
  
  // Test log record with full context
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.key", StringValue("log-value"))
  
  let full_record = LogRecord::new_with_context(
    Error,
    Some("Error occurred"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(full_record), Error)
  assert_eq(LogRecord::body(full_record), Some("Error occurred"))
  assert_eq(LogRecord::trace_id(full_record), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(LogRecord::span_id(full_record), Some("b7ad6b7169203331"))
}

// Test 5: Context propagation
test "context propagation operations" {
  let root_ctx = Context::root()
  let key1 = ContextKey::new("test.key1")
  let key2 = ContextKey::new("test.key2")
  
  // Test context with single value
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  let retrieved1 = Context::get(ctx1, key1)
  assert_eq(retrieved1, Some("value1"))
  
  // Test context with different key
  let retrieved_missing = Context::get(ctx1, key2)
  assert_eq(retrieved_missing, None)
  
  // Test baggage operations
  let baggage = Baggage::new()
  let baggage_with_entry = Baggage::set_entry(baggage, "baggage.key", "baggage.value")
  let retrieved_baggage = Baggage::get_entry(baggage_with_entry, "baggage.key")
  assert_eq(retrieved_baggage, Some("baggage.value"))
  
  // Test baggage removal
  let baggage_after_removal = Baggage::remove_entry(baggage_with_entry, "baggage.key")
  let removed_retrieved = Baggage::get_entry(baggage_after_removal, "baggage.key")
  assert_eq(removed_retrieved, None)
}

// Test 6: Resource management
test "resource creation and operations" {
  // Test empty resource
  let resource = Resource::new()
  let missing_attr = Resource::get_attribute(resource, "missing.key")
  assert_eq(missing_attr, None)
  
  // Test resource with attributes
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let service_instance_id = Resource::get_attribute(resource_with_attrs, "service.instance.id")
  
  assert_eq(service_name, Some(StringValue("test-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(service_instance_id, Some(StringValue("instance-123")))
  
  // Test resource merging
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("environment", StringValue("production"))
  ]
  let override_resource = Resource::with_attributes(resource, override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  // In simplified implementation, override resource takes precedence
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let merged_environment = Resource::get_attribute(merged_resource, "environment")
  
  assert_eq(merged_service_name, Some(StringValue("override-service")))
  assert_eq(merged_environment, Some(StringValue("production")))
}

// Test 7: Propagator operations
test "propagator injection and extraction" {
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test W3C Trace Context propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  
  // Test W3C Baggage propagator
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Test Composite propagator
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Test injection
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(injected_traceparent, Some("00-test-trace-id-test-span-id-01"))
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

// Test 8: HTTP client operations
test "http client request and response" {
  // Test HTTP request
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123")
  ]
  let request = HttpRequest::new("POST", "https://api.example.com/data", headers, Some("{\"key\":\"value\"}"))
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("{\"key\":\"value\"}"))
  
  // Test HTTP response
  let response_headers = [
    ("Content-Type", "application/json"),
    ("Content-Length", "42")
  ]
  let response = HttpResponse::new(201, response_headers, Some("{\"id\":123,\"status\":\"created\"}"))
  
  assert_eq(HttpResponse::status_code(response), 201)
  assert_eq(HttpResponse::body(response), Some("{\"id\":123,\"status\":\"created\"}"))
}

// Test 9: Boundary conditions and edge cases
test "boundary conditions and edge cases" {
  // Test empty trace context
  let empty_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(empty_ctx))
  assert_false(SpanContext::is_sampled(empty_ctx))
  
  // Test valid trace context
  let valid_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key1=value1")
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
  
  // Test empty baggage
  let empty_baggage = Baggage::new()
  let empty_entry = Baggage::get_entry(empty_baggage, "any.key")
  assert_eq(empty_entry, None)
  
  // Test empty attributes
  let empty_attrs = Attributes::new()
  let empty_attr = Attributes::get(empty_attrs, "any.key")
  assert_eq(empty_attr, None)
  
  // Test empty carrier
  let empty_carrier = TextMapCarrier::new()
  let empty_header = TextMapCarrier::get(empty_carrier, "any.header")
  assert_eq(empty_header, None)
  
  // Test log record with no body
  let no_body_record = LogRecord::new(Debug, "")
  assert_eq(LogRecord::body(no_body_record), Some(""))
  
  // Test instrumentation scope with optional values
  let scope_with_options = InstrumentationScope::{
    name: "test-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  assert_eq(scope_with_options.name, "test-scope")
  assert_eq(scope_with_options.version, Some("1.0.0"))
  assert_eq(scope_with_options.schema_url, Some("https://example.com/schema"))
  
  let scope_without_options = InstrumentationScope::{
    name: "minimal-scope",
    version: None,
    schema_url: None
  }
  assert_eq(scope_without_options.name, "minimal-scope")
  assert_eq(scope_without_options.version, None)
  assert_eq(scope_without_options.schema_url, None)
}

// Test 10: Logger operations
test "logger creation and operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // Test log record emission
  let info_record = LogRecord::new(Info, "Information message")
  Logger::emit(logger, info_record)
  
  let error_record = LogRecord::new(Error, "Error message")
  Logger::emit(logger, error_record)
  
  // Test logger scope
  assert_eq(logger.scope.name, "test-logger")
  assert_eq(logger.scope.version, None)
  assert_eq(logger.scope.schema_url, None)
  
  // Test logger with scope options
  let logger_with_options = LoggerProvider::get_logger(provider, "scoped-logger")
  assert_eq(logger_with_options.scope.name, "scoped-logger")
}