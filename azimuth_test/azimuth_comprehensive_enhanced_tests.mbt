// Comprehensive Enhanced Test Suite for Azimuth Telemetry System
// This file contains focused test cases covering core telemetry functionality

test "attributes operations with different value types" {
  let attrs = Attributes::new()
  
  // Test setting and getting string attributes
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  let string_result = Attributes::get(attrs, "string.key")
  assert_true(string_result.is_some())
  
  // Test setting and getting int attributes
  Attributes::set(attrs, "int.key", IntValue(42))
  let int_result = Attributes::get(attrs, "int.key")
  assert_true(int_result.is_some())
  
  // Test non-existent key
  let missing = Attributes::get(attrs, "missing.key")
  assert_true(missing.is_none())
}

test "span lifecycle management" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  let span = Span::new("test-span", Server, span_ctx)
  
  // Test initial state
  assert_eq(Span::name(span), "test-span")
  assert_true(Span::is_recording(span))
  
  // Test span status operations
  Span::set_status(span, Ok)
  
  // Test event addition
  Span::add_event(span, "user.login")
  
  // Test span termination
  Span::end(span)
  assert_true(true)  // If we reach here, span end didn't crash
}

test "baggage propagation across contexts" {
  let baggage = Baggage::new()
  
  // Test setting baggage entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let updated_baggage2 = Baggage::set_entry(updated_baggage, "session.id", "abcdef")
  
  // Test retrieving baggage entries
  let user_id = Baggage::get_entry(updated_baggage2, "user.id")
  let session_id = Baggage::get_entry(updated_baggage2, "session.id")
  let missing = Baggage::get_entry(updated_baggage2, "missing")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(session_id, Some("abcdef"))
  assert_eq(missing, None)
  
  // Test baggage removal
  let final_baggage = Baggage::remove_entry(updated_baggage2, "session.id")
  let removed_session = Baggage::get_entry(final_baggage, "session.id")
  assert_eq(removed_session, None)
}

test "resource management and merging" {
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  let attrs1 = [("service.name", StringValue("payment-service")), ("service.version", StringValue("1.0.0"))]
  let attrs2 = [("service.namespace", StringValue("production")), ("service.instance.id", StringValue("instance-123"))]
  
  let resource1_with_attrs = Resource::with_attributes(resource1, attrs1)
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  
  // Test attribute retrieval
  let service_name = Resource::get_attribute(resource1_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource1_with_attrs, "service.version")
  
  assert_true(service_name.is_some())
  assert_true(service_version.is_some())
  
  // Test resource merging
  let merged = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  let namespace = Resource::get_attribute(merged, "service.namespace")
  assert_true(namespace.is_some())
}

test "composite propagator injection and extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test injection
  CompositePropagator::inject(composite, ctx, carrier)
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_trace.is_some())
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extraction_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extraction_key)
  assert_eq(extracted_value, Some("true"))
}

test "advanced log record with full context" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.level", StringValue("INFO"))
  
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_correlation = Context::with_value(ctx, key, "corr-12345")
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Database connection timeout"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(ctx_with_correlation)
  )
  
  assert_eq(LogRecord::body(record), Some("Database connection timeout"))
  assert_eq(LogRecord::trace_id(record), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(LogRecord::span_id(record), Some("b7ad6b7169203331"))
  
  // Test log emission
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  Logger::emit(logger, record)
  assert_true(true)  // If we reach here, emit didn't crash
}

test "metrics operations with different instrument types" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-metrics")
  
  // Test Counter
  let counter = Meter::create_counter(meter, "http.requests.total")
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  
  // Test Histogram
  let histogram = Meter::create_histogram(meter, "http.request.duration")
  Histogram::record(histogram, 150.5)
  Histogram::record(histogram, 200.0)
  
  // Test UpDownCounter
  let updown_counter = Meter::create_updown_counter(meter, "active.connections")
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // Test Gauge
  let gauge = Meter::create_gauge(meter, "memory.usage")
  assert_eq(gauge.name, "memory.usage")
  assert_true(gauge.description.is_some())
  assert_true(gauge.unit.is_some())
}

test "context chain propagation with multiple values" {
  let root_ctx = Context::root()
  
  // Create context chain with multiple values
  let key1 = ContextKey::new("request.id")
  let key2 = ContextKey::new("user.id")
  let key3 = ContextKey::new("trace.id")
  
  let ctx1 = Context::with_value(root_ctx, key1, "req-12345")
  let ctx2 = Context::with_value(ctx1, key2, "user-67890")
  let ctx3 = Context::with_value(ctx2, key3, "trace-abcdef")
  
  // Test that all values are accessible
  let request_id = Context::get(ctx3, key1)
  let user_id = Context::get(ctx3, key2)
  let trace_id = Context::get(ctx3, key3)
  
  assert_eq(request_id, Some("req-12345"))
  assert_eq(user_id, Some("user-67890"))
  assert_eq(trace_id, Some("trace-abcdef"))
  
  // Test that missing keys return None
  let missing_key = ContextKey::new("missing")
  let missing_value = Context::get(ctx3, missing_key)
  assert_eq(missing_value, None)
}