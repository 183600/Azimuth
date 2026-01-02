// Focused Telemetry Tests for Azimuth System
// This file contains focused test cases covering core telemetry functionality

// Test 1: Basic span context operations
test "basic span context operations" {
  // Test span context creation and validation
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  // Verify span context integrity
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Test invalid span context
  let invalid_ctx = SpanContext::new("", "span-123", true, "")
  assert_false(SpanContext::is_valid(invalid_ctx))
}

// Test 2: Metrics operations with different instruments
test "metrics operations with different instruments" {
  // Test metrics creation and operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "http.requests.total")
  
  // Test counter properties
  assert_eq(counter.name, "http.requests.total")
  
  // Test counter operations
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 10.5)
  
  // Test histogram
  let histogram = Meter::create_histogram(meter, "response.time")
  assert_eq(histogram.name, "response.time")
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 250.0)
  
  // Test updown counter
  let updown_counter = Meter::create_updown_counter(meter, "active.connections")
  assert_eq(updown_counter.name, "active.connections")
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // Test gauge
  let gauge = Meter::create_gauge(meter, "cpu.usage")
  assert_eq(gauge.name, "cpu.usage")
}

// Test 3: Log record with context validation
test "log record with context validation" {
  // Test log record creation with context
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_value = Context::with_value(ctx, key, "corr-12345")
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Warning with context"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-123"),
    Some("span-456"),
    Some(ctx_with_value)
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::body(record), Some("Warning with context"))
  assert_eq(LogRecord::trace_id(record), Some("trace-123"))
  assert_eq(LogRecord::span_id(record), Some("span-456"))
}

// Test 4: Context propagation validation
test "context propagation validation" {
  // Test context propagation operations
  let initial_ctx = Context::root()
  let key = ContextKey::new("trace.id")
  let ctx_with_trace = Context::with_value(initial_ctx, key, "trace-12345")
  
  // Test context value retrieval
  let retrieved_value = Context::get(ctx_with_trace, key)
  assert_eq(retrieved_value, Some("trace-12345"))
  
  // Test nested context
  let nested_key = ContextKey::new("user.id")
  let nested_ctx = Context::with_value(ctx_with_trace, nested_key, "user-67890")
  
  let retrieved_trace = Context::get(nested_ctx, key)
  let retrieved_user = Context::get(nested_ctx, nested_key)
  
  assert_eq(retrieved_trace, Some("trace-12345"))
  assert_eq(retrieved_user, Some("user-67890"))
}

// Test 5: Resource attributes operations
test "resource attributes operations" {
  // Test resource creation with attributes
  let resource = Resource::new()
  let attrs = [
    ("service.name", "azimuth.service"),
    ("service.version", "1.0.0"),
    ("deployment.environment", "production")
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Verify resource attributes
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.name"), Some("azimuth.service"))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.version"), Some("1.0.0"))
  assert_eq(Resource::get_attribute(resource_with_attrs, "deployment.environment"), Some("production"))
  
  // Test non-existent attribute
  assert_eq(Resource::get_attribute(resource_with_attrs, "non.existent"), None)
}

// Test 6: Baggage operations
test "baggage operations" {
  // Test baggage creation and operations
  let baggage = Baggage::new()
  let baggage_with_entry = Baggage::set_entry(baggage, "user.id", "user-12345")
  
  // Verify baggage entry
  assert_eq(Baggage::get_entry(baggage_with_entry, "user.id"), Some("user-12345"))
  
  // Test multiple baggage entries
  let baggage_multiple = Baggage::set_entry(baggage_with_entry, "request.id", "req-67890")
  
  assert_eq(Baggage::get_entry(baggage_multiple, "user.id"), Some("user-12345"))
  assert_eq(Baggage::get_entry(baggage_multiple, "request.id"), Some("req-67890"))
  
  // Test baggage removal
  let baggage_removed = Baggage::delete_entry(baggage_multiple, "user.id")
  assert_eq(Baggage::get_entry(baggage_removed, "user.id"), None)
  assert_eq(Baggage::get_entry(baggage_removed, "request.id"), Some("req-67890"))
}

// Test 7: Propagator operations
test "propagator operations" {
  // Test composite propagator
  let text_map_propagator = TextMapPropagator::new()
  let composite_propagator = CompositePropagator::new([text_map_propagator])
  
  // Test injection
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  Propagator::inject(composite_propagator, ctx, carrier)
  
  // Test extraction
  let extracted_ctx = Propagator::extract(composite_propagator, carrier)
  assert_true(Context::is_valid(extracted_ctx))
}

// Test 8: Attribute value operations
test "attribute value operations" {
  // Test different attribute value types
  let string_attr = StringValue("test.value")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14159)
  let bool_attr = BoolValue(true)
  
  // Test attribute value conversion
  assert_eq(AttributeValue::as_string(string_attr), Some("test.value"))
  assert_eq(AttributeValue::as_int(int_attr), Some(42))
  assert_eq(AttributeValue::as_float(float_attr), Some(3.14159))
  assert_eq(AttributeValue::as_bool(bool_attr), Some(true))
  
  // Test array attributes
  let string_array = ArrayStringValue(["item1", "item2", "item3"])
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  
  assert_eq(AttributeValue::as_string_array(string_array), Some(["item1", "item2", "item3"]))
  assert_eq(AttributeValue::as_int_array(int_array), Some([1, 2, 3, 4, 5]))
}

// Test 9: Span kind and status operations
test "span kind and status operations" {
  // Test span kinds
  let client_span = SpanKind::Client
  let server_span = SpanKind::Server
  let producer_span = SpanKind::Producer
  let consumer_span = SpanKind::Consumer
  
  assert_eq(SpanKind::to_string(client_span), "CLIENT")
  assert_eq(SpanKind::to_string(server_span), "SERVER")
  assert_eq(SpanKind::to_string(producer_span), "PRODUCER")
  assert_eq(SpanKind::to_string(consumer_span), "CONSUMER")
  
  // Test status codes
  let ok_status = StatusCode::Ok
  let error_status = StatusCode::Error
  
  assert_eq(StatusCode::to_string(ok_status), "OK")
  assert_eq(StatusCode::to_string(error_status), "ERROR")
}

// Test 10: Telemetry configuration
test "telemetry configuration" {
  // Test configuration creation
  let config = TelemetryConfig::default()
  
  // Test configuration properties
  assert_true(TelemetryConfig::is_enabled(config))
  assert_eq(TelemetryConfig::get_sampling_rate(config), 1.0)
  assert_eq(TelemetryConfig::get_max_batch_size(config), 512)
  assert_eq(TelemetryConfig::get_export_timeout(config), 30000)
  
  // Test configuration update
  let updated_config = TelemetryConfig::with_sampling_rate(config, 0.5)
  assert_eq(TelemetryConfig::get_sampling_rate(updated_config), 0.5)
  
  let final_config = TelemetryConfig::with_max_batch_size(updated_config, 1024)
  assert_eq(TelemetryConfig::get_max_batch_size(final_config), 1024)
}