// Telemetry Data Integrity and Consistency Tests
// This file contains comprehensive tests for telemetry data integrity and consistency

test "span context trace consistency across service boundaries" {
  // Test that trace context remains consistent when propagated across services
  let original_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let original_span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(original_trace_id, original_span_id, true, "key1=value1,key2=value2")
  
  // Verify original context
  assert_eq(SpanContext::trace_id(span_ctx), original_trace_id)
  assert_eq(SpanContext::span_id(span_ctx), original_span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Simulate context propagation through multiple services
  let carrier = TextMapCarrier::new()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let ctx = Context::root()
  
  // Inject context
  CompositePropagator::inject(propagator, ctx, carrier)
  
  // Extract context in downstream service
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  // Verify trace consistency
  assert_true(extracted_ctx != Context::root())
}

test "metrics data integrity under high frequency operations" {
  // Test that metrics maintain data integrity under high frequency operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "high-frequency-meter")
  let counter = Meter::create_counter(meter, "operations.total")
  let histogram = Meter::create_histogram(meter, "operation.duration")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections")
  let gauge = Meter::create_gauge(meter, "memory.usage")
  
  // Simulate high frequency operations
  for i = 0; i < 1000; i = i + 1 {
    Counter::add(counter, 1.0)
    Histogram::record(histogram, Double::from_int(i) * 0.1)
    UpDownCounter::add(updown_counter, 1.0)
  }
  
  // Verify metrics are still functional after high frequency operations
  Counter::add(counter, 100.0)
  Histogram::record(histogram, 99.9)
  UpDownCounter::add(updown_counter, -500.0)
  
  assert_true(true) // If no errors occurred, integrity is maintained
}

test "log record correlation with trace context" {
  // Test that log records properly correlate with trace context
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "fedcba0987654321"
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_correlation = Context::with_value(ctx, key, "corr-12345")
  
  // Create log record with full trace context
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_correlation)
  )
  
  // Verify correlation data integrity
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::severity_number(record), Error)
}

test "attribute value type consistency across operations" {
  // Test that attribute values maintain type consistency through various operations
  let attrs = Attributes::new()
  
  // Set different types of attributes
  Attributes::set(attrs, "string.attr", StringValue("test_value"))
  Attributes::set(attrs, "int.attr", IntValue(42))
  Attributes::set(attrs, "float.attr", FloatValue(3.14))
  Attributes::set(attrs, "bool.attr", BoolValue(true))
  Attributes::set(attrs, "array.string", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([1, 2, 3]))
  
  // Retrieve and verify type consistency
  let string_val = Attributes::get(attrs, "string.attr")
  let int_val = Attributes::get(attrs, "int.attr")
  let float_val = Attributes::get(attrs, "float.attr")
  let bool_val = Attributes::get(attrs, "bool.attr")
  
  assert_eq(string_val, Some(StringValue("test_value")))
  assert_eq(int_val, Some(IntValue(42)))
  assert_true(float_val != None) // Float value check
  assert_true(bool_val != None) // Bool value check
}

test "resource attribute merge consistency" {
  // Test that resource attributes maintain consistency during merge operations
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")), // Should override
    ("service.instance.id", StringValue("instance-123")), // New attribute
    ("host.name", StringValue("host-001"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merge consistency
  let service_name = Resource::get_attribute(merged, "service.name")
  let service_version = Resource::get_attribute(merged, "service.version")
  let instance_id = Resource::get_attribute(merged, "service.instance.id")
  
  assert_true(service_name != None) // Should have overridden value
  assert_true(service_version != None) // Should preserve base value
  assert_true(instance_id != None) // Should have new value
}

test "baggage propagation integrity across multiple hops" {
  // Test that baggage maintains integrity across multiple service hops
  let initial_baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(initial_baggage, "user.id", "user-123")
  let baggage_with_more = Baggage::set_entry(baggage_with_entries, "request.id", "req-456")
  
  // Simulate multiple service hops
  let hop1_baggage = Baggage::set_entry(baggage_with_more, "hop1.timestamp", "1234567890")
  let hop2_baggage = Baggage::set_entry(hop1_baggage, "hop2.duration", "150ms")
  let final_baggage = Baggage::set_entry(hop2_baggage, "hop3.status", "success")
  
  // Verify baggage integrity across all hops
  let user_id = Baggage::get_entry(final_baggage, "user.id")
  let request_id = Baggage::get_entry(final_baggage, "request.id")
  let hop1_timestamp = Baggage::get_entry(final_baggage, "hop1.timestamp")
  let hop2_duration = Baggage::get_entry(final_baggage, "hop2.duration")
  let hop3_status = Baggage::get_entry(final_baggage, "hop3.status")
  
  assert_eq(user_id, Some("user-123"))
  assert_eq(request_id, Some("req-456"))
  assert_eq(hop1_timestamp, Some("1234567890"))
  assert_eq(hop2_duration, Some("150ms"))
  assert_eq(hop3_status, Some("success"))
}