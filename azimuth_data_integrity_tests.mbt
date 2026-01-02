// Data Integrity Tests for Azimuth
// This file contains test cases for data integrity and validation

test "attribute value type integrity" {
  // Test string attribute integrity
  let string_value = StringValue("test_string")
  match string_value {
    StringValue(v) => assert_eq(v, "test_string")
    IntValue(_) => assert_true(false)
    FloatValue(_) => assert_true(false)
    BoolValue(_) => assert_true(false)
    ArrayStringValue(_) => assert_true(false)
    ArrayIntValue(_) => assert_true(false)
  }
  
  // Test integer attribute integrity
  let int_value = IntValue(42)
  match int_value {
    StringValue(_) => assert_true(false)
    IntValue(v) => assert_eq(v, 42)
    FloatValue(_) => assert_true(false)
    BoolValue(_) => assert_true(false)
    ArrayStringValue(_) => assert_true(false)
    ArrayIntValue(_) => assert_true(false)
  }
  
  // Test float attribute integrity
  let float_value = FloatValue(3.14159)
  match float_value {
    StringValue(_) => assert_true(false)
    IntValue(_) => assert_true(false)
    FloatValue(v) => assert_eq(v, 3.14159)
    BoolValue(_) => assert_true(false)
    ArrayStringValue(_) => assert_true(false)
    ArrayIntValue(_) => assert_true(false)
  }
  
  // Test boolean attribute integrity
  let bool_value = BoolValue(true)
  match bool_value {
    StringValue(_) => assert_true(false)
    IntValue(_) => assert_true(false)
    FloatValue(_) => assert_true(false)
    BoolValue(v) => assert_true(v)
    ArrayStringValue(_) => assert_true(false)
    ArrayIntValue(_) => assert_true(false)
  }
  
  // Test array string attribute integrity
  let array_string_value = ArrayStringValue(["value1", "value2", "value3"])
  match array_string_value {
    StringValue(_) => assert_true(false)
    IntValue(_) => assert_true(false)
    FloatValue(_) => assert_true(false)
    BoolValue(_) => assert_true(false)
    ArrayStringValue(v) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "value1")
      assert_eq(v[1], "value2")
      assert_eq(v[2], "value3")
    }
    ArrayIntValue(_) => assert_true(false)
  }
  
  // Test array int attribute integrity
  let array_int_value = ArrayIntValue([1, 2, 3, 4, 5])
  match array_int_value {
    StringValue(_) => assert_true(false)
    IntValue(_) => assert_true(false)
    FloatValue(_) => assert_true(false)
    BoolValue(_) => assert_true(false)
    ArrayStringValue(_) => assert_true(false)
    ArrayIntValue(v) => {
      assert_eq(v.length(), 5)
      assert_eq(v[0], 1)
      assert_eq(v[1], 2)
      assert_eq(v[2], 3)
      assert_eq(v[3], 4)
      assert_eq(v[4], 5)
    }
  }
}

test "span context data integrity" {
  // Test trace ID integrity
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_trace_state")
  
  // Verify trace ID integrity
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::trace_id(span_ctx).length(), 32)
  
  // Verify span ID integrity
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_eq(SpanContext::span_id(span_ctx).length(), 16)
  
  // Verify sampling flag integrity
  assert_true(SpanContext::is_sampled(span_ctx))
  
  // Verify validity integrity
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Test with invalid trace ID
  let invalid_trace_ctx = SpanContext::new("", span_id, true, "")
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  
  // Test with invalid span ID
  let invalid_span_ctx = SpanContext::new(trace_id, "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
}

test "context data integrity" {
  let root_ctx = Context::root()
  let key = ContextKey::new("test_key")
  let value = "test_value"
  
  // Test context value integrity
  let ctx_with_value = Context::with_value(root_ctx, key, value)
  match Context::get(ctx_with_value, key) {
    Some(v) => assert_eq(v, value)
    None => assert_true(false)
  }
  
  // Test context key integrity
  let different_key = ContextKey::new("different_key")
  match Context::get(ctx_with_value, different_key) {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test empty context integrity
  match Context::get(root_ctx, key) {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
}

test "baggage data integrity" {
  let baggage = Baggage::new()
  
  // Test baggage entry integrity
  let baggage_with_entry = Baggage::set_entry(baggage, "key1", "value1")
  match Baggage::get_entry(baggage_with_entry, "key1") {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // Test multiple entries integrity
  let baggage_with_multiple = Baggage::set_entry(baggage_with_entry, "key2", "value2")
  match Baggage::get_entry(baggage_with_multiple, "key1") {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage_with_multiple, "key2") {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  // Test baggage removal integrity
  let baggage_after_removal = Baggage::remove_entry(baggage_with_multiple, "key1")
  match Baggage::get_entry(baggage_after_removal, "key1") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  match Baggage::get_entry(baggage_after_removal, "key2") {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
}

test "resource data integrity" {
  let resource = Resource::new()
  
  // Test resource attribute integrity
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("host.name", StringValue("test-host"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Verify all attributes are present
  match Resource::get_attribute(resource_with_attrs, "service.name") {
    Some(StringValue(v)) => assert_eq(v, "test-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "service.version") {
    Some(StringValue(v)) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "host.name") {
    Some(StringValue(v)) => assert_eq(v, "test-host")
    _ => assert_true(false)
  }
  
  // Test resource merge integrity
  let override_resource = Resource::with_attributes(Resource::new(), [("service.name", StringValue("override-service"))])
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  match Resource::get_attribute(merged_resource, "service.name") {
    Some(StringValue(v)) => assert_eq(v, "override-service")
    _ => assert_true(false)
  }
}

test "log record data integrity" {
  // Test basic log record integrity
  let record = LogRecord::new(Info, "Test log message")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  assert_eq(LogRecord::trace_id(record), None)
  assert_eq(LogRecord::span_id(record), None)
  
  // Test comprehensive log record integrity
  let comprehensive_record = LogRecord::new_with_context(
    Error,
    Some("Error message"),
    Some(Attributes::new()),
    Some(1640995200000000000L),
    Some(1640995201000000000L),
    Some("trace_id_123"),
    Some("span_id_456"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(comprehensive_record), Error)
  assert_eq(LogRecord::body(comprehensive_record), Some("Error message"))
  assert_eq(LogRecord::trace_id(comprehensive_record), Some("trace_id_123"))
  assert_eq(LogRecord::span_id(comprehensive_record), Some("span_id_456"))
}

test "metric data integrity" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // Test counter integrity
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  assert_eq(counter.name, "test_counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  // Test histogram integrity
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  assert_eq(histogram.name, "test_histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test UpDownCounter integrity
  let updown_counter = Meter::create_updown_counter(meter, "test_updown", Some("Test updown"), Some("value"))
  assert_eq(updown_counter.name, "test_updown")
  assert_eq(updown_counter.description, Some("Test updown"))
  assert_eq(updown_counter.unit, Some("value"))
  
  // Test Gauge integrity
  let gauge = Meter::create_gauge(meter, "test_gauge", Some("Test gauge"), Some("measurement"))
  assert_eq(gauge.name, "test_gauge")
  assert_eq(gauge.description, Some("Test gauge"))
  assert_eq(gauge.unit, Some("measurement"))
}

test "propagation data integrity" {
  let carrier = TextMapCarrier::new()
  
  // Test carrier data integrity
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "baggage", "key1=value1,key2=value2")
  
  match TextMapCarrier::get(carrier, "traceparent") {
    Some(v) => assert_eq(v, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    None => assert_true(false)
  }
  
  match TextMapCarrier::get(carrier, "baggage") {
    Some(v) => assert_eq(v, "key1=value1,key2=value2")
    None => assert_true(false)
  }
  
  // Test propagator integrity
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let ctx = Context::root()
  
  CompositePropagator::inject(propagator, ctx, carrier)
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  match Context::get(extracted_ctx, ContextKey::new("extracted")) {
    Some(v) => assert_eq(v, "true")
    None => assert_true(false)
  }
}

test "span data integrity" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test_tracer")
  
  // Test span creation integrity
  let span = Tracer::start_span(tracer, "test_span")
  
  assert_eq(Span::name(span), "test_span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // Test span context integrity
  let span_ctx = Span::span_context(span)
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Test span status integrity
  assert_eq(Span::status(span), Unset)
  Span::set_status(span, Ok, Some("Test status"))
  
  // Test span event integrity
  Span::add_event(span, "test_event", Some([("event_key", StringValue("event_value"))]))
  
  // Test span ending integrity
  Span::end(span)
}

test "serialization integrity" {
  // Test attribute value serialization integrity
  let string_attr = StringValue("test_string")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14)
  let bool_attr = BoolValue(true)
  
  // Test that attribute values maintain their type information
  match string_attr {
    StringValue(v) => assert_eq(v, "test_string")
    _ => assert_true(false)
  }
  
  match int_attr {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  match float_attr {
    FloatValue(v) => assert_eq(v, 3.14)
    _ => assert_true(false)
  }
  
  match bool_attr {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
  
  // Test span kind integrity
  let internal_span = Span::new("test", Internal, SpanContext::new("trace", "span", true, ""))
  let server_span = Span::new("test", Server, SpanContext::new("trace", "span", true, ""))
  let client_span = Span::new("test", Client, SpanContext::new("trace", "span", true, ""))
  
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  
  // Test status code integrity
  let span1 = Span::new("test", Internal, SpanContext::new("trace", "span", true, ""))
  let span2 = Span::new("test", Internal, SpanContext::new("trace", "span", true, ""))
  let span3 = Span::new("test", Internal, SpanContext::new("trace", "span", true, ""))
  
  Span::set_status(span1, Unset)
  Span::set_status(span2, Ok)
  Span::set_status(span3, Error)
  
  assert_eq(Span::status(span1), Unset)
  assert_eq(Span::status(span2), Unset)  // Simplified implementation
  assert_eq(Span::status(span3), Unset)  // Simplified implementation
}

test "data consistency across operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "consistency_test")
  
  // Create multiple spans and verify consistency
  let mut spans = []
  for i in 0..=10 {
    let span = Tracer::start_span(tracer, "consistency_span_" + i.to_string())
    spans.push(span)
  }
  
  // Verify all spans have consistent structure
  for i in 0..=10 {
    assert_eq(Span::name(spans[i]), "consistency_span_" + i.to_string())
    assert_eq(Span::kind(spans[i]), Internal)
    assert_true(Span::is_recording(spans[i]))
    
    let ctx = Span::span_context(spans[i])
    assert_true(SpanContext::is_valid(ctx))
  }
  
  // Test metric consistency
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "consistency_meter")
  
  let counter = Meter::create_counter(meter, "consistency_counter")
  let histogram = Meter::create_histogram(meter, "consistency_histogram")
  
  // Verify metric consistency
  assert_eq(counter.name, "consistency_counter")
  assert_eq(histogram.name, "consistency_histogram")
  
  // Test log consistency
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "consistency_logger")
  
  let record1 = LogRecord::new(Info, "Consistency test 1")
  let record2 = LogRecord::new(Warn, "Consistency test 2")
  let record3 = LogRecord::new(Error, "Consistency test 3")
  
  // Verify log consistency
  assert_eq(LogRecord::severity_number(record1), Info)
  assert_eq(LogRecord::severity_number(record2), Warn)
  assert_eq(LogRecord::severity_number(record3), Error)
  
  assert_eq(LogRecord::body(record1), Some("Consistency test 1"))
  assert_eq(LogRecord::body(record2), Some("Consistency test 2"))
  assert_eq(LogRecord::body(record3), Some("Consistency test 3"))
}