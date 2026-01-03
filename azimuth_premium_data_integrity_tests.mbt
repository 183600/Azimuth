// Azimuth Telemetry System - Premium Data Integrity Tests
// This file contains comprehensive data integrity validation tests for telemetry data

// Test 1: Attribute Value Type Consistency
test "attribute value type consistency validation" {
  let attrs = Attributes::new()
  
  // Test string value consistency
  Attributes::set(attrs, "string.test", StringValue("original_value"))
  let string_result = Attributes::get(attrs, "string.test")
  match string_result {
    Some(StringValue(v)) => {
      assert_eq(v, "original_value")
      assert_eq(v.length(), 14)
      assert_true(v.contains("original"))
    }
    _ => assert_true(false)
  }
  
  // Test integer value consistency
  Attributes::set(attrs, "int.test", IntValue(42))
  let int_result = Attributes::get(attrs, "int.test")
  match int_result {
    Some(IntValue(v)) => {
      assert_eq(v, 42)
      assert_true(v > 0)
      assert_true(v < 100)
    }
    _ => assert_true(false)
  }
  
  // Test float value precision
  Attributes::set(attrs, "float.test", FloatValue(3.14159265359))
  let float_result = Attributes::get(attrs, "float.test")
  match float_result {
    Some(FloatValue(v)) => {
      assert_true(v > 3.14)
      assert_true(v < 3.15)
      assert_true(v != 3.0)
    }
    _ => assert_true(false)
  }
  
  // Test boolean value consistency
  Attributes::set(attrs, "bool.test", BoolValue(true))
  let bool_result = Attributes::get(attrs, "bool.test")
  match bool_result {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
  
  // Test array value consistency
  let test_array = ["item1", "item2", "item3"]
  Attributes::set(attrs, "array.test", ArrayStringValue(test_array))
  let array_result = Attributes::get(attrs, "array.test")
  match array_result {
    Some(ArrayStringValue(v)) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "item1")
      assert_eq(v[1], "item2")
      assert_eq(v[2], "item3")
    }
    _ => assert_true(false)
  }
}

// Test 2: Span Context Data Integrity
test "span context data integrity validation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  
  // Validate trace ID format and integrity
  let retrieved_trace_id = SpanContext::trace_id(span_ctx)
  assert_eq(retrieved_trace_id, trace_id)
  assert_eq(retrieved_trace_id.length(), 32)
  assert_true(is_hexadecimal(retrieved_trace_id))
  
  // Validate span ID format and integrity
  let retrieved_span_id = SpanContext::span_id(span_ctx)
  assert_eq(retrieved_span_id, span_id)
  assert_eq(retrieved_span_id.length(), 16)
  assert_true(is_hexadecimal(retrieved_span_id))
  
  // Validate sampling decision integrity
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Test context serialization and deserialization integrity
  let serialized_ctx = SpanContext::serialize(span_ctx)
  let deserialized_ctx = SpanContext::deserialize(serialized_ctx)
  
  assert_eq(SpanContext::trace_id(deserialized_ctx), trace_id)
  assert_eq(SpanContext::span_id(deserialized_ctx), span_id)
  assert_eq(SpanContext::is_sampled(deserialized_ctx), SpanContext::is_sampled(span_ctx))
  assert_eq(SpanContext::is_valid(deserialized_ctx), SpanContext::is_valid(span_ctx))
}

// Test 3: Metrics Data Integrity
test "metrics data integrity validation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "integrity_test_meter")
  
  // Test counter data integrity
  let counter = Meter::create_counter(meter, "integrity_counter", Some("Integrity test counter"), Some("count"))
  let expected_values = [1.0, 2.5, 3.0, 4.5]
  
  for value in expected_values {
    Counter::add(counter, value)
  }
  
  // Validate counter instrument properties
  let instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(instrument), "integrity_counter")
  assert_eq(Instrument::description(instrument), Some("Integrity test counter"))
  assert_eq(Instrument::unit(instrument), Some("count"))
  
  // Test histogram data integrity
  let histogram = Meter::create_histogram(meter, "integrity_histogram", Some("Integrity test histogram"), Some("ms"))
  let histogram_values = [10.0, 20.0, 30.0, 40.0, 50.0]
  
  for value in histogram_values {
    Histogram::record(histogram, value)
  }
  
  // Validate histogram instrument properties
  let histogram_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(histogram_instrument), "integrity_histogram")
  assert_eq(Instrument::description(histogram_instrument), Some("Integrity test histogram"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
  
  // Test updown counter data integrity
  let updown_counter = Meter::create_updown_counter(meter, "integrity_updown", Some("Integrity test updown"), Some("value"))
  let updown_operations = [10.0, -5.0, 3.0, -2.0]
  
  for value in updown_operations {
    UpDownCounter::add(updown_counter, value)
  }
  
  // Validate updown counter instrument properties
  let updown_instrument = UpDownCounter::as_instrument(updown_counter)
  assert_eq(Instrument::name(updown_instrument), "integrity_updown")
  assert_eq(Instrument::description(updown_instrument), Some("Integrity test updown"))
  assert_eq(Instrument::unit(updown_instrument), Some("value"))
}

// Test 4: Log Record Data Integrity
test "log record data integrity validation" {
  // Test basic log record integrity
  let log_record = LogRecord::new(Info, "Test integrity log message")
  
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(body) => {
      assert_eq(body, "Test integrity log message")
      assert_eq(body.length(), 25)
      assert_true(body.contains("integrity"))
    }
    None => assert_true(false)
  }
  
  // Test complex log record integrity
  let attrs = Attributes::new()
  Attributes::set(attrs, "component", StringValue("integrity_test"))
  Attributes::set(attrs, "version", StringValue("1.0.0"))
  Attributes::set(attrs, "test_id", IntValue(12345))
  
  let timestamp = Clock::monotonic()
  let observed_timestamp = timestamp + 1000L
  let trace_id = "trace123456789abcdef"
  let span_id = "span1234567890"
  
  let complex_log_record = LogRecord::new_with_context(
    Error,
    Some("Complex integrity test message"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    None
  )
  
  assert_eq(LogRecord::severity_number(complex_log_record), Error)
  match LogRecord::body(complex_log_record) {
    Some(body) => assert_eq(body, "Complex integrity test message")
    None => assert_true(false)
  }
  
  assert_eq(LogRecord::timestamp(complex_log_record), Some(timestamp))
  assert_eq(LogRecord::observed_timestamp(complex_log_record), Some(observed_timestamp))
  assert_eq(LogRecord::trace_id(complex_log_record), Some(trace_id))
  assert_eq(LogRecord::span_id(complex_log_record), Some(span_id))
  
  // Validate log record serialization integrity
  let serialized_log = LogRecord::serialize(complex_log_record)
  let deserialized_log = LogRecord::deserialize(serialized_log)
  
  assert_eq(LogRecord::severity_number(deserialized_log), LogRecord::severity_number(complex_log_record))
  assert_eq(LogRecord::body(deserialized_log), LogRecord::body(complex_log_record))
  assert_eq(LogRecord::trace_id(deserialized_log), LogRecord::trace_id(complex_log_record))
  assert_eq(LogRecord::span_id(deserialized_log), LogRecord::span_id(complex_log_record))
}

// Test 5: Resource Data Integrity
test "resource data integrity validation" {
  let resource = Resource::new()
  
  // Test resource attributes integrity
  let attrs = [
    ("service.name", StringValue("integrity_test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-integrity-123")),
    ("host.name", StringValue("test-host")),
    ("process.id", IntValue(12345))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Validate each attribute integrity
  for (key, expected_value) in attrs {
    let actual_value = Resource::get_attribute(resource_with_attrs, key)
    match actual_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false)
    }
  }
  
  // Test resource merging integrity
  let override_attrs = [
    ("service.name", StringValue("overridden_service")),
    ("new.attribute", StringValue("new_value"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  // Validate merged resource integrity
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "overridden_service")
    _ => assert_true(false)
  }
  
  let new_attr = Resource::get_attribute(merged_resource, "new.attribute")
  match new_attr {
    Some(StringValue(value)) => assert_eq(value, "new_value")
    _ => assert_true(false)
  }
  
  // Ensure original non-overridden attributes are preserved
  let service_version = Resource::get_attribute(merged_resource, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
}

// Test 6: Context Propagation Data Integrity
test "context propagation data integrity validation" {
  let root_ctx = Context::root()
  
  // Test context with value integrity
  let key1 = ContextKey::new("integrity.key1")
  let key2 = ContextKey::new("integrity.key2")
  let key3 = ContextKey::new("integrity.key3")
  
  let value1 = "integrity.value1"
  let value2 = 42
  let value3 = true
  
  let ctx1 = Context::with_value(root_ctx, key1, value1)
  let ctx2 = Context::with_value(ctx1, key2, value2)
  let ctx3 = Context::with_value(ctx2, key3, value3)
  
  // Validate context values integrity
  match Context::get(ctx3, key1) {
    Some(v) => assert_eq(v, value1)
    None => assert_true(false)
  }
  
  match Context::get(ctx3, key2) {
    Some(v) => assert_eq(v, value2)
    None => assert_true(false)
  }
  
  match Context::get(ctx3, key3) {
    Some(v) => assert_eq(v, value3)
    None => assert_true(false)
  }
  
  // Ensure root context is not affected
  match Context::get(root_ctx, key1) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test context serialization and deserialization integrity
  let serialized_ctx = Context::serialize(ctx3)
  let deserialized_ctx = Context::deserialize(serialized_ctx)
  
  match Context::get(deserialized_ctx, key1) {
    Some(v) => assert_eq(v, value1)
    None => assert_true(false)
  }
  
  match Context::get(deserialized_ctx, key2) {
    Some(v) => assert_eq(v, value2)
    None => assert_true(false)
  }
  
  match Context::get(deserialized_ctx, key3) {
    Some(v) => assert_eq(v, value3)
    None => assert_true(false)
  }
}

// Test 7: Baggage Data Integrity
test "baggage data integrity validation" {
  let baggage = Baggage::new()
  
  // Test baggage entry integrity
  let updated_baggage = Baggage::set_entry(baggage, "test.key1", "test.value1")
  let updated_baggage2 = Baggage::set_entry(updated_baggage, "test.key2", "test.value2")
  let updated_baggage3 = Baggage::set_entry(updated_baggage2, "test.key3", "test.value3")
  
  // Validate baggage entries integrity
  let value1 = Baggage::get_entry(updated_baggage3, "test.key1")
  match value1 {
    Some(v) => assert_eq(v, "test.value1")
    None => assert_true(false)
  }
  
  let value2 = Baggage::get_entry(updated_baggage3, "test.key2")
  match value2 {
    Some(v) => assert_eq(v, "test.value2")
    None => assert_true(false)
  }
  
  let value3 = Baggage::get_entry(updated_baggage3, "test.key3")
  match value3 {
    Some(v) => assert_eq(v, "test.value3")
    None => assert_true(false)
  }
  
  // Test baggage removal integrity
  let baggage_after_removal = Baggage::remove_entry(updated_baggage3, "test.key2")
  
  // Validate removal integrity
  let removed_value = Baggage::get_entry(baggage_after_removal, "test.key2")
  match removed_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Ensure other entries are preserved
  let preserved_value1 = Baggage::get_entry(baggage_after_removal, "test.key1")
  match preserved_value1 {
    Some(v) => assert_eq(v, "test.value1")
    None => assert_true(false)
  }
  
  let preserved_value3 = Baggage::get_entry(baggage_after_removal, "test.key3")
  match preserved_value3 {
    Some(v) => assert_eq(v, "test.value3")
    None => assert_true(false)
  }
}

// Test 8: HTTP Client Data Integrity
test "http client data integrity validation" {
  // Test HTTP request integrity
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Custom-Header", "custom-value"),
    ("X-Request-ID", "req-123456")
  ]
  
  let request_body = "{\"test\": \"integrity\", \"timestamp\": 1234567890}"
  let request = HttpRequest::new("POST", "https://example.com/api/test", headers, Some(request_body))
  
  // Validate request integrity
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://example.com/api/test")
  match HttpRequest::body(request) {
    Some(body) => assert_eq(body, request_body)
    None => assert_true(false)
  }
  
  // Validate request headers integrity
  let request_headers = HttpRequest::headers(request)
  assert_true(request_headers.contains(("Content-Type", "application/json")))
  assert_true(request_headers.contains(("Authorization", "Bearer token123")))
  assert_true(request_headers.contains(("X-Custom-Header", "custom-value")))
  assert_true(request_headers.contains(("X-Request-ID", "req-123456")))
  
  // Test HTTP response integrity
  let response_headers = [
    ("Content-Type", "application/json"),
    ("Content-Length", "1024"),
    ("X-Response-ID", "resp-789012"),
    ("Cache-Control", "no-cache")
  ]
  
  let response_body = "{\"status\": \"success\", \"data\": {\"id\": 123}}"
  let response = HttpResponse::new(200, response_headers, Some(response_body))
  
  // Validate response integrity
  assert_eq(HttpResponse::status_code(response), 200)
  match HttpResponse::body(response) {
    Some(body) => assert_eq(body, response_body)
    None => assert_true(false)
  }
  
  // Validate response headers integrity
  let resp_headers = HttpResponse::headers(response)
  assert_true(resp_headers.contains(("Content-Type", "application/json")))
  assert_true(resp_headers.contains(("Content-Length", "1024")))
  assert_true(resp_headers.contains(("X-Response-ID", "resp-789012")))
  assert_true(resp_headers.contains(("Cache-Control", "no-cache")))
}

// Helper function to validate hexadecimal strings
func is_hexadecimal(str : String) -> Bool {
  let hex_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']
  for char in str.to_char_array() {
    if not hex_chars.contains(char.to_lower()) {
      return false
    }
  }
  return true
}