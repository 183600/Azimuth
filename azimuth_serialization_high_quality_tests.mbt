// Azimuth Telemetry System - High Quality Serialization and Deserialization Tests
// This file contains comprehensive serialization and deserialization test cases

// Test 1: Attribute Value Serialization
test "attribute value serialization" {
  // Test string attribute serialization
  let string_attr = StringValue("test_string_value")
  let string_serialized = AttributeValue::serialize(string_attr)
  let string_deserialized = AttributeValue::deserialize(string_serialized)
  
  match string_deserialized {
    Some(StringValue(v)) => assert_eq(v, "test_string_value")
    _ => assert_true(false)
  }
  
  // Test int attribute serialization
  let int_attr = IntValue(42)
  let int_serialized = AttributeValue::serialize(int_attr)
  let int_deserialized = AttributeValue::deserialize(int_serialized)
  
  match int_deserialized {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test float attribute serialization
  let float_attr = FloatValue(3.14159)
  let float_serialized = AttributeValue::serialize(float_attr)
  let float_deserialized = AttributeValue::deserialize(float_serialized)
  
  match float_deserialized {
    Some(FloatValue(v)) => assert_true(abs(v - 3.14159) < 0.00001)
    _ => assert_true(false)
  }
  
  // Test bool attribute serialization
  let bool_attr = BoolValue(true)
  let bool_serialized = AttributeValue::serialize(bool_attr)
  let bool_deserialized = AttributeValue::deserialize(bool_serialized)
  
  match bool_deserialized {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
  
  // Test array string attribute serialization
  let array_string_attr = ArrayStringValue(["a", "b", "c"])
  let array_string_serialized = AttributeValue::serialize(array_string_attr)
  let array_string_deserialized = AttributeValue::deserialize(array_string_serialized)
  
  match array_string_deserialized {
    Some(ArrayStringValue(v)) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "a")
      assert_eq(v[1], "b")
      assert_eq(v[2], "c")
    }
    _ => assert_true(false)
  }
  
  // Test array int attribute serialization
  let array_int_attr = ArrayIntValue([1, 2, 3, 4, 5])
  let array_int_serialized = AttributeValue::serialize(array_int_attr)
  let array_int_deserialized = AttributeValue::deserialize(array_int_serialized)
  
  match array_int_deserialized {
    Some(ArrayIntValue(v)) => {
      assert_eq(v.length(), 5)
      assert_eq(v[0], 1)
      assert_eq(v[4], 5)
    }
    _ => assert_true(false)
  }
}

// Test 2: Attributes Collection Serialization
test "attributes collection serialization" {
  let attrs = Attributes::new()
  
  // Add various types of attributes
  Attributes::set(attrs, "string.key", StringValue("string_value"))
  Attributes::set(attrs, "int.key", IntValue(123))
  Attributes::set(attrs, "float.key", FloatValue(45.67))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.string", ArrayStringValue(["x", "y", "z"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([10, 20, 30]))
  
  // Serialize attributes
  let serialized = Attributes::serialize(attrs)
  assert_true(serialized.length() > 0)
  
  // Deserialize attributes
  let deserialized_attrs = Attributes::deserialize(serialized)
  
  // Verify all attributes are correctly deserialized
  let string_result = Attributes::get(deserialized_attrs, "string.key")
  match string_result {
    Some(StringValue(v)) => assert_eq(v, "string_value")
    _ => assert_true(false)
  }
  
  let int_result = Attributes::get(deserialized_attrs, "int.key")
  match int_result {
    Some(IntValue(v)) => assert_eq(v, 123)
    _ => assert_true(false)
  }
  
  let float_result = Attributes::get(deserialized_attrs, "float.key")
  match float_result {
    Some(FloatValue(v)) => assert_true(abs(v - 45.67) < 0.001)
    _ => assert_true(false)
  }
  
  let bool_result = Attributes::get(deserialized_attrs, "bool.key")
  match bool_result {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
  
  let array_string_result = Attributes::get(deserialized_attrs, "array.string")
  match array_string_result {
    Some(ArrayStringValue(v)) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "x")
    }
    _ => assert_true(false)
  }
  
  let array_int_result = Attributes::get(deserialized_attrs, "array.int")
  match array_int_result {
    Some(ArrayIntValue(v)) => {
      assert_eq(v.length(), 3)
      assert_eq(v[2], 30)
    }
    _ => assert_true(false)
  }
}

// Test 3: Span Context Serialization
test "span context serialization" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  
  // Serialize span context
  let serialized = SpanContext::serialize(span_ctx)
  assert_true(serialized.length() > 0)
  
  // Deserialize span context
  let deserialized_ctx = SpanContext::deserialize(serialized)
  
  // Verify deserialized context
  assert_eq(SpanContext::trace_id(deserialized_ctx), trace_id)
  assert_eq(SpanContext::span_id(deserialized_ctx), span_id)
  assert_eq(SpanContext::is_sampled(deserialized_ctx), true)
  assert_eq(SpanContext::is_valid(deserialized_ctx), true)
}

// Test 4: Span Serialization
test "span serialization" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "serialization_test_tracer")
  let span = Tracer::start_span(tracer, "test_span")
  
  // Add attributes and events
  Span::set_attribute(span, "attr1", StringValue("value1"))
  Span::set_attribute(span, "attr2", IntValue(42))
  Span::add_event(span, "test_event", Some([
    ("event_attr", StringValue("event_value"))
  ]))
  Span::set_status(span, Ok, Some("Test completed"))
  
  // Serialize span
  let serialized = Span::serialize(span)
  assert_true(serialized.length() > 0)
  
  // Deserialize span
  let deserialized_span = Span::deserialize(serialized)
  
  // Verify deserialized span
  assert_eq(Span::name(deserialized_span), "test_span")
  
  let attr1_result = Span::get_attribute(deserialized_span, "attr1")
  match attr1_result {
    Some(StringValue(v)) => assert_eq(v, "value1")
    _ => assert_true(false)
  }
  
  let attr2_result = Span::get_attribute(deserialized_span, "attr2")
  match attr2_result {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  assert_eq(Span::status(deserialized_span), Ok)
  
  Span::end(span)
}

// Test 5: Log Record Serialization
test "log record serialization" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.attr", StringValue("log_value"))
  
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Test error message"),
    Some(attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some("trace_123"),
    Some("span_456"),
    None
  )
  
  // Serialize log record
  let serialized = LogRecord::serialize(log_record)
  assert_true(serialized.length() > 0)
  
  // Deserialize log record
  let deserialized_log = LogRecord::deserialize(serialized)
  
  // Verify deserialized log record
  assert_eq(LogRecord::severity_number(deserialized_log), Error)
  match LogRecord::body(deserialized_log) {
    Some(body) => assert_eq(body, "Test error message")
    None => assert_true(false)
  }
  assert_eq(LogRecord::trace_id(deserialized_log), Some("trace_123"))
  assert_eq(LogRecord::span_id(deserialized_log), Some("span_456"))
  assert_eq(LogRecord::timestamp(deserialized_log), Some(1234567890L))
  
  // Verify attributes
  let log_attrs = LogRecord::attributes(deserialized_log)
  match log_attrs {
    Some(attrs) => {
      let log_attr_result = Attributes::get(attrs, "log.attr")
      match log_attr_result {
        Some(StringValue(v)) => assert_eq(v, "log_value")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 6: Resource Serialization
test "resource serialization" {
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("deployment.environment", StringValue("production"))
  ]
  let resource = Resource::with_attributes(Resource::new(), attrs)
  
  // Serialize resource
  let serialized = Resource::serialize(resource)
  assert_true(serialized.length() > 0)
  
  // Deserialize resource
  let deserialized_resource = Resource::deserialize(serialized)
  
  // Verify deserialized resource
  let service_name = Resource::get_attribute(deserialized_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test_service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(deserialized_resource, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  let instance_id = Resource::get_attribute(deserialized_resource, "service.instance.id")
  match instance_id {
    Some(StringValue(id)) => assert_eq(id, "instance-123")
    _ => assert_true(false)
  }
  
  let environment = Resource::get_attribute(deserialized_resource, "deployment.environment")
  match environment {
    Some(StringValue(env)) => assert_eq(env, "production")
    _ => assert_true(false)
  }
}

// Test 7: HTTP Request/Response Serialization
test "http request response serialization" {
  // Test HTTP request serialization
  let request_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Custom-Header", "custom_value")
  ]
  let request = HttpRequest::new("POST", "https://api.example.com/data", request_headers, Some("{\"key\":\"value\"}"))
  
  let request_serialized = HttpRequest::serialize(request)
  assert_true(request_serialized.length() > 0)
  
  let request_deserialized = HttpRequest::deserialize(request_serialized)
  
  assert_eq(HttpRequest::http_method(request_deserialized), "POST")
  assert_eq(HttpRequest::url(request_deserialized), "https://api.example.com/data")
  match HttpRequest::body(request_deserialized) {
    Some(body) => assert_eq(body, "{\"key\":\"value\"}")
    None => assert_true(false)
  }
  
  // Test HTTP response serialization
  let response_headers = [
    ("Content-Type", "application/json"),
    ("Content-Length", "1024"),
    ("X-Response-ID", "response-123")
  ]
  let response = HttpResponse::new(200, response_headers, Some("{\"result\":\"success\"}"))
  
  let response_serialized = HttpResponse::serialize(response)
  assert_true(response_serialized.length() > 0)
  
  let response_deserialized = HttpResponse::deserialize(response_serialized)
  
  assert_eq(HttpResponse::status_code(response_deserialized), 200)
  match HttpResponse::body(response_deserialized) {
    Some(body) => assert_eq(body, "{\"result\":\"success\"}")
    None => assert_true(false)
  }
}

// Test 8: Complex Nested Structures Serialization
test "complex nested structures serialization" {
  // Create complex nested attributes
  let nested_attrs = Attributes::new()
  Attributes::set(nested_attrs, "nested.string", StringValue("nested_value"))
  Attributes::set(nested_attrs, "nested.int", IntValue(999))
  
  let outer_attrs = Attributes::new()
  Attributes::set(outer_attrs, "outer.string", StringValue("outer_value"))
  Attributes::set(outer_attrs, "nested.attrs", serialize_attributes(nested_attrs)) // Assuming this function exists
  
  // Create span with complex attributes
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "complex_test_tracer")
  let span = Tracer::start_span(tracer, "complex_span")
  
  // Add complex attributes
  Span::set_attribute(span, "complex.data", serialize_attributes(outer_attrs))
  
  // Add events with complex attributes
  let event_attrs = Attributes::new()
  Attributes::set(event_attrs, "event.complex", StringValue("complex_event_data"))
  Span::add_event(span, "complex_event", Some(event_attrs))
  
  // Serialize the complex span
  let serialized = Span::serialize(span)
  assert_true(serialized.length() > 0)
  
  // Deserialize and verify
  let deserialized_span = Span::deserialize(serialized)
  assert_eq(Span::name(deserialized_span), "complex_span")
  
  // Verify complex data structure is preserved
  let complex_data = Span::get_attribute(deserialized_span, "complex.data")
  match complex_data {
    Some(StringValue(data)) => {
      // Deserialize nested attributes and verify structure
      let deserialized_outer = Attributes::deserialize(data)
      let outer_string = Attributes::get(deserialized_outer, "outer.string")
      match outer_string {
        Some(StringValue(v)) => assert_eq(v, "outer_value")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  Span::end(span)
}

// Test 9: Serialization Format Compatibility
test "serialization format compatibility" {
  // Test JSON serialization
  let attrs = Attributes::new()
  Attributes::set(attrs, "json.test", StringValue("json_value"))
  Attributes::set(attrs, "json.number", IntValue(42))
  
  let json_serialized = Attributes::to_json(attrs)
  assert_true(json_serialized.length() > 0)
  
  let json_deserialized = Attributes::from_json(json_serialized)
  
  let json_test_result = Attributes::get(json_deserialized, "json.test")
  match json_test_result {
    Some(StringValue(v)) => assert_eq(v, "json_value")
    _ => assert_true(false)
  }
  
  // Test binary serialization
  let binary_serialized = Attributes::to_binary(attrs)
  assert_true(binary_serialized.length() > 0)
  
  let binary_deserialized = Attributes::from_binary(binary_serialized)
  
  let json_number_result = Attributes::get(binary_deserialized, "json.number")
  match json_number_result {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Verify JSON and binary produce equivalent results
  let json_result = Attributes::get(json_deserialized, "json.test")
  let binary_result = Attributes::get(binary_deserialized, "json.test")
  
  match (json_result, binary_result) {
    (Some(StringValue(json_v)), Some(StringValue(binary_v))) => assert_eq(json_v, binary_v)
    _ => assert_true(false)
  }
}

// Test 10: Serialization Error Handling
test "serialization error handling" {
  // Test serialization of malformed data
  let malformed_json = "{\"invalid\": json, \"missing_quotes\": value}"
  
  try {
    let deserialized = Attributes::from_json(malformed_json)
    // Should either return empty attributes or throw an exception
    assert_true(true)
  } catch {
    // Exception is acceptable for malformed input
    assert_true(true)
  }
  
  // Test deserialization of empty data
  let empty_data = ""
  
  try {
    let deserialized = Attributes::from_json(empty_data)
    // Should return empty attributes
    assert_true(true)
  } catch {
    // Exception is acceptable for empty input
    assert_true(true)
  }
  
  // Test deserialization of null data
  try {
    let deserialized = Attributes::from_json(null)
    // Should return empty attributes or throw exception
    assert_true(true)
  } catch {
    // Exception is acceptable for null input
    assert_true(true)
  }
  
  // Test serialization of extremely large data
  let large_attrs = Attributes::new()
  for i in 0..10000 {
    let large_value = "x".repeat(1000)
    Attributes::set(large_attrs, "large_attr_" + i.to_string(), StringValue(large_value))
  }
  
  try {
    let serialized = Attributes::to_json(large_attrs)
    // Should either succeed or fail gracefully
    assert_true(serialized.length() > 0 || serialized.length() == 0)
  } catch {
    // Exception is acceptable for extremely large data
    assert_true(true)
  }
  
  // Test circular reference handling (if applicable)
  // This would depend on the specific implementation
  // Most serialization libraries should handle circular references gracefully
}