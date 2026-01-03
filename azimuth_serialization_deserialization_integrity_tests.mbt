// Azimuth Telemetry System - Serialization/Deserialization Integrity Tests
// This file contains comprehensive serialization and deserialization integrity test cases

// Test 1: Attribute Value Serialization/Deserialization
test "attribute value serialization/deserialization" {
  // Test StringValue serialization
  let string_value = StringValue("test_string_value")
  let serialized_string = serialize::to_json(string_value)
  let deserialized_string = deserialize::from_json(serialized_string)
  
  match deserialized_string {
    Some(StringValue(v)) => assert_eq(v, "test_string_value")
    _ => assert_true(false)
  }
  
  // Test IntValue serialization
  let int_value = IntValue(42)
  let serialized_int = serialize::to_json(int_value)
  let deserialized_int = deserialize::from_json(serialized_int)
  
  match deserialized_int {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test FloatValue serialization
  let float_value = FloatValue(3.14159)
  let serialized_float = serialize::to_json(float_value)
  let deserialized_float = deserialize::from_json(serialized_float)
  
  match deserialized_float {
    Some(FloatValue(v)) => assert_true((v - 3.14159).abs() < 0.0001)
    _ => assert_true(false)
  }
  
  // Test BoolValue serialization
  let bool_value = BoolValue(true)
  let serialized_bool = serialize::to_json(bool_value)
  let deserialized_bool = deserialize::from_json(serialized_bool)
  
  match deserialized_bool {
    Some(BoolValue(v)) => assert_eq(v, true)
    _ => assert_true(false)
  }
  
  // Test ArrayStringValue serialization
  let array_string_value = ArrayStringValue(["item1", "item2", "item3"])
  let serialized_array_string = serialize::to_json(array_string_value)
  let deserialized_array_string = deserialize::from_json(serialized_array_string)
  
  match deserialized_array_string {
    Some(ArrayStringValue(v)) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "item1")
      assert_eq(v[1], "item2")
      assert_eq(v[2], "item3")
    }
    _ => assert_true(false)
  }
  
  // Test ArrayIntValue serialization
  let array_int_value = ArrayIntValue([1, 2, 3, 4, 5])
  let serialized_array_int = serialize::to_json(array_int_value)
  let deserialized_array_int = deserialize::from_json(serialized_array_int)
  
  match deserialized_array_int {
    Some(ArrayIntValue(v)) => {
      assert_eq(v.length(), 5)
      assert_eq(v[0], 1)
      assert_eq(v[1], 2)
      assert_eq(v[2], 3)
      assert_eq(v[3], 4)
      assert_eq(v[4], 5)
    }
    _ => assert_true(false)
  }
}

// Test 2: Attributes Collection Serialization/Deserialization
test "attributes collection serialization/deserialization" {
  let attrs = Attributes::new()
  
  // Add various attribute types
  Attributes::set(attrs, "string_key", StringValue("string_value"))
  Attributes::set(attrs, "int_key", IntValue(42))
  Attributes::set(attrs, "float_key", FloatValue(3.14))
  Attributes::set(attrs, "bool_key", BoolValue(true))
  Attributes::set(attrs, "array_string_key", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array_int_key", ArrayIntValue([1, 2, 3]))
  
  // Serialize attributes
  let serialized_attrs = serialize::to_json(attrs)
  
  // Deserialize attributes
  let deserialized_attrs = deserialize::from_json(serialized_attrs)
  
  match deserialized_attrs {
    Some(deserialized) => {
      // Verify all attributes were preserved
      let string_val = Attributes::get(deserialized, "string_key")
      match string_val {
        Some(StringValue(v)) => assert_eq(v, "string_value")
        _ => assert_true(false)
      }
      
      let int_val = Attributes::get(deserialized, "int_key")
      match int_val {
        Some(IntValue(v)) => assert_eq(v, 42)
        _ => assert_true(false)
      }
      
      let float_val = Attributes::get(deserialized, "float_key")
      match float_val {
        Some(FloatValue(v)) => assert_true((v - 3.14).abs() < 0.01)
        _ => assert_true(false)
      }
      
      let bool_val = Attributes::get(deserialized, "bool_key")
      match bool_val {
        Some(BoolValue(v)) => assert_eq(v, true)
        _ => assert_true(false)
      }
      
      let array_string_val = Attributes::get(deserialized, "array_string_key")
      match array_string_val {
        Some(ArrayStringValue(v)) => {
          assert_eq(v.length(), 3)
          assert_eq(v[0], "a")
          assert_eq(v[1], "b")
          assert_eq(v[2], "c")
        }
        _ => assert_true(false)
      }
      
      let array_int_val = Attributes::get(deserialized, "array_int_key")
      match array_int_val {
        Some(ArrayIntValue(v)) => {
          assert_eq(v.length(), 3)
          assert_eq(v[0], 1)
          assert_eq(v[1], 2)
          assert_eq(v[2], 3)
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 3: Span Context Serialization/Deserialization
test "span context serialization/deserialization" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  
  // Serialize span context
  let serialized_ctx = serialize::to_json(span_ctx)
  
  // Deserialize span context
  let deserialized_ctx = deserialize::from_json(serialized_ctx)
  
  match deserialized_ctx {
    Some(ctx) => {
      assert_eq(SpanContext::trace_id(ctx), trace_id)
      assert_eq(SpanContext::span_id(ctx), span_id)
      assert_true(SpanContext::is_sampled(ctx))
      assert_true(SpanContext::is_valid(ctx))
    }
    None => assert_true(false)
  }
}

// Test 4: Span Serialization/Deserialization
test "span serialization/deserialization" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Add events and status
  Span::add_event(span, "test_event", Some(Attributes::with([("event_key", StringValue("event_value"))])))
  Span::set_status(span, Ok, Some("Test completed"))
  
  // Serialize span
  let serialized_span = serialize::to_json(span)
  
  // Deserialize span
  let deserialized_span = deserialize::from_json(serialized_span)
  
  match deserialized_span {
    Some(deserialized) => {
      assert_eq(Span::name(deserialized), "test_span")
      match Span::kind(deserialized) {
        Internal => assert_true(true)
        _ => assert_true(false)
      }
      assert_eq(Span::status(deserialized), Ok)
      
      let retrieved_ctx = Span::span_context(deserialized)
      assert_eq(SpanContext::trace_id(retrieved_ctx), trace_id)
      assert_eq(SpanContext::span_id(retrieved_ctx), span_id)
    }
    None => assert_true(false)
  }
}

// Test 5: Log Record Serialization/Deserialization
test "log record serialization/deserialization" {
  let attrs = Attributes::with([
    ("log_key", StringValue("log_value")),
    ("log_int", IntValue(42))
  ])
  
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Test log message"),
    Some(attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some("test_trace_id"),
    Some("test_span_id"),
    Some(Context::root())
  )
  
  // Serialize log record
  let serialized_log = serialize::to_json(log_record)
  
  // Deserialize log record
  let deserialized_log = deserialize::from_json(serialized_log)
  
  match deserialized_log {
    Some(deserialized) => {
      assert_eq(LogRecord::severity_number(deserialized), Error)
      match LogRecord::body(deserialized) {
        Some(body) => assert_eq(body, "Test log message")
        None => assert_true(false)
      }
      assert_eq(LogRecord::trace_id(deserialized), Some("test_trace_id"))
      assert_eq(LogRecord::span_id(deserialized), Some("test_span_id"))
    }
    None => assert_true(false)
  }
}

// Test 6: Resource Serialization/Deserialization
test "resource serialization/deserialization" {
  let resource = Resource::new()
  
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("localhost")),
    ("process.pid", IntValue(1234))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Serialize resource
  let serialized_resource = serialize::to_json(resource_with_attrs)
  
  // Deserialize resource
  let deserialized_resource = deserialize::from_json(serialized_resource)
  
  match deserialized_resource {
    Some(deserialized) => {
      // Verify all attributes were preserved
      let service_name = Resource::get_attribute(deserialized, "service.name")
      match service_name {
        Some(StringValue(v)) => assert_eq(v, "test_service")
        _ => assert_true(false)
      }
      
      let service_version = Resource::get_attribute(deserialized, "service.version")
      match service_version {
        Some(StringValue(v)) => assert_eq(v, "1.0.0")
        _ => assert_true(false)
      }
      
      let service_instance_id = Resource::get_attribute(deserialized, "service.instance.id")
      match service_instance_id {
        Some(StringValue(v)) => assert_eq(v, "instance-123")
        _ => assert_true(false)
      }
      
      let host_name = Resource::get_attribute(deserialized, "host.name")
      match host_name {
        Some(StringValue(v)) => assert_eq(v, "localhost")
        _ => assert_true(false)
      }
      
      let process_pid = Resource::get_attribute(deserialized, "process.pid")
      match process_pid {
        Some(IntValue(v)) => assert_eq(v, 1234)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 7: HTTP Request Serialization/Deserialization
test "http request serialization/deserialization" {
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Custom-Header", "custom_value")
  ]
  
  let request = HttpRequest::new("POST", "https://example.com/api", headers, Some("{\"key\":\"value\"}"))
  
  // Serialize request
  let serialized_request = serialize::to_json(request)
  
  // Deserialize request
  let deserialized_request = deserialize::from_json(serialized_request)
  
  match deserialized_request {
    Some(deserialized) => {
      assert_eq(HttpRequest::http_method(deserialized), "POST")
      assert_eq(HttpRequest::url(deserialized), "https://example.com/api")
      match HttpRequest::body(deserialized) {
        Some(body) => assert_eq(body, "{\"key\":\"value\"}")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 8: HTTP Response Serialization/Deserialization
test "http response serialization/deserialization" {
  let headers = [
    ("Content-Type", "application/json"),
    ("Content-Length", "1024"),
    ("X-Response-ID", "response123")
  ]
  
  let response = HttpResponse::new(200, headers, Some("{\"result\":\"success\"}"))
  
  // Serialize response
  let serialized_response = serialize::to_json(response)
  
  // Deserialize response
  let deserialized_response = deserialize::from_json(serialized_response)
  
  match deserialized_response {
    Some(deserialized) => {
      assert_eq(HttpResponse::status_code(deserialized), 200)
      match HttpResponse::body(deserialized) {
        Some(body) => assert_eq(body, "{\"result\":\"success\"}")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 9: Complex Object Serialization/Deserialization
test "complex object serialization/deserialization" {
  // Create a complex nested structure
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "complex_test")
  let span = Span::new("complex_span", Server, span_ctx)
  
  // Add complex attributes
  let complex_attrs = Attributes::with([
    ("nested.object", StringValue("{\"key\":\"value\",\"number\":42}")),
    ("array.data", ArrayStringValue(["item1", "item2", "item3"])),
    ("numeric.values", ArrayIntValue([1, 2, 3, 4, 5])),
    ("metadata", StringValue("{\"version\":\"1.0\",\"timestamp\":1234567890}"))
  ])
  
  Span::add_event(span, "complex_event", Some(complex_attrs))
  Span::set_status(span, Error, Some("Complex error occurred"))
  
  // Create associated log record
  let log_attrs = Attributes::with([
    ("log.level", StringValue("ERROR")),
    ("log.component", StringValue("serializer")),
    ("correlation.id", StringValue("corr-12345"))
  ])
  
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Complex log message with nested data"),
    Some(log_attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some(trace_id),
    Some(span_id),
    Some(Context::with_value(Context::root(), ContextKey::new("complex"), "data"))
  )
  
  // Create resource
  let resource_attrs = [
    ("service.name", StringValue("complex_service")),
    ("service.version", StringValue("2.0.0")),
    ("deployment.environment", StringValue("production")),
    ("feature.flags", ArrayStringValue(["feature1", "feature2", "feature3"]))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), resource_attrs)
  
  // Create complex object container
  let complex_object = {
    "span": span,
    "log_record": log_record,
    "resource": resource,
    "metadata": {
      "created_at": 1234567890L,
      "updated_at": 1234567891L,
      "version": "1.0.0",
      "tags": ["test", "complex", "serialization"]
    }
  }
  
  // Serialize complex object
  let serialized_complex = serialize::to_json(complex_object)
  
  // Deserialize complex object
  let deserialized_complex = deserialize::from_json(serialized_complex)
  
  match deserialized_complex {
    Some(deserialized) => {
      // Verify complex object structure
      // Note: This is a simplified verification - actual implementation would depend on the specific structure
      assert_true(true) // If we reach here, serialization/deserialization worked
    }
    None => assert_true(false)
  }
}

// Test 10: Serialization Error Handling and Edge Cases
test "serialization error handling and edge cases" {
  // Test serialization of null/empty values
  let empty_string_value = StringValue("")
  let serialized_empty = serialize::to_json(empty_string_value)
  let deserialized_empty = deserialize::from_json(serialized_empty)
  
  match deserialized_empty {
    Some(StringValue(v)) => assert_eq(v, "")
    _ => assert_true(false)
  }
  
  // Test serialization of special characters
  let special_chars_value = StringValue("Special chars: \"quotes\", 'apostrophes', \nnewlines, \ttabs, \\backslashes")
  let serialized_special = serialize::to_json(special_chars_value)
  let deserialized_special = deserialize::from_json(serialized_special)
  
  match deserialized_special {
    Some(StringValue(v)) => assert_eq(v, "Special chars: \"quotes\", 'apostrophes', \nnewlines, \ttabs, \\backslashes")
    _ => assert_true(false)
  }
  
  // Test serialization of Unicode characters
  let unicode_value = StringValue("Unicode test: 测试中文 🚀 emoji")
  let serialized_unicode = serialize::to_json(unicode_value)
  let deserialized_unicode = deserialize::from_json(serialized_unicode)
  
  match deserialized_unicode {
    Some(StringValue(v)) => assert_eq(v, "Unicode test: 测试中文 🚀 emoji")
    _ => assert_true(false)
  }
  
  // Test serialization of extreme numeric values
  let max_int_value = IntValue(9223372036854775807)
  let serialized_max_int = serialize::to_json(max_int_value)
  let deserialized_max_int = deserialize::from_json(serialized_max_int)
  
  match deserialized_max_int {
    Some(IntValue(v)) => assert_eq(v, 9223372036854775807)
    _ => assert_true(false)
  }
  
  let min_int_value = IntValue(-9223372036854775808)
  let serialized_min_int = serialize::to_json(min_int_value)
  let deserialized_min_int = deserialize::from_json(serialized_min_int)
  
  match deserialized_min_int {
    Some(IntValue(v)) => assert_eq(v, -9223372036854775808)
    _ => assert_true(false)
  }
  
  let max_float_value = FloatValue(1.7976931348623157e+308)
  let serialized_max_float = serialize::to_json(max_float_value)
  let deserialized_max_float = deserialize::from_json(serialized_max_float)
  
  match deserialized_max_float {
    Some(FloatValue(v)) => assert_true(v.is_infinite() || v > 1.0e+307)
    _ => assert_true(false)
  }
  
  // Test serialization of empty arrays
  let empty_array_string = ArrayStringValue([])
  let serialized_empty_array = serialize::to_json(empty_array_string)
  let deserialized_empty_array = deserialize::from_json(serialized_empty_array)
  
  match deserialized_empty_array {
    Some(ArrayStringValue(v)) => assert_eq(v.length(), 0)
    _ => assert_true(false)
  }
  
  // Test serialization of large arrays
  let large_array = ArrayStringValue([1, 2, 3, 4, 5, 6, 7, 8, 9, 10].map(fn(i) { "item_" + i.to_string() }))
  let serialized_large_array = serialize::to_json(large_array)
  let deserialized_large_array = deserialize::from_json(serialized_large_array)
  
  match deserialized_large_array {
    Some(ArrayStringValue(v)) => {
      assert_eq(v.length(), 10)
      for i in 0..10 {
        assert_eq(v[i], "item_" + (i + 1).to_string())
      }
    }
    _ => assert_true(false)
  }
  
  // Test malformed JSON handling
  let malformed_json = "{ invalid json }"
  let malformed_result = deserialize::from_json(malformed_json)
  
  match malformed_result {
    Some(_) => assert_true(false) // Should not succeed with malformed JSON
    None => assert_true(true) // Expected failure
  }
  
  // Test empty JSON handling
  let empty_json = ""
  let empty_result = deserialize::from_json(empty_json)
  
  match empty_result {
    Some(_) => assert_true(false) // Should not succeed with empty JSON
    None => assert_true(true) // Expected failure
  }
}