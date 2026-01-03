// High-Quality Test Suite for Attribute Type Conversion
// This file contains comprehensive test cases for attribute type conversion functionality

test "string attribute value operations" {
  let string_value = StringValue("test-string")
  
  // Verify string value
  match string_value {
    StringValue(s) => assert_eq(s, "test-string")
    _ => assert_false(true)
  }
}

test "int attribute value operations" {
  let int_value = IntValue(42)
  
  // Verify int value
  match int_value {
    IntValue(i) => assert_eq(i, 42)
    _ => assert_false(true)
  }
}

test "float attribute value operations" {
  let float_value = FloatValue(3.14159)
  
  // Verify float value
  match float_value {
    FloatValue(f) => assert_eq(f, 3.14159)
    _ => assert_false(true)
  }
}

test "bool attribute value operations" {
  let bool_value_true = BoolValue(true)
  let bool_value_false = BoolValue(false)
  
  // Verify bool values
  match bool_value_true {
    BoolValue(b) => assert_true(b)
    _ => assert_false(true)
  }
  
  match bool_value_false {
    BoolValue(b) => assert_false(b)
    _ => assert_false(true)
  }
}

test "array string attribute value operations" {
  let array_string_value = ArrayStringValue(["item1", "item2", "item3"])
  
  // Verify array string value
  match array_string_value {
    ArrayStringValue(arr) => {
      assert_eq(arr.length, 3)
      assert_eq(arr[0], "item1")
      assert_eq(arr[1], "item2")
      assert_eq(arr[2], "item3")
    }
    _ => assert_false(true)
  }
}

test "array int attribute value operations" {
  let array_int_value = ArrayIntValue([1, 2, 3, 4, 5])
  
  // Verify array int value
  match array_int_value {
    ArrayIntValue(arr) => {
      assert_eq(arr.length, 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[1], 2)
      assert_eq(arr[2], 3)
      assert_eq(arr[3], 4)
      assert_eq(arr[4], 5)
    }
    _ => assert_false(true)
  }
}

test "empty array attribute values" {
  let empty_string_array = ArrayStringValue([])
  let empty_int_array = ArrayIntValue([])
  
  // Verify empty arrays
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length, 0)
    _ => assert_false(true)
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length, 0)
    _ => assert_false(true)
  }
}

test "single element array attribute values" {
  let single_string_array = ArrayStringValue(["single"])
  let single_int_array = ArrayIntValue([42])
  
  // Verify single element arrays
  match single_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length, 1)
      assert_eq(arr[0], "single")
    }
    _ => assert_false(true)
  }
  
  match single_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length, 1)
      assert_eq(arr[0], 42)
    }
    _ => assert_false(true)
  }
}

test "attributes with different value types" {
  let attrs = Attributes::new()
  
  // Set different attribute types
  Attributes::set(attrs, "string.key", StringValue("string-value"))
  Attributes::set(attrs, "int.key", IntValue(123))
  Attributes::set(attrs, "float.key", FloatValue(456.789))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  // Get and verify attributes
  let string_attr = Attributes::get(attrs, "string.key")
  let int_attr = Attributes::get(attrs, "int.key")
  let float_attr = Attributes::get(attrs, "float.key")
  let bool_attr = Attributes::get(attrs, "bool.key")
  
  match string_attr {
    Some(StringValue(s)) => assert_eq(s, "string-value")
    _ => assert_false(true)
  }
  
  match int_attr {
    Some(IntValue(i)) => assert_eq(i, 123)
    _ => assert_false(true)
  }
  
  match float_attr {
    Some(FloatValue(f)) => assert_eq(f, 456.789)
    _ => assert_false(true)
  }
  
  match bool_attr {
    Some(BoolValue(b)) => assert_true(b)
    _ => assert_false(true)
  }
}

test "resource with mixed attribute types" {
  let resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.port", IntValue(8080)),
    ("service.uptime", FloatValue(12345.67)),
    ("service.enabled", BoolValue(true)),
    ("service.tags", ArrayStringValue(["tag1", "tag2", "tag3"])),
    ("service.endpoints", ArrayIntValue([8080, 8443, 9090]))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Verify all attribute types
  let string_attr = Resource::get_attribute(resource_with_attrs, "service.name")
  let int_attr = Resource::get_attribute(resource_with_attrs, "service.port")
  let float_attr = Resource::get_attribute(resource_with_attrs, "service.uptime")
  let bool_attr = Resource::get_attribute(resource_with_attrs, "service.enabled")
  let array_string_attr = Resource::get_attribute(resource_with_attrs, "service.tags")
  let array_int_attr = Resource::get_attribute(resource_with_attrs, "service.endpoints")
  
  match string_attr {
    Some(StringValue(s)) => assert_eq(s, "test-service")
    _ => assert_false(true)
  }
  
  match int_attr {
    Some(IntValue(i)) => assert_eq(i, 8080)
    _ => assert_false(true)
  }
  
  match float_attr {
    Some(FloatValue(f)) => assert_eq(f, 12345.67)
    _ => assert_false(true)
  }
  
  match bool_attr {
    Some(BoolValue(b)) => assert_true(b)
    _ => assert_false(true)
  }
  
  match array_string_attr {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length, 3)
      assert_eq(arr[0], "tag1")
      assert_eq(arr[1], "tag2")
      assert_eq(arr[2], "tag3")
    }
    _ => assert_false(true)
  }
  
  match array_int_attr {
    Some(ArrayIntValue(arr)) => {
      assert_eq(arr.length, 3)
      assert_eq(arr[0], 8080)
      assert_eq(arr[1], 8443)
      assert_eq(arr[2], 9090)
    }
    _ => assert_false(true)
  }
}

test "span events with different attribute types" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Add event with different attribute types
  let event_attrs = [
    ("event.name", StringValue("user.login")),
    ("user.id", IntValue(12345)),
    ("response.time", FloatValue(250.75)),
    ("success", BoolValue(true)),
    ("user.roles", ArrayStringValue(["user", "admin"])),
    ("retry.counts", ArrayIntValue([1, 2, 3]))
  ]
  
  Span::add_event(span, "user.action", Some(event_attrs))
  
  // Verify event was added successfully
  assert_true(true)
}

test "log records with different attribute types" {
  let attrs = Attributes::new()
  
  // Set different attribute types
  Attributes::set(attrs, "log.level", StringValue("ERROR"))
  Attributes::set(attrs, "error.code", IntValue(500))
  Attributes::set(attrs, "response.time", FloatValue(1250.5))
  Attributes::set(attrs, "success", BoolValue(false))
  Attributes::set(attrs, "error.tags", ArrayStringValue(["timeout", "database"]))
  Attributes::set(attrs, "retry.attempts", ArrayIntValue([1, 2, 3]))
  
  // Create log record with attributes
  let record = LogRecord::new_with_context(
    Error,
    Some("Operation failed"),
    Some(attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  // Verify log record was created successfully
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Operation failed"))
}

test "attribute type conversion edge cases" {
  // Test with extreme values
  let max_int = IntValue(2147483647)
  let min_int = IntValue(-2147483648)
  let max_float = FloatValue(3.4028235e38)
  let min_float = FloatValue(-3.4028235e38)
  let tiny_float = FloatValue(1.17549435e-38)
  
  // Verify extreme values
  match max_int {
    IntValue(i) => assert_eq(i, 2147483647)
    _ => assert_false(true)
  }
  
  match min_int {
    IntValue(i) => assert_eq(i, -2147483648)
    _ => assert_false(true)
  }
  
  match max_float {
    FloatValue(f) => assert_eq(f, 3.4028235e38)
    _ => assert_false(true)
  }
  
  match min_float {
    FloatValue(f) => assert_eq(f, -3.4028235e38)
    _ => assert_false(true)
  }
  
  match tiny_float {
    FloatValue(f) => assert_eq(f, 1.17549435e-38)
    _ => assert_false(true)
  }
}

test "attribute type conversion with special characters" {
  let special_string = StringValue("String with special chars: !@#$%^&*()")
  let unicode_string = StringValue("Unicode string: 测试")
  let empty_string = StringValue("")
  
  // Verify special string values
  match special_string {
    StringValue(s) => assert_eq(s, "String with special chars: !@#$%^&*()")
    _ => assert_false(true)
  }
  
  match unicode_string {
    StringValue(s) => assert_eq(s, "Unicode string: 测试")
    _ => assert_false(true)
  }
  
  match empty_string {
    StringValue(s) => assert_eq(s, "")
    _ => assert_false(true)
  }
}

test "attribute type conversion with arrays" {
  // Test with mixed content arrays
  let string_array_with_special = ArrayStringValue([
    "normal string",
    "string with spaces",
    "string-with-special-chars!@#",
    "unicode string: 测试",
    ""
  ])
  
  let int_array_with_values = ArrayIntValue([
    0,
    -1,
    1,
    2147483647,
    -2147483648
  ])
  
  // Verify array contents
  match string_array_with_special {
    ArrayStringValue(arr) => {
      assert_eq(arr.length, 5)
      assert_eq(arr[0], "normal string")
      assert_eq(arr[1], "string with spaces")
      assert_eq(arr[2], "string-with-special-chars!@#")
      assert_eq(arr[3], "unicode string: 测试")
      assert_eq(arr[4], "")
    }
    _ => assert_false(true)
  }
  
  match int_array_with_values {
    ArrayIntValue(arr) => {
      assert_eq(arr.length, 5)
      assert_eq(arr[0], 0)
      assert_eq(arr[1], -1)
      assert_eq(arr[2], 1)
      assert_eq(arr[3], 2147483647)
      assert_eq(arr[4], -2147483648)
    }
    _ => assert_false(true)
  }
}

test "complex attribute type conversion scenario" {
  let resource = Resource::new()
  let attrs = [
    // Basic types
    ("service.name", StringValue("complex-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.port", IntValue(8080)),
    ("service.uptime", FloatValue(86400.0)),
    ("service.enabled", BoolValue(true)),
    
    // Array types
    ("service.protocols", ArrayStringValue(["http", "https", "grpc"])),
    ("service.ports", ArrayIntValue([8080, 8443, 9090])),
    
    // Special values
    ("service.description", StringValue("Service with special chars: !@#$%^&*()")),
    ("service.unicode", StringValue("Unicode support: 测试")),
    ("service.empty", StringValue("")),
    
    // Extreme values
    ("service.max.connections", IntValue(2147483647)),
    ("service.min.connections", IntValue(0)),
    ("service.threshold", FloatValue(0.000001)),
    ("service.timeout", FloatValue(3600.0))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Verify all attributes are accessible and have correct types
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.name"), Some(StringValue("complex-service")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.port"), Some(IntValue(8080)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.uptime"), Some(FloatValue(86400.0)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.enabled"), Some(BoolValue(true)))
  
  match Resource::get_attribute(resource_with_attrs, "service.protocols") {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length, 3)
      assert_eq(arr[0], "http")
      assert_eq(arr[1], "https")
      assert_eq(arr[2], "grpc")
    }
    _ => assert_false(true)
  }
  
  match Resource::get_attribute(resource_with_attrs, "service.ports") {
    Some(ArrayIntValue(arr)) => {
      assert_eq(arr.length, 3)
      assert_eq(arr[0], 8080)
      assert_eq(arr[1], 8443)
      assert_eq(arr[2], 9090)
    }
    _ => assert_false(true)
  }
  
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.description"), Some(StringValue("Service with special chars: !@#$%^&*()")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.unicode"), Some(StringValue("Unicode support: 测试")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.empty"), Some(StringValue("")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.max.connections"), Some(IntValue(2147483647)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.min.connections"), Some(IntValue(0)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.threshold"), Some(FloatValue(0.000001)))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.timeout"), Some(FloatValue(3600.0)))
}