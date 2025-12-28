// Azimuth Telemetry System - Enhanced Attributes Operations Tests
// Comprehensive test suite for attributes functionality

test "attributes_creation_and_basic_operations" {
  let attrs = Attributes::new()
  
  // Test initial state
  assert_eq(attrs.values.length(), 0)
  
  // Test setting string attribute
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  let string_value = Attributes::get(attrs, "string.key")
  assert_eq(string_value, Some(StringValue("test_value")))
  
  // Test setting int attribute
  Attributes::set(attrs, "int.key", IntValue(42))
  let int_value = Attributes::get(attrs, "int.key")
  assert_eq(int_value, Some(IntValue(42)))
  
  // Test missing key
  let missing_value = Attributes::get(attrs, "missing.key")
  assert_eq(missing_value, None)
}

test "attributes_with_different_value_types" {
  let attrs = Attributes::new()
  
  // Test string value
  Attributes::set(attrs, "service.name", StringValue("test-service"))
  let string_val = Attributes::get(attrs, "service.name")
  assert_eq(string_val, Some(StringValue("test-service")))
  
  // Test int value
  Attributes::set(attrs, "port.number", IntValue(8080))
  let int_val = Attributes::get(attrs, "port.number")
  assert_eq(int_val, Some(IntValue(8080)))
  
  // Test float value
  Attributes::set(attrs, "cpu.usage", FloatValue(75.5))
  let float_val = Attributes::get(attrs, "cpu.usage")
  assert_eq(float_val, Some(FloatValue(75.5)))
  
  // Test boolean value
  Attributes::set(attrs, "service.enabled", BoolValue(true))
  let bool_val = Attributes::get(attrs, "service.enabled")
  assert_eq(bool_val, Some(BoolValue(true)))
}

test "attributes_with_array_values" {
  let attrs = Attributes::new()
  
  // Test string array
  let string_array = Array[String]::from(["item1", "item2", "item3"])
  Attributes::set(attrs, "string.array", ArrayStringValue(string_array))
  let string_array_val = Attributes::get(attrs, "string.array")
  assert_eq(string_array_val, Some(ArrayStringValue(string_array)))
  
  // Test int array
  let int_array = Array[Int]::from([1, 2, 3, 4, 5])
  Attributes::set(attrs, "int.array", ArrayIntValue(int_array))
  let int_array_val = Attributes::get(attrs, "int.array")
  assert_eq(int_array_val, Some(ArrayIntValue(int_array)))
}

test "attributes_overwrite_behavior" {
  let attrs = Attributes::new()
  let key = "overwrite.test"
  
  // Set initial value
  Attributes::set(attrs, key, StringValue("initial_value"))
  let initial_val = Attributes::get(attrs, key)
  assert_eq(initial_val, Some(StringValue("initial_value")))
  
  // Overwrite with different type
  Attributes::set(attrs, key, IntValue(123))
  let int_val = Attributes::get(attrs, key)
  assert_eq(int_val, Some(IntValue(123)))
  
  // Overwrite again with another type
  Attributes::set(attrs, key, BoolValue(false))
  let bool_val = Attributes::get(attrs, key)
  assert_eq(bool_val, Some(BoolValue(false)))
}

test "attributes_with_special_characters_in_keys" {
  let attrs = Attributes::new()
  
  // Test keys with special characters
  Attributes::set(attrs, "dotted.key", StringValue("dotted_value"))
  Attributes::set(attrs, "hyphenated-key", StringValue("hyphenated_value"))
  Attributes::set(attrs, "underscore_key", StringValue("underscore_value"))
  Attributes::set(attrs, "namespace.subnamespace.key", StringValue("namespaced_value"))
  
  // Test retrieval with special character keys
  let dotted_val = Attributes::get(attrs, "dotted.key")
  let hyphenated_val = Attributes::get(attrs, "hyphenated-key")
  let underscore_val = Attributes::get(attrs, "underscore_key")
  let namespaced_val = Attributes::get(attrs, "namespace.subnamespace.key")
  
  assert_eq(dotted_val, Some(StringValue("dotted_value")))
  assert_eq(hyphenated_val, Some(StringValue("hyphenated_value")))
  assert_eq(underscore_val, Some(StringValue("underscore_value")))
  assert_eq(namespaced_val, Some(StringValue("namespaced_value")))
}

test "attributes_with_special_characters_in_values" {
  let attrs = Attributes::new()
  
  // Test values with special characters
  Attributes::set(attrs, "special.chars", StringValue("special!@#$%^&*()"))
  Attributes::set(attrs, "unicode.chars", StringValue("测试中文"))
  Attributes::set(attrs, "json.value", StringValue("{\"key\":\"value\",\"number\":123}"))
  Attributes::set(attrs, "url.encoded", StringValue("Hello%20World%21"))
  Attributes::set(attrs, "base64.value", StringValue("SGVsbG8gV29ybGQh"))
  
  // Test retrieval
  let special_val = Attributes::get(attrs, "special.chars")
  let unicode_val = Attributes::get(attrs, "unicode.chars")
  let json_val = Attributes::get(attrs, "json.value")
  let url_val = Attributes::get(attrs, "url.encoded")
  let base64_val = Attributes::get(attrs, "base64.value")
  
  assert_eq(special_val, Some(StringValue("special!@#$%^&*()")))
  assert_eq(unicode_val, Some(StringValue("测试中文")))
  assert_eq(json_val, Some(StringValue("{\"key\":\"value\",\"number\":123}")))
  assert_eq(url_val, Some(StringValue("Hello%20World%21")))
  assert_eq(base64_val, Some(StringValue("SGVsbG8gV29ybGQh")))
}

test "attributes_with_numeric_edge_cases" {
  let attrs = Attributes::new()
  
  // Test edge case numeric values
  Attributes::set(attrs, "zero.int", IntValue(0))
  Attributes::set(attrs, "negative.int", IntValue(-42))
  Attributes::set(attrs, "large.int", IntValue(2147483647))
  Attributes::set(attrs, "small.float", FloatValue(0.000001))
  Attributes::set(attrs, "large.float", FloatValue(999999.999))
  Attributes::set(attrs, "negative.float", FloatValue(-273.15))
  Attributes::set(attrs, "zero.float", FloatValue(0.0))
  
  // Test retrieval
  let zero_int = Attributes::get(attrs, "zero.int")
  let negative_int = Attributes::get(attrs, "negative.int")
  let large_int = Attributes::get(attrs, "large.int")
  let small_float = Attributes::get(attrs, "small.float")
  let large_float = Attributes::get(attrs, "large.float")
  let negative_float = Attributes::get(attrs, "negative.float")
  let zero_float = Attributes::get(attrs, "zero.float")
  
  assert_eq(zero_int, Some(IntValue(0)))
  assert_eq(negative_int, Some(IntValue(-42)))
  assert_eq(large_int, Some(IntValue(2147483647)))
  assert_eq(small_float, Some(FloatValue(0.000001)))
  assert_eq(large_float, Some(FloatValue(999999.999)))
  assert_eq(negative_float, Some(FloatValue(-273.15)))
  assert_eq(zero_float, Some(FloatValue(0.0)))
}

test "attributes_with_empty_and_null_values" {
  let attrs = Attributes::new()
  
  // Test empty string
  Attributes::set(attrs, "empty.string", StringValue(""))
  let empty_string = Attributes::get(attrs, "empty.string")
  assert_eq(empty_string, Some(StringValue("")))
  
  // Test empty arrays
  let empty_string_array = Array[String]::from([])
  let empty_int_array = Array[Int]::from([])
  Attributes::set(attrs, "empty.string.array", ArrayStringValue(empty_string_array))
  Attributes::set(attrs, "empty.int.array", ArrayIntValue(empty_int_array))
  
  let empty_string_array_val = Attributes::get(attrs, "empty.string.array")
  let empty_int_array_val = Attributes::get(attrs, "empty.int.array")
  
  assert_eq(empty_string_array_val, Some(ArrayStringValue(empty_string_array)))
  assert_eq(empty_int_array_val, Some(ArrayIntValue(empty_int_array)))
}

test "attributes_with_common_telemetry_keys" {
  let attrs = Attributes::new()
  
  // Test common OpenTelemetry attribute keys
  Attributes::set(attrs, "service.name", StringValue("payment-service"))
  Attributes::set(attrs, "service.version", StringValue("1.2.3"))
  Attributes::set(attrs, "service.instance.id", StringValue("instance-123"))
  
  Attributes::set(attrs, "telemetry.sdk.name", StringValue("opentelemetry"))
  Attributes::set(attrs, "telemetry.sdk.language", StringValue("moonbit"))
  Attributes::set(attrs, "telemetry.sdk.version", StringValue("1.0.0"))
  
  Attributes::set(attrs, "http.method", StringValue("GET"))
  Attributes::set(attrs, "http.status_code", IntValue(200))
  Attributes::set(attrs, "http.url", StringValue("https://api.example.com/users"))
  
  Attributes::set(attrs, "db.system", StringValue("postgresql"))
  Attributes::set(attrs, "db.statement", StringValue("SELECT * FROM users"))
  Attributes::set(attrs, "db.operation", StringValue("SELECT"))
  
  // Test retrieval of common attributes
  assert_eq(Attributes::get(attrs, "service.name"), Some(StringValue("payment-service")))
  assert_eq(Attributes::get(attrs, "service.version"), Some(StringValue("1.2.3")))
  assert_eq(Attributes::get(attrs, "telemetry.sdk.name"), Some(StringValue("opentelemetry")))
  assert_eq(Attributes::get(attrs, "http.method"), Some(StringValue("GET")))
  assert_eq(Attributes::get(attrs, "http.status_code"), Some(IntValue(200)))
  assert_eq(Attributes::get(attrs, "db.system"), Some(StringValue("postgresql")))
}

test "attributes_with_complex_array_operations" {
  let attrs = Attributes::new()
  
  // Test arrays with different types of values
  let string_array = Array[String]::from(["tag1", "tag2", "tag3", "special-tag"])
  let int_array = Array[Int]::from([100, 200, 300, 400, 500])
  let mixed_int_array = Array[Int]::from([0, -1, 999, -999, 2147483647, -2147483648])
  
  Attributes::set(attrs, "tags", ArrayStringValue(string_array))
  Attributes::set(attrs, "response.sizes", ArrayIntValue(int_array))
  Attributes::set(attrs, "edge.cases", ArrayIntValue(mixed_int_array))
  
  // Test array retrieval
  let tags_val = Attributes::get(attrs, "tags")
  let response_sizes_val = Attributes::get(attrs, "response.sizes")
  let edge_cases_val = Attributes::get(attrs, "edge.cases")
  
  assert_eq(tags_val, Some(ArrayStringValue(string_array)))
  assert_eq(response_sizes_val, Some(ArrayIntValue(int_array)))
  assert_eq(edge_cases_val, Some(ArrayIntValue(mixed_int_array)))
  
  // Test array contents (first and last elements)
  match tags_val {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], "tag1")
      assert_eq(arr[3], "special-tag")
    }
    _ => @assertion.assert_true(false)
  }
  
  match response_sizes_val {
    Some(ArrayIntValue(arr)) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 100)
      assert_eq(arr[4], 500)
    }
    _ => @assertion.assert_true(false)
  }
}