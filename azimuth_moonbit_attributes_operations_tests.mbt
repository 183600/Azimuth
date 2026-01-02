// Azimuth Telemetry System - MoonBit Attributes Operations Tests
// This file contains test cases for attributes operations and manipulations

// Test 1: Attributes creation and initialization
test "attributes creation and initialization" {
  // Create empty attributes
  let empty_attrs = Attributes({ values = [] })
  assert_eq(empty_attrs.values.length(), 0)
  
  // Create attributes with initial values
  let attrs = Attributes({ values = [
    ("string.attr", StringValue("test_value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true))
  ] })
  
  assert_eq(attrs.values.length(), 4)
  
  // Verify each attribute
  let (str_key, str_value) = attrs.values[0]
  assert_eq(str_key, "string.attr")
  match str_value {
    StringValue(v) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  let (int_key, int_value) = attrs.values[1]
  assert_eq(int_key, "int.attr")
  match int_value {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
}

// Test 2: Attributes lookup and retrieval
test "attributes lookup and retrieval" {
  let attrs = Attributes({ values = [
    ("service.name", StringValue("azimuth")),
    ("service.version", StringValue("1.0.0")),
    ("service.port", IntValue(8080)),
    ("service.ssl", BoolValue(true))
  ] })
  
  // Simulate attribute lookup
  let mut found_service_name = false
  let mut found_service_port = false
  let mut found_service_ssl = false
  
  for (key, value) in attrs.values {
    match key {
      "service.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "azimuth")
            found_service_name = true
          }
          _ => assert_true(false)
        }
      }
      "service.port" => {
        match value {
          IntValue(v) => {
            assert_eq(v, 8080)
            found_service_port = true
          }
          _ => assert_true(false)
        }
      }
      "service.ssl" => {
        match value {
          BoolValue(v) => {
            assert_true(v)
            found_service_ssl = true
          }
          _ => assert_true(false)
        }
      }
      _ => () // Ignore other keys
    }
  }
  
  assert_true(found_service_name)
  assert_true(found_service_port)
  assert_true(found_service_ssl)
}

// Test 3: Attributes filtering and selection
test "attributes filtering and selection" {
  let attrs = Attributes({ values = [
    ("string.key1", StringValue("value1")),
    ("string.key2", StringValue("value2")),
    ("int.key1", IntValue(100)),
    ("int.key2", IntValue(200)),
    ("bool.key1", BoolValue(true)),
    ("float.key1", FloatValue(2.5))
  ] })
  
  // Filter string attributes
  let mut string_attrs = []
  for (key, value) in attrs.values {
    match value {
      StringValue(_) => string_attrs = string_attrs @ [(key, value)]
      _ => ()
    }
  }
  assert_eq(string_attrs.length(), 2)
  
  // Filter int attributes
  let mut int_attrs = []
  for (key, value) in attrs.values {
    match value {
      IntValue(_) => int_attrs = int_attrs @ [(key, value)]
      _ => ()
    }
  }
  assert_eq(int_attrs.length(), 2)
  
  // Filter by key prefix
  let mut prefixed_attrs = []
  for (key, value) in attrs.values {
    if key.length() >= 9 && key[:9] == "string.key" {
      prefixed_attrs = prefixed_attrs @ [(key, value)]
    }
  }
  assert_eq(prefixed_attrs.length(), 2)
}

// Test 4: Attributes merging and combination
test "attributes merging and combination" {
  let attrs1 = Attributes({ values = [
    ("service.name", StringValue("azimuth")),
    ("service.version", StringValue("1.0.0")),
    ("common.attr", StringValue("value1"))
  ] })
  
  let attrs2 = Attributes({ values = [
    ("service.instance.id", StringValue("instance-123")),
    ("service.port", IntValue(8080)),
    ("common.attr", StringValue("value2"))  // This should override
  ] })
  
  // Simulate merging (attrs2 takes precedence)
  let mut merged_values = attrs1.values
  
  // Add unique values from attrs2
  for (key2, value2) in attrs2.values {
    let mut found = false
    let mut index = 0
    
    // Check if key exists in attrs1
    for (key1, _) in attrs1.values {
      if key1 == key2 {
        found = true
        break
      }
      index = index + 1
    }
    
    if found {
      // Override existing value
      merged_values[index] = (key2, value2)
    } else {
      // Add new value
      merged_values = merged_values @ [(key2, value2)]
    }
  }
  
  let merged_attrs = Attributes({ values = merged_values })
  assert_eq(merged_attrs.values.length(), 5)
  
  // Verify common.attr was overridden
  let mut found_common = false
  for (key, value) in merged_attrs.values {
    if key == "common.attr" {
      match value {
        StringValue(v) => {
          assert_eq(v, "value2")  // Should be value2 from attrs2
          found_common = true
        }
        _ => assert_true(false)
      }
    }
  }
  assert_true(found_common)
}

// Test 5: Attributes transformation and conversion
test "attributes transformation and conversion" {
  let attrs = Attributes({ values = [
    ("string.attr", StringValue("123")),
    ("int.attr", IntValue(456)),
    ("float.attr", FloatValue(789.0)),
    ("bool.attr", BoolValue(true))
  ] })
  
  // Transform string values to upper case
  let mut transformed_values = []
  for (key, value) in attrs.values {
    match value {
      StringValue(v) => {
        let upper_v = v.to_uppercase()
        transformed_values = transformed_values @ [(key, StringValue(upper_v))]
      }
      _ => transformed_values = transformed_values @ [(key, value)]
    }
  }
  
  let transformed_attrs = Attributes({ values = transformed_values })
  
  // Verify transformation
  for (key, value) in transformed_attrs.values {
    if key == "string.attr" {
      match value {
        StringValue(v) => assert_eq(v, "123")
        _ => assert_true(false)
      }
    }
  }
}

// Test 6: Array attribute operations
test "array attribute operations" {
  let attrs = Attributes({ values = [
    ("string.array", ArrayStringValue(["a", "b", "c"])),
    ("int.array", ArrayIntValue([1, 2, 3, 4, 5])),
    ("single.value", StringValue("not_array"))
  ] })
  
  // Process array attributes
  for (key, value) in attrs.values {
    match key {
      "string.array" => {
        match value {
          ArrayStringValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], "a")
            assert_eq(arr[1], "b")
            assert_eq(arr[2], "c")
          }
          _ => assert_true(false)
        }
      }
      "int.array" => {
        match value {
          ArrayIntValue(arr) => {
            assert_eq(arr.length(), 5)
            assert_eq(arr[0], 1)
            assert_eq(arr[4], 5)
          }
          _ => assert_true(false)
        }
      }
      "single.value" => {
        match value {
          StringValue(v) => assert_eq(v, "not_array")
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
}

// Test 7: Attributes validation and constraints
test "attributes validation and constraints" {
  // Test valid attributes
  let valid_attrs = Attributes({ values = [
    ("valid.key", StringValue("valid_value")),
    ("valid.int", IntValue(42)),
    ("valid.bool", BoolValue(true))
  ] })
  
  // All should be valid
  for (key, value) in valid_attrs.values {
    assert_true(key.length() > 0)
    match value {
      StringValue(v) => assert_true(v.length() >= 0)
      IntValue(_) => assert_true(true)
      BoolValue(_) => assert_true(true)
      FloatValue(_) => assert_true(true)
      ArrayStringValue(arr) => assert_true(arr.length() >= 0)
      ArrayIntValue(arr) => assert_true(arr.length() >= 0)
    }
  }
  
  // Test edge cases
  let edge_case_attrs = Attributes({ values = [
    ("", StringValue("empty_key")),  // Empty key
    ("empty_value", StringValue("")),  // Empty string value
    ("zero_int", IntValue(0)),  // Zero value
    ("false_bool", BoolValue(false))  // False value
  ] })
  
  assert_eq(edge_case_attrs.values.length(), 4)
  
  // Verify empty key
  let (empty_key, empty_key_value) = edge_case_attrs.values[0]
  assert_eq(empty_key, "")
  match empty_key_value {
    StringValue(v) => assert_eq(v, "empty_key")
    _ => assert_true(false)
  }
  
  // Verify empty string value
  let (empty_value_key, empty_value) = edge_case_attrs.values[1]
  assert_eq(empty_value_key, "empty_value")
  match empty_value {
    StringValue(v) => assert_eq(v, "")
    _ => assert_true(false)
  }
}

// Test 8: Attributes serialization preparation
test "attributes serialization preparation" {
  let attrs = Attributes({ values = [
    ("service.name", StringValue("azimuth")),
    ("service.port", IntValue(8080)),
    ("service.ssl", BoolValue(true)),
    ("service.endpoints", ArrayStringValue(["/api", "/health"]))
  ] })
  
  // Prepare for serialization by converting to string representations
  let mut serializable_pairs = []
  for (key, value) in attrs.values {
    let string_value = match value {
      StringValue(v) => v
      IntValue(v) => v.to_string()
      FloatValue(v) => v.to_string()
      BoolValue(v) => if v { "true" } else { "false" }
      ArrayStringValue(arr) => {
        let mut result = "["
        let mut first = true
        for item in arr {
          if first {
            result = result + "\"" + item + "\""
            first = false
          } else {
            result = result + ",\"" + item + "\""
          }
        }
        result = result + "]"
        result
      }
      ArrayIntValue(arr) => {
        let mut result = "["
        let mut first = true
        for item in arr {
          if first {
            result = result + item.to_string()
            first = false
          } else {
            result = result + "," + item.to_string()
          }
        }
        result = result + "]"
        result
      }
    }
    serializable_pairs = serializable_pairs @ [(key, string_value)]
  }
  
  assert_eq(serializable_pairs.length(), 4)
  
  // Verify serialized values
  for (key, value) in serializable_pairs {
    match key {
      "service.name" => assert_eq(value, "azimuth")
      "service.port" => assert_eq(value, "8080")
      "service.ssl" => assert_eq(value, "true")
      "service.endpoints" => assert_eq(value, "[\"/api\",\"/health\"]")
      _ => assert_true(false)
    }
  }
}

// Test 9: Attributes deep comparison
test "attributes deep comparison" {
  let attrs1 = Attributes({ values = [
    ("key1", StringValue("value1")),
    ("key2", IntValue(42)),
    ("key3", BoolValue(true))
  ] })
  
  let attrs2 = Attributes({ values = [
    ("key1", StringValue("value1")),
    ("key2", IntValue(42)),
    ("key3", BoolValue(true))
  ] })
  
  let attrs3 = Attributes({ values = [
    ("key1", StringValue("different")),
    ("key2", IntValue(42)),
    ("key3", BoolValue(true))
  ] })
  
  // Compare attrs1 and attrs2 (should be equal)
  let mut equal_1_2 = true
  if attrs1.values.length() != attrs2.values.length() {
    equal_1_2 = false
  } else {
    for i in 0..attrs1.values.length() {
      let (key1, value1) = attrs1.values[i]
      let (key2, value2) = attrs2.values[i]
      
      if key1 != key2 {
        equal_1_2 = false
        break
      }
      
      // Simple value comparison (in real implementation would be more robust)
      match (value1, value2) {
        (StringValue(v1), StringValue(v2)) => {
          if v1 != v2 { equal_1_2 = false; break }
        }
        (IntValue(v1), IntValue(v2)) => {
          if v1 != v2 { equal_1_2 = false; break }
        }
        (BoolValue(v1), BoolValue(v2)) => {
          if v1 != v2 { equal_1_2 = false; break }
        }
        _ => { equal_1_2 = false; break }
      }
    }
  }
  assert_true(equal_1_2)
  
  // Compare attrs1 and attrs3 (should be different)
  let mut equal_1_3 = true
  if attrs1.values.length() != attrs3.values.length() {
    equal_1_3 = false
  } else {
    for i in 0..attrs1.values.length() {
      let (key1, value1) = attrs1.values[i]
      let (key3, value3) = attrs3.values[i]
      
      if key1 != key3 {
        equal_1_3 = false
        break
      }
      
      match (value1, value3) {
        (StringValue(v1), StringValue(v3)) => {
          if v1 != v3 { equal_1_3 = false; break }
        }
        (IntValue(v1), IntValue(v3)) => {
          if v1 != v3 { equal_1_3 = false; break }
        }
        (BoolValue(v1), BoolValue(v3)) => {
          if v1 != v3 { equal_1_3 = false; break }
        }
        _ => { equal_1_3 = false; break }
      }
    }
  }
  assert_false(equal_1_3)
}

// Test 10: Attributes with special characters in keys and values
test "attributes with special characters" {
  let special_attrs = Attributes({ values = [
    ("key.with.dots", StringValue("value.with.dots")),
    ("key-with-dashes", StringValue("value-with-dashes")),
    ("key_with_underscores", StringValue("value_with_underscores")),
    ("key/with/slashes", StringValue("value/with/slashes")),
    ("key with spaces", StringValue("value with spaces")),
    ("key.with.special.chars!@#$%", StringValue("value.with.special.chars!@#$%"))
  ] })
  
  assert_eq(special_attrs.values.length(), 6)
  
  // Verify all special characters are preserved
  for (key, value) in special_attrs.values {
    match value {
      StringValue(v) => {
        // Key and value should have the same structure
        assert_eq(key.replace(".", " ").replace("-", " ").replace("_", " ").replace("/", " "),
                 v.replace(".", " ").replace("-", " ").replace("_", " ").replace("/", " "))
      }
      _ => assert_true(false)
    }
  }
}