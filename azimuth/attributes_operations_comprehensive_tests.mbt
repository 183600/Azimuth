// Attributes Operations Comprehensive Tests
// Tests for attribute value operations and type conversions

test "attribute value string operations" {
  let string_attr = StringValue("test_value")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14)
  let bool_attr = BoolValue(true)
  
  // Test attribute creation and type handling
  assert_eq(match string_attr { StringValue(v) => v, _ => "" }, "test_value")
  assert_eq(match int_attr { IntValue(v) => v, _ => 0 }, 42)
  assert_eq(match float_attr { FloatValue(v) => v, _ => 0.0 }, 3.14)
  assert_eq(match bool_attr { BoolValue(v) => v, _ => false }, true)
}

test "attributes collection operations" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  
  // Test attribute retrieval
  let string_result = Attributes::get(attrs, "string.key")
  let int_result = Attributes::get(attrs, "int.key")
  let missing_result = Attributes::get(attrs, "missing.key")
  
  assert_eq(match string_result { Some(StringValue(v)) => v, _ => "" }, "test_value")
  assert_eq(match int_result { Some(IntValue(v)) => v, _ => 0 }, 42)
  assert_eq(missing_result, None)
}

test "array attribute values operations" {
  let string_array = ArrayStringValue(["item1", "item2", "item3"])
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  
  // Test array attribute creation
  assert_eq(match string_array { ArrayStringValue(arr) => arr.length(), _ => 0 }, 3)
  assert_eq(match int_array { ArrayIntValue(arr) => arr.length(), _ => 0 }, 5)
  
  // Test array content access
  let string_content = match string_array { ArrayStringValue(arr) => arr[0], _ => "" }
  let int_content = match int_array { ArrayIntValue(arr) => arr[0], _ => 0 }
  
  assert_eq(string_content, "item1")
  assert_eq(int_content, 1)
}

test "attribute type conversion and validation" {
  let attrs = Attributes::new()
  
  // Test setting different attribute types
  Attributes::set(attrs, "service.name", StringValue("azimuth"))
  Attributes::set(attrs, "service.version", StringValue("1.0.0"))
  Attributes::set(attrs, "service.instance.id", StringValue("instance-123"))
  Attributes::set(attrs, "process.pid", IntValue(12345))
  Attributes::set(attrs, "process.cpu.percent", FloatValue(75.5))
  Attributes::set(attrs, "service.healthy", BoolValue(true))
  
  // Verify attribute retrieval and type matching
  let service_name = Attributes::get(attrs, "service.name")
  let process_pid = Attributes::get(attrs, "process.pid")
  let service_healthy = Attributes::get(attrs, "service.healthy")
  
  assert_eq(match service_name { Some(StringValue(v)) => v, _ => "" }, "azimuth")
  assert_eq(match process_pid { Some(IntValue(v)) => v, _ => 0 }, 12345)
  assert_eq(match service_healthy { Some(BoolValue(v)) => v, _ => false }, true)
}

test "attributes with special characters and edge cases" {
  let attrs = Attributes::new()
  
  // Test attributes with special characters
  Attributes::set(attrs, "key.with.dots", StringValue("value1"))
  Attributes::set(attrs, "key-with-dashes", StringValue("value2"))
  Attributes::set(attrs, "key_with_underscores", StringValue("value3"))
  Attributes::set(attrs, "key.with.numbers123", StringValue("value4"))
  
  // Test empty and null-like values
  Attributes::set(attrs, "empty.string", StringValue(""))
  Attributes::set(attrs, "zero.number", IntValue(0))
  Attributes::set(attrs, "false.boolean", BoolValue(false))
  Attributes::set(attrs, "zero.float", FloatValue(0.0))
  
  // Verify retrieval
  let dotted_key = Attributes::get(attrs, "key.with.dots")
  let empty_string = Attributes::get(attrs, "empty.string")
  let zero_number = Attributes::get(attrs, "zero.number")
  let false_boolean = Attributes::get(attrs, "false.boolean")
  
  assert_eq(match dotted_key { Some(StringValue(v)) => v, _ => "" }, "value1")
  assert_eq(match empty_string { Some(StringValue(v)) => v, _ => "" }, "")
  assert_eq(match zero_number { Some(IntValue(v)) => v, _ => 0 }, 0)
  assert_eq(match false_boolean { Some(BoolValue(v)) => v, _ => false }, false)
}