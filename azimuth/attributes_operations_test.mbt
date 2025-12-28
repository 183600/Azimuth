// Attributes Operations Test Suite for Azimuth Telemetry System
// Tests various attribute operations including setting, getting, and type conversions

test "attributes creation and basic operations" {
  let attrs = Attributes::new()
  
  // Test setting string attribute
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  let string_value = Attributes::get(attrs, "string.key")
  assert_eq(string_value, Some(StringValue("test_value")))
  
  // Test setting int attribute
  Attributes::set(attrs, "int.key", IntValue(42))
  let int_value = Attributes::get(attrs, "int.key")
  assert_eq(int_value, Some(IntValue(42)))
  
  // Test getting non-existent attribute
  let missing_value = Attributes::get(attrs, "missing.key")
  assert_eq(missing_value, None)
}

test "attributes with different value types" {
  let attrs = Attributes::new()
  
  // Test float value
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  let float_value = Attributes::get(attrs, "float.key")
  assert_eq(float_value, Some(FloatValue(3.14)))
  
  // Test boolean value
  Attributes::set(attrs, "bool.key", BoolValue(true))
  let bool_value = Attributes::get(attrs, "bool.key")
  assert_eq(bool_value, Some(BoolValue(true)))
  
  // Test array string value
  let string_array = ["value1", "value2", "value3"]
  Attributes::set(attrs, "array.string.key", ArrayStringValue(string_array))
  let array_string_value = Attributes::get(attrs, "array.string.key")
  assert_eq(array_string_value, Some(ArrayStringValue(string_array)))
  
  // Test array int value
  let int_array = [1, 2, 3, 4, 5]
  Attributes::set(attrs, "array.int.key", ArrayIntValue(int_array))
  let array_int_value = Attributes::get(attrs, "array.int.key")
  assert_eq(array_int_value, Some(ArrayIntValue(int_array)))
}

test "attributes with special characters in keys" {
  let attrs = Attributes::new()
  
  // Test key with dots
  Attributes::set(attrs, "service.name", StringValue("azimuth"))
  let service_name = Attributes::get(attrs, "service.name")
  assert_eq(service_name, Some(StringValue("azimuth")))
  
  // Test key with underscores
  Attributes::set(attrs, "user_id", IntValue(12345))
  let user_id = Attributes::get(attrs, "user_id")
  assert_eq(user_id, Some(IntValue(12345)))
  
  // Test key with hyphens
  Attributes::set(attrs, "http-status-code", IntValue(200))
  let http_status = Attributes::get(attrs, "http-status-code")
  assert_eq(http_status, Some(IntValue(200)))
}

test "attributes with empty and null values" {
  let attrs = Attributes::new()
  
  // Test empty string
  Attributes::set(attrs, "empty.string", StringValue(""))
  let empty_string = Attributes::get(attrs, "empty.string")
  assert_eq(empty_string, Some(StringValue("")))
  
  // Test zero values
  Attributes::set(attrs, "zero.int", IntValue(0))
  let zero_int = Attributes::get(attrs, "zero.int")
  assert_eq(zero_int, Some(IntValue(0)))
  
  Attributes::set(attrs, "zero.float", FloatValue(0.0))
  let zero_float = Attributes::get(attrs, "zero.float")
  assert_eq(zero_float, Some(FloatValue(0.0)))
  
  // Test false boolean
  Attributes::set(attrs, "false.bool", BoolValue(false))
  let false_bool = Attributes::get(attrs, "false.bool")
  assert_eq(false_bool, Some(BoolValue(false)))
}

test "attributes with large values" {
  let attrs = Attributes::new()
  
  // Test large int value
  Attributes::set(attrs, "large.int", IntValue(9223372036854775807))
  let large_int = Attributes::get(attrs, "large.int")
  assert_eq(large_int, Some(IntValue(9223372036854775807)))
  
  // Test very small float value
  Attributes::set(attrs, "small.float", FloatValue(0.0000001))
  let small_float = Attributes::get(attrs, "small.float")
  assert_eq(small_float, Some(FloatValue(0.0000001)))
  
  // Test very large float value
  Attributes::set(attrs, "large.float", FloatValue(999999999.999999))
  let large_float = Attributes::get(attrs, "large.float")
  assert_eq(large_float, Some(FloatValue(999999999.999999)))
}