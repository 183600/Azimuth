// Comprehensive Attributes Operations Test Suite
// Tests for attribute setting, getting, and type conversions

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
  
  // Test string value
  Attributes::set(attrs, "service.name", StringValue("azimuth-telemetry"))
  let service_name = Attributes::get(attrs, "service.name")
  assert_eq(service_name, Some(StringValue("azimuth-telemetry")))
  
  // Test int value
  Attributes::set(attrs, "service.version", IntValue(1))
  let service_version = Attributes::get(attrs, "service.version")
  assert_eq(service_version, Some(IntValue(1)))
  
  // Test float value
  Attributes::set(attrs, "cpu.usage", FloatValue(0.75))
  let cpu_usage = Attributes::get(attrs, "cpu.usage")
  assert_eq(cpu_usage, Some(FloatValue(0.75)))
  
  // Test boolean value
  Attributes::set(attrs, "service.enabled", BoolValue(true))
  let service_enabled = Attributes::get(attrs, "service.enabled")
  assert_eq(service_enabled, Some(BoolValue(true)))
}

test "attributes with array values" {
  let attrs = Attributes::new()
  
  // Test string array
  let string_array = ["tag1", "tag2", "tag3"]
  Attributes::set(attrs, "service.tags", ArrayStringValue(string_array))
  let tags = Attributes::get(attrs, "service.tags")
  assert_eq(tags, Some(ArrayStringValue(string_array)))
  
  // Test int array
  let int_array = [1, 2, 3, 4, 5]
  Attributes::set(attrs, "port.numbers", ArrayIntValue(int_array))
  let ports = Attributes::get(attrs, "port.numbers")
  assert_eq(ports, Some(ArrayIntValue(int_array)))
}

test "attributes with special characters" {
  let attrs = Attributes::new()
  
  // Test attribute names with special characters
  Attributes::set(attrs, "service.name-with-dashes", StringValue("test-service"))
  let dash_name = Attributes::get(attrs, "service.name-with-dashes")
  assert_eq(dash_name, Some(StringValue("test-service")))
  
  Attributes::set(attrs, "service.name_with_underscores", StringValue("test-service"))
  let underscore_name = Attributes::get(attrs, "service.name_with_underscores")
  assert_eq(underscore_name, Some(StringValue("test-service")))
  
  Attributes::set(attrs, "service.name.with.dots", StringValue("test-service"))
  let dot_name = Attributes::get(attrs, "service.name.with.dots")
  assert_eq(dot_name, Some(StringValue("test-service")))
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
  
  // Test large string
  let large_string = "This is a very long string that contains a lot of text and should be handled properly by the attributes system without any issues or memory problems"
  Attributes::set(attrs, "large.string", StringValue(large_string))
  let retrieved_large = Attributes::get(attrs, "large.string")
  assert_eq(retrieved_large, Some(StringValue(large_string)))
  
  // Test large int
  let large_int = 9223372036854775807  // Max int64
  Attributes::set(attrs, "large.int", IntValue(large_int))
  let retrieved_large_int = Attributes::get(attrs, "large.int")
  assert_eq(retrieved_large_int, Some(IntValue(large_int)))
  
  // Test small float
  let small_float = 0.0000000001
  Attributes::set(attrs, "small.float", FloatValue(small_float))
  let retrieved_small_float = Attributes::get(attrs, "small.float")
  assert_eq(retrieved_small_float, Some(FloatValue(small_float)))
}

test "attributes with unicode characters" {
  let attrs = Attributes::new()
  
  // Test unicode strings
  Attributes::set(attrs, "unicode.chinese", StringValue("遥测系统"))
  let chinese = Attributes::get(attrs, "unicode.chinese")
  assert_eq(chinese, Some(StringValue("遥测系统")))
  
  Attributes::set(attrs, "unicode.emoji", StringValue("🚀📊"))
  let emoji = Attributes::get(attrs, "unicode.emoji")
  assert_eq(emoji, Some(StringValue("🚀📊")))
  
  Attributes::set(attrs, "unicode.mixed", StringValue("Azimuth 遥测 📊 System"))
  let mixed = Attributes::get(attrs, "unicode.mixed")
  assert_eq(mixed, Some(StringValue("Azimuth 遥测 📊 System")))
}