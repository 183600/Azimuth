// Attribute Value Type Conversion Tests for Azimuth Telemetry System
// This file contains test cases for attribute value type conversion operations

test "string attribute value operations" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  
  let retrieved = Attributes::get(attrs, "string.key")
  assert_eq(retrieved, Some(StringValue("test_value")))
  
  // Test with different string values
  Attributes::set(attrs, "empty.string", StringValue(""))
  let empty_string = Attributes::get(attrs, "empty.string")
  assert_eq(empty_string, Some(StringValue("")))
  
  Attributes::set(attrs, "unicode.string", StringValue("测试中文"))
  let unicode_string = Attributes::get(attrs, "unicode.string")
  assert_eq(unicode_string, Some(StringValue("测试中文")))
}

test "integer attribute value operations" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "int.key", IntValue(42))
  
  let retrieved = Attributes::get(attrs, "int.key")
  assert_eq(retrieved, Some(IntValue(42)))
  
  // Test with different integer values
  Attributes::set(attrs, "zero.int", IntValue(0))
  let zero_int = Attributes::get(attrs, "zero.int")
  assert_eq(zero_int, Some(IntValue(0)))
  
  Attributes::set(attrs, "negative.int", IntValue(-100))
  let negative_int = Attributes::get(attrs, "negative.int")
  assert_eq(negative_int, Some(IntValue(-100)))
}

test "float attribute value operations" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "float.key", FloatValue(3.14159))
  
  let retrieved = Attributes::get(attrs, "float.key")
  assert_eq(retrieved, Some(FloatValue(3.14159)))
  
  // Test with different float values
  Attributes::set(attrs, "zero.float", FloatValue(0.0))
  let zero_float = Attributes::get(attrs, "zero.float")
  assert_eq(zero_float, Some(FloatValue(0.0)))
  
  Attributes::set(attrs, "negative.float", FloatValue(-2.71828))
  let negative_float = Attributes::get(attrs, "negative.float")
  assert_eq(negative_float, Some(FloatValue(-2.71828)))
}

test "boolean attribute value operations" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "bool.true", BoolValue(true))
  Attributes::set(attrs, "bool.false", BoolValue(false))
  
  let true_value = Attributes::get(attrs, "bool.true")
  let false_value = Attributes::get(attrs, "bool.false")
  
  assert_eq(true_value, Some(BoolValue(true)))
  assert_eq(false_value, Some(BoolValue(false)))
}

test "array string attribute value operations" {
  let attrs = Attributes::new()
  let string_array = ["item1", "item2", "item3"]
  Attributes::set(attrs, "array.string", ArrayStringValue(string_array))
  
  let retrieved = Attributes::get(attrs, "array.string")
  assert_eq(retrieved, Some(ArrayStringValue(string_array)))
  
  // Test with empty array
  let empty_array = [] : Array[String]
  Attributes::set(attrs, "array.empty", ArrayStringValue(empty_array))
  let empty_retrieved = Attributes::get(attrs, "array.empty")
  assert_eq(empty_retrieved, Some(ArrayStringValue(empty_array)))
}

test "array integer attribute value operations" {
  let attrs = Attributes::new()
  let int_array = [1, 2, 3, 4, 5]
  Attributes::set(attrs, "array.int", ArrayIntValue(int_array))
  
  let retrieved = Attributes::get(attrs, "array.int")
  assert_eq(retrieved, Some(ArrayIntValue(int_array)))
  
  // Test with single element array
  let single_array = [42]
  Attributes::set(attrs, "array.single", ArrayIntValue(single_array))
  let single_retrieved = Attributes::get(attrs, "array.single")
  assert_eq(single_retrieved, Some(ArrayIntValue(single_array)))
}

test "mixed attribute value types" {
  let attrs = Attributes::new()
  
  // Set different types of attributes
  Attributes::set(attrs, "name", StringValue("test-service"))
  Attributes::set(attrs, "port", IntValue(8080))
  Attributes::set(attrs, "enabled", BoolValue(true))
  Attributes::set(attrs, "version", StringValue("1.0.0"))
  
  // Retrieve and verify each type
  let name = Attributes::get(attrs, "name")
  let port = Attributes::get(attrs, "port")
  let enabled = Attributes::get(attrs, "enabled")
  let version = Attributes::get(attrs, "version")
  
  assert_eq(name, Some(StringValue("test-service")))
  assert_eq(port, Some(IntValue(8080)))
  assert_eq(enabled, Some(BoolValue(true)))
  assert_eq(version, Some(StringValue("1.0.0")))
}

test "attribute value special cases" {
  let attrs = Attributes::new()
  
  // Test with special characters in strings
  Attributes::set(attrs, "special.chars", StringValue("hello@world#123"))
  let special_chars = Attributes::get(attrs, "special.chars")
  assert_eq(special_chars, Some(StringValue("hello@world#123")))
  
  // Test with very large numbers
  Attributes::set(attrs, "large.int", IntValue(2147483647))
  let large_int = Attributes::get(attrs, "large.int")
  assert_eq(large_int, Some(IntValue(2147483647)))
  
  // Test with very small float values
  Attributes::set(attrs, "small.float", FloatValue(0.000001))
  let small_float = Attributes::get(attrs, "small.float")
  assert_eq(small_float, Some(FloatValue(0.000001)))
}

test "attribute value null and missing cases" {
  let attrs = Attributes::new()
  
  // Test missing attribute
  let missing = Attributes::get(attrs, "nonexistent.key")
  assert_eq(missing, None)
  
  // Test with empty key
  let empty_key = Attributes::get(attrs, "")
  assert_eq(empty_key, None)
  
  // Test with special key names
  Attributes::set(attrs, "null.key", StringValue("null"))
  let null_key = Attributes::get(attrs, "null.key")
  assert_eq(null_key, Some(StringValue("null")))
}