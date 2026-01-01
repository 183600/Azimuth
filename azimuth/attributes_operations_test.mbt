// Attributes Operations Test Suite for Azimuth Telemetry System
// This file contains test cases for Attributes operations

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
  
  // Test with different attribute value types
  Attributes::set(attrs, "string.attr", StringValue("hello"))
  Attributes::set(attrs, "int.attr", IntValue(123))
  Attributes::set(attrs, "float.attr", FloatValue(3.14))
  Attributes::set(attrs, "bool.attr", BoolValue(true))
  Attributes::set(attrs, "array.string.attr", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int.attr", ArrayIntValue([1, 2, 3]))
  
  // Verify string attribute
  let string_result = Attributes::get(attrs, "string.attr")
  match string_result {
    Some(StringValue(value)) => assert_eq(value, "hello")
    _ => assert_true(false, "Expected StringValue")
  }
  
  // Verify int attribute
  let int_result = Attributes::get(attrs, "int.attr")
  match int_result {
    Some(IntValue(value)) => assert_eq(value, 123)
    _ => assert_true(false, "Expected IntValue")
  }
  
  // Verify float attribute
  let float_result = Attributes::get(attrs, "float.attr")
  match float_result {
    Some(FloatValue(value)) => assert_eq(value, 3.14)
    _ => assert_true(false, "Expected FloatValue")
  }
  
  // Verify bool attribute
  let bool_result = Attributes::get(attrs, "bool.attr")
  match bool_result {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false, "Expected BoolValue")
  }
  
  // Verify array string attribute
  let array_string_result = Attributes::get(attrs, "array.string.attr")
  match array_string_result {
    Some(ArrayStringValue(values)) => assert_eq(values, ["a", "b", "c"])
    _ => assert_true(false, "Expected ArrayStringValue")
  }
  
  // Verify array int attribute
  let array_int_result = Attributes::get(attrs, "array.int.attr")
  match array_int_result {
    Some(ArrayIntValue(values)) => assert_eq(values, [1, 2, 3])
    _ => assert_true(false, "Expected ArrayIntValue")
  }
}

test "attributes with special characters" {
  let attrs = Attributes::new()
  
  // Test with keys containing special characters
  Attributes::set(attrs, "key.with.dots", StringValue("value1"))
  Attributes::set(attrs, "key-with-dashes", StringValue("value2"))
  Attributes::set(attrs, "key_with_underscores", StringValue("value3"))
  Attributes::set(attrs, "key with spaces", StringValue("value4"))
  
  // Verify values can be retrieved
  let value1 = Attributes::get(attrs, "key.with.dots")
  assert_eq(value1, Some(StringValue("value1")))
  
  let value2 = Attributes::get(attrs, "key-with-dashes")
  assert_eq(value2, Some(StringValue("value2")))
  
  let value3 = Attributes::get(attrs, "key_with_underscores")
  assert_eq(value3, Some(StringValue("value3")))
  
  let value4 = Attributes::get(attrs, "key with spaces")
  assert_eq(value4, Some(StringValue("value4")))
}

test "attributes with empty values" {
  let attrs = Attributes::new()
  
  // Test with empty string
  Attributes::set(attrs, "empty.string", StringValue(""))
  let empty_string = Attributes::get(attrs, "empty.string")
  assert_eq(empty_string, Some(StringValue("")))
  
  // Test with zero values
  Attributes::set(attrs, "zero.int", IntValue(0))
  let zero_int = Attributes::get(attrs, "zero.int")
  assert_eq(zero_int, Some(IntValue(0)))
  
  Attributes::set(attrs, "zero.float", FloatValue(0.0))
  let zero_float = Attributes::get(attrs, "zero.float")
  assert_eq(zero_float, Some(FloatValue(0.0)))
  
  // Test with empty arrays
  Attributes::set(attrs, "empty.array.string", ArrayStringValue([]))
  let empty_array_string = Attributes::get(attrs, "empty.array.string")
  assert_eq(empty_array_string, Some(ArrayStringValue([])))
  
  Attributes::set(attrs, "empty.array.int", ArrayIntValue([]))
  let empty_array_int = Attributes::get(attrs, "empty.array.int")
  assert_eq(empty_array_int, Some(ArrayIntValue([])))
}