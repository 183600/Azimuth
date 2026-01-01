// Attribute Value Type Conversion and Boundary Condition Tests
// Test cases for comprehensive attribute value type handling and edge cases

test "attribute_value_string_type_operations" {
  // Test string attribute value creation and operations
  let string_val = StringValue("test_value")
  let attrs = Attributes::new()
  
  // Test setting and getting string values
  Attributes::set(attrs, "string.key", string_val)
  let retrieved = Attributes::get(attrs, "string.key")
  
  @assertion.assert_eq(retrieved, Some(StringValue("test_value")))?
  
  // Test with empty string
  let empty_string_val = StringValue("")
  Attributes::set(attrs, "empty.key", empty_string_val)
  let empty_retrieved = Attributes::get(attrs, "empty.key")
  @assertion.assert_eq(empty_retrieved, Some(StringValue("")))?
}

test "attribute_value_numeric_type_operations" {
  // Test numeric attribute value operations
  let attrs = Attributes::new()
  
  // Test integer values
  let int_val = IntValue(42)
  Attributes::set(attrs, "int.key", int_val)
  let int_retrieved = Attributes::get(attrs, "int.key")
  @assertion.assert_eq(int_retrieved, Some(IntValue(42)))?
  
  // Test with zero
  let zero_val = IntValue(0)
  Attributes::set(attrs, "zero.key", zero_val)
  let zero_retrieved = Attributes::get(attrs, "zero.key")
  @assertion.assert_eq(zero_retrieved, Some(IntValue(0)))?
  
  // Test with negative integers
  let negative_val = IntValue(-100)
  Attributes::set(attrs, "negative.key", negative_val)
  let negative_retrieved = Attributes::get(attrs, "negative.key")
  @assertion.assert_eq(negative_retrieved, Some(IntValue(-100)))?
  
  // Test float values
  let float_val = FloatValue(3.14159)
  Attributes::set(attrs, "float.key", float_val)
  let float_retrieved = Attributes::get(attrs, "float.key")
  @assertion.assert_eq(float_retrieved, Some(FloatValue(3.14159)))?
  
  // Test with special float values
  let infinity_val = FloatValue(1.0 / 0.0)  // Positive infinity
  Attributes::set(attrs, "infinity.key", infinity_val)
  let infinity_retrieved = Attributes::get(attrs, "infinity.key")
  @assertion.assert_eq(infinity_retrieved, Some(FloatValue(1.0 / 0.0)))?
}

test "attribute_value_boolean_type_operations" {
  // Test boolean attribute value operations
  let attrs = Attributes::new()
  
  // Test true value
  let true_val = BoolValue(true)
  Attributes::set(attrs, "true.key", true_val)
  let true_retrieved = Attributes::get(attrs, "true.key")
  @assertion.assert_eq(true_retrieved, Some(BoolValue(true)))?
  
  // Test false value
  let false_val = BoolValue(false)
  Attributes::set(attrs, "false.key", false_val)
  let false_retrieved = Attributes::get(attrs, "false.key")
  @assertion.assert_eq(false_retrieved, Some(BoolValue(false)))?
}

test "attribute_value_array_type_operations" {
  // Test array attribute value operations
  let attrs = Attributes::new()
  
  // Test string arrays
  let string_array = Array::["item1", "item2", "item3"]
  let string_array_val = ArrayStringValue(string_array)
  Attributes::set(attrs, "string.array.key", string_array_val)
  let string_array_retrieved = Attributes::get(attrs, "string.array.key")
  @assertion.assert_eq(string_array_retrieved, Some(ArrayStringValue(string_array)))?
  
  // Test empty string array
  let empty_string_array = Array::[]
  let empty_string_array_val = ArrayStringValue(empty_string_array)
  Attributes::set(attrs, "empty.string.array.key", empty_string_array_val)
  let empty_string_array_retrieved = Attributes::get(attrs, "empty.string.array.key")
  @assertion.assert_eq(empty_string_array_retrieved, Some(ArrayStringValue(empty_string_array)))?
  
  // Test integer arrays
  let int_array = Array::[1, 2, 3, 4, 5]
  let int_array_val = ArrayIntValue(int_array)
  Attributes::set(attrs, "int.array.key", int_array_val)
  let int_array_retrieved = Attributes::get(attrs, "int.array.key")
  @assertion.assert_eq(int_array_retrieved, Some(ArrayIntValue(int_array)))?
  
  // Test empty integer array
  let empty_int_array = Array::[]
  let empty_int_array_val = ArrayIntValue(empty_int_array)
  Attributes::set(attrs, "empty.int.array.key", empty_int_array_val)
  let empty_int_array_retrieved = Attributes::get(attrs, "empty.int.array.key")
  @assertion.assert_eq(empty_int_array_retrieved, Some(ArrayIntValue(empty_int_array)))?
}

test "attribute_value_boundary_conditions" {
  // Test attribute value boundary conditions
  let attrs = Attributes::new()
  
  // Test with very long strings
  let long_string = "a" * 10000  // 10K character string
  let long_string_val = StringValue(long_string)
  Attributes::set(attrs, "long.string.key", long_string_val)
  let long_string_retrieved = Attributes::get(attrs, "long.string.key")
  @assertion.assert_eq(long_string_retrieved, Some(StringValue(long_string)))?
  
  // Test with maximum integer values
  let max_int_val = IntValue(2147483647)  // Max 32-bit int
  Attributes::set(attrs, "max.int.key", max_int_val)
  let max_int_retrieved = Attributes::get(attrs, "max.int.key")
  @assertion.assert_eq(max_int_retrieved, Some(IntValue(2147483647)))?
  
  // Test with minimum integer values
  let min_int_val = IntValue(-2147483648)  // Min 32-bit int
  Attributes::set(attrs, "min.int.key", min_int_val)
  let min_int_retrieved = Attributes::get(attrs, "min.int.key")
  @assertion.assert_eq(min_int_retrieved, Some(IntValue(-2147483648)))?
  
  // Test with very small float values
  let tiny_float_val = FloatValue(1.0e-10)
  Attributes::set(attrs, "tiny.float.key", tiny_float_val)
  let tiny_float_retrieved = Attributes::get(attrs, "tiny.float.key")
  @assertion.assert_eq(tiny_float_retrieved, Some(FloatValue(1.0e-10)))?
}

test "attribute_value_nonexistent_key_handling" {
  // Test handling of nonexistent keys
  let attrs = Attributes::new()
  
  // Test getting nonexistent key
  let nonexistent = Attributes::get(attrs, "nonexistent.key")
  @assertion.assert_eq(nonexistent, None)?
  
  // Test with empty key
  let empty_key = Attributes::get(attrs, "")
  @assertion.assert_eq(empty_key, None)?
  
  // Test with special characters in key
  let special_key = Attributes::get(attrs, "special.key.with.dots_and_underscores")
  @assertion.assert_eq(special_key, None)?
}

test "attribute_value_type_consistency" {
  // Test that attribute value types remain consistent
  let attrs = Attributes::new()
  
  // Set multiple different types
  Attributes::set(attrs, "string.type", StringValue("string_value"))
  Attributes::set(attrs, "int.type", IntValue(123))
  Attributes::set(attrs, "float.type", FloatValue(45.67))
  Attributes::set(attrs, "bool.type", BoolValue(true))
  
  // Verify each type is preserved correctly
  let string_result = Attributes::get(attrs, "string.type")
  let int_result = Attributes::get(attrs, "int.type")
  let float_result = Attributes::get(attrs, "float.type")
  let bool_result = Attributes::get(attrs, "bool.type")
  
  @assertion.assert_eq(string_result, Some(StringValue("string_value")))?
  @assertion.assert_eq(int_result, Some(IntValue(123)))?
  @assertion.assert_eq(float_result, Some(FloatValue(45.67)))?
  @assertion.assert_eq(bool_result, Some(BoolValue(true)))?
}