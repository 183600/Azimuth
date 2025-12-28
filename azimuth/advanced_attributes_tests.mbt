// Advanced Attributes Operations Test Suite
// Tests complex attribute operations and edge cases

test "attributes deep nesting operations" {
  let attrs = Attributes::new()
  
  // Test setting various attribute types
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.string.key", ArrayStringValue(["value1", "value2", "value3"]))
  Attributes::set(attrs, "array.int.key", ArrayIntValue([1, 2, 3, 4, 5]))
  
  // Test retrieving string attributes
  let string_value = Attributes::get(attrs, "string.key")
  assert_eq(string_value, Some(StringValue("test_value")))
  
  // Test retrieving int attributes
  let int_value = Attributes::get(attrs, "int.key")
  assert_eq(int_value, Some(IntValue(42)))
  
  // Test retrieving float attributes
  let float_value = Attributes::get(attrs, "float.key")
  assert_eq(float_value, Some(FloatValue(3.14)))
  
  // Test retrieving boolean attributes
  let bool_value = Attributes::get(attrs, "bool.key")
  assert_eq(bool_value, Some(BoolValue(true)))
  
  // Test retrieving array string attributes
  let array_string_value = Attributes::get(attrs, "array.string.key")
  assert_eq(array_string_value, Some(ArrayStringValue(["value1", "value2", "value3"])))
  
  // Test retrieving array int attributes
  let array_int_value = Attributes::get(attrs, "array.int.key")
  assert_eq(array_int_value, Some(ArrayIntValue([1, 2, 3, 4, 5])))
  
  // Test missing attribute
  let missing_value = Attributes::get(attrs, "missing.key")
  assert_eq(missing_value, None)
}

test "attributes type conversion operations" {
  let attrs = Attributes::new()
  
  // Test setting and retrieving various numeric types
  Attributes::set(attrs, "small.int", IntValue(1))
  Attributes::set(attrs, "large.int", IntValue(2147483647))
  Attributes::set(attrs, "negative.int", IntValue(-42))
  Attributes::set(attrs, "zero.int", IntValue(0))
  
  // Test float precision
  Attributes::set(attrs, "precision.float", FloatValue(3.14159265359))
  Attributes::set(attrs, "scientific.float", FloatValue(1.23e-4))
  Attributes::set(attrs, "negative.float", FloatValue(-2.718))
  
  // Verify retrieval
  assert_eq(Attributes::get(attrs, "small.int"), Some(IntValue(1)))
  assert_eq(Attributes::get(attrs, "large.int"), Some(IntValue(2147483647)))
  assert_eq(Attributes::get(attrs, "negative.int"), Some(IntValue(-42)))
  assert_eq(Attributes::get(attrs, "zero.int"), Some(IntValue(0)))
  
  assert_eq(Attributes::get(attrs, "precision.float"), Some(FloatValue(3.14159265359)))
  assert_eq(Attributes::get(attrs, "scientific.float"), Some(FloatValue(1.23e-4)))
  assert_eq(Attributes::get(attrs, "negative.float"), Some(FloatValue(-2.718)))
}

test "attributes complex string operations" {
  let attrs = Attributes::new()
  
  // Test complex string values
  Attributes::set(attrs, "empty.string", StringValue(""))
  Attributes::set(attrs, "whitespace.string", StringValue("   "))
  Attributes::set(attrs, "unicode.string", StringValue("测试字符串🚀"))
  Attributes::set(attrs, "json.string", StringValue("{\"key\": \"value\", \"number\": 42}"))
  Attributes::set(attrs, "url.string", StringValue("https://example.com/path?param=value&other=123"))
  Attributes::set(attrs, "multiline.string", StringValue("Line 1\nLine 2\nLine 3"))
  
  // Verify retrieval
  assert_eq(Attributes::get(attrs, "empty.string"), Some(StringValue("")))
  assert_eq(Attributes::get(attrs, "whitespace.string"), Some(StringValue("   ")))
  assert_eq(Attributes::get(attrs, "unicode.string"), Some(StringValue("测试字符串🚀")))
  assert_eq(Attributes::get(attrs, "json.string"), Some(StringValue("{\"key\": \"value\", \"number\": 42}")))
  assert_eq(Attributes::get(attrs, "url.string"), Some(StringValue("https://example.com/path?param=value&other=123")))
  assert_eq(Attributes::get(attrs, "multiline.string"), Some(StringValue("Line 1\nLine 2\nLine 3")))
}

test "attributes array operations edge cases" {
  let attrs = Attributes::new()
  
  // Test empty arrays
  Attributes::set(attrs, "empty.string.array", ArrayStringValue([]))
  Attributes::set(attrs, "empty.int.array", ArrayIntValue([]))
  
  // Test single element arrays
  Attributes::set(attrs, "single.string.array", ArrayStringValue(["only"]))
  Attributes::set(attrs, "single.int.array", ArrayIntValue([42]))
  
  // Test large arrays
  let large_string_array = Array[String]::make(100, "element")
  let large_int_array = Array[Int]::make(100, 1)
  Attributes::set(attrs, "large.string.array", ArrayStringValue(large_string_array))
  Attributes::set(attrs, "large.int.array", ArrayIntValue(large_int_array))
  
  // Verify retrieval
  assert_eq(Attributes::get(attrs, "empty.string.array"), Some(ArrayStringValue([])))
  assert_eq(Attributes::get(attrs, "empty.int.array"), Some(ArrayIntValue([])))
  assert_eq(Attributes::get(attrs, "single.string.array"), Some(ArrayStringValue(["only"])))
  assert_eq(Attributes::get(attrs, "single.int.array"), Some(ArrayIntValue([42])))
  
  // Test large arrays (first element verification)
  let retrieved_large_string = Attributes::get(attrs, "large.string.array")
  match retrieved_large_string {
    Some(ArrayStringValue(arr)) => assert_eq(arr.length(), 100)
    _ => assert_false(true)
  }
  
  let retrieved_large_int = Attributes::get(attrs, "large.int.array")
  match retrieved_large_int {
    Some(ArrayIntValue(arr)) => assert_eq(arr.length(), 100)
    _ => assert_false(true)
  }
}

test "attributes boolean logic operations" {
  let attrs = Attributes::new()
  
  // Test various boolean values
  Attributes::set(attrs, "true.value", BoolValue(true))
  Attributes::set(attrs, "false.value", BoolValue(false))
  
  // Test boolean-like strings
  Attributes::set(attrs, "string.true", StringValue("true"))
  Attributes::set(attrs, "string.false", StringValue("false"))
  Attributes::set(attrs, "string.one", StringValue("1"))
  Attributes::set(attrs, "string.zero", StringValue("0"))
  
  // Test boolean-like numbers
  Attributes::set(attrs, "int.one", IntValue(1))
  Attributes::set(attrs, "int.zero", IntValue(0))
  Attributes::set(attrs, "int.negative", IntValue(-1))
  
  // Verify retrieval
  assert_eq(Attributes::get(attrs, "true.value"), Some(BoolValue(true)))
  assert_eq(Attributes::get(attrs, "false.value"), Some(BoolValue(false)))
  assert_eq(Attributes::get(attrs, "string.true"), Some(StringValue("true")))
  assert_eq(Attributes::get(attrs, "string.false"), Some(StringValue("false")))
  assert_eq(Attributes::get(attrs, "string.one"), Some(StringValue("1")))
  assert_eq(Attributes::get(attrs, "string.zero"), Some(StringValue("0")))
  assert_eq(Attributes::get(attrs, "int.one"), Some(IntValue(1)))
  assert_eq(Attributes::get(attrs, "int.zero"), Some(IntValue(0)))
  assert_eq(Attributes::get(attrs, "int.negative"), Some(IntValue(-1)))
}

test "attributes mixed type key operations" {
  let attrs = Attributes::new()
  
  // Test same key with different value types (should overwrite)
  Attributes::set(attrs, "mixed.key", StringValue("string_value"))
  Attributes::set(attrs, "mixed.key", IntValue(42))
  Attributes::set(attrs, "mixed.key", BoolValue(true))
  
  // Verify final value
  let final_value = Attributes::get(attrs, "mixed.key")
  assert_eq(final_value, Some(BoolValue(true)))
  
  // Test similar but different keys
  Attributes::set(attrs, "key", StringValue("simple"))
  Attributes::set(attrs, "key.with.dots", StringValue("dotted"))
  Attributes::set(attrs, "key-with-dashes", StringValue("dashed"))
  Attributes::set(attrs, "key_with_underscores", StringValue("underscored"))
  Attributes::set(attrs, "KEY", StringValue("uppercase"))
  Attributes::set(attrs, "key", StringValue("lowercase"))
  
  // Verify all different keys are treated separately
  assert_eq(Attributes::get(attrs, "key"), Some(StringValue("lowercase")))
  assert_eq(Attributes::get(attrs, "key.with.dots"), Some(StringValue("dotted")))
  assert_eq(Attributes::get(attrs, "key-with-dashes"), Some(StringValue("dashed")))
  assert_eq(Attributes::get(attrs, "key_with_underscores"), Some(StringValue("underscored")))
  assert_eq(Attributes::get(attrs, "KEY"), Some(StringValue("uppercase")))
}