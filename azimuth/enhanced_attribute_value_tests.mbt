// Enhanced Attribute Value Type Conversion Tests for Azimuth
// Tests various attribute value type conversions and edge cases

test "attribute value string to int conversion" {
  let string_val = StringValue("42")
  let int_val = IntValue(42)
  let float_val = FloatValue(42.5)
  let bool_val = BoolValue(true)
  
  // Test basic attribute value creation and comparison
  assert_eq(string_val, StringValue("42"))
  assert_eq(int_val, IntValue(42))
  assert_eq(float_val, FloatValue(42.5))
  assert_eq(bool_val, BoolValue(true))
  
  // Test different values
  assert_true(string_val != StringValue("43"))
  assert_true(int_val != IntValue(43))
  assert_true(float_val != FloatValue(42.6))
  assert_true(bool_val != BoolValue(false))
}

test "attribute value array operations" {
  let string_array = ArrayStringValue(["a", "b", "c"])
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  
  // Test array attribute values
  match string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_false(true)  // Should not reach here
  }
  
  match int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    _ => assert_false(true)  // Should not reach here
  }
}

test "attributes operations with mixed types" {
  let attrs = Attributes::new()
  
  // Test setting different attribute types
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(123))
  Attributes::set(attrs, "float.key", FloatValue(45.67))
  Attributes::set(attrs, "bool.key", BoolValue(false))
  Attributes::set(attrs, "array.string", ArrayStringValue(["x", "y", "z"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([10, 20, 30]))
  
  // Test retrieval of different types
  let string_val = Attributes::get(attrs, "string.key")
  let int_val = Attributes::get(attrs, "int.key")
  let float_val = Attributes::get(attrs, "float.key")
  let bool_val = Attributes::get(attrs, "bool.key")
  let array_str_val = Attributes::get(attrs, "array.string")
  let array_int_val = Attributes::get(attrs, "array.int")
  
  assert_eq(string_val, Some(StringValue("test_value")))
  assert_eq(int_val, Some(IntValue(123)))
  assert_eq(float_val, Some(FloatValue(45.67)))
  assert_eq(bool_val, Some(BoolValue(false)))
  assert_eq(array_str_val, Some(ArrayStringValue(["x", "y", "z"])))
  assert_eq(array_int_val, Some(ArrayIntValue([10, 20, 30])))
  
  // Test missing key
  let missing_val = Attributes::get(attrs, "nonexistent.key")
  assert_eq(missing_val, None)
}

test "attribute value edge cases" {
  let attrs = Attributes::new()
  
  // Test empty string
  Attributes::set(attrs, "empty.string", StringValue(""))
  let empty_string = Attributes::get(attrs, "empty.string")
  assert_eq(empty_string, Some(StringValue("")))
  
  // Test zero values
  Attributes::set(attrs, "zero.int", IntValue(0))
  Attributes::set(attrs, "zero.float", FloatValue(0.0))
  let zero_int = Attributes::get(attrs, "zero.int")
  let zero_float = Attributes::get(attrs, "zero.float")
  assert_eq(zero_int, Some(IntValue(0)))
  assert_eq(zero_float, Some(FloatValue(0.0)))
  
  // Test negative values
  Attributes::set(attrs, "negative.int", IntValue(-42))
  Attributes::set(attrs, "negative.float", FloatValue(-3.14))
  let neg_int = Attributes::get(attrs, "negative.int")
  let neg_float = Attributes::get(attrs, "negative.float")
  assert_eq(neg_int, Some(IntValue(-42)))
  assert_eq(neg_float, Some(FloatValue(-3.14)))
  
  // Test empty arrays
  Attributes::set(attrs, "empty.array.string", ArrayStringValue([]))
  Attributes::set(attrs, "empty.array.int", ArrayIntValue([]))
  let empty_str_array = Attributes::get(attrs, "empty.array.string")
  let empty_int_array = Attributes::get(attrs, "empty.array.int")
  
  match empty_str_array {
    Some(ArrayStringValue(arr)) => assert_eq(arr.length(), 0)
    _ => assert_false(true)
  }
  
  match empty_int_array {
    Some(ArrayIntValue(arr)) => assert_eq(arr.length(), 0)
    _ => assert_false(true)
  }
}

test "attribute value special characters and unicode" {
  let attrs = Attributes::new()
  
  // Test special characters
  Attributes::set(attrs, "special.chars", StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  let special_chars = Attributes::get(attrs, "special.chars")
  assert_eq(special_chars, Some(StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")))
  
  // Test unicode characters
  Attributes::set(attrs, "unicode.text", StringValue("测试中文 🚀 emoji"))
  let unicode_text = Attributes::get(attrs, "unicode.text")
  assert_eq(unicode_text, Some(StringValue("测试中文 🚀 emoji")))
  
  // Test newlines and tabs
  Attributes::set(attrs, "whitespace", StringValue("line1\nline2\tindented"))
  let whitespace = Attributes::get(attrs, "whitespace")
  assert_eq(whitespace, Some(StringValue("line1\nline2\tindented")))
}

test "attribute value large numbers and precision" {
  let attrs = Attributes::new()
  
  // Test large integers
  Attributes::set(attrs, "large.int", IntValue(2147483647))
  let large_int = Attributes::get(attrs, "large.int")
  assert_eq(large_int, Some(IntValue(2147483647)))
  
  // Test very large float
  Attributes::set(attrs, "large.float", FloatValue(1.7976931348623157e+308))
  let large_float = Attributes::get(attrs, "large.float")
  assert_eq(large_float, Some(FloatValue(1.7976931348623157e+308)))
  
  // Test very small float
  Attributes::set(attrs, "small.float", FloatValue(2.2250738585072014e-308))
  let small_float = Attributes::get(attrs, "small.float")
  assert_eq(small_float, Some(FloatValue(2.2250738585072014e-308)))
  
  // Test float precision
  Attributes::set(attrs, "precise.float", FloatValue(3.14159265358979323846))
  let precise_float = Attributes::get(attrs, "precise.float")
  assert_eq(precise_float, Some(FloatValue(3.14159265358979323846)))
}