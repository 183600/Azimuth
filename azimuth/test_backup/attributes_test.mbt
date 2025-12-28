// Test file for Attributes functionality

pub test "attributes creation and basic operations" {
  let attrs = Attributes::new()
  assert_eq(attrs.values.length, 0)
  
  Attributes::set(attrs, "test.key", StringValue("test.value"))
  Attributes::set(attrs, "number.key", IntValue(123))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  // Test getting string value
  let string_val = Attributes::get(attrs, "string.key")
  assert_eq(string_val, Some(StringValue("test_value")))
  
  // Test getting int value
  let int_val = Attributes::get(attrs, "int.key")
  assert_eq(int_val, Some(IntValue(42)))
  
  // Test non-existent key
  let none_val = Attributes::get(attrs, "nonexistent.key")
  assert_eq(none_val, None)
}

pub test "attributes with different value types" {
  let attrs = Attributes::new()
  
  // Test all attribute value types
  let string_attr = StringValue("hello world")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14159)
  let bool_attr = BoolValue(false)
  let array_string_attr = ArrayStringValue(["a", "b", "c"])
  let array_int_attr = ArrayIntValue([1, 2, 3])
  
  assert_eq(string_attr, StringValue("hello world"))
  assert_eq(int_attr, IntValue(42))
  assert_eq(float_attr, FloatValue(3.14159))
  assert_eq(bool_attr, BoolValue(false))
  assert_eq(array_string_attr, ArrayStringValue(["a", "b", "c"]))
  assert_eq(array_int_attr, ArrayIntValue([1, 2, 3]))
}

pub test "attributes edge cases" {
  let attrs = Attributes::new()
  
  // Test empty key
  Attributes::set(attrs, "", StringValue("empty key"))
  let empty_key_val = Attributes::get(attrs, "")
  assert_eq(empty_key_val, None)
  
  // Test special characters in key
  Attributes::set(attrs, "special.key.with.dots", StringValue("special"))
  let special_val = Attributes::get(attrs, "special.key.with.dots")
  assert_eq(special_val, None)
  
  // Test very long key
  let long_key = "a".repeat(1000)
  Attributes::set(attrs, long_key, StringValue("long key"))
  let long_key_val = Attributes::get(attrs, long_key)
  assert_eq(long_key_val, None)
}