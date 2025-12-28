// Attributes Operations Comprehensive Tests for Azimuth Telemetry System
// This file contains comprehensive test cases for attribute operations

test "attributes deep nested operations" {
  let attrs = Attributes::new()
  
  // Test setting multiple attributes of different types
  Attributes::set(attrs, "user.id", IntValue(12345))
  Attributes::set(attrs, "user.name", StringValue("john.doe"))
  Attributes::set(attrs, "user.active", BoolValue(true))
  Attributes::set(attrs, "user.score", FloatValue(95.5))
  Attributes::set(attrs, "user.tags", ArrayStringValue(["admin", "premium", "verified"]))
  Attributes::set(attrs, "user.levels", ArrayIntValue([1, 2, 3, 4, 5]))
  
  // Test retrieving each attribute type
  let user_id = Attributes::get(attrs, "user.id")
  assert_eq(user_id, Some(IntValue(12345)))
  
  let user_name = Attributes::get(attrs, "user.name")
  assert_eq(user_name, Some(StringValue("john.doe")))
  
  let user_active = Attributes::get(attrs, "user.active")
  assert_eq(user_active, Some(BoolValue(true)))
  
  let user_score = Attributes::get(attrs, "user.score")
  assert_eq(user_score, Some(FloatValue(95.5)))
  
  let user_tags = Attributes::get(attrs, "user.tags")
  assert_eq(user_tags, Some(ArrayStringValue(["admin", "premium", "verified"])))
  
  let user_levels = Attributes::get(attrs, "user.levels")
  assert_eq(user_levels, Some(ArrayIntValue([1, 2, 3, 4, 5])))
}

test "attributes value type conversion and validation" {
  let attrs = Attributes::new()
  
  // Test string values with special characters
  Attributes::set(attrs, "special.chars", StringValue("Hello, World! @#$%^&*()"))
  let special_chars = Attributes::get(attrs, "special.chars")
  assert_eq(special_chars, Some(StringValue("Hello, World! @#$%^&*()")))
  
  // Test empty string value
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
  let negative_int = Attributes::get(attrs, "negative.int")
  let negative_float = Attributes::get(attrs, "negative.float")
  assert_eq(negative_int, Some(IntValue(-42)))
  assert_eq(negative_float, Some(FloatValue(-3.14)))
  
  // Test large values
  Attributes::set(attrs, "large.int", IntValue(2147483647))
  Attributes::set(attrs, "large.float", FloatValue(999999.999999))
  let large_int = Attributes::get(attrs, "large.int")
  let large_float = Attributes::get(attrs, "large.float")
  assert_eq(large_int, Some(IntValue(2147483647)))
  assert_eq(large_float, Some(FloatValue(999999.999999)))
}

test "attributes array operations" {
  let attrs = Attributes::new()
  
  // Test empty arrays
  Attributes::set(attrs, "empty.string.array", ArrayStringValue([]))
  Attributes::set(attrs, "empty.int.array", ArrayIntValue([]))
  let empty_string_array = Attributes::get(attrs, "empty.string.array")
  let empty_int_array = Attributes::get(attrs, "empty.int.array")
  assert_eq(empty_string_array, Some(ArrayStringValue([])))
  assert_eq(empty_int_array, Some(ArrayIntValue([])))
  
  // Test single element arrays
  Attributes::set(attrs, "single.string.array", ArrayStringValue(["only"]))
  Attributes::set(attrs, "single.int.array", ArrayIntValue([42]))
  let single_string_array = Attributes::get(attrs, "single.string.array")
  let single_int_array = Attributes::get(attrs, "single.int.array")
  assert_eq(single_string_array, Some(ArrayStringValue(["only"])))
  assert_eq(single_int_array, Some(ArrayIntValue([42])))
  
  // Test large arrays
  let large_string_array = ArrayStringValue(["item1", "item2", "item3", "item4", "item5", "item6", "item7", "item8", "item9", "item10"])
  let large_int_array = ArrayIntValue([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  Attributes::set(attrs, "large.string.array", large_string_array)
  Attributes::set(attrs, "large.int.array", large_int_array)
  let retrieved_large_string_array = Attributes::get(attrs, "large.string.array")
  let retrieved_large_int_array = Attributes::get(attrs, "large.int.array")
  assert_eq(retrieved_large_string_array, Some(large_string_array))
  assert_eq(retrieved_large_int_array, Some(large_int_array))
}

test "attributes key naming conventions" {
  let attrs = Attributes::new()
  
  // Test various key naming patterns
  Attributes::set(attrs, "simple.key", StringValue("simple"))
  Attributes::set(attrs, "key.with.dots", StringValue("dots"))
  Attributes::set(attrs, "key_with_underscores", StringValue("underscores"))
  Attributes::set(attrs, "key-with-hyphens", StringValue("hyphens"))
  Attributes::set(attrs, "UPPERCASE.KEY", StringValue("uppercase"))
  Attributes::set(attrs, "mixedCase.Key", StringValue("mixedcase"))
  Attributes::set(attrs, "key123.with456.numbers789", StringValue("numbers"))
  
  // Test retrieval with exact key matching
  assert_eq(Attributes::get(attrs, "simple.key"), Some(StringValue("simple")))
  assert_eq(Attributes::get(attrs, "key.with.dots"), Some(StringValue("dots")))
  assert_eq(Attributes::get(attrs, "key_with_underscores"), Some(StringValue("underscores")))
  assert_eq(Attributes::get(attrs, "key-with-hyphens"), Some(StringValue("hyphens")))
  assert_eq(Attributes::get(attrs, "UPPERCASE.KEY"), Some(StringValue("uppercase")))
  assert_eq(Attributes::get(attrs, "mixedCase.Key"), Some(StringValue("mixedcase")))
  assert_eq(Attributes::get(attrs, "key123.with456.numbers789"), Some(StringValue("numbers")))
  
  // Test case sensitivity
  assert_eq(Attributes::get(attrs, "SIMPLE.KEY"), None)  // Should not match lowercase
  assert_eq(Attributes::get(attrs, "Simple.Key"), None)  // Should not match different case
}

test "attributes complex scenarios" {
  let attrs = Attributes::new()
  
  // Test overwriting attributes
  Attributes::set(attrs, "overwrite.test", StringValue("original"))
  assert_eq(Attributes::get(attrs, "overwrite.test"), Some(StringValue("original")))
  
  Attributes::set(attrs, "overwrite.test", IntValue(42))
  assert_eq(Attributes::get(attrs, "overwrite.test"), Some(IntValue(42)))
  
  // Test attribute value type changes
  Attributes::set(attrs, "type.change.test", BoolValue(true))
  assert_eq(Attributes::get(attrs, "type.change.test"), Some(BoolValue(true)))
  
  Attributes::set(attrs, "type.change.test", FloatValue(3.14159))
  assert_eq(Attributes::get(attrs, "type.change.test"), Some(FloatValue(3.14159)))
  
  // Test very long keys and values
  let long_key = "this.is.a.very.long.attribute.key.that.tests.the.systems.ability.to.handle.extended.key.names.with.multiple.segments.and.various.characters"
  let long_value = "This is a very long attribute value that tests the system's ability to handle extended text content with various characters and punctuation marks! @#$%^&*()_+-={}[]|\\:;\"'<>?,./"
  
  Attributes::set(attrs, long_key, StringValue(long_value))
  let retrieved_long = Attributes::get(attrs, long_key)
  assert_eq(retrieved_long, Some(StringValue(long_value)))
}

test "attributes pattern matching" {
  let attrs = Attributes::new()
  
  // Set up test attributes
  Attributes::set(attrs, "string.test", StringValue("test_value"))
  Attributes::set(attrs, "int.test", IntValue(100))
  Attributes::set(attrs, "float.test", FloatValue(50.5))
  Attributes::set(attrs, "bool.test", BoolValue(true))
  Attributes::set(attrs, "array.string.test", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int.test", ArrayIntValue([1, 2, 3]))
  
  // Test pattern matching for different attribute types
  match Attributes::get(attrs, "string.test") {
    Some(StringValue(value)) => assert_eq(value, "test_value")
    _ => assert_true(false, "Expected StringValue")
  }
  
  match Attributes::get(attrs, "int.test") {
    Some(IntValue(value)) => assert_eq(value, 100)
    _ => assert_true(false, "Expected IntValue")
  }
  
  match Attributes::get(attrs, "float.test") {
    Some(FloatValue(value)) => assert_eq(value, 50.5)
    _ => assert_true(false, "Expected FloatValue")
  }
  
  match Attributes::get(attrs, "bool.test") {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false, "Expected BoolValue")
  }
  
  match Attributes::get(attrs, "array.string.test") {
    Some(ArrayStringValue(values)) => assert_eq(values, ["a", "b", "c"])
    _ => assert_true(false, "Expected ArrayStringValue")
  }
  
  match Attributes::get(attrs, "array.int.test") {
    Some(ArrayIntValue(values)) => assert_eq(values, [1, 2, 3])
    _ => assert_true(false, "Expected ArrayIntValue")
  }
}

test "attributes boundary conditions" {
  let attrs = Attributes::new()
  
  // Test attribute with empty key (should be handled gracefully)
  Attributes::set(attrs, "", StringValue("empty.key.test"))
  let empty_key_result = Attributes::get(attrs, "")
  // The behavior depends on implementation - testing that it doesn't crash
  
  // Test attribute with whitespace-only key
  Attributes::set(attrs, "   ", StringValue("whitespace.key"))
  let whitespace_key_result = Attributes::get(attrs, "   ")
  
  // Test attribute with special characters in key
  Attributes::set(attrs, "key!@#$%^&*()", StringValue("special.chars.key"))
  let special_chars_key_result = Attributes::get(attrs, "key!@#$%^&*()")
  
  // Test Unicode characters in key and value
  Attributes::set(attrs, "unicode.键", StringValue("unicode值测试"))
  let unicode_result = Attributes::get(attrs, "unicode.键")
  assert_eq(unicode_result, Some(StringValue("unicode值测试")))
  
  // Test numeric strings
  Attributes::set(attrs, "numeric.string", StringValue("12345"))
  let numeric_string_result = Attributes::get(attrs, "numeric.string")
  assert_eq(numeric_string_result, Some(StringValue("12345")))
  
  // Test boolean strings
  Attributes::set(attrs, "boolean.string.true", StringValue("true"))
  Attributes::set(attrs, "boolean.string.false", StringValue("false"))
  let bool_string_true_result = Attributes::get(attrs, "boolean.string.true")
  let bool_string_false_result = Attributes::get(attrs, "boolean.string.false")
  assert_eq(bool_string_true_result, Some(StringValue("true")))
  assert_eq(bool_string_false_result, Some(StringValue("false")))
}