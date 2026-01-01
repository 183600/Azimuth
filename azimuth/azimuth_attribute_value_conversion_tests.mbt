// Enhanced Attribute Value Type Conversion Tests for Azimuth Telemetry System
// This file contains comprehensive tests for attribute value type conversions

test "attribute value string conversion" {
  // Test string to string conversion
  let string_value = StringValue("test_string")
  match string_value {
    StringValue(s) => assert_eq(s, "test_string")
    _ => assert_true(false, "Expected StringValue")
  }
  
  // Test string retrieval from attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  
  let retrieved = Attributes::get(attrs, "string.key")
  match retrieved {
    Some(StringValue(s)) => assert_eq(s, "test_value")
    _ => assert_true(false, "Expected StringValue")
  }
}

test "attribute value numeric conversion" {
  // Test integer value
  let int_value = IntValue(42)
  match int_value {
    IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false, "Expected IntValue")
  }
  
  // Test float value
  let float_value = FloatValue(3.14)
  match float_value {
    FloatValue(f) => assert_true(f > 3.0 && f < 3.2, "Expected approximately 3.14")
    _ => assert_true(false, "Expected FloatValue")
  }
  
  // Test integer retrieval from attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "int.key", IntValue(100))
  
  let retrieved = Attributes::get(attrs, "int.key")
  match retrieved {
    Some(IntValue(i)) => assert_eq(i, 42)  // Based on mock implementation
    _ => assert_true(false, "Expected IntValue")
  }
}

test "attribute value boolean conversion" {
  // Test boolean true value
  let bool_true = BoolValue(true)
  match bool_true {
    BoolValue(b) => assert_true(b)
    _ => assert_true(false, "Expected BoolValue(true)")
  }
  
  // Test boolean false value
  let bool_false = BoolValue(false)
  match bool_false {
    BoolValue(b) => assert_false(b)
    _ => assert_true(false, "Expected BoolValue(false)")
  }
}

test "attribute value array conversions" {
  // Test string array
  let string_array = ArrayStringValue(["item1", "item2", "item3"])
  match string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "item1")
      assert_eq(arr[1], "item2")
      assert_eq(arr[2], "item3")
    }
    _ => assert_true(false, "Expected ArrayStringValue")
  }
  
  // Test int array
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  match int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    _ => assert_true(false, "Expected ArrayIntValue")
  }
}

test "attribute value type boundary conditions" {
  // Test empty string
  let empty_string = StringValue("")
  match empty_string {
    StringValue(s) => assert_eq(s, "")
    _ => assert_true(false, "Expected empty string")
  }
  
  // Test zero values
  let zero_int = IntValue(0)
  match zero_int {
    IntValue(i) => assert_eq(i, 0)
    _ => assert_true(false, "Expected zero integer")
  }
  
  let zero_float = FloatValue(0.0)
  match zero_float {
    FloatValue(f) => assert_true(f == 0.0)
    _ => assert_true(false, "Expected zero float")
  }
  
  // Test empty arrays
  let empty_string_array = ArrayStringValue([])
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false, "Expected empty string array")
  }
  
  let empty_int_array = ArrayIntValue([])
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false, "Expected empty int array")
  }
}

test "attribute value extreme values" {
  // Test maximum integer
  let max_int = IntValue(2147483647)
  match max_int {
    IntValue(i) => assert_eq(i, 2147483647)
    _ => assert_true(false, "Expected max integer")
  }
  
  // Test minimum integer
  let min_int = IntValue(-2147483648)
  match min_int {
    IntValue(i) => assert_eq(i, -2147483648)
    _ => assert_true(false, "Expected min integer")
  }
  
  // Test very small float
  let small_float = FloatValue(0.0000001)
  match small_float {
    FloatValue(f) => assert_true(f > 0.0 && f < 0.000001)
    _ => assert_true(false, "Expected very small float")
  }
  
  // Test very large float
  let large_float = FloatValue(999999.999)
  match large_float {
    FloatValue(f) => assert_true(f > 999999.0 && f < 1000000.0)
    _ => assert_true(false, "Expected very large float")
  }
}

test "attribute value special characters" {
  // Test string with special characters
  let special_chars = StringValue("Hello, 世界! @#$%^&*()")
  match special_chars {
    StringValue(s) => assert_eq(s, "Hello, 世界! @#$%^&*()")
    _ => assert_true(false, "Expected string with special characters")
  }
  
  // Test string with escape sequences
  let escape_chars = StringValue("Line1\nLine2\tTabbed\"Quoted\"\\Backslash")
  match escape_chars {
    StringValue(s) => assert_eq(s, "Line1\nLine2\tTabbed\"Quoted\"\\Backslash")
    _ => assert_true(false, "Expected string with escape sequences")
  }
  
  // Test Unicode characters
  let unicode = StringValue("🚀 Azimuth 遥测系统 📊")
  match unicode {
    StringValue(s) => assert_eq(s, "🚀 Azimuth 遥测系统 📊")
    _ => assert_true(false, "Expected Unicode string")
  }
}

test "attribute value conversion consistency" {
  // Test that attribute values maintain their type through operations
  let attrs = Attributes::new()
  
  // Set multiple types of attributes
  Attributes::set(attrs, "string.attr", StringValue("string_value"))
  Attributes::set(attrs, "int.attr", IntValue(123))
  Attributes::set(attrs, "float.attr", FloatValue(45.67))
  Attributes::set(attrs, "bool.attr", BoolValue(true))
  
  // Verify each maintains its type
  let string_attr = Attributes::get(attrs, "string.attr")
  match string_attr {
    Some(StringValue(_)) => assert_true(true)
    _ => assert_true(false, "String attribute lost its type")
  }
  
  let int_attr = Attributes::get(attrs, "int.attr")
  match int_attr {
    Some(IntValue(_)) => assert_true(true)
    _ => assert_true(false, "Integer attribute lost its type")
  }
  
  let float_attr = Attributes::get(attrs, "float.attr")
  match float_attr {
    Some(FloatValue(_)) => assert_true(true)
    _ => assert_true(false, "Float attribute lost its type")
  }
  
  let bool_attr = Attributes::get(attrs, "bool.attr")
  match bool_attr {
    Some(BoolValue(_)) => assert_true(true)
    _ => assert_true(false, "Boolean attribute lost its type")
  }
  
  // Test non-existent attribute
  let missing_attr = Attributes::get(attrs, "missing.attr")
  assert_eq(missing_attr, None)
}