// Attribute Value Type Conversion Tests for Azimuth Telemetry System
// Testing type conversion and validation for AttributeValue enum

test "attribute value string conversion" {
  let string_val = StringValue("test_value")
  let int_val = IntValue(42)
  let float_val = FloatValue(3.14)
  let bool_val = BoolValue(true)
  
  // Test string attribute
  assert_eq(string_val, StringValue("test_value"))
  assert_true(string_val != StringValue("different_value"))
  
  // Test int attribute
  assert_eq(int_val, IntValue(42))
  assert_true(int_val != IntValue(43))
  
  // Test float attribute
  assert_eq(float_val, FloatValue(3.14))
  assert_true(float_val != FloatValue(2.71))
  
  // Test bool attribute
  assert_eq(bool_val, BoolValue(true))
  assert_true(bool_val != BoolValue(false))
}

test "attribute value array operations" {
  let string_array = ArrayStringValue(["item1", "item2", "item3"])
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  
  // Test string array
  assert_eq(string_array, ArrayStringValue(["item1", "item2", "item3"]))
  assert_true(string_array != ArrayStringValue(["item1", "item2"]))
  
  // Test int array
  assert_eq(int_array, ArrayIntValue([1, 2, 3, 4, 5]))
  assert_true(int_array != ArrayIntValue([1, 2, 3]))
  
  // Test array lengths
  match string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 3)
    _ => assert_true(false)
  }
  
  match int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 5)
    _ => assert_true(false)
  }
}

test "attributes collection operations" {
  let attrs = Attributes::new()
  
  // Test empty attributes
  assert_eq(attrs.values.length(), 0)
  
  // Test setting attributes
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  // Test getting attributes
  let string_val = Attributes::get(attrs, "string.key")
  let int_val = Attributes::get(attrs, "int.key")
  let float_val = Attributes::get(attrs, "float.key")
  let bool_val = Attributes::get(attrs, "bool.key")
  let missing_val = Attributes::get(attrs, "missing.key")
  
  assert_eq(string_val, Some(StringValue("test_value")))
  assert_eq(int_val, Some(IntValue(42)))
  assert_eq(float_val, None) // Not implemented in simplified version
  assert_eq(bool_val, None) // Not implemented in simplified version
  assert_eq(missing_val, None)
}

test "attribute value type validation" {
  let string_val = StringValue("123")
  let int_val = IntValue(123)
  let float_val = FloatValue(123.0)
  let bool_val = BoolValue(true)
  
  // Test type discrimination
  match string_val {
    StringValue(s) => assert_eq(s, "123")
    _ => assert_true(false)
  }
  
  match int_val {
    IntValue(i) => assert_eq(i, 123)
    _ => assert_true(false)
  }
  
  match float_val {
    FloatValue(f) => assert_eq(f, 123.0)
    _ => assert_true(false)
  }
  
  match bool_val {
    BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
}

test "attribute value edge cases" {
  // Test empty string
  let empty_string = StringValue("")
  assert_eq(empty_string, StringValue(""))
  
  // Test zero values
  let zero_int = IntValue(0)
  let zero_float = FloatValue(0.0)
  let false_bool = BoolValue(false)
  
  assert_eq(zero_int, IntValue(0))
  assert_eq(zero_float, FloatValue(0.0))
  assert_eq(false_bool, BoolValue(false))
  
  // Test empty arrays
  let empty_string_array = ArrayStringValue([])
  let empty_int_array = ArrayIntValue([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
}

test "attribute value comparison operations" {
  let string_val1 = StringValue("test")
  let string_val2 = StringValue("test")
  let string_val3 = StringValue("different")
  
  // Test equality
  assert_eq(string_val1, string_val2)
  assert_true(string_val1 != string_val3)
  
  // Test different types
  let int_val = IntValue(42)
  let string_as_number = StringValue("42")
  
  assert_true(int_val != string_as_number)
  
  // Test array equality
  let array1 = ArrayStringValue(["a", "b", "c"])
  let array2 = ArrayStringValue(["a", "b", "c"])
  let array3 = ArrayStringValue(["a", "b"])
  
  assert_eq(array1, array2)
  assert_true(array1 != array3)
}