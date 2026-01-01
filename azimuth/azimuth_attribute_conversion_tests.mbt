// Attribute Value Type Conversion Tests for Azimuth System
// This file contains test cases for attribute value type conversion and validation

test "string attribute value operations" {
  let string_value = StringValue("test_string")
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "string.key", string_value)
  let retrieved = Attributes::get(attrs, "string.key")
  
  match retrieved {
    Some(StringValue(value)) => assert_eq(value, "test_value")
    _ => assert_true(false)
  }
}

test "int attribute value operations" {
  let int_value = IntValue(42)
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "int.key", int_value)
  let retrieved = Attributes::get(attrs, "int.key")
  
  match retrieved {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
}

test "float attribute value operations" {
  let float_value = FloatValue(3.14159)
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "float.key", float_value)
  let retrieved = Attributes::get(attrs, "float.key")
  
  match retrieved {
    Some(FloatValue(value)) => assert_eq(value, 3.14159)
    _ => assert_true(false)
  }
}

test "bool attribute value operations" {
  let bool_value = BoolValue(true)
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "bool.key", bool_value)
  let retrieved = Attributes::get(attrs, "bool.key")
  
  match retrieved {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
}

test "array string attribute value operations" {
  let string_array = ArrayStringValue(["item1", "item2", "item3"])
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "array.string.key", string_array)
  let retrieved = Attributes::get(attrs, "array.string.key")
  
  match retrieved {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length, 3)
      assert_eq(values[0], "item1")
      assert_eq(values[1], "item2")
      assert_eq(values[2], "item3")
    }
    _ => assert_true(false)
  }
}

test "array int attribute value operations" {
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "array.int.key", int_array)
  let retrieved = Attributes::get(attrs, "array.int.key")
  
  match retrieved {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length, 5)
      assert_eq(values[0], 1)
      assert_eq(values[4], 5)
    }
    _ => assert_true(false)
  }
}

test "mixed attribute type operations" {
  let attrs = Attributes::new()
  
  // Set different types of attributes
  Attributes::set(attrs, "service.name", StringValue("test-service"))
  Attributes::set(attrs, "service.port", IntValue(8080))
  Attributes::set(attrs, "service.uptime", FloatValue(123.45))
  Attributes::set(attrs, "service.active", BoolValue(true))
  Attributes::set(attrs, "service.tags", ArrayStringValue(["web", "api"]))
  Attributes::set(attrs, "service.metrics", ArrayIntValue([100, 200, 300]))
  
  // Retrieve and verify each type
  let name = Attributes::get(attrs, "service.name")
  let port = Attributes::get(attrs, "service.port")
  let uptime = Attributes::get(attrs, "service.uptime")
  let active = Attributes::get(attrs, "service.active")
  let tags = Attributes::get(attrs, "service.tags")
  let metrics = Attributes::get(attrs, "service.metrics")
  
  match name {
    Some(StringValue(value)) => assert_eq(value, "test_value")
    _ => assert_true(false)
  }
  
  match port {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  match uptime {
    Some(FloatValue(value)) => assert_eq(value, 123.45)
    _ => assert_true(false)
  }
  
  match active {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
  
  match tags {
    Some(ArrayStringValue(values)) => assert_eq(values.length, 3)
    _ => assert_true(false)
  }
  
  match metrics {
    Some(ArrayIntValue(values)) => assert_eq(values.length, 3)
    _ => assert_true(false)
  }
}

test "attribute value edge cases" {
  let attrs = Attributes::new()
  
  // Test empty string
  Attributes::set(attrs, "empty.string", StringValue(""))
  
  // Test zero values
  Attributes::set(attrs, "zero.int", IntValue(0))
  Attributes::set(attrs, "zero.float", FloatValue(0.0))
  
  // Test false boolean
  Attributes::set(attrs, "false.bool", BoolValue(false))
  
  // Test empty arrays
  Attributes::set(attrs, "empty.string.array", ArrayStringValue([]))
  Attributes::set(attrs, "empty.int.array", ArrayIntValue([]))
  
  // Verify all edge cases can be retrieved
  let empty_string = Attributes::get(attrs, "empty.string")
  let zero_int = Attributes::get(attrs, "zero.int")
  let zero_float = Attributes::get(attrs, "zero.float")
  let false_bool = Attributes::get(attrs, "false.bool")
  let empty_string_array = Attributes::get(attrs, "empty.string.array")
  let empty_int_array = Attributes::get(attrs, "empty.int.array")
  
  assert_true(empty_string != None)
  assert_true(zero_int != None)
  assert_true(zero_float != None)
  assert_true(false_bool != None)
  assert_true(empty_string_array != None)
  assert_true(empty_int_array != None)
}

test "attribute value special characters" {
  let attrs = Attributes::new()
  
  // Test strings with special characters
  Attributes::set(attrs, "special.chars", StringValue("Hello, 世界! @#$%^&*()"))
  Attributes::set(attrs, "unicode.chars", StringValue("测试🚀emoji"))
  Attributes::set(attrs, "json.string", StringValue("{\"key\": \"value\", \"array\": [1, 2, 3]}"))
  
  // Test large integers and floats
  Attributes::set(attrs, "large.int", IntValue(9223372036854775807))
  Attributes::set(attrs, "small.float", FloatValue(0.0000000001))
  
  // Verify special characters are preserved
  let special_chars = Attributes::get(attrs, "special.chars")
  let unicode_chars = Attributes::get(attrs, "unicode.chars")
  let json_string = Attributes::get(attrs, "json.string")
  let large_int = Attributes::get(attrs, "large.int")
  let small_float = Attributes::get(attrs, "small.float")
  
  assert_true(special_chars != None)
  assert_true(unicode_chars != None)
  assert_true(json_string != None)
  assert_true(large_int != None)
  assert_true(small_float != None)
}