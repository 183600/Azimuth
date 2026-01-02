// Azimuth AttributeValue Type Conversion Tests
// Test various type conversions and validations for AttributeValue enum

test "attribute_value_string_conversion" {
  let string_val = StringValue("test_string")
  match string_val {
    StringValue(s) => assert_eq!(s, "test_string")
    _ => @test.abort("Expected StringValue")
  }
  
  // Test empty string
  let empty_val = StringValue("")
  match empty_val {
    StringValue(s) => assert_eq!(s, "")
    _ => @test.abort("Expected StringValue for empty string")
  }
  
  // Test special characters
  let special_val = StringValue("特殊字符_!@#$%^&*()")
  match special_val {
    StringValue(s) => assert_eq!(s, "特殊字符_!@#$%^&*()")
    _ => @test.abort("Expected StringValue for special characters")
  }
}

test "attribute_value_int_conversion" {
  let int_val = IntValue(42)
  match int_val {
    IntValue(i) => assert_eq!(i, 42)
    _ => @test.abort("Expected IntValue")
  }
  
  // Test negative values
  let neg_val = IntValue(-100)
  match neg_val {
    IntValue(i) => assert_eq!(i, -100)
    _ => @test.abort("Expected IntValue for negative number")
  }
  
  // Test zero
  let zero_val = IntValue(0)
  match zero_val {
    IntValue(i) => assert_eq!(i, 0)
    _ => @test.abort("Expected IntValue for zero")
  }
}

test "attribute_value_float_conversion" {
  let float_val = FloatValue(3.14159)
  match float_val {
    FloatValue(f) => assert_eq!(f, 3.14159)
    _ => @test.abort("Expected FloatValue")
  }
  
  // Test negative float
  let neg_float = FloatValue(-2.71828)
  match neg_float {
    FloatValue(f) => assert_eq!(f, -2.71828)
    _ => @test.abort("Expected FloatValue for negative float")
  }
  
  // Test zero float
  let zero_float = FloatValue(0.0)
  match zero_float {
    FloatValue(f) => assert_eq!(f, 0.0)
    _ => @test.abort("Expected FloatValue for zero")
  }
}

test "attribute_value_bool_conversion" {
  let true_val = BoolValue(true)
  match true_val {
    BoolValue(b) => assert_eq!(b, true)
    _ => @test.abort("Expected BoolValue(true)")
  }
  
  let false_val = BoolValue(false)
  match false_val {
    BoolValue(b) => assert_eq!(b, false)
    _ => @test.abort("Expected BoolValue(false)")
  }
}

test "attribute_value_array_string_conversion" {
  let string_array = ["item1", "item2", "item3"]
  let array_val = ArrayStringValue(string_array)
  
  match array_val {
    ArrayStringValue(arr) => {
      assert_eq!(arr.length, 3)
      assert_eq!(arr[0], "item1")
      assert_eq!(arr[1], "item2")
      assert_eq!(arr[2], "item3")
    }
    _ => @test.abort("Expected ArrayStringValue")
  }
  
  // Test empty array
  let empty_array : Array[String] = []
  let empty_val = ArrayStringValue(empty_array)
  match empty_val {
    ArrayStringValue(arr) => assert_eq!(arr.length, 0)
    _ => @test.abort("Expected ArrayStringValue for empty array")
  }
}

test "attribute_value_array_int_conversion" {
  let int_array = [1, 2, 3, 4, 5]
  let array_val = ArrayIntValue(int_array)
  
  match array_val {
    ArrayIntValue(arr) => {
      assert_eq!(arr.length, 5)
      assert_eq!(arr[0], 1)
      assert_eq!(arr[4], 5)
    }
    _ => @test.abort("Expected ArrayIntValue")
  }
  
  // Test with negative numbers
  let mixed_array = [-1, 0, 1]
  let mixed_val = ArrayIntValue(mixed_array)
  match mixed_val {
    ArrayIntValue(arr) => {
      assert_eq!(arr.length, 3)
      assert_eq!(arr[0], -1)
      assert_eq!(arr[1], 0)
      assert_eq!(arr[2], 1)
    }
    _ => @test.abort("Expected ArrayIntValue for mixed values")
  }
}

test "attribute_value_type_safety" {
  // Test that pattern matching is type-safe
  let values = [
    StringValue("test"),
    IntValue(42),
    FloatValue(3.14),
    BoolValue(true),
    ArrayStringValue(["a", "b"]),
    ArrayIntValue([1, 2, 3])
  ]
  
  for (i, value) in values.enumerate() {
    match value {
      StringValue(s) => assert_eq!(s, "test")
      IntValue(i) => assert_eq!(i, 42)
      FloatValue(f) => @test.assert_eq!(f, 3.14, "Float value mismatch")
      BoolValue(b) => assert_eq!(b, true)
      ArrayStringValue(arr) => assert_eq!(arr.length, 2)
      ArrayIntValue(arr) => assert_eq!(arr.length, 3)
    }
  }
}

test "attribute_value_edge_cases" {
  // Test very large numbers
  let large_int = IntValue(2147483647)
  match large_int {
    IntValue(i) => assert_eq!(i, 2147483647)
    _ => @test.abort("Expected IntValue for large number")
  }
  
  // Test very small float
  let small_float = FloatValue(0.0000001)
  match small_float {
    FloatValue(f) => @test.assert_eq!(f, 0.0000001, "Small float value mismatch")
    _ => @test.abort("Expected FloatValue for small number")
  }
  
  // Test long string
  let long_string = "a".repeat(1000)
  let long_val = StringValue(long_string)
  match long_val {
    StringValue(s) => assert_eq!(s.length, 1000)
    _ => @test.abort("Expected StringValue for long string")
  }
}