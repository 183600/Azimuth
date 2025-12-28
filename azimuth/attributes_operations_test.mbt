// Azimuth Telemetry System - Attributes Operations Test
// Comprehensive test suite for Attributes functionality

test "attributes_creation_and_initialization" {
  // Test empty attributes creation
  let empty_attrs = Attributes::new()
  @assertion.assert_eq(empty_attrs.values.length, 0)?
  
  // Test that empty attributes return None for any key
  let non_existent = Attributes::get(empty_attrs, "non.existent.key")
  @assertion.assert_eq(non_existent, None)?
}

test "attributes_string_value_operations" {
  let attrs = Attributes::new()
  
  // Test setting string value
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  
  // Test getting string value
  let retrieved = Attributes::get(attrs, "string.key")
  match retrieved {
    Some(StringValue(value)) => @assertion.assert_eq(value, "test_value")?
    _ => @test.fail("Expected StringValue")?
  }
  
  // Test empty string value
  Attributes::set(attrs, "empty.string", StringValue(""))
  let empty_retrieved = Attributes::get(attrs, "empty.string")
  match empty_retrieved {
    Some(StringValue(value)) => @assertion.assert_eq(value, "")?
    _ => @test.fail("Expected empty StringValue")?
  }
  
  // Test unicode string value
  Attributes::set(attrs, "unicode.string", StringValue("测试字符串"))
  let unicode_retrieved = Attributes::get(attrs, "unicode.string")
  match unicode_retrieved {
    Some(StringValue(value)) => @assertion.assert_eq(value, "测试字符串")?
    _ => @test.fail("Expected unicode StringValue")?
  }
}

test "attributes_numeric_value_operations" {
  let attrs = Attributes::new()
  
  // Test integer value
  Attributes::set(attrs, "int.key", IntValue(42))
  let int_retrieved = Attributes::get(attrs, "int.key")
  match int_retrieved {
    Some(IntValue(value)) => @assertion.assert_eq(value, 42)?
    _ => @test.fail("Expected IntValue")?
  }
  
  // Test negative integer value
  Attributes::set(attrs, "negative.int", IntValue(-100))
  let negative_retrieved = Attributes::get(attrs, "negative.int")
  match negative_retrieved {
    Some(IntValue(value)) => @assertion.assert_eq(value, -100)?
    _ => @test.fail("Expected negative IntValue")?
  }
  
  // Test zero integer value
  Attributes::set(attrs, "zero.int", IntValue(0))
  let zero_retrieved = Attributes::get(attrs, "zero.int")
  match zero_retrieved {
    Some(IntValue(value)) => @assertion.assert_eq(value, 0)?
    _ => @test.fail("Expected zero IntValue")?
  }
  
  // Test float value
  Attributes::set(attrs, "float.key", FloatValue(3.14159))
  let float_retrieved = Attributes::get(attrs, "float.key")
  match float_retrieved {
    Some(FloatValue(value)) => @assertion.assert_eq(value, 3.14159)?
    _ => @test.fail("Expected FloatValue")?
  }
  
  // Test negative float value
  Attributes::set(attrs, "negative.float", FloatValue(-2.71828))
  let negative_float_retrieved = Attributes::get(attrs, "negative.float")
  match negative_float_retrieved {
    Some(FloatValue(value)) => @assertion.assert_eq(value, -2.71828)?
    _ => @test.fail("Expected negative FloatValue")?
  }
}

test "attributes_boolean_value_operations" {
  let attrs = Attributes::new()
  
  // Test true value
  Attributes::set(attrs, "true.key", BoolValue(true))
  let true_retrieved = Attributes::get(attrs, "true.key")
  match true_retrieved {
    Some(BoolValue(value)) => @assertion.assert_eq(value, true)?
    _ => @test.fail("Expected BoolValue(true)")?
  }
  
  // Test false value
  Attributes::set(attrs, "false.key", BoolValue(false))
  let false_retrieved = Attributes::get(attrs, "false.key")
  match false_retrieved {
    Some(BoolValue(value)) => @assertion.assert_eq(value, false)?
    _ => @test.fail("Expected BoolValue(false)")?
  }
}

test "attributes_array_value_operations" {
  let attrs = Attributes::new()
  
  // Test string array
  Attributes::set(attrs, "string.array", ArrayStringValue(["item1", "item2", "item3"]))
  let string_array_retrieved = Attributes::get(attrs, "string.array")
  match string_array_retrieved {
    Some(ArrayStringValue(values)) => {
      @assertion.assert_eq(values.length, 3)?
      @assertion.assert_eq(values[0], "item1")?
      @assertion.assert_eq(values[1], "item2")?
      @assertion.assert_eq(values[2], "item3")?
    }
    _ => @test.fail("Expected ArrayStringValue")?
  }
  
  // Test empty string array
  Attributes::set(attrs, "empty.string.array", ArrayStringValue([]))
  let empty_string_array_retrieved = Attributes::get(attrs, "empty.string.array")
  match empty_string_array_retrieved {
    Some(ArrayStringValue(values)) => @assertion.assert_eq(values.length, 0)?
    _ => @test.fail("Expected empty ArrayStringValue")?
  }
  
  // Test int array
  Attributes::set(attrs, "int.array", ArrayIntValue([1, 2, 3, 4, 5]))
  let int_array_retrieved = Attributes::get(attrs, "int.array")
  match int_array_retrieved {
    Some(ArrayIntValue(values)) => {
      @assertion.assert_eq(values.length, 5)?
      @assertion.assert_eq(values[0], 1)?
      @assertion.assert_eq(values[1], 2)?
      @assertion.assert_eq(values[2], 3)?
      @assertion.assert_eq(values[3], 4)?
      @assertion.assert_eq(values[4], 5)?
    }
    _ => @test.fail("Expected ArrayIntValue")?
  }
  
  // Test empty int array
  Attributes::set(attrs, "empty.int.array", ArrayIntValue([]))
  let empty_int_array_retrieved = Attributes::get(attrs, "empty.int.array")
  match empty_int_array_retrieved {
    Some(ArrayIntValue(values)) => @assertion.assert_eq(values.length, 0)?
    _ => @test.fail("Expected empty ArrayIntValue")?
  }
}

test "attributes_key_naming_conventions" {
  let attrs = Attributes::new()
  
  // Test dot notation keys
  Attributes::set(attrs, "service.name", StringValue("test-service"))
  let dot_key = Attributes::get(attrs, "service.name")
  match dot_key {
    Some(StringValue(value)) => @assertion.assert_eq(value, "test-service")?
    _ => @test.fail("Expected value for dot notation key")?
  }
  
  // Test underscore keys
  Attributes::set(attrs, "user_id", StringValue("12345"))
  let underscore_key = Attributes::get(attrs, "user_id")
  match underscore_key {
    Some(StringValue(value)) => @assertion.assert_eq(value, "12345")?
    _ => @test.fail("Expected value for underscore key")?
  }
  
  // Test hyphen keys
  Attributes::set(attrs, "http-status-code", IntValue(200))
  let hyphen_key = Attributes::get(attrs, "http-status-code")
  match hyphen_key {
    Some(IntValue(value)) => @assertion.assert_eq(value, 200)?
    _ => @test.fail("Expected value for hyphen key")?
  }
  
  // Test numeric key suffix
  Attributes::set(attrs, "retry.count.3", IntValue(3))
  let numeric_suffix = Attributes::get(attrs, "retry.count.3")
  match numeric_suffix {
    Some(IntValue(value)) => @assertion.assert_eq(value, 3)?
    _ => @test.fail("Expected value for numeric suffix key")?
  }
}

test "attributes_value_overwrite_behavior" {
  let attrs = Attributes::new()
  
  // Set initial value
  Attributes::set(attrs, "overwrite.test", StringValue("initial"))
  let initial = Attributes::get(attrs, "overwrite.test")
  match initial {
    Some(StringValue(value)) => @assertion.assert_eq(value, "initial")?
    _ => @test.fail("Expected initial value")?
  }
  
  // Overwrite with same type
  Attributes::set(attrs, "overwrite.test", StringValue("updated"))
  let updated = Attributes::get(attrs, "overwrite.test")
  match updated {
    Some(StringValue(value)) => @assertion.assert_eq(value, "updated")?
    _ => @test.fail("Expected updated value")?
  }
  
  // Overwrite with different type
  Attributes::set(attrs, "overwrite.test", IntValue(42))
  let type_changed = Attributes::get(attrs, "overwrite.test")
  match type_changed {
    Some(IntValue(value)) => @assertion.assert_eq(value, 42)?
    _ => @test.fail("Expected type-changed value")?
  }
}

test "attributes_case_sensitivity" {
  let attrs = Attributes::new()
  
  // Set lowercase key
  Attributes::set(attrs, "case.test", StringValue("lowercase"))
  
  // Test case-sensitive retrieval
  let lowercase = Attributes::get(attrs, "case.test")
  match lowercase {
    Some(StringValue(value)) => @assertion.assert_eq(value, "lowercase")?
    _ => @test.fail("Expected lowercase key value")?
  }
  
  // Test uppercase key (should be different)
  let uppercase = Attributes::get(attrs, "CASE.TEST")
  @assertion.assert_eq(uppercase, None)?
  
  // Test mixed case key (should be different)
  let mixed_case = Attributes::get(attrs, "Case.Test")
  @assertion.assert_eq(mixed_case, None)?
}

test "attributes_special_characters_in_keys" {
  let attrs = Attributes::new()
  
  // Test keys with special characters
  Attributes::set(attrs, "special.key.with.dots", StringValue("dots"))
  let dots_key = Attributes::get(attrs, "special.key.with.dots")
  match dots_key {
    Some(StringValue(value)) => @assertion.assert_eq(value, "dots")?
    _ => @test.fail("Expected value for key with dots")?
  }
  
  Attributes::set(attrs, "special-key-with-hyphens", StringValue("hyphens"))
  let hyphens_key = Attributes::get(attrs, "special-key-with-hyphens")
  match hyphens_key {
    Some(StringValue(value)) => @assertion.assert_eq(value, "hyphens")?
    _ => @test.fail("Expected value for key with hyphens")?
  }
  
  Attributes::set(attrs, "special_key_with_underscores", StringValue("underscores"))
  let underscores_key = Attributes::get(attrs, "special_key_with_underscores")
  match underscores_key {
    Some(StringValue(value)) => @assertion.assert_eq(value, "underscores")?
    _ => @test.fail("Expected value for key with underscores")?
  }
}

test "attributes_large_value_handling" {
  let attrs = Attributes::new()
  
  // Test large string value
  let large_string = "This is a very long string that contains a lot of characters and should test the system's ability to handle large attribute values without any issues or performance degradation."
  Attributes::set(attrs, "large.string", StringValue(large_string))
  let large_retrieved = Attributes::get(attrs, "large.string")
  match large_retrieved {
    Some(StringValue(value)) => @assertion.assert_eq(value, large_string)?
    _ => @test.fail("Expected large string value")?
  }
  
  // Test large array
  let large_array = [for i in range(0, 100) { "item" + i.to_string() }]
  Attributes::set(attrs, "large.array", ArrayStringValue(large_array))
  let large_array_retrieved = Attributes::get(attrs, "large.array")
  match large_array_retrieved {
    Some(ArrayStringValue(values)) => {
      @assertion.assert_eq(values.length, 100)?
      @assertion.assert_eq(values[0], "item0")?
      @assertion.assert_eq(values[99], "item99")?
    }
    _ => @test.fail("Expected large array value")?
  }
}