// Common API 属性值类型转换测试
// 测试AttributeValue各种类型之间的转换和边界情况

test "attribute_value_string_conversions" {
  // 测试字符串类型的创建和验证
  let string_attr = @azimuth.telemetry.api.common.AttributeValue::string("test_string")
  
  match string_attr {
    @azimuth.telemetry.api.common.StringValue(value) => {
      assert_eq(value, "test_string")
    }
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试空字符串
  let empty_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("")
  match empty_string_attr {
    @azimuth.telemetry.api.common.StringValue(value) => {
      assert_eq(value, "")
    }
    _ => @test.fail("Expected empty StringValue")
  }
  
  // 测试长字符串
  let long_string = "a".repeat(1000)
  let long_string_attr = @azimuth.telemetry.api.common.AttributeValue::string(long_string)
  match long_string_attr {
    @azimuth.telemetry.api.common.StringValue(value) => {
      assert_eq(value.length(), 1000)
    }
    _ => @test.fail("Expected long StringValue")
  }
  
  // 测试特殊字符字符串
  let special_string = "Special: \n\t\r\"'\\{}[]()<>@#$%^&*()_+-=|/?.>,<~` 你好 🌍"
  let special_string_attr = @azimuth.telemetry.api.common.AttributeValue::string(special_string)
  match special_string_attr {
    @azimuth.telemetry.api.common.StringValue(value) => {
      assert_eq(value, special_string)
    }
    _ => @test.fail("Expected special character StringValue")
  }
}

test "attribute_value_int_conversions" {
  // 测试整数类型的边界值
  let max_int_attr = @azimuth.telemetry.api.common.AttributeValue::int(9223372036854775807L)  // Int64最大值
  let min_int_attr = @azimuth.telemetry.api.common.AttributeValue::int(-9223372036854775808L) // Int64最小值
  let zero_int_attr = @azimuth.telemetry.api.common.AttributeValue::int(0L)
  
  match max_int_attr {
    @azimuth.telemetry.api.common.IntValue(value) => {
      assert_eq(value, 9223372036854775807L)
    }
    _ => @test.fail("Expected max IntValue")
  }
  
  match min_int_attr {
    @azimuth.telemetry.api.common.IntValue(value) => {
      assert_eq(value, -9223372036854775808L)
    }
    _ => @test.fail("Expected min IntValue")
  }
  
  match zero_int_attr {
    @azimuth.telemetry.api.common.IntValue(value) => {
      assert_eq(value, 0L)
    }
    _ => @test.fail("Expected zero IntValue")
  }
  
  // 测试常用整数值
  let positive_int_attr = @azimuth.telemetry.api.common.AttributeValue::int(42L)
  let negative_int_attr = @azimuth.telemetry.api.common.AttributeValue::int(-42L)
  
  match positive_int_attr {
    @azimuth.telemetry.api.common.IntValue(value) => {
      assert_eq(value, 42L)
    }
    _ => @test.fail("Expected positive IntValue")
  }
  
  match negative_int_attr {
    @azimuth.telemetry.api.common.IntValue(value) => {
      assert_eq(value, -42L)
    }
    _ => @test.fail("Expected negative IntValue")
  }
}

test "attribute_value_float_conversions" {
  // 测试浮点数类型的边界值
  let max_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(1.7976931348623157e308)  // Double最大值
  let min_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(-1.7976931348623157e308) // Double最小值
  let zero_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(0.0)
  
  match max_float_attr {
    @azimuth.telemetry.api.common.FloatValue(value) => {
      assert_eq(value, 1.7976931348623157e308)
    }
    _ => @test.fail("Expected max FloatValue")
  }
  
  match min_float_attr {
    @azimuth.telemetry.api.common.FloatValue(value) => {
      assert_eq(value, -1.7976931348623157e308)
    }
    _ => @test.fail("Expected min FloatValue")
  }
  
  match zero_float_attr {
    @azimuth.telemetry.api.common.FloatValue(value) => {
      assert_eq(value, 0.0)
    }
    _ => @test.fail("Expected zero FloatValue")
  }
  
  // 测试特殊浮点值
  let infinity_attr = @azimuth.telemetry.api.common.AttributeValue::float(1.0/0.0)      // 正无穷
  let neg_infinity_attr = @azimuth.telemetry.api.common.AttributeValue::float(-1.0/0.0) // 负无穷
  let nan_attr = @azimuth.telemetry.api.common.AttributeValue::float(0.0/0.0)          // NaN
  
  match infinity_attr {
    @azimuth.telemetry.api.common.FloatValue(value) => {
      assert_eq(value, 1.0/0.0)
    }
    _ => @test.fail("Expected infinity FloatValue")
  }
  
  match neg_infinity_attr {
    @azimuth.telemetry.api.common.FloatValue(value) => {
      assert_eq(value, -1.0/0.0)
    }
    _ => @test.fail("Expected negative infinity FloatValue")
  }
  
  match nan_attr {
    @azimuth.telemetry.api.common.FloatValue(value) => {
      // NaN的比较需要特殊处理
      assert_eq(value.is_nan(), true)
    }
    _ => @test.fail("Expected NaN FloatValue")
  }
  
  // 测试科学计数法
  let scientific_attr = @azimuth.telemetry.api.common.AttributeValue::float(1.23e-4)
  match scientific_attr {
    @azimuth.telemetry.api.common.FloatValue(value) => {
      assert_eq(value, 1.23e-4)
    }
    _ => @test.fail("Expected scientific notation FloatValue")
  }
}

test "attribute_value_bool_conversions" {
  // 测试布尔值类型
  let true_attr = @azimuth.telemetry.api.common.AttributeValue::bool(true)
  let false_attr = @azimuth.telemetry.api.common.AttributeValue::bool(false)
  
  match true_attr {
    @azimuth.telemetry.api.common.BoolValue(value) => {
      assert_eq(value, true)
    }
    _ => @test.fail("Expected true BoolValue")
  }
  
  match false_attr {
    @azimuth.telemetry.api.common.BoolValue(value) => {
      assert_eq(value, false)
    }
    _ => @test.fail("Expected false BoolValue")
  }
}

test "attribute_value_array_string_conversions" {
  // 测试字符串数组类型
  let empty_string_array = @azimuth.telemetry.api.common.AttributeValue::array_string([])
  match empty_string_array {
    @azimuth.telemetry.api.common.ArrayStringValue(values) => {
      assert_eq(values.length(), 0)
    }
    _ => @test.fail("Expected empty ArrayStringValue")
  }
  
  let single_string_array = @azimuth.telemetry.api.common.AttributeValue::array_string(["single"])
  match single_string_array {
    @azimuth.telemetry.api.common.ArrayStringValue(values) => {
      assert_eq(values.length(), 1)
      assert_eq(values[0], "single")
    }
    _ => @test.fail("Expected single element ArrayStringValue")
  }
  
  let multi_string_array = @azimuth.telemetry.api.common.AttributeValue::array_string(["first", "second", "third"])
  match multi_string_array {
    @azimuth.telemetry.api.common.ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "first")
      assert_eq(values[1], "second")
      assert_eq(values[2], "third")
    }
    _ => @test.fail("Expected multi element ArrayStringValue")
  }
  
  // 测试包含特殊字符的字符串数组
  let special_string_array = @azimuth.telemetry.api.common.AttributeValue::array_string([
    "Special chars: \n\t\"",
    "Unicode: 你好 🌍",
    "Empty: ",
    "Numbers: 123"
  ])
  match special_string_array {
    @azimuth.telemetry.api.common.ArrayStringValue(values) => {
      assert_eq(values.length(), 4)
      assert_eq(values[0], "Special chars: \n\t\"")
      assert_eq(values[1], "Unicode: 你好 🌍")
      assert_eq(values[2], "Empty: ")
      assert_eq(values[3], "Numbers: 123")
    }
    _ => @test.fail("Expected special character ArrayStringValue")
  }
}

test "attribute_value_array_int_conversions" {
  // 测试整数数组类型
  let empty_int_array = @azimuth.telemetry.api.common.AttributeValue::array_int([])
  match empty_int_array {
    @azimuth.telemetry.api.common.ArrayIntValue(values) => {
      assert_eq(values.length(), 0)
    }
    _ => @test.fail("Expected empty ArrayIntValue")
  }
  
  let boundary_int_array = @azimuth.telemetry.api.common.AttributeValue::array_int([
    -9223372036854775808L,  // Int64最小值
    0L,                      // 零
    9223372036854775807L     // Int64最大值
  ])
  match boundary_int_array {
    @azimuth.telemetry.api.common.ArrayIntValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], -9223372036854775808L)
      assert_eq(values[1], 0L)
      assert_eq(values[2], 9223372036854775807L)
    }
    _ => @test.fail("Expected boundary ArrayIntValue")
  }
  
  let sequence_int_array = @azimuth.telemetry.api.common.AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])
  match sequence_int_array {
    @azimuth.telemetry.api.common.ArrayIntValue(values) => {
      assert_eq(values.length(), 5)
      let mut i = 0
      while i < values.length() {
        assert_eq(values[i], (i + 1).to_int64())
        i = i + 1
      }
    }
    _ => @test.fail("Expected sequence ArrayIntValue")
  }
}

test "attribute_value_array_float_conversions" {
  // 测试浮点数数组类型
  let empty_float_array = @azimuth.telemetry.api.common.AttributeValue::array_float([])
  match empty_float_array {
    @azimuth.telemetry.api.common.ArrayFloatValue(values) => {
      assert_eq(values.length(), 0)
    }
    _ => @test.fail("Expected empty ArrayFloatValue")
  }
  
  let special_float_array = @azimuth.telemetry.api.common.AttributeValue::array_float([
    0.0,                    // 零
    1.0/0.0,               // 正无穷
    -1.0/0.0,              // 负无穷
    0.0/0.0,               // NaN
    3.14159265359,         // π
    -2.71828182846,        // -e
    1.23e-10,              // 科学计数法小数
    9.99e20                // 科学计数法大数
  ])
  match special_float_array {
    @azimuth.telemetry.api.common.ArrayFloatValue(values) => {
      assert_eq(values.length(), 8)
      assert_eq(values[0], 0.0)
      assert_eq(values[1], 1.0/0.0)
      assert_eq(values[2], -1.0/0.0)
      assert_eq(values[3].is_nan(), true)
      assert_eq(values[4], 3.14159265359)
      assert_eq(values[5], -2.71828182846)
      assert_eq(values[6], 1.23e-10)
      assert_eq(values[7], 9.99e20)
    }
    _ => @test.fail("Expected special ArrayFloatValue")
  }
}

test "attribute_value_array_bool_conversions" {
  // 测试布尔数组类型
  let empty_bool_array = @azimuth.telemetry.api.common.AttributeValue::array_bool([])
  match empty_bool_array {
    @azimuth.telemetry.api.common.ArrayBoolValue(values) => {
      assert_eq(values.length(), 0)
    }
    _ => @test.fail("Expected empty ArrayBoolValue")
  }
  
  let pattern_bool_array = @azimuth.telemetry.api.common.AttributeValue::array_bool([
    true, false, true, false, true
  ])
  match pattern_bool_array {
    @azimuth.telemetry.api.common.ArrayBoolValue(values) => {
      assert_eq(values.length(), 5)
      let mut i = 0
      while i < values.length() {
        assert_eq(values[i], i % 2 == 0)
        i = i + 1
      }
    }
    _ => @test.fail("Expected pattern ArrayBoolValue")
  }
  
  let all_true_array = @azimuth.telemetry.api.common.AttributeValue::array_bool([true, true, true, true])
  match all_true_array {
    @azimuth.telemetry.api.common.ArrayBoolValue(values) => {
      assert_eq(values.length(), 4)
      let mut i = 0
      while i < values.length() {
        assert_eq(values[i], true)
        i = i + 1
      }
    }
    _ => @test.fail("Expected all true ArrayBoolValue")
  }
  
  let all_false_array = @azimuth.telemetry.api.common.AttributeValue::array_bool([false, false, false])
  match all_false_array {
    @azimuth.telemetry.api.common.ArrayBoolValue(values) => {
      assert_eq(values.length(), 3)
      let mut i = 0
      while i < values.length() {
        assert_eq(values[i], false)
        i = i + 1
      }
    }
    _ => @test.fail("Expected all false ArrayBoolValue")
  }
}

test "attribute_value_mixed_type_attributes" {
  // 测试混合类型的属性集合
  let mixed_attributes = [
    ("string_field", @azimuth.telemetry.api.common.AttributeValue::string("string_value")),
    ("int_field", @azimuth.telemetry.api.common.AttributeValue::int(42L)),
    ("float_field", @azimuth.telemetry.api.common.AttributeValue::float(3.14)),
    ("bool_field", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
    ("string_array_field", @azimuth.telemetry.api.common.AttributeValue::array_string(["a", "b", "c"])),
    ("int_array_field", @azimuth.telemetry.api.common.AttributeValue::array_int([1L, 2L, 3L])),
    ("float_array_field", @azimuth.telemetry.api.common.AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool_array_field", @azimuth.telemetry.api.common.AttributeValue::array_bool([true, false, true]))
  ]
  
  assert_eq(mixed_attributes.length(), 8)
  
  // 验证每种类型
  let mut string_found = false
  let mut int_found = false
  let mut float_found = false
  let mut bool_found = false
  let mut string_array_found = false
  let mut int_array_found = false
  let mut float_array_found = false
  let mut bool_array_found = false
  
  let mut i = 0
  while i < mixed_attributes.length() {
    match mixed_attributes[i] {
      (key, @azimuth.telemetry.api.common.StringValue(_)) => {
        if key == "string_field" { string_found = true }
      }
      (key, @azimuth.telemetry.api.common.IntValue(_)) => {
        if key == "int_field" { int_found = true }
      }
      (key, @azimuth.telemetry.api.common.FloatValue(_)) => {
        if key == "float_field" { float_found = true }
      }
      (key, @azimuth.telemetry.api.common.BoolValue(_)) => {
        if key == "bool_field" { bool_found = true }
      }
      (key, @azimuth.telemetry.api.common.ArrayStringValue(_)) => {
        if key == "string_array_field" { string_array_found = true }
      }
      (key, @azimuth.telemetry.api.common.ArrayIntValue(_)) => {
        if key == "int_array_field" { int_array_found = true }
      }
      (key, @azimuth.telemetry.api.common.ArrayFloatValue(_)) => {
        if key == "float_array_field" { float_array_found = true }
      }
      (key, @azimuth.telemetry.api.common.ArrayBoolValue(_)) => {
        if key == "bool_array_field" { bool_array_found = true }
      }
      _ => {}
    }
    i = i + 1
  }
  
  assert(string_found, "String attribute not found")
  assert(int_found, "Int attribute not found")
  assert(float_found, "Float attribute not found")
  assert(bool_found, "Bool attribute not found")
  assert(string_array_found, "String array attribute not found")
  assert(int_array_found, "Int array attribute not found")
  assert(float_array_found, "Float array attribute not found")
  assert(bool_array_found, "Bool array attribute not found")
}

test "attribute_value_large_arrays" {
  // 测试大型数组
  let large_string_array = @azimuth.telemetry.api.common.AttributeValue::array_string(
    [for i = 0; i < 1000; i = i + 1].map(fn(i) { "item_" + i.to_string() })
  )
  match large_string_array {
    @azimuth.telemetry.api.common.ArrayStringValue(values) => {
      assert_eq(values.length(), 1000)
      assert_eq(values[0], "item_0")
      assert_eq(values[999], "item_999")
    }
    _ => @test.fail("Expected large ArrayStringValue")
  }
  
  let large_int_array = @azimuth.telemetry.api.common.AttributeValue::array_int(
    [for i = 0; i < 1000; i = i + 1].map(fn(i) { i.to_int64() })
  )
  match large_int_array {
    @azimuth.telemetry.api.common.ArrayIntValue(values) => {
      assert_eq(values.length(), 1000)
      assert_eq(values[0], 0L)
      assert_eq(values[999], 999L)
    }
    _ => @test.fail("Expected large ArrayIntValue")
  }
  
  let large_float_array = @azimuth.telemetry.api.common.AttributeValue::array_float(
    [for i = 0; i < 1000; i = i + 1].map(fn(i) { i.to_float() * 0.1 })
  )
  match large_float_array {
    @azimuth.telemetry.api.common.ArrayFloatValue(values) => {
      assert_eq(values.length(), 1000)
      assert_eq(values[0], 0.0)
      assert_eq(values[999], 99.9)
    }
    _ => @test.fail("Expected large ArrayFloatValue")
  }
  
  let large_bool_array = @azimuth.telemetry.api.common.AttributeValue::array_bool(
    [for i = 0; i < 1000; i = i + 1].map(fn(i) { i % 2 == 0 })
  )
  match large_bool_array {
    @azimuth.telemetry.api.common.ArrayBoolValue(values) => {
      assert_eq(values.length(), 1000)
      assert_eq(values[0], true)
      assert_eq(values[999], false)
    }
    _ => @test.fail("Expected large ArrayBoolValue")
  }
}