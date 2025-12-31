// AttributeValue类型转换边界测试
// 测试AttributeValue在各种边界情况下的类型转换和行为

test "attribute_value_string_boundary_cases" {
  // 测试字符串类型的边界情况
  
  // 空字符串
  let empty_string = AttributeValue::string("")
  match empty_string {
    StringValue(value) => assert_eq(value, "")
    _ => @test.fail("Test failed")
  }
  
  // 单字符字符串
  let single_char = AttributeValue::string("a")
  match single_char {
    StringValue(value) => assert_eq(value, "a")
    _ => @test.fail("Test failed")
  }
  
  // 长字符串
  let long_string = AttributeValue::string("This is a very long string that contains many characters and tests the system's ability to handle lengthy attribute values without any issues or performance degradation")
  match long_string {
    StringValue(value) => assert_eq(value.length() > 100, true)
    _ => @test.fail("Test failed")
  }
  
  // 包含特殊字符的字符串
  let special_chars = AttributeValue::string("!@#$%^&*()_+-=[]{}|;':\",./<>?~`")
  match special_chars {
    StringValue(value) => assert_eq(value, "!@#$%^&*()_+-=[]{}|;':\",./<>?~`")
    _ => @test.fail("Test failed")
  }
  
  // 包含换行符的字符串
  let multiline = AttributeValue::string("Line 1\nLine 2\nLine 3")
  match multiline {
    StringValue(value) => assert_eq(value, "Line 1\nLine 2\nLine 3")
    _ => @test.fail("Test failed")
  }
  
  // Unicode字符串
  let unicode = AttributeValue::string("Hello 世界 🌍 ñáéíóú 中文 العربية русский")
  match unicode {
    StringValue(value) => assert_eq(value, "Hello 世界 🌍 ñáéíóú 中文 العربية русский")
    _ => @test.fail("Test failed")
  }
  
  // 只包含空格的字符串
  let spaces = AttributeValue::string("   ")
  match spaces {
    StringValue(value) => assert_eq(value, "   ")
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_int_boundary_cases" {
  // 测试整数类型的边界情况
  
  // 零
  let zero = AttributeValue::int(0L)
  match zero {
    IntValue(value) => assert_eq(value, 0L)
    _ => @test.fail("Test failed")
  }
  
  // 最小值
  let min_int = AttributeValue::int(-9223372036854775808L)
  match min_int {
    IntValue(value) => assert_eq(value, -9223372036854775808L)
    _ => @test.fail("Test failed")
  }
  
  // 最大值
  let max_int = AttributeValue::int(9223372036854775807L)
  match max_int {
    IntValue(value) => assert_eq(value, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  
  // 负数
  let negative = AttributeValue::int(-42L)
  match negative {
    IntValue(value) => assert_eq(value, -42L)
    _ => @test.fail("Test failed")
  }
  
  // 大正数
  let large_positive = AttributeValue::int(1000000000000L)
  match large_positive {
    IntValue(value) => assert_eq(value, 1000000000000L)
    _ => @test.fail("Test failed")
  }
  
  // 大负数
  let large_negative = AttributeValue::int(-1000000000000L)
  match large_negative {
    IntValue(value) => assert_eq(value, -1000000000000L)
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_float_boundary_cases" {
  // 测试浮点数类型的边界情况
  
  // 零
  let zero_float = AttributeValue::float(0.0)
  match zero_float {
    FloatValue(value) => assert_eq(value, 0.0)
    _ => @test.fail("Test failed")
  }
  
  // 负零
  let negative_zero = AttributeValue::float(-0.0)
  match negative_zero {
    FloatValue(value) => assert_eq(value, -0.0)
    _ => @test.fail("Test failed")
  }
  
  // 非常小的正数
  let tiny_positive = AttributeValue::float(1.0e-10)
  match tiny_positive {
    FloatValue(value) => assert_eq(value, 1.0e-10)
    _ => @test.fail("Test failed")
  }
  
  // 非常小的负数
  let tiny_negative = AttributeValue::float(-1.0e-10)
  match tiny_negative {
    FloatValue(value) => assert_eq(value, -1.0e-10)
    _ => @test.fail("Test failed")
  }
  
  // 大正数
  let large_float = AttributeValue::float(1.0e10)
  match large_float {
    FloatValue(value) => assert_eq(value, 1.0e10)
    _ => @test.fail("Test failed")
  }
  
  // 大负数
  let large_negative_float = AttributeValue::float(-1.0e10)
  match large_negative_float {
    FloatValue(value) => assert_eq(value, -1.0e10)
    _ => @test.fail("Test failed")
  }
  
  // 精确小数
  let precise = AttributeValue::float(3.14159265359)
  match precise {
    FloatValue(value) => assert_eq(value, 3.14159265359)
    _ => @test.fail("Test failed")
  }
  
  // 科学计数法
  let scientific = AttributeValue::float(1.23e-5)
  match scientific {
    FloatValue(value) => assert_eq(value, 1.23e-5)
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_bool_boundary_cases" {
  // 测试布尔类型的边界情况
  
  // true
  let true_value = AttributeValue::bool(true)
  match true_value {
    BoolValue(value) => assert_eq(value, true)
    _ => @test.fail("Test failed")
  }
  
  // false
  let false_value = AttributeValue::bool(false)
  match false_value {
    BoolValue(value) => assert_eq(value, false)
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_array_string_boundary_cases" {
  // 测试字符串数组的边界情况
  
  // 空数组
  let empty_array = AttributeValue::array_string([])
  match empty_array {
    ArrayStringValue(values) => assert_eq(values.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  // 单元素数组
  let single_element = AttributeValue::array_string(["single"])
  match single_element {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 1)
      assert_eq(values[0], "single")
    }
    _ => @test.fail("Test failed")
  }
  
  // 包含空字符串的数组
  let with_empty = AttributeValue::array_string(["", "non-empty", ""])
  match with_empty {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "")
      assert_eq(values[1], "non-empty")
      assert_eq(values[2], "")
    }
    _ => @test.fail("Test failed")
  }
  
  // 包含特殊字符的数组
  let special_chars_array = AttributeValue::array_string(["!@#$%", "^&*()", "[]{}"])
  match special_chars_array {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "!@#$%")
      assert_eq(values[1], "^&*()")
      assert_eq(values[2], "[]{}")
    }
    _ => @test.fail("Test failed")
  }
  
  // 大数组
  let mut large_array = []
  let mut i = 0
  while i < 100 {
    large_array.push("item_" + i.to_string())
    i = i + 1
  }
  let large = AttributeValue::array_string(large_array)
  match large {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 100)
      assert_eq(values[0], "item_0")
      assert_eq(values[99], "item_99")
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_array_int_boundary_cases" {
  // 测试整数数组的边界情况
  
  // 空数组
  let empty_int_array = AttributeValue::array_int([])
  match empty_int_array {
    ArrayIntValue(values) => assert_eq(values.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  // 包含边界值的数组
  let boundary_ints = AttributeValue::array_int([
    -9223372036854775808L,  // 最小值
    0L,                       // 零
    9223372036854775807L     // 最大值
  ])
  match boundary_ints {
    ArrayIntValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], -9223372036854775808L)
      assert_eq(values[1], 0L)
      assert_eq(values[2], 9223372036854775807L)
    }
    _ => @test.fail("Test failed")
  }
  
  // 包含正负数的数组
  let mixed_ints = AttributeValue::array_int([-100L, -50L, 0L, 50L, 100L])
  match mixed_ints {
    ArrayIntValue(values) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], -100L)
      assert_eq(values[2], 0L)
      assert_eq(values[4], 100L)
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_array_float_boundary_cases" {
  // 测试浮点数数组的边界情况
  
  // 包含特殊浮点值的数组
  let special_floats = AttributeValue::array_float([
    0.0,        // 零
    -0.0,       // 负零
    1.0e-10,    // 非常小的正数
    -1.0e-10,   // 非常小的负数
    1.0e10,     // 大正数
    -1.0e10,    // 大负数
    3.14159265359  // 圆周率
  ])
  match special_floats {
    ArrayFloatValue(values) => {
      assert_eq(values.length(), 7)
      assert_eq(values[0], 0.0)
      assert_eq(values[1], -0.0)
      assert_eq(values[2], 1.0e-10)
      assert_eq(values[6], 3.14159265359)
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_array_bool_boundary_cases" {
  // 测试布尔数组的边界情况
  
  // 空数组
  let empty_bool_array = AttributeValue::array_bool([])
  match empty_bool_array {
    ArrayBoolValue(values) => assert_eq(values.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  // 单元素数组
  let single_true = AttributeValue::array_bool([true])
  match single_true {
    ArrayBoolValue(values) => {
      assert_eq(values.length(), 1)
      assert_eq(values[0], true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 单元素false数组
  let single_false = AttributeValue::array_bool([false])
  match single_false {
    ArrayBoolValue(values) => {
      assert_eq(values.length(), 1)
      assert_eq(values[0], false)
    }
    _ => @test.fail("Test failed")
  }
  
  // 混合布尔值数组
  let mixed_bools = AttributeValue::array_bool([true, false, true, true, false])
  match mixed_bools {
    ArrayBoolValue(values) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], true)
      assert_eq(values[1], false)
      assert_eq(values[2], true)
      assert_eq(values[3], true)
      assert_eq(values[4], false)
    }
    _ => @test.fail("Test failed")
  }
  
  // 全true数组
  let all_true = AttributeValue::array_bool([true, true, true, true, true])
  match all_true {
    ArrayBoolValue(values) => {
      assert_eq(values.length(), 5)
      let mut i = 0
      while i < values.length() {
        assert_eq(values[i], true)
        i = i + 1
      }
    }
    _ => @test.fail("Test failed")
  }
  
  // 全false数组
  let all_false = AttributeValue::array_bool([false, false, false, false, false])
  match all_false {
    ArrayBoolValue(values) => {
      assert_eq(values.length(), 5)
      let mut i = 0
      while i < values.length() {
        assert_eq(values[i], false)
        i = i + 1
      }
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_complex_attribute_collections" {
  // 测试复杂属性集合
  
  // 创建包含各种类型的属性集合
  let complex_attributes = [
    ("string.simple", AttributeValue::string("simple_value")),
    ("string.empty", AttributeValue::string("")),
    ("string.special", AttributeValue::string("!@#$%^&*()")),
    ("string.unicode", AttributeValue::string("Hello 世界 🌍")),
    ("int.zero", AttributeValue::int(0L)),
    ("int.min", AttributeValue::int(-9223372036854775808L)),
    ("int.max", AttributeValue::int(9223372036854775807L)),
    ("int.negative", AttributeValue::int(-42L)),
    ("float.zero", AttributeValue::float(0.0)),
    ("float.tiny", AttributeValue::float(1.0e-10)),
    ("float.large", AttributeValue::float(1.0e10)),
    ("float.precise", AttributeValue::float(3.14159265359)),
    ("bool.true", AttributeValue::bool(true)),
    ("bool.false", AttributeValue::bool(false)),
    ("array.string.empty", AttributeValue::array_string([])),
    ("array.string.single", AttributeValue::array_string(["single"])),
    ("array.string.mixed", AttributeValue::array_string(["", "value", ""])),
    ("array.int.empty", AttributeValue::array_int([])),
    ("array.int.boundary", AttributeValue::array_int([-1L, 0L, 1L])),
    ("array.float.empty", AttributeValue::array_float([])),
    ("array.float.special", AttributeValue::array_float([0.0, -0.0, 1.0e-10])),
    ("array.bool.empty", AttributeValue::array_bool([])),
    ("array.bool.mixed", AttributeValue::array_bool([true, false, true]))
  ]
  
  // 验证所有属性都正确创建
  assert_eq(complex_attributes.length(), 24)
  
  // 验证字符串属性
  match complex_attributes[0].1 {
    StringValue(value) => assert_eq(value, "simple_value")
    _ => @test.fail("Test failed")
  }
  
  match complex_attributes[1].1 {
    StringValue(value) => assert_eq(value, "")
    _ => @test.fail("Test failed")
  }
  
  // 验证整数属性
  match complex_attributes[4].1 {
    IntValue(value) => assert_eq(value, 0L)
    _ => @test.fail("Test failed")
  }
  
  match complex_attributes[5].1 {
    IntValue(value) => assert_eq(value, -9223372036854775808L)
    _ => @test.fail("Test failed")
  }
  
  // 验证浮点数属性
  match complex_attributes[9].1 {
    FloatValue(value) => assert_eq(value, 0.0)
    _ => @test.fail("Test failed")
  }
  
  match complex_attributes[10].1 {
    FloatValue(value) => assert_eq(value, 1.0e-10)
    _ => @test.fail("Test failed")
  }
  
  // 验证布尔属性
  match complex_attributes[12].1 {
    BoolValue(value) => assert_eq(value, true)
    _ => @test.fail("Test failed")
  }
  
  match complex_attributes[13].1 {
    BoolValue(value) => assert_eq(value, false)
    _ => @test.fail("Test failed")
  }
  
  // 验证数组属性
  match complex_attributes[14].1 {
    ArrayStringValue(values) => assert_eq(values.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match complex_attributes[15].1 {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 1)
      assert_eq(values[0], "single")
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_type_safety_validation" {
  // 测试AttributeValue的类型安全性
  
  // 创建各种类型的值
  let string_val = AttributeValue::string("test")
  let int_val = AttributeValue::int(42L)
  let float_val = AttributeValue::float(3.14)
  let bool_val = AttributeValue::bool(true)
  let string_array_val = AttributeValue::array_string(["a", "b"])
  let int_array_val = AttributeValue::array_int([1L, 2L])
  let float_array_val = AttributeValue::array_float([1.0, 2.0])
  let bool_array_val = AttributeValue::array_bool([true, false])
  
  // 验证每个值的类型
  match string_val {
    StringValue(_) => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match int_val {
    IntValue(_) => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match float_val {
    FloatValue(_) => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match bool_val {
    BoolValue(_) => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match string_array_val {
    ArrayStringValue(_) => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match int_array_val {
    ArrayIntValue(_) => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match float_array_val {
    ArrayFloatValue(_) => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match bool_array_val {
    ArrayBoolValue(_) => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
}