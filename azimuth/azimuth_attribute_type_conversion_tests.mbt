// Azimuth Attribute Type Conversion Tests
// 属性类型转换测试

test "基础属性类型转换测试" {
  let attrs = Attributes::new()
  
  // 测试字符串类型
  let string_value = StringValue("test.string.value")
  Attributes::set(attrs, "string.key", string_value)
  
  let retrieved_string = Attributes::get(attrs, "string.key")
  match retrieved_string {
    Some(StringValue(value)) => assert_eq(value, "test.string.value")
    _ => assert_true(false)
  }
  
  // 测试整数类型
  let int_value = IntValue(42)
  Attributes::set(attrs, "int.key", int_value)
  
  let retrieved_int = Attributes::get(attrs, "int.key")
  match retrieved_int {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  // 测试浮点类型
  let float_value = FloatValue(3.14159)
  Attributes::set(attrs, "float.key", float_value)
  
  let retrieved_float = Attributes::get(attrs, "float.key")
  match retrieved_float {
    Some(FloatValue(value)) => assert_eq(value, 3.14159)
    _ => assert_true(false)
  }
  
  // 测试布尔类型
  let bool_value = BoolValue(true)
  Attributes::set(attrs, "bool.key", bool_value)
  
  let retrieved_bool = Attributes::get(attrs, "bool.key")
  match retrieved_bool {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
}

test "数组属性类型转换测试" {
  let attrs = Attributes::new()
  
  // 测试字符串数组类型
  let string_array_value = ArrayStringValue(["item1", "item2", "item3"])
  Attributes::set(attrs, "string.array.key", string_array_value)
  
  let retrieved_string_array = Attributes::get(attrs, "string.array.key")
  match retrieved_string_array {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "item1")
      assert_eq(values[1], "item2")
      assert_eq(values[2], "item3")
    }
    _ => assert_true(false)
  }
  
  // 测试整数数组类型
  let int_array_value = ArrayIntValue([1, 2, 3, 4, 5])
  Attributes::set(attrs, "int.array.key", int_array_value)
  
  let retrieved_int_array = Attributes::get(attrs, "int.array.key")
  match retrieved_int_array {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], 1)
      assert_eq(values[1], 2)
      assert_eq(values[2], 3)
      assert_eq(values[3], 4)
      assert_eq(values[4], 5)
    }
    _ => assert_true(false)
  }
  
  // 测试空数组
  let empty_string_array = ArrayStringValue([])
  Attributes::set(attrs, "empty.string.array.key", empty_string_array)
  
  let retrieved_empty_string_array = Attributes::get(attrs, "empty.string.array.key")
  match retrieved_empty_string_array {
    Some(ArrayStringValue(values)) => assert_eq(values.length(), 0)
    _ => assert_true(false)
  }
  
  let empty_int_array = ArrayIntValue([])
  Attributes::set(attrs, "empty.int.array.key", empty_int_array)
  
  let retrieved_empty_int_array = Attributes::get(attrs, "empty.int.array.key")
  match retrieved_empty_int_array {
    Some(ArrayIntValue(values)) => assert_eq(values.length(), 0)
    _ => assert_true(false)
  }
}

test "属性类型边界值转换测试" {
  let attrs = Attributes::new()
  
  // 测试整数的边界值
  let max_int = IntValue(2147483647)
  Attributes::set(attrs, "max.int.key", max_int)
  
  let min_int = IntValue(-2147483648)
  Attributes::set(attrs, "min.int.key", min_int)
  
  let zero_int = IntValue(0)
  Attributes::set(attrs, "zero.int.key", zero_int)
  
  // 验证边界整数
  let retrieved_max_int = Attributes::get(attrs, "max.int.key")
  match retrieved_max_int {
    Some(IntValue(value)) => assert_eq(value, 2147483647)
    _ => assert_true(false)
  }
  
  let retrieved_min_int = Attributes::get(attrs, "min.int.key")
  match retrieved_min_int {
    Some(IntValue(value)) => assert_eq(value, -2147483648)
    _ => assert_true(false)
  }
  
  let retrieved_zero_int = Attributes::get(attrs, "zero.int.key")
  match retrieved_zero_int {
    Some(IntValue(value)) => assert_eq(value, 0)
    _ => assert_true(false)
  }
  
  // 测试浮点数的边界值
  let max_float = FloatValue(1.7976931348623157e+308) // 最大double值
  Attributes::set(attrs, "max.float.key", max_float)
  
  let min_float = FloatValue(2.2250738585072014e-308) // 最小正double值
  Attributes::set(attrs, "min.float.key", min_float)
  
  let zero_float = FloatValue(0.0)
  Attributes::set(attrs, "zero.float.key", zero_float)
  
  let negative_zero_float = FloatValue(-0.0)
  Attributes::set(attrs, "negative.zero.float.key", negative_zero_float)
  
  // 验证边界浮点数
  let retrieved_max_float = Attributes::get(attrs, "max.float.key")
  match retrieved_max_float {
    Some(FloatValue(value)) => assert_eq(value, 1.7976931348623157e+308)
    _ => assert_true(false)
  }
  
  let retrieved_min_float = Attributes::get(attrs, "min.float.key")
  match retrieved_min_float {
    Some(FloatValue(value)) => assert_eq(value, 2.2250738585072014e-308)
    _ => assert_true(false)
  }
  
  let retrieved_zero_float = Attributes::get(attrs, "zero.float.key")
  match retrieved_zero_float {
    Some(FloatValue(value)) => assert_eq(value, 0.0)
    _ => assert_true(false)
  }
  
  let retrieved_negative_zero_float = Attributes::get(attrs, "negative.zero.float.key")
  match retrieved_negative_zero_float {
    Some(FloatValue(value)) => assert_eq(value, -0.0)
    _ => assert_true(false)
  }
  
  // 测试特殊浮点值
  let infinity_float = FloatValue(1.0 / 0.0) // 正无穷
  Attributes::set(attrs, "infinity.float.key", infinity_float)
  
  let negative_infinity_float = FloatValue(-1.0 / 0.0) // 负无穷
  Attributes::set(attrs, "negative.infinity.float.key", negative_infinity_float)
  
  let nan_float = FloatValue(0.0 / 0.0) // NaN
  Attributes::set(attrs, "nan.float.key", nan_float)
  
  // 验证特殊浮点值
  let retrieved_infinity_float = Attributes::get(attrs, "infinity.float.key")
  match retrieved_infinity_float {
    Some(FloatValue(value)) => assert_true(value.is_infinite() && value > 0.0)
    _ => assert_true(false)
  }
  
  let retrieved_negative_infinity_float = Attributes::get(attrs, "negative.infinity.float.key")
  match retrieved_negative_infinity_float {
    Some(FloatValue(value)) => assert_true(value.is_infinite() && value < 0.0)
    _ => assert_true(false)
  }
  
  let retrieved_nan_float = Attributes::get(attrs, "nan.float.key")
  match retrieved_nan_float {
    Some(FloatValue(value)) => assert_true(value.is_nan())
    _ => assert_true(false)
  }
}

test "字符串属性特殊值转换测试" {
  let attrs = Attributes::new()
  
  // 测试空字符串
  let empty_string = StringValue("")
  Attributes::set(attrs, "empty.string.key", empty_string)
  
  let retrieved_empty_string = Attributes::get(attrs, "empty.string.key")
  match retrieved_empty_string {
    Some(StringValue(value)) => assert_eq(value, "")
    _ => assert_true(false)
  }
  
  // 测试单字符字符串
  let single_char_string = StringValue("a")
  Attributes::set(attrs, "single.char.string.key", single_char_string)
  
  let retrieved_single_char_string = Attributes::get(attrs, "single.char.string.key")
  match retrieved_single_char_string {
    Some(StringValue(value)) => assert_eq(value, "a")
    _ => assert_true(false)
  }
  
  // 测试包含空格的字符串
  let space_string = StringValue("string with spaces")
  Attributes::set(attrs, "space.string.key", space_string)
  
  let retrieved_space_string = Attributes::get(attrs, "space.string.key")
  match retrieved_space_string {
    Some(StringValue(value)) => assert_eq(value, "string with spaces")
    _ => assert_true(false)
  }
  
  // 测试包含特殊字符的字符串
  let special_char_string = StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  Attributes::set(attrs, "special.char.string.key", special_char_string)
  
  let retrieved_special_char_string = Attributes::get(attrs, "special.char.string.key")
  match retrieved_special_char_string {
    Some(StringValue(value)) => assert_eq(value, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
    _ => assert_true(false)
  }
  
  // 测试包含Unicode字符的字符串
  let unicode_string = StringValue("测试字符串🚀📊αβγδεζηθ")
  Attributes::set(attrs, "unicode.string.key", unicode_string)
  
  let retrieved_unicode_string = Attributes::get(attrs, "unicode.string.key")
  match retrieved_unicode_string {
    Some(StringValue(value)) => assert_eq(value, "测试字符串🚀📊αβγδεζηθ")
    _ => assert_true(false)
  }
  
  // 测试非常长的字符串
  let long_string = StringValue("This is a very long string that exceeds normal length limits and tests boundary conditions for string handling in the telemetry system. It contains multiple sentences and should test how the system handles unusually long strings.")
  Attributes::set(attrs, "long.string.key", long_string)
  
  let retrieved_long_string = Attributes::get(attrs, "long.string.key")
  match retrieved_long_string {
    Some(StringValue(value)) => assert_eq(value, "This is a very long string that exceeds normal length limits and tests boundary conditions for string handling in the telemetry system. It contains multiple sentences and should test how the system handles unusually long strings.")
    _ => assert_true(false)
  }
  
  // 测试只包含数字的字符串
  let numeric_string = StringValue("1234567890")
  Attributes::set(attrs, "numeric.string.key", numeric_string)
  
  let retrieved_numeric_string = Attributes::get(attrs, "numeric.string.key")
  match retrieved_numeric_string {
    Some(StringValue(value)) => assert_eq(value, "1234567890")
    _ => assert_true(false)
  }
}

test "属性类型混合转换测试" {
  let attrs = Attributes::new()
  
  // 设置各种类型的属性
  Attributes::set(attrs, "string.value", StringValue("test"))
  Attributes::set(attrs, "int.value", IntValue(42))
  Attributes::set(attrs, "float.value", FloatValue(3.14))
  Attributes::set(attrs, "bool.value", BoolValue(true))
  Attributes::set(attrs, "string.array.value", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "int.array.value", ArrayIntValue([1, 2, 3]))
  
  // 验证所有类型都能正确检索
  let string_value = Attributes::get(attrs, "string.value")
  match string_value {
    Some(StringValue(value)) => assert_eq(value, "test")
    _ => assert_true(false)
  }
  
  let int_value = Attributes::get(attrs, "int.value")
  match int_value {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  let float_value = Attributes::get(attrs, "float.value")
  match float_value {
    Some(FloatValue(value)) => assert_eq(value, 3.14)
    _ => assert_true(false)
  }
  
  let bool_value = Attributes::get(attrs, "bool.value")
  match bool_value {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
  
  let string_array_value = Attributes::get(attrs, "string.array.value")
  match string_array_value {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "a")
      assert_eq(values[1], "b")
      assert_eq(values[2], "c")
    }
    _ => assert_true(false)
  }
  
  let int_array_value = Attributes::get(attrs, "int.array.value")
  match int_array_value {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], 1)
      assert_eq(values[1], 2)
      assert_eq(values[2], 3)
    }
    _ => assert_true(false)
  }
  
  // 测试覆盖不同类型的值
  Attributes::set(attrs, "mixed.value", StringValue("initial.string"))
  Attributes::set(attrs, "mixed.value", IntValue(123))
  Attributes::set(attrs, "mixed.value", FloatValue(456.789))
  Attributes::set(attrs, "mixed.value", BoolValue(false))
  
  // 验证最终值
  let final_mixed_value = Attributes::get(attrs, "mixed.value")
  match final_mixed_value {
    Some(BoolValue(value)) => assert_eq(value, false)
    _ => assert_true(false)
  }
}

test "数组属性复杂值转换测试" {
  let attrs = Attributes::new()
  
  // 测试包含特殊字符的字符串数组
  let special_string_array = ArrayStringValue([
    "item with spaces",
    "item@with#special$chars",
    "测试项目",
    "item🚀with📊emoji"
  ])
  Attributes::set(attrs, "special.string.array.key", special_string_array)
  
  let retrieved_special_string_array = Attributes::get(attrs, "special.string.array.key")
  match retrieved_special_string_array {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 4)
      assert_eq(values[0], "item with spaces")
      assert_eq(values[1], "item@with#special$chars")
      assert_eq(values[2], "测试项目")
      assert_eq(values[3], "item🚀with📊emoji")
    }
    _ => assert_true(false)
  }
  
  // 测试包含边界值的整数数组
  let boundary_int_array = ArrayIntValue([
    -2147483648, // 最小int
    0,           // 零
    2147483647   // 最大int
  ])
  Attributes::set(attrs, "boundary.int.array.key", boundary_int_array)
  
  let retrieved_boundary_int_array = Attributes::get(attrs, "boundary.int.array.key")
  match retrieved_boundary_int_array {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], -2147483648)
      assert_eq(values[1], 0)
      assert_eq(values[2], 2147483647)
    }
    _ => assert_true(false)
  }
  
  // 测试包含重复元素的数组
  let duplicate_string_array = ArrayStringValue([
    "duplicate",
    "duplicate",
    "unique",
    "duplicate"
  ])
  Attributes::set(attrs, "duplicate.string.array.key", duplicate_string_array)
  
  let retrieved_duplicate_string_array = Attributes::get(attrs, "duplicate.string.array.key")
  match retrieved_duplicate_string_array {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 4)
      assert_eq(values[0], "duplicate")
      assert_eq(values[1], "duplicate")
      assert_eq(values[2], "unique")
      assert_eq(values[3], "duplicate")
    }
    _ => assert_true(false)
  }
  
  // 测试单元素数组
  let single_string_array = ArrayStringValue(["single.element"])
  Attributes::set(attrs, "single.string.array.key", single_string_array)
  
  let retrieved_single_string_array = Attributes::get(attrs, "single.string.array.key")
  match retrieved_single_string_array {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 1)
      assert_eq(values[0], "single.element")
    }
    _ => assert_true(false)
  }
  
  let single_int_array = ArrayIntValue([42])
  Attributes::set(attrs, "single.int.array.key", single_int_array)
  
  let retrieved_single_int_array = Attributes::get(attrs, "single.int.array.key")
  match retrieved_single_int_array {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 1)
      assert_eq(values[0], 42)
    }
    _ => assert_true(false)
  }
}

test "属性类型转换错误处理测试" {
  let attrs = Attributes::new()
  
  // 测试获取不存在的属性
  let missing_attribute = Attributes::get(attrs, "nonexistent.key")
  assert_eq(missing_attribute, None)
  
  // 测试设置和获取不同类型的属性
  Attributes::set(attrs, "test.key", StringValue("string.value"))
  
  // 尝试以不同类型获取（应该返回None或正确的值）
  let string_result = Attributes::get(attrs, "test.key")
  match string_result {
    Some(StringValue(value)) => assert_eq(value, "string.value")
    _ => assert_true(false)
  }
  
  // 设置整数并尝试以字符串获取
  Attributes::set(attrs, "int.key", IntValue(123))
  let int_result = Attributes::get(attrs, "int.key")
  match int_result {
    Some(IntValue(value)) => assert_eq(value, 123)
    _ => assert_true(false)
  }
  
  // 测试空键名
  Attributes::set(attrs, "", StringValue("empty.key.value"))
  let empty_key_result = Attributes::get(attrs, "")
  match empty_key_result {
    Some(StringValue(value)) => assert_eq(value, "empty.key.value")
    _ => assert_true(false)
  }
  
  // 测试包含空格的键名
  Attributes::set(attrs, "key with spaces", StringValue("spaces.key.value"))
  let spaces_key_result = Attributes::get(attrs, "key with spaces")
  match spaces_key_result {
    Some(StringValue(value)) => assert_eq(value, "spaces.key.value")
    _ => assert_true(false)
  }
  
  // 测试包含特殊字符的键名
  Attributes::set(attrs, "key@with#special$chars", StringValue("special.chars.key.value"))
  let special_chars_key_result = Attributes::get(attrs, "key@with#special$chars")
  match special_chars_key_result {
    Some(StringValue(value)) => assert_eq(value, "special.chars.key.value")
    _ => assert_true(false)
  }
  
  // 测试Unicode键名
  Attributes::set(attrs, "测试键", StringValue("Unicode键值"))
  let unicode_key_result = Attributes::get(attrs, "测试键")
  match unicode_key_result {
    Some(StringValue(value)) => assert_eq(value, "Unicode键值")
    _ => assert_true(false)
  }
}

test "属性类型转换性能测试" {
  let attrs = Attributes::new()
  
  // 测试大量属性设置和检索的性能
  let attribute_count = 1000
  
  // 设置大量属性
  for i = 0; i < attribute_count; i = i + 1 {
    let key = "perf.test.key." + i.to_string()
    
    // 循环设置不同类型的属性
    if i % 6 == 0 {
      Attributes::set(attrs, key, StringValue("string.value." + i.to_string()))
    } else if i % 6 == 1 {
      Attributes::set(attrs, key, IntValue(i))
    } else if i % 6 == 2 {
      Attributes::set(attrs, key, FloatValue(i.to_double() * 0.1))
    } else if i % 6 == 3 {
      Attributes::set(attrs, key, BoolValue(i % 2 == 0))
    } else if i % 6 == 4 {
      Attributes::set(attrs, key, ArrayStringValue(["item1", "item2", "item3"]))
    } else {
      Attributes::set(attrs, key, ArrayIntValue([1, 2, 3, 4, 5]))
    }
  }
  
  // 检索大量属性
  for i = 0; i < attribute_count; i = i + 1 {
    let key = "perf.test.key." + i.to_string()
    let value = Attributes::get(attrs, key)
    
    // 验证检索到的值不为None
    assert_true(value is Some)
  }
  
  // 测试数组属性的性能
  let array_attr_count = 100
  
  for i = 0; i < array_attr_count; i = i + 1 {
    let string_array_key = "perf.string.array.key." + i.to_string()
    let int_array_key = "perf.int.array.key." + i.to_string()
    
    let string_array = ArrayStringValue([
      "item1." + i.to_string(),
      "item2." + i.to_string(),
      "item3." + i.to_string()
    ])
    
    let int_array = ArrayIntValue([
      i * 1,
      i * 2,
      i * 3,
      i * 4,
      i * 5
    ])
    
    Attributes::set(attrs, string_array_key, string_array)
    Attributes::set(attrs, int_array_key, int_array)
  }
  
  // 检索数组属性
  for i = 0; i < array_attr_count; i = i + 1 {
    let string_array_key = "perf.string.array.key." + i.to_string()
    let int_array_key = "perf.int.array.key." + i.to_string()
    
    let string_array_value = Attributes::get(attrs, string_array_key)
    let int_array_value = Attributes::get(attrs, int_array_key)
    
    // 验证检索到的数组不为None
    assert_true(string_array_value is Some)
    assert_true(int_array_value is Some)
  }
  
  assert_true(true)
}
