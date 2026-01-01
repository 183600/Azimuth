// 属性值类型转换测试用例
// 测试各种属性值类型之间的转换和操作

test "字符串属性值转换" {
  // 测试字符串属性值的创建和操作
  let string_value = StringValue("test-string")
  
  // 创建属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", string_value)
  
  // 获取属性值
  let retrieved_value = Attributes::get(attrs, "string.key")
  assert_eq(retrieved_value, Some(StringValue("test-string")))
  
  // 测试特殊字符串
  let empty_string = StringValue("")
  let whitespace_string = StringValue("   ")
  let unicode_string = StringValue("测试字符串")
  let special_chars_string = StringValue("!@#$%^&*()")
  
  Attributes::set(attrs, "empty.string", empty_string)
  Attributes::set(attrs, "whitespace.string", whitespace_string)
  Attributes::set(attrs, "unicode.string", unicode_string)
  Attributes::set(attrs, "special.chars.string", special_chars_string)
  
  assert_eq(Attributes::get(attrs, "empty.string"), Some(empty_string))
  assert_eq(Attributes::get(attrs, "whitespace.string"), Some(whitespace_string))
  assert_eq(Attributes::get(attrs, "unicode.string"), Some(unicode_string))
  assert_eq(Attributes::get(attrs, "special.chars.string"), Some(special_chars_string))
  
  assert_true(true)
}

test "整数属性值转换" {
  // 测试整数属性值的创建和操作
  let int_value = IntValue(42)
  
  // 创建属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "int.key", int_value)
  
  // 获取属性值
  let retrieved_value = Attributes::get(attrs, "int.key")
  assert_eq(retrieved_value, Some(IntValue(42)))
  
  // 测试边界整数
  let zero_int = IntValue(0)
  let negative_int = IntValue(-42)
  let max_int = IntValue(2147483647)
  let min_int = IntValue(-2147483648)
  
  Attributes::set(attrs, "zero.int", zero_int)
  Attributes::set(attrs, "negative.int", negative_int)
  Attributes::set(attrs, "max.int", max_int)
  Attributes::set(attrs, "min.int", min_int)
  
  assert_eq(Attributes::get(attrs, "zero.int"), Some(zero_int))
  assert_eq(Attributes::get(attrs, "negative.int"), Some(negative_int))
  assert_eq(Attributes::get(attrs, "max.int"), Some(max_int))
  assert_eq(Attributes::get(attrs, "min.int"), Some(min_int))
  
  assert_true(true)
}

test "浮点数属性值转换" {
  // 测试浮点数属性值的创建和操作
  let float_value = FloatValue(3.14)
  
  // 创建属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "float.key", float_value)
  
  // 获取属性值
  let retrieved_value = Attributes::get(attrs, "float.key")
  assert_eq(retrieved_value, Some(FloatValue(3.14)))
  
  // 测试特殊浮点数
  let zero_float = FloatValue(0.0)
  let negative_float = FloatValue(-3.14)
  let small_float = FloatValue(0.000001)
  let large_float = FloatValue(1000000.0)
  let scientific_float = FloatValue(1.23e-4)
  
  Attributes::set(attrs, "zero.float", zero_float)
  Attributes::set(attrs, "negative.float", negative_float)
  Attributes::set(attrs, "small.float", small_float)
  Attributes::set(attrs, "large.float", large_float)
  Attributes::set(attrs, "scientific.float", scientific_float)
  
  assert_eq(Attributes::get(attrs, "zero.float"), Some(zero_float))
  assert_eq(Attributes::get(attrs, "negative.float"), Some(negative_float))
  assert_eq(Attributes::get(attrs, "small.float"), Some(small_float))
  assert_eq(Attributes::get(attrs, "large.float"), Some(large_float))
  assert_eq(Attributes::get(attrs, "scientific.float"), Some(scientific_float))
  
  assert_true(true)
}

test "布尔属性值转换" {
  // 测试布尔属性值的创建和操作
  let bool_value_true = BoolValue(true)
  let bool_value_false = BoolValue(false)
  
  // 创建属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "bool.true.key", bool_value_true)
  Attributes::set(attrs, "bool.false.key", bool_value_false)
  
  // 获取属性值
  let retrieved_true = Attributes::get(attrs, "bool.true.key")
  let retrieved_false = Attributes::get(attrs, "bool.false.key")
  
  assert_eq(retrieved_true, Some(BoolValue(true)))
  assert_eq(retrieved_false, Some(BoolValue(false)))
  
  assert_true(true)
}

test "字符串数组属性值转换" {
  // 测试字符串数组属性值的创建和操作
  let string_array_value = ArrayStringValue(["a", "b", "c"])
  
  // 创建属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.array.key", string_array_value)
  
  // 获取属性值
  let retrieved_value = Attributes::get(attrs, "string.array.key")
  assert_eq(retrieved_value, Some(ArrayStringValue(["a", "b", "c"])))
  
  // 测试特殊字符串数组
  let empty_array = ArrayStringValue([])
  let single_element_array = ArrayStringValue(["single"])
  let unicode_array = ArrayStringValue(["测试", "字符串", "数组"])
  let special_chars_array = ArrayStringValue(["!@#", "$%^", "&*()"])
  
  Attributes::set(attrs, "empty.array", empty_array)
  Attributes::set(attrs, "single.element.array", single_element_array)
  Attributes::set(attrs, "unicode.array", unicode_array)
  Attributes::set(attrs, "special.chars.array", special_chars_array)
  
  assert_eq(Attributes::get(attrs, "empty.array"), Some(empty_array))
  assert_eq(Attributes::get(attrs, "single.element.array"), Some(single_element_array))
  assert_eq(Attributes::get(attrs, "unicode.array"), Some(unicode_array))
  assert_eq(Attributes::get(attrs, "special.chars.array"), Some(special_chars_array))
  
  assert_true(true)
}

test "整数数组属性值转换" {
  // 测试整数数组属性值的创建和操作
  let int_array_value = ArrayIntValue([1, 2, 3])
  
  // 创建属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "int.array.key", int_array_value)
  
  // 获取属性值
  let retrieved_value = Attributes::get(attrs, "int.array.key")
  assert_eq(retrieved_value, Some(ArrayIntValue([1, 2, 3])))
  
  // 测试特殊整数数组
  let empty_int_array = ArrayIntValue([])
  let single_int_array = ArrayIntValue([42])
  let negative_int_array = ArrayIntValue([-1, -2, -3])
  let mixed_int_array = ArrayIntValue([0, 1, -1, 2147483647, -2147483648])
  
  Attributes::set(attrs, "empty.int.array", empty_int_array)
  Attributes::set(attrs, "single.int.array", single_int_array)
  Attributes::set(attrs, "negative.int.array", negative_int_array)
  Attributes::set(attrs, "mixed.int.array", mixed_int_array)
  
  assert_eq(Attributes::get(attrs, "empty.int.array"), Some(empty_int_array))
  assert_eq(Attributes::get(attrs, "single.int.array"), Some(single_int_array))
  assert_eq(Attributes::get(attrs, "negative.int.array"), Some(negative_int_array))
  assert_eq(Attributes::get(attrs, "mixed.int.array"), Some(mixed_int_array))
  
  assert_true(true)
}

test "混合属性值类型转换" {
  // 测试混合不同类型的属性值
  let attrs = Attributes::new()
  
  // 添加不同类型的属性
  Attributes::set(attrs, "string.value", StringValue("test"))
  Attributes::set(attrs, "int.value", IntValue(42))
  Attributes::set(attrs, "float.value", FloatValue(3.14))
  Attributes::set(attrs, "bool.value", BoolValue(true))
  Attributes::set(attrs, "string.array.value", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "int.array.value", ArrayIntValue([1, 2, 3]))
  
  // 验证所有类型的属性值
  assert_eq(Attributes::get(attrs, "string.value"), Some(StringValue("test")))
  assert_eq(Attributes::get(attrs, "int.value"), Some(IntValue(42)))
  assert_eq(Attributes::get(attrs, "float.value"), Some(FloatValue(3.14)))
  assert_eq(Attributes::get(attrs, "bool.value"), Some(BoolValue(true)))
  assert_eq(Attributes::get(attrs, "string.array.value"), Some(ArrayStringValue(["a", "b", "c"])))
  assert_eq(Attributes::get(attrs, "int.array.value"), Some(ArrayIntValue([1, 2, 3])))
  
  // 测试覆盖不同类型的值
  Attributes::set(attrs, "string.value", StringValue("updated"))
  Attributes::set(attrs, "int.value", IntValue(100))
  
  assert_eq(Attributes::get(attrs, "string.value"), Some(StringValue("updated")))
  assert_eq(Attributes::get(attrs, "int.value"), Some(IntValue(100)))
  
  assert_true(true)
}

test "属性值类型边界条件" {
  // 测试属性值类型的边界条件
  let attrs = Attributes::new()
  
  // 测试极长字符串
  let long_string = "a" * 1000
  Attributes::set(attrs, "long.string", StringValue(long_string))
  assert_eq(Attributes::get(attrs, "long.string"), Some(StringValue(long_string)))
  
  // 测试大整数
  let large_int = IntValue(2147483647)
  Attributes::set(attrs, "large.int", large_int)
  assert_eq(Attributes::get(attrs, "large.int"), Some(large_int))
  
  // 测试小整数
  let small_int = IntValue(-2147483648)
  Attributes::set(attrs, "small.int", small_int)
  assert_eq(Attributes::get(attrs, "small.int"), Some(small_int))
  
  // 测试极大浮点数
  let large_float = FloatValue(1.7976931348623157e+308)
  Attributes::set(attrs, "large.float", large_float)
  assert_eq(Attributes::get(attrs, "large.float"), Some(large_float))
  
  // 测试极小浮点数
  let small_float = FloatValue(2.2250738585072014e-308)
  Attributes::set(attrs, "small.float", small_float)
  assert_eq(Attributes::get(attrs, "small.float"), Some(small_float))
  
  // 测试大数组
  let large_string_array = ArrayStringValue(["item" + i.to_string() for i in range(0, 100)])
  Attributes::set(attrs, "large.string.array", large_string_array)
  assert_eq(Attributes::get(attrs, "large.string.array"), Some(large_string_array))
  
  let large_int_array = ArrayIntValue([i for i in range(0, 100)])
  Attributes::set(attrs, "large.int.array", large_int_array)
  assert_eq(Attributes::get(attrs, "large.int.array"), Some(large_int_array))
  
  assert_true(true)
}

test "属性值类型转换错误处理" {
  // 测试属性值类型转换的错误处理
  let attrs = Attributes::new()
  
  // 测试不存在的键
  assert_eq(Attributes::get(attrs, "nonexistent.key"), None)
  
  // 测试空属性集合
  let empty_attrs = Attributes::new()
  assert_eq(empty_attrs.values.length(), 0)
  assert_eq(Attributes::get(empty_attrs, "any.key"), None)
  
  // 测试覆盖不同类型的值
  Attributes::set(attrs, "test.key", StringValue("string"))
  assert_eq(Attributes::get(attrs, "test.key"), Some(StringValue("string")))
  
  Attributes::set(attrs, "test.key", IntValue(42))
  assert_eq(Attributes::get(attrs, "test.key"), Some(IntValue(42)))
  
  Attributes::set(attrs, "test.key", FloatValue(3.14))
  assert_eq(Attributes::get(attrs, "test.key"), Some(FloatValue(3.14)))
  
  Attributes::set(attrs, "test.key", BoolValue(true))
  assert_eq(Attributes::get(attrs, "test.key"), Some(BoolValue(true)))
  
  Attributes::set(attrs, "test.key", ArrayStringValue(["a", "b", "c"]))
  assert_eq(Attributes::get(attrs, "test.key"), Some(ArrayStringValue(["a", "b", "c"])))
  
  Attributes::set(attrs, "test.key", ArrayIntValue([1, 2, 3]))
  assert_eq(Attributes::get(attrs, "test.key"), Some(ArrayIntValue([1, 2, 3])))
  
  assert_true(true)
}

test "属性值类型转换性能" {
  // 测试属性值类型转换的性能
  let attrs = Attributes::new()
  
  // 测试大量属性设置
  for i = 0; i < 1000; i = i + 1 {
    let key = "key" + i.to_string()
    let value = StringValue("value" + i.to_string())
    Attributes::set(attrs, key, value)
  }
  
  // 测试大量属性获取
  for i = 0; i < 1000; i = i + 1 {
    let key = "key" + i.to_string()
    let expected_value = StringValue("value" + i.to_string())
    assert_eq(Attributes::get(attrs, key), Some(expected_value))
  }
  
  // 测试不同类型的混合操作
  for i = 0; i < 100; i = i + 1 {
    let string_key = "string.key" + i.to_string()
    let int_key = "int.key" + i.to_string()
    let float_key = "float.key" + i.to_string()
    let bool_key = "bool.key" + i.to_string()
    
    Attributes::set(attrs, string_key, StringValue("string" + i.to_string()))
    Attributes::set(attrs, int_key, IntValue(i))
    Attributes::set(attrs, float_key, FloatValue(i * 1.1))
    Attributes::set(attrs, bool_key, BoolValue(i % 2 == 0))
    
    assert_eq(Attributes::get(attrs, string_key), Some(StringValue("string" + i.to_string())))
    assert_eq(Attributes::get(attrs, int_key), Some(IntValue(i)))
    assert_eq(Attributes::get(attrs, float_key), Some(FloatValue(i * 1.1)))
    assert_eq(Attributes::get(attrs, bool_key), Some(BoolValue(i % 2 == 0)))
  }
  
  assert_true(true)
}