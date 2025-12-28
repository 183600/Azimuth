// Specialized Attribute Value Tests - 属性值深度操作和类型转换测试
// 专注于AttributeValue的各种类型转换、操作和边界条件

test "属性值类型转换和验证测试" {
  // 测试所有属性值类型的创建和验证
  let string_attr = StringValue("test.string.value")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14159)
  let bool_attr = BoolValue(true)
  let empty_string_attr = StringValue("")
  let zero_int_attr = IntValue(0)
  let zero_float_attr = FloatValue(0.0)
  let false_bool_attr = BoolValue(false)
  
  // 测试数组类型属性
  let empty_string_array = ArrayStringValue([])
  let single_string_array = ArrayStringValue(["single"])
  let multi_string_array = ArrayStringValue(["item1", "item2", "item3"])
  let empty_int_array = ArrayIntValue([])
  let single_int_array = ArrayIntValue([42])
  let multi_int_array = ArrayIntValue([1, 2, 3, 4, 5])
  
  // 验证基本属性值
  assert_true(match string_attr { StringValue(v) => v == "test.string.value" _ => false })
  assert_true(match int_attr { IntValue(v) => v == 42 _ => false })
  assert_true(match float_attr { FloatValue(v) => v == 3.14159 _ => false })
  assert_true(match bool_attr { BoolValue(v) => v == true _ => false })
  
  // 验证边界值
  assert_true(match empty_string_attr { StringValue(v) => v == "" _ => false })
  assert_true(match zero_int_attr { IntValue(v) => v == 0 _ => false })
  assert_true(match zero_float_attr { FloatValue(v) => v == 0.0 _ => false })
  assert_true(match false_bool_attr { BoolValue(v) => v == false _ => false })
  
  // 验证数组属性
  assert_true(match empty_string_array { ArrayStringValue(v) => v.length() == 0 _ => false })
  assert_true(match single_string_array { ArrayStringValue(v) => v.length() == 1 && v[0] == "single" _ => false })
  assert_true(match multi_string_array { ArrayStringValue(v) => v.length() == 3 _ => false })
  assert_true(match empty_int_array { ArrayIntValue(v) => v.length() == 0 _ => false })
  assert_true(match single_int_array { ArrayIntValue(v) => v.length() == 1 && v[0] == 42 _ => false })
  assert_true(match multi_int_array { ArrayIntValue(v) => v.length() == 5 _ => false })
}

test "属性值序列化和反序列化测试" {
  // 创建包含各种类型属性的Attributes
  let attrs = Attributes::new()
  
  // 设置各种类型的属性
  Attributes::set(attrs, "string.key", StringValue("serialized.value"))
  Attributes::set(attrs, "int.key", IntValue(-12345))
  Attributes::set(attrs, "float.key", FloatValue(-3.14159))
  Attributes::set(attrs, "bool.key", BoolValue(false))
  Attributes::set(attrs, "unicode.key", StringValue("测试中文 🚀 العربية"))
  Attributes::set(attrs, "special.chars.key", StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  
  // 设置数组属性
  Attributes::set(attrs, "string.array.key", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "int.array.key", ArrayIntValue([100, 200, 300]))
  
  // 验证属性获取和类型匹配
  let string_result = Attributes::get(attrs, "string.key")
  assert_true(string_result is Some)
  match string_result {
    Some(StringValue(value)) => assert_eq(value, "serialized.value")
    _ => assert_true(false)
  }
  
  let int_result = Attributes::get(attrs, "int.key")
  assert_true(int_result is Some)
  match int_result {
    Some(IntValue(value)) => assert_eq(value, -12345)
    _ => assert_true(false)
  }
  
  let float_result = Attributes::get(attrs, "float.key")
  assert_true(float_result is Some)
  match float_result {
    Some(FloatValue(value)) => assert_true(value > -3.15 && value < -3.14)
    _ => assert_true(false)
  }
  
  let bool_result = Attributes::get(attrs, "bool.key")
  assert_true(bool_result is Some)
  match bool_result {
    Some(BoolValue(value)) => assert_false(value)
    _ => assert_true(false)
  }
  
  // 验证Unicode和特殊字符处理
  let unicode_result = Attributes::get(attrs, "unicode.key")
  assert_true(unicode_result is Some)
  match unicode_result {
    Some(StringValue(value)) => assert_eq(value, "测试中文 🚀 العربية")
    _ => assert_true(false)
  }
  
  let special_chars_result = Attributes::get(attrs, "special.chars.key")
  assert_true(special_chars_result is Some)
  match special_chars_result {
    Some(StringValue(value)) => assert_eq(value, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
    _ => assert_true(false)
  }
}

test "属性值边界条件和极限值测试" {
  let attrs = Attributes::new()
  
  // 测试数值极限值
  let max_int = IntValue(2147483647)
  let min_int = IntValue(-2147483648)
  let max_float = FloatValue(1.7976931348623157e+308)
  let min_float = FloatValue(-1.7976931348623157e+308)
  let infinity_float = FloatValue(1.0/0.0)
  let neg_infinity_float = FloatValue(-1.0/0.0)
  let nan_float = FloatValue(0.0/0.0)
  
  Attributes::set(attrs, "max.int", max_int)
  Attributes::set(attrs, "min.int", min_int)
  Attributes::set(attrs, "max.float", max_float)
  Attributes::set(attrs, "min.float", min_float)
  Attributes::set(attrs, "infinity.float", infinity_float)
  Attributes::set(attrs, "neg.infinity.float", neg_infinity_float)
  Attributes::set(attrs, "nan.float", nan_float)
  
  // 验证极限值
  let max_int_result = Attributes::get(attrs, "max.int")
  assert_true(max_int_result is Some)
  match max_int_result {
    Some(IntValue(value)) => assert_eq(value, 2147483647)
    _ => assert_true(false)
  }
  
  let min_int_result = Attributes::get(attrs, "min.int")
  assert_true(min_int_result is Some)
  match min_int_result {
    Some(IntValue(value)) => assert_eq(value, -2147483648)
    _ => assert_true(false)
  }
  
  // 测试字符串长度边界
  let empty_string = StringValue("")
  let single_char = StringValue("a")
  let long_string = StringValue("a" * 1000)
  let very_long_string = StringValue("测试" * 500)
  
  Attributes::set(attrs, "empty.string", empty_string)
  Attributes::set(attrs, "single.char", single_char)
  Attributes::set(attrs, "long.string", long_string)
  Attributes::set(attrs, "very.long.string", very_long_string)
  
  // 验证字符串边界
  let empty_result = Attributes::get(attrs, "empty.string")
  assert_true(empty_result is Some)
  match empty_result {
    Some(StringValue(value)) => assert_eq(value, "")
    _ => assert_true(false)
  }
  
  let single_char_result = Attributes::get(attrs, "single.char")
  assert_true(single_char_result is Some)
  match single_char_result {
    Some(StringValue(value)) => assert_eq(value, "a")
    _ => assert_true(false)
  }
  
  // 测试数组边界条件
  let empty_string_array = ArrayStringValue([])
  let large_string_array = ArrayStringValue([1, 2, 3, 4, 5].map(fn(i) { "item" + i.to_string() }))
  let empty_int_array = ArrayIntValue([])
  let large_int_array = ArrayIntValue([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  
  Attributes::set(attrs, "empty.string.array", empty_string_array)
  Attributes::set(attrs, "large.string.array", large_string_array)
  Attributes::set(attrs, "empty.int.array", empty_int_array)
  Attributes::set(attrs, "large.int.array", large_int_array)
  
  // 验证数组边界
  let empty_array_result = Attributes::get(attrs, "empty.string.array")
  assert_true(empty_array_result is Some)
  match empty_array_result {
    Some(ArrayStringValue(value)) => assert_eq(value.length(), 0)
    _ => assert_true(false)
  }
}

test "属性值类型转换和比较测试" {
  // 测试属性值的类型转换逻辑
  let string_number = StringValue("42")
  let int_number = IntValue(42)
  let float_number = FloatValue(42.0)
  let bool_true = BoolValue(true)
  let bool_false = BoolValue(false)
  
  // 创建属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.number", string_number)
  Attributes::set(attrs, "int.number", int_number)
  Attributes::set(attrs, "float.number", float_number)
  Attributes::set(attrs, "bool.true", bool_true)
  Attributes::set(attrs, "bool.false", bool_false)
  
  // 测试布尔值的字符串表示
  let bool_true_result = Attributes::get(attrs, "bool.true")
  assert_true(bool_true_result is Some)
  match bool_true_result {
    Some(BoolValue(value)) => assert_true(value)
    _ => assert_true(false)
  }
  
  let bool_false_result = Attributes::get(attrs, "bool.false")
  assert_true(bool_false_result is Some)
  match bool_false_result {
    Some(BoolValue(value)) => assert_false(value)
    _ => assert_true(false)
  }
  
  // 测试数值类型的精度保持
  let float_precision = FloatValue(3.141592653589793)
  Attributes::set(attrs, "float.precision", float_precision)
  
  let precision_result = Attributes::get(attrs, "float.precision")
  assert_true(precision_result is Some)
  match precision_result {
    Some(FloatValue(value)) => assert_true(value > 3.1415 && value < 3.1416)
    _ => assert_true(false)
  }
  
  // 测试数组类型的一致性
  let string_array = ArrayStringValue(["a", "b", "c"])
  let int_array = ArrayIntValue([1, 2, 3])
  
  Attributes::set(attrs, "string.array", string_array)
  Attributes::set(attrs, "int.array", int_array)
  
  let string_array_result = Attributes::get(attrs, "string.array")
  assert_true(string_array_result is Some)
  match string_array_result {
    Some(ArrayStringValue(value)) => {
      assert_eq(value.length(), 3)
      assert_eq(value[0], "a")
      assert_eq(value[1], "b")
      assert_eq(value[2], "c")
    }
    _ => assert_true(false)
  }
  
  let int_array_result = Attributes::get(attrs, "int.array")
  assert_true(int_array_result is Some)
  match int_array_result {
    Some(ArrayIntValue(value)) => {
      assert_eq(value.length(), 3)
      assert_eq(value[0], 1)
      assert_eq(value[1], 2)
      assert_eq(value[2], 3)
    }
    _ => assert_true(false)
  }
}