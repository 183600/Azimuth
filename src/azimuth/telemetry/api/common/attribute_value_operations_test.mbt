// AttributeValue类型转换和操作的综合测试

test "attribute_value_string_operations" {
  // 测试字符串类型的AttributeValue操作
  
  let string_attr = AttributeValue::string("test-value")
  let string_attr2 = AttributeValue::string("")
  let string_attr3 = AttributeValue::string("unicode-test-测试-123")
  
  // 验证字符串属性创建
  assert_eq(string_attr is StringValue, true)
  assert_eq(string_attr2 is StringValue, true)
  assert_eq(string_attr3 is StringValue, true)
  
  // 验证字符串属性长度
  match string_attr {
    StringValue(s) => assert_eq(s.length(), 10)
    _ => assert_eq(false, true)
  }
  
  match string_attr2 {
    StringValue(s) => assert_eq(s.length(), 0)
    _ => assert_eq(false, true)
  }
  
  match string_attr3 {
    StringValue(s) => assert_eq(s.length(), 18)
    _ => assert_eq(false, true)
  }
}

test "attribute_value_numeric_operations" {
  // 测试数值类型的AttributeValue操作
  
  let int_attr = AttributeValue::int(42L)
  let int_attr_negative = AttributeValue::int(-100L)
  let int_attr_zero = AttributeValue::int(0L)
  let int_attr_max = AttributeValue::int(9223372036854775807L)
  
  // 验证整数属性创建
  assert_eq(int_attr is IntValue, true)
  assert_eq(int_attr_negative is IntValue, true)
  assert_eq(int_attr_zero is IntValue, true)
  assert_eq(int_attr_max is IntValue, true)
  
  // 验证整数属性值
  match int_attr {
    IntValue(i) => assert_eq(i, 42L)
    _ => assert_eq(false, true)
  }
  
  match int_attr_negative {
    IntValue(i) => assert_eq(i, -100L)
    _ => assert_eq(false, true)
  }
  
  let float_attr = AttributeValue::float(3.14159)
  let float_attr_negative = AttributeValue::float(-2.71828)
  let float_attr_zero = AttributeValue::float(0.0)
  
  // 验证浮点数属性创建
  assert_eq(float_attr is FloatValue, true)
  assert_eq(float_attr_negative is FloatValue, true)
  assert_eq(float_attr_zero is FloatValue, true)
  
  // 验证浮点数属性值
  match float_attr {
    FloatValue(f) => assert_eq(f > 3.14 && f < 3.15, true)
    _ => assert_eq(false, true)
  }
}

test "attribute_value_boolean_operations" {
  // 测试布尔类型的AttributeValue操作
  
  let bool_attr_true = AttributeValue::bool(true)
  let bool_attr_false = AttributeValue::bool(false)
  
  // 验证布尔属性创建
  assert_eq(bool_attr_true is BoolValue, true)
  assert_eq(bool_attr_false is BoolValue, true)
  
  // 验证布尔属性值
  match bool_attr_true {
    BoolValue(b) => assert_eq(b, true)
    _ => assert_eq(false, true)
  }
  
  match bool_attr_false {
    BoolValue(b) => assert_eq(b, false)
    _ => assert_eq(false, true)
  }
}

test "attribute_value_array_operations" {
  // 测试数组类型的AttributeValue操作
  
  let string_array = ["value1", "value2", "value3"]
  let int_array = [1L, 2L, 3L, 4L, 5L]
  let float_array = [1.1, 2.2, 3.3]
  let bool_array = [true, false, true, false]
  
  let string_array_attr = AttributeValue::array_string(string_array)
  let int_array_attr = AttributeValue::array_int(int_array)
  let float_array_attr = AttributeValue::array_float(float_array)
  let bool_array_attr = AttributeValue::array_bool(bool_array)
  
  // 验证数组属性创建
  assert_eq(string_array_attr is ArrayStringValue, true)
  assert_eq(int_array_attr is ArrayIntValue, true)
  assert_eq(float_array_attr is ArrayFloatValue, true)
  assert_eq(bool_array_attr is ArrayBoolValue, true)
  
  // 验证数组长度
  match string_array_attr {
    ArrayStringValue(arr) => assert_eq(arr.length(), 3)
    _ => assert_eq(false, true)
  }
  
  match int_array_attr {
    ArrayIntValue(arr) => assert_eq(arr.length(), 5)
    _ => assert_eq(false, true)
  }
  
  match float_array_attr {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 3)
    _ => assert_eq(false, true)
  }
  
  match bool_array_attr {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 4)
    _ => assert_eq(false, true)
  }
  
  // 验证空数组
  let empty_string_array = [] : Array[String]
  let empty_string_attr = AttributeValue::array_string(empty_string_array)
  
  match empty_string_attr {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_eq(false, true)
  }
}

test "attribute_value_complex_scenarios" {
  // 测试AttributeValue的复杂场景
  
  // 创建包含各种类型的属性列表
  let attributes = [
    ("string.key", AttributeValue::string("string.value")),
    ("int.key", AttributeValue::int(123L)),
    ("float.key", AttributeValue::float(45.67)),
    ("bool.key", AttributeValue::bool(true)),
    ("string_array.key", AttributeValue::array_string(["a", "b", "c"])),
    ("int_array.key", AttributeValue::array_int([1L, 2L, 3L])),
    ("float_array.key", AttributeValue::array_float([1.1, 2.2])),
    ("bool_array.key", AttributeValue::array_bool([true, false]))
  ]
  
  // 验证属性数量
  assert_eq(attributes.length(), 8)
  
  // 验证特定属性
  assert_eq(attributes[0].0, "string.key")
  assert_eq(attributes[1].0, "int.key")
  assert_eq(attributes[2].0, "float.key")
  assert_eq(attributes[3].0, "bool.key")
  
  // 验证属性值类型
  assert_eq(attributes[0].1 is StringValue, true)
  assert_eq(attributes[1].1 is IntValue, true)
  assert_eq(attributes[2].1 is FloatValue, true)
  assert_eq(attributes[3].1 is BoolValue, true)
  assert_eq(attributes[4].1 is ArrayStringValue, true)
  assert_eq(attributes[5].1 is ArrayIntValue, true)
  assert_eq(attributes[6].1 is ArrayFloatValue, true)
  assert_eq(attributes[7].1 is ArrayBoolValue, true)
  
  // 验证特殊字符和边界值
  let special_chars = AttributeValue::string("special!@#$%^&*()_+-=[]{}|;':\",./<>?")
  let unicode_chars = AttributeValue::string("🌟 测试 🚀 emoji")
  let very_long_string = AttributeValue::string("a".repeat(1000))
  
  assert_eq(special_chars is StringValue, true)
  assert_eq(unicode_chars is StringValue, true)
  assert_eq(very_long_string is StringValue, true)
  
  // 验证数值边界
  let max_int = AttributeValue::int(9223372036854775807L)
  let min_int = AttributeValue::int(-9223372036854775808L)
  let very_large_float = AttributeValue::float(1.7976931348623157e+308)
  let very_small_float = AttributeValue::float(-1.7976931348623157e+308)
  
  assert_eq(max_int is IntValue, true)
  assert_eq(min_int is IntValue, true)
  assert_eq(very_large_float is FloatValue, true)
  assert_eq(very_small_float is FloatValue, true)
}