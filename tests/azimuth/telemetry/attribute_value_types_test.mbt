// Azimuth Telemetry System - Attribute Value Types Comprehensive Test
// 测试Azimuth遥测系统的属性值类型全面功能

test "属性值类型基础测试" {
  // 测试所有AttributeValue枚举类型的创建和匹配
  
  // 字符串值类型
  let string_val = StringValue("test_string")
  assert_eq(match string_val {
    StringValue(s) => s
    _ => "not_string"
  }, "test_string")
  
  // 整数值类型
  let int_val = IntValue(42)
  assert_eq(match int_val {
    IntValue(i) => i
    _ => 0
  }, 42)
  
  // 浮点数值类型
  let float_val = FloatValue(3.14159)
  assert_eq(match float_val {
    FloatValue(f) => f
    _ => 0.0
  }, 3.14159)
  
  // 布尔值类型
  let bool_val = BoolValue(true)
  assert_eq(match bool_val {
    BoolValue(b) => b
    _ => false
  }, true)
  
  // 字符串数组值类型
  let string_array_val = ArrayStringValue(["item1", "item2", "item3"])
  assert_eq(match string_array_val {
    ArrayStringValue(arr) => arr.length()
    _ => 0
  }, 3)
  
  // 整数数组值类型
  let int_array_val = ArrayIntValue([1, 2, 3, 4, 5])
  assert_eq(match int_array_val {
    ArrayIntValue(arr) => arr.length()
    _ => 0
  }, 5)
}

test "属性值边界值测试" {
  // 测试边界值和特殊值
  
  // 零值和负数
  let zero_int = IntValue(0)
  let neg_int = IntValue(-42)
  let zero_float = FloatValue(0.0)
  let neg_float = FloatValue(-3.14)
  
  assert_eq(match zero_int { IntValue(i) => i _ => 0 }, 0)
  assert_eq(match neg_int { IntValue(i) => i _ => 0 }, -42)
  assert_eq(match zero_float { FloatValue(f) => f _ => 0.0 }, 0.0)
  assert_eq(match neg_float { FloatValue(f) => f _ => 0.0 }, -3.14)
  
  // 最大值和最小值
  let max_int = IntValue(2147483647)
  let min_int = IntValue(-2147483648)
  let max_float = FloatValue(1.7976931348623157e+308)
  let min_float = FloatValue(-1.7976931348623157e+308)
  
  assert_eq(match max_int { IntValue(i) => i _ => 0 }, 2147483647)
  assert_eq(match min_int { IntValue(i) => i _ => 0 }, -2147483648)
  assert_eq(match max_float { FloatValue(f) => f _ => 0.0 }, 1.7976931348623157e+308)
  assert_eq(match min_float { FloatValue(f) => f _ => 0.0 }, -1.7976931348623157e+308)
  
  // 空数组和单元素数组
  let empty_string_array = ArrayStringValue([])
  let single_string_array = ArrayStringValue(["single"])
  let empty_int_array = ArrayIntValue([])
  let single_int_array = ArrayIntValue([42])
  
  assert_eq(match empty_string_array { ArrayStringValue(arr) => arr.length() _ => 0 }, 0)
  assert_eq(match single_string_array { ArrayStringValue(arr) => arr.length() _ => 0 }, 1)
  assert_eq(match empty_int_array { ArrayIntValue(arr) => arr.length() _ => 0 }, 0)
  assert_eq(match single_int_array { ArrayIntValue(arr) => arr.length() _ => 0 }, 1)
}

test "属性值特殊字符测试" {
  // 测试包含特殊字符的字符串值
  
  // 包含特殊字符的字符串
  let special_chars = StringValue("特殊字符: !@#$%^&*()_+-=[]{}|;':\",./<>?")
  let unicode_chars = StringValue("Unicode: 🚀 🌟 💻 📊")
  let empty_string = StringValue("")
  let whitespace_string = StringValue("   \t\n\r   ")
  
  assert_eq(match special_chars { StringValue(s) => s.length() > 10 _ => false }, true)
  assert_eq(match unicode_chars { StringValue(s) => s.contains("🚀") _ => false }, true)
  assert_eq(match empty_string { StringValue(s) => s.length() _ => 1 }, 0)
  assert_eq(match whitespace_string { StringValue(s) => s.trim().length() _ => 1 }, 0)
  
  // 包含特殊字符的字符串数组
  let special_array = ArrayStringValue([
    "item with spaces",
    "item-with-dashes",
    "item_with_underscores",
    "item.with.dots",
    "item/with/slashes"
  ])
  
  assert_eq(match special_array { ArrayStringValue(arr) => arr.length() _ => 0 }, 5)
}

test "属性值类型转换测试" {
  // 测试属性值类型的逻辑转换
  
  // 布尔值的真值测试
  let true_val = BoolValue(true)
  let false_val = BoolValue(false)
  
  assert_eq(match true_val { BoolValue(b) => b _ => false }, true)
  assert_eq(match false_val { BoolValue(b) => b _ => true }, false)
  
  // 数值类型的精度测试
  let precise_float = FloatValue(123.456789012345)
  let scientific_float = FloatValue(1.23e-4)
  let large_float = FloatValue(1.23e+10)
  
  assert_eq(match precise_float { FloatValue(f) => f > 123.0 _ => false }, true)
  assert_eq(match scientific_float { FloatValue(f) => f < 0.001 _ => false }, true)
  assert_eq(match large_float { FloatValue(f) => f > 10000000000.0 _ => false }, true)
  
  // 数组元素的访问测试
  let string_array = ArrayStringValue(["first", "second", "third"])
  let int_array = ArrayIntValue([10, 20, 30, 40])
  
  let first_string = match string_array {
    ArrayStringValue(arr) => arr[0]
    _ => "default"
  }
  let last_int = match int_array {
    ArrayIntValue(arr) => arr[arr.length() - 1]
    _ => 0
  }
  
  assert_eq(first_string, "first")
  assert_eq(last_int, 40)
}

test "属性值组合测试" {
  // 测试属性值的组合使用
  
  // 创建多种类型的属性值
  let attributes = [
    ("string_attr", StringValue("string_value")),
    ("int_attr", IntValue(123)),
    ("float_attr", FloatValue(45.67)),
    ("bool_attr", BoolValue(true)),
    ("string_array_attr", ArrayStringValue(["a", "b", "c"])),
    ("int_array_attr", ArrayIntValue([1, 2, 3]))
  ]
  
  // 验证属性数量
  assert_eq(attributes.length(), 6)
  
  // 验证每个属性的类型和值
  for attr in attributes {
    match attr.1 {
      StringValue(s) => assert_eq(s, "string_value")
      IntValue(i) => assert_eq(i, 123)
      FloatValue(f) => assert_eq(f, 45.67)
      BoolValue(b) => assert_eq(b, true)
      ArrayStringValue(arr) => assert_eq(arr.length(), 3)
      ArrayIntValue(arr) => assert_eq(arr.length(), 3)
    }
  } else {
    assert_true(false) // 不应该到达这里
  }
}

test "属性值性能测试" {
  // 测试属性值的创建和匹配性能
  
  // 创建大量属性值
  let mut string_values : Array[StringValue] = []
  let mut int_values : Array[IntValue] = []
  let mut float_values : Array[FloatValue] = []
  
  for i = 0; i < 100; i = i + 1 {
    string_values.push(StringValue("string_" + i.to_string()))
    int_values.push(IntValue(i))
    float_values.push(FloatValue(i.to_double() * 1.5))
  }
  
  // 验证数量
  assert_eq(string_values.length(), 100)
  assert_eq(int_values.length(), 100)
  assert_eq(float_values.length(), 100)
  
  // 验证边界值
  let first_string = match string_values[0] { StringValue(s) => s _ => "" }
  let last_int = match int_values[99] { IntValue(i) => i _ => 0 }
  let middle_float = match float_values[50] { FloatValue(f) => f _ => 0.0 }
  
  assert_eq(first_string, "string_0")
  assert_eq(last_int, 99)
  assert_eq(middle_float, 75.0)
}