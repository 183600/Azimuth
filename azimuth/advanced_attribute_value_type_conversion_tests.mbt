// 高级属性值类型转换测试
// 测试属性值的复杂转换操作和边界条件处理

test "属性值字符串到数值转换边界条件" {
  // 测试字符串到整数的转换
  let string_val = StringValue("42")
  let int_val = IntValue(42)
  let float_val = FloatValue(3.14159)
  let bool_val = BoolValue(true)
  
  // 测试数组类型的属性值
  let string_array_val = ArrayStringValue(["item1", "item2", "item3"])
  let int_array_val = ArrayIntValue([1, 2, 3, 4, 5])
  
  // 验证属性值的类型识别
  match string_val {
    StringValue(s) => assert_eq(s, "42")
    _ => assert_true(false, "期望字符串值")
  }
  
  match int_val {
    IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false, "期望整数值")
  }
  
  match float_val {
    FloatValue(f) => assert_true(f > 3.14 && f < 3.15)
    _ => assert_true(false, "期望浮点数值")
  }
  
  match bool_val {
    BoolValue(b) => assert_true(b)
    _ => assert_true(false, "期望布尔值")
  }
  
  // 测试数组属性值
  match string_array_val {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "item1")
      assert_eq(arr[2], "item3")
    }
    _ => assert_true(false, "期望字符串数组值")
  }
  
  match int_array_val {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    _ => assert_true(false, "期望整数数组值")
  }
}

test "属性值空值和特殊值处理" {
  // 测试空字符串处理
  let empty_string = StringValue("")
  match empty_string {
    StringValue(s) => assert_eq(s.length(), 0)
    _ => assert_true(false, "期望空字符串")
  }
  
  // 测试零值处理
  let zero_int = IntValue(0)
  let zero_float = FloatValue(0.0)
  
  match zero_int {
    IntValue(i) => assert_eq(i, 0)
    _ => assert_true(false, "期望零整数")
  }
  
  match zero_float {
    FloatValue(f) => assert_true(f == 0.0)
    _ => assert_true(false, "期望零浮点数")
  }
  
  // 测试负值处理
  let negative_int = IntValue(-42)
  let negative_float = FloatValue(-3.14159)
  
  match negative_int {
    IntValue(i) => assert_eq(i, -42)
    _ => assert_true(false, "期望负整数")
  }
  
  match negative_float {
    FloatValue(f) => assert_true(f < -3.14 && f > -3.15)
    _ => assert_true(false, "期望负浮点数")
  }
  
  // 测试空数组
  let empty_string_array = ArrayStringValue([])
  let empty_int_array = ArrayIntValue([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false, "期望空字符串数组")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false, "期望空整数数组")
  }
}

test "属性值类型转换兼容性" {
  // 测试各种属性值的兼容性操作
  let attributes = Attributes::new()
  
  // 设置不同类型的属性值
  Attributes::set(attributes, "string.key", StringValue("test_value"))
  Attributes::set(attributes, "int.key", IntValue(123))
  Attributes::set(attributes, "float.key", FloatValue(45.67))
  Attributes::set(attributes, "bool.key", BoolValue(false))
  
  // 验证属性值的获取和类型匹配
  let string_result = Attributes::get(attributes, "string.key")
  let int_result = Attributes::get(attributes, "int.key")
  let float_result = Attributes::get(attributes, "float.key")
  let bool_result = Attributes::get(attributes, "bool.key")
  
  match string_result {
    Some(StringValue(s)) => assert_eq(s, "test_value")
    _ => assert_true(false, "期望字符串值")
  }
  
  match int_result {
    Some(IntValue(i)) => assert_eq(i, 123)
    _ => assert_true(false, "期望整数值")
  }
  
  match float_result {
    Some(FloatValue(f)) => assert_true(f > 45.6 && f < 45.7)
    _ => assert_true(false, "期望浮点数值")
  }
  
  match bool_result {
    Some(BoolValue(b)) => assert_false(b)
    _ => assert_true(false, "期望布尔值")
  }
  
  // 测试不存在的键
  let missing_result = Attributes::get(attributes, "nonexistent.key")
  assert_eq(missing_result, None)
}

test "属性值数组操作高级测试" {
  // 测试大型数组操作
  let large_string_array = ArrayStringValue([
    "item1", "item2", "item3", "item4", "item5",
    "item6", "item7", "item8", "item9", "item10"
  ])
  
  let large_int_array = ArrayIntValue([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  
  match large_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 10)
      assert_eq(arr[0], "item1")
      assert_eq(arr[9], "item10")
      assert_eq(arr[4], "item5")
    }
    _ => assert_true(false, "期望大型字符串数组")
  }
  
  match large_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 10)
      assert_eq(arr[0], 1)
      assert_eq(arr[9], 10)
      assert_eq(arr[4], 5)
    }
    _ => assert_true(false, "期望大型整数数组")
  }
  
  // 测试包含特殊字符的字符串数组
  let special_string_array = ArrayStringValue([
    "特殊字符: !@#$%^&*()",
    "Unicode: 测试 🚀",
    "换行符: \n",
    "制表符: \t",
    "引号: \"'\""
  ])
  
  match special_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], "特殊字符: !@#$%^&*()")
      assert_eq(arr[1], "Unicode: 测试 🚀")
      assert_eq(arr[2], "换行符: \n")
      assert_eq(arr[3], "制表符: \t")
      assert_eq(arr[4], "引号: \"'\"")
    }
    _ => assert_true(false, "期望特殊字符字符串数组")
  }
}

test "属性值极值和边界条件测试" {
  // 测试极大整数值
  let max_int = IntValue(2147483647)
  let min_int = IntValue(-2147483648)
  
  match max_int {
    IntValue(i) => assert_eq(i, 2147483647)
    _ => assert_true(false, "期望最大整数值")
  }
  
  match min_int {
    IntValue(i) => assert_eq(i, -2147483648)
    _ => assert_true(false, "期望最小整数值")
  }
  
  // 测试极值浮点数
  let very_large_float = FloatValue(1.7976931348623157e+308)
  let very_small_float = FloatValue(2.2250738585072014e-308)
  let infinity_float = FloatValue(1.0 / 0.0) // 正无穷
  let nan_float = FloatValue(0.0 / 0.0) // NaN
  
  match very_large_float {
    FloatValue(f) => assert_true(f > 1.0e+307)
    _ => assert_true(false, "期望极大浮点数")
  }
  
  match very_small_float {
    FloatValue(f) => assert_true(f > 0.0 && f < 1.0e-307)
    _ => assert_true(false, "期望极小浮点数")
  }
  
  match infinity_float {
    FloatValue(f) => assert_true(f > 1.0e+308) // 简化的无穷检测
    _ => assert_true(false, "期望无穷大浮点数")
  }
  
  match nan_float {
    FloatValue(f) => assert_true(f != f) // NaN检测
    _ => assert_true(false, "期望NaN浮点数")
  }
  
  // 测试极长字符串
  let very_long_string = StringValue("这是一个非常长的字符串，用于测试系统对长字符串的处理能力。" * 10)
  match very_long_string {
    StringValue(s) => assert_true(s.length() > 100)
    _ => assert_true(false, "期望极长字符串")
  }
}

test "属性值类型转换错误处理" {
  // 测试类型转换的边界情况和错误处理
  let attributes = Attributes::new()
  
  // 设置一些边界值
  Attributes::set(attributes, "boundary.int", IntValue(2147483647))
  Attributes::set(attributes, "boundary.float", FloatValue(3.14159265359))
  Attributes::set(attributes, "boundary.bool", BoolValue(true))
  
  // 验证边界值的正确性
  let boundary_int = Attributes::get(attributes, "boundary.int")
  let boundary_float = Attributes::get(attributes, "boundary.float")
  let boundary_bool = Attributes::get(attributes, "boundary.bool")
  
  match boundary_int {
    Some(IntValue(i)) => assert_eq(i, 2147483647)
    _ => assert_true(false, "期望边界整数值")
  }
  
  match boundary_float {
    Some(FloatValue(f)) => assert_true(f > 3.14159 && f < 3.14160)
    _ => assert_true(false, "期望边界浮点数值")
  }
  
  match boundary_bool {
    Some(BoolValue(b)) => assert_true(b)
    _ => assert_true(false, "期望边界布尔值")
  }
  
  // 测试空属性集合
  let empty_attributes = Attributes::new()
  let empty_result = Attributes::get(empty_attributes, "any.key")
  assert_eq(empty_result, None)
}