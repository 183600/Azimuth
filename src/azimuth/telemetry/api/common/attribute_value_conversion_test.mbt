// 属性值类型转换和操作测试用例

test "attribute_value_string_conversion" {
  // 测试字符串属性值的创建和转换
  
  let string_attr = AttributeValue::string("http.method")
  let string_value = "GET"
  
  // 验证字符串属性值
  match string_attr {
    StringValue(value) => assert_eq(value, "http.method")
    _ => assert_eq(false, true) // 不应该到达这里
  }
  
  // 创建字符串数组属性值
  let string_array = ["GET", "POST", "PUT", "DELETE"]
  let array_attr = AttributeValue::array_string(string_array)
  
  // 验证字符串数组属性值
  match array_attr {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 4)
      assert_eq(values[0], "GET")
      assert_eq(values[3], "DELETE")
    }
    _ => assert_eq(false, true) // 不应该到达这里
  }
}

test "attribute_value_numeric_conversion" {
  // 测试数值属性值的创建和转换
  
  let int_attr = AttributeValue::int(12345L)
  let float_attr = AttributeValue::float(67.89)
  
  // 验证整数属性值
  match int_attr {
    IntValue(value) => {
      assert_eq(value, 12345L)
      assert_eq(value > 10000L, true)
    }
    _ => assert_eq(false, true) // 不应该到达这里
  }
  
  // 验证浮点数属性值
  match float_attr {
    FloatValue(value) => {
      assert_eq(value > 60.0, true)
      assert_eq(value < 70.0, true)
    }
    _ => assert_eq(false, true) // 不应该到达这里
  }
  
  // 创建数值数组属性值
  let int_array = [100L, 200L, 300L, 400L]
  let int_array_attr = AttributeValue::array_int(int_array)
  
  let float_array = [1.1, 2.2, 3.3, 4.4]
  let float_array_attr = AttributeValue::array_float(float_array)
  
  // 验证整数数组属性值
  match int_array_attr {
    ArrayIntValue(values) => {
      assert_eq(values.length(), 4)
      assert_eq(values[0], 100L)
      assert_eq(values[3], 400L)
    }
    _ => assert_eq(false, true) // 不应该到达这里
  }
  
  // 验证浮点数数组属性值
  match float_array_attr {
    ArrayFloatValue(values) => {
      assert_eq(values.length(), 4)
      assert_eq(values[1], 2.2)
      assert_eq(values[2], 3.3)
    }
    _ => assert_eq(false, true) // 不应该到达这里
  }
}

test "attribute_value_boolean_conversion" {
  // 测试布尔属性值的创建和转换
  
  let true_attr = AttributeValue::bool(true)
  let false_attr = AttributeValue::bool(false)
  
  // 验证布尔属性值
  match true_attr {
    BoolValue(value) => assert_eq(value, true)
    _ => assert_eq(false, true) // 不应该到达这里
  }
  
  match false_attr {
    BoolValue(value) => assert_eq(value, false)
    _ => assert_eq(false, true) // 不应该到达这里
  }
  
  // 创建布尔数组属性值
  let bool_array = [true, false, true, false, true]
  let bool_array_attr = AttributeValue::array_bool(bool_array)
  
  // 验证布尔数组属性值
  match bool_array_attr {
    ArrayBoolValue(values) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], true)
      assert_eq(values[1], false)
      assert_eq(values[4], true)
    }
    _ => assert_eq(false, true) // 不应该到达这里
  }
}

test "attribute_value_type_safety" {
  // 测试属性值类型安全性
  
  let string_attr = AttributeValue::string("test_value")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14)
  let bool_attr = AttributeValue::bool(true)
  
  // 验证类型匹配
  let mut string_matched = false
  let mut int_matched = false
  let mut float_matched = false
  let mut bool_matched = false
  
  match string_attr {
    StringValue(_) => string_matched = true
    _ => assert_eq(false, true)
  }
  
  match int_attr {
    IntValue(_) => int_matched = true
    _ => assert_eq(false, true)
  }
  
  match float_attr {
    FloatValue(_) => float_matched = true
    _ => assert_eq(false, true)
  }
  
  match bool_attr {
    BoolValue(_) => bool_matched = true
    _ => assert_eq(false, true)
  }
  
  assert_eq(string_matched, true)
  assert_eq(int_matched, true)
  assert_eq(float_matched, true)
  assert_eq(bool_matched, true)
}

test "attribute_value_edge_cases" {
  // 测试属性值边界情况
  
  // 空字符串
  let empty_string = AttributeValue::string("")
  match empty_string {
    StringValue(value) => assert_eq(value.length(), 0)
    _ => assert_eq(false, true)
  }
  
  // 零值
  let zero_int = AttributeValue::int(0L)
  let zero_float = AttributeValue::float(0.0)
  
  match zero_int {
    IntValue(value) => assert_eq(value, 0L)
    _ => assert_eq(false, true)
  }
  
  match zero_float {
    FloatValue(value) => assert_eq(value, 0.0)
    _ => assert_eq(false, true)
  }
  
  // 空数组
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  
  match empty_string_array {
    ArrayStringValue(values) => assert_eq(values.length(), 0)
    _ => assert_eq(false, true)
  }
  
  match empty_int_array {
    ArrayIntValue(values) => assert_eq(values.length(), 0)
    _ => assert_eq(false, true)
  }
}

test "attribute_value_complex_operations" {
  // 测试属性值复杂操作
  
  // 创建混合类型的属性值数组
  let attributes = [
    ("string_attr", AttributeValue::string("test_string")),
    ("int_attr", AttributeValue::int(100L)),
    ("float_attr", AttributeValue::float(99.99)),
    ("bool_attr", AttributeValue::bool(true)),
    ("array_attr", AttributeValue::array_string(["a", "b", "c"]))
  ]
  
  // 验证属性数量
  assert_eq(attributes.length(), 5)
  
  // 验证每个属性的类型和值
  assert_eq(attributes[0].0, "string_attr")
  match attributes[0].1 {
    StringValue(value) => assert_eq(value, "test_string")
    _ => assert_eq(false, true)
  }
  
  assert_eq(attributes[1].0, "int_attr")
  match attributes[1].1 {
    IntValue(value) => assert_eq(value, 100L)
    _ => assert_eq(false, true)
  }
  
  assert_eq(attributes[2].0, "float_attr")
  match attributes[2].1 {
    FloatValue(value) => assert_eq(value > 99.0, true)
    _ => assert_eq(false, true)
  }
  
  assert_eq(attributes[3].0, "bool_attr")
  match attributes[3].1 {
    BoolValue(value) => assert_eq(value, true)
    _ => assert_eq(false, true)
  }
  
  assert_eq(attributes[4].0, "array_attr")
  match attributes[4].1 {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "a")
      assert_eq(values[2], "c")
    }
    _ => assert_eq(false, true)
  }
}