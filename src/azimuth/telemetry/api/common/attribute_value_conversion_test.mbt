// 属性值转换测试用例
// 测试AttributeValue的各种类型转换和操作

test "attribute_value_string_conversion" {
  // 测试字符串属性值的转换
  
  let string_attr = AttributeValue::string("test_value")
  let empty_string_attr = AttributeValue::string("")
  let special_string_attr = AttributeValue::string("special!@#$%^&*()_+{}|:<>?[]\\\\;'\",./")
  
  // 验证字符串属性值创建
  match string_attr {
    StringValue(value) => {
      assert_eq(value, "test_value")
      assert_eq(value.length(), 10)
      assert_eq(value.has_prefix("test"), true)
      assert_eq(value.has_suffix("value"), true)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证空字符串
  match empty_string_attr {
    StringValue(value) => {
      assert_eq(value, "")
      assert_eq(value.length(), 0)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证特殊字符字符串
  match special_string_attr {
    StringValue(value) => {
      assert_eq(value.length(), 32)
      assert_eq(value.contains("!@#$%"), true)
    }
    _ => assert_eq(false, true)
  }
}

test "attribute_value_numeric_conversion" {
  // 测试数值属性值的转换
  
  let int_attr = AttributeValue::int(12345L)
  let negative_int_attr = AttributeValue::int(-67890L)
  let zero_int_attr = AttributeValue::int(0L)
  let float_attr = AttributeValue::float(123.456)
  let negative_float_attr = AttributeValue::float(-789.012)
  let zero_float_attr = AttributeValue::float(0.0)
  
  // 验证整数属性值
  match int_attr {
    IntValue(value) => {
      assert_eq(value, 12345L)
      assert_eq(value > 10000L, true)
      assert_eq(value < 20000L, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证负整数属性值
  match negative_int_attr {
    IntValue(value) => {
      assert_eq(value, -67890L)
      assert_eq(value < 0L, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证零整数属性值
  match zero_int_attr {
    IntValue(value) => {
      assert_eq(value, 0L)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证浮点数属性值
  match float_attr {
    FloatValue(value) => {
      assert_eq(value > 100.0, true)
      assert_eq(value < 200.0, true)
      assert_eq(value - 123.456 < 0.001, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证负浮点数属性值
  match negative_float_attr {
    FloatValue(value) => {
      assert_eq(value < 0.0, true)
      assert_eq(value + 789.012 < 0.001, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证零浮点数属性值
  match zero_float_attr {
    FloatValue(value) => {
      assert_eq(value, 0.0)
    }
    _ => assert_eq(false, true)
  }
}

test "attribute_value_boolean_conversion" {
  // 测试布尔属性值的转换
  
  let true_attr = AttributeValue::bool(true)
  let false_attr = AttributeValue::bool(false)
  
  // 验证真值属性
  match true_attr {
    BoolValue(value) => {
      assert_eq(value, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证假值属性
  match false_attr {
    BoolValue(value) => {
      assert_eq(value, false)
    }
    _ => assert_eq(false, true)
  }
}

test "attribute_value_array_conversion" {
  // 测试数组属性值的转换
  
  let string_array = ["item1", "item2", "item3"]
  let int_array = [1L, 2L, 3L, 4L, 5L]
  let float_array = [1.1, 2.2, 3.3]
  let bool_array = [true, false, true, false]
  
  let string_array_attr = AttributeValue::array_string(string_array)
  let int_array_attr = AttributeValue::array_int(int_array)
  let float_array_attr = AttributeValue::array_float(float_array)
  let bool_array_attr = AttributeValue::array_bool(bool_array)
  
  // 验证字符串数组属性
  match string_array_attr {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "item1")
      assert_eq(values[1], "item2")
      assert_eq(values[2], "item3")
    }
    _ => assert_eq(false, true)
  }
  
  // 验证整数数组属性
  match int_array_attr {
    ArrayIntValue(values) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], 1L)
      assert_eq(values[2], 3L)
      assert_eq(values[4], 5L)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证浮点数数组属性
  match float_array_attr {
    ArrayFloatValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0] - 1.1 < 0.001, true)
      assert_eq(values[1] - 2.2 < 0.001, true)
      assert_eq(values[2] - 3.3 < 0.001, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证布尔数组属性
  match bool_array_attr {
    ArrayBoolValue(values) => {
      assert_eq(values.length(), 4)
      assert_eq(values[0], true)
      assert_eq(values[1], false)
      assert_eq(values[2], true)
      assert_eq(values[3], false)
    }
    _ => assert_eq(false, true)
  }
}

test "attribute_value_edge_cases" {
  // 测试属性值的边界情况
  
  // 测试空数组
  let empty_string_array = []
  let empty_int_array = []
  let empty_float_array = []
  let empty_bool_array = []
  
  let empty_string_array_attr = AttributeValue::array_string(empty_string_array)
  let empty_int_array_attr = AttributeValue::array_int(empty_int_array)
  let empty_float_array_attr = AttributeValue::array_float(empty_float_array)
  let empty_bool_array_attr = AttributeValue::array_bool(empty_bool_array)
  
  match empty_string_array_attr {
    ArrayStringValue(values) => assert_eq(values.length(), 0)
    _ => assert_eq(false, true)
  }
  
  match empty_int_array_attr {
    ArrayIntValue(values) => assert_eq(values.length(), 0)
    _ => assert_eq(false, true)
  }
  
  match empty_float_array_attr {
    ArrayFloatValue(values) => assert_eq(values.length(), 0)
    _ => assert_eq(false, true)
  }
  
  match empty_bool_array_attr {
    ArrayBoolValue(values) => assert_eq(values.length(), 0)
    _ => assert_eq(false, true)
  }
  
  // 测试极值
  let max_int = AttributeValue::int(9223372036854775807L)  // Int64最大值
  let min_int = AttributeValue::int(-9223372036854775808L)  // Int64最小值
  let max_float = AttributeValue::float(1.7976931348623157e+308)  // Double最大值
  let min_float = AttributeValue::float(-1.7976931348623157e+308)  // Double最小值
  
  match max_int {
    IntValue(value) => assert_eq(value, 9223372036854775807L)
    _ => assert_eq(false, true)
  }
  
  match min_int {
    IntValue(value) => assert_eq(value, -9223372036854775808L)
    _ => assert_eq(false, true)
  }
  
  match max_float {
    FloatValue(value) => assert_eq(value > 1.0e+308, true)
    _ => assert_eq(false, true)
  }
  
  match min_float {
    FloatValue(value) => assert_eq(value < -1.0e+308, true)
    _ => assert_eq(false, true)
  }
}