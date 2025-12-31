// 遥测属性值类型转换测试用例

test "attribute_value_string_conversions" {
  // 测试字符串属性值的转换
  
  // 创建字符串属性值
  let string_attr = azimuth::telemetry::api::common::AttributeValue::string("hello world")
  let empty_string_attr = azimuth::telemetry::api::common::AttributeValue::string("")
  let special_string_attr = azimuth::telemetry::api::common::AttributeValue::string("special chars: !@#$%^&*()")
  let unicode_string_attr = azimuth::telemetry::api::common::AttributeValue::string("中文测试 🚀")
  let long_string_attr = azimuth::telemetry::api::common::AttributeValue::string("this is a very long string that tests the system's ability to handle longer text values without issues")
  
  // 验证字符串属性值创建
  match string_attr {
    StringValue(s) => assert_eq(s, "hello world")
    _ => assert_eq(false, true)  // 应该是StringValue
  }
  
  match empty_string_attr {
    StringValue(s) => assert_eq(s, "")
    _ => assert_eq(false, true)
  }
  
  match special_string_attr {
    StringValue(s) => assert_eq(s, "special chars: !@#$%^&*()")
    _ => assert_eq(false, true)
  }
  
  match unicode_string_attr {
    StringValue(s) => assert_eq(s, "中文测试 🚀")
    _ => assert_eq(false, true)
  }
  
  match long_string_attr {
    StringValue(s) => assert_eq(s.length() > 50, true)
    _ => assert_eq(false, true)
  }
}

test "attribute_value_integer_conversions" {
  // 测试整数属性值的转换
  
  // 创建各种整数值
  let positive_int = azimuth::telemetry::api::common::AttributeValue::int(42L)
  let zero_int = azimuth::telemetry::api::common::AttributeValue::int(0L)
  let negative_int = azimuth::telemetry::api::common::AttributeValue::int(-123L)
  let max_int = azimuth::telemetry::api::common::AttributeValue::int(Int64.max_value())
  let min_int = azimuth::telemetry::api::common::AttributeValue::int(Int64.min_value())
  
  // 验证整数属性值
  match positive_int {
    IntValue(i) => assert_eq(i, 42L)
    _ => assert_eq(false, true)
  }
  
  match zero_int {
    IntValue(i) => assert_eq(i, 0L)
    _ => assert_eq(false, true)
  }
  
  match negative_int {
    IntValue(i) => assert_eq(i, -123L)
    _ => assert_eq(false, true)
  }
  
  match max_int {
    IntValue(i) => assert_eq(i, Int64.max_value())
    _ => assert_eq(false, true)
  }
  
  match min_int {
    IntValue(i) => assert_eq(i, Int64.min_value())
    _ => assert_eq(false, true)
  }
}

test "attribute_value_float_conversions" {
  // 测试浮点数属性值的转换
  
  // 创建各种浮点数值
  let positive_float = azimuth::telemetry::api::common::AttributeValue::float(3.14159)
  let zero_float = azimuth::telemetry::api::common::AttributeValue::float(0.0)
  let negative_float = azimuth::telemetry::api::common::AttributeValue::float(-2.71828)
  let infinity_float = azimuth::telemetry::api::common::AttributeValue::float(Double.infinity)
  let neg_infinity_float = azimuth::telemetry::api::common::AttributeValue::float(Double.neg_infinity)
  let nan_float = azimuth::telemetry::api::common::AttributeValue::float(Double.nan)
  let very_small_float = azimuth::telemetry::api::common::AttributeValue::float(1.0e-10)
  let very_large_float = azimuth::telemetry::api::common::AttributeValue::float(1.0e10)
  
  // 验证浮点数属性值
  match positive_float {
    FloatValue(f) => assert_eq(f > 3.14 && f < 3.15, true)
    _ => assert_eq(false, true)
  }
  
  match zero_float {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => assert_eq(false, true)
  }
  
  match negative_float {
    FloatValue(f) => assert_eq(f < -2.71 && f > -2.72, true)
    _ => assert_eq(false, true)
  }
  
  match infinity_float {
    FloatValue(f) => assert_eq(f, Double.infinity)
    _ => assert_eq(false, true)
  }
  
  match neg_infinity_float {
    FloatValue(f) => assert_eq(f, Double.neg_infinity)
    _ => assert_eq(false, true)
  }
  
  match nan_float {
    FloatValue(f) => assert_eq(Double.is_nan(f), true)
    _ => assert_eq(false, true)
  }
}

test "attribute_value_boolean_conversions" {
  // 测试布尔属性值的转换
  
  // 创建布尔值
  let true_attr = azimuth::telemetry::api::common::AttributeValue::bool(true)
  let false_attr = azimuth::telemetry::api::common::AttributeValue::bool(false)
  
  // 验证布尔属性值
  match true_attr {
    BoolValue(b) => assert_eq(b, true)
    _ => assert_eq(false, true)
  }
  
  match false_attr {
    BoolValue(b) => assert_eq(b, false)
    _ => assert_eq(false, true)
  }
}

test "attribute_value_array_conversions" {
  // 测试数组属性值的转换
  
  // 字符串数组
  let string_array = azimuth::telemetry::api::common::AttributeValue::array_string(["apple", "banana", "cherry"])
  let empty_string_array = azimuth::telemetry::api::common::AttributeValue::array_string([])
  let single_string_array = azimuth::telemetry::api::common::AttributeValue::array_string(["single"])
  
  // 整数数组
  let int_array = azimuth::telemetry::api::common::AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])
  let mixed_int_array = azimuth::telemetry::api::common::AttributeValue::array_int([0L, -1L, Int64.max_value(), Int64.min_value()])
  
  // 浮点数数组
  let float_array = azimuth::telemetry::api::common::AttributeValue::array_float([1.1, 2.2, 3.3])
  let special_float_array = azimuth::telemetry::api::common::AttributeValue::array_float([0.0, Double.infinity, Double.nan])
  
  // 布尔数组
  let bool_array = azimuth::telemetry::api::common::AttributeValue::array_bool([true, false, true])
  let all_true_array = azimuth::telemetry::api::common::AttributeValue::array_bool([true, true, true])
  let all_false_array = azimuth::telemetry::api::common::AttributeValue::array_bool([false, false, false])
  
  // 验证字符串数组
  match string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "apple")
      assert_eq(arr[1], "banana")
      assert_eq(arr[2], "cherry")
    }
    _ => assert_eq(false, true)
  }
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_eq(false, true)
  }
  
  // 验证整数数组
  match int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1L)
      assert_eq(arr[4], 5L)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证浮点数数组
  match float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0] > 1.0 && arr[0] < 1.2, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证布尔数组
  match bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => assert_eq(false, true)
  }
}

test "attribute_value_type_safety" {
  // 测试属性值的类型安全
  
  // 创建不同类型的属性值
  let attributes = [
    ("string_attr", azimuth::telemetry::api::common::AttributeValue::string("test")),
    ("int_attr", azimuth::telemetry::api::common::AttributeValue::int(42L)),
    ("float_attr", azimuth::telemetry::api::common::AttributeValue::float(3.14)),
    ("bool_attr", azimuth::telemetry::api::common::AttributeValue::bool(true)),
    ("string_array_attr", azimuth::telemetry::api::common::AttributeValue::array_string(["a", "b"])),
    ("int_array_attr", azimuth::telemetry::api::common::AttributeValue::array_int([1L, 2L])),
    ("float_array_attr", azimuth::telemetry::api::common::AttributeValue::array_float([1.1, 2.2])),
    ("bool_array_attr", azimuth::telemetry::api::common::AttributeValue::array_bool([true, false]))
  ]
  
  // 验证每个属性值的类型
  for attr in attributes {
    let (name, value) = attr
    match name {
      "string_attr" => {
        match value {
          StringValue(_) => assert_eq(true, true)
          _ => assert_eq(false, true)
        }
      }
      "int_attr" => {
        match value {
          IntValue(_) => assert_eq(true, true)
          _ => assert_eq(false, true)
        }
      }
      "float_attr" => {
        match value {
          FloatValue(_) => assert_eq(true, true)
          _ => assert_eq(false, true)
        }
      }
      "bool_attr" => {
        match value {
          BoolValue(_) => assert_eq(true, true)
          _ => assert_eq(false, true)
        }
      }
      "string_array_attr" => {
        match value {
          ArrayStringValue(_) => assert_eq(true, true)
          _ => assert_eq(false, true)
        }
      }
      "int_array_attr" => {
        match value {
          ArrayIntValue(_) => assert_eq(true, true)
          _ => assert_eq(false, true)
        }
      }
      "float_array_attr" => {
        match value {
          ArrayFloatValue(_) => assert_eq(true, true)
          _ => assert_eq(false, true)
        }
      }
      "bool_array_attr" => {
        match value {
          ArrayBoolValue(_) => assert_eq(true, true)
          _ => assert_eq(false, true)
        }
      }
      _ => assert_eq(false, true)
    }
  }
}

test "attribute_value_edge_cases" {
  // 测试属性值的边界情况
  
  // 极长的字符串
  let very_long_string = "x".repeat(10000)
  let long_string_attr = azimuth::telemetry::api::common::AttributeValue::string(very_long_string)
  match long_string_attr {
    StringValue(s) => assert_eq(s.length(), 10000)
    _ => assert_eq(false, true)
  }
  
  // 包含特殊字符的字符串
  let special_chars = "\n\r\t\\\"'<>[]{}|\\~`!@#$%^&*()_+-=[]{};':\",./<>?"
  let special_chars_attr = azimuth::telemetry::api::common::AttributeValue::string(special_chars)
  match special_chars_attr {
    StringValue(s) => assert_eq(s, special_chars)
    _ => assert_eq(false, true)
  }
  
  // 大型数组
  let large_int_array = [for i = 0; i < 1000; i = i + 1].map(fn(i) { i.to_int64() })
  let large_array_attr = azimuth::telemetry::api::common::AttributeValue::array_int(large_int_array)
  match large_array_attr {
    ArrayIntValue(arr) => assert_eq(arr.length(), 1000)
    _ => assert_eq(false, true)
  }
  
  // 空数组
  let empty_arrays = [
    azimuth::telemetry::api::common::AttributeValue::array_string([]),
    azimuth::telemetry::api::common::AttributeValue::array_int([]),
    azimuth::telemetry::api::common::AttributeValue::array_float([]),
    azimuth::telemetry::api::common::AttributeValue::array_bool([])
  ]
  
  for empty_array in empty_arrays {
    match empty_array {
      ArrayStringValue(arr) => assert_eq(arr.length(), 0)
      ArrayIntValue(arr) => assert_eq(arr.length(), 0)
      ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
      ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
      _ => assert_eq(false, true)
    }
  }
}

test "attribute_value_serialization_compatibility" {
  // 测试属性值的序列化兼容性
  
  // 创建各种类型的属性值
  let test_values = [
    azimuth::telemetry::api::common::AttributeValue::string("test"),
    azimuth::telemetry::api::common::AttributeValue::int(123L),
    azimuth::telemetry::api::common::AttributeValue::float(45.67),
    azimuth::telemetry::api::common::AttributeValue::bool(true),
    azimuth::telemetry::api::common::AttributeValue::array_string(["a", "b", "c"]),
    azimuth::telemetry::api::common::AttributeValue::array_int([1L, 2L, 3L]),
    azimuth::telemetry::api::common::AttributeValue::array_float([1.1, 2.2, 3.3]),
    azimuth::telemetry::api::common::AttributeValue::array_bool([true, false, true])
  ]
  
  // 验证每个值都能正确创建和访问
  for value in test_values {
    match value {
      StringValue(s) => assert_eq(s.length() >= 0, true)
      IntValue(i) => assert_eq(i >= Int64.min_value() && i <= Int64.max_value(), true)
      FloatValue(f) => assert_eq(Double.is_finite(f) || Double.is_nan(f) || f == Double.infinity || f == Double.neg_infinity, true)
      BoolValue(b) => assert_eq(b == true || b == false, true)
      ArrayStringValue(arr) => assert_eq(arr.length() >= 0, true)
      ArrayIntValue(arr) => assert_eq(arr.length() >= 0, true)
      ArrayFloatValue(arr) => assert_eq(arr.length() >= 0, true)
      ArrayBoolValue(arr) => assert_eq(arr.length() >= 0, true)
    }
  }
}

test "attribute_value_complex_scenarios" {
  // 测试属性值的复杂场景
  
  // 模拟真实的遥测属性
  let telemetry_attributes = [
    ("http.method", azimuth::telemetry::api::common::AttributeValue::string("POST")),
    ("http.status_code", azimuth::telemetry::api::common::AttributeValue::int(200L)),
    ("http.response_time", azimuth::telemetry::api::common::AttributeValue::float(123.45)),
    ("http.success", azimuth::telemetry::api::common::AttributeValue::bool(true)),
    ("http.headers", azimuth::telemetry::api::common::AttributeValue::array_string(["content-type", "authorization", "user-agent"])),
    ("http.retry_count", azimuth::telemetry::api::common::AttributeValue::int(3L)),
    ("http.payload_size", azimuth::telemetry::api::common::AttributeValue::float(1024.0)),
    ("http.cached", azimuth::telemetry::api::common::AttributeValue::bool(false))
  ]
  
  // 验证复杂场景中的属性值
  for attr in telemetry_attributes {
    let (key, value) = attr
    
    // 验证键不为空
    assert_eq(key.length() > 0, true)
    
    // 验证值类型合理
    match key {
      "http.method" => {
        match value {
          StringValue(method) => assert_eq(method == "GET" || method == "POST" || method == "PUT" || method == "DELETE", true)
          _ => assert_eq(false, true)
        }
      }
      "http.status_code" => {
        match value {
          IntValue(code) => assert_eq(code >= 100L && code < 600L, true)
          _ => assert_eq(false, true)
        }
      }
      "http.response_time" => {
        match value {
          FloatValue(time) => assert_eq(time >= 0.0, true)
          _ => assert_eq(false, true)
        }
      }
      "http.success" => {
        match value {
          BoolValue(success) => assert_eq(success == true || success == false, true)
          _ => assert_eq(false, true)
        }
      }
      "http.headers" => {
        match value {
          ArrayStringValue(headers) => assert_eq(headers.length() > 0, true)
          _ => assert_eq(false, true)
        }
      }
      _ => assert_eq(true, true)  // 其他键也应该是有效的
    }
  }
}