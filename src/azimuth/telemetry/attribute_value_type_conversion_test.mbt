// 属性值类型转换测试用例
// 测试不同类型之间的转换和兼容性

test "attribute_value_string_conversion" {
  // 测试字符串类型的属性值转换
  
  let string_attr = @azimuth.telemetry.api.common.AttributeValue::string("test_value")
  let int_attr = @azimuth.telemetry.api.common.AttributeValue::int(42L)
  let float_attr = @azimuth.telemetry.api.common.AttributeValue::float(3.14)
  let bool_attr = @azimuth.telemetry.api.common.AttributeValue::bool(true)
  
  // 验证字符串属性
  match string_attr {
    @azimuth.telemetry.api.common.StringValue(value) => {
      assert_eq(value, "test_value")
      assert_eq(value.length(), 10)
    }
    _ => assert_eq(false, true) // 不应该到达这里
  }
  
  // 验证整数属性转换为字符串表示
  match int_attr {
    @azimuth.telemetry.api.common.IntValue(value) => {
      assert_eq(value, 42L)
      let value_str = value.to_string()
      assert_eq(value_str, "42")
    }
    _ => assert_eq(false, true) // 不应该到达这里
  }
  
  // 验证浮点数属性转换为字符串表示
  match float_attr {
    @azimuth.telemetry.api.common.FloatValue(value) => {
      assert_eq(value > 3.0, true)
      assert_eq(value < 3.2, true)
      let value_str = value.to_string()
      assert_eq(value_str.has_prefix("3.1"), true)
    }
    _ => assert_eq(false, true) // 不应该到达这里
  }
  
  // 验证布尔属性转换为字符串表示
  match bool_attr {
    @azimuth.telemetry.api.common.BoolValue(value) => {
      assert_eq(value, true)
      let value_str = value.to_string()
      assert_eq(value_str, "true")
    }
    _ => assert_eq(false, true) // 不应该到达这里
  }
}

test "attribute_value_array_conversion" {
  // 测试数组类型的属性值转换
  
  let string_array = @azimuth.telemetry.api.common.AttributeValue::array_string(["a", "b", "c"])
  let int_array = @azimuth.telemetry.api.common.AttributeValue::array_int([1L, 2L, 3L])
  let float_array = @azimuth.telemetry.api.common.AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array = @azimuth.telemetry.api.common.AttributeValue::array_bool([true, false, true])
  
  // 验证字符串数组
  match string_array {
    @azimuth.telemetry.api.common.ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "a")
      assert_eq(values[2], "c")
    }
    _ => assert_eq(false, true)
  }
  
  // 验证整数数组
  match int_array {
    @azimuth.telemetry.api.common.ArrayIntValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], 1L)
      assert_eq(values[1], 2L)
      assert_eq(values[2], 3L)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证浮点数数组
  match float_array {
    @azimuth.telemetry.api.common.ArrayFloatValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0] > 1.0, true)
      assert_eq(values[2] > 3.0, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证布尔数组
  match bool_array {
    @azimuth.telemetry.api.common.ArrayBoolValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], true)
      assert_eq(values[1], false)
      assert_eq(values[2], true)
    }
    _ => assert_eq(false, true)
  }
}

test "attribute_value_type_safety" {
  // 测试属性值的类型安全性
  
  let attributes = [
    ("string_key", @azimuth.telemetry.api.common.AttributeValue::string("string_value")),
    ("int_key", @azimuth.telemetry.api.common.AttributeValue::int(123L)),
    ("float_key", @azimuth.telemetry.api.common.AttributeValue::float(45.67)),
    ("bool_key", @azimuth.telemetry.api.common.AttributeValue::bool(false))
  ]
  
  // 验证属性数组长度
  assert_eq(attributes.length(), 4)
  
  // 验证每个属性的类型和值
  let mut i = 0
  while i < attributes.length() {
    let (key, value) = attributes[i]
    
    match key {
      "string_key" => {
        match value {
          @azimuth.telemetry.api.common.StringValue(v) => assert_eq(v, "string_value")
          _ => assert_eq(false, true)
        }
      }
      "int_key" => {
        match value {
          @azimuth.telemetry.api.common.IntValue(v) => assert_eq(v, 123L)
          _ => assert_eq(false, true)
        }
      }
      "float_key" => {
        match value {
          @azimuth.telemetry.api.common.FloatValue(v) => assert_eq(v > 45.0, true)
          _ => assert_eq(false, true)
        }
      }
      "bool_key" => {
        match value {
          @azimuth.telemetry.api.common.BoolValue(v) => assert_eq(v, false)
          _ => assert_eq(false, true)
        }
      }
      _ => assert_eq(false, true) // 不应该有其他键
    }
    
    i = i + 1
  }
}

test "attribute_value_serialization_format" {
  // 测试属性值的序列化格式
  
  let complex_attributes = [
    ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("payment-service")),
    ("service.version", @azimuth.telemetry.api.common.AttributeValue::string("1.2.3")),
    ("request.count", @azimuth.telemetry.api.common.AttributeValue::int(1000L)),
    ("response.time", @azimuth.telemetry.api.common.AttributeValue::float(125.5)),
    ("service.healthy", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
    ("service.endpoints", @azimuth.telemetry.api.common.AttributeValue::array_string(["/api/v1/pay", "/api/v1/status"]))
  ]
  
  // 创建序列化字符串
  let mut serialized = "{"}
  let mut i = 0
  while i < complex_attributes.length() {
    let (key, value) = complex_attributes[i]
    
    if i > 0 {
      serialized = serialized + ","
    }
    
    serialized = serialized + "\"" + key + "\":"
    
    match value {
      @azimuth.telemetry.api.common.StringValue(v) => {
        serialized = serialized + "\"" + v + "\""
      }
      @azimuth.telemetry.api.common.IntValue(v) => {
        serialized = serialized + v.to_string()
      }
      @azimuth.telemetry.api.common.FloatValue(v) => {
        serialized = serialized + v.to_string()
      }
      @azimuth.telemetry.api.common.BoolValue(v) => {
        serialized = serialized + v.to_string()
      }
      @azimuth.telemetry.api.common.ArrayStringValue(v) => {
        serialized = serialized + "["
        let mut j = 0
        while j < v.length() {
          if j > 0 {
            serialized = serialized + ","
          }
          serialized = serialized + "\"" + v[j] + "\""
          j = j + 1
        }
        serialized = serialized + "]"
      }
      _ => {
        serialized = serialized + "\"unsupported\""
      }
    }
    
    i = i + 1
  }
  serialized = serialized + "}"
  
  // 验证序列化格式
  assert_eq(serialized.has_prefix("{"), true)
  assert_eq(serialized.has_suffix("}"), true)
  assert_eq(serialized.contains("\"service.name\":\"payment-service\""), true)
  assert_eq(serialized.contains("\"request.count\":1000"), true)
  assert_eq(serialized.contains("\"service.healthy\":true"), true)
  assert_eq(serialized.contains("\"/api/v1/pay\""), true)
}