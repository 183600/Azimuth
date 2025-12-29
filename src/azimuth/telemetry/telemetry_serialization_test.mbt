// 遥测序列化测试用例

test "serialization_basic_types" {
  // 测试基本类型的序列化
  
  // 字符串序列化
  let string_value = "telemetry.test.string"
  let serialized_string = "\"" + string_value + "\""
  assert_eq(serialized_string, "\"telemetry.test.string\"")
  
  // 整数序列化
  let int_value = 42L
  let serialized_int = int_value.to_string()
  assert_eq(serialized_int, "42")
  
  // 浮点数序列化
  let float_value = 3.14159
  let serialized_float = float_value.to_string()
  assert_eq(serialized_float.contains("3.14159"), true)
  
  // 布尔值序列化
  let bool_value = true
  let serialized_bool = bool_value.to_string()
  assert_eq(serialized_bool, "true")
  
  let bool_value_false = false
  let serialized_bool_false = bool_value_false.to_string()
  assert_eq(serialized_bool_false, "false")
}

test "serialization_attribute_values" {
  // 测试属性值序列化
  use azimuth.telemetry.api.common.AttributeValue
  
  let string_attr = AttributeValue::string("test.value")
  let int_attr = AttributeValue::int(123L)
  let float_attr = AttributeValue::float(45.67)
  let bool_attr = AttributeValue::bool(true)
  
  // 模拟序列化过程
  let serialized_string_attr = match string_attr {
    StringValue(s) => "string:" + s
    _ => "unknown"
  }
  
  let serialized_int_attr = match int_attr {
    IntValue(i) => "int:" + i.to_string()
    _ => "unknown"
  }
  
  let serialized_float_attr = match float_attr {
    FloatValue(f) => "float:" + f.to_string()
    _ => "unknown"
  }
  
  let serialized_bool_attr = match bool_attr {
    BoolValue(b) => "bool:" + b.to_string()
    _ => "unknown"
  }
  
  // 验证序列化结果
  assert_eq(serialized_string_attr, "string:test.value")
  assert_eq(serialized_int_attr, "int:123")
  assert_eq(serialized_float_attr.contains("float:45.67"), true)
  assert_eq(serialized_bool_attr, "bool:true")
}

test "serialization_array_types" {
  // 测试数组类型序列化
  use azimuth.telemetry.api.common.AttributeValue
  
  let string_array = AttributeValue::array_string(["a", "b", "c"])
  let int_array = AttributeValue::array_int([1L, 2L, 3L, 4L])
  let float_array = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array = AttributeValue::array_bool([true, false, true])
  
  // 模拟数组序列化
  let serialized_string_array = match string_array {
    ArrayStringValue(arr) => {
      let mut result = "[string:"
      let mut i = 0
      while i < arr.length() {
        if i > 0 {
          result = result + ","
        }
        result = result + arr[i]
        i = i + 1
      }
      result = result + "]"
      result
    }
    _ => "unknown"
  }
  
  // 验证数组序列化
  assert_eq(serialized_string_array, "[string:a,b,c]")
  
  // 测试空数组序列化
  let empty_array = AttributeValue::array_string([])
  let serialized_empty = match empty_array {
    ArrayStringValue(arr) => {
      if arr.length() == 0 {
        "[string:]"
      } else {
        "not_empty"
      }
    }
    _ => "unknown"
  }
  
  assert_eq(serialized_empty, "[string:]")
}

test "serialization_complex_structures" {
  // 测试复杂结构序列化
  
  // 模拟复杂遥测数据结构
  let telemetry_data = [
    ("trace.id", "0af7651916cd43dd8448eb211c80319c"),
    ("span.id", "b7ad6b7169203331"),
    ("service.name", "payment-service"),
    ("service.version", "1.2.3"),
    ("http.method", "GET"),
    ("http.status_code", "200"),
    ("http.url", "/api/v1/payments"),
    ("user.id", "user-12345"),
    ("session.id", "session-abcdef"),
    ("request.duration_ms", "150")
  ]
  
  // 序列化为键值对字符串
  let mut serialized = ""
  let mut i = 0
  while i < telemetry_data.length() {
    let (key, value) = telemetry_data[i]
    if i > 0 {
      serialized = serialized + "&"
    }
    serialized = serialized + key + "=" + value
    i = i + 1
  }
  
  // 验证序列化结果
  assert_eq(serialized.contains("trace.id=0af7651916cd43dd8448eb211c80319c"), true)
  assert_eq(serialized.contains("service.name=payment-service"), true)
  assert_eq(serialized.contains("http.method=GET"), true)
  assert_eq(serialized.contains("&"), true)  // 确保有分隔符
  
  // 计算键值对数量
  let pairs = serialized.split("&")
  assert_eq(pairs.length(), telemetry_data.length())
}

test "serialization_deserialization_roundtrip" {
  // 测试序列化/反序列化往返
  
  // 原始数据
  let original_data = [
    ("string.key", "string.value"),
    ("int.key", "42"),
    ("float.key", "3.14"),
    ("bool.key", "true")
  ]
  
  // 序列化
  let mut serialized = ""
  let mut i = 0
  while i < original_data.length() {
    let (key, value) = original_data[i]
    if i > 0 {
      serialized = serialized + ";"
    }
    serialized = serialized + key + ":" + value
    i = i + 1
  }
  
  // 反序列化
  let deserialized_pairs = serialized.split(";")
  let mut roundtrip_success = true
  
  let mut j = 0
  while j < deserialized_pairs.length() {
    let pair = deserialized_pairs[j]
    let parts = pair.split(":")
    if parts.length() == 2 {
      let key = parts[0]
      let value = parts[1]
      
      // 验证键值对
      let mut found = false
      let mut k = 0
      while k < original_data.length() {
        let (orig_key, orig_value) = original_data[k]
        if key == orig_key && value == orig_value {
          found = true
          break
        }
        k = k + 1
      }
      
      if !found {
        roundtrip_success = false
        break
      }
    } else {
      roundtrip_success = false
      break
    }
    j = j + 1
  }
  
  assert_eq(roundtrip_success, true)
}

test "serialization_edge_cases" {
  // 测试序列化边界情况
  
  // 空字符串
  let empty_string = ""
  let serialized_empty = "\"" + empty_string + "\""
  assert_eq(serialized_empty, "\"\"")
  
  // 特殊字符
  let special_chars = "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"
  let serialized_special = "\"" + special_chars + "\""
  assert_eq(serialized_special.contains("!@#$%^&*()"), true)
  
  // Unicode字符
  let unicode_string = "Unicode测试🚀"
  let serialized_unicode = "\"" + unicode_string + "\""
  assert_eq(serialized_unicode.contains("Unicode"), true)
  assert_eq(serialized_unicode.contains("测试"), true)
  assert_eq(serialized_unicode.contains("🚀"), true)
  
  // 长字符串
  let long_string = "This is a very long string that tests serialization performance with large amounts of data that need to be properly handled without loss or corruption during the serialization and deserialization process."
  let serialized_long = "\"" + long_string + "\""
  assert_eq(serialized_long.length() > long_string.length(), true)
  assert_eq(serialized_long.contains("very long string"), true)
  
  // 数值边界
  let max_int = 9223372036854775807L
  let min_int = -9223372036854775808L
  let zero = 0L
  
  assert_eq(max_int.to_string(), "9223372036854775807")
  assert_eq(min_int.to_string(), "-9223372036854775808")
  assert_eq(zero.to_string(), "0")
  
  // 浮点数边界
  let infinity = 1.0/0.0
  let neg_infinity = -1.0/0.0
  let nan = 0.0/0.0
  
  let infinity_str = infinity.to_string()
  let neg_infinity_str = neg_infinity.to_string()
  let nan_str = nan.to_string()
  
  // 验证特殊浮点数值的字符串表示
  assert_eq(infinity_str.length() > 0, true)
  assert_eq(neg_infinity_str.length() > 0, true)
  assert_eq(nan_str.length() > 0, true)
}