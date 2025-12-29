// 属性值类型转换测试用例 - 测试AttributeValue的各种类型转换功能

use azimuth.telemetry.api.common.{AttributeValue, Attributes}

test "attribute_value_string_conversions" {
  // 测试字符串属性值的转换
  
  // 1. 基本字符串转换
  let string_attr = AttributeValue::string("test_value")
  match string_attr {
    StringValue(value) => {
      assert_eq(value, "test_value")
      assert_eq(value.length(), 10)
    }
    _ => @test.fail("Test failed")
  }
  
  // 2. 从其他类型转换为字符串
  let int_attr = AttributeValue::int(123L)
  let int_to_string = match int_attr {
    IntValue(value) => value.to_string()
    _ => @test.fail("Test failed")
  }
  assert_eq(int_to_string, "123")
  
  let float_attr = AttributeValue::float(45.67)
  let float_to_string = match float_attr {
    FloatValue(value) => value.to_string()
    _ => @test.fail("Test failed")
  }
  assert_eq(float_to_string.contains("45.67"), true)
  
  let bool_attr = AttributeValue::bool(true)
  let bool_to_string = match bool_attr {
    BoolValue(value) => value.to_string()
    _ => @test.fail("Test failed")
  }
  assert_eq(bool_to_string, "true")
  
  // 3. 字符串数组转换
  let string_array_attr = AttributeValue::array_string(["a", "b", "c"])
  match string_array_attr {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "a")
      assert_eq(values[1], "b")
      assert_eq(values[2], "c")
      
      // 转换为逗号分隔的字符串
      let mut joined_string = ""
      let mut i = 0
      while i < values.length() {
        if i > 0 {
          joined_string = joined_string + ","
        }
        joined_string = joined_string + values[i]
        i = i + 1
      }
      assert_eq(joined_string, "a,b,c")
    }
    _ => @test.fail("Test failed")
  }
  
  // 4. 特殊字符串处理
  let special_string = "Hello, 世界! @#$%^&*()"
  let special_attr = AttributeValue::string(special_string)
  match special_attr {
    StringValue(value) => {
      assert_eq(value.contains("Hello"), true)
      assert_eq(value.contains("世界"), true)
      assert_eq(value.contains("@#$%^&*()"), true)
      assert_eq(value.length(), 18)
    }
    _ => @test.fail("Test failed")
  }
  
  // 5. 空字符串和边界情况
  let empty_string_attr = AttributeValue::string("")
  match empty_string_attr {
    StringValue(value) => {
      assert_eq(value, "")
      assert_eq(value.length(), 0)
    }
    _ => @test.fail("Test failed")
  }
  
  let long_string = "a" * 1000
  let long_string_attr = AttributeValue::string(long_string)
  match long_string_attr {
    StringValue(value) => {
      assert_eq(value.length(), 1000)
      assert_eq(value.has_prefix("aaa"), true)
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_numeric_conversions" {
  // 测试数值属性值的转换
  
  // 1. 整数转换
  let int_attr = AttributeValue::int(42L)
  match int_attr {
    IntValue(value) => {
      assert_eq(value, 42L)
      assert_eq(value > 0L, true)
      
      // 转换为字符串
      let string_value = value.to_string()
      assert_eq(string_value, "42")
      
      // 转换为浮点数
      let float_value = value.to_double()
      assert_eq(float_value, 42.0)
    }
    _ => @test.fail("Test failed")
  }
  
  // 2. 浮点数转换
  let float_attr = AttributeValue::float(3.14159)
  match float_attr {
    FloatValue(value) => {
      assert_eq(value > 3.0 && value < 4.0, true)
      
      // 转换为字符串
      let string_value = value.to_string()
      assert_eq(string_value.contains("3.14159"), true)
      
      // 转换为整数（截断）
      let int_value = value.to_int64()
      assert_eq(int_value, 3L)
    }
    _ => @test.fail("Test failed")
  }
  
  // 3. 数值数组转换
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])
  match int_array_attr {
    ArrayIntValue(values) => {
      assert_eq(values.length(), 5)
      
      // 计算总和
      let mut sum = 0L
      let mut i = 0
      while i < values.length() {
        sum = sum + values[i]
        i = i + 1
      }
      assert_eq(sum, 15L)
      
      // 计算平均值
      let average = sum.to_double() / values.length().to_double()
      assert_eq(average, 3.0)
    }
    _ => @test.fail("Test failed")
  }
  
  let float_array_attr = AttributeValue::array_float([1.1, 2.2, 3.3, 4.4, 5.5])
  match float_array_attr {
    ArrayFloatValue(values) => {
      assert_eq(values.length(), 5)
      
      // 计算总和
      let mut sum = 0.0
      let mut i = 0
      while i < values.length() {
        sum = sum + values[i]
        i = i + 1
      }
      assert_eq(sum > 16.0 && sum < 17.0, true)
      
      // 计算平均值
      let average = sum / values.length().to_double()
      assert_eq(average > 3.0 && average < 4.0, true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 4. 边界数值转换
  let max_int = AttributeValue::int(9223372036854775807L)  // Int64最大值
  match max_int {
    IntValue(value) => {
      assert_eq(value, 9223372036854775807L)
      assert_eq(value > 0L, true)
    }
    _ => @test.fail("Test failed")
  }
  
  let min_int = AttributeValue::int(-9223372036854775808L)  // Int64最小值
  match min_int {
    IntValue(value) => {
      assert_eq(value, -9223372036854775808L)
      assert_eq(value < 0L, true)
    }
    _ => @test.fail("Test failed")
  }
  
  let max_float = AttributeValue::float(1.7976931348623157e+308)  // Double最大值
  match max_float {
    FloatValue(value) => {
      assert_eq(value > 1.0e+308, true)
    }
    _ => @test.fail("Test failed")
  }
  
  let min_float = AttributeValue::float(-1.7976931348623157e+308)  // Double最小值
  match min_float {
    FloatValue(value) => {
      assert_eq(value < -1.0e+308, true)
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_boolean_conversions" {
  // 测试布尔属性值的转换
  
  // 1. 基本布尔转换
  let true_attr = AttributeValue::bool(true)
  match true_attr {
    BoolValue(value) => {
      assert_eq(value, true)
      
      // 转换为字符串
      let string_value = value.to_string()
      assert_eq(string_value, "true")
      
      // 转换为整数
      let int_value = if value { 1L } else { 0L }
      assert_eq(int_value, 1L)
    }
    _ => @test.fail("Test failed")
  }
  
  let false_attr = AttributeValue::bool(false)
  match false_attr {
    BoolValue(value) => {
      assert_eq(value, false)
      
      // 转换为字符串
      let string_value = value.to_string()
      assert_eq(string_value, "false")
      
      // 转换为整数
      let int_value = if value { 1L } else { 0L }
      assert_eq(int_value, 0L)
    }
    _ => @test.fail("Test failed")
  }
  
  // 2. 布尔数组转换
  let bool_array_attr = AttributeValue::array_bool([true, false, true, true, false])
  match bool_array_attr {
    ArrayBoolValue(values) => {
      assert_eq(values.length(), 5)
      
      // 统计true的数量
      let mut true_count = 0
      let mut false_count = 0
      let mut i = 0
      while i < values.length() {
        if values[i] {
          true_count = true_count + 1
        } else {
          false_count = false_count + 1
        }
        i = i + 1
      }
      assert_eq(true_count, 3)
      assert_eq(false_count, 2)
      
      // 转换为字符串数组
      let string_values = []
      i = 0
      while i < values.length() {
        string_values.push(if values[i] { "true" } else { "false" })
        i = i + 1
      }
      assert_eq(string_values, ["true", "false", "true", "true", "false"])
    }
    _ => @test.fail("Test failed")
  }
  
  // 3. 从字符串转换为布尔值
  let string_to_bool_true = "true"
  let bool_from_string_true = if string_to_bool_true == "true" { true } else { false }
  assert_eq(bool_from_string_true, true)
  
  let string_to_bool_false = "false"
  let bool_from_string_false = if string_to_bool_false == "true" { true } else { false }
  assert_eq(bool_from_string_false, false)
  
  // 4. 从数值转换为布尔值
  let int_to_bool_true = 1L
  let bool_from_int_true = int_to_bool_true != 0L
  assert_eq(bool_from_int_true, true)
  
  let int_to_bool_false = 0L
  let bool_from_int_false = int_to_bool_false != 0L
  assert_eq(bool_from_int_false, false)
  
  let float_to_bool_true = 1.5
  let bool_from_float_true = float_to_bool_true != 0.0
  assert_eq(bool_from_float_true, true)
  
  let float_to_bool_false = 0.0
  let bool_from_float_false = float_to_bool_false != 0.0
  assert_eq(bool_from_float_false, false)
}

test "attribute_value_array_conversions" {
  // 测试数组属性值的转换
  
  // 1. 字符串数组转换
  let string_array = ["apple", "banana", "cherry", "date"]
  let string_array_attr = AttributeValue::array_string(string_array)
  match string_array_attr {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 4)
      assert_eq(values[0], "apple")
      assert_eq(values[1], "banana")
      assert_eq(values[2], "cherry")
      assert_eq(values[3], "date")
      
      // 转换为逗号分隔的字符串
      let mut csv_string = ""
      let mut i = 0
      while i < values.length() {
        if i > 0 {
          csv_string = csv_string + ","
        }
        csv_string = csv_string + values[i]
        i = i + 1
      }
      assert_eq(csv_string, "apple,banana,cherry,date")
      
      // 转换为JSON数组字符串
      let json_string = "[" + csv_string + "]"
      assert_eq(json_string, "[apple,banana,cherry,date]")
    }
    _ => @test.fail("Test failed")
  }
  
  // 2. 整数数组转换
  let int_array = [10L, 20L, 30L, 40L, 50L]
  let int_array_attr = AttributeValue::array_int(int_array)
  match int_array_attr {
    ArrayIntValue(values) => {
      assert_eq(values.length(), 5)
      
      // 计算统计信息
      let mut sum = 0L
      let mut min = values[0]
      let mut max = values[0]
      let mut i = 0
      while i < values.length() {
        sum = sum + values[i]
        if values[i] < min {
          min = values[i]
        }
        if values[i] > max {
          max = values[i]
        }
        i = i + 1
      }
      
      let average = sum.to_double() / values.length().to_double()
      assert_eq(sum, 150L)
      assert_eq(min, 10L)
      assert_eq(max, 50L)
      assert_eq(average, 30.0)
    }
    _ => @test.fail("Test failed")
  }
  
  // 3. 浮点数数组转换
  let float_array = [1.1, 2.2, 3.3, 4.4, 5.5]
  let float_array_attr = AttributeValue::array_float(float_array)
  match float_array_attr {
    ArrayFloatValue(values) => {
      assert_eq(values.length(), 5)
      
      // 计算统计信息
      let mut sum = 0.0
      let mut min = values[0]
      let mut max = values[0]
      let mut i = 0
      while i < values.length() {
        sum = sum + values[i]
        if values[i] < min {
          min = values[i]
        }
        if values[i] > max {
          max = values[i]
        }
        i = i + 1
      }
      
      let average = sum / values.length().to_double()
      assert_eq(sum > 16.0 && sum < 17.0, true)
      assert_eq(min, 1.1)
      assert_eq(max, 5.5)
      assert_eq(average > 3.0 && average < 4.0, true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 4. 布尔数组转换
  let bool_array = [true, true, false, true, false, false]
  let bool_array_attr = AttributeValue::array_bool(bool_array)
  match bool_array_attr {
    ArrayBoolValue(values) => {
      assert_eq(values.length(), 6)
      
      // 统计和转换
      let mut true_count = 0
      let mut false_count = 0
      let mut string_values = []
      let mut i = 0
      while i < values.length() {
        if values[i] {
          true_count = true_count + 1
          string_values.push("1")
        } else {
          false_count = false_count + 1
          string_values.push("0")
        }
        i = i + 1
      }
      
      assert_eq(true_count, 3)
      assert_eq(false_count, 3)
      assert_eq(string_values, ["1", "1", "0", "1", "0", "0"])
      
      // 转换为位掩码字符串
      let mut bitmask = ""
      i = 0
      while i < string_values.length() {
        bitmask = bitmask + string_values[i]
        i = i + 1
      }
      assert_eq(bitmask, "110100")
    }
    _ => @test.fail("Test failed")
  }
  
  // 5. 空数组转换
  let empty_string_array = [] : Array[String]
  let empty_string_array_attr = AttributeValue::array_string(empty_string_array)
  match empty_string_array_attr {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 0)
    }
    _ => @test.fail("Test failed")
  }
  
  let empty_int_array = [] : Array[Int64]
  let empty_int_array_attr = AttributeValue::array_int(empty_int_array)
  match empty_int_array_attr {
    ArrayIntValue(values) => {
      assert_eq(values.length(), 0)
    }
    _ => @test.fail("Test failed")
  }
  
  let empty_float_array = [] : Array[Double]
  let empty_float_array_attr = AttributeValue::array_float(empty_float_array)
  match empty_float_array_attr {
    ArrayFloatValue(values) => {
      assert_eq(values.length(), 0)
    }
    _ => @test.fail("Test failed")
  }
  
  let empty_bool_array = [] : Array[Bool]
  let empty_bool_array_attr = AttributeValue::array_bool(empty_bool_array)
  match empty_bool_array_attr {
    ArrayBoolValue(values) => {
      assert_eq(values.length(), 0)
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_type_safety_and_validation" {
  // 测试属性值类型安全和验证
  
  // 1. 类型安全检查
  let string_attr = AttributeValue::string("test")
  let is_string = match string_attr {
    StringValue(_) => true
    _ => false
  }
  assert_eq(is_string, true)
  
  let int_attr = AttributeValue::int(42L)
  let is_int = match int_attr {
    IntValue(_) => true
    _ => false
  }
  assert_eq(is_int, true)
  
  let float_attr = AttributeValue::float(3.14)
  let is_float = match float_attr {
    FloatValue(_) => true
    _ => false
  }
  assert_eq(is_float, true)
  
  let bool_attr = AttributeValue::bool(true)
  let is_bool = match bool_attr {
    BoolValue(_) => true
    _ => false
  }
  assert_eq(is_bool, true)
  
  // 2. 类型转换函数
  fn attribute_to_string(attr : AttributeValue) -> String {
    match attr {
      StringValue(value) => value
      IntValue(value) => value.to_string()
      FloatValue(value) => value.to_string()
      BoolValue(value) => value.to_string()
      ArrayStringValue(values) => {
        let mut result = "["
        let mut i = 0
        while i < values.length() {
          if i > 0 {
            result = result + ","
          }
          result = result + values[i]
          i = i + 1
        }
        result = result + "]"
        result
      }
      ArrayIntValue(values) => {
        let mut result = "["
        let mut i = 0
        while i < values.length() {
          if i > 0 {
            result = result + ","
          }
          result = result + values[i].to_string()
          i = i + 1
        }
        result = result + "]"
        result
      }
      ArrayFloatValue(values) => {
        let mut result = "["
        let mut i = 0
        while i < values.length() {
          if i > 0 {
            result = result + ","
          }
          result = result + values[i].to_string()
          i = i + 1
        }
        result = result + "]"
        result
      }
      ArrayBoolValue(values) => {
        let mut result = "["
        let mut i = 0
        while i < values.length() {
          if i > 0 {
            result = result + ","
          }
          result = result + (if values[i] { "true" } else { "false" })
          i = i + 1
        }
        result = result + "]"
        result
      }
    }
  }
  
  // 测试转换函数
  assert_eq(attribute_to_string(string_attr), "test")
  assert_eq(attribute_to_string(int_attr), "42")
  assert_eq(attribute_to_string(float_attr).contains("3.14"), true)
  assert_eq(attribute_to_string(bool_attr), "true")
  
  let string_array_attr = AttributeValue::array_string(["a", "b", "c"])
  assert_eq(attribute_to_string(string_array_attr), "[a,b,c]")
  
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L])
  assert_eq(attribute_to_string(int_array_attr), "[1,2,3]")
  
  let bool_array_attr = AttributeValue::array_bool([true, false])
  assert_eq(attribute_to_string(bool_array_attr), "[true,false]")
  
  // 3. 属性值验证
  fn validate_string_attribute(attr : AttributeValue) -> Bool {
    match attr {
      StringValue(value) => value.length() > 0 && value.length() <= 1000
      _ => false
    }
  }
  
  fn validate_int_attribute(attr : AttributeValue) -> Bool {
    match attr {
      IntValue(value) => value >= -1000000L && value <= 1000000L
      _ => false
    }
  }
  
  fn validate_float_attribute(attr : AttributeValue) -> Bool {
    match attr {
      FloatValue(value) => value.is_finite() && value >= -1000000.0 && value <= 1000000.0
      _ => false
    }
  }
  
  // 测试验证函数
  assert_eq(validate_string_attribute(AttributeValue::string("valid")), true)
  assert_eq(validate_string_attribute(AttributeValue::string("")), false)
  
  assert_eq(validate_int_attribute(AttributeValue::int(500L)), true)
  assert_eq(validate_int_attribute(AttributeValue::int(2000000L)), false)
  
  assert_eq(validate_float_attribute(AttributeValue::float(123.456)), true)
  assert_eq(validate_float_attribute(AttributeValue::float(2000000.0)), false)
  
  // 4. 数组长度验证
  fn validate_array_length(attr : AttributeValue, max_length : Int) -> Bool {
    match attr {
      ArrayStringValue(values) => values.length() <= max_length
      ArrayIntValue(values) => values.length() <= max_length
      ArrayFloatValue(values) => values.length() <= max_length
      ArrayBoolValue(values) => values.length() <= max_length
      _ => false
    }
  }
  
  let small_array = AttributeValue::array_string(["a", "b", "c"])
  let large_array = AttributeValue::array_string(["a"] * 100)
  
  assert_eq(validate_array_length(small_array, 10), true)
  assert_eq(validate_array_length(large_array, 10), false)
}

test "attribute_value_serialization_formatting" {
  // 测试属性值序列化和格式化
  
  // 1. JSON格式化
  fn attribute_to_json(attr : AttributeValue, key : String) -> String {
    match attr {
      StringValue(value) => "\"" + key + "\":\"" + value + "\""
      IntValue(value) => "\"" + key + "\":" + value.to_string()
      FloatValue(value) => "\"" + key + "\":" + value.to_string()
      BoolValue(value) => "\"" + key + "\":" + (if value { "true" } else { "false" })
      ArrayStringValue(values) => {
        let mut result = "\"" + key + "\":["
        let mut i = 0
        while i < values.length() {
          if i > 0 {
            result = result + ","
          }
          result = result + "\"" + values[i] + "\""
          i = i + 1
        }
        result = result + "]"
        result
      }
      ArrayIntValue(values) => {
        let mut result = "\"" + key + "\":["
        let mut i = 0
        while i < values.length() {
          if i > 0 {
            result = result + ","
          }
          result = result + values[i].to_string()
          i = i + 1
        }
        result = result + "]"
        result
      }
      ArrayFloatValue(values) => {
        let mut result = "\"" + key + "\":["
        let mut i = 0
        while i < values.length() {
          if i > 0 {
            result = result + ","
          }
          result = result + values[i].to_string()
          i = i + 1
        }
        result = result + "]"
        result
      }
      ArrayBoolValue(values) => {
        let mut result = "\"" + key + "\":["
        let mut i = 0
        while i < values.length() {
          if i > 0 {
            result = result + ","
          }
          result = result + (if values[i] { "true" } else { "false" })
          i = i + 1
        }
        result = result + "]"
        result
      }
    }
  }
  
  // 测试JSON格式化
  let string_attr = AttributeValue::string("test_value")
  assert_eq(attribute_to_json(string_attr, "message"), "\"message\":\"test_value\"")
  
  let int_attr = AttributeValue::int(42)
  assert_eq(attribute_to_json(int_attr, "count"), "\"count\":42")
  
  let float_attr = AttributeValue::float(3.14)
  assert_eq(attribute_to_json(float_attr, "ratio").contains("\"ratio\":"), true)
  
  let bool_attr = AttributeValue::bool(true)
  assert_eq(attribute_to_json(bool_attr, "enabled"), "\"enabled\":true")
  
  let string_array_attr = AttributeValue::array_string(["a", "b", "c"])
  assert_eq(attribute_to_json(string_array_attr, "tags"), "\"tags\":[\"a\",\"b\",\"c\"]")
  
  // 2. Prometheus格式化
  fn attribute_to_prometheus(attr : AttributeValue, key : String) -> String {
    match attr {
      StringValue(value) => key + "=\"" + value + "\""
      IntValue(value) => key + " " + value.to_string()
      FloatValue(value) => key + " " + value.to_string()
      BoolValue(value) => key + " " + (if value { "1" } else { "0" })
      ArrayStringValue(_) => key + " \"[array]\""
      ArrayIntValue(_) => key + " [array]"
      ArrayFloatValue(_) => key + " [array]"
      ArrayBoolValue(_) => key + " [array]"
    }
  }
  
  // 测试Prometheus格式化
  assert_eq(attribute_to_prometheus(string_attr, "service_name"), "service_name=\"test_value\"")
  assert_eq(attribute_to_prometheus(int_attr, "request_count"), "request_count 42")
  assert_eq(attribute_to_prometheus(bool_attr, "is_healthy"), "is_healthy 1")
  
  // 3. 键值对格式化
  fn attribute_to_kv_pair(attr : AttributeValue, key : String) -> String {
    match attr {
      StringValue(value) => key + "=" + value
      IntValue(value) => key + "=" + value.to_string()
      FloatValue(value) => key + "=" + value.to_string()
      BoolValue(value) => key + "=" + (if value { "true" } else { "false" })
      ArrayStringValue(values) => {
        let mut result = key + "=["
        let mut i = 0
        while i < values.length() {
          if i > 0 {
            result = result + ","
          }
          result = result + values[i]
          i = i + 1
        }
        result = result + "]"
        result
      }
      ArrayIntValue(values) => {
        let mut result = key + "=["
        let mut i = 0
        while i < values.length() {
          if i > 0 {
            result = result + ","
          }
          result = result + values[i].to_string()
          i = i + 1
        }
        result = result + "]"
        result
      }
      ArrayFloatValue(values) => {
        let mut result = key + "=["
        let mut i = 0
        while i < values.length() {
          if i > 0 {
            result = result + ","
          }
          result = result + values[i].to_string()
          i = i + 1
        }
        result = result + "]"
        result
      }
      ArrayBoolValue(values) => {
        let mut result = key + "=["
        let mut i = 0
        while i < values.length() {
          if i > 0 {
            result = result + ","
          }
          result = result + (if values[i] { "true" } else { "false" })
          i = i + 1
        }
        result = result + "]"
        result
      }
    }
  }
  
  // 测试键值对格式化
  assert_eq(attribute_to_kv_pair(string_attr, "user"), "user=test_value")
  assert_eq(attribute_to_kv_pair(int_attr, "age"), "age=42")
  assert_eq(attribute_to_kv_pair(bool_attr, "active"), "active=true")
  
  let string_array_attr = AttributeValue::array_string(["red", "green", "blue"])
  assert_eq(attribute_to_kv_pair(string_array_attr, "colors"), "colors=[red,green,blue]")
}