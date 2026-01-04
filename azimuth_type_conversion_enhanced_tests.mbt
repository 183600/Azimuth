// Azimuth Telemetry System - Enhanced Type Conversion Tests
// 增强类型转换测试用例，测试各种数据类型之间的转换功能

test "字符串到数值类型转换测试" {
  // 测试字符串到整数转换
  let int_str = "42"
  let int_result = int_str.to_int()
  match int_result {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  // 测试字符串到浮点数转换
  let float_str = "3.14159"
  let float_result = float_str.to_float()
  match float_result {
    Some(value) => assert_eq(value, 3.14159)
    None => assert_true(false)
  }
  
  // 测试无效字符串转换
  let invalid_str = "not_a_number"
  let invalid_result = invalid_str.to_int()
  match invalid_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试边界值转换
  let max_int_str = "2147483647"
  let max_int_result = max_int_str.to_int()
  match max_int_result {
    Some(value) => assert_eq(value, 2147483647)
    None => assert_true(false)
  }
  
  let min_int_str = "-2147483648"
  let min_int_result = min_int_str.to_int()
  match min_int_result {
    Some(value) => assert_eq(value, -2147483648)
    None => assert_true(false)
  }
}

test "数值类型到字符串转换测试" {
  // 测试整数到字符串转换
  let int_val = 2024
  let int_str = int_val.to_string()
  assert_eq(int_str, "2024")
  
  // 测试负整数到字符串转换
  let neg_int_val = -2024
  let neg_int_str = neg_int_val.to_string()
  assert_eq(neg_int_str, "-2024")
  
  // 测试浮点数到字符串转换
  let float_val = 3.14159
  let float_str = float_val.to_string()
  assert_eq(float_str, "3.14159")
  
  // 测试零值转换
  let zero_int = 0
  let zero_int_str = zero_int.to_string()
  assert_eq(zero_int_str, "0")
  
  let zero_float = 0.0
  let zero_float_str = zero_float.to_string()
  assert_eq(zero_float_str, "0.0")
}

test "布尔类型转换测试" {
  // 测试布尔值到字符串转换
  let true_val = true
  let true_str = true_val.to_string()
  assert_eq(true_str, "true")
  
  let false_val = false
  let false_str = false_val.to_string()
  assert_eq(false_str, "false")
  
  // 测试字符串到布尔值转换
  let true_str = "true"
  let true_result = true_str.to_bool()
  match true_result {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let false_str = "false"
  let false_result = false_str.to_bool()
  match false_result {
    Some(value) => assert_false(value)
    None => assert_true(false)
  }
  
  // 测试其他字符串值转换
  let one_str = "1"
  let one_result = one_str.to_bool()
  match one_result {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let zero_str = "0"
  let zero_result = zero_str.to_bool()
  match zero_result {
    Some(value) => assert_false(value)
    None => assert_true(false)
  }
}

test "数组类型转换测试" {
  // 测试字符串数组到字符串转换
  let string_array = ["apple", "banana", "cherry"]
  let array_str = string_array.join(", ")
  assert_eq(array_str, "apple, banana, cherry")
  
  // 测试整数数组到字符串转换
  let int_array = [1, 2, 3, 4, 5]
  let int_array_str = int_array.join(", ")
  assert_eq(int_array_str, "1, 2, 3, 4, 5")
  
  // 测试空数组转换
  let empty_array = []
  let empty_array_str = empty_array.join(", ")
  assert_eq(empty_array_str, "")
  
  // 测试字符串到数组转换
  let csv_str = "red,green,blue"
  let str_array = csv_str.split(",")
  assert_eq(str_array.length(), 3)
  assert_eq(str_array[0], "red")
  assert_eq(str_array[1], "green")
  assert_eq(str_array[2], "blue")
}

test "属性值类型转换测试" {
  // 测试字符串属性值转换
  let string_attr = azimuth::AttributeValue::StringValue("test_value")
  let string_to_int = azimuth::AttributeValue::to_int(string_attr)
  match string_to_int {
    Some(_) => assert_true(false) // 字符串不能转换为整数
    None => assert_true(true)
  }
  
  let string_to_string = azimuth::AttributeValue::to_string(string_attr)
  match string_to_string {
    Some(value) => assert_eq(value, "test_value")
    None => assert_true(false)
  }
  
  // 测试整数属性值转换
  let int_attr = azimuth::AttributeValue::IntValue(42)
  let int_to_string = azimuth::AttributeValue::to_string(int_attr)
  match int_to_string {
    Some(value) => assert_eq(value, "42")
    None => assert_true(false)
  }
  
  let int_to_float = azimuth::AttributeValue::to_float(int_attr)
  match int_to_float {
    Some(value) => assert_eq(value, 42.0)
    None => assert_true(false)
  }
  
  // 测试浮点数属性值转换
  let float_attr = azimuth::AttributeValue::FloatValue(3.14)
  let float_to_string = azimuth::AttributeValue::to_string(float_attr)
  match float_to_string {
    Some(value) => assert_eq(value, "3.14")
    None => assert_true(false)
  }
  
  let float_to_int = azimuth::AttributeValue::to_int(float_attr)
  match float_to_int {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  
  // 测试布尔属性值转换
  let bool_attr = azimuth::AttributeValue::BoolValue(true)
  let bool_to_string = azimuth::AttributeValue::to_string(bool_attr)
  match bool_to_string {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  let bool_to_int = azimuth::AttributeValue::to_int(bool_attr)
  match bool_to_int {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
}

test "枚举类型转换测试" {
  // 测试跨度状态枚举转换
  let status_ok = azimuth::SpanStatus::Ok
  let status_ok_str = azimuth::SpanStatus::to_string(status_ok)
  assert_eq(status_ok_str, "Ok")
  
  let status_error = azimuth::SpanStatus::Error
  let status_error_str = azimuth::SpanStatus::to_string(status_error)
  assert_eq(status_error_str, "Error")
  
  // 测试字符串到枚举转换
  let ok_str = "Ok"
  let str_to_status = azimuth::SpanStatus::from_string(ok_str)
  match str_to_status {
    Some(azimuth::SpanStatus::Ok) => assert_true(true)
    _ => assert_true(false)
  }
  
  let error_str = "Error"
  let str_to_error_status = azimuth::SpanStatus::from_string(error_str)
  match str_to_error_status {
    Some(azimuth::SpanStatus::Error) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试无效字符串转换
  let invalid_status_str = "InvalidStatus"
  let invalid_status = azimuth::SpanStatus::from_string(invalid_status_str)
  match invalid_status {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

test "时间戳转换测试" {
  // 测试时间戳到字符串转换
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  let timestamp_str = timestamp.to_string()
  assert_eq(timestamp_str, "1609459200000")
  
  // 测试字符串到时间戳转换
  let time_str = "1609459200000"
  let str_to_timestamp = time_str.to_long()
  match str_to_timestamp {
    Some(value) => assert_eq(value, 1609459200000L)
    None => assert_true(false)
  }
  
  // 测试时间戳到日期字符串转换
  let date_str = timestamp.to_date_string()
  assert_eq(date_str, "2021-01-01")
  
  // 测试日期字符串到时间戳转换
  let date_time_str = "2021-01-01"
  let date_to_timestamp = date_time_str.to_timestamp()
  match date_to_timestamp {
    Some(value) => assert_eq(value, 1609459200000L)
    None => assert_true(false)
  }
}

test "JSON数据转换测试" {
  // 测试简单JSON对象转换
  let json_str = "{\"name\":\"test\",\"value\":42,\"enabled\":true}"
  let json_obj = json_str.parse_json()
  match json_obj {
    Some(obj) => {
      let name_value = json_obj_get_string(obj, "name")
      match name_value {
        Some(value) => assert_eq(value, "test")
        None => assert_true(false)
      }
      
      let value_number = json_obj_get_int(obj, "value")
      match value_number {
        Some(value) => assert_eq(value, 42)
        None => assert_true(false)
      }
      
      let enabled_bool = json_obj_get_bool(obj, "enabled")
      match enabled_bool {
        Some(value) => assert_true(value)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试JSON数组转换
  let json_array_str = "[1,2,3,4,5]"
  let json_array = json_array_str.parse_json_array()
  match json_array {
    Some(array) => {
      assert_eq(json_array_length(array), 5)
      let first_element = json_array_get_int(array, 0)
      match first_element {
        Some(value) => assert_eq(value, 1)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试无效JSON转换
  let invalid_json = "{invalid json}"
  let invalid_result = invalid_json.parse_json()
  match invalid_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

test "二进制数据转换测试" {
  // 测试字符串到Base64转换
  let original_str = "Hello, World!"
  let base64_str = original_str.to_base64()
  assert_eq(base64_str, "SGVsbG8sIFdvcmxkIQ==")
  
  // 测试Base64到字符串转换
  let decoded_str = base64_str.from_base64()
  match decoded_str {
    Some(value) => assert_eq(value, "Hello, World!")
    None => assert_true(false)
  }
  
  // 测试十六进制字符串转换
  let hex_str = "48656c6c6f2c20576f726c6421"
  let hex_decoded = hex_str.from_hex()
  match hex_decoded {
    Some(value) => assert_eq(value, "Hello, World!")
    None => assert_true(false)
  }
  
  // 测试字符串到十六进制转换
  let str_to_hex = original_str.to_hex()
  assert_eq(str_to_hex, "48656c6c6f2c20576f726c6421")
  
  // 测试无效Base64转换
  let invalid_base64 = "Invalid Base64@@@!"
  let invalid_decode = invalid_base64.from_base64()
  match invalid_decode {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

test "复杂数据结构转换测试" {
  // 测试嵌套结构转换
  let nested_data = {
    "user": {
      "id": 123,
      "name": "张三",
      "active": true
    },
    "permissions": ["read", "write", "admin"],
    "metadata": {
      "created_at": "2021-01-01T00:00:00Z",
      "updated_at": "2021-12-31T23:59:59Z"
    }
  }
  
  // 转换为JSON字符串
  let json_str = nested_data.to_json_string()
  assert_true(json_str.contains("\"id\":123"))
  assert_true(json_str.contains("\"name\":\"张三\""))
  assert_true(json_str.contains("\"active\":true"))
  assert_true(json_str.contains("\"permissions\":[\"read\",\"write\",\"admin\"]"))
  
  // 从JSON字符串转换回结构
  let parsed_data = json_str.parse_json()
  match parsed_data {
    Some(data) => {
      let user_obj = json_obj_get_object(data, "user")
      match user_obj {
        Some(user) => {
          let user_name = json_obj_get_string(user, "name")
          match user_name {
            Some(name) => assert_eq(name, "张三")
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试数组的平铺转换
  let nested_arrays = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  let flattened = nested_arrays.flatten()
  assert_eq(flattened.length(), 9)
  assert_eq(flattened[0], 1)
  assert_eq(flattened[8], 9)
  
  // 测试数组的分组转换
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]
  let grouped = numbers.group_by(3)
  assert_eq(grouped.length(), 3)
  assert_eq(grouped[0], [1, 2, 3])
  assert_eq(grouped[1], [4, 5, 6])
  assert_eq(grouped[2], [7, 8, 9])
}