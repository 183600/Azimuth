// AttributeValue 高级类型转换和操作测试用例
// 测试各种AttributeValue类型的创建、转换和操作

test "attribute_value_type_conversions_comprehensive" {
  // 测试字符串类型
  let string_attr = AttributeValue::string("test_value")
  match string_attr {
    StringValue(s) => assert_eq(s, "test_value")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试整数类型
  let int_attr = AttributeValue::int(42L)
  match int_attr {
    IntValue(i) => assert_eq(i, 42L)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试浮点类型
  let float_attr = AttributeValue::float(3.14159)
  match float_attr {
    FloatValue(f) => assert_eq(f > 3.14 && f < 3.15, true)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试布尔类型
  let bool_attr = AttributeValue::bool(true)
  match bool_attr {
    BoolValue(b) => assert_eq(b, true)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试字符串数组类型
  let string_array = ["item1", "item2", "item3"]
  let string_array_attr = AttributeValue::array_string(string_array)
  match string_array_attr {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "item1")
      assert_eq(arr[2], "item3")
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试整数数组类型
  let int_array = [1L, 2L, 3L, 4L, 5L]
  let int_array_attr = AttributeValue::array_int(int_array)
  match int_array_attr {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1L)
      assert_eq(arr[4], 5L)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试浮点数组类型
  let float_array = [1.1, 2.2, 3.3]
  let float_array_attr = AttributeValue::array_float(float_array)
  match float_array_attr {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0] > 1.0 && arr[0] < 1.2, true)
      assert_eq(arr[2] > 3.2 && arr[2] < 3.4, true)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试布尔数组类型
  let bool_array = [true, false, true]
  let bool_array_attr = AttributeValue::array_bool(bool_array)
  match bool_array_attr {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}

test "attribute_value_edge_cases_and_boundary_values" {
  // 测试边界值和特殊情况
  
  // 空字符串
  let empty_string = common::AttributeValue::string("")
  match empty_string {
    common::StringValue(s) => assert_eq(s.length(), 0)
    _ => assert_eq(false, true)
  }
  
  // 长字符串
  let long_string = "a".repeat(1000)
  let long_string_attr = common::AttributeValue::string(long_string)
  match long_string_attr {
    common::StringValue(s) => assert_eq(s.length(), 1000)
    _ => assert_eq(false, true)
  }
  
  // 零值
  let zero_int = common::AttributeValue::int(0L)
  match zero_int {
    common::IntValue(i) => assert_eq(i, 0L)
    _ => assert_eq(false, true)
  }
  
  let zero_float = common::AttributeValue::float(0.0)
  match zero_float {
    common::FloatValue(f) => assert_eq(f, 0.0)
    _ => assert_eq(false, true)
  }
  
  // 负值
  let negative_int = common::AttributeValue::int(-100L)
  match negative_int {
    common::IntValue(i) => assert_eq(i, -100L)
    _ => assert_eq(false, true)
  }
  
  let negative_float = common::AttributeValue::float(-3.14)
  match negative_float {
    common::FloatValue(f) => assert_eq(f < -3.13 && f > -3.15, true)
    _ => assert_eq(false, true)
  }
  
  // 空数组
  let empty_string_array = common::AttributeValue::array_string([])
  match empty_string_array {
    common::ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_eq(false, true)
  }
  
  let empty_int_array = common::AttributeValue::array_int([])
  match empty_int_array {
    common::ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_eq(false, true)
  }
  
  // 单元素数组
  let single_element_array = common::AttributeValue::array_string(["only"])
  match single_element_array {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "only")
    }
    _ => assert_eq(false, true)
  }
}

test "attribute_value_complex_scenarios" {
  // 测试复杂场景和实际用例
  
  // HTTP请求相关属性
  let http_method = common::AttributeValue::string("GET")
  let http_status = common::AttributeValue::int(200L)
  let http_duration = common::AttributeValue::float(123.456)
  let http_success = common::AttributeValue::bool(true)
  
  // 验证HTTP属性
  match http_method {
    common::StringValue(method) => {
      assert_eq(method, "GET")
      assert_eq(method.length(), 3)
    }
    _ => assert_eq(false, true)
  }
  
  match http_status {
    common::IntValue(status) => {
      assert_eq(status, 200L)
      assert_eq(status >= 200L && status < 300L, true)  // 成功状态码
    }
    _ => assert_eq(false, true)
  }
  
  match http_duration {
    common::FloatValue(duration) => {
      assert_eq(duration > 100.0 && duration < 200.0, true)
    }
    _ => assert_eq(false, true)
  }
  
  match http_success {
    common::BoolValue(success) => assert_eq(success, true)
    _ => assert_eq(false, true)
  }
  
  // 复杂数组属性
  let user_tags = common::AttributeValue::array_string(["premium", "active", "verified"])
  let response_sizes = common::AttributeValue::array_int([1024L, 2048L, 4096L])
  let response_times = common::AttributeValue::array_float([0.1, 0.2, 0.15])
  let feature_flags = common::AttributeValue::array_bool([true, false, true])
  
  // 验证数组属性
  match user_tags {
    common::ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags.contains("premium"), true)
      assert_eq(tags.contains("active"), true)
      assert_eq(tags.contains("verified"), true)
    }
    _ => assert_eq(false, true)
  }
  
  match response_sizes {
    common::ArrayIntValue(sizes) => {
      assert_eq(sizes.length(), 3)
      assert_eq(sizes[0], 1024L)
      assert_eq(sizes[1], 2048L)
      assert_eq(sizes[2], 4096L)
    }
    _ => assert_eq(false, true)
  }
  
  match response_times {
    common::ArrayFloatValue(times) => {
      assert_eq(times.length(), 3)
      assert_eq(times[0] > 0.09 && times[0] < 0.11, true)
      assert_eq(times[1] > 0.19 && times[1] < 0.21, true)
      assert_eq(times[2] > 0.14 && times[2] < 0.16, true)
    }
    _ => assert_eq(false, true)
  }
  
  match feature_flags {
    common::ArrayBoolValue(flags) => {
      assert_eq(flags.length(), 3)
      assert_eq(flags[0], true)
      assert_eq(flags[1], false)
      assert_eq(flags[2], true)
    }
    _ => assert_eq(false, true)
  }
}