// Azimuth Attribute Value Type Conversion Tests
// 测试属性值类型转换功能

test "string to attribute value conversions" {
  // 测试字符串到字符串属性值的转换
  let str_val = "test_string"
  let string_attr = @azimuth.StringValue(str_val)
  match string_attr {
    @azimuth.StringValue(v) => assert_eq(v, "test_string")
    _ => assert_true(false)
  }
  
  // 测试空字符串
  let empty_str = ""
  let empty_attr = @azimuth.StringValue(empty_str)
  match empty_attr {
    @azimuth.StringValue(v) => assert_eq(v, "")
    _ => assert_true(false)
  }
  
  // 测试长字符串
  let long_str = "这是一个很长的测试字符串，用来测试系统对长字符串的处理能力"
  let long_attr = @azimuth.StringValue(long_str)
  match long_attr {
    @azimuth.StringValue(v) => assert_eq(v.length(), long_str.length())
    _ => assert_true(false)
  }
}

test "numeric to attribute value conversions" {
  // 测试整数转换
  let int_val = 42
  let int_attr = @azimuth.IntValue(int_val)
  match int_attr {
    @azimuth.IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // 测试负整数
  let neg_int = -100
  let neg_attr = @azimuth.IntValue(neg_int)
  match neg_attr {
    @azimuth.IntValue(v) => assert_eq(v, -100)
    _ => assert_true(false)
  }
  
  // 测试零值
  let zero = 0
  let zero_attr = @azimuth.IntValue(zero)
  match zero_attr {
    @azimuth.IntValue(v) => assert_eq(v, 0)
    _ => assert_true(false)
  }
  
  // 测试浮点数转换
  let float_val = 3.14159
  let float_attr = @azimuth.FloatValue(float_val)
  match float_attr {
    @azimuth.FloatValue(v) => assert_eq(v, 3.14159)
    _ => assert_true(false)
  }
  
  // 测试负浮点数
  let neg_float = -2.71828
  let neg_float_attr = @azimuth.FloatValue(neg_float)
  match neg_float_attr {
    @azimuth.FloatValue(v) => assert_eq(v, -2.71828)
    _ => assert_true(false)
  }
}

test "boolean to attribute value conversions" {
  // 测试true值
  let true_val = true
  let true_attr = @azimuth.BoolValue(true_val)
  match true_attr {
    @azimuth.BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
  
  // 测试false值
  let false_val = false
  let false_attr = @azimuth.BoolValue(false_val)
  match false_attr {
    @azimuth.BoolValue(v) => assert_false(v)
    _ => assert_true(false)
  }
}

test "array to attribute value conversions" {
  // 测试字符串数组
  let str_array = ["item1", "item2", "item3"]
  let str_array_attr = @azimuth.ArrayStringValue(str_array)
  match str_array_attr {
    @azimuth.ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "item1")
      assert_eq(arr[1], "item2")
      assert_eq(arr[2], "item3")
    }
    _ => assert_true(false)
  }
  
  // 测试整数数组
  let int_array = [1, 2, 3, 4, 5]
  let int_array_attr = @azimuth.ArrayIntValue(int_array)
  match int_array_attr {
    @azimuth.ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    _ => assert_true(false)
  }
  
  // 测试空数组
  let empty_array = []
  let empty_str_array_attr = @azimuth.ArrayStringValue(empty_array)
  match empty_str_array_attr {
    @azimuth.ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
}

test "attribute value type introspection" {
  let string_attr = @azimuth.StringValue("test")
  let int_attr = @azimuth.IntValue(42)
  let float_attr = @azimuth.FloatValue(3.14)
  let bool_attr = @azimuth.BoolValue(true)
  let str_array_attr = @azimuth.ArrayStringValue(["a", "b"])
  let int_array_attr = @azimuth.ArrayIntValue([1, 2])
  
  // 测试类型识别
  let mut string_count = 0
  let mut int_count = 0
  let mut float_count = 0
  let mut bool_count = 0
  let mut str_array_count = 0
  let mut int_array_count = 0
  
  let attributes = [string_attr, int_attr, float_attr, bool_attr, str_array_attr, int_array_attr]
  
  for attr in attributes {
    match attr {
      @azimuth.StringValue(_) => string_count = string_count + 1
      @azimuth.IntValue(_) => int_count = int_count + 1
      @azimuth.FloatValue(_) => float_count = float_count + 1
      @azimuth.BoolValue(_) => bool_count = bool_count + 1
      @azimuth.ArrayStringValue(_) => str_array_count = str_array_count + 1
      @azimuth.ArrayIntValue(_) => int_array_count = int_array_count + 1
    }
  }
  
  assert_eq(string_count, 1)
  assert_eq(int_count, 1)
  assert_eq(float_count, 1)
  assert_eq(bool_count, 1)
  assert_eq(str_array_count, 1)
  assert_eq(int_array_count, 1)
}

test "complex attribute value scenarios" {
  // 测试混合类型的属性集合
  let complex_attrs = @azimuth.Attributes {
    values : [
      ("service.name", @azimuth.StringValue("payment-service")),
      ("service.port", @azimuth.IntValue(8080)),
      ("service.enabled", @azimuth.BoolValue(true)),
      ("service.version", @azimuth.StringValue("1.2.3")),
      ("service.endpoints", @azimuth.ArrayStringValue(["/api/v1/pay", "/api/v1/refund"])),
      ("service.retry.count", @azimuth.IntValue(3)),
      ("service.timeout", @azimuth.FloatValue(30.5)),
      ("service.tags", @azimuth.ArrayStringValue(["critical", "payment", "api"]))
    ]
  }
  
  // 验证属性数量
  assert_eq(complex_attrs.values.length(), 8)
  
  // 验证字符串属性
  let service_name = complex_attrs.values[0]
  match service_name.1 {
    @azimuth.StringValue(v) => assert_eq(v, "payment-service")
    _ => assert_true(false)
  }
  
  // 验证整数属性
  let service_port = complex_attrs.values[1]
  match service_port.1 {
    @azimuth.IntValue(v) => assert_eq(v, 8080)
    _ => assert_true(false)
  }
  
  // 验证布尔属性
  let service_enabled = complex_attrs.values[2]
  match service_enabled.1 {
    @azimuth.BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
  
  // 验证字符串数组属性
  let service_endpoints = complex_attrs.values[4]
  match service_endpoints.1 {
    @azimuth.ArrayStringValue(arr) => {
      assert_eq(arr.length(), 2)
      assert_eq(arr[0], "/api/v1/pay")
      assert_eq(arr[1], "/api/v1/refund")
    }
    _ => assert_true(false)
  }
  
  // 验证浮点数属性
  let service_timeout = complex_attrs.values[6]
  match service_timeout.1 {
    @azimuth.FloatValue(v) => assert_eq(v, 30.5)
    _ => assert_true(false)
  }
}

test "edge case conversions" {
  // 测试极大数值
  let large_int = 2147483647  // Int32 max
  let large_attr = @azimuth.IntValue(large_int)
  match large_attr {
    @azimuth.IntValue(v) => assert_eq(v, 2147483647)
    _ => assert_true(false)
  }
  
  // 测试极小数值
  let small_int = -2147483648  // Int32 min
  let small_attr = @azimuth.IntValue(small_int)
  match small_attr {
    @azimuth.IntValue(v) => assert_eq(v, -2147483648)
    _ => assert_true(false)
  }
  
  // 测试极大浮点数
  let large_float = 1.7976931348623157e+308  // Double max
  let large_float_attr = @azimuth.FloatValue(large_float)
  match large_float_attr {
    @azimuth.FloatValue(v) => assert_eq(v, 1.7976931348623157e+308)
    _ => assert_true(false)
  }
  
  // 测试极小浮点数
  let small_float = 2.2250738585072014e-308  // Double min
  let small_float_attr = @azimuth.FloatValue(small_float)
  match small_float_attr {
    @azimuth.FloatValue(v) => assert_eq(v, 2.2250738585072014e-308)
    _ => assert_true(false)
  }
  
  // 测试特殊浮点值
  let zero_float = 0.0
  let zero_float_attr = @azimuth.FloatValue(zero_float)
  match zero_float_attr {
    @azimuth.FloatValue(v) => assert_eq(v, 0.0)
    _ => assert_true(false)
  }
}