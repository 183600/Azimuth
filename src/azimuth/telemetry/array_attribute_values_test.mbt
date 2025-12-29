// Azimuth Telemetry - Array Attribute Values Tests
// 测试Array类型的AttributeValue

test "array_attribute_values_int_creation" {
  // 测试ArrayIntValue的创建和使用
  let int_array = @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue([1L, 2L, 3L, 4L, 5L])
  
  match int_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
      assert_eq(arr[2], 3L)
      assert_eq(arr[3], 4L)
      assert_eq(arr[4], 5L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  // 测试空数组
  let empty_int_array = @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue([])
  match empty_int_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 0)
    }
    _ => @test.fail("Expected empty ArrayIntValue")
  }
  
  // 测试边界值
  let boundary_int_array = @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue([
    -9223372036854775808L, // Int64最小值
    0L, // 零值
    9223372036854775807L  // Int64最大值
  ])
  match boundary_int_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], -9223372036854775808L)
      assert_eq(arr[1], 0L)
      assert_eq(arr[2], 9223372036854775807L)
    }
    _ => @test.fail("Expected boundary ArrayIntValue")
  }
}

test "array_attribute_values_float_creation" {
  // 测试ArrayFloatValue的创建和使用
  let float_array = @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue([1.1, 2.2, 3.3, 4.4, 5.5])
  
  match float_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[1], 2.2)
      assert_eq(arr[2], 3.3)
      assert_eq(arr[3], 4.4)
      assert_eq(arr[4], 5.5)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  // 测试空数组
  let empty_float_array = @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue([])
  match empty_float_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 0)
    }
    _ => @test.fail("Expected empty ArrayFloatValue")
  }
  
  // 测试特殊浮点值
  let special_float_array = @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue([
    0.0, // 零值
    -0.0, // 负零
    1.7976931348623157e+308, // Double最大值
    -1.7976931348623157e+308, // Double最小值
    0.0/0.0, // NaN
    1.0/0.0, // 正无穷
    -1.0/0.0  // 负无穷
  ])
  match special_float_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 7)
      
      // 验证NaN
      assert_eq(arr[4]/arr[4] != arr[4]/arr[4], true) // NaN检查
      
      // 验证无穷
      assert_eq(arr[5] > 0.0, true) // 正无穷检查
      assert_eq(arr[6] < 0.0, true) // 负无穷检查
    }
    _ => @test.fail("Expected special ArrayFloatValue")
  }
}

test "array_attribute_values_bool_creation" {
  // 测试ArrayBoolValue的创建和使用
  let bool_array = @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue([true, false, true, false, true])
  
  match bool_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
      assert_eq(arr[3], false)
      assert_eq(arr[4], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
  
  // 测试空数组
  let empty_bool_array = @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue([])
  match empty_bool_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 0)
    }
    _ => @test.fail("Expected empty ArrayBoolValue")
  }
  
  // 测试全true和全false数组
  let all_true_array = @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue([true, true, true, true, true])
  let all_false_array = @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue([false, false, false, false, false])
  
  match all_true_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 5)
      let mut index = 0
      while index < arr.length() {
        assert_eq(arr[index], true)
        index = index + 1
      }
    }
    _ => @test.fail("Expected all true ArrayBoolValue")
  }
  
  match all_false_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 5)
      let mut index = 0
      while index < arr.length() {
        assert_eq(arr[index], false)
        index = index + 1
      }
    }
    _ => @test.fail("Expected all false ArrayBoolValue")
  }
}

test "array_attribute_values_in_log_records" {
  // 测试Array类型的AttributeValue在LogRecord中的使用
  let log_record = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Log with array attributes")
    .with_attribute("int.values", @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue([10L, 20L, 30L]))
    .with_attribute("float.values", @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue([1.5, 2.5, 3.5]))
    .with_attribute("bool.values", @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue([true, false, true]))
    .with_attribute("string.values", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(["a", "b", "c"]))
    .build()
  
  assert_eq(log_record.attributes.length(), 4)
  
  // 验证int数组属性
  match log_record.attributes[0] {
    ("int.values", @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue(int_arr)) => {
      assert_eq(int_arr.length(), 3)
      assert_eq(int_arr[0], 10L)
      assert_eq(int_arr[1], 20L)
      assert_eq(int_arr[2], 30L)
    }
    _ => @test.fail("Expected int.values attribute")
  }
  
  // 验证float数组属性
  match log_record.attributes[1] {
    ("float.values", @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue(float_arr)) => {
      assert_eq(float_arr.length(), 3)
      assert_eq(float_arr[0], 1.5)
      assert_eq(float_arr[1], 2.5)
      assert_eq(float_arr[2], 3.5)
    }
    _ => @test.fail("Expected float.values attribute")
  }
  
  // 验证bool数组属性
  match log_record.attributes[2] {
    ("bool.values", @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue(bool_arr)) => {
      assert_eq(bool_arr.length(), 3)
      assert_eq(bool_arr[0], true)
      assert_eq(bool_arr[1], false)
      assert_eq(bool_arr[2], true)
    }
    _ => @test.fail("Expected bool.values attribute")
  }
  
  // 验证string数组属性
  match log_record.attributes[3] {
    ("string.values", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(string_arr)) => {
      assert_eq(string_arr.length(), 3)
      assert_eq(string_arr[0], "a")
      assert_eq(string_arr[1], "b")
      assert_eq(string_arr[2], "c")
    }
    _ => @test.fail("Expected string.values attribute")
  }
}

test "array_attribute_values_in_spans" {
  // 测试Array类型的AttributeValue在Span中的使用
  let (_, span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    @azimuth.telemetry.api.context.Context::empty(),
    "span-with-array-attributes",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("error.codes", @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue([404L, 500L, 503L])),
      ("response.times", @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue([100.5, 200.3, 150.7])),
      ("success.flags", @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue([true, false, true])),
      ("endpoints", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(["/api/users", "/api/orders", "/api/products"]))
    ])
  )
  
  assert_eq(span.attributes.length(), 4)
  
  // 验证error codes数组
  match span.attributes[0] {
    ("error.codes", @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue(error_codes)) => {
      assert_eq(error_codes.length(), 3)
      assert_eq(error_codes[0], 404L)
      assert_eq(error_codes[1], 500L)
      assert_eq(error_codes[2], 503L)
    }
    _ => @test.fail("Expected error.codes attribute")
  }
  
  // 验证response times数组
  match span.attributes[1] {
    ("response.times", @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue(response_times)) => {
      assert_eq(response_times.length(), 3)
      assert_eq(response_times[0], 100.5)
      assert_eq(response_times[1], 200.3)
      assert_eq(response_times[2], 150.7)
    }
    _ => @test.fail("Expected response.times attribute")
  }
  
  // 验证success flags数组
  match span.attributes[2] {
    ("success.flags", @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue(success_flags)) => {
      assert_eq(success_flags.length(), 3)
      assert_eq(success_flags[0], true)
      assert_eq(success_flags[1], false)
      assert_eq(success_flags[2], true)
    }
    _ => @test.fail("Expected success.flags attribute")
  }
  
  // 验证endpoints数组
  match span.attributes[3] {
    ("endpoints", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(endpoints)) => {
      assert_eq(endpoints.length(), 3)
      assert_eq(endpoints[0], "/api/users")
      assert_eq(endpoints[1], "/api/orders")
      assert_eq(endpoints[2], "/api/products")
    }
    _ => @test.fail("Expected endpoints attribute")
  }
}

test "array_attribute_values_edge_cases" {
  // 测试Array类型的AttributeValue的边界情况
  // 测试包含单个元素的数组
  let single_int = @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue([42L])
  let single_float = @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue([3.14])
  let single_bool = @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue([true])
  let single_string = @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(["single"])
  
  match single_int {
    @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 42L)
    }
    _ => @test.fail("Expected single element ArrayIntValue")
  }
  
  match single_float {
    @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 3.14)
    }
    _ => @test.fail("Expected single element ArrayFloatValue")
  }
  
  match single_bool {
    @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], true)
    }
    _ => @test.fail("Expected single element ArrayBoolValue")
  }
  
  match single_string {
    @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "single")
    }
    _ => @test.fail("Expected single element ArrayStringValue")
  }
  
  // 测试包含特殊值的数组
  let special_int_array = @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue([0L, -1L, 1L])
  let special_float_array = @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue([-0.0, 0.0, 1e-10, -1e-10])
  let special_string_array = @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(["", " ", "\t", "\n"])
  
  match special_int_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 0L)
      assert_eq(arr[1], -1L)
      assert_eq(arr[2], 1L)
    }
    _ => @test.fail("Expected special ArrayIntValue")
  }
  
  match special_string_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], "")
      assert_eq(arr[1], " ")
      assert_eq(arr[2], "\t")
      assert_eq(arr[3], "\n")
    }
    _ => @test.fail("Expected special ArrayStringValue")
  }
}