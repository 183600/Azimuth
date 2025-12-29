// LogRecordBuilder复杂链式调用测试
test "log_record_builder_basic_chaining" {
  // 测试基本的链式调用
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Info)
    .body("Basic chain test")
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  assert_eq(log_record.severity_number, Info)
  match log_record.body {
    Some(body) => assert_eq(body, "Basic chain test")
    None => @test.fail("Test failed")
  }
  
  // 测试带有属性的链式调用
  let log_record_with_attrs = LogRecord::builder()
    .timestamp(1234567891L)
    .severity(Error)
    .body("Error with attributes")
    .with_attribute("error.code", AttributeValue::int(500L))
    .with_attribute("error.message", AttributeValue::string("Internal Server Error"))
    .with_attribute("service.name", AttributeValue::string("test-service"))
    .build()
  
  assert_eq(log_record_with_attrs.timestamp_unix_nanos, 1234567891L)
  assert_eq(log_record_with_attrs.severity_number, Error)
  assert_eq(log_record_with_attrs.attributes.length(), 3)
  
  // 验证属性
  let mut found_error_code = false
  let mut found_error_message = false
  let mut found_service_name = false
  
  let mut i = 0
  while i < log_record_with_attrs.attributes.length() {
    let (key, value) = log_record_with_attrs.attributes[i]
    
    match key {
      "error.code" => {
        match value {
          IntValue(code) => {
            assert_eq(code, 500L)
            found_error_code = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "error.message" => {
        match value {
          StringValue(message) => {
            assert_eq(message, "Internal Server Error")
            found_error_message = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "service.name" => {
        match value {
          StringValue(name) => {
            assert_eq(name, "test-service")
            found_service_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => () // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_error_code, true)
  assert_eq(found_error_message, true)
  assert_eq(found_service_name, true)
}

test "log_record_builder_complex_chaining" {
  // 测试复杂的链式调用场景
  let resource = Resource::default("complex-test-service")
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte })
  let trace_flags = 1_byte
  
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .observed_timestamp(1234567891L)
    .severity(Warn)
    .severity_text("WARN")
    .body("Complex log record")
    .with_attribute("service.name", AttributeValue::string("complex-service"))
    .with_attribute("service.version", AttributeValue::string("2.1.0"))
    .with_attribute("operation.name", AttributeValue::string("complex.operation"))
    .with_attribute("user.id", AttributeValue::string("user-123456"))
    .with_attribute("session.id", AttributeValue::string("session-abcdef"))
    .with_attribute("request.id", AttributeValue::string("req-1234567890"))
    .with_attribute("host.name", AttributeValue::string("prod-server-01"))
    .with_attribute("process.pid", AttributeValue::int(12345L))
    .with_attribute("process.cpu.percent", AttributeValue::float(75.5))
    .with_attribute("process.healthy", AttributeValue::bool(true))
    .with_attribute("tags", AttributeValue::array_string(["api", "critical", "production"]))
    .with_attribute("ports", AttributeValue::array_int([8080L, 8443L, 9090L]))
    .with_attribute("thresholds", AttributeValue::array_float([60.0, 80.0, 95.0]))
    .with_attribute("features", AttributeValue::array_bool([true, false, true, false]))
    .with_trace_id(trace_id)
    .with_span_id(span_id)
    .with_trace_flags(trace_flags)
    .with_resource(resource)
    .build()
  
  // 验证基本字段
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  match log_record.observed_timestamp_unix_nanos {
    Some(observed) => assert_eq(observed, 1234567891L)
    None => @test.fail("Test failed")
  }
  assert_eq(log_record.severity_number, Warn)
  match log_record.severity_text {
    Some(text) => assert_eq(text, "WARN")
    None => @test.fail("Test failed")
  }
  match log_record.body {
    Some(body) => assert_eq(body, "Complex log record")
    None => @test.fail("Test failed")
  }
  
  // 验证属性数量
  assert_eq(log_record.attributes.length(), 15)
  
  // 验证追踪信息
  match log_record.trace_id {
    Some(id) => assert_eq(id.length(), 16)
    None => @test.fail("Test failed")
  }
  
  match log_record.span_id {
    Some(id) => assert_eq(id.length(), 8)
    None => @test.fail("Test failed")
  }
  
  match log_record.trace_flags {
    Some(flags) => assert_eq(flags, 1_byte)
    None => @test.fail("Test failed")
  }
  
  // 验证资源
  match log_record.resource {
    Some(res) => {
      assert_eq(res.service_name, "complex-test-service")
      assert_eq(res.telemetry_sdk_name, "azimuth")
      assert_eq(res.telemetry_sdk_version, "0.1.0")
    }
    None => @test.fail("Test failed")
  }
}

test "log_record_builder_severity_chaining" {
  // 测试不同严重程度的链式调用
  let trace_log = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Trace)
    .severity_text("TRACE")
    .body("Trace level message")
    .build()
  
  let debug_log = LogRecord::builder()
    .timestamp(1234567891L)
    .severity(Debug)
    .severity_text("DEBUG")
    .body("Debug level message")
    .build()
  
  let info_log = LogRecord::builder()
    .timestamp(1234567892L)
    .severity(Info)
    .severity_text("INFO")
    .body("Info level message")
    .build()
  
  let warn_log = LogRecord::builder()
    .timestamp(1234567893L)
    .severity(Warn)
    .severity_text("WARN")
    .body("Warning level message")
    .build()
  
  let error_log = LogRecord::builder()
    .timestamp(1234567894L)
    .severity(Error)
    .severity_text("ERROR")
    .body("Error level message")
    .build()
  
  let fatal_log = LogRecord::builder()
    .timestamp(1234567895L)
    .severity(Fatal)
    .severity_text("FATAL")
    .body("Fatal level message")
    .build()
  
  // 验证所有严重程度
  assert_eq(trace_log.severity_number, Trace)
  assert_eq(debug_log.severity_number, Debug)
  assert_eq(info_log.severity_number, Info)
  assert_eq(warn_log.severity_number, Warn)
  assert_eq(error_log.severity_number, Error)
  assert_eq(fatal_log.severity_number, Fatal)
  
  // 验证严重程度文本
  match trace_log.severity_text {
    Some(text) => assert_eq(text, "TRACE")
    None => @test.fail("Test failed")
  }
  
  match debug_log.severity_text {
    Some(text) => assert_eq(text, "DEBUG")
    None => @test.fail("Test failed")
  }
  
  match info_log.severity_text {
    Some(text) => assert_eq(text, "INFO")
    None => @test.fail("Test failed")
  }
  
  match warn_log.severity_text {
    Some(text) => assert_eq(text, "WARN")
    None => @test.fail("Test failed")
  }
  
  match error_log.severity_text {
    Some(text) => assert_eq(text, "ERROR")
    None => @test.fail("Test failed")
  }
  
  match fatal_log.severity_text {
    Some(text) => assert_eq(text, "FATAL")
    None => @test.fail("Test failed")
  }
  
  // 验证时间顺序
  assert_eq(trace_log.timestamp_unix_nanos, 1234567890L)
  assert_eq(debug_log.timestamp_unix_nanos, 1234567891L)
  assert_eq(info_log.timestamp_unix_nanos, 1234567892L)
  assert_eq(warn_log.timestamp_unix_nanos, 1234567893L)
  assert_eq(error_log.timestamp_unix_nanos, 1234567894L)
  assert_eq(fatal_log.timestamp_unix_nanos, 1234567895L)
}

test "log_record_builder_attribute_type_chaining" {
  // 测试不同类型属性的链式调用
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Info)
    .body("Attribute type test")
    // 字符串属性
    .with_attribute("string.attr", AttributeValue::string("string_value"))
    // 整数属性
    .with_attribute("int.attr", AttributeValue::int(42L))
    // 浮点数属性
    .with_attribute("float.attr", AttributeValue::float(3.14159))
    // 布尔属性
    .with_attribute("bool.attr", AttributeValue::bool(true))
    // 字符串数组属性
    .with_attribute("string.array.attr", AttributeValue::array_string(["a", "b", "c"]))
    // 整数数组属性
    .with_attribute("int.array.attr", AttributeValue::array_int([1L, 2L, 3L, 4L, 5L]))
    // 浮点数数组属性
    .with_attribute("float.array.attr", AttributeValue::array_float([1.1, 2.2, 3.3]))
    // 布尔数组属性
    .with_attribute("bool.array.attr", AttributeValue::array_bool([true, false, true]))
    .build()
  
  assert_eq(log_record.attributes.length(), 8)
  
  // 验证每种类型的属性
  let mut string_found = false
  let mut int_found = false
  let mut float_found = false
  let mut bool_found = false
  let mut string_array_found = false
  let mut int_array_found = false
  let mut float_array_found = false
  let mut bool_array_found = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    match key {
      "string.attr" => {
        match value {
          StringValue(s) => {
            assert_eq(s, "string_value")
            string_found = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "int.attr" => {
        match value {
          IntValue(iv) => {
            assert_eq(iv, 42L)
            int_found = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "float.attr" => {
        match value {
          FloatValue(fv) => {
            assert_eq(fv, 3.14159)
            float_found = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "bool.attr" => {
        match value {
          BoolValue(bv) => {
            assert_eq(bv, true)
            bool_found = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "string.array.attr" => {
        match value {
          ArrayStringValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], "a")
            assert_eq(arr[1], "b")
            assert_eq(arr[2], "c")
            string_array_found = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "int.array.attr" => {
        match value {
          ArrayIntValue(arr) => {
            assert_eq(arr.length(), 5)
            assert_eq(arr[0], 1L)
            assert_eq(arr[4], 5L)
            int_array_found = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "float.array.attr" => {
        match value {
          ArrayFloatValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], 1.1)
            assert_eq(arr[2], 3.3)
            float_array_found = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "bool.array.attr" => {
        match value {
          ArrayBoolValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], true)
            assert_eq(arr[1], false)
            assert_eq(arr[2], true)
            bool_array_found = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => () // 忽略其他属性
    }
    
    i = i + 1
  }
  
  // 验证所有类型都找到了
  assert_eq(string_found, true)
  assert_eq(int_found, true)
  assert_eq(float_found, true)
  assert_eq(bool_found, true)
  assert_eq(string_array_found, true)
  assert_eq(int_array_found, true)
  assert_eq(float_array_found, true)
  assert_eq(bool_array_found, true)
}

test "log_record_builder_trace_context_chaining" {
  // 测试追踪上下文的链式调用
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x12_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x34_byte })
  let trace_flags = 0x01_byte
  
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Info)
    .body("Trace context test")
    .with_trace_id(trace_id)
    .with_span_id(span_id)
    .with_trace_flags(trace_flags)
    .with_attribute("trace.id", AttributeValue::string("12345678901234567890123456789012"))
    .with_attribute("span.id", AttributeValue::string("1234567890123456"))
    .with_attribute("trace.sampled", AttributeValue::bool(true))
    .build()
  
  // 验证追踪上下文
  match log_record.trace_id {
    Some(id) => {
      assert_eq(id.length(), 16)
      assert_eq(id[0], 0x12_byte)
      assert_eq(id[15], 0x12_byte)
    }
    None => @test.fail("Test failed")
  }
  
  match log_record.span_id {
    Some(id) => {
      assert_eq(id.length(), 8)
      assert_eq(id[0], 0x34_byte)
      assert_eq(id[7], 0x34_byte)
    }
    None => @test.fail("Test failed")
  }
  
  match log_record.trace_flags {
    Some(flags) => assert_eq(flags, 0x01_byte)
    None => @test.fail("Test failed")
  }
  
  // 验证追踪相关属性
  assert_eq(log_record.attributes.length(), 3)
  
  let mut trace_id_found = false
  let mut span_id_found = false
  let mut trace_sampled_found = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    match key {
      "trace.id" => {
        match value {
          StringValue(id) => {
            assert_eq(id, "12345678901234567890123456789012")
            trace_id_found = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "span.id" => {
        match value {
          StringValue(id) => {
            assert_eq(id, "1234567890123456")
            span_id_found = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "trace.sampled" => {
        match value {
          BoolValue(sampled) => {
            assert_eq(sampled, true)
            trace_sampled_found = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => () // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(trace_id_found, true)
  assert_eq(span_id_found, true)
  assert_eq(trace_sampled_found, true)
}

test "log_record_builder_resource_chaining" {
  // 测试资源的链式调用
  let resource = Resource::default("resource-test-service")
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Info)
    .body("Resource test")
    .with_resource(resource)
    .with_attribute("service.name", AttributeValue::string("resource-test-service"))
    .with_attribute("service.namespace", AttributeValue::string("production"))
    .with_attribute("service.version", AttributeValue::string("1.0.0"))
    .with_attribute("deployment.environment", AttributeValue::string("prod"))
    .build()
  
  // 验证资源
  match log_record.resource {
    Some(res) => {
      assert_eq(res.service_name, "resource-test-service")
      assert_eq(res.telemetry_sdk_name, "azimuth")
      assert_eq(res.telemetry_sdk_version, "0.1.0")
    }
    None => @test.fail("Test failed")
  }
  
  // 验证资源相关属性
  assert_eq(log_record.attributes.length(), 4)
}

test "log_record_builder_edge_cases_chaining" {
  // 测试边界情况的链式调用
  
  // 测试空字符串和特殊值
  let edge_case_log = LogRecord::builder()
    .timestamp(0L)
    .severity(Info)
    .body("")
    .severity_text("")
    .with_attribute("empty.string", AttributeValue::string(""))
    .with_attribute("zero.int", AttributeValue::int(0L))
    .with_attribute("negative.int", AttributeValue::int(-1L))
    .with_attribute("zero.float", AttributeValue::float(0.0))
    .with_attribute("negative.float", AttributeValue::float(-1.0))
    .with_attribute("false.bool", AttributeValue::bool(false))
    .with_attribute("empty.array.string", AttributeValue::array_string([]))
    .with_attribute("empty.array.int", AttributeValue::array_int([]))
    .with_attribute("empty.array.float", AttributeValue::array_float([]))
    .with_attribute("empty.array.bool", AttributeValue::array_bool([]))
    .build()
  
  // 验证边界值
  assert_eq(edge_case_log.timestamp_unix_nanos, 0L)
  match edge_case_log.body {
    Some(body) => assert_eq(body, "")
    None => @test.fail("Test failed")
  }
  match edge_case_log.severity_text {
    Some(text) => assert_eq(text, "")
    None => @test.fail("Test failed")
  }
  
  // 验证空数组属性
  assert_eq(edge_case_log.attributes.length(), 9)
  
  // 测试极大值
  let max_values_log = LogRecord::builder()
    .timestamp(9223372036854775807L)
    .severity(Fatal)
    .body("Maximum values test")
    .with_attribute("max.int", AttributeValue::int(9223372036854775807L))
    .with_attribute("min.int", AttributeValue::int(-9223372036854775808L))
    .with_attribute("max.float", AttributeValue::float(1.7976931348623157e+308))
    .with_attribute("min.float", AttributeValue::float(-1.7976931348623157e+308))
    .with_attribute("infinity.float", AttributeValue::float(1.0/0.0))
    .with_attribute("neg.infinity.float", AttributeValue::float(-1.0/0.0))
    .with_attribute("nan.float", AttributeValue::float(0.0/0.0))
    .build()
  
  // 验证极大值
  assert_eq(max_values_log.timestamp_unix_nanos, 9223372036854775807L)
  assert_eq(max_values_log.severity_number, Fatal)
  assert_eq(max_values_log.attributes.length(), 7)
}

test "log_record_builder_performance_chaining" {
  // 测试性能相关的链式调用
  
  // 测试大量属性的链式调用
  let mut builder = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Info)
    .body("Performance test with many attributes")
  
  // 添加100个属性
  let mut i = 0
  while i < 100 {
    builder = builder.with_attribute(
      "attr." + i.to_string(),
      AttributeValue::int(i.to_int64())
    )
    i = i + 1
  }
  
  let performance_log = builder.build()
  
  // 验证所有属性都被添加
  assert_eq(performance_log.attributes.length(), 100)
  
  // 验证第一个和最后一个属性
  match performance_log.attributes[0].1 {
    IntValue(val) => assert_eq(val, 0L)
    _ => @test.fail("Test failed")
  }
  
  match performance_log.attributes[99].1 {
    IntValue(val) => assert_eq(val, 99L)
    _ => @test.fail("Test failed")
  }
  
  // 测试大型数组属性
  let large_string_array = [for i = 0; i < 1000; i = i + 1].map(fn(_) { "large_array_item" })
  let large_int_array = [for i = 0; i < 1000; i = i + 1].map(fn(_) { 42L })
  let large_float_array = [for i = 0; i < 1000; i = i + 1].map(fn(_) { 3.14 })
  let large_bool_array = [for i = 0; i < 1000; i = i + 1].map(fn(_) { true })
  
  let large_array_log = LogRecord::builder()
    .timestamp(1234567891L)
    .severity(Debug)
    .body("Large arrays test")
    .with_attribute("large.string.array", AttributeValue::array_string(large_string_array))
    .with_attribute("large.int.array", AttributeValue::array_int(large_int_array))
    .with_attribute("large.float.array", AttributeValue::array_float(large_float_array))
    .with_attribute("large.bool.array", AttributeValue::array_bool(large_bool_array))
    .build()
  
  // 验证大型数组
  assert_eq(large_array_log.attributes.length(), 4)
  
  match large_array_log.attributes[0].1 {
    ArrayStringValue(arr) => assert_eq(arr.length(), 1000)
    _ => @test.fail("Test failed")
  }
  
  match large_array_log.attributes[1].1 {
    ArrayIntValue(arr) => assert_eq(arr.length(), 1000)
    _ => @test.fail("Test failed")
  }
  
  match large_array_log.attributes[2].1 {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 1000)
    _ => @test.fail("Test failed")
  }
  
  match large_array_log.attributes[3].1 {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 1000)
    _ => @test.fail("Test failed")
  }
}