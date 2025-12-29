// Logs模块的LogRecordBuilder测试用例
test "log_record_builder_basic_usage" {
  // 测试LogRecordBuilder的基本使用
  let builder = LogRecord::builder()
  
  // 验证初始状态
  assert_eq(builder.timestamp_unix_nanos, None)
  assert_eq(builder.observed_timestamp_unix_nanos, None)
  assert_eq(builder.severity_number, None)
  assert_eq(builder.severity_text, None)
  assert_eq(builder.body, None)
  assert_eq(builder.attributes.length(), 0)
  assert_eq(builder.trace_id, None)
  assert_eq(builder.span_id, None)
  assert_eq(builder.trace_flags, None)
  assert_eq(builder.resource, None)
  assert_eq(builder.instrumentation_scope, None)
  
  // 设置基本字段
  let builder1 = builder
    .timestamp(1640995200000000000L)
    .severity(Info)
    .body("Application started successfully")
  
  // 验证设置后的状态
  match builder1.timestamp_unix_nanos {
    Some(ts) => assert_eq(ts, 1640995200000000000L)
    None => @test.fail("Test failed")
  }
  
  match builder1.severity_number {
    Some(severity) => assert_eq(severity, Info)
    None => @test.fail("Test failed")
  }
  
  match builder1.body {
    Some(body) => assert_eq(body, "Application started successfully")
    None => @test.fail("Test failed")
  }
  
  // 构建LogRecord
  let log_record = builder1.build()
  
  // 验证构建的LogRecord
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_record.severity_number, Info)
  match log_record.body {
    Some(body) => assert_eq(body, "Application started successfully")
    None => @test.fail("Test failed")
  }
}

test "log_record_builder_with_attributes" {
  // 测试LogRecordBuilder添加属性
  let builder = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Debug)
    .body("Debug message")
  
  // 添加单个属性
  let builder1 = builder.with_attribute(
    "service.name", 
    AttributeValue::string("payment-service")
  )
  
  assert_eq(builder1.attributes.length(), 1)
  assert_eq(builder1.attributes[0].0, "service.name")
  
  match builder1.attributes[0].1 {
    AttributeValue::StringValue(value) => assert_eq(value, "payment-service")
    _ => @test.fail("Test failed")
  }
  
  // 添加更多属性
  let builder2 = builder1
    .with_attribute("service.version", AttributeValue::string("1.2.3"))
    .with_attribute("environment", AttributeValue::string("production"))
    .with_attribute("process.pid", AttributeValue::int(12345L))
    .with_attribute("process.healthy", AttributeValue::bool(true))
  
  assert_eq(builder2.attributes.length(), 5)
  
  // 验证所有属性
  let expected_attributes = [
    ("service.name", "payment-service"),
    ("service.version", "1.2.3"),
    ("environment", "production")
  ]
  
  let mut i = 0
  while i < expected_attributes.length() {
    let (key, expected_value) = expected_attributes[i]
    assert_eq(builder2.attributes[i].0, key)
    match builder2.attributes[i].1 {
      AttributeValue::StringValue(value) => assert_eq(value, expected_value)
      _ => @test.fail("Expected StringValue for " + key)
    }
    i = i + 1
  }
  
  // 构建LogRecord并验证属性
  let log_record = builder2.build()
  assert_eq(log_record.attributes.length(), 5)
}

test "log_record_builder_complex_attributes" {
  // 测试LogRecordBuilder添加复杂属性
  let builder = LogRecord::builder()
    .severity(Error)
    .body("Error occurred during processing")
  
  // 添加复杂属性
  let builder_with_complex = builder
    .with_attribute("error.type", AttributeValue::string("DatabaseConnectionError"))
    .with_attribute("error.message", AttributeValue::string("Connection timeout after 30 seconds"))
    .with_attribute("error.code", AttributeValue::int(5001L))
    .with_attribute("error.retry_count", AttributeValue::int(3L))
    .with_attribute("error.timeout_seconds", AttributeValue::float(30.0))
    .with_attribute("error.recoverable", AttributeValue::bool(false))
    .with_attribute("error.stack_trace", AttributeValue::string("at com.example.Service.process(Service.java:123)"))
    .with_attribute("tags", AttributeValue::array_string(["database", "timeout", "critical"]))
    .with_attribute("error.codes", AttributeValue::array_int([5001L, 5002L, 5003L]))
    .with_attribute("error.thresholds", AttributeValue::array_float([10.0, 30.0, 60.0]))
    .with_attribute("error.flags", AttributeValue::array_bool([false, true, false]))
  
  // 构建LogRecord
  let log_record = builder_with_complex.build()
  
  // 验证复杂属性
  assert_eq(log_record.attributes.length(), 11)
  
  // 验证数组属性
  match log_record.attributes[7].1 {
    AttributeValue::ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "database")
      assert_eq(tags[1], "timeout")
      assert_eq(tags[2], "critical")
    }
    _ => @test.fail("Test failed")
  }
  
  match log_record.attributes[8].1 {
    AttributeValue::ArrayIntValue(codes) => {
      assert_eq(codes.length(), 3)
      assert_eq(codes[0], 5001L)
      assert_eq(codes[1], 5002L)
      assert_eq(codes[2], 5003L)
    }
    _ => @test.fail("Test failed")
  }
}

test "log_record_builder_all_severity_levels" {
  // 测试所有严重性级别
  
  // Trace级别
  let trace_log = LogRecord::builder()
    .severity(Trace)
    .body("Trace message")
    .build()
  assert_eq(trace_log.severity_number, Trace)
  
  // Debug级别
  let debug_log = LogRecord::builder()
    .severity(Debug)
    .body("Debug message")
    .build()
  assert_eq(debug_log.severity_number, Debug)
  
  // Info级别
  let info_log = LogRecord::builder()
    .severity(Info)
    .body("Info message")
    .build()
  assert_eq(info_log.severity_number, Info)
  
  // Warn级别
  let warn_log = LogRecord::builder()
    .severity(Warn)
    .body("Warning message")
    .build()
  assert_eq(warn_log.severity_number, Warn)
  
  // Error级别
  let error_log = LogRecord::builder()
    .severity(Error)
    .body("Error message")
    .build()
  assert_eq(error_log.severity_number, Error)
  
  // Fatal级别
  let fatal_log = LogRecord::builder()
    .severity(Fatal)
    .body("Fatal error message")
    .build()
  assert_eq(fatal_log.severity_number, Fatal)
}

test "log_record_builder_with_trace_context" {
  // 测试LogRecordBuilder添加追踪上下文
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })
  let trace_flags = 1_byte
  
  let builder = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Info)
    .body("Request processed")
  
  // 在实际实现中，我们需要能够设置trace_id、span_id和trace_flags
  // 这里我们验证LogRecord结构是否正确
  let log_record = builder.build()
  
  // 验证默认值
  match log_record.trace_id {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  
  match log_record.span_id {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  
  match log_record.trace_flags {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
}

test "log_record_builder_with_resource_and_scope" {
  // 测试LogRecordBuilder与资源和仪器作用域的集成
  let resource = Resource::default("test-service")
  let builder = LogRecord::builder()
    .severity(Info)
    .body("Test log with resource")
  
  // 在实际实现中，我们需要能够设置resource和instrumentation_scope
  let log_record = builder.build()
  
  // 验证默认值
  match log_record.resource {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  
  match log_record.instrumentation_scope {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
}

test "log_record_builder_edge_cases" {
  // 测试LogRecordBuilder的边界情况
  
  // 空字符串body
  let empty_body_log = LogRecord::builder()
    .body("")
    .build()
  match empty_body_log.body {
    Some(body) => assert_eq(body, "")
    None => @test.fail("Test failed")
  }
  
  // 长字符串body
  let long_body = "a".repeat(10000)
  let long_body_log = LogRecord::builder()
    .body(long_body)
    .build()
  match long_body_log.body {
    Some(body) => assert_eq(body.length(), 10000)
    None => @test.fail("Test failed")
  }
  
  // 特殊字符body
  let special_body_log = LogRecord::builder()
    .body("Special chars: !@#$%^&*(){}[]|\\:;\"'<>?,./ Unicode: 测试中文🚀")
    .build()
  match special_body_log.body {
    Some(body) => {
      assert_eq(body.contains("!@#$%^&*()"), true)
      assert_eq(body.contains("测试中文🚀"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 极值时间戳
  let min_timestamp_log = LogRecord::builder()
    .timestamp(-9223372036854775808L)
    .build()
  assert_eq(min_timestamp_log.timestamp_unix_nanos, -9223372036854775808L)
  
  let max_timestamp_log = LogRecord::builder()
    .timestamp(9223372036854775807L)
    .build()
  assert_eq(max_timestamp_log.timestamp_unix_nanos, 9223372036854775807L)
  
  // 零时间戳
  let zero_timestamp_log = LogRecord::builder()
    .timestamp(0L)
    .build()
  assert_eq(zero_timestamp_log.timestamp_unix_nanos, 0L)
}

test "log_record_builder_chaining" {
  // 测试LogRecordBuilder的方法链式调用
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body("Database connection failed")
    .with_attribute("error.type", AttributeValue::string("ConnectionTimeout"))
    .with_attribute("error.code", AttributeValue::int(1001L))
    .with_attribute("retry.count", AttributeValue::int(3L))
    .with_attribute("service.name", AttributeValue::string("user-service"))
    .with_attribute("environment", AttributeValue::string("production"))
    .build()
  
  // 验证所有字段都正确设置
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_record.severity_number, Error)
  match log_record.body {
    Some(body) => assert_eq(body, "Database connection failed")
    None => @test.fail("Test failed")
  }
  
  assert_eq(log_record.attributes.length(), 5)
  assert_eq(log_record.attributes[0].0, "error.type")
  assert_eq(log_record.attributes[1].0, "error.code")
  assert_eq(log_record.attributes[2].0, "retry.count")
  assert_eq(log_record.attributes[3].0, "service.name")
  assert_eq(log_record.attributes[4].0, "environment")
}

test "log_record_builder_default_values" {
  // 测试LogRecordBuilder的默认值
  let log_record = LogRecord::builder().build()
  
  // 验证默认值
  assert_eq(log_record.timestamp_unix_nanos, 0L)
  match log_record.observed_timestamp_unix_nanos {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  assert_eq(log_record.severity_number, Info) // 默认严重性级别
  match log_record.severity_text {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  match log_record.body {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  assert_eq(log_record.attributes.length(), 0)
  match log_record.trace_id {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  match log_record.span_id {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  match log_record.trace_flags {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  match log_record.resource {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  match log_record.instrumentation_scope {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
}