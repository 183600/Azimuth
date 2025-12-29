// LogRecordBuilder综合测试
test "log_record_builder_basic_workflow" {
  // 测试LogRecordBuilder的基本工作流程
  
  let builder = LogRecord::builder()
  
  // 设置基本字段
  let builder_with_timestamp = builder.timestamp(1234567890L)
  let builder_with_severity = builder_with_timestamp.severity(Info)
  let builder_with_body = builder_with_severity.body("Test log message")
  
  // 构建LogRecord
  let log_record = builder_with_body.build()
  
  // 验证基本字段
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  match log_record.severity_number {
    Info => assert_eq(true, true)
    _ => @test.fail("Expected Info severity")
  }
  match log_record.body {
    Some(message) => assert_eq(message, "Test log message")
    None => @test.fail("Expected body message")
  }
  
  // 验证默认值
  assert_eq(log_record.observed_timestamp_unix_nanos, None)
  assert_eq(log_record.severity_text, None)
  assert_eq(log_record.attributes.length(), 0)
  assert_eq(log_record.trace_id, None)
  assert_eq(log_record.span_id, None)
  assert_eq(log_record.trace_flags, None)
  assert_eq(log_record.resource, None)
  assert_eq(log_record.instrumentation_scope, None)
}

test "log_record_builder_with_attributes" {
  // 测试使用LogRecordBuilder添加属性
  
  let builder = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Error)
    .body("Error occurred")
  
  // 添加单个属性
  let builder_with_attr1 = builder.with_attribute("error.code", AttributeValue::int(500L))
  let builder_with_attr2 = builder_with_attr1.with_attribute("error.message", AttributeValue::string("Internal server error"))
  let builder_with_attr3 = builder_with_attr2.with_attribute("user.id", AttributeValue::int(12345L))
  
  // 构建LogRecord
  let log_record = builder_with_attr3.build()
  
  // 验证属性
  assert_eq(log_record.attributes.length(), 3)
  assert_eq(log_record.attributes[0].0, "error.code")
  assert_eq(log_record.attributes[1].0, "error.message")
  assert_eq(log_record.attributes[2].0, "user.id")
  
  // 验证属性值
  match log_record.attributes[0].1 {
    IntValue(code) => assert_eq(code, 500L)
    _ => @test.fail("Expected IntValue for error.code")
  }
  
  match log_record.attributes[1].1 {
    StringValue(message) => assert_eq(message, "Internal server error")
    _ => @test.fail("Expected StringValue for error.message")
  }
  
  match log_record.attributes[2].1 {
    IntValue(user_id) => assert_eq(user_id, 12345L)
    _ => @test.fail("Expected IntValue for user.id")
  }
}

test "log_record_builder_with_complex_attributes" {
  // 测试LogRecordBuilder添加复杂属性
  
  let builder = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Warn)
    .body("Complex log with various attribute types")
  
  // 添加各种类型的属性
  let builder = builder
    .with_attribute("string.value", AttributeValue::string("test string"))
    .with_attribute("int.value", AttributeValue::int(42L))
    .with_attribute("float.value", AttributeValue::float(3.14159))
    .with_attribute("bool.value", AttributeValue::bool(true))
    .with_attribute("string.array", AttributeValue::array_string(["a", "b", "c"]))
    .with_attribute("int.array", AttributeValue::array_int([1L, 2L, 3L]))
    .with_attribute("float.array", AttributeValue::array_float([1.1, 2.2, 3.3]))
    .with_attribute("bool.array", AttributeValue::array_bool([true, false, true]))
  
  // 构建LogRecord
  let log_record = builder.build()
  
  // 验证属性数量
  assert_eq(log_record.attributes.length(), 8)
  
  // 验证各种属性类型
  match log_record.attributes[0].1 {
    StringValue(s) => assert_eq(s, "test string")
    _ => @test.fail("Expected StringValue")
  }
  
  match log_record.attributes[1].1 {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  match log_record.attributes[2].1 {
    FloatValue(f) => assert_eq(f, 3.14159)
    _ => @test.fail("Expected FloatValue")
  }
  
  match log_record.attributes[3].1 {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match log_record.attributes[4].1 {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
}

test "log_record_builder_with_trace_context" {
  // 测试LogRecordBuilder添加追踪上下文
  
  let trace_id = Array::make(16, 0x01_byte)
  let span_id = Array::make(8, 0x02_byte)
  
  let builder = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Debug)
    .body("Debug log with trace context")
  
  // 由于LogRecordBuilder没有直接的trace_id和span_id设置方法，
  // 我们需要验证默认情况下这些字段为None
  let log_record = builder.build()
  
  assert_eq(log_record.trace_id, None)
  assert_eq(log_record.span_id, None)
  assert_eq(log_record.trace_flags, None)
  
  // 验证其他字段设置正确
  match log_record.severity_number {
    Debug => assert_eq(true, true)
    _ => @test.fail("Expected Debug severity")
  }
  
  match log_record.body {
    Some(message) => assert_eq(message, "Debug log with trace context")
    None => @test.fail("Expected body message")
  }
}

test "log_record_builder_all_severity_levels" {
  // 测试LogRecordBuilder设置所有严重性级别
  
  let base_timestamp = 1234567890L
  
  // 测试所有严重性级别
  let trace_record = LogRecord::builder()
    .timestamp(base_timestamp)
    .severity(Trace)
    .body("Trace message")
    .build()
  
  let debug_record = LogRecord::builder()
    .timestamp(base_timestamp + 1)
    .severity(Debug)
    .body("Debug message")
    .build()
  
  let info_record = LogRecord::builder()
    .timestamp(base_timestamp + 2)
    .severity(Info)
    .body("Info message")
    .build()
  
  let warn_record = LogRecord::builder()
    .timestamp(base_timestamp + 3)
    .severity(Warn)
    .body("Warning message")
    .build()
  
  let error_record = LogRecord::builder()
    .timestamp(base_timestamp + 4)
    .severity(Error)
    .body("Error message")
    .build()
  
  let fatal_record = LogRecord::builder()
    .timestamp(base_timestamp + 5)
    .severity(Fatal)
    .body("Fatal message")
    .build()
  
  // 验证所有严重性级别
  match trace_record.severity_number {
    Trace => assert_eq(true, true)
    _ => @test.fail("Expected Trace severity")
  }
  
  match debug_record.severity_number {
    Debug => assert_eq(true, true)
    _ => @test.fail("Expected Debug severity")
  }
  
  match info_record.severity_number {
    Info => assert_eq(true, true)
    _ => @test.fail("Expected Info severity")
  }
  
  match warn_record.severity_number {
    Warn => assert_eq(true, true)
    _ => @test.fail("Expected Warn severity")
  }
  
  match error_record.severity_number {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error severity")
  }
  
  match fatal_record.severity_number {
    Fatal => assert_eq(true, true)
    _ => @test.fail("Expected Fatal severity")
  }
  
  // 验证时间戳递增
  assert_eq(trace_record.timestamp_unix_nanos, base_timestamp)
  assert_eq(debug_record.timestamp_unix_nanos, base_timestamp + 1)
  assert_eq(info_record.timestamp_unix_nanos, base_timestamp + 2)
  assert_eq(warn_record.timestamp_unix_nanos, base_timestamp + 3)
  assert_eq(error_record.timestamp_unix_nanos, base_timestamp + 4)
  assert_eq(fatal_record.timestamp_unix_nanos, base_timestamp + 5)
}

test "log_record_builder_chaining_operations" {
  // 测试LogRecordBuilder的链式操作
  
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Info)
    .body("Chained operations test")
    .with_attribute("attr1", AttributeValue::string("value1"))
    .with_attribute("attr2", AttributeValue::int(42L))
    .with_attribute("attr3", AttributeValue::float(3.14))
    .with_attribute("attr4", AttributeValue::bool(true))
    .with_attribute("attr5", AttributeValue::array_string(["a", "b"]))
    .build()
  
  // 验证所有字段都正确设置
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  
  match log_record.severity_number {
    Info => assert_eq(true, true)
    _ => @test.fail("Expected Info severity")
  }
  
  match log_record.body {
    Some(message) => assert_eq(message, "Chained operations test")
    None => @test.fail("Expected body message")
  }
  
  // 验证所有属性
  assert_eq(log_record.attributes.length(), 5)
  
  let attr_names = []
  let mut i = 0
  while i < log_record.attributes.length() {
    attr_names.push(log_record.attributes[i].0)
    i = i + 1
  }
  
  assert_eq(attr_names.contains("attr1"), true)
  assert_eq(attr_names.contains("attr2"), true)
  assert_eq(attr_names.contains("attr3"), true)
  assert_eq(attr_names.contains("attr4"), true)
  assert_eq(attr_names.contains("attr5"), true)
}

test "log_record_builder_edge_cases" {
  // 测试LogRecordBuilder的边界情况
  
  // 测试空字符串
  let empty_body_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Info)
    .body("")
    .build()
  
  match empty_body_record.body {
    Some(message) => assert_eq(message, "")
    None => @test.fail("Expected empty body message")
  }
  
  // 测试零时间戳
  let zero_timestamp_record = LogRecord::builder()
    .timestamp(0L)
    .severity(Debug)
    .body("Zero timestamp test")
    .build()
  
  assert_eq(zero_timestamp_record.timestamp_unix_nanos, 0L)
  
  // 测试负时间戳（虽然不合理，但测试边界情况）
  let negative_timestamp_record = LogRecord::builder()
    .timestamp(-1L)
    .severity(Warn)
    .body("Negative timestamp test")
    .build()
  
  assert_eq(negative_timestamp_record.timestamp_unix_nanos, -1L)
  
  // 测试极大时间戳
  let max_timestamp_record = LogRecord::builder()
    .timestamp(9223372036854775807L)
    .severity(Error)
    .body("Max timestamp test")
    .build()
  
  assert_eq(max_timestamp_record.timestamp_unix_nanos, 9223372036854775807L)
  
  // 测试空属性键和特殊字符
  let special_attrs_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Info)
    .body("Special attributes test")
    .with_attribute("", AttributeValue::string("empty key"))
    .with_attribute("special.chars!@#$%", AttributeValue::string("special value"))
    .with_attribute("unicode测试🚀", AttributeValue::string("unicode value"))
    .with_attribute("spaces in key", AttributeValue::string("spaces value"))
    .build()
  
  assert_eq(special_attrs_record.attributes.length(), 4)
}

test "log_record_builder_default_values" {
  // 测试LogRecordBuilder的默认值
  
  // 使用最小配置创建LogRecord
  let minimal_record = LogRecord::builder().build()
  
  // 验证默认值
  assert_eq(minimal_record.timestamp_unix_nanos, 0L)
  assert_eq(minimal_record.observed_timestamp_unix_nanos, None)
  
  match minimal_record.severity_number {
    Info => assert_eq(true, true) // 默认严重性级别
    _ => @test.fail("Expected default Info severity")
  }
  
  assert_eq(minimal_record.severity_text, None)
  assert_eq(minimal_record.body, None)
  assert_eq(minimal_record.attributes.length(), 0)
  assert_eq(minimal_record.trace_id, None)
  assert_eq(minimal_record.span_id, None)
  assert_eq(minimal_record.trace_flags, None)
  assert_eq(minimal_record.resource, None)
  assert_eq(minimal_record.instrumentation_scope, None)
}