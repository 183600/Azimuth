test "LogRecord builder and logging test" {
  // 测试LogRecordBuilder基本功能
  let builder = LogRecord::builder()
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
  
  // 测试链式构建
  let log_record = builder
    .timestamp(1234567890L)
    .severity(Info)
    .body("Test log message")
    .with_attribute("key1", AttributeValue::string("value1"))
    .with_attribute("key2", AttributeValue::int(42L))
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  assert_eq(log_record.observed_timestamp_unix_nanos, None)
  assert_eq(log_record.severity_number, logs::Info)
  assert_eq(log_record.severity_text, None)
  match log_record.body {
    Some(body) => assert_eq(body, "Test log message")
    None => @test.fail("Expected body to be Some")
  }
  assert_eq(log_record.attributes.length(), 2)
  assert_eq(log_record.trace_id, None)
  assert_eq(log_record.span_id, None)
  assert_eq(log_record.trace_flags, None)
  assert_eq(log_record.resource, None)
  assert_eq(log_record.instrumentation_scope, None)
  
  // 验证属性
  let attr1 = log_record.attributes[0]
  assert_eq(attr1.0, "key1")
  match attr1.1 {
    StringValue(s) => assert_eq(s, "value1")
    _ => @test.fail("Expected StringValue for key1")
  }
  
  let attr2 = log_record.attributes[1]
  assert_eq(attr2.0, "key2")
  match attr2.1 {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Expected IntValue for key2")
  }
  
  // 测试所有严重级别
  let trace_log = LogRecord::builder()
    .severity(Trace)
    .body("Trace message")
    .build()
  assert_eq(trace_log.severity_number, Trace)
  
  let debug_log = LogRecord::builder()
    .severity(Debug)
    .body("Debug message")
    .build()
  assert_eq(debug_log.severity_number, Debug)
  
  let warn_log = LogRecord::builder()
    .severity(Warn)
    .body("Warning message")
    .build()
  assert_eq(warn_log.severity_number, Warn)
  
  let error_log = LogRecord::builder()
    .severity(Error)
    .body("Error message")
    .build()
  assert_eq(error_log.severity_number, Error)
  
  let fatal_log = LogRecord::builder()
    .severity(Fatal)
    .body("Fatal message")
    .build()
  assert_eq(fatal_log.severity_number, Fatal)
  
  // 测试带trace信息的LogRecord
  let trace_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 
                  9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]
  let span_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  let trace_flags = 1_byte
  
  let trace_log_record = LogRecord::builder()
    .timestamp(9876543210L)
    .severity(Info)
    .body("Log with trace")
    .with_attribute("trace_id", AttributeValue::array_string(trace_id.map(fn(b) { b.to_string() })))
    .build()
  
  // 手动设置trace相关字段（因为builder没有这些方法）
  let mut log_with_trace = trace_log_record
  log_with_trace.trace_id = trace_id
  log_with_trace.span_id = span_id
  log_with_trace.trace_flags = trace_flags
  
  assert_eq(log_with_trace.trace_id, trace_id)
  assert_eq(log_with_trace.span_id, span_id)
  assert_eq(log_with_trace.trace_flags, trace_flags)
  
  // 测试带Resource和InstrumentationScope的LogRecord
  let resource = Resource::default("test-service")
  let scope = InstrumentationScope{
    name: "test-logger",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  let full_log_record = LogRecord::builder()
    .timestamp(1111111111L)
    .severity(Info)
    .body("Full log record")
    .with_attribute("service", AttributeValue::string("test"))
    .build()
  
  let mut full_log = full_log_record
  full_log.resource = resource
  full_log.instrumentation_scope = scope
  
  match full_log.resource {
    Some(r) => {
      assert_eq(r.service_name, "test-service")
      assert_eq(r.telemetry_sdk_name, "azimuth")
    }
    None => @test.fail("Expected resource to be Some")
  }
  
  match full_log.instrumentation_scope {
    Some(s) => {
      assert_eq(s.name, "test-logger")
      match s.version {
        Some(v) => assert_eq(v, "1.0.0")
        None => @test.fail("Expected version to be Some")
      }
    }
    None => @test.fail("Expected instrumentation_scope to be Some")
  }
  
  // 测试NoopLogger
  let noop_logger = NoopLogger::{}
  let test_log = LogRecord::builder()
    .severity(Info)
    .body("Test noop")
    .build()
  
  // NoopLogger的emit方法应该不产生任何效果，但不应该崩溃
  noop_logger.emit(test_log)
  noop_logger.debug("Debug message", [])
  noop_logger.info("Info message", [])
  noop_logger.warn("Warning message", [])
  noop_logger.error("Error message", [])
  noop_logger.fatal("Fatal message", [])
  
  // 测试NoopLoggerProvider
  let noop_provider = NoopLoggerProvider::{}
  let logger = noop_provider.get_logger("test-logger", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 验证返回的是NoopLogger类型
  // 由于类型擦除，我们只能验证方法调用不会崩溃
  logger.debug("Test message", [("key", AttributeValue::string("value"))])
  
  // 测试边界条件
  let empty_log = LogRecord::builder().build()
  assert_eq(empty_log.timestamp_unix_nanos, 0L)  // 默认值
  assert_eq(empty_log.severity_number, Info)  // 默认值
  assert_eq(empty_log.body, None)
  assert_eq(empty_log.attributes.length(), 0)
  
  // 测试长消息
  let long_message = "This is a very long log message that tests the boundary conditions of the logging system to ensure it can handle messages of significant length without any issues or performance degradation"
  let long_log = LogRecord::builder()
    .severity(Info)
    .body(long_message)
    .build()
  
  match long_log.body {
    Some(body) => assert_eq(body, long_message)
    None => @test.fail("Expected body to be Some for long message")
  }
  
  // 测试大量属性
  let mut builder_with_many_attrs = LogRecord::builder()
    .severity(Info)
    .body("Log with many attributes")
  
  // 添加多个属性
  for i = 0; i < 10; i = i + 1 {
    builder_with_many_attrs = builder_with_many_attrs
      .with_attribute("attr_" + i.to_string(), AttributeValue::int(i.to_int64()))
  }
  
  let log_with_many_attrs = builder_with_many_attrs.build()
  assert_eq(log_with_many_attrs.attributes.length(), 10)
  
  // 验证属性
  for i = 0; i < 10; i = i + 1 {
    let attr = log_with_many_attrs.attributes[i]
    assert_eq(attr.0, "attr_" + i.to_string())
    match attr.1 {
      IntValue(val) => assert_eq(val, i.to_int64())
      _ => @test.fail("Expected IntValue for attr_" + i.to_string())
    }
  }
}