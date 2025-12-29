// SeverityNumber枚举的测试用例
// 测试日志严重性级别

test "severity_number_all_values" {
  // 测试所有SeverityNumber枚举值
  let trace_log = LogRecord::builder()
    .severity(Trace)
    .body("Trace message")
    .build()
  
  let debug_log = LogRecord::builder()
    .severity(Debug)
    .body("Debug message")
    .build()
  
  let info_log = LogRecord::builder()
    .severity(Info)
    .body("Info message")
    .build()
  
  let warn_log = LogRecord::builder()
    .severity(Warn)
    .body("Warning message")
    .build()
  
  let error_log = LogRecord::builder()
    .severity(Error)
    .body("Error message")
    .build()
  
  let fatal_log = LogRecord::builder()
    .severity(Fatal)
    .body("Fatal message")
    .build()
  
  // 验证每种严重性级别
  match trace_log.severity_number {
    Trace => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match debug_log.severity_number {
    Debug => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match info_log.severity_number {
    Info => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match warn_log.severity_number {
    Warn => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match error_log.severity_number {
    Error => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match fatal_log.severity_number {
    Fatal => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  // 验证body内容
  match trace_log.body {
    Some(body) => assert_eq(body, "Trace message")
    None => @test.fail("Test failed")
  }
  
  match fatal_log.body {
    Some(body) => assert_eq(body, "Fatal message")
    None => @test.fail("Test failed")
  }
}

test "log_record_with_all_severity_and_attributes" {
  // 测试包含所有严重性级别和属性的日志记录
  let base_attributes = [
    ("service.name", AttributeValue::string("test-service")),
    ("service.version", AttributeValue::string("1.0.0"))
  ]
  
  let severity_levels = [Trace, Debug, Info, Warn, Error, Fatal]
  let severity_texts = ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]
  
  let mut i = 0
  while i < severity_levels.length() {
    let severity = severity_levels[i]
    let severity_text = severity_texts[i]
    
    let log_record = LogRecord::builder()
      .timestamp(1234567890L + i.to_int64())
      .severity(severity)
      .severity_text(severity_text)
      .body("Message with severity " + severity_text)
      .with_attribute("severity.index", AttributeValue::int(i.to_int64()))
      .with_attribute("service.name", AttributeValue::string("test-service"))
      .with_attribute("service.version", AttributeValue::string("1.0.0"))
      .build()
    
    // 验证时间戳
    assert_eq(log_record.timestamp_unix_nanos, 1234567890L + i.to_int64())
    
    // 验证严重性文本
    match log_record.severity_text {
      Some(text) => assert_eq(text, severity_text)
      None => @test.fail("Test failed")
    }
    
    // 验证body
    match log_record.body {
      Some(body) => assert_eq(body, "Message with severity " + severity_text)
      None => @test.fail("Test failed")
    }
    
    // 验证属性数量
    assert_eq(log_record.attributes.length(), 3)
    
    i = i + 1
  }
}

test "log_record_with_trace_and_span_context" {
  // 测试包含trace和span上下文的日志记录
  let trace_id = [
    1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte,
    9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte
  ]
  
  let span_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Error)
    .body("Error in operation")
    .with_attribute("error.code", AttributeValue::int(500L))
    .with_attribute("error.message", AttributeValue::string("Internal Server Error"))
    .build()
  
  // 由于LogRecordBuilder没有设置trace_id和span_id的方法，
  // 我们直接创建LogRecord结构体
  let log_with_trace = LogRecord::{
    timestamp_unix_nanos: log_record.timestamp_unix_nanos,
    observed_timestamp_unix_nanos: log_record.observed_timestamp_unix_nanos,
    severity_number: log_record.severity_number,
    severity_text: log_record.severity_text,
    body: log_record.body,
    attributes: log_record.attributes,
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1_byte),
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证trace上下文
  match log_with_trace.trace_id {
    Some(id) => {
      assert_eq(id.length(), 16)
      assert_eq(id[0], 1_byte)
      assert_eq(id[15], 16_byte)
    }
    None => @test.fail("Test failed")
  }
  
  match log_with_trace.span_id {
    Some(id) => {
      assert_eq(id.length(), 8)
      assert_eq(id[0], 1_byte)
      assert_eq(id[7], 8_byte)
    }
    None => @test.fail("Test failed")
  }
  
  match log_with_trace.trace_flags {
    Some(flags) => assert_eq(flags, 1_byte)
    None => @test.fail("Test failed")
  }
}

test "log_record_with_resource_and_instrumentation_scope" {
  // 测试包含资源和仪器范围的日志记录
  let resource = Resource::default("test-service")
  let instrumentation_scope = InstrumentationScope::{
    name: "test-logger",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567895L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Info message with resource and scope"),
    attributes: [("key", AttributeValue::string("value"))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  // 验证时间戳
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  match log_record.observed_timestamp_unix_nanos {
    Some(observed) => assert_eq(observed, 1234567895L)
    None => @test.fail("Test failed")
  }
  
  // 验证资源和仪器范围
  match log_record.resource {
    Some(res) => {
      assert_eq(res.service_name, "test-service")
      assert_eq(res.telemetry_sdk_name, "azimuth")
    }
    None => @test.fail("Test failed")
  }
  
  match log_record.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "test-logger")
      match scope.version {
        Some(version) => assert_eq(version, "1.0.0")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}

test "logger_convenience_methods" {
  // 测试Logger的便捷方法
  let provider = NoopLoggerProvider::{}
  let logger = provider.get_logger("test-logger", "1.0.0")
  
  // 测试各种便捷方法
  logger.debug("Debug message", [("level", AttributeValue::string("debug"))])
  logger.info("Info message", [("level", AttributeValue::string("info"))])
  logger.warn("Warning message", [("level", AttributeValue::string("warn"))])
  logger.error("Error message", [("level", AttributeValue::string("error"))])
  logger.fatal("Fatal message", [("level", AttributeValue::string("fatal"))])
  
  // 测试不带属性的便捷方法
  logger.debug("Debug message without attributes")
  logger.info("Info message without attributes")
  logger.warn("Warning message without attributes")
  logger.error("Error message without attributes")
  logger.fatal("Fatal message without attributes")
  
  assert_eq(true, true) // 如果没有崩溃，测试通过
}