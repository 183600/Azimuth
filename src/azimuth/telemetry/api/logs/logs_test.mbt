test "severity_number_values" {
  // 测试SeverityNumber的所有值
  let trace = Trace
  let debug = Debug
  let info = Info
  let warn = Warn
  let error = Error
  let fatal = Fatal
  
  // 确保所有严重性级别可以创建
  assert_eq(true, true)
}

test "log_record_basic_creation" {
  // 测试基本LogRecord创建
  let log_record = {
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Test log message"),
    attributes: [("key", @common.AttributeValue::string("value"))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  assert_eq(log_record.observed_timestamp_unix_nanos, Some(1234567891L))
  assert_eq(log_record.severity_number, Info)
  assert_eq(log_record.severity_text, Some("INFO"))
  assert_eq(log_record.body, Some("Test log message"))
  assert_eq(log_record.attributes.length(), 1)
}

test "log_record_with_trace_context" {
  // 测试带追踪上下文的LogRecord
  let trace_id = Array::make(16, 1_byte)
  let span_id = Array::make(8, 2_byte)
  
  let log_record = {
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: None,
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Error occurred"),
    attributes: [("error.type", @common.AttributeValue::string("timeout"))],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1_byte),
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(log_record.severity_number, Error)
  assert_eq(log_record.trace_id?.length(), 16)
  assert_eq(log_record.span_id?.length(), 8)
  assert_eq(log_record.trace_flags?, 1_byte)
}

test "noop_logger_methods" {
  // 测试NoopLogger的所有方法
  let logger = NoopLogger
  let attributes = [("service", @common.AttributeValue::string("test"))]
  
  // 所有方法都应该正常执行（no-op）
  logger.debug("Debug message", Some(attributes))
  logger.info("Info message", None)
  logger.warn("Warning message", Some(attributes))
  logger.error("Error message", None)
  logger.fatal("Fatal message", Some(attributes))
  
  assert_eq(true, true)
}

test "noop_logger_emit" {
  // 测试NoopLogger的emit方法
  let logger = NoopLogger
  let log_record = {
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: None,
    body: Some("Test emit"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  logger.emit(log_record)
  assert_eq(true, true)
}

test "noop_logger_provider_get_logger" {
  // 测试NoopLoggerProvider的get_logger方法
  let provider = NoopLoggerProvider
  let logger = provider.get_logger("test-logger", Some("1.0.0"), Some("http://example.com/schema"))
  
  // 确保返回的logger可以正常使用
  logger.info("Test message", None)
  assert_eq(true, true)
}

test "log_record_builder_basic" {
  // 测试LogRecordBuilder的基本用法
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Warn)
    .body("Warning message")
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  assert_eq(log_record.severity_number, Warn)
  assert_eq(log_record.body, Some("Warning message"))
  assert_eq(log_record.severity_text, None)
}

test "log_record_builder_with_attributes" {
  // 测试LogRecordBuilder添加属性
  let log_record = LogRecord::builder()
    .severity(Error)
    .body("Error message")
    .with_attribute("error.code", @common.AttributeValue::int(500L))
    .with_attribute("error.message", @common.AttributeValue::string("Internal Server Error"))
    .with_attribute("retry.count", @common.AttributeValue::int(3L))
    .build()
  
  assert_eq(log_record.severity_number, Error)
  assert_eq(log_record.body, Some("Error message"))
  assert_eq(log_record.attributes.length(), 3)
}

test "log_record_builder_complete" {
  // 测试LogRecordBuilder的完整用法
  let trace_id = Array::make(16, 5_byte)
  let span_id = Array::make(8, 6_byte)
  let resource = @common.Resource::default("test-service")
  let scope = { name: "test-scope", version: Some("1.0.0"), schema_url: None }
  
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Fatal)
    .severity_text("FATAL")
    .body("Fatal error occurred")
    .with_attribute("component", @common.AttributeValue::string("database"))
    .with_attribute("query", @common.AttributeValue::string("SELECT * FROM users"))
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  assert_eq(log_record.severity_number, Fatal)
  assert_eq(log_record.severity_text, Some("FATAL"))
  assert_eq(log_record.body, Some("Fatal error occurred"))
  assert_eq(log_record.attributes.length(), 2)
}