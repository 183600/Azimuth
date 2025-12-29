// Logs API基础功能测试
test "SeverityNumber enum values" {
  let trace_severity = Trace
  let debug_severity = Debug
  let info_severity = Info
  let warn_severity = Warn
  let error_severity = Error
  let fatal_severity = Fatal
  
  assert match (trace_severity) {
    Trace => true
    _ => false
  }
  
  assert match (debug_severity) {
    Debug => true
    _ => false
  }
  
  assert match (info_severity) {
    Info => true
    _ => false
  }
  
  assert match (warn_severity) {
    Warn => true
    _ => false
  }
  
  assert match (error_severity) {
    Error => true
    _ => false
  }
  
  assert match (fatal_severity) {
    Fatal => true
    _ => false
  }
}

test "LogRecord creation with all fields" {
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte })
  let resource = Resource::default("test-service")
  let scope = InstrumentationScope::{
    name: "test-logger",
    version: None,
    schema_url: None
  }
  
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Something went wrong"),
    attributes: [
      ("error.code", AttributeValue::int(500L)),
      ("error.message", AttributeValue::string("Internal server error"))
    ],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1_byte),
    resource: Some(resource),
    instrumentation_scope: Some(scope)
  }
  
  assert log_record.timestamp_unix_nanos == 1234567890L
  assert match (log_record.observed_timestamp_unix_nanos) {
    Some(time) => time == 1234567891L
    None => false
  }
  assert match (log_record.severity_number) {
    Error => true
    _ => false
  }
  assert match (log_record.severity_text) {
    Some(text) => text == "ERROR"
    None => false
  }
  assert match (log_record.body) {
    Some(body) => body == "Something went wrong"
    None => false
  }
  assert log_record.attributes.length == 2
  assert log_record.trace_id?.length == 16
  assert log_record.span_id?.length == 8
  assert log_record.trace_flags? == 1_byte
}

test "LogRecord minimal creation" {
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: None,
    body: None,
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert log_record.timestamp_unix_nanos == 1234567890L
  assert log_record.observed_timestamp_unix_nanos == None
  assert match (log_record.severity_number) {
    Info => true
    _ => false
  }
  assert log_record.severity_text == None
  assert log_record.body == None
  assert log_record.attributes.length == 0
  assert log_record.trace_id == None
  assert log_record.span_id == None
  assert log_record.trace_flags == None
}

test "NoopLogger emit functionality" {
  let logger = NoopLogger::{}
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: None,
    body: Some("Test log message"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 测试emit方法不会抛出异常
  logger.emit(log_record)
  assert true
}

test "NoopLogger convenience methods" {
  let logger = NoopLogger::{}
  
  // 测试所有便捷方法都不会抛出异常
  logger.debug("Debug message", attributes: [("level", AttributeValue::string("debug"))])
  logger.info("Info message")
  logger.warn("Warning message", attributes: [("type", AttributeValue::string("deprecated"))])
  logger.error("Error message")
  logger.fatal("Fatal message", attributes: [("critical", AttributeValue::bool(true))])
  
  assert true
}

test "NoopLoggerProvider functionality" {
  let provider = NoopLoggerProvider::{}
  
  let logger1 = provider.get_logger("test-logger")
  let logger2 = provider.get_logger("another-logger", version: Some("1.0.0"))
  let logger3 = provider.get_logger("schema-logger", schema_url: Some("https://example.com/schema"))
  
  // 测试所有logger都可以创建并使用
  logger1.info("Test from logger1")
  logger2.warn("Test from logger2")
  logger3.error("Test from logger3")
  
  assert true
}

test "LogRecordBuilder basic usage" {
  let builder = LogRecord::builder()
  let log_record = builder
    .timestamp(1234567890L)
    .severity(Error)
    .body("Test error message")
    .with_attribute("error.code", AttributeValue::int(404L))
    .with_attribute("error.type", AttributeValue::string("NotFound"))
    .build()
  
  assert log_record.timestamp_unix_nanos == 1234567890L
  assert match (log_record.severity_number) {
    Error => true
    _ => false
  }
  assert match (log_record.body) {
    Some(body) => body == "Test error message"
    None => false
  }
  assert log_record.attributes.length == 2
}

test "LogRecordBuilder minimal usage" {
  let builder = LogRecord::builder()
  let log_record = builder.build()
  
  // 验证默认值
  assert log_record.timestamp_unix_nanos == 0L
  assert match (log_record.severity_number) {
    Info => true
    _ => false
  }
  assert log_record.body == None
  assert log_record.attributes.length == 0
}

test "LogRecordBuilder chain operations" {
  let log_record = LogRecord::builder()
    .timestamp(9876543210L)
    .severity(Warn)
    .body("Warning message")
    .with_attribute("component", AttributeValue::string("auth"))
    .with_attribute("user.id", AttributeValue::int(12345L))
    .with_attribute("retry.count", AttributeValue::int(3L))
    .build()
  
  assert log_record.timestamp_unix_nanos == 9876543210L
  assert match (log_record.severity_number) {
    Warn => true
    _ => false
  }
  assert match (log_record.body) {
    Some(body) => body == "Warning message"
    None => false
  }
  assert log_record.attributes.length == 3
}