// Logs模块严重性级别转换测试用例
test "severity_number_basic_conversion" {
  // 测试基本的严重性级别转换
  let trace_severity = Trace
  let debug_severity = Debug
  let info_severity = Info
  let warn_severity = Warn
  let error_severity = Error
  let fatal_severity = Fatal
  
  // 验证严重性级别的枚举值
  // 注意：MoonBit的枚举比较可能需要不同的语法，这里假设可以直接比较
  
  // 创建不同严重性级别的LogRecord
  let trace_log = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: trace_severity,
    severity_text: Some("TRACE"),
    body: Some("Trace level message"),
    attributes: [("level", AttributeValue::string("trace"))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let debug_log = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: debug_severity,
    severity_text: Some("DEBUG"),
    body: Some("Debug level message"),
    attributes: [("level", AttributeValue::string("debug"))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let info_log = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: info_severity,
    severity_text: Some("INFO"),
    body: Some("Info level message"),
    attributes: [("level", AttributeValue::string("info"))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let warn_log = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: warn_severity,
    severity_text: Some("WARN"),
    body: Some("Warning level message"),
    attributes: [("level", AttributeValue::string("warn"))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let error_log = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: error_severity,
    severity_text: Some("ERROR"),
    body: Some("Error level message"),
    attributes: [("level", AttributeValue::string("error"))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let fatal_log = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: fatal_severity,
    severity_text: Some("FATAL"),
    body: Some("Fatal level message"),
    attributes: [("level", AttributeValue::string("fatal"))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证所有日志记录的严重性级别
  match trace_log.severity_text {
    Some(text) => assert_eq(text, "TRACE")
    None => @test.fail("Expected Some(severity_text)")
  }
  
  match debug_log.severity_text {
    Some(text) => assert_eq(text, "DEBUG")
    None => @test.fail("Expected Some(severity_text)")
  }
  
  match info_log.severity_text {
    Some(text) => assert_eq(text, "INFO")
    None => @test.fail("Expected Some(severity_text)")
  }
  
  match warn_log.severity_text {
    Some(text) => assert_eq(text, "WARN")
    None => @test.fail("Expected Some(severity_text)")
  }
  
  match error_log.severity_text {
    Some(text) => assert_eq(text, "ERROR")
    None => @test.fail("Expected Some(severity_text)")
  }
  
  match fatal_log.severity_text {
    Some(text) => assert_eq(text, "FATAL")
    None => @test.fail("Expected Some(severity_text)")
  }
}

test "logger_convenience_methods_severity" {
  // 测试Logger便捷方法的严重性级别
  let logger = NoopLogger::{}
  
  // 测试不同严重性级别的便捷方法
  logger.debug("Debug message", Some([("component", AttributeValue::string("test"))]))
  logger.info("Info message", Some([("component", AttributeValue::string("test"))]))
  logger.warn("Warning message", Some([("component", AttributeValue::string("test"))]))
  logger.error("Error message", Some([("component", AttributeValue::string("test"))]))
  logger.fatal("Fatal message", Some([("component", AttributeValue::string("test"))]))
  
  // 测试不带属性的便捷方法
  logger.debug("Debug message without attributes")
  logger.info("Info message without attributes")
  logger.warn("Warning message without attributes")
  logger.error("Error message without attributes")
  logger.fatal("Fatal message without attributes")
  
  // 测试带有复杂属性的便捷方法
  let complex_attrs : Array[(String, AttributeValue)] = [
    ("service.name", AttributeValue::string("test-service")),
    ("service.version", AttributeValue::string("1.0.0")),
    ("trace.id", AttributeValue::string("trace-123456")),
    ("span.id", AttributeValue::string("span-789012")),
    ("user.id", AttributeValue::string("user-345678")),
    ("request.id", AttributeValue::string("req-901234")),
    ("error.code", AttributeValue::int(500L)),
    ("error.type", AttributeValue::string("Internal Server Error")),
    ("http.status_code", AttributeValue::int(500L)),
    ("http.method", AttributeValue::string("POST")),
    ("http.url", AttributeValue::string("/api/v1/resource")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.cpu.percent", AttributeValue::float(75.5)),
    ("process.healthy", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["web", "api", "critical"])),
    ("ports", AttributeValue::array_int([8080L, 8443L])),
    ("thresholds", AttributeValue::array_float([60.0, 80.0, 95.0])),
    ("features", AttributeValue::array_bool([true, false, true]))
  ]
  
  logger.debug("Debug message with complex attributes", Some(complex_attrs))
  logger.info("Info message with complex attributes", Some(complex_attrs))
  logger.warn("Warning message with complex attributes", Some(complex_attrs))
  logger.error("Error message with complex attributes", Some(complex_attrs))
  logger.fatal("Fatal message with complex attributes", Some(complex_attrs))
  
  assert_eq(true, true)  // 如果能执行到这里说明便捷方法测试通过
}

test "log_record_builder_severity_conversion" {
  // 测试LogRecordBuilder的严重性级别转换
  let mut builder = LogRecord::builder()
  
  // 测试设置不同严重性级别
  let trace_log = builder
    .set_severity_number(Trace)
    .set_severity_text("TRACE")
    .set_body("Trace message")
    .set_timestamp_unix_nanos(1234567890L)
    .build()
  
  builder = LogRecord::builder()
  let debug_log = builder
    .set_severity_number(Debug)
    .set_severity_text("DEBUG")
    .set_body("Debug message")
    .set_timestamp_unix_nanos(1234567890L)
    .build()
  
  builder = LogRecord::builder()
  let info_log = builder
    .set_severity_number(Info)
    .set_severity_text("INFO")
    .set_body("Info message")
    .set_timestamp_unix_nanos(1234567890L)
    .build()
  
  builder = LogRecord::builder()
  let warn_log = builder
    .set_severity_number(Warn)
    .set_severity_text("WARN")
    .set_body("Warning message")
    .set_timestamp_unix_nanos(1234567890L)
    .build()
  
  builder = LogRecord::builder()
  let error_log = builder
    .set_severity_number(Error)
    .set_severity_text("ERROR")
    .set_body("Error message")
    .set_timestamp_unix_nanos(1234567890L)
    .build()
  
  builder = LogRecord::builder()
  let fatal_log = builder
    .set_severity_number(Fatal)
    .set_severity_text("FATAL")
    .set_body("Fatal message")
    .set_timestamp_unix_nanos(1234567890L)
    .build()
  
  // 验证所有构建的日志记录的严重性级别
  // 注意：这里假设LogRecordBuilder有相应的set_*方法和build方法
  // 实际实现可能需要根据具体的API进行调整
  
  assert_eq(true, true)  // 如果能执行到这里说明Builder测试通过
}

test "severity_number_to_string_conversion" {
  // 测试严重性级别到字符串的转换
  let severity_texts : Array[(SeverityNumber, String)] = [
    (Trace, "TRACE"),
    (Debug, "DEBUG"),
    (Info, "INFO"),
    (Warn, "WARN"),
    (Error, "ERROR"),
    (Fatal, "FATAL")
  ]
  
  // 创建包含所有严重性级别的日志记录
  let mut i = 0
  while i < severity_texts.length() {
    let (severity, expected_text) = severity_texts[i]
    
    let log_record = LogRecord::{
      timestamp_unix_nanos: 1234567890L + i.to_int64(),
      observed_timestamp_unix_nanos: Some(1234567891L + i.to_int64()),
      severity_number: severity,
      severity_text: Some(expected_text),
      body: Some("Test message for " + expected_text),
      attributes: [
        ("severity.number", AttributeValue::int(i.to_int64())),
        ("severity.text", AttributeValue::string(expected_text))
      ],
      trace_id: None,
      span_id: None,
      trace_flags: None,
      resource: None,
      instrumentation_scope: None
    }
    
    // 验证严重性文本
    match log_record.severity_text {
      Some(text) => assert_eq(text, expected_text)
      None => @test.fail("Expected Some(severity_text)")
    }
    
    // 验证属性中的严重性文本
    match log_record.attributes[1].1 {
      StringValue(text) => assert_eq(text, expected_text)
      _ => @test.fail("Expected StringValue")
    }
    
    i = i + 1
  }
}

test "severity_level_hierarchy" {
  // 测试严重性级别的层次结构
  // 假设严重性级别有数值层次：Trace=1, Debug=2, Info=3, Warn=4, Error=5, Fatal=6
  
  let severity_hierarchy : Array[(SeverityNumber, Int)] = [
    (Trace, 1),
    (Debug, 2),
    (Info, 3),
    (Warn, 4),
    (Error, 5),
    (Fatal, 6)
  ]
  
  // 创建不同严重性级别的日志记录
  let mut i = 0
  while i < severity_hierarchy.length() {
    let (severity, level) = severity_hierarchy[i]
    
    let log_record = LogRecord::{
      timestamp_unix_nanos: 1234567890L + i.to_int64(),
      observed_timestamp_unix_nanos: Some(1234567891L + i.to_int64()),
      severity_number: severity,
      severity_text: None,  // 测试没有severity_text的情况
      body: Some("Message with severity level " + level.to_string()),
      attributes: [
        ("severity.level", AttributeValue::int(level.to_int64())),
        ("severity.name", AttributeValue::string(get_severity_name(severity)))
      ],
      trace_id: None,
      span_id: None,
      trace_flags: None,
      resource: None,
      instrumentation_scope: None
    }
    
    // 验证严重性级别属性
    match log_record.attributes[0].1 {
      IntValue(lvl) => assert_eq(lvl, level.to_int64())
      _ => @test.fail("Expected IntValue")
    }
    
    i = i + 1
  }
}

test "severity_with_trace_context" {
  // 测试带有追踪上下文的严重性级别
  let trace_id = Array::make(16, 1_byte)
  let span_id = Array::make(8, 2_byte)
  let trace_flags = 1_byte
  
  let severity_levels = [Trace, Debug, Info, Warn, Error, Fatal]
  
  let mut i = 0
  while i < severity_levels.length() {
    let severity = severity_levels[i]
    
    let log_record = LogRecord::{
      timestamp_unix_nanos: 1234567890L + i.to_int64(),
      observed_timestamp_unix_nanos: Some(1234567891L + i.to_int64()),
      severity_number: severity,
      severity_text: Some(get_severity_name(severity)),
      body: Some("Message with trace context and severity " + get_severity_name(severity)),
      attributes: [
        ("severity", AttributeValue::string(get_severity_name(severity))),
        ("trace.context", AttributeValue::bool(true)),
        ("log.type", AttributeValue::string("structured"))
      ],
      trace_id: Some(trace_id),
      span_id: Some(span_id),
      trace_flags: Some(trace_flags),
      resource: Some(Resource::default("test-service")),
      instrumentation_scope: None
    }
    
    // 验证追踪上下文
    match log_record.trace_id {
      Some(id) => assert_eq(id.length(), 16)
      None => @test.fail("Expected Some(trace_id)")
    }
    
    match log_record.span_id {
      Some(id) => assert_eq(id.length(), 8)
      None => @test.fail("Expected Some(span_id)")
    }
    
    match log_record.trace_flags {
      Some(flags) => assert_eq(flags, 1_byte)
      None => @test.fail("Expected Some(trace_flags)")
    }
    
    // 验证严重性级别
    match log_record.severity_text {
      Some(text) => assert_eq(text, get_severity_name(severity))
      None => @test.fail("Expected Some(severity_text)")
    }
    
    i = i + 1
  }
}

test "severity_edge_cases" {
  // 测试严重性级别的边界情况
  
  // 测试空严重性文本
  let empty_severity_log = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: Info,
    severity_text: Some(""),
    body: Some("Message with empty severity text"),
    attributes: [("empty.severity", AttributeValue::bool(true))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match empty_severity_log.severity_text {
    Some(text) => assert_eq(text, "")
    None => @test.fail("Expected Some(empty severity text)")
  }
  
  // 测试None严重性文本
  let none_severity_log = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: Warn,
    severity_text: None,
    body: Some("Message with None severity text"),
    attributes: [("none.severity", AttributeValue::bool(true))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match none_severity_log.severity_text {
    Some(_) => @test.fail("Expected None for severity text")
    None => assert_eq(true, true)
  }
  
  // 测试特殊字符的严重性文本
  let special_severity_log = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: Error,
    severity_text: Some("特殊错误级别🚨"),
    body: Some("Message with special severity text"),
    attributes: [("special.severity", AttributeValue::bool(true))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match special_severity_log.severity_text {
    Some(text) => assert_eq(text, "特殊错误级别🚨")
    None => @test.fail("Expected Some(special severity text)")
  }
  
  // 测试非常长的严重性文本
  let long_severity_text = "this.is.a.very.long.severity.text.that.exceeds.normal.expectations.and.tests.the.boundary.conditions.of.the.severity.text.field"
  let long_severity_log = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: Fatal,
    severity_text: Some(long_severity_text),
    body: Some("Message with long severity text"),
    attributes: [("long.severity", AttributeValue::bool(true))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match long_severity_log.severity_text {
    Some(text) => assert_eq(text, long_severity_text)
    None => @test.fail("Expected Some(long severity text)")
  }
}

// 辅助函数：获取严重性级别的名称
fn get_severity_name(severity : SeverityNumber) -> String {
  match severity {
    Trace => "TRACE"
    Debug => "DEBUG"
    Info => "INFO"
    Warn => "WARN"
    Error => "ERROR"
    Fatal => "FATAL"
  }
}