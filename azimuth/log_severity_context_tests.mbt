// 日志记录严重性和上下文测试用例
test "LogRecord严重性级别测试" {
  let trace_record = LogRecord::new(Trace, "Trace level message")
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  
  let debug_record = LogRecord::new(Debug, "Debug level message")
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  
  let info_record = LogRecord::new(Info, "Info level message")
  assert_eq(LogRecord::severity_number(info_record), Info)
  
  let warn_record = LogRecord::new(Warn, "Warning level message")
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  
  let error_record = LogRecord::new(Error, "Error level message")
  assert_eq(LogRecord::severity_number(error_record), Error)
  
  let fatal_record = LogRecord::new(Fatal, "Fatal level message")
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
}

test "LogRecord内容获取测试" {
  let record = LogRecord::new(Info, "Test log message")
  
  let body = LogRecord::body(record)
  assert_true(body is Some)
  match body {
    Some(message) => assert_eq(message, "Test log message")
    _ => assert_true(false)
  }
  
  let empty_record = LogRecord::new(Error, "")
  let empty_body = LogRecord::body(empty_record)
  match empty_body {
    Some(message) => assert_eq(message, "")
    _ => assert_true(false)
  }
}

test "LogRecord带上下文创建测试" {
  let trace_id = Some("trace-123")
  let span_id = Some("span-456")
  let timestamp = Some(1735689600000000000L)
  let context = Context::with_value(Context::root(), ContextKey::new("user.id"), "user-789")
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Contextual message"),
    Some(Attributes::new()),
    timestamp,
    None,
    trace_id,
    span_id,
    Some(context)
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::trace_id(record), trace_id)
  assert_eq(LogRecord::span_id(record), span_id)
}

test "Logger提供程序和Logger测试" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test.logger")
  
  let record = LogRecord::new(Info, "Test emission")
  Logger::emit(logger, record)
  
  // 测试通过表示没有崩溃
  assert_true(true)
}

test "LogRecord时间戳处理测试" {
  let fixed_timestamp = 1735689600000000000L
  let record = LogRecord::new_with_context(
    Info,
    Some("Timestamp test"),
    None,
    Some(fixed_timestamp),
    Some(fixed_timestamp + 1000L),
    None,
    None,
    None
  )
  
  // 验证时间戳设置正确
  assert_eq(record.timestamp, Some(fixed_timestamp))
  assert_eq(record.observed_timestamp, Some(fixed_timestamp + 1000L))
}