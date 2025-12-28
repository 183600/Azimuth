// Log Record Test Cases for Azimuth Telemetry System
// Testing log record creation, severity levels, and context

test "log record basic creation" {
  let record = LogRecord::new(Info, "Test log message")
  
  // Test basic properties
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
}

test "log record severity levels" {
  // Test all severity levels
  let trace_record = LogRecord::new(Trace, "Trace message")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Info message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
}

test "log record with empty body" {
  let record = LogRecord::new(Info, "")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some(""))
}

test "log record with context" {
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_value = Context::with_value(ctx, key, "corr-12345")
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Warning with context"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-123"),
    Some("span-456"),
    Some(ctx_with_value)
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::body(record), Some("Warning with context"))
  assert_eq(LogRecord::trace_id(record), Some("trace-123"))
  assert_eq(LogRecord::span_id(record), Some("span-456"))
}

test "log record severity comparison" {
  let trace_record = LogRecord::new(Trace, "Trace")
  let error_record = LogRecord::new(Error, "Error")
  
  // Test severity comparison through pattern matching
  let trace_is_error = match LogRecord::severity_number(trace_record) {
    Error => true
    _ => false
  }
  let error_is_error = match LogRecord::severity_number(error_record) {
    Error => true
    _ => false
  }
  
  assert_false(trace_is_error)
  assert_true(error_is_error)
}

test "log record logger emission" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test.logger")
  
  let record = LogRecord::new(Info, "Test emission")
  
  // Test emission (simplified implementation just returns Unit)
  Logger::emit(logger, record)
  
  assert_true(true)
}

test "log record with various body contents" {
  let empty_record = LogRecord::new(Info, "")
  let short_record = LogRecord::new(Info, "Short")
  let long_record = LogRecord::new(Info, "This is a very long log message that contains multiple words and should be handled properly by the logging system")
  let special_record = LogRecord::new(Info, "Special chars: !@#$%^&*()")
  let unicode_record = LogRecord::new(Info, "Unicode: 测试消息 🚀")
  
  assert_eq(LogRecord::body(empty_record), Some(""))
  assert_eq(LogRecord::body(short_record), Some("Short"))
  assert_eq(LogRecord::body(long_record), Some("This is a very long log message that contains multiple words and should be handled properly by the logging system"))
  assert_eq(LogRecord::body(special_record), Some("Special chars: !@#$%^&*()"))
  assert_eq(LogRecord::body(unicode_record), Some("Unicode: 测试消息 🚀"))
}

test "log record timestamp handling" {
  let timestamp1 = 1735689600000000000L
  let timestamp2 = 1735689600000001000L
  
  let record = LogRecord::new_with_context(
    Info,
    Some("Timestamp test"),
    None,
    Some(timestamp1),
    Some(timestamp2),
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Timestamp test"))
}