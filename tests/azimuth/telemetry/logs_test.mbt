test "log record creation and properties" {
  // Test log record creation with different severity levels
  let info_record = LogRecord::new(Info, "Information message")
  let error_record = LogRecord::new(Error, "Error message")
  let debug_record = LogRecord::new(Debug, "Debug message")
  
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  
  assert_eq(LogRecord::body(info_record), Some("Information message"))
  assert_eq(LogRecord::body(error_record), Some("Error message"))
  assert_eq(LogRecord::body(debug_record), Some("Debug message"))
}

test "log record with context" {
  // Test log record creation with full context
  let severity = Warn
  let body = Some("Warning with context")
  let attributes = Attributes::new()
  let timestamp = Some(1735689600000000000L)
  let observed_timestamp = Some(1735689601000000000L)
  let trace_id = Some("trace123")
  let span_id = Some("span456")
  let context = Context::root()
  
  let record = LogRecord::new_with_context(
    severity, body, attributes, timestamp, observed_timestamp, trace_id, span_id, context
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::body(record), Some("Warning with context"))
  assert_eq(LogRecord::trace_id(record), Some("trace123"))
  assert_eq(LogRecord::span_id(record), Some("span456"))
}

test "logger provider and logger creation" {
  // Test logger provider and logger creation
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test.logger", Some("1.0.0"))
  
  let scope = logger.scope
  assert_eq(scope.name, "test.logger")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)
}

test "logger emit functionality" {
  // Test logger emit functionality
  let provider = LoggerProvider::noop()
  let logger = LoggerProvider::get_logger(provider, "emit.test")
  
  let record = LogRecord::new(Info, "Test emit message")
  
  // Emit log record
  Logger::emit(logger, record)
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}

test "different severity levels" {
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
  // Test log record with empty body
  let record = LogRecord::new(Info, "")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some(""))
}

test "log record with special characters" {
  // Test log record with special characters and unicode
  let special_chars_record = LogRecord::new(Info, "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?")
  let unicode_record = LogRecord::new(Info, "Unicode: 中文测试 🚀 emoji")
  
  assert_eq(LogRecord::body(special_chars_record), Some("Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"))
  assert_eq(LogRecord::body(unicode_record), Some("Unicode: 中文测试 🚀 emoji"))
}