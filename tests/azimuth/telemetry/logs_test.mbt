test "log record creation and properties" {
  // Test log record creation with different severity levels
  let info_record = @azimuth.LogRecord::new(@azimuth.Info, "Information message")
  let error_record = @azimuth.LogRecord::new(@azimuth.Error, "Error message")
  let debug_record = @azimuth.LogRecord::new(@azimuth.Debug, "Debug message")
  
  assert_eq(@azimuth.LogRecord::severity_number(info_record), @azimuth.Info)
  assert_eq(@azimuth.LogRecord::severity_number(error_record), @azimuth.Error)
  assert_eq(@azimuth.LogRecord::severity_number(debug_record), @azimuth.Debug)
  
  assert_eq(@azimuth.LogRecord::body(info_record), Some("Information message"))
  assert_eq(@azimuth.LogRecord::body(error_record), Some("Error message"))
  assert_eq(@azimuth.LogRecord::body(debug_record), Some("Debug message"))
}

test "log record with context" {
  // Test log record creation with full context
  let severity = @azimuth.Warn
  let body = Some("Warning with context")
  let attributes = @azimuth.Attributes::new()
  let timestamp = Some(1735689600000000000L)
  let observed_timestamp = Some(1735689601000000000L)
  let trace_id = Some("trace123")
  let span_id = Some("span456")
  let context = @azimuth.Context::root()
  
  let record = @azimuth.LogRecord::new_with_context(
    severity, body, attributes, timestamp, observed_timestamp, trace_id, span_id, context
  )
  
  assert_eq(@azimuth.LogRecord::severity_number(record), @azimuth.Warn)
  assert_eq(@azimuth.LogRecord::body(record), Some("Warning with context"))
  assert_eq(@azimuth.LogRecord::trace_id(record), Some("trace123"))
  assert_eq(@azimuth.LogRecord::span_id(record), Some("span456"))
}

test "logger provider and logger creation" {
  // Test logger provider and logger creation
  let provider = @azimuth.LoggerProvider::default()
  let logger = @azimuth.LoggerProvider::get_logger(provider, "test.logger", Some("1.0.0"))
  
  let scope = logger.scope
  assert_eq(scope.name, "test.logger")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)
}

test "logger emit functionality" {
  // Test logger emit functionality
  let provider = @azimuth.LoggerProvider::noop()
  let logger = @azimuth.LoggerProvider::get_logger(provider, "emit.test")
  
  let record = @azimuth.LogRecord::new(@azimuth.Info, "Test emit message")
  
  // Emit log record
  @azimuth.Logger::emit(logger, record)
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}

test "different severity levels" {
  // Test all severity levels
  let trace_record = @azimuth.LogRecord::new(@azimuth.Trace, "Trace message")
  let debug_record = @azimuth.LogRecord::new(@azimuth.Debug, "Debug message")
  let info_record = @azimuth.LogRecord::new(@azimuth.Info, "Info message")
  let warn_record = @azimuth.LogRecord::new(@azimuth.Warn, "Warning message")
  let error_record = @azimuth.LogRecord::new(@azimuth.Error, "Error message")
  let fatal_record = @azimuth.LogRecord::new(@azimuth.Fatal, "Fatal message")
  
  assert_eq(@azimuth.LogRecord::severity_number(trace_record), @azimuth.Trace)
  assert_eq(@azimuth.LogRecord::severity_number(debug_record), @azimuth.Debug)
  assert_eq(@azimuth.LogRecord::severity_number(info_record), @azimuth.Info)
  assert_eq(@azimuth.LogRecord::severity_number(warn_record), @azimuth.Warn)
  assert_eq(@azimuth.LogRecord::severity_number(error_record), @azimuth.Error)
  assert_eq(@azimuth.LogRecord::severity_number(fatal_record), @azimuth.Fatal)
}

test "log record with empty body" {
  // Test log record with empty body
  let record = @azimuth.LogRecord::new(@azimuth.Info, "")
  
  assert_eq(@azimuth.LogRecord::severity_number(record), @azimuth.Info)
  assert_eq(@azimuth.LogRecord::body(record), Some(""))
}

test "log record with special characters" {
  // Test log record with special characters and unicode
  let special_chars_record = @azimuth.LogRecord::new(@azimuth.Info, "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?")
  let unicode_record = @azimuth.LogRecord::new(@azimuth.Info, "Unicode: 中文测试 🚀 emoji")
  
  assert_eq(@azimuth.LogRecord::body(special_chars_record), Some("Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"))
  assert_eq(@azimuth.LogRecord::body(unicode_record), Some("Unicode: 中文测试 🚀 emoji"))
}