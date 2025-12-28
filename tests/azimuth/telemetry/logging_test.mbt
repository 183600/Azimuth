test "logger provider and logger creation" {
  // Test logger provider and logger creation
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test.logger", Some("1.0.0"))
  
  let scope = logger.scope
  assert_eq(scope.name, "test.logger")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)
}

test "log record creation and properties" {
  // Test log record creation with basic properties
  let record = LogRecord::new(Info, "Test log message")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  assert_eq(LogRecord::trace_id(record), None)
  assert_eq(LogRecord::span_id(record), None)
}

test "log record with full context" {
  // Test log record creation with full context
  let attributes = Attributes::new()
  let context = Context::root()
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000000001L
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Error message"),
    Some(attributes),
    Some(timestamp),
    Some(observed_timestamp),
    Some("trace123"),
    Some("span456"),
    Some(context)
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Error message"))
  assert_eq(LogRecord::trace_id(record), Some("trace123"))
  assert_eq(LogRecord::span_id(record), Some("span456"))
}

test "log severity levels" {
  // Test all log severity levels
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

test "logger emit functionality" {
  // Test logging through logger
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "emit.test")
  
  // Create different log records
  let info_record = LogRecord::new(Info, "Information message")
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Error occurred"),
    None,
    Some(1735689600000000000L),
    None,
    Some("trace789"),
    Some("span101"),
    None
  )
  
  // Emit log records
  Logger::emit(logger, info_record)
  Logger::emit(logger, error_record)
  
  // Test passes if no exceptions are thrown during emission
  assert_true(true)
}

test "log record with optional body" {
  // Test log record without body
  let record_no_body = LogRecord::new_with_context(
    Warn,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record_no_body), Warn)
  assert_eq(LogRecord::body(record_no_body), None)
}

test "logger with noop provider" {
  // Test logger with noop provider
  let noop_provider = LoggerProvider::noop()
  let logger = LoggerProvider::get_logger(noop_provider, "noop.logger")
  
  let record = LogRecord::new(Info, "Noop log message")
  
  // Should not throw exceptions even with noop provider
  Logger::emit(logger, record)
  
  assert_eq(logger.scope.name, "noop.logger")
}