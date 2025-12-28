test "logger provider and logger creation" {
  // Test logger provider and logger creation
  let provider = @azimuth.LoggerProvider::default()
  let logger = @azimuth.LoggerProvider::get_logger(provider, "test.logger", Some("1.0.0"))
  
  let scope = logger.scope
  assert_eq(scope.name, "test.logger")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)
}

test "log record creation and properties" {
  // Test log record creation with basic properties
  let record = @azimuth.LogRecord::new(@azimuth.Info, "Test log message")
  
  assert_eq(@azimuth.LogRecord::severity_number(record), @azimuth.Info)
  assert_eq(@azimuth.LogRecord::body(record), Some("Test log message"))
  assert_eq(@azimuth.LogRecord::trace_id(record), None)
  assert_eq(@azimuth.LogRecord::span_id(record), None)
}

test "log record with full context" {
  // Test log record creation with full context
  let attributes = @azimuth.Attributes::new()
  let context = @azimuth.Context::root()
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000000001L
  
  let record = @azimuth.LogRecord::new_with_context(
    @azimuth.Error,
    Some("Error message"),
    Some(attributes),
    Some(timestamp),
    Some(observed_timestamp),
    Some("trace123"),
    Some("span456"),
    Some(context)
  )
  
  assert_eq(@azimuth.LogRecord::severity_number(record), @azimuth.Error)
  assert_eq(@azimuth.LogRecord::body(record), Some("Error message"))
  assert_eq(@azimuth.LogRecord::trace_id(record), Some("trace123"))
  assert_eq(@azimuth.LogRecord::span_id(record), Some("span456"))
}

test "log severity levels" {
  // Test all log severity levels
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

test "logger emit functionality" {
  // Test logging through logger
  let provider = @azimuth.LoggerProvider::default()
  let logger = @azimuth.LoggerProvider::get_logger(provider, "emit.test")
  
  // Create different log records
  let info_record = @azimuth.LogRecord::new(@azimuth.Info, "Information message")
  let error_record = @azimuth.LogRecord::new_with_context(
    @azimuth.Error,
    Some("Error occurred"),
    None,
    Some(1735689600000000000L),
    None,
    Some("trace789"),
    Some("span101"),
    None
  )
  
  // Emit log records
  @azimuth.Logger::emit(logger, info_record)
  @azimuth.Logger::emit(logger, error_record)
  
  // Test passes if no exceptions are thrown during emission
  assert_true(true)
}

test "log record with optional body" {
  // Test log record without body
  let record_no_body = @azimuth.LogRecord::new_with_context(
    @azimuth.Warn,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(@azimuth.LogRecord::severity_number(record_no_body), @azimuth.Warn)
  assert_eq(@azimuth.LogRecord::body(record_no_body), None)
}

test "logger with noop provider" {
  // Test logger with noop provider
  let noop_provider = @azimuth.LoggerProvider::noop()
  let logger = @azimuth.LoggerProvider::get_logger(noop_provider, "noop.logger")
  
  let record = @azimuth.LogRecord::new(@azimuth.Info, "Noop log message")
  
  // Should not throw exceptions even with noop provider
  @azimuth.Logger::emit(logger, record)
  
  assert_eq(logger.scope.name, "noop.logger")
}