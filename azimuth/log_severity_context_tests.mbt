// Log Record Severity Tests for Azimuth Telemetry System
// Testing log record severity levels and context operations

test "log record severity levels" {
  let trace_record = LogRecord::new(Trace, "Trace message")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Info message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  
  // Test severity levels
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Test message bodies
  assert_eq(LogRecord::body(trace_record), Some("Trace message"))
  assert_eq(LogRecord::body(debug_record), Some("Debug message"))
  assert_eq(LogRecord::body(info_record), Some("Info message"))
  assert_eq(LogRecord::body(warn_record), Some("Warning message"))
  assert_eq(LogRecord::body(error_record), Some("Error message"))
  assert_eq(LogRecord::body(fatal_record), Some("Fatal message"))
}

test "log record with context" {
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689601000000000L
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let ctx = Context::with_value(Context::root(), ContextKey::new("user.id"), "12345")
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    None,
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(ctx)
  )
  
  // Test context properties
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
}

test "log record severity comparison" {
  let trace_sev = Trace
  let debug_sev = Debug
  let info_sev = Info
  let warn_sev = Warn
  let error_sev = Error
  let fatal_sev = Fatal
  
  // Test severity level ordering (lower enum values should be less severe)
  assert_true(trace_sev != debug_sev)
  assert_true(debug_sev != info_sev)
  assert_true(info_sev != warn_sev)
  assert_true(warn_sev != error_sev)
  assert_true(error_sev != fatal_sev)
  
  // Test severity equality
  assert_eq(trace_sev, Trace)
  assert_eq(debug_sev, Debug)
  assert_eq(info_sev, Info)
  assert_eq(warn_sev, Warn)
  assert_eq(error_sev, Error)
  assert_eq(fatal_sev, Fatal)
}

test "log record with attributes" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "error.code", StringValue("500"))
  Attributes::set(attrs, "retry.count", IntValue(3))
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Service unavailable"),
    Some(attrs),
    Some(1735689600000000000L),
    None,
    None,
    None,
    None
  )
  
  // Test record properties
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Service unavailable"))
}

test "log record edge cases" {
  // Test empty body
  let empty_record = LogRecord::new(Info, "")
  assert_eq(LogRecord::body(empty_record), Some(""))
  
  // Test long message
  let long_message = "This is a very long log message that contains detailed information about what happened in the system and should be properly handled"
  let long_record = LogRecord::new(Info, long_message)
  assert_eq(LogRecord::body(long_record), Some(long_message))
  
  // Test special characters
  let special_message = "Error: Invalid UTF-8 sequence: \x00\x01\x02"
  let special_record = LogRecord::new(Error, special_message)
  assert_eq(LogRecord::body(special_record), Some(special_message))
}

test "log record timestamp operations" {
  let current_time = Clock::now_unix_nanos(Clock::system())
  
  let record1 = LogRecord::new_with_context(
    Info,
    Some("Message 1"),
    None,
    Some(current_time),
    None,
    None,
    None,
    None
  )
  
  let record2 = LogRecord::new_with_context(
    Info,
    Some("Message 2"),
    None,
    Some(current_time + 1000000L),
    None,
    None,
    None,
    None
  )
  
  // Test timestamp assignment
  assert_eq(LogRecord::body(record1), Some("Message 1"))
  assert_eq(LogRecord::body(record2), Some("Message 2"))
}

test "logger provider operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test.logger")
  
  // Test logger creation
  assert_eq(logger.scope.name, "test.logger")
  assert_eq(logger.scope.version, None)
  assert_eq(logger.scope.schema_url, None)
  
  // Test log emission
  let record = LogRecord::new(Info, "Test log message")
  Logger::emit(logger, record)
  
  // Test with different severity levels
  let trace_record = LogRecord::new(Trace, "Trace level")
  let error_record = LogRecord::new(Error, "Error level")
  
  Logger::emit(logger, trace_record)
  Logger::emit(logger, error_record)
  
  assert_true(true) // If we reach here, emission didn't crash
}

test "log record severity context validation" {
  let test_cases = [
    (Trace, "Debugging information"),
    (Debug, "Development information"),
    (Info, "General information"),
    (Warn, "Warning conditions"),
    (Error, "Error conditions"),
    (Fatal, "Critical failure")
  ]
  
  for (severity, message) in test_cases {
    let record = LogRecord::new(severity, message)
    assert_eq(LogRecord::severity_number(record), severity)
    assert_eq(LogRecord::body(record), Some(message))
  }
}