// Test file for LogRecord and Logger functionality

pub test "log record creation and basic properties" {
  let record = LogRecord::new(Info, "Test log message")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  assert_eq(record.attributes, None)
  assert_eq(record.timestamp, None)
  assert_eq(record.observed_timestamp, None)
  assert_eq(record.trace_id, None)
  assert_eq(record.span_id, None)
  assert_eq(record.context, None)
}

pub test "log record with all severity levels" {
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
  
  assert_eq(LogRecord::body(trace_record), Some("Trace message"))
  assert_eq(LogRecord::body(debug_record), Some("Debug message"))
  assert_eq(LogRecord::body(info_record), Some("Info message"))
  assert_eq(LogRecord::body(warn_record), Some("Warning message"))
  assert_eq(LogRecord::body(error_record), Some("Error message"))
  assert_eq(LogRecord::body(fatal_record), Some("Fatal message"))
}

pub test "log record with extended context" {
  let trace_id = Some("0af7651916cd43dd8448eb211c80319c")
  let span_id = Some("b7ad6b7169203331")
  let timestamp = Some(1735689600000000000L)
  let observed_timestamp = Some(1735689600000000001L)
  let context = Some(Context::root())
  let attributes = Some(Attributes::new())
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Detailed error message"),
    attributes,
    timestamp,
    observed_timestamp,
    trace_id,
    span_id,
    context
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Detailed error message"))
  assert_eq(record.attributes, attributes)
  assert_eq(record.timestamp, timestamp)
  assert_eq(record.observed_timestamp, observed_timestamp)
  assert_eq(LogRecord::trace_id(record), trace_id)
  assert_eq(LogRecord::span_id(record), span_id)
  assert_eq(record.context, context)
}

pub test "logger provider and logger creation" {
  let provider = LoggerProvider::noop()
  let logger1 = LoggerProvider::get_logger(provider, "test.logger")
  let logger2 = LoggerProvider::get_logger(provider, "test.logger2", Some("1.0.0"))
  
  assert_eq(logger1.scope.name, "test.logger")
  assert_eq(logger1.scope.version, None)
  assert_eq(logger1.scope.schema_url, None)
  
  assert_eq(logger2.scope.name, "test.logger2")
  assert_eq(logger2.scope.version, Some("1.0.0"))
  assert_eq(logger2.scope.schema_url, None)
}

pub test "logger emit operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "emit.logger")
  
  // Test simple log record emission
  let simple_record = LogRecord::new(Info, "Simple log message")
  Logger::emit(logger, simple_record)
  
  // Test complex log record emission
  let complex_record = LogRecord::new_with_context(
    Warn,
    Some("Complex log message"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  Logger::emit(logger, complex_record)
  
  // Test emission with all severity levels
  let trace_record = LogRecord::new(Trace, "Trace level")
  let debug_record = LogRecord::new(Debug, "Debug level")
  let info_record = LogRecord::new(Info, "Info level")
  let warn_record = LogRecord::new(Warn, "Warning level")
  let error_record = LogRecord::new(Error, "Error level")
  let fatal_record = LogRecord::new(Fatal, "Fatal level")
  
  Logger::emit(logger, trace_record)
  Logger::emit(logger, debug_record)
  Logger::emit(logger, info_record)
  Logger::emit(logger, warn_record)
  Logger::emit(logger, error_record)
  Logger::emit(logger, fatal_record)
}

pub test "log record edge cases" {
  // Test with empty body
  let empty_body_record = LogRecord::new(Info, "")
  assert_eq(LogRecord::body(empty_body_record), Some(""))
  
  // Test with None body using extended constructor
  let none_body_record = LogRecord::new_with_context(
    Debug,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::body(none_body_record), None)
  
  // Test with very long message
  let long_message = "a".repeat(10000)
  let long_record = LogRecord::new(Error, long_message)
  assert_eq(LogRecord::body(long_record), Some(long_message))
  
  // Test with special characters
  let special_message = "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_record = LogRecord::new(Warn, special_message)
  assert_eq(LogRecord::body(special_record), Some(special_message))
  
  // Test with unicode characters
  let unicode_message = "Unicode: 测试 🚀 emoji"
  let unicode_record = LogRecord::new(Info, unicode_message)
  assert_eq(LogRecord::body(unicode_record), Some(unicode_message))
}

pub test "log record with timestamps" {
  let current_time = 1735689600000000000L
  let future_time = 1735689700000000000L
  let past_time = 1735689500000000000L
  
  let record1 = LogRecord::new_with_context(
    Info,
    Some("Current time"),
    None,
    Some(current_time),
    None,
    None,
    None,
    None
  )
  
  let record2 = LogRecord::new_with_context(
    Info,
    Some("Future time"),
    None,
    Some(future_time),
    Some(future_time + 1L),
    None,
    None,
    None
  )
  
  let record3 = LogRecord::new_with_context(
    Info,
    Some("Past time"),
    None,
    Some(past_time),
    Some(past_time + 1000L),
    None,
    None,
    None
  )
  
  assert_eq(record1.timestamp, Some(current_time))
  assert_eq(record1.observed_timestamp, None)
  
  assert_eq(record2.timestamp, Some(future_time))
  assert_eq(record2.observed_timestamp, Some(future_time + 1L))
  
  assert_eq(record3.timestamp, Some(past_time))
  assert_eq(record3.observed_timestamp, Some(past_time + 1000L))
}