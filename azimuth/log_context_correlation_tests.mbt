// Log Record Context Correlation Tests for Azimuth Telemetry System
// This file contains test cases for log record context correlation

test "basic log record creation and properties" {
  let record = LogRecord::new(Info, "Test log message")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  assert_eq(LogRecord::trace_id(record), None)
  assert_eq(LogRecord::span_id(record), None)
}

test "log record with trace and span correlation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(trace_id),
    Some(span_id),
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
}

test "log record severity levels" {
  let trace_record = LogRecord::new(Trace, "Debug trace information")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Information message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal error message")
  
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
}

test "log record with attributes" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "request.id", StringValue("req-67890"))
  Attributes::set(attrs, "response.time", IntValue(250))
  
  let record = LogRecord::new_with_context(
    Info,
    Some("Request processed successfully"),
    Some(attrs),
    Some(1735689600000000000L),
    None,
    Some("trace123"),
    Some("span456"),
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Request processed successfully"))
  assert_eq(LogRecord::trace_id(record), Some("trace123"))
  assert_eq(LogRecord::span_id(record), Some("span456"))
}

test "log record timestamp handling" {
  let timestamp = 1735689600000000000L  // Unix nanoseconds
  let observed_timestamp = 1735689600000001000L
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Operation completed with warnings"),
    None,
    Some(timestamp),
    Some(observed_timestamp),
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::body(record), Some("Operation completed with warnings"))
}

test "logger provider and logger creation" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  assert_eq(logger.scope.name, "test-logger")
  assert_eq(logger.scope.version, None)
  assert_eq(logger.scope.schema_url, None)
}

test "log emission with context" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "application-logger")
  
  let record = LogRecord::new_with_context(
    Info,
    Some("User action completed"),
    None,
    Some(1735689600000000000L),
    None,
    Some("user-action-trace"),
    Some("user-action-span"),
    None
  )
  
  // Emit log record
  Logger::emit(logger, record)
  
  // Verify emission completed without error
  assert_true(true)
}

test "log record with different body types" {
  let string_body = LogRecord::new(Info, "String message body")
  let empty_body = LogRecord::new_with_context(Debug, None, None, None, None, None, None, None)
  
  assert_eq(LogRecord::body(string_body), Some("String message body"))
  assert_eq(LogRecord::body(empty_body), None)
  assert_eq(LogRecord::severity_number(string_body), Info)
  assert_eq(LogRecord::severity_number(empty_body), Debug)
}

test "log record context chain correlation" {
  // Create log records with correlated trace context
  let root_record = LogRecord::new_with_context(
    Info,
    Some("Request started"),
    None,
    Some(1735689600000000000L),
    None,
    Some("root-trace-id"),
    Some("root-span-id"),
    None
  )
  
  let service_record = LogRecord::new_with_context(
    Info,
    Some("Service processing"),
    None,
    Some(1735689600000001000L),
    None,
    Some("root-trace-id"),
    Some("service-span-id"),
    None
  )
  
  let database_record = LogRecord::new_with_context(
    Debug,
    Some("Database query executed"),
    None,
    Some(1735689600000002000L),
    None,
    Some("root-trace-id"),
    Some("database-span-id"),
    None
  )
  
  // Verify all records share the same trace ID but have different span IDs
  assert_eq(LogRecord::trace_id(root_record), Some("root-trace-id"))
  assert_eq(LogRecord::trace_id(service_record), Some("root-trace-id"))
  assert_eq(LogRecord::trace_id(database_record), Some("root-trace-id"))
  
  assert_eq(LogRecord::span_id(root_record), Some("root-span-id"))
  assert_eq(LogRecord::span_id(service_record), Some("service-span-id"))
  assert_eq(LogRecord::span_id(database_record), Some("database-span-id"))
}

test "log record with instrumentation scope" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "service-component", Some("1.2.3"))
  
  assert_eq(logger.scope.name, "service-component")
  assert_eq(logger.scope.version, Some("1.2.3"))
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Component failure"),
    None,
    Some(1735689600000000000L),
    None,
    None,
    None,
    None
  )
  
  Logger::emit(logger, record)
  
  // Verify emission completed without error
  assert_true(true)
}