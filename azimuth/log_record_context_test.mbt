// Log Record Context Test Suite
// Tests for log record operations with context and trace information

test "log record creation with different severity levels" {
  // Test different severity levels
  let trace_record = LogRecord::new(Trace, "Debug trace message")
  let debug_record = LogRecord::new(Debug, "Debug information")
  let info_record = LogRecord::new(Info, "Information message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error occurred")
  let fatal_record = LogRecord::new(Fatal, "Fatal error")
  
  // Test severity levels
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Test bodies
  assert_eq(LogRecord::body(trace_record), Some("Debug trace message"))
  assert_eq(LogRecord::body(info_record), Some("Information message"))
  assert_eq(LogRecord::body(error_record), Some("Error occurred"))
}

test "log record with full context" {
  let attributes = Attributes::new()
  let context = Context::root()
  let timestamp = 1735689600000000000L  // 2025 timestamp
  let observed_timestamp = 1735689600000001000L
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attributes),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(context)
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
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
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Request processed successfully"))
}

test "logger operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "my-logger", Some("1.0.0"))
  
  // Test logger scope
  assert_eq(logger.scope.name, "my-logger")
  assert_eq(logger.scope.version, Some("1.0.0"))
  assert_eq(logger.scope.schema_url, None)
  
  // Test log emission
  let info_record = LogRecord::new(Info, "Application started")
  Logger::emit(logger, info_record)
  
  let error_record = LogRecord::new(Error, "Failed to connect to database")
  Logger::emit(logger, error_record)
  
  assert_true(true)  // Simplified test - just verify operations don't crash
}

test "log record with trace correlation" {
  let trace_id = "trace-123456789"
  let span_id = "span-987654321"
  
  // Create log record with trace correlation
  let record = LogRecord::new_with_context(
    Warn,
    Some("Operation took longer than expected"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
  assert_eq(LogRecord::body(record), Some("Operation took longer than expected"))
}

test "log record timestamp operations" {
  let clock = Clock::system()
  let current_time = Clock::now_unix_nanos(clock)
  let observed_time = current_time + 1000000L  // 1ms later
  
  let record = LogRecord::new_with_context(
    Info,
    Some("Timestamp test"),
    None,
    Some(current_time),
    Some(observed_time),
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Timestamp test"))
}

test "noop logger provider" {
  let noop_provider = LoggerProvider::noop()
  let noop_logger = LoggerProvider::get_logger(noop_provider, "noop-logger")
  
  // Test noop logger
  assert_eq(noop_logger.scope.name, "noop-logger")
  
  // Test emission with noop logger
  let record = LogRecord::new(Error, "This should be discarded")
  Logger::emit(noop_logger, record)
  
  assert_true(true)  // Simplified test - just verify operations don't crash
}

test "log record with complex body" {
  // Test log records with different body formats
  let json_body = "{\"error\": \"Database timeout\", \"code\": 500}"
  let record1 = LogRecord::new(Error, json_body)
  
  let multi_line_body = "Multi-line error message:\nLine 1: Connection failed\nLine 2: Retry attempt 1"
  let record2 = LogRecord::new(Error, multi_line_body)
  
  let empty_body = ""
  let record3 = LogRecord::new(Info, empty_body)
  
  assert_eq(LogRecord::body(record1), Some(json_body))
  assert_eq(LogRecord::body(record2), Some(multi_line_body))
  assert_eq(LogRecord::body(record3), Some(empty_body))
}

test "log record severity ordering" {
  // Test that severity levels have the expected order
  assert_true(Trace as Int < Debug as Int)
  assert_true(Debug as Int < Info as Int)
  assert_true(Info as Int < Warn as Int)
  assert_true(Warn as Int < Error as Int)
  assert_true(Error as Int < Fatal as Int)
  
  // Test severity comparisons
  let trace_record = LogRecord::new(Trace, "Trace message")
  let error_record = LogRecord::new(Error, "Error message")
  
  assert_true(LogRecord::severity_number(trace_record) as Int < LogRecord::severity_number(error_record) as Int)
}