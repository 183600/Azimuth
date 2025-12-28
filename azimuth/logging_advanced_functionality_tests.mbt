// Logging Advanced Functionality Tests for Azimuth Telemetry System
// This file contains comprehensive test cases for advanced logging functionality

test "log record severity levels comprehensive" {
  // Test all severity levels
  let trace_record = LogRecord::new(Trace, "Trace level message")
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::body(trace_record), Some("Trace level message"))
  
  let debug_record = LogRecord::new(Debug, "Debug level message")
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::body(debug_record), Some("Debug level message"))
  
  let info_record = LogRecord::new(Info, "Info level message")
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::body(info_record), Some("Info level message"))
  
  let warn_record = LogRecord::new(Warn, "Warning level message")
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::body(warn_record), Some("Warning level message"))
  
  let error_record = LogRecord::new(Error, "Error level message")
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::body(error_record), Some("Error level message"))
  
  let fatal_record = LogRecord::new(Fatal, "Fatal level message")
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  assert_eq(LogRecord::body(fatal_record), Some("Fatal level message"))
}

test "log record with comprehensive attributes" {
  let attrs = Attributes::new()
  
  // Set various attribute types
  Attributes::set(attrs, "user.id", IntValue(12345))
  Attributes::set(attrs, "user.name", StringValue("john.doe"))
  Attributes::set(attrs, "request.id", StringValue("req-abc-123"))
  Attributes::set(attrs, "request.size", IntValue(1024))
  Attributes::set(attrs, "response.code", IntValue(200))
  Attributes::set(attrs, "cache.hit", BoolValue(true))
  Attributes::set(attrs, "processing.time", FloatValue(125.5))
  Attributes::set(attrs, "tags", ArrayStringValue(["api", "v1", "authenticated"]))
  Attributes::set(attrs, "retry.counts", ArrayIntValue([1, 2, 3]))
  
  let record_with_attrs = LogRecord::new_with_context(
    Info,
    Some("Request processed successfully"),
    Some(attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-12345"),
    Some("span-67890"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(record_with_attrs), Info)
  assert_eq(LogRecord::body(record_with_attrs), Some("Request processed successfully"))
  assert_eq(LogRecord::trace_id(record_with_attrs), Some("trace-12345"))
  assert_eq(LogRecord::span_id(record_with_attrs), Some("span-67890"))
}

test "log record with timestamps" {
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  let observed_timestamp = timestamp + 1000000L  // 1ms later
  
  let record_with_timestamps = LogRecord::new_with_context(
    Warn,
    Some("Warning with timestamps"),
    None,
    Some(timestamp),
    Some(observed_timestamp),
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record_with_timestamps), Warn)
  assert_eq(LogRecord::body(record_with_timestamps), Some("Warning with timestamps"))
  // Note: In a real implementation, we would have getters for timestamps
}

test "log record with trace and span context" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let record_with_context = LogRecord::new_with_context(
    Error,
    Some("Error in trace context"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(record_with_context), Error)
  assert_eq(LogRecord::body(record_with_context), Some("Error in trace context"))
  assert_eq(LogRecord::trace_id(record_with_context), Some(trace_id))
  assert_eq(LogRecord::span_id(record_with_context), Some(span_id))
}

test "logger provider and logger operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test.logger", Some("1.0.0"))
  
  // Test basic log record emission
  let basic_record = LogRecord::new(Info, "Basic log message")
  Logger::emit(logger, basic_record)
  
  // Test complex log record emission
  let attrs = Attributes::new()
  Attributes::set(attrs, "operation", StringValue("user.login"))
  Attributes::set(attrs, "user.id", IntValue(12345))
  
  let complex_record = LogRecord::new_with_context(
    Info,
    Some("User login successful"),
    Some(attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-123"),
    Some("span-456"),
    Some(Context::root())
  )
  
  Logger::emit(logger, complex_record)
  
  // Test error log record emission
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-error"),
    Some("span-error"),
    Some(Context::root())
  )
  
  Logger::emit(logger, error_record)
}

test "log record with special content" {
  // Test log with empty body
  let empty_body_record = LogRecord::new_with_context(
    Info,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::body(empty_body_record), None)
  
  // Test log with very long body
  let long_body = "This is a very long log message that tests the system's ability to handle extended log content with various characters, numbers (12345), special characters @#$%^&*(), and should be processed correctly without issues."
  let long_body_record = LogRecord::new(Info, long_body)
  assert_eq(LogRecord::body(long_body_record), Some(long_body))
  
  // Test log with special characters
  let special_body_record = LogRecord::new(Warn, "Special chars: !@#$%^&*()_+-={}[]|\\:;\"'<>?,./ and Unicode: 测试 and Emoji: 🚀🌟")
  assert_eq(LogRecord::body(special_body_record), Some("Special chars: !@#$%^&*()_+-={}[]|\\:;\"'<>?,./ and Unicode: 测试 and Emoji: 🚀🌟"))
  
  // Test log with JSON content
  let json_body = "{\"error\":{\"code\":500,\"message\":\"Internal server error\",\"details\":{\"service\":\"payment\",\"transaction_id\":\"txn_12345\"}},\"timestamp\":\"2025-12-28T10:30:00Z\"}"
  let json_record = LogRecord::new(Error, json_body)
  assert_eq(LogRecord::body(json_record), Some(json_body))
}

test "log record performance and batch operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "performance.logger")
  
  // Test creating multiple log records
  let records = []
  for i in range(0, 10) {
    let severity = match i % 6 {
      0 => Trace
      1 => Debug
      2 => Info
      3 => Warn
      4 => Error
      _ => Fatal
    }
    let message = "Log message " + i.to_string()
    let record = LogRecord::new(severity, message)
    records.push(record)
  }
  
  // Test batch emission
  for record in records {
    Logger::emit(logger, record)
  }
  
  // Verify all records were created
  assert_eq(length(records), 10)
  
  // Test with different severity levels
  assert_eq(LogRecord::severity_number(records[0]), Trace)
  assert_eq(LogRecord::severity_number(records[1]), Debug)
  assert_eq(LogRecord::severity_number(records[2]), Info)
  assert_eq(LogRecord::severity_number(records[3]), Warn)
  assert_eq(LogRecord::severity_number(records[4]), Error)
  assert_eq(LogRecord::severity_number(records[5]), Fatal)
}

test "log record with context integration" {
  let ctx = Context::root()
  let key = ContextKey::new("request.id")
  let ctx_with_request = Context::with_value(ctx, key, "req-abc-123")
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("user-service"))
  Attributes::set(attrs, "operation", StringValue("authenticate"))
  
  let contextual_record = LogRecord::new_with_context(
    Info,
    Some("Authentication attempt"),
    Some(attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-auth-123"),
    Some("span-auth-456"),
    Some(ctx_with_request)
  )
  
  assert_eq(LogRecord::severity_number(contextual_record), Info)
  assert_eq(LogRecord::body(contextual_record), Some("Authentication attempt"))
  assert_eq(LogRecord::trace_id(contextual_record), Some("trace-auth-123"))
  assert_eq(LogRecord::span_id(contextual_record), Some("span-auth-456"))
}

test "log record error scenarios and edge cases" {
  // Test log with minimal information
  let minimal_record = LogRecord::new_with_context(
    Info,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::severity_number(minimal_record), Info)
  assert_eq(LogRecord::body(minimal_record), None)
  
  // Test log with invalid trace/span IDs (empty strings)
  let invalid_context_record = LogRecord::new_with_context(
    Warn,
    Some("Warning with invalid context"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(""),
    Some(""),
    Some(Context::root())
  )
  assert_eq(LogRecord::trace_id(invalid_context_record), Some(""))
  assert_eq(LogRecord::span_id(invalid_context_record), Some(""))
  
  // Test log with zero timestamp
  let zero_timestamp_record = LogRecord::new_with_context(
    Error,
    Some("Error with zero timestamp"),
    None,
    Some(0L),
    None,
    None,
    None,
    None
  )
  
  // Test log with negative timestamp (should be handled gracefully)
  let negative_timestamp_record = LogRecord::new_with_context(
    Fatal,
    Some("Fatal with negative timestamp"),
    None,
    Some(-1000L),
    None,
    None,
    None,
    None
  )
}

test "log record with structured data" {
  let attrs = Attributes::new()
  
  // Test structured error information
  Attributes::set(attrs, "error.type", StringValue("DatabaseError"))
  Attributes::set(attrs, "error.code", IntValue(500))
  Attributes::set(attrs, "error.message", StringValue("Connection timeout"))
  Attributes::set(attrs, "error.stack_trace", StringValue("at Database.query (db.js:123)"))
  Attributes::set(attrs, "error.retryable", BoolValue(true))
  Attributes::set(attrs, "error.retry_count", IntValue(3))
  
  let structured_error_record = LogRecord::new_with_context(
    Error,
    Some("Database operation failed"),
    Some(attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-db-error"),
    Some("span-db-operation"),
    Some(Context::root())
  )
  
  // Test structured performance information
  let perf_attrs = Attributes::new()
  Attributes::set(perf_attrs, "operation.name", StringValue("http.request"))
  Attributes::set(perf_attrs, "operation.duration_ms", FloatValue(245.7))
  Attributes::set(perf_attrs, "operation.status", StringValue("success"))
  Attributes::set(perf_attrs, "operation.bytes_sent", IntValue(1024))
  Attributes::set(perf_attrs, "operation.bytes_received", IntValue(2048))
  Attributes::set(perf_attrs, "operation.cache_hit", BoolValue(false))
  
  let structured_perf_record = LogRecord::new_with_context(
    Info,
    Some("HTTP request completed"),
    Some(perf_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-http-req"),
    Some("span-http-client"),
    Some(Context::root())
  )
  
  // Test structured audit information
  let audit_attrs = Attributes::new()
  Attributes::set(audit_attrs, "audit.action", StringValue("user.delete"))
  Attributes::set(audit_attrs, "audit.user_id", IntValue(12345))
  Attributes::set(audit_attrs, "audit.target_user", IntValue(67890))
  Attributes::set(audit_attrs, "audit.reason", StringValue("Violation of terms"))
  Attributes::set(audit_attrs, "audit.admin_id", IntValue(999))
  Attributes::set(audit_attrs, "audit.ip_address", StringValue("192.168.1.100"))
  
  let structured_audit_record = LogRecord::new_with_context(
    Warn,
    Some("User account deleted by admin"),
    Some(audit_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-audit"),
    Some("span-admin-action"),
    Some(Context::root())
  )
}