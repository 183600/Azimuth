// Logging severity and comprehensive log record tests
// Focus on testing different severity levels and log record creation

test "log severity levels" {
  // Test all severity levels
  let trace_record = LogRecord::new(Trace, "Trace message")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Info message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  
  // Verify severity levels
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Verify log bodies
  match LogRecord::body(trace_record) {
    Some(body) => assert_eq(body, "Trace message")
    None => assert_true(false, "Expected trace message body")
  }
  
  match LogRecord::body(error_record) {
    Some(body) => assert_eq(body, "Error message")
    None => assert_true(false, "Expected error message body")
  }
}

test "log record with full context" {
  // Create log record with all optional fields
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "request.id", StringValue("req-67890"))
  
  let ctx = Context::root()
  let ctx_with_value = Context::with_value(ctx, ContextKey::new("correlation.id"), "corr-abcde")
  
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(1735689600000000000L),  // timestamp
    Some(1735689600000001000L),  // observed timestamp
    Some("trace-123"),
    Some("span-456"),
    Some(ctx_with_value)
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Error)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Database connection failed")
    None => assert_true(false, "Expected log body")
  }
  
  match LogRecord::trace_id(log_record) {
    Some(trace_id) => assert_eq(trace_id, "trace-123")
    None => assert_true(false, "Expected trace ID")
  }
  
  match LogRecord::span_id(log_record) {
    Some(span_id) => assert_eq(span_id, "span-456")
    None => assert_true(false, "Expected span ID")
  }
}

test "log record edge cases" {
  // Test log record with empty body
  let empty_body_record = LogRecord::new_with_context(
    Info,
    None,  // No body
    None,  // No attributes
    None,  // No timestamp
    None,  // No observed timestamp
    None,  // No trace ID
    None,  // No span ID
    None   // No context
  )
  
  assert_eq(LogRecord::severity_number(empty_body_record), Info)
  assert_eq(LogRecord::body(empty_body_record), None)
  assert_eq(LogRecord::trace_id(empty_body_record), None)
  assert_eq(LogRecord::span_id(empty_body_record), None)
  
  // Test log record with empty string body
  let empty_string_record = LogRecord::new_with_context(
    Warn,
    Some(""),  // Empty string body
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(empty_string_record), Warn)
  match LogRecord::body(empty_string_record) {
    Some(body) => assert_eq(body.length(), 0)
    None => assert_true(false, "Expected empty string body")
  }
}

test "logger provider and logger creation" {
  // Test different logger providers
  let noop_provider = LoggerProvider::noop()
  let default_provider = LoggerProvider::default()
  
  // Test creating loggers from providers
  let noop_logger = LoggerProvider::get_logger(noop_provider, "noop-logger")
  let default_logger = LoggerProvider::get_logger(default_provider, "default-logger")
  
  // Test logger scope properties
  let noop_scope = noop_logger.scope
  let default_scope = default_logger.scope
  
  assert_eq(noop_scope.name, "noop-logger")
  assert_eq(default_scope.name, "default-logger")
  assert_eq(noop_scope.version, None)
  assert_eq(default_scope.version, None)
  assert_eq(noop_scope.schema_url, None)
  assert_eq(default_scope.schema_url, None)
}

test "log emission with different scenarios" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // Test emitting simple log records
  let simple_record = LogRecord::new(Info, "Simple log message")
  Logger::emit(logger, simple_record)
  
  // Test emitting log records with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "component", StringValue("auth"))
  Attributes::set(attrs, "action", StringValue("login"))
  
  let attr_record = LogRecord::new_with_context(
    Info,
    Some("User login attempt"),
    Some(attrs),
    Some(Clock::now_unix_nanos({})),
    None,
    Some("user-login-trace"),
    Some("auth-span"),
    None
  )
  Logger::emit(logger, attr_record)
  
  // Test emitting error log records
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Authentication failed"),
    Some(attrs),
    Some(Clock::now_unix_nanos({})),
    None,
    Some("auth-failed-trace"),
    Some("error-span"),
    None
  )
  Logger::emit(logger, error_record)
  
  // Test that emission doesn't crash
  assert_true(true)
}

test "log record with complex attributes" {
  // Create complex attributes for log record
  let complex_attrs = Attributes::new()
  Attributes::set(complex_attrs, "string.array", ArrayStringValue(["error1", "error2", "error3"]))
  Attributes::set(complex_attrs, "int.array", ArrayIntValue([100, 200, 300]))
  Attributes::set(complex_attrs, "metric.value", FloatValue(99.99))
  Attributes::set(complex_attrs, "success", BoolValue(false))
  Attributes::set(complex_attrs, "empty.value", StringValue(""))
  
  // Create log record with complex attributes
  let complex_record = LogRecord::new_with_context(
    Warn,
    Some("Complex log with multiple attribute types"),
    Some(complex_attrs),
    Some(1735689600000000000L),
    Some(1735689600000000500L),
    Some("complex-trace-id"),
    Some("complex-span-id"),
    Some(Context::with_value(Context::root(), ContextKey::new("request.id"), "req-12345"))
  )
  
  // Verify complex log record properties
  assert_eq(LogRecord::severity_number(complex_record), Warn)
  match LogRecord::body(complex_record) {
    Some(body) => assert_eq(body, "Complex log with multiple attribute types")
    None => assert_true(false, "Expected complex log body")
  }
  
  // Test that complex log record can be emitted
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "complex-logger")
  Logger::emit(logger, complex_record)
  
  assert_true(true)  // Test passes if no crash occurs
}