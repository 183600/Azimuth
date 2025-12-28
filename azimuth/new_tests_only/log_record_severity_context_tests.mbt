// Log Record Severity and Context Tests
// Tests for log record creation, severity levels, and context management

test "log record creation with different severity levels" {
  let trace_log = LogRecord::new(Trace, "Debug trace information")
  let debug_log = LogRecord::new(Debug, "Debug message")
  let info_log = LogRecord::new(Info, "Information message")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error occurred")
  let fatal_log = LogRecord::new(Fatal, "Fatal error occurred")
  
  // Test severity levels
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // Test log bodies
  assert_eq(LogRecord::body(trace_log), Some("Debug trace information"))
  assert_eq(LogRecord::body(debug_log), Some("Debug message"))
  assert_eq(LogRecord::body(info_log), Some("Information message"))
  assert_eq(LogRecord::body(warn_log), Some("Warning message"))
  assert_eq(LogRecord::body(error_log), Some("Error occurred"))
  assert_eq(LogRecord::body(fatal_log), Some("Fatal error occurred"))
}

test "log record with comprehensive context" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000000100L
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("user123"))
  Attributes::set(attrs, "request.id", StringValue("req456"))
  Attributes::set(attrs, "response.time", IntValue(250))
  
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_value = Context::with_value(ctx, key, "corr789")
  
  let log = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_value)
  )
  
  // Test log properties
  assert_eq(LogRecord::severity_number(log), Error)
  assert_eq(LogRecord::body(log), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(log), Some(trace_id))
  assert_eq(LogRecord::span_id(log), Some(span_id))
}

test "logger provider and logger lifecycle" {
  let provider = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(provider, "service.logger")
  let logger2 = LoggerProvider::get_logger(provider, "auth.logger", Some("1.0.0"))
  
  // Test logger scope properties
  assert_eq(logger1.scope.name, "service.logger")
  assert_eq(logger1.scope.version, None)
  assert_eq(logger2.scope.name, "auth.logger")
  assert_eq(logger2.scope.version, Some("1.0.0"))
  
  // Test log emission
  let info_log = LogRecord::new(Info, "Service started successfully")
  let error_log = LogRecord::new(Error, "Authentication failed")
  
  Logger::emit(logger1, info_log)
  Logger::emit(logger2, error_log)
}

test "log record with empty and null values" {
  // Test log with empty body
  let empty_body_log = LogRecord::new(Info, "")
  assert_eq(LogRecord::body(empty_body_log), Some(""))
  
  // Test log with no attributes
  let no_attrs_log = LogRecord::new(Debug, "Debug message")
  assert_eq(LogRecord::severity_number(no_attrs_log), Debug)
  
  // Test log with minimal context
  let minimal_log = LogRecord::new_with_context(
    Warn,
    Some("Warning with minimal context"),
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(minimal_log), Warn)
  assert_eq(LogRecord::body(minimal_log), Some("Warning with minimal context"))
  assert_eq(LogRecord::trace_id(minimal_log), None)
  assert_eq(LogRecord::span_id(minimal_log), None)
}

test "log record with complex attributes" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("payment-service"))
  Attributes::set(attrs, "service.version", StringValue("2.1.0"))
  Attributes::set(attrs, "service.instance.id", StringValue("instance-abc123"))
  Attributes::set(attrs, "transaction.id", StringValue("txn-789xyz"))
  Attributes::set(attrs, "amount", FloatValue(99.99))
  Attributes::set(attrs, "currency", StringValue("USD"))
  Attributes::set(attrs, "customer.id", IntValue(12345))
  Attributes::set(attrs, "payment.method", StringValue("credit_card"))
  Attributes::set(attrs, "payment.status", StringValue("completed"))
  Attributes::set(attrs, "processing.time", IntValue(1500))
  Attributes::set(attrs, "retry.count", IntValue(0))
  Attributes::set(attrs, "success", BoolValue(true))
  
  let log = LogRecord::new_with_context(
    Info,
    Some("Payment processed successfully"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000050L),
    Some("trace123456"),
    Some("span789012"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(log), Info)
  assert_eq(LogRecord::body(log), Some("Payment processed successfully"))
  assert_eq(LogRecord::trace_id(log), Some("trace123456"))
  assert_eq(LogRecord::span_id(log), Some("span789012"))
}

test "log record severity comparison and ordering" {
  let logs = [
    LogRecord::new(Trace, "Trace message"),
    LogRecord::new(Debug, "Debug message"),
    LogRecord::new(Info, "Info message"),
    LogRecord::new(Warn, "Warning message"),
    LogRecord::new(Error, "Error message"),
    LogRecord::new(Fatal, "Fatal message")
  ]
  
  // Test severity levels in order
  assert_eq(LogRecord::severity_number(logs[0]), Trace)
  assert_eq(LogRecord::severity_number(logs[1]), Debug)
  assert_eq(LogRecord::severity_number(logs[2]), Info)
  assert_eq(LogRecord::severity_number(logs[3]), Warn)
  assert_eq(LogRecord::severity_number(logs[4]), Error)
  assert_eq(LogRecord::severity_number(logs[5]), Fatal)
  
  // Test that all logs have bodies
  for log in logs {
    assert_true(LogRecord::body(log) != None)
  }
}

test "log record with array attributes" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "tags", ArrayStringValue(["payment", "api", "v2"]))
  Attributes::set(attrs, "error.codes", ArrayIntValue([400, 401, 403, 500]))
  Attributes::set(attrs, "service.endpoints", ArrayStringValue(["/api/payments", "/api/refunds"]))
  
  let log = LogRecord::new_with_context(
    Warn,
    Some("Multiple validation errors occurred"),
    Some(attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(log), Warn)
  assert_eq(LogRecord::body(log), Some("Multiple validation errors occurred"))
}

test "log record context value operations" {
  let ctx = Context::root()
  let key1 = ContextKey::new("request.id")
  let key2 = ContextKey::new("user.session")
  let key3 = ContextKey::new("correlation.id")
  
  // Test context with single value
  let ctx1 = Context::with_value(ctx, key1, "req-123")
  assert_eq(Context::get(ctx1, key1), Some("req-123"))
  assert_eq(Context::get(ctx1, key2), None)
  
  // Test context with different values
  let ctx2 = Context::with_value(ctx, key2, "session-456")
  assert_eq(Context::get(ctx2, key2), Some("session-456"))
  assert_eq(Context::get(ctx2, key1), None)
  
  // Create log with context
  let log1 = LogRecord::new_with_context(
    Info,
    Some("Request processed"),
    None,
    None,
    None,
    None,
    None,
    Some(ctx1)
  )
  
  let log2 = LogRecord::new_with_context(
    Info,
    Some("Session created"),
    None,
    None,
    None,
    None,
    None,
    Some(ctx2)
  )
  
  assert_eq(LogRecord::body(log1), Some("Request processed"))
  assert_eq(LogRecord::body(log2), Some("Session created"))
}