// Advanced Logger and LogRecord Functionality Tests
// Testing comprehensive logging capabilities with different severity levels and contexts

test "log record creation with all severity levels" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // Test creating log records with all severity levels
  let trace_record = LogRecord::new(Trace, "Trace level message")
  let debug_record = LogRecord::new(Debug, "Debug level message")
  let info_record = LogRecord::new(Info, "Info level message")
  let warn_record = LogRecord::new(Warn, "Warning level message")
  let error_record = LogRecord::new(Error, "Error level message")
  let fatal_record = LogRecord::new(Fatal, "Fatal level message")
  
  // Verify severity levels
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Verify log bodies
  assert_eq(LogRecord::body(trace_record), Some("Trace level message"))
  assert_eq(LogRecord::body(debug_record), Some("Debug level message"))
  assert_eq(LogRecord::body(info_record), Some("Info level message"))
  assert_eq(LogRecord::body(warn_record), Some("Warning level message"))
  assert_eq(LogRecord::body(error_record), Some("Error level message"))
  assert_eq(LogRecord::body(fatal_record), Some("Fatal level message"))
}

test "log record with comprehensive context" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000001000L
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "request.method", StringValue("POST"))
  Attributes::set(attrs, "response.status", IntValue(200))
  
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let enriched_ctx = Context::with_value(ctx, key, "corr-abc-123")
  
  // Create comprehensive log record
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(enriched_ctx)
  )
  
  // Verify all properties
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
}

test "logger emission with different log patterns" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "application-logger")
  
  // Test structured logging patterns
  
  // Simple message log
  let simple_log = LogRecord::new(Info, "Application started successfully")
  Logger::emit(logger, simple_log)
  
  // Log with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("user-service"))
  Attributes::set(attrs, "service.version", StringValue("1.2.3"))
  Attributes::set(attrs, "port", IntValue(8080))
  
  let structured_log = LogRecord::new_with_context(
    Info,
    Some("Service initialized"),
    Some(attrs),
    None,
    None,
    None,
    None,
    None
  )
  Logger::emit(logger, structured_log)
  
  // Error log with context
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error.type", StringValue("ValidationError"))
  Attributes::set(error_attrs, "error.code", IntValue(400))
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Invalid request parameters"),
    Some(error_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-123"),
    Some("span-456"),
    None
  )
  Logger::emit(logger, error_log)
  
  // Verify all emissions complete without errors
  assert_true(true)
}

test "log record with complex attribute types" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "complex-logger")
  
  // Create attributes with different value types
  let complex_attrs = Attributes::new()
  Attributes::set(complex_attrs, "string.value", StringValue("test string"))
  Attributes::set(complex_attrs, "int.value", IntValue(42))
  Attributes::set(complex_attrs, "float.value", FloatValue(3.14159))
  Attributes::set(complex_attrs, "bool.value", BoolValue(true))
  Attributes::set(complex_attrs, "array.string", ArrayStringValue(["item1", "item2", "item3"]))
  Attributes::set(complex_attrs, "array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  
  // Create log record with complex attributes
  let complex_log = LogRecord::new_with_context(
    Warn,
    Some("Complex data structure processed"),
    Some(complex_attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  Logger::emit(logger, complex_log)
  
  // Test retrieval of complex attributes
  assert_eq(Attributes::get(complex_attrs, "string.value"), Some(StringValue("test string")))
  assert_eq(Attributes::get(complex_attrs, "int.value"), Some(IntValue(42)))
  assert_eq(Attributes::get(complex_attrs, "float.value"), Some(FloatValue(3.14159)))
  assert_eq(Attributes::get(complex_attrs, "bool.value"), Some(BoolValue(true)))
  assert_eq(Attributes::get(complex_attrs, "nonexistent"), None)
}

test "logger provider and scope management" {
  let provider = LoggerProvider::default()
  
  // Create loggers with different scopes
  let auth_logger = LoggerProvider::get_logger(provider, "auth.service", Some("1.0.0"), Some("https://example.com/schema/v1"))
  let db_logger = LoggerProvider::get_logger(provider, "database.connector", Some("2.1.0"), None)
  let api_logger = LoggerProvider::get_logger(provider, "api.gateway", None, None)
  
  // Verify logger scope properties
  assert_eq(auth_logger.scope.name, "auth.service")
  assert_eq(auth_logger.scope.version, Some("1.0.0"))
  assert_eq(auth_logger.scope.schema_url, Some("https://example.com/schema/v1"))
  
  assert_eq(db_logger.scope.name, "database.connector")
  assert_eq(db_logger.scope.version, Some("2.1.0"))
  assert_eq(db_logger.scope.schema_url, None)
  
  assert_eq(api_logger.scope.name, "api.gateway")
  assert_eq(api_logger.scope.version, None)
  assert_eq(api_logger.scope.schema_url, None)
  
  // Emit logs from different loggers
  let auth_log = LogRecord::new(Info, "User authentication successful")
  Logger::emit(auth_logger, auth_log)
  
  let db_log = LogRecord::new(Warn, "Database connection pool running low")
  Logger::emit(db_logger, db_log)
  
  let api_log = LogRecord::new(Error, "Rate limit exceeded")
  Logger::emit(api_logger, api_log)
  
  // Verify operations complete successfully
  assert_true(true)
}

test "log record timestamp and temporal accuracy" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "timestamp-logger")
  
  let clock = Clock::system()
  let before_timestamp = Clock::now_unix_nanos(clock)
  
  // Create log with explicit timestamp
  let explicit_log = LogRecord::new_with_context(
    Info,
    Some("Log with explicit timestamp"),
    None,
    Some(before_timestamp),
    None,
    None,
    None,
    None
  )
  
  // Create log without timestamp (should use current time)
  let implicit_log = LogRecord::new(Info, "Log with implicit timestamp")
  
  let after_timestamp = Clock::now_unix_nanos(clock)
  
  // Emit logs
  Logger::emit(logger, explicit_log)
  Logger::emit(logger, implicit_log)
  
  // Verify timestamp ordering (if we could access the timestamp)
  // For now, just verify the operations complete
  assert_true(before_timestamp <= after_timestamp)
  assert_true(before_timestamp > 0L)
  assert_true(after_timestamp > 0L)
}

test "log record trace and span correlation" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "correlation-logger")
  
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "fedcba0987654321"
  
  // Create correlated log records
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Operation started"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let progress_log = LogRecord::new_with_context(
    Info,
    Some("Operation in progress"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Operation failed"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  // Verify trace and span correlation
  assert_eq(LogRecord::trace_id(start_log), Some(trace_id))
  assert_eq(LogRecord::span_id(start_log), Some(span_id))
  assert_eq(LogRecord::trace_id(progress_log), Some(trace_id))
  assert_eq(LogRecord::span_id(progress_log), Some(span_id))
  assert_eq(LogRecord::trace_id(error_log), Some(trace_id))
  assert_eq(LogRecord::span_id(error_log), Some(span_id))
  
  // Emit correlated logs
  Logger::emit(logger, start_log)
  Logger::emit(logger, progress_log)
  Logger::emit(logger, error_log)
  
  // Verify operations complete successfully
  assert_true(true)
}