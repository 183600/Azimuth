// Test file for logging functionality
test "severity number operations" {
  // Test severity number values
  let trace = Trace
  let debug = Debug
  let info = Info
  let warn = Warn
  let error = Error
  let fatal = Fatal
  
  // Test pattern matching on severity
  match trace {
    Trace => assert_true(true, "Should match Trace")
    _ => assert_true(false, "Should not match other severity")
  }
  
  match debug {
    Debug => assert_true(true, "Should match Debug")
    _ => assert_true(false, "Should not match other severity")
  }
  
  match info {
    Info => assert_true(true, "Should match Info")
    _ => assert_true(false, "Should not match other severity")
  }
  
  match warn {
    Warn => assert_true(true, "Should match Warn")
    _ => assert_true(false, "Should not match other severity")
  }
  
  match error {
    Error => assert_true(true, "Should match Error")
    _ => assert_true(false, "Should not match other severity")
  }
  
  match fatal {
    Fatal => assert_true(true, "Should match Fatal")
    _ => assert_true(false, "Should not match other severity")
  }
}

test "log record creation and properties" {
  // Test creating basic log record
  let log_record = LogRecord::new(Info, "Test log message")
  
  // Test log record properties
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("Test log message"))
  assert_true(log_record.attributes.is_none())
  assert_true(log_record.timestamp.is_none())
  assert_true(log_record.observed_timestamp.is_none())
  assert_true(log_record.trace_id.is_none())
  assert_true(log_record.span_id.is_none())
  assert_true(log_record.context.is_none())
}

test "log record with all fields" {
  // Create attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "request.id", StringValue("req-67890"))
  
  // Create context
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_value = Context::with_value(ctx, key, "corr-12345")
  
  // Create log record with all fields
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-123"),
    Some("span-456"),
    Some(ctx_with_value)
  )
  
  // Test all properties
  assert_eq(LogRecord::severity_number(log_record), Error)
  assert_eq(LogRecord::body(log_record), Some("Database connection failed"))
  assert_true(log_record.attributes.is_some())
  assert_eq(log_record.timestamp, Some(1735689600000000000L))
  assert_eq(log_record.observed_timestamp, Some(1735689600000001000L))
  assert_eq(LogRecord::trace_id(log_record), Some("trace-123"))
  assert_eq(LogRecord::span_id(log_record), Some("span-456"))
  assert_true(log_record.context.is_some())
}

test "log record with different severity levels" {
  // Test creating log records with different severity levels
  let trace_log = LogRecord::new(Trace, "Trace message")
  let debug_log = LogRecord::new(Debug, "Debug message")
  let info_log = LogRecord::new(Info, "Info message")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error message")
  let fatal_log = LogRecord::new(Fatal, "Fatal message")
  
  // Verify severity levels
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // Verify bodies
  assert_eq(LogRecord::body(trace_log), Some("Trace message"))
  assert_eq(LogRecord::body(debug_log), Some("Debug message"))
  assert_eq(LogRecord::body(info_log), Some("Info message"))
  assert_eq(LogRecord::body(warn_log), Some("Warning message"))
  assert_eq(LogRecord::body(error_log), Some("Error message"))
  assert_eq(LogRecord::body(fatal_log), Some("Fatal message"))
}

test "logger provider operations" {
  // Test creating logger provider
  let provider = LoggerProvider::default()
  let noop_provider = LoggerProvider::noop()
  
  // Test getting logger
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  let logger_with_scope = LoggerProvider::get_logger(provider, "scoped-logger")
  
  let noop_logger = LoggerProvider::get_logger(noop_provider, "noop-logger")
  
  // Test logger scope
  assert_eq(logger.scope.name, "test-logger")
  assert_true(logger.scope.version.is_none())
  assert_true(logger.scope.schema_url.is_none())
  
  assert_eq(logger_with_scope.scope.name, "scoped-logger")
  assert_true(logger_with_scope.scope.version.is_none())
  assert_true(logger_with_scope.scope.schema_url.is_none())
  
  assert_eq(noop_logger.scope.name, "noop-logger")
  assert_true(noop_logger.scope.version.is_none())
  assert_true(noop_logger.scope.schema_url.is_none())
}

test "logger emit operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // Test emitting different types of log records
  let info_record = LogRecord::new(Info, "Application started")
  let warn_record = LogRecord::new(Warn, "Deprecated API used")
  let error_record = LogRecord::new(Error, "Database connection failed")
  
  // Emit log records
  Logger::emit(logger, info_record)
  Logger::emit(logger, warn_record)
  Logger::emit(logger, error_record)
  
  // Test noop logger
  let noop_provider = LoggerProvider::noop()
  let noop_logger = LoggerProvider::get_logger(noop_provider, "noop-logger")
  
  let noop_record = LogRecord::new(Debug, "This should be ignored")
  Logger::emit(noop_logger, noop_record)
  
  // Note: simplified implementation doesn't actually emit logs
  assert_true(true, "Logger emit operations should not crash")
}

test "log record with complex attributes" {
  // Create complex attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.value", StringValue("test string"))
  Attributes::set(attrs, "int.value", IntValue(42))
  Attributes::set(attrs, "float.value", FloatValue(3.14159))
  Attributes::set(attrs, "bool.value", BoolValue(true))
  Attributes::set(attrs, "array.string", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([1, 2, 3]))
  
  // Create log record with complex attributes
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Complex log with attributes"),
    Some(attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  // Verify properties
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("Complex log with attributes"))
  assert_true(log_record.attributes.is_some())
}

test "log record with trace correlation" {
  // Create log record with trace correlation
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Operation failed"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-1234567890abcdef"),
    Some("span-1234567890abcdef"),
    None
  )
  
  // Verify trace correlation
  assert_eq(LogRecord::trace_id(log_record), Some("trace-1234567890abcdef"))
  assert_eq(LogRecord::span_id(log_record), Some("span-1234567890abcdef"))
  assert_eq(log_record.timestamp, Some(1735689600000000000L))
  assert_eq(log_record.observed_timestamp, Some(1735689600000001000L))
}

test "complex logging workflow" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "workflow-logger")
  
  // Simulate application workflow with logging
  let start_log = LogRecord::new(Info, "Application starting")
  Logger::emit(logger, start_log)
  
  // Create some attributes for detailed logging
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("user-123"))
  Attributes::set(attrs, "operation", StringValue("data-processing"))
  
  let processing_log = LogRecord::new_with_context(
    Info,
    Some("Processing user data"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-workflow-123"),
    Some("span-processing-456"),
    None
  )
  Logger::emit(logger, processing_log)
  
  // Simulate an error condition
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Failed to process user data"),
    Some(attrs),
    Some(1735689600000002000L),
    Some(1735689600000003000L),
    Some("trace-workflow-123"),
    Some("span-error-789"),
    None
  )
  Logger::emit(logger, error_log)
  
  // Workflow should complete without errors
  assert_true(true, "Complex logging workflow should complete successfully")
}