// High-Quality Test Suite for LogRecord and Context Association
// This file contains comprehensive test cases for log record and context association functionality

test "log record basic creation" {
  let record = LogRecord::new(Info, "Test log message")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  assert_eq(LogRecord::trace_id(record), None)
  assert_eq(LogRecord::span_id(record), None)
}

test "log record with different severity levels" {
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

test "log record with full context" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000001000L
  
  let ctx = Context::root()
  let attrs = Attributes::new()
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(ctx)
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
}

test "log record with trace and span correlation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  // Create log record with trace and span ID
  let record = LogRecord::new_with_context(
    Info,
    Some("Processing user request"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
  
  // Create another log record with same trace but different span
  let child_span_id = "c7ad6b7169203331"
  let child_record = LogRecord::new_with_context(
    Info,
    Some("Validating user input"),
    None,
    None,
    None,
    Some(trace_id),
    Some(child_span_id),
    None
  )
  
  assert_eq(LogRecord::trace_id(child_record), Some(trace_id))
  assert_eq(LogRecord::span_id(child_record), Some(child_span_id))
}

test "log record with attributes" {
  let attrs = Attributes::new()
  
  // Create log record with attributes
  let record = LogRecord::new_with_context(
    Warn,
    Some("User authentication failed"),
    Some(attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::body(record), Some("User authentication failed"))
}

test "log record with timestamps" {
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000001000L
  
  // Create log record with timestamps
  let record = LogRecord::new_with_context(
    Info,
    Some("Operation completed"),
    None,
    Some(timestamp),
    Some(observed_timestamp),
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Operation completed"))
}

test "log record with context data" {
  let ctx = Context::root()
  let key = ContextKey::new("user.id")
  let ctx_with_user = Context::with_value(ctx, key, "user123")
  
  // Create log record with context
  let record = LogRecord::new_with_context(
    Info,
    Some("User operation performed"),
    None,
    None,
    None,
    None,
    None,
    Some(ctx_with_user)
  )
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("User operation performed"))
}

test "logger creation and emission" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  assert_eq(logger.scope.name, "test-logger")
  assert_eq(logger.scope.version, None)
  assert_eq(logger.scope.schema_url, None)
  
  // Create log record
  let record = LogRecord::new(Info, "Test log message")
  
  // Emit log record
  Logger::emit(logger, record)
  
  assert_true(true) // If we reach here, emission was successful
}

test "complex log record scenario" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000001000L
  
  let ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let ctx_with_user = Context::with_value(ctx, user_key, "user123")
  
  let attrs = Attributes::new()
  
  // Create comprehensive log record
  let record = LogRecord::new_with_context(
    Error,
    Some("Payment processing failed"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_user)
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Payment processing failed"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
}

test "log record sequence for operation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let root_span_id = "b7ad6b7169203331"
  
  // Start of operation
  let start_record = LogRecord::new_with_context(
    Info,
    Some("Starting user authentication"),
    None,
    None,
    None,
    Some(trace_id),
    Some(root_span_id),
    None
  )
  
  // Middle of operation
  let middle_span_id = "c7ad6b7169203331"
  let middle_record = LogRecord::new_with_context(
    Info,
    Some("Validating user credentials"),
    None,
    None,
    None,
    Some(trace_id),
    Some(middle_span_id),
    None
  )
  
  // End of operation
  let end_record = LogRecord::new_with_context(
    Info,
    Some("User authentication completed"),
    None,
    None,
    None,
    Some(trace_id),
    Some(root_span_id),
    None
  )
  
  // Verify all records have the same trace ID
  assert_eq(LogRecord::trace_id(start_record), Some(trace_id))
  assert_eq(LogRecord::trace_id(middle_record), Some(trace_id))
  assert_eq(LogRecord::trace_id(end_record), Some(trace_id))
  
  // Verify span IDs are different where expected
  assert_eq(LogRecord::span_id(start_record), Some(root_span_id))
  assert_eq(LogRecord::span_id(middle_record), Some(middle_span_id))
  assert_eq(LogRecord::span_id(end_record), Some(root_span_id))
}

test "log record with different loggers" {
  let provider = LoggerProvider::default()
  
  // Create loggers for different components
  let auth_logger = LoggerProvider::get_logger(provider, "authentication-service")
  let db_logger = LoggerProvider::get_logger(provider, "database-service")
  let api_logger = LoggerProvider::get_logger(provider, "api-gateway")
  
  // Verify logger names
  assert_eq(auth_logger.scope.name, "authentication-service")
  assert_eq(db_logger.scope.name, "database-service")
  assert_eq(api_logger.scope.name, "api-gateway")
  
  // Create log records for each logger
  let auth_record = LogRecord::new(Info, "User login attempt")
  let db_record = LogRecord::new(Warn, "Database connection slow")
  let api_record = LogRecord::new(Error, "API request failed")
  
  // Emit records
  Logger::emit(auth_logger, auth_record)
  Logger::emit(db_logger, db_record)
  Logger::emit(api_logger, api_record)
  
  assert_true(true) // If we reach here, all emissions were successful
}

test "log record with error handling" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  // Create error log record with context
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Unhandled exception in user service"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  // Create fatal log record with context
  let fatal_record = LogRecord::new_with_context(
    Fatal,
    Some("System shutdown due to critical error"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  assert_eq(LogRecord::trace_id(error_record), Some(trace_id))
  assert_eq(LogRecord::trace_id(fatal_record), Some(trace_id))
}