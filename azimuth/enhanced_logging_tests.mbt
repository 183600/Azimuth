// Azimuth Telemetry System - Enhanced Logging Tests
// Comprehensive test suite for logging functionality

test "log_record_creation_and_basic_properties" {
  // Test basic log record creation
  let log_record = LogRecord::new(Info, "Test log message")
  
  // Test basic properties
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("Test log message"))
  assert_eq(LogRecord::trace_id(log_record), None)
  assert_eq(LogRecord::span_id(log_record), None)
}

test "log_record_with_different_severity_levels" {
  // Test all severity levels
  let trace_log = LogRecord::new(Trace, "Trace message")
  let debug_log = LogRecord::new(Debug, "Debug message")
  let info_log = LogRecord::new(Info, "Info message")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error message")
  let fatal_log = LogRecord::new(Fatal, "Fatal message")
  
  // Test severity levels
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // Test bodies
  assert_eq(LogRecord::body(trace_log), Some("Trace message"))
  assert_eq(LogRecord::body(debug_log), Some("Debug message"))
  assert_eq(LogRecord::body(info_log), Some("Info message"))
  assert_eq(LogRecord::body(warn_log), Some("Warning message"))
  assert_eq(LogRecord::body(error_log), Some("Error message"))
  assert_eq(LogRecord::body(fatal_log), Some("Fatal message"))
}

test "log_record_with_full_context" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000001000L
  
  let attributes = Attributes::new()
  let context = Context::root()
  
  // Create log record with full context
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Critical error occurred"),
    Some(attributes),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(context)
  )
  
  // Test all properties
  assert_eq(LogRecord::severity_number(log_record), Error)
  assert_eq(LogRecord::body(log_record), Some("Critical error occurred"))
  assert_eq(LogRecord::trace_id(log_record), Some(trace_id))
  assert_eq(LogRecord::span_id(log_record), Some(span_id))
}

test "logger_creation_and_operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // Test logger scope
  let scope = logger.scope
  assert_eq(scope.name, "test-logger")
  assert_eq(scope.version, None)
  assert_eq(scope.schema_url, None)
  
  // Test log emission
  let info_log = LogRecord::new(Info, "Info message")
  let error_log = LogRecord::new(Error, "Error message")
  
  Logger::emit(logger, info_log)
  Logger::emit(logger, error_log)
  
  // All operations should complete without errors
  @assertion.assert_true(true)
}

test "logger_with_version_and_schema" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "service-logger", Some("1.2.3"))
  
  // Test logger scope with version
  let scope = logger.scope
  assert_eq(scope.name, "service-logger")
  assert_eq(scope.version, Some("1.2.3"))
  assert_eq(scope.schema_url, None)
  
  // Test log emission with different severities
  let trace_log = LogRecord::new(Trace, "Trace level message")
  let debug_log = LogRecord::new(Debug, "Debug level message")
  let warn_log = LogRecord::new(Warn, "Warning level message")
  
  Logger::emit(logger, trace_log)
  Logger::emit(logger, debug_log)
  Logger::emit(logger, warn_log)
  
  // All operations should complete without errors
  @assertion.assert_true(true)
}

test "noop_logger_operations" {
  let provider = LoggerProvider::noop()
  let logger = LoggerProvider::get_logger(provider, "noop-logger")
  
  // Test noop logger scope
  let scope = logger.scope
  assert_eq(scope.name, "noop-logger")
  
  // Test log emission with noop logger
  let log_record = LogRecord::new(Error, "This should be ignored")
  Logger::emit(logger, log_record)
  
  // All operations should complete without errors
  @assertion.assert_true(true)
}

test "log_record_with_attributes" {
  let attributes = Attributes::new()
  Attributes::set(attributes, "service.name", StringValue("test-service"))
  Attributes::set(attributes, "operation.type", StringValue("http.request"))
  Attributes::set(attributes, "status.code", IntValue(500))
  Attributes::set(attributes, "error.message", StringValue("Internal server error"))
  
  // Create log record with attributes
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Request failed"),
    Some(attributes),
    None,
    None,
    None,
    None,
    None
  )
  
  // Test log record properties
  assert_eq(LogRecord::severity_number(log_record), Error)
  assert_eq(LogRecord::body(log_record), Some("Request failed"))
}

test "log_record_timestamp_operations" {
  let timestamp1 = 1735689600000000000L
  let timestamp2 = 1735689600000001000L
  let timestamp3 = 1735689600000002000L
  
  // Create log records with different timestamps
  let log1 = LogRecord::new_with_context(
    Info,
    Some("Message 1"),
    None,
    Some(timestamp1),
    None,
    None,
    None,
    None
  )
  
  let log2 = LogRecord::new_with_context(
    Warn,
    Some("Message 2"),
    None,
    Some(timestamp2),
    Some(timestamp2 + 1000L),
    None,
    None,
    None
  )
  
  let log3 = LogRecord::new_with_context(
    Error,
    Some("Message 3"),
    None,
    Some(timestamp3),
    Some(timestamp3 + 1000L),
    Some("trace123"),
    Some("span456"),
    None
  )
  
  // Test log record properties
  assert_eq(LogRecord::severity_number(log1), Info)
  assert_eq(LogRecord::severity_number(log2), Warn)
  assert_eq(LogRecord::severity_number(log3), Error)
  
  assert_eq(LogRecord::body(log1), Some("Message 1"))
  assert_eq(LogRecord::body(log2), Some("Message 2"))
  assert_eq(LogRecord::body(log3), Some("Message 3"))
  
  assert_eq(LogRecord::trace_id(log1), None)
  assert_eq(LogRecord::trace_id(log2), None)
  assert_eq(LogRecord::trace_id(log3), Some("trace123"))
}

test "multiple_logger_operations" {
  let provider = LoggerProvider::default()
  
  // Create multiple loggers
  let logger1 = LoggerProvider::get_logger(provider, "auth-service")
  let logger2 = LoggerProvider::get_logger(provider, "db-service", Some("2.1.0"))
  let logger3 = LoggerProvider::get_logger(provider, "cache-service")
  
  // Create different log records
  let auth_log = LogRecord::new(Info, "User authenticated successfully")
  let db_log = LogRecord::new(Error, "Database connection failed")
  let cache_log = LogRecord::new(Warn, "Cache miss for key")
  
  // Emit logs from different loggers
  Logger::emit(logger1, auth_log)
  Logger::emit(logger2, db_log)
  Logger::emit(logger3, cache_log)
  
  // Test logger scopes
  assert_eq(logger1.scope.name, "auth-service")
  assert_eq(logger2.scope.name, "db-service")
  assert_eq(logger2.scope.version, Some("2.1.0"))
  assert_eq(logger3.scope.name, "cache-service")
  
  // All operations should complete without errors
  @assertion.assert_true(true)
}