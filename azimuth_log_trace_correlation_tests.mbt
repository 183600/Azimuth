// Azimuth Telemetry System - Log and Trace Correlation Tests
// This file contains comprehensive test cases for log and trace correlation

// Test 1: Basic log record creation with trace correlation
test "basic log record creation with trace correlation" {
  // Create a logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test-logger")
  
  // Create a log record with trace and span IDs
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Test log message with trace correlation"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace123456789"),
    Some("span987654321"),
    None
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Test log message with trace correlation")
    None => assert_true(false)
  }
  match LogRecord::trace_id(log_record) {
    Some(trace_id) => assert_eq(trace_id, "trace123456789")
    None => assert_true(false)
  }
  match LogRecord::span_id(log_record) {
    Some(span_id) => assert_eq(span_id, "span987654321")
    None => assert_true(false)
  }
  
  // Emit the log record
  Logger::emit(logger, log_record)
}

// Test 2: Log records with different severity levels and trace correlation
test "log records with different severity levels and trace correlation" {
  // Create a logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "severity-test-logger")
  
  // Create log records with different severity levels
  let trace_trace = LogRecord::new_with_context(
    Trace, Some("Trace message"), None, None, None, 
    Some("trace123"), Some("span123"), None
  )
  
  let debug_log = LogRecord::new_with_context(
    Debug, Some("Debug message"), None, None, None,
    Some("trace123"), Some("span123"), None
  )
  
  let info_log = LogRecord::new_with_context(
    Info, Some("Info message"), None, None, None,
    Some("trace123"), Some("span123"), None
  )
  
  let warn_log = LogRecord::new_with_context(
    Warn, Some("Warning message"), None, None, None,
    Some("trace123"), Some("span123"), None
  )
  
  let error_log = LogRecord::new_with_context(
    Error, Some("Error message"), None, None, None,
    Some("trace123"), Some("span123"), None
  )
  
  let fatal_log = LogRecord::new_with_context(
    Fatal, Some("Fatal message"), None, None, None,
    Some("trace123"), Some("span123"), None
  )
  
  // Verify severity levels
  assert_eq(LogRecord::severity_number(trace_trace), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // Verify all logs have the same trace and span IDs
  for log in [trace_trace, debug_log, info_log, warn_log, error_log, fatal_log] {
    match LogRecord::trace_id(log) {
      Some(trace_id) => assert_eq(trace_id, "trace123")
      None => assert_true(false)
    }
    match LogRecord::span_id(log) {
      Some(span_id) => assert_eq(span_id, "span123")
      None => assert_true(false)
    }
    
    // Emit each log record
    Logger::emit(logger, log)
  }
}

// Test 3: Log records with attributes and trace correlation
test "log records with attributes and trace correlation" {
  // Create a logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "attribute-test-logger")
  
  // Create attributes for the log record
  let log_attributes = Attributes {
    values: [
      ("service.name", AttributeValue::StringValue("test-service")),
      ("operation.type", AttributeValue::StringValue("database.query")),
      ("operation.duration_ms", AttributeValue::IntValue(150)),
      ("operation.success", AttributeValue::BoolValue(true)),
      ("user.id", AttributeValue::StringValue("user123"))
    ]
  }
  
  // Create a log record with attributes and trace correlation
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Database query completed successfully"),
    Some(log_attributes),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace456789"),
    Some("span123456"),
    None
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Database query completed successfully")
    None => assert_true(false)
  }
  match LogRecord::trace_id(log_record) {
    Some(trace_id) => assert_eq(trace_id, "trace456789")
    None => assert_true(false)
  }
  match LogRecord::span_id(log_record) {
    Some(span_id) => assert_eq(span_id, "span123456")
    None => assert_true(false)
  }
  
  // Emit the log record
  Logger::emit(logger, log_record)
}

// Test 4: Log records with context and trace correlation
test "log records with context and trace correlation" {
  // Create a logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "context-test-logger")
  
  // Create a context with data
  let context_key = ContextKey::new("correlation.id")
  let context = Context::with_value(Context::root(), context_key, "corr-12345")
  
  // Create a log record with context and trace correlation
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Log with context and trace correlation"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace789"),
    Some("span456"),
    Some(context)
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Log with context and trace correlation")
    None => assert_true(false)
  }
  match LogRecord::trace_id(log_record) {
    Some(trace_id) => assert_eq(trace_id, "trace789")
    None => assert_true(false)
  }
  match LogRecord::span_id(log_record) {
    Some(span_id) => assert_eq(span_id, "span456")
    None => assert_true(false)
  }
  
  // Emit the log record
  Logger::emit(logger, log_record)
}

// Test 5: Log records without trace correlation
test "log records without trace correlation" {
  // Create a logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "no-trace-logger")
  
  // Create a log record without trace and span IDs
  let log_record = LogRecord::new_with_context(
    Warn,
    Some("Log without trace correlation"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    None,
    None,
    None
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Warn)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Log without trace correlation")
    None => assert_true(false)
  }
  match LogRecord::trace_id(log_record) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match LogRecord::span_id(log_record) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Emit the log record
  Logger::emit(logger, log_record)
}

// Test 6: Log records with partial trace correlation
test "log records with partial trace correlation" {
  // Create a logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "partial-trace-logger")
  
  // Create a log record with only trace ID
  let trace_only_log = LogRecord::new_with_context(
    Info,
    Some("Log with only trace ID"),
    None,
    None,
    None,
    Some("trace123"),
    None,
    None
  )
  
  // Create a log record with only span ID
  let span_only_log = LogRecord::new_with_context(
    Info,
    Some("Log with only span ID"),
    None,
    None,
    None,
    None,
    Some("span456"),
    None
  )
  
  // Verify trace-only log
  match LogRecord::trace_id(trace_only_log) {
    Some(trace_id) => assert_eq(trace_id, "trace123")
    None => assert_true(false)
  }
  match LogRecord::span_id(trace_only_log) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Verify span-only log
  match LogRecord::trace_id(span_only_log) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match LogRecord::span_id(span_only_log) {
    Some(span_id) => assert_eq(span_id, "span456")
    None => assert_true(false)
  }
  
  // Emit both log records
  Logger::emit(logger, trace_only_log)
  Logger::emit(logger, span_only_log)
}

// Test 7: Log records with complex attributes and trace correlation
test "log records with complex attributes and trace correlation" {
  // Create a logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "complex-attr-logger")
  
  // Create complex attributes
  let complex_attributes = Attributes {
    values: [
      ("string.array", AttributeValue::ArrayStringValue(["error1", "error2", "error3"])),
      ("int.array", AttributeValue::ArrayIntValue([10, 20, 30])),
      ("float.value", AttributeValue::FloatValue(3.14159)),
      ("bool.value", AttributeValue::BoolValue(false)),
      ("nested.object", AttributeValue::StringValue("{\"key\":\"value\"}")),
      ("http.status_code", AttributeValue::IntValue(500)),
      ("http.method", AttributeValue::StringValue("POST"))
    ]
  }
  
  // Create a log record with complex attributes and trace correlation
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Request failed with multiple errors"),
    Some(complex_attributes),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("error-trace-123"),
    Some("error-span-456"),
    None
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Error)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Request failed with multiple errors")
    None => assert_true(false)
  }
  match LogRecord::trace_id(log_record) {
    Some(trace_id) => assert_eq(trace_id, "error-trace-123")
    None => assert_true(false)
  }
  match LogRecord::span_id(log_record) {
    Some(span_id) => assert_eq(span_id, "error-span-456")
    None => assert_true(false)
  }
  
  // Emit the log record
  Logger::emit(logger, log_record)
}

// Test 8: Log records with timestamps and trace correlation
test "log records with timestamps and trace correlation" {
  // Create a logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "timestamp-logger")
  
  // Create a clock for getting timestamps
  let clock = Clock::system()
  
  // Get current timestamp
  let current_timestamp = Clock::now_unix_nanos(clock)
  
  // Create a log record with timestamps and trace correlation
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Log with precise timestamps"),
    None,
    Some(current_timestamp),
    Some(current_timestamp + 1000000L), // 1ms later
    Some("timestamp-trace"),
    Some("timestamp-span"),
    None
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Log with precise timestamps")
    None => assert_true(false)
  }
  match LogRecord::trace_id(log_record) {
    Some(trace_id) => assert_eq(trace_id, "timestamp-trace")
    None => assert_true(false)
  }
  match LogRecord::span_id(log_record) {
    Some(span_id) => assert_eq(span_id, "timestamp-span")
    None => assert_true(false)
  }
  
  // Emit the log record
  Logger::emit(logger, log_record)
}

// Test 9: Log records with empty or special values and trace correlation
test "log records with empty or special values and trace correlation" {
  // Create a logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "special-values-logger")
  
  // Create a log record with empty body
  let empty_body_log = LogRecord::new_with_context(
    Info,
    None,
    None,
    None,
    None,
    Some("trace-empty-body"),
    Some("span-empty-body"),
    None
  )
  
  // Create a log record with empty trace and span IDs
  let empty_ids_log = LogRecord::new_with_context(
    Warn,
    Some("Log with empty trace and span IDs"),
    None,
    None,
    None,
    Some(""),
    Some(""),
    None
  )
  
  // Create a log record with special characters in trace and span IDs
  let special_chars_log = LogRecord::new_with_context(
    Error,
    Some("Log with special characters in IDs"),
    None,
    None,
    None,
    Some("trace/with/special/chars"),
    Some("span?with=special&symbols"),
    None
  )
  
  // Verify empty body log
  match LogRecord::body(empty_body_log) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match LogRecord::trace_id(empty_body_log) {
    Some(trace_id) => assert_eq(trace_id, "trace-empty-body")
    None => assert_true(false)
  }
  
  // Verify empty IDs log
  match LogRecord::trace_id(empty_ids_log) {
    Some(trace_id) => assert_eq(trace_id, "")
    None => assert_true(false)
  }
  match LogRecord::span_id(empty_ids_log) {
    Some(span_id) => assert_eq(span_id, "")
    None => assert_true(false)
  }
  
  // Verify special characters log
  match LogRecord::trace_id(special_chars_log) {
    Some(trace_id) => assert_eq(trace_id, "trace/with/special/chars")
    None => assert_true(false)
  }
  match LogRecord::span_id(special_chars_log) {
    Some(span_id) => assert_eq(span_id, "span?with=special&symbols")
    None => assert_true(false)
  }
  
  // Emit all log records
  Logger::emit(logger, empty_body_log)
  Logger::emit(logger, empty_ids_log)
  Logger::emit(logger, special_chars_log)
}

// Test 10: Multiple log records with trace correlation across different operations
test "multiple log records with trace correlation across different operations" {
  // Create a logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "operation-logger")
  
  // Create log records for different operations in the same trace
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Operation started"),
    None,
    Some(1735689600000000000L),
    None,
    Some("operation-trace-123"),
    Some("start-span-456"),
    None
  )
  
  let processing_log = LogRecord::new_with_context(
    Info,
    Some("Processing data"),
    None,
    Some(1735689600000000000L + 500000000L),
    None,
    Some("operation-trace-123"),
    Some("processing-span-789"),
    None
  )
  
  let database_log = LogRecord::new_with_context(
    Info,
    Some("Querying database"),
    None,
    Some(1735689600000000000L + 1000000000L),
    None,
    Some("operation-trace-123"),
    Some("database-span-012"),
    None
  )
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Database query failed"),
    None,
    Some(1735689600000000000L + 1500000000L),
    None,
    Some("operation-trace-123"),
    Some("database-span-012"),
    None
  )
  
  let retry_log = LogRecord::new_with_context(
    Warn,
    Some("Retrying database query"),
    None,
    Some(1735689600000000000L + 2000000000L),
    None,
    Some("operation-trace-123"),
    Some("retry-span-345"),
    None
  )
  
  let success_log = LogRecord::new_with_context(
    Info,
    Some("Operation completed successfully"),
    None,
    Some(1735689600000000000L + 3000000000L),
    None,
    Some("operation-trace-123"),
    Some("success-span-678"),
    None
  )
  
  // Verify all logs have the same trace ID
  let logs = [start_log, processing_log, database_log, error_log, retry_log, success_log]
  for log in logs {
    match LogRecord::trace_id(log) {
      Some(trace_id) => assert_eq(trace_id, "operation-trace-123")
      None => assert_true(false)
    }
    
    // Emit each log record
    Logger::emit(logger, log)
  }
  
  // Verify different span IDs
  assert_ne(LogRecord::span_id(start_log), LogRecord::span_id(processing_log))
  assert_ne(LogRecord::span_id(processing_log), LogRecord::span_id(database_log))
  assert_ne(LogRecord::span_id(database_log), LogRecord::span_id(retry_log))
  assert_ne(LogRecord::span_id(retry_log), LogRecord::span_id(success_log))
}