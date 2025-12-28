// LogRecord Complex Scenarios Test Suite for Azimuth Telemetry System
// Tests advanced logging scenarios including complex attributes, context propagation, and severity levels

test "log record with all severity levels" {
  // Test all severity levels
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
  
  // Verify messages
  assert_eq(LogRecord::body(trace_record), Some("Trace level message"))
  assert_eq(LogRecord::body(debug_record), Some("Debug level message"))
  assert_eq(LogRecord::body(info_record), Some("Info level message"))
  assert_eq(LogRecord::body(warn_record), Some("Warning level message"))
  assert_eq(LogRecord::body(error_record), Some("Error level message"))
  assert_eq(LogRecord::body(fatal_record), Some("Fatal level message"))
}

test "log record with complete context and attributes" {
  // Create attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("user-123"))
  Attributes::set(attrs, "request.id", StringValue("req-456"))
  Attributes::set(attrs, "response.time", IntValue(250))
  
  // Create context
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_value = Context::with_value(ctx, key, "corr-789")
  
  // Create timestamps
  let timestamp = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let observed_timestamp = 1735689600000000001L
  
  // Create comprehensive log record
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some("trace-123"),
    Some("span-456"),
    Some(ctx_with_value)
  )
  
  // Verify all fields
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::timestamp(record), Some(timestamp))
  assert_eq(LogRecord::observed_timestamp(record), Some(observed_timestamp))
  assert_eq(LogRecord::trace_id(record), Some("trace-123"))
  assert_eq(LogRecord::span_id(record), Some("span-456"))
}

test "log record with empty and optional fields" {
  // Test with minimal fields
  let minimal_record = LogRecord::new(Info, "Minimal log")
  assert_eq(LogRecord::severity_number(minimal_record), Info)
  assert_eq(LogRecord::body(minimal_record), Some("Minimal log"))
  assert_eq(LogRecord::timestamp(minimal_record), None)
  assert_eq(LogRecord::trace_id(minimal_record), None)
  assert_eq(LogRecord::span_id(minimal_record), None)
  
  // Test with None body
  let no_body_record = LogRecord::new_with_context(
    Warn,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::severity_number(no_body_record), Warn)
  assert_eq(LogRecord::body(no_body_record), None)
  
  // Test with empty attributes
  let empty_attrs = Attributes::new()
  let empty_attrs_record = LogRecord::new_with_context(
    Debug,
    Some("Debug message"),
    Some(empty_attrs),
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::severity_number(empty_attrs_record), Debug)
  assert_eq(LogRecord::body(empty_attrs_record), Some("Debug message"))
}

test "log record emission with different loggers" {
  let provider = LoggerProvider::default()
  
  // Create loggers with different names
  let http_logger = LoggerProvider::get_logger(provider, "http-logger")
  let db_logger = LoggerProvider::get_logger(provider, "db-logger")
  let auth_logger = LoggerProvider::get_logger(provider, "auth-logger")
  
  // Create log records
  let http_record = LogRecord::new(Info, "HTTP request processed")
  let db_record = LogRecord::new(Error, "Database query failed")
  let auth_record = LogRecord::new(Warn, "Authentication attempt failed")
  
  // Emit records
  Logger::emit(http_logger, http_record)
  Logger::emit(db_logger, db_record)
  Logger::emit(auth_logger, auth_record)
  
  // Verify logger scopes
  assert_eq(http_logger.scope.name, "http-logger")
  assert_eq(db_logger.scope.name, "db-logger")
  assert_eq(auth_logger.scope.name, "auth-logger")
}

test "log record with special characters and unicode" {
  // Test log messages with special characters
  let special_chars_record = LogRecord::new(Info, "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?")
  assert_eq(LogRecord::body(special_chars_record), Some("Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"))
  
  // Test log messages with newlines and tabs
  let whitespace_record = LogRecord::new(Info, "Line 1\nLine 2\tTabbed content")
  assert_eq(LogRecord::body(whitespace_record), Some("Line 1\nLine 2\tTabbed content"))
  
  // Test log messages with unicode
  let unicode_record = LogRecord::new(Info, "Unicode: ñáéíóú 中文 العربية русский")
  assert_eq(LogRecord::body(unicode_record), Some("Unicode: ñáéíóú 中文 العربية русский"))
  
  // Test log messages with emojis
  let emoji_record = LogRecord::new(Info, "Emojis: 🚀 🔥 💯 ✅ ❌")
  assert_eq(LogRecord::body(emoji_record), Some("Emojis: 🚀 🔥 💯 ✅ ❌"))
}

test "log record with large content" {
  // Create large log message
  let large_content = "This is a very large log message that contains a lot of text. ".repeat(100)
  let large_record = LogRecord::new(Error, large_content)
  
  assert_eq(LogRecord::severity_number(large_record), Error)
  assert_eq(LogRecord::body(large_record), Some(large_content))
  
  // Create large attributes
  let large_attrs = Attributes::new()
  Attributes::set(large_attrs, "large.data", StringValue("x".repeat(1000)))
  
  let large_attrs_record = LogRecord::new_with_context(
    Warn,
    Some("Record with large attributes"),
    Some(large_attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(large_attrs_record), Warn)
  assert_eq(LogRecord::body(large_attrs_record), Some("Record with large attributes"))
}

test "log record with complex timestamp scenarios" {
  let base_timestamp = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  
  // Test with same timestamp and observed_timestamp
  let same_time_record = LogRecord::new_with_context(
    Info,
    Some("Same time"),
    None,
    Some(base_timestamp),
    Some(base_timestamp),
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::timestamp(same_time_record), Some(base_timestamp))
  assert_eq(LogRecord::observed_timestamp(same_time_record), Some(base_timestamp))
  
  // Test with different timestamps
  let later_observed = base_timestamp + 1000000L  // 1ms later
  let different_time_record = LogRecord::new_with_context(
    Info,
    Some("Different times"),
    None,
    Some(base_timestamp),
    Some(later_observed),
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::timestamp(different_time_record), Some(base_timestamp))
  assert_eq(LogRecord::observed_timestamp(different_time_record), Some(later_observed))
  
  // Test with only observed_timestamp
  let only_observed_record = LogRecord::new_with_context(
    Info,
    Some("Only observed"),
    None,
    None,
    Some(base_timestamp),
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::timestamp(only_observed_record), None)
  assert_eq(LogRecord::observed_timestamp(only_observed_record), Some(base_timestamp))
}

test "log record chain and sequence operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "chain-test")
  
  // Create a sequence of log records
  let record1 = LogRecord::new(Info, "Operation started")
  let record2 = LogRecord::new(Debug, "Processing step 1")
  let record3 = LogRecord::new(Debug, "Processing step 2")
  let record4 = LogRecord::new(Warn, "Potential issue detected")
  let record5 = LogRecord::new(Info, "Operation completed")
  
  // Create trace and span IDs for correlation
  let trace_id = "trace-operation-123"
  let span_id = "span-operation-456"
  
  // Add trace context to all records
  let traced_record1 = LogRecord::new_with_context(
    LogRecord::severity_number(record1),
    LogRecord::body(record1),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let traced_record2 = LogRecord::new_with_context(
    LogRecord::severity_number(record2),
    LogRecord::body(record2),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let traced_record3 = LogRecord::new_with_context(
    LogRecord::severity_number(record3),
    LogRecord::body(record3),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let traced_record4 = LogRecord::new_with_context(
    LogRecord::severity_number(record4),
    LogRecord::body(record4),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let traced_record5 = LogRecord::new_with_context(
    LogRecord::severity_number(record5),
    LogRecord::body(record5),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  // Emit all records
  Logger::emit(logger, traced_record1)
  Logger::emit(logger, traced_record2)
  Logger::emit(logger, traced_record3)
  Logger::emit(logger, traced_record4)
  Logger::emit(logger, traced_record5)
  
  // Verify all records have the same trace context
  assert_eq(LogRecord::trace_id(traced_record1), Some(trace_id))
  assert_eq(LogRecord::trace_id(traced_record2), Some(trace_id))
  assert_eq(LogRecord::trace_id(traced_record3), Some(trace_id))
  assert_eq(LogRecord::trace_id(traced_record4), Some(trace_id))
  assert_eq(LogRecord::trace_id(traced_record5), Some(trace_id))
  
  assert_eq(LogRecord::span_id(traced_record1), Some(span_id))
  assert_eq(LogRecord::span_id(traced_record2), Some(span_id))
  assert_eq(LogRecord::span_id(traced_record3), Some(span_id))
  assert_eq(LogRecord::span_id(traced_record4), Some(span_id))
  assert_eq(LogRecord::span_id(traced_record5), Some(span_id))
}