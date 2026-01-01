// Log Record Context Correlation Tests for Azimuth Telemetry System
// This file contains comprehensive tests for log record and context correlation

test "log record creation with basic properties" {
  let record = LogRecord::new(Info, "Test log message")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
}

test "log record with all severity levels" {
  // Test all severity levels
  let trace_record = LogRecord::new(Trace, "Trace message")
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  
  let debug_record = LogRecord::new(Debug, "Debug message")
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  
  let info_record = LogRecord::new(Info, "Info message")
  assert_eq(LogRecord::severity_number(info_record), Info)
  
  let warn_record = LogRecord::new(Warn, "Warning message")
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  
  let error_record = LogRecord::new(Error, "Error message")
  assert_eq(LogRecord::severity_number(error_record), Error)
  
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
}

test "log record creation with comprehensive context" {
  let trace_id = Some("0af7651916cd43dd8448eb211c80319c")
  let span_id = Some("b7ad6b7169203331")
  let timestamp = Some(1735689600000000000L)
  let observed_timestamp = Some(1735689600000001000L)
  
  let attributes = Attributes::new()
  let ctx = Context::root()
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Comprehensive error message"),
    Some(attributes),
    timestamp,
    observed_timestamp,
    trace_id,
    span_id,
    Some(ctx)
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Comprehensive error message"))
  assert_eq(LogRecord::trace_id(record), trace_id)
  assert_eq(LogRecord::span_id(record), span_id)
}

test "log record with span context correlation" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "key1=value1")
  let trace_id = Some("trace123")
  let span_id = Some("span456")
  
  let record = LogRecord::new_with_context(
    Info,
    Some("Operation completed"),
    None,
    None,
    None,
    trace_id,
    span_id,
    None
  )
  
  assert_eq(LogRecord::trace_id(record), trace_id)
  assert_eq(LogRecord::span_id(record), span_id)
}

test "log record with context values" {
  let ctx = Context::root()
  let key1 = ContextKey::new("user.id")
  let ctx_with_user = Context::with_value(ctx, key1, "user123")
  
  let key2 = ContextKey::new("request.id")
  let ctx_full = Context::with_value(ctx_with_user, key2, "req456")
  
  let record = LogRecord::new_with_context(
    Info,
    Some("User request processed"),
    None,
    None,
    None,
    None,
    None,
    Some(ctx_full)
  )
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("User request processed"))
}

test "log record with attributes" {
  let attributes = Attributes::new()
  Attributes::set(attributes, "http.method", StringValue("GET"))
  Attributes::set(attributes, "http.status_code", IntValue(200))
  Attributes::set(attributes, "user.id", StringValue("user123"))
  
  let record = LogRecord::new_with_context(
    Info,
    Some("HTTP request processed"),
    Some(attributes),
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("HTTP request processed"))
}

test "log record with timestamps" {
  let timestamp = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let observed_timestamp = 1735689600000001000L  // 1 microsecond later
  
  let record = LogRecord::new_with_context(
    Info,
    Some("Timestamped message"),
    None,
    Some(timestamp),
    Some(observed_timestamp),
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Timestamped message"))
}

test "log record with special characters" {
  let special_message = "Special chars: !@#$%^&*() and Unicode: 中文测试 🚀"
  
  let record = LogRecord::new_with_context(
    Warn,
    Some(special_message),
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::body(record), Some(special_message))
}

test "log record with empty and null values" {
  // Test empty body
  let empty_record = LogRecord::new_with_context(
    Info,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(empty_record), Info)
  assert_eq(LogRecord::body(empty_record), None)
  
  // Test empty string body
  let empty_string_record = LogRecord::new_with_context(
    Info,
    Some(""),
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(empty_string_record), Info)
  assert_eq(LogRecord::body(empty_string_record), Some(""))
}

test "logger provider and logger functionality" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // Test logger scope
  let scope = logger.scope
  assert_eq(scope.name, "test-logger")
  assert_eq(scope.version, None)
  assert_eq(scope.schema_url, None)
  
  // Create a log record
  let record = LogRecord::new(Info, "Test message from logger")
  
  // Emit the log record
  Logger::emit(logger, record)
  
  // Test that emission doesn't crash
  assert_true(true)
}

test "log record with complex attributes" {
  let attributes = Attributes::new()
  Attributes::set(attributes, "string.array", ArrayStringValue(["item1", "item2", "item3"]))
  Attributes::set(attributes, "int.array", ArrayIntValue([1, 2, 3, 4, 5]))
  Attributes::set(attributes, "float.value", FloatValue(3.14159))
  Attributes::set(attributes, "bool.value", BoolValue(true))
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Complex attribute test"),
    Some(attributes),
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Complex attribute test"))
}

test "log record correlation across operations" {
  // Simulate a series of operations with correlated logs
  let trace_id = Some("operation-trace-123")
  let span_id = Some("operation-span-456")
  
  // Start operation
  let start_record = LogRecord::new_with_context(
    Info,
    Some("Operation started"),
    None,
    None,
    None,
    trace_id,
    span_id,
    None
  )
  
  // Operation progress
  let progress_record = LogRecord::new_with_context(
    Info,
    Some("Operation in progress"),
    None,
    None,
    None,
    trace_id,
    span_id,
    None
  )
  
  // Operation completion
  let complete_record = LogRecord::new_with_context(
    Info,
    Some("Operation completed"),
    None,
    None,
    None,
    trace_id,
    span_id,
    None
  )
  
  // All records should have the same trace_id and span_id
  assert_eq(LogRecord::trace_id(start_record), trace_id)
  assert_eq(LogRecord::span_id(start_record), span_id)
  assert_eq(LogRecord::trace_id(progress_record), trace_id)
  assert_eq(LogRecord::span_id(progress_record), span_id)
  assert_eq(LogRecord::trace_id(complete_record), trace_id)
  assert_eq(LogRecord::span_id(complete_record), span_id)
}