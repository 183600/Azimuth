// Log Record Comprehensive Test Suite for Azimuth Telemetry System
// Testing complete log record functionality and operations

test "log record basic creation" {
  let record = LogRecord::new(Info, "Test log message")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  assert_eq(record.attributes, None)
  assert_eq(record.timestamp, None)
  assert_eq(record.observed_timestamp, None)
  assert_eq(record.trace_id, None)
  assert_eq(record.span_id, None)
  assert_eq(record.context, None)
}

test "log record severity levels" {
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

test "log record with empty body" {
  let record = LogRecord::new(Info, "")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some(""))
}

test "log record with comprehensive context" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "request.id", StringValue("req-67890"))
  
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000001000L
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(record.attributes, Some(attrs))
  assert_eq(record.timestamp, Some(timestamp))
  assert_eq(record.observed_timestamp, Some(observed_timestamp))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
}

test "log record with context" {
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_value = Context::with_value(ctx, key, "corr-12345")
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Processing warning"),
    None,
    None,
    None,
    None,
    None,
    Some(ctx_with_value)
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::body(record), Some("Processing warning"))
  assert_eq(record.context, Some(ctx_with_value))
}

test "log record all fields populated" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("payment-service"))
  Attributes::set(attrs, "operation.name", StringValue("process-payment"))
  
  let ctx = Context::root()
  let key = ContextKey::new("request.id")
  let ctx_with_value = Context::with_value(ctx, key, "req-abc123")
  
  let record = LogRecord::new_with_context(
    Info,
    Some("Payment processed successfully"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace123456789012345678901234567890"),
    Some("span1234567890123456"),
    Some(ctx_with_value)
  )
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Payment processed successfully"))
  assert_eq(record.attributes, Some(attrs))
  assert_eq(record.timestamp, Some(1735689600000000000L))
  assert_eq(record.observed_timestamp, Some(1735689600000001000L))
  assert_eq(LogRecord::trace_id(record), Some("trace123456789012345678901234567890"))
  assert_eq(LogRecord::span_id(record), Some("span1234567890123456"))
  assert_eq(record.context, Some(ctx_with_value))
}

test "logger provider operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  assert_eq(logger.scope.name, "test-logger")
  assert_eq(logger.scope.version, None)
  assert_eq(logger.scope.schema_url, None)
  
  // Test emitting a log record
  let record = LogRecord::new(Info, "Test log emission")
  Logger::emit(logger, record)
  
  assert_true(true)
}

test "logger provider with version and schema" {
  let provider = LoggerProvider::noop()
  let logger = LoggerProvider::get_logger(provider, "service-logger", Some("1.0.0"), Some("https://example.com/schema"))
  
  assert_eq(logger.scope.name, "service-logger")
  assert_eq(logger.scope.version, Some("1.0.0"))
  assert_eq(logger.scope.schema_url, Some("https://example.com/schema"))
  
  let record = LogRecord::new(Error, "Service error occurred")
  Logger::emit(logger, record)
  
  assert_true(true)
}

test "log record with complex attributes" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.value", StringValue("complex string"))
  Attributes::set(attrs, "int.value", IntValue(42))
  Attributes::set(attrs, "float.value", FloatValue(3.14159))
  Attributes::set(attrs, "bool.value", BoolValue(true))
  Attributes::set(attrs, "array.string", ArrayStringValue(["error", "warning", "info"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([200, 404, 500]))
  
  let record = LogRecord::new_with_context(
    Debug,
    Some("Complex attribute test"),
    Some(attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(record), Debug)
  assert_eq(LogRecord::body(record), Some("Complex attribute test"))
  assert_eq(record.attributes, Some(attrs))
}

test "log record timestamp scenarios" {
  let clock = Clock::system()
  let current_time = Clock::now_unix_nanos(clock)
  
  // Test with only timestamp
  let record1 = LogRecord::new_with_context(
    Info,
    Some("Timestamp only"),
    None,
    Some(current_time),
    None,
    None,
    None,
    None
  )
  
  assert_eq(record1.timestamp, Some(current_time))
  assert_eq(record1.observed_timestamp, None)
  
  // Test with only observed timestamp
  let record2 = LogRecord::new_with_context(
    Info,
    Some("Observed timestamp only"),
    None,
    None,
    Some(current_time + 1000L),
    None,
    None,
    None
  )
  
  assert_eq(record2.timestamp, None)
  assert_eq(record2.observed_timestamp, Some(current_time + 1000L))
  
  // Test with both timestamps
  let record3 = LogRecord::new_with_context(
    Info,
    Some("Both timestamps"),
    None,
    Some(current_time),
    Some(current_time + 1000L),
    None,
    None,
    None
  )
  
  assert_eq(record3.timestamp, Some(current_time))
  assert_eq(record3.observed_timestamp, Some(current_time + 1000L))
}

test "log record trace correlation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Trace correlation test"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
  
  // Test with empty trace/span IDs
  let empty_record = LogRecord::new_with_context(
    Warn,
    Some("Empty trace/span"),
    None,
    None,
    None,
    Some(""),
    Some(""),
    None
  )
  
  assert_eq(LogRecord::trace_id(empty_record), Some(""))
  assert_eq(LogRecord::span_id(empty_record), Some(""))
}

test "log record multiple logger operations" {
  let provider = LoggerProvider::default()
  
  let logger1 = LoggerProvider::get_logger(provider, "logger1")
  let logger2 = LoggerProvider::get_logger(provider, "logger2", Some("2.0.0"))
  let logger3 = LoggerProvider::get_logger(provider, "logger3", Some("3.0.0"), Some("https://example.com"))
  
  // Create different log records for each logger
  let record1 = LogRecord::new(Info, "Logger 1 message")
  let record2 = LogRecord::new(Warn, "Logger 2 message")
  let record3 = LogRecord::new(Error, "Logger 3 message")
  
  Logger::emit(logger1, record1)
  Logger::emit(logger2, record2)
  Logger::emit(logger3, record3)
  
  assert_eq(logger1.scope.name, "logger1")
  assert_eq(logger2.scope.name, "logger2")
  assert_eq(logger2.scope.version, Some("2.0.0"))
  assert_eq(logger3.scope.name, "logger3")
  assert_eq(logger3.scope.version, Some("3.0.0"))
  assert_eq(logger3.scope.schema_url, Some("https://example.com"))
}