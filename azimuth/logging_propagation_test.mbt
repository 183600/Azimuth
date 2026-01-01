// Logging Propagation Test Suite for Azimuth Telemetry System
// This file contains test cases for log record context propagation

test "log record creation and basic operations" {
  let record = LogRecord::new(Info, "Test log message")
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  assert_eq(LogRecord::trace_id(record), None)
  assert_eq(LogRecord::span_id(record), None)
}

test "log record with different severity levels" {
  // Test all severity levels
  let trace_record = LogRecord::new(Trace, "Trace message")
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::body(trace_record), Some("Trace message"))
  
  let debug_record = LogRecord::new(Debug, "Debug message")
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::body(debug_record), Some("Debug message"))
  
  let info_record = LogRecord::new(Info, "Info message")
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::body(info_record), Some("Info message"))
  
  let warn_record = LogRecord::new(Warn, "Warning message")
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::body(warn_record), Some("Warning message"))
  
  let error_record = LogRecord::new(Error, "Error message")
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::body(error_record), Some("Error message"))
  
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  assert_eq(LogRecord::body(fatal_record), Some("Fatal message"))
}

test "log record with context and trace information" {
  // Create a span context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Create a context with span information
  let ctx = Context::root()
  let span_key = ContextKey::new("span-context")
  let ctx_with_span = Context::with_value(ctx, span_key, span_id)
  
  // Create log record with full context
  let timestamp = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let observed_timestamp = 1735689600000000001L
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    None,  // No attributes for this test
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_span)
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
}

test "log record with attributes" {
  // Create attributes for the log record
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "request.id", StringValue("req-abc-123"))
  Attributes::set(attrs, "http.method", StringValue("POST"))
  Attributes::set(attrs, "http.status_code", IntValue(500))
  Attributes::set(attrs, "error.code", IntValue(1001))
  Attributes::set(attrs, "retry.count", IntValue(3))
  Attributes::set(attrs, "processing.time_ms", FloatValue(1234.56))
  Attributes::set(attrs, "cache.hit", BoolValue(false))
  
  // Create log record with attributes
  let record = LogRecord::new_with_context(
    Error,
    Some("Request processing failed"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    None
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Request processing failed"))
  assert_eq(LogRecord::trace_id(record), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(LogRecord::span_id(record), Some("b7ad6b7169203331"))
}

test "logger provider and logger operations" {
  // Test logger provider
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // Verify logger has correct scope
  assert_eq(logger.scope.name, "test-logger")
  assert_eq(logger.scope.version, None)
  assert_eq(logger.scope.schema_url, None)
  
  // Create and emit log records
  let info_record = LogRecord::new(Info, "Application started")
  Logger::emit(logger, info_record)
  
  let error_record = LogRecord::new(Error, "Database connection failed")
  Logger::emit(logger, error_record)
  
  // Create log record with full context
  let full_record = LogRecord::new_with_context(
    Warn,
    Some("High memory usage detected"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    None
  )
  Logger::emit(logger, full_record)
}

test "log record with different body types" {
  // Test with empty body
  let empty_record = LogRecord::new_with_context(
    Debug,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::body(empty_record), None)
  
  // Test with short body
  let short_record = LogRecord::new(Info, "Short")
  assert_eq(LogRecord::body(short_record), Some("Short"))
  
  // Test with long body
  let long_body = "This is a very long log message that contains detailed information about what happened in the system, including error details, stack traces, and other diagnostic information that might be useful for troubleshooting."
  let long_record = LogRecord::new(Error, long_body)
  assert_eq(LogRecord::body(long_record), Some(long_body))
  
  // Test with special characters in body
  let special_body = "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_record = LogRecord::new(Warn, special_body)
  assert_eq(LogRecord::body(special_record), Some(special_body))
  
  // Test with Unicode characters in body
  let unicode_body = "Unicode: 中文测试 🚀 emoji test ñáéíóú"
  let unicode_record = LogRecord::new(Info, unicode_body)
  assert_eq(LogRecord::body(unicode_record), Some(unicode_body))
}

test "log record propagation across service boundaries" {
  // Create a span context for trace propagation
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Create a context with span information
  let ctx = Context::root()
  let span_key = ContextKey::new("span-context")
  let ctx_with_span = Context::with_value(ctx, span_key, span_id)
  
  // Service A creates a log record
  let service_a_provider = LoggerProvider::default()
  let service_a_logger = LoggerProvider::get_logger(service_a_provider, "service-a")
  
  let service_a_record = LogRecord::new_with_context(
    Info,
    Some("Processing request in Service A"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_span)
  )
  Logger::emit(service_a_logger, service_a_record)
  
  // Service B creates a log record with same trace context
  let service_b_provider = LoggerProvider::default()
  let service_b_logger = LoggerProvider::get_logger(service_b_provider, "service-b")
  
  let service_b_record = LogRecord::new_with_context(
    Info,
    Some("Processing request in Service B"),
    None,
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some(trace_id),
    Some("b7ad6b7169203332"),  // Different span ID for Service B
    Some(ctx_with_span)
  )
  Logger::emit(service_b_logger, service_b_record)
  
  // Verify both records have the same trace ID but different span IDs
  assert_eq(LogRecord::trace_id(service_a_record), Some(trace_id))
  assert_eq(LogRecord::trace_id(service_b_record), Some(trace_id))
  assert_eq(LogRecord::span_id(service_a_record), Some(span_id))
  assert_eq(LogRecord::span_id(service_b_record), Some("b7ad6b7169203332"))
}

test "log record with structured data" {
  // Create attributes with structured data
  let attrs = Attributes::new()
  Attributes::set(attrs, "event.name", StringValue("user.login"))
  Attributes::set(attrs, "user.id", StringValue("user-123"))
  Attributes::set(attrs, "user.email", StringValue("user@example.com"))
  Attributes::set(attrs, "ip.address", StringValue("192.168.1.100"))
  Attributes::set(attrs, "user.agent", StringValue("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"))
  Attributes::set(attrs, "login.success", BoolValue(true))
  Attributes::set(attrs, "login.duration_ms", FloatValue(125.5))
  Attributes::set(attrs, "session.id", StringValue("sess-abc-123"))
  Attributes::set(attrs, "permissions", ArrayStringValue(["read", "write", "admin"]))
  
  // Create log record with structured data
  let record = LogRecord::new_with_context(
    Info,
    Some("User login successful"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    None
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("User login successful"))
  assert_eq(LogRecord::trace_id(record), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(LogRecord::span_id(record), Some("b7ad6b7169203331"))
}