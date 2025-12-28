// Log Record Comprehensive Functionality Tests
// Tests for log record creation, severity, context, and emission

test "log record creation with severity levels" {
  // Test all severity levels
  let trace_record = LogRecord::new(Trace, "Debug trace information")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Information message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error occurred")
  let fatal_record = LogRecord::new(Fatal, "Fatal error")
  
  // Test severity levels
  assert_eq(match LogRecord::severity_number(trace_record) { Trace => true, _ => false }, true)
  assert_eq(match LogRecord::severity_number(debug_record) { Debug => true, _ => false }, true)
  assert_eq(match LogRecord::severity_number(info_record) { Info => true, _ => false }, true)
  assert_eq(match LogRecord::severity_number(warn_record) { Warn => true, _ => false }, true)
  assert_eq(match LogRecord::severity_number(error_record) { Error => true, _ => false }, true)
  assert_eq(match LogRecord::severity_number(fatal_record) { Fatal => true, _ => false }, true)
  
  // Test body content
  assert_eq(LogRecord::body(trace_record), Some("Debug trace information"))
  assert_eq(LogRecord::body(debug_record), Some("Debug message"))
  assert_eq(LogRecord::body(info_record), Some("Information message"))
  assert_eq(LogRecord::body(warn_record), Some("Warning message"))
  assert_eq(LogRecord::body(error_record), Some("Error occurred"))
  assert_eq(LogRecord::body(fatal_record), Some("Fatal error"))
}

test "log record with comprehensive context" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("user123"))
  Attributes::set(attrs, "request.id", StringValue("req456"))
  
  let ctx = Context::with_value(Context::root(), ContextKey::new("correlation.id"), "corr789")
  
  // Create log record with full context
  let record = LogRecord::new_with_context(
    Info,
    Some("User action completed"),
    Some(attrs),
    Some(1735689600000000000L),  // timestamp
    Some(1735689600000001000L),  // observed timestamp
    Some("trace123"),
    Some("span456"),
    Some(ctx)
  )
  
  // Test all properties
  assert_eq(match LogRecord::severity_number(record) { Info => true, _ => false }, true)
  assert_eq(LogRecord::body(record), Some("User action completed"))
  assert_eq(LogRecord::trace_id(record), Some("trace123"))
  assert_eq(LogRecord::span_id(record), Some("span456"))
}

test "log record with different body types" {
  // Test with empty body
  let empty_body_record = LogRecord::new_with_context(
    Debug,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::body(empty_body_record), None)
  
  // Test with short body
  let short_record = LogRecord::new(Info, "Short")
  assert_eq(LogRecord::body(short_record), Some("Short"))
  
  // Test with long body
  let long_body = "This is a very long log message that contains detailed information about what happened in the system and provides context for debugging purposes"
  let long_record = LogRecord::new(Error, long_body)
  assert_eq(LogRecord::body(long_record), Some(long_body))
  
  // Test with special characters
  let special_body = "Message with special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_record = LogRecord::new(Warn, special_body)
  assert_eq(LogRecord::body(special_record), Some(special_body))
}

test "log record with attributes" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.attr", StringValue("string-value"))
  Attributes::set(attrs, "int.attr", IntValue(42))
  Attributes::set(attrs, "float.attr", FloatValue(3.14))
  Attributes::set(attrs, "bool.attr", BoolValue(true))
  Attributes::set(attrs, "array.attr", ArrayStringValue(["item1", "item2"]))
  
  let record = LogRecord::new_with_context(
    Info,
    Some("Log with attributes"),
    Some(attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  // Test record creation with attributes
  assert_eq(match LogRecord::severity_number(record) { Info => true, _ => false }, true)
  assert_eq(LogRecord::body(record), Some("Log with attributes"))
}

test "log record timestamp operations" {
  let current_time = 1735689600000000000L
  let observed_time = 1735689600000001000L
  
  // Test with both timestamps
  let record_with_times = LogRecord::new_with_context(
    Info,
    Some("Timestamp test"),
    None,
    Some(current_time),
    Some(observed_time),
    None,
    None,
    None
  )
  
  // Test with only timestamp
  let record_with_timestamp = LogRecord::new_with_context(
    Debug,
    Some("Only timestamp"),
    None,
    Some(current_time),
    None,
    None,
    None,
    None
  )
  
  // Test with only observed timestamp
  let record_with_observed = LogRecord::new_with_context(
    Warn,
    Some("Only observed"),
    None,
    None,
    Some(observed_time),
    None,
    None,
    None
  )
  
  // Test with no timestamps
  let record_no_times = LogRecord::new_with_context(
    Error,
    Some("No timestamps"),
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  // Verify all records created successfully
  assert_eq(match LogRecord::severity_number(record_with_times) { Info => true, _ => false }, true)
  assert_eq(match LogRecord::severity_number(record_with_timestamp) { Debug => true, _ => false }, true)
  assert_eq(match LogRecord::severity_number(record_with_observed) { Warn => true, _ => false }, true)
  assert_eq(match LogRecord::severity_number(record_no_times) { Error => true, _ => false }, true)
}

test "log record trace and span context" {
  // Test with trace and span IDs
  let record_with_trace = LogRecord::new_with_context(
    Info,
    Some("With trace context"),
    None,
    None,
    None,
    Some("trace-id-12345"),
    Some("span-id-67890"),
    None
  )
  
  // Test with only trace ID
  let record_trace_only = LogRecord::new_with_context(
    Debug,
    Some("Trace only"),
    None,
    None,
    None,
    Some("trace-only-123"),
    None,
    None
  )
  
  // Test with only span ID
  let record_span_only = LogRecord::new_with_context(
    Warn,
    Some("Span only"),
    None,
    None,
    None,
    None,
    Some("span-only-456"),
    None
  )
  
  // Test with no trace context
  let record_no_trace = LogRecord::new_with_context(
    Error,
    Some("No trace context"),
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  // Test trace and span ID retrieval
  assert_eq(LogRecord::trace_id(record_with_trace), Some("trace-id-12345"))
  assert_eq(LogRecord::span_id(record_with_trace), Some("span-id-67890"))
  assert_eq(LogRecord::trace_id(record_trace_only), Some("trace-only-123"))
  assert_eq(LogRecord::span_id(record_trace_only), None)
  assert_eq(LogRecord::trace_id(record_span_only), None)
  assert_eq(LogRecord::span_id(record_span_only), Some("span-only-456"))
  assert_eq(LogRecord::trace_id(record_no_trace), None)
  assert_eq(LogRecord::span_id(record_no_trace), None)
}

test "logger provider and emission" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger", Some("1.0.0"))
  
  // Test logger properties
  let scope = logger.scope
  assert_eq(scope.name, "test-logger")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)
  
  // Test log emission
  let simple_record = LogRecord::new(Info, "Simple log message")
  Logger::emit(logger, simple_record)
  
  // Test complex log emission
  let attrs = Attributes::new()
  Attributes::set(attrs, "component", StringValue("auth"))
  
  let complex_record = LogRecord::new_with_context(
    Error,
    Some("Authentication failed"),
    Some(attrs),
    Some(1735689600000000000L),
    None,
    Some("trace123"),
    Some("span456"),
    None
  )
  Logger::emit(logger, complex_record)
}

test "log record severity comparison and filtering" {
  let records = [
    LogRecord::new(Trace, "Trace level"),
    LogRecord::new(Debug, "Debug level"),
    LogRecord::new(Info, "Info level"),
    LogRecord::new(Warn, "Warning level"),
    LogRecord::new(Error, "Error level"),
    LogRecord::new(Fatal, "Fatal level")
  ]
  
  // Test severity level ordering (conceptual)
  let trace_severity = LogRecord::severity_number(records[0])
  let debug_severity = LogRecord::severity_number(records[1])
  let info_severity = LogRecord::severity_number(records[2])
  let warn_severity = LogRecord::severity_number(records[3])
  let error_severity = LogRecord::severity_number(records[4])
  let fatal_severity = LogRecord::severity_number(records[5])
  
  // Verify each severity level
  assert_eq(match trace_severity { Trace => true, _ => false }, true)
  assert_eq(match debug_severity { Debug => true, _ => false }, true)
  assert_eq(match info_severity { Info => true, _ => false }, true)
  assert_eq(match warn_severity { Warn => true, _ => false }, true)
  assert_eq(match error_severity { Error => true, _ => false }, true)
  assert_eq(match fatal_severity { Fatal => true, _ => false }, true)
}