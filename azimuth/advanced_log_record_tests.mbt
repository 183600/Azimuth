// Advanced Log Record Tests for Azimuth Telemetry System
// Focus on timestamp handling, context correlation, and comprehensive logging scenarios

test "log_record_timestamp_operations" {
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // Test log record with timestamp
  let log_with_timestamp = LogRecord::new_with_context(
    Info,
    Some("Log with timestamp"),
    None,
    Some(base_timestamp),
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(log_with_timestamp), Info)
  match LogRecord::body(log_with_timestamp) {
    Some(body) => assert_eq(body, "Log with timestamp")
    _ => assert_true(false)
  }
  
  // Test log record with both timestamp and observed timestamp
  let observed_timestamp = base_timestamp + 1000000L  // 1ms later
  let log_with_both_timestamps = LogRecord::new_with_context(
    Warn,
    Some("Log with both timestamps"),
    None,
    Some(base_timestamp),
    Some(observed_timestamp),
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(log_with_both_timestamps), Warn)
  match LogRecord::body(log_with_both_timestamps) {
    Some(body) => assert_eq(body, "Log with both timestamps")
    _ => assert_true(false)
  }
  
  // Test log record without timestamps
  let log_without_timestamps = LogRecord::new(Error, "Log without timestamps")
  assert_eq(LogRecord::severity_number(log_without_timestamps), Error)
  match LogRecord::body(log_without_timestamps) {
    Some(body) => assert_eq(body, "Log without timestamps")
    _ => assert_true(false)
  }
}

test "log_record_context_correlation" {
  // Test log record with trace context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let log_with_trace = LogRecord::new_with_context(
    Info,
    Some("Log with trace context"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  assert_eq(LogRecord::severity_number(log_with_trace), Info)
  match LogRecord::trace_id(log_with_trace) {
    Some(id) => assert_eq(id, trace_id)
    _ => assert_true(false)
  }
  
  match LogRecord::span_id(log_with_trace) {
    Some(id) => assert_eq(id, span_id)
    _ => assert_true(false)
  }
  
  // Test log record with context data
  let ctx_key = ContextKey::new("user.id")
  let ctx_value = "user123"
  let context = Context::with_value(Context::root(), ctx_key, ctx_value)
  
  let log_with_context = LogRecord::new_with_context(
    Debug,
    Some("Log with context data"),
    None,
    None,
    None,
    None,
    None,
    Some(context)
  )
  
  assert_eq(LogRecord::severity_number(log_with_context), Debug)
  match LogRecord::body(log_with_context) {
    Some(body) => assert_eq(body, "Log with context data")
    _ => assert_true(false)
  }
  
  // Test log record with both trace context and context data
  let log_with_both = LogRecord::new_with_context(
    Error,
    Some("Log with trace and context"),
    None,
    Some(1634567890000000000L),
    Some(1634567890100000000L),
    Some(trace_id),
    Some(span_id),
    Some(context)
  )
  
  assert_eq(LogRecord::severity_number(log_with_both), Error)
  match LogRecord::trace_id(log_with_both) {
    Some(id) => assert_eq(id, trace_id)
    _ => assert_true(false)
  }
}

test "log_record_with_attributes" {
  // Create attributes for log record
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("user123"))
  Attributes::set(attrs, "request.id", StringValue("req-456"))
  Attributes::set(attrs, "response.time", IntValue(150))
  Attributes::set(attrs, "success", BoolValue(true))
  
  let log_with_attrs = LogRecord::new_with_context(
    Info,
    Some("Request processed successfully"),
    Some(attrs),
    Some(1634567890000000000L),
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(log_with_attrs), Info)
  match LogRecord::body(log_with_attrs) {
    Some(body) => assert_eq(body, "Request processed successfully")
    _ => assert_true(false)
  }
  
  // Test log record with complex attributes
  let complex_attrs = Attributes::new()
  Attributes::set(complex_attrs, "service.name", StringValue("azimuth-api"))
  Attributes::set(complex_attrs, "service.version", StringValue("1.2.3"))
  Attributes::set(complex_attrs, "host.name", StringValue("server-01"))
  Attributes::set(complex_attrs, "cpu.usage", FloatValue(67.5))
  Attributes::set(complex_attrs, "memory.usage", IntValue(1024))
  Attributes::set(complex_attrs, "healthy", BoolValue(true))
  Attributes::set(complex_attrs, "tags", ArrayStringValue(["api", "production", "v1"]))
  Attributes::set(complex_attrs, "ports", ArrayIntValue([8080, 8443, 9090]))
  
  let log_with_complex_attrs = LogRecord::new_with_context(
    Warn,
    Some("Service health warning"),
    Some(complex_attrs),
    None,
    None,
    Some("trace123"),
    Some("span456"),
    None
  )
  
  assert_eq(LogRecord::severity_number(log_with_complex_attrs), Warn)
  match LogRecord::body(log_with_complex_attrs) {
    Some(body) => assert_eq(body, "Service health warning")
    _ => assert_true(false)
  }
}

test "log_record_severity_levels" {
  // Test all severity levels
  let severity_levels = [
    (Trace, "Trace level message"),
    (Debug, "Debug level message"),
    (Info, "Info level message"),
    (Warn, "Warning level message"),
    (Error, "Error level message"),
    (Fatal, "Fatal level message")
  ]
  
  for (severity, message) in severity_levels {
    let log_record = LogRecord::new(severity, message)
    
    assert_eq(LogRecord::severity_number(log_record), severity)
    match LogRecord::body(log_record) {
      Some(body) => assert_eq(body, message)
      _ => assert_true(false)
    }
  }
}

test "log_record_comprehensive_scenario" {
  // Create a comprehensive log record with all fields
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "fedcba0987654321"
  let timestamp = 1634567890000000000L
  let observed_timestamp = 1634567890050000000L
  
  // Create comprehensive attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "event.name", StringValue("user.login"))
  Attributes::set(attrs, "user.id", StringValue("user123"))
  Attributes::set(attrs, "user.email", StringValue("user@example.com"))
  Attributes::set(attrs, "ip.address", StringValue("192.168.1.100"))
  Attributes::set(attrs, "login.method", StringValue("oauth"))
  Attributes::set(attrs, "response.time", IntValue(250))
  Attributes::set(attrs, "success", BoolValue(true))
  Attributes::set(attrs, "session.duration", FloatValue(123.45))
  Attributes::set(attrs, "permissions", ArrayStringValue(["read", "write", "admin"]))
  Attributes::set(attrs, "attempts", ArrayIntValue([1, 2, 3]))
  
  // Create context
  let ctx_key = ContextKey::new("request.id")
  let context = Context::with_value(Context::root(), ctx_key, "req-789")
  
  // Create comprehensive log record
  let comprehensive_log = LogRecord::new_with_context(
    Info,
    Some("User login successful"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(context)
  )
  
  // Verify all properties
  assert_eq(LogRecord::severity_number(comprehensive_log), Info)
  
  match LogRecord::body(comprehensive_log) {
    Some(body) => assert_eq(body, "User login successful")
    _ => assert_true(false)
  }
  
  match LogRecord::trace_id(comprehensive_log) {
    Some(id) => assert_eq(id, trace_id)
    _ => assert_true(false)
  }
  
  match LogRecord::span_id(comprehensive_log) {
    Some(id) => assert_eq(id, span_id)
    _ => assert_true(false)
  }
}

test "logger_emission_and_provider_operations" {
  // Test logger provider operations
  let logger_provider = LoggerProvider::noop()
  let logger = LoggerProvider::get_logger(logger_provider, "test.logger")
  
  // Test emitting different types of log records
  let info_log = LogRecord::new(Info, "Information message")
  Logger::emit(logger, info_log)
  
  let warn_log = LogRecord::new(Warn, "Warning message")
  Logger::emit(logger, warn_log)
  
  let error_log = LogRecord::new(Error, "Error message")
  Logger::emit(logger, error_log)
  
  // Test logger with context
  let trace_id = "trace123456"
  let span_id = "span789012"
  let contextual_log = LogRecord::new_with_context(
    Debug,
    Some("Debug message with context"),
    None,
    Some(1634567890000000000L),
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  Logger::emit(logger, contextual_log)
  
  // Test logger with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "component", StringValue("auth"))
  Attributes::set(attrs, "version", StringValue("2.1.0"))
  
  let attr_log = LogRecord::new_with_context(
    Info,
    Some("Component started"),
    Some(attrs),
    None,
    None,
    None,
    None,
    None
  )
  Logger::emit(logger, attr_log)
  
  // Test default logger provider
  let default_provider = LoggerProvider::default()
  let default_logger = LoggerProvider::get_logger(default_provider, "default.logger")
  
  let default_log = LogRecord::new(Info, "Default logger test")
  Logger::emit(default_logger, default_log)
}

test "log_record_edge_cases" {
  // Test log record with empty body
  let empty_body_log = LogRecord::new_with_context(
    Info,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::severity_number(empty_body_log), Info)
  assert_eq(LogRecord::body(empty_body_log), None)
  
  // Test log record with empty string body
  let empty_string_log = LogRecord::new(Info, "")
  match LogRecord::body(empty_string_log) {
    Some(body) => assert_eq(body, "")
    _ => assert_true(false)
  }
  
  // Test log record with very long body
  let long_body = "This is a very long log message " * 100
  let long_log = LogRecord::new(Error, long_body)
  match LogRecord::body(long_log) {
    Some(body) => assert_true(body.length > 1000)
    _ => assert_true(false)
  }
  
  // Test log record with special characters in body
  let special_body = "Special chars: ~!@#$%^&*()_+-=[]{}|;':\",./<>? 中文测试 🚀\nNew lines\tTabs"
  let special_log = LogRecord::new(Warn, special_body)
  match LogRecord::body(special_log) {
    Some(body) => assert_eq(body, special_body)
    _ => assert_true(false)
  }
  
  // Test log record with zero timestamps
  let zero_timestamp_log = LogRecord::new_with_context(
    Debug,
    Some("Zero timestamp test"),
    None,
    Some(0L),
    Some(0L),
    Some(""),
    Some(""),
    None
  )
  assert_eq(LogRecord::severity_number(zero_timestamp_log), Debug)
  
  // Test log record with maximum timestamps
  let max_timestamp = 9223372036854775807L  // Max int64
  let max_timestamp_log = LogRecord::new_with_context(
    Fatal,
    Some("Max timestamp test"),
    None,
    Some(max_timestamp),
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::severity_number(max_timestamp_log), Fatal)
}