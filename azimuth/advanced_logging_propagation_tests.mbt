// Advanced Logging and Propagation Test Suite
// Tests complex log record operations, propagation scenarios, and logging integration

test "comprehensive log record creation and properties" {
  // Test basic log record creation
  let simple_record = LogRecord::new(Info, "Simple log message")
  assert_eq(LogRecord::severity_number(simple_record), Info)
  assert_eq(LogRecord::body(simple_record), Some("Simple log message"))
  assert_eq(LogRecord::trace_id(simple_record), None)
  assert_eq(LogRecord::span_id(simple_record), None)
  
  // Test log records with different severity levels
  let trace_record = LogRecord::new(Trace, "Trace level message")
  let debug_record = LogRecord::new(Debug, "Debug level message")
  let info_record = LogRecord::new(Info, "Info level message")
  let warn_record = LogRecord::new(Warn, "Warning level message")
  let error_record = LogRecord::new(Error, "Error level message")
  let fatal_record = LogRecord::new(Fatal, "Fatal level message")
  
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Test log records with complex bodies
  let json_record = LogRecord::new(Info, "{\"event\": \"user_login\", \"user_id\": 12345}")
  let multiline_record = LogRecord::new(Error, "Error occurred:\n  Line 1: Database connection failed\n  Line 2: Retrying connection")
  let unicode_record = LogRecord::new(Info, "用户登录成功 🎉")
  let empty_record = LogRecord::new(Debug, "")
  
  assert_eq(LogRecord::body(json_record), Some("{\"event\": \"user_login\", \"user_id\": 12345}"))
  assert_eq(LogRecord::body(multiline_record), Some("Error occurred:\n  Line 1: Database connection failed\n  Line 2: Retrying connection"))
  assert_eq(LogRecord::body(unicode_record), Some("用户登录成功 🎉"))
  assert_eq(LogRecord::body(empty_record), Some(""))
}

test "log record with full context and attributes" {
  // Create attributes for log record
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("user-12345"))
  Attributes::set(attrs, "request.id", StringValue("req-67890"))
  Attributes::set(attrs, "response.time", IntValue(250))
  Attributes::set(attrs, "cache.hit", BoolValue(true))
  Attributes::set(attrs, "tags", ArrayStringValue(["web", "api", "v1"]))
  
  // Create context
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_value = Context::with_value(ctx, key, "corr-abc123")
  
  // Create timestamps
  let timestamp = 1735689600000000000L // 2025-01-01 00:00:00 UTC
  let observed_timestamp = 1735689600000000500L // Slightly later
  
  // Create comprehensive log record
  let comprehensive_record = LogRecord::new_with_context(
    Warn,
    Some("Operation completed with warnings"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(ctx_with_value)
  )
  
  // Verify all properties
  assert_eq(LogRecord::severity_number(comprehensive_record), Warn)
  assert_eq(LogRecord::body(comprehensive_record), Some("Operation completed with warnings"))
  assert_eq(LogRecord::trace_id(comprehensive_record), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(LogRecord::span_id(comprehensive_record), Some("b7ad6b7169203331"))
}

test "logger provider and logger operations" {
  // Test different logger provider configurations
  let noop_provider = LoggerProvider::noop()
  let default_provider = LoggerProvider::default()
  
  // Create loggers with different scope configurations
  let simple_logger = LoggerProvider::get_logger(noop_provider, "simple-logger")
  let versioned_logger = LoggerProvider::get_logger(default_provider, "versioned-logger", Some("2.1.0"))
  let full_scope_logger = LoggerProvider::get_logger(default_provider, "full-scope-logger", Some("3.0.0"))
  
  // Test logger scope properties
  assert_eq(simple_logger.scope.name, "simple-logger")
  assert_eq(simple_logger.scope.version, None)
  assert_eq(simple_logger.scope.schema_url, None)
  
  assert_eq(versioned_logger.scope.name, "versioned-logger")
  assert_eq(versioned_logger.scope.version, Some("2.1.0"))
  assert_eq(versioned_logger.scope.schema_url, None)
  
  assert_eq(full_scope_logger.scope.name, "full-scope-logger")
  assert_eq(full_scope_logger.scope.version, Some("3.0.0"))
  assert_eq(full_scope_logger.scope.schema_url, None)
  
  // Test emitting different types of log records
  let info_record = LogRecord::new(Info, "Information message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  
  Logger::emit(simple_logger, info_record)
  Logger::emit(versioned_logger, warn_record)
  Logger::emit(full_scope_logger, error_record)
  
  // Test emitting records with full context
  let attrs = Attributes::new()
  Attributes::set(attrs, "service", StringValue("api-service"))
  
  let contextual_record = LogRecord::new_with_context(
    Error,
    Some("Contextual error message"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000100L),
    Some("trace-id-123"),
    Some("span-id-456"),
    Some(Context::root())
  )
  
  Logger::emit(simple_logger, contextual_record)
}

test "log record timestamp and temporal operations" {
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // Test log records with different timestamp configurations
  let no_timestamp_record = LogRecord::new(Info, "No timestamp")
  let timestamp_only_record = LogRecord::new_with_context(
    Info,
    Some("Timestamp only"),
    None,
    Some(base_timestamp),
    None,
    None,
    None,
    None
  )
  let observed_only_record = LogRecord::new_with_context(
    Info,
    Some("Observed timestamp only"),
    None,
    None,
    Some(base_timestamp + 1000L),
    None,
    None,
    None
  )
  let both_timestamps_record = LogRecord::new_with_context(
    Info,
    Some("Both timestamps"),
    None,
    Some(base_timestamp),
    Some(base_timestamp + 500L),
    None,
    None,
    None
  )
  
  // Test timestamp arithmetic scenarios
  let past_timestamp = base_timestamp - 86400000000000L // 1 day ago
  let future_timestamp = base_timestamp + 86400000000000L // 1 day in future
  let zero_timestamp = 0L
  let max_timestamp = 9223372036854775807L // Max int64
  
  let past_record = LogRecord::new_with_context(
    Debug,
    Some("Past timestamp"),
    None,
    Some(past_timestamp),
    None,
    None,
    None,
    None
  )
  
  let future_record = LogRecord::new_with_context(
    Warn,
    Some("Future timestamp"),
    None,
    Some(future_timestamp),
    None,
    None,
    None,
    None
  )
  
  let zero_record = LogRecord::new_with_context(
    Info,
    Some("Zero timestamp"),
    None,
    Some(zero_timestamp),
    None,
    None,
    None,
    None
  )
  
  let max_record = LogRecord::new_with_context(
    Error,
    Some("Max timestamp"),
    None,
    Some(max_timestamp),
    None,
    None,
    None,
    None
  )
  
  // Test timestamp differences
  let time_diff_record = LogRecord::new_with_context(
    Info,
    Some("Time difference test"),
    None,
    Some(base_timestamp),
    Some(base_timestamp + 1000000L), // 1ms difference
    None,
    None,
    None
  )
}

test "log record trace and span integration" {
  // Test various trace ID formats
  let valid_trace_record = LogRecord::new_with_context(
    Info,
    Some("Valid trace ID"),
    None,
    None,
    None,
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    None
  )
  
  let empty_trace_record = LogRecord::new_with_context(
    Info,
    Some("Empty trace ID"),
    None,
    None,
    None,
    Some(""),
    Some(""),
    None
  )
  
  let missing_trace_record = LogRecord::new_with_context(
    Info,
    Some("Missing trace ID"),
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  // Test with different span ID scenarios
  let valid_span_record = LogRecord::new_with_context(
    Info,
    Some("Valid span ID"),
    None,
    None,
    None,
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    None
  )
  
  let mismatched_record = LogRecord::new_with_context(
    Warn,
    Some("Mismatched trace/span"),
    None,
    None,
    None,
    Some("trace-id-here"),
    Some(""),
    None
  )
  
  // Test integration with span context
  let span_ctx = SpanContext::new("1234567890abcdef1234567890abcdef", "1234567890abcdef", true, "key=value")
  let span_integration_record = LogRecord::new_with_context(
    Info,
    Some("Span integration"),
    None,
    None,
    None,
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    None
  )
  
  // Verify trace and span IDs
  assert_eq(LogRecord::trace_id(valid_trace_record), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(LogRecord::span_id(valid_trace_record), Some("b7ad6b7169203331"))
  assert_eq(LogRecord::trace_id(empty_trace_record), Some(""))
  assert_eq(LogRecord::span_id(empty_trace_record), Some(""))
  assert_eq(LogRecord::trace_id(missing_trace_record), None)
  assert_eq(LogRecord::span_id(missing_trace_record), None)
}

test "log record context propagation" {
  // Test context propagation scenarios
  let root_ctx = Context::root()
  let correlation_key = ContextKey::new("correlation.id")
  let user_key = ContextKey::new("user.id")
  let request_key = ContextKey::new("request.id")
  
  // Build context with multiple values
  let ctx_with_correlation = Context::with_value(root_ctx, correlation_key, "corr-abc123")
  let ctx_with_user = Context::with_value(ctx_with_correlation, user_key, "user-45678")
  let full_ctx = Context::with_value(ctx_with_user, request_key, "req-7890")
  
  // Create log records with different context levels
  let no_context_record = LogRecord::new(Info, "No context")
  let correlation_record = LogRecord::new_with_context(
    Info,
    Some("With correlation ID"),
    None,
    None,
    None,
    None,
    None,
    Some(ctx_with_correlation)
  )
  let user_record = LogRecord::new_with_context(
    Info,
    Some("With user context"),
    None,
    None,
    None,
    None,
    None,
    Some(ctx_with_user)
  )
  let full_context_record = LogRecord::new_with_context(
    Info,
    Some("Full context"),
    None,
    None,
    None,
    None,
    None,
    Some(full_ctx)
  )
  
  // Test context value retrieval
  let retrieved_correlation = Context::get(ctx_with_correlation, correlation_key)
  let retrieved_user = Context::get(ctx_with_user, user_key)
  let retrieved_request = Context::get(full_ctx, request_key)
  
  assert_eq(retrieved_correlation, Some("corr-abc123"))
  assert_eq(retrieved_user, Some("user-45678"))
  assert_eq(retrieved_request, Some("req-7890"))
  
  // Test missing context values
  let missing_correlation = Context::get(root_ctx, correlation_key)
  let missing_user = Context::get(ctx_with_correlation, user_key)
  
  assert_eq(missing_correlation, None)
  assert_eq(missing_user, None)
}

test "complex logging scenarios and edge cases" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "complex-logger", Some("1.0.0"))
  
  // Test high-volume logging scenario
  for i in 0..50 {
    let message = "Log message " + Int::to_string(i)
    let record = LogRecord::new(Info, message)
    Logger::emit(logger, record)
  }
  
  // Test logging with complex attribute combinations
  let complex_attrs = Attributes::new()
  Attributes::set(complex_attrs, "service.name", StringValue("api-service"))
  Attributes::set(complex_attrs, "service.version", StringValue("v1.2.3"))
  Attributes::set(complex_attrs, "deployment.environment", StringValue("production"))
  Attributes::set(complex_attrs, "host.name", StringValue("api-server-01"))
  Attributes::set(complex_attrs, "process.id", IntValue(12345))
  Attributes::set(complex_attrs, "thread.id", IntValue(67890))
  Attributes::set(complex_attrs, "request.method", StringValue("POST"))
  Attributes::set(complex_attrs, "request.url", StringValue("/api/v1/users"))
  Attributes::set(complex_attrs, "response.status", IntValue(201))
  Attributes::set(complex_attrs, "response.time", FloatValue(245.6))
  Attributes::set(complex_attrs, "user.id", StringValue("user-12345"))
  Attributes::set(complex_attrs, "session.id", StringValue("sess-abcdef"))
  Attributes::set(complex_attrs, "trace.id", StringValue("trace-123456"))
  Attributes::set(complex_attrs, "span.id", StringValue("span-789012"))
  
  let complex_record = LogRecord::new_with_context(
    Info,
    Some("Complex log with many attributes"),
    Some(complex_attrs),
    Some(1735689600000000000L),
    Some(1735689600000000100L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(Context::root())
  )
  
  Logger::emit(logger, complex_record)
  
  // Test error logging scenarios
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error.type", StringValue("DatabaseError"))
  Attributes::set(error_attrs, "error.code", StringValue("DB_CONN_FAILED"))
  Attributes::set(error_attrs, "error.message", StringValue("Connection timeout after 30 seconds"))
  Attributes::set(error_attrs, "retry.count", IntValue(3))
  Attributes::set(error_attrs, "stack.trace", StringValue("at Database.connect (db.js:123)\n  at Service.getUser (service.js:456)"))
  
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(error_attrs),
    Some(1735689600000000000L),
    None,
    Some("error-trace-id"),
    Some("error-span-id"),
    None
  )
  
  Logger::emit(logger, error_record)
  
  // Test edge cases
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
  
  let very_long_message = "A" * 1000
  let long_message_record = LogRecord::new(Info, very_long_message)
  
  let special_chars_record = LogRecord::new(Warn, "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?")
  let unicode_complex_record = LogRecord::new(Info, "Unicode: 测试 🚀 ñáéíóú 中文 العربية")
  
  Logger::emit(logger, empty_body_record)
  Logger::emit(logger, long_message_record)
  Logger::emit(logger, special_chars_record)
  Logger::emit(logger, unicode_complex_record)
}