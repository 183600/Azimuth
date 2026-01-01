// Logging Context Test Suite for Azimuth Telemetry System
// This file contains test cases for logging operations with context integration

test "basic log record with context" {
  // Test basic log record creation with context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "context.test")
  
  // Create context
  let ctx = Context::root()
  let key = ContextKey::new("request.id")
  let ctx_with_value = Context::with_value(ctx, key, "req-12345")
  
  // Create log record with context
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Test log with context"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace123"),
    Some("span456"),
    Some(ctx_with_value)
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("Test log with context"))
  assert_eq(LogRecord::trace_id(log_record), Some("trace123"))
  assert_eq(LogRecord::span_id(log_record), Some("span456"))
  
  // Emit log
  Logger::emit(logger, log_record)
}

test "log record severity levels with context" {
  // Test different severity levels with context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "severity.test")
  
  let ctx = Context::root()
  let operation_key = ContextKey::new("operation")
  let ctx_with_operation = Context::with_value(ctx, operation_key, "data.processing")
  
  // Create log records with different severity levels
  let trace_log = LogRecord::new_with_context(
    Trace,
    Some("Trace level message"),
    None,
    None,
    None,
    Some("trace123"),
    Some("span456"),
    Some(ctx_with_operation)
  )
  
  let debug_log = LogRecord::new_with_context(
    Debug,
    Some("Debug level message"),
    None,
    None,
    None,
    Some("trace123"),
    Some("span456"),
    Some(ctx_with_operation)
  )
  
  let info_log = LogRecord::new_with_context(
    Info,
    Some("Info level message"),
    None,
    None,
    None,
    Some("trace123"),
    Some("span456"),
    Some(ctx_with_operation)
  )
  
  let warn_log = LogRecord::new_with_context(
    Warn,
    Some("Warning level message"),
    None,
    None,
    None,
    Some("trace123"),
    Some("span456"),
    Some(ctx_with_operation)
  )
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Error level message"),
    None,
    None,
    None,
    Some("trace123"),
    Some("span456"),
    Some(ctx_with_operation)
  )
  
  let fatal_log = LogRecord::new_with_context(
    Fatal,
    Some("Fatal level message"),
    None,
    None,
    None,
    Some("trace123"),
    Some("span456"),
    Some(ctx_with_operation)
  )
  
  // Verify severity levels
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // Emit all logs
  Logger::emit(logger, trace_log)
  Logger::emit(logger, debug_log)
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, fatal_log)
}

test "log record with attributes and context" {
  // Test log record with attributes and context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "attributes.test")
  
  // Create attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("user123"))
  Attributes::set(attrs, "session.id", StringValue("session456"))
  
  // Create context
  let ctx = Context::root()
  let request_key = ContextKey::new("request.id")
  let ctx_with_request = Context::with_value(ctx, request_key, "req-789")
  
  // Create log record with attributes and context
  let log_record = LogRecord::new_with_context(
    Info,
    Some("User action completed"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000005000L),
    Some("trace123"),
    Some("span456"),
    Some(ctx_with_request)
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("User action completed"))
  assert_eq(LogRecord::trace_id(log_record), Some("trace123"))
  assert_eq(LogRecord::span_id(log_record), Some("span456"))
  
  // Emit log
  Logger::emit(logger, log_record)
}

test "log record temporal correlation" {
  // Test temporal correlation of log records
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "temporal.test")
  
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // Create context for correlation
  let ctx = Context::root()
  let correlation_key = ContextKey::new("correlation.id")
  let ctx_with_correlation = Context::with_value(ctx, correlation_key, "corr-12345")
  
  // Create correlated log records
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Operation started"),
    None,
    Some(base_timestamp),
    Some(base_timestamp + 1000000L),
    Some("trace123"),
    Some("span456"),
    Some(ctx_with_correlation)
  )
  
  let progress_log = LogRecord::new_with_context(
    Info,
    Some("Operation in progress"),
    None,
    Some(base_timestamp + 2000000L),
    Some(base_timestamp + 2001000L),
    Some("trace123"),
    Some("span456"),
    Some(ctx_with_correlation)
  )
  
  let end_log = LogRecord::new_with_context(
    Info,
    Some("Operation completed"),
    None,
    Some(base_timestamp + 3000000L),
    Some(base_timestamp + 3001000L),
    Some("trace123"),
    Some("span456"),
    Some(ctx_with_correlation)
  )
  
  // Verify temporal correlation
  assert_eq(LogRecord::trace_id(start_log), Some("trace123"))
  assert_eq(LogRecord::trace_id(progress_log), Some("trace123"))
  assert_eq(LogRecord::trace_id(end_log), Some("trace123"))
  
  assert_eq(LogRecord::span_id(start_log), Some("span456"))
  assert_eq(LogRecord::span_id(progress_log), Some("span456"))
  assert_eq(LogRecord::span_id(end_log), Some("span456"))
  
  // Emit correlated logs
  Logger::emit(logger, start_log)
  Logger::emit(logger, progress_log)
  Logger::emit(logger, end_log)
}

test "log record with complex context" {
  // Test log record with complex nested context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "complex.context.test")
  
  // Create complex nested context
  let ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  let request_key = ContextKey::new("request.id")
  let operation_key = ContextKey::new("operation.type")
  
  let complex_ctx = Context::with_value(
    Context::with_value(
      Context::with_value(
        Context::with_value(ctx, user_key, "user123"),
        session_key,
        "session456"
      ),
      request_key,
      "req-789"
    ),
    operation_key,
    "data.processing"
  )
  
  // Create log record with complex context
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Complex context operation"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000002000L),
    Some("trace123"),
    Some("span456"),
    Some(complex_ctx)
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("Complex context operation"))
  
  // Emit log
  Logger::emit(logger, log_record)
}

test "log record with span integration" {
  // Test log record integration with spans
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "span.integration.test")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "span.integration.test")
  
  // Create span
  let span = Tracer::start_span(tracer, "integration.test.span")
  let span_ctx = Span::span_context(span)
  
  // Create context
  let ctx = Context::root()
  let key = ContextKey::new("integration.test")
  let ctx_with_value = Context::with_value(ctx, key, "span.log.integration")
  
  // Create log record linked to span
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Log within span"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000003000L),
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    Some(ctx_with_value)
  )
  
  // Verify span-log integration
  assert_eq(LogRecord::trace_id(log_record), Some("test_trace_id"))
  assert_eq(LogRecord::span_id(log_record), Some("test_span_id"))
  
  // Add event to span
  Span::add_event(span, "log.recorded", Some([("log.level", StringValue("info"))]))
  
  // Emit log and end span
  Logger::emit(logger, log_record)
  Span::end(span)
}

test "log record error context" {
  // Test log record with error context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "error.context.test")
  
  // Create error context
  let ctx = Context::root()
  let error_key = ContextKey::new("error.code")
  let retry_key = ContextKey::new("retry.count")
  let error_ctx = Context::with_value(
    Context::with_value(ctx, error_key, "TIMEOUT"),
    retry_key,
    "3"
  )
  
  // Create error log record
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Operation failed with timeout"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000004000L),
    Some("trace123"),
    Some("span456"),
    Some(error_ctx)
  )
  
  // Create recovery log record
  let recovery_ctx = Context::with_value(
    Context::with_value(ctx, error_key, "RECOVERED"),
    retry_key,
    "4"
  )
  
  let recovery_log = LogRecord::new_with_context(
    Info,
    Some("Operation recovered after retry"),
    None,
    Some(1735689600000005000L),
    Some(1735689600000006000L),
    Some("trace123"),
    Some("span456"),
    Some(recovery_ctx)
  )
  
  // Verify error context logs
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(recovery_log), Info)
  assert_eq(LogRecord::body(error_log), Some("Operation failed with timeout"))
  assert_eq(LogRecord::body(recovery_log), Some("Operation recovered after retry"))
  
  // Emit error and recovery logs
  Logger::emit(logger, error_log)
  Logger::emit(logger, recovery_log)
}

test "log record with internationalization context" {
  // Test log record with internationalization context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n.test")
  
  // Create context with locale information
  let ctx = Context::root()
  let locale_key = ContextKey::new("locale")
  let timezone_key = ContextKey::new("timezone")
  let i18n_ctx = Context::with_value(
    Context::with_value(ctx, locale_key, "zh-CN"),
    timezone_key,
    "Asia/Shanghai"
  )
  
  // Create log records in different languages
  let english_log = LogRecord::new_with_context(
    Info,
    Some("Operation completed successfully"),
    None,
    None,
    None,
    Some("trace123"),
    Some("span456"),
    Some(i18n_ctx)
  )
  
  let chinese_log = LogRecord::new_with_context(
    Info,
    Some("操作成功完成"),
    None,
    None,
    None,
    Some("trace123"),
    Some("span456"),
    Some(i18n_ctx)
  )
  
  let mixed_log = LogRecord::new_with_context(
    Info,
    Some("Mixed: 操作 completed successfully"),
    None,
    None,
    None,
    Some("trace123"),
    Some("span456"),
    Some(i18n_ctx)
  )
  
  // Verify internationalized logs
  assert_eq(LogRecord::body(english_log), Some("Operation completed successfully"))
  assert_eq(LogRecord::body(chinese_log), Some("操作成功完成"))
  assert_eq(LogRecord::body(mixed_log), Some("Mixed: 操作 completed successfully"))
  
  // Emit internationalized logs
  Logger::emit(logger, english_log)
  Logger::emit(logger, chinese_log)
  Logger::emit(logger, mixed_log)
}