// Log Record Context Correlation Test Suite for Azimuth Telemetry System
// This file contains test cases for log record context correlation and tracing

test "log record with trace and span correlation" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "correlation.test")
  
  // Create log records with trace and span correlation
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Operation completed successfully"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Verify correlation data
  assert_eq(LogRecord::trace_id(log_record), Some(trace_id))
  assert_eq(LogRecord::span_id(log_record), Some(span_id))
  assert_eq(LogRecord::body(log_record), Some("Operation completed successfully"))
  assert_eq(LogRecord::severity_number(log_record), Info)
  
  // Emit the log record
  Logger::emit(logger, log_record)
}

test "log record with context data correlation" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "context.correlation.test")
  
  // Create context with correlation data
  let ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  let request_key = ContextKey::new("request.id")
  
  let ctx = Context::with_value(ctx, user_key, "user-12345")
  let ctx = Context::with_value(ctx, session_key, "session-abcde")
  let ctx = Context::with_value(ctx, request_key, "req-67890")
  
  // Create log record with context correlation
  let log_record = LogRecord::new_with_context(
    Warn,
    Some("User operation requires attention"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-123"),
    Some("span-123"),
    Some(ctx)
  )
  
  // Verify context correlation
  assert_eq(LogRecord::trace_id(log_record), Some("trace-123"))
  assert_eq(LogRecord::span_id(log_record), Some("span-123"))
  assert_eq(LogRecord::body(log_record), Some("User operation requires attention"))
  assert_eq(LogRecord::severity_number(log_record), Warn)
  
  // Emit the log record
  Logger::emit(logger, log_record)
}

test "log record with attributes correlation" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "attributes.correlation.test")
  
  // Create attributes for correlation
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("user.service"))
  Attributes::set(attrs, "operation.type", StringValue("database.query"))
  Attributes::set(attrs, "duration.ms", IntValue(150))
  Attributes::set(attrs, "success", BoolValue(true))
  
  // Create log record with attributes correlation
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Database query executed"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-db-123"),
    Some("span-db-123"),
    Some(Context::root())
  )
  
  // Verify attributes correlation
  assert_eq(LogRecord::trace_id(log_record), Some("trace-db-123"))
  assert_eq(LogRecord::span_id(log_record), Some("span-db-123"))
  assert_eq(LogRecord::body(log_record), Some("Database query executed"))
  assert_eq(LogRecord::severity_number(log_record), Info)
  
  // Emit the log record
  Logger::emit(logger, log_record)
}

test "log record temporal correlation sequence" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "temporal.correlation.test")
  
  let base_timestamp = 1735689600000000000L
  let trace_id = "temporal-trace-123"
  
  // Create sequence of log records with temporal correlation
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Operation started"),
    None,
    Some(base_timestamp),
    Some(base_timestamp + 1000000L),
    Some(trace_id),
    Some("span-start"),
    Some(Context::root())
  )
  
  let progress_log = LogRecord::new_with_context(
    Info,
    Some("Operation in progress"),
    None,
    Some(base_timestamp + 50000000L),  // 50ms later
    Some(base_timestamp + 50001000L),
    Some(trace_id),
    Some("span-progress"),
    Some(Context::root())
  )
  
  let warning_log = LogRecord::new_with_context(
    Warn,
    Some("Operation taking longer than expected"),
    None,
    Some(base_timestamp + 100000000L),  // 100ms later
    Some(base_timestamp + 100001000L),
    Some(trace_id),
    Some("span-warning"),
    Some(Context::root())
  )
  
  let completion_log = LogRecord::new_with_context(
    Info,
    Some("Operation completed"),
    None,
    Some(base_timestamp + 200000000L),  // 200ms later
    Some(base_timestamp + 200001000L),
    Some(trace_id),
    Some("span-completion"),
    Some(Context::root())
  )
  
  // Verify temporal correlation
  assert_eq(LogRecord::trace_id(start_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(progress_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(warning_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(completion_log), Some(trace_id))
  
  // Verify temporal ordering
  assert_eq(LogRecord::body(start_log), Some("Operation started"))
  assert_eq(LogRecord::body(progress_log), Some("Operation in progress"))
  assert_eq(LogRecord::body(warning_log), Some("Operation taking longer than expected"))
  assert_eq(LogRecord::body(completion_log), Some("Operation completed"))
  
  // Emit all log records
  Logger::emit(logger, start_log)
  Logger::emit(logger, progress_log)
  Logger::emit(logger, warning_log)
  Logger::emit(logger, completion_log)
}

test "log record error correlation with context" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "error.correlation.test")
  
  // Create error context
  let error_ctx = Context::root()
  let error_key = ContextKey::new("error.type")
  let retry_key = ContextKey::new("retry.count")
  let component_key = ContextKey::new("component.name")
  
  let error_ctx = Context::with_value(error_ctx, error_key, "DATABASE_TIMEOUT")
  let error_ctx = Context::with_value(error_ctx, retry_key, "3")
  let error_ctx = Context::with_value(error_ctx, component_key, "payment.service")
  
  // Create error log record with context correlation
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Database operation failed after 3 retries"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("error-trace-123"),
    Some("error-span-123"),
    Some(error_ctx)
  )
  
  // Create fatal error log record
  let fatal_log = LogRecord::new_with_context(
    Fatal,
    Some("Critical system failure - immediate attention required"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("fatal-trace-123"),
    Some("fatal-span-123"),
    Some(Context::root())
  )
  
  // Verify error correlation
  assert_eq(LogRecord::trace_id(error_log), Some("error-trace-123"))
  assert_eq(LogRecord::span_id(error_log), Some("error-span-123"))
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::body(error_log), Some("Database operation failed after 3 retries"))
  
  assert_eq(LogRecord::trace_id(fatal_log), Some("fatal-trace-123"))
  assert_eq(LogRecord::span_id(fatal_log), Some("fatal-span-123"))
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  assert_eq(LogRecord::body(fatal_log), Some("Critical system failure - immediate attention required"))
  
  // Emit error log records
  Logger::emit(logger, error_log)
  Logger::emit(logger, fatal_log)
}

test "log record cross-service correlation" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "cross.service.correlation.test")
  
  // Create cross-service correlation scenario
  let correlation_id = "corr-12345"
  let user_id = "user-67890"
  
  // Service A log
  let service_a_log = LogRecord::new_with_context(
    Info,
    Some("Request received in Service A"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-service-a"),
    Some("span-service-a"),
    Some(Context::root())
  )
  
  // Service B log
  let service_b_log = LogRecord::new_with_context(
    Info,
    Some("Processing request in Service B"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-service-a"),  // Same trace ID
    Some("span-service-b"),
    Some(Context::root())
  )
  
  // Service C log
  let service_c_log = LogRecord::new_with_context(
    Info,
    Some("Request completed in Service C"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-service-a"),  // Same trace ID
    Some("span-service-c"),
    Some(Context::root())
  )
  
  // Verify cross-service correlation
  assert_eq(LogRecord::trace_id(service_a_log), Some("trace-service-a"))
  assert_eq(LogRecord::trace_id(service_b_log), Some("trace-service-a"))
  assert_eq(LogRecord::trace_id(service_c_log), Some("trace-service-a"))
  
  // Verify different span IDs
  assert_eq(LogRecord::span_id(service_a_log), Some("span-service-a"))
  assert_eq(LogRecord::span_id(service_b_log), Some("span-service-b"))
  assert_eq(LogRecord::span_id(service_c_log), Some("span-service-c"))
  
  // Emit cross-service log records
  Logger::emit(logger, service_a_log)
  Logger::emit(logger, service_b_log)
  Logger::emit(logger, service_c_log)
}

test "log record with baggage correlation" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "baggage.correlation.test")
  
  // Create baggage context
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage = Baggage::set_entry(baggage, "request.id", "req-67890")
  let baggage = Baggage::set_entry(baggage, "service.version", "1.2.3")
  
  // Create log record with baggage correlation
  let baggage_log = LogRecord::new_with_context(
    Info,
    Some("Operation with baggage correlation"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("baggage-trace-123"),
    Some("baggage-span-123"),
    Some(Context::root())
  )
  
  // Verify baggage correlation
  assert_eq(LogRecord::trace_id(baggage_log), Some("baggage-trace-123"))
  assert_eq(LogRecord::span_id(baggage_log), Some("baggage-span-123"))
  assert_eq(LogRecord::body(baggage_log), Some("Operation with baggage correlation"))
  assert_eq(LogRecord::severity_number(baggage_log), Info)
  
  // Emit baggage log record
  Logger::emit(logger, baggage_log)
}