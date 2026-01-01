// Logger Context Correlation Test Suite for Azimuth Telemetry System
// This file contains test cases for logger context correlation operations

test "logger basic context correlation" {
  // Test basic logger context correlation
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "context.test.logger")
  
  // Create context with correlation data
  let ctx = Context::root()
  let correlation_key = ContextKey::new("correlation.id")
  let ctx_with_correlation = Context::with_value(ctx, correlation_key, "corr-12345")
  
  // Create log record with context
  let log_with_context = LogRecord::new_with_context(
    Info,
    Some("Log with correlation context"),
    None,
    Some(1735689600000000000L),
    None,
    None,
    None,
    Some(ctx_with_correlation)
  )
  
  // Verify log record has context
  assert_eq(log_with_context.context, Some(ctx_with_correlation))
  assert_eq(LogRecord::body(log_with_context), Some("Log with correlation context"))
  
  // Emit log record
  Logger::emit(logger, log_with_context)
}

test "logger trace context correlation" {
  // Test logger correlation with trace context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "trace.context.logger")
  
  // Create span context
  let span_ctx = SpanContext::new("trace-logger-001", "span-logger-001", true, "key1=value1")
  let trace_id = SpanContext::trace_id(span_ctx)
  let span_id = SpanContext::span_id(span_ctx)
  
  // Create log record with trace context
  let log_with_trace = LogRecord::new_with_context(
    Info,
    Some("Log with trace context"),
    None,
    Some(1735689600000000000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Verify trace context
  assert_eq(log_with_trace.trace_id, Some(trace_id))
  assert_eq(log_with_trace.span_id, Some(span_id))
  assert_eq(LogRecord::body(log_with_trace), Some("Log with trace context"))
  
  // Emit log record
  Logger::emit(logger, log_with_trace)
}

test "logger span lifecycle correlation" {
  // Test logger correlation throughout span lifecycle
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "span.lifecycle.tracer")
  let logger = LoggerProvider::get_logger(logger_provider, "span.lifecycle.logger")
  
  // Start span
  let span = Tracer::start_span(tracer, "lifecycle.operation")
  let span_ctx = Span::span_context(span)
  let trace_id = SpanContext::trace_id(span_ctx)
  let span_id = SpanContext::span_id(span_ctx)
  
  // Log at span start
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Span started"),
    None,
    Some(1735689600000000000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  Logger::emit(logger, start_log)
  
  // Add span event
  Span::add_event(span, "processing.started", Some([("step", StringValue("1"))]))
  
  // Log during processing
  let processing_log = LogRecord::new_with_context(
    Debug,
    Some("Processing in progress"),
    None,
    Some(1735689600000001000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  Logger::emit(logger, processing_log)
  
  // Add another span event
  Span::add_event(span, "processing.completed", Some([("step", StringValue("2"))]))
  
  // Log at span end
  let end_log = LogRecord::new_with_context(
    Info,
    Some("Span completed"),
    None,
    Some(1735689600000002000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  Logger::emit(logger, end_log)
  
  Span::end(span)
}

test "logger error context correlation" {
  // Test logger correlation with error context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "error.context.logger")
  
  // Create error context
  let ctx = Context::root()
  let error_key = ContextKey::new("error.code")
  let retry_key = ContextKey::new("retry.count")
  let ctx_with_error = Context::with_value(ctx, error_key, "TIMEOUT")
  let ctx_with_retry = Context::with_value(ctx_with_error, retry_key, "3")
  
  // Create span context for error
  let error_span_ctx = SpanContext::new("trace-error-001", "span-error-001", true, "")
  
  // Create error log record with full context
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Operation failed with timeout"),
    None,
    Some(1735689600000000000L),
    None,
    Some(SpanContext::trace_id(error_span_ctx)),
    Some(SpanContext::span_id(error_span_ctx)),
    Some(ctx_with_retry)
  )
  
  // Verify error context
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(error_log.trace_id, Some(SpanContext::trace_id(error_span_ctx)))
  assert_eq(error_log.span_id, Some(SpanContext::span_id(error_span_ctx)))
  assert_eq(error_log.context, Some(ctx_with_retry))
  
  // Create recovery log record
  let recovery_log = LogRecord::new_with_context(
    Info,
    Some("Operation recovered after retry"),
    None,
    Some(1735689600000005000L),
    None,
    Some(SpanContext::trace_id(error_span_ctx)),
    Some(SpanContext::span_id(error_span_ctx)),
    Some(ctx_with_retry)
  )
  
  // Emit both log records
  Logger::emit(logger, error_log)
  Logger::emit(logger, recovery_log)
}

test "logger baggage context correlation" {
  // Test logger correlation with baggage context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "baggage.context.logger")
  
  // Create context with baggage-like data
  let ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  let request_key = ContextKey::new("request.id")
  
  let ctx_with_user = Context::with_value(ctx, user_key, "user-12345")
  let ctx_with_session = Context::with_value(ctx_with_user, session_key, "session-67890")
  let ctx_with_request = Context::with_value(ctx_with_session, request_key, "req-abcde")
  
  // Create span context
  let span_ctx = SpanContext::new("trace-baggage-001", "span-baggage-001", true, "")
  
  // Create log record with baggage context
  let baggage_log = LogRecord::new_with_context(
    Info,
    Some("Operation with baggage context"),
    None,
    Some(1735689600000000000L),
    None,
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    Some(ctx_with_request)
  )
  
  // Verify baggage context
  assert_eq(baggage_log.context, Some(ctx_with_request))
  assert_eq(baggage_log.trace_id, Some(SpanContext::trace_id(span_ctx)))
  assert_eq(baggage_log.span_id, Some(SpanContext::span_id(span_ctx)))
  
  // Emit log record
  Logger::emit(logger, baggage_log)
}

test "logger multi-service context correlation" {
  // Test logger correlation across multiple services
  let logger_provider = LoggerProvider::default()
  let service_a_logger = LoggerProvider::get_logger(logger_provider, "service.a.logger")
  let service_b_logger = LoggerProvider::get_logger(logger_provider, "service.b.logger")
  let service_c_logger = LoggerProvider::get_logger(logger_provider, "service.c.logger")
  
  // Create distributed trace context
  let trace_id = "trace-multi-service-001"
  
  // Service A: Create initial span and log
  let service_a_span_ctx = SpanContext::new(trace_id, "span-a-001", true, "")
  let service_a_ctx = Context::root()
  let service_a_key = ContextKey::new("service.a")
  let service_a_ctx_with_data = Context::with_value(service_a_ctx, service_a_key, "started")
  
  let service_a_log = LogRecord::new_with_context(
    Info,
    Some("Service A started processing"),
    None,
    Some(1735689600000000000L),
    None,
    Some(trace_id),
    Some("span-a-001"),
    Some(service_a_ctx_with_data)
  )
  Logger::emit(service_a_logger, service_a_log)
  
  // Service B: Continue trace with new span
  let service_b_span_ctx = SpanContext::new(trace_id, "span-b-001", true, "")
  let service_b_ctx = Context::root()
  let service_b_key = ContextKey::new("service.b")
  let service_b_ctx_with_data = Context::with_value(service_b_ctx, service_b_key, "processing")
  
  let service_b_log = LogRecord::new_with_context(
    Info,
    Some("Service B processing request"),
    None,
    Some(1735689600000001000L),
    None,
    Some(trace_id),
    Some("span-b-001"),
    Some(service_b_ctx_with_data)
  )
  Logger::emit(service_b_logger, service_b_log)
  
  // Service C: Complete trace
  let service_c_span_ctx = SpanContext::new(trace_id, "span-c-001", true, "")
  let service_c_ctx = Context::root()
  let service_c_key = ContextKey::new("service.c")
  let service_c_ctx_with_data = Context::with_value(service_c_ctx, service_c_key, "completed")
  
  let service_c_log = LogRecord::new_with_context(
    Info,
    Some("Service C completed request"),
    None,
    Some(1735689600000002000L),
    None,
    Some(trace_id),
    Some("span-c-001"),
    Some(service_c_ctx_with_data)
  )
  Logger::emit(service_c_logger, service_c_log)
}

test "logger context with attributes" {
  // Test logger context with rich attributes
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "attributes.context.logger")
  
  // Create attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "operation.type", StringValue("data.processing"))
  Attributes::set(attrs, "operation.id", IntValue(12345))
  Attributes::set(attrs, "operation.success", BoolValue(true))
  Attributes::set(attrs, "operation.tags", ArrayStringValue(["batch", "critical", "scheduled"]))
  
  // Create context with additional data
  let ctx = Context::root()
  let ctx_key = ContextKey::new("workflow.id")
  let ctx_with_data = Context::with_value(ctx, ctx_key, "workflow-abc123")
  
  // Create span context
  let span_ctx = SpanContext::new("trace-attrs-001", "span-attrs-001", true, "")
  
  // Create log record with attributes and context
  let rich_log = LogRecord::new_with_context(
    Info,
    Some("Rich log with attributes and context"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    Some(ctx_with_data)
  )
  
  // Verify rich log record
  assert_eq(LogRecord::body(rich_log), Some("Rich log with attributes and context"))
  assert_eq(rich_log.attributes, Some(attrs))
  assert_eq(rich_log.context, Some(ctx_with_data))
  assert_eq(rich_log.trace_id, Some(SpanContext::trace_id(span_ctx)))
  assert_eq(rich_log.span_id, Some(SpanContext::span_id(span_ctx)))
  assert_eq(rich_log.timestamp, Some(1735689600000000000L))
  assert_eq(rich_log.observed_timestamp, Some(1735689600000001000L))
  
  // Emit rich log record
  Logger::emit(logger, rich_log)
}

test "logger context correlation with different severity levels" {
  // Test logger context correlation with different severity levels
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "severity.context.logger")
  
  // Create span context
  let span_ctx = SpanContext::new("trace-severity-001", "span-severity-001", true, "")
  let trace_id = SpanContext::trace_id(span_ctx)
  let span_id = SpanContext::span_id(span_ctx)
  
  // Create context
  let ctx = Context::root()
  let operation_key = ContextKey::new("operation.id")
  let ctx_with_operation = Context::with_value(ctx, operation_key, "op-98765")
  
  // Create logs with different severity levels
  let trace_log = LogRecord::new_with_context(
    Trace,
    Some("Detailed trace information"),
    None,
    Some(1735689600000000000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_operation)
  )
  
  let debug_log = LogRecord::new_with_context(
    Debug,
    Some("Debug information"),
    None,
    Some(1735689600000001000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_operation)
  )
  
  let info_log = LogRecord::new_with_context(
    Info,
    Some("General information"),
    None,
    Some(1735689600000002000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_operation)
  )
  
  let warn_log = LogRecord::new_with_context(
    Warn,
    Some("Warning condition"),
    None,
    Some(1735689600000003000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_operation)
  )
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Error occurred"),
    None,
    Some(1735689600000004000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_operation)
  )
  
  let fatal_log = LogRecord::new_with_context(
    Fatal,
    Some("Fatal error"),
    None,
    Some(1735689600000005000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_operation)
  )
  
  // Verify all logs have the same context correlation
  assert_eq(trace_log.trace_id, Some(trace_id))
  assert_eq(debug_log.trace_id, Some(trace_id))
  assert_eq(info_log.trace_id, Some(trace_id))
  assert_eq(warn_log.trace_id, Some(trace_id))
  assert_eq(error_log.trace_id, Some(trace_id))
  assert_eq(fatal_log.trace_id, Some(trace_id))
  
  assert_eq(trace_log.span_id, Some(span_id))
  assert_eq(debug_log.span_id, Some(span_id))
  assert_eq(info_log.span_id, Some(span_id))
  assert_eq(warn_log.span_id, Some(span_id))
  assert_eq(error_log.span_id, Some(span_id))
  assert_eq(fatal_log.span_id, Some(span_id))
  
  assert_eq(trace_log.context, Some(ctx_with_operation))
  assert_eq(debug_log.context, Some(ctx_with_operation))
  assert_eq(info_log.context, Some(ctx_with_operation))
  assert_eq(warn_log.context, Some(ctx_with_operation))
  assert_eq(error_log.context, Some(ctx_with_operation))
  assert_eq(fatal_log.context, Some(ctx_with_operation))
  
  // Emit all logs
  Logger::emit(logger, trace_log)
  Logger::emit(logger, debug_log)
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, fatal_log)
}