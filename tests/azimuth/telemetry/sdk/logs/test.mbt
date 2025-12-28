test "sdk logs provider functionality" {
  // Test SDK logs provider functionality
  
  let logs_provider = telemetry.sdk.logs.LoggerProvider.default()
  let logger = telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "sdk.test.logger", Some("1.0.0"))
  
  // Test logger creation and properties
  assert_true(logger != telemetry.sdk.logs.Logger::{})
  
  let scope = telemetry.sdk.logs.Logger.instrumentation_scope(logger)
  assert_eq(scope.name(), "sdk.test.logger")
  assert_eq(scope.version(), Some("1.0.0"))
  
  // Test log emission
  telemetry.sdk.logs.Logger.info(logger, "SDK test info message")
  telemetry.sdk.logs.Logger.warn(logger, "SDK test warning message")
  telemetry.sdk.logs.Logger.error(logger, "SDK test error message")
  
  // Test log with attributes
  let attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(attrs, "component", telemetry.api.common.StringValue("sdk"))
  telemetry.api.common.Attributes.set(attrs, "version", telemetry.api.common.StringValue("1.0.0"))
  
  telemetry.sdk.logs.Logger.info_with_attributes(logger, "SDK message with attributes", attrs)
}

test "sdk logs with context and trace correlation" {
  // Test SDK logs with context and trace correlation
  
  let logs_provider = telemetry.sdk.logs.LoggerProvider.default()
  let logger = telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "context.test.logger")
  
  // Create trace context
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "log.trace.test")
  let ctx = telemetry.api.context.Context.root()
  let (ctx_with_span, span) = telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "operation.span")
  
  // Log with trace context
  telemetry.sdk.logs.Logger.info_with_context(logger, ctx_with_span, "Operation started")
  telemetry.sdk.logs.Logger.debug_with_context(logger, ctx_with_span, "Processing data")
  
  // Add events to span and log
  telemetry.api.trace.Span.add_event(span, "log.correlation.test", None, None)
  telemetry.sdk.logs.Logger.info_with_context(logger, ctx_with_span, "Event logged to span")
  
  // Create log record with explicit trace context
  let log_record = telemetry.sdk.logs.LogRecord.new(
    telemetry.api.logs.SeverityNumber.Info,
    "Explicit trace context log",
    None,
    None,
    None,
    Some(telemetry.api.trace.SpanContext.trace_id(telemetry.api.trace.Span.span_context(span))),
    Some(telemetry.api.trace.SpanContext.span_id(telemetry.api.trace.Span.span_context(span))),
    Some(ctx_with_span)
  )
  
  telemetry.sdk.logs.Logger.emit(logger, log_record)
  
  telemetry.api.trace.Span.end(span)
  telemetry.sdk.logs.Logger.info_with_context(logger, ctx_with_span, "Operation completed")
}

test "sdk logs with different severity levels and structured data" {
  // Test SDK logs with different severity levels and structured data
  
  let logs_provider = telemetry.sdk.logs.LoggerProvider.default()
  let logger = telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "severity.test.logger")
  
  // Test all severity levels
  telemetry.sdk.logs.Logger.trace(logger, "Trace level message")
  telemetry.sdk.logs.Logger.debug(logger, "Debug level message")
  telemetry.sdk.logs.Logger.info(logger, "Info level message")
  telemetry.sdk.logs.Logger.warn(logger, "Warning level message")
  telemetry.sdk.logs.Logger.error(logger, "Error level message")
  telemetry.sdk.logs.Logger.fatal(logger, "Fatal level message")
  
  // Test structured logging with complex attributes
  let user_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(user_attrs, "user.id", telemetry.api.common.StringValue("12345"))
  telemetry.api.common.Attributes.set(user_attrs, "user.role", telemetry.api.common.StringValue("admin"))
  telemetry.api.common.Attributes.set(user_attrs, "user.session", telemetry.api.common.StringValue("sess_abcdef"))
  
  telemetry.sdk.logs.Logger.info_with_attributes(
    logger, 
    "User action performed", 
    user_attrs
  )
  
  // Test exception logging
  let exception_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(exception_attrs, "exception.type", telemetry.api.common.StringValue("SDKTestException"))
  telemetry.api.common.Attributes.set(exception_attrs, "exception.message", telemetry.api.common.StringValue("SDK test exception"))
  telemetry.api.common.Attributes.set(exception_attrs, "exception.stack", telemetry.api.common.StringValue("at sdk.test\n  at test_function()"))
  
  telemetry.sdk.logs.Logger.error_with_attributes(
    logger,
    "Exception occurred in SDK",
    exception_attrs
  )
  
  // Test performance logging
  let perf_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(perf_attrs, "operation.name", telemetry.api.common.StringValue("database.query"))
  telemetry.api.common.Attributes.set(perf_attrs, "operation.duration_ms", telemetry.api.common.FloatValue(125.5))
  telemetry.api.common.Attributes.set(perf_attrs, "operation.success", telemetry.api.common.BoolValue(true))
  
  telemetry.sdk.logs.Logger.info_with_attributes(
    logger,
    "Performance metric recorded",
    perf_attrs
  )
}