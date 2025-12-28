test "sdk logs provider functionality" {
  // Test SDK logs provider functionality
  
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "sdk.test.logger", Some("1.0.0"))
  
  // Test logger creation and properties
  assert_true(logger != @azimuth.telemetry.sdk.logs.Logger::{})
  
  let scope = @azimuth.telemetry.sdk.logs.Logger.instrumentation_scope(logger)
  assert_eq(scope.name(), "sdk.test.logger")
  assert_eq(scope.version(), Some("1.0.0"))
  
  // Test log emission
  @azimuth.telemetry.sdk.logs.Logger.info(logger, "SDK test info message")
  @azimuth.telemetry.sdk.logs.Logger.warn(logger, "SDK test warning message")
  @azimuth.telemetry.sdk.logs.Logger.error(logger, "SDK test error message")
  
  // Test log with attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "component", @azimuth.telemetry.api.common.StringValue("sdk"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "version", @azimuth.telemetry.api.common.StringValue("1.0.0"))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_attributes(logger, "SDK message with attributes", attrs)
}

test "sdk logs with context and trace correlation" {
  // Test SDK logs with context and trace correlation
  
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "context.test.logger")
  
  // Create trace context
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "log.trace.test")
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (ctx_with_span, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "operation.span")
  
  // Log with trace context
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(logger, ctx_with_span, "Operation started")
  @azimuth.telemetry.sdk.logs.Logger.debug_with_context(logger, ctx_with_span, "Processing data")
  
  // Add events to span and log
  @azimuth.telemetry.api.trace.Span.add_event(span, "log.correlation.test", None, None)
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(logger, ctx_with_span, "Event logged to span")
  
  // Create log record with explicit trace context
  let log_record = @azimuth.telemetry.sdk.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Explicit trace context log",
    None,
    None,
    None,
    Some(@azimuth.telemetry.api.trace.SpanContext.trace_id(@azimuth.telemetry.api.trace.Span.span_context(span))),
    Some(@azimuth.telemetry.api.trace.SpanContext.span_id(@azimuth.telemetry.api.trace.Span.span_context(span))),
    Some(ctx_with_span)
  )
  
  @azimuth.telemetry.sdk.logs.Logger.emit(logger, log_record)
  
  @azimuth.telemetry.api.trace.Span.end(span)
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(logger, ctx_with_span, "Operation completed")
}

test "sdk logs with different severity levels and structured data" {
  // Test SDK logs with different severity levels and structured data
  
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "severity.test.logger")
  
  // Test all severity levels
  @azimuth.telemetry.sdk.logs.Logger.trace(logger, "Trace level message")
  @azimuth.telemetry.sdk.logs.Logger.debug(logger, "Debug level message")
  @azimuth.telemetry.sdk.logs.Logger.info(logger, "Info level message")
  @azimuth.telemetry.sdk.logs.Logger.warn(logger, "Warning level message")
  @azimuth.telemetry.sdk.logs.Logger.error(logger, "Error level message")
  @azimuth.telemetry.sdk.logs.Logger.fatal(logger, "Fatal level message")
  
  // Test structured logging with complex attributes
  let user_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(user_attrs, "user.id", @azimuth.telemetry.api.common.StringValue("12345"))
  @azimuth.telemetry.api.common.Attributes.set(user_attrs, "user.role", @azimuth.telemetry.api.common.StringValue("admin"))
  @azimuth.telemetry.api.common.Attributes.set(user_attrs, "user.session", @azimuth.telemetry.api.common.StringValue("sess_abcdef"))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_attributes(
    logger, 
    "User action performed", 
    user_attrs
  )
  
  // Test exception logging
  let exception_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(exception_attrs, "exception.type", @azimuth.telemetry.api.common.StringValue("SDKTestException"))
  @azimuth.telemetry.api.common.Attributes.set(exception_attrs, "exception.message", @azimuth.telemetry.api.common.StringValue("SDK test exception"))
  @azimuth.telemetry.api.common.Attributes.set(exception_attrs, "exception.stack", @azimuth.telemetry.api.common.StringValue("at sdk.test\n  at test_function()"))
  
  @azimuth.telemetry.sdk.logs.Logger.error_with_attributes(
    logger,
    "Exception occurred in SDK",
    exception_attrs
  )
  
  // Test performance logging
  let perf_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "operation.name", @azimuth.telemetry.api.common.StringValue("database.query"))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "operation.duration_ms", @azimuth.telemetry.api.common.FloatValue(125.5))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "operation.success", @azimuth.telemetry.api.common.BoolValue(true))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_attributes(
    logger,
    "Performance metric recorded",
    perf_attrs
  )
}