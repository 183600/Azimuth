test "sdk logs provider functionality" {
  // Test SDK logs provider functionality and integration
  
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "sdk.test.logger", Some("1.0.0"))
  
  // Test logger creation and properties
  assert_true(logger != @azimuth.telemetry.sdk.logs.Logger::{})
  
  let scope = @azimuth.telemetry.sdk.logs.Logger.instrumentation_scope(logger)
  assert_eq(scope.name(), "sdk.test.logger")
  assert_eq(scope.version(), Some("1.0.0"))
  
  // Test basic log emission
  @azimuth.telemetry.sdk.logs.Logger.trace(logger, "SDK trace message")
  @azimuth.telemetry.sdk.logs.Logger.debug(logger, "SDK debug message")
  @azimuth.telemetry.sdk.logs.Logger.info(logger, "SDK info message")
  @azimuth.telemetry.sdk.logs.Logger.warn(logger, "SDK warn message")
  @azimuth.telemetry.sdk.logs.Logger.error(logger, "SDK error message")
  @azimuth.telemetry.sdk.logs.Logger.fatal(logger, "SDK fatal message")
  
  // Test log with attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "component", @azimuth.telemetry.api.common.StringValue("sdk_auth"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "user.id", @azimuth.telemetry.api.common.StringValue("12345"))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_attributes(logger, "User authenticated via SDK", attrs)
  
  // Test log with context
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    ""
  )
  let ctx_with_span = @azimuth.telemetry.api.context.Context.with_span_context(ctx, span_ctx)
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(logger, ctx_with_span, "SDK operation completed")
}

test "sdk logs with structured data" {
  // Test SDK logs with structured data and complex attributes
  
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "structured.test.logger")
  
  // Create comprehensive attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.value", @azimuth.telemetry.api.common.StringValue("test_string"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.value", @azimuth.telemetry.api.common.IntValue(42))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.value", @azimuth.telemetry.api.common.FloatValue(3.14159))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "bool.value", @azimuth.telemetry.api.common.BoolValue(true))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.string", @azimuth.telemetry.api.common.ArrayStringValue(["a", "b", "c"]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.int", @azimuth.telemetry.api.common.ArrayIntValue([1, 2, 3]))
  
  // Create log record with structured data
  let log_record = @azimuth.telemetry.sdk.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Structured log message from SDK",
    Some(attrs),
    Some(1735689600000000000L),
    None,
    Some("trace123"),
    Some("span456"),
    Some(@azimuth.telemetry.api.context.Context.root())
  )
  
  // Emit the log record
  @azimuth.telemetry.sdk.logs.Logger.emit(logger, log_record)
  
  // Verify log record properties
  assert_eq(log_record.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Info)
  assert_eq(log_record.body(), Some("Structured log message from SDK"))
  assert_eq(log_record.timestamp(), Some(1735689600000000000L))
  assert_eq(log_record.trace_id(), Some("trace123"))
  assert_eq(log_record.span_id(), Some("span456"))
}

test "sdk logs with exception handling" {
  // Test SDK logs with exception handling and error scenarios
  
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "exception.test.logger")
  
  // Create exception attributes
  let exception_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(exception_attrs, "exception.type", @azimuth.telemetry.api.common.StringValue("SDKException"))
  @azimuth.telemetry.api.common.Attributes.set(exception_attrs, "exception.message", @azimuth.telemetry.api.common.StringValue("SDK operation failed"))
  @azimuth.telemetry.api.common.Attributes.set(exception_attrs, "exception.stacktrace", @azimuth.telemetry.api.common.StringValue("at sdk.test.function()\n  at main()"))
  @azimuth.telemetry.api.common.Attributes.set(exception_attrs, "error.code", @azimuth.telemetry.api.common.IntValue(500))
  
  // Log error with exception details
  let error_record = @azimuth.telemetry.sdk.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Error,
    "SDK operation failed with exception",
    Some(exception_attrs),
    Some(@azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())),
    None,
    Some("error_trace_123"),
    Some("error_span_456"),
    None
  )
  
  @azimuth.telemetry.sdk.logs.Logger.emit(logger, error_record)
  
  // Test different exception scenarios
  let network_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(network_attrs, "exception.type", @azimuth.telemetry.api.common.StringValue("NetworkException"))
  @azimuth.telemetry.api.common.Attributes.set(network_attrs, "retry.count", @azimuth.telemetry.api.common.IntValue(3))
  @azimuth.telemetry.api.common.Attributes.set(network_attrs, "timeout.ms", @azimuth.telemetry.api.common.IntValue(5000))
  
  @azimuth.telemetry.sdk.logs.Logger.error_with_attributes(logger, "Network operation failed", network_attrs)
  
  let validation_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(validation_attrs, "exception.type", @azimuth.telemetry.api.common.StringValue("ValidationException"))
  @azimuth.telemetry.api.common.Attributes.set(validation_attrs, "field.name", @azimuth.telemetry.api.common.StringValue("user_email"))
  @azimuth.telemetry.api.common.Attributes.set(validation_attrs, "validation.error", @azimuth.telemetry.api.common.StringValue("Invalid email format"))
  
  @azimuth.telemetry.sdk.logs.Logger.warn_with_attributes(logger, "Input validation failed", validation_attrs)
}

test "sdk logs performance and batching" {
  // Test SDK logs performance and batching functionality
  
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "performance.test.logger")
  
  // Test batch log emission
  let log_records = []
  
  // Create multiple log records
  for i in 0..100 {
    let attrs = @azimuth.telemetry.api.common.Attributes.new()
    @azimuth.telemetry.api.common.Attributes.set(attrs, "batch.id", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.common.Attributes.set(attrs, "iteration", @azimuth.telemetry.api.common.StringValue("batch_" + i.to_string()))
    
    let record = @azimuth.telemetry.sdk.logs.LogRecord.new(
      @azimuth.telemetry.api.logs.SeverityNumber.Info,
      "Batch log message " + i.to_string(),
      Some(attrs),
      Some(1735689600000000000L + i.to_int64()),
      None,
      Some("batch_trace_" + i.to_string()),
      Some("batch_span_" + i.to_string()),
      None
    )
    
    log_records = log_records @ [record]
  }
  
  // Emit batch logs
  for record in log_records {
    @azimuth.telemetry.sdk.logs.Logger.emit(logger, record)
  }
  
  // Test high-frequency logging
  let start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  for i in 0..1000 {
    @azimuth.telemetry.sdk.logs.Logger.debug(logger, "High frequency log " + i.to_string())
  }
  
  let end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let duration = end_time - start_time
  
  // Log performance metrics
  let perf_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "log.count", @azimuth.telemetry.api.common.IntValue(1000))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "duration.ns", @azimuth.telemetry.api.common.IntValue(duration.to_int()))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_attributes(logger, "Performance test completed", perf_attrs)
}

test "sdk logs configuration and providers" {
  // Test SDK logs configuration and different provider types
  
  // Test default provider
  let default_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let default_logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(default_provider, "default.logger")
  
  // Test noop provider
  let noop_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.noop()
  let noop_logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(noop_provider, "noop.logger")
  
  // Both should create valid loggers
  assert_true(default_logger != @azimuth.telemetry.sdk.logs.Logger::{})
  assert_true(noop_logger != @azimuth.telemetry.sdk.logs.Logger::{})
  
  // Test logger with different configurations
  let configured_logger1 = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(
    default_provider, 
    "configured.logger", 
    Some("2.0.0"),
    Some("https://example.com/schema/v1")
  )
  
  let configured_logger2 = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(
    default_provider, 
    "configured.logger", 
    Some("2.0.0"),
    Some("https://example.com/schema/v1")
  )
  
  // Same configuration should return same logger
  assert_true(configured_logger1 == configured_logger2)
  
  // Test logger instrumentation scope
  let scope1 = @azimuth.telemetry.sdk.logs.Logger.instrumentation_scope(configured_logger1)
  assert_eq(scope1.name(), "configured.logger")
  assert_eq(scope1.version(), Some("2.0.0"))
  assert_eq(scope1.schema_url(), Some("https://example.com/schema/v1"))
  
  // Test logging with different loggers
  @azimuth.telemetry.sdk.logs.Logger.info(default_logger, "Message from default logger")
  @azimuth.telemetry.sdk.logs.Logger.info(noop_logger, "Message from noop logger")
  @azimuth.telemetry.sdk.logs.Logger.info(configured_logger1, "Message from configured logger")
}