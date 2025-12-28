test "logger provider and logger creation" {
  // Test logger provider and logger creation
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger1 = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  let logger2 = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(
    logger_provider, 
    "test.logger", 
    Some("1.0.0"),
    Some("https://example.com/schema")
  )
  
  // Same name should return same logger instance (for noop provider)
  assert_true(logger1 == logger2)
  
  // Different names should return different loggers
  let logger3 = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "other.logger")
  assert_true(logger1 != logger3)
  
  // Test logger instrumentation scope
  let scope = @azimuth.telemetry.api.logs.Logger.instrumentation_scope(logger1)
  assert_eq(scope.name(), "test.logger")
}

test "log record creation and basic properties" {
  // Test log record creation and basic properties
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Create a basic log record
  let log_record = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Test log message",
    None,  // no attributes
    None,  // no timestamp
    None,  // no observed timestamp
    None,  // no trace context
    None,  // no span context
    None   // no context
  )
  
  assert_eq(log_record.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Info)
  assert_eq(log_record.severity_text(), Some("INFO"))
  assert_eq(log_record.body(), Some("Test log message"))
  assert_true(log_record.attributes() == None || log_record.attributes().unwrap().length() == 0)
}

test "log record with attributes" {
  // Test log record with attributes
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Create attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "user.id", @azimuth.telemetry.api.common.StringValue("12345"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "request.id", @azimuth.telemetry.api.common.StringValue("req-abcdef"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "duration_ms", @azimuth.telemetry.api.common.IntValue(250))
  
  // Create log record with attributes
  let log_record = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Warn,
    "Request processing completed with warnings",
    Some(attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(log_record.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Warn)
  assert_eq(log_record.severity_text(), Some("WARN"))
  assert_eq(log_record.body(), Some("Request processing completed with warnings"))
  
  let record_attrs = log_record.attributes()
  assert_true(record_attrs != None)
  let attrs_map = record_attrs.unwrap()
  
  assert_eq(@azimuth.telemetry.api.common.Attributes.get(attrs_map, "user.id"), 
           Some(@azimuth.telemetry.api.common.StringValue("12345")))
  assert_eq(@azimuth.telemetry.api.common.Attributes.get(attrs_map, "request.id"), 
           Some(@azimuth.telemetry.api.common.StringValue("req-abcdef")))
  assert_eq(@azimuth.telemetry.api.common.Attributes.get(attrs_map, "duration_ms"), 
           Some(@azimuth.telemetry.api.common.IntValue(250)))
}

test "log record with trace context" {
  // Test log record with trace context
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Create context with span information
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE"
  )
  let ctx_with_span = @azimuth.telemetry.api.context.Context.with_span_context(ctx, span_ctx)
  
  // Create log record with trace context
  let log_record = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Error,
    "Database connection failed",
    None,
    None,
    None,
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(ctx_with_span)
  )
  
  assert_eq(log_record.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Error)
  assert_eq(log_record.severity_text(), Some("ERROR"))
  assert_eq(log_record.trace_id(), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(log_record.span_id(), Some("b7ad6b7169203331"))
  assert_eq(log_record.body(), Some("Database connection failed"))
}

test "log severity levels" {
  // Test different log severity levels
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Test all severity levels
  let severity_levels = [
    (@azimuth.telemetry.api.logs.SeverityNumber.Trace, "TRACE"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Debug, "DEBUG"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Info, "INFO"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Warn, "WARN"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Error, "ERROR"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Fatal, "FATAL")
  ]
  
  for (severity, text) in severity_levels {
    let log_record = @azimuth.telemetry.api.logs.LogRecord.new(
      severity,
      "Test message with ${text} severity",
      None,
      None,
      None,
      None,
      None,
      None
    )
    
    assert_eq(log_record.severity_number(), severity)
    assert_eq(log_record.severity_text(), Some(text))
  }
}

test "log emission with different methods" {
  // Test different log emission methods
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Test different log methods
  @azimuth.telemetry.api.logs.Logger.trace(logger, "Trace level message")
  @azimuth.telemetry.api.logs.Logger.debug(logger, "Debug level message")
  @azimuth.telemetry.api.logs.Logger.info(logger, "Info level message")
  @azimuth.telemetry.api.logs.Logger.warn(logger, "Warning level message")
  @azimuth.telemetry.api.logs.Logger.error(logger, "Error level message")
  @azimuth.telemetry.api.logs.Logger.fatal(logger, "Fatal level message")
  
  // Test log with attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "component", @azimuth.telemetry.api.common.StringValue("auth"))
  
  @azimuth.telemetry.api.logs.Logger.info_with_attributes(logger, "User authenticated successfully", attrs)
  
  // Test log with context
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    ""
  )
  let ctx_with_span = @azimuth.telemetry.api.context.Context.with_span_context(ctx, span_ctx)
  
  @azimuth.telemetry.api.logs.Logger.info_with_context(logger, ctx_with_span, "Operation completed")
}

test "log record timestamps" {
  // Test log record with timestamps
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Get current time
  let current_time = @azimuth.telemetry.sdk.platform.time.now_unix_nanos()
  
  // Create log record with explicit timestamp
  let log_record = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Message with timestamp",
    None,
    Some(current_time),
    None,
    None,
    None,
    None
  )
  
  assert_eq(log_record.timestamp_unix_nano(), Some(current_time))
  
  // Create log record without timestamp (should use observed time)
  let log_record_auto = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Message with auto timestamp",
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  // Should have observed timestamp
  assert_true(log_record_auto.observed_timestamp_unix_nano() != None)
  assert_true(log_record_auto.observed_timestamp_unix_nano().unwrap() > 0)
}

test "log record with exception" {
  // Test log record with exception information
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Create attributes with exception information
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "exception.type", @azimuth.telemetry.api.common.StringValue("DatabaseException"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "exception.message", @azimuth.telemetry.api.common.StringValue("Connection timeout"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "exception.stacktrace", @azimuth.telemetry.api.common.StringValue("at database.connect()\n  at main()"))
  
  // Create log record with exception
  let log_record = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Error,
    "Failed to connect to database",
    Some(attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(log_record.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Error)
  assert_eq(log_record.body(), Some("Failed to connect to database"))
  
  let record_attrs = log_record.attributes()
  assert_true(record_attrs != None)
  let attrs_map = record_attrs.unwrap()
  
  assert_eq(@azimuth.telemetry.api.common.Attributes.get(attrs_map, "exception.type"), 
           Some(@azimuth.telemetry.api.common.StringValue("DatabaseException")))
  assert_eq(@azimuth.telemetry.api.common.Attributes.get(attrs_map, "exception.message"), 
           Some(@azimuth.telemetry.api.common.StringValue("Connection timeout")))
}