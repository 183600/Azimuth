test "logger provider and logger creation" {
  // Test logger provider and logger creation
  let logger_provider = telemetry.api.logs.LoggerProvider.noop()
  let logger1 = telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  let logger2 = telemetry.api.logs.LoggerProvider.get_logger(
    logger_provider, 
    "test.logger", 
    Some("1.0.0"),
    Some("https://example.com/schema")
  )
  
  // Same name should return same logger instance (for noop provider)
  assert_true(logger1 == logger2)
  
  // Different names should return different loggers
  let logger3 = telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "other.logger")
  assert_true(logger1 != logger3)
  
  // Test logger instrumentation scope
  let scope = telemetry.api.logs.Logger.instrumentation_scope(logger1)
  assert_eq(scope.name(), "test.logger")
}

test "log record creation and basic properties" {
  // Test log record creation and basic properties
  let logger_provider = telemetry.api.logs.LoggerProvider.noop()
  let logger = telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Create a basic log record
  let log_record = telemetry.api.logs.LogRecord.new(
    telemetry.api.logs.SeverityNumber.Info,
    "Test log message",
    None,  // no attributes
    None,  // no timestamp
    None,  // no observed timestamp
    None,  // no trace context
    None,  // no span context
    None   // no context
  )
  
  assert_eq(log_record.severity_number(), telemetry.api.logs.SeverityNumber.Info)
  assert_eq(log_record.severity_text(), Some("INFO"))
  assert_eq(log_record.body(), Some("Test log message"))
  assert_true(log_record.attributes() == None || log_record.attributes().unwrap().length() == 0)
}

test "log record with attributes" {
  // Test log record with attributes
  let logger_provider = telemetry.api.logs.LoggerProvider.noop()
  let logger = telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Create attributes
  let attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(attrs, "user.id", telemetry.api.common.StringValue("12345"))
  telemetry.api.common.Attributes.set(attrs, "request.id", telemetry.api.common.StringValue("req-abcdef"))
  telemetry.api.common.Attributes.set(attrs, "duration_ms", telemetry.api.common.IntValue(250))
  
  // Create log record with attributes
  let log_record = telemetry.api.logs.LogRecord.new(
    telemetry.api.logs.SeverityNumber.Warn,
    "Request processing completed with warnings",
    Some(attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(log_record.severity_number(), telemetry.api.logs.SeverityNumber.Warn)
  assert_eq(log_record.severity_text(), Some("WARN"))
  assert_eq(log_record.body(), Some("Request processing completed with warnings"))
  
  let record_attrs = log_record.attributes()
  assert_true(record_attrs != None)
  let attrs_map = record_attrs.unwrap()
  
  assert_eq(telemetry.api.common.Attributes.get(attrs_map, "user.id"), 
           Some(telemetry.api.common.StringValue("12345")))
  assert_eq(telemetry.api.common.Attributes.get(attrs_map, "request.id"), 
           Some(telemetry.api.common.StringValue("req-abcdef")))
  assert_eq(telemetry.api.common.Attributes.get(attrs_map, "duration_ms"), 
           Some(telemetry.api.common.IntValue(250)))
}

test "log record with trace context" {
  // Test log record with trace context
  let logger_provider = telemetry.api.logs.LoggerProvider.noop()
  let logger = telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Create context with span information
  let ctx = telemetry.api.context.Context.root()
  let span_ctx = telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE"
  )
  let ctx_with_span = telemetry.api.context.Context.with_span_context(ctx, span_ctx)
  
  // Create log record with trace context
  let log_record = telemetry.api.logs.LogRecord.new(
    telemetry.api.logs.SeverityNumber.Error,
    "Database connection failed",
    None,
    None,
    None,
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(ctx_with_span)
  )
  
  assert_eq(log_record.severity_number(), telemetry.api.logs.SeverityNumber.Error)
  assert_eq(log_record.severity_text(), Some("ERROR"))
  assert_eq(log_record.trace_id(), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(log_record.span_id(), Some("b7ad6b7169203331"))
  assert_eq(log_record.body(), Some("Database connection failed"))
}

test "log severity levels" {
  // Test different log severity levels
  let logger_provider = telemetry.api.logs.LoggerProvider.noop()
  let logger = telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Test all severity levels
  let severity_levels = [
    (telemetry.api.logs.SeverityNumber.Trace, "TRACE"),
    (telemetry.api.logs.SeverityNumber.Debug, "DEBUG"),
    (telemetry.api.logs.SeverityNumber.Info, "INFO"),
    (telemetry.api.logs.SeverityNumber.Warn, "WARN"),
    (telemetry.api.logs.SeverityNumber.Error, "ERROR"),
    (telemetry.api.logs.SeverityNumber.Fatal, "FATAL")
  ]
  
  for (severity, text) in severity_levels {
    let log_record = telemetry.api.logs.LogRecord.new(
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
  let logger_provider = telemetry.api.logs.LoggerProvider.noop()
  let logger = telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Test different log methods
  telemetry.api.logs.Logger.trace(logger, "Trace level message")
  telemetry.api.logs.Logger.debug(logger, "Debug level message")
  telemetry.api.logs.Logger.info(logger, "Info level message")
  telemetry.api.logs.Logger.warn(logger, "Warning level message")
  telemetry.api.logs.Logger.error(logger, "Error level message")
  telemetry.api.logs.Logger.fatal(logger, "Fatal level message")
  
  // Test log with attributes
  let attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(attrs, "component", telemetry.api.common.StringValue("auth"))
  
  telemetry.api.logs.Logger.info_with_attributes(logger, "User authenticated successfully", attrs)
  
  // Test log with context
  let ctx = telemetry.api.context.Context.root()
  let span_ctx = telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    ""
  )
  let ctx_with_span = telemetry.api.context.Context.with_span_context(ctx, span_ctx)
  
  telemetry.api.logs.Logger.info_with_context(logger, ctx_with_span, "Operation completed")
}

test "log record timestamps" {
  // Test log record with timestamps
  let logger_provider = telemetry.api.logs.LoggerProvider.noop()
  let logger = telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Get current time
  let current_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  // Create log record with explicit timestamp
  let log_record = telemetry.api.logs.LogRecord.new(
    telemetry.api.logs.SeverityNumber.Info,
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
  let log_record_auto = telemetry.api.logs.LogRecord.new(
    telemetry.api.logs.SeverityNumber.Info,
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
  let logger_provider = telemetry.api.logs.LoggerProvider.noop()
  let logger = telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Create attributes with exception information
  let attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(attrs, "exception.type", telemetry.api.common.StringValue("DatabaseException"))
  telemetry.api.common.Attributes.set(attrs, "exception.message", telemetry.api.common.StringValue("Connection timeout"))
  telemetry.api.common.Attributes.set(attrs, "exception.stacktrace", telemetry.api.common.StringValue("at database.connect()\n  at main()"))
  
  // Create log record with exception
  let log_record = telemetry.api.logs.LogRecord.new(
    telemetry.api.logs.SeverityNumber.Error,
    "Failed to connect to database",
    Some(attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(log_record.severity_number(), telemetry.api.logs.SeverityNumber.Error)
  assert_eq(log_record.body(), Some("Failed to connect to database"))
  
  let record_attrs = log_record.attributes()
  assert_true(record_attrs != None)
  let attrs_map = record_attrs.unwrap()
  
  assert_eq(telemetry.api.common.Attributes.get(attrs_map, "exception.type"), 
           Some(telemetry.api.common.StringValue("DatabaseException")))
  assert_eq(telemetry.api.common.Attributes.get(attrs_map, "exception.message"), 
           Some(telemetry.api.common.StringValue("Connection timeout")))
}

test "logger provider default creation" {
  // Test default logger provider creation
  let logger_provider = telemetry.api.logs.LoggerProvider.default()
  let logger = telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "default.test.logger")
  
  // Test that logger can be created and has correct scope
  let scope = telemetry.api.logs.Logger.instrumentation_scope(logger)
  assert_eq(scope.name(), "default.test.logger")
  assert_eq(scope.version(), None)
  assert_eq(scope.schema_url(), None)
}

test "log record with empty body" {
  // Test log record creation with empty body
  let logger_provider = telemetry.api.logs.LoggerProvider.noop()
  let logger = telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Create log record with empty body
  let log_record = telemetry.api.logs.LogRecord.new(
    telemetry.api.logs.SeverityNumber.Info,
    "",
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(log_record.severity_number(), telemetry.api.logs.SeverityNumber.Info)
  assert_eq(log_record.body(), Some(""))
}

test "log record complex attribute types" {
  // Test log record with complex attribute types
  let logger_provider = telemetry.api.logs.LoggerProvider.noop()
  let logger = telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  // Create attributes with different value types
  let attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(attrs, "string.value", telemetry.api.common.StringValue("test"))
  telemetry.api.common.Attributes.set(attrs, "int.value", telemetry.api.common.IntValue(42))
  telemetry.api.common.Attributes.set(attrs, "float.value", telemetry.api.common.FloatValue(3.14))
  telemetry.api.common.Attributes.set(attrs, "bool.value", telemetry.api.common.BoolValue(true))
  telemetry.api.common.Attributes.set(attrs, "array.string", telemetry.api.common.ArrayStringValue(["a", "b", "c"]))
  
  // Create log record with complex attributes
  let log_record = telemetry.api.logs.LogRecord.new_with_context(
    telemetry.api.logs.SeverityNumber.Debug,
    Some("Complex attribute test"),
    Some(attrs),
    Some(1735689600000000000L),  // fixed timestamp
    Some(1735689600000000001L),  // fixed observed timestamp
    Some("trace123"),
    Some("span456"),
    Some(telemetry.api.context.Context.root())
  )
  
  assert_eq(log_record.severity_number(), telemetry.api.logs.SeverityNumber.Debug)
  assert_eq(log_record.body(), Some("Complex attribute test"))
  assert_eq(log_record.timestamp(), Some(1735689600000000000L))
  assert_eq(log_record.observed_timestamp(), Some(1735689600000000001L))
  assert_eq(log_record.trace_id(), Some("trace123"))
  assert_eq(log_record.span_id(), Some("span456"))
}