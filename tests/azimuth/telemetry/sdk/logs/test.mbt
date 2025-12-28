test "log record creation with different severity levels" {
  // Test log record creation across all severity levels
  
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "severity.test")
  
  // Create log records for each severity level
  let trace_log = @azimuth.telemetry.api.logs.LogRecord.new(@azimuth.telemetry.api.logs.SeverityNumber.Trace, "Trace level message")
  let debug_log = @azimuth.telemetry.api.logs.LogRecord.new(@azimuth.telemetry.api.logs.SeverityNumber.Debug, "Debug level message")
  let info_log = @azimuth.telemetry.api.logs.LogRecord.new(@azimuth.telemetry.api.logs.SeverityNumber.Info, "Info level message")
  let warn_log = @azimuth.telemetry.api.logs.LogRecord.new(@azimuth.telemetry.api.logs.SeverityNumber.Warn, "Warning level message")
  let error_log = @azimuth.telemetry.api.logs.LogRecord.new(@azimuth.telemetry.api.logs.SeverityNumber.Error, "Error level message")
  let fatal_log = @azimuth.telemetry.api.logs.LogRecord.new(@azimuth.telemetry.api.logs.SeverityNumber.Fatal, "Fatal level message")
  
  // Verify severity levels
  assert_eq(trace_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Trace)
  assert_eq(debug_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Debug)
  assert_eq(info_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Info)
  assert_eq(warn_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Warn)
  assert_eq(error_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Error)
  assert_eq(fatal_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Fatal)
  
  // Verify log bodies
  assert_eq(trace_log.body(), Some("Trace level message"))
  assert_eq(debug_log.body(), Some("Debug level message"))
  assert_eq(info_log.body(), Some("Info level message"))
  assert_eq(warn_log.body(), Some("Warning level message"))
  assert_eq(error_log.body(), Some("Error level message"))
  assert_eq(fatal_log.body(), Some("Fatal level message"))
  
  // Emit logs through logger
  @azimuth.telemetry.api.logs.Logger.emit(logger, trace_log)
  @azimuth.telemetry.api.logs.Logger.emit(logger, info_log)
  @azimuth.telemetry.api.logs.Logger.emit(logger, warn_log)
  @azimuth.telemetry.api.logs.Logger.emit(logger, error_log)
}

test "log record with context and trace correlation" {
  // Test log record creation with trace and span context correlation
  
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "context.test")
  
  // Create log record with full context
  let contextual_log = @azimuth.telemetry.api.logs.LogRecord.new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    Some("User authentication completed"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(@azimuth.telemetry.api.context.Context.root())
  )
  
  // Verify log record properties
  assert_eq(contextual_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Info)
  assert_eq(contextual_log.body(), Some("User authentication completed"))
  assert_eq(contextual_log.timestamp(), Some(1735689600000000000L))
  assert_eq(contextual_log.observed_timestamp(), Some(1735689600000001L))
  assert_eq(contextual_log.trace_id(), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(contextual_log.span_id(), Some("b7ad6b7169203331"))
  
  // Create log record with minimal context
  let minimal_context_log = @azimuth.telemetry.api.logs.LogRecord.new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber.Debug,
    Some("Debug message"),
    None,
    None,
    None,
    Some("trace12345678901234567890123456789012"),
    None,
    None
  )
  
  // Verify minimal context
  assert_eq(minimal_context_log.trace_id(), Some("trace12345678901234567890123456789012"))
  assert_eq(minimal_context_log.span_id(), None)
  
  // Emit logs
  @azimuth.telemetry.api.logs.Logger.emit(logger, contextual_log)
  @azimuth.telemetry.api.logs.Logger.emit(logger, minimal_context_log)
}

test "logger scope and provider management" {
  // Test logger creation with different scopes and provider configurations
  
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.default()
  
  // Create loggers with different scopes
  let service_logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(
    logger_provider,
    "service.logger",
    Some("1.2.3"),
    Some("https://example.com/schemas/service")
  )
  
  let component_logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(
    logger_provider,
    "component.logger",
    Some("2.0.0"),
    Some("https://example.com/schemas/component")
  )
  
  let minimal_logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "minimal.logger")
  
  // Create log records from different loggers
  let service_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Service started successfully"
  )
  
  let component_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Component initialized"
  )
  
  let minimal_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Debug,
    "Minimal log message"
  )
  
  // Emit logs through different loggers
  @azimuth.telemetry.api.logs.Logger.emit(service_logger, service_log)
  @azimuth.telemetry.api.logs.Logger.emit(component_logger, component_log)
  @azimuth.telemetry.api.logs.Logger.emit(minimal_logger, minimal_log)
  
  // Test log with different severity formats
  let structured_log = @azimuth.telemetry.api.logs.LogRecord.new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber.Warn,
    Some("Structured log with timestamp"),
    None,
    Some(@azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())),
    None,
    None,
    None,
    None
  )
  
  @azimuth.telemetry.api.logs.Logger.emit(service_logger, structured_log)
}

test "log record lifecycle and emission" {
  // Test complete log record lifecycle from creation to emission
  
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "lifecycle.test")
  
  // Create log record at different stages
  let initial_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Process started"
  )
  
  // Create log with processing information
  let processing_log = @azimuth.telemetry.api.logs.LogRecord.new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    Some("Processing user request"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000005L),
    Some("trace12345678901234567890123456789012"),
    Some("span12345678901234"),
    None
  )
  
  // Create completion log
  let completion_log = @azimuth.telemetry.api.logs.LogRecord.new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    Some("Process completed successfully"),
    None,
    Some(1735689600000000010L),
    Some(1735689600000000015L),
    Some("trace12345678901234567890123456789012"),
    Some("span12345678901234"),
    None
  )
  
  // Create error log for failure scenario
  let error_log = @azimuth.telemetry.api.logs.LogRecord.new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber.Error,
    Some("Process failed with exception"),
    None,
    Some(1735689600000000020L),
    Some(1735689600000000025L),
    Some("trace12345678901234567890123456789012"),
    Some("span12345678901234"),
    None
  )
  
  // Emit all logs in sequence
  @azimuth.telemetry.api.logs.Logger.emit(logger, initial_log)
  @azimuth.telemetry.api.logs.Logger.emit(logger, processing_log)
  @azimuth.telemetry.api.logs.Logger.emit(logger, completion_log)
  @azimuth.telemetry.api.logs.Logger.emit(logger, error_log)
  
  // Verify log properties
  assert_eq(initial_log.body(), Some("Process started"))
  assert_eq(processing_log.body(), Some("Processing user request"))
  assert_eq(completion_log.body(), Some("Process completed successfully"))
  assert_eq(error_log.body(), Some("Process failed with exception"))
  
  assert_eq(initial_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Info)
  assert_eq(processing_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Info)
  assert_eq(completion_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Info)
  assert_eq(error_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Error)
}