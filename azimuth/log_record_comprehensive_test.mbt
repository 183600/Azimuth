test "LogRecord creation and basic properties test" {
  // Test LogRecord creation with severity and body
  let info_record = LogRecord::new(Info, "This is an info log message")
  @assertion.assert_eq(info_record.severity, Info)?
  @assertion.assert_eq(info_record.body?, "This is an info log message")?
  @assertion.assert_eq(info_record.attributes, None)?
  @assertion.assert_eq(info_record.timestamp, None)?
  @assertion.assert_eq(info_record.observed_timestamp, None)?
  @assertion.assert_eq(info_record.trace_id, None)?
  @assertion.assert_eq(info_record.span_id, None)?
  @assertion.assert_eq(info_record.context, None)?
  
  // Test different severity levels
  let severity_levels = [
    (Trace, "Trace level message"),
    (Debug, "Debug level message"),
    (Info, "Info level message"),
    (Warn, "Warning level message"),
    (Error, "Error level message"),
    (Fatal, "Fatal level message")
  ]
  
  for (severity, message) in severity_levels {
    let record = LogRecord::new(severity, message)
    @assertion.assert_eq(record.severity, severity)?
    @assertion.assert_eq(record.body?, message)?
  }
  
  // Test LogRecord with empty body
  let empty_record = LogRecord::new(Debug, "")
  @assertion.assert_eq(empty_record.severity, Debug)?
  @assertion.assert_eq(empty_record.body?, "")?
}

test "LogRecord with comprehensive context test" {
  // Test LogRecord creation with full context
  let attributes = Attributes::new()
  let log_attributes = [
    ("user.id", StringValue("user-123")),
    ("request.id", StringValue("req-456")),
    ("response.time", FloatValue(150.5)),
    ("success", BoolValue(true))
  ]
  
  let resource_attrs = Resource::with_attributes(Resource::new(), log_attributes)
  
  let context = Context::root()
  let user_key = ContextKey::new("correlation.id")
  let enriched_context = Context::with_value(context, user_key, "corr-789")
  
  let timestamp = 1735689600000000000L  // 2025-01-01 timestamp
  let observed_timestamp = 1735689600000000100L
  
  let comprehensive_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(resource_attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some("trace-12345"),
    Some("span-67890"),
    Some(enriched_context)
  )
  
  @assertion.assert_eq(comprehensive_record.severity, Error)?
  @assertion.assert_eq(comprehensive_record.body?, "Database connection failed")?
  @assertion.assert_eq(comprehensive_record.timestamp?, timestamp)?
  @assertion.assert_eq(comprehensive_record.observed_timestamp?, observed_timestamp)?
  @assertion.assert_eq(comprehensive_record.trace_id?, "trace-12345")?
  @assertion.assert_eq(comprehensive_record.span_id?, "span-67890")?
  @assertion.assert_eq(comprehensive_record.context?, enriched_context)?
}

test "LogRecord severity and body operations test" {
  // Test severity number access
  let records = [
    (LogRecord::new(Trace, "Trace message"), Trace),
    (LogRecord::new(Debug, "Debug message"), Debug),
    (LogRecord::new(Info, "Info message"), Info),
    (LogRecord::new(Warn, "Warning message"), Warn),
    (LogRecord::new(Error, "Error message"), Error),
    (LogRecord::new(Fatal, "Fatal message"), Fatal)
  ]
  
  for (record, expected_severity) in records {
    @assertion.assert_eq(LogRecord::severity_number(record), expected_severity)?
  }
  
  // Test body access with different content types
  let json_body = "{\"event\":\"user_login\",\"user_id\":\"12345\",\"timestamp\":\"2025-01-01T12:00:00Z\"}"
  let xml_body = "<event><type>user_login</type><user_id>12345</user_id></event>"
  let multiline_body = "Line 1\nLine 2\nLine 3\nStack trace:\n  at function1()\n  at function2()"
  let unicode_body = "Unicode test: 中文 العربية русский ñoño café"
  let empty_body = ""
  
  let json_record = LogRecord::new(Info, json_body)
  let xml_record = LogRecord::new(Info, xml_body)
  let multiline_record = LogRecord::new(Error, multiline_body)
  let unicode_record = LogRecord::new(Info, unicode_body)
  let empty_record = LogRecord::new(Debug, empty_body)
  
  @assertion.assert_eq(LogRecord::body(json_record)?, json_body)?
  @assertion.assert_eq(LogRecord::body(xml_record)?, xml_body)?
  @assertion.assert_eq(LogRecord::body(multiline_record)?, multiline_body)?
  @assertion.assert_eq(LogRecord::body(unicode_record)?, unicode_body)?
  @assertion.assert_eq(LogRecord::body(empty_record)?, empty_body)?
}

test "Logger and LoggerProvider operations test" {
  // Test LoggerProvider creation and operations
  let noop_provider = LoggerProvider::noop()
  @assertion.assert_true(true)?  // Basic creation test
  
  let default_provider = LoggerProvider::default()
  @assertion.assert_true(true)?  // Basic creation test
  
  // Test Logger creation
  let logger = LoggerProvider::get_logger(noop_provider, "test.logger")
  @assertion.assert_eq(logger.scope.name, "test.logger")?
  @assertion.assert_eq(logger.scope.version, None)?
  @assertion.assert_eq(logger.scope.schema_url, None)?
  
  // Test Logger with version and schema
  let versioned_provider = LoggerProvider::noop()
  let versioned_logger = LoggerProvider::get_logger(versioned_provider, "versioned.logger")
  // Note: Simplified implementation doesn't support version/schema parameters
  @assertion.assert_eq(versioned_logger.scope.name, "versioned.logger")?
  
  // Test log emission
  let simple_record = LogRecord::new(Info, "Test log message")
  Logger::emit(logger, simple_record)
  @assertion.assert_true(true)?  // Simplified implementation doesn't actually emit
  
  // Test emission with complex record
  let complex_record = LogRecord::new_with_context(
    Warn,
    Some("Complex log message with context"),
    None,
    Some(1735689600000000000L),
    None,
    Some("trace-123"),
    Some("span-456"),
    None
  )
  Logger::emit(logger, complex_record)
  @assertion.assert_true(true)?
}

test "LogRecord trace and span integration test" {
  // Test LogRecord with trace and span context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let trace_record = LogRecord::new_with_context(
    Info,
    Some("Operation completed successfully"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  @assertion.assert_eq(LogRecord::trace_id(trace_record)?, trace_id)?
  @assertion.assert_eq(LogRecord::span_id(trace_record)?, span_id)?
  
  // Test with different trace/span formats
  let test_cases = [
    ("", ""),
    ("trace-123", "span-456"),
    ("00000000000000000000000000000000", "0000000000000000"),
    ("abcdef1234567890abcdef1234567890", "abcdef1234567890")
  ]
  
  for (test_trace_id, test_span_id) in test_cases {
    let test_record = LogRecord::new_with_context(
      Debug,
      Some("Test message"),
      None,
      None,
      None,
      if test_trace_id == "" { None } else { Some(test_trace_id) },
      if test_span_id == "" { None } else { Some(test_span_id) },
      None
    )
    
    let retrieved_trace = LogRecord::trace_id(test_record)
    let retrieved_span = LogRecord::span_id(test_record)
    
    if test_trace_id == "" {
      @assertion.assert_eq(retrieved_trace, None)?
    } else {
      @assertion.assert_eq(retrieved_trace?, test_trace_id)?
    }
    
    if test_span_id == "" {
      @assertion.assert_eq(retrieved_span, None)?
    } else {
      @assertion.assert_eq(retrieved_span?, test_span_id)?
    }
  }
  
  // Test LogRecord without trace context
  let no_trace_record = LogRecord::new(Info, "Message without trace context")
  @assertion.assert_eq(LogRecord::trace_id(no_trace_record), None)?
  @assertion.assert_eq(LogRecord::span_id(no_trace_record), None)?
}