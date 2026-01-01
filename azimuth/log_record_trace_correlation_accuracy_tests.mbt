// Log Record Trace Correlation Accuracy Tests
// Test cases for accurate correlation between log records and trace/span context

test "log_record_basic_creation_with_trace_correlation" {
  // Test basic log record creation with trace correlation
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let log_record = LogRecord::new_with_context(
    SeverityNumber::Info,
    Some("User login successful"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some(trace_id),
    Some(span_id),
    None
  )
  
  // Verify trace correlation fields
  @assertion.assert_eq(LogRecord::severity_number(log_record), SeverityNumber::Info)?
  @assertion.assert_eq(LogRecord::body(log_record), Some("User login successful"))?
  @assertion.assert_eq(LogRecord::trace_id(log_record), Some(trace_id))?
  @assertion.assert_eq(LogRecord::span_id(log_record), Some(span_id))?
}

test "log_record_with_span_context_integration" {
  // Test log record integration with span context
  let span_ctx = SpanContext::new("trace123", "span456", true, "key=value")
  
  let log_record = LogRecord::new_with_context(
    SeverityNumber::Warn,
    Some("Database connection slow"),
    None,
    Some(1735689600000000000L),
    None,
    Some(span_ctx.trace_id),
    Some(span_ctx.span_id),
    None
  )
  
  // Verify correlation with span context
  @assertion.assert_eq(LogRecord::trace_id(log_record), Some("trace123"))?
  @assertion.assert_eq(LogRecord::span_id(log_record), Some("span456"))?
  @assertion.assert_eq(LogRecord::severity_number(log_record), SeverityNumber::Warn)?
  @assertion.assert_eq(LogRecord::body(log_record), Some("Database connection slow"))?
}

test "log_record_with_attributes_and_trace_correlation" {
  // Test log record with attributes and trace correlation
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "operation", StringValue("login"))
  Attributes::set(attrs, "duration_ms", IntValue(250))
  
  let log_record = LogRecord::new_with_context(
    SeverityNumber::Error,
    Some("Authentication failed"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace789"),
    Some("span101112"),
    None
  )
  
  // Verify all fields are correctly set
  @assertion.assert_eq(LogRecord::severity_number(log_record), SeverityNumber::Error)?
  @assertion.assert_eq(LogRecord::body(log_record), Some("Authentication failed"))?
  @assertion.assert_eq(LogRecord::trace_id(log_record), Some("trace789"))?
  @assertion.assert_eq(LogRecord::span_id(log_record), Some("span101112"))?
  @assertion.assert_eq(log_record.attributes, Some(attrs))?
}

test "log_record_severity_levels_with_trace_correlation" {
  // Test all severity levels with trace correlation
  let trace_id = "severity_test_trace"
  let span_id = "severity_test_span"
  
  // Test each severity level
  let trace_log = LogRecord::new_with_context(
    SeverityNumber::Trace,
    Some("Trace message"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let debug_log = LogRecord::new_with_context(
    SeverityNumber::Debug,
    Some("Debug message"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let info_log = LogRecord::new_with_context(
    SeverityNumber::Info,
    Some("Info message"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let warn_log = LogRecord::new_with_context(
    SeverityNumber::Warn,
    Some("Warning message"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let error_log = LogRecord::new_with_context(
    SeverityNumber::Error,
    Some("Error message"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let fatal_log = LogRecord::new_with_context(
    SeverityNumber::Fatal,
    Some("Fatal message"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  // Verify all severity levels maintain trace correlation
  @assertion.assert_eq(LogRecord::severity_number(trace_log), SeverityNumber::Trace)?
  @assertion.assert_eq(LogRecord::severity_number(debug_log), SeverityNumber::Debug)?
  @assertion.assert_eq(LogRecord::severity_number(info_log), SeverityNumber::Info)?
  @assertion.assert_eq(LogRecord::severity_number(warn_log), SeverityNumber::Warn)?
  @assertion.assert_eq(LogRecord::severity_number(error_log), SeverityNumber::Error)?
  @assertion.assert_eq(LogRecord::severity_number(fatal_log), SeverityNumber::Fatal)?
  
  // Verify all maintain trace correlation
  @assertion.assert_eq(LogRecord::trace_id(trace_log), Some(trace_id))?
  @assertion.assert_eq(LogRecord::trace_id(debug_log), Some(trace_id))?
  @assertion.assert_eq(LogRecord::trace_id(info_log), Some(trace_id))?
  @assertion.assert_eq(LogRecord::trace_id(warn_log), Some(trace_id))?
  @assertion.assert_eq(LogRecord::trace_id(error_log), Some(trace_id))?
  @assertion.assert_eq(LogRecord::trace_id(fatal_log), Some(trace_id))?
}

test "log_record_context_integration" {
  // Test log record integration with context system
  let root_ctx = Context::root()
  let ctx_key = ContextKey::new("correlation.id")
  let ctx_with_value = Context::with_value(root_ctx, ctx_key, "corr-12345")
  
  let log_record = LogRecord::new_with_context(
    SeverityNumber::Info,
    Some("Context-aware log message"),
    None,
    Some(1735689600000000000L),
    None,
    Some("context_trace"),
    Some("context_span"),
    Some(ctx_with_value)
  )
  
  // Verify context integration
  @assertion.assert_eq(log_record.context, Some(ctx_with_value))?
  @assertion.assert_eq(LogRecord::trace_id(log_record), Some("context_trace"))?
  @assertion.assert_eq(LogRecord::span_id(log_record), Some("context_span"))?
  
  // Verify context value can be retrieved
  let retrieved_value = Context::get(ctx_with_value, ctx_key)
  @assertion.assert_eq(retrieved_value, Some("corr-12345"))?
}

test "log_record_timestamp_accuracy_with_trace_correlation" {
  // Test timestamp accuracy in trace correlation
  let timestamp = 1735689600000000000L  // Fixed timestamp for testing
  let observed_timestamp = 1735689600000000000L
  
  let log_record = LogRecord::new_with_context(
    SeverityNumber::Info,
    Some("Timestamp test"),
    None,
    Some(timestamp),
    Some(observed_timestamp),
    Some("timestamp_trace"),
    Some("timestamp_span"),
    None
  )
  
  // Verify timestamp fields
  @assertion.assert_eq(log_record.timestamp, Some(timestamp))?
  @assertion.assert_eq(log_record.observed_timestamp, Some(observed_timestamp))?
  @assertion.assert_eq(LogRecord::trace_id(log_record), Some("timestamp_trace"))?
  @assertion.assert_eq(LogRecord::span_id(log_record), Some("timestamp_span"))?
}

test "log_record_logger_emission_with_trace_correlation" {
  // Test logger emission with trace correlation
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "trace.test.logger")
  
  let log_record = LogRecord::new_with_context(
    SeverityNumber::Info,
    Some("Emission test with trace"),
    None,
    Some(1735689600000000000L),
    None,
    Some("emission_trace"),
    Some("emission_span"),
    None
  )
  
  // Test emission (simplified implementation)
  Logger::emit(logger, log_record)
  
  // Verify log record properties are maintained
  @assertion.assert_eq(LogRecord::severity_number(log_record), SeverityNumber::Info)?
  @assertion.assert_eq(LogRecord::body(log_record), Some("Emission test with trace"))?
  @assertion.assert_eq(LogRecord::trace_id(log_record), Some("emission_trace"))?
  @assertion.assert_eq(LogRecord::span_id(log_record), Some("emission_span"))?
  
  // Verify logger scope
  @assertion.assert_eq(logger.scope.name, "trace.test.logger")?
}

test "log_record_complex_trace_scenarios" {
  // Test complex trace correlation scenarios
  let parent_trace_id = "parent_trace_123"
  let parent_span_id = "parent_span_456"
  let child_trace_id = "child_trace_789"
  let child_span_id = "child_span_101112"
  
  // Parent log record
  let parent_log = LogRecord::new_with_context(
    SeverityNumber::Info,
    Some("Parent operation started"),
    None,
    Some(1735689600000000000L),
    None,
    Some(parent_trace_id),
    Some(parent_span_id),
    None
  )
  
  // Child log record
  let child_log = LogRecord::new_with_context(
    SeverityNumber::Info,
    Some("Child operation started"),
    None,
    Some(1735689600000000000L),
    None,
    Some(child_trace_id),
    Some(child_span_id),
    None
  )
  
  // Verify parent-child relationship through trace correlation
  @assertion.assert_eq(LogRecord::trace_id(parent_log), Some(parent_trace_id))?
  @assertion.assert_eq(LogRecord::span_id(parent_log), Some(parent_span_id))?
  @assertion.assert_eq(LogRecord::trace_id(child_log), Some(child_trace_id))?
  @assertion.assert_eq(LogRecord::span_id(child_log), Some(child_span_id))?
  
  // Verify they are different operations
  @assertion.assert_not_eq(LogRecord::body(parent_log), LogRecord::body(child_log))?
  @assertion.assert_not_eq(LogRecord::span_id(parent_log), LogRecord::span_id(child_log))?
}

test "log_record_edge_cases_with_trace_correlation" {
  // Test edge cases in trace correlation
  // Test with empty trace ID
  let empty_trace_log = LogRecord::new_with_context(
    SeverityNumber::Info,
    Some("Empty trace ID"),
    None,
    None,
    None,
    Some(""),
    Some("span123"),
    None
  )
  
  // Test with empty span ID
  let empty_span_log = LogRecord::new_with_context(
    SeverityNumber::Info,
    Some("Empty span ID"),
    None,
    None,
    None,
    Some("trace123"),
    Some(""),
    None
  )
  
  // Test with None trace ID
  let none_trace_log = LogRecord::new_with_context(
    SeverityNumber::Info,
    Some("None trace ID"),
    None,
    None,
    None,
    None,
    Some("span123"),
    None
  )
  
  // Test with None span ID
  let none_span_log = LogRecord::new_with_context(
    SeverityNumber::Info,
    Some("None span ID"),
    None,
    None,
    None,
    Some("trace123"),
    None,
    None
  )
  
  // Verify edge cases are handled correctly
  @assertion.assert_eq(LogRecord::trace_id(empty_trace_log), Some(""))?
  @assertion.assert_eq(LogRecord::span_id(empty_trace_log), Some("span123"))?
  @assertion.assert_eq(LogRecord::trace_id(empty_span_log), Some("trace123"))?
  @assertion.assert_eq(LogRecord::span_id(empty_span_log), Some(""))?
  @assertion.assert_eq(LogRecord::trace_id(none_trace_log), None)?
  @assertion.assert_eq(LogRecord::span_id(none_trace_log), Some("span123"))?
  @assertion.assert_eq(LogRecord::trace_id(none_span_log), Some("trace123"))?
  @assertion.assert_eq(LogRecord::span_id(none_span_log), None)?
}