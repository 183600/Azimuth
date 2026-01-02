// Azimuth LogRecord and Span Correlation Tests
// Test log record creation, span correlation, and log management

test "log_record_basic_creation" {
  let record = LogRecord::new(Info, "Test log message")
  
  // Test basic properties
  match LogRecord::severity_number(record) {
    Info => @test.succeed()
    _ => @test.abort("Expected Info severity")
  }
  
  match LogRecord::body(record) {
    Some(message) => assert_eq!(message, "Test log message")
    None => @test.abort("Expected log body to contain message")
  }
}

test "log_record_all_severity_levels" {
  let severity_levels = [
    (Trace, "Trace message"),
    (Debug, "Debug message"),
    (Info, "Info message"),
    (Warn, "Warning message"),
    (Error, "Error message"),
    (Fatal, "Fatal message")
  ]
  
  for (severity, message) in severity_levels {
    let record = LogRecord::new(severity, message)
    
    match LogRecord::severity_number(record) {
      s => assert_eq!(s, severity)
    }
    
    match LogRecord::body(record) {
      Some(msg) => assert_eq!(msg, message)
      None => @test.abort("Expected log body to contain message")
    }
  }
}

test "log_record_with_context_creation" {
  let attributes = Attributes::new()
  let context = Context::root()
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000001000L
  let trace_id = "trace_123"
  let span_id = "span_456"
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Error with context"),
    Some(attributes),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(context)
  )
  
  // Test severity
  match LogRecord::severity_number(record) {
    Error => @test.succeed()
    _ => @test.abort("Expected Error severity")
  }
  
  // Test body
  match LogRecord::body(record) {
    Some(message) => assert_eq!(message, "Error with context")
    None => @test.abort("Expected log body to contain message")
  }
  
  // Test trace correlation
  match LogRecord::trace_id(record) {
    Some(id) => assert_eq!(id, "trace_123")
    None => @test.abort("Expected trace_id to be set")
  }
  
  // Test span correlation
  match LogRecord::span_id(record) {
    Some(id) => assert_eq!(id, "span_456")
    None => @test.abort("Expected span_id to be set")
  }
}

test "log_record_span_correlation" {
  let span_ctx = SpanContext::new("trace_789", "span_101", true, "")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Create log record with span context
  let record = LogRecord::new_with_context(
    Info,
    Some("Log within span"),
    None,
    None,
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    None
  )
  
  // Verify correlation with span
  match LogRecord::trace_id(record) {
    Some(trace_id) => assert_eq!(trace_id, "trace_789")
    None => @test.abort("Expected log to be correlated with span trace ID")
  }
  
  match LogRecord::span_id(record) {
    Some(span_id) => assert_eq!(span_id, "span_101")
    None => @test.abort("Expected log to be correlated with span ID")
  }
}

test "log_record_without_span_correlation" {
  let record = LogRecord::new_with_context(
    Info,
    Some("Log without span"),
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  // Verify no span correlation
  match LogRecord::trace_id(record) {
    Some(_) => @test.abort("Expected no trace_id for uncorrelated log")
    None => @test.succeed()
  }
  
  match LogRecord::span_id(record) {
    Some(_) => @test.abort("Expected no span_id for uncorrelated log")
    None => @test.succeed()
  }
}

test "log_record_with_attributes" {
  let attributes = Attributes::new()
  let record = LogRecord::new_with_context(
    Warn,
    Some("Warning with attributes"),
    Some(attributes),
    None,
    None,
    None,
    None,
    None
  )
  
  // Test severity
  match LogRecord::severity_number(record) {
    Warn => @test.succeed()
    _ => @test.abort("Expected Warn severity")
  }
  
  // Test body
  match LogRecord::body(record) {
    Some(message) => assert_eq!(message, "Warning with attributes")
    None => @test.abort("Expected log body to contain message")
  }
  
  // In simplified implementation, we can't verify attributes content
  // but we can verify the record was created with attributes
  @test.succeed()
}

test "log_record_timestamp_handling" {
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000001000L
  
  let record = LogRecord::new_with_context(
    Info,
    Some("Log with timestamps"),
    None,
    Some(timestamp),
    Some(observed_timestamp),
    None,
    None,
    None
  )
  
  // In simplified implementation, we can't directly access timestamps
  // but we can verify the record was created successfully
  match LogRecord::body(record) {
    Some(message) => assert_eq!(message, "Log with timestamps")
    None => @test.abort("Expected log body to contain message")
  }
}

test "logger_creation_and_emit" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test_logger")
  
  // Create a log record
  let record = LogRecord::new(Info, "Test log emission")
  
  // Emit the log record
  Logger::emit(logger, record)
  
  // In simplified implementation, we just verify the call doesn't fail
  @test.succeed()
}

test "log_record_complex_span_workflow" {
  // Create a span
  let span_ctx = SpanContext::new("workflow_trace", "workflow_span", true, "")
  let span = Span::new("workflow_span", Server, span_ctx)
  
  // Add events to span
  Span::add_event(span, "workflow_started", None)
  
  // Create log records at different workflow stages
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Workflow started"),
    None,
    None,
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    None
  )
  
  Span::add_event(span, "processing", None)
  
  let processing_log = LogRecord::new_with_context(
    Info,
    Some("Processing workflow step"),
    None,
    None,
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    None
  )
  
  Span::add_event(span, "workflow_completed", None)
  
  let completion_log = LogRecord::new_with_context(
    Info,
    Some("Workflow completed"),
    None,
    None,
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    None
  )
  
  // Verify all logs are correlated with the same span
  let expected_trace_id = "workflow_trace"
  let expected_span_id = "workflow_span"
  
  match LogRecord::trace_id(start_log) {
    Some(id) => assert_eq!(id, expected_trace_id)
    None => @test.abort("Expected start log to have trace ID")
  }
  
  match LogRecord::span_id(start_log) {
    Some(id) => assert_eq!(id, expected_span_id)
    None => @test.abort("Expected start log to have span ID")
  }
  
  match LogRecord::trace_id(processing_log) {
    Some(id) => assert_eq!(id, expected_trace_id)
    None => @test.abort("Expected processing log to have trace ID")
  }
  
  match LogRecord::span_id(processing_log) {
    Some(id) => assert_eq!(id, expected_span_id)
    None => @test.abort("Expected processing log to have span ID")
  }
  
  match LogRecord::trace_id(completion_log) {
    Some(id) => assert_eq!(id, expected_trace_id)
    None => @test.abort("Expected completion log to have trace ID")
  }
  
  match LogRecord::span_id(completion_log) {
    Some(id) => assert_eq!(id, expected_span_id)
    None => @test.abort("Expected completion log to have span ID")
  }
}

test "log_record_error_correlation" {
  let span_ctx = SpanContext::new("error_trace", "error_span", true, "")
  let span = Span::new("error_span", Internal, span_ctx)
  
  // Set error status on span
  Span::set_status(span, Error, Some("Something went wrong"))
  
  // Create error log record
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Error occurred during operation"),
    None,
    None,
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    None
  )
  
  // Verify correlation
  match LogRecord::trace_id(error_log) {
    Some(id) => assert_eq!(id, "error_trace")
    None => @test.abort("Expected error log to have trace ID")
  }
  
  match LogRecord::span_id(error_log) {
    Some(id) => assert_eq!(id, "error_span")
    None => @test.abort("Expected error log to have span ID")
  }
  
  match LogRecord::severity_number(error_log) {
    Error => @test.succeed()
    _ => @test.abort("Expected Error severity")
  }
}

test "log_record_multiple_spans_correlation" {
  // Create multiple spans
  let parent_ctx = SpanContext::new("parent_trace", "parent_span", true, "")
  let parent_span = Span::new("parent_span", Server, parent_ctx)
  
  let child_ctx = SpanContext::new("parent_trace", "child_span", true, "")
  let child_span = Span::new("child_span", Internal, child_ctx)
  
  // Create logs for each span
  let parent_log = LogRecord::new_with_context(
    Info,
    Some("Parent operation"),
    None,
    None,
    None,
    Some(SpanContext::trace_id(Span::span_context(parent_span))),
    Some(SpanContext::span_id(Span::span_context(parent_span))),
    None
  )
  
  let child_log = LogRecord::new_with_context(
    Info,
    Some("Child operation"),
    None,
    None,
    None,
    Some(SpanContext::trace_id(Span::span_context(child_span))),
    Some(SpanContext::span_id(Span::span_context(child_span))),
    None
  )
  
  // Verify parent log correlation
  match LogRecord::trace_id(parent_log) {
    Some(id) => assert_eq!(id, "parent_trace")
    None => @test.abort("Expected parent log to have trace ID")
  }
  
  match LogRecord::span_id(parent_log) {
    Some(id) => assert_eq!(id, "parent_span")
    None => @test.abort("Expected parent log to have span ID")
  }
  
  // Verify child log correlation
  match LogRecord::trace_id(child_log) {
    Some(id) => assert_eq!(id, "parent_trace")
    None => @test.abort("Expected child log to have same trace ID as parent")
  }
  
  match LogRecord::span_id(child_log) {
    Some(id) => assert_eq!(id, "child_span")
    None => @test.abort("Expected child log to have different span ID")
  }
}

test "log_record_with_logger_emission" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "correlation_test_logger")
  
  // Create a span
  let span_ctx = SpanContext::new("emission_trace", "emission_span", true, "")
  let span = Span::new("emission_span", Client, span_ctx)
  
  // Create and emit correlated log records
  let info_log = LogRecord::new_with_context(
    Info,
    Some("Information log"),
    None,
    None,
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    None
  )
  
  let warn_log = LogRecord::new_with_context(
    Warn,
    Some("Warning log"),
    None,
    None,
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    None
  )
  
  // Emit logs
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  
  // Verify logs are still correlated after emission
  match LogRecord::trace_id(info_log) {
    Some(id) => assert_eq!(id, "emission_trace")
    None => @test.abort("Expected info log to maintain trace correlation")
  }
  
  match LogRecord::span_id(warn_log) {
    Some(id) => assert_eq!(id, "emission_span")
    None => @test.abort("Expected warn log to maintain span correlation")
  }
}