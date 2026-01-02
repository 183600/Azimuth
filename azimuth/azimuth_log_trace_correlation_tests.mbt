// Log Record and Trace Correlation Tests for Azimuth
// This file contains test cases for correlation between log records and trace spans

test "log record with trace context" {
  // Test creating log records with trace context
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "correlation-test")
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "correlation-test")
  
  // Create a span
  let span = azimuth::Tracer::start_span(tracer, "operation-with-logs")
  let span_ctx = azimuth::Span::span_context(span)
  
  // Create log record with trace context
  let trace_id = azimuth::SpanContext::trace_id(span_ctx)
  let span_id = azimuth::SpanContext::span_id(span_ctx)
  
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Operation started"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  // Verify trace correlation
  assert_eq(azimuth::LogRecord::trace_id(log_record), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(log_record), Some(span_id))
  
  // Emit log record
  azimuth::Logger::emit(logger, log_record)
  
  // End span
  azimuth::Span::end(span)
}

test "multiple logs within single span" {
  // Test multiple log records within a single span
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "multi-log-test")
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "multi-log-test")
  
  // Create a span
  let span = azimuth::Tracer::start_span(tracer, "multi-log-operation")
  let span_ctx = azimuth::Span::span_context(span)
  
  let trace_id = azimuth::SpanContext::trace_id(span_ctx)
  let span_id = azimuth::SpanContext::span_id(span_ctx)
  let clock = azimuth::Clock::system()
  
  // Create multiple log records within the same span
  let log_start = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Starting multi-log operation"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(clock)),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  // Add event to span
  azimuth::Span::add_event(span, "operation.step1", Some([
    ("step", azimuth::IntValue(1)),
    ("status", azimuth::StringValue("in_progress"))
  ]))
  
  let log_progress = azimuth::LogRecord::new_with_context(
    azimuth::Debug,
    Some("Operation in progress"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(clock) + 1000000L),  // +1ms
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  // Add another event to span
  azimuth::Span::add_event(span, "operation.step2", Some([
    ("step", azimuth::IntValue(2)),
    ("status", azimuth::StringValue("completed"))
  ]))
  
  let log_complete = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Multi-log operation completed"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(clock) + 2000000L),  // +2ms
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  // Verify all logs have the same trace context
  assert_eq(azimuth::LogRecord::trace_id(log_start), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(log_start), Some(span_id))
  
  assert_eq(azimuth::LogRecord::trace_id(log_progress), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(log_progress), Some(span_id))
  
  assert_eq(azimuth::LogRecord::trace_id(log_complete), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(log_complete), Some(span_id))
  
  // Emit all log records
  azimuth::Logger::emit(logger, log_start)
  azimuth::Logger::emit(logger, log_progress)
  azimuth::Logger::emit(logger, log_complete)
  
  // End span
  azimuth::Span::end(span)
}

test "log correlation across nested spans" {
  // Test log correlation across parent and child spans
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "nested-span-test")
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "nested-span-test")
  
  // Create parent span
  let parent_span = azimuth::Tracer::start_span(tracer, "parent-operation")
  let parent_span_ctx = azimuth::Span::span_context(parent_span)
  
  let parent_trace_id = azimuth::SpanContext::trace_id(parent_span_ctx)
  let parent_span_id = azimuth::SpanContext::span_id(parent_span_ctx)
  
  // Create log in parent span
  let parent_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Starting parent operation"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(parent_trace_id),
    Some(parent_span_id),
    Some(azimuth::Context::root())
  )
  
  // Create child span
  let child_span = azimuth::Tracer::start_span(tracer, "child-operation")
  let child_span_ctx = azimuth::Span::span_context(child_span)
  
  let child_trace_id = azimuth::SpanContext::trace_id(child_span_ctx)
  let child_span_id = azimuth::SpanContext::span_id(child_span_ctx)
  
  // Create log in child span
  let child_log = azimuth::LogRecord::new_with_context(
    azimuth::Debug,
    Some("Executing child operation"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 1000000L),
    None,
    Some(child_trace_id),
    Some(child_span_id),
    Some(azimuth::Context::root())
  )
  
  // Verify trace ID is the same across parent and child
  assert_eq(parent_trace_id, child_trace_id)
  
  // Verify span IDs are different
  assert_true(parent_span_id != child_span_id)
  
  // Verify logs are associated with correct spans
  assert_eq(azimuth::LogRecord::trace_id(parent_log), Some(parent_trace_id))
  assert_eq(azimuth::LogRecord::span_id(parent_log), Some(parent_span_id))
  
  assert_eq(azimuth::LogRecord::trace_id(child_log), Some(child_trace_id))
  assert_eq(azimuth::LogRecord::span_id(child_log), Some(child_span_id))
  
  // Emit log records
  azimuth::Logger::emit(logger, parent_log)
  azimuth::Logger::emit(logger, child_log)
  
  // End spans
  azimuth::Span::end(child_span)
  azimuth::Span::end(parent_span)
}

test "log correlation with context propagation" {
  // Test log correlation with context propagation
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "context-prop-test")
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "context-prop-test")
  
  // Create initial span
  let initial_span = azimuth::Tracer::start_span(tracer, "initial-operation")
  let initial_span_ctx = azimuth::Span::span_context(initial_span)
  
  let trace_id = azimuth::SpanContext::trace_id(initial_span_ctx)
  let initial_span_id = azimuth::SpanContext::span_id(initial_span_ctx)
  
  // Create context with trace information
  let ctx = azimuth::Context::root()
  let trace_key = azimuth::ContextKey::new("trace.id")
  let span_key = azimuth::ContextKey::new("span.id")
  
  let ctx_with_trace = azimuth::Context::with_value(ctx, trace_key, trace_id)
  let ctx_with_span = azimuth::Context::with_value(ctx_with_trace, span_key, initial_span_id)
  
  // Create log record with propagated context
  let propagated_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Operation with propagated context"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some(initial_span_id),
    Some(ctx_with_span)
  )
  
  // Verify context propagation
  let propagated_trace_id = azimuth::Context::get(ctx_with_span, trace_key)
  let propagated_span_id = azimuth::Context::get(ctx_with_span, span_key)
  
  assert_eq(propagated_trace_id, Some(trace_id))
  assert_eq(propagated_span_id, Some(initial_span_id))
  
  // Verify log correlation
  assert_eq(azimuth::LogRecord::trace_id(propagated_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(propagated_log), Some(initial_span_id))
  
  // Emit log record
  azimuth::Logger::emit(logger, propagated_log)
  
  // End span
  azimuth::Span::end(initial_span)
}

test "log correlation with baggage" {
  // Test log correlation with baggage information
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "baggage-test")
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "baggage-test")
  
  // Create span
  let span = azimuth::Tracer::start_span(tracer, "baggage-operation")
  let span_ctx = azimuth::Span::span_context(span)
  
  let trace_id = azimuth::SpanContext::trace_id(span_ctx)
  let span_id = azimuth::SpanContext::span_id(span_ctx)
  
  // Create baggage
  let baggage = azimuth::Baggage::new()
  let baggage = azimuth::Baggage::set_entry(baggage, "user.id", "user123")
  let baggage = azimuth::Baggage::set_entry(baggage, "request.id", "req456")
  let baggage = azimuth::Baggage::set_entry(baggage, "session.id", "session789")
  
  // Create attributes with baggage information
  let attrs = azimuth::Attributes::new()
  
  // Add baggage entries as log attributes
  let user_id = azimuth::Baggage::get_entry(baggage, "user.id")
  let request_id = azimuth::Baggage::get_entry(baggage, "request.id")
  let session_id = azimuth::Baggage::get_entry(baggage, "session.id")
  
  match user_id {
    Some(id) => azimuth::Attributes::set(attrs, "user.id", azimuth::StringValue(id))
    None => ()
  }
  
  match request_id {
    Some(id) => azimuth::Attributes::set(attrs, "request.id", azimuth::StringValue(id))
    None => ()
  }
  
  match session_id {
    Some(id) => azimuth::Attributes::set(attrs, "session.id", azimuth::StringValue(id))
    None => ()
  }
  
  // Create log record with baggage
  let baggage_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Operation with baggage"),
    Some(attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  // Verify log correlation with baggage
  assert_eq(azimuth::LogRecord::trace_id(baggage_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(baggage_log), Some(span_id))
  
  // Verify baggage attributes are included
  let log_attrs = azimuth::LogRecord::attributes(baggage_log)
  match log_attrs {
    Some(attrs) => {
      let log_user_id = azimuth::Attributes::get(attrs, "user.id")
      let log_request_id = azimuth::Attributes::get(attrs, "request.id")
      let log_session_id = azimuth::Attributes::get(attrs, "session.id")
      
      assert_eq(log_user_id, Some(azimuth::StringValue("user123")))
      assert_eq(log_request_id, Some(azimuth::StringValue("req456")))
      assert_eq(log_session_id, Some(azimuth::StringValue("session789")))
    }
    None => assert_true(false)  // Should have attributes
  }
  
  // Emit log record
  azimuth::Logger::emit(logger, baggage_log)
  
  // End span
  azimuth::Span::end(span)
}

test "log correlation with error handling" {
  // Test log correlation in error scenarios
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "error-test")
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "error-test")
  
  // Create span
  let span = azimuth::Tracer::start_span(tracer, "error-operation")
  let span_ctx = azimuth::Span::span_context(span)
  
  let trace_id = azimuth::SpanContext::trace_id(span_ctx)
  let span_id = azimuth::SpanContext::span_id(span_ctx)
  
  // Create error log record
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Operation failed with error"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  // Set span status to error
  azimuth::Span::set_status(span, azimuth::Error, Some("Operation failed"))
  
  // Add error event to span
  azimuth::Span::add_event(span, "error.occurred", Some([
    ("error.type", azimuth::StringValue("ValidationError")),
    ("error.message", azimuth::StringValue("Invalid input parameter")),
    ("error.code", azimuth::IntValue(400))
  ]))
  
  // Verify error correlation
  assert_eq(azimuth::LogRecord::trace_id(error_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(error_log), Some(span_id))
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  
  // Emit error log
  azimuth::Logger::emit(logger, error_log)
  
  // End span
  azimuth::Span::end(span)
}

test "log correlation with different severity levels" {
  // Test log correlation with different severity levels
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "severity-test")
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "severity-test")
  
  // Create span
  let span = azimuth::Tracer::start_span(tracer, "severity-operation")
  let span_ctx = azimuth::Span::span_context(span)
  
  let trace_id = azimuth::SpanContext::trace_id(span_ctx)
  let span_id = azimuth::SpanContext::span_id(span_ctx)
  let clock = azimuth::Clock::system()
  let base_timestamp = azimuth::Clock::now_unix_nanos(clock)
  
  // Create log records with different severity levels
  let trace_log = azimuth::LogRecord::new_with_context(
    azimuth::Trace,
    Some("Detailed trace information"),
    Some(azimuth::Attributes::new()),
    Some(base_timestamp),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  let debug_log = azimuth::LogRecord::new_with_context(
    azimuth::Debug,
    Some("Debug information"),
    Some(azimuth::Attributes::new()),
    Some(base_timestamp + 1000000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  let info_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("General information"),
    Some(azimuth::Attributes::new()),
    Some(base_timestamp + 2000000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  let warn_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Warning message"),
    Some(azimuth::Attributes::new()),
    Some(base_timestamp + 3000000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Error message"),
    Some(azimuth::Attributes::new()),
    Some(base_timestamp + 4000000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  let fatal_log = azimuth::LogRecord::new_with_context(
    azimuth::Fatal,
    Some("Fatal error message"),
    Some(azimuth::Attributes::new()),
    Some(base_timestamp + 5000000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  // Verify all logs have the same trace correlation
  let logs = [trace_log, debug_log, info_log, warn_log, error_log, fatal_log]
  let severities = [azimuth::Trace, azimuth::Debug, azimuth::Info, azimuth::Warn, azimuth::Error, azimuth::Fatal]
  
  for i in 0..logs.length() {
    let log = logs[i]
    let expected_severity = severities[i]
    
    assert_eq(azimuth::LogRecord::trace_id(log), Some(trace_id))
    assert_eq(azimuth::LogRecord::span_id(log), Some(span_id))
    assert_eq(azimuth::LogRecord::severity_number(log), expected_severity)
    
    // Emit log record
    azimuth::Logger::emit(logger, log)
  }
  
  // End span
  azimuth::Span::end(span)
}

test "log correlation with resource information" {
  // Test log correlation with resource information
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "resource-test")
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "resource-test")
  
  // Create resource
  let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("correlation-service")),
    ("service.version", azimuth::StringValue("1.2.3")),
    ("service.instance.id", azimuth::StringValue("instance-123")),
    ("deployment.environment", azimuth::StringValue("production"))
  ])
  
  // Create span
  let span = azimuth::Tracer::start_span(tracer, "resource-operation")
  let span_ctx = azimuth::Span::span_context(span)
  
  let trace_id = azimuth::SpanContext::trace_id(span_ctx)
  let span_id = azimuth::SpanContext::span_id(span_ctx)
  
  // Create log attributes with resource information
  let log_attrs = azimuth::Attributes::new()
  
  // Add resource attributes to log
  let service_name = azimuth::Resource::get_attribute(resource, "service.name")
  let service_version = azimuth::Resource::get_attribute(resource, "service.version")
  let instance_id = azimuth::Resource::get_attribute(resource, "service.instance.id")
  let environment = azimuth::Resource::get_attribute(resource, "deployment.environment")
  
  match service_name {
    Some(azimuth::StringValue(name)) => azimuth::Attributes::set(log_attrs, "service.name", azimuth::StringValue(name))
    _ => ()
  }
  
  match service_version {
    Some(azimuth::StringValue(version)) => azimuth::Attributes::set(log_attrs, "service.version", azimuth::StringValue(version))
    _ => ()
  }
  
  match instance_id {
    Some(azimuth::StringValue(id)) => azimuth::Attributes::set(log_attrs, "service.instance.id", azimuth::StringValue(id))
    _ => ()
  }
  
  match environment {
    Some(azimuth::StringValue(env)) => azimuth::Attributes::set(log_attrs, "deployment.environment", azimuth::StringValue(env))
    _ => ()
  }
  
  // Create log record with resource information
  let resource_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Operation with resource context"),
    Some(log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  // Verify log correlation with resource information
  assert_eq(azimuth::LogRecord::trace_id(resource_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(resource_log), Some(span_id))
  
  // Verify resource attributes are included in log
  let log_attrs_result = azimuth::LogRecord::attributes(resource_log)
  match log_attrs_result {
    Some(attrs) => {
      let log_service_name = azimuth::Attributes::get(attrs, "service.name")
      let log_service_version = azimuth::Attributes::get(attrs, "service.version")
      let log_instance_id = azimuth::Attributes::get(attrs, "service.instance.id")
      let log_environment = azimuth::Attributes::get(attrs, "deployment.environment")
      
      assert_eq(log_service_name, Some(azimuth::StringValue("correlation-service")))
      assert_eq(log_service_version, Some(azimuth::StringValue("1.2.3")))
      assert_eq(log_instance_id, Some(azimuth::StringValue("instance-123")))
      assert_eq(log_environment, Some(azimuth::StringValue("production")))
    }
    None => assert_true(false)  // Should have attributes
  }
  
  // Emit log record
  azimuth::Logger::emit(logger, resource_log)
  
  // End span
  azimuth::Span::end(span)
}