// Cross-Component Integration Tests for Azimuth Telemetry System
// This file contains integration tests for telemetry system components

test "trace and metrics integration workflow" {
  // Test integration between tracing and metrics
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "integration_tracer")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "integration_meter")
  
  // Create a span and associated metrics
  let parent_span = Tracer::start_span(tracer, "operation_workflow")
  let span_context = Span::span_context(parent_span)
  
  // Create metrics associated with the operation
  let operation_counter = Meter::create_counter(meter, "operation_count")
  let operation_histogram = Meter::create_histogram(meter, "operation_duration")
  
  // Simulate operation workflow
  Counter::add(operation_counter, 1.0)
  Histogram::record(operation_histogram, 100.5)
  
  // Create child spans for sub-operations
  let child_span_1 = Tracer::start_span(tracer, "sub_operation_1")
  Counter::add(operation_counter, 1.0)
  Histogram::record(operation_histogram, 50.25)
  
  let child_span_2 = Tracer::start_span(tracer, "sub_operation_2")
  Counter::add(operation_counter, 1.0)
  Histogram::record(operation_histogram, 75.75)
  
  // Verify all components work together
  assert_eq(Span::name(parent_span), "operation_workflow")
  assert_eq(Span::name(child_span_1), "sub_operation_1")
  assert_eq(Span::name(child_span_2), "sub_operation_2")
  assert_true(Span::is_recording(parent_span))
}

test "logging with trace correlation integration" {
  // Test integration between logging and tracing
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "integration_logger")
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "correlation_tracer")
  
  // Create a span for context
  let operation_span = Tracer::start_span(tracer, "logged_operation")
  let span_context = Span::span_context(operation_span)
  
  // Create log records with trace correlation
  let trace_id = Some(SpanContext::trace_id(span_context))
  let span_id = Some(SpanContext::span_id(span_context))
  
  let info_log = LogRecord::new_with_context(
    SeverityNumber::Info,
    Some("Operation started successfully"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    trace_id,
    span_id,
    None
  )
  
  let warning_log = LogRecord::new_with_context(
    SeverityNumber::Warn,
    Some("Operation taking longer than expected"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    trace_id,
    span_id,
    None
  )
  
  // Emit log records
  Logger::emit(logger, info_log)
  Logger::emit(logger, warning_log)
  
  // Verify log records have correct trace correlation
  assert_eq(LogRecord::trace_id(info_log), trace_id)
  assert_eq(LogRecord::span_id(info_log), span_id)
  assert_eq(LogRecord::severity_number(info_log), SeverityNumber::Info)
  assert_eq(LogRecord::severity_number(warning_log), SeverityNumber::Warn)
}