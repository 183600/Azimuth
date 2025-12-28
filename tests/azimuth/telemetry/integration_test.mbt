test "trace and metrics integration" {
  // Test integration between tracing and metrics
  let tracer_provider = @azimuth.TracerProvider::default()
  let meter_provider = @azimuth.MeterProvider::default()
  
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "integration.test")
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "integration.test")
  
  // Create span and metrics
  let span = @azimuth.Tracer::start_span(tracer, "integrated_operation")
  let counter = @azimuth.Meter::create_counter(meter, "operation_count")
  let histogram = @azimuth.Meter::create_histogram(meter, "operation_duration")
  
  // Use both tracing and metrics together
  @azimuth.Span::add_event(span, "operation_started")
  @azimuth.Counter::add(counter, 1.0)
  @azimuth.Histogram::record(histogram, 100.0)
  @azimuth.Span::set_status(span, @azimuth.Ok)
  @azimuth.Span::end(span)
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}

test "trace and logging integration" {
  // Test integration between tracing and logging
  let tracer_provider = @azimuth.TracerProvider::default()
  let logger_provider = @azimuth.LoggerProvider::default()
  
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "trace.log.test")
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "trace.log.test")
  
  // Create span and log records
  let span = @azimuth.Tracer::start_span(tracer, "logged_operation")
  let span_ctx = @azimuth.Span::span_context(span)
  
  let info_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Operation started"),
    None,
    None,
    None,
    Some(@azimuth.SpanContext::trace_id(span_ctx)),
    Some(@azimuth.SpanContext::span_id(span_ctx)),
    None
  )
  
  let error_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Error,
    Some("Operation failed"),
    None,
    None,
    None,
    Some(@azimuth.SpanContext::trace_id(span_ctx)),
    Some(@azimuth.SpanContext::span_id(span_ctx)),
    None
  )
  
  @azimuth.Logger::emit(logger, info_log)
  @azimuth.Logger::emit(logger, error_log)
  @azimuth.Span::end(span)
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}

test "context propagation across components" {
  // Test context propagation across different telemetry components
  let root_ctx = @azimuth.Context::root()
  let correlation_key = @azimuth.ContextKey::new("correlation_id")
  
  // Add correlation ID to context
  let ctx_with_correlation = @azimuth.Context::with_value(root_ctx, correlation_key, "corr-12345")
  
  // Use context in tracing
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "context.prop.test")
  let span = @azimuth.Tracer::start_span(tracer, "context_operation")
  
  // Use context in logging
  let logger_provider = @azimuth.LoggerProvider::default()
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "context.prop.test")
  let log_record = @azimuth.LogRecord::new(@azimuth.Info, "Operation with context")
  
  @azimuth.Logger::emit(logger, log_record)
  @azimuth.Span::end(span)
  
  // Verify correlation ID is still accessible
  let retrieved_correlation = @azimuth.Context::get(ctx_with_correlation, correlation_key)
  assert_eq(retrieved_correlation, Some("corr-12345"))
}

test "end-to-end telemetry workflow" {
  // Test complete end-to-end telemetry workflow
  let tracer_provider = @azimuth.TracerProvider::default()
  let meter_provider = @azimuth.MeterProvider::default()
  let logger_provider = @azimuth.LoggerProvider::default()
  
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "e2e.test")
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "e2e.test")
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "e2e.test")
  
  // Start trace
  let root_span = @azimuth.Tracer::start_span(tracer, "root_operation")
  let root_span_ctx = @azimuth.Span::span_context(root_span)
  
  // Create metrics
  let request_counter = @azimuth.Meter::create_counter(meter, "requests_total")
  let duration_histogram = @azimuth.Meter::create_histogram(meter, "request_duration_ms")
  
  // Log start of operation
  let start_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Starting operation"),
    None,
    Some(@azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())),
    None,
    Some(@azimuth.SpanContext::trace_id(root_span_ctx)),
    Some(@azimuth.SpanContext::span_id(root_span_ctx)),
    None
  )
  
  @azimuth.Logger::emit(logger, start_log)
  @azimuth.Counter::add(request_counter, 1.0)
  
  // Simulate sub-operation
  let sub_span = @azimuth.Tracer::start_span(tracer, "sub_operation")
  @azimuth.Span::add_event(sub_span, "processing_data")
  @azimuth.Histogram::record(duration_histogram, 150.0)
  @azimuth.Span::end(sub_span)
  
  // Log completion
  let end_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Operation completed"),
    None,
    Some(@azimuth.Clock::now_unix_nanos(@azimuth.Clock::system())),
    None,
    Some(@azimuth.SpanContext::trace_id(root_span_ctx)),
    Some(@azimuth.SpanContext::span_id(root_span_ctx)),
    None
  )
  
  @azimuth.Logger::emit(logger, end_log)
  @azimuth.Histogram::record(duration_histogram, 200.0)
  @azimuth.Span::end(root_span)
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}

test "propagation with active trace" {
  // Test context propagation with active trace
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "propagation.test")
  
  // Start trace
  let span = @azimuth.Tracer::start_span(tracer, "propagated_operation")
  let span_ctx = @azimuth.Span::span_context(span)
  
  // Create propagator
  let trace_propagator = @azimuth.W3CTraceContextPropagator::new()
  let composite = @azimuth.CompositePropagator::new([trace_propagator])
  
  // Inject trace context into carrier
  let carrier = @azimuth.TextMapCarrier::new()
  let ctx = @azimuth.Context::root()
  @azimuth.CompositePropagator::inject(composite, ctx, carrier)
  
  // Extract trace context in different context
  let extracted_ctx = @azimuth.CompositePropagator::extract(composite, carrier)
  
  // Use extracted context for new operations
  let new_tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "extracted.operation")
  let new_span = @azimuth.Tracer::start_span(new_tracer, "continuation_operation")
  
  @azimuth.Span::end(span)
  @azimuth.Span::end(new_span)
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}

test "multi-component telemetry scenario" {
  // Test complex scenario with multiple telemetry components
  let tracer_provider = @azimuth.TracerProvider::default()
  let meter_provider = @azimuth.MeterProvider::default()
  let logger_provider = @azimuth.LoggerProvider::default()
  
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "multi.comp.test")
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "multi.comp.test")
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "multi.comp.test")
  
  // Initialize components
  let request_counter = @azimuth.Meter::create_counter(meter, "http_requests")
  let error_counter = @azimuth.Meter::create_counter(meter, "http_errors")
  let response_histogram = @azimuth.Meter::create_histogram(meter, "http_response_time")
  
  // Simulate HTTP request processing
  let request_span = @azimuth.Tracer::start_span(tracer, "http_request")
  @azimuth.Span::add_event(request_span, "request_received")
  
  // Log request
  let request_log = @azimuth.LogRecord::new(@azimuth.Info, "Processing HTTP request")
  @azimuth.Logger::emit(logger, request_log)
  
  // Update metrics
  @azimuth.Counter::add(request_counter, 1.0)
  
  // Simulate processing
  let processing_span = @azimuth.Tracer::start_span(tracer, "request_processing")
  @azimuth.Span::add_event(processing_span, "validating_input")
  @azimuth.Span::add_event(processing_span, "executing_business_logic")
  @azimuth.Histogram::record(response_histogram, 75.0)
  @azimuth.Span::end(processing_span)
  
  // Simulate error scenario
  let error_span = @azimuth.Tracer::start_span(tracer, "error_handling")
  @azimuth.Span::set_status(error_span, @azimuth.Error, Some("Validation failed"))
  
  let error_log = @azimuth.LogRecord::new(@azimuth.Error, "Request validation failed")
  @azimuth.Logger::emit(logger, error_log)
  
  @azimuth.Counter::add(error_counter, 1.0)
  @azimuth.Span::end(error_span)
  
  // Complete request
  @azimuth.Span::set_status(request_span, @azimuth.Error)
  @azimuth.Span::end(request_span)
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}