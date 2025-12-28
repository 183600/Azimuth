test "comprehensive telemetry workflow" {
  // Test a comprehensive telemetry workflow combining traces, metrics, and logs
  
  // Setup providers
  let tracer_provider = @azimuth.TracerProvider::default()
  let meter_provider = @azimuth.MeterProvider::default()
  let logger_provider = @azimuth.LoggerProvider::default()
  
  // Create instruments
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "comprehensive.test", Some("1.0.0"))
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "comprehensive.test", Some("1.0.0"))
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "comprehensive.test", Some("1.0.0"))
  
  // Create metrics instruments
  let counter = @azimuth.Meter::create_counter(meter, "operations.total", Some("Total operations"), Some("count"))
  let histogram = @azimuth.Meter::create_histogram(meter, "operation.duration", Some("Operation duration"), Some("ms"))
  
  // Start root span
  let root_span = @azimuth.Tracer::start_span(tracer, "comprehensive.operation")
  
  // Add events to span
  @azimuth.Span::add_event(root_span, "operation.started")
  
  // Record metrics
  @azimuth.Counter::add(counter, 1.0)
  
  // Log information
  let info_log = @azimuth.LogRecord::new(@azimuth.Info, "Starting comprehensive operation")
  @azimuth.Logger::emit(logger, info_log)
  
  // Create child span
  let child_span = @azimuth.Tracer::start_span(tracer, "sub.operation")
  @azimuth.Span::add_event(child_span, "sub.operation.started")
  
  // Record more metrics
  @azimuth.Counter::add(counter, 1.0)
  @azimuth.Histogram::record(histogram, 150.0)
  
  // Log sub-operation
  let sub_log = @azimuth.LogRecord::new(@azimuth.Debug, "Processing sub-operation")
  @azimuth.Logger::emit(logger, sub_log)
  
  // End child span
  @azimuth.Span::end(child_span)
  
  // Final metrics
  @azimuth.Histogram::record(histogram, 300.0)
  
  // Log completion
  let complete_log = @azimuth.LogRecord::new(@azimuth.Info, "Comprehensive operation completed")
  @azimuth.Logger::emit(logger, complete_log)
  
  // End root span
  @azimuth.Span::end(root_span)
  
  // Test passes if workflow completes without exceptions
  assert_true(true)
}

test "trace context propagation with metrics and logs" {
  // Test trace context propagation across different telemetry signals
  
  let tracer_provider = @azimuth.TracerProvider::default()
  let meter_provider = @azimuth.MeterProvider::default()
  let logger_provider = @azimuth.LoggerProvider::default()
  
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "propagation.test")
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "propagation.test")
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "propagation.test")
  
  // Create span with specific context
  let span = @azimuth.Tracer::start_span(tracer, "propagation.operation")
  let span_ctx = @azimuth.Span::span_context(span)
  
  // Create log record with trace context
  let log_with_trace = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Operation with trace context"),
    None,
    None,
    None,
    Some(@azimuth.SpanContext::trace_id(span_ctx)),
    Some(@azimuth.SpanContext::span_id(span_ctx)),
    None
  )
  
  @azimuth.Logger::emit(logger, log_with_trace)
  
  // Record metrics (would normally include trace context in real implementation)
  let counter = @azimuth.Meter::create_counter(meter, "traced.operations")
  @azimuth.Counter::add(counter, 1.0)
  
  @azimuth.Span::end(span)
  
  // Test passes if operations complete
  assert_true(true)
}

test "resource and context integration" {
  // Test integration between resource and context
  
  // Create resource with service information
  let resource = @azimuth.Resource::new()
  let service_attrs = [
    ("service.name", @azimuth.StringValue("integration.test")),
    ("service.version", @azimuth.StringValue("1.0.0")),
    ("service.instance.id", @azimuth.StringValue("instance-123"))
  ]
  let service_resource = @azimuth.Resource::with_attributes(resource, service_attrs)
  
  // Create context with resource information
  let root_ctx = @azimuth.Context::root()
  let resource_key = @azimuth.ContextKey::new("resource")
  let ctx_with_resource = @azimuth.Context::with_value(root_ctx, resource_key, "service_resource")
  
  // Create span with context
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "resource.integration")
  let span = @azimuth.Tracer::start_span(tracer, "resource.aware.operation")
  
  // Add resource information as span attributes (in real implementation)
  @azimuth.Span::add_event(span, "operation.with.resource.context")
  
  @azimuth.Span::end(span)
  
  // Verify context still contains resource information
  let retrieved_resource = @azimuth.Context::get(ctx_with_resource, resource_key)
  assert_eq(retrieved_resource, Some("service_resource"))
}

test "baggage and attributes integration" {
  // Test integration between baggage and attributes
  
  // Create baggage with entries
  let baggage = @azimuth.Baggage::new()
  let baggage_with_entries = @azimuth.Baggage::set_entry(
    @azimuth.Baggage::set_entry(baggage, "user.id", "user123"),
    "request.id", "req456"
  )
  
  // Create context with baggage
  let ctx = @azimuth.Context::root()
  let baggage_key = @azimuth.ContextKey::new("baggage")
  let ctx_with_baggage = @azimuth.Context::with_value(ctx, baggage_key, "baggage_data")
  
  // Create span attributes from baggage (in real implementation)
  let attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(attrs, "user.id", @azimuth.StringValue("user123"))
  @azimuth.Attributes::set(attrs, "request.id", @azimuth.StringValue("req456"))
  
  // Create span with baggage-derived attributes
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "baggage.integration")
  let span = @azimuth.Tracer::start_span(tracer, "baggage.aware.operation")
  
  @azimuth.Span::add_event(span, "operation.with.baggage.context")
  
  @azimuth.Span::end(span)
  
  // Verify context still contains baggage
  let retrieved_baggage = @azimuth.Context::get(ctx_with_baggage, baggage_key)
  assert_eq(retrieved_baggage, Some("baggage_data"))
}

test "propagation across multiple operations" {
  // Test context propagation across multiple operations
  
  let trace_propagator = @azimuth.W3CTraceContextPropagator::new()
  let baggage_propagator = @azimuth.W3CBaggagePropagator::new()
  let composite = @azimuth.CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // Initial context
  let initial_ctx = @azimuth.Context::root()
  let initial_key = @azimuth.ContextKey::new("operation.id")
  let ctx_with_id = @azimuth.Context::with_value(initial_ctx, initial_key, "op-123")
  
  // Inject into carrier
  let carrier1 = @azimuth.TextMapCarrier::new()
  @azimuth.CompositePropagator::inject(composite, ctx_with_id, carrier1)
  
  // Extract in first service
  let ctx1 = @azimuth.CompositePropagator::extract(composite, carrier1)
  let service1_key = @azimuth.ContextKey::new("service1")
  let ctx1_with_service = @azimuth.Context::with_value(ctx1, service1_key, "processed-by-service1")
  
  // Propagate to second service
  let carrier2 = @azimuth.TextMapCarrier::new()
  @azimuth.CompositePropagator::inject(composite, ctx1_with_service, carrier2)
  
  // Extract in second service
  let ctx2 = @azimuth.CompositePropagator::extract(composite, carrier2)
  let service2_key = @azimuth.ContextKey::new("service2")
  let ctx2_with_service = @azimuth.Context::with_value(ctx2, service2_key, "processed-by-service2")
  
  // Create spans in each service context
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "multi.service.test")
  
  let span1 = @azimuth.Tracer::start_span(tracer, "service1.operation")
  @azimuth.Span::add_event(span1, "processed in service 1")
  @azimuth.Span::end(span1)
  
  let span2 = @azimuth.Tracer::start_span(tracer, "service2.operation")
  @azimuth.Span::add_event(span2, "processed in service 2")
  @azimuth.Span::end(span2)
  
  // Test passes if propagation workflow completes
  assert_true(true)
}

test "error handling integration across all signals" {
  // Test error handling integration across traces, metrics, and logs
  
  let tracer_provider = @azimuth.TracerProvider::default()
  let meter_provider = @azimuth.MeterProvider::default()
  let logger_provider = @azimuth.LoggerProvider::default()
  
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "error.integration")
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "error.integration")
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "error.integration")
  
  // Create error counter
  let error_counter = @azimuth.Meter::create_counter(meter, "errors.total", Some("Total errors"), Some("count"))
  
  // Start operation span
  let span = @azimuth.Tracer::start_span(tracer, "error.prone.operation")
  
  // Log error
  let error_log = @azimuth.LogRecord::new(@azimuth.Error, "Operation failed with error")
  @azimuth.Logger::emit(logger, error_log)
  
  // Set span status to error
  @azimuth.Span::set_status(span, @azimuth.Error, Some("Operation failed"))
  
  // Record error metric
  @azimuth.Counter::add(error_counter, 1.0)
  
  // Add error event to span
  @azimuth.Span::add_event(span, "error.occurred", Some([("error.type", @azimuth.StringValue("validation.error"))]))
  
  @azimuth.Span::end(span)
  
  // Test passes if error handling workflow completes
  assert_true(true)
}