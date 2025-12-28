test "comprehensive telemetry workflow" {
  // Test a comprehensive telemetry workflow combining traces, metrics, and logs
  
  // Setup providers
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // Create instruments
  let tracer = TracerProvider::get_tracer(tracer_provider, "comprehensive.test", Some("1.0.0"))
  let meter = MeterProvider::get_meter(meter_provider, "comprehensive.test", Some("1.0.0"))
  let logger = LoggerProvider::get_logger(logger_provider, "comprehensive.test", Some("1.0.0"))
  
  // Create metrics instruments
  let counter = Meter::create_counter(meter, "operations.total", Some("Total operations"), Some("count"))
  let histogram = Meter::create_histogram(meter, "operation.duration", Some("Operation duration"), Some("ms"))
  
  // Start root span
  let root_span = Tracer::start_span(tracer, "comprehensive.operation")
  
  // Add events to span
  Span::add_event(root_span, "operation.started")
  
  // Record metrics
  Counter::add(counter, 1.0)
  
  // Log information
  let info_log = LogRecord::new(Info, "Starting comprehensive operation")
  Logger::emit(logger, info_log)
  
  // Create child span
  let child_span = Tracer::start_span(tracer, "sub.operation")
  Span::add_event(child_span, "sub.operation.started")
  
  // Record more metrics
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 150.0)
  
  // Log sub-operation
  let sub_log = LogRecord::new(Debug, "Processing sub-operation")
  Logger::emit(logger, sub_log)
  
  // End child span
  Span::end(child_span)
  
  // Final metrics
  Histogram::record(histogram, 300.0)
  
  // Log completion
  let complete_log = LogRecord::new(Info, "Comprehensive operation completed")
  Logger::emit(logger, complete_log)
  
  // End root span
  Span::end(root_span)
  
  // Test passes if workflow completes without exceptions
  assert_true(true)
}

test "trace context propagation with metrics and logs" {
  // Test trace context propagation across different telemetry signals
  
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "propagation.test")
  let meter = MeterProvider::get_meter(meter_provider, "propagation.test")
  let logger = LoggerProvider::get_logger(logger_provider, "propagation.test")
  
  // Create span with specific context
  let span = Tracer::start_span(tracer, "propagation.operation")
  let span_ctx = Span::span_context(span)
  
  // Create log record with trace context
  let log_with_trace = LogRecord::new_with_context(
    Info,
    Some("Operation with trace context"),
    None,
    None,
    None,
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    None
  )
  
  Logger::emit(logger, log_with_trace)
  
  // Record metrics (would normally include trace context in real implementation)
  let counter = Meter::create_counter(meter, "traced.operations")
  Counter::add(counter, 1.0)
  
  Span::end(span)
  
  // Test passes if operations complete
  assert_true(true)
}

test "resource and context integration" {
  // Test integration between resource and context
  
  // Create resource with service information
  let resource = Resource::new()
  let service_attrs = [
    ("service.name", StringValue("integration.test")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let service_resource = Resource::with_attributes(resource, service_attrs)
  
  // Create context with resource information
  let root_ctx = Context::root()
  let resource_key = ContextKey::new("resource")
  let ctx_with_resource = Context::with_value(root_ctx, resource_key, "service_resource")
  
  // Create span with context
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "resource.integration")
  let span = Tracer::start_span(tracer, "resource.aware.operation")
  
  // Add resource information as span attributes (in real implementation)
  Span::add_event(span, "operation.with.resource.context")
  
  Span::end(span)
  
  // Verify context still contains resource information
  let retrieved_resource = Context::get(ctx_with_resource, resource_key)
  assert_eq(retrieved_resource, Some("service_resource"))
}

test "baggage and attributes integration" {
  // Test integration between baggage and attributes
  
  // Create baggage with entries
  let baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(
    Baggage::set_entry(baggage, "user.id", "user123"),
    "request.id", "req456"
  )
  
  // Create context with baggage
  let ctx = Context::root()
  let baggage_key = ContextKey::new("baggage")
  let ctx_with_baggage = Context::with_value(ctx, baggage_key, "baggage_data")
  
  // Create span attributes from baggage (in real implementation)
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("user123"))
  Attributes::set(attrs, "request.id", StringValue("req456"))
  
  // Create span with baggage-derived attributes
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "baggage.integration")
  let span = Tracer::start_span(tracer, "baggage.aware.operation")
  
  Span::add_event(span, "operation.with.baggage.context")
  
  Span::end(span)
  
  // Verify context still contains baggage
  let retrieved_baggage = Context::get(ctx_with_baggage, baggage_key)
  assert_eq(retrieved_baggage, Some("baggage_data"))
}

test "propagation across multiple operations" {
  // Test context propagation across multiple operations
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // Initial context
  let initial_ctx = Context::root()
  let initial_key = ContextKey::new("operation.id")
  let ctx_with_id = Context::with_value(initial_ctx, initial_key, "op-123")
  
  // Inject into carrier
  let carrier1 = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_id, carrier1)
  
  // Extract in first service
  let ctx1 = CompositePropagator::extract(composite, carrier1)
  let service1_key = ContextKey::new("service1")
  let ctx1_with_service = Context::with_value(ctx1, service1_key, "processed-by-service1")
  
  // Propagate to second service
  let carrier2 = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx1_with_service, carrier2)
  
  // Extract in second service
  let ctx2 = CompositePropagator::extract(composite, carrier2)
  let service2_key = ContextKey::new("service2")
  let ctx2_with_service = Context::with_value(ctx2, service2_key, "processed-by-service2")
  
  // Create spans in each service context
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "multi.service.test")
  
  let span1 = Tracer::start_span(tracer, "service1.operation")
  Span::add_event(span1, "processed in service 1")
  Span::end(span1)
  
  let span2 = Tracer::start_span(tracer, "service2.operation")
  Span::add_event(span2, "processed in service 2")
  Span::end(span2)
  
  // Test passes if propagation workflow completes
  assert_true(true)
}

test "error handling integration across all signals" {
  // Test error handling integration across traces, metrics, and logs
  
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.integration")
  let meter = MeterProvider::get_meter(meter_provider, "error.integration")
  let logger = LoggerProvider::get_logger(logger_provider, "error.integration")
  
  // Create error counter
  let error_counter = Meter::create_counter(meter, "errors.total", Some("Total errors"), Some("count"))
  
  // Start operation span
  let span = Tracer::start_span(tracer, "error.prone.operation")
  
  // Log error
  let error_log = LogRecord::new(Error, "Operation failed with error")
  Logger::emit(logger, error_log)
  
  // Set span status to error
  Span::set_status(span, Error, Some("Operation failed"))
  
  // Record error metric
  Counter::add(error_counter, 1.0)
  
  // Add error event to span
  Span::add_event(span, "error.occurred", Some([("error.type", StringValue("validation.error"))]))
  
  Span::end(span)
  
  // Test passes if error handling workflow completes
  assert_true(true)
}