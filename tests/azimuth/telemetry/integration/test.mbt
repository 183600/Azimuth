test "end-to-end telemetry flow" {
  // Test complete end-to-end telemetry flow with traces, metrics, and logs
  
  // Initialize providers
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  
  // Create instruments
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "e2e.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "e2e.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "e2e.logger")
  
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "e2e.operations")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "e2e.duration")
  
  // Start trace
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (ctx_with_span, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "e2e.operation")
  
  // Log start of operation
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(logger, ctx_with_span, "Starting E2E operation")
  
  // Record metrics
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0)
  
  // Simulate work with child span
  let (child_ctx, child_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, ctx_with_span, "e2e.suboperation")
  
  let start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  // Log suboperation
  @azimuth.telemetry.sdk.logs.Logger.debug_with_context(logger, child_ctx, "Processing suboperation")
  
  // Record more metrics
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "operation", @azimuth.telemetry.api.common.StringValue("suboperation"))
  
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 25.5, Some(attrs))
  
  // End child span
  @azimuth.telemetry.api.trace.Span.end(child_span)
  
  let end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let duration = (end_time - start_time).to_double() / 1000000.0  // Convert to milliseconds
  
  // Record total duration
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, duration)
  
  // Log completion
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(logger, ctx_with_span, "E2E operation completed")
  
  // End main span
  @azimuth.telemetry.api.trace.Span.end(span)
}

test "cross-service telemetry propagation" {
  // Test telemetry propagation across service boundaries
  
  // Service A: Create and inject context
  let service_a_tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(
    @azimuth.telemetry.sdk.trace.TracerProvider.default(), 
    "service-a"
  )
  let service_a_logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(
    @azimuth.telemetry.sdk.logs.LoggerProvider.default(), 
    "service-a"
  )
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (service_a_ctx, service_a_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(service_a_tracer, ctx, "service-a.operation")
  
  // Log in Service A
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(service_a_logger, service_a_ctx, "Service A: Processing request")
  
  // Create carrier for propagation
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  let propagator = @azimuth.telemetry.api.propagation.CompositePropagator.new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator.new()
  ])
  
  // Inject context into carrier
  @azimuth.telemetry.api.propagation.CompositePropagator.inject(propagator, service_a_ctx, carrier)
  
  // Service B: Extract and use context
  let service_b_tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(
    @azimuth.telemetry.sdk.trace.TracerProvider.default(), 
    "service-b"
  )
  let service_b_logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(
    @azimuth.telemetry.sdk.logs.LoggerProvider.default(), 
    "service-b"
  )
  
  // Extract context from carrier
  let service_b_ctx = @azimuth.telemetry.api.propagation.CompositePropagator.extract(propagator, carrier)
  
  // Continue trace in Service B
  let (service_b_ctx_with_span, service_b_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(
    service_b_tracer, 
    service_b_ctx, 
    "service-b.operation"
  )
  
  // Log in Service B
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(service_b_logger, service_b_ctx_with_span, "Service B: Processing forwarded request")
  
  // End spans
  @azimuth.telemetry.api.trace.Span.end(service_b_span)
  @azimuth.telemetry.api.trace.Span.end(service_a_span)
}

test "telemetry with resource attributes" {
  // Test telemetry with resource-level attributes
  
  // Create resources with different attributes
  let base_resource = @azimuth.telemetry.api.common.Resource.with_attributes(
    @azimuth.telemetry.api.common.Resource.new(),
    [
      ("service.name", @azimuth.telemetry.api.common.StringValue("telemetry-test-service")),
      ("service.version", @azimuth.telemetry.api.common.StringValue("1.0.0")),
      ("service.instance.id", @azimuth.telemetry.api.common.StringValue("instance-123"))
    ]
  )
  
  let env_resource = @azimuth.telemetry.api.common.Resource.with_attributes(
    @azimuth.telemetry.api.common.Resource.new(),
    [
      ("deployment.environment", @azimuth.telemetry.api.common.StringValue("test")),
      ("host.name", @azimuth.telemetry.api.common.StringValue("test-host")),
      ("os.type", @azimuth.telemetry.api.common.StringValue("linux"))
    ]
  )
  
  // Merge resources
  let merged_resource = @azimuth.telemetry.api.common.Resource.merge(base_resource, env_resource)
  
  // Create providers with merged resource
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.with_resource(merged_resource)
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.with_resource(merged_resource)
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.with_resource(merged_resource)
  
  // Create instruments
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "resource.test.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "resource.test.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "resource.test.logger")
  
  // Create telemetry with resource context
  let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "resource.operation")
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "operation.type", @azimuth.telemetry.api.common.StringValue("resource_test"))
  
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "resource.operations")
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0)
  
  @azimuth.telemetry.sdk.logs.Logger.info(logger, "Resource-aware telemetry test")
  
  @azimuth.telemetry.api.trace.Span.end(span)
}

test "telemetry error handling and recovery" {
  // Test telemetry error handling and recovery scenarios
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "error.test.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "error.test.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "error.test.logger")
  
  let error_counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "errors.total")
  let recovery_counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "recoveries.total")
  
  // Simulate operation with error
  let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "error.prone.operation")
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(logger, @azimuth.telemetry.api.context.Context.root(), "Starting error-prone operation")
  
  // Record error
  @azimuth.telemetry.sdk.metrics.Counter.add(error_counter, 1.0)
  
  @azimuth.telemetry.api.trace.Span.record_exception(
    span,
    "Simulated operation error",
    Some("OperationError"),
    Some("at error.prone.operation()\n  at main()"),
    None
  )
  
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Error, Some("Operation failed"))
  
  @azimuth.telemetry.sdk.logs.Logger.error_with_context(logger, @azimuth.telemetry.api.context.Context.root(), "Operation failed with error")
  
  @azimuth.telemetry.api.trace.Span.end(span)
  
  // Simulate recovery operation
  let (_, recovery_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "recovery.operation")
  
  @azimuth.telemetry.sdk.metrics.Counter.add(recovery_counter, 1.0)
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(logger, @azimuth.telemetry.api.context.Context.root(), "Starting recovery operation")
  
  // Recovery succeeds
  @azimuth.telemetry.api.trace.Span.set_status(recovery_span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Recovery completed"))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(logger, @azimuth.telemetry.api.context.Context.root(), "Recovery operation completed successfully")
  
  @azimuth.telemetry.api.trace.Span.end(recovery_span)
}

test "telemetry performance under load" {
  // Test telemetry performance under high load
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "load.test.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "load.test.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "load.test.logger")
  
  let request_counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "requests.total")
  let duration_histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "request.duration")
  
  let start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  // Simulate high load
  for i in 0..1000 {
    let request_start = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
    
    // Create span for each request
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "request." + i.to_string())
    
    // Set request attributes
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "request.id", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "http.method", @azimuth.telemetry.api.common.StringValue("GET"))
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "http.status_code", @azimuth.telemetry.api.common.IntValue(200))
    
    // Log request
    @azimuth.telemetry.sdk.logs.Logger.debug(logger, "Processing request " + i.to_string())
    
    // Record metrics
    @azimuth.telemetry.sdk.metrics.Counter.add(request_counter, 1.0)
    
    let request_end = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
    let request_duration = (request_end - request_start).to_double() / 1000000.0
    
    @azimuth.telemetry.sdk.metrics.Histogram.record(duration_histogram, request_duration)
    
    // End span
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  let end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let total_duration = (end_time - start_time).to_double() / 1000000.0
  
  // Log performance summary
  let perf_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "total.requests", @azimuth.telemetry.api.common.IntValue(1000))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "total.duration.ms", @azimuth.telemetry.api.common.FloatValue(total_duration))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "requests.per.second", @azimuth.telemetry.api.common.FloatValue(1000.0 / (total_duration / 1000.0)))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_attributes(logger, "Load test completed", perf_attrs)
}