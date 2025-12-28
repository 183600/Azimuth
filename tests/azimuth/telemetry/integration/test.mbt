test "end-to-end telemetry flow" {
  // Test complete end-to-end telemetry flow with traces, metrics, and logs
  
  // Initialize providers
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = telemetry.sdk.metrics.MeterProvider.default()
  let logs_provider = telemetry.sdk.logs.LoggerProvider.default()
  
  // Create instruments
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "e2e.tracer")
  let meter = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "e2e.meter")
  let logger = telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "e2e.logger")
  
  let counter = telemetry.sdk.metrics.Meter.create_counter(meter, "e2e.operations")
  let histogram = telemetry.sdk.metrics.Meter.create_histogram(meter, "e2e.duration")
  
  // Start trace
  let ctx = telemetry.api.context.Context.root()
  let (ctx_with_span, span) = telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "e2e.operation")
  
  // Log start of operation
  telemetry.sdk.logs.Logger.info_with_context(logger, ctx_with_span, "Starting E2E operation")
  
  // Record metrics
  telemetry.sdk.metrics.Counter.add(counter, 1.0)
  
  // Simulate work with child span
  let (child_ctx, child_span) = telemetry.sdk.trace.Tracer.start_span(tracer, ctx_with_span, "e2e.suboperation")
  
  let start_time = telemetry.sdk.platform.time.Clock.now_unix_nanos(telemetry.sdk.platform.time.Clock.system())
  
  // Log suboperation
  telemetry.sdk.logs.Logger.debug_with_context(logger, child_ctx, "Processing suboperation")
  
  // Record more metrics
  let attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(attrs, "operation", telemetry.api.common.StringValue("suboperation"))
  
  telemetry.sdk.metrics.Histogram.record(histogram, 25.5, Some(attrs))
  
  // End child span
  telemetry.api.trace.Span.end(child_span)
  
  let end_time = telemetry.sdk.platform.time.Clock.now_unix_nanos(telemetry.sdk.platform.time.Clock.system())
  let duration = (end_time - start_time).to_double() / 1000000.0  // Convert to milliseconds
  
  // Record total duration
  telemetry.sdk.metrics.Histogram.record(histogram, duration)
  
  // Log completion
  telemetry.sdk.logs.Logger.info_with_context(logger, ctx_with_span, "E2E operation completed")
  
  // End main span
  telemetry.api.trace.Span.end(span)
}

test "cross-service telemetry propagation" {
  // Test telemetry propagation across service boundaries
  
  // Service A: Create and inject context
  let service_a_tracer = telemetry.sdk.trace.TracerProvider.get_tracer(
    telemetry.sdk.trace.TracerProvider.default(), 
    "service-a"
  )
  let service_a_logger = telemetry.sdk.logs.LoggerProvider.get_logger(
    telemetry.sdk.logs.LoggerProvider.default(), 
    "service-a"
  )
  
  let ctx = telemetry.api.context.Context.root()
  let (service_a_ctx, service_a_span) = telemetry.sdk.trace.Tracer.start_span(service_a_tracer, ctx, "service-a.operation")
  
  // Log in Service A
  telemetry.sdk.logs.Logger.info_with_context(service_a_logger, service_a_ctx, "Service A: Processing request")
  
  // Create carrier for propagation
  let carrier = telemetry.api.propagation.TextMapCarrier.new()
  let propagator = telemetry.api.propagation.CompositePropagator.new([
    telemetry.api.propagation.W3CTraceContextPropagator.new()
  ])
  
  // Inject context into carrier
  telemetry.api.propagation.CompositePropagator.inject(propagator, service_a_ctx, carrier)
  
  // Service B: Extract and use context
  let service_b_tracer = telemetry.sdk.trace.TracerProvider.get_tracer(
    telemetry.sdk.trace.TracerProvider.default(), 
    "service-b"
  )
  let service_b_logger = telemetry.sdk.logs.LoggerProvider.get_logger(
    telemetry.sdk.logs.LoggerProvider.default(), 
    "service-b"
  )
  
  // Extract context from carrier
  let service_b_ctx = telemetry.api.propagation.CompositePropagator.extract(propagator, carrier)
  
  // Continue trace in Service B
  let (service_b_ctx_with_span, service_b_span) = telemetry.sdk.trace.Tracer.start_span(
    service_b_tracer, 
    service_b_ctx, 
    "service-b.operation"
  )
  
  // Log in Service B
  telemetry.sdk.logs.Logger.info_with_context(service_b_logger, service_b_ctx_with_span, "Service B: Processing forwarded request")
  
  // End spans
  telemetry.api.trace.Span.end(service_b_span)
  telemetry.api.trace.Span.end(service_a_span)
}

test "telemetry with resource attributes" {
  // Test telemetry with resource-level attributes
  
  // Create resources with different attributes
  let base_resource = telemetry.api.common.Resource.with_attributes(
    telemetry.api.common.Resource.new(),
    [
      ("service.name", telemetry.api.common.StringValue("telemetry-test-service")),
      ("service.version", telemetry.api.common.StringValue("1.0.0")),
      ("service.instance.id", telemetry.api.common.StringValue("instance-123"))
    ]
  )
  
  let env_resource = telemetry.api.common.Resource.with_attributes(
    telemetry.api.common.Resource.new(),
    [
      ("deployment.environment", telemetry.api.common.StringValue("test")),
      ("host.name", telemetry.api.common.StringValue("test-host")),
      ("os.type", telemetry.api.common.StringValue("linux"))
    ]
  )
  
  // Merge resources
  let merged_resource = telemetry.api.common.Resource.merge(base_resource, env_resource)
  
  // Create providers with merged resource
  let trace_provider = telemetry.sdk.trace.TracerProvider.with_resource(merged_resource)
  let metrics_provider = telemetry.sdk.metrics.MeterProvider.with_resource(merged_resource)
  let logs_provider = telemetry.sdk.logs.LoggerProvider.with_resource(merged_resource)
  
  // Create instruments
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "resource.test.tracer")
  let meter = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "resource.test.meter")
  let logger = telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "resource.test.logger")
  
  // Create telemetry with resource context
  let (_, span) = telemetry.sdk.trace.Tracer.start_span(tracer, "resource.operation")
  telemetry.api.trace.Span.set_attribute(span, "operation.type", telemetry.api.common.StringValue("resource_test"))
  
  let counter = telemetry.sdk.metrics.Meter.create_counter(meter, "resource.operations")
  telemetry.sdk.metrics.Counter.add(counter, 1.0)
  
  telemetry.sdk.logs.Logger.info(logger, "Resource-aware telemetry test")
  
  telemetry.api.trace.Span.end(span)
}

test "telemetry error handling and recovery" {
  // Test telemetry error handling and recovery scenarios
  
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = telemetry.sdk.metrics.MeterProvider.default()
  let logs_provider = telemetry.sdk.logs.LoggerProvider.default()
  
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "error.test.tracer")
  let meter = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "error.test.meter")
  let logger = telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "error.test.logger")
  
  let error_counter = telemetry.sdk.metrics.Meter.create_counter(meter, "errors.total")
  let recovery_counter = telemetry.sdk.metrics.Meter.create_counter(meter, "recoveries.total")
  
  // Simulate operation with error
  let (_, span) = telemetry.sdk.trace.Tracer.start_span(tracer, "error.prone.operation")
  
  telemetry.sdk.logs.Logger.info_with_context(logger, telemetry.api.context.Context.root(), "Starting error-prone operation")
  
  // Record error
  telemetry.sdk.metrics.Counter.add(error_counter, 1.0)
  
  telemetry.api.trace.Span.record_exception(
    span,
    "Simulated operation error",
    Some("OperationError"),
    Some("at error.prone.operation()\n  at main()"),
    None
  )
  
  telemetry.api.trace.Span.set_status(span, telemetry.api.trace.StatusCode.Error, Some("Operation failed"))
  
  telemetry.sdk.logs.Logger.error_with_context(logger, telemetry.api.context.Context.root(), "Operation failed with error")
  
  telemetry.api.trace.Span.end(span)
  
  // Simulate recovery operation
  let (_, recovery_span) = telemetry.sdk.trace.Tracer.start_span(tracer, "recovery.operation")
  
  telemetry.sdk.metrics.Counter.add(recovery_counter, 1.0)
  
  telemetry.sdk.logs.Logger.info_with_context(logger, telemetry.api.context.Context.root(), "Starting recovery operation")
  
  // Recovery succeeds
  telemetry.api.trace.Span.set_status(recovery_span, telemetry.api.trace.StatusCode.Ok, Some("Recovery completed"))
  
  telemetry.sdk.logs.Logger.info_with_context(logger, telemetry.api.context.Context.root(), "Recovery operation completed successfully")
  
  telemetry.api.trace.Span.end(recovery_span)
}

test "telemetry performance under load" {
  // Test telemetry performance under high load
  
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = telemetry.sdk.metrics.MeterProvider.default()
  let logs_provider = telemetry.sdk.logs.LoggerProvider.default()
  
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "load.test.tracer")
  let meter = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "load.test.meter")
  let logger = telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "load.test.logger")
  
  let request_counter = telemetry.sdk.metrics.Meter.create_counter(meter, "requests.total")
  let duration_histogram = telemetry.sdk.metrics.Meter.create_histogram(meter, "request.duration")
  
  let start_time = telemetry.sdk.platform.time.Clock.now_unix_nanos(telemetry.sdk.platform.time.Clock.system())
  
  // Simulate high load
  for i in 0..1000 {
    let request_start = telemetry.sdk.platform.time.Clock.now_unix_nanos(telemetry.sdk.platform.time.Clock.system())
    
    // Create span for each request
    let (_, span) = telemetry.sdk.trace.Tracer.start_span(tracer, "request." + i.to_string())
    
    // Set request attributes
    telemetry.api.trace.Span.set_attribute(span, "request.id", telemetry.api.common.IntValue(i))
    telemetry.api.trace.Span.set_attribute(span, "http.method", telemetry.api.common.StringValue("GET"))
    telemetry.api.trace.Span.set_attribute(span, "http.status_code", telemetry.api.common.IntValue(200))
    
    // Log request
    telemetry.sdk.logs.Logger.debug(logger, "Processing request " + i.to_string())
    
    // Record metrics
    telemetry.sdk.metrics.Counter.add(request_counter, 1.0)
    
    let request_end = telemetry.sdk.platform.time.Clock.now_unix_nanos(telemetry.sdk.platform.time.Clock.system())
    let request_duration = (request_end - request_start).to_double() / 1000000.0
    
    telemetry.sdk.metrics.Histogram.record(duration_histogram, request_duration)
    
    // End span
    telemetry.api.trace.Span.end(span)
  }
  
  let end_time = telemetry.sdk.platform.time.Clock.now_unix_nanos(telemetry.sdk.platform.time.Clock.system())
  let total_duration = (end_time - start_time).to_double() / 1000000.0
  
  // Log performance summary
  let perf_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(perf_attrs, "total.requests", telemetry.api.common.IntValue(1000))
  telemetry.api.common.Attributes.set(perf_attrs, "total.duration.ms", telemetry.api.common.FloatValue(total_duration))
  telemetry.api.common.Attributes.set(perf_attrs, "requests.per.second", telemetry.api.common.FloatValue(1000.0 / (total_duration / 1000.0)))
  
  telemetry.sdk.logs.Logger.info_with_attributes(logger, "Load test completed", perf_attrs)
}