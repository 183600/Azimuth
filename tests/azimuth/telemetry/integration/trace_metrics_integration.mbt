test "telemetry integration traces and metrics" {
  // Test integration between traces and metrics
  
  // Setup providers
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = telemetry.sdk.metrics.MeterProvider.default()
  
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "integration.test")
  let meter = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "integration.test")
  
  // Create instruments
  let counter = telemetry.sdk.metrics.Meter.create_counter(
    meter,
    "operation.counter",
    Some("Operation count"),
    Some("operations")
  )
  
  let histogram = telemetry.sdk.metrics.Meter.create_histogram(
    meter,
    "operation.duration",
    Some("Operation duration"),
    Some("ms")
  )
  
  // Simulate operation with tracing and metrics
  let ctx = telemetry.api.context.Context.root()
  let (operation_ctx, operation_span) = telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "test.operation")
  
  // Record operation start
  telemetry.sdk.metrics.Counter.add(counter, 1.0)
  telemetry.api.trace.Span.add_event(operation_span, "operation.started", None, None)
  
  // Simulate work
  let work_span = telemetry.sdk.trace.Tracer.start_span_with_attributes(
    tracer,
    operation_ctx,
    "work.phase",
    Some(telemetry.api.trace.SpanKind.Internal),
    Some([("work.type", telemetry.api.common.StringValue("processing"))])
  )
  
  telemetry.sdk.metrics.Histogram.record(histogram, 100.0)
  telemetry.api.trace.Span.add_event(work_span, "work.completed", None, None)
  telemetry.api.trace.Span.end(work_span)
  
  // Complete operation
  telemetry.sdk.metrics.Histogram.record(histogram, 150.0)
  telemetry.api.trace.Span.set_status(operation_span, telemetry.api.trace.StatusCode.Ok, Some("Operation completed"))
  telemetry.api.trace.Span.end(operation_span)
  
  // Verify correlation through trace context in metrics
  let attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(attrs, "trace.id", telemetry.api.common.StringValue(
    telemetry.api.trace.SpanContext.trace_id(telemetry.api.trace.Span.span_context(operation_span))
  ))
  
  telemetry.sdk.metrics.Counter.add(counter, 1.0, Some(attrs))
}