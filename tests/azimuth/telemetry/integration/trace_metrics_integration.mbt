test "telemetry integration traces and metrics" {
  // Test integration between traces and metrics
  
  // Setup providers
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "integration.test")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "integration.test")
  
  // Create instruments
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(
    meter,
    "operation.counter",
    Some("Operation count"),
    Some("operations")
  )
  
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(
    meter,
    "operation.duration",
    Some("Operation duration"),
    Some("ms")
  )
  
  // Simulate operation with tracing and metrics
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (operation_ctx, operation_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "test.operation")
  
  // Record operation start
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0)
  @azimuth.telemetry.api.trace.Span.add_event(operation_span, "operation.started", None, None)
  
  // Simulate work
  let work_span = @azimuth.telemetry.sdk.trace.Tracer.start_span_with_attributes(
    tracer,
    operation_ctx,
    "work.phase",
    Some(@azimuth.telemetry.api.trace.SpanKind.Internal),
    Some([("work.type", @azimuth.telemetry.api.common.StringValue("processing"))])
  )
  
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 100.0)
  @azimuth.telemetry.api.trace.Span.add_event(work_span, "work.completed", None, None)
  @azimuth.telemetry.api.trace.Span.end(work_span)
  
  // Complete operation
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 150.0)
  @azimuth.telemetry.api.trace.Span.set_status(operation_span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Operation completed"))
  @azimuth.telemetry.api.trace.Span.end(operation_span)
  
  // Verify correlation through trace context in metrics
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "trace.id", @azimuth.telemetry.api.common.StringValue(
    @azimuth.telemetry.api.trace.SpanContext.trace_id(@azimuth.telemetry.api.trace.Span.span_context(operation_span))
  ))
  
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0, Some(attrs))
}