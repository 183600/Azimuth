test "resource merging and conflict resolution" {
  // Test resource merging with attribute conflicts
  let base_resource = @azimuth.telemetry.api.common.Resource.new()
  let base_with_attrs = @azimuth.telemetry.api.common.Resource.with_attributes(base_resource, [
    ("service.name", @azimuth.telemetry.api.common.StringValue("base-service")),
    ("service.version", @azimuth.telemetry.api.common.StringValue("1.0.0")),
    ("environment", @azimuth.telemetry.api.common.StringValue("development"))
  ])
  
  let override_resource = @azimuth.telemetry.api.common.Resource.new()
  let override_with_attrs = @azimuth.telemetry.api.common.Resource.with_attributes(override_resource, [
    ("service.name", @azimuth.telemetry.api.common.StringValue("override-service")),
    ("service.instance.id", @azimuth.telemetry.api.common.StringValue("instance-123")),
    ("environment", @azimuth.telemetry.api.common.StringValue("production"))
  ])
  
  let merged = @azimuth.telemetry.api.common.Resource.merge(base_with_attrs, override_with_attrs)
  
  // Override resource should take precedence
  assert_eq(@azimuth.telemetry.api.common.Resource.get_attribute(merged, "service.name"), 
           Some(@azimuth.telemetry.api.common.StringValue("override-service")))
  assert_eq(@azimuth.telemetry.api.common.Resource.get_attribute(merged, "service.version"), 
           Some(@azimuth.telemetry.api.common.StringValue("1.0.0")))
  assert_eq(@azimuth.telemetry.api.common.Resource.get_attribute(merged, "environment"), 
           Some(@azimuth.telemetry.api.common.StringValue("production")))
  assert_eq(@azimuth.telemetry.api.common.Resource.get_attribute(merged, "service.instance.id"), 
           Some(@azimuth.telemetry.api.common.StringValue("instance-123")))
}

test "context with multiple values and hierarchical keys" {
  // Test context operations with multiple values and hierarchical key structures
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  let user_key = @azimuth.telemetry.api.context.ContextKey.new("user.id")
  let request_key = @azimuth.telemetry.api.context.ContextKey.new("request.id")
  let trace_key = @azimuth.telemetry.api.context.ContextKey.new("trace.correlation_id")
  
  let ctx_with_user = @azimuth.telemetry.api.context.Context.with_value(ctx, user_key, "user-12345")
  let ctx_with_request = @azimuth.telemetry.api.context.Context.with_value(ctx_with_user, request_key, "req-abcdef")
  let ctx_final = @azimuth.telemetry.api.context.Context.with_value(ctx_with_request, trace_key, "trace-789")
  
  // Test that all values are preserved
  assert_eq(@azimuth.telemetry.api.context.Context.get(ctx_final, user_key), Some("user-12345"))
  assert_eq(@azimuth.telemetry.api.context.Context.get(ctx_final, request_key), Some("req-abcdef"))
  assert_eq(@azimuth.telemetry.api.context.Context.get(ctx_final, trace_key), Some("trace-789"))
  
  // Test that missing keys return None
  let missing_key = @azimuth.telemetry.api.context.ContextKey.new("missing.key")
  assert_eq(@azimuth.telemetry.api.context.Context.get(ctx_final, missing_key), None)
}

test "span with complex attribute types and nested structures" {
  // Test span with complex attribute types including arrays and nested values
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "complex-test")
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  let (_, span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "complex.span")
  
  // Set various attribute types
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "string.value", @azimuth.telemetry.api.common.StringValue("test"))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "int.value", @azimuth.telemetry.api.common.IntValue(42))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "float.value", @azimuth.telemetry.api.common.FloatValue(3.14159))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "bool.value", @azimuth.telemetry.api.common.BoolValue(true))
  
  // Set array attributes
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "string.array", @azimuth.telemetry.api.common.ArrayStringValue(["a", "b", "c"]))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "int.array", @azimuth.telemetry.api.common.ArrayIntValue([1, 2, 3, 4, 5]))
  
  // Add complex event with attributes
  let event_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(event_attrs, "event.type", @azimuth.telemetry.api.common.StringValue("complex"))
  @azimuth.telemetry.api.common.Attributes.set(event_attrs, "event.data", @azimuth.telemetry.api.common.StringValue("{\"key\":\"value\"}"))
  
  @azimuth.telemetry.api.trace.Span.add_event(span, "complex.event", Some(event_attrs), None)
  
  @azimuth.telemetry.api.trace.Span.end(span)
}

test "metrics with observable instruments and asynchronous measurements" {
  // Test metrics with observable instruments and asynchronous measurement patterns
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "async-test")
  
  // Create multiple instruments for testing
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "async.counter")
  let histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(meter, "async.histogram")
  let up_down_counter = @azimuth.telemetry.api.metrics.Meter.create_up_down_counter(meter, "async.up_down")
  
  // Simulate asynchronous measurements with different attributes
  let attrs1 = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs1, "worker.id", @azimuth.telemetry.api.common.StringValue("worker-1"))
  @azimuth.telemetry.api.common.Attributes.set(attrs1, "operation", @azimuth.telemetry.api.common.StringValue("process"))
  
  let attrs2 = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs2, "worker.id", @azimuth.telemetry.api.common.StringValue("worker-2"))
  @azimuth.telemetry.api.common.Attributes.set(attrs2, "operation", @azimuth.telemetry.api.common.StringValue("process"))
  
  // Record measurements with different timing patterns
  for i in 0..10 {
    @azimuth.telemetry.api.metrics.Counter.add(counter, i.to_double(), Some(attrs1))
    @azimuth.telemetry.api.metrics.Histogram.record(histogram, (i * 10).to_double(), Some(attrs1))
    @azimuth.telemetry.api.metrics.UpDownCounter.add(up_down_counter, i.to_double(), Some(attrs2))
  }
  
  // Test instrument properties
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.name(counter), "async.counter")
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.name(histogram), "async.histogram")
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.name(up_down_counter), "async.up_down")
}

test "log records with structured data and correlation" {
  // Test log records with structured data and correlation across multiple logs
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "structured-test")
  
  // Create correlation context
  let correlation_id = "corr-12345"
  let request_id = "req-abcdef"
  
  // Create structured attributes
  let base_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(base_attrs, "correlation.id", @azimuth.telemetry.api.common.StringValue(correlation_id))
  @azimuth.telemetry.api.common.Attributes.set(base_attrs, "request.id", @azimuth.telemetry.api.common.StringValue(request_id))
  
  // Log multiple related events
  let start_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(start_attrs, "correlation.id", @azimuth.telemetry.api.common.StringValue(correlation_id))
  @azimuth.telemetry.api.common.Attributes.set(start_attrs, "request.id", @azimuth.telemetry.api.common.StringValue(request_id))
  @azimuth.telemetry.api.common.Attributes.set(start_attrs, "event.phase", @azimuth.telemetry.api.common.StringValue("start"))
  
  @azimuth.telemetry.api.logs.Logger.info_with_attributes(logger, "Processing started", start_attrs)
  
  let process_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(process_attrs, "correlation.id", @azimuth.telemetry.api.common.StringValue(correlation_id))
  @azimuth.telemetry.api.common.Attributes.set(process_attrs, "request.id", @azimuth.telemetry.api.common.StringValue(request_id))
  @azimuth.telemetry.api.common.Attributes.set(process_attrs, "event.phase", @azimuth.telemetry.api.common.StringValue("processing"))
  @azimuth.telemetry.api.common.Attributes.set(process_attrs, "records.processed", @azimuth.telemetry.api.common.IntValue(150))
  
  @azimuth.telemetry.api.logs.Logger.info_with_attributes(logger, "Processing in progress", process_attrs)
  
  let end_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(end_attrs, "correlation.id", @azimuth.telemetry.api.common.StringValue(correlation_id))
  @azimuth.telemetry.api.common.Attributes.set(end_attrs, "request.id", @azimuth.telemetry.api.common.StringValue(request_id))
  @azimuth.telemetry.api.common.Attributes.set(end_attrs, "event.phase", @azimuth.telemetry.api.common.StringValue("end"))
  @azimuth.telemetry.api.common.Attributes.set(end_attrs, "total.duration.ms", @azimuth.telemetry.api.common.IntValue(250))
  
  @azimuth.telemetry.api.logs.Logger.info_with_attributes(logger, "Processing completed", end_attrs)
}

test "propagation with multiple headers and edge cases" {
  // Test propagation with multiple headers and edge cases
  let carrier = @azimuth.telemetry.api.propagation.HttpHeadersCarrier.new()
  
  // Set multiple headers including edge cases
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "tracestate", "congo=t61rcWkgMzE,rojo=00f067aa0ba902b7")
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "baggage", "userId=12345;sessionId=abcdef;serverNode=alpha")
  
  // Test case sensitivity
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "Custom-Header", "custom-value")
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "another-header", "another-value")
  
  // Test extraction with composite propagator
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let trace_propagator = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator.new()
  let baggage_propagator = @azimuth.telemetry.api.propagation.W3CBaggagePropagator.new()
  let composite = @azimuth.telemetry.api.propagation.CompositePropagator.new([trace_propagator, baggage_propagator])
  
  let extracted_ctx = @azimuth.telemetry.api.propagation.TextMapPropagator.extract(composite, ctx, carrier)
  
  // Verify trace context extraction
  let span_ctx = @azimuth.telemetry.api.context.Context.get_span_context(extracted_ctx)
  assert_true(span_ctx != @azimuth.telemetry.api.trace.SpanContext.invalid())
  assert_eq(span_ctx.trace_id(), "0af7651916cd43dd8448eb211c80319c")
  
  // Verify baggage extraction
  let baggage = @azimuth.telemetry.api.context.Context.get_baggage(extracted_ctx)
  assert_eq(@azimuth.telemetry.api.context.Baggage.get_value(baggage, "userId"), Some("12345"))
  assert_eq(@azimuth.telemetry.api.context.Baggage.get_value(baggage, "sessionId"), Some("abcdef"))
  assert_eq(@azimuth.telemetry.api.context.Baggage.get_value(baggage, "serverNode"), Some("alpha"))
}

test "span lifecycle with nested operations and exception handling" {
  // Test complex span lifecycle with nested operations and exception handling
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "lifecycle-test")
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Create parent span
  let (parent_ctx, parent_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "parent.operation")
  
  // Add initial event
  @azimuth.telemetry.api.trace.Span.add_event(parent_span, "operation.started", None, None)
  
  // Create nested child spans
  let (child1_ctx, child1_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, parent_ctx, "child.operation.1")
  @azimuth.telemetry.api.trace.Span.set_attribute(child1_span, "child.id", @azimuth.telemetry.api.common.IntValue(1))
  
  // Simulate exception in first child
  @azimuth.telemetry.api.trace.Span.record_exception(
    child1_span,
    "Simulated exception in child operation",
    Some("ChildOperationException"),
    Some("at child.operation.1\n  at parent.operation"),
    None
  )
  @azimuth.telemetry.api.trace.Span.set_status(child1_span, @azimuth.telemetry.api.trace.StatusCode.Error, Some("Child operation failed"))
  @azimuth.telemetry.api.trace.Span.end(child1_span)
  
  // Create second child that succeeds
  let (child2_ctx, child2_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, parent_ctx, "child.operation.2")
  @azimuth.telemetry.api.trace.Span.set_attribute(child2_span, "child.id", @azimuth.telemetry.api.common.IntValue(2))
  @azimuth.telemetry.api.trace.Span.add_event(child2_span, "child.completed", None, None)
  @azimuth.telemetry.api.trace.Span.set_status(child2_span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Child operation succeeded"))
  @azimuth.telemetry.api.trace.Span.end(child2_span)
  
  // Complete parent with warning status due to child failure
  @azimuth.telemetry.api.trace.Span.set_attribute(parent_span, "children.completed", @azimuth.telemetry.api.common.IntValue(2))
  @azimuth.telemetry.api.trace.Span.set_attribute(parent_span, "children.failed", @azimuth.telemetry.api.common.IntValue(1))
  @azimuth.telemetry.api.trace.Span.set_status(parent_span, @azimuth.telemetry.api.trace.StatusCode.Error, Some("Some child operations failed"))
  @azimuth.telemetry.api.trace.Span.add_event(parent_span, "operation.completed", None, None)
  @azimuth.telemetry.api.trace.Span.end(parent_span)
}

test "comprehensive telemetry integration scenario" {
  // Test a comprehensive scenario integrating all telemetry components
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "integration-test")
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "integration-test")
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "integration-test")
  
  // Create instruments
  let request_counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "http.requests.total")
  let response_histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(meter, "http.response.duration.ms")
  let active_gauge = @azimuth.telemetry.api.metrics.Meter.create_gauge(meter, "http.active_requests")
  
  // Start main operation span
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (op_ctx, main_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "http.request.processing")
  
  // Set span attributes
  @azimuth.telemetry.api.trace.Span.set_attribute(main_span, "http.method", @azimuth.telemetry.api.common.StringValue("POST"))
  @azimuth.telemetry.api.trace.Span.set_attribute(main_span, "http.url", @azimuth.telemetry.api.common.StringValue("/api/process"))
  @azimuth.telemetry.api.trace.Span.set_attribute(main_span, "user.id", @azimuth.telemetry.api.common.StringValue("user-123"))
  
  // Log start of operation
  let start_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(start_attrs, "trace.id", @azimuth.telemetry.api.common.StringValue(main_span.span_context().trace_id()))
  @azimuth.telemetry.api.common.Attributes.set(start_attrs, "span.id", @azimuth.telemetry.api.common.StringValue(main_span.span_context().span_id()))
  
  @azimuth.telemetry.api.logs.Logger.info_with_attributes(logger, "HTTP request processing started", start_attrs)
  
  // Record metrics
  @azimuth.telemetry.api.metrics.Gauge.record(active_gauge, 1.0)
  @azimuth.telemetry.api.metrics.Counter.add(request_counter, 1.0)
  
  // Simulate processing with nested operations
  let (db_ctx, db_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, op_ctx, "database.query")
  @azimuth.telemetry.api.trace.Span.set_attribute(db_span, "db.operation", @azimuth.telemetry.api.common.StringValue("SELECT"))
  @azimuth.telemetry.api.trace.Span.set_attribute(db_span, "db.collection", @azimuth.telemetry.api.common.StringValue("users"))
  
  // Log database operation
  let db_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(db_attrs, "operation", @azimuth.telemetry.api.common.StringValue("database_query"))
  @azimuth.telemetry.api.logs.Logger.debug_with_attributes(logger, "Executing database query", db_attrs)
  
  @azimuth.telemetry.api.trace.Span.end(db_span)
  
  // Simulate response processing
  let (resp_ctx, resp_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, op_ctx, "response.serialization")
  @azimuth.telemetry.api.trace.Span.set_attribute(resp_span, "response.format", @azimuth.telemetry.api.common.StringValue("json"))
  @azimuth.telemetry.api.trace.Span.set_attribute(resp_span, "response.size", @azimuth.telemetry.api.common.IntValue(1024))
  
  @azimuth.telemetry.api.trace.Span.end(resp_span)
  
  // Record final metrics
  @azimuth.telemetry.api.metrics.Histogram.record(response_histogram, 150.0)
  @azimuth.telemetry.api.metrics.Gauge.record(active_gauge, 0.0)
  
  // Set final span status and log completion
  @azimuth.telemetry.api.trace.Span.set_status(main_span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Request processed successfully"))
  
  let end_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(end_attrs, "duration.ms", @azimuth.telemetry.api.common.IntValue(150))
  @azimuth.telemetry.api.common.Attributes.set(end_attrs, "status.code", @azimuth.telemetry.api.common.IntValue(200))
  
  @azimuth.telemetry.api.logs.Logger.info_with_attributes(logger, "HTTP request processing completed", end_attrs)
  
  @azimuth.telemetry.api.trace.Span.end(main_span)
}