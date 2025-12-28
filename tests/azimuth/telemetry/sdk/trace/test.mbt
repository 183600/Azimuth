test "span attribute management and validation" {
  // Test comprehensive span attribute management
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "attribute.test")
  
  // Create span with different kinds
  let server_span = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, "server.operation")
  let client_span = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, "client.operation")
  let producer_span = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, "producer.operation")
  let consumer_span = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, "consumer.operation")
  
  // Test span names
  assert_eq(server_span.name(), "server.operation")
  assert_eq(client_span.name(), "client.operation")
  assert_eq(producer_span.name(), "producer.operation")
  assert_eq(consumer_span.name(), "consumer.operation")
  
  // Test span kinds
  assert_eq(server_span.kind(), @azimuth.telemetry.api.trace.SpanKind.Internal)
  assert_eq(client_span.kind(), @azimuth.telemetry.api.trace.SpanKind.Internal)
  assert_eq(producer_span.kind(), @azimuth.telemetry.api.trace.SpanKind.Internal)
  assert_eq(consumer_span.kind(), @azimuth.telemetry.api.trace.SpanKind.Internal)
  
  // Test span recording state
  assert_true(server_span.is_recording())
  assert_true(client_span.is_recording())
  assert_true(producer_span.is_recording())
  assert_true(consumer_span.is_recording())
  
  // Test span context
  let server_ctx = server_span.span_context()
  let client_ctx = client_span.span_context()
  
  assert_true(@azimuth.telemetry.api.trace.SpanContext.is_valid(server_ctx))
  assert_true(@azimuth.telemetry.api.trace.SpanContext.is_valid(client_ctx))
  
  // End spans
  @azimuth.telemetry.api.trace.Span.end(server_span)
  @azimuth.telemetry.api.trace.Span.end(client_span)
  @azimuth.telemetry.api.trace.Span.end(producer_span)
  @azimuth.telemetry.api.trace.Span.end(consumer_span)
}

test "span event and status lifecycle" {
  // Test span event addition and status management throughout lifecycle
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "lifecycle.test")
  
  let span = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, "lifecycle.span")
  
  // Initial status should be Unset
  assert_eq(span.status(), @azimuth.telemetry.api.trace.StatusCode.Unset)
  
  // Add lifecycle events
  @azimuth.telemetry.api.trace.Span.add_event(span, "span.started")
  @azimuth.telemetry.api.trace.Span.add_event(span, "processing.began")
  
  // Set status to Ok during operation
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Operation in progress"))
  
  // Add more events
  @azimuth.telemetry.api.trace.Span.add_event(span, "processing.continued")
  @azimuth.telemetry.api.trace.Span.add_event(span, "nearing.completion")
  
  // Update status to indicate completion
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Operation completed successfully"))
  
  // Add final event
  @azimuth.telemetry.api.trace.Span.add_event(span, "span.completed")
  
  // End span
  @azimuth.telemetry.api.trace.Span.end(span)
  
  // Verify span is no longer recording
  assert_false(span.is_recording())
}

test "tracer instrumentation scope management" {
  // Test tracer instrumentation scope creation and management
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.default()
  
  // Test tracer with minimal scope
  let minimal_tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "minimal.tracer")
  let minimal_scope = @azimuth.telemetry.api.trace.Tracer.instrumentation_scope(minimal_tracer)
  
  assert_eq(minimal_scope.name(), "minimal.tracer")
  assert_eq(minimal_scope.version(), None)
  assert_eq(minimal_scope.schema_url(), None)
  
  // Test tracer with version
  let versioned_tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(
    tracer_provider, 
    "versioned.tracer", 
    Some("2.1.0")
  )
  let versioned_scope = @azimuth.telemetry.api.trace.Tracer.instrumentation_scope(versioned_tracer)
  
  assert_eq(versioned_scope.name(), "versioned.tracer")
  assert_eq(versioned_scope.version(), Some("2.1.0"))
  assert_eq(versioned_scope.schema_url(), None)
  
  // Test span creation from different tracers
  let minimal_span = @azimuth.telemetry.api.trace.Tracer.start_span(minimal_tracer, "minimal.span")
  let versioned_span = @azimuth.telemetry.api.trace.Tracer.start_span(versioned_tracer, "versioned.span")
  
  assert_eq(minimal_span.name(), "minimal.span")
  assert_eq(versioned_span.name(), "versioned.span")
  
  // End spans
  @azimuth.telemetry.api.trace.Span.end(minimal_span)
  @azimuth.telemetry.api.trace.Span.end(versioned_span)
}

test "span context validation and properties" {
  // Test span context validation and property access
  
  // Test valid span context
  let valid_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE,rojo=00f067aa0ba902b7"
  )
  
  assert_eq(valid_ctx.trace_id(), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(valid_ctx.span_id(), "b7ad6b7169203331")
  assert_true(valid_ctx.is_sampled())
  assert_true(valid_ctx.is_valid())
  
  // Test invalid span context (empty trace ID)
  let invalid_trace_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "",
    "b7ad6b7169203331",
    true,
    ""
  )
  
  assert_eq(invalid_trace_ctx.trace_id(), "")
  assert_eq(invalid_trace_ctx.span_id(), "b7ad6b7169203331")
  assert_true(invalid_trace_ctx.is_sampled())
  assert_false(invalid_trace_ctx.is_valid())
  
  // Test invalid span context (empty span ID)
  let invalid_span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "",
    false,
    ""
  )
  
  assert_eq(invalid_span_ctx.trace_id(), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(invalid_span_ctx.span_id(), "")
  assert_false(invalid_span_ctx.is_sampled())
  assert_false(invalid_span_ctx.is_valid())
  
  // Test not sampled span context
  let not_sampled_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    false,
    ""
  )
  
  assert_false(not_sampled_ctx.is_sampled())
  assert_true(not_sampled_ctx.is_valid())
}