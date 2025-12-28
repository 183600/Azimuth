test "span context creation and validation" {
  // Test span context creation and basic properties
  let span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,  // sampled
    "congo=t61rcWkgMzE"
  )
  
  assert_eq(span_ctx.trace_id(), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(span_ctx.span_id(), "b7ad6b7169203331")
  assert_true(span_ctx.is_valid())
  assert_true(span_ctx.is_sampled())
  assert_eq(span_ctx.trace_state(), "congo=t61rcWkgMzE")
  
  // Test invalid span context
  let invalid_ctx = @azimuth.telemetry.api.trace.SpanContext.invalid()
  assert_false(invalid_ctx.is_valid())
  assert_false(invalid_ctx.is_sampled())
}

test "span creation and basic operations" {
  // Test span creation and basic operations
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "test.tracer", Some("1.0.0"))
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (ctx_with_span, span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    tracer,
    ctx,
    "test.span",
    Some(@azimuth.telemetry.api.trace.SpanKind.Internal),
    None,  // no attributes
    None   // no start time
  )
  
  assert_eq(span.name(), "test.span")
  assert_eq(span.kind(), @azimuth.telemetry.api.trace.SpanKind.Internal)
  assert_true(span.is_recording())
  
  let span_ctx = span.span_context()
  assert_true(span_ctx.is_valid())
  
  // Test setting attributes
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "string.attr", @azimuth.telemetry.api.common.StringValue("test"))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "int.attr", @azimuth.telemetry.api.common.IntValue(42))
  
  // Test adding events
  @azimuth.telemetry.api.trace.Span.add_event(
    span,
    "test.event",
    Some(@azimuth.telemetry.api.common.Attributes.new()),
    None
  )
  
  // Test setting status
  @azimuth.telemetry.api.trace.Span.set_status(
    span,
    @azimuth.telemetry.api.trace.StatusCode.Ok,
    Some("Operation completed successfully")
  )
  
  // End the span
  @azimuth.telemetry.api.trace.Span.end(span, None)
  assert_false(span.is_recording())
}

test "span with parent context" {
  // Test creating a span with a parent context
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "test.tracer")
  
  // Create parent span
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (parent_ctx, parent_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "parent.span")
  
  // Create child span
  let (child_ctx, child_span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    tracer,
    parent_ctx,
    "child.span"
  )
  
  let parent_span_ctx = parent_span.span_context()
  let child_span_ctx = child_span.span_context()
  
  // Child should have same trace ID as parent
  assert_eq(child_span_ctx.trace_id(), parent_span_ctx.trace_id())
  
  // Child should have different span ID than parent
  assert_true(child_span_ctx.span_id() != parent_span_ctx.span_id())
  
  // Child should reference parent
  let parent_id = child_span.parent_span_id()
  assert_eq(parent_id, Some(parent_span_ctx.span_id()))
  
  @azimuth.telemetry.api.trace.Span.end(child_span)
  @azimuth.telemetry.api.trace.Span.end(parent_span)
}

test "span kinds" {
  // Test different span kinds
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "test.tracer")
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Test Internal span
  let (_, internal_span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    tracer, ctx, "internal", Some(@azimuth.telemetry.api.trace.SpanKind.Internal)
  )
  assert_eq(internal_span.kind(), @azimuth.telemetry.api.trace.SpanKind.Internal)
  @azimuth.telemetry.api.trace.Span.end(internal_span)
  
  // Test Server span
  let (_, server_span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    tracer, ctx, "server", Some(@azimuth.telemetry.api.trace.SpanKind.Server)
  )
  assert_eq(server_span.kind(), @azimuth.telemetry.api.trace.SpanKind.Server)
  @azimuth.telemetry.api.trace.Span.end(server_span)
  
  // Test Client span
  let (_, client_span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    tracer, ctx, "client", Some(@azimuth.telemetry.api.trace.SpanKind.Client)
  )
  assert_eq(client_span.kind(), @azimuth.telemetry.api.trace.SpanKind.Client)
  @azimuth.telemetry.api.trace.Span.end(client_span)
  
  // Test Producer span
  let (_, producer_span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    tracer, ctx, "producer", Some(@azimuth.telemetry.api.trace.SpanKind.Producer)
  )
  assert_eq(producer_span.kind(), @azimuth.telemetry.api.trace.SpanKind.Producer)
  @azimuth.telemetry.api.trace.Span.end(producer_span)
  
  // Test Consumer span
  let (_, consumer_span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    tracer, ctx, "consumer", Some(@azimuth.telemetry.api.trace.SpanKind.Consumer)
  )
  assert_eq(consumer_span.kind(), @azimuth.telemetry.api.trace.SpanKind.Consumer)
  @azimuth.telemetry.api.trace.Span.end(consumer_span)
}

test "span exception recording" {
  // Test recording exceptions on spans
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "test.tracer")
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  let (_, span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "exception.span")
  
  // Record an exception
  @azimuth.telemetry.api.trace.Span.record_exception(
    span,
    "Test exception message",
    Some("TestException"),
    Some("stack trace line 1\nstack trace line 2"),
    Some(@azimuth.telemetry.api.common.Attributes.new())
  )
  
  // Set error status
  @azimuth.telemetry.api.trace.Span.set_status(
    span,
    @azimuth.telemetry.api.trace.StatusCode.Error,
    Some("Exception occurred")
  )
  
  @azimuth.telemetry.api.trace.Span.end(span)
}

test "tracer provider and tracer properties" {
  // Test tracer provider and tracer properties
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let tracer1 = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "test.tracer")
  let tracer2 = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(
    tracer_provider, 
    "test.tracer", 
    Some("1.0.0"),
    Some("https://example.com/schema")
  )
  
  // Same name should return same tracer instance (for noop provider)
  assert_true(tracer1 == tracer2)
  
  // Different names should return different tracers
  let tracer3 = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "other.tracer")
  assert_true(tracer1 != tracer3)
  
  // Test tracer instrumentation scope
  let scope = @azimuth.telemetry.api.trace.Tracer.instrumentation_scope(tracer1)
  assert_eq(scope.name(), "test.tracer")
}

test "span status codes" {
  // Test different status codes
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "test.tracer")
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Test Ok status
  let (_, ok_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "ok.span")
  @azimuth.telemetry.api.trace.Span.set_status(ok_span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Success"))
  @azimuth.telemetry.api.trace.Span.end(ok_span)
  
  // Test Error status
  let (_, error_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "error.span")
  @azimuth.telemetry.api.trace.Span.set_status(error_span, @azimuth.telemetry.api.trace.StatusCode.Error, Some("Failed"))
  @azimuth.telemetry.api.trace.Span.end(error_span)
  
  // Test Unset status
  let (_, unset_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "unset.span")
  @azimuth.telemetry.api.trace.Span.set_status(unset_span, @azimuth.telemetry.api.trace.StatusCode.Unset, None)
  @azimuth.telemetry.api.trace.Span.end(unset_span)
}