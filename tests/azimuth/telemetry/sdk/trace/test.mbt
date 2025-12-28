test "sdk trace provider functionality" {
  // Test SDK trace provider functionality and integration
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "sdk.test.tracer", Some("1.0.0"))
  
  // Test tracer creation and properties
  assert_true(tracer != @azimuth.telemetry.sdk.trace.Tracer::{})
  
  let scope = @azimuth.telemetry.sdk.trace.Tracer.instrumentation_scope(tracer)
  assert_eq(scope.name(), "sdk.test.tracer")
  assert_eq(scope.version(), Some("1.0.0"))
  
  // Test span creation with SDK tracer
  let span = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "sdk.test.span", None)
  assert_true(span != @azimuth.telemetry.api.trace.Span::{})
  
  let span_ctx = @azimuth.telemetry.api.trace.Span.span_context(span)
  assert_true(@azimuth.telemetry.api.trace.SpanContext.is_valid(span_ctx))
  
  // Test span with SDK-specific features
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "sdk.attribute", @azimuth.telemetry.api.common.StringValue("sdk_value"))
  @azimuth.telemetry.api.trace.Span.add_event(span, "sdk.event", None, None)
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("SDK operation completed"))
  
  // End the span
  @azimuth.telemetry.api.trace.Span.end(span)
  assert_false(@azimuth.telemetry.api.trace.Span.is_recording(span))
}

test "sdk trace provider with different configurations" {
  // Test SDK trace provider with different configurations
  
  // Test default provider
  let default_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let default_tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(default_provider, "default.tracer")
  
  // Test provider with custom configuration
  let custom_provider = @azimuth.telemetry.sdk.trace.TracerProvider.new()
  let custom_tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(custom_provider, "custom.tracer", Some("2.0.0"))
  
  // Both should create valid tracers
  assert_true(default_tracer != @azimuth.telemetry.sdk.trace.Tracer::{})
  assert_true(custom_tracer != @azimuth.telemetry.sdk.trace.Tracer::{})
  
  // Test span creation with different providers
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (_, default_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(default_tracer, "default.span")
  let (_, custom_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(custom_tracer, "custom.span")
  
  // Both spans should be valid
  assert_true(@azimuth.telemetry.api.trace.Span.is_recording(default_span))
  assert_true(@azimuth.telemetry.api.trace.Span.is_recording(custom_span))
  
  @azimuth.telemetry.api.trace.Span.end(default_span)
  @azimuth.telemetry.api.trace.Span.end(custom_span)
}

test "sdk trace context propagation" {
  // Test SDK trace context propagation functionality
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "propagation.test.tracer")
  
  // Create parent span
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (parent_ctx, parent_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "parent.span")
  
  // Create child span with propagated context
  let (child_ctx, child_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, parent_ctx, "child.span")
  
  // Verify trace context propagation
  let parent_span_ctx = @azimuth.telemetry.api.trace.Span.span_context(parent_span)
  let child_span_ctx = @azimuth.telemetry.api.trace.Span.span_context(child_span)
  
  assert_eq(@azimuth.telemetry.api.trace.SpanContext.trace_id(parent_span_ctx), 
           @azimuth.telemetry.api.trace.SpanContext.trace_id(child_span_ctx))
  assert_true(@azimuth.telemetry.api.trace.SpanContext.span_id(parent_span_ctx) != 
              @azimuth.telemetry.api.trace.SpanContext.span_id(child_span_ctx))
  
  // Test grandchild span
  let (_, grandchild_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, child_ctx, "grandchild.span")
  let grandchild_span_ctx = @azimuth.telemetry.api.trace.Span.span_context(grandchild_span)
  
  assert_eq(@azimuth.telemetry.api.trace.SpanContext.trace_id(grandchild_span_ctx), 
           @azimuth.telemetry.api.trace.SpanContext.trace_id(parent_span_ctx))
  
  @azimuth.telemetry.api.trace.Span.end(grandchild_span)
  @azimuth.telemetry.api.trace.Span.end(child_span)
  @azimuth.telemetry.api.trace.Span.end(parent_span)
}

test "sdk trace with custom attributes and events" {
  // Test SDK trace with comprehensive attributes and events
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "attributes.test.tracer")
  let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "attributes.test.span")
  
  // Test setting various attribute types
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "string.attr", @azimuth.telemetry.api.common.StringValue("test_string"))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "int.attr", @azimuth.telemetry.api.common.IntValue(42))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "float.attr", @azimuth.telemetry.api.common.FloatValue(3.14159))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "bool.attr", @azimuth.telemetry.api.common.BoolValue(true))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "array.string", @azimuth.telemetry.api.common.ArrayStringValue(["a", "b", "c"]))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "array.int", @azimuth.telemetry.api.common.ArrayIntValue([1, 2, 3]))
  
  // Test adding events with attributes
  let event_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(event_attrs, "event.type", @azimuth.telemetry.api.common.StringValue("user_action"))
  @azimuth.telemetry.api.common.Attributes.set(event_attrs, "event.id", @azimuth.telemetry.api.common.IntValue(12345))
  
  @azimuth.telemetry.api.trace.Span.add_event(span, "user.login", Some([("event.type", @azimuth.telemetry.api.common.StringValue("user_action"))]), None)
  @azimuth.telemetry.api.trace.Span.add_event(span, "database.query", Some([("query.time", @azimuth.telemetry.api.common.FloatValue(25.5))]), Some(1735689600000000000L))
  
  // Test recording exceptions
  @azimuth.telemetry.api.trace.Span.record_exception(
    span,
    "Test exception for SDK trace",
    Some("SDKTestException"),
    Some("at sdk.test.trace\n  at test_function()"),
    Some(event_attrs)
  )
  
  // Set final status
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Error, Some("Test completed with exception"))
  
  @azimuth.telemetry.api.trace.Span.end(span)
}