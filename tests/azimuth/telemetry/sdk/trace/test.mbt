test "sdk trace provider functionality" {
  // Test SDK trace provider functionality and integration
  
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "sdk.test.tracer", Some("1.0.0"))
  
  // Test tracer creation and properties
  assert_true(tracer != telemetry.sdk.trace.Tracer::{})
  
  let scope = telemetry.sdk.trace.Tracer.instrumentation_scope(tracer)
  assert_eq(scope.name(), "sdk.test.tracer")
  assert_eq(scope.version(), Some("1.0.0"))
  
  // Test span creation with SDK tracer
  let span = telemetry.sdk.trace.Tracer.start_span(tracer, "sdk.test.span", None)
  assert_true(span != telemetry.api.trace.Span::{})
  
  let span_ctx = telemetry.api.trace.Span.span_context(span)
  assert_true(telemetry.api.trace.SpanContext.is_valid(span_ctx))
  
  // Test span with SDK-specific features
  telemetry.api.trace.Span.set_attribute(span, "sdk.attribute", telemetry.api.common.StringValue("sdk_value"))
  telemetry.api.trace.Span.add_event(span, "sdk.event", None, None)
  telemetry.api.trace.Span.set_status(span, telemetry.api.trace.StatusCode.Ok, Some("SDK operation completed"))
  
  // End the span
  telemetry.api.trace.Span.end(span)
  assert_false(telemetry.api.trace.Span.is_recording(span))
}

test "sdk trace provider with different configurations" {
  // Test SDK trace provider with different configurations
  
  // Test default provider
  let default_provider = telemetry.sdk.trace.TracerProvider.default()
  let default_tracer = telemetry.sdk.trace.TracerProvider.get_tracer(default_provider, "default.tracer")
  
  // Test provider with custom configuration
  let custom_provider = telemetry.sdk.trace.TracerProvider.new()
  let custom_tracer = telemetry.sdk.trace.TracerProvider.get_tracer(custom_provider, "custom.tracer", Some("2.0.0"))
  
  // Both should create valid tracers
  assert_true(default_tracer != telemetry.sdk.trace.Tracer::{})
  assert_true(custom_tracer != telemetry.sdk.trace.Tracer::{})
  
  // Test span creation with different providers
  let ctx = telemetry.api.context.Context.root()
  let (_, default_span) = telemetry.sdk.trace.Tracer.start_span(default_tracer, "default.span")
  let (_, custom_span) = telemetry.sdk.trace.Tracer.start_span(custom_tracer, "custom.span")
  
  // Both spans should be valid
  assert_true(telemetry.api.trace.Span.is_recording(default_span))
  assert_true(telemetry.api.trace.Span.is_recording(custom_span))
  
  telemetry.api.trace.Span.end(default_span)
  telemetry.api.trace.Span.end(custom_span)
}

test "sdk trace context propagation" {
  // Test SDK trace context propagation functionality
  
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "propagation.test.tracer")
  
  // Create parent span
  let ctx = telemetry.api.context.Context.root()
  let (parent_ctx, parent_span) = telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "parent.span")
  
  // Create child span with propagated context
  let (child_ctx, child_span) = telemetry.sdk.trace.Tracer.start_span(tracer, parent_ctx, "child.span")
  
  // Verify trace context propagation
  let parent_span_ctx = telemetry.api.trace.Span.span_context(parent_span)
  let child_span_ctx = telemetry.api.trace.Span.span_context(child_span)
  
  assert_eq(telemetry.api.trace.SpanContext.trace_id(parent_span_ctx), 
           telemetry.api.trace.SpanContext.trace_id(child_span_ctx))
  assert_true(telemetry.api.trace.SpanContext.span_id(parent_span_ctx) != 
              telemetry.api.trace.SpanContext.span_id(child_span_ctx))
  
  // Test grandchild span
  let (_, grandchild_span) = telemetry.sdk.trace.Tracer.start_span(tracer, child_ctx, "grandchild.span")
  let grandchild_span_ctx = telemetry.api.trace.Span.span_context(grandchild_span)
  
  assert_eq(telemetry.api.trace.SpanContext.trace_id(grandchild_span_ctx), 
           telemetry.api.trace.SpanContext.trace_id(parent_span_ctx))
  
  telemetry.api.trace.Span.end(grandchild_span)
  telemetry.api.trace.Span.end(child_span)
  telemetry.api.trace.Span.end(parent_span)
}

test "sdk trace with custom attributes and events" {
  // Test SDK trace with comprehensive attributes and events
  
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "attributes.test.tracer")
  let (_, span) = telemetry.sdk.trace.Tracer.start_span(tracer, "attributes.test.span")
  
  // Test setting various attribute types
  telemetry.api.trace.Span.set_attribute(span, "string.attr", telemetry.api.common.StringValue("test_string"))
  telemetry.api.trace.Span.set_attribute(span, "int.attr", telemetry.api.common.IntValue(42))
  telemetry.api.trace.Span.set_attribute(span, "float.attr", telemetry.api.common.FloatValue(3.14159))
  telemetry.api.trace.Span.set_attribute(span, "bool.attr", telemetry.api.common.BoolValue(true))
  telemetry.api.trace.Span.set_attribute(span, "array.string", telemetry.api.common.ArrayStringValue(["a", "b", "c"]))
  telemetry.api.trace.Span.set_attribute(span, "array.int", telemetry.api.common.ArrayIntValue([1, 2, 3]))
  
  // Test adding events with attributes
  let event_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(event_attrs, "event.type", telemetry.api.common.StringValue("user_action"))
  telemetry.api.common.Attributes.set(event_attrs, "event.id", telemetry.api.common.IntValue(12345))
  
  telemetry.api.trace.Span.add_event(span, "user.login", Some([("event.type", telemetry.api.common.StringValue("user_action"))]), None)
  telemetry.api.trace.Span.add_event(span, "database.query", Some([("query.time", telemetry.api.common.FloatValue(25.5))]), Some(1735689600000000000L))
  
  // Test recording exceptions
  telemetry.api.trace.Span.record_exception(
    span,
    "Test exception for SDK trace",
    Some("SDKTestException"),
    Some("at sdk.test.trace\n  at test_function()"),
    Some(event_attrs)
  )
  
  // Set final status
  telemetry.api.trace.Span.set_status(span, telemetry.api.trace.StatusCode.Error, Some("Test completed with exception"))
  
  telemetry.api.trace.Span.end(span)
}