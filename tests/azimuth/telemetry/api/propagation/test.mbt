// Additional propagation test cases for Azimuth telemetry system
// This file contains test cases for context propagation functionality

test "text map carrier basic operations" {
  // Test basic text map carrier operations
  
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  
  // Test setting and getting headers
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "baggage", "key1=value1,key2=value2")
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "custom-header", "custom-value")
  
  // Test header retrieval
  let traceparent = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "traceparent")
  let baggage = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "baggage")
  let custom = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "custom-header")
  
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(baggage, Some("key1=value1,key2=value2"))
  assert_eq(custom, Some("custom-value"))
  
  // Test non-existent header
  let non_existent = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "non-existent")
  assert_eq(non_existent, None)
}

test "w3c trace context propagator" {
  // Test W3C Trace Context propagator functionality
  
  let propagator = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator.new()
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  
  // Create a span context
  let span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE,rojo=00f067aa0ba902b7"
  )
  
  // Create context with span
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let ctx_with_span = @azimuth.telemetry.api.context.Context.with_span_context(ctx, span_ctx)
  
  // Inject context
  @azimuth.telemetry.api.propagation.TextMapPropagator.inject(propagator, ctx_with_span, carrier)
  
  // Verify injected traceparent
  let injected_traceparent = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "traceparent")
  assert_true(injected_traceparent != None)
  
  // Extract context
  let empty_ctx = @azimuth.telemetry.api.context.Context.root()
  let extracted_ctx = @azimuth.telemetry.api.propagation.TextMapPropagator.extract(propagator, empty_ctx, carrier)
  
  // Verify extracted context
  let extracted_span_ctx = @azimuth.telemetry.api.context.Context.get_span_context(extracted_ctx)
  assert_true(extracted_span_ctx.is_valid())
}
