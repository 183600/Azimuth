test "text map carrier operations" {
  // Test basic text map carrier implementation
  let carrier = @azimuth.telemetry.api.propagation.HttpHeadersCarrier.new()
  
  // Test setting and getting values
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "tracestate", "congo=t61rcWkgMzE")
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "baggage", "userId=12345,sessionId=abcdef")
  
  // Test retrieving values
  let traceparent = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "traceparent")
  let tracestate = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "tracestate")
  let baggage = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "baggage")
  
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(tracestate, Some("congo=t61rcWkgMzE"))
  assert_eq(baggage, Some("userId=12345,sessionId=abcdef"))
  
  // Test getting all keys
  let keys = @azimuth.telemetry.api.propagation.TextMapCarrier.keys(carrier)
  assert_true(keys.length() >= 3)
  assert_true(keys.contains("traceparent"))
  assert_true(keys.contains("tracestate"))
  assert_true(keys.contains("baggage"))
}

test "w3c trace context propagator injection" {
  // Test W3C trace context header injection
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,  // sampled
    "congo=t61rcWkgMzE"
  )
  let ctx_with_span = @azimuth.telemetry.api.context.Context.with_span_context(ctx, span_ctx)
  
  let carrier = @azimuth.telemetry.api.propagation.HttpHeadersCarrier.new()
  let propagator = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator.new()
  
  @azimuth.telemetry.api.propagation.TextMapPropagator.inject(propagator, ctx_with_span, carrier)
  
  let traceparent = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "traceparent")
  let tracestate = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "tracestate")
  
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(tracestate, Some("congo=t61rcWkgMzE"))
}

test "w3c trace context propagator extraction" {
  // Test W3C trace context header extraction
  let carrier = @azimuth.telemetry.api.propagation.HttpHeadersCarrier.new()
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "tracestate", "congo=t61rcWkgMzE")
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let propagator = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator.new()
  
  let extracted_ctx = @azimuth.telemetry.api.propagation.TextMapPropagator.extract(propagator, ctx, carrier)
  let span_ctx = @azimuth.telemetry.api.context.Context.get_span_context(extracted_ctx)
  
  assert_true(span_ctx != @azimuth.telemetry.api.trace.SpanContext.invalid())
  assert_eq(span_ctx.trace_id(), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(span_ctx.span_id(), "b7ad6b7169203331")
  assert_true(span_ctx.is_sampled())
  assert_eq(span_ctx.trace_state(), "congo=t61rcWkgMzE")
}

test "w3c baggage propagator injection" {
  // Test W3C baggage header injection
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let baggage = @azimuth.telemetry.api.context.Baggage.new()
  let baggage_with_values = @azimuth.telemetry.api.context.Baggage.set_value(baggage, "userId", "12345")
  let baggage_final = @azimuth.telemetry.api.context.Baggage.set_value(baggage_with_values, "sessionId", "abcdef")
  
  let ctx_with_baggage = @azimuth.telemetry.api.context.Context.with_baggage(ctx, baggage_final)
  
  let carrier = @azimuth.telemetry.api.propagation.HttpHeadersCarrier.new()
  let propagator = @azimuth.telemetry.api.propagation.W3CBaggagePropagator.new()
  
  @azimuth.telemetry.api.propagation.TextMapPropagator.inject(propagator, ctx_with_baggage, carrier)
  
  let baggage_header = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "baggage")
  assert_true(baggage_header != None)
  let baggage_value = baggage_header.unwrap()
  
  // Check that both values are present (order may vary)
  assert_true(baggage_value.contains("userId=12345"))
  assert_true(baggage_value.contains("sessionId=abcdef"))
}

test "w3c baggage propagator extraction" {
  // Test W3C baggage header extraction
  let carrier = @azimuth.telemetry.api.propagation.HttpHeadersCarrier.new()
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "baggage", "userId=12345,sessionId=abcdef")
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let propagator = @azimuth.telemetry.api.propagation.W3CBaggagePropagator.new()
  
  let extracted_ctx = @azimuth.telemetry.api.propagation.TextMapPropagator.extract(propagator, ctx, carrier)
  let extracted_baggage = @azimuth.telemetry.api.context.Context.get_baggage(extracted_ctx)
  
  let user_id = @azimuth.telemetry.api.context.Baggage.get_value(extracted_baggage, "userId")
  let session_id = @azimuth.telemetry.api.context.Baggage.get_value(extracted_baggage, "sessionId")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(session_id, Some("abcdef"))
}

test "composite propagator" {
  // Test composite propagator with both trace context and baggage
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Add span context
  let span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE"
  )
  let ctx_with_span = @azimuth.telemetry.api.context.Context.with_span_context(ctx, span_ctx)
  
  // Add baggage
  let baggage = @azimuth.telemetry.api.context.Baggage.new()
  let baggage_with_values = @azimuth.telemetry.api.context.Baggage.set_value(baggage, "userId", "12345")
  let ctx_final = @azimuth.telemetry.api.context.Context.with_baggage(ctx_with_span, baggage_with_values)
  
  // Create composite propagator
  let trace_propagator = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator.new()
  let baggage_propagator = @azimuth.telemetry.api.propagation.W3CBaggagePropagator.new()
  let composite = @azimuth.telemetry.api.propagation.CompositePropagator.new([trace_propagator, baggage_propagator])
  
  // Test injection
  let carrier = @azimuth.telemetry.api.propagation.HttpHeadersCarrier.new()
  @azimuth.telemetry.api.propagation.TextMapPropagator.inject(composite, ctx_final, carrier)
  
  let traceparent = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "traceparent")
  let baggage_header = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "baggage")
  
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_true(baggage_header != None)
  assert_true(baggage_header.unwrap().contains("userId=12345"))
  
  // Test extraction
  let empty_ctx = @azimuth.telemetry.api.context.Context.root()
  let extracted_ctx = @azimuth.telemetry.api.propagation.TextMapPropagator.extract(composite, empty_ctx, carrier)
  
  let extracted_span_ctx = @azimuth.telemetry.api.context.Context.get_span_context(extracted_ctx)
  let extracted_baggage = @azimuth.telemetry.api.context.Context.get_baggage(extracted_ctx)
  
  assert_eq(extracted_span_ctx.trace_id(), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(@azimuth.telemetry.api.context.Baggage.get_value(extracted_baggage, "userId"), Some("12345"))
}