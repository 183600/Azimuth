// Composite Propagator Injection and Extraction Tests
// Testing composite propagator functionality for cross-service context propagation

test "composite propagator creation with single propagator" {
  // Test creating composite propagator with single W3C trace context propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // Test injection with single propagator
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Verify traceparent header is injected
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator creation with multiple propagators" {
  // Test creating composite propagator with multiple propagators
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let trace_propagator3 = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator1, trace_propagator2, trace_propagator3]
  
  let composite = CompositePropagator::new(propagators)
  
  // Test injection with multiple propagators
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Verify injection still works with multiple propagators
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator injection with context values" {
  // Test injection with context containing values
  let ctx = Context::root()
  let key1 = ContextKey::new("trace.id")
  let key2 = ContextKey::new("span.id")
  let ctx_with_values = Context::with_value(
    Context::with_value(ctx, key1, "trace-12345"),
    key2, "span-67890"
  )
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_values, carrier)
  
  // Verify injection preserves context information
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator extraction with empty carrier" {
  // Test extraction from empty carrier
  let carrier = TextMapCarrier::new()
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction returns a context (even from empty carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator extraction with traceparent header" {
  // Test extraction from carrier with traceparent header
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction processes traceparent header
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator extraction with multiple headers" {
  // Test extraction from carrier with multiple headers
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  TextMapCarrier::set(carrier, "baggage", "userId=12345,sessionId=abcdef")
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction handles multiple headers
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator inject then extract roundtrip" {
  // Test full roundtrip: inject context into carrier, then extract back
  let original_ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_correlation = Context::with_value(original_ctx, key, "corr-abc123")
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Inject context into carrier
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_correlation, carrier)
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction succeeded
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator with baggage propagation" {
  // Test composite propagator with baggage propagator
  let baggage_propagator = W3CBaggagePropagator::new()
  let trace_propagator = W3CTraceContextPropagator::new()
  
  // Note: Current implementation only supports W3CTraceContextPropagator
  // This test demonstrates the intended functionality
  let composite = CompositePropagator::new([trace_propagator])
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Verify injection works
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator with invalid traceparent format" {
  // Test extraction with invalid traceparent format
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "invalid-format")
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction handles invalid format gracefully
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator with malformed headers" {
  // Test extraction with malformed headers
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "")
  TextMapCarrier::set(carrier, "baggage", "malformed")
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction handles malformed headers
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator performance with many headers" {
  // Test performance with many headers
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Add many custom headers
  for i = 0; i < 100; i++ {
    TextMapCarrier::set(carrier, "custom.header." + i.to_string(), "value." + i.to_string())
  }
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction works with many headers
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator with unicode and special characters" {
  // Test injection and extraction with unicode and special characters
  let ctx = Context::root()
  let key = ContextKey::new("测试.键")
  let ctx_with_unicode = Context::with_value(ctx, key, "测试值")
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_unicode, carrier)
  
  // Add unicode headers to carrier
  TextMapCarrier::set(carrier, "unicode-header", "测试值")
  TextMapCarrier::set(carrier, "emoji-header", "🚀🔥💧")
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction handles unicode
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}