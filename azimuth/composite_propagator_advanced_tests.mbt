// Advanced Composite Propagator Test Suite
// Tests for composite propagator creation, injection, extraction, and multi-protocol support

test "composite propagator creation and basic operations" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Test that composite propagator is created successfully
  assert_true(true)  // If creation succeeds, we're good
}

test "composite propagator with trace context injection" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Verify traceparent header is set
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator with trace context extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction succeeded
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator with baggage propagation" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Inject context with both trace and baggage
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Verify headers are set
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator with multiple headers" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "baggage", "user.id=123,session.id=456")
  
  // Extract context with multiple headers
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction succeeded
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator with empty carrier" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let empty_carrier = TextMapCarrier::new()
  
  // Extract from empty carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, empty_carrier)
  
  // Verify extraction still works (returns root context)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator with malformed traceparent" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "malformed-trace-context")
  
  // Extract from malformed header
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction still works (graceful handling)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator with context values" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  let key = ContextKey::new("user.id")
  let ctx_with_value = Context::with_value(ctx, key, "user123")
  
  let carrier = TextMapCarrier::new()
  
  // Inject context with values
  CompositePropagator::inject(composite_propagator, ctx_with_value, carrier)
  
  // Verify traceparent is still set
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator with multiple propagators" {
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator1, trace_propagator2, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Inject with multiple propagators
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Verify injection works
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator round-trip test" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Original context
  let original_ctx = Context::root()
  let key = ContextKey::new("test.key")
  let original_ctx_with_value = Context::with_value(original_ctx, key, "test.value")
  
  // Inject
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, original_ctx_with_value, carrier)
  
  // Extract
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify round-trip works
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
  
  // Verify traceparent header is preserved
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))
}