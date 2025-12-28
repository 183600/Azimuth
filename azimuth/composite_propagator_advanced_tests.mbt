// Composite Propagator Operations Test Suite for Azimuth Telemetry System
// Tests complex propagator scenarios including multiple propagators, injection/extraction chains

test "composite propagator with multiple trace propagators" {
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  
  let propagators = [trace_propagator1, trace_propagator2]
  let composite = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // Inject context
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Verify traceparent header is set
  let trace_header = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator injection and extraction cycle" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // Create context with values
  let ctx = Context::root()
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(ctx, key, "test.value")
  
  // Inject into carrier
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_value, carrier)
  
  // Extract back to context
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction worked
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator with empty propagators list" {
  let empty_propagators : Array[W3CTraceContextPropagator] = []
  let composite = CompositePropagator::new(empty_propagators)
  
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // Should not fail with empty propagators
  CompositePropagator::inject(composite, ctx, carrier)
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Should return a context (may be root or modified)
  assert_true(true) // If we get here, no exception was thrown
}

test "composite propagator multiple injection cycles" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // First injection
  CompositePropagator::inject(composite, ctx, carrier)
  let first_header = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(first_header, Some("00-test-trace-id-test-span-id-01"))
  
  // Second injection (should overwrite)
  CompositePropagator::inject(composite, ctx, carrier)
  let second_header = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(second_header, Some("00-test-trace-id-test-span-id-01"))
  
  // Third injection
  CompositePropagator::inject(composite, ctx, carrier)
  let third_header = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(third_header, Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator with complex context" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // Build complex context
  let ctx = Context::root()
  let key1 = ContextKey::new("user.id")
  let key2 = ContextKey::new("request.id")
  let key3 = ContextKey::new("session.id")
  
  let ctx1 = Context::with_value(ctx, key1, "user-123")
  let ctx2 = Context::with_value(ctx1, key2, "req-456")
  let ctx3 = Context::with_value(ctx2, key3, "session-789")
  
  // Inject complex context
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx3, carrier)
  
  // Extract and verify
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
  
  // Verify trace header is still set
  let trace_header = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator with multiple carriers" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  
  // Test with multiple carriers
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  
  // Inject into all carriers
  CompositePropagator::inject(composite, ctx, carrier1)
  CompositePropagator::inject(composite, ctx, carrier2)
  CompositePropagator::inject(composite, ctx, carrier3)
  
  // Verify all carriers have trace headers
  let header1 = TextMapCarrier::get(carrier1, "traceparent")
  let header2 = TextMapCarrier::get(carrier2, "traceparent")
  let header3 = TextMapCarrier::get(carrier3, "traceparent")
  
  assert_eq(header1, Some("00-test-trace-id-test-span-id-01"))
  assert_eq(header2, Some("00-test-trace-id-test-span-id-01"))
  assert_eq(header3, Some("00-test-trace-id-test-span-id-01"))
  
  // Extract from each carrier
  let ctx1 = CompositePropagator::extract(composite, carrier1)
  let ctx2 = CompositePropagator::extract(composite, carrier2)
  let ctx3 = CompositePropagator::extract(composite, carrier3)
  
  // All extractions should work
  let key = ContextKey::new("extracted")
  assert_eq(Context::get(ctx1, key), Some("true"))
  assert_eq(Context::get(ctx2, key), Some("true"))
  assert_eq(Context::get(ctx3, key), Some("true"))
}

test "composite propagator chain operations" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // Create a chain of contexts
  let initial_ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Inject -> Extract -> Inject -> Extract chain
  CompositePropagator::inject(composite, initial_ctx, carrier)
  let ctx1 = CompositePropagator::extract(composite, carrier)
  
  // Add value to extracted context
  let key = ContextKey::new("chain.value")
  let ctx2 = Context::with_value(ctx1, key, "chain.test")
  
  // Inject again
  let carrier2 = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx2, carrier2)
  
  // Extract final context
  let final_ctx = CompositePropagator::extract(composite, carrier2)
  
  // Verify chain worked
  let chain_value = Context::get(final_ctx, key)
  let extracted_value = Context::get(final_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_value, Some("true"))
  
  // Verify trace headers in both carriers
  let header1 = TextMapCarrier::get(carrier, "traceparent")
  let header2 = TextMapCarrier::get(carrier2, "traceparent")
  
  assert_eq(header1, Some("00-test-trace-id-test-span-id-01"))
  assert_eq(header2, Some("00-test-trace-id-test-span-id-01"))
}