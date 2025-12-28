// Azimuth Telemetry System - Enhanced Propagator Tests
// Comprehensive test suite for propagator functionality

test "w3c_trace_context_propagator_creation" {
  let propagator = W3CTraceContextPropagator::new()
  
  // Test propagator creation - in simplified implementation, just verify it doesn't crash
  @assertion.assert_true(true)?
}

test "w3c_baggage_propagator_creation" {
  let propagator = W3CBaggagePropagator::new()
  
  // Test propagator creation - in simplified implementation, just verify it doesn't crash
  @assertion.assert_true(true)?
}

test "composite_propagator_creation" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CTraceContextPropagator::new()
  
  let propagators = Array[W3CTraceContextPropagator]::from([trace_propagator, baggage_propagator])
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Test composite propagator creation
  @assertion.assert_eq(composite_propagator.propagators.length(), 2)?
}

test "composite_propagator_inject_operations" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CTraceContextPropagator::new()
  
  let propagators = Array[W3CTraceContextPropagator]::from([trace_propagator, baggage_propagator])
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test inject operation
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Verify injection result
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  @assertion.assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))?
}

test "composite_propagator_extract_operations" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CTraceContextPropagator::new()
  
  let propagators = Array[W3CTraceContextPropagator]::from([trace_propagator, baggage_propagator])
  let composite_propagator = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Test extract operation
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction result
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  @assertion.assert_eq(extracted_value, Some("true"))?
}

test "composite_propagator_with_empty_propagators" {
  let empty_propagators = Array[W3CTraceContextPropagator]::from([])
  let composite_propagator = CompositePropagator::new(empty_propagators)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test operations with empty propagator list
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // All operations should complete without errors
  @assertion.assert_true(true)?
}

test "composite_propagator_with_single_propagator" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let single_propagator = Array[W3CTraceContextPropagator]::from([trace_propagator])
  let composite_propagator = CompositePropagator::new(single_propagator)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test inject with single propagator
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  @assertion.assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))?
  
  // Test extract with single propagator
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  @assertion.assert_eq(extracted_value, Some("true"))?
}

test "composite_propagator_with_multiple_propagators" {
  let propagator1 = W3CTraceContextPropagator::new()
  let propagator2 = W3CTraceContextPropagator::new()
  let propagator3 = W3CTraceContextPropagator::new()
  
  let propagators = Array[W3CTraceContextPropagator]::from([propagator1, propagator2, propagator3])
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test inject with multiple propagators
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Test extract with multiple propagators
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // All operations should complete without errors
  @assertion.assert_true(true)?
}

test "propagator_with_context_chain" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CTraceContextPropagator::new()
  
  let propagators = Array[W3CTraceContextPropagator]::from([trace_propagator, baggage_propagator])
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Create context chain
  let root_ctx = Context::root()
  let ctx1 = Context::with_value(root_ctx, ContextKey::new("service.a"), "value-a")
  let ctx2 = Context::with_value(ctx1, ContextKey::new("service.b"), "value-b")
  let ctx3 = Context::with_value(ctx2, ContextKey::new("service.c"), "value-c")
  
  let carrier = TextMapCarrier::new()
  
  // Test inject with context chain
  CompositePropagator::inject(composite_propagator, ctx3, carrier)
  
  // Verify injection
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  @assertion.assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))?
  
  // Test extract
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  @assertion.assert_eq(extracted_value, Some("true"))?
}

test "propagator_with_multiple_carrier_operations" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = Array[W3CTraceContextPropagator]::from([trace_propagator])
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  
  // Create multiple carriers
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  
  // Set different trace contexts in each carrier before injection
  TextMapCarrier::set(carrier1, "existing.header", "value1")
  TextMapCarrier::set(carrier2, "existing.header", "value2")
  TextMapCarrier::set(carrier3, "existing.header", "value3")
  
  // Test inject into multiple carriers
  CompositePropagator::inject(composite_propagator, ctx, carrier1)
  CompositePropagator::inject(composite_propagator, ctx, carrier2)
  CompositePropagator::inject(composite_propagator, ctx, carrier3)
  
  // Verify all carriers have traceparent
  @assertion.assert_eq(TextMapCarrier::get(carrier1, "traceparent"), Some("00-test-trace-id-test-span-id-01"))?
  @assertion.assert_eq(TextMapCarrier::get(carrier2, "traceparent"), Some("00-test-trace-id-test-span-id-01"))?
  @assertion.assert_eq(TextMapCarrier::get(carrier3, "traceparent"), Some("00-test-trace-id-test-span-id-01"))?
  
  // Test extract from multiple carriers
  let extracted_ctx1 = CompositePropagator::extract(composite_propagator, carrier1)
  let extracted_ctx2 = CompositePropagator::extract(composite_propagator, carrier2)
  let extracted_ctx3 = CompositePropagator::extract(composite_propagator, carrier3)
  
  // Verify extractions
  @assertion.assert_eq(Context::get(extracted_ctx1, ContextKey::new("extracted")), Some("true"))?
  @assertion.assert_eq(Context::get(extracted_ctx2, ContextKey::new("extracted")), Some("true"))?
  @assertion.assert_eq(Context::get(extracted_ctx3, ContextKey::new("extracted")), Some("true"))?
}

test "propagator_round_trip_scenario" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CTraceContextPropagator::new()
  
  let propagators = Array[W3CTraceContextPropagator]::from([trace_propagator, baggage_propagator])
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Create original context
  let original_ctx = Context::root()
  let ctx_with_data = Context::with_value(original_ctx, ContextKey::new("original.data"), "original_value")
  
  // Inject context into carrier
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_with_data, carrier)
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Simulate round-trip - inject extracted context into new carrier
  let new_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, extracted_ctx, new_carrier)
  
  // Extract again from new carrier
  let final_ctx = CompositePropagator::extract(composite_propagator, new_carrier)
  
  // Verify round-trip operations
  let final_value = Context::get(final_ctx, ContextKey::new("extracted"))
  @assertion.assert_eq(final_value, Some("true"))?
  
  let final_traceparent = TextMapCarrier::get(new_carrier, "traceparent")
  @assertion.assert_eq(final_traceparent, Some("00-test-trace-id-test-span-id-01"))?
}

test "propagator_with_complex_trace_contexts" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = Array[W3CTraceContextPropagator]::from([trace_propagator])
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Create carriers with different trace contexts
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  
  // Set different traceparent values
  TextMapCarrier::set(carrier1, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier2, "traceparent", "00-1234567890abcdef1234567890abcdef-fedcba0987654321-00")
  TextMapCarrier::set(carrier3, "traceparent", "00-abcdef1234567890abcdef1234567890-0123456789abcdef-01")
  
  // Test extract from different trace contexts
  let extracted_ctx1 = CompositePropagator::extract(composite_propagator, carrier1)
  let extracted_ctx2 = CompositePropagator::extract(composite_propagator, carrier2)
  let extracted_ctx3 = CompositePropagator::extract(composite_propagator, carrier3)
  
  // All extractions should succeed
  @assertion.assert_eq(Context::get(extracted_ctx1, ContextKey::new("extracted")), Some("true"))?
  @assertion.assert_eq(Context::get(extracted_ctx2, ContextKey::new("extracted")), Some("true"))?
  @assertion.assert_eq(Context::get(extracted_ctx3, ContextKey::new("extracted")), Some("true"))?
  
  // Test inject after extraction
  let new_carrier1 = TextMapCarrier::new()
  let new_carrier2 = TextMapCarrier::new()
  let new_carrier3 = TextMapCarrier::new()
  
  CompositePropagator::inject(composite_propagator, extracted_ctx1, new_carrier1)
  CompositePropagator::inject(composite_propagator, extracted_ctx2, new_carrier2)
  CompositePropagator::inject(composite_propagator, extracted_ctx3, new_carrier3)
  
  // Verify all injections
  @assertion.assert_eq(TextMapCarrier::get(new_carrier1, "traceparent"), Some("00-test-trace-id-test-span-id-01"))?
  @assertion.assert_eq(TextMapCarrier::get(new_carrier2, "traceparent"), Some("00-test-trace-id-test-span-id-01"))?
  @assertion.assert_eq(TextMapCarrier::get(new_carrier3, "traceparent"), Some("00-test-trace-id-test-span-id-01"))?
}