// Advanced Composite Propagator Test Suite
// Tests complex propagation scenarios, text map carrier operations, and cross-service consistency

test "basic propagator creation and configuration" {
  // Test W3C Trace Context Propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  
  // Test W3C Baggage Propagator
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Test Composite Propagator with single propagator
  let single_propagator = CompositePropagator::new([trace_propagator])
  assert_eq(single_propagator.propagators.length(), 1)
  
  // Test Composite Propagator with multiple propagators
  let multiple_propagators = [trace_propagator, trace_propagator]
  let composite_propagator = CompositePropagator::new(multiple_propagators)
  assert_eq(composite_propagator.propagators.length(), 2)
  
  // Test Composite Propagator with many propagators
  let many_propagators = Array[W3CTraceContextPropagator]::make(5, trace_propagator)
  let large_composite = CompositePropagator::new(many_propagators)
  assert_eq(large_composite.propagators.length(), 5)
}

test "text map carrier operations" {
  // Test empty carrier creation
  let empty_carrier = TextMapCarrier::new()
  assert_eq(empty_carrier.headers.length(), 0)
  
  // Test carrier with single header
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let retrieved_value = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(retrieved_value, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Test missing header
  let missing_value = TextMapCarrier::get(carrier, "missing-header")
  assert_eq(missing_value, None)
  
  // Test multiple headers
  TextMapCarrier::set(carrier, "baggage", "user.id=user123,session.id=sess456")
  TextMapCarrier::set(carrier, "x-custom-header", "custom-value")
  TextMapCarrier::set(carrier, "authorization", "Bearer token123")
  
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(TextMapCarrier::get(carrier, "baggage"), Some("user.id=user123,session.id=sess456"))
  assert_eq(TextMapCarrier::get(carrier, "x-custom-header"), Some("custom-value"))
  assert_eq(TextMapCarrier::get(carrier, "authorization"), Some("Bearer token123"))
}

test "composite propagator injection operations" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Test injection into empty carrier
  let ctx = Context::root()
  let empty_carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite_propagator, ctx, empty_carrier)
  
  // Verify injected trace context
  let injected_trace = TextMapCarrier::get(empty_carrier, "traceparent")
  assert_eq(injected_trace, Some("00-test-trace-id-test-span-id-01"))
  
  // Test injection into carrier with existing headers
  let existing_carrier = TextMapCarrier::new()
  TextMapCarrier::set(existing_carrier, "existing-header", "existing-value")
  TextMapCarrier::set(existing_carrier, "authorization", "Bearer token")
  
  CompositePropagator::inject(composite_propagator, ctx, existing_carrier)
  
  // Verify existing headers are preserved and new headers are added
  assert_eq(TextMapCarrier::get(existing_carrier, "existing-header"), Some("existing-value"))
  assert_eq(TextMapCarrier::get(existing_carrier, "authorization"), Some("Bearer token"))
  assert_eq(TextMapCarrier::get(existing_carrier, "traceparent"), Some("00-test-trace-id-test-span-id-01"))
  
  // Test injection with context values
  let ctx_with_values = Context::with_value(ctx, ContextKey::new("user.id"), "user123")
  let ctx_carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite_propagator, ctx_with_values, ctx_carrier)
  assert_eq(TextMapCarrier::get(ctx_carrier, "traceparent"), Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator extraction operations" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Test extraction from carrier with trace context
  let carrier_with_trace = TextMapCarrier::new()
  TextMapCarrier::set(carrier_with_trace, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier_with_trace)
  
  // Verify extraction result (simplified implementation returns different context)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
  
  // Test extraction from carrier with multiple headers
  let carrier_with_multiple = TextMapCarrier::new()
  TextMapCarrier::set(carrier_with_multiple, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier_with_multiple, "baggage", "user.id=user123,session.id=sess456")
  TextMapCarrier::set(carrier_with_multiple, "x-custom-header", "custom-value")
  
  let extracted_multiple_ctx = CompositePropagator::extract(composite_propagator, carrier_with_multiple)
  let extracted_multiple_value = Context::get(extracted_multiple_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_multiple_value, Some("true"))
  
  // Test extraction from empty carrier
  let empty_carrier = TextMapCarrier::new()
  let extracted_empty_ctx = CompositePropagator::extract(composite_propagator, empty_carrier)
  let extracted_empty_value = Context::get(extracted_empty_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_empty_value, Some("true"))
  
  // Test extraction from carrier with invalid trace context
  let invalid_carrier = TextMapCarrier::new()
  TextMapCarrier::set(invalid_carrier, "traceparent", "invalid-trace-context")
  
  let extracted_invalid_ctx = CompositePropagator::extract(composite_propagator, invalid_carrier)
  let extracted_invalid_value = Context::get(extracted_invalid_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_invalid_value, Some("true"))
}

test "complex propagation scenarios" {
  // Test multiple propagators in composite
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let multi_propagator = CompositePropagator::new([trace_propagator, trace_propagator])
  
  // Test injection and extraction cycle
  let original_ctx = Context::root()
  let original_ctx_with_values = Context::with_value(original_ctx, ContextKey::new("correlation.id"), "corr-123")
  
  // Inject context
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(multi_propagator, original_ctx_with_values, carrier)
  
  // Extract context
  let extracted_ctx = CompositePropagator::extract(multi_propagator, carrier)
  
  // Verify extraction worked
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
  
  // Test propagation through multiple hops
  let hop1_carrier = TextMapCarrier::new()
  CompositePropagator::inject(multi_propagator, original_ctx_with_values, hop1_carrier)
  
  let hop1_ctx = CompositePropagator::extract(multi_propagator, hop1_carrier)
  let hop1_ctx_with_extra = Context::with_value(hop1_ctx, ContextKey::new("hop1.info"), "processed-by-hop1")
  
  let hop2_carrier = TextMapCarrier::new()
  CompositePropagator::inject(multi_propagator, hop1_ctx_with_extra, hop2_carrier)
  
  let hop2_ctx = CompositePropagator::extract(multi_propagator, hop2_carrier)
  let hop2_value = Context::get(hop2_ctx, ContextKey::new("extracted"))
  assert_eq(hop2_value, Some("true"))
}

test "propagator header formats and edge cases" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Test various traceparent formats
  let valid_trace_formats = [
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01",
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-00"
  ]
  
  for trace_format in valid_trace_formats {
    let carrier = TextMapCarrier::new()
    TextMapCarrier::set(carrier, "traceparent", trace_format)
    
    let ctx = CompositePropagator::extract(composite_propagator, carrier)
    let value = Context::get(ctx, ContextKey::new("extracted"))
    assert_eq(value, Some("true"))
  }
  
  // Test baggage header formats
  let baggage_formats = [
    "user.id=user123",
    "user.id=user123,session.id=sess456",
    "key1=value1,key2=value2,key3=value3",
    "complex.key=complex.value,with=spaces,unicode=测试"
  ]
  
  for baggage_format in baggage_formats {
    let carrier = TextMapCarrier::new()
    TextMapCarrier::set(carrier, "baggage", baggage_format)
    
    let ctx = CompositePropagator::extract(composite_propagator, carrier)
    let value = Context::get(ctx, ContextKey::new("extracted"))
    assert_eq(value, Some("true"))
  }
  
  // Test edge cases
  let edge_case_carrier = TextMapCarrier::new()
  TextMapCarrier::set(edge_case_carrier, "traceparent", "")
  TextMapCarrier::set(edge_case_carrier, "baggage", "")
  TextMapCarrier::set(edge_case_carrier, "empty-header", "")
  
  let edge_case_ctx = CompositePropagator::extract(composite_propagator, edge_case_carrier)
  let edge_case_value = Context::get(edge_case_ctx, ContextKey::new("extracted"))
  assert_eq(edge_case_value, Some("true"))
}

test "propagator performance and large scale operations" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let large_composite = CompositePropagator::new(Array[W3CTraceContextPropagator]::make(10, trace_propagator))
  
  // Test injection with large composite propagator
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(large_composite, ctx, carrier)
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(injected_trace, Some("00-test-trace-id-test-span-id-01"))
  
  // Test extraction with large composite propagator
  let extracted_ctx = CompositePropagator::extract(large_composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
  
  // Test carrier with many headers
  let busy_carrier = TextMapCarrier::new()
  TextMapCarrier::set(busy_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Add many headers
  for i in 0..20 {
    let header_name = "custom-header-" + Int::to_string(i)
    let header_value = "custom-value-" + Int::to_string(i)
    TextMapCarrier::set(busy_carrier, header_name, header_value)
  }
  
  let busy_ctx = CompositePropagator::extract(large_composite, busy_carrier)
  let busy_value = Context::get(busy_ctx, ContextKey::new("extracted"))
  assert_eq(busy_value, Some("true"))
  
  // Verify all custom headers are preserved
  for i in 0..20 {
    let header_name = "custom-header-" + Int::to_string(i)
    let expected_value = "custom-value-" + Int::to_string(i)
    let actual_value = TextMapCarrier::get(busy_carrier, header_name)
    assert_eq(actual_value, Some(expected_value))
  }
}

test "propagator integration with span context" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Test with valid span context
  let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key1=value1")
  let ctx_with_span = Context::with_value(Context::root(), ContextKey::new("span.context"), "span-data")
  
  // Inject span context
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_with_span, carrier)
  
  // Verify trace context is injected
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(injected_trace, Some("00-test-trace-id-test-span-id-01"))
  
  // Extract and verify
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
  
  // Test with invalid span context
  let invalid_span_ctx = SpanContext::new("", "", false, "")
  let ctx_with_invalid_span = Context::with_value(Context::root(), ContextKey::new("span.context"), "invalid-span-data")
  
  let invalid_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_with_invalid_span, invalid_carrier)
  
  let invalid_injected_trace = TextMapCarrier::get(invalid_carrier, "traceparent")
  assert_eq(invalid_injected_trace, Some("00-test-trace-id-test-span-id-01"))
  
  // Test with multiple span contexts in propagation chain
  let parent_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_with_span, parent_carrier)
  
  let parent_ctx = CompositePropagator::extract(composite_propagator, parent_carrier)
  let child_ctx = Context::with_value(parent_ctx, ContextKey::new("child.span"), "child-span-data")
  
  let child_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, child_ctx, child_carrier)
  
  let child_extracted_ctx = CompositePropagator::extract(composite_propagator, child_carrier)
  let child_extracted_value = Context::get(child_extracted_ctx, ContextKey::new("extracted"))
  assert_eq(child_extracted_value, Some("true"))
}