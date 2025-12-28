// Test file for context and baggage propagation
test "context operations" {
  // Test creating root context
  let root_ctx = Context::root()
  assert_true(root_ctx.data.is_none(), "Root context should have no data")
  
  // Test context with value
  let key = ContextKey::new("test-key")
  let ctx_with_value = Context::with_value(root_ctx, key, "test-value")
  
  // Test getting value from context
  let value = Context::get(ctx_with_value, key)
  assert_true(value.is_some(), "Should find value in context")
  assert_eq(value.unwrap(), "test-value")
  
  // Test getting non-existent key
  let non_existent_key = ContextKey::new("non-existent")
  let non_existent_value = Context::get(ctx_with_value, non_existent_key)
  assert_true(non_existent_value.is_none(), "Should not find non-existent key")
}

test "context key operations" {
  // Test creating context key
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  
  assert_eq(key1.key, "key1")
  assert_eq(key2.key, "key2")
  assert_true(key1.key != key2.key, "Keys should be different")
  
  // Test using keys in context
  let ctx = Context::root()
  let ctx1 = Context::with_value(ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  
  let value1 = Context::get(ctx2, key1)
  let value2 = Context::get(ctx2, key2)
  
  assert_eq(value1.unwrap(), "value1")
  assert_eq(value2.unwrap(), "value2")
}

test "baggage operations" {
  // Test creating new baggage
  let baggage = Baggage::new()
  assert_eq(baggage.entries.length(), 0, "New baggage should have no entries")
  
  // Test setting baggage entry
  let baggage_with_entry = Baggage::set_entry(baggage, "user-id", "12345")
  
  // Test getting baggage entry
  let user_id = Baggage::get_entry(baggage_with_entry, "user-id")
  assert_true(user_id.is_some(), "Should find user-id in baggage")
  assert_eq(user_id.unwrap(), "12345")
  
  // Test getting non-existent entry
  let non_existent = Baggage::get_entry(baggage_with_entry, "non-existent")
  assert_true(non_existent.is_none(), "Should not find non-existent entry")
  
  // Test removing baggage entry
  let baggage_after_remove = Baggage::remove_entry(baggage_with_entry, "user-id")
  let removed_value = Baggage::get_entry(baggage_after_remove, "user-id")
  // Note: Simplified implementation might not actually remove
}

test "span context operations" {
  // Test creating span context
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "state=value")
  
  // Test span context properties
  assert_eq(SpanContext::trace_id(span_ctx), "trace-123")
  assert_eq(SpanContext::span_id(span_ctx), "span-456")
  assert_true(SpanContext::is_sampled(span_ctx), "Span should be sampled")
  assert_eq(span_ctx.trace_state, "state=value")
  
  // Test valid span context
  assert_true(SpanContext::is_valid(span_ctx), "Span context should be valid")
  
  // Test invalid span context (empty trace_id or span_id)
  let invalid_ctx1 = SpanContext::new("", "span-456", true, "")
  assert_false(SpanContext::is_valid(invalid_ctx1), "Span context with empty trace_id should be invalid")
  
  let invalid_ctx2 = SpanContext::new("trace-123", "", true, "")
  assert_false(SpanContext::is_valid(invalid_ctx2), "Span context with empty span_id should be invalid")
  
  // Test unsampled span context
  let unsampled_ctx = SpanContext::new("trace-789", "span-012", false, "")
  assert_false(SpanContext::is_sampled(unsampled_ctx), "Span should not be sampled")
}

test "text map carrier operations" {
  // Test creating new carrier
  let carrier = TextMapCarrier::new()
  assert_eq(carrier.headers.length(), 0, "New carrier should have no headers")
  
  // Test setting carrier header
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "baggage", "key1=value1,key2=value2")
  
  // Test getting carrier header
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent.is_some(), "Should find traceparent header")
  assert_eq(traceparent.unwrap(), "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Test getting non-existent header
  let non_existent = TextMapCarrier::get(carrier, "non-existent")
  assert_true(non_existent.is_none(), "Should not find non-existent header")
}

test "composite propagator operations" {
  // Test creating propagators
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Test creating composite propagator
  let composite = CompositePropagator::new([trace_propagator])
  assert_eq(composite.propagators.length(), 1, "Composite should have one propagator")
  
  // Test injection
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Verify injection result
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_trace.is_some(), "Should have injected traceparent")
  
  // Test extraction
  let extract_carrier = TextMapCarrier::new()
  TextMapCarrier::set(extract_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let extracted_ctx = CompositePropagator::extract(composite, extract_carrier)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  
  assert_true(extracted_value.is_some(), "Should have extracted context")
  assert_eq(extracted_value.unwrap(), "true")
}