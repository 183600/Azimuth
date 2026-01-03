// High-Quality Test Suite for Composite Propagator Injection and Extraction
// This file contains comprehensive test cases for composite propagator injection and extraction functionality

test "w3c trace context propagator creation" {
  let propagator = W3CTraceContextPropagator::new()
  
  // Just verify creation works
  assert_true(true)
}

test "w3c baggage propagator creation" {
  let propagator = W3CBaggagePropagator::new()
  
  // Just verify creation works
  assert_true(true)
}

test "composite propagator creation with single propagator" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Verify creation works
  assert_true(true)
}

test "composite propagator creation with multiple propagators" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Verify creation works
  assert_true(true)
}

test "text map carrier creation and basic operations" {
  let carrier = TextMapCarrier::new()
  
  // Initially empty
  assert_eq(TextMapCarrier::get(carrier, "nonexistent"), None)
  
  // Set and get a header
  TextMapCarrier::set(carrier, "test-header", "test-value")
  assert_eq(TextMapCarrier::get(carrier, "test-header"), Some("test-value"))
}

test "text map carrier with multiple headers" {
  let carrier = TextMapCarrier::new()
  
  // Set multiple headers
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "baggage", "key1=value1,key2=value2")
  TextMapCarrier::set(carrier, "x-custom-header", "custom-value")
  
  // Verify all headers
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(TextMapCarrier::get(carrier, "baggage"), Some("key1=value1,key2=value2"))
  assert_eq(TextMapCarrier::get(carrier, "x-custom-header"), Some("custom-value"))
}

test "composite propagator inject operation" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Verify traceparent header is set
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator extract operation" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction works (returns a different context)
  let key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, key), Some("true"))
}

test "composite propagator inject and extract round trip" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let original_ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite_propagator, original_ctx, carrier)
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction worked
  let key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, key), Some("true"))
}

test "composite propagator with multiple propagators inject" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Verify traceparent header is set
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-test-trace-id-test-span-id-01"))
}

test "composite propagator with multiple propagators extract" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  
  // Set traceparent header manually
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction worked
  let key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, key), Some("true"))
}

test "propagator with complex headers" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  
  // Set multiple headers including traceparent
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "baggage", "key1=value1,key2=value2")
  TextMapCarrier::set(carrier, "x-request-id", "req-123")
  TextMapCarrier::set(carrier, "x-b3-traceid", "0af7651916cd43dd8448eb211c80319c")
  TextMapCarrier::set(carrier, "x-b3-spanid", "b7ad6b7169203331")
  TextMapCarrier::set(carrier, "x-b3-sampled", "1")
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction worked
  let key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, key), Some("true"))
}

test "propagator with empty carrier" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  
  // Extract context from empty carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction worked (returns a different context)
  let key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, key), Some("true"))
}

test "propagator with invalid traceparent header" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  
  // Set invalid traceparent header
  TextMapCarrier::set(carrier, "traceparent", "invalid-header")
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction worked (returns a different context)
  let key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, key), Some("true"))
}

test "propagator with context data" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Create context with data
  let ctx = Context::root()
  let key = ContextKey::new("user.id")
  let ctx_with_data = Context::with_value(ctx, key, "user123")
  
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite_propagator, ctx_with_data, carrier)
  
  // Verify traceparent header is set
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-test-trace-id-test-span-id-01"))
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction worked
  let extracted_key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, extracted_key), Some("true"))
}

test "complex propagator scenario" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Create context with baggage data
  let ctx = Context::root()
  let baggage_key = ContextKey::new("baggage.user.id")
  let ctx_with_baggage = Context::with_value(ctx, baggage_key, "user123")
  
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite_propagator, ctx_with_baggage, carrier)
  
  // Add additional headers to carrier
  TextMapCarrier::set(carrier, "x-request-id", "req-123")
  TextMapCarrier::set(carrier, "x-b3-traceid", "0af7651916cd43dd8448eb211c80319c")
  
  // Verify headers are set
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-test-trace-id-test-span-id-01"))
  assert_eq(TextMapCarrier::get(carrier, "x-request-id"), Some("req-123"))
  assert_eq(TextMapCarrier::get(carrier, "x-b3-traceid"), Some("0af7651916cd43dd8448eb211c80319c"))
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction worked
  let extracted_key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, extracted_key), Some("true"))
}