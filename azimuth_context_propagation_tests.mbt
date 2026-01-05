// Azimuth Telemetry System - Distributed Context Propagation Tests
// This file contains test cases for distributed tracing context propagation

// Test 1: Basic Context Propagation
test "basic context propagation" {
  // Create a root span context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let root_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Create a propagator
  let propagator = TraceContextPropagator::new()
  
  // Inject context into carrier
  let carrier = TextMapCarrier::new()
  Propagator::inject(propagator, root_ctx, carrier)
  
  // Extract context from carrier
  let extracted_ctx = Propagator::extract(propagator, carrier)
  
  // Verify extracted context matches original
  assert_eq(SpanContext::trace_id(extracted_ctx), trace_id)
  assert_eq(SpanContext::span_id(extracted_ctx), span_id)
  assert_true(SpanContext::is_sampled(extracted_ctx))
}

// Test 2: HTTP Headers Context Propagation
test "http headers context propagation" {
  // Create a span context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  
  // Create HTTP headers carrier
  let headers = HttpHeaders::new()
  
  // Inject context into HTTP headers
  let propagator = TraceContextPropagator::new()
  Propagator::inject(propagator, span_ctx, headers)
  
  // Verify traceparent header is present
  let traceparent = HttpHeaders::get(headers, "traceparent")
  match traceparent {
    Some(value) => {
      assert_true(value.contains("00"))
      assert_true(value.contains(trace_id))
      assert_true(value.contains(span_id))
      assert_true(value.contains("01")) // sampled flag
    }
    None => assert_true(false)
  }
  
  // Verify tracestate header is present
  let tracestate = HttpHeaders::get(headers, "tracestate")
  match tracestate {
    Some(value) => assert_eq(value, "test_state")
    None => assert_true(false)
  }
  
  // Extract context from HTTP headers
  let extracted_ctx = Propagator::extract(propagator, headers)
  
  // Verify extracted context matches original
  assert_eq(SpanContext::trace_id(extracted_ctx), trace_id)
  assert_eq(SpanContext::span_id(extracted_ctx), span_id)
  assert_true(SpanContext::is_sampled(extracted_ctx))
}

// Test 3: Baggage Propagation
test "baggage propagation" {
  // Create a context with baggage
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let final_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  
  let context = Context::with_value(Context::root(), ContextKey::new("baggage"), final_baggage)
  
  // Create a baggage propagator
  let baggage_propagator = BaggagePropagator::new()
  
  // Inject baggage into carrier
  let carrier = TextMapCarrier::new()
  Propagator::inject(baggage_propagator, context, carrier)
  
  // Extract baggage from carrier
  let extracted_context = Propagator::extract(baggage_propagator, carrier)
  
  // Verify baggage entries are preserved
  let extracted_baggage = Context::get(extracted_context, ContextKey::new("baggage"))
  match extracted_baggage {
    Some(b) => {
      let user_id = Baggage::get_entry(b, "user.id")
      let request_id = Baggage::get_entry(b, "request.id")
      
      match (user_id, request_id) {
        (Some(uid), Some(rid)) => {
          assert_eq(uid, "12345")
          assert_eq(rid, "req-67890")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 4: Composite Propagation
test "composite propagation" {
  // Create a span context with baggage
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  
  // Create a context with both span context and baggage
  let context = Context::with_value(
    Context::with_value(Context::root(), ContextKey::new("baggage"), updated_baggage),
    ContextKey::new("span_context"),
    span_ctx
  )
  
  // Create a composite propagator
  let trace_propagator = TraceContextPropagator::new()
  let baggage_propagator = BaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // Inject context into carrier
  let carrier = TextMapCarrier::new()
  Propagator::inject(composite_propagator, context, carrier)
  
  // Extract context from carrier
  let extracted_context = Propagator::extract(composite_propagator, carrier)
  
  // Verify span context is preserved
  let extracted_span_ctx = Context::get(extracted_context, ContextKey::new("span_context"))
  match extracted_span_ctx {
    Some(ctx) => {
      assert_eq(SpanContext::trace_id(ctx), trace_id)
      assert_eq(SpanContext::span_id(ctx), span_id)
      assert_true(SpanContext::is_sampled(ctx))
    }
    None => assert_true(false)
  }
  
  // Verify baggage is preserved
  let extracted_baggage = Context::get(extracted_context, ContextKey::new("baggage"))
  match extracted_baggage {
    Some(b) => {
      let user_id = Baggage::get_entry(b, "user.id")
      match user_id {
        Some(uid) => assert_eq(uid, "12345")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 5: Cross-Service Context Propagation
test "cross-service context propagation" {
  // Service A creates a span
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_a_id = "b7ad6b7169203331"
  let span_a_ctx = SpanContext::new(trace_id, span_a_id, true, "")
  
  // Service A injects context into HTTP headers
  let headers_a = HttpHeaders::new()
  let propagator = TraceContextPropagator::new()
  Propagator::inject(propagator, span_a_ctx, headers_a)
  
  // Service B extracts context from HTTP headers
  let extracted_ctx_b = Propagator::extract(propagator, headers_a)
  
  // Service B creates a child span
  let span_b_id = "c7ad6b7169203331"
  let span_b_ctx = SpanContext::new(trace_id, span_b_id, true, "")
  span_b_ctx.parent_span_id = Some(span_a_id)
  
  // Service B injects context into new headers for Service C
  let headers_b = HttpHeaders::new()
  Propagator::inject(propagator, span_b_ctx, headers_b)
  
  // Service C extracts context from headers
  let extracted_ctx_c = Propagator::extract(propagator, headers_b)
  
  // Verify trace context is preserved across services
  assert_eq(SpanContext::trace_id(extracted_ctx_c), trace_id)
  assert_eq(SpanContext::span_id(extracted_ctx_c), span_b_id)
  assert_true(SpanContext::is_sampled(extracted_ctx_c))
}

// Test 6: Context Propagation with Sampling Decision
test "context propagation with sampling decision" {
  // Create a non-sampled span context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let non_sampled_ctx = SpanContext::new(trace_id, span_id, false, "")
  
  // Create a propagator
  let propagator = TraceContextPropagator::new()
  
  // Inject context into carrier
  let carrier = TextMapCarrier::new()
  Propagator::inject(propagator, non_sampled_ctx, carrier)
  
  // Extract context from carrier
  let extracted_ctx = Propagator::extract(propagator, carrier)
  
  // Verify sampling decision is preserved
  assert_eq(SpanContext::trace_id(extracted_ctx), trace_id)
  assert_eq(SpanContext::span_id(extracted_ctx), span_id)
  assert_false(SpanContext::is_sampled(extracted_ctx))
}

// Test 7: Context Propagation with Custom Fields
test "context propagation with custom fields" {
  // Create a span context with custom state
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let trace_state = "vendor1=value1,vendor2=value2"
  let span_ctx = SpanContext::new(trace_id, span_id, true, trace_state)
  
  // Create a propagator
  let propagator = TraceContextPropagator::new()
  
  // Inject context into carrier
  let carrier = TextMapCarrier::new()
  Propagator::inject(propagator, span_ctx, carrier)
  
  // Extract context from carrier
  let extracted_ctx = Propagator::extract(propagator, carrier)
  
  // Verify trace state is preserved
  assert_eq(SpanContext::trace_id(extracted_ctx), trace_id)
  assert_eq(SpanContext::span_id(extracted_ctx), span_id)
  assert_true(SpanContext::is_sampled(extracted_ctx))
  assert_eq(SpanContext::trace_state(extracted_ctx), trace_state)
}

// Test 8: Context Propagation with Corrupted Data
test "context propagation with corrupted data" {
  // Create a propagator
  let propagator = TraceContextPropagator::new()
  
  // Test with corrupted traceparent
  let corrupted_carrier = TextMapCarrier::new()
  TextMapCarrier::set(corrupted_carrier, "traceparent", "invalid-format")
  
  // Extract should handle corrupted data gracefully
  let extracted_ctx = Propagator::extract(propagator, corrupted_carrier)
  
  // Should return an invalid context
  assert_false(SpanContext::is_valid(extracted_ctx))
  
  // Test with missing traceparent
  let empty_carrier = TextMapCarrier::new()
  
  // Extract should handle missing data gracefully
  let empty_ctx = Propagator::extract(propagator, empty_carrier)
  
  // Should return an invalid context
  assert_false(SpanContext::is_valid(empty_ctx))
}

// Test 9: Context Propagation with Multiple Formats
test "context propagation with multiple formats" {
  // Create a span context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Test with text map carrier
  let text_carrier = TextMapCarrier::new()
  let text_propagator = TraceContextPropagator::new()
  Propagator::inject(text_propagator, span_ctx, text_carrier)
  let extracted_from_text = Propagator::extract(text_propagator, text_carrier)
  
  assert_eq(SpanContext::trace_id(extracted_from_text), trace_id)
  assert_eq(SpanContext::span_id(extracted_from_text), span_id)
  
  // Test with HTTP headers carrier
  let http_carrier = HttpHeaders::new()
  Propagator::inject(text_propagator, span_ctx, http_carrier)
  let extracted_from_http = Propagator::extract(text_propagator, http_carrier)
  
  assert_eq(SpanContext::trace_id(extracted_from_http), trace_id)
  assert_eq(SpanContext::span_id(extracted_from_http), span_id)
  
  // Test with binary carrier
  let binary_carrier = BinaryCarrier::new()
  let binary_propagator = BinaryTraceContextPropagator::new()
  Propagator::inject(binary_propagator, span_ctx, binary_carrier)
  let extracted_from_binary = Propagator::extract(binary_propagator, binary_carrier)
  
  assert_eq(SpanContext::trace_id(extracted_from_binary), trace_id)
  assert_eq(SpanContext::span_id(extracted_from_binary), span_id)
}

// Test 10: Context Propagation with Remote Parent
test "context propagation with remote parent" {
  // Create a remote parent span context
  let remote_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let remote_span_id = "b7ad6b7169203331"
  let remote_ctx = SpanContext::new(remote_trace_id, remote_span_id, true, "")
  remote_ctx.is_remote = true
  
  // Create a propagator
  let propagator = TraceContextPropagator::new()
  
  // Inject remote context into carrier
  let carrier = TextMapCarrier::new()
  Propagator::inject(propagator, remote_ctx, carrier)
  
  // Extract context from carrier
  let extracted_ctx = Propagator::extract(propagator, carrier)
  
  // Verify remote flag is preserved
  assert_eq(SpanContext::trace_id(extracted_ctx), remote_trace_id)
  assert_eq(SpanContext::span_id(extracted_ctx), remote_span_id)
  assert_true(SpanContext::is_sampled(extracted_ctx))
  assert_true(SpanContext::is_remote(extracted_ctx))
  
  // Create a local child span
  let local_span_id = "c7ad6b7169203331"
  let local_ctx = SpanContext::new(remote_trace_id, local_span_id, true, "")
  local_ctx.parent_span_id = Some(remote_span_id)
  local_ctx.is_remote = false
  
  // Inject local context into carrier
  let local_carrier = TextMapCarrier::new()
  Propagator::inject(propagator, local_ctx, local_carrier)
  
  // Extract local context from carrier
  let extracted_local_ctx = Propagator::extract(propagator, local_carrier)
  
  // Verify local context
  assert_eq(SpanContext::trace_id(extracted_local_ctx), remote_trace_id)
  assert_eq(SpanContext::span_id(extracted_local_ctx), local_span_id)
  assert_true(SpanContext::is_sampled(extracted_local_ctx))
  assert_false(SpanContext::is_remote(extracted_local_ctx))
}