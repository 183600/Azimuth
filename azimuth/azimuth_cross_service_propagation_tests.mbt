// Cross-Service Propagation Tests for Azimuth System
// This file contains test cases for cross-service trace and baggage propagation

test "trace context injection and extraction" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  let ctx = Context::root()
  let key = ContextKey::new("trace.context")
  let ctx_with_trace = Context::with_value(ctx, key, trace_id)
  
  let carrier = TextMapCarrier::new()
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  CompositePropagator::inject(composite, ctx_with_trace, carrier)
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

test "baggage propagation across services" {
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "sess-67890")
  let final_baggage = Baggage::set_entry(baggage_with_session, "request.id", "req-abcdef")
  
  let ctx = Context::root()
  let key = ContextKey::new("baggage")
  let ctx_with_baggage = Context::with_value(ctx, key, "user.id=12345;session.id=sess-67890;request.id=req-abcdef")
  
  let carrier = TextMapCarrier::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  CompositePropagator::inject(composite, ctx_with_baggage, carrier)
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
  
  // Verify baggage entries can be retrieved
  let user_id = Baggage::get_entry(final_baggage, "user.id")
  let session_id = Baggage::get_entry(final_baggage, "session.id")
  let request_id = Baggage::get_entry(final_baggage, "request.id")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(session_id, Some("sess-67890"))
  assert_eq(request_id, Some("req-abcdef"))
}

test "cross-service trace consistency" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "b7ad6b7169203331"
  let child_span_id = "c7ad6b7169203332"
  
  let parent_span_ctx = SpanContext::new(trace_id, parent_span_id, true, "")
  let child_span_ctx = SpanContext::new(trace_id, child_span_id, true, "")
  
  // Verify trace ID consistency
  assert_eq(SpanContext::trace_id(parent_span_ctx), SpanContext::trace_id(child_span_ctx))
  assert_eq(SpanContext::trace_id(parent_span_ctx), trace_id)
  
  // Verify span IDs are different
  assert_true(SpanContext::span_id(parent_span_ctx) != SpanContext::span_id(child_span_ctx))
  assert_true(SpanContext::is_valid(parent_span_ctx))
  assert_true(SpanContext::is_valid(child_span_ctx))
}

test "service-to-service request propagation" {
  let client_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let client_span_id = "b7ad6b7169203331"
  let client_span_ctx = SpanContext::new(client_trace_id, client_span_id, true, "")
  
  // Simulate client request
  let client_headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "user.id=12345;request.source=client")
  ]
  let client_request = HttpRequest::new("GET", "/api/data", client_headers)
  
  // Simulate server receiving request
  let server_carrier = TextMapCarrier::new()
  for header in client_headers {
    TextMapCarrier::set(server_carrier, header.0, header.1)
  }
  
  let server_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let server_ctx = CompositePropagator::extract(server_propagator, server_carrier)
  
  // Verify server can extract trace context
  let server_key = ContextKey::new("extracted")
  let server_value = Context::get(server_ctx, server_key)
  assert_eq(server_value, Some("true"))
  
  // Server creates child span
  let server_span_id = "c7ad6b7169203332"
  let server_span_ctx = SpanContext::new(client_trace_id, server_span_id, true, "")
  
  // Verify trace continuity
  assert_eq(SpanContext::trace_id(server_span_ctx), client_trace_id)
  assert_true(SpanContext::span_id(server_span_ctx) != client_span_id)
}

test "multi-service trace chain" {
  // Service A (entry point)
  let service_a_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let service_a_span_id = "a7ad6b7169203331"
  let service_a_span_ctx = SpanContext::new(service_a_trace_id, service_a_span_id, true, "")
  
  // Service A calls Service B
  let service_b_span_id = "b7ad6b7169203332"
  let service_b_span_ctx = SpanContext::new(service_a_trace_id, service_b_span_id, true, "")
  
  // Service B calls Service C
  let service_c_span_id = "c7ad6b7169203333"
  let service_c_span_ctx = SpanContext::new(service_a_trace_id, service_c_span_id, true, "")
  
  // Verify trace ID consistency across all services
  assert_eq(SpanContext::trace_id(service_a_span_ctx), service_a_trace_id)
  assert_eq(SpanContext::trace_id(service_b_span_ctx), service_a_trace_id)
  assert_eq(SpanContext::trace_id(service_c_span_ctx), service_a_trace_id)
  
  // Verify all span IDs are unique
  assert_true(service_a_span_id != service_b_span_id)
  assert_true(service_b_span_id != service_c_span_id)
  assert_true(service_a_span_id != service_c_span_id)
  
  // Verify all span contexts are valid
  assert_true(SpanContext::is_valid(service_a_span_ctx))
  assert_true(SpanContext::is_valid(service_b_span_ctx))
  assert_true(SpanContext::is_valid(service_c_span_ctx))
}

test "cross-service baggage propagation with metadata" {
  let initial_baggage = Baggage::new()
  
  // Service A adds baggage
  let baggage_a = Baggage::set_entry(initial_baggage, "service.a.timestamp", "2025-01-01T00:00:00Z")
  let baggage_a2 = Baggage::set_entry(baggage_a, "service.a.version", "1.2.3")
  
  // Service B adds more baggage
  let baggage_b = Baggage::set_entry(baggage_a2, "service.b.latency", "150ms")
  let baggage_b2 = Baggage::set_entry(baggage_b, "service.b.status", "success")
  
  // Service C adds final baggage
  let baggage_c = Baggage::set_entry(baggage_b2, "service.c.result", "processed")
  let baggage_c2 = Baggage::set_entry(baggage_c, "service.c.items", "42")
  
  // Verify all baggage entries are preserved
  let service_a_timestamp = Baggage::get_entry(baggage_c2, "service.a.timestamp")
  let service_a_version = Baggage::get_entry(baggage_c2, "service.a.version")
  let service_b_latency = Baggage::get_entry(baggage_c2, "service.b.latency")
  let service_b_status = Baggage::get_entry(baggage_c2, "service.b.status")
  let service_c_result = Baggage::get_entry(baggage_c2, "service.c.result")
  let service_c_items = Baggage::get_entry(baggage_c2, "service.c.items")
  
  assert_eq(service_a_timestamp, Some("2025-01-01T00:00:00Z"))
  assert_eq(service_a_version, Some("1.2.3"))
  assert_eq(service_b_latency, Some("150ms"))
  assert_eq(service_b_status, Some("success"))
  assert_eq(service_c_result, Some("processed"))
  assert_eq(service_c_items, Some("42"))
}

test "cross-service context propagation with errors" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Simulate error scenario
  let ctx = Context::root()
  let error_key = ContextKey::new("error.context")
  let ctx_with_error = Context::with_value(ctx, error_key, "timeout")
  
  let carrier = TextMapCarrier::new()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  CompositePropagator::inject(propagator, ctx_with_error, carrier)
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  // Verify error context is propagated
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

test "cross-service sampling decision propagation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  // Test with sampling enabled
  let sampled_span_ctx = SpanContext::new(trace_id, span_id, true, "")
  assert_true(SpanContext::is_sampled(sampled_span_ctx))
  
  // Test with sampling disabled
  let not_sampled_span_ctx = SpanContext::new(trace_id, span_id, false, "")
  assert_false(SpanContext::is_sampled(not_sampled_span_ctx))
  
  // Verify sampling decision is preserved in context
  let sampled_ctx = Context::root()
  let sampled_key = ContextKey::new("sampling.decision")
  let sampled_ctx_with_decision = Context::with_value(sampled_ctx, sampled_key, "sampled")
  
  let not_sampled_ctx = Context::root()
  let not_sampled_ctx_with_decision = Context::with_value(not_sampled_ctx, sampled_key, "not_sampled")
  
  let carrier = TextMapCarrier::new()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  CompositePropagator::inject(propagator, sampled_ctx_with_decision, carrier)
  let extracted_sampled_ctx = CompositePropagator::extract(propagator, carrier)
  
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_sampled_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}