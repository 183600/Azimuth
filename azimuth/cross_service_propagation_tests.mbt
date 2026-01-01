// Cross-Service Telemetry Propagation Tests for Azimuth Telemetry System
// This file contains test cases for cross-service telemetry propagation

test "trace context injection and extraction" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  let root_ctx = Context::root()
  let ctx_key = ContextKey::new("trace.context")
  let ctx_with_span = Context::with_value(root_ctx, ctx_key, trace_id + ":" + span_id)
  
  let carrier = TextMapCarrier::new()
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // Inject context into carrier
  CompositePropagator::inject(composite, ctx_with_span, carrier)
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_value, Some("true"))
}

test "baggage propagation across services" {
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let final_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  
  let user_id = Baggage::get_entry(final_baggage, "user.id")
  let request_id = Baggage::get_entry(final_baggage, "request.id")
  let missing = Baggage::get_entry(final_baggage, "nonexistent")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(missing, None)
}

test "HTTP request trace propagation" {
  let headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  ]
  
  let request = HttpRequest::new("GET", "https://api.example.com/users", headers)
  let carrier = TextMapCarrier::new()
  
  // Simulate extracting trace context from HTTP headers
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  let extracted_traceparent = TextMapCarrier::get(carrier, "traceparent")
  let extracted_tracestate = TextMapCarrier::get(carrier, "tracestate")
  
  assert_eq(extracted_traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(extracted_tracestate, Some("rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"))
}

test "cross-service context chain" {
  // Service A creates initial context
  let service_a_ctx = Context::root()
  let service_a_key = ContextKey::new("service.a")
  let ctx_with_a = Context::with_value(service_a_ctx, service_a_key, "started")
  
  // Service B adds to context
  let service_b_key = ContextKey::new("service.b")
  let ctx_with_b = Context::with_value(ctx_with_a, service_b_key, "processed")
  
  // Service C adds to context
  let service_c_key = ContextKey::new("service.c")
  let ctx_with_c = Context::with_value(ctx_with_b, service_c_key, "completed")
  
  // Verify all context values are preserved
  let service_a_value = Context::get(ctx_with_c, service_a_key)
  let service_b_value = Context::get(ctx_with_c, service_b_key)
  let service_c_value = Context::get(ctx_with_c, service_c_key)
  
  assert_eq(service_a_value, Some("started"))
  assert_eq(service_b_value, Some("processed"))
  assert_eq(service_c_value, Some("completed"))
}

test "multi-propagator coordination" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // Inject using composite propagator
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Extract using composite propagator
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction succeeded
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "trace context sampling decisions" {
  // Test with sampled context
  let sampled_ctx = SpanContext::new("trace123", "span456", true, "key1=value1")
  assert_true(SpanContext::is_sampled(sampled_ctx))
  assert_true(SpanContext::is_valid(sampled_ctx))
  
  // Test with non-sampled context
  let nonsampled_ctx = SpanContext::new("trace789", "span101", false, "key2=value2")
  assert_false(SpanContext::is_sampled(nonsampled_ctx))
  assert_true(SpanContext::is_valid(nonsampled_ctx))
  
  // Test with invalid context
  let invalid_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_sampled(invalid_ctx))
  assert_false(SpanContext::is_valid(invalid_ctx))
}

test "baggage entry removal and modification" {
  let baggage = Baggage::new()
  let with_entries = Baggage::set_entry(baggage, "temp.value", "should-be-removed")
  let with_entries2 = Baggage::set_entry(with_entries, "permanent.value", "should-remain")
  
  // Verify both entries exist
  let temp_value = Baggage::get_entry(with_entries2, "temp.value")
  let permanent_value = Baggage::get_entry(with_entries2, "permanent.value")
  
  assert_eq(temp_value, Some("should-be-removed"))
  assert_eq(permanent_value, Some("should-remain"))
  
  // Remove temporary entry
  let final_baggage = Baggage::remove_entry(with_entries2, "temp.value")
  
  // Verify removal
  let removed_temp = Baggage::get_entry(final_baggage, "temp.value")
  let still_permanent = Baggage::get_entry(final_baggage, "permanent.value")
  
  assert_eq(removed_temp, None)
  assert_eq(still_permanent, Some("should-remain"))
}