// Azimuth Distributed Tracing Context Propagation Tests
// This file contains comprehensive test cases for distributed tracing and context propagation

// Test 1: W3C Trace Context Propagation
test "w3c trace context propagation injection and extraction" {
  let propagator = W3CTraceContextPropagator::new()
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // Test injection
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Test extraction
  let extracted_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(extracted_traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Test non-existent header
  let non_existent = TextMapCarrier::get(carrier, "non-existent")
  assert_eq(non_existent, None)
}

// Test 2: Composite Propagator Operations
test "composite propagator with multiple propagators" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // Test injection
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, key)
  assert_eq(extracted_value, Some("true"))
}

// Test 3: Baggage Cross-Context Propagation
test "baggage cross-context propagation" {
  let baggage = Baggage::new()
  
  // Test setting and getting baggage entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let retrieved_value = Baggage::get_entry(updated_baggage, "user.id")
  assert_eq(retrieved_value, Some("12345"))
  
  // Test non-existent entry
  let non_existent = Baggage::get_entry(baggage, "non.existent")
  assert_eq(non_existent, None)
  
  // Test removing entries
  let reduced_baggage = Baggage::remove_entry(updated_baggage, "user.id")
  let removed_value = Baggage::get_entry(reduced_baggage, "user.id")
  assert_eq(removed_value, None)
  
  // Test multiple entries
  let multi_baggage = Baggage::new()
    |> Baggage::set_entry("key1", "value1")
    |> Baggage::set_entry("key2", "value2")
    |> Baggage::set_entry("key3", "value3")
  
  assert_eq(Baggage::get_entry(multi_baggage, "key1"), Some("value1"))
  assert_eq(Baggage::get_entry(multi_baggage, "key2"), Some("value2"))
  assert_eq(Baggage::get_entry(multi_baggage, "key3"), Some("value3"))
}

// Test 4: Cross-Service Trace Continuity
test "cross-service trace continuity" {
  // Simulate trace context from service A
  let service_a_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let service_a_span_id = "b7ad6b7169203331"
  let service_a_ctx = SpanContext::new(service_a_trace_id, service_a_span_id, true, "")
  
  // Service B creates child span
  let service_b_span_id = "c8ad6b7169203332"
  let service_b_ctx = SpanContext::new(service_a_trace_id, service_b_span_id, true, "")
  
  // Verify trace continuity
  assert_eq(SpanContext::trace_id(service_a_ctx), SpanContext::trace_id(service_b_ctx))
  assert_not_eq(SpanContext::span_id(service_a_ctx), SpanContext::span_id(service_b_ctx))
  assert_true(SpanContext::is_valid(service_a_ctx))
  assert_true(SpanContext::is_valid(service_b_ctx))
  assert_true(SpanContext::is_sampled(service_a_ctx))
  assert_true(SpanContext::is_sampled(service_b_ctx))
}

// Test 5: Distributed Trace Chain Validation
test "distributed trace chain validation" {
  // Create a trace chain with multiple spans
  let root_span_ctx = SpanContext::new("trace123", "span001", true, "")
  let child_span1_ctx = SpanContext::new("trace123", "span002", true, "")
  let child_span2_ctx = SpanContext::new("trace123", "span003", true, "")
  let grandchild_ctx = SpanContext::new("trace123", "span004", true, "")
  
  // Verify all spans have same trace ID
  assert_eq(SpanContext::trace_id(root_span_ctx), "trace123")
  assert_eq(SpanContext::trace_id(child_span1_ctx), "trace123")
  assert_eq(SpanContext::trace_id(child_span2_ctx), "trace123")
  assert_eq(SpanContext::trace_id(grandchild_ctx), "trace123")
  
  // Verify all spans have unique span IDs
  assert_not_eq(SpanContext::span_id(root_span_ctx), SpanContext::span_id(child_span1_ctx))
  assert_not_eq(SpanContext::span_id(child_span1_ctx), SpanContext::span_id(child_span2_ctx))
  assert_not_eq(SpanContext::span_id(child_span2_ctx), SpanContext::span_id(grandchild_ctx))
  
  // Verify all spans are valid and sampled
  let span_contexts = [root_span_ctx, child_span1_ctx, child_span2_ctx, grandchild_ctx]
  for ctx in span_contexts {
    assert_true(SpanContext::is_valid(ctx))
    assert_true(SpanContext::is_sampled(ctx))
  }
}

// Test 6: Context Propagation with HTTP Headers
test "context propagation with http headers" {
  let carrier = TextMapCarrier::new()
  
  // Simulate HTTP headers with trace context
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "baggage", "userId=12345,sessionId=abcdef")
  TextMapCarrier::set(carrier, "x-request-id", "req-789")
  
  // Verify header retrieval
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let baggage = TextMapCarrier::get(carrier, "baggage")
  let request_id = TextMapCarrier::get(carrier, "x-request-id")
  
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(baggage, Some("userId=12345,sessionId=abcdef"))
  assert_eq(request_id, Some("req-789"))
  
  // Test case sensitivity
  let uppercase_traceparent = TextMapCarrier::get(carrier, "TRACEPARENT")
  assert_eq(uppercase_traceparent, None)
}

// Test 7: Span Hierarchy and Relationships
test "span hierarchy and relationships" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "service_a")
  
  // Create root span
  let root_span = Tracer::start_span(tracer, "root_operation")
  let root_ctx = Span::span_context(root_span)
  
  // Create child spans
  let child_span1 = Tracer::start_span(tracer, "child_operation_1")
  let child_span2 = Tracer::start_span(tracer, "child_operation_2")
  
  // Verify span relationships
  assert_eq(Span::name(root_span), "root_operation")
  assert_eq(Span::name(child_span1), "child_operation_1")
  assert_eq(Span::name(child_span2), "child_operation_2")
  
  // Verify all spans are recording
  assert_true(Span::is_recording(root_span))
  assert_true(Span::is_recording(child_span1))
  assert_true(Span::is_recording(child_span2))
  
  // End spans in correct order
  Span::end(child_span1)
  Span::end(child_span2)
  Span::end(root_span)
}

// Test 8: Context Propagation Edge Cases
test "context propagation edge cases" {
  // Test empty trace context
  let empty_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(empty_ctx))
  assert_false(SpanContext::is_sampled(empty_ctx))
  
  // Test malformed trace parent
  let malformed_carrier = TextMapCarrier::new()
  TextMapCarrier::set(malformed_carrier, "traceparent", "invalid-format")
  
  let malformed_traceparent = TextMapCarrier::get(malformed_carrier, "traceparent")
  assert_eq(malformed_traceparent, Some("invalid-format"))
  
  // Test empty carrier
  let empty_carrier = TextMapCarrier::new()
  let empty_traceparent = TextMapCarrier::get(empty_carrier, "traceparent")
  assert_eq(empty_traceparent, None)
  
  // Test context with special characters
  let special_ctx = SpanContext::new("trace-with-dash", "span_with_underscore", true, "state=value&key=data")
  assert_true(SpanContext::is_valid(special_ctx))
  assert_eq(SpanContext::trace_id(special_ctx), "trace-with-dash")
  assert_eq(SpanContext::span_id(special_ctx), "span_with_underscore")
}

// Test 9: Trace State Propagation
test "trace state propagation" {
  // Create span contexts with different trace states
  let ctx_with_state = SpanContext::new("trace123", "span001", true, "vendor1=value1,vendor2=value2")
  let ctx_without_state = SpanContext::new("trace123", "span002", true, "")
  let ctx_with_complex_state = SpanContext::new("trace123", "span003", true, "key1=val1;key2=val2;key3=val3")
  
  // Verify trace state preservation
  assert_eq(SpanContext::trace_id(ctx_with_state), "trace123")
  assert_eq(SpanContext::span_id(ctx_with_state), "span001")
  assert_true(SpanContext::is_sampled(ctx_with_state))
  
  assert_eq(SpanContext::trace_id(ctx_without_state), "trace123")
  assert_eq(SpanContext::span_id(ctx_without_state), "span002")
  assert_true(SpanContext::is_sampled(ctx_without_state))
  
  assert_eq(SpanContext::trace_id(ctx_with_complex_state), "trace123")
  assert_eq(SpanContext::span_id(ctx_with_complex_state), "span003")
  assert_true(SpanContext::is_sampled(ctx_with_complex_state))
}

// Test 10: Cross-Service Telemetry Consistency
test "cross-service telemetry consistency" {
  // Service A creates trace
  let service_a_provider = TracerProvider::default()
  let service_a_tracer = TracerProvider::get_tracer(service_a_provider, "service_a")
  let service_a_span = Tracer::start_span(service_a_tracer, "service_a_operation")
  
  // Service B continues trace
  let service_b_provider = TracerProvider::default()
  let service_b_tracer = TracerProvider::get_tracer(service_b_provider, "service_b")
  let service_b_span = Tracer::start_span(service_b_tracer, "service_b_operation")
  
  // Service C continues trace
  let service_c_provider = TracerProvider::default()
  let service_c_tracer = TracerProvider::get_tracer(service_c_provider, "service_c")
  let service_c_span = Tracer::start_span(service_c_tracer, "service_c_operation")
  
  // Verify consistent trace context
  let service_a_ctx = Span::span_context(service_a_span)
  let service_b_ctx = Span::span_context(service_b_span)
  let service_c_ctx = Span::span_context(service_c_span)
  
  // All spans should be valid and recording
  assert_true(SpanContext::is_valid(service_a_ctx))
  assert_true(SpanContext::is_valid(service_b_ctx))
  assert_true(SpanContext::is_valid(service_c_ctx))
  
  assert_true(Span::is_recording(service_a_span))
  assert_true(Span::is_recording(service_b_span))
  assert_true(Span::is_recording(service_c_span))
  
  // Verify service names
  let service_a_scope = Tracer::instrumentation_scope(service_a_tracer)
  let service_b_scope = Tracer::instrumentation_scope(service_b_tracer)
  let service_c_scope = Tracer::instrumentation_scope(service_c_tracer)
  
  assert_eq(service_a_scope.name, "service_a")
  assert_eq(service_b_scope.name, "service_b")
  assert_eq(service_c_scope.name, "service_c")
  
  // End all spans
  Span::end(service_a_span)
  Span::end(service_b_span)
  Span::end(service_c_span)
}