// Azimuth Telemetry System - Distributed Tracing Tests
// This file contains test cases for distributed tracing functionality

// Test 1: Trace Context Propagation
test "trace context propagation" {
  // Create initial trace context
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_id = "00f067aa0ba902b7"
  let parent_ctx = TraceContext::new(trace_id, span_id, true)
  
  // Create child span
  let child_span = TraceContext::create_child_span(parent_ctx, "child_operation")
  let child_ctx = Span::context(child_span)
  
  // Verify trace ID is preserved
  assert_eq(TraceContext::trace_id(child_ctx), trace_id)
  
  // Verify span ID is different
  assert_not_eq(TraceContext::span_id(child_ctx), span_id)
  
  // Verify parent relationship
  match TraceContext::parent_span_id(child_ctx) {
    Some(parent_id) => assert_eq(parent_id, span_id)
    None => assert_true(false)
  }
  
  // Create grandchild span
  let grandchild_span = TraceContext::create_child_span(child_span, "grandchild_operation")
  let grandchild_ctx = Span::context(grandchild_span)
  
  // Verify trace ID is still preserved
  assert_eq(TraceContext::trace_id(grandchild_ctx), trace_id)
  
  // Verify parent chain
  match TraceContext::parent_span_id(grandchild_ctx) {
    Some(parent_id) => assert_eq(parent_id, TraceContext::span_id(child_ctx))
    None => assert_true(false)
  }
}

// Test 2: Cross-Service Trace Context Propagation
test "cross-service trace context propagation" {
  // Create trace context in service A
  let trace_id = "a1b2c3d4e5f678901234567890123456"
  let span_id = "1234567890abcdef"
  let service_a_ctx = TraceContext::new(trace_id, span_id, true)
  
  // Serialize context for cross-service propagation
  let serialized_ctx = TraceContext::serialize(service_a_ctx)
  assert_true(serialized_ctx.length() > 0)
  
  // Deserialize context in service B
  let service_b_ctx = TraceContext::deserialize(serialized_ctx)
  match service_b_ctx {
    Some(ctx) => {
      assert_eq(TraceContext::trace_id(ctx), trace_id)
      assert_eq(TraceContext::span_id(ctx), span_id)
      assert_true(TraceContext::is_sampled(ctx))
    }
    None => assert_true(false)
  }
  
  // Create span in service B
  match service_b_ctx {
    Some(ctx) => {
      let service_b_span = TraceContext::create_child_span_with_context(ctx, "service_b_operation")
      let service_b_final_ctx = Span::context(service_b_span)
      
      // Verify trace context is maintained across services
      assert_eq(TraceContext::trace_id(service_b_final_ctx), trace_id)
      assert_not_eq(TraceContext::span_id(service_b_final_ctx), span_id)
    }
    None => assert_true(false)
  }
}

// Test 3: Trace Sampling Strategy
test "trace sampling strategy" {
  // Create sampler with 50% sampling rate
  let sampler = TraceSampler::new(0.5)
  
  let trace_id_1 = "11111111111111111111111111111111"
  let trace_id_2 = "22222222222222222222222222222222"
  let trace_id_3 = "33333333333333333333333333333333"
  let trace_id_4 = "44444444444444444444444444444444"
  
  // Test sampling decisions
  let decision_1 = TraceSampler::should_sample(sampler, trace_id_1)
  let decision_2 = TraceSampler::should_sample(sampler, trace_id_2)
  let decision_3 = TraceSampler::should_sample(sampler, trace_id_3)
  let decision_4 = TraceSampler::should_sample(sampler, trace_id_4)
  
  // With 50% sampling, we should have a mix of true/false
  let true_count = [decision_1, decision_2, decision_3, decision_4]
    .filter(d => d)
    .length()
  
  assert_true(true_count >= 1 && true_count <= 3)
  
  // Test always-on sampler
  let always_on_sampler = TraceSampler::always_on()
  let always_on_decision = TraceSampler::should_sample(always_on_sampler, trace_id_1)
  assert_true(always_on_decision)
  
  // Test always-off sampler
  let always_off_sampler = TraceSampler::always_off()
  let always_off_decision = TraceSampler::should_sample(always_off_sampler, trace_id_1)
  assert_false(always_off_decision)
}

// Test 4: Trace Span Events and Attributes
test "trace span events and attributes" {
  let trace_id = "abcdef1234567890abcdef1234567890"
  let span_id = "1234567890abcdef"
  let span_ctx = TraceContext::new(trace_id, span_id, true)
  let span = TraceContext::create_root_span(span_ctx, "test_operation")
  
  // Add attributes to span
  Span::set_attribute(span, "user.id", IntValue(42))
  Span::set_attribute(span, "user.name", StringValue("test_user"))
  Span::set_attribute(span, "operation.type", StringValue("read"))
  
  // Verify attributes
  match Span::get_attribute(span, "user.id") {
    Some(IntValue(id)) => assert_eq(id, 42)
    _ => assert_true(false)
  }
  
  match Span::get_attribute(span, "user.name") {
    Some(StringValue(name)) => assert_eq(name, "test_user")
    _ => assert_true(false)
  }
  
  // Add events to span
  Span::add_event(span, "operation_started", Some(1234567890L))
  Span::add_event_with_attributes(span, "operation_completed", Some(1234567950L), [
    ("result", StringValue("success")),
    ("duration_ms", IntValue(60))
  ])
  
  // Verify events
  let events = Span::events(span)
  assert_eq(events.length(), 2)
  
  match events[0] {
    SpanEvent { name: "operation_started", timestamp: Some(ts), attributes: attrs } => {
      assert_eq(ts, 1234567890L)
      assert_eq(attrs.length(), 0)
    }
    _ => assert_true(false)
  }
  
  match events[1] {
    SpanEvent { name: "operation_completed", timestamp: Some(ts), attributes: attrs } => {
      assert_eq(ts, 1234567950L)
      assert_eq(attrs.length(), 2)
    }
    _ => assert_true(false)
  }
  
  // End span
  Span::end(span)
  
  // Verify span is ended
  assert_false(Span::is_recording(span))
}