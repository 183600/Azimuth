// Azimuth Distributed Tracing Consistency High-Quality Tests
// This file contains comprehensive test cases for distributed tracing consistency across services

// Test 1: Cross-Service Trace Context Propagation
test "cross-service trace context propagation" {
  // Initialize trace context with proper format
  let trace_id = "5d81832f6c1744b79b67e3e3a6996298"
  let parent_span_id = "2e5e0a5a7c9a4b5d"
  let trace_flags = "01"
  let trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  
  // Create initial span context
  let initial_context = TraceContext::new(
    trace_id, 
    parent_span_id, 
    trace_flags, 
    trace_state, 
    true
  )
  
  // Simulate service A creating a child span
  let service_a_span = TraceContext::create_child(
    initial_context, 
    "service-a-operation",
    "service-a"
  )
  
  // Verify child span properties
  assert_eq(TraceContext::trace_id(service_a_span), trace_id)
  assert_not_eq(TraceContext::span_id(service_a_span), parent_span_id)
  assert_eq(TraceContext::parent_span_id(service_a_span), parent_span_id)
  
  // Simulate context propagation to service B
  let propagated_context = TraceContext::extract_from_carrier(
    service_a_span,
    TextMapCarrier::new([
      ("traceparent", "00-" + trace_id + "-" + TraceContext::span_id(service_a_span) + "-" + trace_flags),
      ("tracestate", trace_state)
    ])
  )
  
  match propagated_context {
    Some(ctx) => {
      assert_eq(TraceContext::trace_id(ctx), trace_id)
      assert_eq(TraceContext::parent_span_id(ctx), TraceContext::span_id(service_a_span))
    }
    None => assert_true(false)
  }
  
  // Simulate service B creating another child span
  let service_b_span = TraceContext::create_child(
    propagated_context.unwrap(),
    "service-b-operation", 
    "service-b"
  )
  
  // Verify trace consistency across services
  assert_eq(TraceContext::trace_id(service_b_span), trace_id)
  assert_eq(TraceContext::parent_span_id(service_b_span), TraceContext::span_id(service_a_span))
}

// Test 2: Trace Sampling Consistency
test "trace sampling consistency" {
  // Test parent-based sampling
  let parent_sampler = ParentBasedSampler::new(
    AlwaysOnSampler::new(),
    AlwaysOffSampler::new(),
    TraceIdRatioBasedSampler::new(0.5)
  )
  
  // Create parent span with sampling decision
  let sampled_parent = TraceContext::with_sampling(
    "5d81832f6c1744b79b67e3e3a6996298",
    "2e5e0a5a7c9a4b5d",
    true
  )
  
  let not_sampled_parent = TraceContext::with_sampling(
    "5d81832f6c1744b79b67e3e3a6996299",
    "2e5e0a5a7c9a4b5e",
    false
  )
  
  // Test child inherits sampling from parent
  let sampled_child = TraceContext::create_child(
    sampled_parent,
    "child-operation",
    "child-service"
  )
  
  let not_sampled_child = TraceContext::create_child(
    not_sampled_parent,
    "child-operation",
    "child-service"
  )
  
  assert_true(TraceContext::is_sampled(sampled_child))
  assert_false(TraceContext::is_sampled(not_sampled_child))
  
  // Test root span sampling with ratio-based sampler
  let root_spans = []
  let sampled_count = 0
  
  for i in 0..=1000 {
    let trace_id = generate_trace_id(i)
    let root_span = TraceContext::new_root(trace_id, "root-operation", "root-service")
    let sampling_decision = TraceIdRatioBasedSampler::should_sample(
      TraceIdRatioBasedSampler::new(0.1),
      trace_id
    )
    
    if sampling_decision {
      sampled_count = sampled_count + 1
    }
    
    root_spans.push((root_span, sampling_decision))
  }
  
  // Verify sampling ratio is approximately correct (within 5% margin)
  let actual_ratio = sampled_count.to_float() / 1000.0
  assert_true(actual_ratio > 0.05 && actual_ratio < 0.15)
}

// Test 3: Trace State Consistency
test "trace state consistency" {
  let initial_trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  
  // Create span with initial trace state
  let span1 = TraceContext::with_trace_state(
    "5d81832f6c1744b79b67e3e3a6996298",
    "2e5e0a5a7c9a4b5d",
    initial_trace_state
  )
  
  // Add new entry to trace state
  let updated_span = TraceContext::add_trace_state_entry(
    span1,
    "vendor",
    "value123"
  )
  
  // Verify trace state is updated correctly
  let updated_state = TraceContext::trace_state(updated_span)
  assert_true(updated_state.contains("rojo=00f067aa0ba902b7"))
  assert_true(updated_state.contains("congo=t61rcWkgMzE"))
  assert_true(updated_state.contains("vendor=value123"))
  
  // Create child span and verify trace state inheritance
  let child_span = TraceContext::create_child(
    updated_span,
    "child-operation",
    "child-service"
  )
  
  let child_state = TraceContext::trace_state(child_span)
  assert_eq(child_state, updated_state)
  
  // Test trace state limits and truncation
  let large_state_span = span1
  let mut final_span = large_state_span
  
  // Add many entries to test limit handling
  for i in 0..=50 {
    let key = "key" + i.to_string()
    let value = "value" + i.to_string()
    final_span = TraceContext::add_trace_state_entry(final_span, key, value)
  }
  
  // Verify trace state doesn't exceed reasonable limits
  let final_state = TraceContext::trace_state(final_span)
  assert_true(final_state.length() < 4096) // Common trace state limit
}

// Test 4: Baggage Consistency Across Services
test "baggage consistency across services" {
  let initial_baggage = Baggage::new()
  
  // Add baggage entries
  let baggage1 = Baggage::set_entry(
    initial_baggage,
    "user.id",
    "12345",
    Some([("propagation", "default")])
  )
  
  let baggage2 = Baggage::set_entry(
    baggage1,
    "request.id",
    "req-67890",
    Some([("propagation", "default")])
  )
  
  // Create span with baggage
  let span_with_baggage = TraceContext::with_baggage(
    "5d81832f6c1744b79b67e3e3a6996298",
    "2e5e0a5a7c9a4b5d",
    baggage2
  )
  
  // Serialize baggage for propagation
  let baggage_header = Baggage::serialize(baggage2)
  
  // Simulate cross-service baggage propagation
  let propagated_baggage = Baggage::deserialize(baggage_header)
  
  // Verify baggage consistency
  let user_id = Baggage::get_entry(propagated_baggage, "user.id")
  match user_id {
    Some(entry) => {
      assert_eq(entry.value, "12345")
      assert_true(entry.metadata.contains("propagation=default"))
    }
    None => assert_true(false)
  }
  
  let request_id = Baggage::get_entry(propagated_baggage, "request.id")
  match request_id {
    Some(entry) => {
      assert_eq(entry.value, "req-67890")
      assert_true(entry.metadata.contains("propagation=default"))
    }
    None => assert_true(false)
  }
  
  // Test baggage inheritance in child spans
  let child_span = TraceContext::create_child(
    span_with_baggage,
    "child-operation",
    "child-service"
  )
  
  let child_baggage = TraceContext::baggage(child_span)
  let child_user_id = Baggage::get_entry(child_baggage, "user.id")
  match child_user_id {
    Some(entry) => assert_eq(entry.value, "12345")
    None => assert_true(false)
  }
}

// Test 5: Span Link Consistency
test "span link consistency" {
  // Create parent spans
  let parent1 = TraceContext::new_root(
    "5d81832f6c1744b79b67e3e3a6996298",
    "parent-operation-1",
    "parent-service-1"
  )
  
  let parent2 = TraceContext::new_root(
    "5d81832f6c1744b79b67e3e3a6996299",
    "parent-operation-2", 
    "parent-service-2"
  )
  
  // Create span with links to both parents
  let linked_span = TraceContext::with_links(
    "5d81832f6c1744b79b67e3e3a6996297",
    "linked-operation",
    "linked-service",
    [
      SpanLink::new(
        TraceContext::span_context(parent1),
        Some("linked-to-parent-1"),
        Some(Attributes::from([("link.type", "causal")]))
      ),
      SpanLink::new(
        TraceContext::span_context(parent2),
        Some("linked-to-parent-2"),
        Some(Attributes::from([("link.type", "related")]))
      )
    ]
  )
  
  // Verify link consistency
  let links = TraceContext::links(linked_span)
  assert_eq(links.length(), 2)
  
  let link1 = links[0]
  assert_eq(SpanLink::trace_id(link1), TraceContext::trace_id(parent1))
  assert_eq(SpanLink::span_id(link1), TraceContext::span_id(parent1))
  match SpanLink::attributes(link1) {
    Some(attrs) => {
      let link_type = Attributes::get(attrs, "link.type")
      match link_type {
        Some(StringValue(value)) => assert_eq(value, "causal")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let link2 = links[1]
  assert_eq(SpanLink::trace_id(link2), TraceContext::trace_id(parent2))
  assert_eq(SpanLink::span_id(link2), TraceContext::span_id(parent2))
}

// Test 6: Trace Consistency Under High Concurrency
test "trace consistency under high concurrency" {
  let concurrent_spans = []
  let trace_id = "5d81832f6c1744b79b67e3e3a6996298"
  
  // Create many concurrent spans with the same trace
  for i in 0..=100 {
    let span = TraceContext::create_child(
      TraceContext::new_root(trace_id, "root-operation", "root-service"),
      "concurrent-operation-" + i.to_string(),
      "concurrent-service-" + (i % 10).to_string()
    )
    concurrent_spans.push(span)
  }
  
  // Verify all spans have the same trace ID
  for span in concurrent_spans {
    assert_eq(TraceContext::trace_id(span), trace_id)
  }
  
  // Verify parent-child relationships are consistent
  let root_span = concurrent_spans[0]
  let root_span_id = TraceContext::span_id(root_span)
  
  for i in 1..=concurrent_spans.length() - 1 {
    let span = concurrent_spans[i]
    assert_eq(TraceContext::parent_span_id(span), root_span_id)
    assert_not_eq(TraceContext::span_id(span), root_span_id)
  }
  
  // Test span ordering and timestamps
  let timestamped_spans = []
  for span in concurrent_spans {
    timestamped_spans.push((
      TraceContext::span_id(span),
      TraceContext::start_timestamp(span)
    ))
  }
  
  // Sort by timestamp
  let sorted_spans = sort_by_timestamp(timestamped_spans)
  
  // Verify chronological ordering
  for i in 1..=sorted_spans.length() - 1 {
    let current_time = sorted_spans[i].1
    let previous_time = sorted_spans[i-1].1
    assert_true(current_time >= previous_time)
  }
}

// Test 7: Trace Context Format Consistency
test "trace context format consistency" {
  let trace_id = "5d81832f6c1744b79b67e3e3a6996298"
  let span_id = "2e5e0a5a7c9a4b5d"
  let trace_flags = "01"
  
  // Test W3C traceparent format
  let traceparent = "00-" + trace_id + "-" + span_id + "-" + trace_flags
  
  // Extract context from traceparent
  let extracted_context = TraceContext::extract_from_traceparent(traceparent)
  
  match extracted_context {
    Some(ctx) => {
      assert_eq(TraceContext::trace_id(ctx), trace_id)
      assert_eq(TraceContext::span_id(ctx), span_id)
      assert_eq(TraceContext::trace_flags(ctx), trace_flags)
    }
    None => assert_true(false)
  }
  
  // Test inject/extract roundtrip
  let original_context = TraceContext::new(
    trace_id,
    span_id,
    trace_flags,
    "",
    true
  )
  
  let carrier = TextMapCarrier::new([])
  let injected_carrier = TraceContext::inject_to_carrier(original_context, carrier)
  
  let roundtrip_context = TraceContext::extract_from_carrier(
    injected_carrier,
    TextMapCarrier::new(injected_carrier.headers)
  )
  
  match roundtrip_context {
    Some(ctx) => {
      assert_eq(TraceContext::trace_id(ctx), trace_id)
      assert_eq(TraceContext::span_id(ctx), span_id)
      assert_eq(TraceContext::trace_flags(ctx), trace_flags)
    }
    None => assert_true(false)
  }
}

// Test 8: Trace Consistency Recovery
test "trace consistency recovery" {
  // Simulate partial trace context corruption
  let partial_traceparent = "00-5d81832f6c1744b79b67e3e3a6996298-" // Missing span_id and flags
  
  // Test recovery strategies
  let recovered_context = TraceContext::recover_from_partial(partial_traceparent)
  
  match recovered_context {
    Some(ctx) => {
      assert_eq(TraceContext::trace_id(ctx), "5d81832f6c1744b79b67e3e3a6996298")
      assert_true(TraceContext::span_id(ctx).length() > 0) // Should generate new span ID
      assert_eq(TraceContext::trace_flags(ctx), "01") // Default flags
    }
    None => assert_true(false)
  }
  
  // Test recovery with missing trace state
  let context_without_state = TraceContext::new(
    "5d81832f6c1744b79b67e3e3a6996298",
    "2e5e0a5a7c9a4b5d",
    "01",
    "", // Empty trace state
    true
  )
  
  let recovered_with_state = TraceContext::recover_trace_state(
    context_without_state,
    ["rojo=00f067aa0ba902b7", "congo=t61rcWkgMzE"]
  )
  
  let recovered_state = TraceContext::trace_state(recovered_with_state)
  assert_true(recovered_state.contains("rojo=00f067aa0ba902b7"))
  assert_true(recovered_state.contains("congo=t61rcWkgMzE"))
}

// Test 9: Cross-Protocol Trace Consistency
test "cross-protocol trace consistency" {
  let trace_id = "5d81832f6c1744b79b67e3e3a6996298"
  let span_id = "2e5e0a5a7c9a4b5d"
  
  // Create context for HTTP protocol
  let http_context = TraceContext::new(
    trace_id,
    span_id,
    "01",
    "http=true",
    true
  )
  
  // Convert to gRPC metadata format
  let grpc_metadata = TraceContext::to_grpc_metadata(http_context)
  
  // Extract from gRPC metadata
  let grpc_context = TraceContext::from_grpc_metadata(grpc_metadata)
  
  match grpc_context {
    Some(ctx) => {
      assert_eq(TraceContext::trace_id(ctx), trace_id)
      assert_eq(TraceContext::span_id(ctx), span_id)
    }
    None => assert_true(false)
  }
  
  // Convert to message queue format
  let mq_headers = TraceContext::to_mq_headers(http_context)
  
  // Extract from message queue headers
  let mq_context = TraceContext::from_mq_headers(mq_headers)
  
  match mq_context {
    Some(ctx) => {
      assert_eq(TraceContext::trace_id(ctx), trace_id)
      assert_eq(TraceContext::span_id(ctx), span_id)
    }
    None => assert_true(false)
  }
}

// Test 10: Trace Consistency Validation
test "trace consistency validation" {
  let validator = TraceConsistencyValidator::new()
  
  // Create a consistent trace
  let root_span = TraceContext::new_root(
    "5d81832f6c1744b79b67e3e3a6996298",
    "root-operation",
    "root-service"
  )
  
  let child_span1 = TraceContext::create_child(
    root_span,
    "child-operation-1",
    "child-service-1"
  )
  
  let child_span2 = TraceContext::create_child(
    root_span,
    "child-operation-2",
    "child-service-2"
  )
  
  let grandchild_span = TraceContext::create_child(
    child_span1,
    "grandchild-operation",
    "grandchild-service"
  )
  
  let trace = Trace::from_spans([root_span, child_span1, child_span2, grandchild_span])
  
  // Validate trace consistency
  let validation_result = TraceConsistencyValidator::validate(validator, trace)
  
  assert_true(validation_result.is_consistent)
  assert_eq(validation_result.errors.length(), 0)
  
  // Test with inconsistent trace
  let inconsistent_span = TraceContext::new(
    "different-trace-id", // Different trace ID
    "inconsistent-span-id",
    "01",
    "",
    true
  )
  
  let inconsistent_trace = Trace::from_spans([root_span, child_span1, inconsistent_span])
  let inconsistent_validation = TraceConsistencyValidator::validate(validator, inconsistent_trace)
  
  assert_false(inconsistent_validation.is_consistent)
  assert_true(inconsistent_validation.errors.length() > 0)
  
  // Verify specific error types
  let has_trace_id_error = inconsistent_validation.errors.any(|error| {
    match error {
      TraceConsistencyError::TraceIdMismatch => true
      _ => false
    }
  })
  assert_true(has_trace_id_error)
}

// Helper functions
fn generate_trace_id(seed : Int) -> String {
  let hex_chars = "0123456789abcdef"
  let mut result = ""
  
  for i in 0..=31 {
    let index = (seed + i) % 16
    result = result + hex_chars[index:index+1]
  }
  
  result
}

fn sort_by_timestamp(spans : Array[(String, Int64)]) -> Array[(String, Int64)] {
  // Simple bubble sort implementation for demonstration
  let sorted = spans.copy()
  let n = sorted.length()
  
  for i in 0..=n-1 {
    for j in 0..=n-i-2 {
      if sorted[j].1 > sorted[j+1].1 {
        let temp = sorted[j]
        sorted[j] = sorted[j+1]
        sorted[j+1] = temp
      }
    }
  }
  
  sorted
}