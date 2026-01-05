// Azimuth Telemetry System - Distributed Tracing Consistency Tests
// This file contains test cases for distributed tracing consistency functionality

// Test 1: Trace Context Propagation
test "trace context propagation" {
  // Create a root span
  let trace_id = "12345678901234567890123456789012"
  let root_span_id = "1111111111111111"
  let root_span = Span::new("root_operation", Server, SpanContext::new(trace_id, root_span_id, true, ""))
  
  // Create a child span
  let child_span_id = "2222222222222222"
  let child_span = Span::create_child(root_span, "child_operation", Client)
  
  // Verify trace context is propagated
  let child_context = Span::span_context(child_span)
  assert_eq(SpanContext::trace_id(child_context), trace_id)
  assert_eq(SpanContext::parent_span_id(child_context), Some(root_span_id))
  assert_eq(SpanContext::span_id(child_context), child_span_id)
  
  // Create a grandchild span
  let grandchild_span_id = "3333333333333333"
  let grandchild_span = Span::create_child(child_span, "grandchild_operation", Internal)
  
  // Verify trace context is propagated through multiple levels
  let grandchild_context = Span::span_context(grandchild_span)
  assert_eq(SpanContext::trace_id(grandchild_context), trace_id)
  assert_eq(SpanContext::parent_span_id(grandchild_context), Some(child_span_id))
  assert_eq(SpanContext::span_id(grandchild_context), grandchild_span_id)
  
  // Test trace context extraction and injection
  let injector = TraceContextInjector::new()
  let headers = Headers::new()
  TraceContextInjector::inject(injector, child_context, headers)
  
  // Verify trace context headers are injected
  assert_true(Headers::has(headers, "traceparent"))
  assert_true(Headers::has(headers, "tracestate"))
  
  // Extract trace context from headers
  let extractor = TraceContextExtractor::new()
  let extracted_context = TraceContextExtractor::extract(extractor, headers)
  
  match extracted_context {
    Some(context) => {
      assert_eq(SpanContext::trace_id(context), trace_id)
      assert_eq(SpanContext::span_id(context), child_span_id)
    }
    None => assert_true(false)
  }
}

// Test 2: Cross-Service Trace Consistency
test "cross-service trace consistency" {
  // Create a trace coordinator
  let coordinator = TraceCoordinator::new()
  
  // Register services
  TraceCoordinator::register_service(coordinator, "service-a", "http://service-a:8080")
  TraceCoordinator::register_service(coordinator, "service-b", "http://service-b:8080")
  TraceCoordinator::register_service(coordinator, "service-c", "http://service-c:8080")
  
  // Create a distributed trace
  let trace_id = TraceCoordinator::create_trace(coordinator)
  let trace_context = TraceContext::new(trace_id, "service-a")
  
  // Service A creates a root span
  let service_a_span = TraceCoordinator::create_span(coordinator, trace_context, "service-a-operation", Server)
  let service_a_context = Span::span_context(service_a_span)
  
  // Service A calls Service B
  let service_b_context = TraceCoordinator::propagate_to_service(coordinator, service_a_context, "service-b")
  let service_b_span = TraceCoordinator::create_span(coordinator, service_b_context, "service-b-operation", Server)
  
  // Service B calls Service C
  let service_c_context = TraceCoordinator::propagate_to_service(coordinator, Span::span_context(service_b_span), "service-c")
  let service_c_span = TraceCoordinator::create_span(coordinator, service_c_context, "service-c-operation", Server)
  
  // End all spans
  Span::end(service_a_span)
  Span::end(service_b_span)
  Span::end(service_c_span)
  
  // Collect the trace
  let collected_trace = TraceCoordinator::collect_trace(coordinator, trace_id)
  
  // Verify trace consistency
  assert_eq(collected_trace.trace_id, trace_id)
  assert_eq(collected_trace.spans.length(), 3)
  
  // Verify parent-child relationships
  let service_a_collected = collected_trace.spans[0]
  let service_b_collected = collected_trace.spans[1]
  let service_c_collected = collected_trace.spans[2]
  
  assert_eq(service_a_collected.parent_span_id, None)
  assert_eq(service_b_collected.parent_span_id, Some(service_a_collected.span_id))
  assert_eq(service_c_collected.parent_span_id, Some(service_b_collected.span_id))
  
  // Verify service names are correctly recorded
  assert_eq(service_a_collected.service_name, "service-a")
  assert_eq(service_b_collected.service_name, "service-b")
  assert_eq(service_c_collected.service_name, "service-c")
}

// Test 3: Trace Sampling Consistency
test "trace sampling consistency" {
  // Create a sampler
  let sampler = TraceSampler::new(0.1) // 10% sampling rate
  
  // Create multiple traces
  let traces = []
  for i in 0..=99 {
    let trace_id = "trace_" + i.to_string()
    let trace_context = TraceContext::new(trace_id, "test-service")
    let is_sampled = TraceSampler::should_sample(sampler, trace_context)
    
    traces.push({
      "trace_id": trace_id,
      "is_sampled": is_sampled
    })
  }
  
  // Verify approximately 10% of traces are sampled
  let sampled_count = traces.filter(fn(t) { t.is_sampled }).length()
  assert_true(sampled_count >= 5 && sampled_count <= 15) // Allow some variance
  
  // Test consistent sampling for the same trace
  let trace_id = "consistency_test_trace"
  let trace_context = TraceContext::new(trace_id, "test-service")
  
  let first_sample = TraceSampler::should_sample(sampler, trace_context)
  let second_sample = TraceSampler::should_sample(sampler, trace_context)
  let third_sample = TraceSampler::should_sample(sampler, trace_context)
  
  // All decisions should be the same for the same trace
  assert_eq(first_sample, second_sample)
  assert_eq(second_sample, third_sample)
  
  // Test parent-based sampling
  let parent_sampler = ParentBasedSampler::new(sampler)
  
  // Create a parent span that is sampled
  let parent_context = TraceContext::with_sampling("parent_trace", "parent-service", true)
  let child_context = TraceContext::with_parent("child_trace", "child-service", parent_context)
  
  // Child should be sampled if parent is sampled
  let child_sampled = ParentBasedSampler::should_sample(parent_sampler, child_context)
  assert_true(child_sampled)
  
  // Create a parent span that is not sampled
  let unsampled_parent_context = TraceContext::with_sampling("unsampled_parent_trace", "parent-service", false)
  let unsampled_child_context = TraceContext::with_parent("unsampled_child_trace", "child-service", unsampled_parent_context)
  
  // Child should not be sampled if parent is not sampled
  let unsampled_child_sampled = ParentBasedSampler::should_sample(parent_sampler, unsampled_child_context)
  assert_false(unsampled_child_sampled)
}

// Test 4: Trace State Propagation
test "trace state propagation" {
  // Create a trace context with state
  let trace_id = "trace_state_test"
  let trace_state = "vendor1=foo,vendor2=bar"
  let trace_context = TraceContext::with_state(trace_id, "service-a", trace_state)
  
  // Create a span
  let span = Span::new("operation", Server, trace_context)
  let span_context = Span::span_context(span)
  
  // Verify trace state is preserved
  assert_eq(SpanContext::trace_state(span_context), trace_state)
  
  // Add to trace state
  let updated_context = SpanContext::add_trace_state_item(span_context, "vendor3", "baz")
  let updated_trace_state = SpanContext::trace_state(updated_context)
  
  // Verify the new item is added
  assert_true(updated_trace_state.includes("vendor3=baz"))
  
  // Propagate to child span
  let child_span = Span::create_child(span, "child_operation", Client)
  let child_context = Span::span_context(child_span)
  
  // Verify trace state is propagated to child
  let child_trace_state = SpanContext::trace_state(child_context)
  assert_true(child_trace_state.includes("vendor1=foo"))
  assert_true(child_trace_state.includes("vendor2=bar"))
  assert_true(child_trace_state.includes("vendor3=baz"))
  
  // Modify trace state in child
  let child_updated_context = SpanContext::add_trace_state_item(child_context, "vendor4", "qux")
  let child_updated_trace_state = SpanContext::trace_state(child_updated_context)
  
  // Verify the new item is added to child
  assert_true(child_updated_trace_state.includes("vendor4=qux"))
  
  // Verify parent is not affected
  let parent_trace_state = SpanContext::trace_state(span_context)
  assert_false(parent_trace_state.includes("vendor4=qux"))
  
  // Test trace state parsing and serialization
  let trace_state_map = TraceState::parse(updated_trace_state)
  assert_eq(TraceState::get(trace_state_map, "vendor1"), Some("foo"))
  assert_eq(TraceState::get(trace_state_map, "vendor2"), Some("bar"))
  assert_eq(TraceState::get(trace_state_map, "vendor3"), Some("baz"))
  
  let serialized_state = TraceState::serialize(trace_state_map)
  assert_true(serialized_state.includes("vendor1=foo"))
  assert_true(serialized_state.includes("vendor2=bar"))
  assert_true(serialized_state.includes("vendor3=baz"))
}

// Test 5: Trace Consistency Validation
test "trace consistency validation" {
  // Create a trace validator
  let validator = TraceValidator::new()
  
  // Create a consistent trace
  let consistent_trace = create_consistent_sample_trace()
  
  // Validate the consistent trace
  let validation_result = TraceValidator::validate(validator, consistent_trace)
  
  // Should pass validation
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // Create an inconsistent trace (break parent-child relationship)
  let inconsistent_trace = create_inconsistent_sample_trace()
  
  // Validate the inconsistent trace
  let inconsistent_validation = TraceValidator::validate(validator, inconsistent_trace)
  
  // Should fail validation
  assert_false(inconsistent_validation.is_valid)
  assert_true(inconsistent_validation.errors.length() > 0)
  
  // Verify specific error
  let has_parent_error = inconsistent_validation.errors.any(fn(error) {
    error.code == "invalid_parent_child_relationship"
  })
  assert_true(has_parent_error)
  
  // Create a trace with missing spans
  let incomplete_trace = create_incomplete_sample_trace()
  
  // Validate the incomplete trace
  let incomplete_validation = TraceValidator::validate(validator, incomplete_trace)
  
  // Should have warnings but may still be valid
  assert_true(incomplete_validation.warnings.length() > 0)
  
  // Verify specific warning
  let has_missing_warning = incomplete_validation.warnings.any(fn(warning) {
    warning.code == "missing_intermediate_spans"
  })
  assert_true(has_missing_warning)
}

// Test 6: Distributed Clock Synchronization
test "distributed clock synchronization" {
  // Create a clock synchronizer
  let synchronizer = ClockSynchronizer::new()
  
  // Register clocks from different services
  let service_a_clock = Clock::new("service-a", 1609459200000L) // 2021-01-01 00:00:00 UTC
  let service_b_clock = Clock::new("service-b", 1609459200100L) // 100ms ahead
  let service_c_clock = Clock::new("service-c", 1609459199900L) // 100ms behind
  
  ClockSynchronizer::register_clock(synchronizer, service_a_clock)
  ClockSynchronizer::register_clock(synchronizer, service_b_clock)
  ClockSynchronizer::register_clock(synchronizer, service_c_clock)
  
  // Synchronize clocks
  let sync_result = ClockSynchronizer::synchronize(synchronizer)
  assert_true(sync_result.is_ok())
  
  // Verify clocks are synchronized
  let sync_time = ClockSynchronizer::get_sync_time(synchronizer)
  
  let service_a_offset = ClockSynchronizer::get_offset(synchronizer, "service-a")
  let service_b_offset = ClockSynchronizer::get_offset(synchronizer, "service-b")
  let service_c_offset = ClockSynchronizer::get_offset(synchronizer, "service-c")
  
  // Offsets should be calculated correctly
  assert_eq(service_a_offset, 0L) // Reference clock
  assert_eq(service_b_offset, -100L) // Was 100ms ahead, now adjusted
  assert_eq(service_c_offset, 100L)  // Was 100ms behind, now adjusted
  
  // Test timestamp conversion
  let service_a_timestamp = 1609459205000L
  let converted_timestamp = ClockSynchronizer::convert_to_sync_time(synchronizer, "service-a", service_a_timestamp)
  
  // Should be the same since service-a is the reference
  assert_eq(converted_timestamp, service_a_timestamp)
  
  let service_b_timestamp = 1609459205000L
  let converted_b_timestamp = ClockSynchronizer::convert_to_sync_time(synchronizer, "service-b", service_b_timestamp)
  
  // Should be adjusted by the offset
  assert_eq(converted_b_timestamp, service_b_timestamp + service_b_offset)
  
  // Test span timestamp normalization
  let span_a = Span::new("span-a", Server, SpanContext::new("trace", "span-a", true, ""))
  Span::set_timestamp(span_a, ClockSynchronizer::convert_to_sync_time(synchronizer, "service-a", 1609459201000L))
  
  let span_b = Span::new("span-b", Server, SpanContext::new("trace", "span-b", true, ""))
  Span::set_timestamp(span_b, ClockSynchronizer::convert_to_sync_time(synchronizer, "service-b", 1609459202000L))
  
  let span_c = Span::new("span-c", Server, SpanContext::new("trace", "span-c", true, ""))
  Span::set_timestamp(span_c, ClockSynchronizer::convert_to_sync_time(synchronizer, "service-c", 1609459203000L))
  
  // Verify timestamps are in chronological order
  assert_true(Span::timestamp(span_a) < Span::timestamp(span_b))
  assert_true(Span::timestamp(span_b) < Span::timestamp(span_c))
}

// Test 7: Trace Recovery and Repair
test "trace recovery and repair" {
  // Create a trace recoverer
  let recoverer = TraceRecoverer::new()
  
  // Create a damaged trace with missing spans
  let damaged_trace = create_damaged_sample_trace()
  
  // Attempt to recover the trace
  let recovery_result = TraceRecoverer::recover(recoverer, damaged_trace)
  
  // Verify recovery was attempted
  assert_true(recovery_result.attempted)
  
  // Check if recovery was successful
  if recovery_result.successful {
    // Verify the recovered trace is more complete
    assert_true(recovery_result.recovered_trace.spans.length() >= damaged_trace.spans.length())
    
    // Verify the recovered trace passes basic validation
    let validator = TraceValidator::new()
    let validation_result = TraceValidator::validate(validator, recovery_result.recovered_trace)
    assert_true(validation_result.is_valid)
  } else {
    // If recovery failed, verify we have information about why
    assert_true(recovery_result.errors.length() > 0)
  }
  
  // Test partial recovery
  let partially_damaged_trace = create_partially_damaged_sample_trace()
  let partial_recovery = TraceRecoverer::recover(recoverer, partially_damaged_trace)
  
  // Partial recovery should have a higher success rate
  assert_true(partial_recovery.successful || partial_recovery.partially_successful)
  
  // Test trace repair
  let repairable_trace = create_repairable_sample_trace()
  let repair_result = TraceRecoverer::repair(recoverer, repairable_trace)
  
  // Verify repair was attempted
  assert_true(repair_result.attempted)
  
  if repair_result.successful {
    // Verify the repaired trace has correct parent-child relationships
    let validator = TraceValidator::new()
    let validation_result = TraceValidator::validate(validator, repair_result.repaired_trace)
    assert_true(validation_result.is_valid)
    assert_eq(validation_result.errors.length(), 0)
  }
}

// Test 8: Trace Consistency Metrics
test "trace consistency metrics" {
  // Create a metrics collector
  let metrics_collector = TraceConsistencyMetrics::new()
  
  // Simulate processing multiple traces
  for i in 0..=99 {
    let trace_id = "trace_" + i.to_string()
    
    // 90% of traces are consistent
    if i < 90 {
      let consistent_trace = create_consistent_trace_with_id(trace_id)
      TraceConsistencyMetrics::record_processed_trace(metrics_collector, consistent_trace, true)
    } else {
      // 10% are inconsistent
      let inconsistent_trace = create_inconsistent_trace_with_id(trace_id)
      TraceConsistencyMetrics::record_processed_trace(metrics_collector, inconsistent_trace, false)
    }
  }
  
  // Get the metrics
  let metrics = TraceConsistencyMetrics::get_metrics(metrics_collector)
  
  // Verify metrics
  assert_eq(metrics.total_traces_processed, 100)
  assert_eq(metrics.consistent_traces, 90)
  assert_eq(metrics.inconsistent_traces, 10)
  assert_eq(metrics.consistency_rate, 0.9)
  
  // Test detailed error metrics
  let error_metrics = TraceConsistencyMetrics::get_error_metrics(metrics_collector)
  
  // Should have metrics for different error types
  assert_true(error_metrics.contains("invalid_parent_child_relationship"))
  assert_true(error_metrics.contains("missing_span"))
  assert_true(error_metrics.contains("invalid_timestamp"))
  
  // Test service-specific metrics
  let service_metrics = TraceConsistencyMetrics::get_service_metrics(metrics_collector)
  
  // Should have metrics for different services
  assert_true(service_metrics.contains("service-a"))
  assert_true(service_metrics.contains("service-b"))
  assert_true(service_metrics.contains("service-c"))
  
  // Verify service-specific consistency rates
  let service_a_metrics = service_metrics["service-a"]
  assert_true(service_a_metrics.consistency_rate >= 0.8 && service_a_metrics.consistency_rate <= 1.0)
  
  // Test time-based metrics
  let time_series_metrics = TraceConsistencyMetrics::get_time_series_metrics(metrics_collector)
  
  // Should have metrics over time
  assert_true(time_series_metrics.length() > 0)
  
  // Verify metrics are properly timestamped
  for metric in time_series_metrics {
    assert_true(metric.timestamp > 0L)
    assert_true(metric.consistency_rate >= 0.0 && metric.consistency_rate <= 1.0)
  }
}

// Helper functions for creating sample traces
fn create_consistent_sample_trace() {
  let trace = Trace::new("consistent_trace")
  
  let root_span = TraceSpan::new("root_span", "service-a", None)
  let child_span = TraceSpan::new("child_span", "service-b", Some(root_span.span_id))
  let grandchild_span = TraceSpan::new("grandchild_span", "service-c", Some(child_span.span_id))
  
  trace.spans.push(root_span)
  trace.spans.push(child_span)
  trace.spans.push(grandchild_span)
  
  trace
}

fn create_inconsistent_sample_trace() {
  let trace = Trace::new("inconsistent_trace")
  
  let root_span = TraceSpan::new("root_span", "service-a", None)
  let child_span = TraceSpan::new("child_span", "service-b", Some("non_existent_parent"))
  let grandchild_span = TraceSpan::new("grandchild_span", "service-c", Some(child_span.span_id))
  
  trace.spans.push(root_span)
  trace.spans.push(child_span)
  trace.spans.push(grandchild_span)
  
  trace
}

fn create_incomplete_sample_trace() {
  let trace = Trace::new("incomplete_trace")
  
  let root_span = TraceSpan::new("root_span", "service-a", None)
  let grandchild_span = TraceSpan::new("grandchild_span", "service-c", Some("missing_parent"))
  
  trace.spans.push(root_span)
  trace.spans.push(grandchild_span)
  
  trace
}

fn create_damaged_sample_trace() {
  let trace = Trace::new("damaged_trace")
  
  let root_span = TraceSpan::new("root_span", "service-a", None)
  // Missing intermediate spans
  let deep_span = TraceSpan::new("deep_span", "service-d", Some("missing_intermediate_parent"))
  
  trace.spans.push(root_span)
  trace.spans.push(deep_span)
  
  trace
}

fn create_partially_damaged_sample_trace() {
  let trace = Trace::new("partially_damaged_trace")
  
  let root_span = TraceSpan::new("root_span", "service-a", None)
  let child_span = TraceSpan::new("child_span", "service-b", Some(root_span.span_id))
  // Missing one intermediate span
  let grandchild_span = TraceSpan::new("grandchild_span", "service-c", Some("missing_parent"))
  
  trace.spans.push(root_span)
  trace.spans.push(child_span)
  trace.spans.push(grandchild_span)
  
  trace
}

fn create_repairable_sample_trace() {
  let trace = Trace::new("repairable_trace")
  
  let root_span = TraceSpan::new("root_span", "service-a", None)
  let child_span = TraceSpan::new("child_span", "service-b", Some(root_span.span_id))
  // Has incorrect parent reference
  let grandchild_span = TraceSpan::new("grandchild_span", "service-c", Some("wrong_parent"))
  
  trace.spans.push(root_span)
  trace.spans.push(child_span)
  trace.spans.push(grandchild_span)
  
  trace
}

fn create_consistent_trace_with_id(trace_id : String) {
  let trace = Trace::new(trace_id)
  
  let root_span = TraceSpan::new("root_span", "service-a", None)
  let child_span = TraceSpan::new("child_span", "service-b", Some(root_span.span_id))
  
  trace.spans.push(root_span)
  trace.spans.push(child_span)
  
  trace
}

fn create_inconsistent_trace_with_id(trace_id : String) {
  let trace = Trace::new(trace_id)
  
  let root_span = TraceSpan::new("root_span", "service-a", None)
  let child_span = TraceSpan::new("child_span", "service-b", Some("non_existent_parent"))
  
  trace.spans.push(root_span)
  trace.spans.push(child_span)
  
  trace
}