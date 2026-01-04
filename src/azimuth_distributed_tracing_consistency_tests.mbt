// Azimuth Telemetry System - Distributed Tracing Consistency Tests
// This file contains comprehensive test cases for distributed tracing consistency across services

// Test 1: Trace Context Propagation
test "trace context propagation across services" {
  // Initialize trace context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, parent_span_id, true, "test_state")
  
  // Simulate service A
  let service_a_span = Span::new("service_a_operation", Server, span_ctx)
  Span::add_event(service_a_span, "service_a_start", Some([("service", "service_a")]))
  
  // Create child span for service B
  let service_b_ctx = SpanContext::child_of(span_ctx, "service_b_span_id")
  let service_b_span = Span::new("service_b_operation", Client, service_b_ctx)
  Span::add_event(service_b_span, "service_b_start", Some([("service", "service_b")]))
  
  // Create child span for service C
  let service_c_ctx = SpanContext::child_of(service_b_ctx, "service_c_span_id")
  let service_c_span = Span::new("service_c_operation", Server, service_c_ctx)
  Span::add_event(service_c_span, "service_c_start", Some([("service", "service_c")]))
  
  // Verify trace ID consistency
  assert_eq(SpanContext::trace_id(Span::span_context(service_a_span)), trace_id)
  assert_eq(SpanContext::trace_id(Span::span_context(service_b_span)), trace_id)
  assert_eq(SpanContext::trace_id(Span::span_context(service_c_span)), trace_id)
  
  // Verify parent-child relationships
  assert_eq(SpanContext::parent_span_id(Span::span_context(service_b_span)), parent_span_id)
  assert_eq(SpanContext::parent_span_id(Span::span_context(service_c_span)), "service_b_span_id")
  
  // End spans in reverse order
  Span::end(service_c_span)
  Span::end(service_b_span)
  Span::end(service_a_span)
}

// Test 2: Cross-Service Trace Consistency
test "cross-service trace consistency" {
  let trace_collector = TraceCollector::new()
  
  // Simulate distributed request flow
  let trace_id = TraceGenerator::generate_trace_id()
  
  // Service A - Entry point
  let service_a_span = TraceCollector::create_span(trace_collector, "service_a", trace_id, None)
  Span::set_attribute(service_a_span, "service.name", "service_a")
  Span::set_attribute(service_a_span, "http.method", "GET")
  Span::set_attribute(service_a_span, "http.url", "/api/v1/resource")
  
  // Service B - Called by Service A
  let service_b_span = TraceCollector::create_child_span(trace_collector, "service_b", service_a_span)
  Span::set_attribute(service_b_span, "service.name", "service_b")
  Span::set_attribute(service_b_span, "db.operation", "SELECT")
  Span::set_attribute(service_b_span, "db.statement", "SELECT * FROM users")
  
  // Service C - Called by Service B
  let service_c_span = TraceCollector::create_child_span(trace_collector, "service_c", service_b_span)
  Span::set_attribute(service_c_span, "service.name", "service_c")
  Span::set_attribute(service_c_span, "cache.operation", "GET")
  Span::set_attribute(service_c_span, "cache.key", "user:123")
  
  // Collect trace data
  let trace_data = TraceCollector::collect_trace(trace_collector, trace_id)
  
  // Verify trace completeness
  assert_eq(trace_data.spans.length(), 3)
  assert_eq(trace_data.trace_id, trace_id)
  
  // Verify span relationships
  let root_spans = trace_data.spans.filter(fn(span) { span.parent_span_id.is_none() })
  assert_eq(root_spans.length(), 1)
  assert_eq(root_spans[0].span_name, "service_a")
  
  // Verify trace timeline consistency
  let sorted_spans = trace_data.spans.sort_by(fn(a, b) { a.start_time - b.start_time })
  assert_true(sorted_spans[0].start_time <= sorted_spans[1].start_time)
  assert_true(sorted_spans[1].start_time <= sorted_spans[2].start_time)
}

// Test 3: Trace Baggage Propagation
test "trace baggage propagation" {
  let baggage_manager = BaggageManager::new()
  
  // Initialize baggage in service A
  let initial_baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(initial_baggage, "user.id", "12345")
  let final_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  
  // Create span with baggage
  let span_ctx = SpanContext::with_baggage("trace_id", "span_id", true, final_baggage)
  let service_a_span = Span::new("service_a_operation", Server, span_ctx)
  
  // Propagate to service B
  let propagated_baggage = SpanContext::baggage(Span::span_context(service_a_span))
  let service_b_span = Span::new("service_b_operation", Client, 
    SpanContext::with_baggage("trace_id", "service_b_span_id", true, propagated_baggage))
  
  // Verify baggage propagation
  let service_b_baggage = SpanContext::baggage(Span::span_context(service_b_span))
  let user_id = Baggage::get_entry(service_b_baggage, "user.id")
  let request_id = Baggage::get_entry(service_b_baggage, "request.id")
  
  match user_id {
    Some(id) => assert_eq(id, "12345")
    None => assert_true(false)
  }
  
  match request_id {
    Some(id) => assert_eq(id, "req-67890")
    None => assert_true(false)
  }
  
  // Add new baggage entry in service B
  let service_b_updated = Baggage::set_entry(service_b_baggage, "service.b.timestamp", "1234567890")
  
  // Verify baggage doesn't affect original
  let original_user_id = Baggage::get_entry(final_baggage, "user.id")
  match original_user_id {
    Some(id) => assert_eq(id, "12345")
    None => assert_true(false)
  }
  
  // Verify new entry exists only in service B
  let service_b_timestamp = Baggage::get_entry(service_b_updated, "service.b.timestamp")
  let original_timestamp = Baggage::get_entry(final_baggage, "service.b.timestamp")
  
  match service_b_timestamp {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  match original_timestamp {
    Some(_) => assert_true(false)  // Should not exist in original
    None => assert_true(true)
  }
}

// Test 4: Trace Sampling Consistency
test "trace sampling consistency" {
  let sampling_manager = SamplingManager::new()
  
  // Configure sampling strategy
  SamplingManager::set_strategy(sampling_manager, SamplingStrategy::probability(0.1))
  
  // Generate multiple traces
  let mut sampled_traces = []
  let mut unsampled_traces = []
  
  for i in 0..=100 {
    let trace_id = TraceGenerator::generate_trace_id()
    let should_sample = SamplingManager::should_sample(sampling_manager, trace_id)
    
    if should_sample {
      let span = Span::new("operation", Server, 
        SpanContext::new(trace_id, "span_id", true, ""))
      sampled_traces = sampled_traces.push(span)
    } else {
      let span = Span::new("operation", Server, 
        SpanContext::new(trace_id, "span_id", false, ""))
      unsampled_traces = unsampled_traces.push(span)
    }
  }
  
  // Verify sampling rate is approximately 10%
  let sample_rate = sampled_traces.length() as Float / 100.0
  assert_float_eq(sample_rate, 0.1, 0.05)
  
  // Verify sampling consistency within a trace
  let trace_id = TraceGenerator::generate_trace_id()
  let should_sample = SamplingManager::should_sample(sampling_manager, trace_id)
  
  let root_span = Span::new("root_operation", Server, 
    SpanContext::new(trace_id, "root_span", should_sample, ""))
  let child_span = Span::new("child_operation", Client, 
    SpanContext::new(trace_id, "child_span", should_sample, ""))
  
  // Both spans should have same sampling decision
  assert_eq(SpanContext::is_sampled(Span::span_context(root_span)), should_sample)
  assert_eq(SpanContext::is_sampled(Span::span_context(child_span)), should_sample)
}

// Test 5: Trace Header Injection and Extraction
test "trace header injection and extraction" {
  let propagator = TracePropagator::new()
  
  // Create span context
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "trace_state")
  
  // Inject trace context into headers
  let headers = []
  let injected_headers = TracePropagator::inject(propagator, span_ctx, headers)
  
  // Verify required headers are injected
  assert_true(injected_headers.contains_key("traceparent"))
  assert_true(injected_headers.contains_key("tracestate"))
  
  // Extract trace context from headers
  let extracted_ctx = TracePropagator::extract(propagator, injected_headers)
  
  // Verify extraction correctness
  assert_eq(SpanContext::trace_id(extracted_ctx), "trace_id")
  assert_eq(SpanContext::span_id(extracted_ctx), "span_id")
  assert_eq(SpanContext::is_sampled(extracted_ctx), true)
  assert_eq(SpanContext::trace_state(extracted_ctx), "trace_state")
}

// Test 6: Distributed Trace Correlation
test "distributed trace correlation" {
  let correlation_manager = TraceCorrelationManager::new()
  
  // Create correlated traces from different services
  let trace_a = TraceData::new("trace_a_id", [
    SpanData::new("service_a", "operation_a1", 1000, 1100),
    SpanData::new("service_b", "operation_b1", 1050, 1150)
  ])
  
  let trace_b = TraceData::new("trace_b_id", [
    SpanData::new("service_a", "operation_a2", 2000, 2100),
    SpanData::new("service_c", "operation_c1", 2050, 2150)
  ])
  
  let trace_c = TraceData::new("trace_c_id", [
    SpanData::new("service_b", "operation_b2", 3000, 3100),
    SpanData::new("service_c", "operation_c2", 3050, 3150)
  ])
  
  // Add traces to correlation manager
  TraceCorrelationManager::add_trace(correlation_manager, trace_a)
  TraceCorrelationManager::add_trace(correlation_manager, trace_b)
  TraceCorrelationManager::add_trace(correlation_manager, trace_c)
  
  // Find correlated traces by service
  let service_a_traces = TraceCorrelationManager::get_traces_by_service(correlation_manager, "service_a")
  assert_eq(service_a_traces.length(), 2)
  
  let service_b_traces = TraceCorrelationManager::get_traces_by_service(correlation_manager, "service_b")
  assert_eq(service_b_traces.length(), 2)
  
  let service_c_traces = TraceCorrelationManager::get_traces_by_service(correlation_manager, "service_c")
  assert_eq(service_c_traces.length(), 2)
  
  // Find traces by time window
  let time_window_traces = TraceCorrelationManager::get_traces_in_time_window(
    correlation_manager, 1000, 2000)
  assert_eq(time_window_traces.length(), 2)
  
  // Analyze service interaction patterns
  let interactions = TraceCorrelationManager::analyze_service_interactions(correlation_manager)
  assert_true(interactions.length() > 0)
  
  // Verify specific interactions
  let a_to_b = interactions.find(fn(interaction) {
    interaction.from_service == "service_a" && interaction.to_service == "service_b"
  })
  assert_true(a_to_b.is_some())
}

// Test 7: Trace Integrity Verification
test "trace integrity verification" {
  let integrity_verifier = TraceIntegrityVerifier::new()
  
  // Create complete trace
  let complete_trace = TraceData::new("complete_trace_id", [
    SpanData::new("service_a", "operation_a", 1000, 1100),
    SpanData::new("service_b", "operation_b", 1050, 1150),
    SpanData::new("service_c", "operation_c", 1100, 1200)
  ])
  
  // Verify complete trace integrity
  let complete_result = TraceIntegrityVerifier::verify(integrity_verifier, complete_trace)
  assert_true(complete_result.is_valid)
  assert_eq(complete_result.errors.length(), 0)
  
  // Create incomplete trace (missing spans)
  let incomplete_trace = TraceData::new("incomplete_trace_id", [
    SpanData::new("service_a", "operation_a", 1000, 1100),
    SpanData::new("service_c", "operation_c", 1100, 1200)  // Missing service_b
  ])
  
  // Verify incomplete trace
  let incomplete_result = TraceIntegrityVerifier::verify(integrity_verifier, incomplete_trace)
  assert_false(incomplete_result.is_valid)
  assert_true(incomplete_result.errors.length() > 0)
  
  // Create trace with timing inconsistencies
  let inconsistent_trace = TraceData::new("inconsistent_trace_id", [
    SpanData::new("service_a", "operation_a", 1000, 1100),
    SpanData::new("service_b", "operation_b", 1050, 1150),
    SpanData::new("service_c", "operation_c", 900, 1000)  // Starts before parent
  ])
  
  // Verify inconsistent trace
  let inconsistent_result = TraceIntegrityVerifier::verify(integrity_verifier, inconsistent_trace)
  assert_false(inconsistent_result.is_valid)
  assert_true(inconsistent_result.errors.length() > 0)
  
  // Check for specific error types
  let has_timing_error = inconsistent_result.errors.any(fn(error) {
    error.contains("timing")
  })
  assert_true(has_timing_error)
}

// Test 8: Cross-Protocol Trace Propagation
test "cross-protocol trace propagation" {
  let protocol_propagator = CrossProtocolPropagator::new()
  
  // Create HTTP trace context
  let http_span_ctx = SpanContext::new("http_trace_id", "http_span_id", true, "")
  let http_headers = [
    ("traceparent", "00-http_trace_id-http_span_id-01"),
    ("tracestate", "key1=value1,key2=value2")
  ]
  
  // Extract from HTTP
  let http_extracted = CrossProtocolPropagator::extract_from_http(protocol_propagator, http_headers)
  assert_eq(SpanContext::trace_id(http_extracted), "http_trace_id")
  assert_eq(SpanContext::span_id(http_extracted), "http_span_id")
  
  // Convert to gRPC metadata
  let grpc_metadata = CrossProtocolPropagator::to_grpc_metadata(protocol_propagator, http_extracted)
  assert_true(grpc_metadata.contains_key("traceparent"))
  assert_true(grpc_metadata.contains_key("tracestate"))
  
  // Extract from gRPC
  let grpc_extracted = CrossProtocolPropagator::extract_from_grpc(protocol_propagator, grpc_metadata)
  assert_eq(SpanContext::trace_id(grpc_extracted), "http_trace_id")
  assert_eq(SpanContext::span_id(grpc_extracted), "http_span_id")
  
  // Convert to messaging system headers
  let messaging_headers = CrossProtocolPropagator::to_messaging_headers(protocol_propagator, grpc_extracted)
  assert_true(messaging_headers.contains_key("trace_id"))
  assert_true(messaging_headers.contains_key("span_id"))
  
  // Extract from messaging
  let messaging_extracted = CrossProtocolPropagator::extract_from_messaging(protocol_propagator, messaging_headers)
  assert_eq(SpanContext::trace_id(messaging_extracted), "http_trace_id")
  assert_eq(SpanContext::span_id(messaging_extracted), "http_span_id")
}

// Test 9: Distributed Trace Performance Impact
test "distributed trace performance impact" {
  let performance_analyzer = TracePerformanceAnalyzer::new()
  
  // Measure performance without tracing
  let baseline_time = performance_analyzer.measure_operation(fn() {
    // Simulate operation without tracing
    for i in 0..=1000 {
      let result = i * 2
      let _ = result.to_string()
    }
  })
  
  // Measure performance with tracing
  let traced_time = performance_analyzer.measure_operation_with_tracing(fn() {
    // Simulate operation with tracing
    let span = Span::new("test_operation", Internal, 
      SpanContext::new("trace_id", "span_id", true, ""))
    
    for i in 0..=1000 {
      let result = i * 2
      let _ = result.to_string()
      Span::add_event(span, "iteration", Some([("count", i.to_string())]))
    }
    
    Span::end(span)
  })
  
  // Calculate performance overhead
  let overhead_percentage = (traced_time - baseline_time) as Float / baseline_time as Float * 100.0
  
  // Performance overhead should be reasonable (less than 50%)
  assert_true(overhead_percentage < 50.0)
  
  // Analyze trace data size impact
  let trace_data_size = performance_analyzer.calculate_trace_data_size(1000)
  let memory_overhead = trace_data_size as Float / 1024.0 / 1024.0  // Convert to MB
  
  // Memory overhead should be reasonable (less than 10MB for 1000 operations)
  assert_true(memory_overhead < 10.0)
}

// Test 10: Trace Consistency Under Failure
test "trace consistency under failure" {
  let failure_simulator = TraceFailureSimulator::new()
  
  // Create trace with potential failure points
  let trace_id = TraceGenerator::generate_trace_id()
  let root_span = Span::new("root_operation", Server, 
    SpanContext::new(trace_id, "root_span", true, ""))
  
  // Simulate service call failure
  let service_a_span = Span::new("service_a_operation", Client, 
    SpanContext::new(trace_id, "service_a_span", true, ""))
  
  // Simulate failure in service A
  Span::set_status(service_a_span, Error, Some("Service unavailable"))
  Span::add_event(service_a_span, "error", Some([
    ("error.type", "service_unavailable"),
    ("error.message", "Service A is down")
  ]))
  
  // Simulate retry with new span
  let retry_span = Span::new("service_a_retry", Client, 
    SpanContext::new(trace_id, "service_a_retry_span", true, ""))
  Span::add_event(retry_span, "retry", Some([("retry.count", "1")]))
  Span::set_status(retry_span, Ok, None)
  
  // Simulate partial success in service B
  let service_b_span = Span::new("service_b_operation", Client, 
    SpanContext::new(trace_id, "service_b_span", true, ""))
  
  // Partial success - some operations failed
  Span::set_status(service_b_span, Error, Some("Partial failure"))
  Span::add_event(service_b_span, "partial_success", Some([
    ("success.count", "8"),
    ("failure.count", "2")
  ]))
  
  // Collect trace data
  let trace_data = TraceCollector::collect_spans([root_span, service_a_span, retry_span, service_b_span])
  
  // Verify trace consistency despite failures
  assert_eq(trace_data.spans.length(), 4)
  
  // Verify all spans have same trace ID
  for span in trace_data.spans {
    assert_eq(span.trace_id, trace_id)
  }
  
  // Verify error propagation
  let error_spans = trace_data.spans.filter(fn(span) { span.status == Error })
  assert_eq(error_spans.length(), 2)
  
  // Verify retry information is preserved
  let retry_spans = trace_data.spans.filter(fn(span) { 
    span.span_name.contains("retry") 
  })
  assert_eq(retry_spans.length(), 1)
  
  // Analyze failure patterns
  let failure_analysis = TraceFailureAnalyzer::analyze(trace_data)
  assert_true(failure_analysis.total_failures > 0)
  assert_true(failure_analysis.retry_attempts > 0)
  assert_true(failure_analysis.partial_failures > 0)
}