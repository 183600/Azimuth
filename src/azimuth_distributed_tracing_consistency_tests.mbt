// Azimuth Telemetry System - Distributed Tracing Consistency Tests
// This file contains comprehensive test cases for distributed tracing consistency

// Test 1: Trace Context Consistency Across Services
test "trace context consistency across services" {
  // Create a distributed trace with multiple services
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // Service A: Entry point
  let tracer_a = TracerProvider::get_tracer("service_a")
  let root_span = Tracer::start_span(tracer_a, "http_request")
  let root_span_ctx = Span::span_context(root_span)
  
  // Verify root span context
  assert_eq(SpanContext::trace_id(root_span_ctx), trace_id)
  assert_true(SpanContext::is_valid(root_span_ctx))
  assert_true(SpanContext::is_sampled(root_span_ctx))
  
  // Service B: First downstream service
  let tracer_b = TracerProvider::get_tracer("service_b")
  let span_b = Tracer::start_span_with_context(tracer_b, "process_data", root_span_ctx)
  let span_b_ctx = Span::span_context(span_b)
  
  // Verify Service B span context
  assert_eq(SpanContext::trace_id(span_b_ctx), trace_id)
  assert_eq(SpanContext::trace_id(span_b_ctx), SpanContext::trace_id(root_span_ctx))
  assert_true(SpanContext::is_valid(span_b_ctx))
  assert_true(SpanContext::is_sampled(span_b_ctx))
  
  // Verify parent-child relationship
  assert_eq(Span::parent_span_id(span_b), Some(SpanContext::span_id(root_span_ctx)))
  
  // Service C: Second downstream service
  let tracer_c = TracerProvider::get_tracer("service_c")
  let span_c = Tracer::start_span_with_context(tracer_c, "store_data", span_b_ctx)
  let span_c_ctx = Span::span_context(span_c)
  
  // Verify Service C span context
  assert_eq(SpanContext::trace_id(span_c_ctx), trace_id)
  assert_eq(SpanContext::trace_id(span_c_ctx), SpanContext::trace_id(root_span_ctx))
  assert_true(SpanContext::is_valid(span_c_ctx))
  assert_true(SpanContext::is_sampled(span_c_ctx))
  
  // Verify parent-child relationship
  assert_eq(Span::parent_span_id(span_c), Some(SpanContext::span_id(span_b_ctx)))
  
  // Service D: Parallel service called by B
  let tracer_d = TracerProvider::get_tracer("service_d")
  let span_d = Tracer::start_span_with_context(tracer_d, "validate_data", span_b_ctx)
  let span_d_ctx = Span::span_context(span_d)
  
  // Verify Service D span context
  assert_eq(SpanContext::trace_id(span_d_ctx), trace_id)
  assert_eq(SpanContext::trace_id(span_d_ctx), SpanContext::trace_id(root_span_ctx))
  assert_true(SpanContext::is_valid(span_d_ctx))
  assert_true(SpanContext::is_sampled(span_d_ctx))
  
  // Verify parent-child relationship
  assert_eq(Span::parent_span_id(span_d), Some(SpanContext::span_id(span_b_ctx)))
  
  // End all spans
  Span::end(span_c)
  Span::end(span_d)
  Span::end(span_b)
  Span::end(root_span)
  
  // Verify trace consistency
  let trace_consistency_checker = TraceConsistencyChecker::new()
  let spans = [root_span, span_b, span_c, span_d]
  let consistency_result = TraceConsistencyChecker::verify_trace_consistency(trace_consistency_checker, spans)
  
  assert_true(ConsistencyResult::is_consistent(consistency_result))
  assert_eq(ConsistencyResult::trace_id(consistency_result), trace_id)
  assert_eq(ConsistencyResult::span_count(consistency_result), 4)
}

// Test 2: Baggage Propagation Consistency
test "baggage propagation consistency" {
  // Create initial baggage
  let initial_baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(initial_baggage, "user_id", "user123")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session_id", "session456")
  let final_baggage = Baggage::set_entry(baggage_with_session, "request_id", "req789")
  
  // Create trace context
  let trace_id = "b7ad6b7169203331c8768e8b8a6b7b7b"
  let span_ctx = SpanContext::new(trace_id, "span123", true, "test_state")
  
  // Create context with baggage
  let ctx = Context::with_value(Context::root(), ContextKey::new("span_context"), span_ctx)
  let ctx_with_baggage = Context::with_value(ctx, ContextKey::new("baggage"), final_baggage)
  
  // Service A: Extract baggage
  let baggage_a = Context::get(ctx_with_baggage, ContextKey::new("baggage"))
  match baggage_a {
    Some(baggage) => {
      let user_id = Baggage::get_entry(baggage, "user_id")
      let session_id = Baggage::get_entry(baggage, "session_id")
      let request_id = Baggage::get_entry(baggage, "request_id")
      
      match (user_id, session_id, request_id) {
        (Some("user123"), Some("session456"), Some("req789")) => assert_true(true)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Service B: Add new baggage entry
  let baggage_b = match baggage_a {
    Some(baggage) => Baggage::set_entry(baggage, "service_b_data", "b_data")
    None => assert_true(false); Baggage::new()
  }
  
  // Service C: Add another baggage entry
  let baggage_c = Baggage::set_entry(baggage_b, "service_c_data", "c_data")
  
  // Verify baggage propagation consistency
  let baggage_consistency_checker = BaggageConsistencyChecker::new()
  let baggage_chain = [final_baggage, baggage_b, baggage_c]
  let consistency_result = BaggageConsistencyChecker::verify_baggage_consistency(baggage_consistency_checker, baggage_chain)
  
  assert_true(BaggageConsistencyResult::is_consistent(consistency_result))
  
  // Verify all original entries are preserved
  let final_baggage_in_c = baggage_c
  let user_id_c = Baggage::get_entry(final_baggage_in_c, "user_id")
  let session_id_c = Baggage::get_entry(final_baggage_in_c, "session_id")
  let request_id_c = Baggage::get_entry(final_baggage_in_c, "request_id")
  
  match (user_id_c, session_id_c, request_id_c) {
    (Some("user123"), Some("session456"), Some("req789")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Verify new entries are added
  let service_b_data = Baggage::get_entry(final_baggage_in_c, "service_b_data")
  let service_c_data = Baggage::get_entry(final_baggage_in_c, "service_c_data")
  
  match (service_b_data, service_c_data) {
    (Some("b_data"), Some("c_data")) => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 3: Sampling Decision Consistency
test "sampling decision consistency" {
  // Create a trace with sampling decisions
  let trace_id = "c8768e8b8a6b7b7b0af7651916cd43dd"
  
  // Test with sampling enabled
  let sampler = TraceIdRatioBasedSampler::new(1.0) // 100% sampling
  let sampling_decision = Sampler::should_sample(sampler, trace_id, "test_operation", None)
  
  match sampling_decision {
    SamplingDecision::RecordAndSample => assert_true(true)
    _ => assert_true(false)
  }
  
  // Create root span with sampling decision
  let tracer = TracerProvider::get_tracer("sampling_test")
  let root_span = Tracer::start_span_with_sampling(tracer, "root_operation", None, sampling_decision)
  let root_span_ctx = Span::span_context(root_span)
  
  // Verify sampling decision is applied
  assert_true(SpanContext::is_sampled(root_span_ctx))
  
  // Create child spans - they should inherit sampling decision
  let child_span1 = Tracer::start_span_with_context(tracer, "child_operation_1", root_span_ctx)
  let child_span1_ctx = Span::span_context(child_span1)
  
  let child_span2 = Tracer::start_span_with_context(tracer, "child_operation_2", root_span_ctx)
  let child_span2_ctx = Span::span_context(child_span2)
  
  // Verify child spans inherit sampling decision
  assert_true(SpanContext::is_sampled(child_span1_ctx))
  assert_true(SpanContext::is_sampled(child_span2_ctx))
  
  // Test with sampling disabled
  let no_sampler = TraceIdRatioBasedSampler::new(0.0) // 0% sampling
  let no_sampling_decision = Sampler::should_sample(no_sampler, trace_id + "_nosample", "test_operation", None)
  
  match no_sampling_decision {
    SamplingDecision::Drop => assert_true(true)
    _ => assert_true(false)
  }
  
  // Create root span with no sampling
  let root_span_no_sample = Tracer::start_span_with_sampling(tracer, "root_operation_no_sample", None, no_sampling_decision)
  let root_span_no_sample_ctx = Span::span_context(root_span_no_sample)
  
  // Verify no sampling decision is applied
  assert_false(SpanContext::is_sampled(root_span_no_sample_ctx))
  
  // Create child spans - they should inherit no sampling decision
  let child_span_no_sample = Tracer::start_span_with_context(tracer, "child_operation_no_sample", root_span_no_sample_ctx)
  let child_span_no_sample_ctx = Span::span_context(child_span_no_sample)
  
  // Verify child spans inherit no sampling decision
  assert_false(SpanContext::is_sampled(child_span_no_sample_ctx))
  
  // Verify sampling consistency
  let sampling_consistency_checker = SamplingConsistencyChecker::new()
  let sampled_spans = [root_span, child_span1, child_span2]
  let unsampled_spans = [root_span_no_sample, child_span_no_sample]
  
  let sampled_consistency = SamplingConsistencyChecker::verify_sampling_consistency(
    sampling_consistency_checker, 
    sampled_spans, 
    true
  )
  assert_true(SamplingConsistencyResult::is_consistent(sampled_consistency))
  
  let unsampled_consistency = SamplingConsistencyChecker::verify_sampling_consistency(
    sampling_consistency_checker, 
    unsampled_spans, 
    false
  )
  assert_true(SamplingConsistencyResult::is_consistent(unsampled_consistency))
}

// Test 4: Trace State Consistency
test "trace state consistency" {
  // Create a trace with trace state
  let trace_id = "d8448eb211c80319c0af7651916cd43dd"
  let initial_trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  
  // Create root span with trace state
  let span_ctx = SpanContext::new(trace_id, "span123", true, initial_trace_state)
  let tracer = TracerProvider::get_tracer("trace_state_test")
  let root_span = Tracer::start_span_with_context(tracer, "root_operation", span_ctx)
  
  // Verify initial trace state
  let root_span_ctx = Span::span_context(root_span)
  assert_eq(SpanContext::trace_state(root_span_ctx), initial_trace_state)
  
  // Service A: Add to trace state
  let updated_trace_state_a = TraceStateBuilder::from_string(initial_trace_state)
    .add("vendor1", "value1")
    .build()
  
  let span_ctx_a = SpanContext::new(trace_id, "span456", true, updated_trace_state_a)
  let span_a = Tracer::start_span_with_context(tracer, "service_a_operation", span_ctx_a)
  
  // Verify updated trace state
  let span_a_ctx = Span::span_context(span_a)
  assert_true(TraceState::contains_key(SpanContext::trace_state(span_a_ctx), "vendor1"))
  assert_eq(TraceState::get_value(SpanContext::trace_state(span_a_ctx), "vendor1"), Some("value1"))
  
  // Service B: Add more to trace state
  let updated_trace_state_b = TraceStateBuilder::from_string(updated_trace_state_a)
    .add("vendor2", "value2")
    .update("rojo", "00f067aa0ba902b8") // Update existing entry
    .build()
  
  let span_ctx_b = SpanContext::new(trace_id, "span789", true, updated_trace_state_b)
  let span_b = Tracer::start_span_with_context(tracer, "service_b_operation", span_ctx_b)
  
  // Verify further updated trace state
  let span_b_ctx = Span::span_context(span_b)
  assert_true(TraceState::contains_key(SpanContext::trace_state(span_b_ctx), "vendor1"))
  assert_true(TraceState::contains_key(SpanContext::trace_state(span_b_ctx), "vendor2"))
  assert_eq(TraceState::get_value(SpanContext::trace_state(span_b_ctx), "rojo"), Some("00f067aa0ba902b8"))
  
  // Service C: Remove from trace state
  let updated_trace_state_c = TraceStateBuilder::from_string(updated_trace_state_b)
    .remove("congo")
    .build()
  
  let span_ctx_c = SpanContext::new(trace_id, "span101112", true, updated_trace_state_c)
  let span_c = Tracer::start_span_with_context(tracer, "service_c_operation", span_ctx_c)
  
  // Verify trace state after removal
  let span_c_ctx = Span::span_context(span_c)
  assert_false(TraceState::contains_key(SpanContext::trace_state(span_c_ctx), "congo"))
  assert_true(TraceState::contains_key(SpanContext::trace_state(span_c_ctx), "vendor1"))
  assert_true(TraceState::contains_key(SpanContext::trace_state(span_c_ctx), "vendor2"))
  
  // Verify trace state consistency
  let trace_state_consistency_checker = TraceStateConsistencyChecker::new()
  let trace_states = [
    initial_trace_state,
    updated_trace_state_a,
    updated_trace_state_b,
    updated_trace_state_c
  ]
  
  let consistency_result = TraceStateConsistencyChecker::verify_trace_state_consistency(
    trace_state_consistency_checker, 
    trace_states
  )
  
  assert_true(TraceStateConsistencyResult::is_consistent(consistency_result))
  
  // Verify trace state evolution rules
  assert_true(TraceStateConsistencyResult::preserves_original_entries(consistency_result))
  assert_true(TraceStateConsistencyResult::respects_entry_limits(consistency_result))
}

// Test 5: Cross-Protocol Trace Propagation Consistency
test "cross-protocol trace propagation consistency" {
  // Create a trace context
  let trace_id = "e8b8a6b7b7b0af7651916cd43dd8448e"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "vendor1=value1")
  
  // Test HTTP/HTTPS header propagation
  let http_propagator = HttpTraceContextPropagator::new()
  let http_headers = []
  
  // Inject trace context into HTTP headers
  HttpTraceContextPropagator::inject(http_propagator, span_ctx, http_headers)
  
  // Verify required headers are present
  let trace_parent = HttpHeader::get(http_headers, "traceparent")
  let trace_state = HttpHeader::get(http_headers, "tracestate")
  
  match trace_parent {
    Some(header_value) => {
      // traceparent format: version-trace_id-parent_id-flags
      assert_true(header_value.contains(trace_id))
      assert_true(header_value.contains(span_id))
    }
    None => assert_true(false)
  }
  
  match trace_state {
    Some(header_value) => {
      assert_eq(header_value, "vendor1=value1")
    }
    None => assert_true(false)
  }
  
  // Extract trace context from HTTP headers
  let extracted_span_ctx = HttpTraceContextPropagator::extract(http_propagator, http_headers)
  
  // Verify extracted context matches original
  assert_eq(SpanContext::trace_id(extracted_span_ctx), trace_id)
  assert_eq(SpanContext::span_id(extracted_span_ctx), span_id)
  assert_true(SpanContext::is_sampled(extracted_span_ctx))
  assert_eq(SpanContext::trace_state(extracted_span_ctx), "vendor1=value1")
  
  // Test gRPC metadata propagation
  let grpc_propagator = GrpcTraceContextPropagator::new()
  let grpc_metadata = []
  
  // Inject trace context into gRPC metadata
  GrpcTraceContextPropagator::inject(grpc_propagator, span_ctx, grpc_metadata)
  
  // Verify required metadata is present
  let grpc_trace_parent = GrpcMetadata::get(grpc_metadata, "grpc-trace-bin")
  
  match grpc_trace_parent {
    Some(metadata_value) => {
      // gRPC uses binary format, so we just verify it's present
      assert_true(metadata_value.length() > 0)
    }
    None => assert_true(false)
  }
  
  // Extract trace context from gRPC metadata
  let extracted_grpc_span_ctx = GrpcTraceContextPropagator::extract(grpc_propagator, grpc_metadata)
  
  // Verify extracted context matches original
  assert_eq(SpanContext::trace_id(extracted_grpc_span_ctx), trace_id)
  assert_eq(SpanContext::span_id(extracted_grpc_span_ctx), span_id)
  assert_true(SpanContext::is_sampled(extracted_grpc_span_ctx))
  
  // Test AMQP message properties propagation
  let amqp_propagator = AmqpTraceContextPropagator::new()
  let amqp_properties = []
  
  // Inject trace context into AMQP properties
  AmqpTraceContextPropagator::inject(amqp_propagator, span_ctx, amqp_properties)
  
  // Verify required properties are present
  let amqp_trace_parent = AmqpProperty::get(amqp_properties, "traceparent")
  let amqp_trace_state = AmqpProperty::get(amqp_properties, "tracestate")
  
  match amqp_trace_parent {
    Some(property_value) => {
      assert_true(property_value.contains(trace_id))
      assert_true(property_value.contains(span_id))
    }
    None => assert_true(false)
  }
  
  // Extract trace context from AMQP properties
  let extracted_amqp_span_ctx = AmqpTraceContextPropagator::extract(amqp_propagator, amqp_properties)
  
  // Verify extracted context matches original
  assert_eq(SpanContext::trace_id(extracted_amqp_span_ctx), trace_id)
  assert_eq(SpanContext::span_id(extracted_amqp_span_ctx), span_id)
  assert_true(SpanContext::is_sampled(extracted_amqp_span_ctx))
  
  // Verify cross-protocol consistency
  let cross_protocol_checker = CrossProtocolConsistencyChecker::new()
  let propagated_contexts = [
    extracted_span_ctx,
    extracted_grpc_span_ctx,
    extracted_amqp_span_ctx
  ]
  
  let consistency_result = CrossProtocolConsistencyChecker::verify_cross_protocol_consistency(
    cross_protocol_checker, 
    propagated_contexts
  )
  
  assert_true(CrossProtocolConsistencyResult::is_consistent(consistency_result))
  assert_eq(CrossProtocolConsistencyResult::common_trace_id(consistency_result), trace_id)
}

// Test 6: Distributed Clock Skew Handling
test "distributed clock skew handling" {
  // Create a trace with multiple services
  let trace_id = "f1916cd43dd8448eb211c80319c0af76"
  let tracer = TracerProvider::get_tracer("clock_skew_test")
  
  // Service A: Start with timestamp 1000
  let span_a = Tracer::start_span_with_timestamp(tracer, "service_a_operation", None, 1000L)
  Span::add_event_with_timestamp(span_a, "start_event", Some([]), 1005L)
  Span::add_event_with_timestamp(span_a, "end_event", Some([]), 1015L)
  
  // Service B: Start with timestamp 990 (10ms behind Service A)
  let span_b_ctx = SpanContext::new(trace_id, "span_b", true, "")
  let span_b = Tracer::start_span_with_timestamp(tracer, "service_b_operation", Some(span_b_ctx), 990L)
  Span::add_event_with_timestamp(span_b, "start_event", Some([]), 995L)
  Span::add_event_with_timestamp(span_b, "end_event", Some([]), 1005L)
  
  // Service C: Start with timestamp 1020 (20ms ahead of Service A)
  let span_c_ctx = SpanContext::new(trace_id, "span_c", true, "")
  let span_c = Tracer::start_span_with_timestamp(tracer, "service_c_operation", Some(span_c_ctx), 1020L)
  Span::add_event_with_timestamp(span_c, "start_event", Some([]), 1025L)
  Span::add_event_with_timestamp(span_c, "end_event", Some([]), 1035L)
  
  // Service D: Start with timestamp 980 (20ms behind Service A)
  let span_d_ctx = SpanContext::new(trace_id, "span_d", true, "")
  let span_d = Tracer::start_span_with_timestamp(tracer, "service_d_operation", Some(span_d_ctx), 980L)
  Span::add_event_with_timestamp(span_d, "start_event", Some([]), 985L)
  Span::add_event_with_timestamp(span_d, "end_event", Some([]), 995L)
  
  // End all spans
  Span::end_with_timestamp(span_a, 1020L)
  Span::end_with_timestamp(span_b, 1010L)
  Span::end_with_timestamp(span_c, 1040L)
  Span::end_with_timestamp(span_d, 1000L)
  
  // Analyze clock skew
  let clock_skew_analyzer = ClockSkewAnalyzer::new()
  let spans = [span_a, span_b, span_c, span_d]
  let skew_analysis = ClockSkewAnalyzer::analyze_clock_skew(clock_skew_analyzer, spans)
  
  // Verify clock skew analysis
  assert_true(ClockSkewAnalysis::has_skew(skew_analysis))
  assert_eq(ClockSkewAnalysis::max_skew_ms(skew_analysis), 40) // 1020 - 980 = 40ms
  assert_eq(ClockSkewAnalysis::min_timestamp(skew_analysis), 980L)
  assert_eq(ClockSkewAnalysis::max_timestamp(skew_analysis), 1040L)
  
  // Apply clock skew correction
  let corrected_spans = ClockSkewAnalyzer::apply_clock_correction(clock_skew_analyzer, spans, 1000L)
  
  // Verify corrected timestamps
  let corrected_span_a = corrected_spans[0]
  let corrected_span_b = corrected_spans[1]
  let corrected_span_c = corrected_spans[2]
  let corrected_span_d = corrected_spans[3]
  
  // All spans should now be normalized around the reference timestamp (1000L)
  assert_eq(Span::start_time(corrected_span_a), 1000L)
  assert_eq(Span::start_time(corrected_span_b), 1010L) // 990 + 20ms correction
  assert_eq(Span::start_time(corrected_span_c), 1000L) // 1020 - 20ms correction
  assert_eq(Span::start_time(corrected_span_d), 1020L) // 980 + 40ms correction
  
  // Verify corrected events
  let corrected_events_a = Span::events(corrected_span_a)
  let corrected_events_b = Span::events(corrected_span_b)
  
  match (corrected_events_a[0], corrected_events_a[1]) {
    (Event(_, _, timestamp1), Event(_, _, timestamp2)) => {
      assert_eq(timestamp1, 1005L)
      assert_eq(timestamp2, 1015L)
    }
    _ => assert_true(false)
  }
  
  match (corrected_events_b[0], corrected_events_b[1]) {
    (Event(_, _, timestamp1), Event(_, _, timestamp2)) => {
      assert_eq(timestamp1, 1015L) // 995 + 20ms correction
      assert_eq(timestamp2, 1025L) // 1005 + 20ms correction
    }
    _ => assert_true(false)
  }
  
  // Verify temporal consistency after correction
  let temporal_consistency_checker = TemporalConsistencyChecker::new()
  let consistency_result = TemporalConsistencyChecker::verify_temporal_consistency(
    temporal_consistency_checker, 
    corrected_spans
  )
  
  assert_true(TemporalConsistencyResult::is_consistent(consistency_result))
}

// Test 7: Trace Integrity Validation
test "trace integrity validation" {
  // Create a complex distributed trace
  let trace_id = "g211c80319c0af7651916cd43dd8448e"
  let tracer = TracerProvider::get_tracer("trace_integrity_test")
  
  // Create a hierarchical trace structure
  let root_span = Tracer::start_span(tracer, "root_operation")
  let root_span_ctx = Span::span_context(root_span)
  
  // Level 1 spans
  let span_1 = Tracer::start_span_with_context(tracer, "operation_1", root_span_ctx)
  let span_2 = Tracer::start_span_with_context(tracer, "operation_2", root_span_ctx)
  let span_3 = Tracer::start_span_with_context(tracer, "operation_3", root_span_ctx)
  
  // Level 2 spans
  let span_1_1 = Tracer::start_span_with_context(tracer, "operation_1_1", Span::span_context(span_1))
  let span_1_2 = Tracer::start_span_with_context(tracer, "operation_1_2", Span::span_context(span_1))
  let span_2_1 = Tracer::start_span_with_context(tracer, "operation_2_1", Span::span_context(span_2))
  let span_3_1 = Tracer::start_span_with_context(tracer, "operation_3_1", Span::span_context(span_3))
  let span_3_2 = Tracer::start_span_with_context(tracer, "operation_3_2", Span::span_context(span_3))
  
  // Level 3 spans
  let span_1_1_1 = Tracer::start_span_with_context(tracer, "operation_1_1_1", Span::span_context(span_1_1))
  let span_3_2_1 = Tracer::start_span_with_context(tracer, "operation_3_2_1", Span::span_context(span_3_2))
  
  // Add attributes to establish relationships
  Span::set_attribute(root_span, "trace.level", IntValue(0))
  Span::set_attribute(span_1, "trace.level", IntValue(1))
  Span::set_attribute(span_2, "trace.level", IntValue(1))
  Span::set_attribute(span_3, "trace.level", IntValue(1))
  Span::set_attribute(span_1_1, "trace.level", IntValue(2))
  Span::set_attribute(span_1_2, "trace.level", IntValue(2))
  Span::set_attribute(span_2_1, "trace.level", IntValue(2))
  Span::set_attribute(span_3_1, "trace.level", IntValue(2))
  Span::set_attribute(span_3_2, "trace.level", IntValue(2))
  Span::set_attribute(span_1_1_1, "trace.level", IntValue(3))
  Span::set_attribute(span_3_2_1, "trace.level", IntValue(3))
  
  // End spans in reverse order (LIFO)
  Span::end(span_1_1_1)
  Span::end(span_3_2_1)
  Span::end(span_1_1)
  Span::end(span_1_2)
  Span::end(span_2_1)
  Span::end(span_3_1)
  Span::end(span_3_2)
  Span::end(span_1)
  Span::end(span_2)
  Span::end(span_3)
  Span::end(root_span)
  
  // Collect all spans
  let all_spans = [
    root_span, span_1, span_2, span_3,
    span_1_1, span_1_2, span_2_1, span_3_1, span_3_2,
    span_1_1_1, span_3_2_1
  ]
  
  // Verify trace integrity
  let trace_integrity_validator = TraceIntegrityValidator::new()
  let integrity_result = TraceIntegrityValidator::validate_trace_integrity(
    trace_integrity_validator, 
    all_spans
  )
  
  // Verify trace structure integrity
  assert_true(TraceIntegrityResult::has_valid_structure(integrity_result))
  assert_eq(TraceIntegrityResult::span_count(integrity_result), 11)
  assert_eq(TraceIntegrityResult::max_depth(integrity_result), 3)
  assert_eq(TraceIntegrityResult::root_span_count(integrity_result), 1)
  
  // Verify parent-child relationships
  assert_true(TraceIntegrityResult::has_valid_parent_child_relationships(integrity_result))
  
  // Verify trace ID consistency
  assert_true(TraceIntegrityResult::has_consistent_trace_id(integrity_result))
  
  // Verify no cycles in trace graph
  assert_true(TraceIntegrityResult::has_no_cycles(integrity_result))
  
  // Verify all spans are reachable from root
  assert_true(TraceIntegrityResult::all_spans_reachable_from_root(integrity_result))
  
  // Create a trace with integrity issues for negative testing
  let invalid_span = Tracer::start_span(tracer, "invalid_span")
  Span::set_attribute(invalid_span, "trace.level", IntValue(1))
  // Don't set proper parent relationship - this creates an orphan span
  
  let spans_with_issues = Array::append(all_spans, [invalid_span])
  let invalid_integrity_result = TraceIntegrityValidator::validate_trace_integrity(
    trace_integrity_validator, 
    spans_with_issues
  )
  
  // Should detect integrity issues
  assert_false(TraceIntegrityResult::has_valid_structure(invalid_integrity_result))
  assert_true(TraceIntegrityResult::has_orphan_spans(invalid_integrity_result))
}