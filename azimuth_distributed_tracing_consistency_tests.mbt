// Azimuth Telemetry System - Distributed Tracing Consistency Tests
// This file contains test cases for distributed tracing consistency across services

// Test 1: Trace Context Propagation
test "trace context propagation" {
  // Create a root span
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let root_span_id = "b7ad6b7169203331"
  let root_span_ctx = SpanContext::new(trace_id, root_span_id, true, "")
  
  // Create a root span
  let root_span = Span::new("root-operation", Server, root_span_ctx)
  
  // Create a child span
  let child_span_id = "b7ad6b7169203332"
  let child_span_ctx = SpanContext::new(trace_id, child_span_id, true, "")
  let child_span = Span::new("child-operation", Client, child_span_ctx)
  
  // Verify trace ID consistency
  assert_eq(SpanContext::trace_id(Span::span_context(root_span)), trace_id)
  assert_eq(SpanContext::trace_id(Span::span_context(child_span)), trace_id)
  
  // Verify span IDs are different
  assert_not_eq(SpanContext::span_id(Span::span_context(root_span)), 
                SpanContext::span_id(Span::span_context(child_span)))
  
  // Verify parent-child relationship
  assert_eq(Span::parent_span_id(child_span), Some(root_span_id))
}

// Test 2: Cross-Service Trace Context Injection and Extraction
test "cross-service trace context injection and extraction" {
  // Create a trace context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Create a propagator
  let propagator = TraceContextPropagator::new()
  
  // Inject trace context into headers
  let carrier = Headers::new()
  TraceContextPropagator::inject(propagator, span_ctx, carrier)
  
  // Verify trace context was injected
  let traceparent_header = Headers::get(carrier, "traceparent")
  match traceparent_header {
    Some(header) => {
      // Header should contain version, trace-id, parent-id, and flags
      assert_true(header.contains(trace_id))
      assert_true(header.contains(span_id))
    }
    None => assert_true(false)
  }
  
  // Extract trace context from headers
  let extracted_ctx = TraceContextPropagator::extract(propagator, carrier)
  
  // Verify extracted context matches original
  match extracted_ctx {
    Some(ctx) => {
      assert_eq(SpanContext::trace_id(ctx), trace_id)
      assert_eq(SpanContext::span_id(ctx), span_id)
      assert_true(SpanContext::is_sampled(ctx))
    }
    None => assert_true(false)
  }
}

// Test 3: Baggage Propagation Across Services
test "baggage propagation across services" {
  // Create initial baggage
  let baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage_with_entries = Baggage::set_entry(baggage_with_entries, "request.id", "req456")
  
  // Create a propagator
  let propagator = TraceContextPropagator::new()
  
  // Inject baggage into headers
  let carrier = Headers::new()
  TraceContextPropagator::inject_baggage(propagator, baggage_with_entries, carrier)
  
  // Verify baggage was injected
  let baggage_header = Headers::get(carrier, "baggage")
  match baggage_header {
    Some(header) => {
      assert_true(header.contains("user.id=user123"))
      assert_true(header.contains("request.id=req456"))
    }
    None => assert_true(false)
  }
  
  // Extract baggage from headers
  let extracted_baggage = TraceContextPropagator::extract_baggage(propagator, carrier)
  
  // Verify extracted baggage matches original
  let user_id = Baggage::get_entry(extracted_baggage, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  let request_id = Baggage::get_entry(extracted_baggage, "request.id")
  match request_id {
    Some(value) => assert_eq(value, "req456")
    None => assert_true(false)
  }
}

// Test 4: Distributed Trace Consistency Validation
test "distributed trace consistency validation" {
  // Create a trace validator
  let validator = TraceConsistencyValidator::new()
  
  // Create a consistent trace
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span1_ctx = SpanContext::new(trace_id, "b7ad6b7169203331", true, "")
  let span2_ctx = SpanContext::new(trace_id, "b7ad6b7169203332", true, "")
  let span3_ctx = SpanContext::new(trace_id, "b7ad6b7169203333", true, "")
  
  // Create spans with consistent timing
  let span1 = Span::new("service-a-operation", Server, span1_ctx)
  Span::set_timestamp(span1, 1640995200000L, 1640995200100L)
  
  let span2 = Span::new("service-b-operation", Client, span2_ctx)
  Span::set_timestamp(span2, 1640995200050L, 1640995200080L)
  Span::set_parent_span_id(span2, "b7ad6b7169203331")
  
  let span3 = Span::new("service-c-operation", Server, span3_ctx)
  Span::set_timestamp(span3, 1640995200090L, 1640995200120L)
  Span::set_parent_span_id(span3, "b7ad6b7169203332")
  
  // Validate trace consistency
  let spans = [span1, span2, span3]
  let validation_result = TraceConsistencyValidator::validate_trace(validator, spans)
  
  // Verify validation passes
  assert_true(TraceConsistencyValidator::is_valid(validation_result))
  assert_eq(TraceConsistencyValidator::error_count(validation_result), 0)
}

// Test 5: Inconsistent Trace Detection
test "inconsistent trace detection" {
  // Create a trace validator
  let validator = TraceConsistencyValidator::new()
  
  // Create spans with inconsistent trace IDs
  let span1_ctx = SpanContext::new("trace1", "b7ad6b7169203331", true, "")
  let span2_ctx = SpanContext::new("trace2", "b7ad6b7169203332", true, "")  // Different trace ID
  let span3_ctx = SpanContext::new("trace1", "b7ad6b7169203333", true, "")
  
  let span1 = Span::new("service-a-operation", Server, span1_ctx)
  let span2 = Span::new("service-b-operation", Client, span2_ctx)
  Span::set_parent_span_id(span2, "b7ad6b7169203331")  // Claims parent from different trace
  
  let span3 = Span::new("service-c-operation", Server, span3_ctx)
  Span::set_parent_span_id(span3, "b7ad6b7169203332")
  
  // Validate trace consistency
  let spans = [span1, span2, span3]
  let validation_result = TraceConsistencyValidator::validate_trace(validator, spans)
  
  // Verify validation fails
  assert_false(TraceConsistencyValidator::is_valid(validation_result))
  assert_true(TraceConsistencyValidator::error_count(validation_result) > 0)
  
  // Check for specific error types
  let trace_id_errors = TraceConsistencyValidator::get_errors_by_type(validation_result, ValidationErrorType::InconsistentTraceId)
  assert_true(trace_id_errors.length() > 0)
}

// Test 6: Temporal Consistency Validation
test "temporal consistency validation" {
  // Create a trace validator
  let validator = TraceConsistencyValidator::new()
  
  // Create spans with timing issues
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span1_ctx = SpanContext::new(trace_id, "b7ad6b7169203331", true, "")
  let span2_ctx = SpanContext::new(trace_id, "b7ad6b7169203332", true, "")
  
  let span1 = Span::new("parent-operation", Server, span1_ctx)
  Span::set_timestamp(span1, 1640995200000L, 1640995200100L)  // 100ms duration
  
  let span2 = Span::new("child-operation", Client, span2_ctx)
  Span::set_timestamp(span2, 1640995200050L, 1640995200080L)  // Within parent timeframe
  Span::set_parent_span_id(span2, "b7ad6b7169203331")
  
  // Validate temporal consistency
  let spans = [span1, span2]
  let validation_result = TraceConsistencyValidator::validate_temporal_consistency(validator, spans)
  
  // Verify validation passes for consistent timing
  assert_true(TraceConsistencyValidator::is_valid(validation_result))
  
  // Create span with inconsistent timing
  let span3_ctx = SpanContext::new(trace_id, "b7ad6b7169203333", true, "")
  let span3 = Span::new("late-child-operation", Client, span3_ctx)
  Span::set_timestamp(span3, 1640995200200L, 1640995200250L)  // Starts after parent ends
  Span::set_parent_span_id(span3, "b7ad6b7169203331")
  
  let spans_with_timing_issue = [span1, span3]
  let timing_validation_result = TraceConsistencyValidator::validate_temporal_consistency(validator, spans_with_timing_issue)
  
  // Verify validation fails for inconsistent timing
  assert_false(TraceConsistencyValidator::is_valid(timing_validation_result))
  
  let timing_errors = TraceConsistencyValidator::get_errors_by_type(timing_validation_result, ValidationErrorType::TemporalInconsistency)
  assert_true(timing_errors.length() > 0)
}

// Test 7: Sampling Consistency Across Services
test "sampling consistency across services" {
  // Create a trace with sampling decisions
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // Create a sampled root span
  let root_span_ctx = SpanContext::new(trace_id, "b7ad6b7169203331", true, "")
  let root_span = Span::new("root-operation", Server, root_span_ctx)
  
  // Create child spans that should respect sampling decision
  let child1_ctx = SpanContext::new(trace_id, "b7ad6b7169203332", true, "")
  let child1 = Span::new("child-operation-1", Client, child1_ctx)
  Span::set_parent_span_id(child1, "b7ad6b7169203331")
  
  let child2_ctx = SpanContext::new(trace_id, "b7ad6b7169203333", true, "")
  let child2 = Span::new("child-operation-2", Server, child2_ctx)
  Span::set_parent_span_id(child2, "b7ad6b7169203331")
  
  // Verify all spans are sampled
  assert_true(SpanContext::is_sampled(Span::span_context(root_span)))
  assert_true(SpanContext::is_sampled(Span::span_context(child1)))
  assert_true(SpanContext::is_sampled(Span::span_context(child2)))
  
  // Create a trace validator
  let validator = TraceConsistencyValidator::new()
  let spans = [root_span, child1, child2]
  let validation_result = TraceConsistencyValidator::validate_sampling_consistency(validator, spans)
  
  // Verify sampling consistency
  assert_true(TraceConsistencyValidator::is_valid(validation_result))
  
  // Create a span with inconsistent sampling
  let inconsistent_ctx = SpanContext::new(trace_id, "b7ad6b7169203334", false, "")  // Not sampled
  let inconsistent_span = Span::new("inconsistent-operation", Client, inconsistent_ctx)
  Span::set_parent_span_id(inconsistent_span, "b7ad6b7169203331")
  
  let spans_with_inconsistent_sampling = [root_span, child1, child2, inconsistent_span]
  let sampling_validation_result = TraceConsistencyValidator::validate_sampling_consistency(validator, spans_with_inconsistent_sampling)
  
  // Verify sampling inconsistency is detected
  assert_false(TraceConsistencyValidator::is_valid(sampling_validation_result))
  
  let sampling_errors = TraceConsistencyValidator::get_errors_by_type(sampling_validation_result, ValidationErrorType::SamplingInconsistency)
  assert_true(sampling_errors.length() > 0)
}

// Test 8: Service Graph Consistency
test "service graph consistency" {
  // Create a service graph validator
  let validator = ServiceGraphValidator::new()
  
  // Create spans representing service calls
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // Service A calls Service B
  let span_a_ctx = SpanContext::new(trace_id, "b7ad6b7169203331", true, "")
  let span_a = Span::new("service-a-operation", Server, span_a_ctx)
  Span::set_service_name(span_a, "service-a")
  
  let span_b_ctx = SpanContext::new(trace_id, "b7ad6b7169203332", true, "")
  let span_b = Span::new("service-b-operation", Server, span_b_ctx)
  Span::set_service_name(span_b, "service-b")
  Span::set_parent_span_id(span_b, "b7ad6b7169203331")
  
  // Service B calls Service C
  let span_c_ctx = SpanContext::new(trace_id, "b7ad6b7169203333", true, "")
  let span_c = Span::new("service-c-operation", Server, span_c_ctx)
  Span::set_service_name(span_c, "service-c")
  Span::set_parent_span_id(span_c, "b7ad6b7169203332")
  
  // Validate service graph
  let spans = [span_a, span_b, span_c]
  let service_graph = ServiceGraphValidator::build_graph(validator, spans)
  let graph_validation_result = ServiceGraphValidator::validate_graph(validator, service_graph)
  
  // Verify graph is valid
  assert_true(ServiceGraphValidator::is_valid(graph_validation_result))
  
  // Verify service relationships
  let service_a_children = ServiceGraphValidator::get_child_services(service_graph, "service-a")
  assert_eq(service_a_children.length(), 1)
  assert_eq(service_a_children[0], "service-b")
  
  let service_b_children = ServiceGraphValidator::get_child_services(service_graph, "service-b")
  assert_eq(service_b_children.length(), 1)
  assert_eq(service_b_children[0], "service-c")
  
  let service_c_children = ServiceGraphValidator::get_child_services(service_graph, "service-c")
  assert_eq(service_c_children.length(), 0)  // Leaf service
  
  // Verify parent relationships
  let service_c_parents = ServiceGraphValidator::get_parent_services(service_graph, "service-c")
  assert_eq(service_c_parents.length(), 1)
  assert_eq(service_c_parents[0], "service-b")
}

// Test 9: Cross-Service Attribute Consistency
test "cross-service attribute consistency" {
  // Create an attribute consistency validator
  let validator = AttributeConsistencyValidator::new()
  
  // Define required attributes that should be consistent across services
  let required_attributes = [
    "service.name",
    "service.version",
    "deployment.environment"
  ]
  
  // Create spans with consistent attributes
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  let span1_ctx = SpanContext::new(trace_id, "b7ad6b7169203331", true, "")
  let span1 = Span::new("service-a-operation", Server, span1_ctx)
  Span::set_attributes(span1, [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("1.2.3")),
    ("deployment.environment", StringValue("production"))
  ])
  
  let span2_ctx = SpanContext::new(trace_id, "b7ad6b7169203332", true, "")
  let span2 = Span::new("service-b-operation", Client, span2_ctx)
  Span::set_attributes(span2, [
    ("service.name", StringValue("auth-service")),
    ("service.version", StringValue("1.2.3")),  // Same version
    ("deployment.environment", StringValue("production"))  // Same environment
  ])
  
  // Validate attribute consistency
  let spans = [span1, span2]
  let validation_result = AttributeConsistencyValidator::validate_attributes(validator, spans, required_attributes)
  
  // Verify validation passes for consistent attributes
  assert_true(AttributeConsistencyValidator::is_valid(validation_result))
  
  // Create a span with inconsistent attributes
  let span3_ctx = SpanContext::new(trace_id, "b7ad6b7169203333", true, "")
  let span3 = Span::new("service-c-operation", Server, span3_ctx)
  Span::set_attributes(span3, [
    ("service.name", StringValue("billing-service")),
    ("service.version", StringValue("2.0.0")),  // Different version
    ("deployment.environment", StringValue("staging"))  // Different environment
  ])
  
  let spans_with_inconsistent_attrs = [span1, span2, span3]
  let attr_validation_result = AttributeConsistencyValidator::validate_attributes(validator, spans_with_inconsistent_attrs, required_attributes)
  
  // Verify attribute inconsistency is detected
  assert_false(AttributeConsistencyValidator::is_valid(attr_validation_result))
  
  let attr_errors = AttributeConsistencyValidator::get_inconsistent_attributes(attr_validation_result)
  assert_true(attr_errors.contains("service.version"))
  assert_true(attr_errors.contains("deployment.environment"))
}

// Test 10: Distributed Trace Reconstruction
test "distributed trace reconstruction" {
  // Create a trace reconstructor
  let reconstructor = TraceReconstructor::new()
  
  // Create spans from different services that form a distributed trace
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // These spans might be received out of order
  let span2_ctx = SpanContext::new(trace_id, "b7ad6b7169203332", true, "")
  let span2 = Span::new("service-b-operation", Server, span2_ctx)
  Span::set_parent_span_id(span2, "b7ad6b7169203331")
  Span::set_timestamp(span2, 1640995200050L, 1640995200080L)
  
  let span1_ctx = SpanContext::new(trace_id, "b7ad6b7169203331", true, "")
  let span1 = Span::new("service-a-operation", Server, span1_ctx)
  Span::set_timestamp(span1, 1640995200000L, 1640995200100L)
  
  let span3_ctx = SpanContext::new(trace_id, "b7ad6b7169203333", true, "")
  let span3 = Span::new("service-c-operation", Client, span3_ctx)
  Span::set_parent_span_id(span3, "b7ad6b7169203332")
  Span::set_timestamp(span3, 1640995200090L, 1640995200120L)
  
  // Reconstruct the trace
  let spans = [span2, span1, span3]  // Out of order
  let reconstructed_trace = TraceReconstructor::reconstruct(reconstructor, spans)
  
  // Verify reconstruction
  assert_eq(TraceReconstructor::get_trace_id(reconstructed_trace), trace_id)
  assert_eq(TraceReconstructor::get_span_count(reconstructed_trace), 3)
  
  // Verify span order (root should be first)
  let ordered_spans = TraceReconstructor::get_ordered_spans(reconstructed_trace)
  assert_eq(SpanContext::span_id(Span::span_context(ordered_spans[0])), "b7ad6b7169203331")  // Root
  assert_eq(SpanContext::span_id(Span::span_context(ordered_spans[1])), "b7ad6b7169203332")  // Child
  assert_eq(SpanContext::span_id(Span::span_context(ordered_spans[2])), "b7ad6b7169203333")  // Grandchild
  
  // Verify trace tree structure
  let root_span = TraceReconstructor::get_root_span(reconstructed_trace)
  match root_span {
    Some(span) => {
      assert_eq(SpanContext::span_id(Span::span_context(span)), "b7ad6b7169203331")
      
      let children = TraceReconstructor::get_child_spans(reconstructed_trace, span)
      assert_eq(children.length(), 1)
      assert_eq(SpanContext::span_id(Span::span_context(children[0])), "b7ad6b7169203332")
      
      let grandchildren = TraceReconstructor::get_child_spans(reconstructed_trace, children[0])
      assert_eq(grandchildren.length(), 1)
      assert_eq(SpanContext::span_id(Span::span_context(grandchildren[0])), "b7ad6b7169203333")
    }
    None => assert_true(false)
  }
}