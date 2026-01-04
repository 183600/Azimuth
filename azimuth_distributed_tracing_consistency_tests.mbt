// Azimuth Telemetry System - Distributed Tracing Consistency Tests
// This file contains test cases for distributed tracing consistency across services

// Test 1: Cross-Service Trace Context Propagation
test "cross-service trace context propagation" {
  let trace_context_manager = TraceContextManager::new()
  
  // Create root trace context
  let root_trace_id = "root_trace_12345"
  let root_span_id = "root_span_67890"
  let root_context = TraceContext::new(root_trace_id, root_span_id, true, [])
    .with_baggage_entry("user_id", "user_123")
    .with_baggage_entry("request_id", "req_456")
  
  // Test context propagation to service A
  let service_a_context = TraceContextManager::propagate_to_service(
    trace_context_manager,
    root_context,
    "service-a"
  )
  
  assert_eq(TraceContext::trace_id(service_a_context), root_trace_id)
  assert_not_eq(TraceContext::span_id(service_a_context), root_span_id) // Should have new span ID
  assert_true(TraceContext::is_sampled(service_a_context))
  
  // Verify baggage is preserved
  let user_id = TraceContext::get_baggage_entry(service_a_context, "user_id")
  match user_id {
    Some(value) => assert_eq(value, "user_123")
    None => assert_true(false)
  }
  
  // Test context propagation from service A to service B
  let service_b_context = TraceContextManager::propagate_to_service(
    trace_context_manager,
    service_a_context,
    "service-b"
  )
  
  assert_eq(TraceContext::trace_id(service_b_context), root_trace_id)
  assert_not_eq(TraceContext::span_id(service_b_context), TraceContext::span_id(service_a_context))
  
  // Test context propagation from service B to service C
  let service_c_context = TraceContextManager::propagate_to_service(
    trace_context_manager,
    service_b_context,
    "service-c"
  )
  
  assert_eq(TraceContext::trace_id(service_c_context), root_trace_id)
  assert_not_eq(TraceContext::span_id(service_c_context), TraceContext::span_id(service_b_context))
  
  // Verify all contexts belong to the same trace
  let trace_spans = TraceContextManager::get_all_spans_in_trace(trace_context_manager, root_trace_id)
  assert_eq(trace_spans.length(), 4) // root + service-a + service-b + service-c
  
  for span in trace_spans {
    assert_eq(TraceContext::trace_id(span), root_trace_id)
  }
}

// Test 2: Distributed Span Relationships
test "distributed span relationships" {
  let span_relationship_manager = SpanRelationshipManager::new()
  
  // Create root span
  let root_span = Span::new("root_operation", Server, TraceContext::new("trace_1", "span_1", true, []))
  
  // Create child spans in different services
  let service_a_span = Span::new("service_a_operation", Server, 
    TraceContext::new("trace_1", "span_2", true, [])
      .with_parent_span_id("span_1")
  )
  
  let service_b_span = Span::new("service_b_operation", Server,
    TraceContext::new("trace_1", "span_3", true, [])
      .with_parent_span_id("span_2")
  )
  
  let service_c_span = Span::new("service_c_operation", Server,
    TraceContext::new("trace_1", "span_4", true, [])
      .with_parent_span_id("span_2")
  )
  
  // Add spans to relationship manager
  SpanRelationshipManager::add_span(span_relationship_manager, root_span)
  SpanRelationshipManager::add_span(span_relationship_manager, service_a_span)
  SpanRelationshipManager::add_span(span_relationship_manager, service_b_span)
  SpanRelationshipManager::add_span(span_relationship_manager, service_c_span)
  
  // Test parent-child relationships
  let root_children = SpanRelationshipManager::get_children(span_relationship_manager, "span_1")
  assert_eq(root_children.length(), 1)
  assert_eq(Span::name(root_children[0]), "service_a_operation")
  
  let service_a_children = SpanRelationshipManager::get_children(span_relationship_manager, "span_2")
  assert_eq(service_a_children.length(), 2)
  
  let child_names = service_a_children.map(|s| Span::name(s))
  assert_true(child_names.contains("service_b_operation"))
  assert_true(child_names.contains("service_c_operation"))
  
  // Test span hierarchy
  let hierarchy = SpanRelationshipManager::build_hierarchy(span_relationship_manager, "trace_1")
  assert_eq(hierarchy.root_span_id, "span_1")
  assert_eq(hierarchy.all_spans.length(), 4)
  
  // Test depth calculation
  let root_depth = SpanRelationshipManager::get_depth(span_relationship_manager, "span_1")
  let service_a_depth = SpanRelationshipManager::get_depth(span_relationship_manager, "span_2")
  let service_b_depth = SpanRelationshipManager::get_depth(span_relationship_manager, "span_3")
  
  assert_eq(root_depth, 0)
  assert_eq(service_a_depth, 1)
  assert_eq(service_b_depth, 2)
}

// Test 3: Trace Consistency Validation
test "trace consistency validation" {
  let trace_validator = TraceConsistencyValidator::new()
  
  // Configure validation rules
  let validation_rules = ValidationRules::new()
    .require_trace_id_consistency(true)
    .require_parent_child_consistency(true)
    .require_timestamp_ordering(true)
    .require_service_name_format(true)
    .require_span_name_format(true)
  
  TraceConsistencyValidator::configure(trace_validator, validation_rules)
  
  // Create a consistent trace
  let consistent_trace = DistributedTrace::new("consistent_trace_001")
    .add_span(Span::new("root", Server, 
      TraceContext::new("consistent_trace_001", "span_1", true, [])
        .with_timestamp(Time::now())
    ))
    .add_span(Span::new("service_a", Client,
      TraceContext::new("consistent_trace_001", "span_2", true, [])
        .with_parent_span_id("span_1")
        .with_timestamp(Time::now() + Duration::milliseconds(10))
    ))
    .add_span(Span::new("service_b", Client,
      TraceContext::new("consistent_trace_001", "span_3", true, [])
        .with_parent_span_id("span_2")
        .with_timestamp(Time::now() + Duration::milliseconds(20))
    ))
  
  // Validate consistent trace
  let consistent_result = TraceConsistencyValidator::validate(trace_validator, consistent_trace)
  match consistent_result {
    ValidationResult::Valid => assert_true(true),
    ValidationResult::Invalid(errors) => assert_true(false, "Consistent trace should be valid: " + errors.join(", "))
    ValidationResult::Warning(warnings) => assert_true(true) // Warnings are acceptable
  }
  
  // Create an inconsistent trace (invalid parent-child relationship)
  let inconsistent_trace = DistributedTrace::new("inconsistent_trace_001")
    .add_span(Span::new("root", Server,
      TraceContext::new("inconsistent_trace_001", "span_1", true, [])
        .with_timestamp(Time::now())
    ))
    .add_span(Span::new("service_a", Client,
      TraceContext::new("inconsistent_trace_001", "span_2", true, [])
        .with_parent_span_id("non_existent_parent") // Invalid parent
        .with_timestamp(Time::now() + Duration::milliseconds(10))
    ))
    .add_span(Span::new("service_b", Client,
      TraceContext::new("different_trace_001", "span_3", true, []) // Different trace ID
        .with_parent_span_id("span_2")
        .with_timestamp(Time::now() + Duration::milliseconds(20))
    ))
  
  // Validate inconsistent trace
  let inconsistent_result = TraceConsistencyValidator::validate(trace_validator, inconsistent_trace)
  match inconsistent_result {
    ValidationResult::Valid => assert_true(false),
    ValidationResult::Invalid(errors) => {
      assert_true(errors.length() >= 2) // Should have at least 2 errors
      assert_true(errors.any(|e| e.contains("parent")))
      assert_true(errors.any(|e| e.contains("trace_id")))
    }
    ValidationResult::Warning(_) => assert_true(false) // Should be errors, not warnings
  }
}

// Test 4: Trace Sampling Consistency
test "trace sampling consistency" {
  let sampling_manager = TraceSamplingManager::new()
  
  // Configure consistent sampling
  let sampling_config = SamplingConfig::new()
    .with_sampling_strategy(TraceIdRatioBased(0.1)) // 10% sampling
    .with_consistent_sampling_across_services(true)
    .with_parent_based_sampling(true)
  
  TraceSamplingManager::configure(sampling_manager, sampling_config)
  
  // Create root span with sampling decision
  let root_trace_id = "sampling_trace_001"
  let root_sampling_decision = TraceSamplingManager::should_sample(sampling_manager, root_trace_id, "root_service")
  
  // Create child spans in different services
  let service_a_sampling_decision = TraceSamplingManager::should_sample_for_child(
    sampling_manager,
    root_trace_id,
    "service-a",
    root_sampling_decision
  )
  
  let service_b_sampling_decision = TraceSamplingManager::should_sample_for_child(
    sampling_manager,
    root_trace_id,
    "service-b",
    root_sampling_decision
  )
  
  let service_c_sampling_decision = TraceSamplingManager::should_sample_for_child(
    sampling_manager,
    root_trace_id,
    "service-c",
    root_sampling_decision
  )
  
  // Verify sampling consistency
  assert_eq(root_sampling_decision.is_sampled, service_a_sampling_decision.is_sampled)
  assert_eq(root_sampling_decision.is_sampled, service_b_sampling_decision.is_sampled)
  assert_eq(root_sampling_decision.is_sampled, service_c_sampling_decision.is_sampled)
  
  // Verify sampling attributes are consistent
  assert_eq(root_sampling_decision.attributes, service_a_sampling_decision.attributes)
  assert_eq(root_sampling_decision.attributes, service_b_sampling_decision.attributes)
  assert_eq(root_sampling_decision.attributes, service_c_sampling_decision.attributes)
  
  // Test with multiple traces
  let mut sampled_traces = 0
  let mut total_traces = 100
  
  for i in 0..total_traces {
    let trace_id = "trace_" + i.to_string()
    let sampling_decision = TraceSamplingManager::should_sample(sampling_manager, trace_id, "test_service")
    
    if sampling_decision.is_sampled {
      sampled_traces = sampled_traces + 1
    }
    
    // Verify child spans have same sampling decision
    let child_decision = TraceSamplingManager::should_sample_for_child(
      sampling_manager,
      trace_id,
      "child_service",
      sampling_decision
    )
    
    assert_eq(sampling_decision.is_sampled, child_decision.is_sampled)
  }
  
  // Verify sampling rate is approximately 10%
  let actual_sampling_rate = sampled_traces.to_float() / total_traces.to_float()
  assert_true(actual_sampling_rate > 0.05 && actual_sampling_rate < 0.15) // Allow some variance
}

// Test 5: Distributed Trace Timeline Consistency
test "distributed trace timeline consistency" {
  let timeline_validator = TimelineConsistencyValidator::new()
  
  // Configure timeline validation
  let timeline_config = TimelineConfig::new()
    .with_max_clock_skew(Duration::seconds(5)) // Allow 5 seconds of clock skew
    .with_require_parent_before_child(true)
    .with_require_span_duration_positive(true)
  
  TimelineConsistencyValidator::configure(timeline_validator, timeline_config)
  
  // Create a trace with consistent timeline
  let base_time = Time::now()
  
  let consistent_trace = DistributedTrace::new("timeline_consistent_trace")
    .add_span(Span::new("root", Server,
      TraceContext::new("timeline_consistent_trace", "span_1", true, [])
        .with_timestamp(base_time)
        .with_duration(Duration::milliseconds(1000))
    ))
    .add_span(Span::new("service_a", Client,
      TraceContext::new("timeline_consistent_trace", "span_2", true, [])
        .with_parent_span_id("span_1")
        .with_timestamp(base_time + Duration::milliseconds(100))
        .with_duration(Duration::milliseconds(500))
    ))
    .add_span(Span::new("service_b", Client,
      TraceContext::new("timeline_consistent_trace", "span_3", true, [])
        .with_parent_span_id("span_2")
        .with_timestamp(base_time + Duration::milliseconds(200))
        .with_duration(Duration::milliseconds(300))
    ))
  
  // Validate consistent timeline
  let consistent_result = TimelineConsistencyValidator::validate(timeline_validator, consistent_trace)
  match consistent_result {
    ValidationResult::Valid => assert_true(true),
    ValidationResult::Invalid(errors) => assert_true(false, "Consistent timeline should be valid: " + errors.join(", "))
    ValidationResult::Warning(warnings) => assert_true(true) // Warnings are acceptable
  }
  
  // Create a trace with inconsistent timeline (child starts before parent)
  let inconsistent_trace = DistributedTrace::new("timeline_inconsistent_trace")
    .add_span(Span::new("root", Server,
      TraceContext::new("timeline_inconsistent_trace", "span_1", true, [])
        .with_timestamp(base_time)
        .with_duration(Duration::milliseconds(1000))
    ))
    .add_span(Span::new("service_a", Client,
      TraceContext::new("timeline_inconsistent_trace", "span_2", true, [])
        .with_parent_span_id("span_1")
        .with_timestamp(base_time - Duration::milliseconds(100)) // Starts before parent
        .with_duration(Duration::milliseconds(500))
    ))
    .add_span(Span::new("service_b", Client,
      TraceContext::new("timeline_inconsistent_trace", "span_3", true, [])
        .with_parent_span_id("span_2")
        .with_timestamp(base_time + Duration::milliseconds(200))
        .with_duration(Duration::milliseconds(300))
    ))
  
  // Validate inconsistent timeline
  let inconsistent_result = TimelineConsistencyValidator::validate(timeline_validator, inconsistent_trace)
  match inconsistent_result {
    ValidationResult::Valid => assert_true(false),
    ValidationResult::Invalid(errors) => {
      assert_true(errors.length() >= 1)
      assert_true(errors.any(|e| e.contains("timeline") || e.contains("parent")))
    }
    ValidationResult::Warning(_) => assert_true(false)
  }
}

// Test 6: Cross-Service Attribute Consistency
test "cross-service attribute consistency" {
  let attribute_manager = CrossServiceAttributeManager::new()
  
  // Configure attribute consistency rules
  let attribute_rules = AttributeRules::new()
    .add_required_attribute("service.name")
    .add_required_attribute("service.version")
    .add_required_attribute("service.instance.id")
    .add_standardized_attribute("http.method")
    .add_standardized_attribute("http.status_code")
    .add_standardized_attribute("http.url")
    .add_propagated_attribute("user.id")
    .add_propagated_attribute("request.id")
    .add_propagated_attribute("tenant.id")
  
  CrossServiceAttributeManager::configure(attribute_manager, attribute_rules)
  
  // Create spans with consistent attributes
  let consistent_spans = [
    Span::new("frontend_request", Server,
      TraceContext::new("attr_consistent_trace", "span_1", true, [])
        .with_attributes([
          ("service.name", StringValue("frontend-service")),
          ("service.version", StringValue("1.2.3")),
          ("service.instance.id", StringValue("frontend-01")),
          ("http.method", StringValue("GET")),
          ("http.url", StringValue("/api/users")),
          ("user.id", StringValue("user_123")),
          ("request.id", StringValue("req_456"))
        ])
    ),
    Span::new("api_call", Client,
      TraceContext::new("attr_consistent_trace", "span_2", true, [])
        .with_parent_span_id("span_1")
        .with_attributes([
          ("service.name", StringValue("api-service")),
          ("service.version", StringValue("2.1.0")),
          ("service.instance.id", StringValue("api-01")),
          ("http.method", StringValue("GET")),
          ("http.url", StringValue("/api/users")),
          ("user.id", StringValue("user_123")),
          ("request.id", StringValue("req_456"))
        ])
    ),
    Span::new("database_query", Client,
      TraceContext::new("attr_consistent_trace", "span_3", true, [])
        .with_parent_span_id("span_2")
        .with_attributes([
          ("service.name", StringValue("database-service")),
          ("service.version", StringValue("3.0.1")),
          ("service.instance.id", StringValue("db-01")),
          ("db.statement", StringValue("SELECT * FROM users")),
          ("user.id", StringValue("user_123")),
          ("request.id", StringValue("req_456"))
        ])
    )
  ]
  
  // Validate consistent attributes
  let consistent_result = CrossServiceAttributeManager::validate_consistency(attribute_manager, consistent_spans)
  match consistent_result {
    ValidationResult::Valid => assert_true(true),
    ValidationResult::Invalid(errors) => assert_true(false, "Consistent attributes should be valid: " + errors.join(", "))
    ValidationResult::Warning(warnings) => assert_true(true) // Warnings are acceptable
  }
  
  // Create spans with inconsistent attributes
  let inconsistent_spans = [
    Span::new("frontend_request", Server,
      TraceContext::new("attr_inconsistent_trace", "span_1", true, [])
        .with_attributes([
          ("service.name", StringValue("frontend-service")),
          // Missing service.version
          ("service.instance.id", StringValue("frontend-01")),
          ("http.method", StringValue("GET")),
          ("http.url", StringValue("/api/users")),
          ("user.id", StringValue("user_123")),
          ("request.id", StringValue("req_456"))
        ])
    ),
    Span::new("api_call", Client,
      TraceContext::new("attr_inconsistent_trace", "span_2", true, [])
        .with_parent_span_id("span_1")
        .with_attributes([
          ("service.name", StringValue("api-service")),
          ("service.version", StringValue("2.1.0")),
          ("service.instance.id", StringValue("api-01")),
          // Non-standard HTTP method attribute
          ("method", StringValue("GET")),
          ("http.url", StringValue("/api/users")),
          ("user.id", StringValue("user_456")), // Different user ID
          ("request.id", StringValue("req_456"))
        ])
    )
  ]
  
  // Validate inconsistent attributes
  let inconsistent_result = CrossServiceAttributeManager::validate_consistency(attribute_manager, inconsistent_spans)
  match inconsistent_result {
    ValidationResult::Valid => assert_true(false),
    ValidationResult::Invalid(errors) => {
      assert_true(errors.length() >= 3) // Should have at least 3 errors
      assert_true(errors.any(|e| e.contains("service.version")))
      assert_true(errors.any(|e| e.contains("http.method")))
      assert_true(errors.any(|e| e.contains("user.id")))
    }
    ValidationResult::Warning(_) => assert_true(false)
  }
}

// Test 7: Distributed Trace Aggregation Consistency
test "distributed trace aggregation consistency" {
  let aggregation_manager = DistributedTraceAggregationManager::new()
  
  // Configure aggregation
  let aggregation_config = AggregationConfig::new()
    .with_time_window(Duration::minutes(5))
    .with_group_by_attributes(["service.name", "operation.name"])
    .with_metrics(["duration", "error_rate", "throughput"])
    .with_percentiles([50.0, 90.0, 95.0, 99.0])
  
  DistributedTraceAggregationManager::configure(aggregation_manager, aggregation_config)
  
  // Generate distributed traces across multiple services
  let services = ["frontend", "api", "database", "cache", "auth"]
  let operations = ["get_user", "create_order", "update_profile", "delete_account"]
  
  let base_time = Time::now() - Duration::minutes(10)
  
  for i in 0..=100 {
    let trace_id = "aggregation_trace_" + i.to_string()
    let service = services[i % services.length()]
    let operation = operations[i % operations.length()]
    let timestamp = base_time + Duration::seconds(i * 6) // Every 6 seconds
    
    let is_error = i % 10 == 0 // 10% error rate
    let duration = if is_error {
      1000 + Math::random() * 2000 // Longer duration for errors
    } else {
      100 + Math::random() * 500
    }
    
    let trace = DistributedTrace::new(trace_id)
      .add_span(Span::new(operation, Server,
        TraceContext::new(trace_id, "span_1", true, [])
          .with_timestamp(timestamp)
          .with_duration(Duration::milliseconds(duration.to_int()))
          .with_attributes([
            ("service.name", StringValue(service)),
            ("operation.name", StringValue(operation)),
            ("error", BoolValue(is_error))
          ])
      ))
    
    DistributedTraceAggregationManager::add_trace(aggregation_manager, trace)
  }
  
  // Test aggregation results
  let aggregation_results = DistributedTraceAggregationManager::get_aggregations(aggregation_manager)
  assert_true(aggregation_results.length() > 0)
  
  // Verify aggregation consistency
  for aggregation in aggregation_results {
    let service_name = AggregationResult::get_attribute(aggregation, "service.name")
    let operation_name = AggregationResult::get_attribute(aggregation, "operation.name")
    
    match (service_name, operation_name) {
      (Some(StringValue(service)), Some(StringValue(operation))) => {
        // Verify metrics are present and consistent
        let duration_metric = AggregationResult::get_metric(aggregation, "duration")
        let error_rate_metric = AggregationResult::get_metric(aggregation, "error_rate")
        let throughput_metric = AggregationResult::get_metric(aggregation, "throughput")
        
        assert_true(duration_metric > 0.0)
        assert_true(error_rate_metric >= 0.0 && error_rate_metric <= 1.0)
        assert_true(throughput_metric > 0.0)
        
        // Verify percentiles are consistent
        let p50 = AggregationResult::get_percentile(aggregation, "duration", 50.0)
        let p90 = AggregationResult::get_percentile(aggregation, "duration", 90.0)
        let p95 = AggregationResult::get_percentile(aggregation, "duration", 95.0)
        let p99 = AggregationResult::get_percentile(aggregation, "duration", 99.0)
        
        assert_true(p50 <= p90)
        assert_true(p90 <= p95)
        assert_true(p95 <= p99)
      }
      _ => assert_true(false)
    }
  }
  
  // Test cross-service aggregation consistency
  let cross_service_results = DistributedTraceAggregationManager::get_cross_service_aggregations(aggregation_manager)
  assert_true(cross_service_results.length() >= services.length())
  
  // Verify service-level metrics are consistent with operation-level metrics
  for service_result in cross_service_results {
    let service_name = AggregationResult::get_attribute(service_result, "service.name")
    match service_name {
      Some(StringValue(service)) => {
        // Get all operation-level aggregations for this service
        let service_operations = aggregation_results.filter(|r| {
          match AggregationResult::get_attribute(r, "service.name") {
            Some(StringValue(s)) => s == service
            _ => false
          }
        })
        
        if service_operations.length() > 0 {
          // Verify service-level error rate is within bounds of operation-level error rates
          let service_error_rate = AggregationResult::get_metric(service_result, "error_rate")
          let operation_error_rates = service_operations.map(|o| AggregationResult::get_metric(o, "error_rate"))
          
          let min_op_error_rate = operation_error_rates.reduce(Math::min)
          let max_op_error_rate = operation_error_rates.reduce(Math::max)
          
          assert_true(service_error_rate >= min_op_error_rate * 0.9) // Allow some variance
          assert_true(service_error_rate <= max_op_error_rate * 1.1) // Allow some variance
        }
      }
      _ => assert_true(false)
    }
  }
}

// Test 8: Distributed Trace Reconstruction
test "distributed trace reconstruction" {
  let trace_reconstructor = DistributedTraceReconstructor::new()
  
  // Configure reconstruction
  let reconstruction_config = ReconstructionConfig::new()
    .with_span_timeout(Duration::seconds(30))
    .with_max_spans_per_trace(1000)
    .with_require_root_span(true)
    .with_validate_timeline(true)
    .with_fill_missing_attributes(true)
  
  DistributedTraceReconstructor::configure(trace_reconstructor, reconstruction_config)
  
  // Simulate distributed trace collection from multiple services
  let trace_id = "reconstruction_trace_001"
  let base_time = Time::now() - Duration::minutes(1)
  
  // Spans collected from different services at different times
  let spans = [
    // From frontend service
    Span::new("http_request", Server,
      TraceContext::new(trace_id, "span_1", true, [])
        .with_timestamp(base_time)
        .with_duration(Duration::milliseconds(1000))
        .with_attributes([
          ("service.name", StringValue("frontend")),
          ("http.method", StringValue("POST")),
          ("http.url", StringValue("/api/orders"))
        ])
    ),
    
    // From API service
    Span::new("process_order", Server,
      TraceContext::new(trace_id, "span_2", true, [])
        .with_parent_span_id("span_1")
        .with_timestamp(base_time + Duration::milliseconds(100))
        .with_duration(Duration::milliseconds(800))
        .with_attributes([
          ("service.name", StringValue("api")),
          ("operation.name", StringValue("process_order"))
        ])
    ),
    
    // From database service
    Span::new("db_query", Client,
      TraceContext::new(trace_id, "span_3", true, [])
        .with_parent_span_id("span_2")
        .with_timestamp(base_time + Duration::milliseconds(200))
        .with_duration(Duration::milliseconds(300))
        .with_attributes([
          ("service.name", StringValue("database")),
          ("db.statement", StringValue("INSERT INTO orders"))
        ])
    ),
    
    // From cache service
    Span::new("cache_get", Client,
      TraceContext::new(trace_id, "span_4", true, [])
        .with_parent_span_id("span_2")
        .with_timestamp(base_time + Duration::milliseconds(150))
        .with_duration(Duration::milliseconds(50))
        .with_attributes([
          ("service.name", StringValue("cache")),
          ("cache.key", StringValue("user_123"))
        ])
    ),
    
    // From auth service
    Span::new("authorize", Client,
      TraceContext::new(trace_id, "span_5", true, [])
        .with_parent_span_id("span_2")
        .with_timestamp(base_time + Duration::milliseconds(120))
        .with_duration(Duration::milliseconds(100))
        .with_attributes([
          ("service.name", StringValue("auth")),
          ("auth.user", StringValue("user_123"))
        ])
    )
  ]
  
  // Add spans to reconstructor in random order (simulating out-of-order arrival)
  let span_indices = [3, 0, 4, 2, 1] // Random order
  for i in span_indices {
    DistributedTraceReconstructor::add_span(trace_reconstructor, spans[i])
  }
  
  // Test trace reconstruction
  let reconstructed_trace = DistributedTraceReconstructor::reconstruct(trace_reconstructor, trace_id)
  match reconstructed_trace {
    Some(trace) => {
      // Verify trace structure
      assert_eq(DistributedTrace::trace_id(trace), trace_id)
      assert_eq(DistributedTrace::span_count(trace), 5)
      
      // Verify span relationships
      let root_span = DistributedTrace::get_root_span(trace)
      match root_span {
        Some(span) => {
          assert_eq(Span::name(span), "http_request")
          assert_eq(Span::get_attribute(span, "service.name"), Some(StringValue("frontend")))
        }
        None => assert_true(false)
      }
      
      // Verify span hierarchy
      let children_of_root = DistributedTrace::get_child_spans(trace, "span_1")
      assert_eq(children_of_root.length(), 1)
      assert_eq(Span::name(children_of_root[0]), "process_order")
      
      let children_of_process = DistributedTrace::get_child_spans(trace, "span_2")
      assert_eq(children_of_process.length(), 3)
      
      let child_names = children_of_process.map(|s| Span::name(s))
      assert_true(child_names.contains("db_query"))
      assert_true(child_names.contains("cache_get"))
      assert_true(child_names.contains("authorize"))
      
      // Verify timeline consistency
      assert_true(DistributedTrace::is_timeline_consistent(trace))
      
      // Verify attribute consistency
      assert_true(DistributedTrace::are_attributes_consistent(trace))
      
      // Verify reconstructed trace metrics
      let trace_metrics = DistributedTrace::calculate_metrics(trace)
      assert_true(TraceMetrics::total_duration(trace_metrics) > 0)
      assert_eq(TraceMetrics::span_count(trace_metrics), 5)
      assert_eq(TraceMetrics::service_count(trace_metrics), 5)
    }
    None => assert_true(false)
  }
  
  // Test incomplete trace reconstruction (missing spans)
  let incomplete_trace_id = "incomplete_trace_001"
  
  // Add only some spans
  DistributedTraceReconstructor::add_span(trace_reconstructor, 
    Span::new("root", Server,
      TraceContext::new(incomplete_trace_id, "span_1", true, [])
        .with_timestamp(base_time)
    )
  )
  
  DistributedTraceReconstructor::add_span(trace_reconstructor,
    Span::new("child", Client,
      TraceContext::new(incomplete_trace_id, "span_2", true, [])
        .with_parent_span_id("span_1")
        .with_timestamp(base_time + Duration::milliseconds(100))
    )
  )
  
  // Try to reconstruct incomplete trace
  let incomplete_result = DistributedTraceReconstructor::reconstruct(trace_reconstructor, incomplete_trace_id)
  match incomplete_result {
    Some(trace) => {
      // Should still reconstruct what's available
      assert_eq(DistributedTrace::span_count(trace), 2)
      assert_true(DistributedTrace::is_incomplete(trace))
    }
    None => assert_true(false)
  }
  
  // Test reconstruction metrics
  let reconstruction_metrics = DistributedTraceReconstructor::get_metrics(trace_reconstructor)
  assert_true(ReconstructionMetrics::successful_reconstructions(reconstruction_metrics) >= 1)
  assert_true(ReconstructionMetrics::incomplete_traces(reconstruction_metrics) >= 1)
}