// Azimuth Telemetry System - Cross-Service Consistency Comprehensive Tests
// This file contains comprehensive test cases for cross-service consistency in telemetry systems

// Test 1: Trace Context Propagation Across Services
test "trace context propagation across services" {
  let consistency_manager = CrossServiceConsistencyManager::new()
  
  // Create initial trace context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let initial_span_id = "b7ad6b7169203331"
  let span_context = SpanContext::new(trace_id, initial_span_id, true, "initial-state")
  
  // Simulate service A processing
  let service_a_span = CrossServiceConsistencyManager::create_child_span(
    consistency_manager, 
    span_context, 
    "service-a-operation",
    "service-a"
  )
  
  let service_a_context = Span::span_context(service_a_span)
  assert_eq(SpanContext::trace_id(service_a_context), trace_id)
  assert_ne(SpanContext::span_id(service_a_context), initial_span_id)
  
  // Simulate service B processing with context from service A
  let service_b_span = CrossServiceConsistencyManager::create_child_span(
    consistency_manager, 
    service_a_context, 
    "service-b-operation",
    "service-b"
  )
  
  let service_b_context = Span::span_context(service_b_span)
  assert_eq(SpanContext::trace_id(service_b_context), trace_id)
  assert_ne(SpanContext::span_id(service_b_context), SpanContext::span_id(service_a_context))
  
  // Simulate service C processing with context from service B
  let service_c_span = CrossServiceConsistencyManager::create_child_span(
    consistency_manager, 
    service_b_context, 
    "service-c-operation",
    "service-c"
  )
  
  let service_c_context = Span::span_context(service_c_span)
  assert_eq(SpanContext::trace_id(service_c_context), trace_id)
  assert_ne(SpanContext::span_id(service_c_context), SpanContext::span_id(service_b_context))
  
  // Verify trace context consistency across all services
  let trace_chain = CrossServiceConsistencyManager::get_trace_chain(consistency_manager, trace_id)
  assert_eq(trace_chain.length(), 4) // Initial + 3 services
  
  // Verify parent-child relationships
  assert_eq(trace_chain[0].span_id, initial_span_id)
  assert_eq(trace_chain[1].parent_span_id, initial_span_id)
  assert_eq(trace_chain[2].parent_span_id, SpanContext::span_id(service_a_context))
  assert_eq(trace_chain[3].parent_span_id, SpanContext::span_id(service_b_context))
  
  // Verify all spans belong to the same trace
  for span_info in trace_chain {
    assert_eq(span_info.trace_id, trace_id)
  }
}

// Test 2: Baggage Propagation and Consistency
test "baggage propagation and consistency" {
  let consistency_manager = CrossServiceConsistencyManager::new()
  
  // Create initial baggage
  let initial_baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(initial_baggage, "user.id", "user-123")
  let updated_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-456")
  let updated_baggage = Baggage::set_entry(updated_baggage, "session.id", "session-789")
  
  // Simulate service A adding baggage entries
  let service_a_baggage = CrossServiceConsistencyManager::propagate_baggage(
    consistency_manager, 
    updated_baggage, 
    "service-a"
  )
  
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "service.a.timestamp", "1609459200")
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "service.a.operation", "process-data")
  
  // Simulate service B adding baggage entries
  let service_b_baggage = CrossServiceConsistencyManager::propagate_baggage(
    consistency_manager, 
    service_a_baggage, 
    "service-b"
  )
  
  let service_b_baggage = Baggage::set_entry(service_b_baggage, "service.b.timestamp", "1609459205")
  let service_b_baggage = Baggage::set_entry(service_b_baggage, "service.b.operation", "validate-data")
  
  // Simulate service C adding baggage entries
  let service_c_baggage = CrossServiceConsistencyManager::propagate_baggage(
    consistency_manager, 
    service_b_baggage, 
    "service-c"
  )
  
  let service_c_baggage = Baggage::set_entry(service_c_baggage, "service.c.timestamp", "1609459210")
  let service_c_baggage = Baggage::set_entry(service_c_baggage, "service.c.operation", "store-data")
  
  // Verify baggage propagation consistency
  let final_baggage = service_c_baggage
  
  // Verify original entries are preserved
  match Baggage::get_entry(final_baggage, "user.id") {
    Some(value) => assert_eq(value, "user-123")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(final_baggage, "request.id") {
    Some(value) => assert_eq(value, "req-456")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(final_baggage, "session.id") {
    Some(value) => assert_eq(value, "session-789")
    None => assert_true(false)
  }
  
  // Verify service-specific entries are added
  match Baggage::get_entry(final_baggage, "service.a.operation") {
    Some(value) => assert_eq(value, "process-data")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(final_baggage, "service.b.operation") {
    Some(value) => assert_eq(value, "validate-data")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(final_baggage, "service.c.operation") {
    Some(value) => assert_eq(value, "store-data")
    None => assert_true(false)
  }
  
  // Verify baggage consistency across services
  let baggage_history = CrossServiceConsistencyManager::get_baggage_history(consistency_manager, "req-456")
  assert_eq(baggage_history.length(), 4) // Initial + 3 services
  
  // Verify baggage entry propagation
  for baggage_state in baggage_history {
    match Baggage::get_entry(baggage_state.baggage, "user.id") {
      Some(value) => assert_eq(value, "user-123")
      None => assert_true(false)
    }
  }
}

// Test 3: Metric Consistency Across Services
test "metric consistency across services" {
  let consistency_manager = CrossServiceConsistencyManager::new()
  
  // Create metric definitions that should be consistent across services
  let metric_definitions = [
    MetricDefinition {
      name: "request.count",
      type: "counter",
      unit: "count",
      description: "Total number of requests"
    },
    MetricDefinition {
      name: "request.duration",
      type: "histogram",
      unit: "ms",
      description: "Request duration in milliseconds"
    },
    MetricDefinition {
      name: "error.count",
      type: "counter",
      unit: "count",
      description: "Total number of errors"
    }
  ]
  
  // Register metric definitions
  for definition in metric_definitions {
    CrossServiceConsistencyManager::register_metric_definition(consistency_manager, definition)
  }
  
  // Simulate service A metrics
  let service_a_metrics = CrossServiceConsistencyManager::get_metrics(consistency_manager, "service-a")
  
  let request_counter_a = Metrics::create_counter(service_a_metrics, "request.count")
  let duration_histogram_a = Metrics::create_histogram(service_a_metrics, "request.duration")
  let error_counter_a = Metrics::create_counter(service_a_metrics, "error.count")
  
  // Record metrics in service A
  Counter::add(request_counter_a, 100.0)
  Histogram::record(duration_histogram_a, 150.0)
  Histogram::record(duration_histogram_a, 200.0)
  Histogram::record(duration_histogram_a, 100.0)
  Counter::add(error_counter_a, 5.0)
  
  // Simulate service B metrics
  let service_b_metrics = CrossServiceConsistencyManager::get_metrics(consistency_manager, "service-b")
  
  let request_counter_b = Metrics::create_counter(service_b_metrics, "request.count")
  let duration_histogram_b = Metrics::create_histogram(service_b_metrics, "request.duration")
  let error_counter_b = Metrics::create_counter(service_b_metrics, "error.count")
  
  // Record metrics in service B
  Counter::add(request_counter_b, 80.0)
  Histogram::record(duration_histogram_b, 120.0)
  Histogram::record(duration_histogram_b, 180.0)
  Counter::add(error_counter_b, 3.0)
  
  // Simulate service C metrics
  let service_c_metrics = CrossServiceConsistencyManager::get_metrics(consistency_manager, "service-c")
  
  let request_counter_c = Metrics::create_counter(service_c_metrics, "request.count")
  let duration_histogram_c = Metrics::create_histogram(service_c_metrics, "request.duration")
  let error_counter_c = Metrics::create_counter(service_c_metrics, "error.count")
  
  // Record metrics in service C
  Counter::add(request_counter_c, 120.0)
  Histogram::record(duration_histogram_c, 200.0)
  Histogram::record(duration_histogram_c, 250.0)
  Histogram::record(duration_histogram_c, 150.0)
  Counter::add(error_counter_c, 7.0)
  
  // Verify metric consistency across services
  let cross_service_metrics = CrossServiceConsistencyManager::aggregate_metrics(consistency_manager)
  
  // Verify aggregated request count
  let total_requests = cross_service_metrics.get("request.count")
  match total_requests {
    Some(value) => assert_eq(value, 300.0) // 100 + 80 + 120
    None => assert_true(false)
  }
  
  // Verify aggregated error count
  let total_errors = cross_service_metrics.get("error.count")
  match total_errors {
    Some(value) => assert_eq(value, 15.0) // 5 + 3 + 7
    None => assert_true(false)
  }
  
  // Verify metric definitions are consistent
  let service_a_definitions = CrossServiceConsistencyManager::get_metric_definitions(consistency_manager, "service-a")
  let service_b_definitions = CrossServiceConsistencyManager::get_metric_definitions(consistency_manager, "service-b")
  let service_c_definitions = CrossServiceConsistencyManager::get_metric_definitions(consistency_manager, "service-c")
  
  assert_eq(service_a_definitions.length(), service_b_definitions.length())
  assert_eq(service_b_definitions.length(), service_c_definitions.length())
  
  // Verify all services have the same metric definitions
  for i in 0..service_a_definitions.length() {
    assert_eq(service_a_definitions[i].name, service_b_definitions[i].name)
    assert_eq(service_a_definitions[i].type, service_b_definitions[i].type)
    assert_eq(service_a_definitions[i].name, service_c_definitions[i].name)
    assert_eq(service_a_definitions[i].type, service_c_definitions[i].type)
  }
}

// Test 4: Log Correlation Across Services
test "log correlation across services" {
  let consistency_manager = CrossServiceConsistencyManager::new()
  
  // Create correlated log context
  let correlation_id = "corr-123456789"
  let trace_id = "trace-987654321"
  
  // Simulate service A logging with correlation
  let service_a_logger = CrossServiceConsistencyManager::get_logger(consistency_manager, "service-a")
  
  let log_record_a = LogRecord::new_with_correlation(
    Info, 
    "Processing request in service A", 
    correlation_id, 
    trace_id, 
    Some("span-a")
  )
  
  Logger::emit(service_a_logger, log_record_a)
  
  // Simulate service B logging with correlation
  let service_b_logger = CrossServiceConsistencyManager::get_logger(consistency_manager, "service-b")
  
  let log_record_b = LogRecord::new_with_correlation(
    Info, 
    "Validating request in service B", 
    correlation_id, 
    trace_id, 
    Some("span-b")
  )
  
  Logger::emit(service_b_logger, log_record_b)
  
  // Simulate service C logging with correlation
  let service_c_logger = CrossServiceConsistencyManager::get_logger(consistency_manager, "service-c")
  
  let log_record_c = LogRecord::new_with_correlation(
    Warn, 
    "Request validation failed in service C", 
    correlation_id, 
    trace_id, 
    Some("span-c")
  )
  
  Logger::emit(service_c_logger, log_record_c)
  
  // Verify log correlation across services
  let correlated_logs = CrossServiceConsistencyManager::get_correlated_logs(consistency_manager, correlation_id)
  assert_eq(correlated_logs.length(), 3)
  
  // Verify all logs have the same correlation ID and trace ID
  for log in correlated_logs {
    assert_eq(log.correlation_id, correlation_id)
    assert_eq(log.trace_id, trace_id)
  }
  
  // Verify log ordering by timestamp
  for i in 1..correlated_logs.length() {
    assert_true(correlated_logs[i-1].timestamp <= correlated_logs[i].timestamp)
  }
  
  // Verify cross-service log analysis
  let log_analysis = CrossServiceConsistencyManager::analyze_cross_service_logs(consistency_manager, trace_id)
  
  // Verify service participation
  assert_true(log_analysis.service_logs.contains("service-a"))
  assert_true(log_analysis.service_logs.contains("service-b"))
  assert_true(log_analysis.service_logs.contains("service-c"))
  
  // Verify error detection
  assert_true(log_analysis.has_errors)
  assert_eq(log_analysis.error_count, 1)
  
  // Verify log severity distribution
  assert_eq(log_analysis.severity_counts.get("info"), Some(2))
  assert_eq(log_analysis.severity_counts.get("warn"), Some(1))
}

// Test 5: Resource Consistency Across Services
test "resource consistency across services" {
  let consistency_manager = CrossServiceConsistencyManager::new()
  
  // Define common resource attributes
  let common_resource_attributes = [
    ("service.namespace", "production"),
    ("service.version", "1.2.3"),
    ("deployment.environment", "prod"),
    ("host.name", "prod-server-01"),
    ("cloud.provider", "aws"),
    ("cloud.region", "us-west-2")
  ]
  
  // Simulate service A resource
  let service_a_resource = CrossServiceConsistencyManager::create_resource(
    consistency_manager, 
    "service-a", 
    common_resource_attributes
  )
  
  Resource::set_attribute(service_a_resource, "service.instance.id", "instance-a-123")
  Resource::set_attribute(service_a_resource, "service.name", "service-a")
  Resource::set_attribute(service_a_resource, "service.type", "web")
  
  // Simulate service B resource
  let service_b_resource = CrossServiceConsistencyManager::create_resource(
    consistency_manager, 
    "service-b", 
    common_resource_attributes
  )
  
  Resource::set_attribute(service_b_resource, "service.instance.id", "instance-b-456")
  Resource::set_attribute(service_b_resource, "service.name", "service-b")
  Resource::set_attribute(service_b_resource, "service.type", "database")
  
  // Simulate service C resource
  let service_c_resource = CrossServiceConsistencyManager::create_resource(
    consistency_manager, 
    "service-c", 
    common_resource_attributes
  )
  
  Resource::set_attribute(service_c_resource, "service.instance.id", "instance-c-789")
  Resource::set_attribute(service_c_resource, "service.name", "service-c")
  Resource::set_attribute(service_c_resource, "service.type", "cache")
  
  // Verify resource consistency across services
  let resource_consistency = CrossServiceConsistencyManager::verify_resource_consistency(consistency_manager)
  
  // Verify common attributes are consistent
  for (key, expected_value) in common_resource_attributes {
    assert_true(resource_consistency.consistent_attributes.contains(key))
  }
  
  // Verify service-specific attributes are different
  assert_ne(
    Resource::get_attribute(service_a_resource, "service.instance.id"),
    Resource::get_attribute(service_b_resource, "service.instance.id")
  )
  
  assert_ne(
    Resource::get_attribute(service_b_resource, "service.instance.id"),
    Resource::get_attribute(service_c_resource, "service.instance.id")
  )
  
  // Verify resource merging
  let merged_resource = CrossServiceConsistencyManager::merge_resources(consistency_manager, [
    "service-a", "service-b", "service-c"
  ])
  
  // Verify merged resource contains all service information
  match Resource::get_attribute(merged_resource, "service.namespace") {
    Some(StringValue(value)) => assert_eq(value, "production")
    _ => assert_true(false)
  }
  
  // Verify service count in merged resource
  match Resource::get_attribute(merged_resource, "service.count") {
    Some(IntValue(count)) => assert_eq(count, 3)
    _ => assert_true(false)
  }
}

// Test 6: Context Propagation Format Consistency
test "context propagation format consistency" {
  let consistency_manager = CrossServiceConsistencyManager::new()
  
  // Create test context
  let trace_id = "trace-123456789"
  let span_id = "span-987654321"
  let span_context = SpanContext::new(trace_id, span_id, true, "test-state")
  
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "test.key", "test.value")
  
  // Test W3C trace context format
  let w3c_traceparent = CrossServiceConsistencyManager::format_traceparent(consistency_manager, span_context)
  assert_true(w3c_traceparent.starts_with("00-"))
  assert_true(w3c_traceparent.contains(trace_id))
  assert_true(w3c_traceparent.contains(span_id))
  
  let w3c_tracestate = CrossServiceConsistencyManager::format_tracestate(consistency_manager, "test-state")
  assert_eq(w3c_tracestate, "test-state")
  
  // Test B3 format
  let b3_trace_id = CrossServiceConsistencyManager::format_b3_trace_id(consistency_manager, span_context)
  let b3_span_id = CrossServiceConsistencyManager::format_b3_span_id(consistency_manager, span_context)
  let b3_sampled = CrossServiceConsistencyManager::format_b3_sampled(consistency_manager, span_context)
  
  assert_eq(b3_trace_id, trace_id)
  assert_eq(b3_span_id, span_id)
  assert_eq(b3_sampled, "1")
  
  // Test baggage header format
  let baggage_header = CrossServiceConsistencyManager::format_baggage_header(consistency_manager, baggage)
  assert_true(baggage_header.contains("test.key=test.value"))
  
  // Test context parsing consistency
  let parsed_traceparent = CrossServiceConsistencyManager::parse_traceparent(consistency_manager, w3c_traceparent)
  match parsed_traceparent {
    Success(parsed_context) => {
      assert_eq(SpanContext::trace_id(parsed_context), trace_id)
      assert_eq(SpanContext::span_id(parsed_context), span_id)
      assert_true(SpanContext::is_sampled(parsed_context))
    }
    Error(_) => assert_true(false)
  }
  
  let parsed_baggage = CrossServiceConsistencyManager::parse_baggage_header(consistency_manager, baggage_header)
  match parsed_baggage {
    Success(parsed_entries) => {
      assert_true(parsed_entries.contains("test.key"))
      match parsed_entries.get("test.key") {
        Some(value) => assert_eq(value, "test.value")
        None => assert_true(false)
      }
    }
    Error(_) => assert_true(false)
  }
  
  // Test cross-format consistency
  let w3c_context = CrossServiceConsistencyManager::extract_from_w3c_headers(consistency_manager, w3c_traceparent, w3c_tracestate, baggage_header)
  let b3_context = CrossServiceConsistencyManager::extract_from_b3_headers(consistency_manager, b3_trace_id, b3_span_id, b3_sampled, baggage_header)
  
  // Both should result in equivalent contexts
  assert_eq(SpanContext::trace_id(w3c_context), SpanContext::trace_id(b3_context))
  assert_eq(SpanContext::span_id(w3c_context), SpanContext::span_id(b3_context))
  assert_eq(SpanContext::is_sampled(w3c_context), SpanContext::is_sampled(b3_context))
}

// Test 7: Sampling Consistency Across Services
test "sampling consistency across services" {
  let consistency_manager = CrossServiceConsistencyManager::new()
  
  // Configure consistent sampling strategy
  let sampling_config = SamplingConfig {
    strategy: "trace-id-ratio",
    probability: 0.1, // 10% sampling
    consistent: true
  }
  
  CrossServiceConsistencyManager::configure_sampling(consistency_manager, sampling_config)
  
  // Generate test trace IDs
  let test_trace_ids = [
    "trace-00000000000000000000000000000001",
    "trace-00000000000000000000000000000002",
    "trace-00000000000000000000000000000003",
    "trace-00000000000000000000000000000004",
    "trace-00000000000000000000000000000005"
  ]
  
  // Test sampling decision consistency
  let mut sampling_decisions = []
  
  for trace_id in test_trace_ids {
    let span_context = SpanContext::new(trace_id, "span-123", true, "")
    let sampling_decision = CrossServiceConsistencyManager::make_sampling_decision(consistency_manager, span_context)
    sampling_decisions = sampling_decisions + [(trace_id, sampling_decision)]
  }
  
  // Verify sampling decisions are consistent across services
  for (trace_id, decision) in sampling_decisions {
    // Simulate different services making the same decision
    let service_a_decision = CrossServiceConsistencyManager::make_sampling_decision(consistency_manager, SpanContext::new(trace_id, "span-a", true, ""))
    let service_b_decision = CrossServiceConsistencyManager::make_sampling_decision(consistency_manager, SpanContext::new(trace_id, "span-b", true, ""))
    let service_c_decision = CrossServiceConsistencyManager::make_sampling_decision(consistency_manager, SpanContext::new(trace_id, "span-c", true, ""))
    
    // All services should make the same decision for the same trace
    assert_eq(service_a_decision, decision)
    assert_eq(service_b_decision, decision)
    assert_eq(service_c_decision, decision)
  }
  
  // Test parent-based sampling consistency
  let parent_span_context = SpanContext::new("parent-trace", "parent-span", true, "parent-state")
  let parent_sampling = CrossServiceConsistencyManager::make_sampling_decision(consistency_manager, parent_span_context)
  
  let child_span_context = SpanContext::new("parent-trace", "child-span", true, "child-state")
  let child_sampling = CrossServiceConsistencyManager::make_sampling_decision_for_child(consistency_manager, child_span_context, parent_span_context)
  
  // Child should respect parent's sampling decision
  assert_eq(child_sampling, parent_sampling)
  
  // Test sampling rate consistency
  let large_trace_ids = generate_trace_ids(10000)
  let mut sampled_count = 0
  
  for trace_id in large_trace_ids {
    let span_context = SpanContext::new(trace_id, "span-123", true, "")
    let sampling_decision = CrossServiceConsistencyManager::make_sampling_decision(consistency_manager, span_context)
    
    if sampling_decision {
      sampled_count = sampled_count + 1
    }
  }
  
  let actual_sampling_rate = sampled_count.to_float() / large_trace_ids.length().to_float()
  let expected_sampling_rate = sampling_config.probability
  
  // Actual rate should be close to expected rate (within 5% tolerance)
  assert_true((actual_sampling_rate - expected_sampling_rate).abs() < 0.05)
}

// Test 8: Error Handling Consistency Across Services
test "error handling consistency across services" {
  let consistency_manager = CrossServiceConsistencyManager::new()
  
  // Define consistent error handling strategy
  let error_handling_config = ErrorHandlingConfig {
    strategy: "consistent-propagation",
    include_stack_trace: true,
    include_context: true,
    sanitize_errors: true
  }
  
  CrossServiceConsistencyManager::configure_error_handling(consistency_manager, error_handling_config)
  
  // Simulate error in service A
  let service_a_error = CrossServiceConsistencyManager::create_error(
    consistency_manager,
    "service-a",
    "ValidationError",
    "Invalid input parameter",
    Some("parameter 'userId' is required")
  )
  
  // Simulate error in service B
  let service_b_error = CrossServiceConsistencyManager::create_error(
    consistency_manager,
    "service-b",
    "ValidationError",
    "Invalid input parameter",
    Some("parameter 'userId' is required")
  )
  
  // Simulate error in service C
  let service_c_error = CrossServiceConsistencyManager::create_error(
    consistency_manager,
    "service-c",
    "ValidationError",
    "Invalid input parameter",
    Some("parameter 'userId' is required")
  )
  
  // Verify error format consistency
  assert_eq(service_a_error.error_type, service_b_error.error_type)
  assert_eq(service_b_error.error_type, service_c_error.error_type)
  
  assert_eq(service_a_error.error_message, service_b_error.error_message)
  assert_eq(service_b_error.error_message, service_c_error.error_message)
  
  // Verify error context consistency
  assert_true(service_a_error.context.contains_key("service"))
  assert_true(service_b_error.context.contains_key("service"))
  assert_true(service_c_error.context.contains_key("service"))
  
  assert_eq(service_a_error.context.get("service"), Some("service-a"))
  assert_eq(service_b_error.context.get("service"), Some("service-b"))
  assert_eq(service_c_error.context.get("service"), Some("service-c"))
  
  // Test error correlation
  let correlation_id = "error-corr-123"
  let trace_id = "error-trace-456"
  
  let correlated_error_a = CrossServiceConsistencyManager::create_correlated_error(
    consistency_manager,
    "service-a",
    "ProcessingError",
    "Failed to process request",
    correlation_id,
    trace_id
  )
  
  let correlated_error_b = CrossServiceConsistencyManager::create_correlated_error(
    consistency_manager,
    "service-b",
    "ProcessingError",
    "Failed to process request",
    correlation_id,
    trace_id
  )
  
  // Verify error correlation
  assert_eq(correlated_error_a.correlation_id, correlated_error_b.correlation_id)
  assert_eq(correlated_error_a.trace_id, correlated_error_b.trace_id)
  
  // Test error aggregation
  let error_summary = CrossServiceConsistencyManager::aggregate_errors(consistency_manager, trace_id)
  
  assert_eq(error_summary.total_errors, 2)
  assert_eq(error_summary.unique_error_types, 1)
  assert_true(error_summary.affected_services.contains("service-a"))
  assert_true(error_summary.affected_services.contains("service-b"))
}

// Test 9: Performance Consistency Across Services
test "performance consistency across services" {
  let consistency_manager = CrossServiceConsistencyManager::new()
  
  // Define performance metrics to track
  let performance_metrics = [
    "request.duration",
    "response.size",
    "cpu.usage",
    "memory.usage"
  ]
  
  // Configure performance tracking
  for metric in performance_metrics {
    CrossServiceConsistencyManager::enable_performance_tracking(consistency_manager, metric)
  }
  
  // Simulate service A performance data
  let service_a_performance = CrossServiceConsistencyManager::record_performance(
    consistency_manager,
    "service-a",
    [
      ("request.duration", 150.0),
      ("response.size", 1024.0),
      ("cpu.usage", 45.5),
      ("memory.usage", 60.2)
    ]
  )
  
  // Simulate service B performance data
  let service_b_performance = CrossServiceConsistencyManager::record_performance(
    consistency_manager,
    "service-b",
    [
      ("request.duration", 200.0),
      ("response.size", 2048.0),
      ("cpu.usage", 55.3),
      ("memory.usage", 70.1)
    ]
  )
  
  // Simulate service C performance data
  let service_c_performance = CrossServiceConsistencyManager::record_performance(
    consistency_manager,
    "service-c",
    [
      ("request.duration", 100.0),
      ("response.size", 512.0),
      ("cpu.usage", 35.7),
      ("memory.usage", 50.8)
    ]
  )
  
  // Verify performance metric consistency
  let performance_summary = CrossServiceConsistencyManager::get_performance_summary(consistency_manager)
  
  // Verify all services are tracked
  assert_true(performance_summary.service_performance.contains_key("service-a"))
  assert_true(performance_summary.service_performance.contains_key("service-b"))
  assert_true(performance_summary.service_performance.contains_key("service-c"))
  
  // Verify metric consistency
  for metric in performance_metrics {
    assert_true(performance_summary.service_performance.get("service-a").unwrap().contains_key(metric))
    assert_true(performance_summary.service_performance.get("service-b").unwrap().contains_key(metric))
    assert_true(performance_summary.service_performance.get("service-c").unwrap().contains_key(metric))
  }
  
  // Verify performance aggregation
  let aggregated_metrics = CrossServiceConsistencyManager::aggregate_performance_metrics(consistency_manager)
  
  // Verify aggregated request duration
  match aggregated_metrics.get("request.duration") {
    Some(AggregatedMetric { min, max, avg, count }) => {
      assert_eq(min, 100.0)
      assert_eq(max, 200.0)
      assert_eq(avg, (150.0 + 200.0 + 100.0) / 3.0)
      assert_eq(count, 3)
    }
    None => assert_true(false)
  }
  
  // Verify performance consistency analysis
  let consistency_analysis = CrossServiceConsistencyManager::analyze_performance_consistency(consistency_manager)
  
  // Verify consistency scores are calculated
  assert_true(consistency_analysis.consistency_scores.contains_key("request.duration"))
  assert_true(consistency_analysis.consistency_scores.contains_key("response.size"))
  assert_true(consistency_analysis.consistency_scores.contains_key("cpu.usage"))
  assert_true(consistency_analysis.consistency_scores.contains_key("memory.usage"))
  
  // Verify outlier detection
  assert_true(consistency_analysis.outliers.length() > 0)
  
  for outlier in consistency_analysis.outliers {
    assert_true(performance_metrics.contains(outlier.metric))
    assert_true(["service-a", "service-b", "service-c"].contains(outlier.service))
  }
}

// Test 10: End-to-End Cross-Service Consistency
test "end-to-end cross-service consistency" {
  let consistency_manager = CrossServiceConsistencyManager::new()
  
  // Simulate complete request flow across services
  let request_id = "req-123456789"
  let trace_id = "trace-987654321"
  
  // Service A: Entry point
  let service_a_context = CrossServiceConsistencyManager::start_request(consistency_manager, request_id, trace_id, "service-a")
  
  let service_a_span = CrossServiceConsistencyManager::create_span(consistency_manager, service_a_context, "service-a-operation")
  Span::add_event(service_a_span, "Request received", None)
  
  let service_a_metrics = CrossServiceConsistencyManager::get_metrics(consistency_manager, "service-a")
  let request_counter = Metrics::create_counter(service_a_metrics, "request.count")
  Counter::add(request_counter, 1.0)
  
  let service_a_logger = CrossServiceConsistencyManager::get_logger(consistency_manager, "service-a")
  let log_record_a = LogRecord::new_with_correlation(Info, "Processing request", request_id, trace_id, Some(Span::span_context(service_a_span).span_id))
  Logger::emit(service_a_logger, log_record_a)
  
  // Service B: Processing
  let service_b_context = CrossServiceConsistencyManager::propagate_context(consistency_manager, service_a_context, "service-b")
  
  let service_b_span = CrossServiceConsistencyManager::create_span(consistency_manager, service_b_context, "service-b-operation")
  Span::add_event(service_b_span, "Processing data", None)
  
  let service_b_metrics = CrossServiceConsistencyManager::get_metrics(consistency_manager, "service-b")
  let processing_counter = Metrics::create_counter(service_b_metrics, "processing.count")
  Counter::add(processing_counter, 1.0)
  
  let service_b_logger = CrossServiceConsistencyManager::get_logger(consistency_manager, "service-b")
  let log_record_b = LogRecord::new_with_correlation(Info, "Processing data", request_id, trace_id, Some(Span::span_context(service_b_span).span_id))
  Logger::emit(service_b_logger, log_record_b)
  
  // Service C: Storage
  let service_c_context = CrossServiceConsistencyManager::propagate_context(consistency_manager, service_b_context, "service-c")
  
  let service_c_span = CrossServiceConsistencyManager::create_span(consistency_manager, service_c_context, "service-c-operation")
  Span::add_event(service_c_span, "Storing data", None)
  
  let service_c_metrics = CrossServiceConsistencyManager::get_metrics(consistency_manager, "service-c")
  let storage_counter = Metrics::create_counter(service_c_metrics, "storage.count")
  Counter::add(storage_counter, 1.0)
  
  let service_c_logger = CrossServiceConsistencyManager::get_logger(consistency_manager, "service-c")
  let log_record_c = LogRecord::new_with_correlation(Info, "Storing data", request_id, trace_id, Some(Span::span_context(service_c_span).span_id))
  Logger::emit(service_c_logger, log_record_c)
  
  // End all spans
  Span::end(service_a_span)
  Span::end(service_b_span)
  Span::end(service_c_span)
  
  // Verify end-to-end consistency
  let e2e_analysis = CrossServiceConsistencyManager::analyze_end_to_end_request(consistency_manager, request_id)
  
  // Verify request flow
  assert_eq(e2e_analysis.request_id, request_id)
  assert_eq(e2e_analysis.trace_id, trace_id)
  assert_eq(e2e_analysis.service_flow.length(), 3)
  assert_eq(e2e_analysis.service_flow[0], "service-a")
  assert_eq(e2e_analysis.service_flow[1], "service-b")
  assert_eq(e2e_analysis.service_flow[2], "service-c")
  
  // Verify span consistency
  assert_eq(e2e_analysis.spans.length(), 3)
  for span in e2e_analysis.spans {
    assert_eq(span.trace_id, trace_id)
    assert_true(e2e_analysis.service_flow.contains(span.service_name))
  }
  
  // Verify parent-child relationships
  assert_eq(e2e_analysis.spans[0].parent_span_id, "")
  assert_eq(e2e_analysis.spans[1].parent_span_id, e2e_analysis.spans[0].span_id)
  assert_eq(e2e_analysis.spans[2].parent_span_id, e2e_analysis.spans[1].span_id)
  
  // Verify log consistency
  assert_eq(e2e_analysis.logs.length(), 3)
  for log in e2e_analysis.logs {
    assert_eq(log.request_id, request_id)
    assert_eq(log.trace_id, trace_id)
    assert_true(e2e_analysis.service_flow.contains(log.service_name))
  }
  
  // Verify metric consistency
  assert_true(e2e_analysis.metrics.contains_key("service-a"))
  assert_true(e2e_analysis.metrics.contains_key("service-b"))
  assert_true(e2e_analysis.metrics.contains_key("service-c"))
  
  assert_eq(e2e_analysis.metrics.get("service-a").unwrap().get("request.count"), Some(1.0))
  assert_eq(e2e_analysis.metrics.get("service-b").unwrap().get("processing.count"), Some(1.0))
  assert_eq(e2e_analysis.metrics.get("service-c").unwrap().get("storage.count"), Some(1.0))
  
  // Verify overall consistency score
  assert_true(e2e_analysis.consistency_score >= 0.9) // Should be highly consistent
  assert_eq(e2e_analysis.inconsistencies.length(), 0) // No inconsistencies
}

// Helper functions
fn generate_trace_ids(count : Int) -> Array[String] {
  let mut trace_ids = []
  
  for i in 0..count {
    let trace_id = "trace-" + i.to_string().pad_left(32, '0')
    trace_ids = trace_ids + [trace_id]
  }
  
  trace_ids
}