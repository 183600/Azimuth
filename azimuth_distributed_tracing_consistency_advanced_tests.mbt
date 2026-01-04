// Azimuth Telemetry System - Advanced Distributed Tracing Consistency Tests
// This file contains comprehensive test cases for distributed tracing consistency across services

// Test 1: Trace Context Propagation
test "trace context propagation across services" {
  // Create initial trace context
  let trace_id = "trace_1234567890"
  let parent_span_id = "span_parent_001"
  let parent_ctx = SpanContext::new(trace_id, parent_span_id, true, "initial_state")
  
  // Service 1: Create child span
  let service1_span = Span::new("service1_operation", Server, parent_ctx)
  Span::add_attribute(service1_span, "service.name", StringValue("service1"))
  Span::add_attribute(service1_span, "operation.type", StringValue("data_processing"))
  
  let service1_ctx = Span::span_context(service1_span)
  let service1_span_id = SpanContext::span_id(service1_ctx)
  
  // Verify trace ID is consistent
  assert_eq(SpanContext::trace_id(service1_ctx), trace_id)
  assert_true(SpanContext::is_sampled(service1_ctx))
  
  // Service 2: Create child span from service1 context
  let service2_span = Span::new("service2_operation", Client, service1_ctx)
  Span::add_attribute(service2_span, "service.name", StringValue("service2"))
  Span::add_attribute(service2_span, "operation.type", StringValue("data_validation"))
  
  let service2_ctx = Span::span_context(service2_span)
  let service2_span_id = SpanContext::span_id(service2_ctx)
  
  // Verify trace ID consistency across services
  assert_eq(SpanContext::trace_id(service2_ctx), trace_id)
  assert_eq(SpanContext::trace_id(service2_ctx), SpanContext::trace_id(service1_ctx))
  
  // Service 3: Create another child span
  let service3_span = Span::new("service3_operation", Internal, service2_ctx)
  Span::add_attribute(service3_span, "service.name", StringValue("service3"))
  Span::add_attribute(service3_span, "operation.type", StringValue("data_storage"))
  
  let service3_ctx = Span::span_context(service3_span)
  
  // Verify trace ID consistency across all services
  assert_eq(SpanContext::trace_id(service3_ctx), trace_id)
  assert_eq(SpanContext::trace_id(service3_ctx), SpanContext::trace_id(service1_ctx))
  assert_eq(SpanContext::trace_id(service3_ctx), SpanContext::trace_id(service2_ctx))
  
  // Verify all spans have different span IDs but same trace ID
  assert_not_eq(parent_span_id, service1_span_id)
  assert_not_eq(service1_span_id, service2_span_id)
  assert_not_eq(service2_span_id, SpanContext::span_id(service3_ctx))
  
  assert_eq(SpanContext::trace_id(parent_ctx), trace_id)
  assert_eq(SpanContext::trace_id(service1_ctx), trace_id)
  assert_eq(SpanContext::trace_id(service2_ctx), trace_id)
  assert_eq(SpanContext::trace_id(service3_ctx), trace_id)
}

// Test 2: Baggage Propagation Consistency
test "baggage propagation consistency across services" {
  // Create initial baggage
  let initial_baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(initial_baggage, "user.id", "user123")
  let baggage_with_request = Baggage::set_entry(baggage_with_user, "request.id", "req456")
  let final_baggage = Baggage::set_entry(baggage_with_request, "session.id", "session789")
  
  // Service 1: Receive and add baggage
  let service1_baggage = Baggage::set_entry(final_baggage, "service1.timestamp", "1234567890")
  
  // Verify original baggage entries are preserved
  let user_id = Baggage::get_entry(service1_baggage, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  let request_id = Baggage::get_entry(service1_baggage, "request.id")
  match request_id {
    Some(value) => assert_eq(value, "req456")
    None => assert_true(false)
  }
  
  let session_id = Baggage::get_entry(service1_baggage, "session.id")
  match session_id {
    Some(value) => assert_eq(value, "session789")
    None => assert_true(false)
  }
  
  // Service 2: Receive and add baggage
  let service2_baggage = Baggage::set_entry(service1_baggage, "service2.operation", "validate")
  
  // Verify all previous entries are still present
  let service1_timestamp = Baggage::get_entry(service2_baggage, "service1.timestamp")
  match service1_timestamp {
    Some(value) => assert_eq(value, "1234567890")
    None => assert_true(false)
  }
  
  // Service 3: Receive baggage
  let service3_baggage = service2_baggage
  
  // Verify complete baggage chain
  let chain_user_id = Baggage::get_entry(service3_baggage, "user.id")
  match chain_user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  let chain_service2_operation = Baggage::get_entry(service3_baggage, "service2.operation")
  match chain_service2_operation {
    Some(value) => assert_eq(value, "validate")
    None => assert_true(false)
  }
}

// Test 3: Cross-Service Event Correlation
test "cross-service event correlation" {
  // Create trace context
  let trace_id = "correlation_trace_001"
  let trace_ctx = SpanContext::new(trace_id, "root_span", true, "")
  
  // Service 1: Create span and events
  let service1_span = Span::new("service1_main", Server, trace_ctx)
  Span::add_event(service1_span, "request_received", Some([
    ("timestamp", IntValue(1000)),
    ("client.id", StringValue("client123"))
  ]))
  Span::add_event(service1_span, "processing_started", Some([
    ("timestamp", IntValue(1100)),
    ("operation.type", StringValue("data_transform"))
  ]))
  
  // Service 2: Create child span and correlated events
  let service2_span = Span::new("service2_process", Client, Span::span_context(service1_span))
  Span::add_event(service2_span, "data_received", Some([
    ("timestamp", IntValue(1200)),
    ("source.service", StringValue("service1"))
  ]))
  Span::add_event(service2_span, "validation_started", Some([
    ("timestamp", IntValue(1250)),
    ("validation.type", StringValue("schema_check"))
  ]))
  
  // Service 3: Create child span and final events
  let service3_span = Span::new("service3_store", Internal, Span::span_context(service2_span))
  Span::add_event(service3_span, "storage_started", Some([
    ("timestamp", IntValue(1300)),
    ("storage.type", StringValue("database"))
  ]))
  Span::add_event(service3_span, "operation_completed", Some([
    ("timestamp", IntValue(1400)),
    ("result.status", StringValue("success"))
  ]))
  
  // Verify trace ID consistency across all spans and events
  assert_eq(SpanContext::trace_id(Span::span_context(service1_span)), trace_id)
  assert_eq(SpanContext::trace_id(Span::span_context(service2_span)), trace_id)
  assert_eq(SpanContext::trace_id(Span::span_context(service3_span)), trace_id)
  
  // Verify event timestamp ordering
  let service1_events = Span::events(service1_span)
  let service2_events = Span::events(service2_span)
  let service3_events = Span::events(service3_span)
  
  assert_eq(service1_events.length(), 2)
  assert_eq(service2_events.length(), 2)
  assert_eq(service3_events.length(), 2)
  
  // Extract timestamps for verification
  let s1_e1_timestamp = Event::get_attribute(service1_events[0], "timestamp")
  let s1_e2_timestamp = Event::get_attribute(service1_events[1], "timestamp")
  let s2_e1_timestamp = Event::get_attribute(service2_events[0], "timestamp")
  let s2_e2_timestamp = Event::get_attribute(service2_events[1], "timestamp")
  let s3_e1_timestamp = Event::get_attribute(service3_events[0], "timestamp")
  let s3_e2_timestamp = Event::get_attribute(service3_events[1], "timestamp")
  
  match (s1_e1_timestamp, s1_e2_timestamp, s2_e1_timestamp, s2_e2_timestamp, s3_e1_timestamp, s3_e2_timestamp) {
    (Some(IntValue(t1)), Some(IntValue(t2)), Some(IntValue(t3)), Some(IntValue(t4)), Some(IntValue(t5)), Some(IntValue(t6))) => {
      assert_true(t1 < t2)  // Service 1 events ordering
      assert_true(t2 < t3)  // Cross-service ordering
      assert_true(t3 < t4)  // Service 2 events ordering
      assert_true(t4 < t5)  // Cross-service ordering
      assert_true(t5 < t6)  // Service 3 events ordering
    }
    _ => assert_true(false)
  }
}

// Test 4: Distributed Transaction Consistency
test "distributed transaction consistency" {
  // Create transaction context
  let transaction_id = "txn_1234567890"
  let trace_ctx = SpanContext::new("trace_txn_001", "txn_root", true, "")
  
  // Transaction coordinator span
  let coordinator_span = Span::new("transaction_coordinator", Server, trace_ctx)
  Span::add_attribute(coordinator_span, "transaction.id", StringValue(transaction_id))
  Span::add_attribute(coordinator_span, "transaction.type", StringValue("two_phase_commit"))
  
  // Service 1: Prepare phase
  let service1_prepare_span = Span::new("service1_prepare", Client, Span::span_context(coordinator_span))
  Span::add_attribute(service1_prepare_span, "transaction.id", StringValue(transaction_id))
  Span::add_attribute(service1_prepare_span, "phase", StringValue("prepare"))
  Span::add_attribute(service1_prepare_span, "service", StringValue("inventory"))
  Span::set_status(service1_prepare_span, Ok, Some("Prepared successfully"))
  
  // Service 2: Prepare phase
  let service2_prepare_span = Span::new("service2_prepare", Client, Span::span_context(coordinator_span))
  Span::add_attribute(service2_prepare_span, "transaction.id", StringValue(transaction_id))
  Span::add_attribute(service2_prepare_span, "phase", StringValue("prepare"))
  Span::add_attribute(service2_prepare_span, "service", StringValue("payment"))
  Span::set_status(service2_prepare_span, Ok, Some("Prepared successfully"))
  
  // Service 3: Prepare phase
  let service3_prepare_span = Span::new("service3_prepare", Client, Span::span_context(coordinator_span))
  Span::add_attribute(service3_prepare_span, "transaction.id", StringValue(transaction_id))
  Span::add_attribute(service3_prepare_span, "phase", StringValue("prepare"))
  Span::add_attribute(service3_prepare_span, "service", StringValue("notification"))
  Span::set_status(service3_prepare_span, Ok, Some("Prepared successfully"))
  
  // Commit phase
  let commit_span = Span::new("transaction_commit", Internal, Span::span_context(coordinator_span))
  Span::add_attribute(commit_span, "transaction.id", StringValue(transaction_id))
  Span::add_attribute(commit_span, "phase", StringValue("commit"))
  
  // Service 1: Commit phase
  let service1_commit_span = Span::new("service1_commit", Client, Span::span_context(commit_span))
  Span::add_attribute(service1_commit_span, "transaction.id", StringValue(transaction_id))
  Span::add_attribute(service1_commit_span, "phase", StringValue("commit"))
  Span::add_attribute(service1_commit_span, "service", StringValue("inventory"))
  Span::set_status(service1_commit_span, Ok, Some("Committed successfully"))
  
  // Verify transaction ID consistency across all spans
  let all_spans = [
    coordinator_span,
    service1_prepare_span,
    service2_prepare_span,
    service3_prepare_span,
    commit_span,
    service1_commit_span
  ]
  
  for span in all_spans {
    let txn_id = Span::get_attribute(span, "transaction.id")
    match txn_id {
      Some(StringValue(id)) => assert_eq(id, transaction_id)
      _ => assert_true(false)
    }
    
    // Verify all spans belong to the same trace
    let span_ctx = Span::span_context(span)
    assert_eq(SpanContext::trace_id(span_ctx), "trace_txn_001")
  }
  
  // Verify phase consistency
  let prepare_spans = [service1_prepare_span, service2_prepare_span, service3_prepare_span]
  for span in prepare_spans {
    let phase = Span::get_attribute(span, "phase")
    match phase {
      Some(StringValue(p)) => assert_eq(p, "prepare")
      _ => assert_true(false)
    }
  }
  
  // Verify commit status
  assert_eq(Span::status(service1_commit_span), Ok)
}

// Test 5: Error Propagation Consistency
test "error propagation consistency across services" {
  // Create trace context
  let trace_ctx = SpanContext::new("error_trace_001", "error_root", true, "")
  
  // Service 1: Generate error
  let service1_span = Span::new("service1_operation", Server, trace_ctx)
  Span::add_event(service1_span, "operation_started", Some([("timestamp", IntValue(1000))]))
  Span::set_status(service1_span, Error, Some("Database connection failed"))
  Span::add_event(service1_span, "error_occurred", Some([
    ("error.type", StringValue("database_error")),
    ("error.code", IntValue(500)),
    ("timestamp", IntValue(1100))
  ]))
  
  // Service 2: Propagate error
  let service2_span = Span::new("service2_operation", Client, Span::span_context(service1_span))
  Span::add_event(service2_span, "error_received", Some([
    ("source.service", StringValue("service1")),
    ("error.type", StringValue("database_error")),
    ("timestamp", IntValue(1200))
  ]))
  Span::set_status(service2_span, Error, Some("Upstream service error"))
  Span::add_event(service2_span, "error_propagated", Some([
    ("error.type", StringValue("propagated_error")),
    ("timestamp", IntValue(1250))
  ]))
  
  // Service 3: Handle propagated error
  let service3_span = Span::new("service3_operation", Internal, Span::span_context(service2_span))
  Span::add_event(service3_span, "error_handling_started", Some([
    ("source.service", StringValue("service2")),
    ("timestamp", IntValue(1300))
  ]))
  Span::add_event(service3_span, "error_handled", Some([
    ("handling.strategy", StringValue("circuit_breaker")),
    ("timestamp", IntValue(1350))
  ]))
  Span::set_status(service3_span, Error, Some("Error handled with circuit breaker"))
  
  // Verify error propagation chain
  assert_eq(Span::status(service1_span), Error)
  assert_eq(Span::status(service2_span), Error)
  assert_eq(Span::status(service3_span), Error)
  
  // Verify trace ID consistency in error scenarios
  assert_eq(SpanContext::trace_id(Span::span_context(service1_span)), "error_trace_001")
  assert_eq(SpanContext::trace_id(Span::span_context(service2_span)), "error_trace_001")
  assert_eq(SpanContext::trace_id(Span::span_context(service3_span)), "error_trace_001")
  
  // Verify error context preservation
  let service1_error = Span::get_attribute(service1_span, "error.type")
  match service1_error {
    Some(StringValue(error_type)) => assert_eq(error_type, "database_error")
    _ => assert_true(false)
  }
  
  let service2_error_source = Span::get_attribute(service2_span, "source.service")
  match service2_error_source {
    Some(StringValue(source)) => assert_eq(source, "service1")
    _ => assert_true(false)
  }
  
  let service3_handling = Span::get_attribute(service3_span, "handling.strategy")
  match service3_handling {
    Some(StringValue(strategy)) => assert_eq(strategy, "circuit_breaker")
    _ => assert_true(false)
  }
}

// Test 6: Sampling Decision Consistency
test "sampling decision consistency across distributed trace" {
  // Create sampled trace context
  let sampled_trace_ctx = SpanContext::new("sampled_trace_001", "sampled_root", true, "")
  
  // Service 1: Create child span
  let service1_sampled_span = Span::new("service1_sampled", Server, sampled_trace_ctx)
  
  // Service 2: Create child span
  let service2_sampled_span = Span::new("service2_sampled", Client, Span::span_context(service1_sampled_span))
  
  // Service 3: Create child span
  let service3_sampled_span = Span::new("service3_sampled", Internal, Span::span_context(service2_sampled_span))
  
  // Verify sampling decision is consistent across all spans
  assert_true(SpanContext::is_sampled(Span::span_context(service1_sampled_span)))
  assert_true(SpanContext::is_sampled(Span::span_context(service2_sampled_span)))
  assert_true(SpanContext::is_sampled(Span::span_context(service3_sampled_span)))
  
  // Create non-sampled trace context
  let non_sampled_trace_ctx = SpanContext::new("non_sampled_trace_001", "non_sampled_root", false, "")
  
  // Service 1: Create child span
  let service1_non_sampled_span = Span::new("service1_non_sampled", Server, non_sampled_trace_ctx)
  
  // Service 2: Create child span
  let service2_non_sampled_span = Span::new("service2_non_sampled", Client, Span::span_context(service1_non_sampled_span))
  
  // Service 3: Create child span
  let service3_non_sampled_span = Span::new("service3_non_sampled", Internal, Span::span_context(service2_non_sampled_span))
  
  // Verify non-sampling decision is consistent across all spans
  assert_false(SpanContext::is_sampled(Span::span_context(service1_non_sampled_span)))
  assert_false(SpanContext::is_sampled(Span::span_context(service2_non_sampled_span)))
  assert_false(SpanContext::is_sampled(Span::span_context(service3_non_sampled_span)))
  
  // Verify trace ID consistency is maintained regardless of sampling decision
  assert_eq(SpanContext::trace_id(Span::span_context(service1_sampled_span)), "sampled_trace_001")
  assert_eq(SpanContext::trace_id(Span::span_context(service2_sampled_span)), "sampled_trace_001")
  assert_eq(SpanContext::trace_id(Span::span_context(service3_sampled_span)), "sampled_trace_001")
  
  assert_eq(SpanContext::trace_id(Span::span_context(service1_non_sampled_span)), "non_sampled_trace_001")
  assert_eq(SpanContext::trace_id(Span::span_context(service2_non_sampled_span)), "non_sampled_trace_001")
  assert_eq(SpanContext::trace_id(Span::span_context(service3_non_sampled_span)), "non_sampled_trace_001")
}

// Test 7: Resource Consistency Across Services
test "resource consistency across services" {
  // Define common resource attributes
  let common_attrs = [
    ("service.namespace", StringValue("production")),
    ("service.version", StringValue("1.2.3")),
    ("deployment.environment", StringValue("prod"))
  ]
  
  // Service 1: Create resource with common and specific attributes
  let service1_specific_attrs = [
    ("service.name", StringValue("user-service")),
    ("service.instance.id", StringValue("user-service-001")),
    ("host.name", StringValue("host-user-001"))
  ]
  let service1_resource = Resource::with_attributes(Resource::new(), common_attrs + service1_specific_attrs)
  
  // Service 2: Create resource with common and specific attributes
  let service2_specific_attrs = [
    ("service.name", StringValue("order-service")),
    ("service.instance.id", StringValue("order-service-001")),
    ("host.name", StringValue("host-order-001"))
  ]
  let service2_resource = Resource::with_attributes(Resource::new(), common_attrs + service2_specific_attrs)
  
  // Service 3: Create resource with common and specific attributes
  let service3_specific_attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.instance.id", StringValue("payment-service-001")),
    ("host.name", StringValue("host-payment-001"))
  ]
  let service3_resource = Resource::with_attributes(Resource::new(), common_attrs + service3_specific_attrs)
  
  // Verify common attributes are consistent across all services
  for resource in [service1_resource, service2_resource, service3_resource] {
    let namespace = Resource::get_attribute(resource, "service.namespace")
    match namespace {
      Some(StringValue(ns)) => assert_eq(ns, "production")
      _ => assert_true(false)
    }
    
    let version = Resource::get_attribute(resource, "service.version")
    match version {
      Some(StringValue(v)) => assert_eq(v, "1.2.3")
      _ => assert_true(false)
    }
    
    let environment = Resource::get_attribute(resource, "deployment.environment")
    match environment {
      Some(StringValue(env)) => assert_eq(env, "prod")
      _ => assert_true(false)
    }
  }
  
  // Verify service-specific attributes are unique per service
  let service1_name = Resource::get_attribute(service1_resource, "service.name")
  match service1_name {
    Some(StringValue(name)) => assert_eq(name, "user-service")
    _ => assert_true(false)
  }
  
  let service2_name = Resource::get_attribute(service2_resource, "service.name")
  match service2_name {
    Some(StringValue(name)) => assert_eq(name, "order-service")
    _ => assert_true(false)
  }
  
  let service3_name = Resource::get_attribute(service3_resource, "service.name")
  match service3_name {
    Some(StringValue(name)) => assert_eq(name, "payment-service")
    _ => assert_true(false)
  }
}

// Test 8: Cross-Service Metric Correlation
test "cross-service metric correlation" {
  // Create trace context for correlated operations
  let trace_ctx = SpanContext::new("metric_correlation_trace_001", "metric_root", true, "")
  
  // Service 1: Create metrics and span
  let provider1 = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(provider1, "service1-meter")
  let service1_counter = Meter::create_counter(meter1, "operations_total", Some("Total operations"), Some("count"))
  let service1_span = Span::new("service1_operation", Server, trace_ctx)
  
  // Record metrics with trace correlation
  Counter::add(service1_counter, 1.0, Some([
    ("service.name", StringValue("service1")),
    ("trace.id", StringValue("metric_correlation_trace_001")),
    ("operation.type", StringValue("process"))
  ]))
  
  // Service 2: Create metrics and span
  let provider2 = MeterProvider::default()
  let meter2 = MeterProvider::get_meter(provider2, "service2-meter")
  let service2_counter = Meter::create_counter(meter2, "operations_total", Some("Total operations"), Some("count"))
  let service2_span = Span::new("service2_operation", Client, Span::span_context(service1_span))
  
  // Record metrics with trace correlation
  Counter::add(service2_counter, 1.0, Some([
    ("service.name", StringValue("service2")),
    ("trace.id", StringValue("metric_correlation_trace_001")),
    ("operation.type", StringValue("validate"))
  ]))
  
  // Service 3: Create metrics and span
  let provider3 = MeterProvider::default()
  let meter3 = MeterProvider::get_meter(provider3, "service3-meter")
  let service3_counter = Meter::create_counter(meter3, "operations_total", Some("Total operations"), Some("count"))
  let service3_span = Span::new("service3_operation", Internal, Span::span_context(service2_span))
  
  // Record metrics with trace correlation
  Counter::add(service3_counter, 1.0, Some([
    ("service.name", StringValue("service3")),
    ("trace.id", StringValue("metric_correlation_trace_001")),
    ("operation.type", StringValue("store"))
  ]))
  
  // Collect metrics from all services
  let metrics1 = Meter::collect(meter1)
  let metrics2 = Meter::collect(meter2)
  let metrics3 = Meter::collect(meter3)
  
  // Verify trace ID correlation in metrics
  let verify_trace_correlation = fn(metrics: MetricData) {
    let metric_list = MetricData::metrics(metrics)
    for metric in metric_list {
      if Metric::name(metric) == "operations_total" {
        match Metric::data(metric) {
          CounterData(points) => {
            for point in points {
              let attrs = DataPoint::attributes(point)
              let trace_id = Attributes::get(attrs, "trace.id")
              match trace_id {
                Some(StringValue(id)) => assert_eq(id, "metric_correlation_trace_001")
                _ => assert_true(false)
              }
            }
          }
          _ => assert_true(false)
        }
      }
    }
  }
  
  verify_trace_correlation(metrics1)
  verify_trace_correlation(metrics2)
  verify_trace_correlation(metrics3)
  
  // Verify trace ID consistency in spans
  assert_eq(SpanContext::trace_id(Span::span_context(service1_span)), "metric_correlation_trace_001")
  assert_eq(SpanContext::trace_id(Span::span_context(service2_span)), "metric_correlation_trace_001")
  assert_eq(SpanContext::trace_id(Span::span_context(service3_span)), "metric_correlation_trace_001")
}

// Test 9: Context Propagation with HTTP Headers
test "context propagation with http headers" {
  // Create initial context
  let trace_ctx = SpanContext::new("http_propagation_trace_001", "http_root", true, "")
  let baggage = Baggage::new()
  let baggage_with_data = Baggage::set_entry(baggage, "user.id", "user123")
  
  // Inject context into HTTP headers
  let headers = []
  let headers_with_trace = TextMapPropagator::inject_trace_context(headers, trace_ctx)
  let headers_with_baggage = TextMapPropagator::inject_baggage(headers_with_trace, baggage_with_data)
  
  // Service 1: Extract context from headers
  let extracted_ctx1 = TextMapPropagator::extract_trace_context(headers_with_baggage)
  let extracted_baggage1 = TextMapPropagator::extract_baggage(headers_with_baggage)
  
  // Verify extracted context
  assert_eq(SpanContext::trace_id(extracted_ctx1), "http_propagation_trace_001")
  assert_true(SpanContext::is_sampled(extracted_ctx1))
  
  let extracted_user_id = Baggage::get_entry(extracted_baggage1, "user.id")
  match extracted_user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  // Service 1: Create child span and add baggage
  let service1_span = Span::new("service1_http", Server, extracted_ctx1)
  let service1_baggage = Baggage::set_entry(extracted_baggage1, "service1.timestamp", "1234567890")
  
  // Inject updated context for next service
  let headers_for_service2 = TextMapPropagator::inject_trace_context(headers, Span::span_context(service1_span))
  let headers_for_service2_with_baggage = TextMapPropagator::inject_baggage(headers_for_service2, service1_baggage)
  
  // Service 2: Extract context from headers
  let extracted_ctx2 = TextMapPropagator::extract_trace_context(headers_for_service2_with_baggage)
  let extracted_baggage2 = TextMapPropagator::extract_baggage(headers_for_service2_with_baggage)
  
  // Verify context propagation consistency
  assert_eq(SpanContext::trace_id(extracted_ctx2), "http_propagation_trace_001")
  assert_eq(SpanContext::trace_id(extracted_ctx2), SpanContext::trace_id(extracted_ctx1))
  
  // Verify baggage propagation
  let user_id_at_service2 = Baggage::get_entry(extracted_baggage2, "user.id")
  match user_id_at_service2 {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  let service1_timestamp_at_service2 = Baggage::get_entry(extracted_baggage2, "service1.timestamp")
  match service1_timestamp_at_service2 {
    Some(value) => assert_eq(value, "1234567890")
    None => assert_true(false)
  }
  
  // Service 2: Create child span and add baggage
  let service2_span = Span::new("service2_http", Client, extracted_ctx2)
  let service2_baggage = Baggage::set_entry(extracted_baggage2, "service2.operation", "process_data")
  
  // Inject for final service
  let headers_for_service3 = TextMapPropagator::inject_trace_context(headers, Span::span_context(service2_span))
  let headers_for_service3_with_baggage = TextMapPropagator::inject_baggage(headers_for_service3, service2_baggage)
  
  // Service 3: Extract context from headers
  let extracted_ctx3 = TextMapPropagator::extract_trace_context(headers_for_service3_with_baggage)
  let extracted_baggage3 = TextMapPropagator::extract_baggage(headers_for_service3_with_baggage)
  
  // Verify end-to-end context propagation
  assert_eq(SpanContext::trace_id(extracted_ctx3), "http_propagation_trace_001")
  assert_eq(SpanContext::trace_id(extracted_ctx3), SpanContext::trace_id(extracted_ctx1))
  assert_eq(SpanContext::trace_id(extracted_ctx3), SpanContext::trace_id(extracted_ctx2))
  
  // Verify complete baggage propagation
  let final_user_id = Baggage::get_entry(extracted_baggage3, "user.id")
  match final_user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  let final_service1_timestamp = Baggage::get_entry(extracted_baggage3, "service1.timestamp")
  match final_service1_timestamp {
    Some(value) => assert_eq(value, "1234567890")
    None => assert_true(false)
  }
  
  let final_service2_operation = Baggage::get_entry(extracted_baggage3, "service2.operation")
  match final_service2_operation {
    Some(value) => assert_eq(value, "process_data")
    None => assert_true(false)
  }
}

// Test 10: Distributed Trace Integrity
test "distributed trace integrity" {
  // Create complex distributed trace
  let root_trace_id = "integrity_trace_001"
  let root_ctx = SpanContext::new(root_trace_id, "root_span", true, "")
  
  // Gateway service
  let gateway_span = Span::new("gateway_request", Server, root_ctx)
  Span::add_attribute(gateway_span, "service.name", StringValue("api-gateway"))
  Span::add_attribute(gateway_span, "http.method", StringValue("POST"))
  Span::add_attribute(gateway_span, "http.url", StringValue("/api/v1/process"))
  
  // Auth service
  let auth_span = Span::new("auth_check", Client, Span::span_context(gateway_span))
  Span::add_attribute(auth_span, "service.name", StringValue("auth-service"))
  Span::add_attribute(auth_span, "auth.method", StringValue("jwt"))
  Span::set_status(auth_span, Ok, Some("Authentication successful"))
  
  // Business logic service
  let business_span = Span::new("business_logic", Client, Span::span_context(gateway_span))
  Span::add_attribute(business_span, "service.name", StringValue("business-service"))
  Span::add_attribute(business_span, "operation.type", StringValue("data_processing"))
  
  // Data service (child of business service)
  let data_span = Span::new("data_access", Client, Span::span_context(business_span))
  Span::add_attribute(data_span, "service.name", StringValue("data-service"))
  Span::add_attribute(data_span, "database.type", StringValue("postgresql"))
  Span::add_attribute(data_span, "operation.type", StringValue("query"))
  
  // Cache service (child of business service)
  let cache_span = Span::new("cache_access", Client, Span::span_context(business_span))
  Span::add_attribute(cache_span, "service.name", StringValue("cache-service"))
  Span::add_attribute(cache_span, "cache.type", StringValue("redis"))
  Span::add_attribute(cache_span, "operation.type", StringValue("get"))
  
  // Notification service (child of gateway service)
  let notification_span = Span::new("send_notification", Client, Span::span_context(gateway_span))
  Span::add_attribute(notification_span, "service.name", StringValue("notification-service"))
  Span::add_attribute(notification_span, "notification.type", StringValue("email"))
  
  // Collect all spans for integrity verification
  let all_spans = [
    gateway_span,
    auth_span,
    business_span,
    data_span,
    cache_span,
    notification_span
  ]
  
  // Verify trace ID consistency across all spans
  for span in all_spans {
    let span_ctx = Span::span_context(span)
    assert_eq(SpanContext::trace_id(span_ctx), root_trace_id)
    assert_true(SpanContext::is_valid(span_ctx))
    assert_true(SpanContext::is_sampled(span_ctx))
  }
  
  // Verify parent-child relationships
  assert_eq(SpanContext::span_id(Span::span_context(auth_span)), "auth_check")
  assert_eq(SpanContext::span_id(Span::span_context(business_span)), "business_logic")
  assert_eq(SpanContext::span_id(Span::span_context(data_span)), "data_access")
  assert_eq(SpanContext::span_id(Span::span_context(cache_span)), "cache_access")
  assert_eq(SpanContext::span_id(Span::span_context(notification_span)), "send_notification")
  
  // Verify span hierarchy integrity
  let gateway_span_id = SpanContext::span_id(Span::span_context(gateway_span))
  let business_span_id = SpanContext::span_id(Span::span_context(business_span))
  let data_span_id = SpanContext::span_id(Span::span_context(data_span))
  
  // All spans should have unique span IDs
  let span_ids = [
    gateway_span_id,
    SpanContext::span_id(Span::span_context(auth_span)),
    business_span_id,
    data_span_id,
    SpanContext::span_id(Span::span_context(cache_span)),
    SpanContext::span_id(Span::span_context(notification_span))
  ]
  
  // Verify all span IDs are unique
  for i in 0..span_ids.length() {
    for j in i+1..span_ids.length() {
      assert_not_eq(span_ids[i], span_ids[j])
    }
  }
  
  // Verify service name consistency
  let service_names = [
    ("api-gateway", gateway_span),
    ("auth-service", auth_span),
    ("business-service", business_span),
    ("data-service", data_span),
    ("cache-service", cache_span),
    ("notification-service", notification_span)
  ]
  
  for (expected_name, span) in service_names {
    let service_name = Span::get_attribute(span, "service.name")
    match service_name {
      Some(StringValue(name)) => assert_eq(name, expected_name)
      _ => assert_true(false)
    }
  }
}