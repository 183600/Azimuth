// Azimuth Telemetry System - Advanced High-Quality Security Tests
// This file contains comprehensive test cases for advanced security and reliability features

// Test 1: Distributed Tracing Consistency
test "distributed tracing consistency across services" {
  // Create a root span for the distributed transaction
  let root_trace_id = "abc123def45678901234567890123456"
  let root_span_id = "1234567890abcdef"
  let root_ctx = SpanContext::new(root_trace_id, root_span_id, true, "root_state")
  let root_span = Span::new("distributed_transaction", Server, root_ctx)
  
  // Simulate service A
  let service_a_ctx = SpanContext::new(root_trace_id, "service_a_span", true, "service_a_state")
  let service_a_span = Span::new("service_a_operation", Internal, service_a_ctx)
  Span::add_event(service_a_span, "service_a_start", Some([("service", StringValue("service_a"))]))
  Span::set_status(service_a_span, Ok, Some("Service A completed successfully"))
  Span::end(service_a_span)
  
  // Simulate service B with context propagation
  let service_b_ctx = SpanContext::new(root_trace_id, "service_b_span", true, "service_b_state")
  let service_b_span = Span::new("service_b_operation", Internal, service_b_ctx)
  Span::add_event(service_b_span, "service_b_start", Some([("service", StringValue("service_b"))]))
  Span::add_event(service_b_span, "service_b_processing", Some([("processing_time_ms", IntValue(150))]))
  Span::set_status(service_b_span, Ok, Some("Service B completed successfully"))
  Span::end(service_b_span)
  
  // Verify trace consistency
  assert_eq(SpanContext::trace_id(service_a_ctx), root_trace_id)
  assert_eq(SpanContext::trace_id(service_b_ctx), root_trace_id)
  assert_true(SpanContext::is_sampled(service_a_ctx))
  assert_true(SpanContext::is_sampled(service_b_ctx))
  
  // Complete the root span
  Span::add_event(root_span, "transaction_complete", Some([
    ("services_count", IntValue(2)),
    ("total_duration_ms", IntValue(300))
  ]))
  Span::set_status(root_span, Ok, Some("Distributed transaction completed"))
  Span::end(root_span)
}

// Test 2: High Concurrency Resource Management
test "high concurrency resource management" {
  let resource_pool = ResourcePool::new(10) // Pool of 10 resources
  let mut active_operations = []
  
  // Simulate 20 concurrent operations
  for i in 0..=20 {
    let operation_id = "op_" + i.to_string()
    let resource = ResourcePool::acquire(resource_pool)
    
    match resource {
      Some(res) => {
        active_operations = active_operations.push((operation_id, res))
        // Simulate resource usage
        Resource::increment_usage(res)
        Resource::set_last_used(res, 1640995200L + i.to_int()) // Different timestamps
      }
      None => {
        // Handle resource exhaustion gracefully
        assert_true(true) // Expected when pool is exhausted
      }
    }
  }
  
  // Verify resource usage statistics
  let total_usage = 0
  for (_, res) in active_operations {
    total_usage = total_usage + Resource::usage_count(res)
  }
  assert_true(total_usage > 0)
  
  // Release all acquired resources
  for (_, res) in active_operations {
    ResourcePool::release(resource_pool, res)
  }
  
  // Verify all resources are returned to pool
  assert_eq(ResourcePool::available_count(resource_pool), 10)
}

// Test 3: Telemetry Data Serialization Integrity
test "telemetry data serialization integrity" {
  // Create complex telemetry data
  let telemetry_data = TelemetryData::new()
  
  // Add various attribute types
  TelemetryData::add_attribute(telemetry_data, "string_attr", StringValue("test_value"))
  TelemetryData::add_attribute(telemetry_data, "int_attr", IntValue(42))
  TelemetryData::add_attribute(telemetry_data, "float_attr", FloatValue(3.14159))
  TelemetryData::add_attribute(telemetry_data, "bool_attr", BoolValue(true))
  TelemetryData::add_attribute(telemetry_data, "array_attr", ArrayStringValue(["a", "b", "c"]))
  
  // Add nested structure
  let nested_attrs = Attributes::new()
  Attributes::set(nested_attrs, "nested_key", StringValue("nested_value"))
  TelemetryData::add_nested_attributes(telemetry_data, "parent", nested_attrs)
  
  // Serialize to binary format
  let serialized_data = TelemetryData::serialize(telemetry_data)
  assert_true(serialized_data.length() > 0)
  
  // Deserialize back to object
  let deserialized_data = TelemetryData::deserialize(serialized_data)
  
  // Verify data integrity
  let original_string = TelemetryData::get_attribute(telemetry_data, "string_attr")
  let deserialized_string = TelemetryData::get_attribute(deserialized_data, "string_attr")
  
  match (original_string, deserialized_string) {
    (Some(StringValue(orig)), Some(StringValue(deser))) => assert_eq(orig, deser)
    _ => assert_true(false)
  }
  
  let original_int = TelemetryData::get_attribute(telemetry_data, "int_attr")
  let deserialized_int = TelemetryData::get_attribute(deserialized_data, "int_attr")
  
  match (original_int, deserialized_int) {
    (Some(IntValue(orig)), Some(IntValue(deser))) => assert_eq(orig, deser)
    _ => assert_true(false)
  }
  
  // Verify nested structure integrity
  let original_nested = TelemetryData::get_nested_attributes(telemetry_data, "parent")
  let deserialized_nested = TelemetryData::get_nested_attributes(deserialized_data, "parent")
  
  match (original_nested, deserialized_nested) {
    (Some(orig), Some(deser)) => {
      let orig_value = Attributes::get(orig, "nested_key")
      let deser_value = Attributes::get(deser, "nested_key")
      
      match (orig_value, deser_value) {
        (Some(StringValue(orig_val)), Some(StringValue(deser_val))) => assert_eq(orig_val, deser_val)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// Test 4: Exception Recovery Mechanisms
test "exception recovery and fault tolerance" {
  let resilient_service = ResilientService::new_with_retry_policy(3, 1000) // 3 retries, 1s delay
  
  // Test successful operation
  let success_result = ResilientService::execute(resilient_service, || {
    // Simulate successful operation
    "success_result"
  })
  match success_result {
    Ok(result) => assert_eq(result, "success_result")
    Err(_) => assert_true(false)
  }
  
  // Test operation with temporary failure
  let mut attempt_count = 0
  let retry_result = ResilientService::execute(resilient_service, || {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Error("temporary_failure")
    } else {
      Ok("retry_success")
    }
  })
  match retry_result {
    Ok(result) => assert_eq(result, "retry_success")
    Err(_) => assert_true(false)
  }
  assert_eq(attempt_count, 3) // Verify retry attempts
  
  // Test operation with permanent failure
  attempt_count = 0
  let failure_result = ResilientService::execute(resilient_service, || {
    attempt_count = attempt_count + 1
    Error("permanent_failure")
  })
  match failure_result {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "permanent_failure")
  }
  assert_eq(attempt_count, 3) // Verify max retry attempts
  
  // Test circuit breaker pattern
  let circuit_breaker = CircuitBreaker::new_with_threshold(5) // 5 failures threshold
  
  // Simulate failures to trigger circuit breaker
  for i in 0..=6 {
    let result = CircuitBreaker::execute(circuit_breaker, || {
      Error("simulated_failure")
    })
    
    if i < 5 {
      match result {
        Err(_) => assert_true(true) // Expected before circuit opens
        Ok(_) => assert_true(false)
      }
    } else {
      match result {
        Err("circuit_open") => assert_true(true) // Expected after circuit opens
        _ => assert_true(false)
      }
    }
  }
}

// Test 5: Performance Boundary Conditions
test "performance boundary conditions and limits" {
  let performance_monitor = PerformanceMonitor::new()
  
  // Test with normal load
  let normal_start_time = PerformanceMonitor::current_time_ms(performance_monitor)
  let mut normal_operations = 0
  
  for i in 0..=100 {
    let telemetry_data = TelemetryData::new()
    TelemetryData::add_attribute(telemetry_data, "operation_id", IntValue(i))
    TelemetryData::process(telemetry_data)
    normal_operations = normal_operations + 1
  }
  
  let normal_end_time = PerformanceMonitor::current_time_ms(performance_monitor)
  let normal_duration = normal_end_time - normal_start_time
  
  // Test with high load
  let high_load_start_time = PerformanceMonitor::current_time_ms(performance_monitor)
  let mut high_load_operations = 0
  
  for i in 0..=10000 {
    let telemetry_data = TelemetryData::new()
    TelemetryData::add_attribute(telemetry_data, "operation_id", IntValue(i))
    TelemetryData::add_attribute(telemetry_data, "payload", StringValue("large_payload_data_" + i.to_string()))
    TelemetryData::process(telemetry_data)
    high_load_operations = high_load_operations + 1
  }
  
  let high_load_end_time = PerformanceMonitor::current_time_ms(performance_monitor)
  let high_load_duration = high_load_end_time - high_load_start_time
  
  // Verify performance characteristics
  assert_true(normal_duration > 0)
  assert_true(high_load_duration > 0)
  assert_true(high_load_duration > normal_duration) // High load should take longer
  
  // Calculate operations per second
  let normal_ops_per_sec = (normal_operations.to_float() / normal_duration.to_float()) * 1000.0
  let high_load_ops_per_sec = (high_load_operations.to_float() / high_load_duration.to_float()) * 1000.0
  
  // Verify reasonable performance boundaries
  assert_true(normal_ops_per_sec > 100.0) // At least 100 ops/sec for normal load
  assert_true(high_load_ops_per_sec > 10.0) // At least 10 ops/sec for high load
  
  // Test memory usage boundaries
  let initial_memory = PerformanceMonitor::memory_usage_mb(performance_monitor)
  
  // Create large telemetry data
  let large_telemetry_data = TelemetryData::new()
  for i in 0..=1000 {
    TelemetryData::add_attribute(large_telemetry_data, "large_attr_" + i.to_string(), StringValue("x".repeat(1000)))
  }
  
  let peak_memory = PerformanceMonitor::memory_usage_mb(performance_monitor)
  let memory_increase = peak_memory - initial_memory
  
  // Verify memory usage is within reasonable bounds
  assert_true(memory_increase < 100.0) // Should not increase by more than 100MB
}

// Test 6: Memory Leak Detection
test "memory leak detection and resource cleanup" {
  let memory_tracker = MemoryTracker::new()
  let initial_objects = MemoryTracker::active_objects(memory_tracker)
  
  // Test with proper resource cleanup
  let mut properly_managed_resources = []
  
  for i in 0..=100 {
    let resource = ManagedResource::new("resource_" + i.to_string())
    properly_managed_resources = properly_managed_resources.push(resource)
    
    // Use the resource
    ManagedResource::process_data(resource, "test_data_" + i.to_string())
    
    // Explicitly clean up
    ManagedResource::cleanup(resource)
  }
  
  let after_proper_cleanup = MemoryTracker::active_objects(memory_tracker)
  
  // Test with improper resource management (potential leaks)
  let mut leaky_resources = []
  
  for i in 0..=100 {
    let resource = ManagedResource::new("leaky_resource_" + i.to_string())
    leaky_resources = leaky_resources.push(resource)
    
    // Use the resource but don't clean up
    ManagedResource::process_data(resource, "leaky_data_" + i.to_string())
    // Missing explicit cleanup - potential leak
  }
  
  let after_leaky_operations = MemoryTracker::active_objects(memory_tracker)
  
  // Now clean up the leaky resources
  for resource in leaky_resources {
    ManagedResource::cleanup(resource)
  }
  
  let after_leaky_cleanup = MemoryTracker::active_objects(memory_tracker)
  
  // Verify memory management
  assert_eq(after_proper_cleanup, initial_objects) // Proper cleanup should return to initial state
  assert_true(after_leaky_operations > after_proper_cleanup) // Leaky operations should increase object count
  assert_eq(after_leaky_cleanup, initial_objects) // Final cleanup should return to initial state
  
  // Test with large data structures
  let large_data_structures = []
  
  for i in 0..=10 {
    let large_structure = LargeDataStructure::new_with_capacity(10000)
    LargeDataStructure::populate_with_sample_data(large_structure)
    large_data_structures = large_data_structures.push(large_structure)
  }
  
  let with_large_structures = MemoryTracker::active_objects(memory_tracker)
  
  // Clean up large structures
  for structure in large_data_structures {
    LargeDataStructure::cleanup(structure)
  }
  
  let after_large_cleanup = MemoryTracker::active_objects(memory_tracker)
  
  // Verify large structure cleanup
  assert_true(with_large_structures > after_leaky_cleanup)
  assert_eq(after_large_cleanup, initial_objects)
}

// Test 7: Cross-Service Context Propagation
test "cross-service context propagation with headers" {
  // Create initial context
  let initial_trace_id = "trace123456789012345678901234567890"
  let initial_span_id = "span123456789012"
  let initial_ctx = SpanContext::new(initial_trace_id, initial_span_id, true, "initial_state")
  
  // Create propagator for HTTP headers
  let propagator = HttpTraceContextPropagator::new()
  
  // Inject context into HTTP headers
  let headers = []
  let injected_headers = Propagator::inject(propagator, initial_ctx, headers)
  
  // Verify injected headers
  let trace_header = Headers::get(injected_headers, "traceparent")
  match trace_header {
    Some(header_value) => {
      // Verify trace ID is present in header
      assert_true(header_value.contains(initial_trace_id))
    }
    None => assert_true(false)
  }
  
  // Simulate HTTP request to another service
  let http_request = HttpRequest::new(
    "POST",
    "https://service-b.example.com/api/process",
    injected_headers,
    Some("{\"data\": \"sample\"}")
  )
  
  // Extract context from HTTP headers in service B
  let extracted_ctx = Propagator::extract(propagator, HttpRequest::headers(http_request))
  
  // Verify extracted context matches original
  assert_eq(SpanContext::trace_id(extracted_ctx), initial_trace_id)
  assert_true(SpanContext::is_sampled(extracted_ctx))
  
  // Create new span in service B
  let service_b_span_id = "serviceb1234567890"
  let service_b_ctx = SpanContext::new(
    SpanContext::trace_id(extracted_ctx),
    service_b_span_id,
    SpanContext::is_sampled(extracted_ctx),
    "service_b_state"
  )
  let service_b_span = Span::new("service_b_operation", Server, service_b_ctx)
  
  // Add baggage items
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "user123")
  let final_baggage = Baggage::set_entry(updated_baggage, "request.id", "req456")
  
  // Inject baggage into headers for next service
  let headers_with_baggage = HttpRequest::headers(http_request)
  let headers_with_injected_baggage = BaggagePropagator::inject(final_baggage, headers_with_baggage)
  
  // Verify baggage propagation
  let user_id_header = Headers::get(headers_with_injected_baggage, "baggage")
  match user_id_header {
    Some(baggage_value) => {
      assert_true(baggage_value.contains("user.id=user123"))
      assert_true(baggage_value.contains("request.id=req456"))
    }
    None => assert_true(false)
  }
  
  // End service B span
  Span::set_status(service_b_span, Ok, Some("Service B operation completed"))
  Span::end(service_b_span)
}

// Test 8: Real-time Stream Processing
test "real-time stream processing with backpressure" {
  let stream_processor = StreamProcessor::new_with_buffer_size(1000)
  let processed_items = []
  
  // Set up processing pipeline
  StreamProcessor::add_stage(stream_processor, "filter", |item| {
    match item {
      StringValue(data) => {
        if data.contains("important") {
          Some(item)
        } else {
          None
        }
      }
      _ => Some(item)
    }
  })
  
  StreamProcessor::add_stage(stream_processor, "transform", |item| {
    match item {
      StringValue(data) => {
        StringValue("processed_" + data)
      }
      IntValue(num) => {
        IntValue(num * 2)
      }
      other => other
    }
  })
  
  StreamProcessor::add_stage(stream_processor, "aggregate", |item| {
    processed_items.push(item)
    Some(item) // Pass through
  })
  
  // Simulate high-volume stream
  let stream_data = []
  for i in 0..=500 {
    if i % 10 == 0 {
      stream_data = stream_data.push(StringValue("important_event_" + i.to_string()))
    } else {
      stream_data = stream_data.push(StringValue("regular_event_" + i.to_string()))
    }
  }
  
  // Process stream with backpressure handling
  let start_time = Time::current_time_ms()
  
  for item in stream_data {
    let result = StreamProcessor::process_with_backpressure(stream_processor, item)
    match result {
      Ok(_) => assert_true(true) // Successfully processed
      Err("buffer_full") => {
        // Handle backpressure
        StreamProcessor::wait_for_available_capacity(stream_processor)
        let retry_result = StreamProcessor::process_with_backpressure(stream_processor, item)
        match retry_result {
          Ok(_) => assert_true(true) // Successfully processed after wait
          Err(_) => assert_true(false) // Still failed
        }
      }
      Err(_) => assert_true(false) // Unexpected error
    }
  }
  
  let end_time = Time::current_time_ms()
  let processing_time = end_time - start_time
  
  // Verify processing results
  assert_eq(processed_items.length(), 500) // All items should be processed
  
  // Verify important items were filtered and transformed
  let important_items = processed_items.filter(|item| {
    match item {
      StringValue(data) => data.contains("processed_important_event_")
      _ => false
    }
  })
  
  assert_eq(important_items.length(), 50) // Every 10th item (500/10)
  
  // Verify processing time is reasonable
  assert_true(processing_time < 5000) // Should complete within 5 seconds
  
  // Test stream metrics
  let metrics = StreamProcessor::get_metrics(stream_processor)
  assert_eq(Metrics::items_processed(metrics), 500)
  assert_eq(Metrics::items_filtered(metrics), 450) // 500 - 50 important
  assert_true(Metrics::average_processing_time_ms(metrics) > 0)
}

// Test 9: Multi-tenant Isolation
test "multi-tenant isolation and security boundaries" {
  let tenant_manager = TenantManager::new()
  
  // Create tenants
  let tenant_a = TenantManager::create_tenant(tenant_manager, "tenant_a", "Tenant A Corporation")
  let tenant_b = TenantManager::create_tenant(tenant_manager, "tenant_b", "Tenant B LLC")
  
  // Create tenant-specific resources
  let tenant_a_resource = TenantResourceManager::create_resource(tenant_a, "telemetry_data")
  let tenant_b_resource = TenantResourceManager::create_resource(tenant_b, "telemetry_data")
  
  // Add data to tenant A
  TenantResource::add_attribute(tenant_a_resource, "sensitive_data", StringValue("tenant_a_secret"))
  TenantResource::add_attribute(tenant_a_resource, "user_count", IntValue(1000))
  TenantResource::add_attribute(tenant_a_resource, "revenue", FloatValue(1000000.50))
  
  // Add data to tenant B
  TenantResource::add_attribute(tenant_b_resource, "sensitive_data", StringValue("tenant_b_secret"))
  TenantResource::add_attribute(tenant_b_resource, "user_count", IntValue(500))
  TenantResource::add_attribute(tenant_b_resource, "revenue", FloatValue(500000.25))
  
  // Test isolation - tenant A cannot access tenant B data
  let tenant_a_access = TenantResource::get_attribute(tenant_a_resource, "sensitive_data")
  let tenant_b_access_from_a = TenantResource::get_attribute_from_tenant(tenant_a, "tenant_b", "sensitive_data")
  
  match tenant_a_access {
    Some(StringValue(data)) => assert_eq(data, "tenant_a_secret")
    _ => assert_true(false)
  }
  
  match tenant_b_access_from_a {
    Some(_) => assert_true(false) // Should not be able to access
    None => assert_true(true) // Expected - access denied
  }
  
  // Test cross-tenant query restrictions
  let cross_tenant_query = TenantResourceManager::query_all_tenants(tenant_manager, "sensitive_data")
  match cross_tenant_query {
    Ok(_) => assert_true(false) // Should not allow cross-tenant queries
    Err("access_denied") => assert_true(true) // Expected
    _ => assert_true(false)
  }
  
  // Test tenant-specific metrics aggregation
  let tenant_a_metrics = TenantResource::aggregate_metrics(tenant_a_resource)
  let tenant_b_metrics = TenantResource::aggregate_metrics(tenant_b_resource)
  
  match tenant_a_metrics {
    Some(metrics) => {
      assert_eq(Metrics::user_count(metrics), 1000)
      assert_eq(Metrics::revenue(metrics), 1000000.50)
    }
    None => assert_true(false)
  }
  
  match tenant_b_metrics {
    Some(metrics) => {
      assert_eq(Metrics::user_count(metrics), 500)
      assert_eq(Metrics::revenue(metrics), 500000.25)
    }
    None => assert_true(false)
  }
  
  // Test tenant isolation at network level
  let tenant_a_network_context = NetworkContext::new_for_tenant(tenant_a)
  let tenant_b_network_context = NetworkContext::new_for_tenant(tenant_b)
  
  // Verify network contexts are isolated
  assert_false(NetworkContext::can_access(tenant_a_network_context, tenant_b_network_context))
  assert_true(NetworkContext::can_access(tenant_a_network_context, tenant_a_network_context))
  assert_true(NetworkContext::can_access(tenant_b_network_context, tenant_b_network_context))
  
  // Test resource cleanup and tenant deletion
  TenantResourceManager::cleanup_resources(tenant_a)
  TenantManager::delete_tenant(tenant_manager, "tenant_a")
  
  // Verify tenant A resources are gone
  let deleted_tenant_access = TenantResourceManager::get_tenant(tenant_manager, "tenant_a")
  match deleted_tenant_access {
    Some(_) => assert_true(false) // Should not exist
    None => assert_true(true) // Expected - tenant deleted
  }
  
  // Verify tenant B resources are still accessible
  let tenant_b_still_accessible = TenantResourceManager::get_tenant(tenant_manager, "tenant_b")
  match tenant_b_still_accessible {
    Some(_) => assert_true(true) // Should still exist
    None => assert_true(false)
  }
}

// Test 10: Custom Metrics Dashboard
test "custom metrics dashboard with real-time updates" {
  let dashboard = CustomMetricsDashboard::new("Application Performance Dashboard")
  
  // Create custom metrics
  let response_time_metric = Dashboard::create_metric(dashboard, "response_time", Histogram, "Response Time", "ms")
  let error_rate_metric = Dashboard::create_metric(dashboard, "error_rate", Gauge, "Error Rate", "%")
  let throughput_metric = Dashboard::create_metric(dashboard, "throughput", Counter, "Request Throughput", "req/s")
  
  // Set up dashboard layout
  Dashboard::add_panel(dashboard, "Performance Overview", [
    response_time_metric,
    error_rate_metric,
    throughput_metric
  ])
  
  // Simulate real-time data updates
  for i in 0..=100 {
    // Simulate response times
    let response_time = 50.0 + (Math::random() * 200.0) // 50-250ms
    Histogram::record(response_time_metric, response_time)
    
    // Simulate error rate
    let error_rate = if i % 20 == 0 { 5.0 + (Math::random() * 10.0) } else { Math::random() * 2.0 }
    Gauge::set(error_rate_metric, error_rate)
    
    // Simulate throughput
    let throughput = 100.0 + (Math::random() * 50.0) // 100-150 req/s
    Counter::add(throughput_metric, throughput)
    
    // Update dashboard
    Dashboard::update_real_time(dashboard)
    
    // Small delay to simulate real-time updates
    Time::sleep(10) // 10ms
  }
  
  // Verify dashboard data
  let dashboard_data = Dashboard::get_current_data(dashboard)
  
  // Verify response time statistics
  let response_time_stats = Dashboard::get_metric_stats(dashboard_data, "response_time")
  match response_time_stats {
    Some(stats) => {
      assert_true(MetricStats::count(stats) > 0)
      assert_true(MetricStats::average(stats) >= 50.0)
      assert_true(MetricStats::average(stats) <= 250.0)
      assert_true(MetricStats::min(stats) >= 50.0)
      assert_true(MetricStats::max(stats) <= 250.0)
    }
    None => assert_true(false)
  }
  
  // Verify error rate
  let error_rate_value = Dashboard::get_metric_value(dashboard_data, "error_rate")
  match error_rate_value {
    Some(GaugeValue(value)) => assert_true(value >= 0.0 && value <= 15.0)
    _ => assert_true(false)
  }
  
  // Verify throughput
  let throughput_value = Dashboard::get_metric_value(dashboard_data, "throughput")
  match throughput_value {
    Some(CounterValue(value)) => assert_true(value > 0.0)
    _ => assert_true(false)
  }
  
  // Test dashboard alerts
  Dashboard::set_alert_threshold(dashboard, "response_time", 200.0, GreaterThan)
  Dashboard::set_alert_threshold(dashboard, "error_rate", 10.0, GreaterThan)
  
  // Trigger alerts with high values
  Histogram::record(response_time_metric, 250.0) // Above threshold
  Gauge::set(error_rate_metric, 12.0) // Above threshold
  
  Dashboard::update_real_time(dashboard)
  
  let active_alerts = Dashboard::get_active_alerts(dashboard)
  assert_eq(active_alerts.length(), 2) // Two alerts should be triggered
  
  // Verify alert details
  let response_time_alert = active_alerts.find(|alert| Alert::metric_name(alert) == "response_time")
  match response_time_alert {
    Some(alert) => {
      assert_eq(Alert::severity(alert), Warning)
      assert_true(Alert::message(alert).contains("exceeds threshold"))
    }
    None => assert_true(false)
  }
  
  let error_rate_alert = active_alerts.find(|alert| Alert::metric_name(alert) == "error_rate")
  match error_rate_alert {
    Some(alert) => {
      assert_eq(Alert::severity(alert), Warning)
      assert_true(Alert::message(alert).contains("exceeds threshold"))
    }
    None => assert_true(false)
  }
  
  // Test dashboard export
  let exported_data = Dashboard::export_data(dashboard, JsonFormat)
  assert_true(exported_data.length() > 0)
  
  // Test dashboard snapshot
  let snapshot = Dashboard::create_snapshot(dashboard)
  assert_true(DashboardSnapshot::timestamp(snapshot) > 0)
  assert_true(DashboardSnapshot::metric_count(snapshot) > 0)
  
  // Verify snapshot can be restored
  let restored_dashboard = Dashboard::from_snapshot(snapshot)
  let restored_data = Dashboard::get_current_data(restored_dashboard)
  
  let restored_response_time = Dashboard::get_metric_stats(restored_data, "response_time")
  match restored_response_time {
    Some(stats) => assert_true(MetricStats::count(stats) > 0)
    None => assert_true(false)
  }
}