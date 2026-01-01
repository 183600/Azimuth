// Specialized Test Cases for Azimuth Telemetry System
// This file contains focused test cases for specific advanced functionality

// Test 1: Telemetry data serialization and deserialization
test "telemetry data serialization" {
  let telemetry_data = TelemetryData::new()
  
  // Test serialization of complex attribute structures
  let complex_attrs = Attributes::new()
  Attributes::set(complex_attrs, "nested.data", StringValue("{\"key\":\"value\",\"number\":42}"))
  Attributes::set(complex_attrs, "timestamp", IntValue(1640995200000))
  Attributes::set(complex_attrs, "metrics.active", BoolValue(true))
  
  let serialized = TelemetryData::serialize(telemetry_data, complex_attrs)
  assert_true(String::length(serialized) > 0)
  
  // Test deserialization integrity
  let deserialized = TelemetryData::deserialize(serialized)
  let original_value = Attributes::get(complex_attrs, "nested.data")
  let restored_value = Attributes::get(deserialized, "nested.data")
  assert_eq(original_value, restored_value)
}

// Test 2: Concurrent telemetry operations
test "concurrent telemetry operations" {
  let telemetry_provider = TelemetryProvider::new()
  
  // Test concurrent span creation
  let span1 = telemetry_provider::create_span("concurrent-operation-1")
  let span2 = telemetry_provider::create_span("concurrent-operation-2")
  let span3 = telemetry_provider::create_span("concurrent-operation-3")
  
  // Test concurrent attribute setting
  Attributes::set(span1::attributes, "thread.id", IntValue(1))
  Attributes::set(span2::attributes, "thread.id", IntValue(2))
  Attributes::set(span3::attributes, "thread.id", IntValue(3))
  
  // Verify concurrent operations don't interfere
  let thread1_id = Attributes::get(span1::attributes, "thread.id")
  let thread2_id = Attributes::get(span2::attributes, "thread.id")
  let thread3_id = Attributes::get(span3::attributes, "thread.id")
  
  assert_eq(thread1_id, Some(IntValue(1)))
  assert_eq(thread2_id, Some(IntValue(2)))
  assert_eq(thread3_id, Some(IntValue(3)))
}

// Test 3: Error boundary and recovery
test "error boundary handling" {
  let error_handler = ErrorHandler::new()
  
  // Test graceful handling of invalid telemetry data
  let invalid_data = "invalid telemetry data"
  let result = error_handler::handle_invalid_data(invalid_data)
  assert_eq(result.status, "handled")
  
  // Test error recovery mechanisms
  let telemetry_system = TelemetrySystem::new()
  let recovered_state = telemetry_system::recover_from_error(error_handler)
  assert_true(recovered_state.is_healthy)
  
  // Test error propagation through telemetry chain
  let error_context = ErrorContext::new("test.error", "Test error for boundary handling")
  let propagated_error = telemetry_system::propagate_error(error_context)
  assert_true(String::contains(propagated_error.message, "test.error"))
}

// Test 4: Resource management and cleanup
test "resource management" {
  let resource_manager = ResourceManager::new()
  
  // Test resource allocation
  let resource1 = resource_manager::allocate("span-resource", 1024)
  let resource2 = resource_manager::allocate("metric-resource", 512)
  
  assert_true(resource_manager::is_allocated(resource1))
  assert_true(resource_manager::is_allocated(resource2))
  
  // Test resource usage tracking
  let usage_stats = resource_manager::get_usage_stats()
  assert_eq(usage_stats.total_allocated, 1536)
  assert_eq(usage_stats.active_resources, 2)
  
  // Test resource cleanup
  resource_manager::deallocate(resource1)
  assert_false(resource_manager::is_allocated(resource1))
  assert_eq(resource_manager::get_usage_stats().active_resources, 1)
}

// Test 5: Advanced metrics aggregation
test "metrics aggregation" {
  let metrics_collector = MetricsCollector::new()
  
  // Test metric collection over time windows
  metrics_collector::record_metric("response.time", 120.5)
  metrics_collector::record_metric("response.time", 95.2)
  metrics_collector::record_metric("response.time", 150.8)
  metrics_collector::record_metric("response.time", 87.3)
  
  // Test aggregation functions
  let avg_response = metrics_collector::get_average("response.time")
  let max_response = metrics_collector::get_maximum("response.time")
  let min_response = metrics_collector::get_minimum("response.time")
  
  assert_true(avg_response > 110.0 && avg_response < 115.0)
  assert_eq(max_response, 150.8)
  assert_eq(min_response, 87.3)
  
  // Test percentile calculations
  let p95_response = metrics_collector::get_percentile("response.time", 95)
  assert_true(p95_response > 140.0)
}

// Test 6: Trace context propagation across service boundaries
test "cross-service trace propagation" {
  let trace_propagator = TraceContextPropagator::new()
  
  // Create initial trace context
  let original_trace = TraceContext::generate("service-A", "operation-1")
  
  // Test context injection into carrier
  let carrier = TextMapCarrier::new()
  trace_propagator::inject(original_trace, carrier)
  
  // Test context extraction from carrier
  let extracted_trace = trace_propagator::extract(carrier)
  
  // Verify trace integrity across boundaries
  assert_eq(original_trace.trace_id, extracted_trace.trace_id)
  assert_eq(original_trace.span_id, extracted_trace.parent_span_id)
  
  // Test context continuation in downstream service
  let downstream_trace = TraceContext::create_child(extracted_trace, "service-B", "operation-2")
  assert_eq(downstream_trace.trace_id, original_trace.trace_id)
  assert_true(downstream_trace.span_id != original_trace.span_id)
}

// Test 7: Telemetry configuration management
test "telemetry configuration" {
  let config_manager = ConfigManager::new()
  
  // Test configuration loading and validation
  let config = config_manager::load_from_file("telemetry.config")
  assert_true(config_manager::is_valid(config))
  
  // Test dynamic configuration updates
  let updated_config = config_manager::update_setting(config, "sampling.rate", 0.1)
  assert_eq(updated_config.sampling_rate, 0.1)
  
  // Test configuration persistence
  config_manager::save_config(updated_config, "telemetry.updated.config")
  let reloaded_config = config_manager::load_from_file("telemetry.updated.config")
  assert_eq(reloaded_config.sampling_rate, 0.1)
}

// Test 8: Baggage propagation with metadata
test "enhanced baggage propagation" {
  let baggage_manager = BaggageManager::new()
  
  // Test baggage with metadata
  let baggage_entry = BaggageEntry::new("user.id", "12345")
  baggage_entry::add_metadata(baggage_entry, "source", "auth.service")
  baggage_entry::add_metadata(baggage_entry, "timestamp", "1640995200")
  
  let baggage = Baggage::new()
  let updated_baggage = baggage_manager::set_entry(baggage, baggage_entry)
  
  // Test baggage propagation with metadata preservation
  let propagated_baggage = baggage_manager::propagate(updated_baggage)
  let propagated_entry = baggage_manager::get_entry(propagated_baggage, "user.id")
  
  assert_eq(propagated_entry.value, "12345")
  assert_true(baggage_entry::has_metadata(propagated_entry, "source"))
  assert_true(baggage_entry::has_metadata(propagated_entry, "timestamp"))
}