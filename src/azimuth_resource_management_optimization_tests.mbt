// Azimuth Telemetry System - Resource Management Optimization Tests
// This file contains comprehensive test cases for resource management and optimization

// Test 1: Span Lifecycle Resource Management
test "span lifecycle resource management" {
  let resource_manager = ResourceManager::new()
  
  // Test span creation and cleanup
  let initial_memory = ResourceManager::get_memory_usage(resource_manager)
  let initial_span_count = ResourceManager::get_active_span_count(resource_manager)
  
  // Create spans
  let spans = []
  for i in 0..=100 {
    let trace_id = "trace_" + Int::to_string(i)
    let span_id = "span_" + Int::to_string(i)
    let span_ctx = SpanContext::new(trace_id, span_id, true, "resource_test_service")
    let span = Span::new("resource_test_operation", Server, span_ctx)
    
    // Add attributes and events
    for j in 0..=5 {
      let key = "attr_" + Int::to_string(j)
      let value = "value_" + Int::to_string(j)
      Span::set_attribute(span, key, StringValue(value))
    }
    
    for j in 0..=3 {
      let event_name = "event_" + Int::to_string(j)
      Span::add_event(span, event_name, None)
    }
    
    spans.push(span)
  }
  
  let after_creation_memory = ResourceManager::get_memory_usage(resource_manager)
  let after_creation_span_count = ResourceManager::get_active_span_count(resource_manager)
  
  // Verify resources were allocated
  assert_true(after_creation_memory > initial_memory)
  assert_eq(after_creation_span_count, initial_span_count + 100)
  
  // End spans and verify cleanup
  for span in spans {
    Span::end(span)
  }
  
  // Force garbage collection
  ResourceManager::force_gc(resource_manager)
  
  let after_cleanup_memory = ResourceManager::get_memory_usage(resource_manager)
  let after_cleanup_span_count = ResourceManager::get_active_span_count(resource_manager)
  
  // Verify resources were cleaned up
  assert_true(after_cleanup_span_count < after_creation_span_count)
  assert_true(after_cleanup_memory < after_creation_memory)
  
  // Verify memory efficiency
  let memory_reclaimed = after_creation_memory - after_cleanup_memory
  let memory_reclaimed_ratio = memory_reclaimed / (after_creation_memory - initial_memory)
  assert_true(memory_reclaimed_ratio > 0.7) // At least 70% of memory reclaimed
}

// Test 2: Metrics Resource Pooling
test "metrics resource pooling" {
  let resource_manager = ResourceManager::new()
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "resource_pooling_meter")
  
  // Test resource pooling for metrics
  let initial_memory = ResourceManager::get_memory_usage(resource_manager)
  
  // Create metrics without pooling (baseline)
  let non_pooled_metrics = []
  for i in 0..=100 {
    let counter_name = "non_pooled_counter_" + Int::to_string(i)
    let counter = Meter::create_counter(meter, counter_name, Some("Non-pooled counter"), Some("count"))
    non_pooled_metrics.push(counter)
  }
  
  let non_pooled_memory = ResourceManager::get_memory_usage(resource_manager)
  
  // Clean up non-pooled metrics
  non_pooled_metrics = []
  ResourceManager::force_gc(resource_manager)
  
  let after_non_pooled_cleanup = ResourceManager::get_memory_usage(resource_manager)
  
  // Create metrics with pooling
  let pooled_metrics = []
  let metric_pool = MetricPool::new(50) // Pool size of 50
  
  for i in 0..=100 {
    let counter_name = "pooled_counter_" + Int::to_string(i)
    let counter = MetricPool::get_or_create_counter(metric_pool, meter, counter_name, Some("Pooled counter"), Some("count"))
    pooled_metrics.push(counter)
  }
  
  let pooled_memory = ResourceManager::get_memory_usage(resource_manager)
  
  // Compare memory usage
  let non_pooled_memory_usage = non_pooled_memory - initial_memory
  let pooled_memory_usage = pooled_memory - after_non_pooled_cleanup
  
  // Pooling should use less memory
  assert_true(pooled_memory_usage < non_pooled_memory_usage)
  
  // Verify pooling efficiency
  let pooling_efficiency = 1.0 - (pooled_memory_usage / non_pooled_memory_usage)
  assert_true(pooling_efficiency > 0.3) // At least 30% memory savings
  
  // Test metric reuse
  let reused_counter = MetricPool::get_or_create_counter(metric_pool, meter, "pooled_counter_0", Some("Reused counter"), Some("count"))
  let reused_counter_ref = pooled_metrics[0]
  
  // Should be the same object (reference equality)
  assert_true(reused_counter === reused_counter_ref)
}

// Test 3: Attribute Value Resource Optimization
test "attribute value resource optimization" {
  let resource_manager = ResourceManager::new()
  
  // Test string interning for attribute values
  let initial_memory = ResourceManager::get_memory_usage(resource_manager)
  
  // Create spans with non-interned strings
  let non_interned_spans = []
  for i in 0..=100 {
    let trace_id = "trace_" + Int::to_string(i)
    let span_id = "span_" + Int::to_string(i)
    let span_ctx = SpanContext::new(trace_id, span_id, true, "string_test_service")
    let span = Span::new("string_test_operation", Server, span_ctx)
    
    // Add attributes with repeated values (non-interned)
    Span::set_attribute(span, "service.name", StringValue("test_service"))
    Span::set_attribute(span, "operation.type", StringValue("http_request"))
    Span::set_attribute(span, "environment", StringValue("production"))
    
    non_interned_spans.push(span)
  }
  
  let non_interned_memory = ResourceManager::get_memory_usage(resource_manager)
  
  // Clean up non-interned spans
  for span in non_interned_spans {
    Span::end(span)
  }
  non_interned_spans = []
  ResourceManager::force_gc(resource_manager)
  
  let after_non_interned_cleanup = ResourceManager::get_memory_usage(resource_manager)
  
  // Create spans with interned strings
  let string_interner = StringInterner::new()
  let interned_spans = []
  
  for i in 0..=100 {
    let trace_id = "trace_" + Int::to_string(i)
    let span_id = "span_" + Int::to_string(i)
    let span_ctx = SpanContext::new(trace_id, span_id, true, "string_test_service")
    let span = Span::new("string_test_operation", Server, span_ctx)
    
    // Add attributes with interned values
    let service_name = StringInterner::intern(string_interner, "test_service")
    let operation_type = StringInterner::intern(string_interner, "http_request")
    let environment = StringInterner::intern(string_interner, "production")
    
    Span::set_attribute(span, "service.name", StringValue(service_name))
    Span::set_attribute(span, "operation.type", StringValue(operation_type))
    Span::set_attribute(span, "environment", StringValue(environment))
    
    interned_spans.push(span)
  }
  
  let interned_memory = ResourceManager::get_memory_usage(resource_manager)
  
  // Compare memory usage
  let non_interned_memory_usage = non_interned_memory - initial_memory
  let interned_memory_usage = interned_memory - after_non_interned_cleanup
  
  // String interning should use less memory
  assert_true(interned_memory_usage < non_interned_memory_usage)
  
  // Verify interning efficiency
  let interning_efficiency = 1.0 - (interned_memory_usage / non_interned_memory_usage)
  assert_true(interning_efficiency > 0.2) // At least 20% memory savings
  
  // Clean up interned spans
  for span in interned_spans {
    Span::end(span)
  }
}

// Test 4: Batch Processing Resource Optimization
test "batch processing resource optimization" {
  let resource_manager = ResourceManager::new()
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "batch_processing_meter")
  
  // Test individual vs batch processing
  let counter = Meter::create_counter(meter, "batch_test_counter", Some("Batch test counter"), Some("count"))
  
  // Measure resource usage for individual processing
  let individual_start_memory = ResourceManager::get_memory_usage(resource_manager)
  let individual_start_time = Timer::now()
  
  for i in 0..=1000 {
    Counter::add(counter, 1.0)
  }
  
  let individual_end_time = Timer::now()
  let individual_end_memory = ResourceManager::get_memory_usage(resource_manager)
  
  // Reset for batch processing test
  ResourceManager::reset_metrics(resource_manager)
  
  // Measure resource usage for batch processing
  let batch_start_memory = ResourceManager::get_memory_usage(resource_manager)
  let batch_start_time = Timer::now()
  
  // Process in batches
  let batch_size = 100
  let batches = 1000 / batch_size
  
  for batch in 0..=batches {
    let batch_measurements = []
    for i in 0..=batch_size {
      batch_measurements.push(1.0)
    }
    Counter::add_batch(counter, batch_measurements)
  }
  
  let batch_end_time = Timer::now()
  let batch_end_memory = ResourceManager::get_memory_usage(resource_manager)
  
  // Compare resource usage
  let individual_time = individual_end_time - individual_start_time
  let batch_time = batch_end_time - batch_start_time
  
  let individual_memory = individual_end_memory - individual_start_memory
  let batch_memory = batch_end_memory - batch_start_memory
  
  // Batch processing should be more efficient
  assert_true(batch_time < individual_time)
  assert_true(batch_memory <= individual_memory)
  
  // Verify efficiency gains
  let time_efficiency = 1.0 - (batch_time / individual_time)
  let memory_efficiency = 1.0 - (batch_memory / individual_memory)
  
  assert_true(time_efficiency > 0.1) // At least 10% time savings
  assert_true(memory_efficiency >= 0.0) // Memory usage should not increase
}

// Test 5: Adaptive Resource Allocation
test "adaptive resource allocation" {
  let resource_manager = ResourceManager::new()
  let adaptive_allocator = AdaptiveResourceAllocator::new()
  
  // Test adaptive resource allocation based on load
  let low_load_scenario = LoadScenario::new("low", 10, 100) // 10 spans, 100 attributes per span
  let medium_load_scenario = LoadScenario::new("medium", 100, 500) // 100 spans, 500 attributes per span
  let high_load_scenario = LoadScenario::new("high", 1000, 1000) // 1000 spans, 1000 attributes per span
  
  // Test low load allocation
  let low_allocation = AdaptiveResourceAllocator::allocate(adaptive_allocator, low_load_scenario)
  let low_memory_pool = ResourceAllocation::memory_pool_size(low_allocation)
  let low_span_limit = ResourceAllocation::span_limit(low_allocation)
  
  // Test medium load allocation
  let medium_allocation = AdaptiveResourceAllocator::allocate(adaptive_allocator, medium_load_scenario)
  let medium_memory_pool = ResourceAllocation::memory_pool_size(medium_allocation)
  let medium_span_limit = ResourceAllocation::span_limit(medium_allocation)
  
  // Test high load allocation
  let high_allocation = AdaptiveResourceAllocator::allocate(adaptive_allocator, high_load_scenario)
  let high_memory_pool = ResourceAllocation::memory_pool_size(high_allocation)
  let high_span_limit = ResourceAllocation::span_limit(high_allocation)
  
  // Verify adaptive allocation
  assert_true(low_memory_pool < medium_memory_pool)
  assert_true(medium_memory_pool < high_memory_pool)
  
  assert_true(low_span_limit < medium_span_limit)
  assert_true(medium_span_limit < high_span_limit)
  
  // Test resource deallocation
  let deallocated_resources = AdaptiveResourceAllocator::deallocate(adaptive_allocator, high_allocation)
  assert_true(deallocated_resources > 0)
  
  // Verify resource monitoring
  let resource_monitor = ResourceMonitor::new()
  ResourceMonitor::start_monitoring(resource_monitor)
  
  // Simulate resource usage
  for i in 0..=100 {
    let trace_id = "trace_" + Int::to_string(i)
    let span_id = "span_" + Int::to_string(i)
    let span_ctx = SpanContext::new(trace_id, span_id, true, "monitoring_service")
    let span = Span::new("monitoring_operation", Server, span_ctx)
    Span::end(span)
  }
  
  let resource_stats = ResourceMonitor::get_stats(resource_monitor)
  assert_true(ResourceStats::peak_memory_usage(resource_stats) > 0)
  assert_true(ResourceStats::peak_span_count(resource_stats) > 0)
  assert_true(ResourceStats::avg_memory_per_span(resource_stats) > 0)
  
  ResourceMonitor::stop_monitoring(resource_monitor)
}

// Test 6: Resource Leak Detection and Prevention
test "resource leak detection and prevention" {
  let resource_manager = ResourceManager::new()
  let leak_detector = ResourceLeakDetector::new()
  
  // Enable leak detection
  LeakDetector::enable_detection(leak_detector)
  
  // Test normal resource usage (no leaks)
  let normal_spans = []
  for i in 0..=10 {
    let trace_id = "trace_" + Int::to_string(i)
    let span_id = "span_" + Int::to_string(i)
    let span_ctx = SpanContext::new(trace_id, span_id, true, "leak_test_service")
    let span = Span::new("leak_test_operation", Server, span_ctx)
    normal_spans.push(span)
    Span::end(span) // Properly end span
  }
  
  let normal_leak_report = LeakDetector::generate_report(leak_detector)
  assert_eq(LeakReport::leaked_spans(normal_leak_report), 0)
  
  // Test resource leak (spans not ended)
  let leaked_spans = []
  for i in 0..=10 {
    let trace_id = "leaked_trace_" + Int::to_string(i)
    let span_id = "leaked_span_" + Int::to_string(i)
    let span_ctx = SpanContext::new(trace_id, span_id, true, "leak_test_service")
    let span = Span::new("leaked_operation", Server, span_ctx)
    leaked_spans.push(span)
    // Don't end span to simulate leak
  }
  
  let leak_report = LeakDetector::generate_report(leak_detector)
  assert_eq(LeakReport::leaked_spans(leak_report), 10)
  
  // Test automatic leak prevention
  LeakDetector::enable_auto_cleanup(leak_detector, 1000) // Auto cleanup after 1000ms
  
  // Wait for auto cleanup (simulated)
  LeakDetector::simulate_time_passage(leak_detector, 1001)
  
  let cleanup_report = LeakDetector::generate_report(leak_detector)
  assert_eq(LeakReport::leaked_spans(cleanup_report), 0) // Should be cleaned up
  
  // Test memory leak detection
  let initial_memory = ResourceManager::get_memory_usage(resource_manager)
  
  // Simulate memory allocation without deallocation
  let memory_leak_objects = []
  for i in 0..=100 {
    let large_object = LargeObject::new(1024 * 1024) // 1MB object
    memory_leak_objects.push(large_object)
  }
  
  let after_allocation_memory = ResourceManager::get_memory_usage(resource_manager)
  let memory_increase = after_allocation_memory - initial_memory
  
  // Detect potential memory leak
  let leak_detected = LeakDetector::detect_memory_leak(leak_detector, memory_increase, 50 * 1024 * 1024) // 50MB threshold
  assert_true(leak_detected)
  
  // Test resource leak prevention strategies
  let prevention_strategy = ResourceLeakPrevention::new()
  ResourceLeakPrevention::enable_span_limiter(prevention_strategy, 1000)
  ResourceLeakPrevention::enable_memory_limiter(prevention_strategy, 100 * 1024 * 1024) // 100MB
  ResourceLeakPrevention::enable_attribute_limiter(prevention_strategy, 100)
  
  // Test prevention in action
  let prevention_test_spans = []
  for i in 0..=1100 { // Exceed span limit
    let trace_id = "prevention_trace_" + Int::to_string(i)
    let span_id = "prevention_span_" + Int::to_string(i)
    let span_ctx = SpanContext::new(trace_id, span_id, true, "prevention_service")
    let span = Span::new("prevention_operation", Server, span_ctx)
    
    // Add many attributes to test attribute limit
    for j in 0..=110 {
      let key = "attr_" + Int::to_string(j)
      let value = "value_" + Int::to_string(j)
      Span::set_attribute(span, key, StringValue(value))
    }
    
    prevention_test_spans.push(span)
  }
  
  // Verify prevention worked
  let prevention_report = LeakDetector::generate_report(leak_detector)
  assert_true(LeakReport::leaked_spans(prevention_report) < 100) // Should be limited
  
  LeakDetector::disable_detection(leak_detector)
}