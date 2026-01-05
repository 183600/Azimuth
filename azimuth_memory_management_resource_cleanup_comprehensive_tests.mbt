// Azimuth Telemetry System - Memory Management Resource Cleanup Comprehensive Tests
// This file contains comprehensive test cases for memory management and resource cleanup

// Test 1: Span Memory Management
test "span memory management and cleanup" {
  let memory_manager = MemoryManager::new()
  memory_manager.set_memory_limit(10000000) // 10MB limit
  
  // Create spans and track memory usage
  let span_count = 1000
  let mut spans = []
  let initial_memory = memory_manager.get_current_usage()
  
  // Create spans
  for i in 0..span_count {
    let span = Span::new("memory_test_span_" + i.to_string(), Internal, SpanContext::new("trace_" + i.to_string(), "span_" + i.to_string(), true, ""))
    
    // Add events and attributes to increase memory usage
    for j in 0..10 {
      let event_name = "event_" + j.to_string()
      Span::add_event(span, event_name, Some([("event_attr", StringValue("event_value_" + j.to_string()))]))
    }
    
    // Add attributes
    for k in 0..5 {
      let attr_key = "attr_" + k.to_string()
      let attr_value = StringValue("attr_value_" + k.to_string())
      Span::set_attribute(span, attr_key, attr_value)
    }
    
    spans.push(span)
  }
  
  let peak_memory = memory_manager.get_current_usage()
  let memory_increase = peak_memory - initial_memory
  
  // Verify memory usage increased
  assert_true(memory_increase > 0)
  assert_true(memory_increase < memory_manager.get_memory_limit())
  
  // End spans to trigger cleanup
  for span in spans {
    Span::end(span)
  }
  
  // Force garbage collection
  memory_manager.force_gc()
  
  let cleanup_memory = memory_manager.get_current_usage()
  let memory_after_cleanup = cleanup_memory - initial_memory
  
  // Verify memory was reclaimed
  assert_true(memory_after_cleanup < memory_increase)
  
  // Check for memory leaks
  let leak_report = memory_manager.detect_leaks()
  assert_eq(leak_report.leaked_spans, 0)
}

// Test 2: Metric Memory Management
test "metric memory management and cleanup" {
  let memory_manager = MemoryManager::new()
  memory_manager.set_memory_limit(5000000) // 5MB limit
  
  // Create metrics and track memory usage
  let metric_count = 500
  let data_points_per_metric = 100
  let mut metrics = []
  let initial_memory = memory_manager.get_current_usage()
  
  // Create metrics
  for i in 0..metric_count {
    let metric = Metric::new_counter("memory_test_metric_" + i.to_string(), "Test metric " + i.to_string(), "count")
    
    // Add data points
    for j in 0..data_points_per_metric {
      let value = (i * data_points_per_metric + j).to_float()
      let attrs = Attributes::new()
      Attributes::set(attrs, "dp_index", IntValue(j))
      Metric::add_data_point(metric, DataPoint::new(value, attrs))
    }
    
    metrics.push(metric)
  }
  
  let peak_memory = memory_manager.get_current_usage()
  let memory_increase = peak_memory - initial_memory
  
  // Verify memory usage increased
  assert_true(memory_increase > 0)
  assert_true(memory_increase < memory_manager.get_memory_limit())
  
  // Clear metrics to trigger cleanup
  for metric in metrics {
    Metric::clear_data_points(metric)
  }
  
  // Force garbage collection
  memory_manager.force_gc()
  
  let cleanup_memory = memory_manager.get_current_usage()
  let memory_after_cleanup = cleanup_memory - initial_memory
  
  // Verify memory was reclaimed
  assert_true(memory_after_cleanup < memory_increase)
  
  // Check for memory leaks
  let leak_report = memory_manager.detect_leaks()
  assert_eq(leak_report.leaked_metrics, 0)
}

// Test 3: Log Record Memory Management
test "log record memory management and cleanup" {
  let memory_manager = MemoryManager::new()
  memory_manager.set_memory_limit(3000000) // 3MB limit
  
  // Create log records and track memory usage
  let log_count = 2000
  let mut logs = []
  let initial_memory = memory_manager.get_current_usage()
  
  // Create log records
  for i in 0..log_count {
    let message = "This is a test log message number " + i.to_string() + " with some additional content to increase memory usage"
    let log_record = LogRecord::new(Info, message)
    
    // Add attributes
    for j in 0..5 {
      let attr_key = "log_attr_" + j.to_string()
      let attr_value = StringValue("log_value_" + i.to_string() + "_" + j.to_string())
      LogRecord::set_attribute(log_record, attr_key, attr_value)
    }
    
    logs.push(log_record)
  }
  
  let peak_memory = memory_manager.get_current_usage()
  let memory_increase = peak_memory - initial_memory
  
  // Verify memory usage increased
  assert_true(memory_increase > 0)
  assert_true(memory_increase < memory_manager.get_memory_limit())
  
  // Clear logs to trigger cleanup
  logs = []
  
  // Force garbage collection
  memory_manager.force_gc()
  
  let cleanup_memory = memory_manager.get_current_usage()
  let memory_after_cleanup = cleanup_memory - initial_memory
  
  // Verify memory was reclaimed
  assert_true(memory_after_cleanup < memory_increase)
  
  // Check for memory leaks
  let leak_report = memory_manager.detect_leaks()
  assert_eq(leak_report.leaked_logs, 0)
}

// Test 4: Attribute Memory Management
test "attribute memory management and cleanup" {
  let memory_manager = MemoryManager::new()
  memory_manager.set_memory_limit(2000000) // 2MB limit
  
  // Create attributes and track memory usage
  let attribute_count = 1000
  let mut attributes = []
  let initial_memory = memory_manager.get_current_usage()
  
  // Create attributes
  for i in 0..attribute_count {
    let attrs = Attributes::new()
    
    // Add string attributes
    for j in 0..10 {
      let key = "string_attr_" + j.to_string()
      let value = StringValue("string_value_" + i.to_string() + "_" + j.to_string())
      Attributes::set(attrs, key, value)
    }
    
    // Add int attributes
    for k in 0..10 {
      let key = "int_attr_" + k.to_string()
      let value = IntValue(i * 10 + k)
      Attributes::set(attrs, key, value)
    }
    
    // Add array attributes
    let array_value = ArrayStringValue(["item1", "item2", "item3", "item4", "item5"])
    Attributes::set(attrs, "array_attr", array_value)
    
    attributes.push(attrs)
  }
  
  let peak_memory = memory_manager.get_current_usage()
  let memory_increase = peak_memory - initial_memory
  
  // Verify memory usage increased
  assert_true(memory_increase > 0)
  assert_true(memory_increase < memory_manager.get_memory_limit())
  
  // Clear attributes to trigger cleanup
  attributes = []
  
  // Force garbage collection
  memory_manager.force_gc()
  
  let cleanup_memory = memory_manager.get_current_usage()
  let memory_after_cleanup = cleanup_memory - initial_memory
  
  // Verify memory was reclaimed
  assert_true(memory_after_cleanup < memory_increase)
  
  // Check for memory leaks
  let leak_report = memory_manager.detect_leaks()
  assert_eq(leak_report.leaked_attributes, 0)
}

// Test 5: Circular Reference Detection and Cleanup
test "circular reference detection and cleanup" {
  let memory_manager = MemoryManager::new()
  memory_manager.enable_circular_reference_detection(true)
  
  // Create objects with circular references
  let span1 = Span::new("span1", Internal, SpanContext::new("trace1", "span1", true, ""))
  let span2 = Span::new("span2", Internal, SpanContext::new("trace2", "span2", true, ""))
  
  // Create circular references
  Span::add_linked_span(span1, span2)
  Span::add_linked_span(span2, span1)
  
  // Create attributes with circular references
  let attrs1 = Attributes::new()
  let attrs2 = Attributes::new()
  
  Attributes::set(attrs1, "linked_attrs", AttributeValue::Attributes(attrs2))
  Attributes::set(attrs2, "linked_attrs", AttributeValue::Attributes(attrs1))
  
  // Add attributes to spans
  Span::set_attributes(span1, attrs1)
  Span::set_attributes(span2, attrs2)
  
  // Detect circular references
  let circular_refs = memory_manager.detect_circular_references()
  assert_true(circular_refs.length() > 0)
  
  // Break circular references
  Span::remove_linked_span(span1, span2)
  Span::remove_linked_span(span2, span1)
  
  Attributes::remove(attrs1, "linked_attrs")
  Attributes::remove(attrs2, "linked_attrs")
  
  // Verify circular references are resolved
  let resolved_circular_refs = memory_manager.detect_circular_references()
  assert_eq(resolved_circular_refs.length(), 0)
  
  // Force garbage collection
  memory_manager.force_gc()
  
  // Check for memory leaks
  let leak_report = memory_manager.detect_leaks()
  assert_eq(leak_report.leaked_spans, 0)
}

// Test 6: Memory Pool Management
test "memory pool management" {
  let memory_pool = MemoryPool::new()
  
  // Configure memory pools for different object types
  memory_pool.create_pool("span", 100, 1000) // Min 100, Max 1000 spans
  memory_pool.create_pool("metric", 200, 2000) // Min 200, Max 2000 metrics
  memory_pool.create_pool("log", 500, 5000) // Min 500, Max 5000 logs
  
  // Test pool allocation
  let mut spans = []
  let mut metrics = []
  let mut logs = []
  
  // Allocate spans
  for i in 0..500 {
    let span = memory_pool.allocate_from_pool("span")
    match span {
      Some(s) => {
        Span::initialize(s, "pool_span_" + i.to_string(), Internal, SpanContext::new("trace", "span", true, ""))
        spans.push(s)
      }
      None => assert_true(false) // Should be able to allocate
    }
  }
  
  // Allocate metrics
  for i in 0..1000 {
    let metric = memory_pool.allocate_from_pool("metric")
    match metric {
      Some(m) => {
        Metric::initialize(m, "pool_metric_" + i.to_string(), "Test metric", "count")
        metrics.push(m)
      }
      None => assert_true(false) // Should be able to allocate
    }
  }
  
  // Allocate logs
  for i in 0..2500 {
    let log = memory_pool.allocate_from_pool("log")
    match log {
      Some(l) => {
        LogRecord::initialize(l, Info, "Pool log message " + i.to_string())
        logs.push(l)
      }
      None => assert_true(false) // Should be able to allocate
    }
  }
  
  // Verify pool statistics
  let span_pool_stats = memory_pool.get_pool_stats("span")
  assert_eq(span_pool_stats.allocated, 500)
  assert_eq(span_pool_stats.available, 500)
  
  let metric_pool_stats = memory_pool.get_pool_stats("metric")
  assert_eq(metric_pool_stats.allocated, 1000)
  assert_eq(metric_pool_stats.available, 1000)
  
  let log_pool_stats = memory_pool.get_pool_stats("log")
  assert_eq(log_pool_stats.allocated, 2500)
  assert_eq(log_pool_stats.available, 2500)
  
  // Return objects to pools
  for span in spans {
    memory_pool.return_to_pool("span", span)
  }
  
  for metric in metrics {
    memory_pool.return_to_pool("metric", metric)
  }
  
  for log in logs {
    memory_pool.return_to_pool("log", log)
  }
  
  // Verify objects were returned to pools
  let span_pool_stats_after = memory_pool.get_pool_stats("span")
  assert_eq(span_pool_stats_after.allocated, 0)
  assert_eq(span_pool_stats_after.available, 1000)
  
  let metric_pool_stats_after = memory_pool.get_pool_stats("metric")
  assert_eq(metric_pool_stats_after.allocated, 0)
  assert_eq(metric_pool_stats_after.available, 2000)
  
  let log_pool_stats_after = memory_pool.get_pool_stats("log")
  assert_eq(log_pool_stats_after.allocated, 0)
  assert_eq(log_pool_stats_after.available, 5000)
}

// Test 7: Memory Pressure Handling
test "memory pressure handling" {
  let memory_manager = MemoryManager::new()
  memory_manager.set_memory_limit(1000000) // 1MB limit
  memory_manager.enable_pressure_handling(true)
  
  // Configure pressure thresholds
  memory_manager.set_pressure_threshold(0.8) // 80% of limit
  memory_manager.set_critical_threshold(0.95) // 95% of limit
  
  // Create objects to increase memory usage
  let mut objects = []
  let initial_memory = memory_manager.get_current_usage()
  let memory_limit = memory_manager.get_memory_limit()
  
  // Keep adding objects until memory pressure is detected
  let mut pressure_detected = false
  let mut critical_detected = false
  
  while not pressure_detected {
    let obj = LargeObject::new(10000) // 10KB per object
    objects.push(obj)
    
    let current_memory = memory_manager.get_current_usage()
    let memory_ratio = current_memory.to_float() / memory_limit.to_float()
    
    if memory_ratio >= 0.95 {
      critical_detected = true
      pressure_detected = true
    } else if memory_ratio >= 0.8 {
      pressure_detected = true
    }
    
    // Safety check to prevent infinite loop
    if objects.length() > 200 {
      break
    }
  }
  
  // Verify pressure was detected
  assert_true(pressure_detected)
  
  // Check pressure handling actions
  if critical_detected {
    // In critical state, aggressive cleanup should be triggered
    let cleanup_actions = memory_manager.get_cleanup_actions()
    assert_true(cleanup_actions.contains("aggressive_gc"))
    assert_true(cleanup_actions.contains("pool_clear"))
  } else {
    // In pressure state, moderate cleanup should be triggered
    let cleanup_actions = memory_manager.get_cleanup_actions()
    assert_true(cleanup_actions.contains("gc"))
    assert_true(cleanup_actions.contains("pool_shrink"))
  }
  
  // Release some objects to reduce pressure
  let objects_to_release = objects.length() / 2
  for i in 0..objects_to_release {
    objects.pop()
  }
  
  // Force cleanup
  memory_manager.handle_pressure()
  
  // Verify memory usage decreased
  let final_memory = memory_manager.get_current_usage()
  let memory_ratio = final_memory.to_float() / memory_limit.to_float()
  assert_true(memory_ratio < 0.8)
}

// Test 8: Resource Lifecycle Management
test "resource lifecycle management" {
  let resource_manager = ResourceManager::new()
  
  // Create resources with different lifecycles
  let short_lived_resource = resource_manager.create_resource("short_lived", ResourceLifecycle::Short)
  let medium_lived_resource = resource_manager.create_resource("medium_lived", ResourceLifecycle::Medium)
  let long_lived_resource = resource_manager.create_resource("long_lived", ResourceLifecycle::Long)
  
  // Track resource creation
  let creation_time = current_time_millis()
  
  // Use resources
  resource_manager.use_resource(short_lived_resource)
  resource_manager.use_resource(medium_lived_resource)
  resource_manager.use_resource(long_lived_resource)
  
  // Simulate time passage for short-lived resource
  simulate_time_passage(30000) // 30 seconds
  
  // Check if short-lived resource should be cleaned up
  let current_time = current_time_millis()
  let resource_age = current_time - creation_time
  
  if resource_age >= 30000 { // 30 seconds
    resource_manager.cleanup_resource(short_lived_resource)
    assert_false(resource_manager.is_resource_active(short_lived_resource))
  }
  
  // Simulate more time passage for medium-lived resource
  simulate_time_passage(270000) // Additional 4.5 minutes (total 5 minutes)
  
  // Check if medium-lived resource should be cleaned up
  let current_time = current_time_millis()
  let resource_age = current_time - creation_time
  
  if resource_age >= 300000 { // 5 minutes
    resource_manager.cleanup_resource(medium_lived_resource)
    assert_false(resource_manager.is_resource_active(medium_lived_resource))
  }
  
  // Long-lived resource should still be active
  assert_true(resource_manager.is_resource_active(long_lived_resource))
  
  // Manually cleanup long-lived resource
  resource_manager.cleanup_resource(long_lived_resource)
  assert_false(resource_manager.is_resource_active(long_lived_resource))
}

// Test 9: Memory Leak Detection
test "memory leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  
  // Enable leak detection
  leak_detector.enable_detection(true)
  leak_detector.set_detection_threshold(100) // Detect leaks of 100+ objects
  
  // Create objects that might leak
  let mut potential_leaks = []
  
  // Create spans without proper cleanup
  for i in 0..150 {
    let span = Span::new("leak_test_span_" + i.to_string(), Internal, SpanContext::new("trace", "span", true, ""))
    potential_leaks.push(span)
    // Don't end spans to simulate leak
  }
  
  // Create metrics without proper cleanup
  for i in 0..120 {
    let metric = Metric::new_counter("leak_test_metric_" + i.to_string(), "Test metric", "count")
    potential_leaks.push(metric)
    // Don't clear metrics to simulate leak
  }
  
  // Create logs without proper cleanup
  for i in 0..200 {
    let log = LogRecord::new(Info, "Leak test log " + i.to_string())
    potential_leaks.push(log)
    // Don't clear logs to simulate leak
  }
  
  // Run leak detection
  let leak_report = leak_detector.detect_leaks()
  
  // Verify leaks are detected
  assert_true(leak_report.has_leaks)
  assert_eq(leak_report.leaked_spans, 150)
  assert_eq(leak_report.leaked_metrics, 120)
  assert_eq(leak_report.leaked_logs, 200)
  
  // Fix leaks by properly cleaning up objects
  for obj in potential_leaks {
    match obj {
      Span(s) => Span::end(s),
      Metric(m) => Metric::clear_data_points(m),
      LogRecord(l) => LogRecord::cleanup(l),
      _ => assert_true(false)
    }
  }
  
  // Run leak detection again
  let fixed_leak_report = leak_detector.detect_leaks()
  
  // Verify leaks are fixed
  assert_false(fixed_leak_report.has_leaks)
  assert_eq(fixed_leak_report.leaked_spans, 0)
  assert_eq(fixed_leak_report.leaked_metrics, 0)
  assert_eq(fixed_leak_report.leaked_logs, 0)
}

// Test 10: Memory Usage Optimization
test "memory usage optimization" {
  let memory_optimizer = MemoryOptimizer::new()
  
  // Enable optimization strategies
  memory_optimizer.enable_strategy(ObjectPooling)
  memory_optimizer.enable_strategy(LazyLoading)
  memory_optimizer.enable_strategy(Compression)
  memory_optimizer.enable_strategy( Deduplication)
  
  // Create telemetry data with optimization
  let telemetry_data = TelemetryData::new()
  
  // Add spans with similar attributes (for deduplication)
  for i in 0..100 {
    let span = Span::new("optimized_span_" + i.to_string(), Internal, SpanContext::new("trace", "span_" + i.to_string(), true, ""))
    
    // Add common attributes (for deduplication)
    Span::set_attribute(span, "service.name", StringValue("optimized_service"))
    Span::set_attribute(span, "service.version", StringValue("1.0.0"))
    Span::set_attribute(span, "environment", StringValue("production"))
    
    // Add unique attributes
    Span::set_attribute(span, "span.id", StringValue("span_" + i.to_string()))
    
    telemetry_data.add_span(span)
  }
  
  // Optimize memory usage
  let optimization_result = memory_optimizer.optimize(telemetry_data)
  
  // Verify optimization was applied
  assert_true(optimization_result.optimized)
  assert_true(optimization_result.memory_reduction > 0)
  
  // Verify specific optimizations
  assert_true(optimization_result.applied_strategies.contains(ObjectPooling))
  assert_true(optimization_result.applied_strategies.contains(Deduplication))
  
  // Verify data integrity after optimization
  let spans = telemetry_data.get_spans()
  assert_eq(spans.length(), 100)
  
  for span in spans {
    let service_name = Span::get_attribute(span, "service.name")
    match service_name {
      Some(StringValue(name)) => assert_eq(name, "optimized_service")
      _ => assert_true(false)
    }
    
    let service_version = Span::get_attribute(span, "service.version")
    match service_version {
      Some(StringValue(version)) => assert_eq(version, "1.0.0")
      _ => assert_true(false)
    }
  }
  
  // Check memory usage before and after optimization
  let memory_before = optimization_result.memory_before
  let memory_after = optimization_result.memory_after
  let memory_reduction_percentage = (memory_before - memory_after).to_float() / memory_before.to_float() * 100.0
  
  // Verify significant memory reduction
  assert_true(memory_reduction_percentage > 10.0) // At least 10% reduction
}