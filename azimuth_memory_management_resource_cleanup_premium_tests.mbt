// Premium Memory Management and Resource Cleanup Tests for Azimuth
// This file contains comprehensive test cases for memory management and resource cleanup

// Test 1: Span Memory Management
test "span memory management" {
  let initial_memory = MemoryProfiler::get_allocated_memory()
  let memory_tracker = MemoryTracker::new()
  
  // Create spans and track memory usage
  let spans = []
  for i in 0..=1000 {
    let span = Span::new("memory_test_span_" + i.to_string(), Internal, 
                          SpanContext::new("trace_" + i.to_string(), "span_" + i.to_string(), true, ""))
    
    // Add attributes to increase memory usage
    Span::set_attribute(span, "iteration", IntValue(i))
    Span::set_attribute(span, "name", StringValue("test_span_" + i.to_string()))
    Span::set_attribute(span, "timestamp", IntValue(Time::now()))
    
    // Add events to increase memory usage
    Span::add_event(span, "span_created", Some([
      ("iteration", IntValue(i)),
      ("memory_phase", StringValue("creation"))
    ]))
    
    MemoryTracker::track(memory_tracker, span)
    spans.push(span)
  }
  
  let after_creation_memory = MemoryProfiler::get_allocated_memory()
  let memory_increase = after_creation_memory - initial_memory
  
  // Verify memory usage is reasonable
  assert_true(memory_increase > 0) // Memory should have increased
  assert_true(memory_increase < 10 * 1024 * 1024) // Should be less than 10MB
  
  // Verify memory tracker
  let tracked_objects = MemoryTracker::get_tracked_objects(memory_tracker)
  assert_eq(tracked_objects.length(), 1001)
  
  // End all spans and verify memory cleanup
  for span in spans {
    Span::end(span)
  }
  
  // Clear references
  spans = []
  
  // Force garbage collection if available
  MemoryProfiler::force_gc()
  
  let after_cleanup_memory = MemoryProfiler::get_allocated_memory()
  let memory_reclaimed = after_creation_memory - after_cleanup_memory
  
  // Verify most memory was reclaimed
  assert_true(memory_reclaimed > memory_increase * 0.7) // At least 70% reclaimed
  
  // Verify memory tracker
  let remaining_tracked = MemoryTracker::get_tracked_objects(memory_tracker)
  assert_true(remaining_tracked.length() < tracked_objects.length() * 0.3) // Most objects cleaned up
}

// Test 2: Metrics Memory Management
test "metrics memory management" {
  let initial_memory = MemoryProfiler::get_allocated_memory()
  let memory_tracker = MemoryTracker::new()
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "memory_test_meter")
  
  // Create metrics instruments and track memory usage
  let instruments = []
  for i in 0..=500 {
    let counter = Meter::create_counter(meter, "counter_" + i.to_string(), 
                                       Some("Test counter " + i.to_string()), Some("count"))
    let histogram = Meter::create_histogram(meter, "histogram_" + i.to_string(), 
                                           Some("Test histogram " + i.to_string()), Some("ms"))
    let gauge = Meter::create_gauge(meter, "gauge_" + i.to_string(), 
                                   Some("Test gauge " + i.to_string()), Some("value"))
    
    MemoryTracker::track(memory_tracker, counter)
    MemoryTracker::track(memory_tracker, histogram)
    MemoryTracker::track(memory_tracker, gauge)
    
    instruments.push((counter, histogram, gauge))
  }
  
  // Record metrics to increase memory usage
  for (counter, histogram, gauge) in instruments {
    for i in 0..=100 {
      let attrs = Attributes::new()
      Attributes::set(attrs, "iteration", IntValue(i))
      
      Counter::add(counter, i.to_float(), Some(attrs))
      Histogram::record(histogram, (i * 10).to_float(), Some(attrs))
      Gauge::record(gauge, i.to_float())
    }
  }
  
  let after_metrics_memory = MemoryProfiler::get_allocated_memory()
  let metrics_memory_increase = after_metrics_memory - initial_memory
  
  // Verify memory usage is reasonable
  assert_true(metrics_memory_increase > 0)
  assert_true(metrics_memory_increase < 5 * 1024 * 1024) // Should be less than 5MB
  
  // Clear references
  instruments = []
  
  // Force garbage collection
  MemoryProfiler::force_gc()
  
  let after_metrics_cleanup = MemoryProfiler::get_allocated_memory()
  let metrics_memory_reclaimed = after_metrics_memory - after_metrics_cleanup
  
  // Verify metrics memory was reclaimed
  assert_true(metrics_memory_reclaimed > metrics_memory_increase * 0.6) // At least 60% reclaimed
}

// Test 3: Attributes Memory Management
test "attributes memory management" {
  let initial_memory = MemoryProfiler::get_allocated_memory()
  
  // Create attributes with various data types
  let attributes_collections = []
  
  for i in 0..=1000 {
    let attrs = Attributes::new()
    
    // Add attributes with different types
    Attributes::set(attrs, "string_attr", StringValue("string_value_" + i.to_string()))
    Attributes::set(attrs, "int_attr", IntValue(i))
    Attributes::set(attrs, "float_attr", FloatValue(i.to_float()))
    Attributes::set(attrs, "bool_attr", BoolValue(i % 2 == 0))
    
    // Add array attributes
    let string_array = ArrayStringValue(["a", "b", "c", "d", "e"])
    let int_array = ArrayIntValue([1, 2, 3, 4, 5])
    
    Attributes::set(attrs, "string_array", string_array)
    Attributes::set(attrs, "int_array", int_array)
    
    attributes_collections.push(attrs)
  }
  
  let after_attrs_memory = MemoryProfiler::get_allocated_memory()
  let attrs_memory_increase = after_attrs_memory - initial_memory
  
  // Verify memory usage is reasonable
  assert_true(attrs_memory_increase > 0)
  assert_true(attrs_memory_increase < 3 * 1024 * 1024) // Should be less than 3MB
  
  // Clear references
  attributes_collections = []
  
  // Force garbage collection
  MemoryProfiler::force_gc()
  
  let after_attrs_cleanup = MemoryProfiler::get_allocated_memory()
  let attrs_memory_reclaimed = after_attrs_memory - after_attrs_cleanup
  
  // Verify attributes memory was reclaimed
  assert_true(attrs_memory_reclaimed > attrs_memory_increase * 0.7) // At least 70% reclaimed
}

// Test 4: Context and Baggage Memory Management
test "context and baggage memory management" {
  let initial_memory = MemoryProfiler::get_allocated_memory()
  
  // Create contexts and baggage
  let contexts = []
  let baggage_collections = []
  
  for i in 0..=2000 {
    let context = SpanContext::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      i % 2 == 0,
      "state_" + i.to_string()
    )
    
    let baggage = Baggage::new()
    let baggage_with_data = Baggage::set_entry(baggage, "entry1", "value1_" + i.to_string())
    let baggage_with_data = Baggage::set_entry(baggage_with_data, "entry2", "value2_" + i.to_string())
    let baggage_with_data = Baggage::set_entry(baggage_with_data, "entry3", "value3_" + i.to_string())
    
    contexts.push(context)
    baggage_collections.push(baggage_with_data)
  }
  
  let after_context_memory = MemoryProfiler::get_allocated_memory()
  let context_memory_increase = after_context_memory - initial_memory
  
  // Verify memory usage is reasonable
  assert_true(context_memory_increase > 0)
  assert_true(context_memory_increase < 2 * 1024 * 1024) // Should be less than 2MB
  
  // Clear references
  contexts = []
  baggage_collections = []
  
  // Force garbage collection
  MemoryProfiler::force_gc()
  
  let after_context_cleanup = MemoryProfiler::get_allocated_memory()
  let context_memory_reclaimed = after_context_memory - after_context_cleanup
  
  // Verify context memory was reclaimed
  assert_true(context_memory_reclaimed > context_memory_increase * 0.8) // At least 80% reclaimed
}

// Test 5: Resource Cleanup on Exception
test "resource cleanup on exception" {
  let initial_memory = MemoryProfiler::get_allocated_memory()
  let resource_tracker = ResourceTracker::new()
  
  // Simulate operations that might throw exceptions
  let operations = []
  
  for i in 0..=500 {
    let operation = ResourceTrackedOperation::new(resource_tracker)
    
    // Register resources that need cleanup
    let span = Span::new("exception_test_span", Internal, 
                          SpanContext::new("trace", "span", true, ""))
    let attrs = Attributes::new()
    Attributes::set(attrs, "operation_id", IntValue(i))
    
    ResourceTrackedOperation::register_resource(operation, span)
    ResourceTrackedOperation::register_resource(operation, attrs)
    
    // Simulate exception in some operations
    if i % 7 == 0 {
      // Operation that throws exception
      ResourceTrackedOperation::execute_with_exception(operation, "simulated exception")
    } else {
      // Normal operation
      ResourceTrackedOperation::execute_successfully(operation)
    }
    
    operations.push(operation)
  }
  
  let after_operations_memory = MemoryProfiler::get_allocated_memory()
  let operations_memory_increase = after_operations_memory - initial_memory
  
  // Verify memory usage is reasonable
  assert_true(operations_memory_increase > 0)
  
  // Clear references
  operations = []
  
  // Force garbage collection
  MemoryProfiler::force_gc()
  
  let after_operations_cleanup = MemoryProfiler::get_allocated_memory()
  let operations_memory_reclaimed = after_operations_memory - after_operations_cleanup
  
  // Verify most memory was reclaimed even with exceptions
  assert_true(operations_memory_reclaimed > operations_memory_increase * 0.6) // At least 60% reclaimed
  
  // Verify resource tracker
  let leaked_resources = ResourceTracker::get_leaked_resources(resource_tracker)
  assert_true(leaked_resources.length() < 50) // Should have minimal resource leaks
}

// Test 6: Circular Reference Detection and Cleanup
test "circular reference detection and cleanup" {
  let initial_memory = MemoryProfiler::get_allocated_memory()
  
  // Create objects with circular references
  let circular_objects = []
  
  for i in 0..=100 {
    let obj1 = CircularReferenceObject::new("obj1_" + i.to_string())
    let obj2 = CircularReferenceObject::new("obj2_" + i.to_string())
    
    // Create circular references
    CircularReferenceObject::set_reference(obj1, obj2)
    CircularReferenceObject::set_reference(obj2, obj1)
    
    // Add telemetry data to increase complexity
    let span = Span::new("circular_test_span", Internal, 
                          SpanContext::new("trace_" + i.to_string(), "span_" + i.to_string(), true, ""))
    CircularReferenceObject::add_telemetry_data(obj1, span)
    
    circular_objects.push((obj1, obj2))
  }
  
  let after_circular_memory = MemoryProfiler::get_allocated_memory()
  let circular_memory_increase = after_circular_memory - initial_memory
  
  // Verify memory usage is reasonable
  assert_true(circular_memory_increase > 0)
  
  // Clear references
  circular_objects = []
  
  // Force garbage collection with circular reference detection
  MemoryProfiler::force_gc_with_circular_reference_detection()
  
  let after_circular_cleanup = MemoryProfiler::get_allocated_memory()
  let circular_memory_reclaimed = after_circular_memory - after_circular_cleanup
  
  // Verify circular references were properly cleaned up
  assert_true(circular_memory_reclaimed > circular_memory_increase * 0.5) // At least 50% reclaimed
}

// Test 7: Large Object Cleanup
test "large object cleanup" {
  let initial_memory = MemoryProfiler::get_allocated_memory()
  
  // Create large objects
  let large_objects = []
  
  for i in 0..=50 {
    let large_attrs = Attributes::new()
    
    // Add many attributes to create a large object
    for j in 0..=1000 {
      let key = "attr_" + j.to_string()
      let value = StringValue("value_" + i.to_string() + "_" + j.to_string())
      Attributes::set(large_attrs, key, value)
    }
    
    let large_span = Span::new("large_span_" + i.to_string(), Internal, 
                               SpanContext::new("trace_" + i.to_string(), "span_" + i.to_string(), true, ""))
    
    // Add many events
    for j in 0..=100 {
      let event_attrs = Attributes::new()
      Attributes::set(event_attrs, "event_id", IntValue(j))
      Attributes::set(event_attrs, "data", StringValue("event_data_" + j.to_string()))
      
      Span::add_event(large_span, "event_" + j.to_string(), Some(event_attrs))
    }
    
    large_objects.push((large_attrs, large_span))
  }
  
  let after_large_memory = MemoryProfiler::get_allocated_memory()
  let large_memory_increase = after_large_memory - initial_memory
  
  // Verify memory usage is reasonable but significant
  assert_true(large_memory_increase > 10 * 1024 * 1024) // Should be more than 10MB
  assert_true(large_memory_increase < 100 * 1024 * 1024) // Should be less than 100MB
  
  // Clear references
  large_objects = []
  
  // Force garbage collection
  MemoryProfiler::force_gc()
  
  let after_large_cleanup = MemoryProfiler::get_allocated_memory()
  let large_memory_reclaimed = after_large_memory - after_large_cleanup
  
  // Verify large objects were properly cleaned up
  assert_true(large_memory_reclaimed > large_memory_increase * 0.8) // At least 80% reclaimed
}

// Test 8: Memory Leak Detection
test "memory leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  MemoryLeakDetector::start_monitoring(leak_detector)
  
  // Perform operations that might leak memory
  let leaky_operations = []
  
  for i in 0..=1000 {
    let operation = PotentiallyLeakyOperation::new()
    
    // Create spans but forget to end some of them
    let span = PotentiallyLeakyOperation::create_span(operation, "leaky_span_" + i.to_string())
    
    // Add attributes
    Span::set_attribute(span, "operation_id", IntValue(i))
    
    // Only end 80% of spans
    if i % 5 != 0 {
      Span::end(span)
    }
    
    leaky_operations.push(operation)
  }
  
  // Check for memory leaks
  let leak_report = MemoryLeakDetector::generate_report(leak_detector)
  
  // Verify leak detection
  assert_true(leak_report.total_objects > 0)
  assert_true(leak_report.leaked_objects > 0)
  assert_true(leak_report.leak_percentage > 0.0)
  
  // Clean up remaining spans
  for operation in leaky_operations {
    PotentiallyLeakyOperation::cleanup(operation)
  }
  
  // Clear references
  leaky_operations = []
  
  // Force garbage collection
  MemoryProfiler::force_gc()
  
  // Check for remaining leaks
  let final_leak_report = MemoryLeakDetector::generate_report(leak_detector)
  
  // Verify most leaks were resolved
  assert_true(final_leak_report.leaked_objects < leak_report.leaked_objects)
  
  MemoryLeakDetector::stop_monitoring(leak_detector)
}

// Test 9: Resource Pool Management
test "resource pool management" {
  let initial_memory = MemoryProfiler::get_allocated_memory()
  
  // Create resource pools
  let span_pool = ResourcePool::new(100, fn() {
    Span::new("pooled_span", Internal, SpanContext::new("trace", "span", true, ""))
  })
  
  let attrs_pool = ResourcePool::new(100, fn() {
    Attributes::new()
  })
  
  // Acquire and release resources
  let acquired_resources = []
  
  for i in 0..=500 {
    // Acquire from pools
    let span = ResourcePool::acquire(span_pool)
    let attrs = ResourcePool::acquire(attrs_pool)
    
    // Use resources
    Span::set_attribute(span, "pool_iteration", IntValue(i))
    Attributes::set(attrs, "pool_iteration", IntValue(i))
    
    // Return some resources to pool
    if i % 2 == 0 {
      ResourcePool::release(span_pool, span)
      ResourcePool::release(attrs_pool, attrs)
    } else {
      acquired_resources.push((span, attrs))
    }
  }
  
  let after_pool_memory = MemoryProfiler::get_allocated_memory()
  let pool_memory_increase = after_pool_memory - initial_memory
  
  // Verify memory usage is reasonable with pooling
  assert_true(pool_memory_increase > 0)
  assert_true(pool_memory_increase < 2 * 1024 * 1024) // Should be less than 2MB due to pooling
  
  // Return remaining resources to pool
  for (span, attrs) in acquired_resources {
    ResourcePool::release(span_pool, span)
    ResourcePool::release(attrs_pool, attrs)
  }
  
  // Clear references
  acquired_resources = []
  
  // Force garbage collection
  MemoryProfiler::force_gc()
  
  let after_pool_cleanup = MemoryProfiler::get_allocated_memory()
  let pool_memory_reclaimed = after_pool_memory - after_pool_cleanup
  
  // Verify pool cleanup
  assert_true(pool_memory_reclaimed > pool_memory_increase * 0.7) // At least 70% reclaimed
  
  // Verify pool statistics
  let span_stats = ResourcePool::get_statistics(span_pool)
  let attrs_stats = ResourcePool::get_statistics(attrs_pool)
  
  assert_true(span_stats.acquired_count > 0)
  assert_true(span_stats.released_count > 0)
  assert_true(attrs_stats.acquired_count > 0)
  assert_true(attrs_stats.released_count > 0)
}

// Test 10: Memory Pressure and Adaptive Cleanup
test "memory pressure and adaptive cleanup" {
  let initial_memory = MemoryProfiler::get_allocated_memory()
  let pressure_monitor = MemoryPressureMonitor::new()
  
  // Configure adaptive cleanup
  MemoryPressureMonitor::set_threshold(pressure_monitor, 50 * 1024 * 1024) // 50MB threshold
  MemoryPressureMonitor::enable_adaptive_cleanup(pressure_monitor)
  
  // Create objects to trigger memory pressure
  let pressure_objects = []
  
  for i in 0..=2000 {
    let attrs = Attributes::new()
    
    // Add many attributes to increase memory usage
    for j in 0..=100 {
      let key = "pressure_attr_" + j.to_string()
      let value = StringValue("pressure_value_" + i.to_string() + "_" + j.to_string())
      Attributes::set(attrs, key, value)
    }
    
    let span = Span::new("pressure_span_" + i.to_string(), Internal, 
                          SpanContext::new("trace_" + i.to_string(), "span_" + i.to_string(), true, ""))
    
    pressure_objects.push((attrs, span))
    
    // Check memory pressure and trigger adaptive cleanup if needed
    if i % 100 == 0 {
      let pressure_level = MemoryPressureMonitor::check_pressure(pressure_monitor)
      
      if pressure_level > 0.8 {
        // High memory pressure, trigger adaptive cleanup
        MemoryPressureMonitor::adaptive_cleanup(pressure_monitor)
        
        // Clear some objects to reduce pressure
        for j in 0..=50 {
          if pressure_objects.length() > 0 {
            pressure_objects.pop()
          }
        }
      }
    }
  }
  
  let after_pressure_memory = MemoryProfiler::get_allocated_memory()
  let pressure_memory_increase = after_pressure_memory - initial_memory
  
  // Verify memory usage is controlled with adaptive cleanup
  assert_true(pressure_memory_increase > 0)
  assert_true(pressure_memory_increase < 20 * 1024 * 1024) // Should be less than 20MB
  
  // Get pressure statistics
  let pressure_stats = MemoryPressureMonitor::get_statistics(pressure_monitor)
  
  assert_true(pressure_stats.max_pressure_level > 0.0)
  assert_true(pressure_stats.adaptive_cleanup_count > 0)
  
  // Clear remaining objects
  pressure_objects = []
  
  // Force final cleanup
  MemoryPressureMonitor::final_cleanup(pressure_monitor)
  MemoryProfiler::force_gc()
  
  let after_pressure_cleanup = MemoryProfiler::get_allocated_memory()
  let pressure_memory_reclaimed = after_pressure_memory - after_pressure_cleanup
  
  // Verify final cleanup
  assert_true(pressure_memory_reclaimed > pressure_memory_increase * 0.7) // At least 70% reclaimed
}