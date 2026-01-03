// Azimuth Telemetry System - Memory Management Tests
// This file contains comprehensive test cases for memory management

// Test 1: Memory Pool Allocation and Deallocation
test "memory pool allocation and deallocation" {
  let memory_manager = MemoryManager::new()
  let memory_pool = MemoryPool::new(1024 * 1024) // 1MB pool
  
  // Test initial state
  assert_eq(MemoryPool::allocated_size(memory_pool), 0)
  assert_eq(MemoryPool::available_size(memory_pool), 1024 * 1024)
  
  // Allocate memory blocks
  let allocations = []
  let allocation_sizes = [1024, 2048, 4096, 8192, 16384] // Various sizes
  
  for size in allocation_sizes {
    let block = MemoryPool::allocate(memory_pool, size)
    assert_true(block !== None)
    allocations.push(block)
  }
  
  // Verify allocations
  let total_allocated = Array::reduce(allocation_sizes, 0, fn(acc, size) { acc + size })
  assert_eq(MemoryPool::allocated_size(memory_pool), total_allocated)
  assert_eq(MemoryPool::available_size(memory_pool), 1024 * 1024 - total_allocated)
  
  // Test memory block access
  for i in 0..=allocations.length() - 1 {
    let block = allocations[i]
    let size = allocation_sizes[i]
    
    match block {
      Some(memory_block) => {
        // Write test pattern
        MemoryBlock::write_pattern(memory_block, 0xAA, size)
        
        // Verify pattern
        assert_true(MemoryBlock::verify_pattern(memory_block, 0xAA, size))
        
        // Write specific data
        let test_data = "Test data for block " + Int::to_string(i)
        MemoryBlock::write_string(memory_block, test_data)
        
        // Read and verify data
        let read_data = MemoryBlock::read_string(memory_block)
        assert_eq(read_data, test_data)
      }
      None => assert_true(false)
    }
  }
  
  // Deallocate memory blocks
  for block in allocations {
    match block {
      Some(memory_block) => {
        MemoryPool::deallocate(memory_pool, memory_block)
      }
      None => assert_true(false)
    }
  }
  
  // Verify deallocation
  assert_eq(MemoryPool::allocated_size(memory_pool), 0)
  assert_eq(MemoryPool::available_size(memory_pool), 1024 * 1024)
  
  // Test pool expansion
  let large_block = MemoryPool::allocate(memory_pool, 2 * 1024 * 1024) // 2MB (larger than pool)
  match large_block {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected to fail
  }
  
  // Test pool expansion with auto-grow
  MemoryPool::set_auto_grow(memory_pool, true)
  MemoryPool::set_max_size(memory_pool, 5 * 1024 * 1024) // 5MB max
  
  let expanded_block = MemoryPool::allocate(memory_pool, 2 * 1024 * 1024) // 2MB
  match expanded_block {
    Some(memory_block) => {
      // Verify pool expanded
      assert_true(MemoryPool::total_size(memory_pool) > 1024 * 1024)
      assert_eq(MemoryPool::allocated_size(memory_pool), 2 * 1024 * 1024)
      
      // Clean up
      MemoryPool::deallocate(memory_pool, memory_block)
    }
    None => assert_true(false)
  }
}

// Test 2: Garbage Collection Optimization
test "garbage collection optimization" {
  let memory_manager = MemoryManager::new()
  let gc_optimizer = GCOptimizer::new()
  
  // Test object lifecycle tracking
  let tracked_objects = []
  
  for i in 0..=1000 {
    let obj = TrackedObject::new("test_object_" + Int::to_string(i), 1024) // 1KB objects
    tracked_objects.push(obj)
  }
  
  // Verify objects are tracked
  assert_eq(GCOptimizer::tracked_object_count(gc_optimizer), 1000)
  assert_eq(GCOptimizer::tracked_memory_size(gc_optimizer), 1000 * 1024)
  
  // Release references to some objects (simulate going out of scope)
  let released_objects = Array::slice(tracked_objects, 0, 500) // Release first 500 objects
  tracked_objects = Array::slice(tracked_objects, 500, 1000) // Keep last 500 objects
  
  // Trigger garbage collection
  GCOptimizer::collect_garbage(gc_optimizer)
  
  // Verify garbage collection results
  assert_eq(GCOptimizer::tracked_object_count(gc_optimizer), 500)
  assert_eq(GCOptimizer::tracked_memory_size(gc_optimizer), 500 * 1024)
  
  // Test generational garbage collection
  GCOptimizer::enable_generational_gc(gc_optimizer)
  
  // Create objects in different generations
  let young_objects = []
  let old_objects = []
  
  // Create young generation objects
  for i in 0..=100 {
    let obj = TrackedObject::new("young_object_" + Int::to_string(i), 512)
    young_objects.push(obj)
  }
  
  // Age some objects to old generation
  for i in 0..=50 {
    let obj = young_objects[i]
    GCOptimizer::promote_to_old_generation(gc_optimizer, obj)
    old_objects.push(obj)
  }
  
  // Verify generation counts
  assert_eq(GCOptimizer::young_generation_count(gc_optimizer), 50)
  assert_eq(GCOptimizer::old_generation_count(gc_optimizer), 50)
  
  // Trigger young generation GC
  GCOptimizer::collect_young_generation(gc_optimizer)
  
  // Young generation should be empty, old generation unchanged
  assert_eq(GCOptimizer::young_generation_count(gc_optimizer), 0)
  assert_eq(GCOptimizer::old_generation_count(gc_optimizer), 50)
  
  // Release all references
  young_objects = []
  old_objects = []
  
  // Trigger full GC
  GCOptimizer::collect_garbage(gc_optimizer)
  
  // Verify all objects collected
  assert_eq(GCOptimizer::tracked_object_count(gc_optimizer), 0)
  assert_eq(GCOptimizer::tracked_memory_size(gc_optimizer), 0)
}

// Test 3: Memory Leak Detection
test "memory leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  
  // Enable leak detection
  LeakDetector::enable(leak_detector)
  
  // Test normal allocation and deallocation (no leaks)
  let normal_allocations = []
  
  for i in 0..=100 {
    let allocation = MemoryManager::allocate(1024) // 1KB
    normal_allocations.push(allocation)
  }
  
  // Deallocate all normal allocations
  for allocation in normal_allocations {
    MemoryManager::deallocate(allocation)
  }
  
  // Check for leaks
  let leak_report_1 = LeakDetector::generate_report(leak_detector)
  assert_eq(LeakReport::leaked_blocks(leak_report_1), 0)
  assert_eq(LeakReport::leaked_bytes(leak_report_1), 0)
  
  // Test memory leak scenario (intentional leaks)
  let leaked_allocations = []
  
  for i in 0..=100 {
    let allocation = MemoryManager::allocate(2048) // 2KB
    leaked_allocations.push(allocation)
  }
  
  // Don't deallocate - simulate leak
  
  // Check for leaks
  let leak_report_2 = LeakDetector::generate_report(leak_detector)
  assert_eq(LeakReport::leaked_blocks(leak_report_2), 100)
  assert_eq(LeakReport::leaked_bytes(leak_report_2), 100 * 2048)
  
  // Test leak detection with stack traces
  LeakDetector::enable_stack_trace_capture(leak_detector)
  
  let traced_allocations = []
  
  for i in 0..=10 {
    let allocation = MemoryManager::allocate_with_trace(4096) // 4KB with trace
    traced_allocations.push(allocation)
  }
  
  // Don't deallocate - simulate leak with trace information
  
  // Check for leaks with traces
  let leak_report_3 = LeakDetector::generate_report(leak_detector)
  let leak_traces = LeakReport::leak_traces(leak_report_3)
  assert_true(leak_traces.length() >= 10)
  
  // Verify trace information
  for trace in leak_traces {
    assert_true(LeakTrace::allocation_size(trace) == 4096)
    assert_true(String::length(LeakTrace::stack_trace(trace)) > 0)
  }
  
  // Test automatic leak cleanup
  LeakDetector::enable_auto_cleanup(leak_detector, 1000) // 1 second timeout
  
  // Wait for cleanup (simulated)
  LeakDetector::simulate_time_passage(leak_detector, 1001)
  
  // Verify cleanup
  let leak_report_4 = LeakDetector::generate_report(leak_detector)
  assert_eq(LeakReport::leaked_blocks(leak_report_4), 0)
  
  // Disable leak detection
  LeakDetector::disable(leak_detector)
}

// Test 4: Memory Fragmentation Management
test "memory fragmentation management" {
  let memory_manager = MemoryManager::new()
  let fragmentation_manager = FragmentationManager::new()
  
  // Test memory allocation pattern that causes fragmentation
  let allocations = []
  
  // Allocate many small blocks
  for i in 0..=1000 {
    let allocation = MemoryManager::allocate(64) // 64 bytes
    allocations.push(allocation)
  }
  
  // Deallocate every other block to create fragmentation
  for i in 0..=allocations.length() - 1 {
    if i % 2 == 0 {
      MemoryManager::deallocate(allocations[i])
    }
  }
  
  // Measure fragmentation
  let fragmentation_info = FragmentationManager::analyze_fragmentation(fragmentation_manager)
  let fragmentation_ratio = FragmentationInfo::fragmentation_ratio(fragmentation_info)
  
  // Should have some fragmentation
  assert_true(fragmentation_ratio > 0.0)
  
  // Test defragmentation
  FragmentationManager::defragment(fragmentation_manager)
  
  // Measure fragmentation after defragmentation
  let defragmentation_info = FragmentationManager::analyze_fragmentation(fragmentation_manager)
  let defragmentation_ratio = FragmentationInfo::fragmentation_ratio(defragmentation_info)
  
  // Fragmentation should be reduced
  assert_true(defragmentation_ratio < fragmentation_ratio)
  
  // Test allocation after defragmentation
  let large_allocation = MemoryManager::allocate(4096) // 4KB
  assert_true(large_allocation !== None) // Should succeed after defragmentation
  
  // Clean up
  for allocation in allocations {
    if allocation !== None {
      MemoryManager::deallocate(allocation)
    }
  }
  
  if large_allocation !== None {
    MemoryManager::deallocate(large_allocation)
  }
}

// Test 5: Memory Usage Monitoring and Alerting
test "memory usage monitoring and alerting" {
  let memory_manager = MemoryManager::new()
  let memory_monitor = MemoryMonitor::new()
  
  // Configure monitoring thresholds
  MemoryMonitor::set_warning_threshold(memory_monitor, 50 * 1024 * 1024) // 50MB
  MemoryMonitor::set_critical_threshold(memory_monitor, 100 * 1024 * 1024) // 100MB
  
  // Start monitoring
  MemoryMonitor::start(memory_monitor)
  
  // Test normal memory usage
  let normal_allocations = []
  
  for i in 0..=100 {
    let allocation = MemoryManager::allocate(1024) // 1KB
    normal_allocations.push(allocation)
  }
  
  let memory_stats_1 = MemoryMonitor::get_stats(memory_monitor)
  assert_eq(MemoryStats::current_usage(memory_stats_1), 100 * 1024) // 100KB
  assert_eq(MemoryStats::allocation_count(memory_stats_1), 100)
  assert_eq(MemoryStats::alert_level(memory_stats_1), AlertLevel::Normal)
  
  // Test warning threshold
  let warning_allocations = []
  
  for i in 0..=10000 {
    let allocation = MemoryManager::allocate(4096) // 4KB
    warning_allocations.push(allocation)
  }
  
  let memory_stats_2 = MemoryMonitor::get_stats(memory_monitor)
  let total_usage = MemoryStats::current_usage(memory_stats_2)
  
  if total_usage > 50 * 1024 * 1024 {
    assert_eq(MemoryStats::alert_level(memory_stats_2), AlertLevel::Warning)
  }
  
  // Test critical threshold
  let critical_allocations = []
  
  for i in 0..=15000 {
    let allocation = MemoryManager::allocate(4096) // 4KB
    critical_allocations.push(allocation)
  }
  
  let memory_stats_3 = MemoryMonitor::get_stats(memory_monitor)
  let total_usage_2 = MemoryStats::current_usage(memory_stats_3)
  
  if total_usage_2 > 100 * 1024 * 1024 {
    assert_eq(MemoryStats::alert_level(memory_stats_3), AlertLevel::Critical)
  }
  
  // Test memory usage trends
  let memory_trends = MemoryMonitor::get_trends(memory_monitor, 60) // Last 60 seconds
  assert_true(memory_trends.length() > 0)
  
  let trend_direction = MemoryMonitor::get_trend_direction(memory_monitor)
  assert_true(trend_direction == TrendDirection::Increasing || 
             trend_direction == TrendDirection::Stable)
  
  // Test memory pressure response
  if MemoryStats::alert_level(memory_stats_3) == AlertLevel::Critical {
    // Simulate memory pressure response
    MemoryMonitor::trigger_pressure_response(memory_monitor)
    
    // Verify response was triggered
    assert_true(MemoryMonitor::pressure_response_triggered(memory_monitor))
  }
  
  // Clean up
  for allocation in normal_allocations {
    MemoryManager::deallocate(allocation)
  }
  
  for allocation in warning_allocations {
    MemoryManager::deallocate(allocation)
  }
  
  for allocation in critical_allocations {
    MemoryManager::deallocate(allocation)
  }
  
  // Stop monitoring
  MemoryMonitor::stop(memory_monitor)
}

// Test 6: Telemetry Memory Optimization
test "telemetry memory optimization" {
  let memory_manager = MemoryManager::new()
  let telemetry_optimizer = TelemetryMemoryOptimizer::new()
  
  // Test span memory optimization
  let spans = []
  
  // Create spans with memory-intensive attributes
  for i in 0..=100 {
    let trace_id = "trace_" + Int::to_string(i)
    let span_id = "span_" + Int::to_string(i)
    let span_ctx = SpanContext::new(trace_id, span_id, true, "memory_test_service")
    let span = Span::new("memory_test_operation", Server, span_ctx)
    
    // Add many attributes
    for j in 0..=50 {
      let key = "attr_" + Int::to_string(j)
      let value = "value_" + Int::to_string(j)
      Span::set_attribute(span, key, StringValue(value))
    }
    
    // Add many events
    for j in 0..=20 {
      let event_name = "event_" + Int::to_string(j)
      Span::add_event(span, event_name, None)
    }
    
    spans.push(span)
  }
  
  // Measure memory usage before optimization
  let memory_before = MemoryManager::get_current_usage()
  
  // Apply telemetry memory optimization
  TelemetryMemoryOptimizer::optimize_spans(telemetry_optimizer, spans)
  
  // Measure memory usage after optimization
  let memory_after = MemoryManager::get_current_usage()
  
  // Verify memory was reduced
  assert_true(memory_after < memory_before)
  
  // Verify optimization effectiveness
  let memory_reduction = memory_before - memory_after
  let reduction_ratio = memory_reduction / memory_before
  assert_true(reduction_ratio > 0.1) // At least 10% reduction
  
  // Verify spans are still functional
  for span in spans {
    assert_true(Span::name(span) == "memory_test_operation")
    assert_true(Span::is_recording(span))
    
    // Verify some attributes are still accessible
    let test_attr = Span::get_attribute(span, "attr_0")
    match test_attr {
      Some(StringValue(value)) => assert_eq(value, "value_0")
      None => assert_true(false)
    }
  }
  
  // Test metrics memory optimization
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "memory_optimization_meter")
  
  let metrics = []
  
  // Create many metrics
  for i in 0..=100 {
    let counter_name = "counter_" + Int::to_string(i)
    let counter = Meter::create_counter(meter, counter_name, Some("Test counter"), Some("count"))
    
    // Record many measurements
    for j in 0..=1000 {
      Counter::add(counter, 1.0)
    }
    
    metrics.push(counter)
  }
  
  // Measure metrics memory usage before optimization
  let metrics_memory_before = MemoryManager::get_current_usage()
  
  // Apply metrics memory optimization
  TelemetryMemoryOptimizer::optimize_metrics(telemetry_optimizer, metrics)
  
  // Measure metrics memory usage after optimization
  let metrics_memory_after = MemoryManager::get_current_usage()
  
  // Verify metrics memory was reduced
  assert_true(metrics_memory_after < metrics_memory_before)
  
  // Verify metrics are still functional
  for counter in metrics {
    let value = Counter::get_value(counter)
    match value {
      Some(v) => assert_eq(v, 1001.0) // 1000 + 1 initial value
      None => assert_true(false)
    }
  }
  
  // Clean up spans
  for span in spans {
    Span::end(span)
  }
}