// Azimuth Telemetry System - Memory Management Optimization Tests
// This file contains test cases for memory management and optimization functionality

// Test 1: Memory Pool Allocation and Deallocation
test "memory pool allocation and deallocation" {
  // Create a memory pool
  let pool = MemoryPool::new(1024)  // 1KB pool
  
  // Verify initial state
  assert_eq(MemoryPool::total_size(pool), 1024)
  assert_eq(MemoryPool::used_size(pool), 0)
  assert_eq(MemoryPool::available_size(pool), 1024)
  
  // Allocate memory from pool
  let block1 = MemoryPool::allocate(pool, 256)
  assert_true(MemoryPool::is_valid_block(block1))
  assert_eq(MemoryPool::block_size(block1), 256)
  assert_eq(MemoryPool::used_size(pool), 256)
  assert_eq(MemoryPool::available_size(pool), 768)
  
  // Allocate another block
  let block2 = MemoryPool::allocate(pool, 512)
  assert_true(MemoryPool::is_valid_block(block2))
  assert_eq(MemoryPool::block_size(block2), 512)
  assert_eq(MemoryPool::used_size(pool), 768)
  assert_eq(MemoryPool::available_size(pool), 256)
  
  // Deallocate first block
  MemoryPool::deallocate(pool, block1)
  assert_eq(MemoryPool::used_size(pool), 512)
  assert_eq(MemoryPool::available_size(pool), 512)
  
  // Deallocate second block
  MemoryPool::deallocate(pool, block2)
  assert_eq(MemoryPool::used_size(pool), 0)
  assert_eq(MemoryPool::available_size(pool), 1024)
  
  // Attempt to allocate more than available
  let oversized_block = MemoryPool::allocate(pool, 2048)
  assert_false(MemoryPool::is_valid_block(oversized_block))
}

// Test 2: Object Pool Reuse
test "object pool reuse" {
  // Create an object pool for telemetry spans
  let pool = ObjectPool::new(|| Span::new("test", Internal, SpanContext::new("", "", false, "")), 10)
  
  // Verify initial state
  assert_eq(ObjectPool::available_count(pool), 10)
  assert_eq(ObjectPool::in_use_count(pool), 0)
  
  // Acquire objects from pool
  let span1 = ObjectPool::acquire(pool)
  let span2 = ObjectPool::acquire(pool)
  let span3 = ObjectPool::acquire(pool)
  
  // Verify state after acquisition
  assert_eq(ObjectPool::available_count(pool), 7)
  assert_eq(ObjectPool::in_use_count(pool), 3)
  
  // Configure spans
  Span::set_name(span1, "operation-1")
  Span::set_name(span2, "operation-2")
  Span::set_name(span3, "operation-3")
  
  assert_eq(Span::name(span1), "operation-1")
  assert_eq(Span::name(span2), "operation-2")
  assert_eq(Span::name(span3), "operation-3")
  
  // Release objects back to pool
  ObjectPool::release(pool, span1)
  ObjectPool::release(pool, span2)
  ObjectPool::release(pool, span3)
  
  // Verify state after release
  assert_eq(ObjectPool::available_count(pool), 10)
  assert_eq(ObjectPool::in_use_count(pool), 0)
  
  // Acquire objects again (should be reused)
  let reused_span1 = ObjectPool::acquire(pool)
  let reused_span2 = ObjectPool::acquire(pool)
  
  // Verify objects are reset to initial state
  assert_eq(Span::name(reused_span1), "test")  // Should be reset
  assert_eq(Span::name(reused_span2), "test")  // Should be reset
}

// Test 3: Memory Leak Detection
test "memory leak detection" {
  // Create a memory leak detector
  let detector = MemoryLeakDetector::new()
  
  // Start monitoring
  MemoryLeakDetector::start_monitoring(detector)
  
  // Allocate some memory
  let allocations = []
  for i in 0..100 {
    let allocation = MemoryLeakDetector::tracked_allocate(detector, 1024)  // 1KB each
    allocations.push(allocation)
  }
  
  // Verify allocations are tracked
  assert_eq(MemoryLeakDetector::get_allocation_count(detector), 100)
  assert_eq(MemoryLeakDetector::get_total_allocated_memory(detector), 102400)  // 100KB
  
  // Deallocate half of the allocations
  for i in 0..50 {
    MemoryLeakDetector::tracked_deallocate(detector, allocations[i])
  }
  
  // Verify deallocations are tracked
  assert_eq(MemoryLeakDetector::get_allocation_count(detector), 50)
  assert_eq(MemoryLeakDetector::get_total_allocated_memory(detector), 51200)  // 50KB
  
  // Check for leaks
  let leak_report = MemoryLeakDetector::generate_leak_report(detector)
  assert_eq(MemoryLeakDetector::get_leaked_allocation_count(leak_report), 50)
  assert_eq(MemoryLeakDetector::get_total_leaked_memory(leak_report), 51200)  // 50KB
  
  // Clean up remaining allocations
  for i in 50..100 {
    MemoryLeakDetector::tracked_deallocate(detector, allocations[i])
  }
  
  // Verify no leaks after cleanup
  let final_report = MemoryLeakDetector::generate_leak_report(detector)
  assert_eq(MemoryLeakDetector::get_leaked_allocation_count(final_report), 0)
  assert_eq(MemoryLeakDetector::get_total_leaked_memory(final_report), 0)
  
  // Stop monitoring
  MemoryLeakDetector::stop_monitoring(detector)
}

// Test 4: Garbage Collection Optimization
test "garbage collection optimization" {
  // Create a garbage collection optimizer
  let optimizer = GCOptimizer::new()
  
  // Configure GC settings
  GCOptimizer::set_threshold(optimizer, 1024 * 1024)  // 1MB threshold
  GCOptimizer::set_strategy(optimizer, GCStrategy::Generational)
  
  // Create objects that will be garbage collected
  let objects = []
  for i in 0..1000 {
    let obj = GCObject::new(1024)  // 1KB objects
    objects.push(obj)
  }
  
  // Verify memory usage before GC
  let memory_before = GCOptimizer::get_memory_usage(optimizer)
  assert_true(memory_before > 1024 * 1000)  // At least 1MB
  
  // Clear references to allow GC
  objects = []
  
  // Trigger garbage collection
  let gc_result = GCOptimizer::collect_garbage(optimizer)
  
  // Verify GC was effective
  assert_true(GCOptimizer::is_successful(gc_result))
  assert_true(GCOptimizer::get_collected_memory(gc_result) > 0)
  
  // Verify memory usage after GC
  let memory_after = GCOptimizer::get_memory_usage(optimizer)
  assert_true(memory_after < memory_before)
  
  // Test generational GC
  GCOptimizer::set_strategy(optimizer, GCStrategy::Generational)
  
  // Create short-lived objects (young generation)
  let young_objects = []
  for i in 0..100 {
    let obj = GCObject::new(512)  // 512B objects
    young_objects.push(obj)
  }
  
  // Create long-lived objects (old generation)
  let old_objects = []
  for i in 0..10 {
    let obj = GCObject::new(2048)  // 2KB objects
    old_objects.push(obj)
  }
  
  // Clear young objects
  young_objects = []
  
  // Trigger young generation GC
  let young_gc_result = GCOptimizer::collect_young_generation(optimizer)
  assert_true(GCOptimizer::is_successful(young_gc_result))
  
  // Verify old objects are still alive
  assert_eq(old_objects.length(), 10)
}

// Test 5: Memory Fragmentation Analysis
test "memory fragmentation analysis" {
  // Create a fragmentation analyzer
  let analyzer = FragmentationAnalyzer::new()
  
  // Create a memory arena
  let arena = MemoryArena::new(1024 * 1024)  // 1MB arena
  
  // Allocate blocks of different sizes to create fragmentation
  let allocations = []
  
  // Allocate many small blocks
  for i in 0..100 {
    let block = MemoryArena::allocate(arena, 1024)  // 1KB blocks
    allocations.push(block)
  }
  
  // Free some blocks to create holes
  for i in range_step(0, allocations.length(), 2) {
    MemoryArena::deallocate(arena, allocations[i])
  }
  
  // Analyze fragmentation
  let fragmentation_report = FragmentationAnalyzer::analyze(analyzer, arena)
  
  // Verify fragmentation metrics
  assert_true(FragmentationAnalyzer::get_fragmentation_ratio(fragmentation_report) > 0.0)
  assert_true(FragmentationAnalyzer::get_fragmentation_ratio(fragmentation_report) < 1.0)
  
  let free_blocks = FragmentationAnalyzer::get_free_blocks(fragmentation_report)
  assert_true(free_blocks.length() > 0)
  
  let total_free_memory = FragmentationAnalyzer::get_total_free_memory(fragmentation_report)
  assert_true(total_free_memory > 0)
  
  // Test defragmentation
  let defragmentation_result = MemoryArena::defragment(arena)
  assert_true(MemoryArena::is_successful(defragmentation_result))
  
  // Analyze fragmentation after defragmentation
  let post_defrag_report = FragmentationAnalyzer::analyze(analyzer, arena)
  
  // Verify fragmentation is reduced
  assert_true(
    FragmentationAnalyzer::get_fragmentation_ratio(post_defrag_report) <=
    FragmentationAnalyzer::get_fragmentation_ratio(fragmentation_report)
  )
  
  // Clean up remaining allocations
  for i in range_step(1, allocations.length(), 2) {
    MemoryArena::deallocate(arena, allocations[i])
  }
}

// Test 6: Memory Usage Monitoring
test "memory usage monitoring" {
  // Create a memory monitor
  let monitor = MemoryMonitor::new()
  
  // Start monitoring
  MemoryMonitor::start(monitor)
  
  // Get initial memory usage
  let initial_usage = MemoryMonitor::get_current_usage(monitor)
  
  // Allocate memory
  let allocations = []
  for i in 0..100 {
    let allocation = MemoryMonitor::tracked_allocate(monitor, 10240)  // 10KB each
    allocations.push(allocation)
  }
  
  // Get memory usage after allocations
  let peak_usage = MemoryMonitor::get_peak_usage(monitor)
  let current_usage = MemoryMonitor::get_current_usage(monitor)
  
  // Verify memory usage increased
  assert_true(current_usage > initial_usage)
  assert_eq(peak_usage, current_usage)  // Should be the same since we just allocated
  
  // Deallocate memory
  for allocation in allocations {
    MemoryMonitor::tracked_deallocate(monitor, allocation)
  }
  
  // Get memory usage after deallocations
  let final_usage = MemoryMonitor::get_current_usage(monitor)
  
  // Verify memory usage decreased
  assert_true(final_usage < current_usage)
  
  // Test memory usage statistics
  let stats = MemoryMonitor::get_statistics(monitor)
  assert_eq(MemoryMonitor::get_total_allocations(stats), 100)
  assert_eq(MemoryMonitor::get_total_deallocations(stats), 100)
  assert_eq(MemoryMonitor::get_peak_memory_usage(stats), peak_usage)
  
  // Stop monitoring
  MemoryMonitor::stop(monitor)
}

// Test 7: Memory Pressure Handling
test "memory pressure handling" {
  // Create a memory pressure handler
  let handler = MemoryPressureHandler::new()
  
  // Configure pressure thresholds
  MemoryPressureHandler::set_warning_threshold(handler, 0.8)  // 80%
  MemoryPressureHandler::set_critical_threshold(handler, 0.9)  // 90%
  
  // Simulate memory pressure
  let pressure_events = []
  
  // Register pressure event callback
  MemoryPressureHandler::on_pressure_event(handler, |level| {
    pressure_events.push(level)
  })
  
  // Create a memory pool to simulate pressure
  let pool = MemoryPool::new(1024 * 1024)  // 1MB pool
  
  // Allocate memory to trigger warning level (80%)
  let warning_allocations = []
  for i in 0..8 {
    let block = MemoryPool::allocate(pool, 1024 * 100)  // 100KB blocks
    warning_allocations.push(block)
  }
  
  // Check memory pressure level
  let pressure_level = MemoryPressureHandler::check_pressure(handler, pool)
  assert_eq(pressure_level, MemoryPressureLevel::Warning)
  
  // Allocate more memory to trigger critical level (90%)
  for i in 0..1 {
    let block = MemoryPool::allocate(pool, 1024 * 100)  // 100KB blocks
    warning_allocations.push(block)
  }
  
  // Check memory pressure level
  let critical_level = MemoryPressureHandler::check_pressure(handler, pool)
  assert_eq(critical_level, MemoryPressureLevel::Critical)
  
  // Test pressure response
  let response = MemoryPressureHandler::handle_pressure(handler, pool, critical_level)
  assert_true(MemoryPressureHandler::is_successful(response))
  
  // Verify pressure mitigation
  let mitigated_level = MemoryPressureHandler::check_pressure(handler, pool)
  assert_true(mitigated_level < critical_level)
  
  // Clean up
  for allocation in warning_allocations {
    MemoryPool::deallocate(pool, allocation)
  }
}

// Test 8: Buffer Pool Management
test "buffer pool management" {
  // Create a buffer pool
  let buffer_pool = BufferPool::new(1024, 100)  // 1KB buffers, 100 buffers
  
  // Verify initial state
  assert_eq(BufferPool::available_buffers(buffer_pool), 100)
  assert_eq(BufferPool::buffer_size(buffer_pool), 1024)
  
  // Acquire buffers
  let buffers = []
  for i in 0..50 {
    let buffer = BufferPool::acquire(buffer_pool)
    assert_true(BufferPool::is_valid_buffer(buffer))
    assert_eq(BufferPool::capacity(buffer), 1024)
    buffers.push(buffer)
  }
  
  // Verify state after acquisition
  assert_eq(BufferPool::available_buffers(buffer_pool), 50)
  
  // Write data to buffers
  for i in 0..buffers.length() {
    let buffer = buffers[i]
    let data = "Test data " + i.to_string()
    BufferPool::write_string(buffer, data)
    assert_eq(BufferPool::read_string(buffer, data.length()), data)
  }
  
  // Release buffers back to pool
  for buffer in buffers {
    BufferPool::release(buffer_pool, buffer)
  }
  
  // Verify state after release
  assert_eq(BufferPool::available_buffers(buffer_pool), 100)
  
  // Test buffer reuse (should be cleared)
  let reused_buffer = BufferPool::acquire(buffer_pool)
  assert_eq(BufferPool::read_string(reused_buffer, 10), "")  // Should be empty
  
  // Test pool expansion when exhausted
  let expansion_buffers = []
  for i in 0..150 {  // More than initial capacity
    let buffer = BufferPool::acquire(buffer_pool)
    assert_true(BufferPool::is_valid_buffer(buffer))
    expansion_buffers.push(buffer)
  }
  
  // Verify pool expanded
  assert_true(BufferPool::total_buffers(buffer_pool) > 100)
  assert_eq(BufferPool::available_buffers(buffer_pool), 0)
  
  // Clean up
  for buffer in expansion_buffers {
    BufferPool::release(buffer_pool, buffer)
  }
}

// Test 9: Smart Memory Management
test "smart memory management" {
  // Create a smart memory manager
  let manager = SmartMemoryManager::new()
  
  // Configure smart management policies
  SmartMemoryManager::enable_auto_compaction(manager, true)
  SmartMemoryManager::set_compaction_threshold(manager, 0.3)  // Compact when 30% fragmented
  SmartMemoryManager::enable_auto_gc(manager, true)
  SmartMemoryManager::set_gc_threshold(manager, 0.8)  // GC when 80% memory used
  
  // Create a managed memory arena
  let arena = SmartMemoryManager::create_arena(manager, 1024 * 1024)  // 1MB arena
  
  // Allocate and deallocate to create fragmentation
  let allocations = []
  for i in 0..100 {
    let block = SmartMemoryManager::allocate(manager, arena, 1024)  // 1KB blocks
    allocations.push(block)
  }
  
  // Free every other block to create fragmentation
  for i in range_step(0, allocations.length(), 2) {
    SmartMemoryManager::deallocate(manager, arena, allocations[i])
  }
  
  // Check if auto-compaction is triggered
  let fragmentation_before = SmartMemoryManager::get_fragmentation_ratio(manager, arena)
  assert_true(fragmentation_before > 0.0)
  
  // Allocate more memory to trigger auto-GC
  let large_allocations = []
  for i in 0..500 {
    let block = SmartMemoryManager::allocate(manager, arena, 1024)  // 1KB blocks
    large_allocations.push(block)
  }
  
  // Check if auto-GC was triggered
  let gc_stats = SmartMemoryManager::get_gc_statistics(manager)
  assert_true(SmartMemoryManager::get_gc_count(gc_stats) > 0)
  
  // Verify memory is optimized
  let fragmentation_after = SmartMemoryManager::get_fragmentation_ratio(manager, arena)
  assert_true(fragmentation_after <= fragmentation_before)
  
  // Test memory efficiency metrics
  let efficiency_metrics = SmartMemoryManager::get_efficiency_metrics(manager)
  assert_true(SmartMemoryManager::get_allocation_efficiency(efficiency_metrics) > 0.8)
  assert_true(SmartMemoryManager::get_memory_utilization(efficiency_metrics) > 0.7)
  
  // Clean up
  for i in range_step(1, allocations.length(), 2) {
    SmartMemoryManager::deallocate(manager, arena, allocations[i])
  }
  
  for allocation in large_allocations {
    SmartMemoryManager::deallocate(manager, arena, allocation)
  }
}

// Test 10: Memory Profiling
test "memory profiling" {
  // Create a memory profiler
  let profiler = MemoryProfiler::new()
  
  // Start profiling
  MemoryProfiler::start(profiler)
  
  // Perform memory operations
  let allocations = []
  
  // Profile allocation patterns
  for i in 0..10 {
    MemoryProfiler::begin_operation(profiler, "allocation-batch-" + i.to_string())
    
    // Allocate different sized blocks
    for j in 0..10 {
      let size = 1024 * (j + 1)  // 1KB to 10KB
      let block = MemoryProfiler::profiled_allocate(profiler, size)
      allocations.push(block)
    }
    
    MemoryProfiler::end_operation(profiler, "allocation-batch-" + i.to_string())
  }
  
  // Profile deallocation patterns
  for i in 0..10 {
    MemoryProfiler::begin_operation(profiler, "deallocation-batch-" + i.to_string())
    
    // Deallocate some blocks
    for j in range_step(0, allocations.length(), 10) {
      if j < allocations.length() {
        MemoryProfiler::profiled_deallocate(profiler, allocations[j])
      }
    }
    
    MemoryProfiler::end_operation(profiler, "deallocation-batch-" + i.to_string())
  }
  
  // Stop profiling
  MemoryProfiler::stop(profiler)
  
  // Generate profiling report
  let profile_report = MemoryProfiler::generate_report(profiler)
  
  // Verify profiling data
  assert_true(MemoryProfiler::get_total_allocations(profile_report) > 0)
  assert_true(MemoryProfiler::get_total_deallocations(profile_report) > 0)
  assert_true(MemoryProfiler::get_peak_memory_usage(profile_report) > 0)
  
  // Verify operation profiling
  let operations = MemoryProfiler::get_operations(profile_report)
  assert_eq(operations.length(), 20)  // 10 allocation + 10 deallocation operations
  
  for operation in operations {
    assert_true(MemoryProfiler::get_operation_duration(operation) >= 0)
    assert_true(MemoryProfiler::get_operation_memory_delta(operation) != 0)
  }
  
  // Verify allocation size distribution
  let size_distribution = MemoryProfiler::get_allocation_size_distribution(profile_report)
  assert_true(size_distribution.size() > 0)
  
  // Verify memory timeline
  let timeline = MemoryProfiler::get_memory_timeline(profile_report)
  assert_true(timeline.length() > 0)
  
  // Clean up remaining allocations
  for allocation in allocations {
    if allocation != None {
      MemoryProfiler::profiled_deallocate(profiler, allocation)
    }
  }
}