// Azimuth Telemetry System - Memory Management and Resource Cleanup Tests
// This file contains test cases for memory management and resource cleanup functionality

// Test 1: Memory Pool Management
test "memory pool management" {
  // Create memory pool
  let pool = MemoryPool::new(1024) // 1KB pool
  
  // Verify initial state
  assert_eq(MemoryPool::total_capacity(pool), 1024)
  assert_eq(MemoryPool::used_capacity(pool), 0)
  assert_eq(MemoryPool::available_capacity(pool), 1024)
  
  // Allocate memory blocks
  let block1 = MemoryPool::allocate(pool, 256)
  let block2 = MemoryPool::allocate(pool, 512)
  let block3 = MemoryPool::allocate(pool, 128)
  
  // Verify allocations
  assert_true(MemoryBlock::is_valid(block1))
  assert_true(MemoryBlock::is_valid(block2))
  assert_true(MemoryBlock::is_valid(block3))
  
  assert_eq(MemoryPool::used_capacity(pool), 896) // 256 + 512 + 128
  assert_eq(MemoryPool::available_capacity(pool), 128) // 1024 - 896
  
  // Test allocation failure (not enough memory)
  let failed_block = MemoryPool::allocate(pool, 256)
  assert_false(MemoryBlock::is_valid(failed_block))
  
  // Free memory blocks
  MemoryPool::deallocate(pool, block2) // Free 512 bytes
  
  // Verify deallocation
  assert_eq(MemoryPool::used_capacity(pool), 384) // 256 + 128
  assert_eq(MemoryPool::available_capacity(pool), 640) // 1024 - 384
  
  // Test allocation after deallocation
  let block4 = MemoryPool::allocate(pool, 400)
  assert_true(MemoryBlock::is_valid(block4))
  
  // Verify final state
  assert_eq(MemoryPool::used_capacity(pool), 784) // 256 + 128 + 400
  assert_eq(MemoryPool::available_capacity(pool), 240) // 1024 - 784
  
  // Clean up all blocks
  MemoryPool::deallocate(pool, block1)
  MemoryPool::deallocate(pool, block3)
  MemoryPool::deallocate(pool, block4)
  
  // Verify pool is empty
  assert_eq(MemoryPool::used_capacity(pool), 0)
  assert_eq(MemoryPool::available_capacity(pool), 1024)
}

// Test 2: Buffer Management and Cleanup
test "buffer management and cleanup" {
  // Create buffer manager
  let buffer_manager = BufferManager::new()
  
  // Create buffers of different sizes
  let small_buffer = BufferManager::create_buffer(buffer_manager, 1024)    // 1KB
  let medium_buffer = BufferManager::create_buffer(buffer_manager, 10240)  // 10KB
  let large_buffer = BufferManager::create_buffer(buffer_manager, 102400)  // 100KB
  
  // Verify buffer creation
  assert_true(Buffer::is_valid(small_buffer))
  assert_true(Buffer::is_valid(medium_buffer))
  assert_true(Buffer::is_valid(large_buffer))
  
  assert_eq(Buffer::capacity(small_buffer), 1024)
  assert_eq(Buffer::capacity(medium_buffer), 10240)
  assert_eq(Buffer::capacity(large_buffer), 102400)
  
  // Write data to buffers
  let test_data = "Hello, World! This is test data for buffer management."
  
  let small_write_result = Buffer::write(small_buffer, test_data)
  let medium_write_result = Buffer::write(medium_buffer, test_data)
  let large_write_result = Buffer::write(large_buffer, test_data)
  
  assert_true(small_write_result)
  assert_true(medium_write_result)
  assert_true(large_write_result)
  
  // Verify data written
  assert_eq(Buffer::size(small_buffer), test_data.length())
  assert_eq(Buffer::size(medium_buffer), test_data.length())
  assert_eq(Buffer::size(large_buffer), test_data.length())
  
  // Read data from buffers
  let small_read_data = Buffer::read(small_buffer, test_data.length())
  let medium_read_data = Buffer::read(medium_buffer, test_data.length())
  let large_read_data = Buffer::read(large_buffer, test_data.length())
  
  match small_read_data {
    Some(data) => assert_eq(data, test_data)
    None => assert_true(false)
  }
  
  match medium_read_data {
    Some(data) => assert_eq(data, test_data)
    None => assert_true(false)
  }
  
  match large_read_data {
    Some(data) => assert_eq(data, test_data)
    None => assert_true(false)
  }
  
  // Test buffer resize
  let resized_small = BufferManager::resize_buffer(buffer_manager, small_buffer, 2048)
  assert_true(Buffer::is_valid(resized_small))
  assert_eq(Buffer::capacity(resized_small), 2048)
  
  // Test buffer copy
  let copied_buffer = BufferManager::copy_buffer(buffer_manager, medium_buffer)
  assert_true(Buffer::is_valid(copied_buffer))
  assert_eq(Buffer::capacity(copied_buffer), Buffer::capacity(medium_buffer))
  assert_eq(Buffer::size(copied_buffer), Buffer::size(medium_buffer))
  
  // Clean up buffers
  BufferManager::destroy_buffer(buffer_manager, small_buffer)
  BufferManager::destroy_buffer(buffer_manager, resized_small)
  BufferManager::destroy_buffer(buffer_manager, medium_buffer)
  BufferManager::destroy_buffer(buffer_manager, copied_buffer)
  BufferManager::destroy_buffer(buffer_manager, large_buffer)
  
  // Verify buffers are cleaned up
  assert_false(Buffer::is_valid(small_buffer))
  assert_false(Buffer::is_valid(resized_small))
  assert_false(Buffer::is_valid(medium_buffer))
  assert_false(Buffer::is_valid(copied_buffer))
  assert_false(Buffer::is_valid(large_buffer))
}

// Test 3: Resource Reference Counting
test "resource reference counting" {
  // Create resource with reference counting
  let resource = RefCountedResource::new("test_resource", 1024)
  
  // Verify initial reference count
  assert_eq(RefCountedResource::ref_count(resource), 1)
  
  // Add references
  let ref1 = RefCountedResource::add_ref(resource)
  let ref2 = RefCountedResource::add_ref(resource)
  let ref3 = RefCountedResource::add_ref(resource)
  
  // Verify reference count after adding references
  assert_eq(RefCountedResource::ref_count(resource), 4)
  assert_eq(RefCountedResource::ref_count(ref1), 4)
  assert_eq(RefCountedResource::ref_count(ref2), 4)
  assert_eq(RefCountedResource::ref_count(ref3), 4)
  
  // Release references
  RefCountedResource::release(ref1)
  assert_eq(RefCountedResource::ref_count(resource), 3)
  
  RefCountedResource::release(ref2)
  assert_eq(RefCountedResource::ref_count(resource), 2)
  
  RefCountedResource::release(ref3)
  assert_eq(RefCountedResource::ref_count(resource), 1)
  
  // Test resource data access
  match RefCountedResource::data(resource) {
    Some(data) => assert_eq(data, "test_resource")
    None => assert_true(false)
  }
  
  // Test resource size
  assert_eq(RefCountedResource::size(resource), 1024)
  
  // Final release (resource should be destroyed)
  RefCountedResource::release(resource)
  
  // Resource should no longer be valid
  assert_false(RefCountedResource::is_valid(resource))
}

// Test 4: Garbage Collection for Telemetry Data
test "garbage collection for telemetry data" {
  // Create garbage collector
  let gc = GarbageCollector::new()
  
  // Create telemetry data objects
  let metric1 = TelemetryMetric::new("cpu.usage", 75.5, None, None)
  let metric2 = TelemetryMetric::new("memory.usage", 60.2, None, None)
  let metric3 = TelemetryMetric::new("disk.usage", 45.8, None, None)
  
  // Register objects with garbage collector
  let gc_with_objects = GarbageCollector::register(
    GarbageCollector::register(
      GarbageCollector::register(gc, metric1),
      metric2),
    metric3)
  
  // Verify objects are registered
  assert_true(GarbageCollector::is_registered(gc_with_objects, metric1))
  assert_true(GarbageCollector::is_registered(gc_with_objects, metric2))
  assert_true(GarbageCollector::is_registered(gc_with_objects, metric3))
  
  // Mark objects as reachable
  let gc_with_reachable = GarbageCollector::mark_reachable(gc_with_objects, [metric1, metric3])
  
  // Run garbage collection
  let gc_after_collection = GarbageCollector::collect(gc_with_reachable)
  
  // Verify that metric2 was collected (not marked as reachable)
  assert_true(GarbageCollector::is_registered(gc_after_collection, metric1))
  assert_false(GarbageCollector::is_registered(gc_after_collection, metric2))
  assert_true(GarbageCollector::is_registered(gc_after_collection, metric3))
  
  // Get collection statistics
  let stats = GarbageCollector::get_stats(gc_after_collection)
  assert_eq(GCStats::objects_collected(stats), 1)
  assert_eq(GCStats::objects_remaining(stats), 2)
  assert_true(GCStats::memory_freed(stats) > 0)
  
  // Test generational garbage collection
  let young_metric = TelemetryMetric::new("network.latency", 25.3, None, None)
  let old_metric = TelemetryMetric::new("cache.hit.rate", 85.7, None, None)
  
  let gc_with_generations = GarbageCollector::register(
    GarbageCollector::register(gc_after_collection, young_metric),
    old_metric)
  
  // Mark young metric as young generation and old metric as old generation
  let gc_with_generation_marks = GarbageCollector::mark_generation(
    GarbageCollector::mark_generation(gc_with_generations, young_metric, "young"),
    old_metric,
    "old"
  )
  
  // Run young generation collection
  let gc_after_young_gc = GarbageCollector::collect_generation(gc_with_generation_marks, "young")
  
  // Verify old generation objects survive young generation collection
  assert_true(GarbageCollector::is_registered(gc_after_young_gc, old_metric))
  assert_true(GarbageCollector::is_registered(gc_after_young_gc, metric1))
  assert_true(GarbageCollector::is_registered(gc_after_young_gc, metric3))
}

// Test 5: Memory Leak Detection
test "memory leak detection" {
  // Create memory leak detector
  let leak_detector = MemoryLeakDetector::new()
  
  // Start tracking
  let tracking_detector = MemoryLeakDetector::start_tracking(leak_detector)
  
  // Allocate some resources
  let resource1 = Resource::new("resource1", 1024)
  let resource2 = Resource::new("resource2", 2048)
  let resource3 = Resource::new("resource3", 4096)
  
  // Register resources with leak detector
  let detector_with_resources = MemoryLeakDetector::track_resource(
    MemoryLeakDetector::track_resource(
      MemoryLeakDetector::track_resource(tracking_detector, resource1),
      resource2),
    resource3)
  
  // Properly clean up some resources
  Resource::destroy(resource1)
  Resource::destroy(resource3)
  
  // Update leak detector with cleaned up resources
  let detector_after_cleanup = MemoryLeakDetector::untrack_resource(
    MemoryLeakDetector::untrack_resource(detector_with_resources, resource1),
    resource3)
  
  // Check for leaks
  let leak_report = MemoryLeakDetector::detect_leaks(detector_after_cleanup)
  
  // Should detect resource2 as a leak
  assert_eq(LeakReport::leaked_resources(leak_report).length(), 1)
  
  let leaked_resource = LeakReport::leaked_resources(leak_report)[0]
  assert_eq(Resource::name(leaked_resource), "resource2")
  assert_eq(Resource::size(leaked_resource), 2048)
  
  // Verify leak statistics
  assert_eq(LeakReport::total_leaked_memory(leak_report), 2048)
  assert_eq(LeakReport::leak_count(leak_report), 1)
  
  // Clean up remaining resource
  Resource::destroy(resource2)
  
  // Final leak check
  let final_leak_report = MemoryLeakDetector::detect_leaks(detector_after_cleanup)
  assert_eq(LeakReport::leaked_resources(final_leak_report).length(), 0)
  assert_eq(LeakReport::total_leaked_memory(final_leak_report), 0)
  
  // Stop tracking
  MemoryLeakDetector::stop_tracking(detector_after_cleanup)
}

// Test 6: Resource Pool Cleanup
test "resource pool cleanup" {
  // Create resource pool
  let resource_pool = ResourcePool::new(10) // Maximum 10 resources
  
  // Verify initial state
  assert_eq(ResourcePool::capacity(resource_pool), 10)
  assert_eq(ResourcePool::available_count(resource_pool), 10)
  assert_eq(ResourcePool::used_count(resource_pool), 0)
  
  // Acquire resources from pool
  let resource1 = ResourcePool::acquire(resource_pool)
  let resource2 = ResourcePool::acquire(resource_pool)
  let resource3 = ResourcePool::acquire(resource_pool)
  
  // Verify resources were acquired
  assert_true(Resource::is_valid(resource1))
  assert_true(Resource::is_valid(resource2))
  assert_true(Resource::is_valid(resource3))
  
  assert_eq(ResourcePool::available_count(resource_pool), 7)
  assert_eq(ResourcePool::used_count(resource_pool), 3)
  
  // Return resources to pool
  ResourcePool::release(resource_pool, resource1)
  ResourcePool::release(resource_pool, resource2)
  
  // Verify resources were returned
  assert_eq(ResourcePool::available_count(resource_pool), 9)
  assert_eq(ResourcePool::used_count(resource_pool), 1)
  
  // Acquire more resources
  let resource4 = ResourcePool::acquire(resource_pool)
  let resource5 = ResourcePool::acquire(resource_pool)
  
  assert_true(Resource::is_valid(resource4))
  assert_true(Resource::is_valid(resource5))
  
  // Fill the pool to capacity
  let remaining_resources = []
  for i in 0..5 {
    let resource = ResourcePool::acquire(resource_pool)
    remaining_resources = Array::push(remaining_resources, resource)
  }
  
  assert_eq(ResourcePool::available_count(resource_pool), 0)
  assert_eq(ResourcePool::used_count(resource_pool), 10)
  
  // Try to acquire from full pool (should fail)
  let failed_resource = ResourcePool::acquire(resource_pool)
  assert_false(Resource::is_valid(failed_resource))
  
  // Clear the pool (release all resources)
  ResourcePool::clear(resource_pool)
  
  // Verify pool is cleared
  assert_eq(ResourcePool::available_count(resource_pool), 10)
  assert_eq(ResourcePool::used_count(resource_pool), 0)
  
  // All previously acquired resources should now be invalid
  assert_false(Resource::is_valid(resource3))
  assert_false(Resource::is_valid(resource4))
  assert_false(Resource::is_valid(resource5))
  
  for resource in remaining_resources {
    assert_false(Resource::is_valid(resource))
  }
}

// Test 7: Circular Reference Detection and Cleanup
test "circular reference detection and cleanup" {
  // Create objects that can reference each other
  let object_a = CircularRefObject::new("A")
  let object_b = CircularRefObject::new("B")
  let object_c = CircularRefObject::new("C")
  
  // Create circular references: A -> B -> C -> A
  CircularRefObject::set_reference(object_a, object_b)
  CircularRefObject::set_reference(object_b, object_c)
  CircularRefObject::set_reference(object_c, object_a)
  
  // Verify circular references
  assert_true(CircularRefObject::has_reference(object_a, object_b))
  assert_true(CircularRefObject::has_reference(object_b, object_c))
  assert_true(CircularRefObject::has_reference(object_c, object_a))
  
  // Create circular reference detector
  let detector = CircularRefDetector::new()
  
  // Detect circular references
  let circular_refs = CircularRefDetector::detect(detector, [object_a, object_b, object_c])
  
  // Should detect the circular reference
  assert_eq(circular_refs.length(), 1)
  
  let circular_ref = circular_refs[0]
  assert_eq(CircularRef::length(circular_ref), 3)
  
  // Break the circular reference
  CircularRefObject::clear_reference(object_c) // Break C -> A
  
  // Verify circular reference is broken
  assert_false(CircularRefObject::has_reference(object_c, object_a))
  
  // Detect circular references again
  let circular_refs_after_break = CircularRefDetector::detect(detector, [object_a, object_b, object_c])
  
  // Should not detect any circular references
  assert_eq(circular_refs_after_break.length(), 0)
  
  // Clean up objects
  CircularRefObject::destroy(object_a)
  CircularRefObject::destroy(object_b)
  CircularRefObject::destroy(object_c)
}

// Test 8: Memory Usage Monitoring and Thresholds
test "memory usage monitoring and thresholds" {
  // Create memory monitor
  let monitor = MemoryMonitor::new()
  
  // Set memory thresholds
  let monitor_with_thresholds = MemoryMonitor::set_thresholds(monitor, [
    ("warning", 80.0),   // 80% usage triggers warning
    ("critical", 90.0),  // 90% usage triggers critical alert
    ("emergency", 95.0)  // 95% usage triggers emergency
  ])
  
  // Simulate memory usage
  let monitor_with_usage = MemoryMonitor::update_usage(monitor_with_thresholds, 50.0)
  
  // Check current status
  assert_eq(MemoryMonitor::current_usage(monitor_with_usage), 50.0)
  assert_eq(MemoryMonitor::status(monitor_with_usage), "normal")
  
  // Increase usage to warning level
  let monitor_at_warning = MemoryMonitor::update_usage(monitor_with_usage, 85.0)
  
  assert_eq(MemoryMonitor::current_usage(monitor_at_warning), 85.0)
  assert_eq(MemoryMonitor::status(monitor_at_warning), "warning")
  
  // Check if warning alert was triggered
  let alerts = MemoryMonitor::get_alerts(monitor_at_warning)
  assert_eq(alerts.length(), 1)
  assert_eq(MemoryAlert::level(alerts[0]), "warning")
  
  // Increase usage to critical level
  let monitor_at_critical = MemoryMonitor::update_usage(monitor_at_warning, 92.0)
  
  assert_eq(MemoryMonitor::current_usage(monitor_at_critical), 92.0)
  assert_eq(MemoryMonitor::status(monitor_at_critical), "critical")
  
  // Check if critical alert was triggered
  let critical_alerts = MemoryMonitor::get_alerts(monitor_at_critical)
  assert_eq(critical_alerts.length(), 2) // Previous warning + new critical
  
  // Increase usage to emergency level
  let monitor_at_emergency = MemoryMonitor::update_usage(monitor_at_critical, 97.0)
  
  assert_eq(MemoryMonitor::current_usage(monitor_at_emergency), 97.0)
  assert_eq(MemoryMonitor::status(monitor_at_emergency), "emergency")
  
  // Check if emergency alert was triggered
  let emergency_alerts = MemoryMonitor::get_alerts(monitor_at_emergency)
  assert_eq(emergency_alerts.length(), 3) // Previous alerts + new emergency
  
  // Test automatic cleanup at emergency level
  let monitor_after_cleanup = MemoryMonitor::trigger_emergency_cleanup(monitor_at_emergency)
  
  // Verify cleanup was triggered
  assert_true(MemoryMonitor::cleanup_triggered(monitor_after_cleanup))
  
  // Usage should be reduced after cleanup
  assert_true(MemoryMonitor::current_usage(monitor_after_cleanup) < 97.0)
  
  // Test memory usage history
  let history = MemoryMonitor::get_usage_history(monitor_after_cleanup, 5) // Get last 5 measurements
  assert_eq(history.length(), 5)
  
  // Verify history contains expected values
  assert_eq(history[0], 50.0)   // Initial
  assert_eq(history[1], 85.0)   // Warning
  assert_eq(history[2], 92.0)   // Critical
  assert_eq(history[3], 97.0)   // Emergency
  assert_true(history[4] < 97.0) // After cleanup
}