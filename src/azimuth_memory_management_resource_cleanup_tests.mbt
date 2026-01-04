// Azimuth Telemetry System - Memory Management and Resource Cleanup Tests
// This file contains comprehensive test cases for memory management and resource cleanup

// Test 1: Memory Allocation and Deallocation
test "memory allocation and deallocation" {
  let memory_manager = MemoryManager::new()
  
  // Test basic memory allocation
  let initial_memory = memory_manager.get_allocated_memory()
  
  let allocated_blocks = []
  for i in 0..=100 {
    let block = memory_manager.allocate(1024 * i)  // Allocate varying sizes
    allocated_blocks = allocated_blocks.push(block)
  }
  
  let peak_memory = memory_manager.get_allocated_memory()
  assert_true(peak_memory > initial_memory)
  
  // Test memory deallocation
  for block in allocated_blocks {
    memory_manager.deallocate(block)
  }
  
  let final_memory = memory_manager.get_allocated_memory()
  
  // Memory should be released (allowing for some fragmentation)
  let memory_leak = final_memory - initial_memory
  assert_true(memory_leak < 1024 * 10)  // Less than 10KB leak
  
  // Test memory pool allocation
  let pool = memory_manager.create_pool(1024, 100)  // 1024 byte blocks, 100 blocks
  
  let pool_blocks = []
  for i in 0..=50 {
    let block = memory_manager.allocate_from_pool(pool)
    pool_blocks = pool_blocks.push(block)
  }
  
  // Return blocks to pool
  for block in pool_blocks {
    memory_manager.deallocate_to_pool(pool, block)
  }
  
  // Pool should still have available blocks
  assert_true(memory_manager.pool_available_count(pool) > 0)
}

// Test 2: Buffer Management
test "buffer management" {
  let buffer_manager = BufferManager::new()
  
  // Test buffer creation and sizing
  let buffer = buffer_manager.create_buffer(1024)
  assert_eq(buffer_manager.get_buffer_size(buffer), 1024)
  assert_eq(buffer_manager.get_buffer_usage(buffer), 0)
  
  // Test buffer writing
  let data = "Hello, World!"
  let bytes_written = buffer_manager.write_to_buffer(buffer, data)
  assert_eq(bytes_written, data.length())
  assert_eq(buffer_manager.get_buffer_usage(buffer), data.length())
  
  // Test buffer reading
  let read_data = buffer_manager.read_from_buffer(buffer, 0, data.length())
  assert_eq(read_data, data)
  
  // Test buffer resizing
  let resize_result = buffer_manager.resize_buffer(buffer, 2048)
  assert_true(resize_result)
  assert_eq(buffer_manager.get_buffer_size(buffer), 2048)
  assert_eq(buffer_manager.get_buffer_usage(buffer), data.length())  // Usage should remain
  
  // Test buffer clearing
  buffer_manager.clear_buffer(buffer)
  assert_eq(buffer_manager.get_buffer_usage(buffer), 0)
  
  // Test buffer pooling
  let buffer_pool = buffer_manager.create_buffer_pool(512, 20)  // 512 byte buffers, 20 buffers
  
  let pooled_buffers = []
  for i in 0..=15 {
    let buf = buffer_manager.get_buffer_from_pool(buffer_pool)
    pooled_buffers = pooled_buffers.push(buf)
  }
  
  // Return buffers to pool
  for buf in pooled_buffers {
    buffer_manager.return_buffer_to_pool(buffer_pool, buf)
  }
  
  // Pool should have available buffers
  assert_true(buffer_manager.pool_available_buffers(buffer_pool) >= 15)
  
  // Clean up
  buffer_manager.destroy_buffer(buffer)
  buffer_manager.destroy_buffer_pool(buffer_pool)
}

// Test 3: Resource Lifecycle Management
test "resource lifecycle management" {
  let resource_manager = ResourceManager::new()
  
  // Test resource creation and tracking
  let resource1 = resource_manager.create_resource("telemetry_collector", ResourceConfig::new(
    auto_cleanup: true,
    cleanup_timeout: 5000
  ))
  
  let resource2 = resource_manager.create_resource("metrics_aggregator", ResourceConfig::new(
    auto_cleanup: true,
    cleanup_timeout: 3000
  ))
  
  // Verify resources are tracked
  let active_resources = resource_manager.get_active_resources()
  assert_eq(active_resources.length(), 2)
  assert_true(active_resources.contains("telemetry_collector"))
  assert_true(active_resources.contains("metrics_aggregator"))
  
  // Test resource access
  let collector = resource_manager.get_resource("telemetry_collector")
  assert_true(collector.is_some())
  
  let non_existent = resource_manager.get_resource("non_existent_resource")
  assert_true(non_existent.is_none())
  
  // Test resource dependency management
  resource_manager.add_dependency("metrics_aggregator", "telemetry_collector")
  
  let dependencies = resource_manager.get_dependencies("metrics_aggregator")
  assert_true(dependencies.contains("telemetry_collector"))
  
  // Test resource cleanup order (dependencies first)
  let cleanup_order = resource_manager.calculate_cleanup_order()
  let collector_index = cleanup_orders.index_of("telemetry_collector")
  let aggregator_index = cleanup_orders.index_of("metrics_aggregator")
  
  assert_true(collector_index > aggregator_index)  // Collector should be cleaned up after aggregator
  
  // Test resource cleanup
  resource_manager.cleanup_resource("telemetry_collector")
  
  let remaining_resources = resource_manager.get_active_resources()
  assert_eq(remaining_resources.length(), 1)
  assert_false(remaining_resources.contains("telemetry_collector"))
  assert_true(remaining_resources.contains("metrics_aggregator"))
  
  // Test cleanup of all resources
  resource_manager.cleanup_all_resources()
  
  let final_resources = resource_manager.get_active_resources()
  assert_eq(final_resources.length(), 0)
}

// Test 4: Garbage Collection Simulation
test "garbage collection simulation" {
  let gc_simulator = GCSimulator::new()
  
  // Create objects with different lifetimes
  let short_lived_objects = []
  for i in 0..=1000 {
    let obj = gc_simulator.create_object("short_lived", 100)  // 100 bytes
    short_lived_objects = short_lived_objects.push(obj)
  }
  
  // Release references to short-lived objects
  short_lived_objects = []
  
  let long_lived_objects = []
  for i in 0..=100 {
    let obj = gc_simulator.create_object("long_lived", 1000)  // 1000 bytes
    long_lived_objects = long_lived_objects.push(obj)
  }
  
  // Check memory before GC
  let memory_before_gc = gc_simulator.get_memory_usage()
  
  // Run garbage collection
  let gc_result = gc_simulator.run_gc()
  
  // Verify GC results
  assert_true(gc_result.objects_collected > 0)
  assert_true(gc_result.memory_freed > 0)
  
  // Check memory after GC
  let memory_after_gc = gc_simulator.get_memory_usage()
  assert_true(memory_after_gc < memory_before_gc)
  
  // Verify long-lived objects are still accessible
  for obj in long_lived_objects {
    assert_true(gc_simulator.is_object_alive(obj))
  }
  
  // Test generational GC
  gc_simulator.configure_generational_gc(
    young_gen_size: 1024 * 1024,    // 1MB young generation
    old_gen_size: 10 * 1024 * 1024,  // 10MB old generation
    promotion_threshold: 10          // Promote after 10 GC cycles
  )
  
  // Create objects to trigger generational GC
  for i in 0..=10000 {
    let obj = gc_simulator.create_object("gen_test", 50)
    if i % 100 == 0 {
      gc_simulator.run_young_gc()
    }
  }
  
  let gen_gc_result = gc_simulator.run_full_gc()
  assert_true(gen_gc_result.young_gen_collected > 0)
  assert_true(gen_gc_result.old_gen_collected >= 0)
}

// Test 5: Memory Leak Detection
test "memory leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  
  // Start monitoring
  leak_detector.start_monitoring()
  
  // Simulate normal operation (no leaks)
  let normal_objects = []
  for i in 0..=100 {
    let obj = leak_detector.allocate_tracked_object(1024)
    normal_objects = normal_objects.push(obj)
  }
  
  // Properly clean up objects
  for obj in normal_objects {
    leak_detector.deallocate_tracked_object(obj)
  }
  
  let normal_report = leak_detector.generate_report()
  assert_eq(normal_report.leaked_objects, 0)
  assert_eq(normal_report.leaked_memory, 0)
  
  // Simulate memory leak (not cleaning up objects)
  let leaked_objects = []
  for i in 0..=100 {
    let obj = leak_detector.allocate_tracked_object(2048)
    leaked_objects = leaked_objects.push(obj)
  }
  
  // Don't clean up these objects (simulate leak)
  
  let leak_report = leak_detector.generate_report()
  assert_eq(leak_report.leaked_objects, 100)
  assert_eq(leak_report.leaked_memory, 2048 * 100)
  
  // Test leak detection with object types
  leak_detector.track_object_type("telemetry_span", 512)
  leak_detector.track_object_type("metric_data", 256)
  
  let span_objects = []
  let metric_objects = []
  
  for i in 0..=50 {
    let span = leak_detector.allocate_typed_object("telemetry_span")
    let metric = leak_detector.allocate_typed_object("metric_data")
    span_objects = span_objects.push(span)
    metric_objects = metric_objects.push(metric)
  }
  
  // Clean up only metrics, leak spans
  for metric in metric_objects {
    leak_detector.deallocate_tracked_object(metric)
  }
  
  let typed_leak_report = leak_detector.generate_typed_report()
  assert_true(typed_leak_report.leaks_by_type.contains_key("telemetry_span"))
  assert_eq(typed_leak_report.leaks_by_type["telemetry_span"], 50)
  assert_false(typed_leak_report.leaks_by_type.contains_key("metric_data"))
  
  // Test automatic leak detection
  leak_detector.enable_auto_detection(1000)  // Check every 1000 allocations
  
  // Continue allocating to trigger auto-detection
  for i in 0..=2000 {
    let _ = leak_detector.allocate_typed_object("auto_test")
  }
  
  let auto_detected = leak_detector.get_auto_detected_leaks()
  assert_true(auto_detected.length() > 0)
}

// Test 6: Resource Pool Management
test "resource pool management" {
  let pool_manager = ResourcePoolManager::new()
  
  // Create connection pool
  let connection_pool = pool_manager.create_pool("database_connections", PoolConfig::new(
    min_size: 5,
    max_size: 20,
    create_timeout: 5000,
    destroy_timeout: 1000
  ))
  
  // Test pool initialization
  assert_eq(pool_manager.pool_size(connection_pool), 5)
  assert_eq(pool_manager.pool_available(connection_pool), 5)
  
  // Acquire connections
  let connections = []
  for i in 0..=10 {
    let conn = pool_manager.acquire(connection_pool)
    connections = connections.push(conn)
  }
  
  assert_eq(pool_manager.pool_available(connection_pool), 0)
  assert_eq(pool_manager.pool_active(connection_pool), 10)
  
  // Release connections
  for conn in connections {
    pool_manager.release(connection_pool, conn)
  }
  
  assert_eq(pool_manager.pool_available(connection_pool), 10)
  assert_eq(pool_manager.pool_active(connection_pool), 0)
  
  // Test pool expansion
  let more_connections = []
  for i in 0..=15 {
    let conn = pool_manager.acquire(connection_pool)
    more_connections = more_connections.push(conn)
  }
  
  assert_eq(pool_manager.pool_size(connection_pool), 15)  // Pool expanded
  assert_eq(pool_manager.pool_available(connection_pool), 0)
  
  // Test pool shrinking
  for conn in more_connections {
    pool_manager.release(connection_pool, conn)
  }
  
  // Pool should shrink back to min_size
  pool_manager.maintain_pools()
  assert_eq(pool_manager.pool_size(connection_pool), 5)
  
  // Test pool statistics
  let stats = pool_manager.get_pool_stats(connection_pool)
  assert_true(stats.total_acquired > 0)
  assert_true(stats.total_released > 0)
  assert_eq(stats.active_connections, 0)
  
  // Test pool destruction
  pool_manager.destroy_pool(connection_pool)
  
  // Create object pool
  let object_pool = pool_manager.create_object_pool("buffer_objects", ObjectPoolConfig::new(
    factory: fn() { Buffer::new(1024) },
    reset: fn(buffer) { buffer.clear() },
    validator: fn(buffer) { buffer.is_valid() },
    initial_size: 10,
    max_size: 50
  ))
  
  // Test object pool usage
  let objects = []
  for i in 0..=20 {
    let obj = pool_manager.acquire_object(object_pool)
    objects = objects.push(obj)
  }
  
  // Use and return objects
  for obj in objects {
    // Simulate object usage
    obj.write("test data")
    pool_manager.release_object(object_pool, obj)
  }
  
  // Objects should be reset and available
  assert_eq(pool_manager.object_pool_available(object_pool), 20)
  
  // Test object validation
  let invalid_obj = pool_manager.acquire_object(object_pool)
  invalid_obj.corrupt()  // Simulate corruption
  
  // Should not be accepted back into pool
  let release_result = pool_manager.release_object(object_pool, invalid_obj)
  assert_false(release_result)
}

// Test 7: File Handle Management
test "file handle management" {
  let file_manager = FileManager::new()
  
  // Test file opening and tracking
  let file1 = file_manager.open_file("/tmp/test1.txt", "w")
  let file2 = file_manager.open_file("/tmp/test2.txt", "w")
  let file3 = file_manager.open_file("/tmp/test3.txt", "w")
  
  // Verify files are tracked
  let open_files = file_manager.get_open_files()
  assert_eq(open_files.length(), 3)
  
  // Test file operations
  file_manager.write_file(file1, "Test content 1")
  file_manager.write_file(file2, "Test content 2")
  file_manager.write_file(file3, "Test content 3")
  
  // Test file closing
  file_manager.close_file(file1)
  
  let remaining_files = file_manager.get_open_files()
  assert_eq(remaining_files.length(), 2)
  assert_false(remaining_files.contains("/tmp/test1.txt"))
  
  // Test automatic cleanup on resource manager destruction
  let scoped_manager = FileManager::new()
  let scoped_file = scoped_manager.open_file("/tmp/scoped.txt", "w")
  scoped_manager.write_file(scoped_file, "Scoped content")
  
  // When scoped_manager goes out of scope, file should be automatically closed
  // (This is simulated by explicit cleanup)
  scoped_manager.cleanup_all()
  
  // Test file handle leak detection
  let leak_detector = FileHandleLeakDetector::new()
  leak_detector.start_monitoring()
  
  // Open files without closing them
  let leaked_files = []
  for i in 0..=10 {
    let file = file_manager.open_file("/tmp/leak" + i.to_string() + ".txt", "w")
    leaked_files = leaked_files.push(file)
  }
  
  // Don't close files (simulate leak)
  
  let leak_report = leak_detector.detect_leaks()
  assert_eq(leak_report.leaked_handles, 10)
  
  // Clean up leaked files
  for file in leaked_files {
    file_manager.close_file(file)
  }
  
  // Test file handle limits
  file_manager.set_handle_limit(5)
  
  let limited_files = []
  for i in 0..=10 {
    let file = file_manager.open_file("/tmp/limited" + i.to_string() + ".txt", "w")
    match file {
      Ok(f) => limited_files = limited_files.push(f),
      Err(error) => assert_true(error.contains("handle limit"))
    }
  }
  
  assert_true(limited_files.length() <= 5)
  
  // Clean up
  for file in limited_files {
    file_manager.close_file(file)
  }
  
  file_manager.cleanup_all()
}

// Test 8: Network Resource Management
test "network resource management" {
  let network_manager = NetworkResourceManager::new()
  
  // Test connection creation and tracking
  let conn1 = network_manager.create_connection("tcp://server1:8080")
  let conn2 = network_manager.create_connection("tcp://server2:8080")
  let conn3 = network_manager.create_connection("tcp://server3:8080")
  
  // Verify connections are tracked
  let active_connections = network_manager.get_active_connections()
  assert_eq(active_connections.length(), 3)
  
  // Test connection usage
  network_manager.send_data(conn1, "Request 1")
  network_manager.send_data(conn2, "Request 2")
  network_manager.send_data(conn3, "Request 3")
  
  // Test connection closing
  network_manager.close_connection(conn1)
  
  let remaining_connections = network_manager.get_active_connections()
  assert_eq(remaining_connections.length(), 2)
  
  // Test connection pooling
  let connection_pool = network_manager.create_connection_pool("tcp://pooled-server:8080", PoolConfig::new(
    min_size: 2,
    max_size: 10,
    connection_timeout: 5000,
    idle_timeout: 30000
  ))
  
  // Acquire connections from pool
  let pooled_connections = []
  for i in 0..=5 {
    let conn = network_manager.acquire_connection(connection_pool)
    pooled_connections = pooled_connections.push(conn)
  }
  
  assert_eq(network_manager.pool_active_connections(connection_pool), 5)
  
  // Return connections to pool
  for conn in pooled_connections {
    network_manager.release_connection(connection_pool, conn)
  }
  
  assert_eq(network_manager.pool_active_connections(connection_pool), 0)
  assert_eq(network_manager.pool_available_connections(connection_pool), 5)
  
  // Test connection health checking
  let unhealthy_conn = network_manager.create_connection("tcp://unhealthy-server:8080")
  network_manager.simulate_connection_failure(unhealthy_conn)
  
  let is_healthy = network_manager.check_connection_health(unhealthy_conn)
  assert_false(is_healthy)
  
  // Test automatic connection recovery
  network_manager.enable_auto_recovery(true)
  let recovered = network_manager.recover_connection(unhealthy_conn)
  assert_true(recovered)
  
  // Test socket cleanup
  let socket = network_manager.create_socket("udp://localhost:9999")
  network_manager.send_socket_data(socket, "UDP test")
  
  let open_sockets = network_manager.get_open_sockets()
  assert_eq(open_sockets.length(), 1)
  
  network_manager.close_socket(socket)
  
  let remaining_sockets = network_manager.get_open_sockets()
  assert_eq(remaining_sockets.length(), 0)
  
  // Clean up all resources
  network_manager.cleanup_all()
}

// Test 9: Cache Memory Management
test "cache memory management" {
  let cache_manager = CacheManager::new()
  
  // Configure cache with memory limits
  cache_manager.configure_cache("telemetry_cache", CacheConfig::new(
    max_memory: 10 * 1024 * 1024,  // 10MB
    max_entries: 10000,
    eviction_policy: "lru"
  ))
  
  // Add items to cache
  for i in 0..=1000 {
    let key = "item_" + i.to_string()
    let value = "data_" + i.to_string() + "_" + "x".repeat(100)  // ~100 bytes per item
    cache_manager.put("telemetry_cache", key, value)
  }
  
  // Check memory usage
  let memory_usage = cache_manager.get_memory_usage("telemetry_cache")
  assert_true(memory_usage > 0)
  assert_true(memory_usage <= 10 * 1024 * 1024)  // Should not exceed limit
  
  let entry_count = cache_manager.get_entry_count("telemetry_cache")
  assert_true(entry_count <= 10000)
  
  // Test cache eviction
  let initial_memory = cache_manager.get_memory_usage("telemetry_cache")
  
  // Add more items to trigger eviction
  for i in 0..=2000 {
    let key = "eviction_item_" + i.to_string()
    let value = "eviction_data_" + i.to_string() + "_" + "x".repeat(100)
    cache_manager.put("telemetry_cache", key, value)
  }
  
  let final_memory = cache_manager.get_memory_usage("telemetry_cache")
  assert_true(final_memory <= 10 * 1024 * 1024)  // Should still be within limit
  
  // Test cache cleanup
  cache_manager.cleanup_expired_entries("telemetry_cache")
  
  // Test cache statistics
  let stats = cache_manager.get_statistics("telemetry_cache")
  assert_true(stats.hits > 0 || stats.misses > 0)
  assert_true(stats.evictions > 0)
  
  // Test memory pressure handling
  cache_manager.simulate_memory_pressure()
  
  // Cache should reduce memory usage under pressure
  let pressure_memory = cache_manager.get_memory_usage("telemetry_cache")
  assert_true(pressure_memory < final_memory)
  
  // Test cache destruction
  cache_manager.destroy_cache("telemetry_cache")
  
  let remaining_memory = cache_manager.get_memory_usage("telemetry_cache")
  assert_eq(remaining_memory, 0)
}

// Test 10: Resource Cleanup on Exception
test "resource cleanup on exception" {
  let exception_handler = ExceptionAwareResourceManager::new()
  
  // Test cleanup when exception occurs during resource usage
  let cleanup_tracker = CleanupTracker::new()
  
  // Register cleanup actions
  exception_handler.register_cleanup_action("file_cleanup", fn() {
    cleanup_tracker.record_cleanup("file")
  })
  
  exception_handler.register_cleanup_action("network_cleanup", fn() {
    cleanup_tracker.record_cleanup("network")
  })
  
  exception_handler.register_cleanup_action("memory_cleanup", fn() {
    cleanup_tracker.record_cleanup("memory")
  })
  
  // Simulate operation that throws exception
  let operation_result = exception_handler.execute_with_cleanup(fn() {
    // Acquire resources
    let file = exception_handler.acquire_file_resource("/tmp/exception_test.txt")
    let connection = exception_handler.acquire_network_resource("tcp://localhost:8080")
    let memory = exception_handler.acquire_memory_resource(1024 * 1024)
    
    // Simulate exception
    if true {
      Exception::throw("Simulated operation failure")
    }
    
    // This code should not be reached
    exception_handler.release_file_resource(file)
    exception_handler.release_network_resource(connection)
    exception_handler.release_memory_resource(memory)
  })
  
  // Operation should fail
  match operation_result {
    Ok(_) => assert_true(false),
    Err(error) => assert_true(error.contains("Simulated operation failure"))
  }
  
  // Cleanup actions should have been executed
  let cleanup_actions = cleanup_tracker.get_executed_actions()
  assert_true(cleanup_actions.contains("file"))
  assert_true(cleanup_actions.contains("network"))
  assert_true(cleanup_actions.contains("memory"))
  
  // Test nested operations with exceptions
  let nested_result = exception_handler.execute_with_cleanup(fn() {
    exception_handler.execute_with_cleanup(fn() {
      // Inner operation that fails
      Exception::throw("Inner operation failure")
    })
    
    // This should not be reached
    assert_true(false)
  })
  
  match nested_result {
    Ok(_) => assert_true(false),
    Err(error) => assert_true(error.contains("Inner operation failure"))
  }
  
  // Test partial cleanup when some resources fail to clean up
  exception_handler.register_cleanup_action("failing_cleanup", fn() {
    cleanup_tracker.record_cleanup("failing")
    Exception::throw("Cleanup failed")
  })
  
  let partial_cleanup_result = exception_handler.execute_with_cleanup(fn() {
    // Acquire resource
    let resource = exception_handler.acquire_custom_resource("test_resource")
    
    // Simulate exception
    Exception::throw("Operation with failing cleanup")
  })
  
  match partial_cleanup_result {
    Ok(_) => assert_true(false),
    Err(error) => assert_true(error.contains("Operation with failing cleanup"))
  }
  
  // Check that cleanup was attempted even if some failed
  let partial_cleanup_actions = cleanup_tracker.get_executed_actions()
  assert_true(partial_cleanup_actions.contains("failing"))
  
  // Test cleanup timeout
  exception_handler.register_cleanup_action("slow_cleanup", fn() {
    cleanup_tracker.record_cleanup("slow")
    Thread::sleep(2000)  // 2 second cleanup
  })
  
  exception_handler.set_cleanup_timeout(1000)  // 1 second timeout
  
  let timeout_result = exception_handler.execute_with_cleanup(fn() {
    Exception::throw("Operation with slow cleanup")
  })
  
  match timeout_result {
    Ok(_) => assert_true(false),
    Err(error) => assert_true(error.contains("timeout"))
  }
  
  // Cleanup should have been attempted but timed out
  let timeout_cleanup_actions = cleanup_tracker.get_executed_actions()
  assert_true(timeout_cleanup_actions.contains("slow"))
}