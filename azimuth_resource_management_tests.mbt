// Azimuth Telemetry System - Resource Management Tests
// This file contains test cases for resource management functionality

// Test 1: Memory Resource Management
test "memory resource management" {
  let memory_manager = MemoryManager::new()
  
  // Test memory pool allocation
  let memory_pool = memory_manager.create_pool(1024 * 1024) // 1MB pool
  assert_eq(memory_pool.total_size, 1024 * 1024)
  assert_eq(memory_pool.used_size, 0)
  assert_eq(memory_pool.free_size, 1024 * 1024)
  
  // Test memory allocation from pool
  let block1 = memory_manager.allocate(memory_pool, 1024) // 1KB
  let block2 = memory_manager.allocate(memory_pool, 2048) // 2KB
  
  assert_eq(memory_pool.used_size, 3072)
  assert_eq(memory_pool.free_size, 1024 * 1024 - 3072)
  assert_eq(memory_pool.allocated_blocks, 2)
  
  // Test memory deallocation
  memory_manager.deallocate(memory_pool, block1)
  
  assert_eq(memory_pool.used_size, 2048)
  assert_eq(memory_pool.free_size, 1024 * 1024 - 2048)
  assert_eq(memory_pool.allocated_blocks, 1)
  
  // Test memory fragmentation detection
  let fragmentation_info = memory_manager.analyze_fragmentation(memory_pool)
  assert_true(fragmentation_info.fragmentation_ratio >= 0.0)
  assert_true(fragmentation_info.fragmentation_ratio <= 1.0)
  assert_true(fragmentation_info.largest_free_block > 0)
  
  // Test memory pool compaction
  if fragmentation_info.fragmentation_ratio > 0.5 {
    memory_manager.compact_pool(memory_pool)
    let compacted_info = memory_manager.analyze_fragmentation(memory_pool)
    assert_true(compacted_info.fragmentation_ratio < fragmentation_info.fragmentation_ratio)
  }
  
  // Test memory usage monitoring
  let usage_stats = memory_manager.get_usage_stats(memory_pool)
  assert_eq(usage_stats.peak_usage, 3072)
  assert_eq(usage_stats.current_usage, 2048)
  assert_true(usage_stats.allocation_count > 0)
  assert_true(usage_stats.deallocation_count > 0)
}

// Test 2: Connection Resource Management
test "connection resource management" {
  let connection_manager = ConnectionManager::new()
  
  // Test connection pool creation
  let pool_config = ConnectionPoolConfig::new()
    .with_max_connections(10)
    .with_min_connections(2)
    .with_connection_timeout(30000) // 30 seconds
    .with_idle_timeout(600000)      // 10 minutes
  
  let connection_pool = connection_manager.create_pool("database", pool_config)
  assert_eq(connection_pool.max_connections, 10)
  assert_eq(connection_pool.min_connections, 2)
  assert_eq(connection_pool.active_connections, 0)
  assert_eq(connection_pool.idle_connections, 0)
  
  // Test connection acquisition
  let conn1 = connection_manager.acquire_connection(connection_pool)
  let conn2 = connection_manager.acquire_connection(connection_pool)
  
  assert_eq(connection_pool.active_connections, 2)
  assert_eq(connection_pool.idle_connections, 0)
  assert_true(conn1.is_valid())
  assert_true(conn2.is_valid())
  
  // Test connection release
  connection_manager.release_connection(connection_pool, conn1)
  
  assert_eq(connection_pool.active_connections, 1)
  assert_eq(connection_pool.idle_connections, 1)
  
  // Test connection validation
  let invalid_conn = connection_manager.create_invalid_connection()
  assert_false(connection_manager.validate_connection(invalid_conn))
  
  // Test connection health check
  let health_status = connection_manager.check_pool_health(connection_pool)
  assert_true(health_status.is_healthy)
  assert_eq(health_status.active_connections, 1)
  assert_eq(health_status.idle_connections, 1)
  assert_eq(health_status.total_connections, 2)
  
  // Test connection pool scaling
  connection_manager.scale_pool(connection_pool, 5) // Scale to 5 connections
  assert_eq(connection_pool.max_connections, 5)
  
  // Test connection pool metrics
  let pool_metrics = connection_manager.get_pool_metrics(connection_pool)
  assert_true(pool_metrics.total_acquisitions >= 2)
  assert_true(pool_metrics.total_releases >= 1)
  assert_true(pool_metrics.peak_connections >= 2)
  assert_true(pool_metrics.avg_connection_lifetime > 0)
}

// Test 3: Thread Resource Management
test "thread resource management" {
  let thread_manager = ThreadManager::new()
  
  // Test thread pool creation
  let thread_pool = thread_manager.create_pool(4) // 4 worker threads
  assert_eq(thread_pool.worker_count, 4)
  assert_eq(thread_pool.active_tasks, 0)
  assert_eq(thread_pool.completed_tasks, 0)
  
  // Test task submission
  let task1 = Task::new("task1", fn() { "result1" })
  let task2 = Task::new("task2", fn() { "result2" })
  let task3 = Task::new("task3", fn() { "result3" })
  
  let future1 = thread_manager.submit_task(thread_pool, task1)
  let future2 = thread_manager.submit_task(thread_pool, task2)
  let future3 = thread_manager.submit_task(thread_pool, task3)
  
  // Test task execution
  let result1 = thread_manager.get_result(future1)
  let result2 = thread_manager.get_result(future2)
  let result3 = thread_manager.get_result(future3)
  
  assert_eq(result1, "result1")
  assert_eq(result2, "result2")
  assert_eq(result3, "result3")
  
  // Test thread pool statistics
  let pool_stats = thread_manager.get_pool_stats(thread_pool)
  assert_eq(pool_stats.completed_tasks, 3)
  assert_true(pool_stats.avg_task_time > 0)
  assert_true(pool_stats.thread_utilization > 0.0)
  assert_true(pool_stats.thread_utilization <= 1.0)
  
  // Test thread pool scaling
  thread_manager.scale_pool(thread_pool, 8) // Scale to 8 threads
  assert_eq(thread_pool.worker_count, 8)
  
  // Test thread pool shutdown
  thread_manager.shutdown_pool(thread_pool)
  assert_true(thread_pool.is_shutdown)
  
  // Test thread resource cleanup
  let cleanup_result = thread_manager.cleanup_resources()
  assert_true(cleanup_result.success)
  assert_eq(cleanup_result.cleaned_pools, 1)
  assert_eq(cleanup_result.freed_threads, 8)
}

// Test 4: File Resource Management
test "file resource management" {
  let file_manager = FileManager::new()
  
  // Test file handle pool creation
  let file_pool = file_manager.create_handle_pool(100) // 100 file handles
  assert_eq(file_pool.max_handles, 100)
  assert_eq(file_pool.active_handles, 0)
  assert_eq(file_pool.idle_handles, 0)
  
  // Test file handle acquisition
  let temp_file = "/tmp/test_file.txt"
  let file_content = "Test file content for resource management"
  
  let handle1 = file_manager.open_file(temp_file, "w")
  let handle2 = file_manager.open_file(temp_file, "r")
  
  assert_eq(file_pool.active_handles, 2)
  assert_true(file_manager.is_handle_valid(handle1))
  assert_true(file_manager.is_handle_valid(handle2))
  
  // Test file operations
  let write_result = file_manager.write_file(handle1, file_content)
  assert_true(write_result.success)
  assert_eq(write_result.bytes_written, file_content.length())
  
  let read_result = file_manager.read_file(handle2, file_content.length())
  assert_true(read_result.success)
  assert_eq(read_result.content, file_content)
  
  // Test file handle release
  file_manager.close_file(handle1)
  file_manager.close_file(handle2)
  
  assert_eq(file_pool.active_handles, 0)
  assert_eq(file_pool.idle_handles, 2)
  
  // Test file resource monitoring
  let resource_monitor = file_manager.get_resource_monitor()
  let file_stats = resource_monitor.get_file_statistics()
  
  assert_true(file_stats.total_files_opened >= 2)
  assert_true(file_stats.total_files_closed >= 2)
  assert_true(file_stats.total_bytes_written >= file_content.length())
  assert_true(file_stats.total_bytes_read >= file_content.length())
  
  // Test file cleanup
  let cleanup_result = file_manager.cleanup_temp_files()
  assert_true(cleanup_result.success)
  assert_true(cleanup_result.files_deleted >= 1)
}

// Test 5: Cache Resource Management
test "cache resource management" {
  let cache_manager = CacheManager::new()
  
  // Test cache creation with resource limits
  let cache_config = CacheConfig::new()
    .with_max_memory(10 * 1024 * 1024) // 10MB
    .with_max_entries(10000)
    .with_ttl(3600000) // 1 hour
    .with_cleanup_interval(600000) // 10 minutes
  
  let cache = cache_manager.create_cache("test_cache", cache_config)
  assert_eq(cache.max_memory, 10 * 1024 * 1024)
  assert_eq(cache.max_entries, 10000)
  assert_eq(cache.current_entries, 0)
  assert_eq(cache.current_memory, 0)
  
  // Test cache entry insertion
  let large_value = "x".repeat(1024) // 1KB value
  for i in 0..=100 {
    let key = "key_" + i.to_string()
    cache_manager.put(cache, key, large_value)
  }
  
  assert_eq(cache.current_entries, 101)
  assert_eq(cache.current_memory, 101 * 1024)
  
  // Test cache eviction when memory limit reached
  for i in 0..=10000 {
    let key = "large_key_" + i.to_string()
    let value = "x".repeat(1024) // 1KB value
    cache_manager.put(cache, key, value)
  }
  
  assert_true(cache.current_entries <= cache.max_entries)
  assert_true(cache.current_memory <= cache.max_memory)
  
  // Test cache resource monitoring
  let cache_monitor = cache_manager.get_resource_monitor()
  let cache_stats = cache_monitor.get_cache_statistics(cache)
  
  assert_true(cache_stats.put_operations > 0)
  assert_true(cache_stats.get_operations >= 0)
  assert_true(cache_stats.evictions > 0)
  assert_true(cache_stats.hit_ratio >= 0.0)
  assert_true(cache_stats.hit_ratio <= 1.0)
  
  // Test cache cleanup
  let cleanup_result = cache_manager.cleanup_expired_entries(cache)
  assert_true(cleanup_result.success)
  assert_true(cleanup_result.entries_cleaned >= 0)
}

// Test 6: Resource Pool Management
test "resource pool management" {
  let resource_pool_manager = ResourcePoolManager::new()
  
  // Test generic resource pool creation
  let pool_config = ResourcePoolConfig::new()
    .with_max_size(50)
    .with_min_size(5)
    .with_acquire_timeout(5000) // 5 seconds
    .with_idle_timeout(300000)  // 5 minutes
  
  let resource_pool = resource_pool_manager.create_pool("generic_resources", pool_config)
  assert_eq(resource_pool.max_size, 50)
  assert_eq(resource_pool.min_size, 5)
  assert_eq(resource_pool.current_size, 0)
  assert_eq(resource_pool.available_resources, 0)
  
  // Test resource creation and acquisition
  let resource_factory = fn() -> Resource {
    Resource::new("test_resource", Random::generate_id())
  }
  
  let resource1 = resource_pool_manager.acquire(resource_pool, resource_factory)
  let resource2 = resource_pool_manager.acquire(resource_pool, resource_factory)
  let resource3 = resource_pool_manager.acquire(resource_pool, resource_factory)
  
  assert_eq(resource_pool.current_size, 3)
  assert_eq(resource_pool.available_resources, 0)
  assert_true(resource1.is_valid())
  assert_true(resource2.is_valid())
  assert_true(resource3.is_valid())
  
  // Test resource release
  resource_pool_manager.release(resource_pool, resource1)
  
  assert_eq(resource_pool.current_size, 3)
  assert_eq(resource_pool.available_resources, 1)
  
  // Test resource validation
  let validator = fn(resource: Resource) -> Bool {
    resource.type == "test_resource" && resource.id.length() > 0
  }
  
  let is_valid = resource_pool_manager.validate_resource(resource2, validator)
  assert_true(is_valid)
  
  // Test resource pool metrics
  let pool_metrics = resource_pool_manager.get_pool_metrics(resource_pool)
  assert_true(pool_metrics.total_acquisitions >= 3)
  assert_true(pool_metrics.total_releases >= 1)
  assert_true(pool_metrics.peak_size >= 3)
  assert_true(pool_metrics.avg_wait_time >= 0)
  
  // Test resource pool scaling
  resource_pool_manager.scale_pool(resource_pool, 20)
  assert_eq(resource_pool.max_size, 20)
  
  // Test resource pool cleanup
  let cleanup_result = resource_pool_manager.cleanup_pool(resource_pool)
  assert_true(cleanup_result.success)
  assert_true(cleanup_result.resources_cleaned >= 0)
}

// Test 7: Resource Leak Detection
test "resource leak detection" {
  let leak_detector = ResourceLeakDetector::new()
  
  // Test memory leak detection
  leak_detector.start_monitoring("memory")
  
  // Simulate potential memory leak
  let mut leaky_data = []
  for i in 0..=1000 {
    leaky_data = leaky_data.push([i, i+1, i+2, i+3, i+4])
  }
  
  let memory_leak_report = leak_detector.stop_monitoring("memory")
  assert_true(memory_leak_report.resources_allocated > 0)
  assert_true(memory_leak_report.resources_released >= 0)
  assert_true(memory_leak_report.potential_leaks >= 0)
  
  // Test connection leak detection
  leak_detector.start_monitoring("connections")
  
  // Simulate connection acquisition without release
  let connection_manager = ConnectionManager::new()
  let pool_config = ConnectionPoolConfig::new().with_max_connections(10)
  let connection_pool = connection_manager.create_pool("test_db", pool_config)
  
  let conn1 = connection_manager.acquire_connection(connection_pool)
  let conn2 = connection_manager.acquire_connection(connection_pool)
  // Don't release connections to simulate leak
  
  let connection_leak_report = leak_detector.stop_monitoring("connections")
  assert_true(connection_leak_report.resources_allocated >= 2)
  assert_true(connection_leak_report.resources_released >= 0)
  assert_true(connection_leak_report.potential_leaks >= 2)
  
  // Test file handle leak detection
  leak_detector.start_monitoring("file_handles")
  
  // Simulate file handle acquisition without release
  let file_manager = FileManager::new()
  let handle1 = file_manager.open_file("/tmp/test1.txt", "w")
  let handle2 = file_manager.open_file("/tmp/test2.txt", "w")
  // Don't close handles to simulate leak
  
  let file_leak_report = leak_detector.stop_monitoring("file_handles")
  assert_true(file_leak_report.resources_allocated >= 2)
  assert_true(file_leak_report.resources_released >= 0)
  assert_true(file_leak_report.potential_leaks >= 2)
  
  // Test comprehensive leak report
  let comprehensive_report = leak_detector.generate_comprehensive_report()
  assert_true(comprehensive_report.total_resources_tracked > 0)
  assert_true(comprehensive_report.total_potential_leaks > 0)
  assert_true(comprehensive_report.resource_types.length() >= 3)
  
  // Test leak prevention recommendations
  let recommendations = leak_detector.get_leak_prevention_recommendations()
  assert_true(recommendations.length() > 0)
  
  let memory_rec = recommendations.find(fn(r) { r.resource_type == "memory" })
  match memory_rec {
    Some(rec) => {
      assert_true(rec.recommendation.contains("memory"))
      assert_true(rec.severity >= Low)
    }
    None => assert_true(false)
  }
}

// Test 8: Resource Quota Management
test "resource quota management" {
  let quota_manager = ResourceQuotaManager::new()
  
  // Test quota definition
  let memory_quota = ResourceQuota::new("memory", 100 * 1024 * 1024) // 100MB
  let connection_quota = ResourceQuota::new("connections", 50)
  let file_handle_quota = ResourceQuota::new("file_handles", 200)
  
  quota_manager.set_quota("memory", memory_quota)
  quota_manager.set_quota("connections", connection_quota)
  quota_manager.set_quota("file_handles", file_handle_quota)
  
  // Test quota checking
  let memory_check = quota_manager.check_quota("memory", 50 * 1024 * 1024) // 50MB
  assert_true(memory_check.allowed)
  assert_eq(memory_check.remaining, 50 * 1024 * 1024)
  
  let memory_exceeded = quota_manager.check_quota("memory", 150 * 1024 * 1024) // 150MB
  assert_false(memory_exceeded.allowed)
  assert_eq(memory_exceeded.remaining, 100 * 1024 * 1024)
  
  // Test quota reservation
  let reservation1 = quota_manager.reserve("connections", 10)
  assert_true(reservation1.success)
  assert_eq(reservation1.reserved_amount, 10)
  
  let reservation2 = quota_manager.reserve("connections", 45)
  assert_false(reservation2.success)
  assert_eq(reservation2.reserved_amount, 0)
  
  // Test quota release
  let release_result = quota_manager.release("connections", reservation1.reservation_id, 5)
  assert_true(release_result.success)
  
  // Test quota monitoring
  let quota_monitor = quota_manager.get_quota_monitor()
  let quota_stats = quota_monitor.get_quota_statistics()
  
  assert_true(quota_stats["memory"].usage > 0)
  assert_true(quota_stats["memory"].utilization >= 0.0)
  assert_true(quota_stats["memory"].utilization <= 1.0)
  
  assert_true(quota_stats["connections"].usage > 0)
  assert_true(quota_stats["connections"].utilization >= 0.0)
  assert_true(quota_stats["connections"].utilization <= 1.0)
  
  // Test quota alerts
  let alert_manager = quota_manager.get_alert_manager()
  alert_manager.set_threshold("memory", 0.8) // 80% threshold
  
  // Simulate high memory usage
  quota_manager.reserve("memory", 85 * 1024 * 1024) // 85MB
  
  let alerts = alert_manager.check_thresholds()
  assert_true(alerts.length() > 0)
  
  let memory_alert = alerts.find(fn(a) { a.resource_type == "memory" })
  match memory_alert {
    Some(alert) => {
      assert_true(alert.threshold_exceeded)
      assert_true(alert.current_utilization > 0.8)
    }
    None => assert_true(false)
  }
  
  // Test quota adjustment
  let adjustment_result = quota_manager.adjust_quota("memory", 200 * 1024 * 1024) // Increase to 200MB
  assert_true(adjustment_result.success)
  assert_eq(quota_manager.get_quota("memory").limit, 200 * 1024 * 1024)
}