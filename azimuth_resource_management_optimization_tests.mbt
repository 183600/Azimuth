// Azimuth Resource Management Optimization Tests
// This file contains comprehensive tests for resource management and optimization

// Test 1: Memory Pool Management
test "memory pool management and optimization" {
  // Create memory pool with different block sizes
  let memory_pool = MemoryPool::new([
    ("small", 64, 100),   // 64-byte blocks, 100 count
    ("medium", 256, 50),  // 256-byte blocks, 50 count
    ("large", 1024, 20)   // 1024-byte blocks, 20 count
  ])
  
  // Test memory allocation from pool
  let small_block1 = MemoryPool::allocate(memory_pool, "small")
  let small_block2 = MemoryPool::allocate(memory_pool, "small")
  let medium_block1 = MemoryPool::allocate(memory_pool, "medium")
  let large_block1 = MemoryPool::allocate(memory_pool, "large")
  
  assert_true(MemoryPool::is_allocated(small_block1))
  assert_true(MemoryPool::is_allocated(small_block2))
  assert_true(MemoryPool::is_allocated(medium_block1))
  assert_true(MemoryPool::is_allocated(large_block1))
  
  // Test pool statistics
  let small_stats = MemoryPool::get_stats(memory_pool, "small")
  assert_eq(small_stats.allocated_count, 2)
  assert_eq(small_stats.available_count, 98)
  
  let medium_stats = MemoryPool::get_stats(memory_pool, "medium")
  assert_eq(medium_stats.allocated_count, 1)
  assert_eq(medium_stats.available_count, 49)
  
  let large_stats = MemoryPool::get_stats(memory_pool, "large")
  assert_eq(large_stats.allocated_count, 1)
  assert_eq(large_stats.available_count, 19)
  
  // Test memory deallocation
  MemoryPool::deallocate(memory_pool, small_block1)
  MemoryPool::deallocate(memory_pool, medium_block1)
  
  let updated_small_stats = MemoryPool::get_stats(memory_pool, "small")
  assert_eq(updated_small_stats.allocated_count, 1)
  assert_eq(updated_small_stats.available_count, 99)
  
  let updated_medium_stats = MemoryPool::get_stats(memory_pool, "medium")
  assert_eq(updated_medium_stats.allocated_count, 0)
  assert_eq(updated_medium_stats.available_count, 50)
  
  // Test pool exhaustion
  let allocated_blocks = []
  
  // Exhaust all small blocks
  for i in 0..98 {
    let block = MemoryPool::allocate(memory_pool, "small")
    allocated_blocks.push(block)
  }
  
  // Next allocation should fail
  let exhausted_block = MemoryPool::allocate(memory_pool, "small")
  assert_true(MemoryPool::is_null(exhausted_block))
  
  // Test pool auto-expansion
  let expandable_pool = MemoryPool::new_with_expansion([
    ("dynamic", 128, 10, 5) // 128-byte blocks, 10 initial, 5 expansion
  ])
  
  let dynamic_blocks = []
  
  // Allocate beyond initial capacity
  for i in 0..15 {
    let block = MemoryPool::allocate(expandable_pool, "dynamic")
    assert_true(MemoryPool::is_allocated(block))
    dynamic_blocks.push(block)
  }
  
  let dynamic_stats = MemoryPool::get_stats(expandable_pool, "dynamic")
  assert_eq(dynamic_stats.total_count, 15) // Should have expanded
  assert_eq(dynamic_stats.allocated_count, 15)
  
  // Clean up
  for block in allocated_blocks {
    MemoryPool::deallocate(memory_pool, block)
  }
  
  for block in dynamic_blocks {
    MemoryPool::deallocate(expandable_pool, block)
  }
}

// Test 2: Object Pool Management
test "object pool management and reuse" {
  // Create object pool for expensive objects
  let object_pool = ObjectPool::new(func() { ExpensiveObject::new() }, 10)
  
  // Test object acquisition
  let obj1 = ObjectPool::acquire(object_pool)
  let obj2 = ObjectPool::acquire(object_pool)
  let obj3 = ObjectPool::acquire(object_pool)
  
  assert_true(ExpensiveObject::is_initialized(obj1))
  assert_true(ExpensiveObject::is_initialized(obj2))
  assert_true(ExpensiveObject::is_initialized(obj3))
  
  // Test object state reset on return
  ExpensiveObject::set_state(obj1, "modified")
  assert_eq(ExpensiveObject::get_state(obj1), "modified")
  
  ObjectPool::release(object_pool, obj1)
  
  // Reacquire object (should be the same instance)
  let obj1_reacquired = ObjectPool::acquire(object_pool)
  assert_eq(ExpensiveObject::get_state(obj1_reacquired), "initial") // Should be reset
  
  // Test pool statistics
  let pool_stats = ObjectPool::get_stats(object_pool)
  assert_eq(pool_stats.active_count, 2) // obj2 and obj3 are still active
  assert_eq(pool_stats.available_count, 8) // 1 returned, 9 still available
  assert_eq(pool_stats.total_count, 10)
  assert_eq(pool_stats.acquisition_count, 4) // 3 initial + 1 reacquisition
  assert_eq(pool_stats.return_count, 1)
  
  // Test pool exhaustion
  let acquired_objects = []
  
  // Acquire all remaining objects
  for i in 0..8 {
    let obj = ObjectPool::acquire(object_pool)
    acquired_objects.push(obj)
  }
  
  // Next acquisition should fail
  let exhausted_obj = ObjectPool::acquire(object_pool)
  assert_true(ObjectPool::is_null(exhausted_obj))
  
  // Test pool with factory function
  let factory_pool = ObjectPool::with_factory(
    func(id) { FactoryObject::new(id) },
    func(obj) { FactoryObject::reset(obj) },
    5
  )
  
  let factory_obj1 = ObjectPool::acquire_with_param(factory_pool, "test_id_1")
  let factory_obj2 = ObjectPool::acquire_with_param(factory_pool, "test_id_2")
  
  assert_eq(FactoryObject::get_id(factory_obj1), "test_id_1")
  assert_eq(FactoryObject::get_id(factory_obj2), "test_id_2")
  
  // Clean up
  ObjectPool::release(object_pool, obj2)
  ObjectPool::release(object_pool, obj3)
  
  for obj in acquired_objects {
    ObjectPool::release(object_pool, obj)
  }
  
  ObjectPool::release(factory_pool, factory_obj1)
  ObjectPool::release(factory_pool, factory_obj2)
}

// Test 3: Connection Pool Management
test "connection pool management and optimization" {
  // Create connection pool
  let connection_pool = ConnectionPool::new(
    func() { DatabaseConnection::new() },
    func(conn) { DatabaseConnection::is_valid(conn) },
    func(conn) { DatabaseConnection::reset(conn) },
    10 // Max connections
  )
  
  // Test connection acquisition
  let conn1 = ConnectionPool::acquire(connection_pool)
  let conn2 = ConnectionPool::acquire(connection_pool)
  let conn3 = ConnectionPool::acquire(connection_pool)
  
  assert_true(DatabaseConnection::is_connected(conn1))
  assert_true(DatabaseConnection::is_connected(conn2))
  assert_true(DatabaseConnection::is_connected(conn3))
  
  // Test connection statistics
  let pool_stats = ConnectionPool::get_stats(connection_pool)
  assert_eq(pool_stats.active_count, 3)
  assert_eq(pool_stats.available_count, 7)
  assert_eq(pool_stats.total_count, 3) // Created on demand
  
  // Test connection return
  ConnectionPool::release(connection_pool, conn1)
  
  let updated_stats = ConnectionPool::get_stats(connection_pool)
  assert_eq(updated_stats.active_count, 2)
  assert_eq(updated_stats.available_count, 8)
  
  // Test connection reuse
  let conn1_reused = ConnectionPool::acquire(connection_pool)
  assert_true(DatabaseConnection::is_connected(conn1_reused))
  
  // Test connection validation
  DatabaseConnection::invalidate(conn2) // Simulate connection failure
  
  ConnectionPool::release(connection_pool, conn2) // Invalid connection should not be returned to pool
  
  let conn4 = ConnectionPool::acquire(connection_pool)
  assert_true(DatabaseConnection::is_connected(conn4)) // Should be a new valid connection
  
  // Test connection timeout
  let timeout_pool = ConnectionPool::with_timeout(
    func() { DatabaseConnection::new() },
    func(conn) { DatabaseConnection::is_valid(conn) },
    func(conn) { DatabaseConnection::reset(conn) },
    5, // Max connections
    1000 // 1 second timeout
  )
  
  // Acquire all connections
  let timeout_conns = []
  for i in 0..5 {
    let conn = ConnectionPool::acquire(timeout_pool)
    timeout_conns.push(conn)
  }
  
  // Next acquisition should timeout
  let start_time = Time::now()
  let timeout_conn = ConnectionPool::acquire(timeout_pool)
  let end_time = Time::now()
  
  assert_true(ConnectionPool::is_null(timeout_conn))
  assert_true(end_time - start_time >= 1000L) // Should wait at least 1 second
  
  // Clean up
  ConnectionPool::release(connection_pool, conn3)
  ConnectionPool::release(connection_pool, conn1_reused)
  ConnectionPool::release(connection_pool, conn4)
  
  for conn in timeout_conns {
    ConnectionPool::release(timeout_pool, conn)
  }
}

// Test 4: Cache Management and Optimization
test "cache management and optimization" {
  // Create LRU cache
  let lru_cache = LRUCache::new(5) // Max 5 items
  
  // Test cache insertion
  LRUCache::put(lru_cache, "key1", "value1")
  LRUCache::put(lru_cache, "key2", "value2")
  LRUCache::put(lru_cache, "key3", "value3")
  
  assert_eq(LRUCache::get(lru_cache, "key1"), Some("value1"))
  assert_eq(LRUCache::get(lru_cache, "key2"), Some("value2"))
  assert_eq(LRUCache::get(lru_cache, "key3"), Some("value3"))
  
  // Test cache eviction
  LRUCache::put(lru_cache, "key4", "value4")
  LRUCache::put(lru_cache, "key5", "value5")
  
  assert_eq(LRUCache::size(lru_cache), 5)
  
  // Access key1 to make it recently used
  LRUCache::get(lru_cache, "key1")
  
  // Add new item, should evict key2 (least recently used)
  LRUCache::put(lru_cache, "key6", "value6")
  
  assert_eq(LRUCache::get(lru_cache, "key1"), Some("value1")) // Should still exist
  assert_eq(LRUCache::get(lru_cache, "key2"), None) // Should be evicted
  assert_eq(LRUCache::get(lru_cache, "key3"), Some("value3"))
  
  // Test TTL cache
  let ttl_cache = TTLCache::new(1000) // 1 second TTL
  
  TTLCache::put(ttl_cache, "temp_key", "temp_value")
  assert_eq(TTLCache::get(ttl_cache, "temp_key"), Some("temp_value"))
  
  // Wait for expiration
  Thread::sleep(1100) // 1.1 seconds
  
  assert_eq(TTLCache::get(ttl_cache, "temp_key"), None) // Should be expired
  
  // Test cache statistics
  let cache_stats = LRUCache::get_stats(lru_cache)
  assert_eq(cache_stats.hits, 4) // key1, key2, key3, key1
  assert_eq(cache_stats.misses, 1) // key2 after eviction
  assert_eq(cache_stats.evictions, 1)
  
  // Test cache with custom loader
  let loading_cache = LoadingCache::new(3, func(key) {
    "loaded_" + key
  })
  
  // First access should load value
  let loaded1 = LoadingCache::get_or_load(loading_cache, "load_key1")
  assert_eq(loaded1, "loaded_load_key1")
  
  // Second access should use cached value
  let loaded2 = LoadingCache::get_or_load(loading_cache, "load_key1")
  assert_eq(loaded2, "loaded_load_key1")
  
  let loading_stats = LoadingCache::get_stats(loading_cache)
  assert_eq(loading_stats.loads, 1)
  assert_eq(loading_stats.hits, 1)
}

// Test 5: Resource Leak Detection
test "resource leak detection and prevention" {
  // Create resource leak detector
  let leak_detector = LeakDetector::new()
  
  // Test memory leak detection
  LeakDetector::track_allocation(leak_detector, "test_buffer", 1024)
  
  // Simulate memory leak (not freeing)
  LeakDetector::track_allocation(leak_detector, "leaked_buffer", 2048)
  
  // Test file handle leak detection
  LeakDetector::track_file_open(leak_detector, "test_file.txt")
  
  // Simulate file handle leak
  LeakDetector::track_file_open(leak_detector, "leaked_file.txt")
  
  // Test network connection leak detection
  LeakDetector::track_connection_open(leak_detector, "example.com:80")
  
  // Simulate connection leak
  LeakDetector::track_connection_open(leak_detector, "leaked.example.com:80")
  
  // Check for leaks
  let leak_report = LeakDetector::generate_report(leak_detector)
  
  assert_eq(leak_report.memory_leaks, 1)
  assert_eq(leak_report.file_handle_leaks, 1)
  assert_eq(leak_report.connection_leaks, 1)
  assert_true(leak_report.total_leaked_memory > 0)
  
  // Fix some leaks
  LeakDetector::track_deallocation(leak_detector, "test_buffer")
  LeakDetector::track_file_close(leak_detector, "test_file.txt")
  LeakDetector::track_connection_close(leak_detector, "example.com:80")
  
  // Check updated report
  let updated_report = LeakDetector::generate_report(leak_detector)
  
  assert_eq(updated_report.memory_leaks, 1) // Still 1 (leaked_buffer)
  assert_eq(updated_report.file_handle_leaks, 1) // Still 1 (leaked_file.txt)
  assert_eq(updated_report.connection_leaks, 1) // Still 1 (leaked.example.com:80)
  
  // Test automatic cleanup
  let auto_cleanup_detector = LeakDetector::with_auto_cleanup(100) // 100ms cleanup interval
  
  LeakDetector::track_allocation(auto_cleanup_detector, "auto_cleanup_buffer", 512)
  
  // Wait for cleanup
  Thread::sleep(150)
  
  let cleanup_report = LeakDetector::generate_report(auto_cleanup_detector)
  assert_eq(cleanup_report.memory_leaks, 0) // Should be auto-cleaned
}

// Test 6: Resource Usage Monitoring
test "resource usage monitoring and alerting" {
  // Create resource monitor
  let resource_monitor = ResourceMonitor::new()
  
  // Test CPU monitoring
  let cpu_usage = ResourceMonitor::get_cpu_usage(resource_monitor)
  assert_true(cpu_usage >= 0.0 && cpu_usage <= 100.0)
  
  // Test memory monitoring
  let memory_usage = ResourceMonitor::get_memory_usage(resource_monitor)
  assert_true(memory_usage.used > 0)
  assert_true(memory_usage.total > 0)
  assert_true(memory_usage.used <= memory_usage.total)
  
  let memory_percent = (memory_usage.used.to_float() / memory_usage.total.to_float()) * 100.0
  assert_true(memory_percent >= 0.0 && memory_percent <= 100.0)
  
  // Test disk I/O monitoring
  let disk_io = ResourceMonitor::get_disk_io(resource_monitor)
  assert_true(disk_io.read_bytes >= 0)
  assert_true(disk_io.write_bytes >= 0)
  
  // Test network I/O monitoring
  let network_io = ResourceMonitor::get_network_io(resource_monitor)
  assert_true(network_io.bytes_sent >= 0)
  assert_true(network_io.bytes_received >= 0)
  
  // Test resource thresholds and alerts
  ResourceMonitor::set_cpu_threshold(resource_monitor, 80.0) // Alert at 80% CPU
  ResourceMonitor::set_memory_threshold(resource_monitor, 90.0) // Alert at 90% memory
  ResourceMonitor::set_disk_threshold(resource_monitor, 95.0) // Alert at 95% disk
  
  // Simulate high resource usage
  let alert_handler = AlertHandler::new()
  ResourceMonitor::set_alert_handler(resource_monitor, alert_handler)
  
  // Generate some load
  let start_time = Time::now()
  let result = 0
  
  // CPU-intensive operation
  for i in 0..1000000 {
    result = result + i * i
  }
  
  let end_time = Time::now()
  let execution_time = end_time - start_time
  
  // Check if any alerts were triggered
  let alerts = AlertHandler::get_alerts(alert_handler)
  assert_true(alerts.length() >= 0)
  
  // Test historical monitoring
  ResourceMonitor::start_historical_monitoring(resource_monitor, 100) // 100ms interval
  
  // Wait for some historical data
  Thread::sleep(350) // 3.5 intervals
  
  let historical_data = ResourceMonitor::get_historical_data(resource_monitor)
  assert_true(historical_data.length() >= 3)
  
  // Test resource usage prediction
  let prediction = ResourceMonitor::predict_usage(resource_monitor, 60000) // Predict 1 minute ahead
  assert_true(prediction.cpu_usage >= 0.0)
  assert_true(prediction.memory_usage >= 0.0)
}

// Test 7: Resource Pool Balancing
test "resource pool balancing and distribution" {
  // Create multiple resource pools
  let pool1 = ResourcePool::new("pool1", 10)
  let pool2 = ResourcePool::new("pool2", 10)
  let pool3 = ResourcePool::new("pool3", 10)
  
  // Create pool balancer
  let pool_balancer = PoolBalancer::new([pool1, pool2, pool3])
  
  // Test round-robin balancing
  PoolBalancer::set_strategy(pool_balancer, BalancingStrategy::RoundRobin)
  
  let resource1 = PoolBalancer::acquire(pool_balancer)
  let resource2 = PoolBalancer::acquire(pool_balancer)
  let resource3 = PoolBalancer::acquire(pool_balancer)
  
  assert_eq(Resource::get_pool_name(resource1), "pool1")
  assert_eq(Resource::get_pool_name(resource2), "pool2")
  assert_eq(Resource::get_pool_name(resource3), "pool3")
  
  // Test least-loaded balancing
  PoolBalancer::set_strategy(pool_balancer, BalancingStrategy::LeastLoaded)
  
  // Release resource from pool1 to make it least loaded
  PoolBalancer::release(pool_balancer, resource1)
  
  let resource4 = PoolBalancer::acquire(pool_balancer)
  assert_eq(Resource::get_pool_name(resource4), "pool1") // Should go to least loaded pool
  
  // Test weighted balancing
  PoolBalancer::set_weights(pool_balancer, [
    ("pool1", 1),
    ("pool2", 2),
    ("pool3", 3)
  ])
  
  PoolBalancer::set_strategy(pool_balancer, BalancingStrategy::Weighted)
  
  let weighted_resources = []
  for i in 0..60 {
    let resource = PoolBalancer::acquire(pool_balancer)
    weighted_resources.push(resource)
  }
  
  // Count resources from each pool
  let pool1_count = 0
  let pool2_count = 0
  let pool3_count = 0
  
  for resource in weighted_resources {
    match Resource::get_pool_name(resource) {
      "pool1" => pool1_count = pool1_count + 1
      "pool2" => pool2_count = pool2_count + 1
      "pool3" => pool3_count = pool3_count + 1
      _ => {}
    }
  }
  
  // Pool3 should have the most resources (highest weight)
  assert_true(pool3_count > pool2_count)
  assert_true(pool2_count > pool1_count)
  
  // Test pool health monitoring
  let pool_health = PoolBalancer::get_pool_health(pool_balancer)
  assert_eq(pool_health.length(), 3)
  
  for health in pool_health {
    assert_true(health.utilization >= 0.0 && health.utilization <= 1.0)
    assert_true(health.response_time >= 0)
  }
  
  // Test automatic pool rebalancing
  PoolBalancer::enable_auto_rebalancing(pool_balancer, 5000) // 5 second interval
  
  // Wait for rebalancing
  Thread::sleep(5500)
  
  let rebalancing_stats = PoolBalancer::get_rebalancing_stats(pool_balancer)
  assert_true(rebalancing_stats.rebalancing_operations >= 0)
  
  // Clean up
  PoolBalancer::release(pool_balancer, resource2)
  PoolBalancer::release(pool_balancer, resource3)
  PoolBalancer::release(pool_balancer, resource4)
  
  for resource in weighted_resources {
    PoolBalancer::release(pool_balancer, resource)
  }
}

// Test 8: Resource Optimization Strategies
test "resource optimization strategies" {
  // Create resource optimizer
  let resource_optimizer = ResourceOptimizer::new()
  
  // Test memory optimization
  let memory_optimizer = MemoryOptimizer::new()
  
  // Create memory-intensive data structure
  let large_data = create_large_data_structure(10000)
  let initial_memory = MemoryProfiler::get_current_usage()
  
  // Optimize memory usage
  let optimized_data = MemoryOptimizer::optimize(memory_optimizer, large_data)
  let optimized_memory = MemoryProfiler::get_current_usage()
  
  assert_true(optimized_memory < initial_memory)
  
  // Test garbage collection optimization
  let gc_optimizer = GCOptimizer::new()
  
  // Create many temporary objects
  let temporary_objects = []
  for i in 0..1000 {
    let obj = TemporaryObject::new(i)
    temporary_objects.push(obj)
  }
  
  // Clear references
  temporary_objects.clear()
  
  // Optimize garbage collection
  let gc_stats_before = GCOptimizer::get_stats(gc_optimizer)
  GCOptimizer::optimize(gc_optimizer)
  let gc_stats_after = GCOptimizer::get_stats(gc_optimizer)
  
  assert_true(gc_stats_after.collections > gc_stats_before.collections)
  assert_true(gc_stats_after.memory_freed > 0)
  
  // Test I/O optimization
  let io_optimizer = IOOptimizer::new()
  
  // Create many small I/O operations
  let io_results = []
  for i in 0..100 {
    let result = IOOptimizer::read_small_file(io_optimizer, "file_" + i.to_string() + ".txt")
    io_results.push(result)
  }
  
  let io_time_before = IOOptimizer::get_total_time(io_optimizer)
  
  // Optimize I/O operations
  let optimized_results = IOOptimizer::batch_read_files(io_optimizer, [
    "file_0.txt", "file_1.txt", "file_2.txt", "file_3.txt", "file_4.txt",
    "file_5.txt", "file_6.txt", "file_7.txt", "file_8.txt", "file_9.txt"
  ])
  
  let io_time_after = IOOptimizer::get_total_time(io_optimizer)
  
  assert_true(optimized_results.length() == 10)
  assert_true(io_time_after > io_time_before)
  
  // Test CPU optimization
  let cpu_optimizer = CPUOptimizer::new()
  
  // Create CPU-intensive task
  let cpu_task = func() {
    let result = 0
    for i in 0..1000000 {
      result = result + i * i
    }
    result
  }
  
  let cpu_time_before = CPUOptimizer::get_execution_time(cpu_optimizer, cpu_task)
  
  // Optimize CPU usage
  let optimized_cpu_task = CPUOptimizer::optimize_task(cpu_optimizer, cpu_task)
  let cpu_time_after = CPUOptimizer::get_execution_time(cpu_optimizer, optimized_cpu_task)
  
  assert_true(cpu_time_after <= cpu_time_before)
  
  // Test network optimization
  let network_optimizer = NetworkOptimizer::new()
  
  // Create many small network requests
  let network_results = []
  for i in 0..50 {
    let result = NetworkOptimizer::make_small_request(network_optimizer, "endpoint_" + i.to_string())
    network_results.push(result)
  }
  
  let network_time_before = NetworkOptimizer::get_total_time(network_optimizer)
  
  // Optimize network requests
  let optimized_network_results = NetworkOptimizer::batch_requests(network_optimizer, [
    "endpoint_0", "endpoint_1", "endpoint_2", "endpoint_3", "endpoint_4",
    "endpoint_5", "endpoint_6", "endpoint_7", "endpoint_8", "endpoint_9"
  ])
  
  let network_time_after = NetworkOptimizer::get_total_time(network_optimizer)
  
  assert_true(optimized_network_results.length() == 10)
  assert_true(network_time_after > network_time_before)
  
  // Test comprehensive resource optimization
  let optimization_plan = ResourceOptimizer::create_optimization_plan(resource_optimizer)
  let optimization_result = ResourceOptimizer::execute_plan(resource_optimizer, optimization_plan)
  
  assert_true(optimization_result.memory_saved > 0)
  assert_true(optimization_result.cpu_time_saved > 0)
  assert_true(optimization_result.io_time_saved > 0)
  assert_true(optimization_result.network_time_saved > 0)
}

// Helper function to create large data structure
func create_large_data_structure(size : Int) -> LargeDataStructure {
  let data = LargeDataStructure::new()
  
  for i in 0..size {
    let item = DataItem::new(
      "item_" + i.to_string(),
      i.to_float(),
      "description_" + i.to_string()
    )
    LargeDataStructure::add_item(data, item)
  }
  
  data
}