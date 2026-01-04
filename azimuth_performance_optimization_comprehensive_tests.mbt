// Azimuth Telemetry System - Performance Optimization Comprehensive Tests
// This file contains comprehensive tests for performance optimization

// Test 1: Memory Pool Optimization
test "memory pool optimization" {
  let memory_pool = MemoryPool::new(1024, 100) // 1KB blocks, 100 blocks
  
  // Test memory allocation without pool
  let start_time = Time::now()
  let allocations_without_pool = []
  
  for i in 0..=1000 {
    let allocation = StandardAllocator::allocate(1024)
    allocations_without_pool = allocations_without_pool.push(allocation)
  }
  
  let without_pool_time = Time::now() - start_time
  
  // Free allocations
  for allocation in allocations_without_pool {
    StandardAllocator::deallocate(allocation)
  }
  
  // Test memory allocation with pool
  let pool_start_time = Time::now()
  let allocations_with_pool = []
  
  for i in 0..=1000 {
    let allocation = MemoryPool::allocate(memory_pool)
    match allocation {
      Some(block) => allocations_with_pool = allocations_with_pool.push(block),
      None => {
        // Pool exhausted, fall back to standard allocator
        let fallback_allocation = StandardAllocator::allocate(1024)
        allocations_with_pool = allocations_with_pool.push(fallback_allocation)
      }
    }
  }
  
  let with_pool_time = Time::now() - pool_start_time
  
  // Free allocations
  for allocation in allocations_with_pool {
    match allocation {
      MemoryBlock(block) => MemoryPool::deallocate(memory_pool, block),
      StandardBlock(block) => StandardAllocator::deallocate(block)
    }
  }
  
  // Verify performance improvement
  assert_true(with_pool_time < without_pool_time)
  
  // Calculate performance improvement percentage
  let improvement_percentage = (without_pool_time - with_pool_time) / without_pool_time * 100.0
  assert_true(improvement_percentage > 10.0) // At least 10% improvement
  
  // Test pool statistics
  let pool_stats = MemoryPool::get_statistics(memory_pool)
  assert_true(pool_stats.total_allocations > 0)
  assert_true(pool_stats.pool_hits > 0)
  assert_true(pool_stats.pool_misses >= 0)
  
  // Verify pool hit rate
  let hit_rate = pool_stats.pool_hits as Float / (pool_stats.pool_hits + pool_stats.pool_misses) as Float * 100.0
  assert_true(hit_rate > 50.0) // At least 50% hit rate
  
  // Test different block sizes
  let small_pool = MemoryPool::new(64, 1000) // 64B blocks
  let medium_pool = MemoryPool::new(512, 500) // 512B blocks
  let large_pool = MemoryPool::new(4096, 100) // 4KB blocks
  
  // Test small allocations
  let small_start = Time::now()
  for i in 0..=5000 {
    let allocation = MemoryPool::allocate(small_pool)
    match allocation {
      Some(block) => MemoryPool::deallocate(small_pool, block),
      None => assert_true(false) // Should not run out
    }
  }
  let small_time = Time::now() - small_start
  
  // Test medium allocations
  let medium_start = Time::now()
  for i in 0..=1000 {
    let allocation = MemoryPool::allocate(medium_pool)
    match allocation {
      Some(block) => MemoryPool::deallocate(medium_pool, block),
      None => assert_true(false) // Should not run out
    }
  }
  let medium_time = Time::now() - medium_start
  
  // Test large allocations
  let large_start = Time::now()
  for i in 0..=200 {
    let allocation = MemoryPool::allocate(large_pool)
    match allocation {
      Some(block) => MemoryPool::deallocate(large_pool, block),
      None => assert_true(false) // Should not run out
    }
  }
  let large_time = Time::now() - large_start
  
  // Verify appropriate block sizes for different allocation patterns
  assert_true(small_time < medium_time)
  assert_true(medium_time < large_time)
  
  // Test pool fragmentation
  let fragmentation_pool = MemoryPool::new(1024, 100)
  let fragmented_allocations = []
  
  // Allocate and deallocate in a pattern that causes fragmentation
  for i in 0..=50 {
    let allocation = MemoryPool::allocate(fragmentation_pool)
    match allocation {
      Some(block) => {
        fragmented_allocations = fragmented_allocations.push(block)
        // Deallocate every other allocation
        if i % 2 == 0 {
          MemoryPool::deallocate(fragmentation_pool, block)
        }
      }
      None => assert_true(false)
    }
  }
  
  // Try to allocate more blocks
  let additional_allocations = []
  for i in 0..=25 {
    let allocation = MemoryPool::allocate(fragmentation_pool)
    match allocation {
      Some(block) => additional_allocations = additional_allocations.push(block),
      None => break // Pool might be fragmented
    }
  }
  
  // Verify fragmentation is handled reasonably
  let fragmentation_stats = MemoryPool::get_statistics(fragmentation_pool)
  assert_true(fragmentation_stats.fragmentation_percentage < 50.0) // Less than 50% fragmentation
  
  // Clean up
  MemoryPool::close(memory_pool)
  MemoryPool::close(small_pool)
  MemoryPool::close(medium_pool)
  MemoryPool::close(large_pool)
  MemoryPool::close(fragmentation_pool)
}

// Test 2: Connection Pool Optimization
test "connection pool optimization" {
  let connection_pool = ConnectionPool::new("postgresql://localhost:5432/telemetry", 10, 5) // 10 max, 5 min
  
  // Test connection creation without pool
  let start_time = Time::now()
  let connections_without_pool = []
  
  for i in 0..=50 {
    let connection = DatabaseClient::create_connection("postgresql://localhost:5432/telemetry")
    match connection {
      Some(conn) => connections_without_pool = connections_without_pool.push(conn),
      None => assert_true(false) // Should not fail
    }
  }
  
  let without_pool_time = Time::now() - start_time
  
  // Close connections
  for connection in connections_without_pool {
    DatabaseClient::close_connection(connection)
  }
  
  // Test connection creation with pool
  let pool_start_time = Time::now()
  let connections_with_pool = []
  
  for i in 0..=50 {
    let connection = ConnectionPool::get_connection(connection_pool)
    match connection {
      Some(conn) => connections_with_pool = connections_with_pool.push(conn),
      None => assert_true(false) // Should not fail with proper pool size
    }
  }
  
  let with_pool_time = Time::now() - pool_start_time
  
  // Return connections to pool
  for connection in connections_with_pool {
    ConnectionPool::return_connection(connection_pool, connection)
  }
  
  // Verify performance improvement
  assert_true(with_pool_time < without_pool_time)
  
  // Calculate performance improvement percentage
  let improvement_percentage = (without_pool_time - with_pool_time) / without_pool_time * 100.0
  assert_true(improvement_percentage > 20.0) // At least 20% improvement
  
  // Test pool statistics
  let pool_stats = ConnectionPool::get_statistics(connection_pool)
  assert_true(pool_stats.total_connections_created > 0)
  assert_true(pool_stats.active_connections >= 0)
  assert_true(pool_stats.idle_connections >= 0)
  assert_true(pool_stats.connection_hits > 0)
  
  // Test concurrent access
  let concurrent_start = Time::now()
  let threads = []
  
  for i in 0..=20 {
    let thread = Thread::spawn(fn() {
      let connections = []
      
      for j in 0..=10 {
        let connection = ConnectionPool::get_connection(connection_pool)
        match connection {
          Some(conn) => {
            // Simulate database operation
            Thread::sleep(10) // 10ms
            ConnectionPool::return_connection(connection_pool, conn)
          }
          None => assert_true(false)
        }
      }
    })
    threads = threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  let concurrent_time = Time::now() - concurrent_start
  
  // Test pool efficiency under load
  let load_stats = ConnectionPool::get_statistics(connection_pool)
  assert_true(load_stats.connection_hits > load_stats.total_connections_created) // Should reuse connections
  
  // Test connection timeout
  let timeout_pool = ConnectionPool::new("postgresql://localhost:5432/telemetry", 2, 1) // 2 max, 1 min
  
  // Get all connections
  let timeout_connections = []
  for i in 0..=2 {
    let connection = ConnectionPool::get_connection(timeout_pool)
    match connection {
      Some(conn) => timeout_connections = timeout_connections.push(conn),
      None => break
    }
  }
  
  // Try to get one more (should timeout)
  let timeout_start = Time::now()
  let timeout_connection = ConnectionPool::get_connection_with_timeout(timeout_pool, 100) // 100ms timeout
  let timeout_end = Time::now() - timeout_start
  
  match timeout_connection {
    Some(_) => assert_true(false), // Should not succeed
    None => assert_true(timeout_end >= 100) // Should wait at least 100ms
  }
  
  // Return connections
  for connection in timeout_connections {
    ConnectionPool::return_connection(timeout_pool, connection)
  }
  
  // Test pool resizing
  let resize_pool = ConnectionPool::new("postgresql://localhost:5432/telemetry", 5, 2) // 5 max, 2 min
  
  let initial_stats = ConnectionPool::get_statistics(resize_pool)
  assert_eq(initial_stats.max_connections, 5)
  
  // Resize pool
  ConnectionPool::resize(resize_pool, 10, 5) // 10 max, 5 min
  
  let resized_stats = ConnectionPool::get_statistics(resize_pool)
  assert_eq(resized_stats.max_connections, 10)
  
  // Test pool cleanup
  ConnectionPool::cleanup(resize_pool)
  let cleanup_stats = ConnectionPool::get_statistics(resize_pool)
  assert_true(cleanup_stats.idle_connections <= cleanup_stats.min_connections)
  
  // Clean up
  ConnectionPool::close(connection_pool)
  ConnectionPool::close(timeout_pool)
  ConnectionPool::close(resize_pool)
}

// Test 3: Cache Optimization
test "cache optimization" {
  let cache = Cache::new("redis", "localhost:6379", 1000) // 1000 max entries
  
  // Test cache performance
  let test_data = "test_data_with_reasonable_length_for_performance_testing"
  
  // Test without cache
  let without_cache_start = Time::now()
  let without_cache_results = []
  
  for i in 0..=1000 {
    // Simulate expensive operation
    Thread::sleep(1) // 1ms
    let result = "result_" + i.to_string()
    without_cache_results = without_cache_results.push(result)
  }
  
  let without_cache_time = Time::now() - without_cache_start
  
  // Test with cache
  let with_cache_start = Time::now()
  let with_cache_results = []
  
  for i in 0..=1000 {
    let key = "key_" + i.to_string()
    
    // Try to get from cache
    let cached_result = Cache::get(cache, key)
    match cached_result {
      Some(value) => with_cache_results = with_cache_results.push(value),
      None => {
        // Simulate expensive operation
        Thread::sleep(1) // 1ms
        let result = "result_" + i.to_string()
        Cache::set(cache, key, result, 3600) // 1 hour TTL
        with_cache_results = with_cache_results.push(result)
      }
    }
  }
  
  let with_cache_time = Time::now() - with_cache_start
  
  // Verify cache performance
  assert_true(with_cache_time < without_cache_time)
  
  // Calculate performance improvement
  let improvement_percentage = (without_cache_time - with_cache_time) / without_cache_time * 100.0
  assert_true(improvement_percentage > 30.0) // At least 30% improvement
  
  // Test cache hit rate
  let cache_stats = Cache::get_statistics(cache)
  assert_true(cache_stats.hit_rate > 0.0)
  
  // Test cache eviction policy
  let eviction_cache = Cache::new("redis", "localhost:6379", 100) // 100 max entries
  
  // Fill cache beyond capacity
  for i in 0..=200 {
    let key = "eviction_key_" + i.to_string()
    let value = "eviction_value_" + i.to_string()
    Cache::set(eviction_cache, key, value, 3600)
  }
  
  // Verify eviction occurred
  let eviction_stats = Cache::get_statistics(eviction_cache)
  assert_true(eviction_stats.evictions > 0)
  assert_true(eviction_stats.current_size <= 100)
  
  // Test LRU eviction
  let lru_cache = Cache::new("redis", "localhost:6379", 50) // 50 max entries
  
  // Add entries
  for i in 0..=50 {
    let key = "lru_key_" + i.to_string()
    let value = "lru_value_" + i.to_string()
    Cache::set(lru_cache, key, value, 3600)
  }
  
  // Access some entries to make them recently used
  for i in 0..=25 {
    let key = "lru_key_" + i.to_string()
    let _ = Cache::get(lru_cache, key)
  }
  
  // Add more entries to trigger eviction
  for i in 51..=75 {
    let key = "lru_key_" + i.to_string()
    let value = "lru_value_" + i.to_string()
    Cache::set(lru_cache, key, value, 3600)
  }
  
  // Verify recently used entries are still in cache
  let recently_used = Cache::get(lru_cache, "lru_key_0")
  match recently_used {
    Some(_) => assert_true(true), // Should still be in cache
    None => assert_true(false)
  }
  
  // Verify old entries were evicted
  let old_entry = Cache::get(lru_cache, "lru_key_30")
  match old_entry {
    Some(_) => assert_true(false), // Should have been evicted
    None => assert_true(true)
  }
  
  // Test cache expiration
  let expiration_cache = Cache::new("redis", "localhost:6379", 100)
  
  // Add entry with short TTL
  Cache::set(expiration_cache, "expiration_key", "expiration_value", 1) // 1 second TTL
  
  // Should be accessible immediately
  let immediate_result = Cache::get(expiration_cache, "expiration_key")
  match immediate_result {
    Some(_) => assert_true(true),
    None => assert_true(false)
  }
  
  // Wait for expiration
  Thread::sleep(1100) // 1.1 seconds
  
  // Should be expired now
  let expired_result = Cache::get(expiration_cache, "expiration_key")
  match expired_result {
    Some(_) => assert_true(false), // Should have expired
    None => assert_true(true)
  }
  
  // Test cache warming
  let warm_cache = Cache::new("redis", "localhost:6379", 1000)
  
  // Warm cache with frequently accessed data
  let warm_data = []
  for i in 0..=100 {
    let key = "warm_key_" + i.to_string()
    let value = "warm_value_" + i.to_string()
    Cache::set(warm_cache, key, value, 3600)
    warm_data = warm_data.push((key, value))
  }
  
  // Test warmed cache performance
  let warm_start = Time::now()
  for i in 0..=1000 {
    let key = "warm_key_" + (i % 100).to_string()
    let _ = Cache::get(warm_cache, key)
  }
  let warm_time = Time::now() - warm_start
  
  // Should be very fast since all data is in cache
  assert_true(warm_time < 100) // Less than 100ms for 1000 operations
  
  // Clean up
  Cache::close(cache)
  Cache::close(eviction_cache)
  Cache::close(lru_cache)
  Cache::close(expiration_cache)
  Cache::close(warm_cache)
}

// Test 4: Batch Processing Optimization
test "batch processing optimization" {
  let batch_processor = BatchProcessor::new(100) // 100 items per batch
  
  // Test individual processing
  let individual_start = Time::now()
  let individual_results = []
  
  for i in 0..=1000 {
    // Simulate individual processing
    Thread::sleep(1) // 1ms per item
    let result = "processed_" + i.to_string()
    individual_results = individual_results.push(result)
  }
  
  let individual_time = Time::now() - individual_start
  
  // Test batch processing
  let batch_start = Time::now()
  let batch_results = []
  let batch_items = []
  
  for i in 0..=1000 {
    let item = "item_" + i.to_string()
    batch_items = batch_items.push(item)
    
    // Process batch when full
    if batch_items.length() >= 100 {
      // Simulate batch processing (more efficient)
      Thread::sleep(10) // 10ms for 100 items (0.1ms per item)
      
      for item in batch_items {
        let result = "processed_" + item
        batch_results = batch_results.push(result)
      }
      
      batch_items = []
    }
  }
  
  // Process remaining items
  if batch_items.length() > 0 {
    Thread::sleep(10) // 10ms for remaining items
    for item in batch_items {
      let result = "processed_" + item
      batch_results = batch_results.push(result)
    }
  }
  
  let batch_time = Time::now() - batch_start
  
  // Verify batch processing performance
  assert_true(batch_time < individual_time)
  
  // Calculate performance improvement
  let improvement_percentage = (individual_time - batch_time) / individual_time * 100.0
  assert_true(improvement_percentage > 50.0) // At least 50% improvement
  
  // Test optimal batch size
  let batch_sizes = [10, 50, 100, 200, 500]
  let optimal_size_results = []
  
  for size in batch_sizes {
    let processor = BatchProcessor::new(size)
    let size_start = Time::now()
    
    for i in 0..=1000 {
      let item = "item_" + i.to_string()
      BatchProcessor::add_item(processor, item)
    }
    
    let size_end = Time::now() - size_start
    optimal_size_results = optimal_size_results.push((size, size_end))
    
    BatchProcessor::close(processor)
  }
  
  // Find optimal batch size
  let optimal_size = optimal_size_results.min_by(fn(a, b) {
    a.1 < b.1
  }).unwrap().0
  
  // Verify optimal size is reasonable
  assert_true(optimal_size >= 50 && optimal_size <= 200)
  
  // Test concurrent batch processing
  let concurrent_processor = BatchProcessor::new(100)
  let concurrent_start = Time::now()
  let threads = []
  
  for i in 0..=10 {
    let thread = Thread::spawn(fn() {
      for j in 0..=100 {
        let item = "thread_" + i.to_string() + "_item_" + j.to_string()
        BatchProcessor::add_item(concurrent_processor, item)
      }
    })
    threads = threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Process all batches
  BatchProcessor::process_all(concurrent_processor)
  
  let concurrent_time = Time::now() - concurrent_start
  
  // Verify concurrent processing is efficient
  assert_true(concurrent_time < individual_time)
  
  // Test batch processor statistics
  let processor_stats = BatchProcessor::get_statistics(concurrent_processor)
  assert_true(processor_stats.total_items > 0)
  assert_true(processor_stats.total_batches > 0)
  assert_true(processor_stats.average_batch_size > 0)
  
  // Clean up
  BatchProcessor::close(batch_processor)
  BatchProcessor::close(concurrent_processor)
}

// Test 5: Async Processing Optimization
test "async processing optimization" {
  let async_processor = AsyncProcessor::new(10) // 10 worker threads
  
  // Test synchronous processing
  let sync_start = Time::now()
  let sync_results = []
  
  for i in 0..=100 {
    // Simulate synchronous processing
    Thread::sleep(10) // 10ms per item
    let result = "sync_processed_" + i.to_string()
    sync_results = sync_results.push(result)
  }
  
  let sync_time = Time::now() - sync_start
  
  // Test asynchronous processing
  let async_start = Time::now()
  let async_futures = []
  
  for i in 0..=100 {
    let future = AsyncProcessor::submit(async_processor, fn() {
      Thread::sleep(10) // 10ms processing time
      return "async_processed_" + i.to_string()
    })
    async_futures = async_futures.push(future)
  }
  
  // Wait for all futures to complete
  let async_results = []
  for future in async_futures {
    let result = AsyncProcessor::get_result(future)
    async_results = async_results.push(result)
  }
  
  let async_time = Time::now() - async_start
  
  // Verify async processing performance
  assert_true(async_time < sync_time)
  
  // Calculate performance improvement
  let improvement_percentage = (sync_time - async_time) / sync_time * 100.0
  assert_true(improvement_percentage > 70.0) // At least 70% improvement
  
  // Test optimal thread pool size
  let thread_counts = [1, 2, 5, 10, 20, 50]
  let thread_results = []
  
  for count in thread_counts {
    let processor = AsyncProcessor::new(count)
    let count_start = Time::now()
    
    let futures = []
    for i in 0..=50 {
      let future = AsyncProcessor::submit(processor, fn() {
        Thread::sleep(20) // 20ms processing time
        return "result_" + i.to_string()
      })
      futures = futures.push(future)
    }
    
    // Wait for all futures
    for future in futures {
      let _ = AsyncProcessor::get_result(future)
    }
    
    let count_end = Time::now() - count_start
    thread_results = thread_results.push((count, count_end))
    
    AsyncProcessor::close(processor)
  }
  
  // Find optimal thread count
  let optimal_threads = thread_results.min_by(fn(a, b) {
    a.1 < b.1
  }).unwrap().0
  
  // Verify optimal thread count is reasonable
  assert_true(optimal_threads >= 5 && optimal_threads <= 20)
  
  // Test async processor with different task types
  let mixed_processor = AsyncProcessor::new(10)
  
  // Quick tasks
  let quick_futures = []
  for i in 0..=50 {
    let future = AsyncProcessor::submit(mixed_processor, fn() {
      Thread::sleep(1) // 1ms
      return "quick_" + i.to_string()
    })
    quick_futures = quick_futures.push(future)
  }
  
  // Slow tasks
  let slow_futures = []
  for i in 0..=10 {
    let future = AsyncProcessor::submit(mixed_processor, fn() {
      Thread::sleep(50) // 50ms
      return "slow_" + i.to_string()
    })
    slow_futures = slow_futures.push(future)
  }
  
  // Wait for all futures
  for future in quick_futures {
    let _ = AsyncProcessor::get_result(future)
  }
  
  for future in slow_futures {
    let _ = AsyncProcessor::get_result(future)
  }
  
  // Test async processor statistics
  let processor_stats = AsyncProcessor::get_statistics(mixed_processor)
  assert_true(processor_stats.total_tasks > 0)
  assert_true(processor_stats.completed_tasks > 0)
  assert_true(processor_stats.average_task_time > 0)
  assert_true(processor_stats.thread_utilization > 0.0)
  
  // Clean up
  AsyncProcessor::close(async_processor)
  AsyncProcessor::close(mixed_processor)
}

// Test 6: Data Structure Optimization
test "data structure optimization" {
  // Test array vs list performance
  let array_size = 10000
  
  // Test array operations
  let array = Array::new(array_size)
  let array_start = Time::now()
  
  // Fill array
  for i in 0..=array_size - 1 {
    Array::set(array, i, i)
  }
  
  // Access array elements
  let mut sum = 0
  for i in 0..=array_size - 1 {
    sum = sum + Array::get(array, i)
  }
  
  let array_time = Time::now() - array_start
  
  // Test list operations
  let list = List::new()
  let list_start = Time::now()
  
  // Fill list
  for i in 0..=array_size - 1 {
    List::append(list, i)
  }
  
  // Access list elements
  let mut list_sum = 0
  for i in 0..=array_size - 1 {
    list_sum = list_sum + List::get(list, i)
  }
  
  let list_time = Time::now() - list_start
  
  // Verify array is faster for sequential access
  assert_true(array_time < list_time)
  
  // Test hash map vs linear search
  let search_size = 1000
  let search_items = []
  
  for i in 0..=search_size - 1 {
    search_items = search_items.push(("key_" + i.to_string(), i))
  }
  
  // Test hash map
  let hash_map = HashMap::new()
  let hash_start = Time::now()
  
  for (key, value) in search_items {
    HashMap::put(hash_map, key, value)
  }
  
  // Search in hash map
  for i in 0..=search_size - 1 {
    let key = "key_" + i.to_string()
    let _ = HashMap::get(hash_map, key)
  }
  
  let hash_time = Time::now() - hash_start
  
  // Test linear search
  let linear_start = Time::now()
  
  // Search in array
  for i in 0..=search_size - 1 {
    let key = "key_" + i.to_string()
    let mut found = false
    
    for (search_key, _) in search_items {
      if search_key == key {
        found = true
        break
      }
    }
    
    assert_true(found)
  }
  
  let linear_time = Time::now() - linear_start
  
  // Verify hash map is much faster for search
  assert_true(hash_time < linear_time / 10) // At least 10x faster
  
  // Test string builder vs concatenation
  let string_parts = []
  for i in 0..=1000 {
    string_parts = string_parts.push("part_" + i.to_string())
  }
  
  // Test string concatenation
  let concat_start = Time::now()
  let mut concatenated_string = ""
  
  for part in string_parts {
    concatenated_string = concatenated_string + part
  }
  
  let concat_time = Time::now() - concat_start
  
  // Test string builder
  let builder_start = Time::now()
  let string_builder = StringBuilder::new()
  
  for part in string_parts {
    StringBuilder::append(string_builder, part)
  }
  
  let built_string = StringBuilder::to_string(string_builder)
  let builder_time = Time::now() - builder_start
  
  // Verify results are the same
  assert_eq(concatenated_string, built_string)
  
  // Verify string builder is faster
  assert_true(builder_time < concat_time)
  
  // Test tree vs array for sorted data
  let sorted_data = []
  for i in 0..=1000 {
    sorted_data = sorted_data.push(i * 2) // Even numbers
  }
  
  // Test binary search in array
  let array_search_start = Time::now()
  
  for i in 0..=100 {
    let target = i * 20 // Some values that exist and some that don't
    let _ = Array::binary_search(sorted_data, target)
  }
  
  let array_search_time = Time::now() - array_search_start
  
  // Test tree search
  let binary_tree = BinarySearchTree::new()
  for value in sorted_data {
    BinaryTree::insert(binary_tree, value)
  }
  
  let tree_search_start = Time::now()
  
  for i in 0..=100 {
    let target = i * 20
    let _ = BinaryTree::search(binary_tree, target)
  }
  
  let tree_search_time = Time::now() - tree_search_start
  
  // Verify both are efficient for search
  assert_true(array_search_time < 100) // Should be very fast
  assert_true(tree_search_time < 100) // Should also be very fast
  
  // Clean up
  Array::close(array)
  List::close(list)
  HashMap::close(hash_map)
  StringBuilder::close(string_builder)
  BinaryTree::close(binary_tree)
}

// Test 7: I/O Optimization
test "io optimization" {
  // Test buffered vs unbuffered I/O
  let test_data = "test_data_for_io_optimization_".repeat(1000)
  
  // Test unbuffered write
  let unbuffered_start = Time::now()
  
  for i in 0..=100 {
    let file_name = "unbuffered_test_" + i.to_string() + ".txt"
    FileSystem::write_file(file_name, test_data)
    FileSystem::delete_file(file_name)
  }
  
  let unbuffered_time = Time::now() - unbuffered_start
  
  // Test buffered write
  let buffered_start = Time::now()
  let buffer = BufferedWriter::new(8192) // 8KB buffer
  
  for i in 0..=100 {
    let file_name = "buffered_test_" + i.to_string() + ".txt"
    BufferedWriter::write_file(buffer, file_name, test_data)
    BufferedWriter::flush(buffer)
    FileSystem::delete_file(file_name)
  }
  
  let buffered_time = Time::now() - buffered_start
  
  // Verify buffered I/O is faster
  assert_true(buffered_time < unbuffered_time)
  
  // Test batch file operations
  let batch_files = []
  for i in 0..=100 {
    let file_name = "batch_test_" + i.to_string() + ".txt"
    let content = "content_" + i.to_string()
    batch_files = batch_files.push((file_name, content))
  }
  
  // Test individual file operations
  let individual_start = Time::now()
  
  for (file_name, content) in batch_files {
    FileSystem::write_file(file_name, content)
  }
  
  for (file_name, _) in batch_files {
    FileSystem::delete_file(file_name)
  }
  
  let individual_time = Time::now() - individual_start
  
  // Test batch file operations
  let batch_start = Time::now()
  
  FileSystem::write_files(batch_files)
  let file_names = batch_files.map(fn(file) { file.0 })
  FileSystem::delete_files(file_names)
  
  let batch_time = Time::now() - batch_start
  
  // Verify batch operations are faster
  assert_true(batch_time < individual_time)
  
  // Test asynchronous I/O
  let async_io = AsyncIO::new(10) // 10 worker threads
  
  // Test async file operations
  let async_start = Time::now()
  let async_futures = []
  
  for i in 0..=100 {
    let file_name = "async_test_" + i.to_string() + ".txt"
    let content = "async_content_" + i.to_string()
    
    let future = AsyncIO::write_file(async_io, file_name, content)
    async_futures = async_futures.push(future)
  }
  
  // Wait for all writes to complete
  for future in async_futures {
    let _ = AsyncIO::get_result(future)
  }
  
  // Async delete
  let delete_futures = []
  for i in 0..=100 {
    let file_name = "async_test_" + i.to_string() + ".txt"
    let future = AsyncIO::delete_file(async_io, file_name)
    delete_futures = delete_futures.push(future)
  }
  
  // Wait for all deletes to complete
  for future in delete_futures {
    let _ = AsyncIO::get_result(future)
  }
  
  let async_time = Time::now() - async_start
  
  // Verify async I/O is efficient
  assert_true(async_time < individual_time)
  
  // Test I/O statistics
  let io_stats = AsyncIO::get_statistics(async_io)
  assert_true(io_stats.total_operations > 0)
  assert_true(io_stats.completed_operations > 0)
  assert_true(io_stats.average_operation_time > 0)
  
  // Clean up
  BufferedWriter::close(buffer)
  AsyncIO::close(async_io)
}

// Test 8: Serialization Optimization
test "serialization optimization" {
  let test_objects = []
  
  // Create test objects
  for i in 0..=1000 {
    let obj = TestObject::new(
      "object_" + i.to_string(),
      i,
      i as Float * 1.5,
      ["attr1", "attr2", "attr3"]
    )
    test_objects = test_objects.push(obj)
  }
  
  // Test JSON serialization
  let json_start = Time::now()
  let json_strings = []
  
  for obj in test_objects {
    let json_string = JsonSerializer::serialize(obj)
    json_strings = json_strings.push(json_string)
  }
  
  let json_serialize_time = Time::now() - json_start
  
  // Test JSON deserialization
  let json_deserialize_start = Time::now()
  let json_deserialized_objects = []
  
  for json_string in json_strings {
    let obj = JsonSerializer::deserialize(json_string)
    json_deserialized_objects = json_deserialized_objects.push(obj)
  }
  
  let json_deserialize_time = Time::now() - json_deserialize_start
  
  // Test binary serialization
  let binary_start = Time::now()
  let binary_data = []
  
  for obj in test_objects {
    let data = BinarySerializer::serialize(obj)
    binary_data = binary_data.push(data)
  }
  
  let binary_serialize_time = Time::now() - binary_start
  
  // Test binary deserialization
  let binary_deserialize_start = Time::now()
  let binary_deserialized_objects = []
  
  for data in binary_data {
    let obj = BinarySerializer::deserialize(data)
    binary_deserialized_objects = binary_deserialized_objects.push(obj)
  }
  
  let binary_deserialize_time = Time::now() - binary_deserialize_start
  
  // Verify binary serialization is faster
  assert_true(binary_serialize_time < json_serialize_time)
  assert_true(binary_deserialize_time < json_deserialize_time)
  
  // Verify binary is more compact
  let total_json_size = json_strings.reduce(0, fn(acc, str) { acc + str.length() })
  let total_binary_size = binary_data.reduce(0, fn(acc, data) { acc + data.length() })
  
  assert_true(total_binary_size < total_json_size)
  
  // Test compression
  let compression_start = Time::now()
  let compressed_data = []
  
  for json_string in json_strings {
    let compressed = Compression::compress(json_string)
    compressed_data = compressed_data.push(compressed)
  }
  
  let compression_time = Time::now() - compression_start
  
  // Test decompression
  let decompression_start = Time::now()
  let decompressed_strings = []
  
  for compressed in compressed_data {
    let decompressed = Compression::decompress(compressed)
    decompressed_strings = decompressed_strings.push(decompressed)
  }
  
  let decompression_time = Time::now() - decompression_start
  
  // Verify compression reduces size
  let total_compressed_size = compressed_data.reduce(0, fn(acc, data) { acc + data.length() })
  assert_true(total_compressed_size < total_json_size)
  
  // Verify decompression restores original data
  assert_eq(json_strings.length(), decompressed_strings.length())
  for i in 0..=json_strings.length() - 1 {
    assert_eq(json_strings[i], decompressed_strings[i])
  }
  
  // Test serialization pool optimization
  let serialization_pool = SerializationPool::new(100) // 100 objects in pool
  
  let pool_start = Time::now()
  
  for obj in test_objects {
    let serialized = SerializationPool::serialize(serialization_pool, obj)
    SerializationPool::release(serialization_pool, serialized)
  }
  
  let pool_time = Time::now() - pool_start
  
  // Verify pool improves performance
  assert_true(pool_time < json_serialize_time)
  
  // Test pool statistics
  let pool_stats = SerializationPool::get_statistics(serialization_pool)
  assert_true(pool_stats.pool_hits > 0)
  assert_true(pool_stats.hit_rate > 0.5) // At least 50% hit rate
  
  // Clean up
  JsonSerializer::close()
  BinarySerializer::close()
  Compression::close()
  SerializationPool::close(serialization_pool)
}

// Test 9: Network Optimization
test "network optimization" {
  // Test connection reuse
  let connection_reuse_start = Time::now()
  
  for i in 0..=100 {
    // Create new connection each time
    let connection = NetworkClient::create_connection("httpbin.org", 80)
    NetworkClient::send_request(connection, "GET /get HTTP/1.1\r\nHost: httpbin.org\r\n\r\n")
    NetworkClient::close_connection(connection)
  }
  
  let connection_reuse_time = Time::now() - connection_reuse_start
  
  // Test connection pooling
  let connection_pool = NetworkConnectionPool::new("httpbin.org", 80, 10)
  let pool_start = Time::now()
  
  for i in 0..=100 {
    let connection = NetworkConnectionPool::get_connection(connection_pool)
    NetworkClient::send_request(connection, "GET /get HTTP/1.1\r\nHost: httpbin.org\r\n\r\n")
    NetworkConnectionPool::return_connection(connection_pool, connection)
  }
  
  let pool_time = Time::now() - pool_start
  
  // Verify connection pooling is faster
  assert_true(pool_time < connection_reuse_time)
  
  // Test request batching
  let requests = []
  for i in 0..=100 {
    let request = NetworkRequest::new("GET", "/get", [("id", i.to_string())], None)
    requests = requests.push(request)
  }
  
  // Test individual requests
  let individual_start = Time::now()
  
  for request in requests {
    let connection = NetworkConnectionPool::get_connection(connection_pool)
    let response = NetworkClient::send_request(connection, request)
    NetworkConnectionPool::return_connection(connection_pool, connection)
  }
  
  let individual_time = Time::now() - individual_start
  
  // Test batch requests
  let batch_start = Time::now()
  
  let batch_request = NetworkRequest::batch(requests)
  let connection = NetworkConnectionPool::get_connection(connection_pool)
  let batch_response = NetworkClient::send_request(connection, batch_request)
  NetworkConnectionPool::return_connection(connection_pool, connection)
  
  let batch_time = Time::now() - batch_start
  
  // Verify batch requests are more efficient
  assert_true(batch_time < individual_time)
  
  // Test HTTP/2 multiplexing
  let http2_client = Http2Client::new("httpbin.org")
  let http2_start = Time::now()
  
  let http2_futures = []
  for i in 0..=50 {
    let request = NetworkRequest::new("GET", "/get", [("id", i.to_string())], None)
    let future = Http2Client::send_request_async(http2_client, request)
    http2_futures = http2_futures.push(future)
  }
  
  // Wait for all requests to complete
  for future in http2_futures {
    let _ = Http2Client::get_result(future)
  }
  
  let http2_time = Time::now() - http2_start
  
  // Test HTTP/1.1 pipelining
  let http11_client = Http11Client::new("httpbin.org")
  let http11_start = Time::now()
  
  let http11_requests = []
  for i in 0..=50 {
    let request = NetworkRequest::new("GET", "/get", [("id", i.to_string())], None)
    http11_requests = http11_requests.push(request)
  }
  
  let connection = NetworkConnectionPool::get_connection(connection_pool)
  let http11_responses = Http11Client::send_pipeline(connection, http11_requests)
  NetworkConnectionPool::return_connection(connection_pool, connection)
  
  let http11_time = Time::now() - http11_start
  
  // Verify HTTP/2 is more efficient than HTTP/1.1 pipelining
  assert_true(http2_time < http11_time)
  
  // Test network statistics
  let network_stats = NetworkConnectionPool::get_statistics(connection_pool)
  assert_true(network_stats.total_connections > 0)
  assert_true(network_stats.connection_reuses > 0)
  assert_true(network_stats.reuse_rate > 0.5) // At least 50% reuse rate
  
  // Clean up
  NetworkConnectionPool::close(connection_pool)
  Http2Client::close(http2_client)
  Http11Client::close(http11_client)
}

// Test 10: CPU Optimization
test "cpu optimization" {
  // Test vectorized operations
  let vector_size = 10000
  let vector_a = []
  let vector_b = []
  
  for i in 0..=vector_size - 1 {
    vector_a = vector_a.push(i as Float)
    vector_b = vector_b.push((i * 2) as Float)
  }
  
  // Test scalar operations
  let scalar_start = Time::now()
  let scalar_result = []
  
  for i in 0..=vector_size - 1 {
    let result = vector_a[i] + vector_b[i]
    scalar_result = scalar_result.push(result)
  }
  
  let scalar_time = Time::now() - scalar_start
  
  // Test vectorized operations
  let vector_start = Time::now()
  let vector_result = VectorOperations::add(vector_a, vector_b)
  
  let vector_time = Time::now() - vector_start
  
  // Verify vectorized operations are faster
  assert_true(vector_time < scalar_time)
  
  // Verify results are the same
  assert_eq(scalar_result.length(), vector_result.length())
  for i in 0..=scalar_result.length() - 1 {
    assert_true((scalar_result[i] - vector_result[i]).abs() < 0.001)
  }
  
  // Test parallel processing
  let parallel_start = Time::now()
  let parallel_result = ParallelProcessor::process(vector_a, fn(x) {
    x * 2.0
  }, 4) // 4 threads
  
  let parallel_time = Time::now() - parallel_start
  
  // Test sequential processing
  let sequential_start = Time::now()
  let sequential_result = []
  
  for x in vector_a {
    let result = x * 2.0
    sequential_result = sequential_result.push(result)
  }
  
  let sequential_time = Time::now() - sequential_start
  
  // Verify parallel processing is faster for large datasets
  assert_true(parallel_time < sequential_time)
  
  // Verify results are the same
  assert_eq(parallel_result.length(), sequential_result.length())
  for i in 0..=parallel_result.length() - 1 {
    assert_true((parallel_result[i] - sequential_result[i]).abs() < 0.001)
  }
  
  // Test CPU cache optimization
  let cache_size = 100000
  let cache_optimized_data = []
  let cache_unoptimized_data = []
  
  // Create cache-unfriendly data (random access pattern)
  for i in 0..=cache_size - 1 {
    cache_unoptimized_data = cache_unoptimized_data.push(i)
  }
  
  // Create cache-friendly data (sequential access pattern)
  for i in 0..=cache_size - 1 {
    cache_optimized_data = cache_optimized_data.push(i)
  }
  
  // Test cache-unfriendly access
  let cache_unfriendly_start = Time::now()
  let mut unfriendly_sum = 0
  
  for i in 0..=cache_size - 1 {
    let random_index = (i * 7) % cache_size // Pseudo-random access
    unfriendly_sum = unfriendly_sum + cache_unoptimized_data[random_index]
  }
  
  let cache_unfriendly_time = Time::now() - cache_unfriendly_start
  
  // Test cache-friendly access
  let cache_friendly_start = Time::now()
  let mut friendly_sum = 0
  
  for i in 0..=cache_size - 1 {
    friendly_sum = friendly_sum + cache_optimized_data[i]
  }
  
  let cache_friendly_time = Time::now() - cache_friendly_start
  
  // Verify cache-friendly access is faster
  assert_true(cache_friendly_time < cache_unfriendly_time)
  
  // Test SIMD optimization
  let simd_data = []
  for i in 0..=10000 - 1 {
    simd_data = simd_data.push(i as Float)
  }
  
  // Test regular operations
  let regular_start = Time::now()
  let mut regular_sum = 0.0
  
  for x in simd_data {
    regular_sum = regular_sum + x
  }
  
  let regular_time = Time::now() - regular_start
  
  // Test SIMD operations
  let simd_start = Time::now()
  let simd_sum = SIMDOperations::sum(simd_data)
  
  let simd_time = Time::now() - simd_start
  
  // Verify SIMD operations are faster
  assert_true(simd_time < regular_time)
  
  // Verify results are the same
  assert_true((regular_sum - simd_sum).abs() < 0.001)
  
  // Test CPU optimization statistics
  let cpu_stats = CPUOptimizer::get_statistics()
  assert_true(cpu_stats.vector_operations_used > 0)
  assert_true(cpu_stats.parallel_operations_used > 0)
  assert_true(cpu_stats.cache_optimizations_used > 0)
  assert_true(cpu_stats.simd_operations_used > 0)
  
  // Clean up
  VectorOperations::close()
  ParallelProcessor::close()
  SIMDOperations::close()
  CPUOptimizer::close()
}