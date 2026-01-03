// Performance Optimization Tests for Azimuth Telemetry System
// This file contains test cases for performance optimization functionality

// Test 1: Memory Pool Management
test "memory pool management" {
  let memory_pool = MemoryPool::new(1024) // 1KB pool
  
  // Test pool allocation
  let block1 = MemoryPool::allocate(memory_pool, 256)
  let block2 = MemoryPool::allocate(memory_pool, 256)
  let block3 = MemoryPool::allocate(memory_pool, 256)
  
  assert_true(MemoryPool::is_valid(block1))
  assert_true(MemoryPool::is_valid(block2))
  assert_true(MemoryPool::is_valid(block3))
  
  // Test pool deallocation
  MemoryPool::deallocate(memory_pool, block2)
  assert_false(MemoryPool::is_valid(block2))
  
  // Test reuse of deallocated block
  let block4 = MemoryPool::allocate(memory_pool, 256)
  assert_true(MemoryPool::is_valid(block4))
  
  // Test pool statistics
  let stats = MemoryPool::get_stats(memory_pool)
  assert_eq(stats.total_size, 1024)
  assert_eq(stats.used_size, 768) // 3 blocks of 256 bytes
  assert_eq(stats.free_size, 256)
  assert_eq(stats.allocation_count, 3)
  assert_eq(stats.deallocation_count, 1)
  
  // Test pool reset
  MemoryPool::reset(memory_pool)
  let reset_stats = MemoryPool::get_stats(memory_pool)
  assert_eq(reset_stats.used_size, 0)
  assert_eq(reset_stats.free_size, 1024)
}

// Test 2: Object Caching
test "object caching" {
  let object_cache = ObjectCache::new(100) // Cache up to 100 objects
  
  // Test cache insertion
  let obj1 = CachedObject::new("key1", "value1")
  let obj2 = CachedObject::new("key2", "value2")
  let obj3 = CachedObject::new("key3", "value3")
  
  ObjectCache::put(object_cache, obj1)
  ObjectCache::put(object_cache, obj2)
  ObjectCache::put(object_cache, obj3)
  
  // Test cache retrieval
  let retrieved1 = ObjectCache::get(object_cache, "key1")
  let retrieved2 = ObjectCache::get(object_cache, "key2")
  let retrieved3 = ObjectCache::get(object_cache, "key3")
  
  assert_true(retrieved1.is_some())
  assert_true(retrieved2.is_some())
  assert_true(retrieved3.is_some())
  
  match retrieved1 {
    Some(obj) => assert_eq(obj.value, "value1")
    None => assert_true(false)
  }
  
  // Test cache eviction (LRU)
  for i = 4; i <= 150; i = i + 1 {
    let obj = CachedObject::new("key" + i.to_string(), "value" + i.to_string())
    ObjectCache::put(object_cache, obj)
  }
  
  let evicted1 = ObjectCache::get(object_cache, "key1")
  let evicted2 = ObjectCache::get(object_cache, "key2")
  
  assert_true(evicted1.is_none()) // Should be evicted
  assert_true(evicted2.is_none()) // Should be evicted
  
  // Test cache statistics
  let stats = ObjectCache::get_stats(object_cache)
  assert_eq(stats.size, 100) // Should be at max capacity
  assert_eq(stats.hits, 3)
  assert_eq(stats.misses, 2)
  assert_eq(stats.evictions, 2)
}

// Test 3: Batch Processing Optimization
test "batch processing optimization" {
  let batch_processor = BatchProcessor::new(10, 1000) // 10 items or 1 second timeout
  
  // Test batch accumulation
  for i = 1; i <= 5; i = i + 1 {
    let item = BatchItem::new("item" + i.to_string(), i.to_float())
    BatchProcessor::add_item(batch_processor, item)
  }
  
  let stats = BatchProcessor::get_stats(batch_processor)
  assert_eq(stats.pending_items, 5)
  
  // Test batch flush
  let flushed_items = BatchProcessor::flush(batch_processor)
  assert_eq(flushed_items.length(), 5)
  
  let flushed_stats = BatchProcessor::get_stats(batch_processor)
  assert_eq(flushed_stats.pending_items, 0)
  assert_eq(flushed_stats.processed_batches, 1)
  
  // Test auto-flush on size limit
  for i = 1; i <= 10; i = i + 1 {
    let item = BatchItem::new("auto_item" + i.to_string(), i.to_float())
    BatchProcessor::add_item(batch_processor, item)
  }
  
  let auto_stats = BatchProcessor::get_stats(batch_processor)
  assert_eq(auto_stats.pending_items, 0) // Should auto-flush
  assert_eq(auto_stats.processed_batches, 2)
  
  // Test batch aggregation
  let aggregator = BatchAggregator::new()
  BatchAggregator::add_aggregation_rule(aggregator, "sum", lambda { items => items.map(lambda { item => item.value }).sum() })
  BatchAggregator::add_aggregation_rule(aggregator, "avg", lambda { items => items.map(lambda { item => item.value }).sum() / items.length().to_float() })
  
  let test_items = [
    BatchItem::new("test1", 10.0),
    BatchItem::new("test2", 20.0),
    BatchItem::new("test3", 30.0)
  ]
  
  let sum_result = BatchAggregator::aggregate(aggregator, "sum", test_items)
  let avg_result = BatchAggregator::aggregate(aggregator, "avg", test_items)
  
  assert_eq(sum_result, 60.0)
  assert_eq(avg_result, 20.0)
}

// Test 4: Lazy Loading
test "lazy loading optimization" {
  // Test lazy value creation
  let lazy_value = Lazy::new(lambda { 
    // Simulate expensive computation
    let mut result = 0
    for i = 1; i <= 1000; i = i + 1 {
      result = result + i
    }
    result
  })
  
  assert_false(Lazy::is_evaluated(lazy_value))
  
  // Test lazy evaluation
  let value = Lazy::get(lazy_value)
  assert_eq(value, 500500) // Sum of 1 to 1000
  assert_true(Lazy::is_evaluated(lazy_value))
  
  // Test multiple accesses don't re-evaluate
  let value2 = Lazy::get(lazy_value)
  assert_eq(value2, 500500)
  
  // Test lazy collection
  let lazy_collection = LazyCollection::new(lambda { 
    // Simulate expensive data loading
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  })
  
  assert_false(LazyCollection::is_loaded(lazy_collection))
  
  let first_item = LazyCollection::get(lazy_collection, 0)
  assert_eq(first_item, 1)
  assert_true(LazyCollection::is_loaded(lazy_collection))
  
  let collection_length = LazyCollection::length(lazy_collection)
  assert_eq(collection_length, 10)
}

// Test 5: Connection Pooling
test "connection pooling optimization" {
  let connection_pool = ConnectionPool::new(5) // Max 5 connections
  
  // Test connection acquisition
  let conn1 = ConnectionPool::acquire(connection_pool)
  let conn2 = ConnectionPool::acquire(connection_pool)
  let conn3 = ConnectionPool::acquire(connection_pool)
  
  assert_true(ConnectionPool::is_valid(conn1))
  assert_true(ConnectionPool::is_valid(conn2))
  assert_true(ConnectionPool::is_valid(conn3))
  
  let pool_stats = ConnectionPool::get_stats(connection_pool)
  assert_eq(pool_stats.active_connections, 3)
  assert_eq(pool_stats.available_connections, 2)
  
  // Test connection release
  ConnectionPool::release(connection_pool, conn2)
  
  let release_stats = ConnectionPool::get_stats(connection_pool)
  assert_eq(release_stats.active_connections, 2)
  assert_eq(release_stats.available_connections, 3)
  
  // Test connection reuse
  let conn4 = ConnectionPool::acquire(connection_pool)
  assert_true(ConnectionPool::is_valid(conn4))
  
  // Test pool exhaustion
  let conn5 = ConnectionPool::acquire(connection_pool)
  let conn6 = ConnectionPool::acquire(connection_pool)
  let conn7 = ConnectionPool::acquire(connection_pool) // Should fail
  
  assert_true(ConnectionPool::is_valid(conn5))
  assert_true(ConnectionPool::is_valid(conn6))
  assert_false(ConnectionPool::is_valid(conn7)) // Pool exhausted
  
  let exhaustion_stats = ConnectionPool::get_stats(connection_pool)
  assert_eq(exhaustion_stats.active_connections, 5)
  assert_eq(exhaustion_stats.available_connections, 0)
  assert_eq(exhaustion_stats.rejected_requests, 1)
}

// Test 6: Data Compression
test "data compression optimization" {
  let original_data = "This is a test string for compression. It contains repeated patterns and should compress well. This is a test string for compression. It contains repeated patterns and should compress well."
  
  // Test GZIP compression
  let compressed_data = Compression::gzip_compress(original_data)
  assert_true(compressed_data.length() < original_data.length())
  
  let decompressed_data = Compression::gzip_decompress(compressed_data)
  assert_eq(decompressed_data, original_data)
  
  // Test LZ4 compression
  let lz4_compressed = Compression::lz4_compress(original_data)
  assert_true(lz4_compressed.length() < original_data.length())
  
  let lz4_decompressed = Compression::lz4_decompress(lz4_compressed)
  assert_eq(lz4_decompressed, original_data)
  
  // Test compression ratio calculation
  let gzip_ratio = Compression::calculate_compression_ratio(original_data, compressed_data)
  let lz4_ratio = Compression::calculate_compression_ratio(original_data, lz4_compressed)
  
  assert_true(gzip_ratio > 0.0 && gzip_ratio < 1.0)
  assert_true(lz4_ratio > 0.0 && lz4_ratio < 1.0)
  
  // Test adaptive compression (chooses best algorithm based on data)
  let adaptive_compressed = Compression::adaptive_compress(original_data)
  let adaptive_decompressed = Compression::adaptive_decompress(adaptive_compressed)
  
  assert_eq(adaptive_decompressed, original_data)
}

// Test 7: Parallel Processing
test "parallel processing optimization" {
  let data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Test parallel map
  let mapped_data = Parallel::map(data, 4, lambda { x => x * 2 })
  assert_eq(mapped_data, [2, 4, 6, 8, 10, 12, 14, 16, 18, 20])
  
  // Test parallel filter
  let filtered_data = Parallel::filter(data, 4, lambda { x => x % 2 == 0 })
  assert_eq(filtered_data, [2, 4, 6, 8, 10])
  
  // Test parallel reduce
  let sum = Parallel::reduce(data, 4, 0, lambda { acc, x => acc + x })
  assert_eq(sum, 55)
  
  // Test parallel foreach
  let results = Array::new(data.length(), 0)
  Parallel::foreach(data, 4, lambda { i, x => results[i] = x * x })
  assert_eq(results, [1, 4, 9, 16, 25, 36, 49, 64, 81, 100])
  
  // Test work stealing
  let work_stealing_pool = WorkStealingPool::new(4)
  let tasks = [
    Task::new(lambda { 1 }),
    Task::new(lambda { 2 }),
    Task::new(lambda { 3 }),
    Task::new(lambda { 4 })
  ]
  
  let task_results = WorkStealingPool::execute_all(work_stealing_pool, tasks)
  assert_eq(task_results.length(), 4)
  assert_true(task_results.contains(1))
  assert_true(task_results.contains(2))
  assert_true(task_results.contains(3))
  assert_true(task_results.contains(4))
}

// Test 8: Lock-Free Data Structures
test "lock-free data structures" {
  // Test lock-free queue
  let lock_free_queue = LockFreeQueue::new()
  
  // Test enqueue
  LockFreeQueue::enqueue(lock_free_queue, 1)
  LockFreeQueue::enqueue(lock_free_queue, 2)
  LockFreeQueue::enqueue(lock_free_queue, 3)
  
  // Test dequeue
  let item1 = LockFreeQueue::dequeue(lock_free_queue)
  let item2 = LockFreeQueue::dequeue(lock_free_queue)
  let item3 = LockFreeQueue::dequeue(lock_free_queue)
  let item4 = LockFreeQueue::dequeue(lock_free_queue) // Should be empty
  
  assert_eq(item1, Some(1))
  assert_eq(item2, Some(2))
  assert_eq(item3, Some(3))
  assert_eq(item4, None)
  
  // Test lock-free stack
  let lock_free_stack = LockFreeStack::new()
  
  // Test push
  LockFreeStack::push(lock_free_stack, 1)
  LockFreeStack::push(lock_free_stack, 2)
  LockFreeStack::push(lock_free_stack, 3)
  
  // Test pop
  let stack_item1 = LockFreeStack::pop(lock_free_stack)
  let stack_item2 = LockFreeStack::pop(lock_free_stack)
  let stack_item3 = LockFreeStack::pop(lock_free_stack)
  let stack_item4 = LockFreeStack::pop(lock_free_stack) // Should be empty
  
  assert_eq(stack_item1, Some(3))
  assert_eq(stack_item2, Some(2))
  assert_eq(stack_item3, Some(1))
  assert_eq(stack_item4, None)
  
  // Test lock-free counter
  let lock_free_counter = LockFreeCounter::new(0)
  
  // Test increment
  LockFreeCounter::increment(lock_free_counter)
  LockFreeCounter::increment(lock_free_counter)
  LockFreeCounter::increment(lock_free_counter)
  
  let counter_value = LockFreeCounter::get(lock_free_counter)
  assert_eq(counter_value, 3)
  
  // Test add
  LockFreeCounter::add(lock_free_counter, 5)
  let added_value = LockFreeCounter::get(lock_free_counter)
  assert_eq(added_value, 8)
}

// Test 9: Adaptive Algorithms
test "adaptive algorithms optimization" {
  // Test adaptive sorting
  let small_array = [3, 1, 4, 1, 5]
  let large_array = Array::new(1000, 0)
  
  // Initialize large array with random values
  for i = 0; i < large_array.length(); i = i + 1 {
    large_array[i] = Random::int(10000)
  }
  
  let sorted_small = AdaptiveSort::sort(small_array)
  let sorted_large = AdaptiveSort::sort(large_array)
  
  assert_eq(sorted_small, [1, 1, 3, 4, 5])
  assert_true(Array::is_sorted(sorted_large))
  
  // Test adaptive hash table
  let adaptive_table = AdaptiveHashTable::new()
  
  // Add items
  AdaptiveHashTable::put(adaptive_table, "key1", "value1")
  AdaptiveHashTable::put(adaptive_table, "key2", "value2")
  AdaptiveHashTable::put(adaptive_table, "key3", "value3")
  
  // Get items
  let value1 = AdaptiveHashTable::get(adaptive_table, "key1")
  let value2 = AdaptiveHashTable::get(adaptive_table, "key2")
  let value3 = AdaptiveHashTable::get(adaptive_table, "key3")
  
  assert_eq(value1, Some("value1"))
  assert_eq(value2, Some("value2"))
  assert_eq(value3, Some("value3"))
  
  // Check if table adapted based on usage patterns
  let stats = AdaptiveHashTable::get_stats(adaptive_table)
  assert_true(stats.resize_count >= 0)
  assert_true(stats.collision_rate >= 0.0)
}

// Test 10: Resource-Aware Processing
test "resource-aware processing" {
  // Test memory-aware processing
  let memory_monitor = MemoryMonitor::new()
  let memory_threshold = MemoryMonitor::get_available_memory(memory_monitor) * 0.8
  
  let processor = ResourceAwareProcessor::new()
  ResourceAwareProcessor::set_memory_threshold(processor, memory_threshold)
  
  // Test CPU-aware processing
  let cpu_monitor = CPUMonitor::new()
  let cpu_cores = CPUMonitor::get_core_count(cpu_monitor)
  
  ResourceAwareProcessor::set_cpu_threshold(processor, cpu_cores * 0.9)
  
  // Test adaptive batch size based on resources
  let initial_batch_size = 100
  let adaptive_batch_size = ResourceAwareProcessor::calculate_adaptive_batch_size(
    processor, 
    initial_batch_size
  )
  
  assert_true(adaptive_batch_size > 0)
  assert_true(adaptive_batch_size <= initial_batch_size)
  
  // Test resource-aware task scheduling
  let scheduler = ResourceAwareScheduler::new()
  let task1 = ResourceAwareTask::new("task1", 100, 50) // 100MB memory, 50% CPU
  let task2 = ResourceAwareTask::new("task2", 200, 25) // 200MB memory, 25% CPU
  let task3 = ResourceAwareTask::new("task3", 50, 75)  // 50MB memory, 75% CPU
  
  ResourceAwareScheduler::schedule_task(scheduler, task1)
  ResourceAwareScheduler::schedule_task(scheduler, task2)
  ResourceAwareScheduler::schedule_task(scheduler, task3)
  
  let scheduled_tasks = ResourceAwareScheduler::get_scheduled_tasks(scheduler)
  assert_eq(scheduled_tasks.length(), 3)
  
  // Test resource usage reporting
  let resource_report = ResourceAwareProcessor::generate_resource_report(processor)
  assert_true(resource_report.memory_usage >= 0.0)
  assert_true(resource_report.cpu_usage >= 0.0)
  assert_true(resource_report.memory_usage <= 100.0)
  assert_true(resource_report.cpu_usage <= 100.0)
}