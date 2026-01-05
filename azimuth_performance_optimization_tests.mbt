// Azimuth Telemetry System - Performance Optimization Tests
// This file contains comprehensive test cases for performance optimization functionality

// Test 1: Memory Usage Optimization Tests
test "memory usage optimization operations" {
  // Test object pooling
  let pool = ObjectPool::new(() -> { TestObject::new() }, 10)
  
  // Acquire objects from pool
  let obj1 = ObjectPool::acquire(pool)
  let obj2 = ObjectPool::acquire(pool)
  let obj3 = ObjectPool::acquire(pool)
  
  assert_true(obj1 != None)
  assert_true(obj2 != None)
  assert_true(obj3 != None)
  
  // Release objects back to pool
  ObjectPool::release(pool, obj1)
  ObjectPool::release(pool, obj2)
  ObjectPool::release(pool, obj3)
  
  // Verify pool has available objects
  assert_eq(ObjectPool::available_count(pool), 3)
  
  // Test memory-efficient data structures
  let large_array = ArrayUtil::create_sparse_array(1000, 0)
  assert_eq(large_array.length(), 1000)
  
  // Only set non-default values
  ArrayUtil::set_sparse(large_array, 100, 100)
  ArrayUtil::set_sparse(large_array, 200, 200)
  
  assert_eq(ArrayUtil::get_sparse(large_array, 100), 100)
  assert_eq(ArrayUtil::get_sparse(large_array, 50), 0)  // Default value
  
  // Test memory usage tracking
  let memory_tracker = MemoryTracker::new()
  let initial_memory = MemoryTracker::get_usage(memory_tracker)
  
  // Allocate memory
  let large_data = ArrayUtil::create_with_capacity(10000)
  let after_allocation = MemoryTracker::get_usage(memory_tracker)
  
  assert_true(after_allocation > initial_memory)
  
  // Free memory
  ArrayUtil::clear(large_data)
  MemoryTracker::force_gc()
  let after_cleanup = MemoryTracker::get_usage(memory_tracker)
  
  assert_true(after_cleanup < after_allocation)
}

// Test 2: CPU Performance Optimization Tests
test "cpu performance optimization operations" {
  // Test parallel processing
  let large_dataset = ArrayUtil::range(1, 10000)
  
  // Sequential processing
  let start_time = TimeUtil::current_time_millis()
  let sequential_result = ArrayUtil::map(large_dataset, (x) -> { x * 2 })
  let sequential_time = TimeUtil::current_time_millis() - start_time
  
  // Parallel processing
  start_time = TimeUtil::current_time_millis()
  let parallel_result = ParallelUtil::map(large_dataset, (x) -> { x * 2 })
  let parallel_time = TimeUtil::current_time_millis() - start_time
  
  // Verify results are identical
  assert_eq(sequential_result.length(), parallel_result.length())
  for i in 0..sequential_result.length() {
    assert_eq(sequential_result[i], parallel_result[i])
  }
  
  // Parallel should be faster for large datasets
  assert_true(parallel_time <= sequential_time)
  
  // Test caching mechanisms
  let cache = LRUCache::new(100)
  
  // Cache expensive computation
  let computation_result = CacheUtil::get_or_compute(cache, "expensive_key", () -> {
    // Simulate expensive computation
    TimeUtil::sleep(10)
    42
  })
  
  assert_eq(computation_result, 42)
  
  // Second call should use cache (no sleep)
  start_time = TimeUtil::current_time_millis()
  let cached_result = CacheUtil::get_or_compute(cache, "expensive_key", () -> {
    TimeUtil::sleep(10)
    24
  })
  let cached_time = TimeUtil::current_time_millis() - start_time
  
  assert_eq(cached_result, 42)  // Should return cached value
  assert_true(cached_time < 10)  // Should be much faster
}

// Test 3: I/O Performance Optimization Tests
test "io performance optimization operations" {
  // Test buffered I/O
  let test_data = "Test data for buffered I/O operations. ".repeat(1000)
  
  // Unbuffered write
  let start_time = TimeUtil::current_time_millis()
  IOUtil::write_file("unbuffered_test.txt", test_data, false)
  let unbuffered_time = TimeUtil::current_time_millis() - start_time
  
  // Buffered write
  start_time = TimeUtil::current_time_millis()
  IOUtil::write_file("buffered_test.txt", test_data, true)
  let buffered_time = TimeUtil::current_time_millis() - start_time
  
  // Buffered should be faster for large data
  assert_true(buffered_time <= unbuffered_time)
  
  // Test batch operations
  let batch_size = 100
  let batch_data = ArrayUtil::range(1, 1000)
  
  // Individual operations
  start_time = TimeUtil::current_time_millis()
  for item in batch_data {
    DatabaseUtil::insert("test_table", item)
  }
  let individual_time = TimeUtil::current_time_millis() - start_time
  
  // Batch operations
  start_time = TimeUtil::current_time_millis()
  DatabaseUtil::batch_insert("test_table", batch_data, batch_size)
  let batch_time = TimeUtil::current_time_millis() - start_time
  
  // Batch should be significantly faster
  assert_true(batch_time < individual_time)
  
  // Test async I/O
  let async_tasks = []
  for i in 0..10 {
    let task = AsyncUtil::run(() -> {
      IOUtil::read_file("test_file_" + i.to_string() + ".txt")
    })
    ArrayUtil::push(async_tasks, task)
  }
  
  start_time = TimeUtil::current_time_millis()
  let async_results = AsyncUtil::wait_all(async_tasks)
  let async_time = TimeUtil::current_time_millis() - start_time
  
  // Sequential reading
  start_time = TimeUtil::current_time_millis()
  let sequential_results = []
  for i in 0..10 {
    let result = IOUtil::read_file("test_file_" + i.to_string() + ".txt")
    ArrayUtil::push(sequential_results, result)
  }
  let sequential_time = TimeUtil::current_time_millis() - start_time
  
  // Async should be faster for multiple I/O operations
  assert_true(async_time < sequential_time)
}

// Test 4: Algorithm Performance Optimization Tests
test "algorithm performance optimization operations" {
  // Test efficient sorting
  let large_array = ArrayUtil::shuffle(ArrayUtil::range(1, 10000))
  
  // Standard sort
  let start_time = TimeUtil::current_time_millis()
  let standard_sorted = ArrayUtil::sort(large_array)
  let standard_time = TimeUtil::current_time_millis() - start_time
  
  // Optimized sort
  start_time = TimeUtil::current_time_millis()
  let optimized_sorted = SortUtil::optimized_sort(large_array)
  let optimized_time = TimeUtil::current_time_millis() - start_time
  
  // Verify both produce correct results
  assert_eq(standard_sorted.length(), optimized_sorted.length())
  for i in 0..standard_sorted.length() {
    assert_eq(standard_sorted[i], optimized_sorted[i])
  }
  
  // Optimized should be faster
  assert_true(optimized_time <= standard_time)
  
  // Test efficient search
  let sorted_array = ArrayUtil::sort(ArrayUtil::range(1, 10000))
  let target = 5000
  
  // Linear search
  start_time = TimeUtil::current_time_millis()
  let linear_result = SearchUtil::linear_search(sorted_array, target)
  let linear_time = TimeUtil::current_time_millis() - start_time
  
  // Binary search
  start_time = TimeUtil::current_time_millis()
  let binary_result = SearchUtil::binary_search(sorted_array, target)
  let binary_time = TimeUtil::current_time_millis() - start_time
  
  // Verify both find the target
  match linear_result {
    Some(index) => assert_eq(sorted_array[index], target)
    None => assert_true(false)
  }
  
  match binary_result {
    Some(index) => assert_eq(sorted_array[index], target)
    None => assert_true(false)
  }
  
  // Binary search should be much faster
  assert_true(binary_time < linear_time)
  
  // Test efficient data structure operations
  let list_data = ArrayUtil::range(1, 1000)
  
  // List operations
  start_time = TimeUtil::current_time_millis()
  let list = ListUtil::from_array(list_data)
  let list_result = ListUtil::find(list, 500)
  let list_time = TimeUtil::current_time_millis() - start_time
  
  // Hash set operations
  start_time = TimeUtil::current_time_millis()
  let hash_set = HashSetUtil::from_array(list_data)
  let hash_result = HashSetUtil::contains(hash_set, 500)
  let hash_time = TimeUtil::current_time_millis() - start_time
  
  // Verify both find the element
  match list_result {
    Some(value) => assert_eq(value, 500)
    None => assert_true(false)
  }
  
  assert_true(hash_result)
  
  // Hash set should be faster for lookups
  assert_true(hash_time < list_time)
}

// Test 5: Network Performance Optimization Tests
test "network performance optimization operations" {
  // Test connection pooling
  let connection_pool = ConnectionPool::new("http://example.com", 10)
  
  // Create multiple requests
  let start_time = TimeUtil::current_time_millis()
  let pooled_results = []
  for i in 0..20 {
    let connection = ConnectionPool::acquire(connection_pool)
    let result = HttpClientUtil::get(connection, "/api/data/" + i.to_string())
    ConnectionPool::release(connection_pool, connection)
    ArrayUtil::push(pooled_results, result)
  }
  let pooled_time = TimeUtil::current_time_millis() - start_time
  
  // Direct connections
  start_time = TimeUtil::current_time_millis()
  let direct_results = []
  for i in 0..20 {
    let connection = HttpClientUtil::create_connection("http://example.com")
    let result = HttpClientUtil::get(connection, "/api/data/" + i.to_string())
    HttpClientUtil::close_connection(connection)
    ArrayUtil::push(direct_results, result)
  }
  let direct_time = TimeUtil::current_time_millis() - start_time
  
  // Connection pooling should be faster
  assert_true(pooled_time < direct_time)
  
  // Test request batching
  let batch_requests = [
    "/api/data/1",
    "/api/data/2",
    "/api/data/3",
    "/api/data/4",
    "/api/data/5"
  ]
  
  // Individual requests
  start_time = TimeUtil::current_time_millis()
  let individual_responses = []
  for request in batch_requests {
    let response = HttpClientUtil::get("http://example.com" + request)
    ArrayUtil::push(individual_responses, response)
  }
  let individual_time = TimeUtil::current_time_millis() - start_time
  
  // Batch request
  start_time = TimeUtil::current_time_millis()
  let batch_response = HttpClientUtil::batch_get("http://example.com", batch_requests)
  let batch_time = TimeUtil::current_time_millis() - start_time
  
  // Batch should be faster
  assert_true(batch_time < individual_time)
  
  // Test compression
  let large_payload = "Large payload data for compression testing. ".repeat(1000)
  
  // Uncompressed request
  start_time = TimeUtil::current_time_millis()
  let uncompressed_response = HttpClientUtil::post("http://example.com/api/data", large_payload, false)
  let uncompressed_time = TimeUtil::current_time_millis() - start_time
  
  // Compressed request
  start_time = TimeUtil::current_time_millis()
  let compressed_response = HttpClientUtil::post("http://example.com/api/data", large_payload, true)
  let compressed_time = TimeUtil::current_time_millis() - start_time
  
  // Compressed should be faster for large payloads
  assert_true(compressed_time < uncompressed_time)
}

// Test 6: Database Performance Optimization Tests
test "database performance optimization operations" {
  // Test query optimization
  let db_connection = DatabaseUtil::connect("test.db")
  
  // Create test table
  DatabaseUtil::execute(db_connection, "CREATE TABLE test_data (id INTEGER, value TEXT, timestamp INTEGER)")
  
  // Insert test data
  let test_records = []
  for i in 0..1000 {
    ArrayUtil::push(test_records, (i, "value_" + i.to_string(), TimeUtil::current_time_millis() + i))
  }
  
  // Batch insert
  let start_time = TimeUtil::current_time_millis()
  DatabaseUtil::batch_insert_records(db_connection, "test_data", test_records)
  let batch_time = TimeUtil::current_time_millis() - start_time
  
  // Individual inserts
  DatabaseUtil::execute(db_connection, "DELETE FROM test_data")
  start_time = TimeUtil::current_time_millis()
  for record in test_records {
    DatabaseUtil::insert_record(db_connection, "test_data", record)
  }
  let individual_time = TimeUtil::current_time_millis() - start_time
  
  // Batch should be significantly faster
  assert_true(batch_time < individual_time)
  
  // Test index optimization
  // Query without index
  start_time = TimeUtil::current_time_millis()
  let no_index_results = DatabaseUtil::query(db_connection, "SELECT * FROM test_data WHERE value = 'value_500'")
  let no_index_time = TimeUtil::current_time_millis() - start_time
  
  // Create index
  DatabaseUtil::execute(db_connection, "CREATE INDEX idx_value ON test_data(value)")
  
  // Query with index
  start_time = TimeUtil::current_time_millis()
  let indexed_results = DatabaseUtil::query(db_connection, "SELECT * FROM test_data WHERE value = 'value_500'")
  let indexed_time = TimeUtil::current_time_millis() - start_time
  
  // Indexed query should be faster
  assert_true(indexed_time < no_index_time)
  
  // Verify results are identical
  assert_eq(no_index_results.length(), indexed_results.length())
  
  // Test prepared statements
  let prepared_statement = DatabaseUtil::prepare(db_connection, "SELECT * FROM test_data WHERE id = ?")
  
  // Direct query
  start_time = TimeUtil::current_time_millis()
  for i in 0..100 {
    DatabaseUtil::query(db_connection, "SELECT * FROM test_data WHERE id = " + i.to_string())
  }
  let direct_time = TimeUtil::current_time_millis() - start_time
  
  // Prepared statement
  start_time = TimeUtil::current_time_millis()
  for i in 0..100 {
    DatabaseUtil::execute_prepared(prepared_statement, [i])
  }
  let prepared_time = TimeUtil::current_time_millis() - start_time
  
  // Prepared statement should be faster
  assert_true(prepared_time < direct_time)
  
  DatabaseUtil::close(db_connection)
}

// Test 7: Caching Performance Optimization Tests
test "caching performance optimization operations" {
  // Test multi-level caching
  let l1_cache = LRUCache::new(100)  // Memory cache
  let l2_cache = DiskCache::new("/tmp/cache", 1000)  // Disk cache
  let multi_cache = MultiLevelCache::new([l1_cache, l2_cache])
  
  // Cache expensive computation
  let computation_key = "expensive_computation"
  let start_time = TimeUtil::current_time_millis()
  let result1 = MultiLevelCache::get_or_compute(multi_cache, computation_key, () -> {
    TimeUtil::sleep(100)  // Simulate expensive computation
    "computed_value"
  })
  let first_time = TimeUtil::current_time_millis() - start_time
  
  // Second call should use L1 cache
  start_time = TimeUtil::current_time_millis()
  let result2 = MultiLevelCache::get_or_compute(multi_cache, computation_key, () -> {
    TimeUtil::sleep(100)
    "should_not_compute"
  })
  let second_time = TimeUtil::current_time_millis() - start_time
  
  // Verify results and performance
  assert_eq(result1, "computed_value")
  assert_eq(result2, "computed_value")
  assert_true(first_time > 100)  // Should include sleep time
  assert_true(second_time < 10)   // Should be very fast from cache
  
  // Test cache warming
  let cache = LRUCache::new(1000)
  let warm_keys = ["key1", "key2", "key3", "key4", "key5"]
  
  // Cold cache
  start_time = TimeUtil::current_time_millis()
  for key in warm_keys {
    CacheUtil::get_or_compute(cache, key, () -> {
      TimeUtil::sleep(10)
      "value_for_" + key
    })
  }
  let cold_time = TimeUtil::current_time_millis() - start_time
  
  // Warm cache
  start_time = TimeUtil::current_time_millis()
  for key in warm_keys {
    CacheUtil::get_or_compute(cache, key, () -> {
      TimeUtil::sleep(10)
      "should_not_compute"
    })
  }
  let warm_time = TimeUtil::current_time_millis() - start_time
  
  // Warm cache should be much faster
  assert_true(warm_time < cold_time)
  
  // Test cache eviction strategies
  let fifo_cache = FIFOCache::new(5)
  let lru_cache = LRUCache::new(5)
  
  // Fill both caches
  for i in 0..5 {
    CacheUtil::put(fifo_cache, "key" + i.to_string(), "value" + i.to_string())
    CacheUtil::put(lru_cache, "key" + i.to_string(), "value" + i.to_string())
  }
  
  // Access some items in LRU cache to update their "recently used" status
  CacheUtil::get(lru_cache, "key1")
  CacheUtil::get(lru_cache, "key2")
  
  // Add one more item to trigger eviction
  CacheUtil::put(fifo_cache, "key6", "value6")
  CacheUtil::put(lru_cache, "key6", "value6")
  
  // FIFO should evict key0 (first inserted)
  assert_false(CacheUtil::contains(fifo_cache, "key0"))
  assert_true(CacheUtil::contains(fifo_cache, "key1"))
  
  // LRU should evict key3 (least recently used)
  assert_true(CacheUtil::contains(lru_cache, "key1"))
  assert_true(CacheUtil::contains(lru_cache, "key2"))
  assert_false(CacheUtil::contains(lru_cache, "key3"))
}

// Test 8: Resource Pool Performance Optimization Tests
test "resource pool performance optimization operations" {
  // Test thread pool
  let thread_pool = ThreadPool::new(4)
  let tasks = []
  
  // Create CPU-intensive tasks
  for i in 0..20 {
    let task = ThreadPoolUtil::submit(thread_pool, () -> {
      // Simulate CPU-intensive work
      let mut result = 0
      for j in 0..1000000 {
        result = result + j
      }
      result
    })
    ArrayUtil::push(tasks, task)
  }
  
  // Wait for all tasks to complete
  let start_time = TimeUtil::current_time_millis()
  let results = []
  for task in tasks {
    let result = ThreadPoolUtil::get_result(task)
    ArrayUtil::push(results, result)
  }
  let thread_pool_time = TimeUtil::current_time_millis() - start_time
  
  // Sequential execution
  start_time = TimeUtil::current_time_millis()
  let sequential_results = []
  for i in 0..20 {
    let mut result = 0
    for j in 0..1000000 {
      result = result + j
    }
    ArrayUtil::push(sequential_results, result)
  }
  let sequential_time = TimeUtil::current_time_millis() - start_time
  
  // Thread pool should be faster for CPU-intensive tasks
  assert_true(thread_pool_time < sequential_time)
  
  // Verify results are identical
  assert_eq(results.length(), sequential_results.length())
  for i in 0..results.length() {
    assert_eq(results[i], sequential_results[i])
  }
  
  // Test connection pool
  let db_pool = ConnectionPool::new("database_url", 10)
  
  // Pool connections
  start_time = TimeUtil::current_time_millis()
  let pool_results = []
  for i in 0..20 {
    let connection = ConnectionPool::acquire(db_pool)
    let result = DatabaseUtil::query(connection, "SELECT * FROM test_table WHERE id = " + i.to_string())
    ConnectionPool::release(db_pool, connection)
    ArrayUtil::push(pool_results, result)
  }
  let pool_time = TimeUtil::current_time_millis() - start_time
  
  // Direct connections
  start_time = TimeUtil::current_time_millis()
  let direct_results = []
  for i in 0..20 {
    let connection = DatabaseUtil::connect("database_url")
    let result = DatabaseUtil::query(connection, "SELECT * FROM test_table WHERE id = " + i.to_string())
    DatabaseUtil::close(connection)
    ArrayUtil::push(direct_results, result)
  }
  let direct_time = TimeUtil::current_time_millis() - start_time
  
  // Connection pool should be faster
  assert_true(pool_time < direct_time)
}

// Test 9: Lazy Loading Performance Optimization Tests
test "lazy loading performance optimization operations" {
  // Test lazy evaluation
  let expensive_computation = LazyUtil::lazy(() -> {
    // Simulate expensive computation
    TimeUtil::sleep(100)
    42
  })
  
  // Lazy value should not be computed yet
  assert_false(LazyUtil::is_computed(expensive_computation))
  
  // First access should trigger computation
  let start_time = TimeUtil::current_time_millis()
  let result1 = LazyUtil::get(expensive_computation)
  let first_access_time = TimeUtil::current_time_millis() - start_time
  
  // Value should be computed now
  assert_true(LazyUtil::is_computed(expensive_computation))
  assert_eq(result1, 42)
  assert_true(first_access_time >= 100)
  
  // Second access should be instant
  start_time = TimeUtil::current_time_millis()
  let result2 = LazyUtil::get(expensive_computation)
  let second_access_time = TimeUtil::current_time_millis() - start_time
  
  assert_eq(result2, 42)
  assert_true(second_access_time < 10)
  
  // Test lazy collection
  let lazy_collection = LazyCollectionUtil::create(() -> {
    // Simulate expensive collection creation
    TimeUtil::sleep(50)
    ArrayUtil::range(1, 1000)
  })
  
  // Collection should not be created yet
  assert_false(LazyCollectionUtil::is_created(lazy_collection))
  
  // First access should trigger creation
  start_time = TimeUtil::current_time_millis()
  let length1 = LazyCollectionUtil::length(lazy_collection)
  let first_access_time = TimeUtil::current_time_millis() - start_time
  
  assert_true(LazyCollectionUtil::is_created(lazy_collection))
  assert_eq(length1, 1000)
  assert_true(first_access_time >= 50)
  
  // Second access should be instant
  start_time = TimeUtil::current_time_millis()
  let length2 = LazyCollectionUtil::length(lazy_collection)
  let second_access_time = TimeUtil::current_time_millis() - start_time
  
  assert_eq(length2, 1000)
  assert_true(second_access_time < 10)
  
  // Test lazy property loading
  let lazy_object = LazyObject::new()
  
  // Properties should not be loaded yet
  assert_false(LazyObject::is_property_loaded(lazy_object, "expensive_property"))
  
  // First access should trigger loading
  start_time = TimeUtil::current_time_millis()
  let property1 = LazyObject::get_property(lazy_object, "expensive_property")
  let first_property_time = TimeUtil::current_time_millis() - start_time
  
  assert_true(LazyObject::is_property_loaded(lazy_object, "expensive_property"))
  assert_eq(property1, "expensive_value")
  assert_true(first_property_time >= 30)
  
  // Second access should be instant
  start_time = TimeUtil::current_time_millis()
  let property2 = LazyObject::get_property(lazy_object, "expensive_property")
  let second_property_time = TimeUtil::current_time_millis() - start_time
  
  assert_eq(property2, "expensive_value")
  assert_true(second_property_time < 10)
}

// Test 10: Profiling and Monitoring Performance Tests
test "profiling and monitoring performance operations" {
  // Test performance profiling
  let profiler = Profiler::new()
  
  Profiler::start(profiler, "test_operation")
  
  // Simulate some work
  TimeUtil::sleep(50)
  
  Profiler::end(profiler, "test_operation")
  
  let profile_data = Profiler::get_profile_data(profiler)
  let operation_time = ProfileDataUtil::get_operation_time(profile_data, "test_operation")
  
  assert_true(operation_time >= 50)
  
  // Test performance counters
  let counters = PerformanceCounters::new()
  
  // Increment counters
  for i in 0..100 {
    PerformanceCounters::increment(counters, "operation_count")
    if i % 2 == 0 {
      PerformanceCounters::increment(counters, "even_operation_count")
    }
  }
  
  assert_eq(PerformanceCounters::get(counters, "operation_count"), 100)
  assert_eq(PerformanceCounters::get(counters, "even_operation_count"), 50)
  
  // Test performance monitoring
  let monitor = PerformanceMonitor::new()
  
  PerformanceMonitor::start_monitoring(monitor)
  
  // Simulate monitored operations
  for i in 0..10 {
    PerformanceMonitor::record_operation(monitor, "test_operation", 10 + i)
  }
  
  PerformanceMonitor::stop_monitoring(monitor)
  
  let metrics = PerformanceMonitor::get_metrics(monitor)
  let avg_time = MetricsUtil::get_average(metrics, "test_operation")
  let min_time = MetricsUtil::get_minimum(metrics, "test_operation")
  let max_time = MetricsUtil::get_maximum(metrics, "test_operation")
  
  assert_eq(avg_time, 14.5)  // Average of 10-19
  assert_eq(min_time, 10.0)
  assert_eq(max_time, 19.0)
  
  // Test performance alerts
  let alert_manager = AlertManager::new()
  
  // Set up alert threshold
  AlertManager::set_threshold(alert_manager, "response_time", 100.0)
  
  // Record normal operation
  AlertManager::record_metric(alert_manager, "response_time", 50.0)
  assert_false(AlertManager::has_alerts(alert_manager))
  
  // Record slow operation
  AlertManager::record_metric(alert_manager, "response_time", 150.0)
  assert_true(AlertManager::has_alerts(alert_manager))
  
  let alerts = AlertManager::get_alerts(alert_manager)
  assert_eq(alerts.length(), 1)
  assert_eq(alerts[0].metric_name, "response_time")
  assert_eq(alerts[0].value, 150.0)
  assert_eq(alerts[0].threshold, 100.0)
}