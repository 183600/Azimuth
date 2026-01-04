// Azimuth Telemetry System - Performance Optimization Tests
// This file contains test cases for performance optimization features

// Test 1: Memory Pool Management and Object Pooling
test "memory pool management and object pooling" {
  let memory_manager = MemoryPoolManager::new()
  
  // Configure memory pools
  let pool_config = MemoryPoolConfig::new()
    .with_small_object_pool(1024, 1000) // 1KB objects, 1000 pool size
    .with_medium_object_pool(8192, 500)  // 8KB objects, 500 pool size
    .with_large_object_pool(65536, 100)  // 64KB objects, 100 pool size
    .with_preallocate_pools(true)
    .with_auto_expansion(true)
    .with_max_expansion_factor(2.0)
  
  MemoryPoolManager::configure(memory_manager, pool_config)
  
  // Test object pooling for telemetry spans
  let span_pool = MemoryPoolManager::get_span_pool(memory_manager)
  
  // Acquire spans from pool
  let mut acquired_spans = []
  for i in 0..=50 {
    let span = MemoryPoolManager::acquire_span(span_pool)
    match span {
      Some(pooled_span) => {
        // Initialize the span
        Span::initialize(pooled_span, "test_span_" + i.to_string(), Internal)
        acquired_spans.push(pooled_span)
      }
      None => assert_true(false) // Should not run out of pool objects
    }
  }
  
  // Verify spans are properly initialized
  for span in acquired_spans {
    assert_true(Span::is_initialized(span))
    assert_true(Span::name(span).starts_with("test_span_"))
  }
  
  // Return spans to pool
  for span in acquired_spans {
    MemoryPoolManager::release_span(span_pool, span)
  }
  
  // Verify pool statistics
  let pool_stats = MemoryPoolManager::get_pool_statistics(span_pool)
  assert_true(PoolStats::acquired_count(pool_stats) >= 50)
  assert_true(PoolStats::released_count(pool_stats) >= 50)
  assert_true(PoolStats::available_objects(pool_stats) > 0)
  
  // Test memory buffer pooling
  let buffer_pool = MemoryPoolManager::get_buffer_pool(memory_manager)
  
  // Acquire buffers of different sizes
  let small_buffer = MemoryPoolManager::acquire_buffer(buffer_pool, 512)   // Should use small pool
  let medium_buffer = MemoryPoolManager::acquire_buffer(buffer_pool, 4096) // Should use medium pool
  let large_buffer = MemoryPoolManager::acquire_buffer(buffer_pool, 32768) // Should use large pool
  
  match (small_buffer, medium_buffer, large_buffer) {
    (Some(small), Some(medium), Some(large)) => {
      assert_true(Buffer::capacity(small) >= 512)
      assert_true(Buffer::capacity(medium) >= 4096)
      assert_true(Buffer::capacity(large) >= 32768)
      
      // Use buffers
      Buffer::write_string(small, "Small buffer test data")
      Buffer::write_string(medium, "Medium buffer test data with more content")
      Buffer::write_string(large, "Large buffer test data with even more content to fill the buffer")
      
      // Return buffers to pool
      MemoryPoolManager::release_buffer(buffer_pool, small)
      MemoryPoolManager::release_buffer(buffer_pool, medium)
      MemoryPoolManager::release_buffer(buffer_pool, large)
    }
    _ => assert_true(false)
  }
  
  // Test pool expansion under load
  let expansion_span_pool = MemoryPoolManager::get_span_pool(memory_manager)
  let initial_capacity = PoolStats::total_capacity(MemoryPoolManager::get_pool_statistics(expansion_span_pool))
  
  // Acquire more objects than initial pool size
  let mut expansion_spans = []
  for i in 0..=1200 { // More than initial pool size of 1000
    let span = MemoryPoolManager::acquire_span(expansion_span_pool)
    match span {
      Some(pooled_span) => {
        Span::initialize(pooled_span, "expansion_span_" + i.to_string(), Internal)
        expansion_spans.push(pooled_span)
      }
      None => assert_true(false) // Should expand pool rather than fail
    }
  }
  
  // Verify pool expanded
  let expanded_capacity = PoolStats::total_capacity(MemoryPoolManager::get_pool_statistics(expansion_span_pool))
  assert_true(expanded_capacity > initial_capacity)
  assert_true(expanded_capacity <= initial_capacity * 2) // Should not exceed max expansion factor
  
  // Clean up
  for span in expansion_spans {
    MemoryPoolManager::release_span(expansion_span_pool, span)
  }
  
  // Test memory usage metrics
  let memory_metrics = MemoryPoolManager::get_memory_metrics(memory_manager)
  assert_true(MemoryMetrics::total_allocated(memory_metrics) > 0)
  assert_true(MemoryMetrics::pool_efficiency(memory_metrics) > 0.8) // Should be highly efficient
  assert_true(MemoryMetrics::gc_pressure(memory_metrics) < 0.1) // Should minimize GC pressure
}

// Test 2: CPU-Affinity and Thread Pool Optimization
test "cpu-affinity and thread pool optimization" {
  let thread_manager = ThreadPoolManager::new()
  
  // Configure thread pools with CPU affinity
  let thread_config = ThreadPoolConfig::new()
    .with_io_threads(4)
    .with_compute_threads(8)
    .with_scheduler_threads(2)
    .with_cpu_affinity(true)
    .with_work_stealing(true)
    .with_min_threads(2)
    .with_max_threads(16)
    .with_thread_idle_timeout(Duration::seconds(30))
  
  ThreadPoolManager::configure(thread_manager, thread_config)
  
  // Test CPU-intensive work distribution
  let compute_pool = ThreadPoolManager::get_compute_pool(thread_manager)
  
  let compute_tasks = []
  let num_tasks = 100
  
  for i in 0..num_tasks {
    let task = async {
      // Simulate CPU-intensive work
      let mut result = 0
      for j in 0..=10000 {
        result = result + (i * j) % 1000
      }
      return result
    }
    compute_tasks.push(task)
  }
  
  // Execute tasks in parallel
  let start_time = Time::now()
  let results = ThreadPoolManager::execute_all(compute_pool, compute_tasks)
  let end_time = Time::now()
  
  // Verify all tasks completed
  assert_eq(results.length(), num_tasks)
  
  // Verify results are correct
  for i in 0..num_tasks {
    let mut expected = 0
    for j in 0..=10000 {
      expected = expected + (i * j) % 1000
    }
    assert_eq(results[i], expected)
  }
  
  // Verify performance (should be much faster than sequential)
  let parallel_time = end_time - start_time
  
  // Calculate sequential time for comparison
  let sequential_start = Time::now()
  for i in 0..num_tasks {
    let mut result = 0
    for j in 0..=10000 {
      result = result + (i * j) % 1000
    }
  }
  let sequential_end = Time::now()
  let sequential_time = sequential_end - sequential_start
  
  // Parallel should be significantly faster
  assert_true(parallel_time < sequential_time * 0.5) // At least 2x speedup
  
  // Test I/O-intensive work distribution
  let io_pool = ThreadPoolManager::get_io_pool(thread_manager)
  
  let io_tasks = []
  let num_io_tasks = 20
  
  for i in 0..num_io_tasks {
    let task = async {
      // Simulate I/O-intensive work
      Time::sleep(100) // 100ms delay
      return "io_result_" + i.to_string()
    }
    io_tasks.push(task)
  }
  
  // Execute I/O tasks
  let io_start_time = Time::now()
  let io_results = ThreadPoolManager::execute_all(io_pool, io_tasks)
  let io_end_time = Time::now()
  
  // Verify all I/O tasks completed
  assert_eq(io_results.length(), num_io_tasks)
  
  // I/O tasks should complete in approximately the time of a single task
  // due to parallel execution
  let io_parallel_time = io_end_time - io_start_time
  assert_true(io_parallel_time < 2000) // Much less than 20 * 100ms = 2000ms
  
  // Test CPU affinity
  let cpu_affinity_enabled = ThreadPoolManager::is_cpu_affinity_enabled(thread_manager)
  assert_true(cpu_affinity_enabled)
  
  let cpu_usage = ThreadPoolManager::get_cpu_usage_by_thread(thread_manager)
  assert_true(cpu_usage.length() > 0)
  
  // Verify work distribution across cores
  let active_cores = cpu_usage.filter(|usage| usage > 0.1).length()
  assert_true(active_cores >= 4) // Should use multiple cores
  
  // Test thread pool metrics
  let pool_metrics = ThreadPoolManager::get_pool_metrics(thread_manager)
  assert_true(PoolMetrics::active_threads(pool_metrics) > 0)
  assert_true(PoolMetrics::completed_tasks(pool_metrics) >= num_tasks + num_io_tasks)
  assert_true(PoolMetrics::queue_length(pool_metrics) == 0) // All tasks completed
  assert_true(PoolMetrics::cpu_utilization(pool_metrics) > 0.5) // Good CPU utilization
}

// Test 3: Lock-Free Data Structures for High Throughput
test "lock-free data structures for high throughput" {
  let lockfree_manager = LockFreeDataManager::new()
  
  // Test lock-free queue for high-throughput telemetry processing
  let telemetry_queue = LockFreeDataManager::create_queue(lockfree_manager, 10000)
  
  // Producer threads
  let num_producers = 4
  let items_per_producer = 2500
  let producer_futures = []
  
  for producer_id in 0..num_producers {
    let future = async {
      for i in 0..items_per_producer {
        let telemetry_event = TelemetryEvent::new("high_throughput_test")
          .with_producer_id(producer_id)
          .with_sequence_id(i)
          .with_timestamp(Time::now())
          .with_metric("value", Math::random() * 100.0)
        
        let success = LockFreeQueue::enqueue(telemetry_queue, telemetry_event)
        assert_true(success)
      }
    }
    producer_futures.push(future)
  }
  
  // Consumer thread
  let consumer_future = async {
    let mut consumed_count = 0
    let mut values = []
    
    while consumed_count < num_producers * items_per_producer {
      match LockFreeQueue::dequeue(telemetry_queue) {
        Some(event) => {
          consumed_count = consumed_count + 1
          values.push(TelemetryEvent::get_metric(event, "value"))
        }
        None => Time::sleep(1) // Brief pause if queue is empty
      }
    }
    
    return (consumed_count, values)
  }
  
  // Execute producers and consumer
  let start_time = Time::now()
  Future::wait_all(producer_futures) |> ignore
  let (consumed_count, values) = Future::wait(consumer_future)
  let end_time = Time::now()
  
  // Verify all items were consumed
  assert_eq(consumed_count, num_producers * items_per_producer)
  assert_eq(values.length(), num_producers * items_per_producer)
  
  // Verify throughput
  let total_time = end_time - start_time
  let throughput = (num_producers * items_per_producer).to_float() / (total_time.to_float() / 1000.0) // items per second
  assert_true(throughput > 10000) // Should achieve high throughput
  
  // Test lock-free map for concurrent attribute lookups
  let attribute_map = LockFreeDataManager::create_map(lockfree_manager, 1000)
  
  // Populate map
  for i in 0..=1000 {
    let key = "attr_" + i.to_string()
    let value = "value_" + i.to_string()
    LockFreeMap::put(attribute_map, key, value)
  }
  
  // Concurrent lookups
  let num_lookup_threads = 8
  let lookups_per_thread = 1000
  let lookup_futures = []
  
  for thread_id in 0..num_lookup_threads {
    let future = async {
      let mut found_count = 0
      let start_index = thread_id * 100
      
      for i in 0..lookups_per_thread {
        let key = "attr_" + ((start_index + i) % 1001).to_string()
        match LockFreeMap::get(attribute_map, key) {
          Some(_) => found_count = found_count + 1
          None => assert_true(false)
        }
      }
      
      return found_count
    }
    lookup_futures.push(future)
  }
  
  let lookup_start = Time::now()
  let lookup_results = Future::wait_all(lookup_futures)
  let lookup_end = Time::now()
  
  // Verify all lookups succeeded
  let total_found = lookup_results.reduce((acc, count) => acc + count)
  assert_eq(total_found, num_lookup_threads * lookups_per_thread)
  
  // Verify lookup performance
  let lookup_time = lookup_end - lookup_start
  let lookup_throughput = (num_lookup_threads * lookups_per_thread).to_float() / (lookup_time.to_float() / 1000.0)
  assert_true(lookup_throughput > 100000) // Should achieve very high lookup throughput
  
  // Test lock-free counter for metrics aggregation
  let counter = LockFreeDataManager::create_counter(lockfree_manager)
  
  // Concurrent increments
  let increment_futures = []
  let increments_per_thread = 10000
  
  for _ in 0..10 {
    let future = async {
      for _ in 0..increments_per_thread {
        LockFreeCounter::increment(counter)
      }
    }
    increment_futures.push(future)
  }
  
  Future::wait_all(increment_futures) |> ignore
  
  // Verify final count
  let final_count = LockFreeCounter::get(counter)
  assert_eq(final_count, 10 * increments_per_thread)
  
  // Test lock-free data structure metrics
  let structure_metrics = LockFreeDataManager::get_metrics(lockfree_manager)
  assert_true(StructureMetrics::total_operations(structure_metrics) > 0)
  assert_true(StructureMetrics::operation_latency_avg(structure_metrics) < 0.001) // Sub-millisecond latency
  assert_true(StructureMetrics::contention_rate(structure_metrics) < 0.01) // Very low contention
}

// Test 4: Adaptive Batching and Batch Optimization
test "adaptive batching and batch optimization" {
  let batch_manager = AdaptiveBatchManager::new()
  
  // Configure adaptive batching
  let batch_config = BatchConfig::new()
    .with_initial_batch_size(100)
    .with_min_batch_size(10)
    .with_max_batch_size(1000)
    .with_adaptive_sizing(true)
    .with_latency_threshold(Duration::milliseconds(100))
    .with_throughput_threshold(1000) // items per second
    .with_adjustment_factor(1.2)
    .with_measurement_window(Duration::seconds(5))
  
  AdaptiveBatchManager::configure(batch_manager, batch_config)
  
  // Test adaptive batch sizing under different load conditions
  let telemetry_processor = AdaptiveBatchManager::create_processor(batch_manager, |batch| {
    // Simulate processing time
    Time::sleep(batch.length() / 100) // 1ms per 100 items
    return batch.length()
  })
  
  // Low load scenario
  let low_load_start = Time::now()
  let low_load_items = 500
  
  for i in 0..low_load_items {
    let item = TelemetryItem::new("low_load_item_" + i.to_string())
    AdaptiveBatchManager::add_item(telemetry_processor, item)
    Time::sleep(10) // 10ms between items (low rate)
  }
  
  let low_load_result = AdaptiveBatchManager::flush(telemetry_processor)
  let low_load_end = Time::now()
  
  match low_load_result {
    Success(processed_count) => {
      assert_eq(processed_count, low_load_items)
      
      // Check batch size adjustments
      let low_load_metrics = AdaptiveBatchManager::get_metrics(telemetry_processor)
      let low_load_batch_size = BatchMetrics::average_batch_size(low_load_metrics)
      
      // Batch size should be smaller for low load
      assert_true(low_load_batch_size < 200)
    }
    Error(_) => assert_true(false)
  }
  
  // High load scenario
  let high_load_start = Time::now()
  let high_load_items = 2000
  
  for i in 0..high_load_items {
    let item = TelemetryItem::new("high_load_item_" + i.to_string())
    AdaptiveBatchManager::add_item(telemetry_processor, item)
    // No delay between items (high rate)
  }
  
  let high_load_result = AdaptiveBatchManager::flush(telemetry_processor)
  let high_load_end = Time::now()
  
  match high_load_result {
    Success(processed_count) => {
      assert_eq(processed_count, high_load_items)
      
      // Check batch size adjustments
      let high_load_metrics = AdaptiveBatchManager::get_metrics(telemetry_processor)
      let high_load_batch_size = BatchMetrics::average_batch_size(high_load_metrics)
      
      // Batch size should be larger for high load
      assert_true(high_load_batch_size > low_load_batch_size)
    }
    Error(_) => assert_true(false)
  }
  
  // Verify performance improvements
  let low_load_time = low_load_end - low_load_start
  let high_load_time = high_load_end - high_load_start
  let low_load_throughput = low_load_items.to_float() / (low_load_time.to_float() / 1000.0)
  let high_load_throughput = high_load_items.to_float() / (high_load_time.to_float() / 1000.0)
  
  assert_true(high_load_throughput > low_load_throughput)
  
  // Test batch optimization based on item characteristics
  let optimized_processor = AdaptiveBatchManager::create_optimized_processor(batch_manager, |batch| {
    // Simulate different processing times for different item types
    let mut processing_time = 0
    for item in batch {
      match TelemetryItem::get_type(item) {
        "simple" => processing_time = processing_time + 1,
        "complex" => processing_time = processing_time + 5,
        "heavy" => processing_time = processing_time + 10,
        _ => processing_time = processing_time + 2
      }
    }
    
    Time::sleep(processing_time)
    return batch.length()
  })
  
  // Add mixed item types
  for i in 0..=300 {
    let item_type = match i % 4 {
      0 => "simple",
      1 => "complex",
      2 => "heavy",
      _ => "medium"
    }
    
    let item = TelemetryItem::new("optimized_item_" + i.to_string())
      .with_type(item_type)
    
    AdaptiveBatchManager::add_item(optimized_processor, item)
  }
  
  let optimized_result = AdaptiveBatchManager::flush(optimized_processor)
  match optimized_result {
    Success(processed_count) => {
      assert_eq(processed_count, 301)
      
      // Verify optimization metrics
      let optimized_metrics = AdaptiveBatchManager::get_metrics(optimized_processor)
      assert_true(BatchMetrics::optimization_score(optimized_metrics) > 0.7)
      assert_true(BatchMetrics::latency_improvement(optimized_metrics) > 0.1)
    }
    Error(_) => assert_true(false)
  }
}

// Test 5: Cache Optimization and Intelligent Caching
test "cache optimization and intelligent caching" {
  let cache_manager = IntelligentCacheManager::new()
  
  // Configure intelligent caching
  let cache_config = CacheConfig::new()
    .with_max_memory_usage(100 * 1024 * 1024) // 100MB
    .with_default_ttl(Duration::minutes(10))
    .with_eviction_policy(LFU) // Least Frequently Used
    .with_adaptive_ttl(true)
    .with_prefetching(true)
    .with_compression(true)
    .with_cache_partitions(16) // 16 cache partitions for parallelism
  
  IntelligentCacheManager::configure(cache_manager, cache_config)
  
  // Test cache with different access patterns
  let telemetry_cache = IntelligentCacheManager::create_cache(cache_manager, "telemetry")
  
  // Hot data (frequently accessed)
  let hot_keys = []
  for i in 0..=100 {
    let key = "hot_key_" + i.to_string()
    let value = TelemetryData::new("hot_data_" + i.to_string())
      .with_size(1024) // 1KB each
      .with_access_frequency(High)
    
    IntelligentCache::put(telemetry_cache, key, value)
    hot_keys.push(key)
  }
  
  // Cold data (infrequently accessed)
  let cold_keys = []
  for i in 0..=100 {
    let key = "cold_key_" + i.to_string()
    let value = TelemetryData::new("cold_data_" + i.to_string())
      .with_size(1024) // 1KB each
      .with_access_frequency(Low)
    
    IntelligentCache::put(telemetry_cache, key, value)
    cold_keys.push(key)
  }
  
  // Simulate access patterns
  let access_start = Time::now()
  
  // Access hot data frequently
  for _ in 0..=10 {
    for key in hot_keys {
      match IntelligentCache::get(telemetry_cache, key) {
        Some(_) => assert_true(true),
        None => assert_true(false)
      }
    }
  }
  
  // Access cold data infrequently
  for key in cold_keys.slice(0, 10) {
    match IntelligentCache::get(telemetry_cache, key) {
      Some(_) => assert_true(true),
      None => assert_true(false)
    }
  }
  
  let access_end = Time::now()
  
  // Test cache hit rates
  let cache_metrics = IntelligentCache::get_metrics(telemetry_cache)
  assert_true(CacheMetrics::hit_rate(cache_metrics) > 0.9) // Should have high hit rate for hot data
  assert_true(CacheMetrics::miss_rate(cache_metrics) < 0.1)
  
  // Test adaptive TTL
  let hot_ttl = IntelligentCache::get_adaptive_ttl(telemetry_cache, hot_keys[0])
  let cold_ttl = IntelligentCache::get_adaptive_ttl(telemetry_cache, cold_keys[0])
  
  // Hot data should have longer TTL
  assert_true(hot_ttl > cold_ttl)
  
  // Test cache eviction under memory pressure
  let memory_usage_before = CacheMetrics::memory_usage(cache_metrics)
  
  // Add more data to trigger eviction
  for i in 0..=200 {
    let key = "pressure_key_" + i.to_string()
    let value = TelemetryData::new("pressure_data_" + i.to_string())
      .with_size(1024) // 1KB each
    
    IntelligentCache::put(telemetry_cache, key, value)
  }
  
  // Wait for eviction to complete
  Time::sleep(1000) // 1 second
  
  let metrics_after_pressure = IntelligentCache::get_metrics(telemetry_cache)
  let memory_usage_after = CacheMetrics::memory_usage(metrics_after_pressure)
  
  // Memory usage should be within limits
  assert_true(memory_usage_after <= 100 * 1024 * 1024)
  
  // Hot data should still be in cache (due to LFU policy)
  let hot_data_preserved = 0
  for key in hot_keys.slice(0, 20) {
    match IntelligentCache::get(telemetry_cache, key) {
      Some(_) => hot_data_preserved = hot_data_preserved + 1,
      None => ()
    }
  }
  
  // Most hot data should be preserved
  assert_true(hot_data_preserved > 15)
  
  // Cold data is more likely to be evicted
  let cold_data_preserved = 0
  for key in cold_keys.slice(0, 20) {
    match IntelligentCache::get(telemetry_cache, key) {
      Some(_) => cold_data_preserved = cold_data_preserved + 1,
      None => ()
    }
  }
  
  // Less cold data should be preserved
  assert_true(cold_data_preserved < hot_data_preserved)
  
  // Test cache compression
  let large_value = TelemetryData::new("large_compressed_data")
    .with_size(50 * 1024) // 50KB
    .with_compressible_content("x".repeat(50 * 1024)) // Highly compressible content
  
  IntelligentCache::put(telemetry_cache, "large_key", large_value)
  
  match IntelligentCache::get(telemetry_cache, "large_key") {
    Some(retrieved_value) => {
      assert_eq(TelemetryData::get_content(retrieved_value), TelemetryData::get_content(large_value))
      
      // Verify compression metrics
      let compression_metrics = IntelligentCache::get_compression_metrics(telemetry_cache)
      assert_true(CompressionMetrics::compression_ratio(compression_metrics) > 2.0) // At least 2x compression
    }
    None => assert_true(false)
  }
  
  // Test cache prefetching
  let prefetch_keys = ["prefetch_1", "prefetch_2", "prefetch_3"]
  
  for key in prefetch_keys {
    let value = TelemetryData::new("prefetch_data_" + key)
    IntelligentCache::put(telemetry_cache, key, value)
  }
  
  // Access first key to trigger prefetching
  IntelligentCache::get(telemetry_cache, "prefetch_1")
  
  // Wait for prefetching
  Time::sleep(500) // 500ms
  
  // Check if related keys were prefetched
  let prefetch_metrics = IntelligentCache::get_prefetch_metrics(telemetry_cache)
  assert_true(PrefetchMetrics::prefetch_attempts(prefetch_metrics) > 0)
  assert_true(PrefetchMetrics::prefetch_hit_rate(prefetch_metrics) > 0.5)
}

// Test 6: Lazy Loading and On-Demand Computation
test "lazy loading and on-demand computation" {
  let lazy_manager = LazyLoadingManager::new()
  
  // Configure lazy loading
  let lazy_config = LazyLoadingConfig::new()
    .with_computation_cache(true)
    .with_max_cached_computations(1000)
    .with_computation_timeout(Duration::seconds(30))
    .with_parallel_computation(true)
    .with_dependency_tracking(true)
  
  LazyLoadingManager::configure(lazy_manager, lazy_config)
  
  // Test lazy computation of expensive telemetry aggregations
  let telemetry_data = []
  
  // Generate test data
  for i in 0..=10000 {
    let data_point = TelemetryData::new("point_" + i.to_string())
      .with_metric("response_time", 100.0 + Math::random() * 500.0)
      .with_metric("error_rate", Math::random() * 0.1)
      .with_attribute("service", "service_" + (i % 10).to_string())
    
    telemetry_data.push(data_point)
  }
  
  // Define lazy computation functions
  let avg_response_time = LazyLoadingManager::create_computation(lazy_manager, "avg_response_time", || {
    let mut sum = 0.0
    let mut count = 0
    
    for data_point in telemetry_data {
      sum = sum + TelemetryData::get_metric(data_point, "response_time")
      count = count + 1
    }
    
    return sum / count.to_float()
  })
  
  let error_rate_by_service = LazyLoadingManager::create_computation(lazy_manager, "error_rate_by_service", || {
    let mut service_errors = Map::new()
    let mut service_counts = Map::new()
    
    for data_point in telemetry_data {
      let service = TelemetryData::get_attribute(data_point, "service")
      let error_rate = TelemetryData::get_metric(data_point, "error_rate")
      
      let current_errors = match Map::get(service_errors, service) {
        Some(value) => value,
        None => 0.0
      }
      
      let current_count = match Map::get(service_counts, service) {
        Some(value) => value,
        None => 0.0
      }
      
      Map::put(service_errors, service, current_errors + error_rate)
      Map::put(service_counts, service, current_count + 1.0)
    }
    
    let mut result = Map::new()
    let service_keys = Map::keys(service_errors)
    
    for service in service_keys {
      let errors = match Map::get(service_errors, service) {
        Some(value) => value,
        None => 0.0
      }
      
      let count = match Map::get(service_counts, service) {
        Some(value) => value,
        None => 1.0
      }
      
      Map::put(result, service, errors / count)
    }
    
    return result
  })
  
  let percentile_95 = LazyLoadingManager::create_computation(lazy_manager, "percentile_95", || {
    let mut response_times = []
    
    for data_point in telemetry_data {
      response_times.push(TelemetryData::get_metric(data_point, "response_time"))
    }
    
    response_times.sort()
    let index = (response_times.length() as Float * 0.95) as Int
    return response_times[index]
  })
  
  // Test that computations are not executed until accessed
  assert_false(LazyLoadingManager::is_computed(lazy_manager, "avg_response_time"))
  assert_false(LazyLoadingManager::is_computed(lazy_manager, "error_rate_by_service"))
  assert_false(LazyLoadingManager::is_computed(lazy_manager, "percentile_95"))
  
  // Access first computation
  let start_time = Time::now()
  let avg_result = LazyLoadingManager::get_computed_value(lazy_manager, "avg_response_time")
  let end_time = Time::now()
  
  match avg_result {
    Some(value) => {
      assert_true(value > 100.0 && value < 600.0)
      assert_true(LazyLoadingManager::is_computed(lazy_manager, "avg_response_time"))
      
      // Computation should take some time
      assert_true(end_time - start_time > 10) // At least 10ms
    }
    None => assert_true(false)
  }
  
  // Access the same computation again (should be cached)
  let cached_start = Time::now()
  let cached_result = LazyLoadingManager::get_computed_value(lazy_manager, "avg_response_time")
  let cached_end = Time::now()
  
  match cached_result {
    Some(value) => {
      assert_eq(value, avg_result.unwrap()) // Should be same result
      assert_true(cached_end - cached_start < 10) // Should be much faster (cached)
    }
    None => assert_true(false)
  }
  
  // Test parallel computation of independent values
  let parallel_start = Time::now()
  
  let error_rate_future = LazyLoadingManager::get_computed_value_async(lazy_manager, "error_rate_by_service")
  let percentile_future = LazyLoadingManager::get_computed_value_async(lazy_manager, "percentile_95")
  
  let error_rate_result = Future::wait(error_rate_future)
  let percentile_result = Future::wait(percentile_future)
  
  let parallel_end = Time::now()
  
  match (error_rate_result, percentile_result) {
    (Some(error_rates), Some(percentile)) => {
      assert_true(LazyLoadingManager::is_computed(lazy_manager, "error_rate_by_service"))
      assert_true(LazyLoadingManager::is_computed(lazy_manager, "percentile_95"))
      
      // Verify error rate results
      match Map::get(error_rates, "service_0") {
        Some(rate) => assert_true(rate >= 0.0 && rate <= 0.1),
        None => assert_true(false)
      }
      
      // Verify percentile result
      assert_true(percentile > 100.0 && percentile < 600.0)
      
      // Parallel execution should be faster than sequential
      assert_true(parallel_end - parallel_start < (end_time - start_time) * 1.5)
    }
    _ => assert_true(false)
  }
  
  // Test computation dependency tracking
  let dependent_computation = LazyLoadingManager::create_computation(lazy_manager, "dependent_metric", || {
    match LazyLoadingManager::get_computed_value(lazy_manager, "avg_response_time") {
      Some(avg) => return avg * 1.5,
      None => return 0.0
    }
  })
  
  // Invalidate dependency
  LazyLoadingManager::invalidate_computation(lazy_manager, "avg_response_time")
  
  assert_false(LazyLoadingManager::is_computed(lazy_manager, "avg_response_time"))
  assert_false(LazyLoadingManager::is_computed(lazy_manager, "dependent_metric"))
  
  // Access dependent computation (should recompute dependency)
  let dependent_result = LazyLoadingManager::get_computed_value(lazy_manager, "dependent_metric")
  match dependent_result {
    Some(value) => {
      assert_true(value > 0.0)
      assert_true(LazyLoadingManager::is_computed(lazy_manager, "avg_response_time"))
      assert_true(LazyLoadingManager::is_computed(lazy_manager, "dependent_metric"))
    }
    None => assert_true(false)
  }
  
  // Test lazy loading metrics
  let lazy_metrics = LazyLoadingManager::get_metrics(lazy_manager)
  assert_true(LazyMetrics::total_computations(lazy_metrics) >= 4)
  assert_true(LazyMetrics::cache_hit_rate(lazy_metrics) > 0.2)
  assert_true(LazyMetrics::parallel_computations(lazy_metrics) >= 1)
}

// Test 7: Resource-Aware Processing and Dynamic Scaling
test "resource-aware processing and dynamic scaling" {
  let resource_manager = ResourceManager::new()
  
  // Configure resource-aware processing
  let resource_config = ResourceConfig::new()
    .with_cpu_threshold(0.8) // 80% CPU threshold
    .with_memory_threshold(0.8) // 80% memory threshold
    .with_auto_scaling(true)
    .with_scale_up_factor(2.0)
    .with_scale_down_factor(0.5)
    .with_min_instances(1)
    .with_max_instances(8)
    .with_scale_up_cooldown(Duration::minutes(2))
    .with_scale_down_cooldown(Duration::minutes(5))
  
  ResourceManager::configure(resource_manager, resource_config)
  
  // Create resource-aware processor
  let processor = ResourceManager::create_processor(resource_manager, |data| {
    // Simulate processing workload
    Time::sleep(10) // 10ms per item
    return data.length()
  })
  
  // Test under normal load
  let normal_load_data = []
  for i in 0..=100 {
    normal_load_data.push("normal_item_" + i.to_string())
  }
  
  let normal_start = Time::now()
  let normal_result = ResourceManager::process_batch(processor, normal_load_data)
  let normal_end = Time::now()
  
  match normal_result {
    Success(count) => {
      assert_eq(count, 101)
      
      let normal_metrics = ResourceManager::get_metrics(processor)
      assert_true(ResourceMetrics::instance_count(normal_metrics) >= 1)
      assert_true(ResourceMetrics::cpu_utilization(normal_metrics) < 0.8)
      assert_true(ResourceMetrics::memory_utilization(normal_metrics) < 0.8)
    }
    Error(_) => assert_true(false)
  }
  
  // Test under high load (should trigger scale-up)
  let high_load_data = []
  for i in 0..=1000 {
    high_load_data.push("high_load_item_" + i.to_string())
  }
  
  let high_load_start = Time::now()
  let high_load_result = ResourceManager::process_batch(processor, high_load_data)
  let high_load_end = Time::now()
  
  match high_load_result {
    Success(count) => {
      assert_eq(count, 1001)
      
      let high_load_metrics = ResourceManager::get_metrics(processor)
      
      // Should have scaled up
      assert_true(ResourceMetrics::instance_count(high_load_metrics) > 1)
      assert_true(ResourceMetrics::scale_up_events(high_load_metrics) > 0)
      
      // Processing should be faster than single instance
      let high_load_time = high_load_end - high_load_start
      let estimated_single_time = 1001 * 10 // 10ms per item
      assert_true(high_load_time < estimated_single_time)
    }
    Error(_) => assert_true(false)
  }
  
  // Test scaling down after load reduction
  let low_load_data = []
  for i in 0..=10 {
    low_load_data.push("low_load_item_" + i.to_string())
  }
  
  // Process multiple low-load batches to trigger scale-down
  for _ in 0..=10 {
    ResourceManager::process_batch(processor, low_load_data) |> ignore
    Time::sleep(100) // 100ms between batches
  }
  
  // Wait for scale-down cooldown
  Time::sleep(500) // 500ms
  
  let low_load_metrics = ResourceManager::get_metrics(processor)
  
  // Should have scaled down (but not below minimum)
  assert_true(ResourceMetrics::instance_count(low_load_metrics) >= 1)
  assert_true(ResourceMetrics::scale_down_events(low_load_metrics) > 0)
  
  // Test resource-aware task scheduling
  let scheduler = ResourceManager::create_scheduler(resource_manager)
  
  // Create tasks with different resource requirements
  let cpu_intensive_task = Task::new("cpu_intensive")
    .with_cpu_requirement(High)
    .with_memory_requirement(Low)
    .with_workload(|| {
      // CPU-intensive work
      for i in 0..=10000 {
        Math::sqrt(i.to_float())
      }
      return "cpu_intensive_complete"
    })
  
  let memory_intensive_task = Task::new("memory_intensive")
    .with_cpu_requirement(Low)
    .with_memory_requirement(High)
    .with_workload(|| {
      // Memory-intensive work
      let large_array = []
      for i in 0..=10000 {
        large_array.push("large_string_data_" + i.to_string())
      }
      return "memory_intensive_complete"
    })
  
  let balanced_task = Task::new("balanced")
    .with_cpu_requirement(Medium)
    .with_memory_requirement(Medium)
    .with_workload(|| {
      // Balanced work
      let mut result = 0
      for i in 0..=1000 {
        result = result + i
      }
      return "balanced_complete_" + result.to_string()
    })
  
  // Schedule tasks
  let task_results = ResourceManager::schedule_tasks(scheduler, [
    cpu_intensive_task,
    memory_intensive_task,
    balanced_task
  ])
  
  assert_eq(task_results.length(), 3)
  
  for result in task_results {
    match result {
      Success(output) => assert_true(output.length() > 0),
      Error(_) => assert_true(false)
    }
  }
  
  // Verify resource-aware scheduling metrics
  let scheduling_metrics = ResourceManager::get_scheduling_metrics(scheduler)
  assert_true(SchedulingMetrics::tasks_scheduled(scheduling_metrics) >= 3)
  assert_true(SchedulingMetrics::resource_aware_decisions(scheduling_metrics) > 0)
  assert_true(SchedulingMetrics::load_balancing_score(scheduling_metrics) > 0.7)
}

// Test 8: Zero-Copy Optimization and Memory Efficiency
test "zero-copy optimization and memory efficiency" {
  let zerocopy_manager = ZeroCopyManager::new()
  
  // Configure zero-copy optimization
  let zerocopy_config = ZeroCopyConfig::new()
    .with_buffer_pooling(true)
    .with_shared_buffers(true)
    .with_memory_mapping(true)
    .with_min_buffer_size(4096) // 4KB
    .with_max_buffer_size(1048576) // 1MB
    .with_buffer_alignment(64) // 64-byte alignment
    .with_prefetch_buffers(true)
  
  ZeroCopyManager::configure(zerocopy_manager, zerocopy_config)
  
  // Test zero-copy data transfer
  let source_data = "Large telemetry data payload that needs to be processed efficiently without unnecessary memory copies. ".repeat(100)
  
  let source_buffer = ZeroCopyManager::create_buffer(zerocopy_manager, source_data.length())
  ZeroCopyBuffer::write_string(source_buffer, source_data)
  
  // Create multiple views of the same data without copying
  let view1 = ZeroCopyManager::create_view(zerocopy_manager, source_buffer, 0, 100)
  let view2 = ZeroCopyManager::create_view(zerocopy_manager, source_buffer, 50, 150)
  let view3 = ZeroCopyManager::create_view(zerocopy_manager, source_buffer, 100, 200)
  
  // Verify views reference the same underlying data
  assert_eq(ZeroCopyView::to_string(view1), source_data.slice(0, 100))
  assert_eq(ZeroCopyView::to_string(view2), source_data.slice(50, 150))
  assert_eq(ZeroCopyView::to_string(view3), source_data.slice(100, 200))
  
  // Verify memory efficiency (no additional allocations)
  let buffer_memory = ZeroCopyBuffer::memory_usage(source_buffer)
  let views_memory = ZeroCopyView::memory_usage(view1) + 
                    ZeroCopyView::memory_usage(view2) + 
                    ZeroCopyView::memory_usage(view3)
  
  // Views should use minimal additional memory
  assert_true(views_memory < buffer_memory * 0.1)
  
  // Test zero-copy data processing pipeline
  let pipeline = ZeroCopyManager::create_pipeline(zerocopy_manager)
  
  // Add processing stages
  ZeroCopyPipeline::add_stage(pipeline, "parse", |view| {
    // Parse telemetry data without copying
    let content = ZeroCopyView::to_string(view)
    let parts = content.split(" ")
    return parts.length()
  })
  
  ZeroCopyPipeline::add_stage(pipeline, "filter", |count| {
    // Filter based on parsed data
    return count > 10
  })
  
  ZeroCopyPipeline::add_stage(pipeline, "aggregate", |valid| {
    // Aggregate results
    return if valid { 1 } else { 0 }
  })
  
  // Process data through pipeline
  let pipeline_result = ZeroCopyPipeline::process(pipeline, view1)
  
  match pipeline_result {
    Success(result) => {
      assert_eq(result, 1) // Should be valid (more than 10 parts)
    }
    Error(_) => assert_true(false)
  }
  
  // Test shared buffer for concurrent processing
  let shared_buffer = ZeroCopyManager::create_shared_buffer(zerocopy_manager, 1024 * 1024) // 1MB
  
  // Write test data to shared buffer
  for i in 0..=100 {
    let offset = i * 10240 // 10KB per item
    let data = "shared_data_item_" + i.to_string() + " ".repeat(1000)
    ZeroCopySharedBuffer::write_string_at(shared_buffer, offset, data)
  }
  
  // Concurrent read from shared buffer
  let read_futures = []
  
  for i in 0..=10 {
    let future = async {
      let offset = (i * 10) * 10240 // Read every 10th item
      let view = ZeroCopySharedBuffer::create_view(shared_buffer, offset, 10240)
      let content = ZeroCopyView::to_string(view)
      return content.starts_with("shared_data_item_")
    }
    read_futures.push(future)
  }
  
  let read_results = Future::wait_all(read_futures)
  
  for result in read_results {
    assert_true(result)
  }
  
  // Test memory-mapped file operations
  let test_data = "Telemetry data for memory-mapped file testing. ".repeat(10000)
  let temp_file = "/tmp/telemetry_test.dat"
  
  // Write test data to file
  let file_result = ZeroCopyManager::write_to_file(zerocopy_manager, temp_file, test_data)
  match file_result {
    Success(_) => assert_true(true),
    Error(_) => assert_true(false)
  }
  
  // Memory-map the file
  let mapped_file = ZeroCopyManager::memory_map_file(zerocopy_manager, temp_file)
  match mapped_file {
    Success(mapped) => {
      // Verify file content without loading into memory
      let file_view = ZeroCopyMappedFile::create_view(mapped, 0, 100)
      let file_content = ZeroCopyView::to_string(file_view)
      assert_eq(file_content, test_data.slice(0, 100))
      
      // Verify memory efficiency
      let file_size = ZeroCopyMappedFile::file_size(mapped)
      let mapped_memory = ZeroCopyMappedFile::memory_usage(mapped)
      
      // Mapped memory should be much less than file size
      assert_true(mapped_memory < file_size * 0.1)
      
      // Clean up
      ZeroCopyMappedFile::unmap(mapped)
    }
    Error(_) => assert_true(false)
  }
  
  // Test zero-copy metrics
  let zerocopy_metrics = ZeroCopyManager::get_metrics(zerocopy_manager)
  assert_true(ZeroCopyMetrics::memory_saved(zerocopy_metrics) > 0)
  assert_true(ZeroCopyMetrics::copy_operations_avoided(zerocopy_metrics) > 0)
  assert_true(ZeroCopyMetrics::buffer_reuse_rate(zerocopy_metrics) > 0.8)
  assert_true(ZeroCopyMetrics::memory_efficiency_score(zerocopy_metrics) > 0.7)
}