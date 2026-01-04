// Azimuth Telemetry System - Performance Optimization Tests
// This file contains comprehensive test cases for performance optimization functionality

// Test 1: Memory Pool Management
test "memory pool management" {
  let pool = MemoryPool::new(1000) // Pool with 1000 objects
  
  // Test object allocation from pool
  let obj1 = MemoryPool::allocate(pool)
  let obj2 = MemoryPool::allocate(pool)
  let obj3 = MemoryPool::allocate(pool)
  
  assert_true(MemoryPool::is_from_pool(pool, obj1))
  assert_true(MemoryPool::is_from_pool(pool, obj2))
  assert_true(MemoryPool::is_from_pool(pool, obj3))
  
  // Test object deallocation back to pool
  MemoryPool::deallocate(pool, obj1)
  MemoryPool::deallocate(pool, obj2)
  
  // Test pool statistics
  let stats = MemoryPool::get_stats(pool)
  assert_eq(stats.total_allocated, 3)
  assert_eq(stats.total_deallocated, 2)
  assert_eq(stats.current_in_use, 1)
  assert_eq(stats.pool_size, 1000)
  
  // Test reallocation from pool
  let obj4 = MemoryPool::allocate(pool)
  let obj5 = MemoryPool::allocate(pool)
  
  // Verify that deallocated objects are reused
  assert_true(MemoryPool::is_from_pool(pool, obj4))
  assert_true(MemoryPool::is_from_pool(pool, obj5))
  
  // Cleanup
  MemoryPool::deallocate(pool, obj3)
  MemoryPool::deallocate(pool, obj4)
  MemoryPool::deallocate(pool, obj5)
}

// Test 2: Lazy Initialization
test "lazy initialization" {
  let lazy_resource = LazyResource::new(fn() {
    // Simulate expensive resource creation
    Resource::with_attributes(Resource::new(), [
      ("expensive.attribute", StringValue("expensive_value")),
      ("creation.time", IntValue(1234567890))
    ])
  })
  
  // Resource should not be initialized yet
  assert_false(LazyResource::is_initialized(lazy_resource))
  
  // Access the resource for the first time
  let resource1 = LazyResource::get(lazy_resource)
  
  // Now it should be initialized
  assert_true(LazyResource::is_initialized(lazy_resource))
  
  // Access again, should return the same instance
  let resource2 = LazyResource::get(lazy_resource)
  
  // Verify they are the same instance
  assert_true(Resource::is_same_instance(resource1, resource2))
  
  // Verify resource attributes
  let attr = Resource::get_attribute(resource1, "expensive.attribute")
  match attr {
    Some(StringValue(value)) => assert_eq(value, "expensive_value")
    _ => assert_true(false)
  }
}

// Test 3: Batching Operations
test "batching operations" {
  let processor = TelemetryProcessor::new()
  let batch_size = 100
  
  // Create a large number of telemetry data points
  let data_points = []
  for i in 0..=1000 {
    data_points.push(TelemetryData::new("metric1", i.to_float(), "count", 1234567890L + i.to_long()))
  }
  
  // Process without batching (baseline)
  let start_time = Performance::current_time_millis()
  for data in data_points {
    TelemetryProcessor::process_single(processor, data)
  }
  let baseline_time = Performance::current_time_millis() - start_time
  
  // Process with batching
  let start_time = Performance::current_time_millis()
  let batches = TelemetryProcessor::create_batches(processor, data_points, batch_size)
  for batch in batches {
    TelemetryProcessor::process_batch(processor, batch)
  }
  let batched_time = Performance::current_time_millis() - start_time
  
  // Batched processing should be faster
  assert_true(batched_time <= baseline_time)
  
  // Verify that all data was processed
  let total_processed = batches.fold_left(0, fn(acc, batch) { acc + batch.length() })
  assert_eq(total_processed, data_points.length())
}

// Test 4: Caching Mechanism
test "caching mechanism" {
  let cache = Cache::new(100) // Cache with 100 entries max
  
  // Test cache miss
  let result1 = Cache::get_or_compute(cache, "key1", fn() {
    // Simulate expensive computation
    Thread::sleep(10) // 10ms delay
    "computed_value1"
  })
  assert_eq(result1, "computed_value1")
  
  // Test cache hit (should be faster)
  let start_time = Performance::current_time_millis()
  let result2 = Cache::get_or_compute(cache, "key1", fn() {
    // This should not be called due to cache hit
    "should_not_be_called"
  })
  let cache_hit_time = Performance::current_time_millis() - start_time
  assert_eq(result2, "computed_value1")
  assert_true(cache_hit_time < 5) // Should be much faster than 10ms
  
  // Test cache capacity
  for i in 0..=150 {
    Cache::get_or_compute(cache, "key" + i.to_string(), fn() { "value" + i.to_string() })
  }
  
  // Cache should not exceed capacity
  let cache_stats = Cache::get_stats(cache)
  assert_true(cache_stats.size <= 100)
  
  // Test eviction policy (LRU)
  let evicted_result = Cache::get(cache, "key0")
  match evicted_result {
    Some(_) => assert_true(false) // Should be evicted
    None => assert_true(true)
  }
  
  // Recent entries should still be cached
  let recent_result = Cache::get(cache, "key150")
  match recent_result {
    Some(value) => assert_eq(value, "value150")
    None => assert_true(false)
  }
}

// Test 5: Parallel Processing
test "parallel processing" {
  let processor = TelemetryProcessor::new()
  let num_threads = 4
  
  // Create a large number of telemetry data points
  let data_points = []
  for i in 0..=1000 {
    data_points.push(TelemetryData::new("metric1", i.to_float(), "count", 1234567890L + i.to_long()))
  }
  
  // Process sequentially (baseline)
  let start_time = Performance::current_time_millis()
  let sequential_results = []
  for data in data_points {
    sequential_results.push(TelemetryProcessor::process_single(processor, data))
  }
  let sequential_time = Performance::current_time_millis() - start_time
  
  // Process in parallel
  let start_time = Performance::current_time_millis()
  let parallel_results = TelemetryProcessor::process_parallel(processor, data_points, num_threads)
  let parallel_time = Performance::current_time_millis() - start_time
  
  // Parallel processing should be faster (on multi-core systems)
  // Note: This might not always be faster due to overhead, but should be comparable
  assert_true(parallel_time <= sequential_time * 1.2) // Allow 20% overhead
  
  // Verify results are the same
  assert_eq(parallel_results.length(), sequential_results.length())
  for i in 0..=parallel_results.length() - 1 {
    assert_eq(parallel_results[i].metric_name, sequential_results[i].metric_name)
    assert_eq(parallel_results[i].value, sequential_results[i].value)
  }
}

// Test 6: Resource Pooling
test "resource pooling" {
  let pool = ResourcePool::new(10, fn() {
    // Simulate expensive resource creation
    ExpensiveResource::new()
  })
  
  // Test resource checkout
  let resource1 = ResourcePool::checkout(pool)
  let resource2 = ResourcePool::checkout(pool)
  let resource3 = ResourcePool::checkout(pool)
  
  assert_true(ResourcePool::is_from_pool(pool, resource1))
  assert_true(ResourcePool::is_from_pool(pool, resource2))
  assert_true(ResourcePool::is_from_pool(pool, resource3))
  
  // Test pool statistics
  let stats = ResourcePool::get_stats(pool)
  assert_eq(stats.created_count, 3)
  assert_eq(stats.checked_out_count, 3)
  assert_eq(stats.available_count, 7)
  
  // Test resource checkin
  ResourcePool::checkin(pool, resource1)
  ResourcePool::checkin(pool, resource2)
  
  // Update stats
  let stats = ResourcePool::get_stats(pool)
  assert_eq(stats.checked_out_count, 1)
  assert_eq(stats.available_count, 9)
  
  // Test resource reuse
  let resource4 = ResourcePool::checkout(pool)
  let resource5 = ResourcePool::checkout(pool)
  
  // Should reuse previously checked-in resources
  assert_true(ExpensiveResource::is_same_instance(resource4, resource1))
  assert_true(ExpensiveResource::is_same_instance(resource5, resource2))
  
  // Cleanup
  ResourcePool::checkin(pool, resource3)
  ResourcePool::checkin(pool, resource4)
  ResourcePool::checkin(pool, resource5)
}

// Test 7: Adaptive Sampling
test "adaptive sampling" {
  let sampler = AdaptiveSampler::new(0.1) // Start with 10% sampling rate
  
  // Create telemetry data with varying importance
  let high_priority_data = TelemetryData::with_attributes("error_count", 1.0, "count", 1234567890L, [
    ("priority", "high"),
    ("error.type", "critical")
  ])
  
  let normal_priority_data = TelemetryData::with_attributes("request_count", 100.0, "count", 1234567890L, [
    ("priority", "normal")
  ])
  
  let low_priority_data = TelemetryData::with_attributes("debug_info", 1.0, "count", 1234567890L, [
    ("priority", "low")
  ])
  
  // Test sampling decisions
  assert_true(AdaptiveSampler::should_sample(sampler, high_priority_data)) // High priority should always be sampled
  assert_true(AdaptiveSampler::should_sample(sampler, normal_priority_data)) // Normal priority based on rate
  assert_false(AdaptiveSampler::should_sample(sampler, low_priority_data)) // Low priority rarely sampled
  
  // Test adaptive rate adjustment
  for i in 0..=1000 {
    sampler = AdaptiveSampler::adjust_rate(sampler, normal_priority_data, true) // Provide feedback
  }
  
  // After many positive feedbacks, sampling rate should increase
  let adjusted_rate = AdaptiveSampler::get_current_rate(sampler)
  assert_true(adjusted_rate > 0.1)
  
  // Test negative feedback
  for i in 0..=1000 {
    sampler = AdaptiveSampler::adjust_rate(sampler, normal_priority_data, false)
  }
  
  // After many negative feedbacks, sampling rate should decrease
  let adjusted_rate = AdaptiveSampler::get_current_rate(sampler)
  assert_true(adjusted_rate < 0.1)
}

// Test 8: Performance Profiling
test "performance profiling" {
  let profiler = Profiler::new()
  
  // Start profiling
  Profiler::start(profiler)
  
  // Perform some operations
  let resource = Resource::new()
  for i in 0..=100 {
    Resource::with_attributes(resource, [("test.attr" + i.to_string(), StringValue("test.value" + i.to_string()))])
  }
  
  // Stop profiling
  let profile = Profiler::stop(profiler)
  
  // Analyze profile
  assert_true(ProfilerProfile::total_time(profile) > 0)
  assert_true(ProfilerProfile::operation_count(profile) > 0)
  
  // Get hottest operations
  let hot_operations = ProfilerProfile::get_hottest_operations(profile, 5)
  assert_true(hot_operations.length() <= 5)
  
  // Verify that operations are ordered by time spent
  for i in 0..=hot_operations.length() - 2 {
    assert_true(hot_operations[i].time_spent >= hot_operations[i + 1].time_spent)
  }
  
  // Get memory usage
  let memory_usage = ProfilerProfile::get_memory_usage(profile)
  assert_true(memory_usage.peak_usage > 0)
  assert_true(memory_usage.average_usage > 0)
}