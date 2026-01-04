// Resource Optimization Tests for Azimuth Telemetry System
// This file contains test cases for resource optimization and efficiency

// Test 1: Memory Pool Optimization
test "memory pool optimization" {
  let memory_pool = MemoryPool::new(1000) // Pool of 1000 objects
  
  // Test object allocation from pool
  let allocated_objects = []
  
  for i in 1..=500 {
    let obj = MemoryPool::allocate(memory_pool)
    allocated_objects.push(obj)
  }
  
  // Verify objects were allocated from pool
  assert_eq(allocated_objects.length(), 500)
  assert_eq(MemoryPool::available_count(memory_pool), 500) // 1000 - 500 = 500
  
  // Test object deallocation back to pool
  for i in 1..=250 {
    let obj = allocated_objects.pop()
    MemoryPool::deallocate(memory_pool, obj)
  }
  
  // Verify objects were returned to pool
  assert_eq(MemoryPool::available_count(memory_pool), 750) // 500 + 250 = 750
  
  // Test pool exhaustion handling
  let additional_objects = []
  
  for i in 1..=800 {
    let obj = MemoryPool::allocate(memory_pool)
    if obj.is_some() {
      additional_objects.push(obj.unwrap())
    }
  }
  
  // Should allocate all available objects (750)
  assert_eq(additional_objects.length(), 750)
  assert_eq(MemoryPool::available_count(memory_pool), 0)
  
  // Test pool expansion
  let expansion_result = MemoryPool::expand(memory_pool, 500)
  match expansion_result {
    Success => assert_eq(MemoryPool::available_count(memory_pool), 500)
    Error(_) => assert_true(false) // Expansion should succeed
  }
  
  // Test allocation after expansion
  let expanded_obj = MemoryPool::allocate(memory_pool)
  assert_true(expanded_obj.is_some())
  assert_eq(MemoryPool::available_count(memory_pool), 499)
  
  // Return all objects to pool
  for obj in additional_objects {
    MemoryPool::deallocate(memory_pool, obj)
  }
  
  MemoryPool::deallocate(memory_pool, expanded_obj.unwrap())
  
  // Verify all objects are returned
  assert_eq(MemoryPool::available_count(memory_pool), 999) // 499 + 500 = 999
}

// Test 2: Connection Pool Optimization
test "connection pool optimization" {
  let connection_pool = ConnectionPool::new("https://telemetry.example.com", 10) // 10 connections
  
  // Test connection acquisition
  let connections = []
  
  for i in 1..=8 {
    let conn = ConnectionPool::acquire(connection_pool, 1000) // 1s timeout
    match conn {
      Some(connection) => connections.push(connection)
      None => assert_true(false) // Should acquire connection
    }
  }
  
  // Verify connections were acquired
  assert_eq(connections.length(), 8)
  assert_eq(ConnectionPool::available_connections(connection_pool), 2) // 10 - 8 = 2
  
  // Test connection release
  for i in 1..=4 {
    let conn = connections.pop()
    ConnectionPool::release(connection_pool, conn.unwrap())
  }
  
  // Verify connections were released
  assert_eq(ConnectionPool::available_connections(connection_pool), 6) // 2 + 4 = 6
  
  // Test connection reuse
  let reused_connections = []
  
  for i in 1..=6 {
    let conn = ConnectionPool::acquire(connection_pool, 1000)
    match conn {
      Some(connection) => {
        reused_connections.push(connection)
        // Test connection is still valid
        assert_true(Connection::is_valid(connection))
      }
      None => assert_true(false) // Should acquire connection
    }
  }
  
  // Verify all available connections were reused
  assert_eq(reused_connections.length(), 6)
  assert_eq(ConnectionPool::available_connections(connection_pool), 0)
  
  // Test connection pool exhaustion
  let exhausted_conn = ConnectionPool::acquire(connection_pool, 100) // Short timeout
  assert_true(exhausted_conn.is_none()) // Should fail due to exhaustion
  
  // Release one connection and try again
  let conn_to_release = reused_connections.pop()
  ConnectionPool::release(connection_pool, conn_to_release.unwrap())
  
  let recovery_conn = ConnectionPool::acquire(connection_pool, 1000)
  assert_true(recovery_conn.is_some()) // Should succeed after release
  
  // Release all connections
  for conn in reused_connections {
    ConnectionPool::release(connection_pool, conn)
  }
  
  ConnectionPool::release(connection_pool, recovery_conn.unwrap())
  
  // Verify all connections are available
  assert_eq(ConnectionPool::available_connections(connection_pool), 10)
}

// Test 3: Batch Size Optimization
test "batch size optimization" {
  let optimizer = BatchSizeOptimizer::new()
  
  // Test with varying data volumes
  let data_volumes = [100, 500, 1000, 5000, 10000]
  
  for volume in data_volumes {
    // Create test data
    let test_data = []
    for i in 1..=volume {
      let attrs = Attributes::new()
      Attributes::set(attrs, "iteration", IntValue(i))
      
      test_data.push(TelemetryData::new(
        (i as Float),
        attrs,
        1234567890L + i
      ))
    }
    
    // Find optimal batch size
    let optimal_size = BatchSizeOptimizer::find_optimal_size(optimizer, test_data)
    
    // Verify optimal size is reasonable
    assert_true(optimal_size >= 10) // Minimum batch size
    assert_true(optimal_size <= 1000) // Maximum batch size
    assert_true(optimal_size <= volume) // Should not exceed data volume
    
    // Test processing with optimal batch size
    let start_time = Time::now()
    
    let processed_count = 0
    for batch in BatchProcessor::process_with_size(test_data, optimal_size) {
      let result = BatchProcessor::process_batch(batch)
      match result {
        Success(count) => processed_count = processed_count + count
        Error(_) => assert_true(false) // Should not fail
      }
    }
    
    let end_time = Time::now()
    let processing_time = Time::duration_between(start_time, end_time)
    
    // Verify all data was processed
    assert_eq(processed_count, volume)
    
    // Verify processing time is reasonable
    let max_expected_time = (volume as Float / optimal_size as Float) * 100.0 // 100ms per batch
    assert_true((processing_time as Float) < max_expected_time)
  }
}

// Test 4: Sampling Rate Optimization
test "sampling rate optimization" {
  let optimizer = SamplingRateOptimizer::new()
  
  // Test with different traffic patterns
  let traffic_patterns = [
    ("low_traffic", 100),
    ("medium_traffic", 1000),
    ("high_traffic", 10000),
    ("very_high_traffic", 100000)
  ]
  
  for (pattern_name, traffic_volume) in traffic_patterns {
    // Simulate traffic pattern
    let traffic_data = []
    for i in 1..=traffic_volume {
      let attrs = Attributes::new()
      Attributes::set(attrs, "pattern", StringValue(pattern_name))
      Attributes::set(attrs, "iteration", IntValue(i))
      
      traffic_data.push(TelemetryData::new(
        (i as Float),
        attrs,
        1234567890L + i
      ))
    }
    
    // Find optimal sampling rate
    let optimal_rate = SamplingRateOptimizer::find_optimal_rate(optimizer, traffic_data)
    
    // Verify optimal rate is reasonable
    assert_true(optimal_rate >= 0.0) // Minimum rate
    assert_true(optimal_rate <= 1.0) // Maximum rate
    
    // Higher traffic should generally have lower sampling rates
    if traffic_volume >= 10000 {
      assert_true(optimal_rate <= 0.5) // High traffic should sample less
    }
    
    // Test sampling with optimal rate
    let sampler = ProbabilitySampler::new(optimal_rate)
    let sampled_count = 0
    
    for data in traffic_data {
      let decision = Sampler::should_sample(
        sampler,
        Context::root(),
        "trace_" + data.iteration.to_string(),
        "test_span",
        Server,
        data.attributes
      )
      
      match decision {
        RecordAndSample => sampled_count = sampled_count + 1
        Drop => {} // Not sampled
      }
    }
    
    // Verify sampling rate is approximately correct
    let actual_rate = (sampled_count as Float) / (traffic_volume as Float)
    let rate_error = (actual_rate - optimal_rate).abs()
    
    // Allow 5% tolerance
    assert_true(rate_error < 0.05)
    
    // Verify sampled data is representative
    assert_true(sampled_count > 0) // Should sample at least some data
    if traffic_volume > 100 {
      assert_true(sampled_count < traffic_volume) // Should not sample all data for high volume
    }
  }
}

// Test 5: Cache Optimization
test "cache optimization" {
  let cache = LRUCache::new(1000) // 1000 item cache
  
  // Test cache population
  let cache_keys = []
  
  for i in 1..=800 {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string()
    
    LRUCache::put(cache, key, value)
    cache_keys.push(key)
  }
  
  // Verify cache size
  assert_eq(LRUCache::size(cache), 800)
  
  // Test cache hits
  let hit_count = 0
  
  for i in 1..=400 {
    let key = "key_" + i.to_string()
    let value = LRUCache::get(cache, key)
    
    match value {
      Some(v) => {
        assert_eq(v, "value_" + i.to_string())
        hit_count = hit_count + 1
      }
      None => assert_true(false) // Should find value
    }
  }
  
  // Verify all lookups were hits
  assert_eq(hit_count, 400)
  
  // Test cache misses for non-existent keys
  let miss_count = 0
  
  for i in 801..=900 {
    let key = "key_" + i.to_string()
    let value = LRUCache::get(cache, key)
    
    match value {
      Some(_) => assert_true(false) // Should not find value
      None => miss_count = miss_count + 1
    }
  }
  
  // Verify all lookups were misses
  assert_eq(miss_count, 100)
  
  // Test cache eviction
  for i in 1..=300 {
    let key = "new_key_" + i.to_string()
    let value = "new_value_" + i.to_string()
    
    LRUCache::put(cache, key, value)
  }
  
  // Cache should be at capacity
  assert_eq(LRUCache::size(cache), 1000)
  
  // Test that least recently used items were evicted
  let evicted_count = 0
  
  for i in 1..=100 {
    let key = "key_" + i.to_string() // These should be the oldest
    let value = LRUCache::get(cache, key)
    
    match value {
      Some(_) => {} // Still in cache
      None => evicted_count = evicted_count + 1
    }
  }
  
  // At least some items should have been evicted
  assert_true(evicted_count > 0)
  
  // Test that recently accessed items are still in cache
  let recent_hit_count = 0
  
  for i in 401..=500 {
    let key = "key_" + i.to_string() // These were accessed recently
    let value = LRUCache::get(cache, key)
    
    match value {
      Some(_) => recent_hit_count = recent_hit_count + 1
      None => {}
    }
  }
  
  // Most recently accessed items should still be in cache
  assert_true(recent_hit_count >= 80) // At least 80% should still be in cache
}

// Test 6: CPU Usage Optimization
test "cpu usage optimization" {
  let optimizer = CPUOptimizer::new()
  
  // Test with different processing loads
  let processing_loads = [100, 500, 1000, 5000]
  
  for load in processing_loads {
    // Create test data
    let test_data = []
    for i in 1..=load {
      let attrs = Attributes::new()
      Attributes::set(attrs, "load", IntValue(load))
      Attributes::set(attrs, "iteration", IntValue(i))
      
      test_data.push(TelemetryData::new(
        (i as Float),
        attrs,
        1234567890L + i
      ))
    }
    
    // Measure CPU usage with default processing
    let default_start_time = Time::now()
    let default_start_cpu = CPU::get_usage()
    
    // Process with default settings
    for data in test_data {
      let result = DefaultProcessor::process(data)
      match result {
        Success(_) => {}
        Error(_) => assert_true(false) // Should not fail
      }
    }
    
    let default_end_time = Time::now()
    let default_end_cpu = CPU::get_usage()
    
    let default_duration = Time::duration_between(default_start_time, default_end_time)
    let default_cpu_usage = default_end_cpu - default_start_cpu
    
    // Optimize processing for this load
    let optimized_settings = CPUOptimizer::optimize_for_load(optimizer, load)
    
    // Measure CPU usage with optimized processing
    let optimized_start_time = Time::now()
    let optimized_start_cpu = CPU::get_usage()
    
    // Process with optimized settings
    for data in test_data {
      let result = OptimizedProcessor::process_with_settings(data, optimized_settings)
      match result {
        Success(_) => {}
        Error(_) => assert_true(false) // Should not fail
      }
    }
    
    let optimized_end_time = Time::now()
    let optimized_end_cpu = CPU::get_usage()
    
    let optimized_duration = Time::duration_between(optimized_start_time, optimized_end_time)
    let optimized_cpu_usage = optimized_end_cpu - optimized_start_cpu
    
    // Verify optimization improved efficiency
    // Optimized processing should use less CPU or complete in similar time
    let cpu_improvement = (default_cpu_usage - optimized_cpu_usage) as Float / (default_cpu_usage as Float)
    let time_ratio = (optimized_duration as Float) / (default_duration as Float)
    
    // CPU usage should be reduced or time should not increase significantly
    assert_true(cpu_improvement > 0.0 || time_ratio <= 1.2)
    
    // For larger loads, optimization should be more effective
    if load >= 1000 {
      assert_true(cpu_improvement > 0.05) // At least 5% CPU reduction for larger loads
    }
  }
}

// Test 7: I/O Optimization
test "io optimization" {
  let optimizer = IOOptimizer::new()
  
  // Test with different I/O patterns
  let io_patterns = [
    ("sequential", 1000),
    ("random", 1000),
    ("burst", 5000)
  ]
  
  for (pattern_name, data_size) in io_patterns {
    // Create test data
    let test_data = []
    for i in 1..=data_size {
      let attrs = Attributes::new()
      Attributes::set(attrs, "pattern", StringValue(pattern_name))
      Attributes::set(attrs, "iteration", IntValue(i))
      
      test_data.push(TelemetryData::new(
        (i as Float),
        attrs,
        1234567890L + i
      ))
    }
    
    // Measure I/O with default settings
    let default_start_time = Time::now()
    
    let default_result = DefaultIOProcessor::write_to_file(test_data, "default_" + pattern_name + ".dat")
    match default_result {
      Success(_) => {}
      Error(_) => assert_true(false) // Should not fail
    }
    
    let default_end_time = Time::now()
    let default_duration = Time::duration_between(default_start_time, default_end_time)
    
    // Optimize I/O for this pattern
    let optimized_settings = IOOptimizer::optimize_for_pattern(optimizer, pattern_name)
    
    // Measure I/O with optimized settings
    let optimized_start_time = Time::now()
    
    let optimized_result = OptimizedIOProcessor::write_to_file(
      test_data,
      "optimized_" + pattern_name + ".dat",
      optimized_settings
    )
    match optimized_result {
      Success(_) => {}
      Error(_) => assert_true(false) // Should not fail
    }
    
    let optimized_end_time = Time::now()
    let optimized_duration = Time::duration_between(optimized_start_time, optimized_end_time)
    
    // Verify optimization improved I/O performance
    let improvement = (default_duration - optimized_duration) as Float / (default_duration as Float)
    
    // Optimized I/O should be faster
    assert_true(improvement > 0.0)
    
    // For certain patterns, optimization should be more effective
    match pattern_name {
      "sequential" => assert_true(improvement > 0.1) // At least 10% improvement for sequential
      "random" => assert_true(improvement > 0.05) // At least 5% improvement for random
      "burst" => assert_true(improvement > 0.15) // At least 15% improvement for burst
      _ => {}
    }
    
    // Verify data integrity
    let default_read_result = DefaultIOProcessor::read_from_file("default_" + pattern_name + ".dat")
    let optimized_read_result = OptimizedIOProcessor::read_from_file("optimized_" + pattern_name + ".dat")
    
    match (default_read_result, optimized_read_result) {
      (Success(default_data), Success(optimized_data)) => {
        assert_eq(default_data.length(), optimized_data.length())
        
        for i in 0..(default_data.length() - 1) {
          assert_eq(default_data[i].value, optimized_data[i].value)
          assert_eq(default_data[i].timestamp, optimized_data[i].timestamp)
        }
      }
      _ => assert_true(false) // Should successfully read both files
    }
  }
}

// Test 8: Resource Cleanup Optimization
test "resource cleanup optimization" {
  let cleanup_manager = ResourceCleanupManager::new()
  
  // Test with various resource types
  let resources = []
  
  // Create spans
  for i in 1..=100 {
    let span_ctx = SpanContext::new(
      "cleanup_trace_" + i.to_string(),
      "cleanup_span_" + i.to_string(),
      true,
      "cleanup_state"
    )
    
    let span = Span::new("cleanup_test", Server, span_ctx)
    resources.push(SpanResource(span))
  }
  
  // Create metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "cleanup_test")
  
  for i in 1..=50 {
    let counter = Meter::create_counter(
      meter,
      "cleanup_counter_" + i.to_string(),
      Some("Test counter"),
      Some("count")
    )
    resources.push(MetricResource(counter))
  }
  
  // Create loggers
  let logger_provider = LoggerProvider::default()
  
  for i in 1..=25 {
    let logger = LoggerProvider::get_logger(
      logger_provider,
      "cleanup_logger_" + i.to_string()
    )
    resources.push(LoggerResource(logger))
  }
  
  // Verify resources are created
  assert_eq(resources.length(), 175) // 100 spans + 50 metrics + 25 loggers
  
  // Measure memory usage before cleanup
  let memory_before = Memory::get_allocated_bytes()
  
  // Perform standard cleanup
  let standard_cleanup_start = Time::now()
  
  for resource in resources {
    ResourceCleanupManager::cleanup_standard(cleanup_manager, resource)
  }
  
  let standard_cleanup_end = Time::now()
  let standard_cleanup_duration = Time::duration_between(standard_cleanup_start, standard_cleanup_end)
  
  // Force garbage collection
  Memory::collect()
  
  let memory_after_standard = Memory::get_allocated_bytes()
  let standard_memory_freed = memory_before - memory_after_standard
  
  // Recreate resources for optimized cleanup test
  let optimized_resources = []
  
  // Create spans again
  for i in 1..=100 {
    let span_ctx = SpanContext::new(
      "optimized_trace_" + i.to_string(),
      "optimized_span_" + i.to_string(),
      true,
      "optimized_state"
    )
    
    let span = Span::new("optimized_cleanup_test", Server, span_ctx)
    optimized_resources.push(SpanResource(span))
  }
  
  // Create metrics again
  for i in 1..=50 {
    let counter = Meter::create_counter(
      meter,
      "optimized_counter_" + i.to_string(),
      Some("Test counter"),
      Some("count")
    )
    optimized_resources.push(MetricResource(counter))
  }
  
  // Create loggers again
  for i in 1..=25 {
    let logger = LoggerProvider::get_logger(
      logger_provider,
      "optimized_logger_" + i.to_string()
    )
    optimized_resources.push(LoggerResource(logger))
  }
  
  // Measure memory usage before optimized cleanup
  let memory_before_optimized = Memory::get_allocated_bytes()
  
  // Perform optimized cleanup
  let optimized_cleanup_start = Time::now()
  
  ResourceCleanupManager::cleanup_batch_optimized(cleanup_manager, optimized_resources)
  
  let optimized_cleanup_end = Time::now()
  let optimized_cleanup_duration = Time::duration_between(optimized_cleanup_start, optimized_cleanup_end)
  
  // Force garbage collection
  Memory::collect()
  
  let memory_after_optimized = Memory::get_allocated_bytes()
  let optimized_memory_freed = memory_before_optimized - memory_after_optimized
  
  // Verify optimized cleanup is more efficient
  assert_true(optimized_cleanup_duration < standard_cleanup_duration)
  
  // Memory freed should be similar or better
  assert_true(optimized_memory_freed >= standard_memory_freed * 0.9) // At least 90% of standard cleanup
  
  // Verify cleanup efficiency
  let cleanup_efficiency = (optimized_memory_freed as Float) / (optimized_cleanup_duration as Float)
  let standard_efficiency = (standard_memory_freed as Float) / (standard_cleanup_duration as Float)
  
  // Optimized cleanup should be more efficient
  assert_true(cleanup_efficiency > standard_efficiency)
}