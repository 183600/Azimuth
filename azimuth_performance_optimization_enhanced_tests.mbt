// Azimuth Telemetry System - Performance Optimization Enhanced Tests
// This file contains comprehensive test cases for performance optimization features

// Test 1: Memory Pool Management for High-Performance Operations
test "memory pool management for performance optimization" {
  let pool = MemoryPool::new(1024) // 1KB pool size
  
  // Test memory allocation from pool
  let block1 = MemoryPool::allocate(pool, 256)
  let block2 = MemoryPool::allocate(pool, 128)
  let block3 = MemoryPool::allocate(pool, 512)
  
  assert_true(MemoryPool::is_allocated(block1))
  assert_true(MemoryPool::is_allocated(block2))
  assert_true(MemoryPool::is_allocated(block3))
  
  // Test memory deallocation and reuse
  MemoryPool::deallocate(pool, block1)
  let block4 = MemoryPool::allocate(pool, 200)
  assert_true(MemoryPool::is_allocated(block4))
  
  // Test pool statistics
  let stats = MemoryPool::get_stats(pool)
  assert_eq(stats.total_allocated, 896) // 128 + 512 + 256 (reused)
  assert_eq(stats.active_allocations, 3)
  assert_eq(stats.pool_utilization, 0.875) // 896/1024
}

// Test 2: Lock-Free Data Structures for Concurrent Performance
test "lock-free data structures for concurrent access" {
  let counter = LockFreeCounter::new()
  
  // Test atomic increment operations
  assert_eq(LockFreeCounter::increment(counter), 1)
  assert_eq(LockFreeCounter::increment(counter), 2)
  assert_eq(LockFreeCounter::increment(counter), 3)
  
  // Test atomic add operations
  assert_eq(LockFreeCounter::add(counter, 5), 8)
  assert_eq(LockFreeCounter::add(counter, -3), 5)
  
  // Test atomic compare-and-swap
  let old_value = LockFreeCounter::compare_and_swap(counter, 5, 10)
  assert_eq(old_value, 5)
  assert_eq(LockFreeCounter::get(counter), 10)
  
  // Test lock-free queue operations
  let queue = LockFreeQueue::new()
  LockFreeQueue::enqueue(queue, "item1")
  LockFreeQueue::enqueue(queue, "item2")
  LockFreeQueue::enqueue(queue, "item3")
  
  let item1 = LockFreeQueue::dequeue(queue)
  match item1 {
    Some(value) => assert_eq(value, "item1")
    None => assert_true(false)
  }
  
  let item2 = LockFreeQueue::dequeue(queue)
  match item2 {
    Some(value) => assert_eq(value, "item2")
    None => assert_true(false)
  }
}

// Test 3: Adaptive Sampling for Performance Optimization
test "adaptive sampling for performance optimization" {
  let adaptive_sampler = AdaptiveSampler::new(
    0.1, // Initial sampling rate 10%
    1000, // Target spans per second
    100   // Adjustment window size
  )
  
  // Test sampling rate adjustment based on load
  for i in 0..=200 {
    let decision = AdaptiveSampler::should_sample(adaptive_sampler, 
      "trace" + i.to_string(), "span" + i.to_string(), "operation", [])
    
    // Initially should sample less due to low rate
    if i < 50 {
      // Low sampling expected
    } else {
      // Rate should adapt based on throughput
    }
  }
  
  // Test performance metrics
  let metrics = AdaptiveSampler::get_metrics(adaptive_sampler)
  assert_true(metrics.total_traces > 0)
  assert_true(metrics.sampled_traces >= 0)
  assert_true(metrics.current_sampling_rate >= 0.0)
  assert_true(metrics.current_sampling_rate <= 1.0)
}

// Test 4: Batch Processing Optimization
test "batch processing optimization strategies" {
  let processor = OptimizedBatchProcessor::new(
    1000,  // Max batch size
    5000,  // Max batch timeout (ms)
    3      // Max concurrent batches
  )
  
  // Test efficient batch creation
  let items = ["item1", "item2", "item3", "item4", "item5"]
  let batch = OptimizedBatchProcessor::create_batch(processor, items)
  assert_eq(Batch::size(batch), 5)
  
  // Test batch compression
  let compressed_batch = Batch::compress(batch)
  assert_true(Batch::is_compressed(compressed_batch))
  assert_true(Batch::compressed_size(compressed_batch) < Batch::original_size(batch))
  
  // Test batch decompression
  let decompressed_batch = Batch::decompress(compressed_batch)
  assert_eq(Batch::size(decompressed_batch), 5)
  
  // Test parallel batch processing
  let batches = [
    Batch::new(["batch1_item1", "batch1_item2"]),
    Batch::new(["batch2_item1", "batch2_item2"]),
    Batch::new(["batch3_item1", "batch3_item2"])
  ]
  
  let results = OptimizedBatchProcessor::process_parallel(processor, batches)
  assert_eq(results.length(), 3)
  for result in results {
    assert_true(BatchProcessingResult::is_success(result))
  }
}

// Test 5: Caching Strategies for Performance
test "caching strategies for performance optimization" {
  let cache = LRUCache::new(100) // 100 item capacity
  
  // Test cache insertion and retrieval
  LRUCache::put(cache, "key1", "value1")
  LRUCache::put(cache, "key2", "value2")
  LRUCache::put(cache, "key3", "value3")
  
  let value1 = LRUCache::get(cache, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // Test cache eviction when capacity exceeded
  for i in 0..=150 {
    LRUCache::put(cache, "key" + i.to_string(), "value" + i.to_string())
  }
  
  // Oldest items should be evicted
  let old_value = LRUCache::get(cache, "key1")
  match old_value {
    Some(_) => assert_true(false) // Should be evicted
    None => assert_true(true)
  }
  
  // Test cache statistics
  let stats = LRUCache::get_stats(cache)
  assert_eq(stats.hits, 1)
  assert_eq(stats.misses, 1)
  assert_eq(stats.evictions, 51) // 151 items inserted, 100 capacity
  assert_eq(stats.hit_ratio, 0.5)
}

// Test 6: Connection Pool Optimization
test "connection pool optimization for network operations" {
  let pool = ConnectionPool::new(
    10, // Min connections
    50, // Max connections
    30000 // Connection timeout (ms)
  )
  
  // Test connection acquisition
  let conn1 = ConnectionPool::acquire(pool)
  let conn2 = ConnectionPool::acquire(pool)
  let conn3 = ConnectionPool::acquire(pool)
  
  assert_true(ConnectionPool::is_active(conn1))
  assert_true(ConnectionPool::is_active(conn2))
  assert_true(ConnectionPool::is_active(conn3))
  
  // Test connection reuse
  ConnectionPool::release(pool, conn1)
  let conn4 = ConnectionPool::acquire(pool)
  assert_true(ConnectionPool::is_active(conn4))
  
  // Test connection health checking
  let healthy = ConnectionPool::check_health(pool)
  assert_true(healthy)
  
  // Test pool statistics
  let stats = ConnectionPool::get_stats(pool)
  assert_eq(stats.active_connections, 3)
  assert_eq(stats.available_connections, 1)
  assert_eq(stats.total_acquisitions, 4)
}

// Test 7: CPU-Accelerated Processing
test "cpu-accelerated processing optimization" {
  let processor = CPUAcceleratedProcessor::new()
  
  // Test SIMD operations for vector processing
  let vector1 = [1.0, 2.0, 3.0, 4.0, 5.0]
  let vector2 = [2.0, 3.0, 4.0, 5.0, 6.0]
  
  let result_vector = CPUAcceleratedProcessor::vector_add(processor, vector1, vector2)
  assert_eq(result_vector.length(), 5)
  assert_eq(result_vector[0], 3.0)
  assert_eq(result_vector[4], 11.0)
  
  // Test parallel processing with CPU affinity
  let data = [1, 2, 3, 4, 5, 6, 7, 8]
  let processed_data = CPUAcceleratedProcessor::process_parallel(processor, data, 4)
  assert_eq(processed_data.length(), 8)
  
  // Test memory-mapped file operations
  let mapped_file = CPUAcceleratedProcessor::map_file(processor, "test_data.bin")
  assert_true(CPUAcceleratedProcessor::is_mapped(mapped_file))
  
  let file_data = CPUAcceleratedProcessor::read_mapped(processor, mapped_file, 0, 100)
  assert_eq(file_data.length(), 100)
  
  CPUAcceleratedProcessor::unmap_file(processor, mapped_file)
}

// Test 8: Just-In-Time Compilation for Hot Paths
test "just-in-time compilation for performance-critical paths" {
  let jit_compiler = JITCompiler::new()
  
  // Test JIT compilation of frequently used functions
  let hot_function = JITCompiler::compile_function(jit_compiler, "
    fn hot_path(x: Int, y: Int) -> Int {
      return x * y + x + y
    }
  ")
  
  assert_true(JITCompiler::is_compiled(hot_function))
  
  // Test execution of compiled function
  let result = JITCompiler::execute_function(hot_function, [5, 3])
  assert_eq(result, 23) // 5*3 + 5 + 3 = 23
  
  // Test performance improvement tracking
  let baseline_time = 1000 // microseconds
  let optimized_time = JITCompiler::benchmark_function(hot_function, [10, 20])
  assert_true(optimized_time < baseline_time)
  
  // Test hot path detection and optimization
  JITCompiler::mark_hot_path(jit_compiler, hot_function)
  let optimized_function = JITCompiler::optimize_hot_path(jit_compiler, hot_function)
  
  let optimized_result = JITCompiler::execute_function(optimized_function, [7, 4])
  assert_eq(optimized_result, 39) // 7*4 + 7 + 4 = 39
}

// Test 9: Adaptive Buffer Management
test "adaptive buffer management for performance" {
  let buffer_manager = AdaptiveBufferManager::new(
    1024,   // Initial buffer size
    1048576 // Maximum buffer size (1MB)
  )
  
  // Test buffer allocation and growth
  let buffer1 = AdaptiveBufferManager::allocate(buffer_manager, 512)
  assert_eq(Buffer::capacity(buffer1), 1024)
  
  // Test buffer expansion when needed
  let buffer2 = AdaptiveBufferManager::allocate(buffer_manager, 2048)
  assert_true(Buffer::capacity(buffer2) >= 2048)
  
  // Test buffer shrinking when idle
  AdaptiveBufferManager::mark_idle(buffer_manager, buffer2)
  AdaptiveBufferManager::shrink_idle_buffers(buffer_manager)
  
  // Test buffer pooling
  AdaptiveBufferManager::release(buffer_manager, buffer1)
  let buffer3 = AdaptiveBufferManager::allocate(buffer_manager, 600)
  assert_eq(Buffer::capacity(buffer3), 1024) // Reused from pool
  
  // Test buffer statistics
  let stats = AdaptiveBufferManager::get_stats(buffer_manager)
  assert_true(stats.total_allocated > 0)
  assert_true(stats.pool_hits > 0)
  assert_true(stats.expansions >= 0)
}

// Test 10: Performance Monitoring and Profiling
test "performance monitoring and profiling integration" {
  let profiler = PerformanceProfiler::new()
  
  // Test profiling session start
  PerformanceProfiler::start_session(profiler, "telemetry_performance_test")
  
  // Test operation profiling
  let profile1 = PerformanceProfiler::start_operation(profiler, "span_creation")
  
  // Simulate some work
  let spans = []
  for i in 0..=1000 {
    spans.push(Span::new("test_span" + i.to_string(), Internal, 
      SpanContext::new("trace123", "span456", true, "")))
  }
  
  PerformanceProfiler::end_operation(profiler, profile1)
  
  // Test memory profiling
  let memory_profile = PerformanceProfiler::profile_memory(profiler)
  assert_true(memory_profile.heap_size > 0)
  assert_true(memory_profile.stack_size >= 0)
  
  // Test CPU profiling
  let cpu_profile = PerformanceProfiler::profile_cpu(profiler, 100) // 100ms sampling
  assert_true(cpu_profile.samples > 0)
  assert_true(cpu_profile.total_time > 0)
  
  // Test performance report generation
  let report = PerformanceProfiler::generate_report(profiler)
  assert_true(report.operations.length() > 0)
  assert_true(report.memory_usage.heap_size > 0)
  assert_true(report.cpu_usage.total_samples > 0)
  
  PerformanceProfiler::end_session(profiler)
}