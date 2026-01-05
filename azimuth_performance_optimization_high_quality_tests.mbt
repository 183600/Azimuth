// Azimuth Telemetry System - Performance Optimization High-Quality Tests
// This file contains comprehensive test cases for performance optimization features

// Test 1: Memory Pool Operations
test "memory pool operations" {
  let pool = MemoryPool::new(100) // Initial capacity
  
  // Test empty pool
  assert_eq(MemoryPool::available_count(pool), 100)
  assert_eq(MemoryPool::allocated_count(pool), 0)
  
  // Test allocation
  let obj1 = MemoryPool::allocate(pool)
  let obj2 = MemoryPool::allocate(pool)
  let obj3 = MemoryPool::allocate(pool)
  
  assert_eq(MemoryPool::available_count(pool), 97)
  assert_eq(MemoryPool::allocated_count(pool), 3)
  
  // Test deallocation
  MemoryPool::deallocate(pool, obj2)
  assert_eq(MemoryPool::available_count(pool), 98)
  assert_eq(MemoryPool::allocated_count(pool), 2)
  
  // Test reuse of deallocated memory
  let obj4 = MemoryPool::allocate(pool)
  assert_eq(MemoryPool::available_count(pool), 97)
  assert_eq(MemoryPool::allocated_count(pool), 3)
  
  // Test pool expansion when capacity is exceeded
  let mut objects = []
  for i in 0..=120 {
    objects.push(MemoryPool::allocate(pool))
  }
  
  assert_true(MemoryPool::available_count(pool) >= 0)
  assert_true(MemoryPool::allocated_count(pool) > 100)
}

// Test 2: Lazy Evaluation Operations
test "lazy evaluation operations" {
  let mut call_count = 0
  
  let expensive_computation = fn() {
    call_count = call_count + 1
    42 * 42
  }
  
  // Test lazy value creation
  let lazy_value = Lazy::new(expensive_computation)
  assert_eq(call_count, 0) // Computation not executed yet
  
  // Test forced evaluation
  let result = Lazy::force(lazy_value)
  assert_eq(result, 1764)
  assert_eq(call_count, 1) // Computation executed once
  
  // Test memoization (should not recompute)
  let result2 = Lazy::force(lazy_value)
  assert_eq(result2, 1764)
  assert_eq(call_count, 1) // Still only executed once
  
  // Test lazy operations
  let lazy_map = Lazy::map(lazy_value, fn(x) { x / 2 })
  let map_result = Lazy::force(lazy_map)
  assert_eq(map_result, 882)
  assert_eq(call_count, 1) // Still only original computation
}

// Test 3: Batch Processing Operations
test "batch processing operations" {
  let processor = BatchProcessor::new(5) // Batch size of 5
  
  // Test empty processor
  assert_eq(BatchProcessor::pending_count(processor), 0)
  assert_eq(BatchProcessor::processed_count(processor), 0)
  
  // Test adding items to batch
  BatchProcessor::add(processor, "item1")
  BatchProcessor::add(processor, "item2")
  BatchProcessor::add(processor, "item3")
  
  assert_eq(BatchProcessor::pending_count(processor), 3)
  assert_eq(BatchProcessor::processed_count(processor), 0)
  
  // Test processing before batch is full (should not process)
  BatchProcessor::try_process(processor)
  assert_eq(BatchProcessor::pending_count(processor), 3)
  assert_eq(BatchProcessor::processed_count(processor), 0)
  
  // Test filling batch to capacity
  BatchProcessor::add(processor, "item4")
  BatchProcessor::add(processor, "item5")
  
  assert_eq(BatchProcessor::pending_count(processor), 5)
  
  // Test automatic processing when batch is full
  BatchProcessor::add(processor, "item6") // This should trigger processing
  
  assert_eq(BatchProcessor::pending_count(processor), 1) // Only item6 remains
  assert_eq(BatchProcessor::processed_count(processor), 5)
  
  // Test manual flush
  BatchProcessor::flush(processor)
  assert_eq(BatchProcessor::pending_count(processor), 0)
  assert_eq(BatchProcessor::processed_count(processor), 6)
}

// Test 4: Object Pool Operations
test "object pool operations" {
  let pool = ObjectPool::new(fn() { [0, 0, 0] }, fn(arr) { arr[0] = 0; arr[1] = 0; arr[2] = 0 })
  
  // Test empty pool
  assert_eq(ObjectPool::available_count(pool), 0)
  
  // Test object acquisition
  let obj1 = ObjectPool::acquire(pool)
  let obj2 = ObjectPool::acquire(pool)
  
  assert_eq(ObjectPool::available_count(pool), 0)
  
  // Test object modification
  obj1[0] = 1
  obj1[1] = 2
  obj1[2] = 3
  
  // Test object release
  ObjectPool::release(pool, obj1)
  assert_eq(ObjectPool::available_count(pool), 1)
  
  // Test object reuse (should be reset)
  let obj3 = ObjectPool::acquire(pool)
  assert_eq(ObjectPool::available_count(pool), 0)
  assert_eq(obj3[0], 0) // Should be reset
  assert_eq(obj3[1], 0)
  assert_eq(obj3[2], 0)
}

// Test 5: Memoization Operations
test "memoization operations" {
  let mut fib_call_count = 0
  
  let fib = fn(n) {
    fib_call_count = fib_call_count + 1
    if n <= 1 {
      n
    } else {
      // Recursive calls without memoization
      fib(n - 1) + fib(n - 2)
    }
  }
  
  // Test memoized function
  let memoized_fib = Memo::new(fib)
  
  // Test first call
  let result1 = Memo::call(memoized_fib, 5)
  assert_eq(result1, 5)
  
  // Test second call (should use memoized result)
  let result2 = Memo::call(memoized_fib, 5)
  assert_eq(result2, 5)
  
  // Test memoization with different arguments
  let result3 = Memo::call(memoized_fib, 3)
  assert_eq(result3, 2)
  
  // Test cache size
  assert_true(Memo::cache_size(memoized_fib) >= 2)
  
  // Test cache clearing
  Memo::clear(memoized_fib)
  assert_eq(Memo::cache_size(memoized_fib), 0)
}

// Test 6: Streaming Operations
test "streaming operations" {
  let data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let stream = Stream::from_array(data)
  
  // Test filtering
  let even_stream = Stream::filter(stream, fn(x) { x % 2 == 0 })
  let even_result = Stream::collect(even_stream)
  assert_eq(even_result, [2, 4, 6, 8, 10])
  
  // Test mapping
  let stream2 = Stream::from_array(data)
  let doubled_stream = Stream::map(stream2, fn(x) { x * 2 })
  let doubled_result = Stream::collect(doubled_stream)
  assert_eq(doubled_result, [2, 4, 6, 8, 10, 12, 14, 16, 18, 20])
  
  // Test chaining operations
  let stream3 = Stream::from_array(data)
  let chained_result = stream3
    .filter(fn(x) { x > 5 })
    .map(fn(x) { x * x })
    .take(3)
    .collect()
  
  assert_eq(chained_result, [36, 49, 64]) // 6², 7², 8²
  
  // Test reduction
  let stream4 = Stream::from_array(data)
  let sum = Stream::reduce(stream4, 0, fn(acc, x) { acc + x })
  assert_eq(sum, 55)
}

// Test 7: Connection Pool Operations
test "connection pool operations" {
  let pool = ConnectionPool::new(3) // Max 3 connections
  
  // Test empty pool
  assert_eq(ConnectionPool::available_count(pool), 0)
  assert_eq(ConnectionPool::active_count(pool), 0)
  
  // Test connection acquisition
  let conn1 = ConnectionPool::acquire(pool)
  let conn2 = ConnectionPool::acquire(pool)
  let conn3 = ConnectionPool::acquire(pool)
  
  assert_eq(ConnectionPool::available_count(pool), 0)
  assert_eq(ConnectionPool::active_count(pool), 3)
  
  // Test connection release
  ConnectionPool::release(pool, conn1)
  assert_eq(ConnectionPool::available_count(pool), 1)
  assert_eq(ConnectionPool::active_count(pool), 2)
  
  // Test connection reuse
  let conn4 = ConnectionPool::acquire(pool)
  assert_eq(ConnectionPool::available_count(pool), 0)
  assert_eq(ConnectionPool::active_count(pool), 3)
  
  // Test timeout when pool is exhausted
  let conn5 = ConnectionPool::try_acquire(pool, 100) // 100ms timeout
  match conn5 {
    Some(_) => assert_true(false) // Should not acquire
    None => assert_true(true) // Should timeout
  }
}

// Test 8: Adaptive Sampling Operations
test "adaptive sampling operations" {
  let sampler = AdaptiveSampler::new(0.1) // Start with 10% sampling rate
  
  // Test initial sampling rate
  assert_eq(AdaptiveSampler::current_rate(sampler), 0.1)
  
  // Test sampling decisions
  let mut sampled_count = 0
  for i in 0..=100 {
    if AdaptiveSampler::should_sample(sampler) {
      sampled_count = sampled_count + 1
    }
  }
  
  // Should be approximately 10% of 100 (allowing for variance)
  assert_true(sampled_count >= 5 && sampled_count <= 15)
  
  // Test adaptive rate adjustment
  AdaptiveSampler::adjust_rate(sampler, 0.5) // Increase to 50%
  assert_eq(AdaptiveSampler::current_rate(sampler), 0.5)
  
  // Test with new sampling rate
  sampled_count = 0
  for i in 0..=100 {
    if AdaptiveSampler::should_sample(sampler) {
      sampled_count = sampled_count + 1
    }
  }
  
  // Should be approximately 50% of 100 (allowing for variance)
  assert_true(sampled_count >= 40 && sampled_count <= 60)
}

// Test 9: Compression Operations
test "compression operations" {
  let data = "This is a test string that will be compressed. It contains repeated patterns: test string test string test string."
  
  // Test compression
  let compressed = Compression::compress(data)
  assert_true(compressed.length() < data.length()) // Compressed data should be smaller
  
  // Test decompression
  let decompressed = Compression::decompress(compressed)
  assert_eq(decompressed, data) // Should match original
  
  // Test compression ratio
  let ratio = Compression::compression_ratio(data, compressed)
  assert_true(ratio > 0.0 && ratio < 1.0) // Should be between 0 and 1
  
  // Test with different compression levels
  let fast_compressed = Compression::compress_with_level(data, 1) // Fast compression
  let best_compressed = Compression::compress_with_level(data, 9) // Best compression
  
  assert_true(best_compressed.length() <= fast_compressed.length())
}

// Test 10: Indexing Operations
test "indexing operations" {
  let items = [
    ("apple", 10),
    ("banana", 5),
    ("cherry", 15),
    ("date", 8),
    ("elderberry", 12)
  ]
  
  // Test hash index creation
  let hash_index = HashIndex::new(items, fn(item) { item.0 }) // Index by name
  
  // Test index lookup
  let result = HashIndex::lookup(hash_index, "cherry")
  match result {
    Some(item) => {
      assert_eq(item.0, "cherry")
      assert_eq(item.1, 15)
    }
    None => assert_true(false)
  }
  
  // Test non-existent key
  let non_existent = HashIndex::lookup(hash_index, "fig")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test range index creation
  let range_index = RangeIndex::new(items, fn(item) { item.1 }) // Index by value
  
  // Test range query
  let range_result = RangeIndex::range_query(range_index, 8, 12)
  assert_eq(range_result.length(), 3) // Items with values 8, 10, 12
  
  // Test index update
  HashIndex::update(hash_index, "banana", ("banana", 6))
  let updated_result = HashIndex::lookup(hash_index, "banana")
  match updated_result {
    Some(item) => assert_eq(item.1, 6)
    None => assert_true(false)
  }
}