// Azimuth Premium Resource Optimization Tests
// This file contains high-quality test cases focusing on resource optimization, memory management, and performance

// Test 1: Memory Pool Management
test "memory pool allocation and deallocation" {
  let pool = MemoryPool::new(1000)  // Pool with 1000 bytes capacity
  
  // Test initial state
  assert_eq(MemoryPool::capacity(pool), 1000)
  assert_eq(MemoryPool::used(pool), 0)
  assert_eq(MemoryPool::available(pool), 1000)
  
  // Test allocation
  let block1 = MemoryPool::allocate(pool, 100)
  assert_true(MemoryPool::is_valid_block(block1))
  assert_eq(MemoryPool::block_size(block1), 100)
  assert_eq(MemoryPool::used(pool), 100)
  assert_eq(MemoryPool::available(pool), 900)
  
  // Test multiple allocations
  let block2 = MemoryPool::allocate(pool, 200)
  let block3 = MemoryPool::allocate(pool, 300)
  assert_eq(MemoryPool::used(pool), 600)
  assert_eq(MemoryPool::available(pool), 400)
  
  // Test deallocation
  MemoryPool::deallocate(pool, block2)
  assert_eq(MemoryPool::used(pool), 400)
  assert_eq(MemoryPool::available(pool), 600)
  
  // Test pool fragmentation
  let block4 = MemoryPool::allocate(pool, 250)  // Should fit in freed space
  assert_true(MemoryPool::is_valid_block(block4))
  assert_eq(MemoryPool::used(pool), 650)
  assert_eq(MemoryPool::available(pool), 350)
  
  // Test cleanup
  MemoryPool::cleanup(pool)
  assert_eq(MemoryPool::used(pool), 0)
  assert_eq(MemoryPool::available(pool), 1000)
}

// Test 2: Resource Cache Optimization
test "resource cache optimization with LRU eviction" {
  let cache = ResourceCache::new(5)  // Cache with capacity for 5 items
  
  // Test initial state
  assert_eq(ResourceCache::size(cache), 0)
  assert_eq(ResourceCache::capacity(cache), 5)
  assert_true(ResourceCache::is_empty(cache))
  
  // Test adding items
  ResourceCache::put(cache, "key1", "value1")
  ResourceCache::put(cache, "key2", "value2")
  ResourceCache::put(cache, "key3", "value3")
  
  assert_eq(ResourceCache::size(cache), 3)
  assert_false(ResourceCache::is_empty(cache))
  
  // Test retrieving items
  match ResourceCache::get(cache, "key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  // Test LRU order after access
  ResourceCache::put(cache, "key4", "value4")
  ResourceCache::put(cache, "key5", "value5")
  assert_eq(ResourceCache::size(cache), 5)
  
  // Test eviction when capacity exceeded
  ResourceCache::put(cache, "key6", "value6")  // Should evict key1 (least recently used)
  assert_eq(ResourceCache::size(cache), 5)
  
  match ResourceCache::get(cache, "key1") {
    Some(_) => assert_true(false)  // key1 should be evicted
    None => assert_true(true)
  }
  
  match ResourceCache::get(cache, "key6") {
    Some(value) => assert_eq(value, "value6")
    None => assert_true(false)
  }
  
  // Test cache statistics
  let stats = ResourceCache::statistics(cache)
  assert_eq(stats.hits, 2)  // key2 and key6
  assert_eq(stats.misses, 1)  // key1 after eviction
  assert_eq(stats.evictions, 1)
}

// Test 3: Telemetry Data Compression
test "telemetry data compression optimization" {
  let compressor = Compressor::new(CompressionLevel::Balanced)
  
  // Test simple string compression
  let original_data = "This is a test string for compression. It contains repeated patterns to test compression efficiency."
  let compressed_data = Compressor::compress(compressor, original_data)
  
  assert_true(compressed_data.length() < original_data.length())
  
  // Test decompression
  let decompressed_data = Compressor::decompress(compressor, compressed_data)
  assert_eq(decompressed_data, original_data)
  
  // Test telemetry batch compression
  let telemetry_batch = [
    ("metric1", 42.5),
    ("metric2", 38.7),
    ("metric3", 91.2),
    ("metric1", 43.1),  // Repeated key
    ("metric2", 39.3),  // Repeated key
    ("metric4", 12.8)
  ]
  
  let compressed_batch = Compressor::compress_telemetry_batch(compressor, telemetry_batch)
  let decompressed_batch = Compressor::decompress_telemetry_batch(compressor, compressed_batch)
  
  assert_eq(decompressed_batch.length(), telemetry_batch.length())
  for i in 0..telemetry_batch.length() {
    assert_eq(decompressed_batch[i], telemetry_batch[i])
  }
  
  // Test compression ratio calculation
  let ratio = Compressor::compression_ratio(compressor, original_data.length(), compressed_data.length())
  assert_true(ratio > 0.0 && ratio < 1.0)
}

// Test 4: Concurrent Resource Access
test "concurrent resource access with synchronization" {
  let resource = SharedResource::new("initial_value")
  let access_count = AtomicCounter::new(0)
  
  // Test concurrent reads
  let reader1 = Reader::new(resource)
  let reader2 = Reader::new(resource)
  
  let value1 = Reader::read(reader1)
  let value2 = Reader::read(reader2)
  
  assert_eq(value1, "initial_value")
  assert_eq(value2, "initial_value")
  assert_eq(AtomicCounter::get(access_count), 2)
  
  // Test concurrent writes with proper synchronization
  let writer = Writer::new(resource)
  
  Writer::write(writer, "updated_value_1")
  assert_eq(Reader::read(reader1), "updated_value_1")
  
  Writer::write(writer, "updated_value_2")
  assert_eq(Reader::read(reader2), "updated_value_2")
  
  // Test read-write lock behavior
  let rw_lock = ReadWriteLock::new()
  
  // Acquire read lock
  ReadWriteLock::acquire_read(rw_lock)
  assert_true(ReadWriteLock::has_readers(rw_lock))
  assert_false(ReadWriteLock::has_writer(rw_lock))
  
  // Try to acquire write lock (should block or fail)
  let write_acquired = ReadWriteLock::try_acquire_write(rw_lock)
  assert_false(write_acquired)
  
  // Release read lock and acquire write lock
  ReadWriteLock::release_read(rw_lock)
  assert_true(ReadWriteLock::try_acquire_write(rw_lock))
  assert_true(ReadWriteLock::has_writer(rw_lock))
  
  ReadWriteLock::release_write(rw_lock)
}

// Test 5: Adaptive Sampling Strategy
test "adaptive sampling strategy optimization" {
  let sampler = AdaptiveSampler::new(
    base_rate = 0.1,  // 10% base sampling rate
    max_rate = 0.5,   // 50% maximum sampling rate
    threshold = 100    // Adjust after 100 samples
  )
  
  // Test initial sampling rate
  assert_eq(AdaptiveSampler::current_rate(sampler), 0.1)
  
  // Test sampling decisions
  let mut sampled_count = 0
  for i in 1..=100 {
    if AdaptiveSampler::should_sample(sampler, "trace_" + i.to_string()) {
      sampled_count = sampled_count + 1
    }
  }
  
  // Should be approximately 10% of 100 = 10 samples (with some variance)
  assert_true(sampled_count >= 5 && sampled_count <= 15)
  
  // Test rate adjustment based on error rate
  AdaptiveSampler::update_error_rate(sampler, 0.05)  // 5% error rate
  assert_true(AdaptiveSampler::current_rate(sampler) > 0.1)
  
  // Test rate adjustment based on throughput
  AdaptiveSampler::update_throughput(sampler, 1000)  // High throughput
  assert_true(AdaptiveSampler::current_rate(sampler) < 0.5)
  
  // Test boundary conditions
  AdaptiveSampler::update_error_rate(sampler, 0.5)   // 50% error rate
  assert_eq(AdaptiveSampler::current_rate(sampler), 0.5)  // Should be at max
  
  AdaptiveSampler::update_error_rate(sampler, 0.0)   // 0% error rate
  assert_true(AdaptiveSampler::current_rate(sampler) > 0.1)  // Should not drop below base
}

// Test 6: Telemetry Buffer Optimization
test "telemetry buffer optimization with circular buffer" {
  let buffer = CircularBuffer::new(100)  // Buffer with capacity for 100 items
  
  // Test initial state
  assert_eq(CircularBuffer::size(buffer), 0)
  assert_eq(CircularBuffer::capacity(buffer), 100)
  assert_true(CircularBuffer::is_empty(buffer))
  assert_false(CircularBuffer::is_full(buffer))
  
  // Test adding items
  for i in 1..=50 {
    CircularBuffer::push(buffer, "item_" + i.to_string())
  }
  
  assert_eq(CircularBuffer::size(buffer), 50)
  assert_false(CircularBuffer::is_empty(buffer))
  assert_false(CircularBuffer::is_full(buffer))
  
  // Test removing items
  let item = CircularBuffer::pop(buffer)
  match item {
    Some(value) => assert_eq(value, "item_1")
    None => assert_true(false)
  }
  
  assert_eq(CircularBuffer::size(buffer), 49)
  
  // Test buffer wraparound
  for i in 51..=150 {  // Add 100 more items (total 150, capacity 100)
    CircularBuffer::push(buffer, "item_" + i.to_string())
  }
  
  assert_eq(CircularBuffer::size(buffer), 100)
  assert_true(CircularBuffer::is_full(buffer))
  
  // Test that oldest items were overwritten
  let old_item = CircularBuffer::pop(buffer)
  match old_item {
    Some(value) => assert_eq(value, "item_51")  // First item should be 51, not 1
    None => assert_true(false)
  }
  
  // Test batch operations
  let batch = CircularBuffer::drain_batch(buffer, 10)
  assert_eq(batch.length(), 10)
  assert_eq(CircularBuffer::size(buffer), 90)
  
  // Test buffer reset
  CircularBuffer::clear(buffer)
  assert_eq(CircularBuffer::size(buffer), 0)
  assert_true(CircularBuffer::is_empty(buffer))
}

// Test 7: Resource Leak Detection
test "resource leak detection and prevention" {
  let leak_detector = LeakDetector::new()
  
  // Test resource tracking
  let resource1 = Resource::tracked_new(leak_detector, "resource1")
  let resource2 = Resource::tracked_new(leak_detector, "resource2")
  
  assert_eq(LeakDetector::active_resources(leak_detector), 2)
  
  // Test proper resource cleanup
  Resource::tracked_drop(resource1)
  assert_eq(LeakDetector::active_resources(leak_detector), 1)
  
  // Test leak detection
  let leak_report = LeakDetector::generate_report(leak_detector)
  assert_eq(leak_report.total_leaks, 1)
  assert_eq(leak_report.leaked_resources[0].name, "resource2")
  
  // Test automatic cleanup
  LeakDetector::cleanup_all(leak_detector)
  assert_eq(LeakDetector::active_resources(leak_detector), 0)
  
  // Test memory usage tracking
  let initial_memory = LeakDetector::memory_usage(leak_detector)
  let large_resource = Resource::tracked_new(leak_detector, "large_resource", 10000)
  let current_memory = LeakDetector::memory_usage(leak_detector)
  
  assert_true(current_memory > initial_memory)
  assert_eq(current_memory - initial_memory, 10000)
  
  Resource::tracked_drop(large_resource)
  assert_eq(LeakDetector::memory_usage(leak_detector), initial_memory)
}

// Test 8: Telemetry Data Aggregation Optimization
test "telemetry data aggregation optimization" {
  let aggregator = DataAggregator::new(window_size = 60)  // 60-second window
  
  // Test adding data points
  DataAggregator::add_point(aggregator, 10.5)
  DataAggregator::add_point(aggregator, 15.3)
  DataAggregator::add_point(aggregator, 12.7)
  DataAggregator::add_point(aggregator, 18.9)
  DataAggregator::add_point(aggregator, 14.2)
  
  // Test statistical calculations
  let stats = DataAggregator::calculate_stats(aggregator)
  assert_eq(stats.count, 5)
  assert_eq(stats.min, 10.5)
  assert_eq(stats.max, 18.9)
  assert_eq(stats.sum, 71.6)
  assert_eq(stats.average, 14.32)
  
  // Test percentile calculations
  let p50 = DataAggregator::percentile(aggregator, 50)
  let p95 = DataAggregator::percentile(aggregator, 95)
  
  assert_true(p50 >= 12.7 && p50 <= 15.3)  // Median should be in this range
  assert_eq(p95, 18.9)  // 95th percentile should be max
  
  // Test time-based window expiration
  DataAggregator::advance_time(aggregator, 61)  // Advance beyond window size
  DataAggregator::add_point(aggregator, 20.1)
  
  let new_stats = DataAggregator::calculate_stats(aggregator)
  assert_eq(new_stats.count, 1)  // Only the new point should remain
  assert_eq(new_stats.sum, 20.1)
  
  // Test batch aggregation
  let batch_data = [5.1, 8.3, 12.7, 16.9, 22.4, 7.8, 11.2, 19.6]
  DataAggregator::add_batch(aggregator, batch_data)
  
  let batch_stats = DataAggregator::calculate_stats(aggregator)
  assert_eq(batch_stats.count, 9)  // 1 existing + 8 new
  assert_eq(batch_stats.min, 5.1)
  assert_eq(batch_stats.max, 22.4)
}

// Test 9: Priority Queue for Telemetry Processing
test "priority queue for telemetry processing optimization" {
  let priority_queue = PriorityQueue::new()  // Max-heap by default
  
  // Test adding items with different priorities
  PriorityQueue::push(priority_queue, TelemetryItem::new("low_priority", 1))
  PriorityQueue::push(priority_queue, TelemetryItem::new("high_priority", 10))
  PriorityQueue::push(priority_queue, TelemetryItem::new("medium_priority", 5))
  PriorityQueue::push(priority_queue, TelemetryItem::new("urgent_priority", 15))
  
  assert_eq(PriorityQueue::size(priority_queue), 4)
  
  // Test priority-based extraction
  let item1 = PriorityQueue::pop(priority_queue)
  match item1 {
    Some(item) => assert_eq(item.name, "urgent_priority")
    None => assert_true(false)
  }
  
  let item2 = PriorityQueue::pop(priority_queue)
  match item2 {
    Some(item) => assert_eq(item.name, "high_priority")
    None => assert_true(false)
  }
  
  let item3 = PriorityQueue::pop(priority_queue)
  match item3 {
    Some(item) => assert_eq(item.name, "medium_priority")
    None => assert_true(false)
  }
  
  let item4 = PriorityQueue::pop(priority_queue)
  match item4 {
    Some(item) => assert_eq(item.name, "low_priority")
    None => assert_true(false)
  }
  
  // Test empty queue
  assert_true(PriorityQueue::is_empty(priority_queue))
  let empty_item = PriorityQueue::pop(priority_queue)
  match empty_item {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test batch operations
  let batch_items = [
    TelemetryItem::new("batch1", 3),
    TelemetryItem::new("batch2", 7),
    TelemetryItem::new("batch3", 2),
    TelemetryItem::new("batch4", 9)
  ]
  
  PriorityQueue::push_batch(priority_queue, batch_items)
  assert_eq(PriorityQueue::size(priority_queue), 4)
  
  // Test peek without removing
  let top_item = PriorityQueue::peek(priority_queue)
  match top_item {
    Some(item) => assert_eq(item.name, "batch4")  // Highest priority
    None => assert_true(false)
  }
  
  // Verify size unchanged after peek
  assert_eq(PriorityQueue::size(priority_queue), 4)
}

// Test 10: Telemetry Data Deduplication
test "telemetry data deduplication optimization" {
  let deduplicator = Deduplicator::new(window_size = 300)  // 5-minute window
  
  // Test duplicate detection
  let original_item = TelemetryData::new("metric1", 42.5, 1000)
  let duplicate_item = TelemetryData::new("metric1", 42.5, 1000)
  let similar_item = TelemetryData::new("metric1", 42.6, 1000)  // Slightly different value
  let different_item = TelemetryData::new("metric2", 42.5, 1000)  // Different metric
  
  // Test first item (should not be duplicate)
  assert_false(Deduplicator::is_duplicate(deduplicator, original_item))
  
  // Test exact duplicate
  assert_true(Deduplicator::is_duplicate(deduplicator, duplicate_item))
  
  // Test similar but not identical
  assert_false(Deduplicator::is_duplicate(deduplicator, similar_item))
  
  // Test different metric
  assert_false(Deduplicator::is_duplicate(deduplicator, different_item))
  
  // Test time-based expiration
  Deduplicator::advance_time(deduplicator, 301)  // Advance beyond window
  let expired_duplicate = TelemetryData::new("metric1", 42.5, 1000)
  assert_false(Deduplicator::is_duplicate(deduplicator, expired_duplicate))
  
  // Test deduplication statistics
  let stats = Deduplicator::statistics(deduplicator)
  assert_eq(stats.total_processed, 4)
  assert_eq(stats.duplicates_found, 1)
  assert_eq(stats.duplicate_rate, 0.25)
  
  // Test batch deduplication
  let batch = [
    TelemetryData::new("batch1", 1.0, 2000),
    TelemetryData::new("batch2", 2.0, 2000),
    TelemetryData::new("batch1", 1.0, 2000),  // Duplicate
    TelemetryData::new("batch3", 3.0, 2000),
    TelemetryData::new("batch2", 2.0, 2000),  // Duplicate
  ]
  
  let deduplicated_batch = Deduplicator::deduplicate_batch(deduplicator, batch)
  assert_eq(deduplicated_batch.length(), 3)  // 2 duplicates removed
  
  // Test fuzzy matching (within tolerance)
  let fuzzy_item1 = TelemetryData::new("fuzzy", 10.0, 3000)
  let fuzzy_item2 = TelemetryData::new("fuzzy", 10.05, 3000)  // Within 0.1 tolerance
  
  Deduplicator::set_tolerance(deduplicator, 0.1)
  assert_false(Deduplicator::is_duplicate(deduplicator, fuzzy_item1))
  assert_true(Deduplicator::is_duplicate(deduplicator, fuzzy_item2))
}