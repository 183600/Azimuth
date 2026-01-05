// Azimuth Caching Mechanism Tests
// This file contains test cases for caching functionality

// Test 1: Basic Cache Operations
test "basic cache operations" {
  let cache = Cache::new()
  
  // Test put and get operations
  Cache::put(cache, "key1", "value1")
  Cache::put(cache, "key2", "value2")
  Cache::put(cache, "key3", "value3")
  
  // Test get operations
  match Cache::get(cache, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match Cache::get(cache, "key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match Cache::get(cache, "key3") {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  // Test get non-existent key
  match Cache::get(cache, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test cache size
  assert_eq(Cache::size(cache), 3)
  
  // Test contains key
  assert_eq(Cache::contains_key(cache, "key1"), true)
  assert_eq(Cache::contains_key(cache, "nonexistent"), false)
  
  // Test remove operation
  Cache::remove(cache, "key2")
  match Cache::get(cache, "key2") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  assert_eq(Cache::size(cache), 2)
  
  // Test clear operation
  Cache::clear(cache)
  assert_eq(Cache::size(cache), 0)
  match Cache::get(cache, "key1") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: TTL (Time To Live) Cache
test "ttl cache operations" {
  let ttl_cache = TTLCache::new()
  
  // Put items with different TTL values
  TTLCache::put_with_ttl(ttl_cache, "short_lived", "value1", 100)   // 100ms TTL
  TTLCache::put_with_ttl(ttl_cache, "medium_lived", "value2", 500)  // 500ms TTL
  TTLCache::put_with_ttl(ttl_cache, "long_lived", "value3", 2000)   // 2000ms TTL
  
  // All items should be available immediately
  match TTLCache::get(ttl_cache, "short_lived") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match TTLCache::get(ttl_cache, "medium_lived") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match TTLCache::get(ttl_cache, "long_lived") {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  // Wait for short-lived item to expire
  Clock::sleep(150)
  
  // Short-lived item should be expired
  match TTLCache::get(ttl_cache, "short_lived") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Medium and long-lived items should still be available
  match TTLCache::get(ttl_cache, "medium_lived") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match TTLCache::get(ttl_cache, "long_lived") {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  // Wait for medium-lived item to expire
  Clock::sleep(400)
  
  // Medium-lived item should now be expired
  match TTLCache::get(ttl_cache, "medium_lived") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Long-lived item should still be available
  match TTLCache::get(ttl_cache, "long_lived") {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  // Test TTL refresh on access
  TTLCache::put_with_ttl(ttl_cache, "refreshable", "value4", 300)  // 300ms TTL
  
  Clock::sleep(200)
  
  // Access the item to refresh its TTL
  match TTLCache::get_and_refresh(ttl_cache, "refreshable") {
    Some(value) => assert_eq(value, "value4")
    None => assert_true(false)
  }
  
  // Wait another 200ms (total 400ms)
  Clock::sleep(200)
  
  // Item should still be available due to refresh
  match TTLCache::get(ttl_cache, "refreshable") {
    Some(value) => assert_eq(value, "value4")
    None => assert_true(false)
  }
}

// Test 3: LRU (Least Recently Used) Cache
test "lru cache operations" {
  let lru_cache = LRUCache::new(3)  // Capacity of 3 items
  
  // Fill cache to capacity
  LRUCache::put(lru_cache, "key1", "value1")
  LRUCache::put(lru_cache, "key2", "value2")
  LRUCache::put(lru_cache, "key3", "value3")
  
  // All items should be available
  assert_eq(LRUCache::size(lru_cache), 3)
  match LRUCache::get(lru_cache, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // Add a fourth item (should evict least recently used)
  LRUCache::put(lru_cache, "key4", "value4")
  
  // Cache size should still be 3
  assert_eq(LRUCache::size(lru_cache), 3)
  
  // key2 should be evicted (it was least recently used)
  match LRUCache::get(lru_cache, "key2") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Other keys should still be available
  match LRUCache::get(lru_cache, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match LRUCache::get(lru_cache, "key3") {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  match LRUCache::get(lru_cache, "key4") {
    Some(value) => assert_eq(value, "value4")
    None => assert_true(false)
  }
  
  // Access key1 to make it most recently used
  match LRUCache::get(lru_cache, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // Add another item (should evict key3, which is now least recently used)
  LRUCache::put(lru_cache, "key5", "value5")
  
  match LRUCache::get(lru_cache, "key3") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // key1 should still be available (it was recently accessed)
  match LRUCache::get(lru_cache, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
}

// Test 4: Cache with Custom Eviction Policy
test "cache with custom eviction policy" {
  // Create cache with size-based eviction policy
  let size_cache = SizeBasedCache::new(100)  // 100 bytes max size
  
  // Add items of different sizes
  SizeBasedCache::put_with_size(size_cache, "small1", "value1", 10)
  SizeBasedCache::put_with_size(size_cache, "small2", "value2", 15)
  SizeBasedCache::put_with_size(size_cache, "medium", "value3", 40)
  
  // Total size: 65 bytes, all should fit
  assert_eq(SizeBasedCache::size(size_cache), 3)
  assert_eq(SizeBasedCache::total_bytes(size_cache), 65)
  
  // Add a large item that would exceed capacity
  SizeBasedCache::put_with_size(size_cache, "large", "value4", 50)
  
  // Cache should evict items to make room
  assert_eq(SizeBasedCache::total_bytes(size_cache) <= 100, true)
  
  // Test LFU (Least Frequently Used) cache
  let lfu_cache = LFUCache::new(3)  // Capacity of 3 items
  
  LFUCache::put(lfu_cache, "key1", "value1")
  LFUCache::put(lfu_cache, "key2", "value2")
  LFUCache::put(lfu_cache, "key3", "value3")
  
  // Access key1 and key2 multiple times to increase their frequency
  for i = 0; i < 3; i = i + 1 {
    match LFUCache::get(lfu_cache, "key1") {
      Some(_) => assert_true(true)
      None => assert_true(false)
    }
  }
  
  for i = 0; i < 2; i = i + 1 {
    match LFUCache::get(lfu_cache, "key2") {
      Some(_) => assert_true(true)
      None => assert_true(false)
    }
  }
  
  // Access key3 only once
  match LFUCache::get(lfu_cache, "key3") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  // Add a new item (should evict key3, which has lowest frequency)
  LFUCache::put(lfu_cache, "key4", "value4")
  
  // key3 should be evicted
  match LFUCache::get(lfu_cache, "key3") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // key1 and key2 should still be available
  match LFUCache::get(lfu_cache, "key1") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  match LFUCache::get(lfu_cache, "key2") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
}

// Test 5: Cache Statistics and Monitoring
test "cache statistics and monitoring" {
  let cache = MonitoredCache::new()
  
  // Perform various operations
  MonitoredCache::put(cache, "key1", "value1")
  MonitoredCache::put(cache, "key2", "value2")
  
  // Test hits
  match MonitoredCache::get(cache, "key1") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  match MonitoredCache::get(cache, "key1") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  // Test miss
  match MonitoredCache::get(cache, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Get statistics
  let stats = MonitoredCache::statistics(cache)
  
  assert_eq(stats.puts, 2)
  assert_eq(stats.gets, 3)
  assert_eq(stats.hits, 2)
  assert_eq(stats.misses, 1)
  assert_eq(stats.hit_rate, 2.0 / 3.0)
  assert_eq(stats.current_size, 2)
  
  // Test eviction statistics
  let lru_monitored = MonitoredLRUCache::new(2)  // Small cache to trigger evictions
  
  MonitoredLRUCache::put(lru_monitored, "key1", "value1")
  MonitoredLRUCache::put(lru_monitored, "key2", "value2")
  MonitoredLRUCache::put(lru_monitored, "key3", "value3")  // Should evict key1
  
  let lru_stats = MonitoredLRUCache::statistics(lru_monitored)
  assert_eq(lru_stats.evictions, 1)
  assert_eq(lru_stats.current_size, 2)
  
  // Test TTL cache statistics
  let ttl_monitored = MonitoredTTLCache::new()
  
  MonitoredTTLCache::put_with_ttl(ttl_monitored, "temp", "value", 50)  // 50ms TTL
  Clock::sleep(100)  // Wait for expiration
  
  match MonitoredTTLCache::get(ttl_monitored, "temp") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let ttl_stats = MonitoredTTLCache::statistics(ttl_monitored)
  assert_eq(ttl_stats.expirations, 1)
}

// Test 6: Cache Persistence
test "cache persistence" {
  let persistent_cache = PersistentCache::new("test_cache.db")
  
  // Add items to cache
  PersistentCache::put(persistent_cache, "user:1", "Alice")
  PersistentCache::put(persistent_cache, "user:2", "Bob")
  PersistentCache::put(persistent_cache, "session:abc", "active")
  
  // Verify items are in cache
  match PersistentCache::get(persistent_cache, "user:1") {
    Some(value) => assert_eq(value, "Alice")
    None => assert_true(false)
  }
  
  // Simulate cache restart by creating new instance with same storage
  let restarted_cache = PersistentCache::new("test_cache.db")
  
  // Items should still be available after restart
  match PersistentCache::get(restarted_cache, "user:1") {
    Some(value) => assert_eq(value, "Alice")
    None => assert_true(false)
  }
  
  match PersistentCache::get(restarted_cache, "user:2") {
    Some(value) => assert_eq(value, "Bob")
    None => assert_true(false)
  }
  
  match PersistentCache::get(restarted_cache, "session:abc") {
    Some(value) => assert_eq(value, "active")
    None => assert_true(false)
  }
  
  // Test persistence with TTL
  PersistentCache::put_with_ttl(restarted_cache, "temp", "temporary", 100)  // 100ms TTL
  Clock::sleep(150)
  
  match PersistentCache::get(restarted_cache, "temp") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Clean up
  PersistentCache::destroy(restarted_cache)
}

// Test 7: Distributed Cache
test "distributed cache operations" {
  let local_cache1 = DistributedCache::new("node1", ["localhost:8001"])
  let local_cache2 = DistributedCache::new("node2", ["localhost:8002"])
  
  // Connect nodes
  DistributedCache::connect(local_cache1, local_cache2)
  
  // Put item in first cache
  DistributedCache::put(local_cache1, "shared_key", "shared_value")
  
  // Item should be available in both caches
  match DistributedCache::get(local_cache1, "shared_key") {
    Some(value) => assert_eq(value, "shared_value")
    None => assert_true(false)
  }
  
  // Allow time for replication
  Clock::sleep(100)
  
  match DistributedCache::get(local_cache2, "shared_key") {
    Some(value) => assert_eq(value, "shared_value")
    None => assert_true(false)
  }
  
  // Test cache invalidation across nodes
  DistributedCache::invalidate(local_cache1, "shared_key")
  
  // Allow time for invalidation to propagate
  Clock::sleep(100)
  
  match DistributedCache::get(local_cache1, "shared_key") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  match DistributedCache::get(local_cache2, "shared_key") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test cache statistics across cluster
  let cluster_stats = DistributedCache::cluster_statistics(local_cache1)
  assert_eq(cluster_stats.node_count, 2)
  assert_eq(cluster_stats.total_items, 0)  // All items invalidated
  
  // Disconnect nodes
  DistributedCache::disconnect(local_cache1, local_cache2)
}

// Test 8: Cache with Write-through and Write-behind Policies
test "cache write policies" {
  let storage = MockStorage::new()
  
  // Test write-through cache
  let write_through_cache = WriteThroughCache::new(storage)
  
  WriteThroughCache::put(write_through_cache, "key1", "value1")
  
  // Value should be in cache and storage
  match WriteThroughCache::get(write_through_cache, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match MockStorage::get(storage, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // Test write-behind cache
  let write_behind_cache = WriteBehindCache::new(storage, 50)  // 50ms delay
  
  WriteBehindCache::put(write_behind_cache, "key2", "value2")
  
  // Value should be in cache immediately
  match WriteBehindCache::get(write_behind_cache, "key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  // But not yet in storage
  match MockStorage::get(storage, "key2") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Wait for write-behind to complete
  Clock::sleep(100)
  
  // Now value should be in storage
  match MockStorage::get(storage, "key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  // Test write-behind with multiple updates
  WriteBehindCache::put(write_behind_cache, "key3", "initial_value")
  WriteBehindCache::put(write_behind_cache, "key3", "updated_value")
  
  Clock::sleep(100)
  
  // Should have the latest value in storage
  match MockStorage::get(storage, "key3") {
    Some(value) => assert_eq(value, "updated_value")
    None => assert_true(false)
  }
}

// Test 9: Cache with Compression
test "cache compression" {
  let compressed_cache = CompressedCache::new()
  
  // Put large data that will benefit from compression
  let large_data = "x" * 10000  // 10KB of 'x' characters
  let repetitive_data = ("abc123" * 1000) + ("def456" * 1000)  // Repetitive data
  
  CompressedCache::put(compressed_cache, "large", large_data)
  CompressedCache::put(compressed_cache, "repetitive", repetitive_data)
  
  // Retrieve and verify data integrity
  match CompressedCache::get(compressed_cache, "large") {
    Some(value) => assert_eq(value, large_data)
    None => assert_true(false)
  }
  
  match CompressedCache::get(compressed_cache, "repetitive") {
    Some(value) => assert_eq(value, repetitive_data)
    None => assert_true(false)
  }
  
  // Check compression statistics
  let compression_stats = CompressedCache::compression_statistics(compressed_cache)
  assert_eq(compression_stats.total_items, 2)
  assert_eq(compression_stats.compression_ratio < 1.0, true)  // Should achieve compression
  assert_eq(compression_stats.space_saved > 0, true)
  
  // Test with data that doesn't compress well
  let random_data = generate_random_string(1000)  // Random data
  CompressedCache::put(compressed_cache, "random", random_data)
  
  let updated_stats = CompressedCache::compression_statistics(compressed_cache)
  assert_eq(updated_stats.total_items, 3)
  
  // Random data should have worse compression ratio
  match CompressedCache::get(compressed_cache, "random") {
    Some(value) => assert_eq(value, random_data)
    None => assert_true(false)
  }
}

// Test 10: Cache Hierarchies and Multi-level Caching
test "cache hierarchies" {
  // Create multi-level cache: L1 (in-memory) -> L2 (disk) -> L3 (network)
  let l1_cache = LRUCache::new(100)  // 100 items in memory
  let l2_cache = DiskCache::new("/tmp/l2_cache", 1000)  // 1000 items on disk
  let l3_cache = NetworkCache::new("http://cache-server.example.com")
  
  let hierarchical_cache = HierarchicalCache::new([l1_cache, l2_cache, l3_cache])
  
  // Test cache miss at all levels
  match HierarchicalCache::get(hierarchical_cache, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test put and get
  HierarchicalCache::put(hierarchical_cache, "key1", "value1")
  
  // Should be in L1 cache
  match LRUCache::get(l1_cache, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // Get should retrieve from L1
  match HierarchicalCache::get(hierarchical_cache, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // Fill L1 cache to capacity to trigger eviction to L2
  for i = 0; i < 105; i = i + 1 {
    let key = "temp_key" + i.to_string()
    let value = "temp_value" + i.to_string()
    HierarchicalCache::put(hierarchical_cache, key, value)
  }
  
  // Original item should have been evicted from L1 but promoted to L2
  match LRUCache::get(l1_cache, "key1") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // But should still be available through hierarchical cache (from L2)
  match HierarchicalCache::get(hierarchical_cache, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // After accessing, it should be back in L1
  match LRUCache::get(l1_cache, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // Test cache statistics across all levels
  let hierarchy_stats = HierarchicalCache::statistics(hierarchical_cache)
  assert_eq(hierarchy_stats.l1_hits > 0, true)
  assert_eq(hierarchy_stats.l2_hits > 0, true)
  assert_eq(hierarchy_stats.total_hits == hierarchy_stats.l1_hits + hierarchy_stats.l2_hits + hierarchy_stats.l3_hits, true)
  
  // Test cache warming
  let warm_keys = ["key1", "key2", "key3"]
  for key in warm_keys {
    HierarchicalCache::put(hierarchical_cache, key, "value_for_" + key)
  }
  
  // Evict from L1
  for i = 0; i < 105; i = i + 1 {
    let key = "evict_key" + i.to_string()
    let value = "evict_value" + i.to_string()
    HierarchicalCache::put(hierarchical_cache, key, value)
  }
  
  // Warm the cache
  HierarchicalCache::warm(hierarchical_cache, warm_keys)
  
  // All warm keys should now be in L1
  for key in warm_keys {
    match LRUCache::get(l1_cache, key) {
      Some(_) => assert_true(true)
      None => assert_true(false)
    }
  }
}