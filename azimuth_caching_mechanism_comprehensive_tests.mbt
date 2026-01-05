// Azimuth Telemetry System - Caching Mechanism Tests
// This file contains comprehensive test cases for caching mechanisms

// Test 1: Basic Cache Operations
test "basic cache operations" {
  // Test LRU cache
  let cache = LRUCache::new(3)
  
  // Test put and get
  LRUCache::put(cache, "key1", "value1")
  LRUCache::put(cache, "key2", "value2")
  LRUCache::put(cache, "key3", "value3")
  
  assert_eq(LRUCache::get(cache, "key1"), Some("value1"))
  assert_eq(LRUCache::get(cache, "key2"), Some("value2"))
  assert_eq(LRUCache::get(cache, "key3"), Some("value3"))
  assert_eq(LRUCache::get(cache, "nonexistent"), None)
  
  // Test cache size
  assert_eq(LRUCache::size(cache), 3)
  assert_true(LRUCache::is_full(cache))
  
  // Test eviction (adding a 4th item should evict the least recently used)
  LRUCache::put(cache, "key4", "value4")
  assert_eq(LRUCache::size(cache), 3)
  assert_eq(LRUCache::get(cache, "key1"), None)  // key1 should be evicted
  assert_eq(LRUCache::get(cache, "key2"), Some("value2"))
  assert_eq(LRUCache::get(cache, "key3"), Some("value3"))
  assert_eq(LRUCache::get(cache, "key4"), Some("value4"))
  
  // Test LRU order (accessing key2 should make it most recently used)
  LRUCache::get(cache, "key2")
  LRUCache::put(cache, "key5", "value5")
  assert_eq(LRUCache::get(cache, "key2"), Some("value2"))  // key2 should not be evicted
  assert_eq(LRUCache::get(cache, "key3"), None)  // key3 should be evicted
  
  // Test remove
  LRUCache::remove(cache, "key2")
  assert_eq(LRUCache::get(cache, "key2"), None)
  assert_eq(LRUCache::size(cache), 2)
  
  // Test clear
  LRUCache::clear(cache)
  assert_eq(LRUCache::size(cache), 0)
  assert_true(LRUCache::is_empty(cache))
}

// Test 2: TTL (Time To Live) Cache
test "ttl cache operations" {
  // Test TTL cache with 1 second expiration
  let cache = TTLCache::new(100, 1000)  // 100 items, 1 second TTL
  
  // Test put and get
  TTLCache::put(cache, "key1", "value1")
  assert_eq(TTLCache::get(cache, "key1"), Some("value1"))
  
  // Test expiration
  sleep(1100)  // Wait longer than TTL
  assert_eq(TTLCache::get(cache, "key1"), None)  // Should be expired
  
  // Test refresh on access
  TTLCache::put(cache, "key2", "value2")
  sleep(500)  // Wait half of TTL
  assert_eq(TTLCache::get(cache, "key2"), Some("value2"))  // Should refresh TTL
  sleep(700)  // Wait remaining time
  assert_eq(TTLCache::get(cache, "key2"), Some("value2"))  // Should still be valid
  
  // Test manual expiration
  TTLCache::put(cache, "key3", "value3")
  TTLCache::expire(cache, "key3")
  assert_eq(TTLCache::get(cache, "key3"), None)  // Should be expired
  
  // Test cleanup of expired items
  TTLCache::put(cache, "key4", "value4")
  TTLCache::put(cache, "key5", "value5")
  sleep(1100)  // Wait for expiration
  TTLCache::cleanup(cache)  // Manually cleanup expired items
  assert_eq(TTLCache::size(cache), 0)  // All items should be cleaned up
  
  // Test auto cleanup
  TTLCache::put(cache, "key6", "value6")
  TTLCache::put(cache, "key7", "value7")
  sleep(1100)  // Wait for expiration
  TTLCache::put(cache, "key8", "value8")  // Auto cleanup should happen
  assert_eq(TTLCache::get(cache, "key6"), None)  // Should be cleaned up
  assert_eq(TTLCache::get(cache, "key7"), None)  // Should be cleaned up
  assert_eq(TTLCache::get(cache, "key8"), Some("value8"))  // Should be valid
}

// Test 3: LFU (Least Frequently Used) Cache
test "lfu cache operations" {
  // Test LFU cache with capacity 3
  let cache = LFUCache::new(3)
  
  // Test put and get
  LFUCache::put(cache, "key1", "value1")
  LFUCache::put(cache, "key2", "value2")
  LFUCache::put(cache, "key3", "value3")
  
  // Access key1 multiple times to increase its frequency
  LFUCache::get(cache, "key1")
  LFUCache::get(cache, "key1")
  LFUCache::get(cache, "key1")
  
  // Access key2 once
  LFUCache::get(cache, "key2")
  
  // key3 has frequency 1, key2 has frequency 2, key1 has frequency 3
  
  // Add a new item, should evict key3 (least frequently used)
  LFUCache::put(cache, "key4", "value4")
  assert_eq(LFUCache::get(cache, "key3"), None)  // key3 should be evicted
  assert_eq(LFUCache::get(cache, "key1"), Some("value1"))  // key1 should remain
  assert_eq(LFUCache::get(cache, "key2"), Some("value2"))  // key2 should remain
  assert_eq(LFUCache::get(cache, "key4"), Some("value4"))  // key4 should be added
  
  // Test frequency update
  LFUCache::get(cache, "key2")
  LFUCache::get(cache, "key2")
  // Now key2 has frequency 3, key1 has frequency 3, key4 has frequency 1
  
  // Add a new item, should evict key4 (least frequently used)
  LFUCache::put(cache, "key5", "value5")
  assert_eq(LFUCache::get(cache, "key4"), None)  // key4 should be evicted
  assert_eq(LFUCache::get(cache, "key1"), Some("value1"))  // key1 should remain
  assert_eq(LFUCache::get(cache, "key2"), Some("value2"))  // key2 should remain
  assert_eq(LFUCache::get(cache, "key5"), Some("value5"))  // key5 should be added
}

// Test 4: Cache Statistics and Monitoring
test "cache statistics and monitoring" {
  // Test cache with statistics enabled
  let cache = MonitoredCache::new(LRUCache::new(3))
  
  // Test hit/miss statistics
  assert_eq(MonitoredCache::hit_count(cache), 0)
  assert_eq(MonitoredCache::miss_count(cache), 0)
  assert_eq(MonitoredCache::hit_rate(cache), 0.0)
  
  // Miss
  MonitoredCache::get(cache, "nonexistent")
  assert_eq(MonitoredCache::hit_count(cache), 0)
  assert_eq(MonitoredCache::miss_count(cache), 1)
  assert_eq(MonitoredCache::hit_rate(cache), 0.0)
  
  // Put and hit
  MonitoredCache::put(cache, "key1", "value1")
  MonitoredCache::get(cache, "key1")
  assert_eq(MonitoredCache::hit_count(cache), 1)
  assert_eq(MonitoredCache::miss_count(cache), 1)
  assert_eq(MonitoredCache::hit_rate(cache), 0.5)
  
  // Another hit
  MonitoredCache::get(cache, "key1")
  assert_eq(MonitoredCache::hit_count(cache), 2)
  assert_eq(MonitoredCache::miss_count(cache), 1)
  assert_eq(MonitoredCache::hit_rate(cache), 2.0/3.0)
  
  // Another miss
  MonitoredCache::get(cache, "nonexistent2")
  assert_eq(MonitoredCache::hit_count(cache), 2)
  assert_eq(MonitoredCache::miss_count(cache), 2)
  assert_eq(MonitoredCache::hit_rate(cache), 0.5)
  
  // Test eviction statistics
  assert_eq(MonitoredCache::eviction_count(cache), 0)
  
  // Fill cache to capacity
  MonitoredCache::put(cache, "key2", "value2")
  MonitoredCache::put(cache, "key3", "value3")
  
  // Add one more to trigger eviction
  MonitoredCache::put(cache, "key4", "value4")
  assert_eq(MonitoredCache::eviction_count(cache), 1)
  
  // Test size statistics
  assert_eq(MonitoredCache::size(cache), 3)
  assert_eq(MonitoredCache::capacity(cache), 3)
  assert_eq(MonitoredCache::utilization(cache), 1.0)
  
  // Reset statistics
  MonitoredCache::reset_stats(cache)
  assert_eq(MonitoredCache::hit_count(cache), 0)
  assert_eq(MonitoredCache::miss_count(cache), 0)
  assert_eq(MonitoredCache::eviction_count(cache), 0)
}

// Test 5: Cache Persistence
test "cache persistence" {
  // Test cache persistence to file
  let cache1 = LRUCache::new(100)
  
  // Add some data
  LRUCache::put(cache1, "key1", "value1")
  LRUCache::put(cache1, "key2", "value2")
  LRUCache::put(cache1, "key3", "value3")
  
  // Save to file
  let file_path = "/tmp/cache_test.dat"
  CachePersistence::save_to_file(cache1, file_path)
  
  // Load into new cache
  let cache2 = LRUCache::new(100)
  CachePersistence::load_from_file(cache2, file_path)
  
  // Verify data was loaded
  assert_eq(LRUCache::get(cache2, "key1"), Some("value1"))
  assert_eq(LRUCache::get(cache2, "key2"), Some("value2"))
  assert_eq(LRUCache::get(cache2, "key3"), Some("value3"))
  assert_eq(LRUCache::size(cache2), 3)
  
  // Clean up
  delete_file(file_path)
  
  // Test cache persistence with TTL
  let ttl_cache1 = TTLCache::new(100, 10000)  // 10 second TTL
  
  // Add some data
  TTLCache::put(ttl_cache1, "key1", "value1")
  TTLCache::put(ttl_cache1, "key2", "value2")
  TTLCache::put(ttl_cache1, "key3", "value3")
  
  // Save to file
  let ttl_file_path = "/tmp/ttl_cache_test.dat"
  CachePersistence::save_to_file(ttl_cache1, ttl_file_path)
  
  // Load into new cache
  let ttl_cache2 = TTLCache::new(100, 10000)
  CachePersistence::load_from_file(ttl_cache2, ttl_file_path)
  
  // Verify data was loaded
  assert_eq(TTLCache::get(ttl_cache2, "key1"), Some("value1"))
  assert_eq(TTLCache::get(ttl_cache2, "key2"), Some("value2"))
  assert_eq(TTLCache::get(ttl_cache2, "key3"), Some("value3"))
  assert_eq(TTLCache::size(ttl_cache2), 3)
  
  // Clean up
  delete_file(ttl_file_path)
}

// Test 6: Distributed Cache
test "distributed cache operations" {
  // Test distributed cache with multiple nodes
  let node1 = DistributedCacheNode::new("node1", "localhost:8001")
  let node2 = DistributedCacheNode::new("node2", "localhost:8002")
  let node3 = DistributedCacheNode::new("node3", "localhost:8003")
  
  // Create distributed cache
  let cache = DistributedCache::new([node1, node2, node3])
  
  // Test put and get
  DistributedCache::put(cache, "key1", "value1")
  assert_eq(DistributedCache::get(cache, "key1"), Some("value1"))
  
  // Test consistent hashing
  let key1_node = DistributedCache::get_node_for_key(cache, "key1")
  let key2_node = DistributedCache::get_node_for_key(cache, "key2")
  let key3_node = DistributedCache::get_node_for_key(cache, "key3")
  
  // Keys should be distributed across nodes
  let nodes = [key1_node.id, key2_node.id, key3_node.id]
  assert_true(nodes.contains("node1"))
  assert_true(nodes.contains("node2"))
  assert_true(nodes.contains("node3"))
  
  // Test node failure handling
  DistributedCache::simulate_node_failure(cache, "node2")
  
  // Should still be able to get data (replicated to other nodes)
  assert_eq(DistributedCache::get(cache, "key1"), Some("value1"))
  
  // Test node recovery
  DistributedCache::recover_node(cache, "node2")
  
  // Test cache invalidation across nodes
  DistributedCache::put(cache, "key2", "value2")
  DistributedCache::invalidate(cache, "key2")
  assert_eq(DistributedCache::get(cache, "key2"), None)
  
  // Test cache update propagation
  DistributedCache::put(cache, "key3", "initial_value")
  DistributedCache::put(cache, "key3", "updated_value")
  assert_eq(DistributedCache::get(cache, "key3"), Some("updated_value"))
}

// Test 7: Cache Hierarchies
test "cache hierarchies" {
  // Test L1 (in-memory) and L2 (disk) cache hierarchy
  let l1_cache = LRUCache::new(10)  // Small in-memory cache
  let l2_cache = LRUCache::new(100) // Larger disk cache
  let hierarchical_cache = HierarchicalCache::new(l1_cache, l2_cache)
  
  // Test put and get (should go to L1)
  HierarchicalCache::put(hierarchical_cache, "key1", "value1")
  assert_eq(HierarchicalCache::get(hierarchical_cache, "key1"), Some("value1"))
  assert_eq(LRUCache::get(l1_cache, "key1"), Some("value1"))
  assert_eq(LRUCache::get(l2_cache, "key1"), None)
  
  // Fill L1 cache
  for i in 0..10 {
    HierarchicalCache::put(hierarchical_cache, "key" + i.to_string(), "value" + i.to_string())
  }
  
  // L1 should be full, key1 should be evicted to L2
  assert_eq(LRUCache::get(l1_cache, "key1"), None)
  assert_eq(LRUCache::get(l2_cache, "key1"), Some("value1"))
  
  // Get key1 (should promote from L2 to L1)
  assert_eq(HierarchicalCache::get(hierarchical_cache, "key1"), Some("value1"))
  assert_eq(LRUCache::get(l1_cache, "key1"), Some("value1"))
  assert_eq(LRUCache::get(l2_cache, "key1"), None)
  
  // Test cache statistics across hierarchy
  let stats = HierarchicalCache::get_stats(hierarchical_cache)
  assert_true(stats.l1_hit_count > 0)
  assert_true(stats.l2_hit_count > 0)
  assert_true(stats.promotion_count > 0)
  assert_true(stats.demotion_count > 0)
}

// Test 8: Cache Warmer and Preloader
test "cache warmer and preloader" {
  // Test cache warmer
  let cache = LRUCache::new(100)
  let warmer = CacheWarmer::new(cache)
  
  // Define data loader function
  let data_loader = || {
    [
      ("key1", "value1"),
      ("key2", "value2"),
      ("key3", "value3"),
      ("key4", "value4"),
      ("key5", "value5")
    ]
  }
  
  // Warm cache with specific keys
  CacheWarmer::warm_keys(warmer, ["key1", "key3", "key5"], data_loader)
  
  // Verify keys are in cache
  assert_eq(LRUCache::get(cache, "key1"), Some("value1"))
  assert_eq(LRUCache::get(cache, "key3"), Some("value3"))
  assert_eq(LRUCache::get(cache, "key5"), Some("value5"))
  assert_eq(LRUCache::get(cache, "key2"), None)
  assert_eq(LRUCache::get(cache, "key4"), None)
  
  // Clear cache
  LRUCache::clear(cache)
  
  // Warm cache with all data
  CacheWarmer::warm_all(warmer, data_loader)
  
  // Verify all keys are in cache
  assert_eq(LRUCache::get(cache, "key1"), Some("value1"))
  assert_eq(LRUCache::get(cache, "key2"), Some("value2"))
  assert_eq(LRUCache::get(cache, "key3"), Some("value3"))
  assert_eq(LRUCache::get(cache, "key4"), Some("value4"))
  assert_eq(LRUCache::get(cache, "key5"), Some("value5"))
  
  // Test cache preloader with priority
  let preloader = CachePreloader::new(cache)
  let priority_keys = [
    ("key1", 10),  // Highest priority
    ("key2", 8),
    ("key3", 5),
    ("key4", 3),
    ("key5", 1)   // Lowest priority
  ]
  
  // Clear cache
  LRUCache::clear(cache)
  
  // Preload with priority (cache capacity is 100, but we'll simulate with 3 for testing)
  let small_cache = LRUCache::new(3)
  let small_preloader = CachePreloader::new(small_cache)
  
  // Should load highest priority keys first
  CachePreloader::preload_with_priority(small_preloader, priority_keys, data_loader)
  
  // Verify highest priority keys are in cache
  assert_eq(LRUCache::get(small_cache, "key1"), Some("value1"))
  assert_eq(LRUCache::get(small_cache, "key2"), Some("value2"))
  assert_eq(LRUCache::get(small_cache, "key3"), Some("value3"))
  assert_eq(LRUCache::get(small_cache, "key4"), None)  // Should not fit in cache
  assert_eq(LRUCache::get(small_cache, "key5"), None)  // Should not fit in cache
}

// Test 9: Cache Invalidation Strategies
test "cache invalidation strategies" {
  // Test time-based invalidation
  let cache = TTLCache::new(100, 1000)  // 1 second TTL
  TTLCache::put(cache, "key1", "value1")
  assert_eq(TTLCache::get(cache, "key1"), Some("value1"))
  
  sleep(1100)  // Wait for expiration
  assert_eq(TTLCache::get(cache, "key1"), None)  // Should be invalidated
  
  // Test size-based invalidation
  let lru_cache = LRUCache::new(3)
  LRUCache::put(lru_cache, "key1", "value1")
  LRUCache::put(lru_cache, "key2", "value2")
  LRUCache::put(lru_cache, "key3", "value3")
  
  // Add one more to trigger eviction
  LRUCache::put(lru_cache, "key4", "value4")
  assert_eq(LRUCache::get(lru_cache, "key1"), None)  // Should be invalidated
  
  // Test manual invalidation
  let manual_cache = LRUCache::new(100)
  LRUCache::put(manual_cache, "key1", "value1")
  LRUCache::put(manual_cache, "key2", "value2")
  
  // Invalidate specific key
  LRUCache::remove(manual_cache, "key1")
  assert_eq(LRUCache::get(manual_cache, "key1"), None)
  assert_eq(LRUCache::get(manual_cache, "key2"), Some("value2"))
  
  // Invalidate by pattern
  LRUCache::put(manual_cache, "user:1", "user1")
  LRUCache::put(manual_cache, "user:2", "user2")
  LRUCache::put(manual_cache, "product:1", "product1")
  
  LRUCache::invalidate_by_pattern(manual_cache, "user:*")
  assert_eq(LRUCache::get(manual_cache, "user:1"), None)
  assert_eq(LRUCache::get(manual_cache, "user:2"), None)
  assert_eq(LRUCache::get(manual_cache, "product:1"), Some("product1"))
  
  // Test tag-based invalidation
  let tagged_cache = TaggedCache::new(100)
  TaggedCache::put_with_tags(tagged_cache, "key1", "value1", ["tag1", "tag2"])
  TaggedCache::put_with_tags(tagged_cache, "key2", "value2", ["tag2", "tag3"])
  TaggedCache::put_with_tags(tagged_cache, "key3", "value3", ["tag3", "tag4"])
  
  // Invalidate by tag
  TaggedCache::invalidate_by_tag(tagged_cache, "tag2")
  assert_eq(TaggedCache::get(tagged_cache, "key1"), None)  // Has tag2
  assert_eq(TaggedCache::get(tagged_cache, "key2"), None)  // Has tag2
  assert_eq(TaggedCache::get(tagged_cache, "key3"), Some("value3"))  // Doesn't have tag2
}

// Test 10: Cache Performance Optimization
test "cache performance optimization" {
  // Test bulk operations
  let cache = LRUCache::new(1000)
  
  // Test bulk put
  let items = []
  for i in 0..100 {
    items.push(("key" + i.to_string(), "value" + i.to_string()))
  }
  
  let bulk_put_time = benchmark(|| {
    LRUCache::put_all(cache, items)
  })
  
  // Test individual put
  let individual_cache = LRUCache::new(1000)
  let individual_put_time = benchmark(|| {
    for i in 0..100 {
      LRUCache::put(individual_cache, "key" + i.to_string(), "value" + i.to_string())
    }
  })
  
  // Bulk put should be faster
  assert_true(bulk_put_time < individual_put_time)
  
  // Test bulk get
  let keys = []
  for i in 0..100 {
    keys.push("key" + i.to_string())
  }
  
  let bulk_get_time = benchmark(|| {
    LRUCache::get_all(cache, keys)
  })
  
  // Test individual get
  let individual_get_time = benchmark(|| {
    for i in 0..100 {
      LRUCache::get(individual_cache, "key" + i.to_string())
    }
  })
  
  // Bulk get should be faster
  assert_true(bulk_get_time < individual_get_time)
  
  // Test cache compression
  let large_cache = CompressedCache::new(100)
  let large_value = "x" * 1000  // 1KB string
  
  CompressedCache::put(large_cache, "large_key", large_value)
  assert_eq(CompressedCache::get(large_cache, "large_key"), Some(large_value))
  
  // Verify compression ratio
  let stats = CompressedCache::get_stats(large_cache)
  assert_true(stats.compression_ratio < 1.0)  // Should be compressed
  
  // Test cache sharding
  let sharded_cache = ShardedCache::new(4, 250)  // 4 shards, 250 items each
  
  // Put items across shards
  for i in 0..1000 {
    ShardedCache::put(sharded_cache, "key" + i.to_string(), "value" + i.to_string())
  }
  
  // Get items from shards
  for i in 0..1000 {
    assert_eq(ShardedCache::get(sharded_cache, "key" + i.to_string()), Some("value" + i.to_string()))
  }
  
  // Test cache with async loading
  let async_cache = AsyncLoadingCache::new(100)
  
  // Define async loader
  let async_loader = || {
    sleep(100)  // Simulate slow operation
    "loaded_value"
  }
  
  // First get should trigger async load
  let future = AsyncLoadingCache::get_or_load(async_cache, "async_key", async_loader)
  assert_eq(Future::get(future), "loaded_value")
  
  // Second get should return cached value
  let future2 = AsyncLoadingCache::get_or_load(async_cache, "async_key", async_loader)
  assert_eq(Future::get(future2), "loaded_value")
  
  // Test cache with write-through policy
  let persistent_store = KeyValueStore::new()
  let write_through_cache = WriteThroughCache::new(100, persistent_store)
  
  WriteThroughCache::put(write_through_cache, "wt_key", "wt_value")
  assert_eq(WriteThroughCache::get(write_through_cache, "wt_key"), Some("wt_value"))
  assert_eq(KeyValueStore::get(persistent_store, "wt_key"), Some("wt_value"))
  
  // Test cache with write-behind policy
  let write_behind_cache = WriteBehindCache::new(100, persistent_store)
  
  WriteBehindCache::put(write_behind_cache, "wb_key", "wb_value")
  assert_eq(WriteBehindCache::get(write_behind_cache, "wb_key"), Some("wb_value"))
  
  // Value should be in cache but not yet in persistent store
  sleep(200)  // Wait for write-behind to complete
  assert_eq(KeyValueStore::get(persistent_store, "wb_key"), Some("wb_value"))
}