// Azimuth Telemetry System - Caching Mechanism Quality Tests
// This file contains comprehensive test cases for caching mechanism functionality

// Test 1: Basic Cache Operations
test "basic cache operations" {
  let cache = TelemetryCache::new(100) // Cache with capacity for 100 items
  
  // Test cache put and get
  let key = "cpu_usage"
  let value = TelemetryData::new("cpu.usage", 75.5, 1634567890L)
  
  let put_result = TelemetryCache::put(cache, key, value)
  assert_true(put_result)
  
  let get_result = TelemetryCache::get(cache, key)
  match get_result {
    Some(retrieved_value) => {
      assert_eq(retrieved_value.metric_name, value.metric_name)
      assert_eq(retrieved_value.value, value.value)
      assert_eq(retrieved_value.timestamp, value.timestamp)
    }
    None => assert_true(false) // Should retrieve value
  }
  
  // Test cache contains
  let contains_result = TelemetryCache::contains(cache, key)
  assert_true(contains_result)
  
  let not_contains_result = TelemetryCache::contains(cache, "non_existent_key")
  assert_false(not_contains_result)
  
  // Test cache remove
  let remove_result = TelemetryCache::remove(cache, key)
  assert_true(remove_result)
  
  let get_after_remove = TelemetryCache::get(cache, key)
  match get_after_remove {
    Some(_) => assert_true(false), // Should not retrieve
    None => assert_true(true)
  }
  
  let contains_after_remove = TelemetryCache::contains(cache, key)
  assert_false(contains_after_remove)
  
  // Test cache size
  let initial_size = TelemetryCache::size(cache)
  assert_eq(initial_size, 0)
  
  // Add items
  for i in 0..=10 {
    let item_key = "item_" + i.to_string()
    let item_value = TelemetryData::new("metric_" + i.to_string(), i.to_float(), 1634567890L + i.to_long())
    TelemetryCache::put(cache, item_key, item_value)
  }
  
  let size_after_add = TelemetryCache::size(cache)
  assert_eq(size_after_add, 10)
  
  // Test cache clear
  TelemetryCache::clear(cache)
  
  let size_after_clear = TelemetryCache::size(cache)
  assert_eq(size_after_clear, 0)
  
  let get_after_clear = TelemetryCache::get(cache, "item_5")
  match get_after_clear {
    Some(_) => assert_true(false), // Should not retrieve
    None => assert_true(true)
  }
}

// Test 2: Cache Eviction Policies
test "cache eviction policies" {
  // Test LRU (Least Recently Used) eviction
  let lru_cache = TelemetryCache::new_with_policy(5, LRU)
  
  // Fill cache
  for i in 0..=5 {
    let key = "lru_item_" + i.to_string()
    let value = TelemetryData::new("metric_" + i.to_string(), i.to_float(), 1634567890L)
    TelemetryCache::put(lru_cache, key, value)
  }
  
  // Cache should have 5 items (item_0 was evicted)
  assert_eq(TelemetryCache::size(lru_cache), 5)
  assert_false(TelemetryCache::contains(lru_cache, "lru_item_0"))
  assert_true(TelemetryCache::contains(lru_cache, "lru_item_1"))
  assert_true(TelemetryCache::contains(lru_cache, "lru_item_5"))
  
  // Access item_1 to make it recently used
  TelemetryCache::get(lru_cache, "lru_item_1")
  
  // Add another item
  let new_key = "lru_item_6"
  let new_value = TelemetryData::new("metric_6", 6.0, 1634567896L)
  TelemetryCache::put(lru_cache, new_key, new_value)
  
  // item_2 should be evicted (least recently used after item_1 access)
  assert_false(TelemetryCache::contains(lru_cache, "lru_item_2"))
  assert_true(TelemetryCache::contains(lru_cache, "lru_item_1"))
  assert_true(TelemetryCache::contains(lru_cache, "lru_item_6"))
  
  // Test LFU (Least Frequently Used) eviction
  let lfu_cache = TelemetryCache::new_with_policy(5, LFU)
  
  // Fill cache
  for i in 0..=5 {
    let key = "lfu_item_" + i.to_string()
    let value = TelemetryData::new("metric_" + i.to_string(), i.to_float(), 1634567890L)
    TelemetryCache::put(lfu_cache, key, value)
  }
  
  // Access items with different frequencies
  for i in 0..=2 {
    TelemetryCache::get(lfu_cache, "lfu_item_1") // Access 3 times
  }
  
  for i in 0..=1 {
    TelemetryCache::get(lfu_cache, "lfu_item_2") // Access 2 times
  }
  
  TelemetryCache::get(lfu_cache, "lfu_item_3") // Access 1 time
  
  // Add another item
  let lfu_new_key = "lfu_item_6"
  let lfu_new_value = TelemetryData::new("metric_6", 6.0, 1634567896L)
  TelemetryCache::put(lfu_cache, lfu_new_key, lfu_new_value)
  
  // item_0 should be evicted (least frequently used)
  assert_false(TelemetryCache::contains(lfu_cache, "lfu_item_0"))
  assert_true(TelemetryCache::contains(lfu_cache, "lfu_item_1"))
  assert_true(TelemetryCache::contains(lfu_cache, "lfu_item_6"))
  
  // Test FIFO (First In First Out) eviction
  let fifo_cache = TelemetryCache::new_with_policy(5, FIFO)
  
  // Fill cache
  for i in 0..=5 {
    let key = "fifo_item_" + i.to_string()
    let value = TelemetryData::new("metric_" + i.to_string(), i.to_float(), 1634567890L)
    TelemetryCache::put(fifo_cache, key, value)
  }
  
  // item_0 should be evicted (first in)
  assert_false(TelemetryCache::contains(fifo_cache, "fifo_item_0"))
  assert_true(TelemetryCache::contains(fifo_cache, "fifo_item_1"))
  assert_true(TelemetryCache::contains(fifo_cache, "fifo_item_5"))
  
  // Test TTL (Time To Live) eviction
  let ttl_cache = TelemetryCache::new_with_ttl(5, 1000) // 1 second TTL
  
  // Fill cache
  for i in 0..=4 {
    let key = "ttl_item_" + i.to_string()
    let value = TelemetryData::new("metric_" + i.to_string(), i.to_float(), 1634567890L)
    TelemetryCache::put(ttl_cache, key, value)
  }
  
  // All items should be present initially
  for i in 0..=4 {
    assert_true(TelemetryCache::contains(ttl_cache, "ttl_item_" + i.to_string()))
  }
  
  // Wait for TTL to expire
  Time::sleep(1100)
  
  // Access item_0 to trigger TTL check
  TelemetryCache::get(ttl_cache, "ttl_item_0")
  
  // All items should be expired
  for i in 0..=4 {
    assert_false(TelemetryCache::contains(ttl_cache, "ttl_item_" + i.to_string()))
  }
}

// Test 3: Cache Performance
test "cache performance" {
  let cache = TelemetryCache::new(1000)
  
  // Prepare test data
  let test_data = []
  for i in 0..=1000 {
    let key = "perf_item_" + i.to_string()
    let value = TelemetryData::new("metric_" + i.to_string(), i.to_float(), 1634567890L + i.to_long())
    test_data.push((key, value))
  }
  
  // Test cache write performance
  let write_start_time = Time::now()
  
  for (key, value) in test_data {
    TelemetryCache::put(cache, key, value)
  }
  
  let write_end_time = Time::now()
  let write_duration = Time::duration(write_start_time, write_end_time)
  
  // Test cache read performance
  let read_start_time = Time::now()
  
  for (key, _) in test_data {
    TelemetryCache::get(cache, key)
  }
  
  let read_end_time = Time::now()
  let read_duration = Time::duration(read_start_time, read_end_time)
  
  // Cache operations should be fast
  assert_true(write_duration < 1000) // Less than 1 second
  assert_true(read_duration < 500)   // Less than 0.5 seconds
  
  // Test cache hit ratio
  let mut hits = 0
  let mut misses = 0
  
  // Access items with 80% hit ratio pattern
  for i in 0..=1000 {
    let key_index = if i % 10 < 8 { i % 100 } else { 100 + (i % 900) }
    let key = "perf_item_" + key_index.to_string()
    
    let result = TelemetryCache::get(cache, key)
    match result {
      Some(_) => hits = hits + 1,
      None => misses = misses + 1
    }
  }
  
  let hit_ratio = hits.to_float() / (hits + misses).to_float()
  assert_true(hit_ratio > 0.75) // Should be around 80%
  
  // Test cache statistics
  let stats = TelemetryCache::get_stats(cache)
  assert_eq(stats.hits, hits)
  assert_eq(stats.misses, misses)
  assert_eq(stats.size, 1000)
  assert_true(stats.hit_ratio > 0.75)
  
  // Test cache memory usage
  let memory_usage = TelemetryCache::get_memory_usage(cache)
  assert_true(memory_usage > 0)
  
  let memory_per_item = memory_usage.to_float() / 1000.0
  assert_true(memory_per_item < 1024) // Less than 1KB per item
}

// Test 4: Distributed Cache
test "distributed cache" {
  let distributed_cache = DistributedTelemetryCache::new()
  
  // Add cache nodes
  let node1 = CacheNode::new("node1", "127.0.0.1", 8001)
  let node2 = CacheNode::new("node2", "127.0.0.1", 8002)
  let node3 = CacheNode::new("node3", "127.0.0.1", 8003)
  
  DistributedTelemetryCache::add_node(distributed_cache, node1)
  DistributedTelemetryCache::add_node(distributed_cache, node2)
  DistributedTelemetryCache::add_node(distributed_cache, node3)
  
  // Test consistent hashing
  let key1 = "distributed_item_1"
  let key2 = "distributed_item_2"
  let key3 = "distributed_item_3"
  
  let value1 = TelemetryData::new("metric_1", 1.0, 1634567890L)
  let value2 = TelemetryData::new("metric_2", 2.0, 1634567891L)
  let value3 = TelemetryData::new("metric_3", 3.0, 1634567892L)
  
  // Put items
  DistributedTelemetryCache::put(distributed_cache, key1, value1)
  DistributedTelemetryCache::put(distributed_cache, key2, value2)
  DistributedTelemetryCache::put(distributed_cache, key3, value3)
  
  // Get items
  let get1 = DistributedTelemetryCache::get(distributed_cache, key1)
  let get2 = DistributedTelemetryCache::get(distributed_cache, key2)
  let get3 = DistributedTelemetryCache::get(distributed_cache, key3)
  
  match (get1, get2, get3) {
    (Some(v1), Some(v2), Some(v3)) => {
      assert_eq(v1.metric_name, value1.metric_name)
      assert_eq(v2.metric_name, value2.metric_name)
      assert_eq(v3.metric_name, value3.metric_name)
    }
    _ => assert_true(false) // Should retrieve all values
  }
  
  // Test cache node failure
  DistributedTelemetryCache::simulate_node_failure(distributed_cache, "node2")
  
  // Should still be able to get items (replicated on other nodes)
  let get_after_failure = DistributedTelemetryCache::get(distributed_cache, key1)
  match get_after_failure {
    Some(_) => assert_true(true),
    None => assert_true(false) // Should still retrieve
  }
  
  // Test cache node recovery
  DistributedTelemetryCache::recover_node(distributed_cache, "node2")
  
  // Test cache rebalancing
  DistributedTelemetryCache::rebalance(distributed_cache)
  
  // Test distributed cache statistics
  let distributed_stats = DistributedTelemetryCache::get_stats(distributed_cache)
  assert_eq(distributed_stats.total_nodes, 3)
  assert_eq(distributed_stats.active_nodes, 3)
  assert_true(distributed_stats.total_items > 0)
}

// Test 5: Cache Persistence
test "cache persistence" {
  let persistent_cache = PersistentTelemetryCache::new("test_cache.db")
  
  // Add items to cache
  for i in 0..=100 {
    let key = "persistent_item_" + i.to_string()
    let value = TelemetryData::new("metric_" + i.to_string(), i.to_float(), 1634567890L + i.to_long())
    PersistentTelemetryCache::put(persistent_cache, key, value)
  }
  
  // Verify items are in cache
  for i in 0..=100 {
    let key = "persistent_item_" + i.to_string()
    let result = PersistentTelemetryCache::get(persistent_cache, key)
    match result {
      Some(_) => assert_true(true),
      None => assert_true(false) // Should retrieve
    }
  }
  
  // Persist cache to disk
  let persist_result = PersistentTelemetryCache::persist(persistent_cache)
  assert_true(persist_result)
  
  // Create new cache instance and load from disk
  let loaded_cache = PersistentTelemetryCache::load("test_cache.db")
  
  // Verify items are loaded
  for i in 0..=100 {
    let key = "persistent_item_" + i.to_string()
    let result = PersistentTelemetryCache::get(loaded_cache, key)
    match result {
      Some(value) => {
        assert_eq(value.metric_name, "metric_" + i.to_string())
        assert_eq(value.value, i.to_float())
      }
      None => assert_true(false) // Should retrieve
    }
  }
  
  // Test cache expiration
  let expiring_cache = PersistentTelemetryCache::new_with_ttl("expiring_cache.db", 1000) // 1 second TTL
  
  let expiring_key = "expiring_item"
  let expiring_value = TelemetryData::new("expiring_metric", 99.9, 1634567899L)
  
  PersistentTelemetryCache::put(expiring_cache, expiring_key, expiring_value)
  
  // Should be present initially
  let get_before_expiry = PersistentTelemetryCache::get(expiring_cache, expiring_key)
  match get_before_expiry {
    Some(_) => assert_true(true),
    None => assert_true(false) // Should retrieve
  }
  
  // Wait for expiration
  Time::sleep(1100)
  
  // Should be expired
  let get_after_expiry = PersistentTelemetryCache::get(expiring_cache, expiring_key)
  match get_after_expiry {
    Some(_) => assert_true(false), // Should not retrieve
    None => assert_true(true)
  }
  
  // Test cache compression
  let compressed_cache = PersistentTelemetryCache::new_with_compression("compressed_cache.db", Gzip)
  
  // Add large items
  for i in 0..=50 {
    let key = "compressed_item_" + i.to_string()
    let large_value = TelemetryData::with_large_attributes("large_metric_" + i.to_string())
    PersistentTelemetryCache::put(compressed_cache, key, large_value)
  }
  
  // Persist compressed cache
  let compress_persist_result = PersistentTelemetryCache::persist(compressed_cache)
  assert_true(compress_persist_result)
  
  // Load compressed cache
  let loaded_compressed_cache = PersistentTelemetryCache::load("compressed_cache.db")
  
  // Verify items are loaded correctly
  for i in 0..=50 {
    let key = "compressed_item_" + i.to_string()
    let result = PersistentTelemetryCache::get(loaded_compressed_cache, key)
    match result {
      Some(_) => assert_true(true),
      None => assert_true(false) // Should retrieve
    }
  }
  
  // Verify compression benefits
  let compressed_size = PersistentTelemetryCache::get_disk_size(loaded_compressed_cache)
  let uncompressed_size = PersistentTelemetryCache::get_memory_size(loaded_compressed_cache)
  
  let compression_ratio = compressed_size.to_float() / uncompressed_size.to_float()
  assert_true(compression_ratio < 0.8) // At least 20% compression
}

// Test 6: Cache Synchronization
test "cache synchronization" {
  let cache1 = TelemetryCache::new(100)
  let cache2 = TelemetryCache::new(100)
  let cache3 = TelemetryCache::new(100)
  
  let cache_cluster = CacheCluster::new()
  CacheCluster::add_cache(cache_cluster, cache1)
  CacheCluster::add_cache(cache_cluster, cache2)
  CacheCluster::add_cache(cache_cluster, cache3)
  
  // Enable synchronization
  CacheCluster::enable_synchronization(cache_cluster, 1000) // Sync every 1 second
  
  // Add item to cache1
  let key = "sync_item"
  let value = TelemetryData::new("sync_metric", 42.0, 1634567890L)
  TelemetryCache::put(cache1, key, value)
  
  // Should be in cache1 but not in others yet
  assert_true(TelemetryCache::contains(cache1, key))
  assert_false(TelemetryCache::contains(cache2, key))
  assert_false(TelemetryCache::contains(cache3, key))
  
  // Wait for synchronization
  Time::sleep(1100)
  
  // Should now be in all caches
  assert_true(TelemetryCache::contains(cache1, key))
  assert_true(TelemetryCache::contains(cache2, key))
  assert_true(TelemetryCache::contains(cache3, key))
  
  // Update item in cache2
  let updated_value = TelemetryData::new("sync_metric", 43.0, 1634567891L)
  TelemetryCache::put(cache2, key, updated_value)
  
  // Wait for synchronization
  Time::sleep(1100)
  
  // All caches should have updated value
  let get1 = TelemetryCache::get(cache1, key)
  let get2 = TelemetryCache::get(cache2, key)
  let get3 = TelemetryCache::get(cache3, key)
  
  match (get1, get2, get3) {
    (Some(v1), Some(v2), Some(v3)) => {
      assert_eq(v1.value, 43.0)
      assert_eq(v2.value, 43.0)
      assert_eq(v3.value, 43.0)
    }
    _ => assert_true(false) // Should retrieve all
  }
  
  // Test conflict resolution
  CacheCluster::set_conflict_resolution(cache_cluster, LastWriteWins)
  
  // Simultaneously update the same item in different caches
  TelemetryCache::put(cache1, key, TelemetryData::new("sync_metric", 44.0, 1634567892L))
  TelemetryCache::put(cache2, key, TelemetryData::new("sync_metric", 45.0, 1634567893L))
  
  // Wait for synchronization
  Time::sleep(1100)
  
  // All caches should have the same value (last write wins)
  let final_get1 = TelemetryCache::get(cache1, key)
  let final_get2 = TelemetryCache::get(cache2, key)
  let final_get3 = TelemetryCache::get(cache3, key)
  
  match (final_get1, final_get2, final_get3) {
    (Some(v1), Some(v2), Some(v3)) => {
      assert_eq(v1.value, v2.value)
      assert_eq(v2.value, v3.value)
      assert_eq(v3.value, 45.0) // Last write wins
    }
    _ => assert_true(false) // Should retrieve all
  }
  
  // Test selective synchronization
  CacheCluster::add_sync_filter(cache_cluster, func(key : String) -> Bool {
    String::starts_with(key, "sync_")
  })
  
  let filtered_key = "filtered_item"
  let filtered_value = TelemetryData::new("filtered_metric", 99.0, 1634567899L)
  
  TelemetryCache::put(cache1, "sync_" + filtered_key, filtered_value)
  TelemetryCache::put(cache1, filtered_key, filtered_value)
  
  // Wait for synchronization
  Time::sleep(1100)
  
  // sync_filtered_item should be synchronized
  assert_true(TelemetryCache::contains(cache2, "sync_" + filtered_key))
  assert_true(TelemetryCache::contains(cache3, "sync_" + filtered_key))
  
  // filtered_item should not be synchronized
  assert_false(TelemetryCache::contains(cache2, filtered_key))
  assert_false(TelemetryCache::contains(cache3, filtered_key))
}

// Test 7: Cache Invalidation
test "cache invalidation" {
  let cache = TelemetryCache::new(100)
  
  // Add items
  for i in 0..=10 {
    let key = "invalidation_item_" + i.to_string()
    let value = TelemetryData::new("metric_" + i.to_string(), i.to_float(), 1634567890L + i.to_long())
    TelemetryCache::put(cache, key, value)
  }
  
  // Test tag-based invalidation
  TelemetryCache::add_tag(cache, "invalidation_item_1", "cpu_metrics")
  TelemetryCache::add_tag(cache, "invalidation_item_2", "cpu_metrics")
  TelemetryCache::add_tag(cache, "invalidation_item_3", "memory_metrics")
  TelemetryCache::add_tag(cache, "invalidation_item_4", "memory_metrics")
  
  // Invalidate by tag
  TelemetryCache::invalidate_by_tag(cache, "cpu_metrics")
  
  // CPU metrics should be invalidated
  assert_false(TelemetryCache::contains(cache, "invalidation_item_1"))
  assert_false(TelemetryCache::contains(cache, "invalidation_item_2"))
  
  // Memory metrics should still be valid
  assert_true(TelemetryCache::contains(cache, "invalidation_item_3"))
  assert_true(TelemetryCache::contains(cache, "invalidation_item_4"))
  
  // Test pattern-based invalidation
  TelemetryCache::invalidate_by_pattern(cache, "invalidation_item_[5-7]")
  
  // Items matching pattern should be invalidated
  assert_false(TelemetryCache::contains(cache, "invalidation_item_5"))
  assert_false(TelemetryCache::contains(cache, "invalidation_item_6"))
  assert_false(TelemetryCache::contains(cache, "invalidation_item_7"))
  
  // Items not matching pattern should still be valid
  assert_true(TelemetryCache::contains(cache, "invalidation_item_8"))
  assert_true(TelemetryCache::contains(cache, "invalidation_item_9"))
  assert_true(TelemetryCache::contains(cache, "invalidation_item_10"))
  
  // Test time-based invalidation
  TelemetryCache::put(cache, "time_sensitive", TelemetryData::new("time_metric", 123.45, 1634567890L))
  TelemetryCache::set_time_to_live(cache, "time_sensitive", 1000) // 1 second TTL
  
  // Should be valid initially
  assert_true(TelemetryCache::contains(cache, "time_sensitive"))
  
  // Wait for expiration
  Time::sleep(1100)
  
  // Try to access to trigger expiration check
  TelemetryCache::get(cache, "time_sensitive")
  
  // Should be expired
  assert_false(TelemetryCache::contains(cache, "time_sensitive"))
  
  // Test dependency-based invalidation
  TelemetryCache::put(cache, "dependent_item_1", TelemetryData::new("dependent_metric_1", 1.0, 1634567890L))
  TelemetryCache::put(cache, "dependent_item_2", TelemetryData::new("dependent_metric_2", 2.0, 1634567891L))
  TelemetryCache::put(cache, "dependency_item", TelemetryData::new("dependency_metric", 3.0, 1634567892L))
  
  TelemetryCache::add_dependency(cache, "dependent_item_1", "dependency_item")
  TelemetryCache::add_dependency(cache, "dependent_item_2", "dependency_item")
  
  // Invalidate dependency
  TelemetryCache::remove(cache, "dependency_item")
  
  // Dependent items should also be invalidated
  assert_false(TelemetryCache::contains(cache, "dependent_item_1"))
  assert_false(TelemetryCache::contains(cache, "dependent_item_2"))
}

// Test 8: Cache Hierarchies
test "cache hierarchies" {
  let l1_cache = TelemetryCache::new(10)  // Level 1 (smallest, fastest)
  let l2_cache = TelemetryCache::new(100) // Level 2 (medium)
  let l3_cache = TelemetryCache::new(1000) // Level 3 (largest, slowest)
  
  let cache_hierarchy = CacheHierarchy::new()
  CacheHierarchy::add_level(cache_hierarchy, l1_cache, 1)
  CacheHierarchy::add_level(cache_hierarchy, l2_cache, 2)
  CacheHierarchy::add_level(cache_hierarchy, l3_cache, 3)
  
  // Test cache hierarchy get
  let key = "hierarchy_item"
  let value = TelemetryData::new("hierarchy_metric", 42.0, 1634567890L)
  
  // Put in L3 (bottom level)
  TelemetryCache::put(l3_cache, key, value)
  
  // Get should retrieve from L3 and promote to higher levels
  let get_result = CacheHierarchy::get(cache_hierarchy, key)
  match get_result {
    Some(retrieved_value) => {
      assert_eq(retrieved_value.metric_name, value.metric_name)
      assert_eq(retrieved_value.value, value.value)
    }
    None => assert_true(false) // Should retrieve
  }
  
  // Should now be in L1 (highest level)
  assert_true(TelemetryCache::contains(l1_cache, key))
  
  // Test cache hierarchy put
  let new_key = "new_hierarchy_item"
  let new_value = TelemetryData::new("new_hierarchy_metric", 43.0, 1634567891L)
  
  CacheHierarchy::put(cache_hierarchy, new_key, new_value)
  
  // Should be in all levels
  assert_true(TelemetryCache::contains(l1_cache, new_key))
  assert_true(TelemetryCache::contains(l2_cache, new_key))
  assert_true(TelemetryCache::contains(l3_cache, new_key))
  
  // Test cache eviction in hierarchy
  // Fill L1 to capacity
  for i in 0..=9 {
    let fill_key = "fill_l1_" + i.to_string()
    let fill_value = TelemetryData::new("fill_metric_" + i.to_string(), i.to_float(), 1634567890L + i.to_long())
    CacheHierarchy::put(cache_hierarchy, fill_key, fill_value)
  }
  
  // L1 should be full
  assert_eq(TelemetryCache::size(l1_cache), 10)
  
  // Add one more item
  let overflow_key = "overflow_item"
  let overflow_value = TelemetryData::new("overflow_metric", 99.0, 1634567999L)
  CacheHierarchy::put(cache_hierarchy, overflow_key, overflow_value)
  
  // L1 should still have 10 items (one evicted)
  assert_eq(TelemetryCache::size(l1_cache), 10)
  
  // L2 should have the evicted item
  assert_true(TelemetryCache::contains(l2_cache, new_key))
  
  // Test cache hierarchy statistics
  let hierarchy_stats = CacheHierarchy::get_stats(cache_hierarchy)
  assert_eq(hierarchy_stats.total_levels, 3)
  assert_true(hierarchy_stats.total_hits > 0)
  assert_true(hierarchy_stats.l1_hits + hierarchy_stats.l2_hits + hierarchy_stats.l3_hits == hierarchy_stats.total_hits)
  
  // Test cache hierarchy promotion
  let promotion_key = "promotion_item"
  let promotion_value = TelemetryData::new("promotion_metric", 44.0, 1634567892L)
  
  // Put in L3
  TelemetryCache::put(l3_cache, promotion_key, promotion_value)
  
  // Access multiple times to promote
  for i in 0..=5 {
    CacheHierarchy::get(cache_hierarchy, promotion_key)
  }
  
  // Should be promoted to higher levels
  assert_true(TelemetryCache::contains(l1_cache, promotion_key))
}

// Test 9: Cache Warming and Preloading
test "cache warming and preloading" {
  let cache = TelemetryCache::new(100)
  
  // Create preloader
  let preloader = CachePreloader::new()
  
  // Define preloading strategies
  let most_used_strategy = MostUsedStrategy::new()
  let recent_strategy = RecentStrategy::new()
  let related_strategy = RelatedStrategy::new()
  
  // Add preloading rules
  CachePreloader::add_rule(preloader, most_used_strategy, 50)
  CachePreloader::add_rule(preloader, recent_strategy, 30)
  CachePreloader::add_rule(preloader, related_strategy, 20)
  
  // Simulate usage patterns
  let usage_data = []
  for i in 0..=1000 {
    let key = "usage_item_" + (i % 100).to_string() // 100 unique items, some used more frequently
    usage_data.push(key)
  }
  
  // Record usage patterns
  for key in usage_data {
    CachePreloader::record_access(preloader, key)
  }
  
  // Preload cache based on usage patterns
  let preload_result = CachePreloader::preload(cache, preloader)
  assert_true(preload_result)
  
  // Verify cache is warmed
  let cache_size = TelemetryCache::size(cache)
  assert_true(cache_size > 0)
  assert_true(cache_size <= 100)
  
  // Test cache warming with known data
  let warmer = CacheWarmer::new()
  
  // Add warming data
  for i in 0..=50 {
    let key = "warm_item_" + i.to_string()
    let value = TelemetryData::new("warm_metric_" + i.to_string(), i.to_float(), 1634567890L + i.to_long())
    CacheWarmer::add_warming_data(warmer, key, value)
  }
  
  // Warm cache
  let warm_result = CacheWarmer::warm(cache, warmer)
  assert_true(warm_result)
  
  // Verify cache is warmed
  for i in 0..=50 {
    let key = "warm_item_" + i.to_string()
    assert_true(TelemetryCache::contains(cache, key))
  }
  
  // Test predictive preloading
  let predictor = CachePredictor::new()
  
  // Train predictor with usage patterns
  for i in 0..=1000 {
    let key = "predict_item_" + (i % 50).to_string()
    CachePredictor::record_access(predictor, key)
  }
  
  // Predict next accesses
  let predictions = CachePredictor::predict_next(predictor, 10)
  assert_true(predictions.length() > 0)
  
  // Preload predicted items
  for prediction in predictions {
    let key = prediction.key
    let value = TelemetryData::new("predict_metric_" + key, 99.0, 1634567899L)
    TelemetryCache::put(cache, key, value)
  }
  
  // Verify predicted items are in cache
  for prediction in predictions {
    assert_true(TelemetryCache::contains(cache, prediction.key))
  }
  
  // Test cache warming performance
  let cold_cache = TelemetryCache::new(100)
  let warmed_cache = TelemetryCache::new(100)
  
  // Warm one cache
  CacheWarmer::warm(warmed_cache, warmer)
  
  // Measure cold cache performance
  let cold_start = Time::now()
  for i in 0..=50 {
    let key = "warm_item_" + i.to_string()
    TelemetryCache::get(cold_cache, key)
  }
  let cold_end = Time::now()
  let cold_duration = Time::duration(cold_start, cold_end)
  
  // Measure warmed cache performance
  let warm_start = Time::now()
  for i in 0..=50 {
    let key = "warm_item_" + i.to_string()
    TelemetryCache::get(warmed_cache, key)
  }
  let warm_end = Time::now()
  let warm_duration = Time::duration(warm_start, warm_end)
  
  // Warmed cache should be faster
  assert_true(warm_duration < cold_duration)
}

// Test 10: Cache Monitoring and Metrics
test "cache monitoring and metrics" {
  let cache = TelemetryCache::new(100)
  
  // Enable monitoring
  let monitor = CacheMonitor::new()
  CacheMonitor::enable_monitoring(monitor, cache)
  
  // Perform cache operations
  for i in 0..=100 {
    let key = "monitor_item_" + i.to_string()
    let value = TelemetryData::new("monitor_metric_" + i.to_string(), i.to_float(), 1634567890L + i.to_long())
    TelemetryCache::put(cache, key, value)
  }
  
  // Access some items multiple times
  for i in 0..=50 {
    let key = "monitor_item_" + (i % 20).to_string()
    TelemetryCache::get(cache, key)
  }
  
  // Get cache metrics
  let metrics = CacheMonitor::get_metrics(monitor)
  
  assert_eq(metrics.total_puts, 101)
  assert_eq(metrics.total_gets, 51)
  assert_eq(metrics.total_hits, 51)
  assert_eq(metrics.total_misses, 0)
  assert_eq(metrics.hit_ratio, 1.0)
  assert_eq(metrics.current_size, 101)
  assert_true(metrics.memory_usage > 0)
  assert_true(metrics.average_put_time > 0)
  assert_true(metrics.average_get_time > 0)
  
  // Test real-time monitoring
  CacheMonitor::start_real_time_monitoring(monitor, 500) // Update every 500ms
  
  // Perform more operations
  for i in 0..=50 {
    let key = "realtime_item_" + i.to_string()
    let value = TelemetryData::new("realtime_metric_" + i.to_string(), i.to_float(), 1634567890L + i.to_long())
    TelemetryCache::put(cache, key, value)
    
    TelemetryCache::get(cache, key)
  }
  
  // Wait for monitoring updates
  Time::sleep(600)
  
  // Get real-time metrics
  let realtime_metrics = CacheMonitor::get_realtime_metrics(monitor)
  
  assert_true(realtime_metrics.total_puts >= 151)
  assert_true(realtime_metrics.total_gets >= 101)
  
  // Test cache alerts
  CacheMonitor::set_alert_threshold(monitor, "hit_ratio", 0.8) // Alert if hit ratio below 80%
  CacheMonitor::set_alert_threshold(monitor, "memory_usage", 10 * 1024 * 1024) // Alert if memory usage above 10MB
  
  // Simulate low hit ratio
  for i in 0..=100 {
    let key = "miss_item_" + i.to_string()
    TelemetryCache::get(cache, key) // These will all miss
  }
  
  // Check for alerts
  let alerts = CacheMonitor::get_alerts(monitor)
  assert_true(alerts.length() > 0)
  
  // Test cache performance analysis
  let analysis = CacheMonitor::analyze_performance(monitor)
  
  assert_true(analysis.put_performance_score > 0.0)
  assert_true(analysis.get_performance_score > 0.0)
  assert_true(analysis.overall_performance_score > 0.0)
  assert_true(analysis.recommendations.length() > 0)
  
  // Test cache optimization recommendations
  let recommendations = CacheMonitor::get_optimization_recommendations(monitor)
  
  assert_true(recommendations.length() > 0)
  
  // Apply some recommendations
  for recommendation in recommendations {
    if recommendation.type == "increase_size" {
      // Ignore for test
    } else if recommendation.type == "adjust_ttl" {
      // Ignore for test
    } else if recommendation.type == "change_eviction_policy" {
      // Apply recommendation
      TelemetryCache::set_eviction_policy(cache, LRU)
    }
  }
  
  // Verify eviction policy was changed
  let stats = TelemetryCache::get_stats(cache)
  assert_eq(stats.eviction_policy, LRU)
  
  // Test cache monitoring dashboard data
  let dashboard_data = CacheMonitor::get_dashboard_data(monitor)
  
  assert_true(dashboard_data.current_metrics.total_puts > 0)
  assert_true(dashboard_data.current_metrics.total_gets > 0)
  assert_true(dashboard_data.performance_chart.data.length() > 0)
  assert_true(dashboard_data.alerts.length() > 0)
}