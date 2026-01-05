// Azimuth Telemetry System - Caching Mechanism Tests
// This file contains test cases for caching mechanism functionality

// Test 1: Basic Cache Operations
test "basic cache operations" {
  // Create a cache
  let cache = Cache::new("test_cache", 100) // Max 100 items
  
  // Test put and get operations
  let key = "test_key"
  let value = "test_value"
  
  let put_result = Cache::put(cache, key, value)
  assert_true(put_result.is_ok())
  
  let get_result = Cache::get(cache, key)
  match get_result {
    Some(retrieved_value) => assert_eq(retrieved_value, value)
    None => assert_true(false)
  }
  
  // Test non-existent key
  let non_existent = Cache::get(cache, "non_existent_key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test has operation
  assert_true(Cache::has(cache, key))
  assert_false(Cache::has(cache, "non_existent_key"))
  
  // Test remove operation
  let remove_result = Cache::remove(cache, key)
  assert_true(remove_result.is_ok())
  
  // Verify the key is removed
  assert_false(Cache::has(cache, key))
  let removed_get = Cache::get(cache, key)
  match removed_get {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test cache size
  assert_eq(Cache::size(cache), 0)
}

// Test 2: Cache Expiration
test "cache expiration" {
  // Create a cache with TTL
  let cache = Cache::with_ttl("ttl_cache", 100, 2000) // 100 items, 2 seconds TTL
  
  // Put an item in the cache
  let key = "expiring_key"
  let value = "expiring_value"
  
  let put_result = Cache::put(cache, key, value)
  assert_true(put_result.is_ok())
  
  // Verify the item is immediately available
  let get_result = Cache::get(cache, key)
  match get_result {
    Some(retrieved_value) => assert_eq(retrieved_value, value)
    None => assert_true(false)
  }
  
  // Simulate time passing (in a real implementation, this would involve waiting)
  // For testing purposes, we'll manually expire the item
  Cache::expire_item(cache, key)
  
  // Verify the item is expired
  let expired_get = Cache::get(cache, key)
  match expired_get {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test custom TTL for specific item
  let custom_key = "custom_ttl_key"
  let custom_value = "custom_ttl_value"
  
  let custom_put = Cache::put_with_ttl(cache, custom_key, custom_value, 5000) // 5 seconds TTL
  assert_true(custom_put.is_ok())
  
  // Verify the item is available
  let custom_get = Cache::get(cache, custom_key)
  match custom_get {
    Some(retrieved_value) => assert_eq(retrieved_value, custom_value)
    None => assert_true(false)
  }
  
  // Verify the cache tracks TTL correctly
  let ttl_info = Cache::get_ttl_info(cache, custom_key)
  match ttl_info {
    Some(info) => assert_true(info.remaining_ms > 4000) // Should have close to 5 seconds remaining
    None => assert_true(false)
  }
}

// Test 3: Cache Eviction Policies
test "cache eviction policies" {
  // Test LRU (Least Recently Used) eviction
  let lru_cache = Cache::with_eviction("lru_cache", 3, LRU)
  
  // Fill the cache to capacity
  Cache::put(lru_cache, "key1", "value1")
  Cache::put(lru_cache, "key2", "value2")
  Cache::put(lru_cache, "key3", "value3")
  
  assert_eq(Cache::size(lru_cache), 3)
  
  // Access key1 to make it recently used
  Cache::get(lru_cache, "key1")
  
  // Add a new item, which should evict key2 (least recently used)
  Cache::put(lru_cache, "key4", "value4")
  
  assert_eq(Cache::size(lru_cache), 3)
  assert_true(Cache::has(lru_cache, "key1")) // Should still exist (recently accessed)
  assert_false(Cache::has(lru_cache, "key2")) // Should be evicted (least recently used)
  assert_true(Cache::has(lru_cache, "key3")) // Should still exist
  assert_true(Cache::has(lru_cache, "key4")) // Should exist (newly added)
  
  // Test LFU (Least Frequently Used) eviction
  let lfu_cache = Cache::with_eviction("lfu_cache", 3, LFU)
  
  // Fill the cache
  Cache::put(lfu_cache, "key1", "value1")
  Cache::put(lfu_cache, "key2", "value2")
  Cache::put(lfu_cache, "key3", "value3")
  
  // Access key1 multiple times to increase its frequency
  Cache::get(lfu_cache, "key1")
  Cache::get(lfu_cache, "key1")
  Cache::get(lfu_cache, "key1")
  
  // Access key2 once
  Cache::get(lfu_cache, "key2")
  
  // Add a new item, which should evict key3 (least frequently used)
  Cache::put(lfu_cache, "key4", "value4")
  
  assert_eq(Cache::size(lfu_cache), 3)
  assert_true(Cache::has(lfu_cache, "key1")) // Should still exist (most frequently used)
  assert_true(Cache::has(lfu_cache, "key2")) // Should still exist
  assert_false(Cache::has(lfu_cache, "key3")) // Should be evicted (least frequently used)
  assert_true(Cache::has(lfu_cache, "key4")) // Should exist (newly added)
  
  // Test FIFO (First In First Out) eviction
  let fifo_cache = Cache::with_eviction("fifo_cache", 3, FIFO)
  
  // Fill the cache
  Cache::put(fifo_cache, "key1", "value1")
  Cache::put(fifo_cache, "key2", "value2")
  Cache::put(fifo_cache, "key3", "value3")
  
  // Add a new item, which should evict key1 (first in)
  Cache::put(fifo_cache, "key4", "value4")
  
  assert_eq(Cache::size(fifo_cache), 3)
  assert_false(Cache::has(fifo_cache, "key1")) // Should be evicted (first in)
  assert_true(Cache::has(fifo_cache, "key2")) // Should still exist
  assert_true(Cache::has(fifo_cache, "key3")) // Should still exist
  assert_true(Cache::has(fifo_cache, "key4")) // Should exist (newly added)
}

// Test 4: Cache Statistics
test "cache statistics" {
  let cache = Cache::new("stats_cache", 10)
  
  // Initially, all stats should be zero
  let stats = Cache::get_statistics(cache)
  assert_eq(stats.hits, 0)
  assert_eq(stats.misses, 0)
  assert_eq(stats.puts, 0)
  assert_eq(stats.evictions, 0)
  assert_eq(stats.hit_rate, 0.0)
  
  // Add an item
  Cache::put(cache, "key1", "value1")
  
  // Stats should show one put
  stats = Cache::get_statistics(cache)
  assert_eq(stats.puts, 1)
  assert_eq(stats.hits, 0)
  assert_eq(stats.misses, 0)
  
  // Get the item (cache hit)
  Cache::get(cache, "key1")
  
  // Stats should show one hit
  stats = Cache::get_statistics(cache)
  assert_eq(stats.hits, 1)
  assert_eq(stats.misses, 0)
  assert_eq(stats.hit_rate, 1.0)
  
  // Get a non-existent item (cache miss)
  Cache::get(cache, "non_existent")
  
  // Stats should show one miss and adjusted hit rate
  stats = Cache::get_statistics(cache)
  assert_eq(stats.hits, 1)
  assert_eq(stats.misses, 1)
  assert_eq(stats.hit_rate, 0.5)
  
  // Fill the cache to test eviction stats
  for i in 2..=11 { // Exceed capacity by 1
    Cache::put(cache, "key" + i.to_string(), "value" + i.to_string())
  }
  
  // Stats should show evictions
  stats = Cache::get_statistics(cache)
  assert_eq(stats.evictions, 1) // One item should be evicted
  assert_eq(stats.puts, 11) // Total puts should be 11
  
  // Reset statistics
  Cache::reset_statistics(cache)
  
  // Stats should be reset
  stats = Cache::get_statistics(cache)
  assert_eq(stats.hits, 0)
  assert_eq(stats.misses, 0)
  assert_eq(stats.puts, 0)
  assert_eq(stats.evictions, 0)
  assert_eq(stats.hit_rate, 0.0)
}

// Test 5: Cache Persistence
test "cache persistence" {
  // Create a cache with persistence enabled
  let persistent_cache = Cache::with_persistence("persistent_cache", 10, "/tmp/cache_data")
  
  // Add items to the cache
  Cache::put(persistent_cache, "persistent_key1", "persistent_value1")
  Cache::put(persistent_cache, "persistent_key2", "persistent_value2")
  Cache::put(persistent_cache, "persistent_key3", "persistent_value3")
  
  // Save the cache to disk
  let save_result = Cache::save(persistent_cache)
  assert_true(save_result.is_ok())
  
  // Create a new cache instance and load from disk
  let loaded_cache = Cache::with_persistence("loaded_cache", 10, "/tmp/cache_data")
  let load_result = Cache::load(loaded_cache)
  assert_true(load_result.is_ok())
  
  // Verify the loaded cache has the same items
  assert_true(Cache::has(loaded_cache, "persistent_key1"))
  assert_true(Cache::has(loaded_cache, "persistent_key2"))
  assert_true(Cache::has(loaded_cache, "persistent_key3"))
  
  let loaded_value1 = Cache::get(loaded_cache, "persistent_key1")
  match loaded_value1 {
    Some(value) => assert_eq(value, "persistent_value1")
    None => assert_true(false)
  }
  
  // Test auto-save functionality
  Cache::enable_auto_save(loaded_cache, 60000) // Auto-save every minute
  
  // Add a new item
  Cache::put(loaded_cache, "auto_save_key", "auto_save_value")
  
  // Simulate auto-save (in a real implementation, this would happen automatically)
  let auto_save_result = Cache::trigger_auto_save(loaded_cache)
  assert_true(auto_save_result.is_ok())
  
  // Create another cache instance and verify the auto-saved data
  let auto_loaded_cache = Cache::with_persistence("auto_loaded_cache", 10, "/tmp/cache_data")
  let auto_load_result = Cache::load(auto_loaded_cache)
  assert_true(auto_load_result.is_ok())
  
  assert_true(Cache::has(auto_loaded_cache, "auto_save_key"))
  let auto_saved_value = Cache::get(auto_loaded_cache, "auto_save_key")
  match auto_saved_value {
    Some(value) => assert_eq(value, "auto_save_value")
    None => assert_true(false)
  }
}

// Test 6: Cache Sharding
test "cache sharding" {
  // Create a sharded cache with 3 shards
  let sharded_cache = ShardedCache::new("sharded_cache", 30, 3) // Total 30 items, 3 shards
  
  // Add items to the sharded cache
  for i in 0..=29 {
    let key = "shard_key" + i.to_string()
    let value = "shard_value" + i.to_string()
    Cache::put(sharded_cache, key, value)
  }
  
  // Verify all items are in the cache
  assert_eq(Cache::size(sharded_cache), 30)
  
  for i in 0..=29 {
    let key = "shard_key" + i.to_string()
    assert_true(Cache::has(sharded_cache, key))
    
    let get_result = Cache::get(sharded_cache, key)
    match get_result {
      Some(value) => assert_eq(value, "shard_value" + i.to_string())
      None => assert_true(false)
    }
  }
  
  // Verify shard distribution
  let shard_stats = ShardedCache::get_shard_statistics(sharded_cache)
  assert_eq(shard_stats.length(), 3)
  
  // Each shard should have approximately 10 items
  for i in 0..=2 {
    let shard_size = shard_stats[i].size
    assert_true(shard_size >= 8 && shard_size <= 12) // Allow some variance
  }
  
  // Test shard-specific operations
  let shard_0 = ShardedCache::get_shard(sharded_cache, 0)
  assert_eq(Cache::size(shard_0), shard_stats[0].size)
  
  // Test adding an item that goes to a specific shard
  let shard_key = "specific_shard_key"
  let shard_value = "specific_shard_value"
  
  // Determine which shard this key would go to
  let target_shard_index = ShardedCache::get_shard_index(sharded_cache, shard_key)
  
  // Add the item
  Cache::put(sharded_cache, shard_key, shard_value)
  
  // Verify it's in the correct shard
  let target_shard = ShardedCache::get_shard(sharded_cache, target_shard_index)
  assert_true(Cache::has(target_shard, shard_key))
  
  // Verify it's not in other shards
  for i in 0..=2 {
    if i != target_shard_index {
      let other_shard = ShardedCache::get_shard(sharded_cache, i)
      assert_false(Cache::has(other_shard, shard_key))
    }
  }
}

// Test 7: Cache Warmer
test "cache warmer" {
  // Create a cache warmer
  let warmer = CacheWarmer::new()
  
  // Define warm-up data sources
  let data_source1 = MockDataSource::new("source1")
  MockDataSource::add_data(data_source1, "warm_key1", "warm_value1")
  MockDataSource::add_data(data_source1, "warm_key2", "warm_value2")
  
  let data_source2 = MockDataSource::new("source2")
  MockDataSource::add_data(data_source2, "warm_key3", "warm_value3")
  MockDataSource::add_data(data_source2, "warm_key4", "warm_value4")
  
  // Add data sources to the warmer
  CacheWarmer::add_data_source(warmer, data_source1)
  CacheWarmer::add_data_source(warmer, data_source2)
  
  // Create a target cache
  let target_cache = Cache::new("warmed_cache", 10)
  
  // Warm up the cache
  let warm_result = CacheWarmer::warm(warmer, target_cache)
  assert_true(warm_result.is_ok())
  
  // Verify the cache is warmed with the expected data
  assert_eq(Cache::size(target_cache), 4)
  assert_true(Cache::has(target_cache, "warm_key1"))
  assert_true(Cache::has(target_cache, "warm_key2"))
  assert_true(Cache::has(target_cache, "warm_key3"))
  assert_true(Cache::has(target_cache, "warm_key4"))
  
  let value1 = Cache::get(target_cache, "warm_key1")
  match value1 {
    Some(value) => assert_eq(value, "warm_value1")
    None => assert_true(false)
  }
  
  // Test selective warming
  let selective_cache = Cache::new("selective_cache", 10)
  let warm_keys = ["warm_key1", "warm_key3"]
  
  let selective_result = CacheWarmer::warm_selective(warmer, selective_cache, warm_keys)
  assert_true(selective_result.is_ok())
  
  // Verify only selected keys are warmed
  assert_eq(Cache::size(selective_cache), 2)
  assert_true(Cache::has(selective_cache, "warm_key1"))
  assert_false(Cache::has(selective_cache, "warm_key2"))
  assert_true(Cache::has(selective_cache, "warm_key3"))
  assert_false(Cache::has(selective_cache, "warm_key4"))
  
  // Test scheduled warming
  let scheduled_cache = Cache::new("scheduled_cache", 10)
  CacheWarmer::schedule_warming(warmer, scheduled_cache, 3600000) // Every hour
  
  // Simulate scheduled warming (in a real implementation, this would happen automatically)
  let scheduled_result = CacheWarmer::trigger_scheduled_warming(warmer, scheduled_cache)
  assert_true(scheduled_result.is_ok())
  
  // Verify the cache is warmed
  assert_eq(Cache::size(scheduled_cache), 4)
}

// Test 8: Cache Invalidation
test "cache invalidation" {
  // Create a cache
  let cache = Cache::new("invalidation_cache", 10)
  
  // Add items with different tags
  Cache::put_with_tags(cache, "user_123", "user_data_123", ["user", "profile"])
  Cache::put_with_tags(cache, "user_456", "user_data_456", ["user", "profile"])
  Cache::put_with_tags(cache, "product_789", "product_data_789", ["product"])
  Cache::put_with_tags(cache, "config_app", "config_data_app", ["config", "app"])
  Cache::put_with_tags(cache, "config_db", "config_data_db", ["config", "database"])
  
  // Verify all items are in the cache
  assert_eq(Cache::size(cache), 5)
  
  // Invalidate by tag
  Cache::invalidate_by_tag(cache, "user")
  
  // Verify user items are invalidated
  assert_eq(Cache::size(cache), 3)
  assert_false(Cache::has(cache, "user_123"))
  assert_false(Cache::has(cache, "user_456"))
  assert_true(Cache::has(cache, "product_789"))
  assert_true(Cache::has(cache, "config_app"))
  assert_true(Cache::has(cache, "config_db"))
  
  // Invalidate by pattern
  Cache::invalidate_by_pattern(cache, "config_*")
  
  // Verify config items are invalidated
  assert_eq(Cache::size(cache), 1)
  assert_true(Cache::has(cache, "product_789"))
  assert_false(Cache::has(cache, "config_app"))
  assert_false(Cache::has(cache, "config_db"))
  
  // Add more items for predicate invalidation
  Cache::put(cache, "temp_1", "temp_data_1")
  Cache::put(cache, "temp_2", "temp_data_2")
  Cache::put(cache, "permanent_1", "permanent_data_1")
  
  // Invalidate by predicate (keys starting with "temp_")
  Cache::invalidate_by_predicate(cache, fn(key) { key.starts_with("temp_") })
  
  // Verify temp items are invalidated
  assert_eq(Cache::size(cache), 2)
  assert_false(Cache::has(cache, "temp_1"))
  assert_false(Cache::has(cache, "temp_2"))
  assert_true(Cache::has(cache, "permanent_1"))
  assert_true(Cache::has(cache, "product_789"))
  
  // Test cascading invalidation
  let parent_cache = Cache::new("parent_cache", 5)
  let child_cache1 = Cache::new("child_cache1", 5)
  let child_cache2 = Cache::new("child_cache2", 5)
  
  // Set up cascading relationships
  Cache::add_child_cache(parent_cache, child_cache1)
  Cache::add_child_cache(parent_cache, child_cache2)
  
  // Add items to parent and child caches
  Cache::put(parent_cache, "parent_key", "parent_value")
  Cache::put(child_cache1, "child1_key", "child1_value")
  Cache::put(child_cache2, "child2_key", "child2_value")
  
  // Invalidate parent cache with cascading
  Cache::invalidate_with_cascade(parent_cache, "parent_key")
  
  // Verify parent item is invalidated
  assert_false(Cache::has(parent_cache, "parent_key"))
  
  // Verify child caches are also invalidated
  assert_eq(Cache::size(child_cache1), 0)
  assert_eq(Cache::size(child_cache2), 0)
}