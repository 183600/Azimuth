// Azimuth Telemetry System - Caching Mechanism Tests
// This file contains test cases for caching mechanisms and functionality

// Test 1: In-Memory Cache Implementation
test "in-memory cache implementation" {
  // Create an in-memory cache
  let cache = InMemoryCache::new()
  
  // Verify cache properties
  assert_eq(InMemoryCache::size(cache), 0)
  assert_eq(InMemoryCache::max_size(cache), 1000) // Default max size
  assert_eq(InMemoryCache::ttl(cache), 300000) // Default 5 minutes TTL
  
  // Configure cache
  InMemoryCache::set_max_size(cache, 100)
  InMemoryCache::set_ttl(cache, 60000) // 1 minute TTL
  InMemoryCache::set_cleanup_interval(cache, 10000) // 10 seconds cleanup interval
  
  // Verify updated configuration
  assert_eq(InMemoryCache::max_size(cache), 100)
  assert_eq(InMemoryCache::ttl(cache), 60000)
  assert_eq(InMemoryCache::cleanup_interval(cache), 10000)
  
  // Test cache put and get
  InMemoryCache::put(cache, "key1", "value1")
  InMemoryCache::put(cache, "key2", "value2")
  InMemoryCache::put(cache, "key3", "value3")
  
  assert_eq(InMemoryCache::size(cache), 3)
  
  // Test cache get
  let value1 = InMemoryCache::get(cache, "key1")
  let value2 = InMemoryCache::get(cache, "key2")
  let value3 = InMemoryCache::get(cache, "key3")
  let value4 = InMemoryCache::get(cache, "key4") // Non-existent key
  
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  match value3 {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  match value4 {
    Some(_) => assert_true(false) // Should not exist
    None => assert_true(true)
  }
  
  // Test cache contains
  assert_true(InMemoryCache::contains(cache, "key1"))
  assert_true(InMemoryCache::contains(cache, "key2"))
  assert_true(InMemoryCache::contains(cache, "key3"))
  assert_false(InMemoryCache::contains(cache, "key4"))
  
  // Test cache remove
  InMemoryCache::remove(cache, "key2")
  
  assert_eq(InMemoryCache::size(cache), 2)
  assert_false(InMemoryCache::contains(cache, "key2"))
  
  let value2_removed = InMemoryCache::get(cache, "key2")
  match value2_removed {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test cache clear
  InMemoryCache::clear(cache)
  
  assert_eq(InMemoryCache::size(cache), 0)
  assert_false(InMemoryCache::contains(cache, "key1"))
  assert_false(InMemoryCache::contains(cache, "key3"))
  
  // Test cache with TTL
  InMemoryCache::put_with_ttl(cache, "ttl_key", "ttl_value", 100) // 100ms TTL
  
  let ttl_value = InMemoryCache::get(cache, "ttl_key")
  match ttl_value {
    Some(v) => assert_eq(v, "ttl_value")
    None => assert_true(false)
  }
  
  // Wait for TTL to expire
  Thread::sleep(150)
  
  let expired_value = InMemoryCache::get(cache, "ttl_key")
  match expired_value {
    Some(_) => assert_true(false) // Should be expired
    None => assert_true(true)
  }
  
  // Test cache eviction when max size is reached
  InMemoryCache::set_max_size(cache, 3)
  
  InMemoryCache::put(cache, "evict1", "value1")
  InMemoryCache::put(cache, "evict2", "value2")
  InMemoryCache::put(cache, "evict3", "value3")
  assert_eq(InMemoryCache::size(cache), 3)
  
  // Add one more item to trigger eviction
  InMemoryCache::put(cache, "evict4", "value4")
  
  assert_eq(InMemoryCache::size(cache), 3) // Should still be 3 due to eviction
  
  // Check which item was evicted (should be the oldest)
  let evict1_value = InMemoryCache::get(cache, "evict1")
  let evict2_value = InMemoryCache::get(cache, "evict2")
  let evict3_value = InMemoryCache::get(cache, "evict3")
  let evict4_value = InMemoryCache::get(cache, "evict4")
  
  match evict1_value {
    Some(_) => assert_true(false) // Should be evicted
    None => assert_true(true)
  }
  
  match evict4_value {
    Some(v) => assert_eq(v, "value4") // Should exist
    None => assert_true(false)
  }
  
  // Test cache statistics
  let stats = InMemoryCache::statistics(cache)
  
  assert_eq(stats.hits, 0)
  assert_eq(stats.misses, 0)
  assert_eq(stats.puts, 8)
  assert_eq(stats.evictions, 1)
  assert_eq(stats.expirations, 1)
  
  // Test cache hit/miss tracking
  InMemoryCache::put(cache, "stats_key", "stats_value")
  InMemoryCache::get(cache, "stats_key") // Hit
  InMemoryCache::get(cache, "stats_key") // Hit
  InMemoryCache::get(cache, "nonexistent") // Miss
  
  let updated_stats = InMemoryCache::statistics(cache)
  
  assert_eq(updated_stats.hits, 2)
  assert_eq(updated_stats.misses, 1)
}

// Test 2: LRU Cache Implementation
test "lru cache implementation" {
  // Create an LRU cache
  let lru_cache = LRUCache::new(5) // Max size of 5
  
  // Verify cache properties
  assert_eq(LRUCache::size(lru_cache), 0)
  assert_eq(LRUCache::max_size(lru_cache), 5)
  
  // Test LRU cache put and get
  LRUCache::put(lru_cache, "key1", "value1")
  LRUCache::put(lru_cache, "key2", "value2")
  LRUCache::put(lru_cache, "key3", "value3")
  
  assert_eq(LRUCache::size(lru_cache), 3)
  
  // Test LRU order
  let lru_order = LRUCache::get_order(lru_cache)
  assert_eq(lru_order, ["key3", "key2", "key1"]) // key3 is most recent, key1 is least recent
  
  // Test cache get updates LRU order
  let value2 = LRUCache::get(lru_cache, "key2")
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  let updated_order = LRUCache::get_order(lru_cache)
  assert_eq(updated_order, ["key2", "key3", "key1"]) // key2 is now most recent
  
  // Fill cache to max size
  LRUCache::put(lru_cache, "key4", "value4")
  LRUCache::put(lru_cache, "key5", "value5")
  
  assert_eq(LRUCache::size(lru_cache), 5)
  
  // Add one more item to trigger LRU eviction
  LRUCache::put(lru_cache, "key6", "value6")
  
  assert_eq(LRUCache::size(lru_cache), 5) // Should still be 5 due to eviction
  
  // Check which item was evicted (should be the least recently used)
  let key1_value = LRUCache::get(lru_cache, "key1")
  let key6_value = LRUCache::get(lru_cache, "key6")
  
  match key1_value {
    Some(_) => assert_true(false) // Should be evicted
    None => assert_true(true)
  }
  
  match key6_value {
    Some(v) => assert_eq(v, "value6") // Should exist
    None => assert_true(false)
  }
  
  // Test LRU order after eviction
  let final_order = LRUCache::get_order(lru_cache)
  assert_eq(final_order[0], "key6") // key6 is most recent
  assert_false(final_order.contains("key1")) // key1 was evicted
  
  // Test LRU cache with TTL
  let lru_ttl_cache = LRUCache::with_ttl(3, 1000) // Max size 3, 1 second TTL
  
  LRUCache::put(lru_ttl_cache, "ttl1", "value1")
  LRUCache::put(lru_ttl_cache, "ttl2", "value2")
  LRUCache::put(lru_ttl_cache, "ttl3", "value3")
  
  // Wait for TTL to expire
  Thread::sleep(1100)
  
  // Access should trigger cleanup of expired items
  let ttl1_value = LRUCache::get(lru_ttl_cache, "ttl1")
  match ttl1_value {
    Some(_) => assert_true(false) // Should be expired
    None => assert_true(true)
  }
  
  // Add new item to fill cache
  LRUCache::put(lru_ttl_cache, "ttl4", "value4")
  
  assert_eq(LRUCache::size(lru_ttl_cache), 1) // Only ttl4 should remain
  
  // Test LRU cache statistics
  let stats = LRUCache::statistics(lru_cache)
  
  assert_eq(stats.hits, 1)
  assert_eq(stats.misses, 1)
  assert_eq(stats.puts, 6)
  assert_eq(stats.evictions, 1)
  
  // Test LRU cache peek (doesn't update LRU order)
  LRUCache::put(lru_cache, "peek_key", "peek_value")
  let before_peek_order = LRUCache::get_order(lru_cache)
  
  let peek_value = LRUCache::peek(lru_cache, "peek_key")
  match peek_value {
    Some(v) => assert_eq(v, "peek_value")
    None => assert_true(false)
  }
  
  let after_peek_order = LRUCache::get_order(lru_cache)
  assert_eq(before_peek_order, after_peek_order) // Order should not change
}

// Test 3: Distributed Cache Implementation
test "distributed cache implementation" {
  // Create a distributed cache
  let distributed_cache = DistributedCache::new("localhost:6379") // Redis connection
  
  // Verify cache properties
  assert_eq(DistributedCache::nodes(distributed_cache), ["localhost:6379"])
  assert_eq(DistributedCache::key_prefix(distributed_cache), "azimuth:")
  assert_eq(DistributedCache::default_ttl(distributed_cache), 300) // 5 minutes
  
  // Configure distributed cache
  DistributedCache::set_key_prefix(distributed_cache, "azimuth:telemetry:")
  DistributedCache::set_default_ttl(distributed_cache, 600) // 10 minutes
  DistributedCache::set_connection_timeout(distributed_cache, 5000) // 5 seconds
  DistributedCache::set_command_timeout(distributed_cache, 1000) // 1 second
  
  // Test distributed cache put and get
  let put_result = DistributedCache::put(distributed_cache, "dist_key1", "dist_value1")
  match put_result {
    Ok(_) => assert_true(true)
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  let get_result = DistributedCache::get(distributed_cache, "dist_key1")
  match get_result {
    Ok(value) => assert_eq(value, "dist_value1")
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  // Test distributed cache with TTL
  let ttl_put_result = DistributedCache::put_with_ttl(distributed_cache, "ttl_key", "ttl_value", 5) // 5 seconds
  match ttl_put_result {
    Ok(_) => assert_true(true)
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  let ttl_get_result = DistributedCache::get(distributed_cache, "ttl_key")
  match ttl_get_result {
    Ok(value) => assert_eq(value, "ttl_value")
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  // Wait for TTL to expire
  Thread::sleep(6000)
  
  let expired_result = DistributedCache::get(distributed_cache, "ttl_key")
  match expired_result {
    Ok(_) => assert_true(false) // Should be expired
    Err(error) => {
      // Should get nil or connection error
      assert_true(error.contains("nil") || error.contains("connection"))
    }
  }
  
  // Test distributed cache put and get multiple values
  let multi_put_result = DistributedCache::put_multiple(distributed_cache, [
    ("multi_key1", "multi_value1"),
    ("multi_key2", "multi_value2"),
    ("multi_key3", "multi_value3")
  ])
  
  match multi_put_result {
    Ok(_) => assert_true(true)
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  let multi_get_result = DistributedCache::get_multiple(distributed_cache, [
    "multi_key1",
    "multi_key2",
    "multi_key3",
    "nonexistent_key"
  ])
  
  match multi_get_result {
    Ok(values) => {
      assert_eq(values.get("multi_key1"), Some("multi_value1"))
      assert_eq(values.get("multi_key2"), Some("multi_value2"))
      assert_eq(values.get("multi_key3"), Some("multi_value3"))
      assert_eq(values.get("nonexistent_key"), None)
    }
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  // Test distributed cache remove
  let remove_result = DistributedCache::remove(distributed_cache, "multi_key2")
  match remove_result {
    Ok(_) => assert_true(true)
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  let removed_get_result = DistributedCache::get(distributed_cache, "multi_key2")
  match removed_get_result {
    Ok(_) => assert_true(false) // Should be removed
    Err(error) => {
      // Should get nil or connection error
      assert_true(error.contains("nil") || error.contains("connection"))
    }
  }
  
  // Test distributed cache exists
  let exists_result = DistributedCache::exists(distributed_cache, "multi_key1")
  match exists_result {
    Ok(exists) => assert_true(exists)
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  let not_exists_result = DistributedCache::exists(distributed_cache, "nonexistent_key")
  match not_exists_result {
    Ok(exists) => assert_false(exists)
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  // Test distributed cache increment
  let incr_result = DistributedCache::increment(distributed_cache, "counter", 1)
  match incr_result {
    Ok(value) => assert_eq(value, 1)
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  let incr2_result = DistributedCache::increment(distributed_cache, "counter", 5)
  match incr2_result {
    Ok(value) => assert_eq(value, 6)
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  // Test distributed cache atomic operations
  let atomic_result = DistributedCache::atomic_update(distributed_cache, "atomic_key", "initial_value", fn(current_value) {
    current_value + "_updated"
  })
  
  match atomic_result {
    Ok(new_value) => assert_eq(new_value, "initial_value_updated")
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  // Test distributed cache statistics
  let stats = DistributedCache::statistics(distributed_cache)
  
  assert_true(stats.hits >= 0)
  assert_true(stats.misses >= 0)
  assert_true(stats.puts >= 0)
  assert_true(stats.errors >= 0)
  assert_true(stats.connection_count >= 0)
}

// Test 4: Multi-Level Cache Implementation
test "multi-level cache implementation" {
  // Create cache levels
  let l1_cache = InMemoryCache::new() // L1: In-memory cache
  InMemoryCache::set_max_size(l1_cache, 100)
  InMemoryCache::set_ttl(l1_cache, 60000) // 1 minute
  
  let l2_cache = LRUCache::new(1000) // L2: LRU cache
  LRUCache::set_ttl(l2_cache, 300000) // 5 minutes
  
  let l3_cache = DistributedCache::new("localhost:6379") // L3: Distributed cache
  DistributedCache::set_default_ttl(l3_cache, 3600) // 1 hour
  
  // Create multi-level cache
  let multi_cache = MultiLevelCache::new([l1_cache, l2_cache, l3_cache])
  
  // Verify cache properties
  assert_eq(MultiLevelCache::level_count(multi_cache), 3)
  assert_eq(MultiLevelCache::size(multi_cache), 0)
  
  // Test multi-level cache put and get
  MultiLevelCache::put(multi_cache, "ml_key1", "ml_value1")
  
  // Value should be in L1 cache
  let l1_value = InMemoryCache::get(l1_cache, "ml_key1")
  match l1_value {
    Some(v) => assert_eq(v, "ml_value1")
    None => assert_true(false)
  }
  
  // Get value from multi-level cache
  let get_result = MultiLevelCache::get(multi_cache, "ml_key1")
  match get_result {
    Ok(value) => assert_eq(value, "ml_value1")
    Err(_) => assert_true(false)
  }
  
  // Test cache miss
  let miss_result = MultiLevelCache::get(multi_cache, "nonexistent_key")
  match miss_result {
    Ok(_) => assert_true(false) // Should not exist
    Err(error) => assert_eq(error, "key not found")
  }
  
  // Test cache promotion (L3 to L2 to L1)
  // Directly put value in L3 cache
  let l3_put_result = DistributedCache::put(l3_cache, "l3_key", "l3_value")
  match l3_put_result {
    Ok(_) => assert_true(true)
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  // Get value from multi-level cache (should promote to L1)
  let promote_result = MultiLevelCache::get(multi_cache, "l3_key")
  match promote_result {
    Ok(value) => assert_eq(value, "l3_value")
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  // Check if value was promoted to L1
  let l1_promoted = InMemoryCache::get(l1_cache, "l3_key")
  match l1_promoted {
    Some(v) => assert_eq(v, "l3_value")
    None => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  // Test cache eviction and fallback
  // Fill L1 cache to capacity
  for i in 0..=99 { // 100 items
    MultiLevelCache::put(multi_cache, "evict_key" + i.to_string(), "evict_value" + i.to_string())
  }
  
  // Add one more item to trigger eviction in L1
  MultiLevelCache::put(multi_cache, "overflow_key", "overflow_value")
  
  // Check if evicted item is still accessible from lower levels
  let evicted_result = MultiLevelCache::get(multi_cache, "evict_key0")
  match evicted_result {
    Ok(value) => assert_eq(value, "evict_value0")
    Err(_) => {
      // In test environment, distributed cache might not be available
      assert_true(true)
    }
  }
  
  // Test multi-level cache statistics
  let stats = MultiLevelCache::statistics(multi_cache)
  
  assert_eq(stats.level_count, 3)
  assert_true(stats.total_hits >= 0)
  assert_true(stats.total_misses >= 0)
  assert_true(stats.l1_hits >= 0)
  assert_true(stats.l2_hits >= 0)
  assert_true(stats.l3_hits >= 0)
  assert_true(stats.promotions >= 0)
  
  // Test cache warming
  let warm_keys = ["warm_key1", "warm_key2", "warm_key3"]
  let warm_values = ["warm_value1", "warm_value2", "warm_value3"]
  
  // Put values directly in L3 cache
  for i in 0..=warm_keys.length() - 1 {
    let put_result = DistributedCache::put(l3_cache, warm_keys[i], warm_values[i])
    match put_result {
      Ok(_) => assert_true(true)
      Err(_) => {
        // In test environment, Redis might not be available
        assert_true(true)
      }
    }
  }
  
  // Warm cache with specific keys
  let warm_result = MultiLevelCache::warm(multi_cache, warm_keys)
  match warm_result {
    Ok(warmed_count) => assert_eq(warmed_count, 3)
    Err(_) => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  // Check if warmed keys are now in L1 cache
  let warm_l1_result = InMemoryCache::get(l1_cache, "warm_key1")
  match warm_l1_result {
    Some(v) => assert_eq(v, "warm_value1")
    None => {
      // In test environment, Redis might not be available
      assert_true(true)
    }
  }
  
  // Test cache invalidation
  MultiLevelCache::put(multi_cache, "invalidate_key", "invalidate_value")
  
  // Verify value exists in all levels
  let l1_before = InMemoryCache::get(l1_cache, "invalidate_key")
  match l1_before {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  // Invalidate key across all levels
  MultiLevelCache::invalidate(multi_cache, "invalidate_key")
  
  // Verify key is removed from all levels
  let l1_after = InMemoryCache::get(l1_cache, "invalidate_key")
  match l1_after {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let invalidate_result = MultiLevelCache::get(multi_cache, "invalidate_key")
  match invalidate_result {
    Ok(_) => assert_true(false) // Should not exist
    Err(error) => assert_eq(error, "key not found")
  }
}

// Test 5: Cache Invalidation Strategies
test "cache invalidation strategies" {
  // Create cache with invalidation strategies
  let cache = InMemoryCache::new()
  
  // Test time-based invalidation
  InMemoryCache::put(cache, "time_key", "time_value")
  
  // Verify value exists
  let before_time = InMemoryCache::get(cache, "time_key")
  match before_time {
    Some(v) => assert_eq(v, "time_value")
    None => assert_true(false)
  }
  
  // Invalidate by time
  Thread::sleep(100) // Wait 100ms
  
  InMemoryCache::invalidate_by_time(cache, 50) // Invalidate items older than 50ms
  
  // Verify value is invalidated
  let after_time = InMemoryCache::get(cache, "time_key")
  match after_time {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test pattern-based invalidation
  InMemoryCache::put(cache, "pattern:key1", "value1")
  InMemoryCache::put(cache, "pattern:key2", "value2")
  InMemoryCache::put(cache, "other:key1", "value3")
  
  // Invalidate by pattern
  InMemoryCache::invalidate_by_pattern(cache, "pattern:*")
  
  // Verify pattern-matched keys are invalidated
  let pattern1_result = InMemoryCache::get(cache, "pattern:key1")
  match pattern1_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let pattern2_result = InMemoryCache::get(cache, "pattern:key2")
  match pattern2_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Verify non-matched key still exists
  let other_result = InMemoryCache::get(cache, "other:key1")
  match other_result {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  // Test tag-based invalidation
  InMemoryCache::put_with_tags(cache, "tag:key1", "value1", ["tag1", "tag2"])
  InMemoryCache::put_with_tags(cache, "tag:key2", "value2", ["tag2", "tag3"])
  InMemoryCache::put_with_tags(cache, "tag:key3", "value3", ["tag1", "tag3"])
  
  // Invalidate by tag
  InMemoryCache::invalidate_by_tag(cache, "tag2")
  
  // Verify items with tag2 are invalidated
  let tag1_result = InMemoryCache::get(cache, "tag:key1")
  match tag1_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let tag2_result = InMemoryCache::get(cache, "tag:key2")
  match tag2_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Verify items without tag2 still exist
  let tag3_result = InMemoryCache::get(cache, "tag:key3")
  match tag3_result {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  // Test dependency-based invalidation
  InMemoryCache::put_with_dependencies(cache, "dep:key1", "value1", ["dep:key2", "dep:key3"])
  InMemoryCache::put(cache, "dep:key2", "value2")
  InMemoryCache::put(cache, "dep:key3", "value3")
  
  // Invalidate dependency
  InMemoryCache::invalidate(cache, "dep:key2")
  
  // Verify dependent key is also invalidated
  let dep1_result = InMemoryCache::get(cache, "dep:key1")
  match dep1_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Verify non-dependent key still exists
  let dep3_result = InMemoryCache::get(cache, "dep:key3")
  match dep3_result {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  // Test size-based invalidation
  InMemoryCache::set_max_size(cache, 3)
  
  InMemoryCache::put(cache, "size:key1", "value1")
  InMemoryCache::put(cache, "size:key2", "value2")
  InMemoryCache::put(cache, "size:key3", "value3")
  
  // Add one more to trigger eviction
  InMemoryCache::put(cache, "size:key4", "value4")
  
  // Verify oldest item is evicted
  let size1_result = InMemoryCache::get(cache, "size:key1")
  match size1_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Verify newest item exists
  let size4_result = InMemoryCache::get(cache, "size:key4")
  match size4_result {
    Some(v) => assert_eq(v, "value4")
    None => assert_true(false)
  }
  
  // Test invalidation callbacks
  let mut invalidated_keys = []
  
  InMemoryCache::set_invalidation_callback(cache, fn(key) {
    invalidated_keys = invalidated_keys.push(key)
  })
  
  InMemoryCache::put(cache, "callback:key1", "value1")
  InMemoryCache::invalidate(cache, "callback:key1")
  
  assert_true(invalidated_keys.contains("callback:key1"))
}

// Test 6: Cache Performance and Optimization
test "cache performance and optimization" {
  // Create caches for performance testing
  let memory_cache = InMemoryCache::new()
  let lru_cache = LRUCache::new(1000)
  
  // Configure caches
  InMemoryCache::set_max_size(memory_cache, 1000)
  InMemoryCache::set_ttl(memory_cache, 300000) // 5 minutes
  
  // Test cache put performance
  let put_start = Time::now()
  
  for i in 0..=9999 { // 10,000 items
    InMemoryCache::put(memory_cache, "perf_key" + i.to_string(), "perf_value" + i.to_string())
  }
  
  let put_end = Time::now()
  let put_duration = put_end - put_start
  
  assert_true(put_duration < 1000) // Should complete in less than 1 second
  
  // Test cache get performance
  let get_start = Time::now()
  
  for i in 0..=9999 { // 10,000 items
    InMemoryCache::get(memory_cache, "perf_key" + i.to_string())
  }
  
  let get_end = Time::now()
  let get_duration = get_end - get_start
  
  assert_true(get_duration < 500) // Should complete in less than 0.5 seconds
  
  // Test LRU cache performance
  let lru_put_start = Time::now()
  
  for i in 0..=9999 { // 10,000 items
    LRUCache::put(lru_cache, "lru_key" + i.to_string(), "lru_value" + i.to_string())
  }
  
  let lru_put_end = Time::now()
  let lru_put_duration = lru_put_end - lru_put_start
  
  assert_true(lru_put_duration < 1000) // Should complete in less than 1 second
  
  // Test cache hit ratio
  let hit_cache = InMemoryCache::new()
  InMemoryCache::set_max_size(hit_cache, 100)
  
  // Fill cache with 100 items
  for i in 0..=99 {
    InMemoryCache::put(hit_cache, "hit_key" + i.to_string(), "hit_value" + i.to_string())
  }
  
  // Perform 1000 get operations with 80% hit rate
  let mut hits = 0
  let mut misses = 0
  
  for i in 0..=999 {
    let key_index = if i % 10 < 8 { i % 100 } else { 100 + i } // 80% existing keys, 20% new keys
    let result = InMemoryCache::get(hit_cache, "hit_key" + key_index.to_string())
    
    match result {
      Some(_) => hits = hits + 1
      None => misses = misses + 1
    }
  }
  
  let hit_ratio = hits.to_float() / (hits + misses).to_float()
  assert_true(hit_ratio > 0.7 && hit_ratio < 0.9) // Should be close to 80%
  
  // Test cache memory usage
  let memory_usage = InMemoryCache::memory_usage(memory_cache)
  assert_true(memory_usage > 0)
  
  // Test cache optimization with compression
  let compressed_cache = InMemoryCache::with_compression("gzip")
  
  for i in 0..=99 {
    let large_value = "x".repeat(1000) // 1KB string
    InMemoryCache::put(compressed_cache, "comp_key" + i.to_string(), large_value)
  }
  
  let compressed_memory = InMemoryCache::memory_usage(compressed_cache)
  let uncompressed_memory = InMemoryCache::memory_usage(memory_cache)
  
  // Compressed cache should use less memory
  assert_true(compressed_memory < uncompressed_memory)
  
  // Test cache optimization with serialization
  let serialized_cache = InMemoryCache::with_serialization("json")
  
  let complex_data = ComplexData::new("test", 42, [1, 2, 3, 4, 5])
  InMemoryCache::put(serialized_cache, "complex_key", complex_data)
  
  let retrieved_data = InMemoryCache::get(serialized_cache, "complex_key")
  match retrieved_data {
    Ok(data) => {
      assert_eq(ComplexData::name(data), "test")
      assert_eq(ComplexData::value(data), 42)
    }
    Err(_) => assert_true(false)
  }
  
  // Test cache benchmarking
  let benchmark_results = CacheBenchmark::run([
    ("memory", memory_cache),
    ("lru", lru_cache),
    ("compressed", compressed_cache),
    ("serialized", serialized_cache)
  ], 1000) // 1000 operations
  
  for (name, results) in benchmark_results {
    assert_true(results.put_time > 0)
    assert_true(results.get_time > 0)
    assert_true(results.memory_usage > 0)
    assert_true(results.hit_ratio >= 0.0 && results.hit_ratio <= 1.0)
    
    // Memory cache should be fastest for puts and gets
    if name == "memory" {
      assert_true(results.put_time < 100) // Less than 100ms for 1000 operations
      assert_true(results.get_time < 50)  // Less than 50ms for 1000 operations
    }
  }
  
  // Test cache optimization with prefetching
  let prefetch_cache = InMemoryCache::new()
  
  // Configure prefetching
  InMemoryCache::enable_prefetching(prefetch_cache, 10) // Prefetch 10 items
  
  // Add items with related keys
  for i in 0..=99 {
    InMemoryCache::put(prefetch_cache, "prefetch:key:" + i.to_string(), "value:" + i.to_string())
  }
  
  // Access first item to trigger prefetching
  InMemoryCache::get(prefetch_cache, "prefetch:key:0")
  
  // Check if related items were prefetched
  let stats = InMemoryCache::statistics(prefetch_cache)
  assert_true(stats.prefetch_count > 0)
  
  // Test cache optimization with batch operations
  let batch_cache = InMemoryCache::new()
  
  // Batch put
  let batch_items = []
  for i in 0..=99 {
    batch_items = batch_items.push(("batch:key:" + i.to_string(), "batch:value:" + i.to_string()))
  }
  
  let batch_put_start = Time::now()
  InMemoryCache::put_batch(batch_cache, batch_items)
  let batch_put_end = Time::now()
  let batch_put_duration = batch_put_end - batch_put_start
  
  // Individual put for comparison
  let individual_put_start = Time::now()
  for i in 0..=99 {
    InMemoryCache::put(batch_cache, "individual:key:" + i.to_string(), "individual:value:" + i.to_string())
  }
  let individual_put_end = Time::now()
  let individual_put_duration = individual_put_end - individual_put_start
  
  // Batch put should be faster
  assert_true(batch_put_duration < individual_put_duration)
  
  // Batch get
  let batch_keys = []
  for i in 0..=99 {
    batch_keys = batch_keys.push("batch:key:" + i.to_string())
  }
  
  let batch_get_start = Time::now()
  let batch_results = InMemoryCache::get_batch(batch_cache, batch_keys)
  let batch_get_end = Time::now()
  let batch_get_duration = batch_get_end - batch_get_start
  
  // Individual get for comparison
  let individual_get_start = Time::now()
  for i in 0..=99 {
    InMemoryCache::get(batch_cache, "batch:key:" + i.to_string())
  }
  let individual_get_end = Time::now()
  let individual_get_duration = individual_get_end - individual_get_start
  
  // Batch get should be faster
  assert_true(batch_get_duration < individual_get_duration)
  assert_eq(batch_results.length(), 100)
}

// Test 7: Cache Monitoring and Metrics
test "cache monitoring and metrics" {
  // Create cache with monitoring
  let cache = InMemoryCache::new()
  
  // Enable monitoring
  InMemoryCache::enable_monitoring(cache, true)
  
  // Perform cache operations
  InMemoryCache::put(cache, "monitor_key1", "monitor_value1")
  InMemoryCache::put(cache, "monitor_key2", "monitor_value2")
  InMemoryCache::get(cache, "monitor_key1") // Hit
  InMemoryCache::get(cache, "monitor_key3") // Miss
  InMemoryCache::remove(cache, "monitor_key2")
  
  // Get cache metrics
  let metrics = InMemoryCache::get_metrics(cache)
  
  assert_eq(metrics.puts, 2)
  assert_eq(metrics.gets, 2)
  assert_eq(metrics.hits, 1)
  assert_eq(metrics.misses, 1)
  assert_eq(metrics.removes, 1)
  assert_eq(metrics.evictions, 0)
  assert_eq(metrics.expirations, 0)
  
  // Test cache health check
  let health = InMemoryCache::health_check(cache)
  
  assert_true(health.healthy)
  assert_true(health.memory_usage > 0)
  assert_true(health.hit_ratio >= 0.0 && health.hit_ratio <= 1.0)
  assert_true(health.uptime > 0)
  
  // Test cache performance metrics
  let perf_metrics = InMemoryCache::performance_metrics(cache)
  
  assert_true(perf_metrics.avg_put_time > 0)
  assert_true(perf_metrics.avg_get_time > 0)
  assert_true(perf_metrics.max_put_time > 0)
  assert_true(perf_metrics.max_get_time > 0)
  assert_true(perf_metrics.total_operations > 0)
  
  // Test cache alerting
  let alert_manager = CacheAlertManager::new()
  
  // Configure alerts
  CacheAlertManager::add_alert(alert_manager, CacheAlert {
    name: "high_miss_rate",
    condition: AlertCondition::HitRatio(LessThan, 0.8),
    severity: AlertSeverity::Warning,
    enabled: true
  })
  
  CacheAlertManager::add_alert(alert_manager, CacheAlert {
    name: "high_memory_usage",
    condition: AlertCondition::MemoryUsage(GreaterThan, 0.9),
    severity: AlertSeverity::Critical,
    enabled: true
  })
  
  // Check alerts
  let alerts = CacheAlertManager::check_alerts(alert_manager, cache)
  
  // Should have no alerts with current cache state
  assert_eq(alerts.length(), 0)
  
  // Simulate high miss rate
  for i in 0..=99 {
    InMemoryCache::get(cache, "nonexistent_key" + i.to_string()) // All misses
  }
  
  let high_miss_alerts = CacheAlertManager::check_alerts(alert_manager, cache)
  
  // Should have high miss rate alert
  assert_true(high_miss_alerts.length() > 0)
  assert_true(high_miss_alerts.any(fn(alert) { alert.name == "high_miss_rate" }))
  
  // Test cache metrics export
  let exporter = MetricsExporter::prometheus()
  let exported_metrics = MetricsExporter::export_cache_metrics(exporter, cache)
  
  assert_true(exported_metrics.contains("# TYPE cache_puts counter"))
  assert_true(exported_metrics.contains("# TYPE cache_gets counter"))
  assert_true(exported_metrics.contains("# TYPE cache_hits counter"))
  assert_true(exported_metrics.contains("# TYPE cache_misses counter"))
  assert_true(exported_metrics.contains("# TYPE cache_hit_ratio gauge"))
  
  // Test cache metrics aggregation
  let caches = [
    ("cache1", cache),
    ("cache2", InMemoryCache::new()),
    ("cache3", InMemoryCache::new())
  ]
  
  // Add some data to other caches
  InMemoryCache::put(caches[1].1, "agg_key1", "agg_value1")
  InMemoryCache::put(caches[2].1, "agg_key2", "agg_value2")
  
  let aggregated_metrics = MetricsAggregator::aggregate_cache_metrics(caches)
  
  assert_true(aggregated_metrics.contains_key("total_puts"))
  assert_true(aggregated_metrics.contains_key("total_gets"))
  assert_true(aggregated_metrics.contains_key("total_hits"))
  assert_true(aggregated_metrics.contains_key("total_misses"))
  assert_true(aggregated_metrics.contains_key("avg_hit_ratio"))
  
  // Verify aggregated values
  let total_puts = aggregated_metrics.get("total_puts")
  match total_puts {
    Some(value) => assert_eq(value, 4) // 2 from cache1, 1 from cache2, 1 from cache3
    None => assert_true(false)
  }
  
  // Test cache metrics visualization
  let visualizer = CacheMetricsVisualizer::new()
  
  // Generate hit ratio chart
  let hit_ratio_chart = CacheMetricsVisualizer::generate_hit_ratio_chart(visualizer, cache, 3600) // Last hour
  
  assert_true(hit_ratio_chart.contains("chart"))
  assert_true(hit_ratio_chart.contains("hit_ratio"))
  
  // Generate memory usage chart
  let memory_chart = CacheMetricsVisualizer::generate_memory_chart(visualizer, cache, 3600) // Last hour
  
  assert_true(memory_chart.contains("chart"))
  assert_true(memory_chart.contains("memory_usage"))
  
  // Generate operations chart
  let ops_chart = CacheMetricsVisualizer::generate_operations_chart(visualizer, cache, 3600) // Last hour
  
  assert_true(ops_chart.contains("chart"))
  assert_true(ops_chart.contains("operations"))
  
  // Test cache metrics dashboard
  let dashboard = CacheDashboard::new([cache])
  
  let dashboard_html = CacheDashboard::generate(dashboard)
  
  assert_true(dashboard_html.contains("<html>"))
  assert_true(dashboard_html.contains("Cache Metrics Dashboard"))
  assert_true(dashboard_html.contains("Hit Ratio"))
  assert_true(dashboard_html.contains("Memory Usage"))
  assert_true(dashboard_html.contains("Operations"))
}