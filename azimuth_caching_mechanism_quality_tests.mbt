// Azimuth Telemetry System - Caching Mechanism Quality Tests
// This file contains comprehensive test cases for caching functionality

// Test 1: Basic Cache Operations
test "basic cache operations" {
  let cache = Cache::new(100)  // Cache with capacity for 100 items
  
  // Test cache is initially empty
  assert_eq(Cache::size(cache), 0)
  assert_true(Cache::is_empty(cache))
  
  // Test putting and getting items
  Cache::put(cache, "key1", "value1")
  assert_eq(Cache::size(cache), 1)
  assert_false(Cache::is_empty(cache))
  
  let value1 = Cache::get(cache, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // Test getting non-existent item
  let non_existent = Cache::get(cache, "non_existent")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test updating existing item
  Cache::put(cache, "key1", "updated_value1")
  let updated_value1 = Cache::get(cache, "key1")
  match updated_value1 {
    Some(v) => assert_eq(v, "updated_value1")
    None => assert_true(false)
  }
  
  // Test removing items
  let removed = Cache::remove(cache, "key1")
  match removed {
    Some(v) => assert_eq(v, "updated_value1")
    None => assert_true(false)
  }
  
  assert_eq(Cache::size(cache), 0)
  assert_true(Cache::is_empty(cache))
  
  // Test removing non-existent item
  let non_existent_removed = Cache::remove(cache, "non_existent")
  match non_existent_removed {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Cache Capacity and Eviction
test "cache capacity and eviction" {
  let cache = Cache::new(3)  // Small cache to test eviction
  
  // Fill cache to capacity
  Cache::put(cache, "key1", "value1")
  Cache::put(cache, "key2", "value2")
  Cache::put(cache, "key3", "value3")
  
  assert_eq(Cache::size(cache), 3)
  
  // All items should be present
  assert_eq(Cache::get(cache, "key1"), Some("value1"))
  assert_eq(Cache::get(cache, "key2"), Some("value2"))
  assert_eq(Cache::get(cache, "key3"), Some("value3"))
  
  // Add one more item (should trigger eviction)
  Cache::put(cache, "key4", "value4")
  
  assert_eq(Cache::size(cache), 3)  // Size should remain at capacity
  
  // One item should have been evicted (LRU policy)
  // key1 should be evicted as it was accessed least recently
  assert_eq(Cache::get(cache, "key1"), None)
  assert_eq(Cache::get(cache, "key2"), Some("value2"))
  assert_eq(Cache::get(cache, "key3"), Some("value3"))
  assert_eq(Cache::get(cache, "key4"), Some("value4"))
  
  // Access key2 to make it most recently used
  Cache::get(cache, "key2")
  
  // Add another item (should evict key3)
  Cache::put(cache, "key5", "value5")
  
  assert_eq(Cache::size(cache), 3)
  assert_eq(Cache::get(cache, "key2"), Some("value2"))  // Still present (recently accessed)
  assert_eq(Cache::get(cache, "key3"), None)  // Evicted
  assert_eq(Cache::get(cache, "key4"), Some("value4"))
  assert_eq(Cache::get(cache, "key5"), Some("value5"))
}

// Test 3: Cache Expiration
test "cache expiration" {
  let cache = Cache::new_with_ttl(100, 1000)  // Cache with 1 second TTL
  
  // Put item in cache
  Cache::put(cache, "key1", "value1")
  
  // Item should be immediately available
  assert_eq(Cache::get(cache, "key1"), Some("value1"))
  
  // Wait for item to expire
  Time::sleep(1100)  // Wait longer than TTL
  
  // Item should be expired
  assert_eq(Cache::get(cache, "key1"), None)
  
  // Add new item
  Cache::put(cache, "key2", "value2")
  
  // Item should be available
  assert_eq(Cache::get(cache, "key2"), Some("value2"))
  
  // Update item before it expires
  Time::sleep(500)  // Wait half of TTL
  Cache::put(cache, "key2", "updated_value2")
  
  // Wait for original TTL to pass
  Time::sleep(600)  // Total wait time = 1100ms > original TTL
  
  // Item should still be available (updated TTL)
  assert_eq(Cache::get(cache, "key2"), Some("updated_value2"))
}

// Test 4: Cache Statistics
test "cache statistics" {
  let cache = Cache::new(10)
  
  // Initial statistics should be zero
  let stats = Cache::stats(cache)
  assert_eq(stats.hits, 0)
  assert_eq(stats.misses, 0)
  assert_eq(stats.puts, 0)
  assert_eq(stats.evictions, 0)
  
  // Put items
  Cache::put(cache, "key1", "value1")
  Cache::put(cache, "key2", "value2")
  
  stats = Cache::stats(cache)
  assert_eq(stats.hits, 0)
  assert_eq(stats.misses, 0)
  assert_eq(stats.puts, 2)
  assert_eq(stats.evictions, 0)
  
  // Get existing items
  Cache::get(cache, "key1")
  Cache::get(cache, "key2")
  
  stats = Cache::stats(cache)
  assert_eq(stats.hits, 2)
  assert_eq(stats.misses, 0)
  assert_eq(stats.puts, 2)
  assert_eq(stats.evictions, 0)
  
  // Get non-existing items
  Cache::get(cache, "non_existent1")
  Cache::get(cache, "non_existent2")
  
  stats = Cache::stats(cache)
  assert_eq(stats.hits, 2)
  assert_eq(stats.misses, 2)
  assert_eq(stats.puts, 2)
  assert_eq(stats.evictions, 0)
  
  // Fill cache to capacity and trigger eviction
  for i in 3..=11 {
    Cache::put(cache, "key" + i.to_string(), "value" + i.to_string())
  }
  
  stats = Cache::stats(cache)
  assert_eq(stats.hits, 2)
  assert_eq(stats.misses, 2)
  assert_eq(stats.puts, 10)  // 2 initial + 8 more (key3-key10)
  assert_eq(stats.evictions, 1)  // key1 should be evicted
  
  // Calculate hit rate
  let hit_rate = Cache::hit_rate(cache)
  assert_eq(hit_rate, 2.0 / 4.0)  // 2 hits out of 4 total gets
}

// Test 5: Cache Clear and Reset
test "cache clear and reset" {
  let cache = Cache::new(10)
  
  // Add items to cache
  for i in 1..=5 {
    Cache::put(cache, "key" + i.to_string(), "value" + i.to_string())
  }
  
  assert_eq(Cache::size(cache), 5)
  
  // Clear cache
  Cache::clear(cache)
  
  // Cache should be empty
  assert_eq(Cache::size(cache), 0)
  assert_true(Cache::is_empty(cache))
  
  // All items should be gone
  for i in 1..=5 {
    assert_eq(Cache::get(cache, "key" + i.to_string()), None)
  }
  
  // Statistics should be preserved
  let stats = Cache::stats(cache)
  assert_eq(stats.puts, 5)
  
  // Reset cache (clears statistics)
  Cache::reset(cache)
  
  stats = Cache::stats(cache)
  assert_eq(stats.hits, 0)
  assert_eq(stats.misses, 0)
  assert_eq(stats.puts, 0)
  assert_eq(stats.evictions, 0)
}

// Test 6: Cache Persistence
test "cache persistence" {
  let cache = Cache::new(10)
  let persistence_file = "cache_persistence.json"
  
  // Add items to cache
  Cache::put(cache, "key1", "value1")
  Cache::put(cache, "key2", "value2")
  Cache::put(cache, "key3", "value3")
  
  // Persist cache to file
  let persist_result = Cache::persist(cache, persistence_file)
  match persist_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // Create new cache and load from file
  let new_cache = Cache::new(10)
  
  let load_result = Cache::load(new_cache, persistence_file)
  match load_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // Verify items were loaded
  assert_eq(Cache::get(new_cache, "key1"), Some("value1"))
  assert_eq(Cache::get(new_cache, "key2"), Some("value2"))
  assert_eq(Cache::get(new_cache, "key3"), Some("value3"))
  
  // Clean up
  FileManager::delete_file(persistence_file)
}

// Test 7: Cache with Custom Loader
test "cache with custom loader" {
  let cache = Cache::new_with_loader(10, func(key) {
    if key == "remote_key1" {
      Some("remote_value1")
    } else if key == "remote_key2" {
      Some("remote_value2")
    } else {
      None
    }
  })
  
  // Get non-existent key (should trigger loader)
  let value1 = Cache::get_or_load(cache, "remote_key1")
  match value1 {
    Some(v) => assert_eq(v, "remote_value1")
    None => assert_true(false)
  }
  
  // Verify item is now in cache
  let cached_value1 = Cache::get(cache, "remote_key1")
  match cached_value1 {
    Some(v) => assert_eq(v, "remote_value1")
    None => assert_true(false)
  }
  
  // Get another key
  let value2 = Cache::get_or_load(cache, "remote_key2")
  match value2 {
    Some(v) => assert_eq(v, "remote_value2")
    None => assert_true(false)
  }
  
  // Get key that loader can't provide
  let value3 = Cache::get_or_load(cache, "unknown_key")
  match value3 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 8: Cache with Filter and Transform
test "cache with filter and transform" {
  let cache = Cache::new(10)
  
  // Add items to cache
  Cache::put(cache, "key1", "value1")
  Cache::put(cache, "key2", "value2")
  Cache::put(cache, "temp_key", "temp_value")
  
  // Filter cache
  let filtered_cache = Cache::filter(cache, func(key, value) {
    !key.contains("temp")
  })
  
  // Filtered cache should only contain non-temp items
  assert_eq(Cache::size(filtered_cache), 2)
  assert_eq(Cache::get(filtered_cache, "key1"), Some("value1"))
  assert_eq(Cache::get(filtered_cache, "key2"), Some("value2"))
  assert_eq(Cache::get(filtered_cache, "temp_key"), None)
  
  // Transform cache values
  let transformed_cache = Cache::transform_values(cache, func(key, value) {
    value + "_transformed"
  })
  
  // Transformed cache should have transformed values
  assert_eq(Cache::get(transformed_cache, "key1"), Some("value1_transformed"))
  assert_eq(Cache::get(transformed_cache, "key2"), Some("value2_transformed"))
  assert_eq(Cache::get(transformed_cache, "temp_key"), Some("temp_value_transformed"))
}

// Test 9: Cache with Size-Based Eviction
test "cache with size-based eviction" {
  let cache = Cache::new_with_max_size(100)  // Max 100 bytes
  
  // Add small items
  Cache::put(cache, "key1", "x")  // 4 bytes
  Cache::put(cache, "key2", "xx")  // 5 bytes
  Cache::put(cache, "key3", "xxx")  // 6 bytes
  
  assert_eq(Cache::size(cache), 3)
  assert_eq(Cache::current_size(cache), 15)  // 4 + 5 + 6
  
  // Add large item that exceeds capacity
  let large_value = "x".repeat(90)  // 90 bytes
  Cache::put(cache, "large_key", large_value)
  
  // Small items should be evicted to make room
  assert_eq(Cache::get(cache, "key1"), None)
  assert_eq(Cache::get(cache, "key2"), None)
  assert_eq(Cache::get(cache, "key3"), None)
  assert_eq(Cache::get(cache, "large_key"), Some(large_value))
  
  assert_eq(Cache::size(cache), 1)
  assert_true(Cache::current_size(cache) <= 100)
}

// Test 10: Multi-Level Cache
test "multi-level cache" {
  // Create L1 (memory) cache and L2 (disk) cache
  let l1_cache = Cache::new(5)  // Small memory cache
  let l2_cache = Cache::new(20)  // Larger disk cache
  
  // Create multi-level cache
  let multi_cache = MultiLevelCache::new(l1_cache, l2_cache)
  
  // Put item in multi-level cache
  MultiLevelCache::put(multi_cache, "key1", "value1")
  
  // Item should be in L1 cache
  assert_eq(Cache::get(l1_cache, "key1"), Some("value1"))
  assert_eq(Cache::get(l2_cache, "key1"), None)  // Not in L2 yet
  
  // Fill L1 cache to trigger eviction
  for i in 2..=6 {
    MultiLevelCache::put(multi_cache, "key" + i.to_string(), "value" + i.to_string())
  }
  
  // key1 should be evicted from L1 but moved to L2
  assert_eq(Cache::get(l1_cache, "key1"), None)
  assert_eq(Cache::get(l2_cache, "key1"), Some("value1"))
  
  // Get evicted item (should promote back to L1)
  let value1 = MultiLevelCache::get(multi_cache, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // key1 should be back in L1
  assert_eq(Cache::get(l1_cache, "key1"), Some("value1"))
  
  // Test statistics
  let stats = MultiLevelCache::stats(multi_cache)
  assert_eq(stats.l1_hits, 1)
  assert_eq(stats.l2_hits, 1)
  assert_eq(stats.total_hits, 2)
}