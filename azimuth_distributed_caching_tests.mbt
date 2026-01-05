// Azimuth Telemetry System - Distributed Caching Tests
// This file contains test cases for distributed caching functionality

// Test 1: Basic Cache Operations
test "basic cache operations" {
  let cache = DistributedCache::new("test_cache")
  
  // Test put and get
  DistributedCache::put(cache, "key1", "value1")
  let result = DistributedCache::get(cache, "key1")
  match result {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // Test non-existent key
  let non_existent = DistributedCache::get(cache, "non_existent_key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test put with TTL
  DistributedCache::put_with_ttl(cache, "key2", "value2", 60)  // 60 seconds TTL
  let ttl_result = DistributedCache::get(cache, "key2")
  match ttl_result {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
}

// Test 2: Cache Expiration
test "cache expiration" {
  let cache = DistributedCache::new("expiration_test_cache")
  
  // Put with very short TTL
  DistributedCache::put_with_ttl(cache, "expire_key", "expire_value", 1)  // 1 second TTL
  
  // Should be available immediately
  let immediate_result = DistributedCache::get(cache, "expire_key")
  match immediate_result {
    Some(value) => assert_eq(value, "expire_value")
    None => assert_true(false)
  }
  
  // Wait for expiration
  Time::sleep(1100)  // Sleep for 1.1 seconds
  
  // Should be expired
  let expired_result = DistributedCache::get(cache, "expire_key")
  match expired_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Cache Eviction Policies
test "cache eviction policies" {
  // Test LRU eviction
  let lru_cache = DistributedCache::new_with_policy("lru_cache", LRU, 3)  // Max 3 items
  
  DistributedCache::put(lru_cache, "key1", "value1")
  DistributedCache::put(lru_cache, "key2", "value2")
  DistributedCache::put(lru_cache, "key3", "value3")
  
  // All should be present
  assert_true(DistributedCache::get(lru_cache, "key1") != None)
  assert_true(DistributedCache::get(lru_cache, "key2") != None)
  assert_true(DistributedCache::get(lru_cache, "key3") != None)
  
  // Access key1 to make it recently used
  DistributedCache::get(lru_cache, "key1")
  
  // Add key4, should evict key2 (least recently used)
  DistributedCache::put(lru_cache, "key4", "value4")
  
  assert_true(DistributedCache::get(lru_cache, "key1") != None)  // Still present
  assert_true(DistributedCache::get(lru_cache, "key2") == None)  // Evicted
  assert_true(DistributedCache::get(lru_cache, "key3") != None)  // Still present
  assert_true(DistributedCache::get(lru_cache, "key4") != None)  // New value
  
  // Test LFU eviction
  let lfu_cache = DistributedCache::new_with_policy("lfu_cache", LFU, 3)  // Max 3 items
  
  DistributedCache::put(lfu_cache, "key1", "value1")
  DistributedCache::put(lfu_cache, "key2", "value2")
  DistributedCache::put(lfu_cache, "key3", "value3")
  
  // Access key1 multiple times to increase frequency
  for _ in 0..=2 {
    DistributedCache::get(lfu_cache, "key1")
  }
  
  // Access key2 once
  DistributedCache::get(lfu_cache, "key2")
  
  // Add key4, should evict key3 (least frequently used)
  DistributedCache::put(lfu_cache, "key4", "value4")
  
  assert_true(DistributedCache::get(lfu_cache, "key1") != None)  // Still present
  assert_true(DistributedCache::get(lfu_cache, "key2") != None)  // Still present
  assert_true(DistributedCache::get(lfu_cache, "key3") == None)  // Evicted
  assert_true(DistributedCache::get(lfu_cache, "key4") != None)  // New value
}

// Test 4: Cache Statistics
test "cache statistics" {
  let cache = DistributedCache::new("stats_cache")
  
  // Initial statistics
  let stats = DistributedCache::get_statistics(cache)
  assert_eq(stats.hits, 0)
  assert_eq(stats.misses, 0)
  assert_eq(stats.puts, 0)
  assert_eq(stats.evictions, 0)
  
  // Put some values
  DistributedCache::put(cache, "key1", "value1")
  DistributedCache::put(cache, "key2", "value2")
  
  let after_puts = DistributedCache::get_statistics(cache)
  assert_eq(after_puts.puts, 2)
  
  // Hit and miss operations
  DistributedCache::get(cache, "key1")  // Hit
  DistributedCache::get(cache, "key2")  // Hit
  DistributedCache::get(cache, "non_existent")  // Miss
  
  let after_operations = DistributedCache::get_statistics(cache)
  assert_eq(after_operations.hits, 2)
  assert_eq(after_operations.misses, 1)
  
  // Calculate hit rate
  let hit_rate = after_operations.hits as Float / (after_operations.hits + after_operations.misses) as Float
  assert_eq(hit_rate, 2.0 / 3.0)
}

// Test 5: Cache Invalidation
test "cache invalidation" {
  let cache = DistributedCache::new("invalidation_cache")
  
  // Put some values
  DistributedCache::put(cache, "key1", "value1")
  DistributedCache::put(cache, "key2", "value2")
  DistributedCache::put(cache, "key3", "value3")
  
  // Verify they exist
  assert_true(DistributedCache::get(cache, "key1") != None)
  assert_true(DistributedCache::get(cache, "key2") != None)
  assert_true(DistributedCache::get(cache, "key3") != None)
  
  // Invalidate specific key
  DistributedCache::invalidate(cache, "key2")
  
  assert_true(DistributedCache::get(cache, "key1") != None)  // Still present
  assert_true(DistributedCache::get(cache, "key2") == None)  // Invalidated
  assert_true(DistributedCache::get(cache, "key3") != None)  // Still present
  
  // Invalidate all
  DistributedCache::invalidate_all(cache)
  
  assert_true(DistributedCache::get(cache, "key1") == None)  // Invalidated
  assert_true(DistributedCache::get(cache, "key3") == None)  // Invalidated
}

// Test 6: Cache Persistence
test "cache persistence" {
  let cache = DistributedCache::new_with_persistence("persistent_cache", "/tmp/cache_data")
  
  // Put some values
  DistributedCache::put(cache, "persistent_key1", "persistent_value1")
  DistributedCache::put(cache, "persistent_key2", "persistent_value2")
  
  // Save to disk
  DistributedCache::persist(cache)
  
  // Create new cache instance (simulate restart)
  let new_cache = DistributedCache::new_with_persistence("persistent_cache", "/tmp/cache_data")
  
  // Load from disk
  DistributedCache::load(new_cache)
  
  // Values should be restored
  let restored_value1 = DistributedCache::get(new_cache, "persistent_key1")
  let restored_value2 = DistributedCache::get(new_cache, "persistent_key2")
  
  match restored_value1 {
    Some(value) => assert_eq(value, "persistent_value1")
    None => assert_true(false)
  }
  
  match restored_value2 {
    Some(value) => assert_eq(value, "persistent_value2")
    None => assert_true(false)
  }
}

// Test 7: Cache Clustering
test "cache clustering" {
  // Create cache cluster with 3 nodes
  let node1 = DistributedCache::new_cluster_node("cluster_cache", "node1", ["node2", "node3"])
  let node2 = DistributedCache::new_cluster_node("cluster_cache", "node2", ["node1", "node3"])
  let node3 = DistributedCache::new_cluster_node("cluster_cache", "node3", ["node1", "node2"])
  
  // Put value on node1
  DistributedCache::put(node1, "cluster_key", "cluster_value")
  
  // Should be accessible from all nodes
  let from_node1 = DistributedCache::get(node1, "cluster_key")
  let from_node2 = DistributedCache::get(node2, "cluster_key")
  let from_node3 = DistributedCache::get(node3, "cluster_key")
  
  match from_node1 {
    Some(value) => assert_eq(value, "cluster_value")
    None => assert_true(false)
  }
  
  match from_node2 {
    Some(value) => assert_eq(value, "cluster_value")
    None => assert_true(false)
  }
  
  match from_node3 {
    Some(value) => assert_eq(value, "cluster_value")
    None => assert_true(false)
  }
  
  // Test invalidation propagation
  DistributedCache::invalidate(node1, "cluster_key")
  
  let after_invalidation_node1 = DistributedCache::get(node1, "cluster_key")
  let after_invalidation_node2 = DistributedCache::get(node2, "cluster_key")
  let after_invalidation_node3 = DistributedCache::get(node3, "cluster_key")
  
  assert_true(after_invalidation_node1 == None)
  assert_true(after_invalidation_node2 == None)
  assert_true(after_invalidation_node3 == None)
}

// Test 8: Cache Consistency
test "cache consistency" {
  let cache = DistributedCache::new("consistency_cache")
  
  // Test compare-and-swap
  DistributedCache::put(cache, "cas_key", "initial_value")
  
  // Successful CAS
  let cas_success = DistributedCache::compare_and_swap(cache, "cas_key", "initial_value", "new_value")
  assert_true(cas_success)
  
  let cas_result = DistributedCache::get(cache, "cas_key")
  match cas_result {
    Some(value) => assert_eq(value, "new_value")
    None => assert_true(false)
  }
  
  // Failed CAS (expected value doesn't match)
  let cas_failure = DistributedCache::compare_and_swap(cache, "cas_key", "wrong_expected", "another_value")
  assert_false(cas_failure)
  
  let cas_after_failure = DistributedCache::get(cache, "cas_key")
  match cas_after_failure {
    Some(value) => assert_eq(value, "new_value")  // Should remain unchanged
    None => assert_true(false)
  }
}

// Test 9: Cache Performance
test "cache performance" {
  let cache = DistributedCache::new("performance_cache")
  
  // Put 1000 items
  let start_time = Time::now()
  for i in 0..=999 {
    DistributedCache::put(cache, "key_" + i.to_string(), "value_" + i.to_string())
  }
  let put_time = Time::now() - start_time
  
  // Get 1000 items
  start_time = Time::now()
  for i in 0..=999 {
    DistributedCache::get(cache, "key_" + i.to_string())
  }
  let get_time = Time::now() - start_time
  
  // Performance should be reasonable
  assert_true(put_time < 5000)  // Less than 5 seconds for 1000 puts
  assert_true(get_time < 1000)  // Less than 1 second for 1000 gets
  
  // Check statistics
  let stats = DistributedCache::get_statistics(cache)
  assert_eq(stats.puts, 1000)
  assert_eq(stats.hits, 1000)
  assert_eq(stats.misses, 0)
}

// Test 10: Cache with Telemetry Data
test "cache with telemetry data" {
  let cache = DistributedCache::new("telemetry_cache")
  
  // Create sample telemetry data
  let telemetry_data = TelemetryData::new(
    "trace_123",
    "span_456",
    "test_service",
    150,
    Ok,
    1234567890L
  )
  
  // Cache telemetry data
  let serialized_data = TelemetryData::serialize(telemetry_data)
  DistributedCache::put(cache, "telemetry_123", serialized_data)
  
  // Retrieve and deserialize
  let cached_data = DistributedCache::get(cache, "telemetry_123")
  match cached_data {
    Some(data) => {
      let deserialized = TelemetryData::deserialize(data)
      assert_eq(deserialized.trace_id, "trace_123")
      assert_eq(deserialized.span_id, "span_456")
      assert_eq(deserialized.service_name, "test_service")
      assert_eq(deserialized.duration, 150)
    }
    None => assert_true(false)
  }
  
  // Test cache with TTL for telemetry data
  DistributedCache::put_with_ttl(cache, "telemetry_short_lived", serialized_data, 60)
  
  let short_lived = DistributedCache::get(cache, "telemetry_short_lived")
  match short_lived {
    Some(data) => {
      let deserialized = TelemetryData::deserialize(data)
      assert_eq(deserialized.trace_id, "trace_123")
    }
    None => assert_true(false)
  }
}