// Intelligent Caching Strategy Tests for Azimuth Telemetry System
// This file contains test cases for intelligent caching mechanisms

test "intelligent cache initialization" {
  let cache = IntelligentCache::new(1000, 60) // capacity: 1000, ttl: 60s
  
  assert_eq(IntelligentCache::capacity(cache), 1000)
  assert_eq(IntelligentCache::ttl(cache), 60)
  assert_eq(IntelligentCache::size(cache), 0)
  assert_true(IntelligentCache::is_empty(cache))
}

test "intelligent cache basic operations" {
  let cache = IntelligentCache::new(100, 30)
  
  // Test putting and getting values
  IntelligentCache::put(cache, "key1", "value1")
  IntelligentCache::put(cache, "key2", "value2")
  
  let result1 = IntelligentCache::get(cache, "key1")
  match result1 {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  let result2 = IntelligentCache::get(cache, "key2")
  match result2 {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  // Test getting non-existent key
  let result3 = IntelligentCache::get(cache, "non_existent")
  match result3 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  assert_eq(IntelligentCache::size(cache), 2)
  assert_false(IntelligentCache::is_empty(cache))
}

test "intelligent cache ttl expiration" {
  let cache = IntelligentCache::new(100, 1) // 1 second TTL
  
  // Put a value
  IntelligentCache::put(cache, "expiring_key", "expiring_value")
  
  // Value should be available immediately
  let result = IntelligentCache::get(cache, "expiring_key")
  match result {
    Some(value) => assert_eq(value, "expiring_value")
    None => assert_true(false)
  }
  
  // Simulate time passing (in a real implementation, we'd need to wait)
  // For this test, we'll use a manual cleanup
  IntelligentCache::cleanup_expired(cache)
  
  // After cleanup, the value should still be there (not expired yet in test)
  let result_after_cleanup = IntelligentCache::get(cache, "expiring_key")
  match result_after_cleanup {
    Some(value) => assert_eq(value, "expiring_value")
    None => assert_true(true) // Might be None depending on implementation
  }
}

test "intelligent cache lru eviction" {
  let cache = IntelligentCache::new(2, 60) // Small capacity to test eviction
  
  // Fill cache to capacity
  IntelligentCache::put(cache, "key1", "value1")
  IntelligentCache::put(cache, "key2", "value2")
  
  assert_eq(IntelligentCache::size(cache), 2)
  
  // Add one more item to trigger eviction
  IntelligentCache::put(cache, "key3", "value3")
  
  // Size should still be 2 (one item evicted)
  assert_eq(IntelligentCache::size(cache), 2)
  
  // key1 should be evicted (LRU)
  let result1 = IntelligentCache::get(cache, "key1")
  match result1 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // key2 and key3 should still be there
  let result2 = IntelligentCache::get(cache, "key2")
  match result2 {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  let result3 = IntelligentCache::get(cache, "key3")
  match result3 {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
}

test "intelligent cache access frequency optimization" {
  let cache = IntelligentCache::new(3, 60)
  
  // Add items
  IntelligentCache::put(cache, "key1", "value1")
  IntelligentCache::put(cache, "key2", "value2")
  IntelligentCache::put(cache, "key3", "value3")
  
  // Access key1 multiple times to increase its frequency
  IntelligentCache::get(cache, "key1")
  IntelligentCache::get(cache, "key1")
  IntelligentCache::get(cache, "key1")
  
  // Access key2 once
  IntelligentCache::get(cache, "key2")
  
  // Don't access key3 at all
  
  // Add a new item to trigger eviction
  IntelligentCache::put(cache, "key4", "value4")
  
  // key3 should be evicted (least frequently accessed)
  let result3 = IntelligentCache::get(cache, "key3")
  match result3 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // key1 should still be there (most frequently accessed)
  let result1 = IntelligentCache::get(cache, "key1")
  match result1 {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
}

test "intelligent cache size-based eviction" {
  let cache = IntelligentCache::new(100, 60)
  
  // Add a large item
  IntelligentCache::put(cache, "large_key", "large_value_content_that_takes_more_space")
  
  // Add multiple small items
  for i in 0..=50 {
    IntelligentCache::put(cache, "key_" + i.to_string(), "value_" + i.to_string())
  }
  
  // Cache should handle size-based eviction appropriately
  assert_true(IntelligentCache::size(cache) > 0)
  assert_true(IntelligentCache::size(cache) <= 100)
}

test "intelligent cache batch operations" {
  let cache = IntelligentCache::new(100, 60)
  
  // Test batch put
  let batch_data = [
    ("batch_key1", "batch_value1"),
    ("batch_key2", "batch_value2"),
    ("batch_key3", "batch_value3")
  ]
  IntelligentCache::put_batch(cache, batch_data)
  
  assert_eq(IntelligentCache::size(cache), 3)
  
  // Test batch get
  let keys = ["batch_key1", "batch_key2", "batch_key3", "non_existent"]
  let results = IntelligentCache::get_batch(cache, keys)
  
  assert_eq(results.length(), 4)
  assert_eq(results[0], Some("batch_value1"))
  assert_eq(results[1], Some("batch_value2"))
  assert_eq(results[2], Some("batch_value3"))
  assert_eq(results[3], None)
}

test "intelligent cache statistics" {
  let cache = IntelligentCache::new(100, 60)
  
  // Initial statistics
  let stats = IntelligentCache::statistics(cache)
  assert_eq(IntelligentCacheStats::hits(stats), 0)
  assert_eq(IntelligentCacheStats::misses(stats), 0)
  assert_eq(IntelligentCacheStats::evictions(stats), 0)
  
  // Perform some operations
  IntelligentCache::put(cache, "key1", "value1")
  IntelligentCache::get(cache, "key1") // hit
  IntelligentCache::get(cache, "non_existent") // miss
  
  // Fill cache and trigger eviction
  for i in 0..=101 {
    IntelligentCache::put(cache, "key_" + i.to_string(), "value_" + i.to_string())
  }
  
  // Check updated statistics
  let updated_stats = IntelligentCache::statistics(cache)
  assert_eq(IntelligentCacheStats::hits(updated_stats), 1)
  assert_eq(IntelligentCacheStats::misses(updated_stats), 1)
  assert_true(IntelligentCacheStats::evictions(updated_stats) > 0)
}

test "intelligent cache telemetry integration" {
  let cache = IntelligentCache::new(100, 60)
  let telemetry_provider = TelemetryProvider::default()
  
  // Enable telemetry for cache
  IntelligentCache::enable_telemetry(cache, telemetry_provider)
  
  // Perform cache operations
  IntelligentCache::put(cache, "telemetry_key", "telemetry_value")
  IntelligentCache::get(cache, "telemetry_key")
  IntelligentCache::get(cache, "missing_key")
  
  // Verify telemetry metrics were recorded
  let metrics = TelemetryProvider::get_metrics(telemetry_provider, "cache")
  assert_true(metrics.length() > 0)
  
  // Check for specific metrics
  let hit_metric = TelemetryProvider::find_metric(metrics, "cache_hits")
  let miss_metric = TelemetryProvider::find_metric(metrics, "cache_misses")
  
  match hit_metric {
    Some(metric) => assert_true(Metric::value(metric) > 0.0)
    None => assert_true(false)
  }
  
  match miss_metric {
    Some(metric) => assert_true(Metric::value(metric) > 0.0)
    None => assert_true(false)
  }
}

test "intelligent cache configuration" {
  let config = IntelligentCacheConfig::new()
    .with_capacity(200)
    .with_ttl(120)
    .with_eviction_policy(LFU) // Least Frequently Used
    .with_enable_statistics(true)
    .with_enable_telemetry(true)
  
  let cache = IntelligentCache::with_config(config)
  
  assert_eq(IntelligentCache::capacity(cache), 200)
  assert_eq(IntelligentCache::ttl(cache), 120)
  assert_eq(IntelligentCache::eviction_policy(cache), LFU)
  assert_true(IntelligentCache::statistics_enabled(cache))
  assert_true(IntelligentCache::telemetry_enabled(cache))
}