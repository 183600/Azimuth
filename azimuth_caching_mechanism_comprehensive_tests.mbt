// Azimuth Telemetry System - Caching Mechanism Tests
// This file contains test cases for caching functionality

// Test 1: Basic Cache Operations
test "basic cache operations" {
  let cache = Cache::new(100) // Cache with capacity of 100 items
  
  // Test cache put and get
  Cache::put(cache, "key1", "value1")
  let result1 = Cache::get(cache, "key1")
  match result1 {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // Test cache put with different types
  Cache::put(cache, "key2", 42)
  let result2 = Cache::get(cache, "key2")
  match result2 {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  Cache::put(cache, "key3", 3.14)
  let result3 = Cache::get(cache, "key3")
  match result3 {
    Some(value) => assert_eq(value, 3.14)
    None => assert_true(false)
  }
  
  // Test cache get for non-existent key
  let result4 = Cache::get(cache, "non_existent_key")
  match result4 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test cache contains
  assert_true(Cache::contains(cache, "key1"))
  assert_true(Cache::contains(cache, "key2"))
  assert_false(Cache::contains(cache, "non_existent_key"))
  
  // Test cache size
  assert_eq(Cache::size(cache), 3)
  
  // Test cache remove
  Cache::remove(cache, "key2")
  assert_false(Cache::contains(cache, "key2"))
  assert_eq(Cache::size(cache), 2)
  
  let result5 = Cache::get(cache, "key2")
  match result5 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test cache clear
  Cache::clear(cache)
  assert_eq(Cache::size(cache), 0)
  assert_false(Cache::contains(cache, "key1"))
  assert_false(Cache::contains(cache, "key3"))
}

// Test 2: Cache Eviction Policies
test "cache eviction policies" {
  // Test LRU (Least Recently Used) eviction
  let lru_cache = Cache::new_with_eviction(3, LRUEviction)
  
  // Fill cache to capacity
  Cache::put(lru_cache, "key1", "value1")
  Cache::put(lru_cache, "key2", "value2")
  Cache::put(lru_cache, "key3", "value3")
  
  assert_eq(Cache::size(lru_cache), 3)
  assert_true(Cache::contains(lru_cache, "key1"))
  assert_true(Cache::contains(lru_cache, "key2"))
  assert_true(Cache::contains(lru_cache, "key3"))
  
  // Access key1 to make it recently used
  Cache::get(lru_cache, "key1") |> ignore
  
  // Add a new item, should evict key2 (least recently used)
  Cache::put(lru_cache, "key4", "value4")
  
  assert_eq(Cache::size(lru_cache), 3)
  assert_true(Cache::contains(lru_cache, "key1")) // Recently accessed
  assert_false(Cache::contains(lru_cache, "key2")) // Evicted
  assert_true(Cache::contains(lru_cache, "key3"))
  assert_true(Cache::contains(lru_cache, "key4"))
  
  // Test LFU (Least Frequently Used) eviction
  let lfu_cache = Cache::new_with_eviction(3, LFUEviction)
  
  // Fill cache to capacity
  Cache::put(lfu_cache, "key1", "value1")
  Cache::put(lfu_cache, "key2", "value2")
  Cache::put(lfu_cache, "key3", "value3")
  
  // Access key1 multiple times to increase its frequency
  for _ in 0..=4 {
    Cache::get(lfu_cache, "key1") |> ignore
  }
  
  // Access key2 once
  Cache::get(lfu_cache, "key2") |> ignore
  
  // Add a new item, should evict key3 (least frequently used)
  Cache::put(lfu_cache, "key4", "value4")
  
  assert_eq(Cache::size(lfu_cache), 3)
  assert_true(Cache::contains(lfu_cache, "key1")) // Most frequently used
  assert_true(Cache::contains(lfu_cache, "key2"))
  assert_false(Cache::contains(lfu_cache, "key3")) // Evicted
  assert_true(Cache::contains(lfu_cache, "key4"))
  
  // Test FIFO (First In First Out) eviction
  let fifo_cache = Cache::new_with_eviction(3, FIFOEviction)
  
  // Fill cache to capacity
  Cache::put(fifo_cache, "key1", "value1")
  Cache::put(fifo_cache, "key2", "value2")
  Cache::put(fifo_cache, "key3", "value3")
  
  // Add a new item, should evict key1 (first in)
  Cache::put(fifo_cache, "key4", "value4")
  
  assert_eq(Cache::size(fifo_cache), 3)
  assert_false(Cache::contains(fifo_cache, "key1")) // Evicted
  assert_true(Cache::contains(fifo_cache, "key2"))
  assert_true(Cache::contains(fifo_cache, "key3"))
  assert_true(Cache::contains(fifo_cache, "key4"))
}

// Test 3: Cache Expiration
test "cache expiration" {
  // Test time-based expiration
  let expiring_cache = Cache::new_with_expiration(100, Duration::seconds(2))
  
  // Add items to cache
  Cache::put(expiring_cache, "key1", "value1")
  Cache::put(expiring_cache, "key2", "value2")
  
  // Items should be available immediately
  assert_true(Cache::contains(expiring_cache, "key1"))
  assert_true(Cache::contains(expiring_cache, "key2"))
  
  // Wait for expiration
  Thread::sleep(Duration::seconds(3))
  
  // Items should be expired
  assert_false(Cache::contains(expiring_cache, "key1"))
  assert_false(Cache::contains(expiring_cache, "key2"))
  
  // Test TTL (Time To Live) per item
  let ttl_cache = Cache::new(100)
  
  // Add items with different TTLs
  Cache::put_with_ttl(ttl_cache, "short_lived", "value1", Duration::seconds(1))
  Cache::put_with_ttl(ttl_cache, "long_lived", "value2", Duration::seconds(5))
  
  // Items should be available immediately
  assert_true(Cache::contains(ttl_cache, "short_lived"))
  assert_true(Cache::contains(ttl_cache, "long_lived"))
  
  // Wait for short-lived item to expire
  Thread::sleep(Duration::seconds(2))
  
  // Short-lived item should be expired, long-lived should still be available
  assert_false(Cache::contains(ttl_cache, "short_lived"))
  assert_true(Cache::contains(ttl_cache, "long_lived"))
  
  // Wait for long-lived item to expire
  Thread::sleep(Duration::seconds(4))
  
  // Long-lived item should also be expired
  assert_false(Cache::contains(ttl_cache, "long_lived"))
}

// Test 4: Cache Statistics
test "cache statistics" {
  let cache = Cache::new(100)
  
  // Initial statistics
  let stats = Cache::get_stats(cache)
  assert_eq(CacheStats::hits(stats), 0)
  assert_eq(CacheStats::misses(stats), 0)
  assert_eq(CacheStats::puts(stats), 0)
  assert_eq(CacheStats::evictions(stats), 0)
  assert_eq(CacheStats::size(stats), 0)
  
  // Add items to cache
  Cache::put(cache, "key1", "value1")
  Cache::put(cache, "key2", "value2")
  
  // Statistics after puts
  stats = Cache::get_stats(cache)
  assert_eq(CacheStats::puts(stats), 2)
  assert_eq(CacheStats::size(stats), 2)
  
  // Cache hits
  Cache::get(cache, "key1") |> ignore
  Cache::get(cache, "key2") |> ignore
  Cache::get(cache, "key1") |> ignore // Access key1 again
  
  // Statistics after hits
  stats = Cache::get_stats(cache)
  assert_eq(CacheStats::hits(stats), 3)
  assert_eq(CacheStats::misses(stats), 0)
  
  // Cache misses
  Cache::get(cache, "non_existent1") |> ignore
  Cache::get(cache, "non_existent2") |> ignore
  
  // Statistics after misses
  stats = Cache::get_stats(cache)
  assert_eq(CacheStats::hits(stats), 3)
  assert_eq(CacheStats::misses(stats), 2)
  
  // Calculate hit rate
  let hit_rate = CacheStats::hit_rate(stats)
  assert_eq(hit_rate, 0.6) // 3 hits out of 5 total requests
  
  // Test cache with eviction to track eviction statistics
  let small_cache = Cache::new(2)
  
  Cache::put(small_cache, "key1", "value1")
  Cache::put(small_cache, "key2", "value2")
  Cache::put(small_cache, "key3", "value3") // Should evict one item
  
  let eviction_stats = Cache::get_stats(small_cache)
  assert_eq(CacheStats::evictions(eviction_stats), 1)
  assert_eq(CacheStats::size(eviction_stats), 2)
}

// Test 5: Cache Persistence
test "cache persistence" {
  let cache = Cache::new(100)
  
  // Add items to cache
  Cache::put(cache, "key1", "value1")
  Cache::put(cache, "key2", "value2")
  Cache::put(cache, "key3", "value3")
  
  // Save cache to disk
  let save_result = Cache::save_to_disk(cache, "/tmp/test_cache.dat")
  assert_true(save_result.is_ok())
  
  // Create a new cache and load from disk
  let loaded_cache = Cache::new(100)
  let load_result = Cache::load_from_disk(loaded_cache, "/tmp/test_cache.dat")
  assert_true(load_result.is_ok())
  
  // Verify loaded cache contains all items
  assert_true(Cache::contains(loaded_cache, "key1"))
  assert_true(Cache::contains(loaded_cache, "key2"))
  assert_true(Cache::contains(loaded_cache, "key3"))
  
  let result1 = Cache::get(loaded_cache, "key1")
  match result1 {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  let result2 = Cache::get(loaded_cache, "key2")
  match result2 {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  let result3 = Cache::get(loaded_cache, "key3")
  match result3 {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  // Clean up test file
  File::delete("/tmp/test_cache.dat") |> ignore
}

// Test 6: Cache with Telemetry Data
test "cache with telemetry data" {
  let telemetry_cache = Cache::new(100)
  
  // Create telemetry data
  let telemetry_data1 = TelemetryData::with_attributes(
    "cpu_usage",
    75.5,
    Attributes::from_list([
      ("host", StringValue("server-01")),
      ("region", StringValue("us-west-1"))
    ])
  )
  
  let telemetry_data2 = TelemetryData::with_attributes(
    "memory_usage",
    60.2,
    Attributes::from_list([
      ("host", StringValue("server-02")),
      ("region", StringValue("us-east-1"))
    ])
  )
  
  // Cache telemetry data
  Cache::put(telemetry_cache, "server-01:cpu", telemetry_data1)
  Cache::put(telemetry_cache, "server-02:memory", telemetry_data2)
  
  // Retrieve telemetry data from cache
  let cached_data1 = Cache::get(telemetry_cache, "server-01:cpu")
  match cached_data1 {
    Some(TelemetryData(data)) => {
      assert_eq(TelemetryData::metric_name(data), "cpu_usage")
      assert_eq(TelemetryData::value(data), 75.5)
    }
    _ => assert_true(false)
  }
  
  let cached_data2 = Cache::get(telemetry_cache, "server-02:memory")
  match cached_data2 {
    Some(TelemetryData(data)) => {
      assert_eq(TelemetryData::metric_name(data), "memory_usage")
      assert_eq(TelemetryData::value(data), 60.2)
    }
    _ => assert_true(false)
  }
  
  // Test cache with time series data
  let time_series_cache = Cache::new(50)
  
  // Create time series
  let time_series = TimeSeries::new("cpu_usage_series")
  for i in 0..=9 {
    let timestamp = Time::now() + (i as Int64)
    let value = 50.0 + (i as Float) * 2.5
    TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Cache time series
  Cache::put(time_series_cache, "cpu_usage_series", time_series)
  
  // Retrieve time series from cache
  let cached_series = Cache::get(time_series_cache, "cpu_usage_series")
  match cached_series {
    Some(TimeSeries(series)) => {
      assert_eq(TimeSeries::name(series), "cpu_usage_series")
      assert_eq(TimeSeries::data_points(series).length(), 10)
    }
    _ => assert_true(false)
  }
}

// Test 7: Distributed Cache
test "distributed cache" {
  // Create a distributed cache cluster
  let cluster = DistributedCacheCluster::new(3) // 3 nodes
  
  // Get a reference to the distributed cache
  let distributed_cache = DistributedCacheCluster::get_cache(cluster)
  
  // Put items in distributed cache
  Cache::put(distributed_cache, "key1", "value1")
  Cache::put(distributed_cache, "key2", "value2")
  Cache::put(distributed_cache, "key3", "value3")
  
  // Get items from distributed cache
  let result1 = Cache::get(distributed_cache, "key1")
  match result1 {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  let result2 = Cache::get(distributed_cache, "key2")
  match result2 {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  let result3 = Cache::get(distributed_cache, "key3")
  match result3 {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  // Test cache consistency across nodes
  let node1_cache = DistributedCacheCluster::get_node_cache(cluster, 0)
  let node2_cache = DistributedCacheCluster::get_node_cache(cluster, 1)
  let node3_cache = DistributedCacheCluster::get_node_cache(cluster, 2)
  
  // Add a new item
  Cache::put(distributed_cache, "key4", "value4")
  
  // Verify item is available on all nodes (after replication)
  Thread::sleep(Duration::milliseconds(100)) // Allow replication to complete
  
  let node1_result = Cache::get(node1_cache, "key4")
  let node2_result = Cache::get(node2_cache, "key4")
  let node3_result = Cache::get(node3_cache, "key4")
  
  // At least one node should have the item (depending on replication strategy)
  let mut found_on_nodes = 0
  if node1_result.is_some() { found_on_nodes = found_on_nodes + 1 }
  if node2_result.is_some() { found_on_nodes = found_on_nodes + 1 }
  if node3_result.is_some() { found_on_nodes = found_on_nodes + 1 }
  
  assert_true(found_on_nodes >= 1)
}

// Test 8: Cache with Custom Loader
test "cache with custom loader" {
  // Define a custom loader function
  let loader = fn(key) {
    if key == "expensive_key1" {
      Thread::sleep(Duration::milliseconds(100)) // Simulate expensive operation
      Some("expensive_value1")
    } else if key == "expensive_key2" {
      Thread::sleep(Duration::milliseconds(100)) // Simulate expensive operation
      Some("expensive_value2")
    } else {
      None
    }
  }
  
  // Create cache with custom loader
  let loading_cache = Cache::new_with_loader(100, loader)
  
  // First access should trigger loader
  let start_time = Time::now()
  let result1 = Cache::get_or_load(loading_cache, "expensive_key1")
  let load_time1 = Time::now() - start_time
  
  match result1 {
    Some(value) => assert_eq(value, "expensive_value1")
    None => assert_true(false)
  }
  
  // Loading should take time
  assert_true(load_time1 >= 100) // At least 100ms
  
  // Second access should be from cache (faster)
  start_time = Time::now()
  let result2 = Cache::get_or_load(loading_cache, "expensive_key1")
  let cache_time = Time::now() - start_time
  
  match result2 {
    Some(value) => assert_eq(value, "expensive_value1")
    None => assert_true(false)
  }
  
  // Cache access should be much faster
  assert_true(cache_time < 10) // Less than 10ms
  
  // Test with non-existent key
  let result3 = Cache::get_or_load(loading_cache, "non_existent_key")
  match result3 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test with another expensive key
  let result4 = Cache::get_or_load(loading_cache, "expensive_key2")
  match result4 {
    Some(value) => assert_eq(value, "expensive_value2")
    None => assert_true(false)
  }
}

// Test 9: Cache with Callbacks
test "cache with callbacks" {
  // Define callbacks
  let mut put_count = 0
  let mut get_count = 0
  let mut evict_count = 0
  
  let on_put = fn(key, value) {
    put_count = put_count + 1
  }
  
  let on_get = fn(key, value) {
    get_count = get_count + 1
  }
  
  let on_evict = fn(key, value) {
    evict_count = evict_count + 1
  }
  
  // Create cache with callbacks
  let callback_cache = Cache::new_with_callbacks(2, on_put, on_get, on_evict)
  
  // Put items
  Cache::put(callback_cache, "key1", "value1")
  assert_eq(put_count, 1)
  
  Cache::put(callback_cache, "key2", "value2")
  assert_eq(put_count, 2)
  
  // Get items
  Cache::get(callback_cache, "key1") |> ignore
  assert_eq(get_count, 1)
  
  Cache::get(callback_cache, "key2") |> ignore
  assert_eq(get_count, 2)
  
  // Add item that causes eviction
  Cache::put(callback_cache, "key3", "value3")
  assert_eq(put_count, 3)
  assert_eq(evict_count, 1) // One item should be evicted
  
  // Get non-existent item
  Cache::get(callback_cache, "non_existent") |> ignore
  assert_eq(get_count, 3) // Should still trigger callback even for misses
}

// Test 10: Cache Performance
test "cache performance" {
  let cache = Cache::new(1000)
  let num_operations = 10000
  
  // Benchmark cache puts
  let start_time = Time::now()
  for i in 0..=(num_operations - 1) {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string()
    Cache::put(cache, key, value)
  }
  let put_time = Time::now() - start_time
  
  // Cache puts should be fast
  assert_true(put_time < 1000) // Less than 1 second for 10000 operations
  
  // Benchmark cache gets
  start_time = Time::now()
  for i in 0..=(num_operations - 1) {
    let key = "key_" + i.to_string()
    Cache::get(cache, key) |> ignore
  }
  let get_time = Time::now() - start_time
  
  // Cache gets should be very fast
  assert_true(get_time < 500) // Less than 0.5 seconds for 10000 operations
  
  // Benchmark cache hits vs misses
  let hit_cache = Cache::new(100)
  
  // Fill cache
  for i in 0..=99 {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string()
    Cache::put(hit_cache, key, value)
  }
  
  // Benchmark hits (50% hits, 50% misses)
  start_time = Time::now()
  for i in 0..=(num_operations - 1) {
    let key = if i % 2 == 0 {
      "key_" + (i % 100).to_string() // Existing key (hit)
    } else {
      "non_existent_" + i.to_string() // Non-existent key (miss)
    }
    Cache::get(hit_cache, key) |> ignore
  }
  let mixed_time = Time::now() - start_time
  
  // Mixed operations should still be fast
  assert_true(mixed_time < 1000) // Less than 1 second for 10000 operations
  
  // Check cache statistics
  let stats = Cache::get_stats(hit_cache)
  assert_eq(CacheStats::hits(stats), num_operations / 2)
  assert_eq(CacheStats::misses(stats), num_operations / 2)
  assert_eq(CacheStats::hit_rate(stats), 0.5)
}