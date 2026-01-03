// Azimuth Telemetry System - Caching Mechanism Tests
// This file contains test cases for caching functionality

// Test 1: Basic In-Memory Cache Operations
test "basic in-memory cache operations" {
  // Create in-memory cache
  let cache = InMemoryCache::new(100)  // Max 100 items
  
  // Test put and get operations
  Cache::put(cache, "key1", "value1")
  Cache::put(cache, "key2", "value2")
  Cache::put(cache, "key3", "value3")
  
  // Verify values can be retrieved
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
  
  // Test non-existent key
  match Cache::get(cache, "non_existent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test contains operation
  assert_true(Cache::contains(cache, "key1"))
  assert_true(Cache::contains(cache, "key2"))
  assert_false(Cache::contains(cache, "non_existent"))
  
  // Test remove operation
  Cache::remove(cache, "key2")
  match Cache::get(cache, "key2") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  assert_false(Cache::contains(cache, "key2"))
  
  // Test size
  assert_eq(Cache::size(cache), 2)
  
  // Test clear operation
  Cache::clear(cache)
  assert_eq(Cache::size(cache), 0)
  match Cache::get(cache, "key1") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Cache Expiration and TTL
test "cache expiration and ttl" {
  // Create cache with TTL support
  let cache = TtlCache::new(100, 2000)  // 100 items, 2 seconds TTL
  
  // Put items with different TTLs
  Cache::put_with_ttl(cache, "short_lived", "value1", 500)   // 0.5 seconds
  Cache::put_with_ttl(cache, "medium_lived", "value2", 1500) // 1.5 seconds
  Cache::put(cache, "default_lived", "value3")  // Uses default 2 seconds
  
  // Verify all items are initially available
  assert_true(Cache::contains(cache, "short_lived"))
  assert_true(Cache::contains(cache, "medium_lived"))
  assert_true(Cache::contains(cache, "default_lived"))
  
  // Wait for short-lived item to expire
  @sleep(600)
  
  // Verify short-lived item has expired
  assert_false(Cache::contains(cache, "short_lived"))
  match Cache::get(cache, "short_lived") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Verify other items are still available
  assert_true(Cache::contains(cache, "medium_lived"))
  assert_true(Cache::contains(cache, "default_lived"))
  
  // Wait for medium-lived item to expire
  @sleep(1000)
  
  // Verify medium-lived item has expired
  assert_false(Cache::contains(cache, "medium_lived"))
  
  // Verify default-lived item is still available
  assert_true(Cache::contains(cache, "default_lived"))
  
  // Wait for default-lived item to expire
  @sleep(500)
  
  // Verify all items have expired
  assert_false(Cache::contains(cache, "default_lived"))
  assert_eq(Cache::size(cache), 0)
  
  // Test manual expiration
  Cache::put(cache, "manual_expire", "value4")
  assert_true(Cache::contains(cache, "manual_expire"))
  
  Cache::expire(cache, "manual_expire")
  assert_false(Cache::contains(cache, "manual_expire"))
}

// Test 3: LRU Cache Eviction
test "lru cache eviction" {
  // Create LRU cache with small capacity
  let lru_cache = LruCache::new(3)  // Max 3 items
  
  // Fill cache to capacity
  Cache::put(lru_cache, "key1", "value1")
  Cache::put(lru_cache, "key2", "value2")
  Cache::put(lru_cache, "key3", "value3")
  
  // Verify all items are in cache
  assert_eq(Cache::size(lru_cache), 3)
  assert_true(Cache::contains(lru_cache, "key1"))
  assert_true(Cache::contains(lru_cache, "key2"))
  assert_true(Cache::contains(lru_cache, "key3"))
  
  // Access key1 to make it most recently used
  match Cache::get(lru_cache, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // Add new item, should evict key2 (least recently used)
  Cache::put(lru_cache, "key4", "value4")
  
  // Verify eviction
  assert_eq(Cache::size(lru_cache), 3)
  assert_true(Cache::contains(lru_cache, "key1"))  // Recently accessed
  assert_false(Cache::contains(lru_cache, "key2"))  // Evicted
  assert_true(Cache::contains(lru_cache, "key3"))
  assert_true(Cache::contains(lru_cache, "key4"))
  
  // Access key3 and key4
  match Cache::get(lru_cache, "key3") {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  match Cache::get(lru_cache, "key4") {
    Some(value) => assert_eq(value, "value4")
    None => assert_true(false)
  }
  
  // Add new item, should evict key1
  Cache::put(lru_cache, "key5", "value5")
  
  // Verify eviction
  assert_false(Cache::contains(lru_cache, "key1"))  // Evicted
  assert_true(Cache::contains(lru_cache, "key3"))
  assert_true(Cache::contains(lru_cache, "key4"))
  assert_true(Cache::contains(lru_cache, "key5"))
  
  // Test LRU order
  let lru_order = LruCache::get_order(lru_cache)
  assert_eq(lru_order[0], "key3")  // Least recently used
  assert_eq(lru_order[1], "key4")
  assert_eq(lru_order[2], "key5")  // Most recently used
}

// Test 4: Cache Statistics and Metrics
test "cache statistics and metrics" {
  // Create cache with statistics tracking
  let cache = StatisticsCache::new(100)
  
  // Perform cache operations
  Cache::put(cache, "key1", "value1")
  Cache::put(cache, "key2", "value2")
  
  // Test hits
  match Cache::get(cache, "key1") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  match Cache::get(cache, "key1") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  // Test misses
  match Cache::get(cache, "non_existent1") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  match Cache::get(cache, "non_existent2") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test puts
  Cache::put(cache, "key3", "value3")
  Cache::put(cache, "key4", "value4")
  
  // Test removals
  Cache::remove(cache, "key2")
  
  // Get statistics
  let stats = StatisticsCache::get_statistics(cache)
  
  // Verify statistics
  assert_eq(CacheStatistics::hits(stats), 2)
  assert_eq(CacheStatistics::misses(stats), 2)
  assert_eq(CacheStatistics::puts(stats), 4)
  assert_eq(CacheStatistics::removals(stats), 1)
  assert_eq(CacheStatistics::current_size(stats), 3)
  
  // Verify hit ratio
  let hit_ratio = CacheStatistics::hit_ratio(stats)
  assert_eq(hit_ratio, 2.0 / (2.0 + 2.0))  // 2 hits / (2 hits + 2 misses)
  
  // Verify miss ratio
  let miss_ratio = CacheStatistics::miss_ratio(stats)
  assert_eq(miss_ratio, 2.0 / (2.0 + 2.0))  // 2 misses / (2 hits + 2 misses)
  
  // Reset statistics
  StatisticsCache::reset_statistics(cache)
  
  let reset_stats = StatisticsCache::get_statistics(cache)
  assert_eq(CacheStatistics::hits(reset_stats), 0)
  assert_eq(CacheStatistics::misses(reset_stats), 0)
  assert_eq(CacheStatistics::puts(reset_stats), 0)
  assert_eq(CacheStatistics::removals(reset_stats), 0)
  assert_eq(CacheStatistics::current_size(reset_stats), 3)  // Size is not reset
}

// Test 5: Distributed Cache Operations
test "distributed cache operations" {
  // Create distributed cache nodes
  let node1 = DistributedCacheNode::new("node1", "localhost:8001")
  let node2 = DistributedCacheNode::new("node2", "localhost:8002")
  let node3 = DistributedCacheNode::new("node3", "localhost:8003")
  
  // Create distributed cache cluster
  let cluster = DistributedCacheCluster::new()
  DistributedCacheCluster::add_node(cluster, node1)
  DistributedCacheCluster::add_node(cluster, node2)
  DistributedCacheCluster::add_node(cluster, node3)
  
  // Configure consistent hashing for key distribution
  DistributedCacheCluster::configure_hashing(cluster, HashingAlgorithm::Consistent)
  
  // Test put operations
  DistributedCacheCluster::put(cluster, "user:123", "user_data_123")
  DistributedCacheCluster::put(cluster, "user:456", "user_data_456")
  DistributedCacheCluster::put(cluster, "product:789", "product_data_789")
  
  // Test get operations
  match DistributedCacheCluster::get(cluster, "user:123") {
    Some(value) => assert_eq(value, "user_data_123")
    None => assert_true(false)
  }
  
  match DistributedCacheCluster::get(cluster, "user:456") {
    Some(value) => assert_eq(value, "user_data_456")
    None => assert_true(false)
  }
  
  match DistributedCacheCluster::get(cluster, "product:789") {
    Some(value) => assert_eq(value, "product_data_789")
    None => assert_true(false)
  }
  
  // Test key distribution
  let node_for_user123 = DistributedCacheCluster::get_node_for_key(cluster, "user:123")
  let node_for_user456 = DistributedCacheCluster::get_node_for_key(cluster, "user:456")
  let node_for_product789 = DistributedCacheCluster::get_node_for_key(cluster, "product:789")
  
  // Keys should be distributed across nodes (might be on same node depending on hash)
  assert_true(
    node_for_user123 == "node1" ||
    node_for_user123 == "node2" ||
    node_for_user123 == "node3"
  )
  
  // Test node failure simulation
  DistributedCacheCluster::simulate_node_failure(cluster, "node2")
  
  // Operations should still work with remaining nodes
  DistributedCacheCluster::put(cluster, "order:111", "order_data_111")
  
  match DistributedCacheCluster::get(cluster, "order:111") {
    Some(value) => assert_eq(value, "order_data_111")
    None => assert_true(false)
  }
  
  // Test node recovery
  DistributedCacheCluster::recover_node(cluster, "node2")
  
  // Verify cluster statistics
  let cluster_stats = DistributedCacheCluster::get_statistics(cluster)
  assert_eq(ClusterStatistics::active_nodes(cluster_stats), 3)
  assert_eq(ClusterStatistics::total_keys(cluster_stats), 4)
  assert_true(ClusterStatistics::total_operations(cluster_stats) > 0)
}

// Test 6: Cache Hierarchies and Multi-Level Caching
test "cache hierarchies and multi-level caching" {
  // Create multi-level cache hierarchy
  let l1_cache = LruCache::new(10)    // L1: Small, fast cache
  let l2_cache = LruCache::new(100)   // L2: Medium cache
  let l3_cache = LruCache::new(1000)  // L3: Large, slower cache
  
  // Create cache hierarchy
  let hierarchy = CacheHierarchy::new()
  CacheHierarchy::add_level(hierarchy, l1_cache, 1)  // Level 1
  CacheHierarchy::add_level(hierarchy, l2_cache, 2)  // Level 2
  CacheHierarchy::add_level(hierarchy, l3_cache, 3)  // Level 3
  
  // Configure fallback data source
  CacheHierarchy::configure_data_source(hierarchy, || {
    // Simulate database fetch
    @sleep(10)  // Simulate latency
    "database_value"
  })
  
  // Test cache miss scenario (data not in any cache)
  let start_time = @time()
  match CacheHierarchy::get(hierarchy, "key1") {
    Some(value) => {
      assert_eq(value, "database_value")
      let end_time = @time()
      let duration = end_time - start_time
      
      // Should take longer due to database fetch
      assert_true(duration >= 10)
    }
    None => assert_true(false)
  }
  
  // Test L1 cache hit
  let start_time = @time()
  match CacheHierarchy::get(hierarchy, "key1") {
    Some(value) => {
      assert_eq(value, "database_value")
      let end_time = @time()
      let duration = end_time - start_time
      
      // Should be faster (from L1 cache)
      assert_true(duration < 5)
    }
    None => assert_true(false)
  }
  
  // Fill L1 cache to capacity
  for i in 0..=9 {
    CacheHierarchy::put(hierarchy, "l1_key" + i.to_string(), "l1_value" + i.to_string())
  }
  
  // Add more data to push some items to L2
  for i in 10..=19 {
    CacheHierarchy::put(hierarchy, "l2_key" + i.to_string(), "l2_value" + i.to_string())
  }
  
  // Verify L1 eviction
  assert_false(Cache::contains(l1_cache, "l1_key0"))
  assert_true(Cache::contains(l2_cache, "l2_key15"))
  
  // Test L2 cache hit
  match CacheHierarchy::get(hierarchy, "l2_key15") {
    Some(value) => assert_eq(value, "l2_value15")
    None => assert_true(false)
  }
  
  // Get hierarchy statistics
  let hierarchy_stats = CacheHierarchy::get_statistics(hierarchy)
  
  assert_eq(HierarchyStatistics::l1_hits(hierarchy_stats), 1)
  assert_eq(HierarchyStatistics::l2_hits(hierarchy_stats), 1)
  assert_eq(HierarchyStatistics::l3_hits(hierarchy_stats), 0)
  assert_eq(HierarchyStatistics::database_hits(hierarchy_stats), 1)
  
  // Verify hit ratios
  let l1_hit_ratio = HierarchyStatistics::l1_hit_ratio(hierarchy_stats)
  let overall_hit_ratio = HierarchyStatistics::overall_hit_ratio(hierarchy_stats)
  
  assert_true(l1_hit_ratio >= 0.0)
  assert_true(overall_hit_ratio >= 0.0)
}

// Test 7: Cache Invalidation Strategies
test "cache invalidation strategies" {
  // Create cache with invalidation support
  let cache = InvalidationCache::new(100)
  
  // Add data with tags
  InvalidationCache::put_with_tags(cache, "user:123", "user_data_123", ["user", "profile"])
  InvalidationCache::put_with_tags(cache, "user:456", "user_data_456", ["user", "profile"])
  InvalidationCache::put_with_tags(cache, "product:789", "product_data_789", ["product", "catalog"])
  InvalidationCache::put_with_tags(cache, "product:101", "product_data_101", ["product", "catalog"])
  
  // Verify all data is present
  assert_true(Cache::contains(cache, "user:123"))
  assert_true(Cache::contains(cache, "user:456"))
  assert_true(Cache::contains(cache, "product:789"))
  assert_true(Cache::contains(cache, "product:101"))
  
  // Invalidate by tag
  InvalidationCache::invalidate_by_tag(cache, "user")
  
  // Verify user data is invalidated
  assert_false(Cache::contains(cache, "user:123"))
  assert_false(Cache::contains(cache, "user:456"))
  
  // Verify product data is still present
  assert_true(Cache::contains(cache, "product:789"))
  assert_true(Cache::contains(cache, "product:101"))
  
  // Invalidate by pattern
  InvalidationCache::invalidate_by_pattern(cache, "product:*")
  
  // Verify all product data is invalidated
  assert_false(Cache::contains(cache, "product:789"))
  assert_false(Cache::contains(cache, "product:101"))
  
  // Add more data for time-based invalidation
  InvalidationCache::put_with_ttl(cache, "temp_data", "temp_value", 1000)  // 1 second
  
  assert_true(Cache::contains(cache, "temp_data"))
  
  // Wait for expiration
  @sleep(1100)
  
  // Verify time-based invalidation
  assert_false(Cache::contains(cache, "temp_data"))
  
  // Test invalidation events
  let mut invalidated_keys = []
  InvalidationCache::set_invalidation_callback(cache, |key| {
    invalidated_keys.push(key)
  })
  
  InvalidationCache::put(cache, "event_test", "event_value")
  InvalidationCache::invalidate_by_pattern(cache, "event_*")
  
  assert_eq(invalidated_keys.length(), 1)
  assert_eq(invalidated_keys[0], "event_test")
}

// Test 8: Cache Persistence and Recovery
test "cache persistence and recovery" {
  // Create persistent cache
  let persistent_cache = PersistentCache::new("/tmp/cache_data", 100)
  
  // Add data to cache
  PersistentCache::put(persistent_cache, "persistent_key1", "persistent_value1")
  PersistentCache::put(persistent_cache, "persistent_key2", "persistent_value2")
  PersistentCache::put(persistent_cache, "persistent_key3", "persistent_value3")
  
  // Verify data is in cache
  assert_true(Cache::contains(persistent_cache, "persistent_key1"))
  assert_true(Cache::contains(persistent_cache, "persistent_key2"))
  assert_true(Cache::contains(persistent_cache, "persistent_key3"))
  
  // Force persistence to disk
  PersistentCache::persist(persistent_cache)
  
  // Create new cache instance and recover from disk
  let recovered_cache = PersistentCache::new("/tmp/cache_data", 100)
  PersistentCache::recover(recovered_cache)
  
  // Verify data is recovered
  match Cache::get(recovered_cache, "persistent_key1") {
    Some(value) => assert_eq(value, "persistent_value1")
    None => assert_true(false)
  }
  
  match Cache::get(recovered_cache, "persistent_key2") {
    Some(value) => assert_eq(value, "persistent_value2")
    None => assert_true(false)
  }
  
  match Cache::get(recovered_cache, "persistent_key3") {
    Some(value) => assert_eq(value, "persistent_value3")
    None => assert_true(false)
  }
  
  // Test persistence with TTL
  PersistentCache::put_with_ttl(recovered_cache, "ttl_key", "ttl_value", 5000)  // 5 seconds
  PersistentCache::persist(recovered_cache)
  
  // Create another cache instance
  let ttl_cache = PersistentCache::new("/tmp/cache_data", 100)
  PersistentCache::recover(ttl_cache)
  
  // Verify TTL data is recovered
  match Cache::get(ttl_cache, "ttl_key") {
    Some(value) => assert_eq(value, "ttl_value")
    None => assert_true(false)
  }
  
  // Test persistence statistics
  let persist_stats = PersistentCache::get_persistence_stats(ttl_cache)
  assert_eq(PersistenceStats::keys_loaded(persist_stats), 4)
  assert_eq(PersistenceStats::load_time(persist_stats), 0)
  assert_true(PersistenceStats::file_size(persist_stats) > 0)
  
  // Clean up
  PersistentCache::cleanup(ttl_cache)
}

// Test 9: Cache Warmer and Preloading
test "cache warmer and preloading" {
  // Create cache warmer
  let cache = LruCache::new(100)
  let warmer = CacheWarmer::new(cache)
  
  // Configure data sources for warming
  CacheWarmer::add_data_source(warmer, "user_profiles", || {
    // Simulate loading user profiles
    [
      ("user:1", "profile_data_1"),
      ("user:2", "profile_data_2"),
      ("user:3", "profile_data_3"),
      ("user:4", "profile_data_4"),
      ("user:5", "profile_data_5")
    ]
  })
  
  CacheWarmer::add_data_source(warmer, "product_catalog", || {
    // Simulate loading product catalog
    [
      ("product:1", "product_data_1"),
      ("product:2", "product_data_2"),
      ("product:3", "product_data_3")
    ]
  })
  
  // Warm specific data sources
  CacheWarmer::warm(warmer, "user_profiles")
  
  // Verify user profiles are loaded
  assert_true(Cache::contains(cache, "user:1"))
  assert_true(Cache::contains(cache, "user:2"))
  assert_true(Cache::contains(cache, "user:3"))
  assert_true(Cache::contains(cache, "user:4"))
  assert_true(Cache::contains(cache, "user:5"))
  
  // Warm all data sources
  CacheWarmer::warm_all(warmer)
  
  // Verify product catalog is also loaded
  assert_true(Cache::contains(cache, "product:1"))
  assert_true(Cache::contains(cache, "product:2"))
  assert_true(Cache::contains(cache, "product:3"))
  
  // Test scheduled warming
  CacheWarmer::schedule_warming(warmer, "user_profiles", 10000)  // Every 10 seconds
  
  // Test warm-up statistics
  let warm_stats = CacheWarmer::get_statistics(warmer)
  assert_eq(WarmupStatistics::total_items_loaded(warm_stats), 8)
  assert_eq(WarmupStatistics::successful_warmups(warm_stats), 2)
  assert_eq(WarmupStatistics::failed_warmups(warm_stats), 0)
  
  // Test selective warming with priority
  CacheWarmer::warm_with_priority(warmer, [
    ("user_profiles", 1),  // High priority
    ("product_catalog", 2)  // Lower priority
  ])
  
  // Verify higher priority items are loaded first
  let load_order = CacheWarmer::get_load_order(warmer)
  assert_eq(load_order[0], "user_profiles")
  assert_eq(load_order[1], "product_catalog")
}

// Test 10: Cache Performance Optimization
test "cache performance optimization" {
  // Create performance-optimized cache
  let optimized_cache = OptimizedCache::new(1000)
  
  // Configure optimization strategies
  OptimizedCache::enable_batch_operations(optimized_cache, 50)
  OptimizedCache::enable_async_operations(optimized_cache, 4)  // 4 worker threads
  OptimizedCache::enable_compression(optimized_cache, CompressionAlgorithm::Lz4)
  OptimizedCache::enable_sharding(optimized_cache, 4)  // 4 shards
  
  // Test batch operations
  let batch_data = []
  for i in 0..=99 {
    batch_data.push(("batch_key" + i.to_string(), "batch_value" + i.to_string()))
  }
  
  let start_time = @time()
  OptimizedCache::put_batch(optimized_cache, batch_data)
  let batch_put_time = @time() - start_time
  
  // Verify batch put was faster than individual puts
  let start_time = @time()
  for i in 0..=99 {
    Cache::put(optimized_cache, "individual_key" + i.to_string(), "individual_value" + i.to_string())
  }
  let individual_put_time = @time() - start_time
  
  // Batch should be faster
  assert_true(batch_put_time < individual_put_time)
  
  // Test batch get
  let keys_to_get = []
  for i in 0..=49 {
    keys_to_get.push("batch_key" + i.to_string())
  }
  
  let start_time = @time()
  let batch_results = OptimizedCache::get_batch(optimized_cache, keys_to_get)
  let batch_get_time = @time() - start_time
  
  // Verify batch get results
  assert_eq(batch_results.length(), 50)
  for i in 0..=49 {
    match batch_results[i] {
      Some(value) => assert_eq(value, "batch_value" + i.to_string())
      None => assert_true(false)
    }
  }
  
  // Test async operations
  let async_future = OptimizedCache::get_async(optimized_cache, "batch_key25")
  let async_result = AsyncFuture::wait(async_future, 1000)  // Wait up to 1 second
  
  match async_result {
    Some(value) => assert_eq(value, "batch_value25")
    None => assert_true(false)
  }
  
  // Test compression benefits
  let large_value = "x" * 10000  // 10KB string
  OptimizedCache::put(optimized_cache, "large_key", large_value)
  
  // Verify compression statistics
  let compression_stats = OptimizedCache::get_compression_stats(optimized_cache)
  assert_true(CompressionStats::original_size(compression_stats) > CompressionStats::compressed_size(compression_stats))
  assert_true(CompressionStats::compression_ratio(compression_stats) > 0.0)
  
  // Test sharding distribution
  let shard_stats = OptimizedCache::get_shard_stats(optimized_cache)
  assert_eq(ShardStats::shard_count(shard_stats), 4)
  
  for shard_id in 0..=3 {
    let shard_size = ShardStats::get_shard_size(shard_stats, shard_id)
    assert_true(shard_size > 0)
  }
  
  // Verify load balancing across shards
  let shard_sizes = []
  for shard_id in 0..=3 {
    shard_sizes.push(ShardStats::get_shard_size(shard_stats, shard_id))
  }
  
  let max_size = shard_sizes.reduce(@(a, b) => if a > b { a } else { b })
  let min_size = shard_sizes.reduce(@(a, b) => if a < b { a } else { b })
  
  // Load should be reasonably balanced (within 50% difference)
  assert_true(max_size < min_size * 1.5)
  
  // Get overall performance metrics
  let performance_metrics = OptimizedCache::get_performance_metrics(optimized_cache)
  
  assert_true(PerformanceMetrics::average_get_time(performance_metrics) > 0.0)
  assert_true(PerformanceMetrics::average_put_time(performance_metrics) > 0.0)
  assert_true(PerformanceMetrics::operations_per_second(performance_metrics) > 0.0)
  assert_true(PerformanceMetrics::memory_efficiency(performance_metrics) > 0.0)
}