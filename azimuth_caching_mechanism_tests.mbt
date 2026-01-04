// Azimuth High-Quality Caching Mechanism Tests
// This file contains comprehensive test cases for caching mechanisms

// Test 1: Basic Cache Operations
test "basic cache operations" {
  let cache = Cache::new()
  
  // Test cache initialization
  assert_true(cache.is_empty())
  assert_eq(cache.size(), 0)
  
  // Test put operation
  cache.put("key1", "value1")
  assert_false(cache.is_empty())
  assert_eq(cache.size(), 1)
  assert_true(cache.contains_key("key1"))
  
  // Test get operation
  let value = cache.get("key1")
  assert_true(value.is_some())
  assert_eq(value.unwrap(), "value1")
  
  // Test get non-existent key
  let non_existent = cache.get("non_existent")
  assert_true(non_existent.is_none())
  
  // Test put with multiple keys
  cache.put("key2", "value2")
  cache.put("key3", "value3")
  assert_eq(cache.size(), 3)
  
  // Test get_or_put
  let value4 = cache.get_or_put("key4", || "value4")
  assert_eq(value4, "value4")
  assert_eq(cache.size(), 4)
  
  // Test get_or_put with existing key
  let value4_again = cache.get_or_put("key4", || "new_value4")
  assert_eq(value4_again, "value4") // Should return existing value
  assert_eq(cache.size(), 4) // Size should not change
  
  // Test remove operation
  let removed_value = cache.remove("key2")
  assert_true(removed_value.is_some())
  assert_eq(removed_value.unwrap(), "value2")
  assert_eq(cache.size(), 3)
  assert_false(cache.contains_key("key2"))
  
  // Test remove non-existent key
  let non_existent_removed = cache.remove("non_existent")
  assert_true(non_existent_removed.is_none())
  assert_eq(cache.size(), 3)
  
  // Test clear operation
  cache.clear()
  assert_true(cache.is_empty())
  assert_eq(cache.size(), 0)
  
  // Test put with expiration
  cache.put_with_ttl("temp_key", "temp_value", Duration::from_seconds(1))
  assert_eq(cache.size(), 1)
  assert_true(cache.contains_key("temp_key"))
  
  // Wait for expiration
  sleep(Duration::from_seconds(2))
  
  // Should be expired
  assert_false(cache.contains_key("temp_key"))
  assert_eq(cache.size(), 0)
  
  // Test keys and values iteration
  cache.put("key1", "value1")
  cache.put("key2", "value2")
  cache.put("key3", "value3")
  
  let keys = cache.keys()
  assert_eq(keys.length(), 3)
  assert_true(keys.contains("key1"))
  assert_true(keys.contains("key2"))
  assert_true(keys.contains("key3"))
  
  let values = cache.values()
  assert_eq(values.length(), 3)
  assert_true(values.contains("value1"))
  assert_true(values.contains("value2"))
  assert_true(values.contains("value3"))
  
  // Test entries iteration
  let entries = cache.entries()
  assert_eq(entries.length(), 3)
  
  for entry in entries {
    assert_true(cache.contains_key(entry.key))
    assert_eq(cache.get(entry.key).unwrap(), entry.value)
  }
  
  // Test retain operation
  cache.retain(|key, _| key.starts_with("key1"))
  assert_eq(cache.size(), 1)
  assert_true(cache.contains_key("key1"))
  assert_false(cache.contains_key("key2"))
  assert_false(cache.contains_key("key3"))
}

// Test 2: Cache Eviction Policies
test "cache eviction policies" {
  // Test LRU (Least Recently Used) eviction
  let lru_cache = Cache::with_capacity(3)
    .with_eviction_policy(EvictionPolicy::LRU)
  
  // Fill cache to capacity
  lru_cache.put("key1", "value1")
  lru_cache.put("key2", "value2")
  lru_cache.put("key3", "value3")
  assert_eq(lru_cache.size(), 3)
  
  // Access key1 to make it recently used
  let _ = lru_cache.get("key1")
  
  // Add new key, should evict key2 (least recently used)
  lru_cache.put("key4", "value4")
  assert_eq(lru_cache.size(), 3)
  assert_true(lru_cache.contains_key("key1")) // Recently used
  assert_false(lru_cache.contains_key("key2")) // Evicted
  assert_true(lru_cache.contains_key("key3"))
  assert_true(lru_cache.contains_key("key4"))
  
  // Test LFU (Least Frequently Used) eviction
  let lfu_cache = Cache::with_capacity(3)
    .with_eviction_policy(EvictionPolicy::LFU)
  
  // Fill cache to capacity
  lfu_cache.put("key1", "value1")
  lfu_cache.put("key2", "value2")
  lfu_cache.put("key3", "value3")
  
  // Access key1 multiple times to increase its frequency
  for i in 0..=5 {
    let _ = lfu_cache.get("key1")
  }
  
  // Access key2 a few times
  for i in 0..=2 {
    let _ = lfu_cache.get("key2")
  }
  
  // Access key3 once (default frequency)
  let _ = lfu_cache.get("key3")
  
  // Add new key, should evict key3 (least frequently used)
  lfu_cache.put("key4", "value4")
  assert_eq(lfu_cache.size(), 3)
  assert_true(lfu_cache.contains_key("key1")) // Most frequently used
  assert_true(lfu_cache.contains_key("key2")) // Second most frequently used
  assert_false(lfu_cache.contains_key("key3")) // Evicted
  assert_true(lfu_cache.contains_key("key4"))
  
  // Test FIFO (First In First Out) eviction
  let fifo_cache = Cache::with_capacity(3)
    .with_eviction_policy(EvictionPolicy::FIFO)
  
  // Fill cache to capacity
  fifo_cache.put("key1", "value1")
  fifo_cache.put("key2", "value2")
  fifo_cache.put("key3", "value3")
  
  // Access keys in different order (shouldn't affect FIFO eviction)
  let _ = fifo_cache.get("key3")
  let _ = fifo_cache.get("key1")
  
  // Add new key, should evict key1 (first in)
  fifo_cache.put("key4", "value4")
  assert_eq(fifo_cache.size(), 3)
  assert_false(fifo_cache.contains_key("key1")) // Evicted
  assert_true(fifo_cache.contains_key("key2"))
  assert_true(fifo_cache.contains_key("key3"))
  assert_true(fifo_cache.contains_key("key4"))
  
  // Test Random eviction
  let random_cache = Cache::with_capacity(3)
    .with_eviction_policy(EvictionPolicy::Random)
  
  // Fill cache to capacity
  random_cache.put("key1", "value1")
  random_cache.put("key2", "value2")
  random_cache.put("key3", "value3")
  
  // Add new key, should evict one random key
  random_cache.put("key4", "value4")
  assert_eq(random_cache.size(), 3)
  assert_true(random_cache.contains_key("key4"))
  
  // Test custom eviction policy
  let custom_cache = Cache::with_capacity(3)
    .with_eviction_policy(EvictionPolicy::Custom(|key, value, access_count, last_accessed| {
      // Custom logic: evict keys with odd length
      key.length() % 2 == 1
    }))
  
  // Fill cache with keys of different lengths
  custom_cache.put("odd", "value1")      // Length 3 (odd)
  custom_cache.put("even", "value2")     // Length 4 (even)
  custom_cache.put("odd2", "value3")     // Length 4 (even)
  
  // Add new key, should evict "odd" based on custom policy
  custom_cache.put("newkey", "value4")
  assert_eq(custom_cache.size(), 3)
  assert_false(custom_cache.contains_key("odd")) // Evicted
  assert_true(custom_cache.contains_key("even"))
  assert_true(custom_cache.contains_key("odd2"))
  assert_true(custom_cache.contains_key("newkey"))
}

// Test 3: Cache Performance and Optimization
test "cache performance and optimization" {
  // Test cache performance with different capacities
  let small_cache = Cache::with_capacity(100)
  let medium_cache = Cache::with_capacity(1000)
  let large_cache = Cache::with_capacity(10000)
  
  // Fill caches with test data
  let test_data = generate_test_data(5000)
  
  let small_start = get_current_timestamp()
  for i in 0..=100 {
    small_cache.put(format!("key_{}", i), test_data[i])
  }
  let small_end = get_current_timestamp()
  let small_time = small_end - small_start
  
  let medium_start = get_current_timestamp()
  for i in 0..=1000 {
    medium_cache.put(format!("key_{}", i), test_data[i])
  }
  let medium_end = get_current_timestamp()
  let medium_time = medium_end - medium_start
  
  let large_start = get_current_timestamp()
  for i in 0..=10000 {
    large_cache.put(format!("key_{}", i), test_data[i])
  }
  let large_end = get_current_timestamp()
  let large_time = large_end - large_start
  
  // Performance should scale reasonably with capacity
  assert_true(small_time.to_millis() < 100)
  assert_true(medium_time.to_millis() < 1000)
  assert_true(large_time.to_millis() < 10000)
  
  // Test cache hit ratio
  let hit_ratio_cache = Cache::with_capacity(1000)
    .with_eviction_policy(EvictionPolicy::LRU)
  
  // Fill cache
  for i in 0..=1000 {
    hit_ratio_cache.put(format!("key_{}", i), test_data[i])
  }
  
  // Perform cache operations
  let mut hits = 0
  let mut misses = 0
  
  // Access existing keys (should be hits)
  for i in 0..=500 {
    if hit_ratio_cache.get(format!("key_{}", i)).is_some() {
      hits = hits + 1
    } else {
      misses = misses + 1
    }
  }
  
  // Access non-existing keys (should be misses)
  for i in 1001..=1500 {
    if hit_ratio_cache.get(format!("key_{}", i)).is_some() {
      hits = hits + 1
    } else {
      misses = misses + 1
    }
  }
  
  let hit_ratio = hits as Float / (hits + misses) as Float
  
  // Should have reasonable hit ratio
  assert_true(hit_ratio > 0.3)
  
  // Verify cache statistics
  let stats = hit_ratio_cache.get_statistics()
  assert_eq(stats.hits, hits)
  assert_eq(stats.misses, misses)
  assert_eq(stats.puts, 1001)
  assert_eq(stats.evictions, 1) // One eviction when adding key 1001
  assert_true(stats.hit_ratio > 0.3)
  
  // Test cache with pre-allocation
  let preallocated_cache = Cache::with_capacity(10000)
    .with_preallocation(true)
  
  let prealloc_start = get_current_timestamp()
  for i in 0..=10000 {
    preallocated_cache.put(format!("key_{}", i), test_data[i])
  }
  let prealloc_end = get_current_timestamp()
  let prealloc_time = prealloc_end - prealloc_start
  
  // Preallocated cache should be faster
  let normal_cache = Cache::with_capacity(10000)
  
  let normal_start = get_current_timestamp()
  for i in 0..=10000 {
    normal_cache.put(format!("key_{}", i), test_data[i])
  }
  let normal_end = get_current_timestamp()
  let normal_time = normal_end - normal_start
  
  assert_true(prealloc_time.to_millis() <= normal_time.to_millis())
  
  // Test cache with hash function optimization
  let optimized_cache = Cache::with_capacity(10000)
    .with_hash_function(|key| {
      // Use a better hash function
      let mut hash = 5381
      for byte in key.bytes() {
        hash = ((hash << 5) + hash) + byte // hash * 33 + byte
      }
      hash
    })
  
  let optimized_start = get_current_timestamp()
  for i in 0..=10000 {
    optimized_cache.put(format!("key_{}", i), test_data[i])
  }
  let optimized_end = get_current_timestamp()
  let optimized_time = optimized_end - optimized_start
  
  // Optimized hash function should improve performance
  assert_true(optimized_time.to_millis() <= normal_time.to_millis())
  
  // Test cache memory usage
  let memory_cache = Cache::with_capacity(10000)
  
  let memory_before = get_memory_usage()
  
  for i in 0..=10000 {
    memory_cache.put(format!("key_{}", i), test_data[i])
  }
  
  let memory_after = get_memory_usage()
  let memory_used = memory_after - memory_before
  
  // Memory usage should be reasonable
  assert_true(memory_used < 100 * 1024 * 1024) // Less than 100MB
  
  // Test cache with compression
  let compressed_cache = Cache::with_capacity(1000)
    .with_compression(true)
    .with_compression_algorithm(CompressionAlgorithm::Gzip)
  
  let large_values = []
  for i in 0..=1000 {
    large_values.push("x".repeat(1000)) // 1KB strings
  }
  
  let compressed_start = get_current_timestamp()
  for i in 0..=1000 {
    compressed_cache.put(format!("key_{}", i), large_values[i])
  }
  let compressed_end = get_current_timestamp()
  let compressed_time = compressed_end - compressed_start
  
  // Test without compression
  let uncompressed_cache = Cache::with_capacity(1000)
  
  let uncompressed_start = get_current_timestamp()
  for i in 0..=1000 {
    uncompressed_cache.put(format!("key_{}", i), large_values[i])
  }
  let uncompressed_end = get_current_timestamp()
  let uncompressed_time = uncompressed_end - uncompressed_start
  
  // Compression should reduce memory usage but might increase CPU time
  let compressed_memory = compressed_cache.get_memory_usage()
  let uncompressed_memory = uncompressed_cache.get_memory_usage()
  
  assert_true(compressed_memory < uncompressed_memory)
}

// Test 4: Distributed Cache
test "distributed cache" {
  // Test distributed cache with multiple nodes
  let node1 = DistributedCacheNode::new("node1", "localhost:8001")
  let node2 = DistributedCacheNode::new("node2", "localhost:8002")
  let node3 = DistributedCacheNode::new("node3", "localhost:8003")
  
  // Configure distributed cache
  let distributed_cache = DistributedCache::new()
    .add_node(node1)
    .add_node(node2)
    .add_node(node3)
    .with_consistency_level(ConsistencyLevel::Eventual)
    .with_replication_factor(2)
  
  // Test put operation
  let put_result = distributed_cache.put("distributed_key", "distributed_value")
  assert_true(put_result.is_success())
  
  // Test get operation
  let get_result = distributed_cache.get("distributed_key")
  assert_true(get_result.is_success())
  assert_eq(get_result.unwrap(), "distributed_value")
  
  // Test get from different node
  let get_from_node2 = distributed_cache.get_from_node("node2", "distributed_key")
  assert_true(get_from_node2.is_success())
  assert_eq(get_from_node2.unwrap(), "distributed_value")
  
  // Test cache consistency
  let consistency_result = distributed_cache.check_consistency("distributed_key")
  assert_true(consistency_result.is_consistent)
  
  // Test cache invalidation
  let invalidate_result = distributed_cache.invalidate("distributed_key")
  assert_true(invalidate_result.is_success())
  
  // Verify invalidation propagated
  let get_after_invalidate = distributed_cache.get("distributed_key")
  assert_true(get_after_invalidate.is_error())
  
  // Test cache with strong consistency
  let strong_cache = DistributedCache::new()
    .add_node(node1)
    .add_node(node2)
    .add_node(node3)
    .with_consistency_level(ConsistencyLevel::Strong)
    .with_replication_factor(3)
  
  let strong_put = strong_cache.put("strong_key", "strong_value")
  assert_true(strong_put.is_success())
  
  let strong_get = strong_cache.get("strong_key")
  assert_true(strong_get.is_success())
  assert_eq(strong_get.unwrap(), "strong_value")
  
  // Test cache with read repair
  let read_repair_cache = DistributedCache::new()
    .add_node(node1)
    .add_node(node2)
    .add_node(node3)
    .with_read_repair(true)
  
  let repair_put = read_repair_cache.put("repair_key", "repair_value")
  assert_true(repair_put.is_success())
  
  // Simulate inconsistency by updating only one node
  let direct_update = node1.put_direct("repair_key", "inconsistent_value")
  assert_true(direct_update.is_success())
  
  // Get should trigger read repair
  let repair_get = read_repair_cache.get("repair_key")
  assert_true(repair_get.is_success())
  
  // Verify consistency after read repair
  let consistency_after_repair = read_repair_cache.check_consistency("repair_key")
  assert_true(consistency_after_repair.is_consistent)
  
  // Test cache with partition tolerance
  let partitioned_cache = DistributedCache::new()
    .add_node(node1)
    .add_node(node2)
    .add_node(node3)
    .with_partition_tolerance(true)
  
  let partition_put = partitioned_cache.put("partition_key", "partition_value")
  assert_true(partition_put.is_success())
  
  // Simulate network partition
  partitioned_cache.simulate_partition(["node2", "node3"])
  
  // Operations should still work during partition
  let partition_get = partitioned_cache.get("partition_key")
  assert_true(partition_get.is_success())
  
  // Restore from partition
  partitioned_cache.heal_partition()
  
  // Test cache with automatic failover
  let failover_cache = DistributedCache::new()
    .add_node(node1)
    .add_node(node2)
    .add_node(node3)
    .with_failover(true)
  
  let failover_put = failover_cache.put("failover_key", "failover_value")
  assert_true(failover_put.is_success())
  
  // Simulate node failure
  failover_cache.simulate_node_failure("node1")
  
  // Operations should failover to other nodes
  let failover_get = failover_cache.get("failover_key")
  assert_true(failover_get.is_success())
  
  // Restore node
  failover_cache.restore_node("node1")
  
  // Test cache with load balancing
  let load_balanced_cache = DistributedCache::new()
    .add_node(node1)
    .add_node(node2)
    .add_node(node3)
    .with_load_balancing(LoadBalancingStrategy::RoundRobin)
  
  // Perform multiple operations
  for i in 0..=100 {
    let key = format!("load_balance_key_{}", i)
    let value = format!("load_balance_value_{}", i)
    
    let put_result = load_balanced_cache.put(key, value)
    assert_true(put_result.is_success())
  }
  
  // Verify load is balanced across nodes
  let load_stats = load_balanced_cache.get_load_statistics()
  assert_true(load_stats.node_loads.get("node1") > 0)
  assert_true(load_stats.node_loads.get("node2") > 0)
  assert_true(load_stats.node_loads.get("node3") > 0)
  
  // Load should be roughly balanced
  let max_load = max(load_stats.node_loads.values())
  let min_load = min(load_stats.node_loads.values())
  assert_true(max_load <= min_load * 2) // Not more than 2x difference
  
  // Test cache with consistent hashing
  let consistent_hash_cache = DistributedCache::new()
    .add_node(node1)
    .add_node(node2)
    .add_node(node3)
    .with_consistent_hashing(true)
  
  let hash_put = consistent_hash_cache.put("hash_key", "hash_value")
  assert_true(hash_put.is_success())
  
  let hash_get = consistent_hash_cache.get("hash_key")
  assert_true(hash_get.is_success())
  assert_eq(hash_get.unwrap(), "hash_value")
  
  // Test key distribution with consistent hashing
  let distribution_result = consistent_hash_cache.analyze_key_distribution(1000)
  assert_true(distribution_result.is_evenly_distributed)
  assert_true(distribution_result.standard_deviation < 100) // Reasonable distribution
}

// Test 5: Cache Persistence and Recovery
test "cache persistence and recovery" {
  // Test cache with file persistence
  let persistent_cache = Cache::with_capacity(1000)
    .with_persistence(PersistenceType::File)
    .with_persistence_path("/tmp/cache_data.bin")
  
  // Fill cache with data
  for i in 0..=1000 {
    persistent_cache.put(format!("key_{}", i), format!("value_{}", i))
  }
  
  // Persist cache to disk
  let persist_result = persistent_cache.persist()
  assert_true(persist_result.is_success())
  
  // Create new cache and load from disk
  let loaded_cache = Cache::with_capacity(1000)
    .with_persistence(PersistenceType::File)
    .with_persistence_path("/tmp/cache_data.bin")
  
  let load_result = loaded_cache.load()
  assert_true(load_result.is_success())
  
  // Verify data was loaded correctly
  assert_eq(loaded_cache.size(), 1000)
  assert_eq(loaded_cache.get("key_1").unwrap(), "value_1")
  assert_eq(loaded_cache.get("key_500").unwrap(), "value_500")
  assert_eq(loaded_cache.get("key_1000").unwrap(), "value_1000")
  
  // Test cache with database persistence
  let db_cache = Cache::with_capacity(1000)
    .with_persistence(PersistenceType::Database)
    .with_database_config(DatabaseConfig::new()
      .with_connection_string("sqlite:///tmp/cache.db")
      .with_table_name("cache_entries"))
  
  // Fill cache with data
  for i in 0..=500 {
    db_cache.put(format!("db_key_{}", i), format!("db_value_{}", i))
  }
  
  // Persist to database
  let db_persist_result = db_cache.persist()
  assert_true(db_persist_result.is_success())
  
  // Create new cache and load from database
  let loaded_db_cache = Cache::with_capacity(1000)
    .with_persistence(PersistenceType::Database)
    .with_database_config(DatabaseConfig::new()
      .with_connection_string("sqlite:///tmp/cache.db")
      .with_table_name("cache_entries"))
  
  let db_load_result = loaded_db_cache.load()
  assert_true(db_load_result.is_success())
  
  // Verify data was loaded correctly
  assert_eq(loaded_db_cache.size(), 500)
  assert_eq(loaded_db_cache.get("db_key_1").unwrap(), "db_value_1")
  assert_eq(loaded_db_cache.get("db_key_250").unwrap(), "db_value_250")
  assert_eq(loaded_db_cache.get("db_key_500").unwrap(), "db_value_500")
  
  // Test cache with incremental persistence
  let incremental_cache = Cache::with_capacity(1000)
    .with_persistence(PersistenceType::File)
    .with_persistence_path("/tmp/incremental_cache.bin")
    .with_incremental_persistence(true)
  
  // Initial data
  for i in 0..=500 {
    incremental_cache.put(format!("inc_key_{}", i), format!("inc_value_{}", i))
  }
  
  // Initial persistence
  let initial_persist = incremental_cache.persist()
  assert_true(initial_persist.is_success())
  
  // Add more data
  for i in 501..=1000 {
    incremental_cache.put(format!("inc_key_{}", i), format!("inc_value_{}", i))
  }
  
  // Incremental persistence (should be faster)
  let incremental_persist_start = get_current_timestamp()
  let incremental_persist = incremental_cache.persist()
  let incremental_persist_end = get_current_timestamp()
  let incremental_persist_time = incremental_persist_end - incremental_persist_start
  
  assert_true(incremental_persist.is_success())
  assert_true(incremental_persist_time.to_millis() < 100) // Should be fast
  
  // Test cache with compression in persistence
  let compressed_persistence_cache = Cache::with_capacity(1000)
    .with_persistence(PersistenceType::File)
    .with_persistence_path("/tmp/compressed_cache.bin")
    .with_persistence_compression(true)
    .with_compression_algorithm(CompressionAlgorithm::Gzip)
  
  // Fill with repetitive data (compresses well)
  for i in 0..=1000 {
    compressed_persistence_cache.put(format!("comp_key_{}", i), "x".repeat(100))
  }
  
  // Persist with compression
  let compressed_persist = compressed_persistence_cache.persist()
  assert_true(compressed_persist.is_success())
  
  // Check file size (should be smaller due to compression)
  let compressed_file_size = get_file_size("/tmp/compressed_cache.bin")
  let uncompressed_file_size = get_file_size("/tmp/cache_data.bin")
  
  assert_true(compressed_file_size < uncompressed_file_size)
  
  // Test cache recovery from corruption
  let recovery_cache = Cache::with_capacity(1000)
    .with_persistence(PersistenceType::File)
    .with_persistence_path("/tmp/recovery_cache.bin")
    .with_auto_recovery(true)
  
  // Fill with data
  for i in 0..=500 {
    recovery_cache.put(format!("recovery_key_{}", i), format!("recovery_value_{}", i))
  }
  
  // Persist cache
  let recovery_persist = recovery_cache.persist()
  assert_true(recovery_persist.is_success())
  
  // Corrupt the file (simulate corruption)
  corrupt_file("/tmp/recovery_cache.bin")
  
  // Try to load with recovery
  let recovery_load = recovery_cache.load()
  assert_true(recovery_load.is_success()) // Should recover from corruption
  
  // Test cache with backup and restore
  let backup_cache = Cache::with_capacity(1000)
    .with_persistence(PersistenceType::File)
    .with_persistence_path("/tmp/backup_cache.bin")
    .with_backup_enabled(true)
    .with_backup_path("/tmp/backup_cache_backup.bin")
  
  // Fill with data
  for i in 0..=500 {
    backup_cache.put(format!("backup_key_{}", i), format!("backup_value_{}", i))
  }
  
  // Create backup
  let backup_result = backup_cache.create_backup()
  assert_true(backup_result.is_success())
  
  // Modify cache
  for i in 501..=1000 {
    backup_cache.put(format!("backup_key_{}", i), format!("backup_value_{}", i))
  }
  
  // Restore from backup
  let restore_result = backup_cache.restore_from_backup()
  assert_true(restore_result.is_success())
  
  // Verify restored data
  assert_eq(backup_cache.size(), 500)
  assert_eq(backup_cache.get("backup_key_1").unwrap(), "backup_value_1")
  assert_false(backup_cache.contains_key("backup_key_600")) // Should not exist after restore
  
  // Test cache with versioned persistence
  let versioned_cache = Cache::with_capacity(1000)
    .with_persistence(PersistenceType::File)
    .with_persistence_path("/tmp/versioned_cache.bin")
    .with_versioned_persistence(true)
    .with_max_versions(5)
  
  // Fill with data
  for i in 0..=100 {
    versioned_cache.put(format!("versioned_key_{}", i), format!("versioned_value_{}", i))
  }
  
  // Persist version 1
  let v1_persist = versioned_cache.persist()
  assert_true(v1_persist.is_success())
  assert_eq(v1_persist.unwrap().version, 1)
  
  // Modify cache
  for i in 101..=200 {
    versioned_cache.put(format!("versioned_key_{}", i), format!("versioned_value_{}", i))
  }
  
  // Persist version 2
  let v2_persist = versioned_cache.persist()
  assert_true(v2_persist.is_success())
  assert_eq(v2_persist.unwrap().version, 2)
  
  // Load specific version
  let load_v1_result = versioned_cache.load_version(1)
  assert_true(load_v1_result.is_success())
  assert_eq(versioned_cache.size(), 100)
  
  let load_v2_result = versioned_cache.load_version(2)
  assert_true(load_v2_result.is_success())
  assert_eq(versioned_cache.size(), 200)
}

// Test 6: Cache Security and Access Control
test "cache security and access control" {
  // Test cache with encryption
  let encryption_key = generate_encryption_key()
  let secure_cache = Cache::with_capacity(1000)
    .with_encryption(true)
    .with_encryption_key(encryption_key)
    .with_encryption_algorithm(EncryptionAlgorithm::AES256)
  
  // Store sensitive data
  secure_cache.put("sensitive_key", "sensitive_value")
  
  // Retrieve data
  let retrieved_value = secure_cache.get("sensitive_key")
  assert_true(retrieved_value.is_some())
  assert_eq(retrieved_value.unwrap(), "sensitive_value")
  
  // Verify data is encrypted in storage
  let raw_storage = secure_cache.get_raw_storage()
  assert_false(raw_storage.contains("sensitive_value")) // Should not contain plain text
  
  // Test cache with access control
  let access_controller = AccessController::new()
  
  // Define roles and permissions
  access_controller.add_role("admin", RolePermissions::new()
    .with_read_all(true)
    .with_write_all(true)
    .with_delete_all(true))
  
  access_controller.add_role("user", RolePermissions::new()
    .with_read_patterns(["user_*", "public_*"])
    .with_write_patterns(["user_*"])
    .with_delete_patterns(["user_*"]))
  
  access_controller.add_role("guest", RolePermissions::new()
    .with_read_patterns(["public_*"]))
  
  let access_controlled_cache = Cache::with_capacity(1000)
    .with_access_controller(access_controller)
  
  // Test admin access
  let admin_context = AccessContext::new("admin")
  let admin_put = access_controlled_cache.put_with_context("admin_key", "admin_value", admin_context)
  assert_true(admin_put.is_success())
  
  let admin_get = access_controlled_cache.get_with_context("admin_key", admin_context)
  assert_true(admin_get.is_success())
  
  // Test user access
  let user_context = AccessContext::new("user")
  
  // User should be able to access user_* keys
  let user_put = access_controlled_cache.put_with_context("user_123", "user_value", user_context)
  assert_true(user_put.is_success())
  
  let user_get = access_controlled_cache.get_with_context("user_123", user_context)
  assert_true(user_get.is_success())
  
  // User should not be able to access admin_* keys
  let user_admin_get = access_controlled_cache.get_with_context("admin_key", user_context)
  assert_true(user_admin_get.is_error())
  assert_eq(user_admin_get.unwrap_error().error_type, AccessErrorType::PermissionDenied)
  
  // Test guest access
  let guest_context = AccessContext::new("guest")
  
  // Guest should be able to access public_* keys
  let public_put = access_controlled_cache.put_with_context("public_info", "public_value", admin_context)
  assert_true(public_put.is_success())
  
  let guest_get = access_controlled_cache.get_with_context("public_info", guest_context)
  assert_true(guest_get.is_success())
  
  // Guest should not be able to access user_* keys
  let guest_user_get = access_controlled_cache.get_with_context("user_123", guest_context)
  assert_true(guest_user_get.is_error())
  
  // Test cache with audit logging
  let audit_logger = AuditLogger::new()
  let audited_cache = Cache::with_capacity(1000)
    .with_audit_logger(audit_logger)
    .with_audit_all_operations(true)
  
  // Perform operations
  audited_cache.put("audit_key", "audit_value")
  audited_cache.get("audit_key")
  audited_cache.remove("audit_key")
  
  // Check audit logs
  let audit_logs = audited_cache.get_audit_logs()
  assert_true(audit_logs.length() >= 3)
  
  let put_log = audit_logs.find(|log| log.operation == "put")
  assert_true(put_log.is_some())
  assert_eq(put_log.unwrap().key, "audit_key")
  
  let get_log = audit_logs.find(|log| log.operation == "get")
  assert_true(get_log.is_some())
  assert_eq(get_log.unwrap().key, "audit_key")
  
  let remove_log = audit_logs.find(|log| log.operation == "remove")
  assert_true(remove_log.is_some())
  assert_eq(remove_log.unwrap().key, "audit_key")
  
  // Test cache with rate limiting
  let rate_limiter = RateLimiter::new()
    .with_max_requests_per_minute(100)
    .with_max_requests_per_hour(1000)
  
  let rate_limited_cache = Cache::with_capacity(1000)
    .with_rate_limiter(rate_limiter)
  
  // Perform operations within rate limit
  for i in 0..=50 {
    let result = rate_limited_cache.put(format!("rate_key_{}", i), format!("rate_value_{}", i))
    assert_true(result.is_success())
  }
  
  // Exceed rate limit
  let mut exceeded_count = 0
  for i in 51..=200 {
    let result = rate_limited_cache.put(format!("rate_key_{}", i), format!("rate_value_{}", i))
    if result.is_error() && result.unwrap_error().error_type == CacheErrorType::RateLimitExceeded {
      exceeded_count = exceeded_count + 1
    }
  }
  
  assert_true(exceeded_count > 0) // Should have some rate limit exceeded errors
  
  // Test cache with data validation
  let validator = DataValidator::new()
  
  // Add validation rules
  validator.add_rule("user_*", ValidationRule::StringLength(1, 100))
  validator.add_rule("id_*", ValidationRule::IntegerRange(1, 1000000))
  validator.add_rule("email_*", ValidationRule::PatternMatches("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"))
  
  let validated_cache = Cache::with_capacity(1000)
    .with_data_validator(validator)
  
  // Valid data
  let valid_user = validated_cache.put("user_123", "valid_username")
  assert_true(valid_user.is_success())
  
  let valid_id = validated_cache.put("id_456", "12345")
  assert_true(valid_id.is_success())
  
  let valid_email = validated_cache.put("email_789", "user@example.com")
  assert_true(valid_email.is_success())
  
  // Invalid data
  let invalid_user = validated_cache.put("user_invalid", "") // Too short
  assert_true(invalid_user.is_error())
  
  let invalid_id = validated_cache.put("id_invalid", "0") // Below range
  assert_true(invalid_id.is_error())
  
  let invalid_email = validated_cache.put("email_invalid", "not_an_email")
  assert_true(invalid_email.is_error())
  
  // Test cache with secure eviction
  let secure_eviction_cache = Cache::with_capacity(100)
    .with_secure_eviction(true)
    .with_eviction_policy(EvictionPolicy::LRU)
  
  // Fill with sensitive data
  for i in 0..=100 {
    secure_eviction_cache.put(format!("secure_key_{}", i), format!("secure_value_{}", i))
  }
  
  // Eviction should securely clear data
  let eviction_result = secure_eviction_cache.put("new_key", "new_value")
  assert_true(eviction_result.is_success())
  
  // Verify evicted data is securely cleared
  let memory_analysis = secure_eviction_cache.analyze_memory()
  assert_true(memory_analysis.securely_cleared_entries > 0)
}

// Test 7: Cache Monitoring and Metrics
test "cache monitoring and metrics" {
  // Test cache with metrics collection
  let monitored_cache = Cache::with_capacity(1000)
    .with_metrics_enabled(true)
    .with_detailed_metrics(true)
  
  // Perform operations
  for i in 0..=500 {
    monitored_cache.put(format!("metrics_key_{}", i), format!("metrics_value_{}", i))
  }
  
  for i in 0..=250 {
    let _ = monitored_cache.get(format!("metrics_key_{}", i))
  }
  
  for i in 251..=300 {
    let _ = monitored_cache.get(format!("non_existent_key_{}", i))
  }
  
  for i in 0..=100 {
    monitored_cache.remove(format!("metrics_key_{}", i))
  }
  
  // Get metrics
  let metrics = monitored_cache.get_metrics()
  
  assert_eq(metrics.puts, 501)
  assert_eq(metrics.gets, 301)
  assert_eq(metrics.hits, 251)
  assert_eq(metrics.misses, 50)
  assert_eq(metrics.removals, 101)
  assert_eq(metrics.evictions, 0) // No evictions yet
  
  let hit_ratio = metrics.hits as Float / metrics.gets as Float
  assert_true(hit_ratio > 0.8) // Should have high hit ratio
  
  // Test cache with real-time monitoring
  let real_time_cache = Cache::with_capacity(1000)
    .with_real_time_monitoring(true)
    .with_monitoring_interval(Duration::from_seconds(1))
  
  // Start monitoring
  let monitor_handle = real_time_cache.start_monitoring()
  
  // Perform operations over time
  for i in 0..=100 {
    real_time_cache.put(format!("realtime_key_{}", i), format!("realtime_value_{}", i))
    sleep(Duration::from_millis(10))
  }
  
  // Get time-series metrics
  let time_series_metrics = real_time_cache.get_time_series_metrics()
  assert_true(time_series_metrics.length() > 0)
  
  for metric in time_series_metrics {
    assert_true(metric.timestamp > 0)
    assert_true(metric.size >= 0)
    assert_true(metric.hit_ratio >= 0.0 && metric.hit_ratio <= 1.0)
  }
  
  // Stop monitoring
  real_time_cache.stop_monitoring(monitor_handle)
  
  // Test cache with performance profiling
  let profiled_cache = Cache::with_capacity(1000)
    .with_profiling_enabled(true)
  
  // Profile operations
  let profile_start = get_current_timestamp()
  
  for i in 0..=1000 {
    profiled_cache.put(format!("profile_key_{}", i), format!("profile_value_{}", i))
  }
  
  let profile_end = get_current_timestamp()
  
  let profile_metrics = profiled_cache.get_profile_metrics()
  
  assert_true(profile_metrics.total_time > Duration::from_seconds(0))
  assert_true(profile_metrics.average_operation_time > Duration::from_seconds(0))
  assert_true(profile_metrics.max_operation_time > Duration::from_seconds(0))
  assert_true(profile_metrics.min_operation_time > Duration::from_seconds(0))
  
  // Test cache with health checking
  let health_checked_cache = Cache::with_capacity(1000)
    .with_health_check_enabled(true)
    .with_health_check_interval(Duration::from_seconds(5))
  
  // Start health checking
  let health_handle = health_checked_cache.start_health_checking()
  
  // Perform operations
  for i in 0..=100 {
    health_checked_cache.put(format!("health_key_{}", i), format!("health_value_{}", i))
  }
  
  // Wait for health check
  sleep(Duration::from_seconds(6))
  
  // Get health status
  let health_status = health_checked_cache.get_health_status()
  assert_true(health_status.is_healthy)
  assert_true(health_status.last_check > 0)
  assert_true(health_status.checks_passed > 0)
  
  // Stop health checking
  health_checked_cache.stop_health_checking(health_handle)
  
  // Test cache with alerting
  let alert_manager = AlertManager::new()
  
  // Configure alerts
  alert_manager.add_alert(AlertConfig::new()
    .with_name("high_miss_ratio")
    .with_condition(|metrics| metrics.hit_ratio < 0.5)
    .with_severity(AlertSeverity::Warning))
  
  alert_manager.add_alert(AlertConfig::new()
    .with_name("memory_usage_high")
    .with_condition(|metrics| metrics.memory_usage > 0.8)
    .with_severity(AlertSeverity::Critical))
  
  let alerting_cache = Cache::with_capacity(100)
    .with_alert_manager(alert_manager)
  
  // Trigger high miss ratio alert
  for i in 0..=100 {
    alerting_cache.put(format!("alert_key_{}", i), format!("alert_value_{}", i))
  }
  
  for i in 0..=200 {
    // Access non-existent keys to lower hit ratio
    let _ = alerting_cache.get(format!("non_existent_{}", i))
  }
  
  // Check for alerts
  let alerts = alerting_cache.get_alerts()
  let high_miss_ratio_alert = alerts.find(|alert| alert.name == "high_miss_ratio")
  assert_true(high_miss_ratio_alert.is_some())
  assert_eq(high_miss_ratio_alert.unwrap().severity, AlertSeverity::Warning)
  
  // Test cache with custom metrics
  let custom_metrics_cache = Cache::with_capacity(1000)
    .with_custom_metrics_enabled(true)
  
  // Add custom metrics
  custom_metrics_cache.add_custom_metric("user_cache_hits", 0)
  custom_metrics_cache.add_custom_metric("admin_cache_hits", 0)
  
  // Update custom metrics
  custom_metrics_cache.increment_custom_metric("user_cache_hits", 150)
  custom_metrics_cache.increment_custom_metric("admin_cache_hits", 50)
  
  // Get custom metrics
  let custom_metrics = custom_metrics_cache.get_custom_metrics()
  assert_eq(custom_metrics.get("user_cache_hits"), 150)
  assert_eq(custom_metrics.get("admin_cache_hits"), 50)
  
  // Test cache with dashboard integration
  let dashboard_cache = Cache::with_capacity(1000)
    .with_dashboard_integration(true)
    .with_dashboard_port(8080)
  
  // Start dashboard
  let dashboard_handle = dashboard_cache.start_dashboard()
  
  // Perform operations
  for i in 0..=100 {
    dashboard_cache.put(format!("dashboard_key_{}", i), format!("dashboard_value_{}", i))
  }
  
  // Get dashboard data
  let dashboard_data = dashboard_cache.get_dashboard_data()
  assert_true(dashboard_data.contains("puts"))
  assert_true(dashboard_data.contains("hits"))
  assert_true(dashboard_data.contains("misses"))
  
  // Stop dashboard
  dashboard_cache.stop_dashboard(dashboard_handle)
}

// Test 8: Cache Specialized Implementations
test "cache specialized implementations" {
  // Test LRU cache implementation
  let lru_cache = LRUCache::with_capacity(100)
  
  // Fill cache
  for i in 0..=100 {
    lru_cache.put(format!("lru_key_{}", i), format!("lru_value_{}", i))
  }
  
  // Access some keys to change recency
  let _ = lru_cache.get("lru_key_50");
  let _ = lru_cache.get("lru_key_75");
  let _ = lru_cache.get("lru_key_90");
  
  // Add new key, should evict least recently used
  lru_cache.put("new_lru_key", "new_lru_value");
  
  // Verify LRU behavior
  assert_true(lru_cache.contains_key("lru_key_50")) // Recently accessed
  assert_true(lru_cache.contains_key("lru_key_75")) // Recently accessed
  assert_true(lru_cache.contains_key("lru_key_90")) // Recently accessed
  assert_true(lru_cache.contains_key("new_lru_key")) // New key
  assert_false(lru_cache.contains_key("lru_key_0")) // Evicted (least recently used)
  
  // Test LFU cache implementation
  let lfu_cache = LFUCache::with_capacity(100)
  
  // Fill cache
  for i in 0..=100 {
    lfu_cache.put(format!("lfu_key_{}", i), format!("lfu_value_{}", i))
  }
  
  // Access keys with different frequencies
  for i in 0..=10 {
    let _ = lfu_cache.get("lfu_key_10"); // High frequency
  }
  
  for i in 0..=5 {
    let _ = lfu_cache.get("lfu_key_20"); // Medium frequency
  }
  
  let _ = lfu_cache.get("lfu_key_30"); // Low frequency
  
  // Add new key, should evict least frequently used
  lfu_cache.put("new_lfu_key", "new_lfu_value");
  
  // Verify LFU behavior
  assert_true(lfu_cache.contains_key("lfu_key_10")) // High frequency
  assert_true(lfu_cache.contains_key("lfu_key_20")) // Medium frequency
  assert_true(lfu_cache.contains_key("new_lfu_key")) // New key
  assert_false(lfu_cache.contains_key("lfu_key_30")) // Evicted (least frequently used)
  
  // Test TTL cache implementation
  let ttl_cache = TTLCache::new()
  
  // Add entries with different TTLs
  ttl_cache.put_with_ttl("short_ttl", "short_value", Duration::from_seconds(1))
  ttl_cache.put_with_ttl("medium_ttl", "medium_value", Duration::from_seconds(5))
  ttl_cache.put_with_ttl("long_ttl", "long_value", Duration::from_seconds(10))
  
  // Verify all entries exist initially
  assert_true(ttl_cache.contains_key("short_ttl"))
  assert_true(ttl_cache.contains_key("medium_ttl"))
  assert_true(ttl_cache.contains_key("long_ttl"))
  
  // Wait for short TTL to expire
  sleep(Duration::from_seconds(2))
  
  // Verify expired entry is removed
  assert_false(ttl_cache.contains_key("short_ttl"))
  assert_true(ttl_cache.contains_key("medium_ttl"))
  assert_true(ttl_cache.contains_key("long_ttl"))
  
  // Wait for medium TTL to expire
  sleep(Duration::from_seconds(4))
  
  // Verify medium TTL entry is removed
  assert_false(ttl_cache.contains_key("medium_ttl"))
  assert_true(ttl_cache.contains_key("long_ttl"))
  
  // Test size-based cache implementation
  let size_cache = SizeBasedCache::with_max_size(1024) // 1KB max
  
  // Add entries of different sizes
  size_cache.put("small", "x") // 1 byte
  size_cache.put("medium", "x".repeat(100)) // 100 bytes
  size_cache.put("large", "x".repeat(500)) // 500 bytes
  
  // Verify all entries fit
  assert_true(size_cache.contains_key("small"))
  assert_true(size_cache.contains_key("medium"))
  assert_true(size_cache.contains_key("large"))
  
  // Add entry that exceeds remaining capacity
  size_cache.put("too_large", "x".repeat(500)) // 500 bytes, would exceed 1KB total
  
  // Verify eviction occurred
  assert_eq(size_cache.current_size(), 1024) // Should be at max capacity
  assert_true(size_cache.contains_key("large")) // Kept (most recent)
  assert_true(size_cache.contains_key("too_large")) // Added
  assert_false(size_cache.contains_key("small")) // Evicted
  assert_false(size_cache.contains_key("medium")) // Evicted
  
  // Test weighted cache implementation
  let weighted_cache = WeightedCache::with_max_weight(100)
  
  // Define weights for different keys
  weighted_cache.put_with_weight("light", "light_value", 10)
  weighted_cache.put_with_weight("medium", "medium_value", 30)
  weighted_cache.put_with_weight("heavy", "heavy_value", 60)
  
  // Verify all entries fit (total weight = 100)
  assert_true(weighted_cache.contains_key("light"))
  assert_true(weighted_cache.contains_key("medium"))
  assert_true(weighted_cache.contains_key("heavy"))
  assert_eq(weighted_cache.current_weight(), 100)
  
  // Add entry that exceeds remaining capacity
  weighted_cache.put_with_weight("new_heavy", "new_heavy_value", 40)
  
  // Verify eviction occurred based on weight
  assert_eq(weighted_cache.current_weight(), 100) // Should be at max capacity
  assert_true(weighted_cache.contains_key("heavy")) // Kept (heaviest)
  assert_true(weighted_cache.contains_key("new_heavy")) // Added
  assert_false(weighted_cache.contains_key("light")) // Evicted (lightest)
  
  // Test concurrent cache implementation
  let concurrent_cache = ConcurrentCache::with_capacity(1000)
  
  // Simulate concurrent operations
  let handles = []
  
  // Concurrent puts
  for i in 0..=10 {
    let handle = spawn_thread(|| {
      for j in 0..=100 {
        concurrent_cache.put(format!("concurrent_key_{}_{}", i, j), format!("concurrent_value_{}_{}", i, j))
      }
    })
    handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in handles {
    handle.join()
  }
  
  // Verify all entries were added
  assert_eq(concurrent_cache.size(), 1100)
  
  // Concurrent gets
  let get_handles = []
  
  for i in 0..=10 {
    let handle = spawn_thread(|| {
      for j in 0..=100 {
        let _ = concurrent_cache.get(format!("concurrent_key_{}_{}", i, j))
      }
    })
    get_handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in get_handles {
    handle.join()
  }
  
  // Verify cache statistics
  let stats = concurrent_cache.get_statistics()
  assert_eq(stats.puts, 1100)
  assert_eq(stats.gets, 1100)
  assert_eq(stats.hits, 1100) // All gets should be hits
  
  // Test async cache implementation
  let async_cache = AsyncCache::with_capacity(1000)
  
  // Async put
  let put_future = async_cache.put_async("async_key", "async_value")
  let put_result = await put_future
  assert_true(put_result.is_success())
  
  // Async get
  let get_future = async_cache.get_async("async_key")
  let get_result = await get_future
  assert_true(get_result.is_some())
  assert_eq(get_result.unwrap(), "async_value")
  
  // Async batch operations
  let batch_put_data = []
  for i in 0..=100 {
    batch_put_data.push((format!("batch_key_{}", i), format!("batch_value_{}", i)))
  }
  
  let batch_put_future = async_cache.put_all_async(batch_put_data)
  let batch_put_result = await batch_put_future
  assert_true(batch_put_result.is_success())
  
  let batch_get_keys = []
  for i in 0..=100 {
    batch_get_keys.push(format!("batch_key_{}", i))
  }
  
  let batch_get_future = async_cache.get_all_async(batch_get_keys)
  let batch_get_result = await batch_get_future
  assert_eq(batch_get_result.length(), 101)
}

// Test 9: Cache Integration Patterns
test "cache integration patterns" {
  // Test cache-aside pattern
  let database = MockDatabase::new()
  let cache = Cache::with_capacity(1000)
  let cache_aside = CacheAside::new(cache, database)
  
  // First get should miss cache and fetch from database
  let get_result = cache_aside.get("user:123")
  assert_true(get_result.is_success())
  assert_eq(get_result.unwrap(), "User 123 data")
  
  // Verify data was cached
  let cache_stats = cache_aside.get_cache_statistics()
  assert_eq(cache_stats.misses, 1)
  assert_eq(cache_stats.puts, 1)
  
  // Second get should hit cache
  let get_result_2 = cache_aside.get("user:123")
  assert_true(get_result_2.is_success())
  assert_eq(get_result_2.unwrap(), "User 123 data")
  
  // Verify cache hit
  let cache_stats_2 = cache_aside.get_cache_statistics()
  assert_eq(cache_stats_2.hits, 1)
  
  // Test write-through pattern
  let write_through_cache = Cache::with_capacity(1000)
  let write_through = WriteThrough::new(write_through_cache, database)
  
  // Write should update both cache and database
  let write_result = write_through.put("user:456", "User 456 data")
  assert_true(write_result.is_success())
  
  // Verify data is in cache
  let cache_value = write_through.get_from_cache("user:456")
  assert_true(cache_value.is_some())
  assert_eq(cache_value.unwrap(), "User 456 data")
  
  // Verify data is in database
  let db_value = database.get("user:456")
  assert_true(db_value.is_some())
  assert_eq(db_value.unwrap(), "User 456 data")
  
  // Test write-behind pattern
  let write_behind_cache = Cache::with_capacity(1000)
  let write_behind = WriteBehind::new(write_behind_cache, database)
    .with_batch_size(10)
    .with_flush_interval(Duration::from_seconds(5))
  
  // Write should update cache immediately and database asynchronously
  let write_result_2 = write_behind.put("user:789", "User 789 data")
  assert_true(write_result_2.is_success())
  
  // Verify data is in cache immediately
  let cache_value_2 = write_behind.get_from_cache("user:789")
  assert_true(cache_value_2.is_some())
  assert_eq(cache_value_2.unwrap(), "User 789 data")
  
  // Database might not have data yet (async write)
  let db_value_2 = database.get("user:789")
  // assert_true(db_value_2.is_none()) // Might not be written yet
  
  // Force flush to database
  write_behind.flush()
  
  // Now data should be in database
  let db_value_3 = database.get("user:789")
  assert_true(db_value_3.is_some())
  assert_eq(db_value_3.unwrap(), "User 789 data")
  
  // Test refresh-ahead pattern
  let refresh_ahead_cache = Cache::with_capacity(1000)
  let refresh_ahead = RefreshAhead::new(refresh_ahead_cache, database)
    .with_refresh_threshold(Duration::from_seconds(10))
  
  // Put data with expiration
  refresh_ahead.put_with_ttl("user:999", "User 999 data", Duration::from_seconds(20))
  
  // Get data
  let get_result_3 = refresh_ahead.get("user:999")
  assert_true(get_result_3.is_success())
  assert_eq(get_result_3.unwrap(), "User 999 data")
  
  // Wait until near expiration
  sleep(Duration::from_seconds(15))
  
  // Get data again, should trigger refresh
  let get_result_4 = refresh_ahead.get("user:999")
  assert_true(get_result_4.is_success())
  assert_eq(get_result_4.unwrap(), "User 999 data")
  
  // Verify refresh occurred
  let refresh_stats = refresh_ahead.get_refresh_statistics()
  assert_true(refresh_stats.refreshes_triggered > 0)
  
  // Test multi-level cache
  let l1_cache = Cache::with_capacity(100)  // L1 (fastest)
  let l2_cache = Cache::with_capacity(1000) // L2 (slower)
  let l3_cache = Cache::with_capacity(10000) // L3 (slowest)
  
  let multi_level = MultiLevelCache::new()
    .add_level(l1_cache, 0.1) // 0.1ms access time
    .add_level(l2_cache, 1.0) // 1.0ms access time
    .add_level(l3_cache, 10.0) // 10.0ms access time
  
  // Put data (should go to L1)
  multi_level.put("multi_key", "multi_value")
  
  // Get data (should hit L1)
  let get_start = get_current_timestamp()
  let get_result_5 = multi_level.get("multi_key")
  let get_end = get_current_timestamp()
  let get_time = get_end - get_start
  
  assert_true(get_result_5.is_some())
  assert_eq(get_result_5.unwrap(), "multi_value")
  assert_true(get_time.to_millis() < 1) // Should be very fast (L1 hit)
  
  // Verify L1 hit
  let level_stats = multi_level.get_level_statistics()
  assert_eq(level_stats[0].hits, 1) // L1 hits
  assert_eq(level_stats[1].hits, 0) // L2 hits
  assert_eq(level_stats[2].hits, 0) // L3 hits
  
  // Test cache with loading cache
  let loading_cache = LoadingCache::with_capacity(1000)
    .with_loader(|key| {
      if key.starts_with("user:") {
        Some(format!("Loaded data for {}", key))
      } else {
        None
      }
    })
  
  // Get non-existent key (should trigger loader)
  let load_result = loading_cache.get("user:123")
  assert_true(load_result.is_some())
  assert_eq(load_result.unwrap(), "Loaded data for user:123")
  
  // Get same key again (should hit cache)
  let load_result_2 = loading_cache.get("user:123")
  assert_true(load_result_2.is_some())
  assert_eq(load_result_2.unwrap(), "Loaded data for user:123")
  
  // Get key that loader can't handle
  let load_result_3 = loading_cache.get("unknown:123")
  assert_true(load_result_3.is_none())
  
  // Test cache with invalidation
  let invalidation_cache = Cache::with_capacity(1000)
  let invalidation_manager = InvalidationManager::new()
  
  // Register invalidation callbacks
  invalidation_manager.register_callback("user:*", |key| {
    println!("Invalidating user cache: {}", key)
  })
  
  invalidation_manager.register_callback("product:*", |key| {
    println!("Invalidating product cache: {}", key)
  })
  
  let cache_with_invalidation = CacheWithInvalidation::new(invalidation_cache, invalidation_manager)
  
  // Put data
  cache_with_invalidation.put("user:123", "User 123 data")
  cache_with_invalidation.put("product:456", "Product 456 data")
  
  // Invalidate by pattern
  cache_with_invalidation.invalidate_pattern("user:*")
  
  // Verify user data was invalidated
  assert_false(cache_with_invalidation.contains_key("user:123"))
  assert_true(cache_with_invalidation.contains_key("product:456")) // Should still exist
  
  // Test cache with versioning
  let versioned_cache = VersionedCache::with_capacity(1000)
  
  // Put version 1
  let put_result_3 = versioned_cache.put("versioned_key", "value_v1")
  assert_true(put_result_3.is_success())
  assert_eq(put_result_3.unwrap().version, 1)
  
  // Get version 1
  let get_result_6 = versioned_cache.get("versioned_key")
  assert_true(get_result_6.is_some())
  assert_eq(get_result_6.unwrap().value, "value_v1")
  assert_eq(get_result_6.unwrap().version, 1)
  
  // Put version 2
  let put_result_4 = versioned_cache.put("versioned_key", "value_v2")
  assert_true(put_result_4.is_success())
  assert_eq(put_result_4.unwrap().version, 2)
  
  // Get version 2
  let get_result_7 = versioned_cache.get("versioned_key")
  assert_true(get_result_7.is_some())
  assert_eq(get_result_7.unwrap().value, "value_v2")
  assert_eq(get_result_7.unwrap().version, 2)
  
  // Get specific version
  let get_v1_result = versioned_cache.get_version("versioned_key", 1)
  assert_true(get_v1_result.is_some())
  assert_eq(get_v1_result.unwrap().value, "value_v1")
  assert_eq(get_v1_result.unwrap().version, 1)
  
  // Test cache with transaction support
  let transactional_cache = TransactionalCache::with_capacity(1000)
  
  // Start transaction
  let transaction = transactional_cache.begin_transaction()
  
  // Put operations within transaction
  transaction.put("tx_key_1", "tx_value_1")
  transaction.put("tx_key_2", "tx_value_2")
  
  // Verify operations are not visible outside transaction
  assert_false(transactional_cache.contains_key("tx_key_1"))
  assert_false(transactional_cache.contains_key("tx_key_2"))
  
  // Commit transaction
  let commit_result = transaction.commit()
  assert_true(commit_result.is_success())
  
  // Verify operations are now visible
  assert_true(transactional_cache.contains_key("tx_key_1"))
  assert_true(transactional_cache.contains_key("tx_key_2"))
  assert_eq(transactional_cache.get("tx_key_1").unwrap(), "tx_value_1")
  assert_eq(transactional_cache.get("tx_key_2").unwrap(), "tx_value_2")
  
  // Test rollback
  let transaction_2 = transactional_cache.begin_transaction()
  transaction_2.put("tx_key_3", "tx_value_3")
  transaction_2.remove("tx_key_1")
  
  // Verify operations within transaction
  assert_eq(transaction_2.get("tx_key_3").unwrap(), "tx_value_3")
  assert_true(transaction_2.get("tx_key_1").is_none())
  
  // Rollback transaction
  transaction_2.rollback()
  
  // Verify rollback worked
  assert_false(transactional_cache.contains_key("tx_key_3"))
  assert_true(transactional_cache.contains_key("tx_key_1")) // Should still exist
}

// Test 10: Cache Advanced Features
test "cache advanced features" {
  // Test cache with predictive preloading
  let predictive_cache = Cache::with_capacity(1000)
  let predictor = AccessPatternPredictor::new()
  
  // Train predictor with access patterns
  for i in 0..=100 {
    // Simulate sequential access pattern
    predictor.record_access(format!("predictive_key_{}", i))
  }
  
  let cache_with_prediction = PredictiveCache::new(predictive_cache, predictor)
  
  // Access first key, should trigger prediction
  let get_result = cache_with_prediction.get("predictive_key_0")
  assert_true(get_result.is_none()) // Not in cache yet
  
  // Predictor should pre-load next keys based on pattern
  sleep(Duration::from_millis(100)) // Allow time for preloading
  
  // Verify predicted keys are pre-loaded
  assert_true(cache_with_prediction.contains_key("predictive_key_1"))
  assert_true(cache_with_prediction.contains_key("predictive_key_2"))
  
  // Test cache with machine learning eviction
  let ml_cache = Cache::with_capacity(100)
  let ml_eviction_policy = MLEvictionPolicy::new()
    .with_model_type(ModelType::DecisionTree)
    .with_training_data(generate_training_data())
  
  let cache_with_ml_eviction = Cache::with_capacity(100)
    .with_eviction_policy(EvictionPolicy::ML(ml_eviction_policy))
  
  // Fill cache
  for i in 0..=100 {
    cache_with_ml_eviction.put(format!("ml_key_{}", i), format!("ml_value_{}", i))
  }
  
  // Access some keys to train the model
  for i in 0..=50 {
    let _ = cache_with_ml_eviction.get(format!("ml_key_{}", i));
  }
  
  // Add new key, ML model should make intelligent eviction decision
  cache_with_ml_eviction.put("new_ml_key", "new_ml_value")
  
  // Verify ML eviction behavior (should keep frequently accessed keys)
  assert_true(cache_with_ml_eviction.contains_key("ml_key_0")) // Frequently accessed
  assert_true(cache_with_ml_eviction.contains_key("new_ml_key")) // New key
  
  // Test cache with adaptive sizing
  let adaptive_cache = AdaptiveCache::with_initial_capacity(100)
    .with_min_capacity(50)
    .with_max_capacity(500)
    .with_growth_threshold(0.8) // Grow when 80% full
    .with_shrink_threshold(0.2) // Shrink when 20% full
  
  // Fill cache to trigger growth
  for i in 0..=90 {
    adaptive_cache.put(format!("adaptive_key_{}", i), format!("adaptive_value_{}", i))
  }
  
  let capacity_after_growth = adaptive_cache.capacity()
  assert_true(capacity_after_growth > 100) // Should have grown
  
  // Remove most entries to trigger shrink
  for i in 0..=80 {
    adaptive_cache.remove(format!("adaptive_key_{}", i))
  }
  
  // Wait for resize
  sleep(Duration::from_millis(100))
  
  let capacity_after_shrink = adaptive_cache.capacity()
  assert_true(capacity_after_shrink < capacity_after_growth) // Should have shrunk
  
  // Test cache with hot data detection
  let hot_data_cache = HotDataCache::with_capacity(1000)
    .with_hot_threshold(10) // Accessed 10+ times to be hot
    .with_hot_data_ratio(0.2) // 20% of capacity for hot data
  
  // Fill cache
  for i in 0..=1000 {
    hot_data_cache.put(format!("hot_key_{}", i), format!("hot_value_{}", i))
  }
  
  // Access some keys frequently to make them hot
  for i in 0..=50 {
    for j in 0..=15 {
      let _ = hot_data_cache.get(format!("hot_key_{}", i));
    }
  }
  
  // Verify hot data detection
  let hot_data = hot_data_cache.get_hot_data()
  assert_true(hot_data.length() > 0)
  
  for key in hot_data {
    assert_true(key.starts_with("hot_key_"))
    let key_num = key.split("_")[2].to_int()
    assert_true(key_num <= 50) // Should be from frequently accessed keys
  }
  
  // Test cache with geographic distribution
  let geo_cache = GeoDistributedCache::new()
    .add_region("us-east", Cache::with_capacity(1000))
    .add_region("us-west", Cache::with_capacity(1000))
    .add_region("eu-west", Cache::with_capacity(1000))
    .add_region("asia-pacific", Cache::with_capacity(1000))
  
  // Put data with locality hint
  let put_result = geo_cache.put_with_locality("geo_key", "geo_value", "us-east")
  assert_true(put_result.is_success())
  
  // Get data from nearest region
  let get_result = geo_cache.get_from_nearest("geo_key", "us-east")
  assert_true(get_result.is_success())
  assert_eq(get_result.unwrap().value, "geo_value")
  assert_eq(get_result.unwrap().region, "us-east") // Should come from nearest region
  
  // Test cache with hierarchical organization
  let hierarchical_cache = HierarchicalCache::new()
    .add_level("memory", Cache::with_capacity(1000))  // L1
    .add_level("ssd", Cache::with_capacity(10000))    // L2
    .add_level("disk", Cache::with_capacity(100000))  // L3
    .add_level("network", Cache::with_capacity(1000000)) // L4
  
  // Put data (should go to L1)
  hierarchical_cache.put("hier_key", "hier_value")
  
  // Get data (should hit L1)
  let get_result_2 = hierarchical_cache.get("hier_key")
  assert_true(get_result_2.is_some())
  assert_eq(get_result_2.unwrap(), "hier_value")
  
  // Verify level statistics
  let level_stats = hierarchical_cache.get_level_statistics()
  assert_eq(level_stats.get("memory").unwrap().hits, 1) // L1 hit
  
  // Test cache with compression
  let compression_cache = Cache::with_capacity(1000)
    .with_compression(true)
    .with_compression_algorithm(CompressionAlgorithm::LZ4)
    .with_compression_threshold(100) // Compress values larger than 100 bytes
  
  // Add small value (should not be compressed)
  compression_cache.put("small_key", "small_value")
  
  // Add large value (should be compressed)
  compression_cache.put("large_key", "x".repeat(1000))
  
  // Verify compression statistics
  let compression_stats = compression_cache.get_compression_statistics()
  assert_eq(compression_stats.uncompressed_entries, 1)
  assert_eq(compression_stats.compressed_entries, 1)
  assert_true(compression_stats.compression_ratio < 1.0) // Should be less than 1 (compressed)
  
  // Test cache with encryption
  let encryption_key = generate_encryption_key()
  let encrypted_cache = Cache::with_capacity(1000)
    .with_encryption(true)
    .with_encryption_key(encryption_key)
    .with_encryption_algorithm(EncryptionAlgorithm::ChaCha20)
  
  // Add sensitive data
  encrypted_cache.put("secret_key", "secret_value")
  
  // Retrieve data
  let get_result_3 = encrypted_cache.get("secret_key")
  assert_true(get_result_3.is_some())
  assert_eq(get_result_3.unwrap(), "secret_value")
  
  // Verify data is encrypted in storage
  let raw_storage = encrypted_cache.get_raw_storage()
  assert_false(raw_storage.contains("secret_value")) // Should not contain plain text
  
  // Test cache with event sourcing
  let event_sourced_cache = EventSourcedCache::with_capacity(1000)
  
  // Put data
  let put_result_2 = event_sourced_cache.put("event_key", "event_value")
  assert_true(put_result_2.is_success())
  
  // Get data
  let get_result_4 = event_sourced_cache.get("event_key")
  assert_true(get_result_4.is_some())
  assert_eq(get_result_4.unwrap(), "event_value")
  
  // Get event history
  let events = event_sourced_cache.get_event_history("event_key")
  assert_true(events.length() > 0)
  
  let put_event = events.find(|event| event.event_type == "put")
  assert_true(put_event.is_some())
  assert_eq(put_event.unwrap().key, "event_key")
  
  // Replay events to reconstruct state
  let reconstructed_cache = EventSourcedCache::with_capacity(1000)
  let replay_result = reconstructed_cache.replay_events(events)
  assert_true(replay_result.is_success())
  
  let replayed_value = reconstructed_cache.get("event_key")
  assert_true(replayed_value.is_some())
  assert_eq(replayed_value.unwrap(), "event_value")
}