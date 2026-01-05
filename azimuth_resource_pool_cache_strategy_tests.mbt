// Azimuth Resource Pool and Cache Strategy Test Suite
// This file contains tests for resource pool management and caching strategies

// Test 1: Basic Resource Pool Management
test "basic resource pool management" {
  // Resource pool structure
  type ResourcePool = {
    resources: Array[String],
    available: Array[Bool],
    max_size: Int,
    current_size: Int
  }
  
  // Create resource pool
  let create_pool = fn(max_size: Int) {
    {
      resources: [],
      available: [],
      max_size,
      current_size: 0
    }
  }
  
  // Add resource to pool
  let add_resource = fn(pool: ResourcePool, resource: String) {
    if pool.current_size >= pool.max_size {
      pool  // Pool is full
    } else {
      {
        resources: pool.resources.push(resource),
        available: pool.available.push(true),
        max_size: pool.max_size,
        current_size: pool.current_size + 1
      }
    }
  }
  
  // Acquire resource from pool
  let acquire_resource = fn(pool: ResourcePool) {
    let mut updated_pool = pool
    let mut acquired_resource = None
    
    for i in 0..pool.resources.length() {
      if pool.available[i] {
        acquired_resource = Some(pool.resources[i])
        updated_pool.available[i] = false
        break
      }
    }
    
    (updated_pool, acquired_resource)
  }
  
  // Release resource back to pool
  let release_resource = fn(pool: ResourcePool, resource: String) {
    let mut updated_pool = pool
    let mut found = false
    
    for i in 0..pool.resources.length() {
      if pool.resources[i] == resource and not(pool.available[i]) {
        updated_pool.available[i] = true
        found = true
        break
      }
    }
    
    if found {
      updated_pool
    } else {
      pool  // Resource not found or already available
    }
  }
  
  // Get pool statistics
  let pool_stats = fn(pool: ResourcePool) {
    let available_count = pool.available.filter(fn(available) { available }).length()
    
    {
      total_resources: pool.current_size,
      available_resources: available_count,
      used_resources: pool.current_size - available_count,
      utilization_rate: if pool.current_size > 0 {
        (pool.current_size - available_count).to_float() / pool.current_size.to_float()
      } else {
        0.0
      }
    }
  }
  
  // Test pool creation
  let pool = create_pool(5)
  assert_eq(pool.max_size, 5)
  assert_eq(pool.current_size, 0)
  assert_eq(pool.resources.length(), 0)
  
  // Test adding resources
  let pool1 = add_resource(pool, "resource-1")
  assert_eq(pool1.current_size, 1)
  assert_eq(pool1.resources.length(), 1)
  assert_eq(pool1.resources[0], "resource-1")
  assert_true(pool1.available[0])
  
  let pool2 = add_resource(pool1, "resource-2")
  let pool3 = add_resource(pool2, "resource-3")
  assert_eq(pool3.current_size, 3)
  assert_eq(pool3.resources.length(), 3)
  
  // Test acquiring resources
  let (pool4, acquired1) = acquire_resource(pool3)
  assert_true(acquired1.is_some())
  assert_eq(acquired1.unwrap(), "resource-1")
  assert_false(pool4.available[0])
  
  let (pool5, acquired2) = acquire_resource(pool4)
  assert_true(acquired2.is_some())
  assert_eq(acquired2.unwrap(), "resource-2")
  assert_false(pool5.available[1])
  
  // Test releasing resources
  let pool6 = release_resource(pool5, "resource-1")
  assert_true(pool6.available[0])
  
  // Test pool statistics
  let stats = pool_stats(pool6)
  assert_eq(stats.total_resources, 3)
  assert_eq(stats.available_resources, 2)
  assert_eq(stats.used_resources, 1)
  assert_eq(stats.utilization_rate, 1.0 / 3.0)
  
  // Test full pool
  let pool7 = add_resource(add_resource(add_resource(pool6, "resource-4"), "resource-5"), "resource-6")
  assert_eq(pool7.current_size, 5)  // Should not exceed max_size
  
  // Test acquiring from empty pool
  let empty_pool = create_pool(3)
  let (_, empty_acquired) = acquire_resource(empty_pool)
  assert_true(empty_acquired.is_none())
}

// Test 2: LRU Cache Implementation
test "lru cache implementation" {
  // Cache entry
  type CacheEntry = {
    key: String,
    value: String,
    access_time: Int,
    access_count: Int
  }
  
  // LRU cache structure
  type LRUCache = {
    entries: Array[CacheEntry],
    max_size: Int,
    current_time: Int
  }
  
  // Create LRU cache
  let create_lru_cache = fn(max_size: Int) {
    {
      entries: [],
      max_size,
      current_time: 0
    }
  }
  
  // Get entry from cache
  let get = fn(cache: LRUCache, key: String) {
    let mut updated_cache = { cache | current_time: cache.current_time + 1 }
    let mut found_value = None
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        found_value = Some(cache.entries[i].value)
        updated_cache.entries[i] = {
          cache.entries[i] |
          access_time: updated_cache.current_time,
          access_count: cache.entries[i].access_count + 1
        }
        break
      }
    }
    
    (updated_cache, found_value)
  }
  
  // Put entry in cache
  let put = fn(cache: LRUCache, key: String, value: String) {
    let mut updated_cache = { cache | current_time: cache.current_time + 1 }
    
    // Check if key already exists
    let mut key_exists = false
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        updated_cache.entries[i] = {
          key,
          value,
          access_time: updated_cache.current_time,
          access_count: cache.entries[i].access_count + 1
        }
        key_exists = true
        break
      }
    }
    
    if key_exists {
      updated_cache
    } else if cache.entries.length() < cache.max_size {
      // Add new entry
      {
        updated_cache |
        entries: updated_cache.entries.push({
          key,
          value,
          access_time: updated_cache.current_time,
          access_count: 1
        })
      }
    } else {
      // Remove LRU entry and add new one
      let mut oldest_time = updated_cache.current_time
      let mut oldest_index = 0
      
      for i in 0..updated_cache.entries.length() {
        if updated_cache.entries[i].access_time < oldest_time {
          oldest_time = updated_cache.entries[i].access_time
          oldest_index = i
        }
      }
      
      let mut new_entries = []
      for i in 0..updated_cache.entries.length() {
        if i != oldest_index {
          new_entries = new_entries.push(updated_cache.entries[i])
        }
      }
      
      {
        updated_cache |
        entries: new_entries.push({
          key,
          value,
          access_time: updated_cache.current_time,
          access_count: 1
        })
      }
    }
  }
  
  // Get cache statistics
  let cache_stats = fn(cache: LRUCache) {
    {
      size: cache.entries.length(),
      max_size: cache.max_size,
      utilization: cache.entries.length().to_float() / cache.max_size.to_float(),
      total_accesses: cache.entries.reduce(fn(acc, entry) { acc + entry.access_count }, 0)
    }
  }
  
  // Test cache creation and basic operations
  let cache = create_lru_cache(3)
  assert_eq(cache.max_size, 3)
  assert_eq(cache.entries.length(), 0)
  
  // Test putting values
  let cache1 = put(cache, "key1", "value1")
  assert_eq(cache1.entries.length(), 1)
  
  let cache2 = put(cache1, "key2", "value2")
  assert_eq(cache2.entries.length(), 2)
  
  let cache3 = put(cache2, "key3", "value3")
  assert_eq(cache3.entries.length(), 3)
  
  // Test getting values
  let (cache4, value1) = get(cache3, "key1")
  assert_true(value1.is_some())
  assert_eq(value1.unwrap(), "value1")
  
  // Test updating existing key
  let cache5 = put(cache4, "key1", "new-value1")
  let (cache6, updated_value) = get(cache5, "key1")
  assert_eq(updated_value.unwrap(), "new-value1")
  assert_eq(cache6.entries.length(), 3)  // Size shouldn't change
  
  // Test LRU eviction
  let cache7 = put(cache6, "key4", "value4")
  assert_eq(cache7.entries.length(), 3)  // Should still be 3
  
  // key3 should be evicted (least recently used)
  let (_, evicted_value) = get(cache7, "key3")
  assert_true(evicted_value.is_none())
  
  // key1, key2, and key4 should be present
  let (_, value1_check) = get(cache7, "key1")
  let (_, value2_check) = get(cache7, "key2")
  let (_, value4_check) = get(cache7, "key4")
  
  assert_true(value1_check.is_some())
  assert_true(value2_check.is_some())
  assert_true(value4_check.is_some())
  
  // Test cache statistics
  let stats = cache_stats(cache7)
  assert_eq(stats.size, 3)
  assert_eq(stats.max_size, 3)
  assert_eq(stats.utilization, 1.0)
  assert_true(stats.total_accesses > 0)
}

// Test 3: Cache Eviction Strategies
test "cache eviction strategies" {
  // Cache entry
  type CacheEntry = {
    key: String,
    value: String,
    access_time: Int,
    creation_time: Int,
    access_count: Int
  }
  
  // Eviction strategy enum
  enum EvictionStrategy {
    LRU  // Least Recently Used
    LFU  // Least Frequently Used
    FIFO // First In First Out
  }
  
  // Cache structure
  type Cache = {
    entries: Array[CacheEntry],
    max_size: Int,
    eviction_strategy: EvictionStrategy,
    current_time: Int
  }
  
  // Create cache with eviction strategy
  let create_cache = fn(max_size: Int, strategy: EvictionStrategy) {
    {
      entries: [],
      max_size,
      eviction_strategy: strategy,
      current_time: 0
    }
  }
  
  // Find index of entry to evict
  let find_eviction_index = fn(cache: Cache) {
    match cache.eviction_strategy {
      EvictionStrategy::LRU => {
        let mut oldest_time = cache.current_time
        let mut oldest_index = 0
        
        for i in 0..cache.entries.length() {
          if cache.entries[i].access_time < oldest_time {
            oldest_time = cache.entries[i].access_time
            oldest_index = i
          }
        }
        
        oldest_index
      }
      EvictionStrategy::LFU => {
        let mut min_count = cache.entries[0].access_count
        let mut min_index = 0
        
        for i in 0..cache.entries.length() {
          if cache.entries[i].access_count < min_count {
            min_count = cache.entries[i].access_count
            min_index = i
          }
        }
        
        min_index
      }
      EvictionStrategy::FIFO => {
        let mut oldest_time = cache.current_time
        let mut oldest_index = 0
        
        for i in 0..cache.entries.length() {
          if cache.entries[i].creation_time < oldest_time {
            oldest_time = cache.entries[i].creation_time
            oldest_index = i
          }
        }
        
        oldest_index
      }
    }
  }
  
  // Put entry in cache
  let put = fn(cache: Cache, key: String, value: String) {
    let mut updated_cache = { cache | current_time: cache.current_time + 1 }
    
    // Check if key already exists
    let mut key_exists = false
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        updated_cache.entries[i] = {
          cache.entries[i] |
          value,
          access_time: updated_cache.current_time,
          access_count: cache.entries[i].access_count + 1
        }
        key_exists = true
        break
      }
    }
    
    if key_exists {
      updated_cache
    } else if cache.entries.length() < cache.max_size {
      // Add new entry
      {
        updated_cache |
        entries: updated_cache.entries.push({
          key,
          value,
          access_time: updated_cache.current_time,
          creation_time: updated_cache.current_time,
          access_count: 1
        })
      }
    } else {
      // Remove entry based on eviction strategy and add new one
      let eviction_index = find_eviction_index(updated_cache)
      
      let mut new_entries = []
      for i in 0..updated_cache.entries.length() {
        if i != eviction_index {
          new_entries = new_entries.push(updated_cache.entries[i])
        }
      }
      
      {
        updated_cache |
        entries: new_entries.push({
          key,
          value,
          access_time: updated_cache.current_time,
          creation_time: updated_cache.current_time,
          access_count: 1
        })
      }
    }
  }
  
  // Get entry from cache
  let get = fn(cache: Cache, key: String) {
    let mut updated_cache = { cache | current_time: cache.current_time + 1 }
    let mut found_value = None
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        found_value = Some(cache.entries[i].value)
        updated_cache.entries[i] = {
          cache.entries[i] |
          access_time: updated_cache.current_time,
          access_count: cache.entries[i].access_count + 1
        }
        break
      }
    }
    
    (updated_cache, found_value)
  }
  
  // Test LRU eviction
  let lru_cache = create_cache(3, EvictionStrategy::LRU)
  
  // Add entries
  let lru1 = put(lru_cache, "key1", "value1")
  let lru2 = put(lru1, "key2", "value2")
  let lru3 = put(lru2, "key3", "value3")
  
  // Access key1 to make it most recently used
  let (lru4, _) = get(lru3, "key1")
  
  // Add key4, should evict key2 (least recently used)
  let lru5 = put(lru4, "key4", "value4")
  
  // Check eviction
  let (_, key1_value) = get(lru5, "key1")  // Should exist
  let (_, key2_value) = get(lru5, "key2")  // Should be evicted
  let (_, key3_value) = get(lru5, "key3")  // Should exist
  let (_, key4_value) = get(lru5, "key4")  // Should exist
  
  assert_true(key1_value.is_some())
  assert_true(key2_value.is_none())
  assert_true(key3_value.is_some())
  assert_true(key4_value.is_some())
  
  // Test LFU eviction
  let lfu_cache = create_cache(3, EvictionStrategy::LFU)
  
  // Add entries
  let lfu1 = put(lfu_cache, "key1", "value1")
  let lfu2 = put(lfu1, "key2", "value2")
  let lfu3 = put(lfu2, "key3", "value3")
  
  // Access key1 multiple times to increase its count
  let (lfu4, _) = get(lfu3, "key1")
  let (lfu5, _) = get(lfu4, "key1")
  let (lfu6, _) = get(lfu5, "key1")
  
  // Access key2 once
  let (lfu7, _) = get(lfu6, "key2")
  
  // Add key4, should evict key3 (least frequently used)
  let lfu8 = put(lfu7, "key4", "value4")
  
  // Check eviction
  let (_, key1_value) = get(lfu8, "key1")  // Should exist
  let (_, key2_value) = get(lfu8, "key2")  // Should exist
  let (_, key3_value) = get(lfu8, "key3")  // Should be evicted
  let (_, key4_value) = get(lfu8, "key4")  // Should exist
  
  assert_true(key1_value.is_some())
  assert_true(key2_value.is_some())
  assert_true(key3_value.is_none())
  assert_true(key4_value.is_some())
  
  // Test FIFO eviction
  let fifo_cache = create_cache(3, EvictionStrategy::FIFO)
  
  // Add entries
  let fifo1 = put(fifo_cache, "key1", "value1")
  let fifo2 = put(fifo1, "key2", "value2")
  let fifo3 = put(fifo2, "key3", "value3")
  
  // Access key1 to make it recently used (but it's still first in)
  let (fifo4, _) = get(fifo3, "key1")
  
  // Add key4, should evict key1 (first in)
  let fifo5 = put(fifo4, "key4", "value4")
  
  // Check eviction
  let (_, key1_value) = get(fifo5, "key1")  // Should be evicted
  let (_, key2_value) = get(fifo5, "key2")  // Should exist
  let (_, key3_value) = get(fifo5, "key3")  // Should exist
  let (_, key4_value) = get(fifo5, "key4")  // Should exist
  
  assert_true(key1_value.is_none())
  assert_true(key2_value.is_some())
  assert_true(key3_value.is_some())
  assert_true(key4_value.is_some())
}

// Test 4: Cache Invalidation Strategies
test "cache invalidation strategies" {
  // Cache entry
  type CacheEntry = {
    key: String,
    value: String,
    ttl: Int,  // Time to live in seconds
    creation_time: Int
  }
  
  // Cache structure
  type TTLCache = {
    entries: Array[CacheEntry],
    current_time: Int
  }
  
  // Create TTL cache
  let create_ttl_cache = fn() {
    {
      entries: [],
      current_time: 0
    }
  }
  
  // Put entry with TTL
  let put = fn(cache: TTLCache, key: String, value: String, ttl: Int) {
    let mut updated_cache = { cache | current_time: cache.current_time + 1 }
    
    // Check if key already exists
    let mut key_exists = false
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        updated_cache.entries[i] = {
          key,
          value,
          ttl,
          creation_time: updated_cache.current_time
        }
        key_exists = true
        break
      }
    }
    
    if key_exists {
      updated_cache
    } else {
      {
        updated_cache |
        entries: updated_cache.entries.push({
          key,
          value,
          ttl,
          creation_time: updated_cache.current_time
        })
      }
    }
  }
  
  // Get entry with TTL validation
  let get = fn(cache: TTLCache, key: String) {
    let mut updated_cache = cache
    let mut found_value = None
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        let entry = cache.entries[i]
        let age = cache.current_time - entry.creation_time
        
        if age < entry.ttl {
          found_value = Some(entry.value)
        } else {
          // Entry expired, remove it
          let mut new_entries = []
          for j in 0..cache.entries.length() {
            if j != i {
              new_entries = new_entries.push(cache.entries[j])
            }
          }
          updated_cache = { updated_cache | entries: new_entries }
        }
        break
      }
    }
    
    (updated_cache, found_value)
  }
  
  // Clean up expired entries
  let cleanup = fn(cache: TTLCache) {
    let mut valid_entries = []
    
    for entry in cache.entries {
      let age = cache.current_time - entry.creation_time
      if age < entry.ttl {
        valid_entries = valid_entries.push(entry)
      }
    }
    
    { cache | entries: valid_entries }
  }
  
  // Advance time
  let advance_time = fn(cache: TTLCache, seconds: Int) {
    { cache | current_time: cache.current_time + seconds }
  }
  
  // Test TTL cache
  let cache = create_ttl_cache()
  
  // Add entries with different TTLs
  let cache1 = put(cache, "key1", "value1", 10)  // 10 seconds TTL
  let cache2 = put(cache1, "key2", "value2", 5)   // 5 seconds TTL
  let cache3 = put(cache2, "key3", "value3", 15)  // 15 seconds TTL
  
  // All entries should be valid
  let (cache4, value1) = get(cache3, "key1")
  let (cache5, value2) = get(cache4, "key2")
  let (cache6, value3) = get(cache5, "key3")
  
  assert_true(value1.is_some())
  assert_true(value2.is_some())
  assert_true(value3.is_some())
  
  // Advance time by 6 seconds (key2 should expire)
  let cache7 = advance_time(cache6, 6)
  
  // Check entries after time advance
  let (cache8, value1_after) = get(cache7, "key1")  // Should still be valid
  let (cache9, value2_after) = get(cache7, "key2")  // Should be expired
  let (cache10, value3_after) = get(cache7, "key3") // Should still be valid
  
  assert_true(value1_after.is_some())
  assert_true(value2_after.is_none())
  assert_true(value3_after.is_some())
  
  // Advance time by another 5 seconds (key1 should expire)
  let cache11 = advance_time(cache10, 5)
  
  // Check entries after more time
  let (cache12, value1_after2) = get(cache11, "key1")  // Should be expired
  let (cache13, value3_after2) = get(cache11, "key3")  // Should still be valid
  
  assert_true(value1_after2.is_none())
  assert_true(value3_after2.is_some())
  
  // Test cleanup
  let cache14 = advance_time(cache13, 10)  // All entries should be expired
  let cache15 = cleanup(cache14)
  
  assert_eq(cache15.entries.length(), 0)
  
  // Test updating TTL
  let cache16 = put(cache15, "key1", "value1", 5)
  let cache17 = advance_time(cache16, 3)
  
  // Update key1 with new TTL
  let cache18 = put(cache17, "key1", "new-value1", 10)
  let cache19 = advance_time(cache18, 4)  // Total 7 seconds from original
  
  // Original TTL would have expired, but updated one should still be valid
  let (cache20, value1_updated) = get(cache19, "key1")
  assert_true(value1_updated.is_some())
  assert_eq(value1_updated.unwrap(), "new-value1")
}

// Test 5: Cache Performance Metrics
test "cache performance metrics" {
  // Cache entry
  type CacheEntry = {
    key: String,
    value: String,
    access_time: Int
  }
  
  // Cache metrics
  type CacheMetrics = {
    hits: Int,
    misses: Int,
    total_requests: Int,
    hit_rate: Float
  }
  
  // Cache structure
  type Cache = {
    entries: Array[CacheEntry],
    max_size: Int,
    metrics: CacheMetrics,
    current_time: Int
  }
  
  // Create cache with metrics
  let create_cache = fn(max_size: Int) {
    {
      entries: [],
      max_size,
      metrics: {
        hits: 0,
        misses: 0,
        total_requests: 0,
        hit_rate: 0.0
      },
      current_time: 0
    }
  }
  
  // Update metrics
  let update_metrics = fn(metrics: CacheMetrics, hit: Bool) {
    let new_hits = if hit { metrics.hits + 1 } else { metrics.hits }
    let new_misses = if hit { metrics.misses } else { metrics.misses + 1 }
    let new_total = metrics.total_requests + 1
    let new_hit_rate = new_hits.to_float() / new_total.to_float()
    
    {
      hits: new_hits,
      misses: new_misses,
      total_requests: new_total,
      hit_rate: new_hit_rate
    }
  }
  
  // Get entry with metrics tracking
  let get = fn(cache: Cache, key: String) {
    let mut updated_cache = { cache | current_time: cache.current_time + 1 }
    let mut found_value = None
    let mut hit = false
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        found_value = Some(cache.entries[i].value)
        updated_cache.entries[i] = {
          cache.entries[i] |
          access_time: updated_cache.current_time
        }
        hit = true
        break
      }
    }
    
    updated_cache.metrics = update_metrics(cache.metrics, hit)
    
    (updated_cache, found_value)
  }
  
  // Put entry
  let put = fn(cache: Cache, key: String, value: String) {
    let mut updated_cache = { cache | current_time: cache.current_time + 1 }
    
    // Check if key already exists
    let mut key_exists = false
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        updated_cache.entries[i] = {
          key,
          value,
          access_time: updated_cache.current_time
        }
        key_exists = true
        break
      }
    }
    
    if key_exists {
      updated_cache
    } else if cache.entries.length() < cache.max_size {
      {
        updated_cache |
        entries: updated_cache.entries.push({
          key,
          value,
          access_time: updated_cache.current_time
        })
      }
    } else {
      // Remove LRU entry and add new one
      let mut oldest_time = updated_cache.current_time
      let mut oldest_index = 0
      
      for i in 0..updated_cache.entries.length() {
        if updated_cache.entries[i].access_time < oldest_time {
          oldest_time = updated_cache.entries[i].access_time
          oldest_index = i
        }
      }
      
      let mut new_entries = []
      for i in 0..updated_cache.entries.length() {
        if i != oldest_index {
          new_entries = new_entries.push(updated_cache.entries[i])
        }
      }
      
      {
        updated_cache |
        entries: new_entries.push({
          key,
          value,
          access_time: updated_cache.current_time
        })
      }
    }
  }
  
  // Test cache metrics
  let cache = create_cache(3)
  assert_eq(cache.metrics.hits, 0)
  assert_eq(cache.metrics.misses, 0)
  assert_eq(cache.metrics.total_requests, 0)
  assert_eq(cache.metrics.hit_rate, 0.0)
  
  // Add entries
  let cache1 = put(cache, "key1", "value1")
  let cache2 = put(cache1, "key2", "value2")
  let cache3 = put(cache2, "key3", "value3")
  
  // Get existing entries (should be hits)
  let (cache4, value1) = get(cache3, "key1")
  let (cache5, value2) = get(cache4, "key2")
  
  assert_true(value1.is_some())
  assert_true(value2.is_some())
  
  // Check metrics after hits
  assert_eq(cache5.metrics.hits, 2)
  assert_eq(cache5.metrics.misses, 0)
  assert_eq(cache5.metrics.total_requests, 2)
  assert_eq(cache5.metrics.hit_rate, 1.0)
  
  // Get non-existing entry (should be miss)
  let (cache6, missing_value) = get(cache5, "missing-key")
  assert_true(missing_value.is_none())
  
  // Check metrics after miss
  assert_eq(cache6.metrics.hits, 2)
  assert_eq(cache6.metrics.misses, 1)
  assert_eq(cache6.metrics.total_requests, 3)
  assert_eq(cache6.metrics.hit_rate, 2.0 / 3.0)
  
  // Add more entries (some will be evicted)
  let cache7 = put(cache6, "key4", "value4")  // Should evict one entry
  let cache8 = put(cache7, "key5", "value5")  // Should evict another
  
  // Try to get evicted entries (should be misses)
  let (cache9, evicted_value) = get(cache8, "key1")  // Might be evicted
  let (cache10, existing_value) = get(cache9, "key2")  // Might still exist
  
  // Check final metrics
  assert_true(cache10.metrics.total_requests > 3)
  assert_true(cache10.metrics.hit_rate >= 0.0 and cache10.metrics.hit_rate <= 1.0)
  
  // Test metrics calculation
  let total_requests = cache10.metrics.hits + cache10.metrics.misses
  assert_eq(total_requests, cache10.metrics.total_requests)
  
  let calculated_hit_rate = if total_requests > 0 {
    cache10.metrics.hits.to_float() / total_requests.to_float()
  } else {
    0.0
  }
  
  assert_eq(calculated_hit_rate, cache10.metrics.hit_rate)
}