// Azimuth Caching Mechanism Test Suite
// This file contains test cases for caching mechanisms in Azimuth telemetry system

// Test 1: LRU Cache Implementation
test "LRU cache implementation for telemetry data" {
  // Define cache entry
  type CacheEntry = {
    key: String,
    value: String,
    access_time: Int
  }
  
  // Define LRU cache
  type LRUCache = {
    entries: Array[CacheEntry>,
    capacity: Int,
    current_time: Int
  }
  
  // Create LRU cache
  let create_lru_cache = fn(capacity: Int) {
    {
      entries: [],
      capacity,
      current_time: 0
    }
  }
  
  // Get value from cache
  let lru_get = fn(cache: LRUCache, key: String) {
    let mut found = false
    let mut value = None
    let mut found_index = -1
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        found = true
        value = Some(cache.entries[i].value)
        found_index = i
      }
    }
    
    if found {
      // Move accessed entry to the end (most recently used)
      let mut updated_entries = []
      for i in 0..cache.entries.length() {
        if i != found_index {
          updated_entries = updated_entries.push(cache.entries[i])
        }
      }
      updated_entries = updated_entries.push({
        key: cache.entries[found_index].key,
        value: cache.entries[found_index].value,
        access_time: cache.current_time
      })
      
      {
        value,
        updated_cache: {
          entries: updated_entries,
          capacity: cache.capacity,
          current_time: cache.current_time
        }
      }
    } else {
      {
        value: None,
        updated_cache: cache
      }
    }
  }
  
  // Put value in cache
  let lru_put = fn(cache: LRUCache, key: String, value: String) {
    // Check if key already exists
    let mut found = false
    let mut found_index = -1
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        found = true
        found_index = i
      }
    }
    
    if found {
      // Update existing entry and move to end
      let mut updated_entries = []
      for i in 0..cache.entries.length() {
        if i != found_index {
          updated_entries = updated_entries.push(cache.entries[i])
        }
      }
      updated_entries = updated_entries.push({
        key,
        value,
        access_time: cache.current_time
      })
      
      {
        entries: updated_entries,
        capacity: cache.capacity,
        current_time: cache.current_time
      }
    } else {
      // Add new entry
      if cache.entries.length() < cache.capacity {
        // Space available
        {
          entries: cache.entries.push({
            key,
            value,
            access_time: cache.current_time
          }),
          capacity: cache.capacity,
          current_time: cache.current_time
        }
      } else {
        // Remove least recently used (first entry)
        let updated_entries = cache.entries.slice(1, cache.entries.length()).push({
          key,
          value,
          access_time: cache.current_time
        })
        
        {
          entries: updated_entries,
          capacity: cache.capacity,
          current_time: cache.current_time
        }
      }
    }
  }
  
  // Create LRU cache
  let mut cache = create_lru_cache(3)
  
  // Test empty cache
  let empty_result = lru_get(cache, "key1")
  assert_eq(empty_result.value, None)
  cache = empty_result.updated_cache
  
  // Put entries in cache
  cache.current_time = 1000
  cache = lru_put(cache, "key1", "value1")
  cache.current_time = 1100
  cache = lru_put(cache, "key2", "value2")
  cache.current_time = 1200
  cache = lru_put(cache, "key3", "value3")
  
  // Test cache contents
  assert_eq(cache.entries.length(), 3)
  assert_eq(cache.entries[0].key, "key1")
  assert_eq(cache.entries[1].key, "key2")
  assert_eq(cache.entries[2].key, "key3")
  
  // Get key1 (should move to end)
  cache.current_time = 1300
  let get_result = lru_get(cache, "key1")
  assert_eq(get_result.value, Some("value1"))
  cache = get_result.updated_cache
  
  // Check order after access
  assert_eq(cache.entries[0].key, "key2")
  assert_eq(cache.entries[1].key, "key3")
  assert_eq(cache.entries[2].key, "key1")
  
  // Add new entry (should evict key2 as least recently used)
  cache.current_time = 1400
  cache = lru_put(cache, "key4", "value4")
  
  // Check eviction
  assert_eq(cache.entries.length(), 3)
  assert_eq(cache.entries[0].key, "key3")
  assert_eq(cache.entries[1].key, "key1")
  assert_eq(cache.entries[2].key, "key4")
  
  // Test that key2 is evicted
  let evicted_result = lru_get(cache, "key2")
  assert_eq(evicted_result.value, None)
  
  // Test updating existing entry
  cache.current_time = 1500
  cache = lru_put(cache, "key1", "updated_value1")
  
  let updated_result = lru_get(cache, "key1")
  assert_eq(updated_result.value, Some("updated_value1"))
}

// Test 2: TTL Cache Implementation
test "TTL cache implementation for time-sensitive telemetry" {
  // Define TTL cache entry
  type TTLEntry = {
    key: String,
    value: String,
    created_at: Int,
    ttl_ms: Int
  }
  
  // Define TTL cache
  type TTLCache = {
    entries: Array[TTLEntry],
    current_time: Int
  }
  
  // Create TTL cache
  let create_ttl_cache = fn() {
    {
      entries: [],
      current_time: 0
    }
  }
  
  // Get value from cache (with TTL check)
  let ttl_get = fn(cache: TTLCache, key: String) {
    let mut found = false
    let mut value = None
    
    for entry in cache.entries {
      if entry.key == key {
        let age = cache.current_time - entry.created_at
        if age < entry.ttl_ms {
          found = true
          value = Some(entry.value)
        }
      }
    }
    
    {
      value,
      expired: not(found)
    }
  }
  
  // Put value in cache with TTL
  let ttl_put = fn(cache: TTLCache, key: String, value: String, ttl_ms: Int) {
    // Check if key already exists
    let mut found = false
    let mut updated_entries = []
    
    for entry in cache.entries {
      if entry.key == key {
        found = true
        updated_entries = updated_entries.push({
          key,
          value,
          created_at: cache.current_time,
          ttl_ms
        })
      } else {
        updated_entries = updated_entries.push(entry)
      }
    }
    
    if not(found) {
      updated_entries = updated_entries.push({
        key,
        value,
        created_at: cache.current_time,
        ttl_ms
      })
    }
    
    {
      entries: updated_entries,
      current_time: cache.current_time
    }
  }
  
  // Clean up expired entries
  let ttl_cleanup = fn(cache: TTLCache) {
    let mut valid_entries = []
    
    for entry in cache.entries {
      let age = cache.current_time - entry.created_at
      if age < entry.ttl_ms {
        valid_entries = valid_entries.push(entry)
      }
    }
    
    {
      entries: valid_entries,
      current_time: cache.current_time
    }
  }
  
  // Create TTL cache
  let mut cache = create_ttl_cache()
  
  // Test empty cache
  cache.current_time = 1000
  let empty_result = ttl_get(cache, "key1")
  assert_eq(empty_result.value, None)
  assert_true(empty_result.expired)
  
  // Put entries with different TTLs
  cache.current_time = 1000
  cache = ttl_put(cache, "short_lived", "value1", 1000)  // Expires at 2000
  cache = ttl_put(cache, "long_lived", "value2", 5000)   // Expires at 6000
  cache = ttl_put(cache, "medium_lived", "value3", 3000) // Expires at 4000
  
  // Test retrieval before expiration
  cache.current_time = 1500
  let short_result = ttl_get(cache, "short_lived")
  assert_eq(short_result.value, Some("value1"))
  assert_false(short_result.expired)
  
  let long_result = ttl_get(cache, "long_lived")
  assert_eq(long_result.value, Some("value2"))
  assert_false(long_result.expired)
  
  let medium_result = ttl_get(cache, "medium_lived")
  assert_eq(medium_result.value, Some("value3"))
  assert_false(medium_result.expired)
  
  // Test retrieval after expiration
  cache.current_time = 2500  // short_lived should be expired
  let expired_short = ttl_get(cache, "short_lived")
  assert_eq(expired_short.value, None)
  assert_true(expired_short.expired)
  
  let still_good_long = ttl_get(cache, "long_lived")
  assert_eq(still_good_long.value, Some("value2"))
  assert_false(still_good_long.expired)
  
  let still_good_medium = ttl_get(cache, "medium_lived")
  assert_eq(still_good_medium.value, Some("value3"))
  assert_false(still_good_medium.expired)
  
  // Test cleanup
  let cleaned_cache = ttl_cleanup(cache)
  assert_eq(cleaned_cache.entries.length(), 2)  // short_lived removed
  
  // Test updating existing entry
  cache.current_time = 2600
  cache = ttl_put(cache, "long_lived", "updated_value2", 5000)
  
  let updated_result = ttl_get(cache, "long_lived")
  assert_eq(updated_result.value, Some("updated_value2"))
  
  // Test that TTL is reset on update
  cache.current_time = 7000  // Original would have expired at 6000, but updated at 2600 with 5000 TTL = 7600
  let updated_after_original_expiry = ttl_get(cache, "long_lived")
  assert_eq(updated_after_original_expiry.value, Some("updated_value2"))
  assert_false(updated_after_original_expiry.expired)
}

// Test 3: Multi-Level Cache
test "multi-level cache for performance optimization" {
  // Define cache level
  type CacheLevel = {
    name: String,
    entries: Array[(String, String)],
    hit_count: Int,
    miss_count: Int,
    max_size: Int
  }
  
  // Define multi-level cache
  type MultiLevelCache = {
    levels: Array[CacheLevel>,
    current_time: Int
  }
  
  // Create multi-level cache
  let create_multi_level_cache = fn(level_configs: Array[(String, Int)]) {
    let mut levels = []
    
    for config in level_configs {
      levels = levels.push({
        name: config.0,
        entries: [],
        hit_count: 0,
        miss_count: 0,
        max_size: config.1
      })
    }
    
    {
      levels,
      current_time: 0
    }
  }
  
  // Get value from multi-level cache
  let ml_get = fn(cache: MultiLevelCache, key: String) {
    let mut value = None
    let mut found_level = -1
    let mut updated_levels = cache.levels
    
    // Search through levels in order (L1, L2, L3, ...)
    for i in 0..updated_levels.length() {
      let level = updated_levels[i]
      let mut found = false
      
      for entry in level.entries {
        if entry.0 == key {
          value = Some(entry.1)
          found = true
        }
      }
      
      if found {
        found_level = i
        // Update hit count
        updated_levels[i] = {
          name: level.name,
          entries: level.entries,
          hit_count: level.hit_count + 1,
          miss_count: level.miss_count,
          max_size: level.max_size
        }
        
        // Promote to higher levels if not already in L1
        if i > 0 {
          let entry_value = value.unwrap()
          
          // Add to higher levels
          for j in 0..i {
            let higher_level = updated_levels[j]
            let mut entry_exists = false
            
            for entry in higher_level.entries {
              if entry.0 == key {
                entry_exists = true
              }
            }
            
            if not(entry_exists) {
              // Add to higher level (evict if necessary)
              let mut updated_entries = higher_level.entries
              if updated_entries.length() >= higher_level.max_size {
                // Evict oldest (simplified)
                updated_entries = updated_entries.slice(1, updated_entries.length())
              }
              updated_entries = updated_entries.push((key, entry_value))
              
              updated_levels[j] = {
                name: higher_level.name,
                entries: updated_entries,
                hit_count: higher_level.hit_count,
                miss_count: higher_level.miss_count,
                max_size: higher_level.max_size
              }
            }
          }
        }
        break
      }
    }
    
    // Update miss counts for levels that didn't have the key
    if found_level < 0 {
      for i in 0..updated_levels.length() {
        let level = updated_levels[i]
        updated_levels[i] = {
          name: level.name,
          entries: level.entries,
          hit_count: level.hit_count,
          miss_count: level.miss_count + 1,
          max_size: level.max_size
        }
      }
    }
    
    {
      value,
      level_found: found_level,
      updated_cache: {
        levels: updated_levels,
        current_time: cache.current_time
      }
    }
  }
  
  // Put value in multi-level cache
  let ml_put = fn(cache: MultiLevelCache, key: String, value: String) {
    let mut updated_levels = cache.levels
    
    // Add to L1 (first level)
    if updated_levels.length() > 0 {
      let l1 = updated_levels[0]
      let mut entry_exists = false
      
      for entry in l1.entries {
        if entry.0 == key {
          entry_exists = true
        }
      }
      
      if not(entry_exists) {
        // Add to L1 (evict if necessary)
        let mut updated_entries = l1.entries
        if updated_entries.length() >= l1.max_size {
          // Evict oldest (simplified)
          updated_entries = updated_entries.slice(1, updated_entries.length())
        }
        updated_entries = updated_entries.push((key, value))
        
        updated_levels[0] = {
          name: l1.name,
          entries: updated_entries,
          hit_count: l1.hit_count,
          miss_count: l1.miss_count,
          max_size: l1.max_size
        }
      }
    }
    
    {
      levels: updated_levels,
      current_time: cache.current_time
    }
  }
  
  // Create multi-level cache (L1=2, L2=5, L3=10)
  let mut cache = create_multi_level_cache([("L1", 2), ("L2", 5), ("L3", 10)])
  
  // Test empty cache
  let empty_result = ml_get(cache, "key1")
  assert_eq(empty_result.value, None)
  assert_eq(empty_result.level_found, -1)
  cache = empty_result.updated_cache
  
  // Put values in cache
  cache = ml_put(cache, "key1", "value1")
  cache = ml_put(cache, "key2", "value2")
  cache = ml_put(cache, "key3", "value3")
  
  // Test L1 hits
  let l1_hit = ml_get(cache, "key1")
  assert_eq(l1_hit.value, Some("value1"))
  assert_eq(l1_hit.level_found, 0)  // Found in L1
  cache = l1_hit.updated_cache
  
  // Check hit counts
  assert_eq(cache.levels[0].hit_count, 1)
  assert_eq(cache.levels[1].hit_count, 0)
  assert_eq(cache.levels[2].hit_count, 0)
  
  // Fill L1 to capacity (key1 and key2 should be in L1, key3 evicted to L2)
  cache = ml_put(cache, "key4", "value4")
  
  // Test L2 hit (key3 should be in L2)
  let l2_hit = ml_get(cache, "key3")
  assert_eq(l2_hit.value, Some("value3"))
  assert_eq(l2_hit.level_found, 1)  // Found in L2
  cache = l2_hit.updated_cache
  
  // Check promotion - key3 should now be in L1
  let promoted_hit = ml_get(cache, "key3")
  assert_eq(promoted_hit.value, Some("value3"))
  assert_eq(promoted_hit.level_found, 0)  // Now in L1
  cache = promoted_hit.updated_cache
  
  // Test miss
  let miss_result = ml_get(cache, "nonexistent")
  assert_eq(miss_result.value, None)
  assert_eq(miss_result.level_found, -1)
  cache = miss_result.updated_cache
  
  // Check miss counts
  assert_eq(cache.levels[0].miss_count, 1)
  assert_eq(cache.levels[1].miss_count, 1)
  assert_eq(cache.levels[2].miss_count, 1)
  
  // Calculate hit rates
  let l1_hit_rate = if cache.levels[0].hit_count + cache.levels[0].miss_count > 0 {
    (cache.levels[0].hit_count.to_float() / (cache.levels[0].hit_count + cache.levels[0].miss_count).to_float()) * 100.0
  } else {
    0.0
  }
  
  let l2_hit_rate = if cache.levels[1].hit_count + cache.levels[1].miss_count > 0 {
    (cache.levels[1].hit_count.to_float() / (cache.levels[1].hit_count + cache.levels[1].miss_count).to_float()) * 100.0
  } else {
    0.0
  }
  
  let l3_hit_rate = if cache.levels[2].hit_count + cache.levels[2].miss_count > 0 {
    (cache.levels[2].hit_count.to_float() / (cache.levels[2].hit_count + cache.levels[2].miss_count).to_float()) * 100.0
  } else {
    0.0
  }
  
  assert_true(l1_hit_rate > 0.0)
  assert_true(l2_hit_rate > 0.0)
  assert_eq(l3_hit_rate, 0.0)  // No hits in L3 yet
}

// Test 4: Write-Through Cache
test "write-through cache for data consistency" {
  // Define database (simulated)
  type Database = {
    data: Array[(String, String)]
  }
  
  // Define write-through cache
  type WriteThroughCache = {
    cache_entries: Array[(String, String)],
    database: Database,
    max_size: Int
  }
  
  // Create write-through cache
  let create_write_through_cache = fn(max_size: Int) {
    {
      cache_entries: [],
      database: {
        data: []
      },
      max_size
    }
  }
  
  // Get value from cache (fallback to database)
  let wt_get = fn(cache: WriteThroughCache, key: String) {
    // Check cache first
    let mut found = false
    let mut value = None
    
    for entry in cache.cache_entries {
      if entry.0 == key {
        found = true
        value = Some(entry.1)
      }
    }
    
    if not(found) {
      // Check database
      for entry in cache.database.data {
        if entry.0 == key {
          found = true
          value = Some(entry.1)
          
          // Add to cache (evict if necessary)
          let mut updated_cache = cache.cache_entries
          if updated_cache.length() >= cache.max_size {
            // Evict oldest (simplified)
            updated_cache = updated_cache.slice(1, updated_cache.length())
          }
          updated_cache = updated_cache.push((key, entry.1))
          
          return {
            value,
            updated_cache: {
              cache_entries: updated_cache,
              database: cache.database,
              max_size: cache.max_size
            }
          }
        }
      }
    }
    
    {
      value,
      updated_cache: cache
    }
  }
  
  // Put value in cache and database
  let wt_put = fn(cache: WriteThroughCache, key: String, value: String) {
    // Update database
    let mut updated_db_data = cache.database.data
    let mut found = false
    
    for i in 0..updated_db_data.length() {
      if updated_db_data[i].0 == key {
        updated_db_data[i] = (key, value)
        found = true
      }
    }
    
    if not(found) {
      updated_db_data = updated_db_data.push((key, value))
    }
    
    // Update cache
    let mut updated_cache_entries = cache.cache_entries
    let mut cache_found = false
    
    for i in 0..updated_cache_entries.length() {
      if updated_cache_entries[i].0 == key {
        updated_cache_entries[i] = (key, value)
        cache_found = true
      }
    }
    
    if not(cache_found) {
      // Add to cache (evict if necessary)
      if updated_cache_entries.length() >= cache.max_size {
        // Evict oldest (simplified)
        updated_cache_entries = updated_cache_entries.slice(1, updated_cache_entries.length())
      }
      updated_cache_entries = updated_cache_entries.push((key, value))
    }
    
    {
      cache_entries: updated_cache_entries,
      database: {
        data: updated_db_data
      },
      max_size: cache.max_size
    }
  }
  
  // Create write-through cache
  let mut cache = create_write_through_cache(3)
  
  // Test empty cache and database
  let empty_result = wt_get(cache, "key1")
  assert_eq(empty_result.value, None)
  cache = empty_result.updated_cache
  
  // Put values
  cache = wt_put(cache, "key1", "value1")
  cache = wt_put(cache, "key2", "value2")
  cache = wt_put(cache, "key3", "value3")
  
  // Verify cache and database
  assert_eq(cache.cache_entries.length(), 3)
  assert_eq(cache.database.data.length(), 3)
  
  // Test cache hit
  let cache_hit = wt_get(cache, "key1")
  assert_eq(cache_hit.value, Some("value1"))
  cache = cache_hit.updated_cache
  
  // Test database hit (cache miss)
  // Clear cache to force database lookup
  cache.cache_entries = []
  let db_hit = wt_get(cache, "key2")
  assert_eq(db_hit.value, Some("value2"))
  cache = db_hit.updated_cache
  
  // Verify key2 is now in cache
  assert_eq(cache.cache_entries.length(), 1)
  assert_eq(cache.cache_entries[0].0, "key2")
  
  // Test update
  cache = wt_put(cache, "key1", "updated_value1")
  
  let updated_result = wt_get(cache, "key1")
  assert_eq(updated_result.value, Some("updated_value1"))
  
  // Verify database is also updated
  let db_updated_result = wt_get({ cache | cache_entries: [] }, "key1")
  assert_eq(db_updated_result.value, Some("updated_value1"))
}

// Test 5: Write-Behind Cache
test "write-behind cache for performance optimization" {
  // Define database (simulated)
  type Database = {
    data: Array[(String, String)]
  }
  
  // Define write-behind cache
  type WriteBehindCache = {
    cache_entries: Array[(String, String)],
    database: Database,
    pending_writes: Array[(String, String)],
    max_size: Int,
    write_batch_size: Int
  }
  
  // Create write-behind cache
  let create_write_behind_cache = fn(max_size: Int, write_batch_size: Int) {
    {
      cache_entries: [],
      database: {
        data: []
      },
      pending_writes: [],
      max_size,
      write_batch_size
    }
  }
  
  // Get value from cache (fallback to database)
  let wb_get = fn(cache: WriteBehindCache, key: String) {
    // Check cache first
    let mut found = false
    let mut value = None
    
    for entry in cache.cache_entries {
      if entry.0 == key {
        found = true
        value = Some(entry.1)
      }
    }
    
    if not(found) {
      // Check database
      for entry in cache.database.data {
        if entry.0 == key {
          found = true
          value = Some(entry.1)
          
          // Add to cache (evict if necessary)
          let mut updated_cache = cache.cache_entries
          if updated_cache.length() >= cache.max_size {
            // Evict oldest (simplified)
            updated_cache = updated_cache.slice(1, updated_cache.length())
          }
          updated_cache = updated_cache.push((key, entry.1))
          
          return {
            value,
            updated_cache: {
              cache_entries: updated_cache,
              database: cache.database,
              pending_writes: cache.pending_writes,
              max_size: cache.max_size,
              write_batch_size: cache.write_batch_size
            }
          }
        }
      }
    }
    
    {
      value,
      updated_cache: cache
    }
  }
  
  // Put value in cache (add to pending writes)
  let wb_put = fn(cache: WriteBehindCache, key: String, value: String) {
    // Add to pending writes
    let mut updated_pending_writes = cache.pending_writes
    let mut found = false
    
    for i in 0..updated_pending_writes.length() {
      if updated_pending_writes[i].0 == key {
        updated_pending_writes[i] = (key, value)
        found = true
      }
    }
    
    if not(found) {
      updated_pending_writes = updated_pending_writes.push((key, value))
    }
    
    // Update cache
    let mut updated_cache_entries = cache.cache_entries
    let mut cache_found = false
    
    for i in 0..updated_cache_entries.length() {
      if updated_cache_entries[i].0 == key {
        updated_cache_entries[i] = (key, value)
        cache_found = true
      }
    }
    
    if not(cache_found) {
      // Add to cache (evict if necessary)
      if updated_cache_entries.length() >= cache.max_size {
        // Evict oldest (simplified)
        updated_cache_entries = updated_cache_entries.slice(1, updated_cache_entries.length())
      }
      updated_cache_entries = updated_cache_entries.push((key, value))
    }
    
    // Check if we should flush pending writes
    let mut updated_db_data = cache.database.data
    if updated_pending_writes.length() >= cache.write_batch_size {
      // Flush pending writes to database
      for pending in updated_pending_writes {
        let mut db_found = false
        
        for i in 0..updated_db_data.length() {
          if updated_db_data[i].0 == pending.0 {
            updated_db_data[i] = pending
            db_found = true
          }
        }
        
        if not(db_found) {
          updated_db_data = updated_db_data.push(pending)
        }
      }
      
      updated_pending_writes = []
    }
    
    {
      cache_entries: updated_cache_entries,
      database: {
        data: updated_db_data
      },
      pending_writes: updated_pending_writes,
      max_size: cache.max_size,
      write_batch_size: cache.write_batch_size
    }
  }
  
  // Flush pending writes
  let wb_flush = fn(cache: WriteBehindCache) {
    let mut updated_db_data = cache.database.data
    
    for pending in cache.pending_writes {
      let mut found = false
      
      for i in 0..updated_db_data.length() {
        if updated_db_data[i].0 == pending.0 {
          updated_db_data[i] = pending
          found = true
        }
      }
      
      if not(found) {
        updated_db_data = updated_db_data.push(pending)
      }
    }
    
    {
      cache_entries: cache.cache_entries,
      database: {
        data: updated_db_data
      },
      pending_writes: [],
      max_size: cache.max_size,
      write_batch_size: cache.write_batch_size
    }
  }
  
  // Create write-behind cache
  let mut cache = create_write_behind_cache(3, 2)
  
  // Test empty cache and database
  let empty_result = wb_get(cache, "key1")
  assert_eq(empty_result.value, None)
  cache = empty_result.updated_cache
  
  // Put values (should be in pending writes)
  cache = wb_put(cache, "key1", "value1")
  cache = wb_put(cache, "key2", "value2")
  
  // Check pending writes
  assert_eq(cache.pending_writes.length(), 2)
  assert_eq(cache.database.data.length(), 0)  // Not flushed yet
  
  // Add one more (should trigger flush)
  cache = wb_put(cache, "key3", "value3")
  
  // Check that pending writes were flushed
  assert_eq(cache.pending_writes.length(), 1)  // key3 still pending
  assert_eq(cache.database.data.length(), 2)  // key1 and key2 flushed
  
  // Test cache hit
  let cache_hit = wb_get(cache, "key1")
  assert_eq(cache_hit.value, Some("value1"))
  cache = cache_hit.updated_cache
  
  // Test database hit (cache miss)
  cache.cache_entries = []
  let db_hit = wb_get(cache, "key2")
  assert_eq(db_hit.value, Some("value2"))
  cache = db_hit.updated_cache
  
  // Test pending write not in database
  let pending_result = wb_get({ cache | cache_entries: [] }, "key3")
  assert_eq(pending_result.value, None)
  
  // Flush remaining pending writes
  cache = wb_flush(cache)
  
  assert_eq(cache.pending_writes.length(), 0)
  assert_eq(cache.database.data.length(), 3)
  
  // Test that pending write is now in database
  let flushed_result = wb_get({ cache | cache_entries: [] }, "key3")
  assert_eq(flushed_result.value, Some("value3"))
}

// Test 6: Cache Aside Pattern
test "cache aside pattern for flexible caching" {
  // Define database (simulated)
  type Database = {
    data: Array[(String, String)]
  }
  
  // Define cache aside cache
  type CacheAsideCache = {
    cache_entries: Array[(String, String)],
    database: Database,
    max_size: Int
  }
  
  // Create cache aside cache
  let create_cache_aside_cache = fn(max_size: Int) {
    {
      cache_entries: [],
      database: {
        data: []
      },
      max_size
    }
  }
  
  // Get value (application manages cache)
  let ca_get_from_cache = fn(cache: CacheAsideCache, key: String) {
    let mut found = false
    let mut value = None
    
    for entry in cache.cache_entries {
      if entry.0 == key {
        found = true
        value = Some(entry.1)
      }
    }
    
    {
      value,
      cache_hit: found
    }
  }
  
  // Get value from database
  let ca_get_from_database = fn(cache: CacheAsideCache, key: String) {
    let mut found = false
    let mut value = None
    
    for entry in cache.database.data {
      if entry.0 == key {
        found = true
        value = Some(entry.1)
      }
    }
    
    {
      value,
      found
    }
  }
  
  // Put value in cache
  let ca_put_in_cache = fn(cache: CacheAsideCache, key: String, value: String) {
    let mut updated_cache_entries = cache.cache_entries
    let mut found = false
    
    for i in 0..updated_cache_entries.length() {
      if updated_cache_entries[i].0 == key {
        updated_cache_entries[i] = (key, value)
        found = true
      }
    }
    
    if not(found) {
      // Add to cache (evict if necessary)
      if updated_cache_entries.length() >= cache.max_size {
        // Evict oldest (simplified)
        updated_cache_entries = updated_cache_entries.slice(1, updated_cache_entries.length())
      }
      updated_cache_entries = updated_cache_entries.push((key, value))
    }
    
    {
      cache_entries: updated_cache_entries,
      database: cache.database,
      max_size: cache.max_size
    }
  }
  
  // Put value in database
  let ca_put_in_database = fn(cache: CacheAsideCache, key: String, value: String) {
    let mut updated_db_data = cache.database.data
    let mut found = false
    
    for i in 0..updated_db_data.length() {
      if updated_db_data[i].0 == key {
        updated_db_data[i] = (key, value)
        found = true
      }
    }
    
    if not(found) {
      updated_db_data = updated_db_data.push((key, value))
    }
    
    {
      cache_entries: cache.cache_entries,
      database: {
        data: updated_db_data
      },
      max_size: cache.max_size
    }
  }
  
  // Invalidate cache entry
  let ca_invalidate = fn(cache: CacheAsideCache, key: String) {
    let mut updated_cache_entries = []
    
    for entry in cache.cache_entries {
      if entry.0 != key {
        updated_cache_entries = updated_cache_entries.push(entry)
      }
    }
    
    {
      cache_entries: updated_cache_entries,
      database: cache.database,
      max_size: cache.max_size
    }
  }
  
  // Create cache aside cache
  let mut cache = create_cache_aside_cache(3)
  
  // Put values in database
  cache = ca_put_in_database(cache, "key1", "value1")
  cache = ca_put_in_database(cache, "key2", "value2")
  cache = ca_put_in_database(cache, "key3", "value3")
  
  // Test cache miss
  let cache_miss = ca_get_from_cache(cache, "key1")
  assert_eq(cache_miss.value, None)
  assert_false(cache_miss.cache_hit)
  
  // Get from database and put in cache
  let db_result = ca_get_from_database(cache, "key1")
  assert_eq(db_result.value, Some("value1"))
  assert_true(db_result.found)
  
  if db_result.found {
    cache = ca_put_in_cache(cache, "key1", db_result.value.unwrap())
  }
  
  // Test cache hit
  let cache_hit = ca_get_from_cache(cache, "key1")
  assert_eq(cache_hit.value, Some("value1"))
  assert_true(cache_hit.cache_hit)
  
  // Put new value in database and cache
  cache = ca_put_in_database(cache, "key4", "value4")
  cache = ca_put_in_cache(cache, "key4", "value4")
  
  // Test cache hit for new value
  let new_cache_hit = ca_get_from_cache(cache, "key4")
  assert_eq(new_cache_hit.value, Some("value4"))
  assert_true(new_cache_hit.cache_hit)
  
  // Update value in database
  cache = ca_put_in_database(cache, "key1", "updated_value1")
  
  // Cache should still have old value
  let stale_cache_hit = ca_get_from_cache(cache, "key1")
  assert_eq(stale_cache_hit.value, Some("value1"))  // Old value
  assert_true(stale_cache_hit.cache_hit)
  
  // Invalidate cache entry
  cache = ca_invalidate(cache, "key1")
  
  // Cache should no longer have the entry
  let invalidated_result = ca_get_from_cache(cache, "key1")
  assert_eq(invalidated_result.value, None)
  assert_false(invalidated_result.cache_hit)
  
  // Get updated value from database
  let updated_db_result = ca_get_from_database(cache, "key1")
  assert_eq(updated_db_result.value, Some("updated_value1"))
  assert_true(updated_db_result.found)
  
  // Put updated value in cache
  if updated_db_result.found {
    cache = ca_put_in_cache(cache, "key1", updated_db_result.value.unwrap())
  }
  
  // Test cache hit with updated value
  let updated_cache_hit = ca_get_from_cache(cache, "key1")
  assert_eq(updated_cache_hit.value, Some("updated_value1"))
  assert_true(updated_cache_hit.cache_hit)
}

// Test 7: Distributed Cache
test "distributed cache for multi-node telemetry" {
  // Define cache node
  type CacheNode = {
    id: String,
    entries: Array[(String, String)],
    max_size: Int
  }
  
  // Define distributed cache
  type DistributedCache = {
    nodes: Array[CacheNode>,
    hash_function: fn(String, Int) -> Int  // Key -> Node index
  }
  
  // Create hash function (simple modulo)
  let create_hash_function = fn() {
    fn(key: String, node_count: Int) {
      let mut hash = 0
      let chars = key.to_char_array()
      
      for i in 0..chars.length() {
        hash = (hash + chars[i].to_int()) % node_count
      }
      
      if hash < 0 {
        hash + node_count
      } else {
        hash
      }
    }
  }
  
  // Create distributed cache
  let create_distributed_cache = fn(node_count: Int, max_size_per_node: Int) {
    let mut nodes = []
    
    for i in 0..node_count {
      nodes = nodes.push({
        id: "node-" + i.to_string(),
        entries: [],
        max_size: max_size_per_node
      })
    }
    
    {
      nodes,
      hash_function: create_hash_function()
    }
  }
  
  // Get value from distributed cache
  let dc_get = fn(cache: DistributedCache, key: String) {
    let node_index = cache.hash_function(key, cache.nodes.length())
    let node = cache.nodes[node_index]
    
    let mut found = false
    let mut value = None
    
    for entry in node.entries {
      if entry.0 == key {
        found = true
        value = Some(entry.1)
      }
    }
    
    {
      value,
      node_id: node.id,
      found
    }
  }
  
  // Put value in distributed cache
  let dc_put = fn(cache: DistributedCache, key: String, value: String) {
    let node_index = cache.hash_function(key, cache.nodes.length())
    let node = cache.nodes[node_index]
    
    let mut updated_entries = node.entries
    let mut found = false
    
    for i in 0..updated_entries.length() {
      if updated_entries[i].0 == key {
        updated_entries[i] = (key, value)
        found = true
      }
    }
    
    if not(found) {
      // Add to node (evict if necessary)
      if updated_entries.length() >= node.max_size {
        // Evict oldest (simplified)
        updated_entries = updated_entries.slice(1, updated_entries.length())
      }
      updated_entries = updated_entries.push((key, value))
    }
    
    let mut updated_nodes = cache.nodes
    updated_nodes[node_index] = {
      id: node.id,
      entries: updated_entries,
      max_size: node.max_size
    }
    
    {
      nodes: updated_nodes,
      hash_function: cache.hash_function
    }
  }
  
  // Create distributed cache with 3 nodes
  let mut cache = create_distributed_cache(3, 2)
  
  // Test empty cache
  let empty_result = dc_get(cache, "key1")
  assert_eq(empty_result.value, None)
  assert_false(empty_result.found)
  
  // Put values
  cache = dc_put(cache, "key1", "value1")
  cache = dc_put(cache, "key2", "value2")
  cache = dc_put(cache, "key3", "value3")
  cache = dc_put(cache, "key4", "value4")
  cache = dc_put(cache, "key5", "value5")
  
  // Test retrieval
  let result1 = dc_get(cache, "key1")
  assert_eq(result1.value, Some("value1"))
  assert_true(result1.found)
  
  let result2 = dc_get(cache, "key2")
  assert_eq(result2.value, Some("value2"))
  assert_true(result2.found)
  
  let result3 = dc_get(cache, "key3")
  assert_eq(result3.value, Some("value3"))
  assert_true(result3.found)
  
  // Check distribution across nodes
  let mut node_counts = []
  for i in 0..cache.nodes.length() {
    node_counts = node_counts.push(cache.nodes[i].entries.length())
  }
  
  // Each node should have some entries
  for count in node_counts {
    assert_true(count > 0)
  }
  
  // Test that keys are consistently mapped to the same node
  let node_for_key1 = dc_get(cache, "key1").node_id
  let node_for_key1_again = dc_get(cache, "key1").node_id
  assert_eq(node_for_key1, node_for_key1_again)
  
  // Test node capacity
  let total_capacity = cache.nodes.length() * 2  // 3 nodes * 2 entries each
  assert_eq(total_capacity, 6)
  
  // Add more entries to test eviction
  cache = dc_put(cache, "key6", "value6")
  cache = dc_put(cache, "key7", "value7")
  
  // Total entries should not exceed capacity
  let mut total_entries = 0
  for node in cache.nodes {
    total_entries = total_entries + node.entries.length()
  }
  
  assert_true(total_entries <= total_capacity)
}

// Test 8: Cache Statistics and Monitoring
test "cache statistics and monitoring for performance analysis" {
  // Define cache statistics
  type CacheStats = {
    hits: Int,
    misses: Int,
    sets: Int,
    deletes: Int,
    evictions: Int,
    total_size: Int,
    max_size: Int,
    created_at: Int,
    last_access_at: Int
  }
  
  // Define monitored cache
  type MonitoredCache = {
    entries: Array[(String, String)],
    stats: CacheStats,
    max_size: Int,
    current_time: Int
  }
  
  // Create monitored cache
  let create_monitored_cache = fn(max_size: Int) {
    {
      entries: [],
      stats: {
        hits: 0,
        misses: 0,
        sets: 0,
        deletes: 0,
        evictions: 0,
        total_size: 0,
        max_size: max_size,
        created_at: 0,
        last_access_at: 0
      },
      max_size,
      current_time: 0
    }
  }
  
  // Get value with statistics
  let mc_get = fn(cache: MonitoredCache, key: String) {
    let mut found = false
    let mut value = None
    let mut found_index = -1
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].0 == key {
        found = true
        value = Some(cache.entries[i].1)
        found_index = i
      }
    }
    
    // Update statistics
    let updated_stats = {
      hits: if found { cache.stats.hits + 1 } else { cache.stats.hits },
      misses: if not(found) { cache.stats.misses + 1 } else { cache.stats.misses },
      sets: cache.stats.sets,
      deletes: cache.stats.deletes,
      evictions: cache.stats.evictions,
      total_size: cache.stats.total_size,
      max_size: cache.stats.max_size,
      created_at: cache.stats.created_at,
      last_access_at: cache.current_time
    }
    
    // Move accessed entry to end (for LRU)
    let mut updated_entries = cache.entries
    if found {
      for i in 0..updated_entries.length() {
        if i != found_index {
          updated_entries = updated_entries.push(cache.entries[i])
        }
      }
    }
    
    {
      value,
      updated_cache: {
        entries: updated_entries,
        stats: updated_stats,
        max_size: cache.max_size,
        current_time: cache.current_time
      }
    }
  }
  
  // Put value with statistics
  let mc_put = fn(cache: MonitoredCache, key: String, value: String) {
    let mut found = false
    let mut found_index = -1
    let mut evicted = false
    
    // Check if key already exists
    for i in 0..cache.entries.length() {
      if cache.entries[i].0 == key {
        found = true
        found_index = i
      }
    }
    
    // Handle eviction if necessary
    let mut updated_entries = cache.entries
    if not(found) && updated_entries.length() >= cache.max_size {
      // Evict oldest
      updated_entries = updated_entries.slice(1, updated_entries.length())
      evicted = true
    }
    
    // Add or update entry
    if found {
      // Update existing
      for i in 0..updated_entries.length() {
        if updated_entries[i].0 == key {
          updated_entries[i] = (key, value)
        }
      }
    } else {
      // Add new
      updated_entries = updated_entries.push((key, value))
    }
    
    // Update statistics
    let updated_stats = {
      hits: cache.stats.hits,
      misses: cache.stats.misses,
      sets: cache.stats.sets + 1,
      deletes: cache.stats.deletes,
      evictions: if evicted { cache.stats.evictions + 1 } else { cache.stats.evictions },
      total_size: updated_entries.length(),
      max_size: cache.stats.max_size,
      created_at: cache.stats.created_at,
      last_access_at: cache.current_time
    }
    
    {
      entries: updated_entries,
      stats: updated_stats,
      max_size: cache.max_size,
      current_time: cache.current_time
    }
  }
  
  // Calculate hit rate
  let hit_rate = fn(stats: CacheStats) {
    let total_requests = stats.hits + stats.misses
    if total_requests > 0 {
      (stats.hits.to_float() / total_requests.to_float()) * 100.0
    } else {
      0.0
    }
  }
  
  // Calculate miss rate
  let miss_rate = fn(stats: CacheStats) {
    let total_requests = stats.hits + stats.misses
    if total_requests > 0 {
      (stats.misses.to_float() / total_requests.to_float()) * 100.0
    } else {
      0.0
    }
  }
  
  // Calculate utilization
  let utilization = fn(stats: CacheStats) {
    if stats.max_size > 0 {
      (stats.total_size.to_float() / stats.max_size.to_float()) * 100.0
    } else {
      0.0
    }
  }
  
  // Create monitored cache
  let mut cache = create_monitored_cache(3)
  
  // Test initial statistics
  assert_eq(cache.stats.hits, 0)
  assert_eq(cache.stats.misses, 0)
  assert_eq(cache.stats.sets, 0)
  assert_eq(cache.stats.evictions, 0)
  assert_eq(cache.stats.total_size, 0)
  
  // Put values
  cache.current_time = 1000
  cache = mc_put(cache, "key1", "value1")
  cache.current_time = 1100
  cache = mc_put(cache, "key2", "value2")
  cache.current_time = 1200
  cache = mc_put(cache, "key3", "value3")
  
  // Check statistics after puts
  assert_eq(cache.stats.sets, 3)
  assert_eq(cache.stats.evictions, 0)
  assert_eq(cache.stats.total_size, 3)
  
  // Get values
  cache.current_time = 1300
  let get_result1 = mc_get(cache, "key1")
  assert_eq(get_result1.value, Some("value1"))
  cache = get_result1.updated_cache
  
  cache.current_time = 1400
  let get_result2 = mc_get(cache, "key2")
  assert_eq(get_result2.value, Some("value2"))
  cache = get_result2.updated_cache
  
  cache.current_time = 1500
  let get_result3 = mc_get(cache, "nonexistent")
  assert_eq(get_result3.value, None)
  cache = get_result3.updated_cache
  
  // Check statistics after gets
  assert_eq(cache.stats.hits, 2)
  assert_eq(cache.stats.misses, 1)
  
  // Calculate metrics
  let hit_rate_value = hit_rate(cache.stats)
  let miss_rate_value = miss_rate(cache.stats)
  let utilization_value = utilization(cache.stats)
  
  assert_eq(hit_rate_value, 66.67)  // 2/3 * 100 ≈ 66.67%
  assert_eq(miss_rate_value, 33.33)  // 1/3 * 100 ≈ 33.33%
  assert_eq(utilization_value, 100.0)  // 3/3 * 100 = 100%
  
  // Put another value (should trigger eviction)
  cache.current_time = 1600
  cache = mc_put(cache, "key4", "value4")
  
  // Check eviction statistics
  assert_eq(cache.stats.evictions, 1)
  assert_eq(cache.stats.sets, 4)
  assert_eq(cache.stats.total_size, 3)  // Still at max capacity
  
  // Test that evicted key is gone
  cache.current_time = 1700
  let evicted_result = mc_get(cache, "key1")  // Should be evicted (oldest)
  assert_eq(evicted_result.value, None)
  assert_eq(evicted_result.updated_cache.stats.misses, 2)
}

// Test 9: Cache Invalidation Strategies
test "cache invalidation strategies for data consistency" {
  // Define cache entry with version
  type VersionedEntry = {
    key: String,
    value: String,
    version: Int,
    created_at: Int
  }
  
  // Define cache with invalidation
  type CacheWithInvalidation = {
    entries: Array[VersionedEntry],
    invalidation_strategy: String,  // "ttl", "version", "manual", "size"
    ttl_ms: Int,
    max_size: Int,
    current_time: Int
  }
  
  // Create cache with invalidation
  let create_cache_with_invalidation = fn(strategy: String, ttl_ms: Int, max_size: Int) {
    {
      entries: [],
      invalidation_strategy: strategy,
      ttl_ms,
      max_size,
      current_time: 0
    }
  }
  
  // Get value with invalidation check
  let ci_get = fn(cache: CacheWithInvalidation, key: String) {
    let mut valid = false
    let mut value = None
    let mut reason = ""
    
    for entry in cache.entries {
      if entry.key == key {
        match cache.invalidation_strategy {
          "ttl" => {
            let age = cache.current_time - entry.created_at
            if age < cache.ttl_ms {
              valid = true
              value = Some(entry.value)
            } else {
              reason = "expired"
            }
          }
          "version" => {
            valid = true
            value = Some(entry.value)
          }
          "manual" => {
            valid = true
            value = Some(entry.value)
          }
          "size" => {
            valid = true
            value = Some(entry.value)
          }
          _ => {
            reason = "unknown strategy"
          }
        }
      }
    }
    
    {
      value,
      valid,
      reason
    }
  }
  
  // Put value
  let ci_put = fn(cache: CacheWithInvalidation, key: String, value: String, version: Int) {
    let mut updated_entries = cache.entries
    let mut found = false
    let mut evicted = false
    
    // Check if key already exists
    for i in 0..updated_entries.length() {
      if updated_entries[i].key == key {
        found = true
        updated_entries[i] = {
          key,
          value,
          version,
          created_at: cache.current_time
        }
      }
    }
    
    // Handle eviction if necessary
    if not(found) {
      if updated_entries.length() >= cache.max_size {
        // Evict based on strategy
        match cache.invalidation_strategy {
          "size" => {
            // Evict oldest
            updated_entries = updated_entries.slice(1, updated_entries.length())
            evicted = true
          }
          "ttl" => {
            // Evict expired entries first, then oldest
            let mut valid_entries = []
            let mut expired_evicted = false
            
            for entry in updated_entries {
              let age = cache.current_time - entry.created_at
              if age < cache.ttl_ms {
                valid_entries = valid_entries.push(entry)
              } else {
                expired_evicted = true
              }
            }
            
            if not(expired_evicted) && valid_entries.length() >= cache.max_size {
              valid_entries = valid_entries.slice(1, valid_entries.length())
              evicted = true
            }
            
            updated_entries = valid_entries
          }
          _ => {
            // Default to oldest eviction
            updated_entries = updated_entries.slice(1, updated_entries.length())
            evicted = true
          }
        }
      }
      
      // Add new entry
      updated_entries = updated_entries.push({
        key,
        value,
        version,
        created_at: cache.current_time
      })
    }
    
    {
      entries: updated_entries,
      invalidation_strategy: cache.invalidation_strategy,
      ttl_ms: cache.ttl_ms,
      max_size: cache.max_size,
      current_time: cache.current_time
    }
  }
  
  // Invalidate by key
  let ci_invalidate_key = fn(cache: CacheWithInvalidation, key: String) {
    let mut updated_entries = []
    
    for entry in cache.entries {
      if entry.key != key {
        updated_entries = updated_entries.push(entry)
      }
    }
    
    {
      entries: updated_entries,
      invalidation_strategy: cache.invalidation_strategy,
      ttl_ms: cache.ttl_ms,
      max_size: cache.max_size,
      current_time: cache.current_time
    }
  }
  
  // Invalidate by version
  let ci_invalidate_version = fn(cache: CacheWithInvalidation, key: String, expected_version: Int) {
    let mut updated_entries = []
    let mut invalidated = false
    
    for entry in cache.entries {
      if entry.key == key && entry.version < expected_version {
        invalidated = true
      } else {
        updated_entries = updated_entries.push(entry)
      }
    }
    
    {
      entries: updated_entries,
      invalidation_strategy: cache.invalidation_strategy,
      ttl_ms: cache.ttl_ms,
      max_size: cache.max_size,
      current_time: cache.current_time
    }
  }
  
  // Invalidate all
  let ci_invalidate_all = fn(cache: CacheWithInvalidation) {
    {
      entries: [],
      invalidation_strategy: cache.invalidation_strategy,
      ttl_ms: cache.ttl_ms,
      max_size: cache.max_size,
      current_time: cache.current_time
    }
  }
  
  // Test TTL invalidation
  let mut ttl_cache = create_cache_with_invalidation("ttl", 1000, 3)
  
  ttl_cache.current_time = 1000
  ttl_cache = ci_put(ttl_cache, "key1", "value1", 1)
  ttl_cache.current_time = 1100
  ttl_cache = ci_put(ttl_cache, "key2", "value2", 1)
  
  // Test valid entries
  let valid_result = ci_get(ttl_cache, "key1")
  assert_eq(valid_result.value, Some("value1"))
  assert_true(valid_result.valid)
  
  // Test expired entry
  ttl_cache.current_time = 2200  // key1 created at 1000, TTL is 1000, so expired at 2000
  let expired_result = ci_get(ttl_cache, "key1")
  assert_eq(expired_result.value, None)
  assert_false(expired_result.valid)
  assert_eq(expired_result.reason, "expired")
  
  // Test manual invalidation
  let mut manual_cache = create_cache_with_invalidation("manual", 0, 3)
  
  manual_cache.current_time = 1000
  manual_cache = ci_put(manual_cache, "key1", "value1", 1)
  manual_cache = ci_put(manual_cache, "key2", "value2", 1)
  
  // Test before invalidation
  let before_result = ci_get(manual_cache, "key1")
  assert_eq(before_result.value, Some("value1"))
  assert_true(before_result.valid)
  
  // Invalidate by key
  manual_cache = ci_invalidate_key(manual_cache, "key1")
  
  // Test after invalidation
  let after_result = ci_get(manual_cache, "key1")
  assert_eq(after_result.value, None)
  assert_false(after_result.valid)
  
  // Test version invalidation
  let mut version_cache = create_cache_with_invalidation("version", 0, 3)
  
  version_cache.current_time = 1000
  version_cache = ci_put(version_cache, "key1", "value1", 1)
  version_cache = ci_put(version_cache, "key2", "value2", 1)
  
  // Test before version invalidation
  let before_version_result = ci_get(version_cache, "key1")
  assert_eq(before_version_result.value, Some("value1"))
  assert_true(before_version_result.valid)
  
  // Invalidate by version
  version_cache = ci_invalidate_version(version_cache, "key1", 2)
  
  // Test after version invalidation
  let after_version_result = ci_get(version_cache, "key1")
  assert_eq(after_version_result.value, None)
  assert_false(after_version_result.valid)
  
  // Test invalidate all
  manual_cache = ci_invalidate_all(manual_cache);
  
  let all_invalid_result = ci_get(manual_cache, "key2");
  assert_eq(all_invalid_result.value, None);
  assert_false(all_invalid_result.valid);
}

// Test 10: Cache Warming and Preloading
test "cache warming and preloading for performance optimization" {
  // Define cache entry
  type CacheEntry = {
    key: String,
    value: String,
    priority: Int,  // Higher = more important
    access_count: Int
  }
  
  // Define cache with warming
  type WarmedCache = {
    entries: Array[CacheEntry],
    max_size: Int,
    warm_up_complete: Bool
  }
  
  // Define data source (simulated)
  type DataSource = {
    data: Array[(String, String)]
  }
  
  // Create data source
  let create_data_source = fn() {
    {
      data: [
        ("user-1", "User One Data"),
        ("user-2", "User Two Data"),
        ("user-3", "User Three Data"),
        ("config-1", "Configuration One"),
        ("config-2", "Configuration Two"),
        ("metric-1", "Metric One Data"),
        ("metric-2", "Metric Two Data"),
        ("metric-3", "Metric Three Data"),
        ("trace-1", "Trace One Data"),
        ("trace-2", "Trace Two Data")
      ]
    }
  }
  
  // Create cache
  let create_cache = fn(max_size: Int) {
    {
      entries: [],
      max_size,
      warm_up_complete: false
    }
  }
  
  // Warm up cache with high-priority data
  let warm_up_cache = fn(cache: WarmedCache, data_source: DataSource, priority_threshold: Int) {
    let mut warmed_entries = []
    
    for data in data_source.data {
      let priority = match data.0 {
        "config-1" => 10,
        "config-2" => 9,
        "user-1" => 8,
        "user-2" => 7,
        "user-3" => 6,
        "metric-1" => 5,
        "metric-2" => 4,
        "metric-3" => 3,
        "trace-1" => 2,
        "trace-2" => 1,
        _ => 0
      }
      
      if priority >= priority_threshold {
        warmed_entries = warmed_entries.push({
          key: data.0,
          value: data.1,
          priority,
          access_count: 0
        })
      }
    }
    
    // Sort by priority (highest first)
    let mut sorted = true
    while sorted {
      sorted = false
      for i in 0..warmed_entries.length() - 1 {
        if warmed_entries[i].priority < warmed_entries[i + 1].priority {
          let temp = warmed_entries[i]
          warmed_entries[i] = warmed_entries[i + 1]
          warmed_entries[i + 1] = temp
          sorted = true
        }
      }
    }
    
    // Limit to cache capacity
    let mut final_entries = []
    for i in 0..warmed_entries.length() {
      if i < cache.max_size {
        final_entries = final_entries.push(warmed_entries[i])
      }
    }
    
    {
      entries: final_entries,
      max_size: cache.max_size,
      warm_up_complete: true
    }
  }
  
  // Preload specific keys
  let preload_keys = fn(cache: WarmedCache, data_source: DataSource, keys: Array[String>) {
    let mut preloaded_entries = cache.entries
    
    for key in keys {
      let mut found = false
      
      // Check if already in cache
      for entry in preloaded_entries {
        if entry.key == key {
          found = true
        }
      }
      
      if not(found) {
        // Find in data source
        for data in data_source.data {
          if data.0 == key {
            let priority = match data.0 {
              "config-1" => 10,
              "config-2" => 9,
              "user-1" => 8,
              "user-2" => 7,
              "user-3" => 6,
              "metric-1" => 5,
              "metric-2" => 4,
              "metric-3" => 3,
              "trace-1" => 2,
              "trace-2" => 1,
              _ => 0
            }
            
            // Add to cache (evict if necessary)
            if preloaded_entries.length() >= cache.max_size {
              // Evict lowest priority
              let mut min_priority = 999
              let mut min_index = -1
              
              for i in 0..preloaded_entries.length() {
                if preloaded_entries[i].priority < min_priority {
                  min_priority = preloaded_entries[i].priority
                  min_index = i
                }
              }
              
              let mut updated_entries = []
              for i in 0..preloaded_entries.length() {
                if i != min_index {
                  updated_entries = updated_entries.push(preloaded_entries[i])
                }
              }
              
              preloaded_entries = updated_entries.push({
                key: data.0,
                value: data.1,
                priority,
                access_count: 0
              })
            } else {
              preloaded_entries = preloaded_entries.push({
                key: data.0,
                value: data.1,
                priority,
                access_count: 0
              })
            }
          }
        }
      }
    }
    
    {
      entries: preloaded_entries,
      max_size: cache.max_size,
      warm_up_complete: cache.warm_up_complete
    }
  }
  
  // Get from cache
  let get_from_cache = fn(cache: WarmedCache, key: String) {
    let mut found = false
    let mut value = None
    let mut found_index = -1
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        found = true
        value = Some(cache.entries[i].value)
        found_index = i
      }
    }
    
    {
      value,
      found
    }
  }
  
  // Create cache and data source
  let mut cache = create_cache(5)
  let data_source = create_data_source()
  
  // Test empty cache
  let empty_result = get_from_cache(cache, "config-1")
  assert_eq(empty_result.value, None)
  assert_false(empty_result.found)
  
  // Warm up cache with high-priority data (priority >= 8)
  cache = warm_up_cache(cache, data_source, 8)
  
  // Check warmed cache
  assert_true(cache.warm_up_complete)
  assert_eq(cache.entries.length(), 3)  // config-1, config-2, user-1, user-2, user-3 -> top 5, but priority >= 8 gives 3
  
  // Check warmed entries (should be sorted by priority)
  assert_eq(cache.entries[0].key, "config-1")
  assert_eq(cache.entries[1].key, "config-2")
  assert_eq(cache.entries[2].key, "user-1")
  
  // Test warmed cache hits
  let warmed_result = get_from_cache(cache, "config-1")
  assert_eq(warmed_result.value, Some("Configuration One"))
  assert_true(warmed_result.found)
  
  // Test cache miss for non-warmed data
  let miss_result = get_from_cache(cache, "metric-1")
  assert_eq(miss_result.value, None)
  assert_false(miss_result.found)
  
  // Preload specific keys
  let keys_to_preload = ["metric-1", "trace-1", "user-999"]  // user-999 doesn't exist
  cache = preload_keys(cache, data_source, keys_to_preload)
  
  // Check preloaded cache
  assert_eq(cache.entries.length(), 5)  // Should be at max capacity
  
  // Check that metric-1 and trace-1 are loaded
  let metric_result = get_from_cache(cache, "metric-1")
  assert_eq(metric_result.value, Some("Metric One Data"))
  assert_true(metric_result.found)
  
  let trace_result = get_from_cache(cache, "trace-1")
  assert_eq(trace_result.value, Some("Trace One Data"))
  assert_true(trace_result.found)
  
  // Check that user-999 is not loaded (doesn't exist)
  let nonexistent_result = get_from_cache(cache, "user-999")
  assert_eq(nonexistent_result.value, None)
  assert_false(nonexistent_result.found)
  
  // Check that low-priority entries were evicted
  let evicted_result = get_from_cache(cache, "user-2")
  assert_eq(evicted_result.value, None)
  assert_false(evicted_result.found)
}