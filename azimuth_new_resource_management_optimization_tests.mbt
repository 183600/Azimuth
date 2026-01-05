// Azimuth Telemetry System - New Resource Management Optimization Tests
// This file contains test cases for resource management and optimization functionality

// Test 1: Resource Pool Management
test "resource pool management operations" {
  // Define resource pool structure
  type ResourcePool = {
    mut resources: Array[String],
    mut allocated: Array[String],
    mut max_size: Int,
    mut current_size: Int
  }
  
  // Create resource pool
  let create_pool = fn(max_size: Int) {
    {
      mut resources: [],
      mut allocated: [],
      mut max_size: max_size,
      mut current_size: 0
    }
  }
  
  // Add resource to pool
  let add_resource = fn(pool: ResourcePool, resource: String) {
    if pool.current_size < pool.max_size {
      pool.resources = pool.resources.push(resource)
      pool.current_size = pool.current_size + 1
      true
    } else {
      false
    }
  }
  
  // Allocate resource from pool
  let allocate_resource = fn(pool: ResourcePool) {
    if pool.resources.length() > 0 {
      let resource = pool.resources[0]
      pool.resources = pool.resources.slice(1, pool.resources.length())
      pool.allocated = pool.allocated.push(resource)
      Some(resource)
    } else {
      None
    }
  }
  
  // Release resource back to pool
  let release_resource = fn(pool: ResourcePool, resource: String) {
    if pool.allocated.contains(resource) {
      pool.allocated = pool.allocated.filter_fn(r) { r != resource }
      pool.resources = pool.resources.push(resource)
      true
    } else {
      false
    }
  }
  
  // Get pool statistics
  let get_pool_stats = fn(pool: ResourcePool) {
    {
      total_resources: pool.current_size,
      available_resources: pool.resources.length(),
      allocated_resources: pool.allocated.length(),
      utilization_rate: if pool.current_size > 0 {
        (pool.allocated.length() * 100) / pool.current_size
      } else {
        0
      }
    }
  }
  
  // Test resource pool operations
  let pool = create_pool(5)
  
  // Add resources to pool
  assert_true(add_resource(pool, "resource-1"))
  assert_true(add_resource(pool, "resource-2"))
  assert_true(add_resource(pool, "resource-3"))
  assert_true(add_resource(pool, "resource-4"))
  assert_true(add_resource(pool, "resource-5"))
  assert_false(add_resource(pool, "resource-6")) // Pool is full
  
  // Check pool statistics
  let stats = get_pool_stats(pool)
  assert_eq(stats.total_resources, 5)
  assert_eq(stats.available_resources, 5)
  assert_eq(stats.allocated_resources, 0)
  assert_eq(stats.utilization_rate, 0)
  
  // Allocate resources
  let allocated1 = allocate_resource(pool)
  let allocated2 = allocate_resource(pool)
  let allocated3 = allocate_resource(pool)
  
  assert_eq(allocated1, Some("resource-1"))
  assert_eq(allocated2, Some("resource-2"))
  assert_eq(allocated3, Some("resource-3"))
  
  // Check updated statistics
  let stats_after_allocation = get_pool_stats(pool)
  assert_eq(stats_after_allocation.total_resources, 5)
  assert_eq(stats_after_allocation.available_resources, 2)
  assert_eq(stats_after_allocation.allocated_resources, 3)
  assert_eq(stats_after_allocation.utilization_rate, 60) // floor(3 * 100 / 5)
  
  // Release resources
  assert_true(release_resource(pool, "resource-1"))
  assert_true(release_resource(pool, "resource-2"))
  assert_false(release_resource(pool, "nonexistent")) // Not allocated
  
  // Check final statistics
  let stats_after_release = get_pool_stats(pool)
  assert_eq(stats_after_release.total_resources, 5)
  assert_eq(stats_after_release.available_resources, 4)
  assert_eq(stats_after_release.allocated_resources, 1)
  assert_eq(stats_after_release.utilization_rate, 20) // floor(1 * 100 / 5)
}

// Test 2: Memory Usage Optimization
test "memory usage optimization operations" {
  // Define memory usage tracker
  type MemoryTracker = {
    mut total_allocated: Int,
    mut peak_usage: Int,
    mut allocation_count: Int,
    mut deallocation_count: Int,
    mut current_allocations: Array[(String, Int)]
  }
  
  // Create memory tracker
  let create_tracker = fn() {
    {
      mut total_allocated: 0,
      mut peak_usage: 0,
      mut allocation_count: 0,
      mut deallocation_count: 0,
      mut current_allocations: []
    }
  }
  
  // Allocate memory
  let allocate_memory = fn(tracker: MemoryTracker, id: String, size: Int) {
    tracker.total_allocated = tracker.total_allocated + size
    tracker.allocation_count = tracker.allocation_count + 1
    tracker.current_allocations = tracker.current_allocations.push((id, size))
    
    let current_usage = tracker.current_allocations.reduce(fn(sum, allocation) {
      sum + allocation.1
    }, 0)
    
    if current_usage > tracker.peak_usage {
      tracker.peak_usage = current_usage
    }
    
    current_usage
  }
  
  // Deallocate memory
  let deallocate_memory = fn(tracker: MemoryTracker, id: String) {
    let allocation = tracker.current_allocations.find_fn(a) { a.0 == id }
    
    match allocation {
      Some((_, size)) => {
        tracker.current_allocations = tracker.current_allocations.filter_fn(a) { a.0 != id }
        tracker.deallocation_count = tracker.deallocation_count + 1
        Some(size)
      }
      None => {
        None
      }
    }
  }
  
  // Get memory statistics
  let get_memory_stats = fn(tracker: MemoryTracker) {
    let current_usage = tracker.current_allocations.reduce(fn(sum, allocation) {
      sum + allocation.1
    }, 0)
    
    {
      total_allocated: tracker.total_allocated,
      peak_usage: tracker.peak_usage,
      current_usage: current_usage,
      allocation_count: tracker.allocation_count,
      deallocation_count: tracker.deallocation_count,
      active_allocations: tracker.current_allocations.length(),
      leak_count: tracker.allocation_count - tracker.deallocation_count
    }
  }
  
  // Test memory tracking
  let tracker = create_tracker()
  
  // Allocate memory
  let usage1 = allocate_memory(tracker, "alloc-1", 1024)
  let usage2 = allocate_memory(tracker, "alloc-2", 2048)
  let usage3 = allocate_memory(tracker, "alloc-3", 512)
  
  assert_eq(usage1, 1024)
  assert_eq(usage2, 3072) // 1024 + 2048
  assert_eq(usage3, 3584) // 3072 + 512
  
  // Check memory statistics
  let stats = get_memory_stats(tracker)
  assert_eq(stats.total_allocated, 3584) // 1024 + 2048 + 512
  assert_eq(stats.peak_usage, 3584)
  assert_eq(stats.current_usage, 3584)
  assert_eq(stats.allocation_count, 3)
  assert_eq(stats.deallocation_count, 0)
  assert_eq(stats.active_allocations, 3)
  assert_eq(stats.leak_count, 3)
  
  // Deallocate memory
  let deallocated1 = deallocate_memory(tracker, "alloc-1")
  let deallocated2 = deallocate_memory(tracker, "nonexistent")
  
  assert_eq(deallocated1, Some(1024))
  assert_eq(deallocated2, None)
  
  // Check updated statistics
  let stats_after_dealloc = get_memory_stats(tracker)
  assert_eq(stats_after_dealloc.total_allocated, 3584) // Unchanged
  assert_eq(stats_after_dealloc.peak_usage, 3584) // Unchanged
  assert_eq(stats_after_dealloc.current_usage, 2560) // 3584 - 1024
  assert_eq(stats_after_dealloc.allocation_count, 3) // Unchanged
  assert_eq(stats_after_dealloc.deallocation_count, 1)
  assert_eq(stats_after_dealloc.active_allocations, 2)
  assert_eq(stats_after_dealloc.leak_count, 2) // 3 - 1
}

// Test 3: Connection Pool Optimization
test "connection pool optimization operations" {
  // Define connection pool
  type ConnectionPool = {
    mut connections: Array[String],
    mut active_connections: Array[String],
    mut max_connections: Int,
    mut min_connections: Int,
    mut connection_timeout: Int,
    mut idle_timeout: Int,
    mut last_usage: Array[(String, Int)]
  }
  
  // Create connection pool
  let create_connection_pool = fn(min_connections: Int, max_connections: Int, connection_timeout: Int, idle_timeout: Int) {
    let mut connections = []
    let mut last_usage = []
    let mut i = 0
    
    while i < min_connections {
      let conn_id = "conn-" + i.to_string()
      connections = connections.push(conn_id)
      last_usage = last_usage.push((conn_id, 1640995200)) // Current time
      i = i + 1
    }
    
    {
      mut connections: connections,
      mut active_connections: [],
      mut max_connections: max_connections,
      mut min_connections: min_connections,
      mut connection_timeout: connection_timeout,
      mut idle_timeout: idle_timeout,
      mut last_usage: last_usage
    }
  }
  
  // Get connection from pool
  let get_connection = fn(pool: ConnectionPool, current_time: Int) {
    // First, clean up idle connections
    let mut i = 0
    while i < pool.connections.length() {
      let conn_id = pool.connections[i]
      let last_used = pool.last_usage.find_fn(usage) { usage.0 == conn_id }.1
      
      if current_time - last_used > pool.idle_timeout and 
         pool.connections.length() > pool.min_connections {
        pool.connections = pool.connections.filter_fn(c) { c != conn_id }
        pool.last_usage = pool.last_usage.filter_fn(usage) { usage.0 != conn_id }
      } else {
        i = i + 1
      }
    }
    
    // Add new connection if needed
    if pool.connections.length() == 0 and 
       pool.active_connections.length() < pool.max_connections {
      let new_conn_id = "conn-new-" + current_time.to_string()
      pool.connections = pool.connections.push(new_conn_id)
      pool.last_usage = pool.last_usage.push((new_conn_id, current_time))
    }
    
    // Get available connection
    if pool.connections.length() > 0 {
      let conn_id = pool.connections[0]
      pool.connections = pool.connections.slice(1, pool.connections.length())
      pool.active_connections = pool.active_connections.push(conn_id)
      
      // Update last usage
      let mut i = 0
      while i < pool.last_usage.length() {
        if pool.last_usage[i].0 == conn_id {
          pool.last_usage[i] = (conn_id, current_time)
          break
        }
        i = i + 1
      }
      
      Some(conn_id)
    } else {
      None
    }
  }
  
  // Release connection back to pool
  let release_connection = fn(pool: ConnectionPool, conn_id: String, current_time: Int) {
    if pool.active_connections.contains(conn_id) {
      pool.active_connections = pool.active_connections.filter_fn(c) { c != conn_id }
      pool.connections = pool.connections.push(conn_id)
      
      // Update last usage
      let mut i = 0
      while i < pool.last_usage.length() {
        if pool.last_usage[i].0 == conn_id {
          pool.last_usage[i] = (conn_id, current_time)
          break
        }
        i = i + 1
      }
      
      true
    } else {
      false
    }
  }
  
  // Get pool statistics
  let get_pool_stats = fn(pool: ConnectionPool) {
    {
      total_connections: pool.connections.length() + pool.active_connections.length(),
      available_connections: pool.connections.length(),
      active_connections: pool.active_connections.length(),
      max_connections: pool.max_connections,
      min_connections: pool.min_connections,
      utilization_rate: if pool.max_connections > 0 {
        (pool.active_connections.length() * 100) / pool.max_connections
      } else {
        0
      }
    }
  }
  
  // Test connection pool
  let pool = create_connection_pool(2, 5, 30, 300) // 2 min, 5 max, 30s timeout, 5min idle
  
  // Check initial state
  let initial_stats = get_pool_stats(pool)
  assert_eq(initial_stats.total_connections, 2)
  assert_eq(initial_stats.available_connections, 2)
  assert_eq(initial_stats.active_connections, 0)
  
  // Get connections
  let conn1 = get_connection(pool, 1640995200)
  let conn2 = get_connection(pool, 1640995200)
  let conn3 = get_connection(pool, 1640995200) // Should create a new connection
  
  assert_eq(conn1, Some("conn-0"))
  assert_eq(conn2, Some("conn-1"))
  assert_true(conn3.unwrap().starts_with("conn-new-")) // New connection
  
  // Check state after getting connections
  let stats_after_get = get_pool_stats(pool)
  assert_eq(stats_after_get.total_connections, 3)
  assert_eq(stats_after_get.available_connections, 0)
  assert_eq(stats_after_get.active_connections, 3)
  assert_eq(stats_after_get.utilization_rate, 60) // floor(3 * 100 / 5)
  
  // Release connections
  assert_true(release_connection(pool, "conn-0", 1640995300))
  assert_true(release_connection(pool, "conn-1", 1640995300))
  assert_false(release_connection(pool, "nonexistent", 1640995300))
  
  // Check state after releasing connections
  let stats_after_release = get_pool_stats(pool)
  assert_eq(stats_after_release.total_connections, 3)
  assert_eq(stats_after_release.available_connections, 2)
  assert_eq(stats_after_release.active_connections, 1)
  assert_eq(stats_after_release.utilization_rate, 20) // floor(1 * 100 / 5)
  
  // Test idle connection cleanup
  let conn4 = get_connection(pool, 1640995300)
  assert_true(release_connection(pool, conn4.unwrap(), 1640995300))
  
  // Simulate time passing (beyond idle timeout)
  let conn5 = get_connection(pool, 1640995700) // 400 seconds later
  
  // Check that idle connections were cleaned up
  let stats_after_idle = get_pool_stats(pool)
  assert_eq(stats_after_idle.total_connections, 3) // One active + two min connections
  assert_eq(stats_after_idle.available_connections, 2)
  assert_eq(stats_after_idle.active_connections, 1)
}

// Test 4: Cache Management Optimization
test "cache management optimization operations" {
  // Define cache entry
  type CacheEntry = {
    key: String,
    value: String,
    created_at: Int,
    last_accessed: Int,
    access_count: Int,
    size: Int
  }
  
  // Define cache
  type Cache = {
    mut entries: Array[CacheEntry],
    mut max_size: Int,
    mut max_memory: Int,
    mut current_memory: Int,
    mut ttl: Int // Time to live in seconds
  }
  
  // Create cache
  let create_cache = fn(max_size: Int, max_memory: Int, ttl: Int) {
    {
      mut entries: [],
      mut max_size: max_size,
      mut max_memory: max_memory,
      mut current_memory: 0,
      mut ttl: ttl
    }
  }
  
  // Add entry to cache
  let add_entry = fn(cache: Cache, key: String, value: String, current_time: Int) {
    let entry_size = key.length() + value.length()
    
    // Check if we need to evict entries
    while (cache.entries.length() >= cache.max_size or 
           cache.current_memory + entry_size > cache.max_memory) and 
          cache.entries.length() > 0 {
      // Find LRU entry (least recently used)
      let mut lru_index = 0
      let mut lru_time = cache.entries[0].last_accessed
      
      let mut i = 1
      while i < cache.entries.length() {
        if cache.entries[i].last_accessed < lru_time {
          lru_time = cache.entries[i].last_accessed
          lru_index = i
        }
        i = i + 1
      }
      
      // Remove LRU entry
      let removed_entry = cache.entries[lru_index]
      cache.current_memory = cache.current_memory - removed_entry.size
      cache.entries = cache.entries.slice(0, lru_index) + 
                     cache.entries.slice(lru_index + 1, cache.entries.length())
    }
    
    // Check if key already exists
    let existing_index = cache.entries.index_of_fn(entry) { entry.key == key }
    
    if existing_index >= 0 {
      // Update existing entry
      let old_entry = cache.entries[existing_index]
      cache.current_memory = cache.current_memory - old_entry.size + entry_size
      
      cache.entries[existing_index] = {
        key: key,
        value: value,
        created_at: old_entry.created_at,
        last_accessed: current_time,
        access_count: old_entry.access_count + 1,
        size: entry_size
      }
    } else {
      // Add new entry
      cache.entries = cache.entries.push({
        key: key,
        value: value,
        created_at: current_time,
        last_accessed: current_time,
        access_count: 1,
        size: entry_size
      })
      cache.current_memory = cache.current_memory + entry_size
    }
  }
  
  // Get entry from cache
  let get_entry = fn(cache: Cache, key: String, current_time: Int) {
    let entry_index = cache.entries.index_of_fn(entry) { entry.key == key }
    
    if entry_index >= 0 {
      let entry = cache.entries[entry_index]
      
      // Check if entry is expired
      if current_time - entry.created_at > cache.ttl {
        // Remove expired entry
        cache.current_memory = cache.current_memory - entry.size
        cache.entries = cache.entries.slice(0, entry_index) + 
                       cache.entries.slice(entry_index + 1, cache.entries.length())
        None
      } else {
        // Update access information
        cache.entries[entry_index] = {
          key: entry.key,
          value: entry.value,
          created_at: entry.created_at,
          last_accessed: current_time,
          access_count: entry.access_count + 1,
          size: entry.size
        }
        
        Some(entry.value)
      }
    } else {
      None
    }
  }
  
  // Get cache statistics
  let get_cache_stats = fn(cache: Cache) {
    {
      entry_count: cache.entries.length(),
      max_size: cache.max_size,
      current_memory: cache.current_memory,
      max_memory: cache.max_memory,
      memory_utilization: if cache.max_memory > 0 {
        (cache.current_memory * 100) / cache.max_memory
      } else {
        0
      },
      size_utilization: if cache.max_size > 0 {
        (cache.entries.length() * 100) / cache.max_size
      } else {
        0
      }
    }
  }
  
  // Test cache operations
  let cache = create_cache(3, 100, 60) // 3 entries, 100 bytes, 60s TTL
  
  // Add entries
  add_entry(cache, "key1", "value1", 1640995200)
  add_entry(cache, "key2", "value2", 1640995200)
  add_entry(cache, "key3", "value3", 1640995200)
  
  // Check cache statistics
  let stats = get_cache_stats(cache)
  assert_eq(stats.entry_count, 3)
  assert_eq(stats.current_memory, 24) // (4+6)+(4+6)+(4+6) = 24
  assert_eq(stats.memory_utilization, 24) // floor(24 * 100 / 100)
  assert_eq(stats.size_utilization, 100) // floor(3 * 100 / 3)
  
  // Test cache hits
  let value1 = get_entry(cache, "key1", 1640995250)
  let value2 = get_entry(cache, "key2", 1640995250)
  let value3 = get_entry(cache, "nonexistent", 1640995250)
  
  assert_eq(value1, Some("value1"))
  assert_eq(value2, Some("value2"))
  assert_eq(value3, None)
  
  // Test cache eviction
  add_entry(cache, "key4", "value4", 1640995300) // Should evict LRU entry
  
  let stats_after_eviction = get_cache_stats(cache)
  assert_eq(stats_after_eviction.entry_count, 3) // Still 3 entries
  
  // Check that key3 was evicted (LRU)
  let evicted_value = get_entry(cache, "key3", 1640995350)
  assert_eq(evicted_value, None)
  
  // Check that key1 and key2 are still there (accessed more recently)
  let retained_value1 = get_entry(cache, "key1", 1640995350)
  let retained_value2 = get_entry(cache, "key2", 1640995350)
  assert_eq(retained_value1, Some("value1"))
  assert_eq(retained_value2, Some("value2"))
  
  // Test TTL expiration
  add_entry(cache, "key5", "value5", 1640995200) // Added at 1640995200
  let expired_value = get_entry(cache, "key5", 1640995400) // Access at 1640995400 (200s later)
  assert_eq(expired_value, None) // Should be expired (TTL is 60s)
}

// Test 5: Resource Usage Monitoring
test "resource usage monitoring operations" {
  // Define resource monitor
  type ResourceMonitor = {
    mut cpu_usage: Array[(Int, Float)], // (timestamp, usage_percent)
    mut memory_usage: Array[(Int, Int)], // (timestamp, usage_bytes)
    mut disk_io: Array[(Int, (Int, Int))], // (timestamp, (read_bytes, write_bytes))
    mut network_io: Array[(Int, (Int, Int))], // (timestamp, (recv_bytes, sent_bytes))
    mut max_history: Int
  }
  
  // Create resource monitor
  let create_monitor = fn(max_history: Int) {
    {
      mut cpu_usage: [],
      mut memory_usage: [],
      mut disk_io: [],
      mut network_io: [],
      mut max_history: max_history
    }
  }
  
  // Record CPU usage
  let record_cpu_usage = fn(monitor: ResourceMonitor, timestamp: Int, usage_percent: Float) {
    monitor.cpu_usage = monitor.cpu_usage.push((timestamp, usage_percent))
    
    // Keep only the most recent entries
    if monitor.cpu_usage.length() > monitor.max_history {
      monitor.cpu_usage = monitor.cpu_usage.slice(
        monitor.cpu_usage.length() - monitor.max_history, 
        monitor.cpu_usage.length()
      )
    }
  }
  
  // Record memory usage
  let record_memory_usage = fn(monitor: ResourceMonitor, timestamp: Int, usage_bytes: Int) {
    monitor.memory_usage = monitor.memory_usage.push((timestamp, usage_bytes))
    
    // Keep only the most recent entries
    if monitor.memory_usage.length() > monitor.max_history {
      monitor.memory_usage = monitor.memory_usage.slice(
        monitor.memory_usage.length() - monitor.max_history, 
        monitor.memory_usage.length()
      )
    }
  }
  
  // Get average CPU usage
  let get_average_cpu_usage = fn(monitor: ResourceMonitor, start_time: Int, end_time: Int) {
    let filtered_usage = monitor.cpu_usage.filter_fn(entry) {
      entry.0 >= start_time and entry.0 <= end_time
    }
    
    if filtered_usage.length() > 0 {
      let total = filtered_usage.reduce(fn(sum, entry) { sum + entry.1 }, 0.0)
      total / filtered_usage.length().to_float()
    } else {
      0.0
    }
  }
  
  // Get peak memory usage
  let get_peak_memory_usage = fn(monitor: ResourceMonitor, start_time: Int, end_time: Int) {
    let filtered_usage = monitor.memory_usage.filter_fn(entry) {
      entry.0 >= start_time and entry.0 <= end_time
    }
    
    if filtered_usage.length() > 0 {
      filtered_usage.reduce(fn(max, entry) { 
        if entry.1 > max { entry.1 } else { max } 
      }, 0)
    } else {
      0
    }
  }
  
  // Get resource trends
  let get_resource_trends = fn(monitor: ResourceMonitor, resource_type: String) {
    let data = match resource_type {
      "cpu" => monitor.cpu_usage.map_fn(entry) { entry.1 }
      "memory" => monitor.memory_usage.map_fn(entry) { entry.1.to_float() }
      _ => []
    }
    
    if data.length() < 2 {
      "stable"
    } else {
      let first_half = data.slice(0, data.length() / 2)
      let second_half = data.slice(data.length() / 2, data.length())
      
      let first_avg = first_half.reduce(fn(sum, value) { sum + value }, 0.0) / first_half.length().to_float()
      let second_avg = second_half.reduce(fn(sum, value) { sum + value }, 0.0) / second_half.length().to_float()
      
      let change_percent = ((second_avg - first_avg) * 100.0) / first_avg
      
      if change_percent > 10.0 {
        "increasing"
      } else if change_percent < -10.0 {
        "decreasing"
      } else {
        "stable"
      }
    }
  }
  
  // Test resource monitoring
  let monitor = create_monitor(10)
  
  // Record resource usage
  record_cpu_usage(monitor, 1640995200, 25.5)
  record_cpu_usage(monitor, 1640995210, 30.2)
  record_cpu_usage(monitor, 1640995220, 28.7)
  record_cpu_usage(monitor, 1640995230, 35.1)
  record_cpu_usage(monitor, 1640995240, 40.3)
  
  record_memory_usage(monitor, 1640995200, 1024000)
  record_memory_usage(monitor, 1640995210, 1126400)
  record_memory_usage(monitor, 1640995220, 1228800)
  record_memory_usage(monitor, 1640995230, 1331200)
  record_memory_usage(monitor, 1640995240, 1433600)
  
  // Test average CPU usage
  let avg_cpu = get_average_cpu_usage(monitor, 1640995200, 1640995240)
  assert_eq(avg_cpu, 31.96) // (25.5 + 30.2 + 28.7 + 35.1 + 40.3) / 5
  
  // Test peak memory usage
  let peak_memory = get_peak_memory_usage(monitor, 1640995200, 1640995240)
  assert_eq(peak_memory, 1433600)
  
  // Test resource trends
  let cpu_trend = get_resource_trends(monitor, "cpu")
  assert_eq(cpu_trend, "increasing") // CPU usage is trending up
  
  let memory_trend = get_resource_trends(monitor, "memory")
  assert_eq(memory_trend, "increasing") // Memory usage is trending up
  
  // Test with insufficient data
  let empty_monitor = create_monitor(10)
  let empty_avg = get_average_cpu_usage(empty_monitor, 1640995200, 1640995240)
  assert_eq(empty_avg, 0.0)
  
  let empty_trend = get_resource_trends(empty_monitor, "cpu")
  assert_eq(empty_trend, "stable")
  
  // Test with single data point
  record_cpu_usage(empty_monitor, 1640995200, 50.0)
  let single_trend = get_resource_trends(empty_monitor, "cpu")
  assert_eq(single_trend, "stable")
}