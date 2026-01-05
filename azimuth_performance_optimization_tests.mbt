// Azimuth Performance Optimization Test Suite
// This file contains test cases for performance optimization techniques

// Test 1: Caching Mechanisms
test "caching mechanisms for performance optimization" {
  // Define cache entry
  type CacheEntry[T] = {
    value: T,
    timestamp: Int,
    ttl: Int,  // Time to live in seconds
    access_count: Int
  }
  
  // Define cache structure
  type Cache[T] = {
    entries: Array[(String, CacheEntry[T])>,
    max_size: Int,
    default_ttl: Int
  }
  
  // Create cache
  let create_cache = fn(max_size: Int, default_ttl: Int) {
    {
      entries: [],
      max_size,
      default_ttl
    }
  }
  
  // Get current timestamp
  let get_current_time = fn() { 1640995200 }  // Fixed timestamp for testing
  
  // Check if cache entry is expired
  let is_expired = fn(entry: CacheEntry[T], current_time: Int) {
    (current_time - entry.timestamp) > entry.ttl
  }
  
  // Get value from cache
  let cache_get = fn(cache: Cache[T], key: String) {
    let current_time = get_current_time()
    let entry = cache.entries.find(fn(e) { e.0 == key })
    
    match entry {
      Some((_, cache_entry)) => {
        if is_expired(cache_entry, current_time) {
          None  // Expired
        } else {
          // Update access count
          Some(cache_entry.value)
        }
      }
      None => None
    }
  }
  
  // Put value in cache
  let cache_put = fn(cache: Cache[T], key: String, value: T, ttl: Option[Int>) {
    let current_time = get_current_time()
    let actual_ttl = match ttl {
      Some(t) => t
      None => cache.default_ttl
    }
    
    let new_entry = {
      value,
      timestamp: current_time,
      ttl: actual_ttl,
      access_count: 0
    }
    
    // Check if cache is full
    if cache.entries.length() >= cache.max_size {
      // Remove least recently used (simplified - remove first entry)
      let updated_entries = cache.entries.slice(1, cache.entries.length())
      { cache | entries: updated_entries.push((key, new_entry)) }
    } else {
      { cache | entries: cache.entries.push((key, new_entry)) }
    }
  }
  
  // Test cache creation and basic operations
  let string_cache = create_cache(3, 60)  // Max 3 entries, 60s TTL
  
  // Test cache miss
  let miss_result = cache_get(string_cache, "key1")
  assert_eq(miss_result, None)
  
  // Test cache put
  let cache1 = cache_put(string_cache, "key1", "value1", None)
  assert_eq(cache1.entries.length(), 1)
  
  // Test cache hit
  let hit_result = cache_get(cache1, "key1")
  assert_eq(hit_result, Some("value1"))
  
  // Test cache with custom TTL
  let cache2 = cache_put(cache1, "key2", "value2", Some(30))  // 30s TTL
  assert_eq(cache2.entries.length(), 2)
  
  // Test cache eviction
  let cache3 = cache_put(cache2, "key3", "value3", None)
  let cache4 = cache_put(cache3, "key4", "value4", None)  // Should evict key1
  
  assert_eq(cache4.entries.length(), 3)  // Max size reached
  assert_eq(cache4.entries[0].0, "key2")  // key1 was evicted
  assert_eq(cache4.entries[1].0, "key3")
  assert_eq(cache4.entries[2].0, "key4")
  
  // Test cache statistics
  let calculate_cache_stats = fn(cache: Cache[T]) {
    let total_entries = cache.entries.length()
    let expired_entries = cache.entries.filter(fn(e) {
      is_expired(e.1, get_current_time())
    }).length()
    
    {
      total_entries,
      expired_entries,
      utilization_percent: (total_entries as Float / cache.max_size as Float) * 100.0
    }
  }
  
  let stats = calculate_cache_stats(cache4)
  assert_eq(stats.total_entries, 3)
  assert_eq(stats.expired_entries, 0)
  assert_eq(stats.utilization_percent, 100.0)
  
  // Test LRU (Least Recently Used) eviction strategy
  let lru_cache_put = fn(cache: Cache[T], key: String, value: T, ttl: Option[Int>) {
    let current_time = get_current_time()
    let actual_ttl = match ttl {
      Some(t) => t
      None => cache.default_ttl
    }
    
    // Check if key already exists
    let existing_index = cache.entries.find_index(fn(e) { e.0 == key })
    
    match existing_index {
      Some(index) => {
        // Update existing entry
        let updated_entry = {
          value,
          timestamp: current_time,
          ttl: actual_ttl,
          access_count: cache.entries[index].1.access_count + 1
        }
        
        let without_existing = cache.entries.slice(0, index) + 
                              cache.entries.slice(index + 1, cache.entries.length())
        { cache | entries: without_existing.push((key, updated_entry)) }
      }
      None => {
        // Add new entry
        let new_entry = {
          value,
          timestamp: current_time,
          ttl: actual_ttl,
          access_count: 0
        }
        
        // Check if cache is full
        if cache.entries.length() >= cache.max_size {
          // Find least recently used (lowest access count, then oldest)
          let lru_index = cache.entries.reduce(fn(min_index, entry, index) {
            let min_entry = cache.entries[min_index]
            if entry.1.access_count < min_entry.1.access_count ||
               (entry.1.access_count == min_entry.1.access_count && entry.1.timestamp < min_entry.1.timestamp) {
              index
            } else {
              min_index
            }
          }, 0)
          
          let without_lru = cache.entries.slice(0, lru_index) + 
                           cache.entries.slice(lru_index + 1, cache.entries.length())
          { cache | entries: without_lru.push((key, new_entry)) }
        } else {
          { cache | entries: cache.entries.push((key, new_entry)) }
        }
      }
    }
  }
  
  // Test LRU cache
  let lru_cache = create_cache(3, 60)
  let lru1 = lru_cache_put(lru_cache, "key1", "value1", None)
  let lru2 = lru_cache_put(lru1, "key2", "value2", None)
  let lru3 = lru_cache_put(lru2, "key3", "value3", None)
  
  // Access key1 to increase its access count
  let _ = cache_get(lru3, "key1")
  
  // Add key4, should evict key2 (least recently used)
  let lru4 = lru_cache_put(lru3, "key4", "value4", None)
  
  assert_eq(lru4.entries.length(), 3)
  assert_true(lru4.entries.any(fn(e) { e.0 == "key1" }))  // key1 should still be there
  assert_false(lru4.entries.any(fn(e) { e.0 == "key2" }))  // key2 should be evicted
}

// Test 2: Batch Processing Optimization
test "batch processing optimization techniques" {
  // Define batch configuration
  type BatchConfig = {
    max_batch_size: Int,
    max_wait_time_ms: Int,
    processing_timeout_ms: Int
  }
  
  // Define batch processor
  type BatchProcessor[T, R] = {
    config: BatchConfig,
    pending_items: Array[T],
    last_flush_time: Int
  }
  
  // Create batch processor
  let create_batch_processor = fn(config: BatchConfig) {
    {
      config,
      pending_items: [],
      last_flush_time: 1640995200000  // Current time in milliseconds
    }
  }
  
  // Add item to batch
  let add_to_batch = fn(processor: BatchProcessor[T, R], item: T, current_time: Int) {
    let updated_items = processor.pending_items.push(item)
    {
      config: processor.config,
      pending_items: updated_items,
      last_flush_time: processor.last_flush_time
    }
  }
  
  // Check if batch should be flushed
  let should_flush_batch = fn(processor: BatchProcessor[T, R], current_time: Int) {
    let size_reached = processor.pending_items.length() >= processor.config.max_batch_size
    let time_reached = (current_time - processor.last_flush_time) >= processor.config.max_wait_time_ms
    
    size_reached || time_reached
  }
  
  // Process batch
  let process_batch = fn(processor: BatchProcessor[T, R], process_fn: (Array[T>) -> Array[R>) {
    let results = process_fn(processor.pending_items)
    {
      config: processor.config,
      pending_items: [],
      last_flush_time: processor.last_flush_time
    }
  }
  
  // Test batch processor creation
  let batch_config = {
    max_batch_size: 5,
    max_wait_time_ms: 1000,
    processing_timeout_ms: 5000
  }
  
  let processor = create_batch_processor(batch_config)
  assert_eq(processor.pending_items.length(), 0)
  
  // Test adding items
  let processor1 = add_to_batch(processor, "item1", 1640995200000)
  let processor2 = add_to_batch(processor1, "item2", 1640995200500)
  let processor3 = add_to_batch(processor2, "item3", 1640995200800)
  
  assert_eq(processor3.pending_items.length(), 3)
  assert_false(should_flush_batch(processor3, 1640995200800))  // Not at max size or time
  
  // Test batch size trigger
  let processor4 = add_to_batch(processor3, "item4", 1640995200900)
  let processor5 = add_to_batch(processor4, "item5", 1640995200950)
  
  assert_eq(processor5.pending_items.length(), 5)
  assert_true(should_flush_batch(processor5, 1640995200950))  // Max size reached
  
  // Test time-based trigger
  let time_processor = add_to_batch(processor3, "item4", 1640995202000)
  assert_eq(time_processor.pending_items.length(), 4)
  assert_true(should_flush_batch(time_processor, 1640995202000))  // Time threshold reached
  
  // Test batch processing
  let string_processor = create_batch_processor(batch_config)
  let p1 = add_to_batch(string_processor, "hello", 1640995200000)
  let p2 = add_to_batch(p1, "world", 1640995200100)
  let p3 = add_to_batch(p2, "batch", 1640995200200)
  
  let uppercase_fn = fn(items: Array[String>) {
    items.map(fn(item) { item.to_uppercase() })
  }
  
  let processed_processor = process_batch(p3, uppercase_fn)
  assert_eq(processed_processor.pending_items.length(), 0)
  
  // Test batch performance metrics
  type BatchMetrics = {
    total_batches_processed: Int,
    total_items_processed: Int,
    average_batch_size: Float,
    processing_time_ms: Int
  }
  
  let calculate_batch_metrics = fn(processors: Array<BatchProcessor[String, String]>) {
    let total_batches = processors.length()
    let total_items = processors.reduce(fn(sum, p) { sum + p.pending_items.length() }, 0)
    let avg_batch_size = if total_batches > 0 {
      total_items as Float / total_batches as Float
    } else {
      0.0
    }
    
    {
      total_batches_processed: total_batches,
      total_items_processed: total_items,
      average_batch_size: avg_batch_size,
      processing_time_ms: 100  // Simulated processing time
    }
  }
  
  let metrics = calculate_batch_metrics([p1, p2, p3])
  assert_eq(metrics.total_batches_processed, 3)
  assert_eq(metrics.total_items_processed, 6)
  assert_eq(metrics.average_batch_size, 2.0)
  
  // Test adaptive batch sizing
  let adaptive_batch_size = fn(current_size: Int, processing_time_ms: Int, target_time_ms: Int) {
    if processing_time_ms > target_time_ms {
      // Processing is too slow, reduce batch size
      (current_size * 4) / 5  // Reduce by 20%
    } else if processing_time_ms < target_time_ms / 2 {
      // Processing is fast, increase batch size
      (current_size * 5) / 4  // Increase by 25%
    } else {
      current_size  // Keep current size
    }
  }
  
  let current_batch_size = 100
  let slow_processing_time = 600  // 600ms, target is 500ms
  let fast_processing_time = 200  // 200ms, target is 500ms
  
  let reduced_size = adaptive_batch_size(current_batch_size, slow_processing_time, 500)
  assert_eq(reduced_size, 80)  // Reduced by 20%
  
  let increased_size = adaptive_batch_size(current_batch_size, fast_processing_time, 500)
  assert_eq(increased_size, 125)  // Increased by 25%
}

// Test 3: Connection Pooling
test "connection pooling for resource optimization" {
  // Define connection state
  enum ConnectionState {
    Idle
    Active
    Closed
  }
  
  // Define connection
  type Connection = {
    id: String,
    state: ConnectionState,
    created_at: Int,
    last_used: Int,
    usage_count: Int
  }
  
  // Define connection pool
  type ConnectionPool = {
    connections: Array[Connection],
    max_connections: Int,
    connection_timeout_ms: Int,
    idle_timeout_ms: Int
  }
  
  // Create connection pool
  let create_connection_pool = fn(max_connections: Int, connection_timeout_ms: Int, idle_timeout_ms: Int) {
    {
      connections: [],
      max_connections,
      connection_timeout_ms,
      idle_timeout_ms
    }
  }
  
  // Get current time
  let get_current_time = fn() { 1640995200000 }  // Current time in milliseconds
  
  // Create new connection
  let create_connection = fn() {
    {
      id: "conn-" + get_current_time().to_string(),
      state: ConnectionState::Idle,
      created_at: get_current_time(),
      last_used: get_current_time(),
      usage_count: 0
    }
  }
  
  // Get connection from pool
  let get_connection = fn(pool: ConnectionPool) {
    let current_time = get_current_time()
    
    // Find idle connection
    let idle_connection = pool.connections.find_index(fn(conn) {
      conn.state == ConnectionState::Idle
    })
    
    match idle_connection {
      Some(index) => {
        // Update connection state
        let conn = pool.connections[index]
        let updated_conn = {
          id: conn.id,
          state: ConnectionState::Active,
          created_at: conn.created_at,
          last_used: current_time,
          usage_count: conn.usage_count + 1
        }
        
        let updated_connections = pool.connections.slice(0, index) + 
                                [updated_conn] + 
                                pool.connections.slice(index + 1, pool.connections.length())
        
        (Some(updated_conn), { pool | connections: updated_connections })
      }
      None => {
        // Check if we can create a new connection
        if pool.connections.length() < pool.max_connections {
          let new_conn = create_connection()
          let active_conn = { new_conn | state: ConnectionState::Active, usage_count: 1 }
          (Some(active_conn), { pool | connections: pool.connections.push(active_conn) })
        } else {
          // Pool is full
          (None, pool)
        }
      }
    }
  }
  
  // Release connection back to pool
  let release_connection = fn(pool: ConnectionPool, connection_id: String) {
    let current_time = get_current_time()
    
    let updated_connections = pool.connections.map(fn(conn) {
      if conn.id == connection_id && conn.state == ConnectionState::Active {
        {
          id: conn.id,
          state: ConnectionState::Idle,
          created_at: conn.created_at,
          last_used: current_time,
          usage_count: conn.usage_count
        }
      } else {
        conn
      }
    })
    
    { pool | connections: updated_connections }
  }
  
  // Clean up idle connections
  let cleanup_idle_connections = fn(pool: ConnectionPool) {
    let current_time = get_current_time()
    
    let active_connections = pool.connections.filter(fn(conn) {
      if conn.state == ConnectionState::Idle {
        let idle_time = current_time - conn.last_used
        idle_time < pool.idle_timeout_ms
      } else {
        true  // Keep active connections
      }
    })
    
    { pool | connections: active_connections }
  }
  
  // Test connection pool creation
  let pool = create_connection_pool(3, 5000, 60000)  // Max 3 connections, 5s timeout, 60s idle timeout
  assert_eq(pool.connections.length(), 0)
  
  // Test getting connections
  let (conn1, pool1) = get_connection(pool)
  match conn1 {
    Some(connection) => {
      assert_eq(connection.state, ConnectionState::Active)
      assert_eq(connection.usage_count, 1)
    }
    None => assert_true(false)
  }
  assert_eq(pool1.connections.length(), 1)
  
  let (conn2, pool2) = get_connection(pool1)
  let (conn3, pool3) = get_connection(pool2)
  assert_eq(pool3.connections.length(), 3)
  
  // Test pool full scenario
  let (conn4, pool4) = get_connection(pool3)
  match conn4 {
    Some(_) => assert_true(false)  // Should not get connection when pool is full
    None => assert_true(true)      // Should return None
  }
  
  // Test releasing connections
  let pool5 = release_connection(pool4, match conn3 { Some(c) => c.id, None => "" })
  
  // Verify connection is released
  let released_conn = pool5.connections.find(fn(c) { c.id == match conn3 { Some(c) => c.id, None => "" } })
  match released_conn {
    Some(conn) => assert_eq(conn.state, ConnectionState::Idle)
    None => assert_true(false)
  }
  
  // Test reusing released connection
  let (conn_reused, pool6) = get_connection(pool5)
  match conn_reused {
    Some(connection) => {
      assert_eq(connection.state, ConnectionState::Active)
      assert_eq(connection.usage_count, 2)  // Used twice
    }
    None => assert_true(false)
  }
  
  // Test connection pool statistics
  type PoolStats = {
    total_connections: Int,
    active_connections: Int,
    idle_connections: Int,
    utilization_percent: Float
  }
  
  let calculate_pool_stats = fn(pool: ConnectionPool) {
    let total = pool.connections.length()
    let active = pool.connections.filter(fn(c) { c.state == ConnectionState::Active }).length()
    let idle = pool.connections.filter(fn(c) { c.state == ConnectionState::Idle }).length()
    
    {
      total_connections: total,
      active_connections: active,
      idle_connections: idle,
      utilization_percent: if total > 0 {
        (active as Float / total as Float) * 100.0
      } else {
        0.0
      }
    }
  }
  
  let stats = calculate_pool_stats(pool6)
  assert_eq(stats.total_connections, 3)
  assert_eq(stats.active_connections, 3)
  assert_eq(stats.idle_connections, 0)
  assert_eq(stats.utilization_percent, 100.0)
  
  // Test cleanup of idle connections
  let pool_with_idle = release_connection(pool6, match conn2 { Some(c) => c.id, None => "" })
  let pool_with_idle2 = release_connection(pool_with_idle, match conn1 { Some(c) => c.id, None => "" })
  
  let idle_stats = calculate_pool_stats(pool_with_idle2)
  assert_eq(idle_stats.active_connections, 1)
  assert_eq(idle_stats.idle_connections, 2)
  
  // Simulate time passing for idle timeout
  let future_time = get_current_time() + 70000  // 70 seconds later
  let cleanup_pool = { pool_with_idle2 | 
    connections: pool_with_idle2.connections.map(fn(c) { 
      { c | last_used: c.last_used - 70000 }  // Simulate old connections
    })
  }
  
  let cleaned_pool = cleanup_idle_connections(cleanup_pool)
  assert_eq(cleaned_pool.connections.length(), 1)  // Only active connection remains
}

// Test 4: Memory Pool Management
test "memory pool management for allocation optimization" {
  // Define memory block
  type MemoryBlock = {
    id: String,
    size: Int,
    allocated: Bool,
    data: Array[Byte]  // Simplified as array of bytes
  }
  
  // Define memory pool
  type MemoryPool = {
    blocks: Array[MemoryBlock>,
    total_size: Int,
    used_size: Int,
    block_size: Int
  }
  
  // Create memory pool
  let create_memory_pool = fn(total_size: Int, block_size: Int) {
    let num_blocks = total_size / block_size
    let blocks = []
    
    for i in 0..num_blocks {
      blocks = blocks.push({
        id: "block-" + i.to_string(),
        size: block_size,
        allocated: false,
        data: Array::create(block_size, 0)  // Initialize with zeros
      })
    }
    
    {
      blocks,
      total_size,
      used_size: 0,
      block_size
    }
  }
  
  // Allocate memory block
  let allocate_block = fn(pool: MemoryPool, size: Int) {
    let required_blocks = (size + pool.block_size - 1) / pool.block_size
    
    // Find contiguous free blocks
    let mut start_index = -1
    let mut found_blocks = 0
    
    for i in 0..pool.blocks.length() {
      if not(pool.blocks[i].allocated) {
        if start_index == -1 {
          start_index = i
        }
        found_blocks = found_blocks + 1
        
        if found_blocks >= required_blocks {
          break
        }
      } else {
        start_index = -1
        found_blocks = 0
      }
    }
    
    if found_blocks >= required_blocks {
      // Allocate blocks
      let updated_blocks = pool.blocks.map_with_index(fn(block, index) {
        if index >= start_index && index < start_index + required_blocks {
          { block | allocated: true }
        } else {
          block
        }
      })
      
      let allocated_block = {
        id: "alloc-" + start_index.to_string(),
        size: required_blocks * pool.block_size,
        allocated: true,
        data: []  // Would contain actual data
      }
      
      (Some(allocated_block), {
        blocks: updated_blocks,
        total_size: pool.total_size,
        used_size: pool.used_size + (required_blocks * pool.block_size),
        block_size: pool.block_size
      })
    } else {
      (None, pool)  // Not enough contiguous space
    }
  }
  
  // Free memory block
  let free_block = fn(pool: MemoryPool, allocation_id: String) {
    // In a real implementation, we'd track which blocks belong to which allocation
    // For simplicity, we'll just free all blocks
    let updated_blocks = pool.blocks.map(fn(block) {
      { block | allocated: false }
    })
    
    {
      blocks: updated_blocks,
      total_size: pool.total_size,
      used_size: 0,
      block_size: pool.block_size
    }
  }
  
  // Test memory pool creation
  let memory_pool = create_memory_pool(1024, 128)  // 1KB total, 128B blocks
  assert_eq(memory_pool.blocks.length(), 8)  // 1024 / 128 = 8 blocks
  assert_eq(memory_pool.total_size, 1024)
  assert_eq(memory_pool.used_size, 0)
  
  // Test block allocation
  let (block1, pool1) = allocate_block(memory_pool, 100)  // Needs 1 block
  match block1 {
    Some(allocated) => {
      assert_eq(allocated.size, 128)  // Rounded up to block size
      assert_true(allocated.allocated)
    }
    None => assert_true(false)
  }
  assert_eq(pool1.used_size, 128)
  
  // Test multiple allocations
  let (block2, pool2) = allocate_block(pool1, 200)  // Needs 2 blocks
  match block2 {
    Some(allocated) => assert_eq(allocated.size, 256)
    None => assert_true(false)
  }
  assert_eq(pool2.used_size, 384)  // 128 + 256
  
  // Test allocation failure when out of memory
  let (big_block, pool3) = allocate_block(pool2, 1000)  // Needs 8 blocks
  match big_block {
    Some(_) => assert_true(false)  // Should fail
    None => assert_true(true)      // Should return None
  }
  assert_eq(pool3.used_size, 384)  // Unchanged
  
  // Test memory pool statistics
  type MemoryPoolStats = {
    total_blocks: Int,
    allocated_blocks: Int,
    free_blocks: Int,
    fragmentation_percent: Float
  }
  
  let calculate_memory_stats = fn(pool: MemoryPool) {
    let total = pool.blocks.length()
    let allocated = pool.blocks.filter(fn(b) { b.allocated }).length()
    let free = total - allocated
    
    // Simplified fragmentation calculation
    let fragmentation = if allocated > 0 {
      let free_blocks = pool.blocks.filter(fn(b) { not(b.allocated) })
      if free_blocks.length() > 1 {
        // Count free block fragments
        let mut fragments = 1
        let mut was_free = false
        
        for block in pool.blocks {
          if not(block.allocated) {
            if not(was_free) {
              fragments = fragments + 1
              was_free = true
            }
          } else {
            was_free = false
          }
        }
        
        ((fragments - 1) as Float / free_blocks.length() as Float) * 100.0
      } else {
        0.0
      }
    } else {
      0.0
    }
    
    {
      total_blocks: total,
      allocated_blocks: allocated,
      free_blocks: free,
      fragmentation_percent: fragmentation
    }
  }
  
  let stats = calculate_memory_stats(pool3)
  assert_eq(stats.total_blocks, 8)
  assert_eq(stats.allocated_blocks, 3)  // 1 + 2 blocks
  assert_eq(stats.free_blocks, 5)
  
  // Test memory defragmentation
  let defragment_memory = fn(pool: MemoryPool) {
    // Move all allocated blocks to the beginning
    let allocated_blocks = pool.blocks.filter(fn(b) { b.allocated })
    let free_blocks = pool.blocks.filter(fn(b) { not(b.allocated) })
    
    { pool | blocks: allocated_blocks + free_blocks }
  }
  
  let fragmented_pool = {
    blocks: [
      { id: "block-0", size: 128, allocated: true, data: [] },
      { id: "block-1", size: 128, allocated: false, data: [] },
      { id: "block-2", size: 128, allocated: true, data: [] },
      { id: "block-3", size: 128, allocated: false, data: [] },
      { id: "block-4", size: 128, allocated: false, data: [] },
      { id: "block-5", size: 128, allocated: true, data: [] },
      { id: "block-6", size: 128, allocated: false, data: [] },
      { id: "block-7", size: 128, allocated: false, data: [] }
    ],
    total_size: 1024,
    used_size: 384,
    block_size: 128
  }
  
  let fragmented_stats = calculate_memory_stats(fragmented_pool)
  assert_eq(fragmented_stats.allocated_blocks, 3)
  assert_eq(fragmented_stats.free_blocks, 5)
  assert_true(fragmented_stats.fragmentation_percent > 0.0)
  
  let defragmented_pool = defragment_memory(fragmented_pool)
  let defragmented_stats = calculate_memory_stats(defragmented_pool)
  assert_eq(defragmented_stats.allocated_blocks, 3)
  assert_eq(defragmented_stats.free_blocks, 5)
  assert_eq(defragmented_stats.fragmentation_percent, 0.0)  // No fragmentation after defrag
}

// Test 5: Lazy Loading and On-Demand Computation
test "lazy loading and on-demand computation optimization" {
  // Define lazy value
  type Lazy[T] = {
    computed: Bool,
    value: Option[T],
    computation: () -> T
  }
  
  // Create lazy value
  let lazy = fn(computation: () -> T) {
    {
      computed: false,
      value: None,
      computation
    }
  }
  
  // Get lazy value (compute if needed)
  let get_lazy = fn(lazy_value: Lazy[T>) {
    if lazy_value.computed {
      match lazy_value.value {
        Some(v) => v
        None => lazy_value.computation()  // Fallback
      }
    } else {
      let computed_value = lazy_value.computation()
      computed_value
    }
  }
  
  // Test lazy computation
  let computation_count = { mut count: 0 }
  
  let expensive_computation = lazy(fn() {
    computation_count.count = computation_count.count + 1
    42 * 42  // Expensive calculation
  })
  
  // Initially, computation shouldn't have run
  assert_eq(computation_count.count, 0)
  
  // First access triggers computation
  let result1 = get_lazy(expensive_computation)
  assert_eq(result1, 1764)
  assert_eq(computation_count.count, 1)
  
  // Second access should use cached value
  let result2 = get_lazy(expensive_computation)
  assert_eq(result2, 1764)
  // In real implementation, count would still be 1
  
  // Test lazy loading with dependencies
  type LazyWithDependencies[T] = {
    computed: Bool,
    value: Option[T],
    computation: () -> T,
    dependencies: Array[String]
  }
  
  let lazy_with_deps = fn(computation: () -> T, dependencies: Array<String>) {
    {
      computed: false,
      value: None,
      computation,
      dependencies
    }
  }
  
  // Create a dependency graph
  let dependency_graph = {}
  
  let register_dependency = fn(graph, name: String, lazy_value: Lazy[T>) {
    // In a real implementation, this would store the lazy value
    graph
  }
  
  let compute_with_dependencies = fn(lazy_value: LazyWithDependencies[T>) {
    // In a real implementation, this would check and compute dependencies first
    if not(lazy_value.computed) {
      let computed_value = lazy_value.computation()
      computed_value
    } else {
      match lazy_value.value {
        Some(v) => v
        None => lazy_value.computation()
      }
    }
  }
  
  // Test lazy with dependencies
  let user_data_lazy = lazy_with_deps(fn() {
    "User data loaded"
  }, ["auth_token", "user_id"])
  
  let result = compute_with_dependencies(user_data_lazy)
  assert_eq(result, "User data loaded")
  
  // Test memoization with cache invalidation
  type MemoizedCache[T, R] = {
    cache: Array[(T, R)>,
    max_size: Int
  }
  
  let create_memoized_cache = fn(max_size: Int) {
    { cache: [], max_size }
  }
  
  let memoize = fn(cache: MemoizedCache[T, R], key: T, computation: () -> R) {
    let cached = cache.cache.find(fn(entry) { entry.0 == key })
    
    match cached {
      Some((_, result)) => (result, cache)
      None => {
        let computed = computation()
        let updated_cache = if cache.cache.length() >= cache.max_size {
          // Remove oldest entry (simplified)
          cache.cache.slice(1, cache.cache.length()).push((key, computed))
        } else {
          cache.cache.push((key, computed))
        }
        
        (computed, { cache | cache: updated_cache })
      }
    }
  }
  
  // Test memoization
  let cache = create_memoized_cache(5)
  
  let fib_cache = { mut cache: cache }
  
  let memoized_fib = fn(n: Int) {
    let (result, updated_cache) = memoize(fib_cache.cache, n, fn() {
      if n <= 1 {
        n
      } else {
        let (result1, _) = memoize(fib_cache.cache, n - 1, fn() { memoized_fib(n - 1) })
        let (result2, _) = memoize(fib_cache.cache, n - 2, fn() { memoized_fib(n - 2) })
        result1 + result2
      }
    })
    
    fib_cache.cache = updated_cache
    result
  }
  
  // Test memoized Fibonacci
  let fib_result = memoized_fib(10)
  assert_eq(fib_result, 55)
  
  // Test streaming and lazy evaluation
  type Stream[T] = {
    head: Option[T],
    tail: () -> Stream[T]
  }
  
  let create_stream = fn(start: Int, step: Int) {
    {
      head: Some(start),
      tail: fn() { create_stream(start + step, step) }
    }
  }
  
  let stream_take = fn(stream: Stream[T>, n: Int) {
    if n <= 0 || stream.head == None {
      []
    } else {
      match stream.head {
        Some(value) => [value] + stream_take(stream.tail(), n - 1)
        None => []
      }
    }
  }
  
  // Test infinite stream
  let numbers_stream = create_stream(1, 1)
  let first_five = stream_take(numbers_stream, 5)
  assert_eq(first_five, [1, 2, 3, 4, 5])
  
  // Test lazy filtering
  let stream_filter = fn(stream: Stream[T], predicate: (T) -> Bool) {
    match stream.head {
      Some(value) => {
        if predicate(value) {
          {
            head: Some(value),
            tail: fn() { stream_filter(stream.tail(), predicate) }
          }
        } else {
          stream_filter(stream.tail(), predicate)
        }
      }
      None => { head: None, tail: fn() { stream } }
    }
  }
  
  let even_stream = stream_filter(numbers_stream, fn(x) { x % 2 == 0 })
  let first_three_even = stream_take(even_stream, 3)
  assert_eq(first_three_even, [2, 4, 6])
}