// Azimuth Performance Optimization Comprehensive Test Suite
// This file contains comprehensive test cases for performance optimization functionality

// Test 1: Caching Mechanisms
test "caching mechanisms for telemetry data" {
  // Define cache types
  type CacheEntry[V] = {
    value: V,
    created_at: Int,
    access_count: Int,
    last_accessed: Int,
    ttl: Int // Time to live in seconds
  }
  
  type Cache[K, V] = {
    entries: Array[(K, CacheEntry[V])>,
    max_size: Int,
    eviction_policy: String
  }
  
  // Cache operations
  let create_cache = fn(max_size: Int, eviction_policy: String) {
    {
      entries: [],
      max_size,
      eviction_policy
    }
  }
  
  let get = fn(cache: Cache[K, V], key: K, current_time: Int) {
    let mut result = None
    let mut updated_entries = cache.entries
    
    for i in 0..cache.entries.length() {
      let (k, entry) = cache.entries[i]
      
      if k == key {
        // Check if entry is expired
        if current_time - entry.created_at <= entry.ttl {
          result = Some(entry.value)
          
          // Update access count and last accessed time
          let updated_entry = {
            value: entry.value,
            created_at: entry.created_at,
            access_count: entry.access_count + 1,
            last_accessed: current_time,
            ttl: entry.ttl
          }
          
          updated_entries = updated_entries.set(i, (k, updated_entry))
        } else {
          // Remove expired entry
          updated_entries = updated_entries.slice(0, i) + updated_entries.slice(i + 1, updated_entries.length())
        }
        break
      }
    }
    
    (result, { cache | entries: updated_entries })
  }
  
  let put = fn(cache: Cache[K, V], key: K, value: V, current_time: Int, ttl: Int) {
    let mut updated_entries = cache.entries
    let mut found = false
    
    // Update existing entry
    for i in 0..cache.entries.length() {
      let (k, entry) = cache.entries[i]
      if k == key {
        updated_entries = updated_entries.set(i, (k, {
          value,
          created_at: current_time,
          access_count: 1,
          last_accessed: current_time,
          ttl
        }))
        found = true
        break
      }
    }
    
    // Add new entry
    if not(found) {
      // Check if cache is full
      if cache.entries.length() >= cache.max_size {
        // Evict entries based on policy
        updated_entries = evict_entries(cache)
      }
      
      updated_entries = updated_entries.push((key, {
        value,
        created_at: current_time,
        access_count: 1,
        last_accessed: current_time,
        ttl
      }))
    }
    
    { cache | entries: updated_entries }
  }
  
  let evict_entries = fn(cache: Cache[K, V]) {
    match cache.eviction_policy {
      "lru" => {
        // Least Recently Used
        let mut oldest_time = 9999999999
        let mut oldest_index = -1
        
        for i in 0..cache.entries.length() {
          let (_, entry) = cache.entries[i]
          if entry.last_accessed < oldest_time {
            oldest_time = entry.last_accessed
            oldest_index = i
          }
        }
        
        if oldest_index >= 0 {
          cache.entries.slice(0, oldest_index) + cache.entries.slice(oldest_index + 1, cache.entries.length())
        } else {
          cache.entries
        }
      }
      "lfu" => {
        // Least Frequently Used
        let mut min_access = 999999
        let mut min_index = -1
        
        for i in 0..cache.entries.length() {
          let (_, entry) = cache.entries[i]
          if entry.access_count < min_access {
            min_access = entry.access_count
            min_index = i
          }
        }
        
        if min_index >= 0 {
          cache.entries.slice(0, min_index) + cache.entries.slice(min_index + 1, cache.entries.length())
        } else {
          cache.entries
        }
      }
      "fifo" => {
        // First In First Out
        if cache.entries.length() > 0 {
          cache.entries.slice(1, cache.entries.length())
        } else {
          cache.entries
        }
      }
      _ => cache.entries // Unknown policy
    }
  }
  
  // Test cache creation
  let cache = create_cache(3, "lru")
  assert_eq(cache.max_size, 3)
  assert_eq(cache.eviction_policy, "lru")
  assert_eq(cache.entries.length(), 0)
  
  // Test putting and getting values
  let current_time = 1640995200
  let cache1 = put(cache, "key1", "value1", current_time, 300) // 5 minutes TTL
  let cache2 = put(cache1, "key2", "value2", current_time + 1, 300)
  let cache3 = put(cache2, "key3", "value3", current_time + 2, 300)
  
  assert_eq(cache3.entries.length(), 3)
  
  // Test getting values
  let (value1, cache4) = get(cache3, "key1", current_time + 10)
  assert_eq(value1, Some("value1"))
  
  // Check that access count and last accessed time were updated
  let (entry1, _) = cache4.entries.find(fn(pair) { pair.0 == "key1" }).unwrap()
  assert_eq(entry1.access_count, 2)
  assert_eq(entry1.last_accessed, current_time + 10)
  
  // Test cache eviction
  let cache5 = put(cache4, "key4", "value4", current_time + 20, 300)
  assert_eq(cache5.entries.length(), 3) // Should still be 3 due to eviction
  
  // Check that least recently used entry was evicted
  let (value2, _) = get(cache5, "key2", current_time + 30)
  assert_eq(value2, Some("value2"))
  
  let (value3, _) = get(cache5, "key3", current_time + 30)
  assert_eq(value3, Some("value3"))
  
  let (value4, _) = get(cache5, "key4", current_time + 30)
  assert_eq(value4, Some("value4"))
  
  // key1 should be evicted (it was accessed at current_time + 10, while key2 and key3 were never accessed)
  let (value1_evicted, _) = get(cache5, "key1", current_time + 30)
  assert_eq(value1_evicted, None)
  
  // Test TTL expiration
  let cache6 = put(cache5, "key5", "value5", current_time, 1) // 1 second TTL
  let (value5_expired, _) = get(cache6, "key5", current_time + 5) // After 5 seconds
  assert_eq(value5_expired, None)
}

// Test 2: Connection Pooling
test "connection pooling for telemetry services" {
  // Define connection pool types
  type Connection = {
    id: String,
    created_at: Int,
    last_used: Int,
    in_use: Bool,
    use_count: Int,
    max_lifetime: Int // in seconds
  }
  
  type ConnectionPool = {
    connections: Array[Connection],
    max_connections: Int,
    min_connections: Int,
    connection_timeout: Int // in seconds
  }
  
  // Connection pool operations
  let create_connection = fn(id: String, current_time: Int, max_lifetime: Int) {
    {
      id,
      created_at: current_time,
      last_used: current_time,
      in_use: false,
      use_count: 0,
      max_lifetime
    }
  }
  
  let create_pool = fn(min_connections: Int, max_connections: Int, connection_timeout: Int, max_lifetime: Int, current_time: Int) {
    let mut connections = []
    for i in 0..min_connections {
      connections = connections.push(create_connection("conn-" + i.to_string(), current_time, max_lifetime))
    }
    
    {
      connections,
      max_connections,
      min_connections,
      connection_timeout
    }
  }
  
  let get_connection = fn(pool: ConnectionPool, current_time: Int) {
    let mut available_connection = None
    let mut updated_connections = pool.connections
    
    // Find an available connection
    for i in 0..pool.connections.length() {
      let conn = pool.connections[i]
      
      if not(conn.in_use) and (current_time - conn.last_used <= pool.connection_timeout) {
        // Check if connection has expired
        if current_time - conn.created_at <= conn.max_lifetime {
          available_connection = Some(conn)
          
          // Mark as in use and update last used time
          updated_connections = updated_connections.set(i, {
            id: conn.id,
            created_at: conn.created_at,
            last_used: current_time,
            in_use: true,
            use_count: conn.use_count + 1,
            max_lifetime: conn.max_lifetime
          })
          break
        } else {
          // Remove expired connection
          updated_connections = updated_connections.slice(0, i) + updated_connections.slice(i + 1, updated_connections.length())
        }
      }
    }
    
    match available_connection {
      Some(conn) => (Some(conn), { pool | connections: updated_connections })
      None => {
        // Try to create a new connection if under max limit
        if updated_connections.length() < pool.max_connections {
          let new_conn = create_connection("conn-new-" + current_time.to_string(), current_time, 3600)
          updated_connections = updated_connections.push({
            id: new_conn.id,
            created_at: new_conn.created_at,
            last_used: new_conn.last_used,
            in_use: true,
            use_count: new_conn.use_count + 1,
            max_lifetime: new_conn.max_lifetime
          })
          
          (Some(new_conn), { pool | connections: updated_connections })
        } else {
          (None, { pool | connections: updated_connections })
        }
      }
    }
  }
  
  let release_connection = fn(pool: ConnectionPool, connection_id: String, current_time: Int) {
    let mut updated_connections = pool.connections
    
    for i in 0..pool.connections.length() {
      let conn = pool.connections[i]
      if conn.id == connection_id {
        updated_connections = updated_connections.set(i, {
          id: conn.id,
          created_at: conn.created_at,
          last_used: current_time,
          in_use: false,
          use_count: conn.use_count,
          max_lifetime: conn.max_lifetime
        })
        break
      }
    }
    
    { pool | connections: updated_connections }
  }
  
  let cleanup_expired_connections = fn(pool: ConnectionPool, current_time: Int) {
    let mut valid_connections = []
    
    for conn in pool.connections {
      if not(conn.in_use) and (current_time - conn.created_at <= conn.max_lifetime) {
        valid_connections = valid_connections.push(conn)
      }
    }
    
    { pool | connections: valid_connections }
  }
  
  // Test pool creation
  let current_time = 1640995200
  let pool = create_pool(2, 5, 300, 3600, current_time)
  assert_eq(pool.min_connections, 2)
  assert_eq(pool.max_connections, 5)
  assert_eq(pool.connections.length(), 2)
  
  for conn in pool.connections {
    assert_false(conn.in_use)
    assert_eq(conn.use_count, 0)
  }
  
  // Test getting connections
  let (conn1, pool1) = get_connection(pool, current_time + 10)
  assert_true(conn1.is_some())
  
  let conn1_unwrapped = conn1.unwrap()
  assert_true(conn1_unwrapped.in_use)
  assert_eq(conn1_unwrapped.use_count, 1)
  
  // Test that the same connection is not returned again
  let (conn2, pool2) = get_connection(pool1, current_time + 20)
  assert_true(conn2.is_some())
  assert_not_equal(conn1_unwrapped.id, conn2.unwrap().id)
  
  // Test releasing connection
  let pool3 = release_connection(pool2, conn1_unwrapped.id, current_time + 30)
  
  let conn1_released = pool3.connections.find(fn(conn) { conn.id == conn1_unwrapped.id }).unwrap()
  assert_false(conn1_released.in_use)
  assert_eq(conn1_released.last_used, current_time + 30)
  
  // Test that released connection can be reused
  let (conn1_reused, pool4) = get_connection(pool3, current_time + 40)
  assert_true(conn1_reused.is_some())
  assert_eq(conn1_reused.unwrap().id, conn1_unwrapped.id)
  assert_eq(conn1_reused.unwrap().use_count, 2)
  
  // Test connection creation when pool is full
  let mut pool_full = pool4
  let mut connection_ids = []
  
  // Get all connections
  for i in 0..4 {
    let (conn, new_pool) = get_connection(pool_full, current_time + 50 + i * 10)
    assert_true(conn.is_some())
    connection_ids = connection_ids.push(conn.unwrap().id)
    pool_full = new_pool
  }
  
  // Pool should now be full
  assert_eq(pool_full.connections.length(), 5)
  
  // Try to get another connection
  let (conn_extra, _) = get_connection(pool_full, current_time + 100)
  assert_eq(conn_extra, None)
  
  // Test cleanup of expired connections
  let expired_time = current_time + 4000 // Beyond max_lifetime
  let pool_cleaned = cleanup_expired_connections(pool_full, expired_time)
  
  // In-use connections should remain, expired unused connections should be removed
  let in_use_count = pool_cleaned.connections.fold(0, fn(acc, conn) {
    if conn.in_use { acc + 1 } else { acc }
  })
  
  assert_eq(in_use_count, 5) // All connections are in use
}

// Test 3: Batch Processing Optimization
test "batch processing optimization for telemetry data" {
  // Define batch processing types
  type BatchConfig = {
    max_batch_size: Int,
    max_wait_time_ms: Int,
    max_batch_memory_mb: Int
  }
  
  type TelemetryBatch = {
    items: Array[String],
    created_at: Int,
    size_bytes: Int
  }
  
  type BatchProcessor = {
    config: BatchConfig,
    current_batch: TelemetryBatch,
    processed_batches: Int,
    total_items_processed: Int
  }
  
  // Batch processing operations
  let create_batch_config = fn(max_batch_size: Int, max_wait_time_ms: Int, max_batch_memory_mb: Int) {
    {
      max_batch_size,
      max_wait_time_ms,
      max_batch_memory_mb
    }
  }
  
  let create_batch = fn() {
    {
      items: [],
      created_at: 1640995200,
      size_bytes: 0
    }
  }
  
  let create_processor = fn(config: BatchConfig) {
    {
      config,
      current_batch: create_batch(),
      processed_batches: 0,
      total_items_processed: 0
    }
  }
  
  let estimate_item_size = fn(item: String) {
    // Rough estimation: 1 byte per character + overhead
    item.length() + 20
  }
  
  let should_flush_batch = fn(processor: BatchProcessor, current_time: Int) {
    let batch = processor.current_batch
    let size_exceeded = batch.size_bytes >= (processor.config.max_batch_memory_mb * 1024 * 1024)
    let count_exceeded = batch.items.length() >= processor.config.max_batch_size
    let time_exceeded = (current_time - batch.created_at) >= processor.config.max_wait_time_ms
    
    size_exceeded or count_exceeded or time_exceeded
  }
  
  let add_item = fn(processor: BatchProcessor, item: String, current_time: Int) {
    let item_size = estimate_item_size(item)
    let new_size_bytes = processor.current_batch.size_bytes + item_size
    
    // Check if adding this item would exceed limits
    let would_exceed_memory = new_size_bytes >= (processor.config.max_batch_memory_mb * 1024 * 1024)
    let would_exceed_count = processor.current_batch.items.length() + 1 >= processor.config.max_batch_size
    
    if would_exceed_memory or would_exceed_count or should_flush_batch(processor, current_time) {
      // Flush current batch and start a new one
      let flushed_processor = flush_batch(processor, current_time)
      
      // Add item to new batch
      let new_batch = {
        items: [item],
        created_at: current_time,
        size_bytes: item_size
      }
      
      {
        config: flushed_processor.config,
        current_batch: new_batch,
        processed_batches: flushed_processor.processed_batches,
        total_items_processed: flushed_processor.total_items_processed
      }
    } else {
      // Add item to current batch
      let new_batch = {
        items: processor.current_batch.items.push(item),
        created_at: processor.current_batch.created_at,
        size_bytes: new_size_bytes
      }
      
      {
        config: processor.config,
        current_batch: new_batch,
        processed_batches: processor.processed_batches,
        total_items_processed: processor.total_items_processed
      }
    }
  }
  
  let flush_batch = fn(processor: BatchProcessor, current_time: Int) {
    if processor.current_batch.items.length() > 0 {
      {
        config: processor.config,
        current_batch: create_batch(),
        processed_batches: processor.processed_batches + 1,
        total_items_processed: processor.total_items_processed + processor.current_batch.items.length()
      }
    } else {
      processor
    }
  }
  
  // Test processor creation
  let config = create_batch_config(100, 5000, 10) // 100 items, 5 seconds, 10MB
  let processor = create_processor(config)
  assert_eq(processor.config.max_batch_size, 100)
  assert_eq(processor.config.max_wait_time_ms, 5000)
  assert_eq(processor.config.max_batch_memory_mb, 10)
  assert_eq(processor.current_batch.items.length(), 0)
  assert_eq(processor.processed_batches, 0)
  assert_eq(processor.total_items_processed, 0)
  
  // Test adding items
  let current_time = 1640995200
  
  // Add items that fit in the batch
  let processor1 = add_item(processor, "item1", current_time)
  let processor2 = add_item(processor1, "item2", current_time)
  let processor3 = add_item(processor2, "item3", current_time)
  
  assert_eq(processor3.current_batch.items.length(), 3)
  assert_eq(processor3.processed_batches, 0)
  
  // Test batch size limit
  let small_batch_config = create_batch_config(3, 5000, 10) // 3 items max
  let small_processor = create_processor(small_batch_config)
  
  let small_processor1 = add_item(small_processor, "item1", current_time)
  let small_processor2 = add_item(small_processor1, "item2", current_time)
  let small_processor3 = add_item(small_processor2, "item3", current_time)
  
  assert_eq(small_processor3.current_batch.items.length(), 3)
  assert_eq(small_processor3.processed_batches, 0)
  
  // Adding a 4th item should flush the batch
  let small_processor4 = add_item(small_processor3, "item4", current_time)
  assert_eq(small_processor4.current_batch.items.length(), 1) // New batch with item4
  assert_eq(small_processor4.processed_batches, 1) // Previous batch was flushed
  assert_eq(small_processor4.total_items_processed, 3)
  
  // Test time-based flushing
  let time_config = create_batch_config(100, 100, 10) // 100ms timeout
  let time_processor = create_processor(time_config)
  
  let time_processor1 = add_item(time_processor, "item1", current_time)
  
  // Simulate time passing
  let time_processor2 = add_item(time_processor1, "item2", current_time + 150) // After timeout
  
  assert_eq(time_processor2.current_batch.items.length(), 1) // New batch with item2
  assert_eq(time_processor2.processed_batches, 1) // Previous batch was flushed due to time
  
  // Test manual flush
  let manual_processor = create_processor(config)
  let manual_processor1 = add_item(manual_processor, "item1", current_time)
  let manual_processor2 = add_item(manual_processor1, "item2", current_time)
  
  assert_eq(manual_processor2.current_batch.items.length(), 2)
  assert_eq(manual_processor2.processed_batches, 0)
  
  let manual_processor3 = flush_batch(manual_processor2, current_time + 10)
  assert_eq(manual_processor3.current_batch.items.length(), 0)
  assert_eq(manual_processor3.processed_batches, 1)
  assert_eq(manual_processor3.total_items_processed, 2)
}

// Test 4: Memory Pool Management
test "memory pool management for telemetry objects" {
  // Define memory pool types
  type MemoryBlock = {
    id: String,
    size: Int,
    in_use: Bool,
    allocated_at: Int,
    last_used: Int,
    data: Option[String>
  }
  
  type MemoryPool = {
    blocks: Array<MemoryBlock>,
    total_allocated: Int,
    total_used: Int,
    allocation_strategy: String
  }
  
  // Memory pool operations
  let create_memory_block = fn(id: String, size: Int, current_time: Int) {
    {
      id,
      size,
      in_use: false,
      allocated_at: current_time,
      last_used: current_time,
      data: None
    }
  }
  
  let create_memory_pool = fn(initial_blocks: Int, block_size: Int, allocation_strategy: String, current_time: Int) {
    let mut blocks = []
    for i in 0..initial_blocks {
      blocks = blocks.push(create_memory_block("block-" + i.to_string(), block_size, current_time))
    }
    
    {
      blocks,
      total_allocated: initial_blocks * block_size,
      total_used: 0,
      allocation_strategy
    }
  }
  
  let allocate_block = fn(pool: MemoryPool, data: String, current_time: Int) {
    let data_size = data.length()
    let mut allocated_block = None
    let mut updated_blocks = pool.blocks
    
    // Find a free block that can accommodate the data
    for i in 0..pool.blocks.length() {
      let block = pool.blocks[i]
      
      if not(block.in_use) and block.size >= data_size {
        allocated_block = Some({
          id: block.id,
          size: block.size,
          in_use: true,
          allocated_at: block.allocated_at,
          last_used: current_time,
          data: Some(data)
        })
        
        updated_blocks = updated_blocks.set(i, allocated_block.unwrap())
        break
      }
    }
    
    match allocated_block {
      Some(block) => {
        (Some(block), {
          blocks: updated_blocks,
          total_allocated: pool.total_allocated,
          total_used: pool.total_used + data_size,
          allocation_strategy: pool.allocation_strategy
        })
      }
      None => {
        // Try to create a new block
        let new_block_size = if data_size > 1024 { data_size * 2 } else { 2048 }
        let new_block = create_memory_block("block-new-" + current_time.to_string(), new_block_size, current_time)
        
        let allocated_new_block = {
          id: new_block.id,
          size: new_block.size,
          in_use: true,
          allocated_at: new_block.allocated_at,
          last_used: current_time,
          data: Some(data)
        }
        
        (Some(allocated_new_block), {
          blocks: updated_blocks.push(allocated_new_block),
          total_allocated: pool.total_allocated + new_block_size,
          total_used: pool.total_used + data_size,
          allocation_strategy: pool.allocation_strategy
        })
      }
    }
  }
  
  let deallocate_block = fn(pool: MemoryPool, block_id: String, current_time: Int) {
    let mut updated_blocks = pool.blocks
    let mut freed_size = 0
    
    for i in 0..pool.blocks.length() {
      let block = pool.blocks[i]
      
      if block.id == block_id and block.in_use {
        let data_size = match block.data {
          Some(data) => data.length()
          None => 0
        }
        
        freed_size = data_size
        
        updated_blocks = updated_blocks.set(i, {
          id: block.id,
          size: block.size,
          in_use: false,
          allocated_at: block.allocated_at,
          last_used: current_time,
          data: None
        })
        break
      }
    }
    
    {
      blocks: updated_blocks,
      total_allocated: pool.total_allocated,
      total_used: pool.total_used - freed_size,
      allocation_strategy: pool.allocation_strategy
    }
  }
  
  let compact_pool = fn(pool: MemoryPool, current_time: Int) {
    let mut compacted_blocks = []
    let mut in_use_blocks = []
    
    // Separate in-use and free blocks
    for block in pool.blocks {
      if block.in_use {
        in_use_blocks = in_use_blocks.push(block)
      } else {
        compacted_blocks = compacted_blocks.push(block)
      }
    }
    
    // Sort free blocks by size (largest first) for better reuse
    for i in 0..compacted_blocks.length() {
      for j in i+1..compacted_blocks.length() {
        if compacted_blocks[j].size > compacted_blocks[i].size {
          let temp = compacted_blocks[i]
          compacted_blocks = compacted_blocks.set(i, compacted_blocks[j])
          compacted_blocks = compacted_blocks.set(j, temp)
        }
      }
    }
    
    {
      blocks: in_use_blocks + compacted_blocks,
      total_allocated: pool.total_allocated,
      total_used: pool.total_used,
      allocation_strategy: pool.allocation_strategy
    }
  }
  
  // Test memory pool creation
  let current_time = 1640995200
  let pool = create_memory_pool(3, 1024, "first-fit", current_time)
  assert_eq(pool.blocks.length(), 3)
  assert_eq(pool.total_allocated, 3072) // 3 * 1024
  assert_eq(pool.total_used, 0)
  
  for block in pool.blocks {
    assert_false(block.in_use)
    assert_eq(block.size, 1024)
  }
  
  // Test block allocation
  let (block1, pool1) = allocate_block(pool, "data1", current_time)
  assert_true(block1.is_some())
  assert_eq(pool1.total_used, 5) // "data1" length
  
  let block1_unwrapped = block1.unwrap()
  assert_true(block1_unwrapped.in_use)
  assert_eq(block1_unwrapped.data, Some("data1"))
  
  // Test allocation of multiple blocks
  let (block2, pool2) = allocate_block(pool1, "data2", current_time)
  let (block3, pool3) = allocate_block(pool2, "data3", current_time)
  
  assert_true(block2.is_some())
  assert_true(block3.is_some())
  
  // All blocks should be in use now
  let in_use_count = pool3.blocks.fold(0, fn(acc, block) {
    if block.in_use { acc + 1 } else { acc }
  })
  assert_eq(in_use_count, 3)
  
  // Test allocation when all blocks are in use
  let (block4, pool4) = allocate_block(pool3, "data4", current_time)
  assert_true(block4.is_some())
  assert_eq(pool4.blocks.length(), 4) // New block should be created
  
  // Test block deallocation
  let pool5 = deallocate_block(pool4, block1_unwrapped.id, current_time + 10)
  
  let block1_deallocated = pool5.blocks.find(fn(block) { block.id == block1_unwrapped.id }).unwrap()
  assert_false(block1_deallocated.in_use)
  assert_eq(block1_deallocated.data, None)
  assert_eq(block1_deallocated.last_used, current_time + 10)
  
  // Test that deallocated block can be reused
  let (block_reused, pool6) = allocate_block(pool5, "new_data", current_time + 20)
  assert_true(block_reused.is_some())
  assert_eq(block_reused.unwrap().id, block1_unwrapped.id) // Should reuse the deallocated block
  
  // Test pool compaction
  let pool7 = deallocate_block(pool6, block2.unwrap().id, current_time + 30)
  let pool8 = compact_pool(pool7, current_time + 40)
  
  // Free blocks should be sorted by size (largest first)
  let free_blocks = pool8.blocks.filter(fn(block) { not(block.in_use) })
  assert_true(free_blocks.length() >= 1)
  
  // In-use blocks should come first
  let first_in_use_index = pool8.blocks.index(fn(block) { block.in_use }).unwrap()
  let first_free_index = pool8.blocks.index(fn(block) { not(block.in_use) }).unwrap()
  assert_true(first_in_use_index < first_free_index)
}

// Test 5: Lazy Loading
test "lazy loading for telemetry resources" {
  // Define lazy loading types
  type LazyResource[T] = {
    id: String,
    loaded: Bool,
    value: Option[T],
    loader: () -> T,
    load_time: Option[Int>,
    access_count: Int
  }
  
  type ResourceCache = {
    resources: Array<LazyResource[String>>,
    hit_count: Int,
    miss_count: Int
  }
  
  // Lazy loading operations
  let create_lazy_resource = fn(id: String, loader: () -> String) {
    {
      id,
      loaded: false,
      value: None,
      loader,
      load_time: None,
      access_count: 0
    }
  }
  
  let create_resource_cache = fn() {
    {
      resources: [],
      hit_count: 0,
      miss_count: 0
    }
  }
  
  let add_resource = fn(cache: ResourceCache, resource: LazyResource[String]) {
    { cache | resources: cache.resources.push(resource) }
  }
  
  let get_resource = fn(cache: ResourceCache, resource_id: String, current_time: Int) {
    let mut result = None
    let mut updated_resources = cache.resources
    let mut hit = false
    
    for i in 0..cache.resources.length() {
      let resource = cache.resources[i]
      
      if resource.id == resource_id {
        if resource.loaded {
          // Resource already loaded
          result = resource.value
          hit = true
          
          // Update access count
          updated_resources = updated_resources.set(i, {
            id: resource.id,
            loaded: resource.loaded,
            value: resource.value,
            loader: resource.loader,
            load_time: resource.load_time,
            access_count: resource.access_count + 1
          })
        } else {
          // Load the resource
          let value = resource.loader()
          
          result = Some(value)
          
          updated_resources = updated_resources.set(i, {
            id: resource.id,
            loaded: true,
            value: Some(value),
            loader: resource.loader,
            load_time: Some(current_time),
            access_count: 1
          })
        }
        break
      }
    }
    
    let new_hit_count = if hit { cache.hit_count + 1 } else { cache.hit_count }
    let new_miss_count = if not(hit) { cache.miss_count + 1 } else { cache.miss_count }
    
    (result, {
      resources: updated_resources,
      hit_count: new_hit_count,
      miss_count: new_miss_count
    })
  }
  
  let preload_resource = fn(cache: ResourceCache, resource_id: String, current_time: Int) {
    let mut updated_resources = cache.resources
    
    for i in 0..cache.resources.length() {
      let resource = cache.resources[i]
      
      if resource.id == resource_id and not(resource.loaded) {
        let value = resource.loader()
        
        updated_resources = updated_resources.set(i, {
          id: resource.id,
          loaded: true,
          value: Some(value),
          loader: resource.loader,
          load_time: Some(current_time),
          access_count: 0
        })
        break
      }
    }
    
    { cache | resources: updated_resources }
  }
  
  let get_cache_stats = fn(cache: ResourceCache) {
    let total_requests = cache.hit_count + cache.miss_count
    let hit_ratio = if total_requests > 0 {
      (cache.hit_count.to_float() / total_requests.to_float()) * 100.0
    } else {
      0.0
    }
    
    let loaded_count = cache.resources.fold(0, fn(acc, resource) {
      if resource.loaded { acc + 1 } else { acc }
    })
    
    {
      hit_count: cache.hit_count,
      miss_count: cache.miss_count,
      hit_ratio,
      total_resources: cache.resources.length(),
      loaded_resources: loaded_count
    }
  }
  
  // Test resource cache creation
  let cache = create_resource_cache()
  assert_eq(cache.resources.length(), 0)
  assert_eq(cache.hit_count, 0)
  assert_eq(cache.miss_count, 0)
  
  // Test adding resources
  let resource1 = create_lazy_resource("config", fn() { "config-data" })
  let resource2 = create_lazy_resource("schema", fn() { "schema-data" })
  let resource3 = create_lazy_resource("rules", fn() { "rules-data" })
  
  let cache1 = add_resource(cache, resource1)
  let cache2 = add_resource(cache1, resource2)
  let cache3 = add_resource(cache2, resource3)
  
  assert_eq(cache3.resources.length(), 3)
  
  // Check that resources are not loaded initially
  for resource in cache3.resources {
    assert_false(resource.loaded)
    assert_eq(resource.value, None)
    assert_eq(resource.access_count, 0)
  }
  
  // Test getting a resource (should trigger lazy loading)
  let current_time = 1640995200
  let (value1, cache4) = get_resource(cache3, "config", current_time)
  assert_eq(value1, Some("config-data"))
  
  // Check that the resource is now loaded
  let config_resource = cache4.resources.find(fn(r) { r.id == "config" }).unwrap()
  assert_true(config_resource.loaded)
  assert_eq(config_resource.value, Some("config-data"))
  assert_eq(config_resource.access_count, 1)
  assert_eq(config_resource.load_time, Some(current_time))
  
  // Test getting the same resource again (should be a hit)
  let (value1_again, cache5) = get_resource(cache4, "config", current_time + 10)
  assert_eq(value1_again, Some("config-data"))
  
  let config_resource_again = cache5.resources.find(fn(r) { r.id == "config" }).unwrap()
  assert_eq(config_resource_again.access_count, 2) // Access count should be incremented
  
  // Test getting a non-existent resource
  let (value_nonexistent, cache6) = get_resource(cache5, "nonexistent", current_time + 20)
  assert_eq(value_nonexistent, None)
  
  // Test preloading a resource
  let cache7 = preload_resource(cache6, "schema", current_time + 30)
  
  let schema_resource = cache7.resources.find(fn(r) { r.id == "schema" }).unwrap()
  assert_true(schema_resource.loaded)
  assert_eq(schema_resource.value, Some("schema-data"))
  assert_eq(schema_resource.access_count, 0) // Not accessed yet, just loaded
  
  // Test cache statistics
  let stats = get_cache_stats(cache7)
  assert_eq(stats.hit_count, 2) // Two hits for "config"
  assert_eq(stats.miss_count, 2) // One miss for "schema", one for "nonexistent"
  assert_eq(stats.total_resources, 3)
  assert_eq(stats.loaded_resources, 2) // "config" and "schema"
  
  // Hit ratio should be 2 / (2 + 2) = 0.5 = 50%
  assert_eq(stats.hit_ratio, 50.0)
}

// Test 6: Data Compression
test "data compression for telemetry transmission" {
  // Define compression types
  type CompressionAlgorithm = 
    | None
    | Gzip
    | Lz4
    | Snappy
  
  type CompressionResult = {
    compressed_data: String,
    original_size: Int,
    compressed_size: Int,
    compression_ratio: Float,
    algorithm: CompressionAlgorithm,
    compression_time_ms: Int
  }
  
  type CompressionStats = {
    total_compressed: Int,
    total_original: Int,
    average_ratio: Float,
    algorithm_usage: Array[(CompressionAlgorithm, Int)]
  }
  
  // Compression operations
  let compress_data = fn(data: String, algorithm: CompressionAlgorithm) {
    let start_time = 1640995200000 // Mock timestamp in milliseconds
    let original_size = data.length()
    
    let (compressed_data, compression_time_ms) = match algorithm {
      None => (data, 1) // No compression
      Gzip => {
        // Simulate gzip compression (roughly 70% of original size)
        let compressed_length = (original_size.to_float() * 0.7).to_int()
        (data.substring(0, compressed_length), 50) // 50ms compression time
      }
      Lz4 => {
        // Simulate LZ4 compression (roughly 50% of original size)
        let compressed_length = (original_size.to_float() * 0.5).to_int()
        (data.substring(0, compressed_length), 20) // 20ms compression time
      }
      Snappy => {
        // Simulate Snappy compression (roughly 60% of original size)
        let compressed_length = (original_size.to_float() * 0.6).to_int()
        (data.substring(0, compressed_length), 10) // 10ms compression time
      }
    }
    
    let compressed_size = compressed_data.length()
    let compression_ratio = if compressed_size > 0 {
      original_size.to_float() / compressed_size.to_float()
    } else {
      1.0
    }
    
    {
      compressed_data,
      original_size,
      compressed_size,
      compression_ratio,
      algorithm,
      compression_time_ms
    }
  }
  
  let decompress_data = fn(compressed_data: String, algorithm: CompressionAlgorithm, original_size: Int) {
    let start_time = 1640995200000 // Mock timestamp in milliseconds
    
    let (decompressed_data, decompression_time_ms) = match algorithm {
      None => (compressed_data, 1) // No decompression
      Gzip => {
        // Simulate decompression time (longer than compression)
        ("x".repeat(original_size), 100) // 100ms decompression time
      }
      Lz4 => {
        // LZ4 is faster for decompression
        ("x".repeat(original_size), 15) // 15ms decompression time
      }
      Snappy => {
        // Snappy is also fast for decompression
        ("x".repeat(original_size), 12) // 12ms decompression time
      }
    }
    
    (decompressed_data, decompression_time_ms)
  }
  
  let update_compression_stats = fn(stats: CompressionStats, result: CompressionResult) {
    let new_total_compressed = stats.total_compressed + result.compressed_size
    let new_total_original = stats.total_original + result.original_size
    let new_average_ratio = if new_total_compressed > 0 {
      new_total_original.to_float() / new_total_compressed.to_float()
    } else {
      1.0
    }
    
    let mut updated_algorithm_usage = stats.algorithm_usage
    let mut found_algorithm = false
    
    for i in 0..stats.algorithm_usage.length() {
      let (alg, count) = stats.algorithm_usage[i]
      
      // Compare algorithms
      let algorithm_match = match (alg, result.algorithm) {
        (None, None) => true
        (Gzip, Gzip) => true
        (Lz4, Lz4) => true
        (Snappy, Snappy) => true
        _ => false
      }
      
      if algorithm_match {
        updated_algorithm_usage = updated_algorithm_usage.set(i, (alg, count + 1))
        found_algorithm = true
      }
    }
    
    if not(found_algorithm) {
      updated_algorithm_usage = updated_algorithm_usage.push((result.algorithm, 1))
    }
    
    {
      total_compressed: new_total_compressed,
      total_original: new_total_original,
      average_ratio: new_average_ratio,
      algorithm_usage: updated_algorithm_usage
    }
  }
  
  let select_best_algorithm = fn(data: String, algorithms: Array[CompressionAlgorithm>) {
    let mut best_algorithm = None
    let mut best_ratio = 1.0
    let mut best_time = 999999
    
    for algorithm in algorithms {
      let result = compress_data(data, algorithm)
      
      // Consider both compression ratio and time
      let score = result.compression_ratio / (result.compression_time_ms.to_float() / 10.0)
      
      if score > best_ratio / (best_time.to_float() / 10.0) {
        best_algorithm = Some(algorithm)
        best_ratio = result.compression_ratio
        best_time = result.compression_time_ms
      }
    }
    
    best_algorithm
  }
  
  // Test compression with different algorithms
  let test_data = "This is a sample telemetry data that will be compressed for transmission over the network. It contains various metrics and spans that need to be efficiently transferred."
  
  let none_result = compress_data(test_data, None)
  assert_eq(none_result.algorithm, None)
  assert_eq(none_result.original_size, test_data.length())
  assert_eq(none_result.compressed_size, test_data.length())
  assert_eq(none_result.compression_ratio, 1.0)
  assert_eq(none_result.compression_time_ms, 1)
  
  let gzip_result = compress_data(test_data, Gzip)
  assert_eq(gzip_result.algorithm, Gzip)
  assert_eq(gzip_result.original_size, test_data.length())
  assert_eq(gzip_result.compressed_size, (test_data.length().to_float() * 0.7).to_int())
  assert_eq(gzip_result.compression_ratio, 1.0 / 0.7)
  assert_eq(gzip_result.compression_time_ms, 50)
  
  let lz4_result = compress_data(test_data, Lz4)
  assert_eq(lz4_result.algorithm, Lz4)
  assert_eq(lz4_result.original_size, test_data.length())
  assert_eq(lz4_result.compressed_size, (test_data.length().to_float() * 0.5).to_int())
  assert_eq(lz4_result.compression_ratio, 1.0 / 0.5)
  assert_eq(lz4_result.compression_time_ms, 20)
  
  let snappy_result = compress_data(test_data, Snappy)
  assert_eq(snappy_result.algorithm, Snappy)
  assert_eq(snappy_result.original_size, test_data.length())
  assert_eq(snappy_result.compressed_size, (test_data.length().to_float() * 0.6).to_int())
  assert_eq(snappy_result.compression_ratio, 1.0 / 0.6)
  assert_eq(snappy_result.compression_time_ms, 10)
  
  // Test decompression
  let (decompressed_gzip, gzip_decompress_time) = decompress_data(gzip_result.compressed_data, Gzip, gzip_result.original_size)
  assert_eq(decompressed_gzip.length(), gzip_result.original_size)
  assert_eq(gzip_decompress_time, 100)
  
  let (decompressed_lz4, lz4_decompress_time) = decompress_data(lz4_result.compressed_data, Lz4, lz4_result.original_size)
  assert_eq(decompressed_lz4.length(), lz4_result.original_size)
  assert_eq(lz4_decompress_time, 15)
  
  // Test compression statistics
  let initial_stats = {
    total_compressed: 0,
    total_original: 0,
    average_ratio: 1.0,
    algorithm_usage: []
  }
  
  let stats1 = update_compression_stats(initial_stats, gzip_result)
  let stats2 = update_compression_stats(stats1, lz4_result)
  let stats3 = update_compression_stats(stats2, snappy_result)
  
  assert_eq(stats3.total_compressed, gzip_result.compressed_size + lz4_result.compressed_size + snappy_result.compressed_size)
  assert_eq(stats3.total_original, gzip_result.original_size + lz4_result.original_size + snappy_result.original_size)
  assert_eq(stats3.algorithm_usage.length(), 3)
  
  // Test algorithm selection
  let algorithms = [None, Gzip, Lz4, Snappy]
  let best_algorithm = select_best_algorithm(test_data, algorithms)
  
  // Based on our scoring, Snappy should be selected (good ratio with fast compression)
  match best_algorithm {
    Some(Snappy) => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 7: Asynchronous Processing
test "asynchronous processing for telemetry data" {
  // Define async processing types
  type AsyncTask = {
    id: String,
    status: String, // "pending", "running", "completed", "failed"
    created_at: Int,
    started_at: Option[Int>,
    completed_at: Option[Int],
    result: Option<String>,
    error: Option<String>
  }
  
  type AsyncProcessor = {
    max_concurrent_tasks: Int,
    task_queue: Array<AsyncTask>,
    running_tasks: Array<AsyncTask>,
    completed_tasks: Array<AsyncTask>
  }
  
  // Async processing operations
  let create_task = fn(id: String) {
    {
      id,
      status: "pending",
      created_at: 1640995200,
      started_at: None,
      completed_at: None,
      result: None,
      error: None
    }
  }
  
  let create_processor = fn(max_concurrent_tasks: Int) {
    {
      max_concurrent_tasks,
      task_queue: [],
      running_tasks: [],
      completed_tasks: []
    }
  }
  
  let submit_task = fn(processor: AsyncProcessor, task: AsyncTask) {
    { processor | task_queue: processor.task_queue.push(task) }
  }
  
  let process_tasks = fn(processor: AsyncProcessor, current_time: Int) {
    let mut updated_processor = processor
    let available_slots = updated_processor.max_concurrent_tasks - updated_processor.running_tasks.length()
    
    // Move tasks from queue to running if slots available
    let mut tasks_to_move = []
    let mut remaining_queue = updated_processor.task_queue
    
    for i in 0..available_slots {
      if i < remaining_queue.length() {
        tasks_to_move = tasks_to_move.push(remaining_queue[i])
      }
    }
    
    if tasks_to_move.length() > 0 {
      remaining_queue = remaining_queue.slice(tasks_to_move.length(), remaining_queue.length())
      
      let mut new_running_tasks = updated_processor.running_tasks
      
      for task in tasks_to_move {
        let running_task = {
          id: task.id,
          status: "running",
          created_at: task.created_at,
          started_at: Some(current_time),
          completed_at: None,
          result: None,
          error: None
        }
        
        new_running_tasks = new_running_tasks.push(running_task)
      }
      
      updated_processor = {
        max_concurrent_tasks: updated_processor.max_concurrent_tasks,
        task_queue: remaining_queue,
        running_tasks: new_running_tasks,
        completed_tasks: updated_processor.completed_tasks
      }
    }
    
    // Simulate task completion
    let mut new_running_tasks = updated_processor.running_tasks
    let mut new_completed_tasks = updated_processor.completed_tasks
    
    for i in 0..updated_processor.running_tasks.length() {
      let task = updated_processor.running_tasks[i]
      
      // Simulate some tasks completing
      if i % 2 == 0 { // Every other task completes
        let completed_task = {
          id: task.id,
          status: "completed",
          created_at: task.created_at,
          started_at: task.started_at,
          completed_at: Some(current_time + 100), // 100ms execution time
          result: Some("result-" + task.id),
          error: None
        }
        
        new_completed_tasks = new_completed_tasks.push(completed_task)
      } else {
        // Keep running
        new_running_tasks = new_running_tasks.push(task)
      }
    }
    
    {
      max_concurrent_tasks: updated_processor.max_concurrent_tasks,
      task_queue: updated_processor.task_queue,
      running_tasks: new_running_tasks,
      completed_tasks: new_completed_tasks
    }
  }
  
  let get_task_status = fn(processor: AsyncProcessor, task_id: String) {
    // Check in all task collections
    for task in processor.task_queue {
      if task.id == task_id {
        return task.status
      }
    }
    
    for task in processor.running_tasks {
      if task.id == task_id {
        return task.status
      }
    }
    
    for task in processor.completed_tasks {
      if task.id == task_id {
        return task.status
      }
    }
    
    "not-found"
  }
  
  // Test processor creation
  let processor = create_processor(3)
  assert_eq(processor.max_concurrent_tasks, 3)
  assert_eq(processor.task_queue.length(), 0)
  assert_eq(processor.running_tasks.length(), 0)
  assert_eq(processor.completed_tasks.length(), 0)
  
  // Test submitting tasks
  let task1 = create_task("task-1")
  let task2 = create_task("task-2")
  let task3 = create_task("task-3")
  let task4 = create_task("task-4")
  let task5 = create_task("task-5")
  
  let processor1 = submit_task(processor, task1)
  let processor2 = submit_task(processor1, task2)
  let processor3 = submit_task(processor2, task3)
  let processor4 = submit_task(processor3, task4)
  let processor5 = submit_task(processor4, task5)
  
  assert_eq(processor5.task_queue.length(), 5)
  assert_eq(processor5.running_tasks.length(), 0)
  
  // Test task processing
  let current_time = 1640995200
  let processor6 = process_tasks(processor5, current_time)
  
  // 3 tasks should be moved to running (max_concurrent_tasks)
  assert_eq(processor6.task_queue.length(), 2)
  assert_eq(processor6.running_tasks.length(), 3)
  
  // Check task statuses
  assert_eq(get_task_status(processor6, "task-1"), "running")
  assert_eq(get_task_status(processor6, "task-2"), "running")
  assert_eq(get_task_status(processor6, "task-3"), "running")
  assert_eq(get_task_status(processor6, "task-4"), "pending")
  assert_eq(get_task_status(processor6, "task-5"), "pending")
  
  // Process tasks again (some should complete)
  let processor7 = process_tasks(processor6, current_time + 100)
  
  // 1 task should complete (every other one), 2 should still be running
  assert_eq(processor7.running_tasks.length(), 1)
  assert_eq(processor7.completed_tasks.length(), 2)
  
  // Check task statuses
  assert_eq(get_task_status(processor7, "task-1"), "completed")
  assert_eq(get_task_status(processor7, "task-2"), "running")
  assert_eq(get_task_status(processor7, "task-3"), "completed")
  assert_eq(get_task_status(processor7, "task-4"), "pending")
  assert_eq(get_task_status(processor7, "task-5"), "pending")
  
  // Process tasks again (remaining tasks should start)
  let processor8 = process_tasks(processor7, current_time + 200)
  
  // 2 more tasks should start running (1 from queue, 1 from running)
  assert_eq(processor8.task_queue.length(), 1)
  assert_eq(processor8.running_tasks.length(), 2)
  assert_eq(processor8.completed_tasks.length(), 2)
  
  // Check task statuses
  assert_eq(get_task_status(processor8, "task-2"), "completed")
  assert_eq(get_task_status(processor8, "task-4"), "running")
  assert_eq(get_task_status(processor8, "task-5"), "pending")
}

// Test 8: Resource Pooling
test "resource pooling for telemetry operations" {
  // Define resource pool types
  type Resource = {
    id: String,
    type: String,
    created_at: Int,
    last_used: Int,
    in_use: Bool,
    use_count: Int,
    health: String // "healthy", "degraded", "unhealthy"
  }
  
  type ResourcePool = {
    resources: Array<Resource>,
    max_size: Int,
    min_size: Int,
    resource_type: String
  }
  
  // Resource pool operations
  let create_resource = fn(id: String, resource_type: String, current_time: Int) {
    {
      id,
      type: resource_type,
      created_at: current_time,
      last_used: current_time,
      in_use: false,
      use_count: 0,
      health: "healthy"
    }
  }
  
  let create_resource_pool = fn(min_size: Int, max_size: Int, resource_type: String, current_time: Int) {
    let mut resources = []
    for i in 0..min_size {
      resources = resources.push(create_resource(resource_type + "-" + i.to_string(), resource_type, current_time))
    }
    
    {
      resources,
      max_size,
      min_size,
      resource_type
    }
  }
  
  let acquire_resource = fn(pool: ResourcePool, current_time: Int) {
    let mut available_resource = None
    let mut updated_resources = pool.resources
    
    // Find a healthy, available resource
    for i in 0..pool.resources.length() {
      let resource = pool.resources[i]
      
      if not(resource.in_use) and resource.health == "healthy" {
        available_resource = Some(resource)
        
        // Mark as in use and update last used time
        updated_resources = updated_resources.set(i, {
          id: resource.id,
          type: resource.type,
          created_at: resource.created_at,
          last_used: current_time,
          in_use: true,
          use_count: resource.use_count + 1,
          health: resource.health
        })
        break
      }
    }
    
    match available_resource {
      Some(resource) => (Some(resource), { pool | resources: updated_resources })
      None => {
        // Try to create a new resource if under max limit
        if updated_resources.length() < pool.max_size {
          let new_resource = create_resource(pool.resource_type + "-" + current_time.to_string(), pool.resource_type, current_time)
          
          let acquired_new_resource = {
            id: new_resource.id,
            type: new_resource.type,
            created_at: new_resource.created_at,
            last_used: current_time,
            in_use: true,
            use_count: new_resource.use_count + 1,
            health: new_resource.health
          }
          
          updated_resources = updated_resources.push(acquired_new_resource)
          
          (Some(acquired_new_resource), { pool | resources: updated_resources })
        } else {
          (None, { pool | resources: updated_resources })
        }
      }
    }
  }
  
  let release_resource = fn(pool: ResourcePool, resource_id: String, current_time: Int) {
    let mut updated_resources = pool.resources
    
    for i in 0..pool.resources.length() {
      let resource = pool.resources[i]
      
      if resource.id == resource_id {
        updated_resources = updated_resources.set(i, {
          id: resource.id,
          type: resource.type,
          created_at: resource.created_at,
          last_used: current_time,
          in_use: false,
          use_count: resource.use_count,
          health: resource.health
        })
        break
      }
    }
    
    { pool | resources: updated_resources }
  }
  
  let update_resource_health = fn(pool: ResourcePool, resource_id: String, health: String) {
    let mut updated_resources = pool.resources
    
    for i in 0..pool.resources.length() {
      let resource = pool.resources[i]
      
      if resource.id == resource_id {
        updated_resources = updated_resources.set(i, {
          id: resource.id,
          type: resource.type,
          created_at: resource.created_at,
          last_used: resource.last_used,
          in_use: resource.in_use,
          use_count: resource.use_count,
          health
        })
        break
      }
    }
    
    { pool | resources: updated_resources }
  }
  
  let cleanup_unhealthy_resources = fn(pool: ResourcePool) {
    let mut healthy_resources = []
    
    for resource in pool.resources {
      if resource.health != "unhealthy" or resource.in_use {
        // Keep healthy resources or unhealthy resources that are in use
        healthy_resources = healthy_resources.push(resource)
      }
    }
    
    { pool | resources: healthy_resources }
  }
  
  // Test resource pool creation
  let current_time = 1640995200
  let pool = create_resource_pool(2, 5, "database-connection", current_time)
  assert_eq(pool.min_size, 2)
  assert_eq(pool.max_size, 5)
  assert_eq(pool.resource_type, "database-connection")
  assert_eq(pool.resources.length(), 2)
  
  for resource in pool.resources {
    assert_false(resource.in_use)
    assert_eq(resource.use_count, 0)
    assert_eq(resource.health, "healthy")
  }
  
  // Test acquiring resources
  let (resource1, pool1) = acquire_resource(pool, current_time + 10)
  assert_true(resource1.is_some())
  
  let resource1_unwrapped = resource1.unwrap()
  assert_true(resource1_unwrapped.in_use)
  assert_eq(resource1_unwrapped.use_count, 1)
  
  // Test acquiring another resource
  let (resource2, pool2) = acquire_resource(pool1, current_time + 20)
  assert_true(resource2.is_some())
  assert_not_equal(resource1_unwrapped.id, resource2.unwrap().id)
  
  // Test releasing a resource
  let pool3 = release_resource(pool2, resource1_unwrapped.id, current_time + 30)
  
  let resource1_released = pool3.resources.find(fn(r) { r.id == resource1_unwrapped.id }).unwrap()
  assert_false(resource1_released.in_use)
  assert_eq(resource1_released.last_used, current_time + 30)
  
  // Test that released resource can be reused
  let (resource1_reused, pool4) = acquire_resource(pool3, current_time + 40)
  assert_true(resource1_reused.is_some())
  assert_eq(resource1_reused.unwrap().id, resource1_unwrapped.id)
  assert_eq(resource1_reused.unwrap().use_count, 2)
  
  // Test resource health management
  let pool5 = update_resource_health(pool4, resource2.unwrap().id, "degraded")
  
  let resource2_updated = pool5.resources.find(fn(r) { r.id == resource2.unwrap().id }).unwrap()
  assert_eq(resource2_updated.health, "degraded")
  
  // Test that degraded resources can still be used
  let (resource2_degraded, pool6) = acquire_resource(pool5, current_time + 50)
  assert_true(resource2_degraded.is_some())
  assert_eq(resource2_degraded.unwrap().id, resource2.unwrap().id)
  
  // Test unhealthy resources
  let pool7 = update_resource_health(pool6, resource1_reused.unwrap().id, "unhealthy")
  
  // Unhealthy resources should not be acquired
  let (resource_unhealthy, pool8) = acquire_resource(pool7, current_time + 60)
  assert_true(resource_unhealthy.is_some()) // Should get a different resource
  
  assert_not_equal(resource_unhealthy.unwrap().id, resource1_reused.unwrap().id)
  
  // Test cleanup of unhealthy resources
  let pool9 = cleanup_unhealthy_resources(pool8)
  
  let unhealthy_count = pool9.resources.fold(0, fn(acc, resource) {
    if resource.health == "unhealthy" and not(resource.in_use) { acc + 1 } else { acc }
  })
  
  assert_eq(unhealthy_count, 0) // Unhealthy resources should be cleaned up
}