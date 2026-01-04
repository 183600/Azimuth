// Azimuth Premium Performance Optimization Tests
// This file contains comprehensive test cases for performance optimization and benchmarking

// Test 1: Memory Pool Allocation Optimization
test "memory pool allocation optimization" {
  // Define memory pool structure
  type MemoryPool = {
    pool_size: Int,
    allocated_blocks: Int,
    free_blocks: Array[Int],
    used_blocks: Array[Int],
    allocation_count: Int,
    deallocation_count: Int
  }
  
  // Create memory pool
  let create_memory_pool = fn(size: Int) {
    {
      pool_size: size,
      allocated_blocks: 0,
      free_blocks: (0..size).to_array(),
      used_blocks: [],
      allocation_count: 0,
      deallocation_count: 0
    }
  }
  
  // Allocate block from pool
  let allocate_block = fn(pool: MemoryPool) {
    if pool.free_blocks.length() > 0 {
      let block_id = pool.free_blocks[0]
      let updated_free = pool.free_blocks.slice(1, pool.free_blocks.length())
      let updated_used = pool.used_blocks.push(block_id)
      
      ({
        pool_size: pool.pool_size,
        allocated_blocks: pool.allocated_blocks + 1,
        free_blocks: updated_free,
        used_blocks: updated_used,
        allocation_count: pool.allocation_count + 1,
        deallocation_count: pool.deallocation_count
      }, Some(block_id))
    } else {
      (pool, None)
    }
  }
  
  // Deallocate block to pool
  let deallocate_block = fn(pool: MemoryPool, block_id: Int) {
    if pool.used_blocks.contains(block_id) {
      let updated_used = pool.used_blocks.filter(fn(id) { id != block_id })
      let updated_free = pool.free_blocks.push(block_id)
      
      ({
        pool_size: pool.pool_size,
        allocated_blocks: pool.allocated_blocks - 1,
        free_blocks: updated_free,
        used_blocks: updated_used,
        allocation_count: pool.allocation_count,
        deallocation_count: pool.deallocation_count + 1
      }, true)
    } else {
      (pool, false)
    }
  }
  
  // Test memory pool creation
  let pool = create_memory_pool(100)
  assert_eq(pool.pool_size, 100)
  assert_eq(pool.free_blocks.length(), 100)
  assert_eq(pool.used_blocks.length(), 0)
  assert_eq(pool.allocation_count, 0)
  assert_eq(pool.deallocation_count, 0)
  
  // Test block allocation
  let (pool1, block1) = allocate_block(pool)
  match block1 {
    Some(id) => {
      assert_eq(id, 0)
      assert_eq(pool1.free_blocks.length(), 99)
      assert_eq(pool1.used_blocks.length(), 1)
      assert_eq(pool1.allocation_count, 1)
    }
    None => assert_true(false)
  }
  
  // Test multiple allocations
  let (pool2, block2) = allocate_block(pool1)
  let (pool3, block3) = allocate_block(pool2)
  let (pool4, block4) = allocate_block(pool3)
  
  match block2 {
    Some(id) => assert_eq(id, 1)
    None => assert_true(false)
  }
  
  match block3 {
    Some(id) => assert_eq(id, 2)
    None => assert_true(false)
  }
  
  match block4 {
    Some(id) => assert_eq(id, 3)
    None => assert_true(false)
  }
  
  assert_eq(pool4.used_blocks.length(), 4)
  assert_eq(pool4.free_blocks.length(), 96)
  assert_eq(pool4.allocation_count, 4)
  
  // Test block deallocation
  let (pool5, dealloc_result1) = deallocate_block(pool4, 1)
  assert_true(dealloc_result1)
  assert_eq(pool5.used_blocks.length(), 3)
  assert_eq(pool5.free_blocks.length(), 97)
  assert_eq(pool5.deallocation_count, 1)
  
  // Test deallocation of non-existent block
  let (pool6, dealloc_result2) = deallocate_block(pool5, 99)
  assert_false(dealloc_result2)
  assert_eq(pool6.used_blocks.length(), 3)
  assert_eq(pool6.free_blocks.length(), 97)
  assert_eq(pool6.deallocation_count, 1)  // No change
  
  // Test pool exhaustion
  let mut exhausted_pool = pool6
  let mut allocated_ids = []
  
  // Allocate remaining blocks
  for i in 0..96 {
    let (new_pool, block_id) = allocate_block(exhausted_pool)
    match block_id {
      Some(id) => {
        exhausted_pool = new_pool
        allocated_ids = allocated_ids.push(id)
      }
      None => assert_true(false)
    }
  }
  
  assert_eq(exhausted_pool.free_blocks.length(), 0)
  assert_eq(exhausted_pool.used_blocks.length(), 99)
  
  // Try to allocate from exhausted pool
  let (final_pool, no_block) = allocate_block(exhausted_pool)
  match no_block {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  assert_eq(final_pool.free_blocks.length(), 0)
  assert_eq(final_pool.used_blocks.length(), 99)
}

// Test 2: Cache Performance Optimization
test "cache performance optimization" {
  // Define cache entry structure
  type CacheEntry[T] = {
    key: String,
    value: T,
    access_count: Int,
    last_accessed: Int,
    size: Int
  }
  
  // Define cache structure
  type Cache[T] = {
    max_size: Int,
    current_size: Int,
    entries: Array[CacheEntry[T]],
    hit_count: Int,
    miss_count: Int,
    eviction_count: Int
  }
  
  // Create cache
  let create_cache = fn(max_size: Int) {
    {
      max_size,
      current_size: 0,
      entries: [],
      hit_count: 0,
      miss_count: 0,
      eviction_count: 0
    }
  }
  
  // Get value from cache
  let get = fn(cache: Cache[T], key: String, current_time: Int) {
    match cache.entries.find_index(fn(entry) { entry.key == key }) {
      Some(index) => {
        let entry = cache.entries[index]
        let updated_entry = { 
          entry | 
          access_count: entry.access_count + 1,
          last_accessed: current_time
        }
        let updated_entries = cache.entries.update(index, updated_entry)
        
        ({
          max_size: cache.max_size,
          current_size: cache.current_size,
          entries: updated_entries,
          hit_count: cache.hit_count + 1,
          miss_count: cache.miss_count,
          eviction_count: cache.eviction_count
        }, Some(updated_entry.value))
      }
      None => {
        ({
          max_size: cache.max_size,
          current_size: cache.current_size,
          entries: cache.entries,
          hit_count: cache.hit_count,
          miss_count: cache.miss_count + 1,
          eviction_count: cache.eviction_count
        }, None)
      }
    }
  }
  
  // Put value in cache
  let put = fn(cache: Cache[T], key: String, value: T, size: Int, current_time: Int) {
    match cache.entries.find_index(fn(entry) { entry.key == key }) {
      Some(index) => {
        // Update existing entry
        let old_entry = cache.entries[index]
        let new_entry = {
          key,
          value,
          access_count: old_entry.access_count + 1,
          last_accessed: current_time,
          size
        }
        let updated_entries = cache.entries.update(index, new_entry)
        let size_diff = size - old_entry.size
        
        ({
          max_size: cache.max_size,
          current_size: cache.current_size + size_diff,
          entries: updated_entries,
          hit_count: cache.hit_count,
          miss_count: cache.miss_count,
          eviction_count: cache.eviction_count
        }, true)
      }
      None => {
        // Add new entry
        if cache.current_size + size <= cache.max_size {
          let new_entry = {
            key,
            value,
            access_count: 1,
            last_accessed: current_time,
            size
          }
          let updated_entries = cache.entries.push(new_entry)
          
          ({
            max_size: cache.max_size,
            current_size: cache.current_size + size,
            entries: updated_entries,
            hit_count: cache.hit_count,
            miss_count: cache.miss_count,
            eviction_count: cache.eviction_count
          }, true)
        } else {
          // Need to evict entries (LRU strategy)
          let sorted_entries = cache.entries.sort_by(fn(a, b) { 
            if a.last_accessed < b.last_accessed { -1 } 
            else if a.last_accessed > b.last_accessed { 1 } 
            else { 0 }
          })
          
          let mut evicted_count = 0
          let mut freed_size = 0
          let mut remaining_entries = cache.entries
          
          while cache.current_size + size - freed_size > cache.max_size && evicted_count < sorted_entries.length() {
            let lru_entry = sorted_entries[evicted_count]
            freed_size = freed_size + lru_entry.size
            remaining_entries = remaining_entries.filter(fn(entry) { entry.key != lru_entry.key })
            evicted_count = evicted_count + 1
          }
          
          if cache.current_size + size - freed_size <= cache.max_size {
            let new_entry = {
              key,
              value,
              access_count: 1,
              last_accessed: current_time,
              size
            }
            let updated_entries = remaining_entries.push(new_entry)
            
            ({
              max_size: cache.max_size,
              current_size: cache.current_size + size - freed_size,
              entries: updated_entries,
              hit_count: cache.hit_count,
              miss_count: cache.miss_count,
              eviction_count: cache.eviction_count + evicted_count
            }, true)
          } else {
            // Still not enough space
            ({
              max_size: cache.max_size,
              current_size: cache.current_size,
              entries: cache.entries,
              hit_count: cache.hit_count,
              miss_count: cache.miss_count,
              eviction_count: cache.eviction_count
            }, false)
          }
        }
      }
    }
  }
  
  // Test cache creation
  let cache = create_cache(1000)
  assert_eq(cache.max_size, 1000)
  assert_eq(cache.current_size, 0)
  assert_eq(cache.entries.length(), 0)
  assert_eq(cache.hit_count, 0)
  assert_eq(cache.miss_count, 0)
  
  // Test cache miss
  let (cache1, result1) = get(cache, "key1", 1640995200)
  match result1 {
    None => {
      assert_eq(cache1.miss_count, 1)
      assert_eq(cache1.hit_count, 0)
    }
    Some(_) => assert_true(false)
  }
  
  // Test cache put
  let (cache2, put_result1) = put(cache1, "key1", "value1", 100, 1640995200)
  assert_true(put_result1)
  assert_eq(cache2.current_size, 100)
  assert_eq(cache2.entries.length(), 1)
  
  // Test cache hit
  let (cache3, result2) = get(cache2, "key1", 1640995300)
  match result2 {
    Some(value) => {
      assert_eq(value, "value1")
      assert_eq(cache3.hit_count, 1)
      assert_eq(cache3.miss_count, 1)
    }
    None => assert_true(false)
  }
  
  // Test multiple cache entries
  let (cache4, _) = put(cache3, "key2", "value2", 200, 1640995400)
  let (cache5, _) = put(cache4, "key3", "value3", 300, 1640995500)
  let (cache6, _) = put(cache5, "key4", "value4", 400, 1640995600)
  
  assert_eq(cache6.current_size, 1000)
  assert_eq(cache6.entries.length(), 4)
  
  // Test cache eviction
  let (cache7, put_result2) = put(cache6, "key5", "value5", 100, 1640995700)
  assert_true(put_result2)
  assert_eq(cache7.current_size, 900)  // 1000 + 100 - 200 (evicted key2)
  assert_eq(cache7.entries.length(), 4)
  assert_eq(cache7.eviction_count, 1)
  
  // Verify LRU eviction
  let (cache8, result3) = get(cache7, "key2", 1640995800)
  match result3 {
    None => assert_true(true)  // key2 should be evicted
    Some(_) => assert_true(false)
  }
  
  // Verify non-evicted entries are still accessible
  let (cache9, result4) = get(cache8, "key1", 1640995900)
  match result4 {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
}

// Test 3: Batch Processing Optimization
test "batch processing optimization" {
  // Define batch configuration
  type BatchConfig = {
    batch_size: Int,
    max_wait_time_ms: Int,
    processing_timeout_ms: Int
  }
  
  // Define batch item
  type BatchItem = {
    id: String,
    data: String,
    timestamp: Int,
    processed: Bool
  }
  
  // Define batch processor
  type BatchProcessor = {
    config: BatchConfig,
    pending_items: Array[BatchItem],
    processed_items: Array[BatchItem],
    batch_count: Int,
    total_items: Int,
    total_processing_time_ms: Int
  }
  
  // Create batch processor
  let create_batch_processor = fn(batch_size: Int, max_wait_time_ms: Int, processing_timeout_ms: Int) {
    {
      config: {
        batch_size,
        max_wait_time_ms,
        processing_timeout_ms
      },
      pending_items: [],
      processed_items: [],
      batch_count: 0,
      total_items: 0,
      total_processing_time_ms: 0
    }
  }
  
  // Add item to batch
  let add_item = fn(processor: BatchProcessor, item: BatchItem) {
    {
      config: processor.config,
      pending_items: processor.pending_items.push(item),
      processed_items: processor.processed_items,
      batch_count: processor.batch_count,
      total_items: processor.total_items + 1,
      total_processing_time_ms: processor.total_processing_time_ms
    }
  }
  
  // Process batch
  let process_batch = fn(processor: BatchProcessor, current_time: Int) {
    if processor.pending_items.length() >= processor.config.batch_size {
      let batch_to_process = processor.pending_items.slice(0, processor.config.batch_size)
      let remaining_pending = processor.pending_items.slice(processor.config.batch_size, processor.pending_items.length())
      
      let processed_batch = batch_to_process.map(fn(item) { 
        { item | processed: true } 
      })
      
      let all_processed = processor.processed_items.concat(processed_batch)
      let processing_time = processor.config.processing_timeout_ms
      
      ({
        config: processor.config,
        pending_items: remaining_pending,
        processed_items: all_processed,
        batch_count: processor.batch_count + 1,
        total_items: processor.total_items,
        total_processing_time_ms: processor.total_processing_time_ms + processing_time
      }, true)
    } else {
      (processor, false)
    }
  }
  
  // Process by timeout
  let process_by_timeout = fn(processor: BatchProcessor, current_time: Int) {
    if processor.pending_items.length() > 0 {
      let oldest_item = processor.pending_items[0]
      if current_time - oldest_item.timestamp >= processor.config.max_wait_time_ms {
        let batch_to_process = processor.pending_items
        let processed_batch = batch_to_process.map(fn(item) { 
          { item | processed: true } 
        })
        
        let all_processed = processor.processed_items.concat(processed_batch)
        let processing_time = processor.config.processing_timeout_ms
        
        ({
          config: processor.config,
          pending_items: [],
          processed_items: all_processed,
          batch_count: processor.batch_count + 1,
          total_items: processor.total_items,
          total_processing_time_ms: processor.total_processing_time_ms + processing_time
        }, true)
      } else {
        (processor, false)
      }
    } else {
      (processor, false)
    }
  }
  
  // Test batch processor creation
  let processor = create_batch_processor(5, 1000, 100)
  assert_eq(processor.config.batch_size, 5)
  assert_eq(processor.config.max_wait_time_ms, 1000)
  assert_eq(processor.config.processing_timeout_ms, 100)
  assert_eq(processor.pending_items.length(), 0)
  assert_eq(processor.processed_items.length(), 0)
  
  // Test adding items
  let item1 = { id: "item1", data: "data1", timestamp: 1640995200000, processed: false }
  let item2 = { id: "item2", data: "data2", timestamp: 1640995200100, processed: false }
  let item3 = { id: "item3", data: "data3", timestamp: 1640995200200, processed: false }
  let item4 = { id: "item4", data: "data4", timestamp: 1640995200300, processed: false }
  let item5 = { id: "item5", data: "data5", timestamp: 1640995200400, processed: false }
  
  let processor1 = add_item(processor, item1)
  let processor2 = add_item(processor1, item2)
  let processor3 = add_item(processor2, item3)
  let processor4 = add_item(processor3, item4)
  let processor5 = add_item(processor4, item5)
  
  assert_eq(processor5.pending_items.length(), 5)
  assert_eq(processor5.total_items, 5)
  
  // Test batch processing by size
  let (processor6, processed1) = process_batch(processor5, 1640995200500)
  assert_true(processed1)
  assert_eq(processor6.pending_items.length(), 0)
  assert_eq(processor6.processed_items.length(), 5)
  assert_eq(processor6.batch_count, 1)
  assert_eq(processor6.total_processing_time_ms, 100)
  
  // Verify all items are processed
  for item in processor6.processed_items {
    assert_true(item.processed)
  }
  
  // Test timeout-based processing
  let item6 = { id: "item6", data: "data6", timestamp: 1640995201000, processed: false }
  let item7 = { id: "item7", data: "data7", timestamp: 1640995201100, processed: false }
  
  let processor7 = add_item(processor6, item6)
  let processor8 = add_item(processor7, item7)
  
  assert_eq(processor8.pending_items.length(), 2)
  
  // Try processing by size (should not process yet)
  let (processor9, processed2) = process_batch(processor8, 1640995201200)
  assert_false(processed2)
  assert_eq(processor9.pending_items.length(), 2)
  
  // Process by timeout
  let (processor10, processed3) = process_by_timeout(processor9, 1640995202500)  // 1500ms after first item
  assert_true(processed3)
  assert_eq(processor10.pending_items.length(), 0)
  assert_eq(processor10.processed_items.length(), 7)
  assert_eq(processor10.batch_count, 2)
  
  // Test processing efficiency
  let avg_processing_time = processor10.total_processing_time_ms.to_float() / processor10.batch_count.to_float()
  assert_eq(avg_processing_time, 100.0)
  
  let items_per_batch = processor10.total_items.to_float() / processor10.batch_count.to_float()
  assert_eq(items_per_batch, 3.5)
}

// Test 4: Connection Pool Optimization
test "connection pool optimization" {
  // Define connection structure
  type Connection = {
    id: String,
    created_at: Int,
    last_used: Int,
    in_use: Bool,
    use_count: Int
  }
  
  // Define connection pool
  type ConnectionPool = {
    max_connections: Int,
    min_connections: Int,
    connections: Array[Connection],
    created_count: Int,
    destroyed_count: Int,
    hit_count: Int,
    miss_count: Int
  }
  
  // Create connection pool
  let create_connection_pool = fn(min_connections: Int, max_connections: Int) {
    let initial_connections = (0..min_connections).map(fn(i) {
      {
        id: "conn-" + i.to_string(),
        created_at: 1640995200000,
        last_used: 1640995200000,
        in_use: false,
        use_count: 0
      }
    })
    
    {
      max_connections,
      min_connections,
      connections: initial_connections,
      created_count: min_connections,
      destroyed_count: 0,
      hit_count: 0,
      miss_count: 0
    }
  }
  
  // Get connection from pool
  let get_connection = fn(pool: ConnectionPool, current_time: Int) {
    match pool.connections.find_index(fn(conn) { !conn.in_use }) {
      Some(index) => {
        let conn = pool.connections[index]
        let updated_conn = {
          id: conn.id,
          created_at: conn.created_at,
          last_used: current_time,
          in_use: true,
          use_count: conn.use_count + 1
        }
        let updated_connections = pool.connections.update(index, updated_conn)
        
        ({
          max_connections: pool.max_connections,
          min_connections: pool.min_connections,
          connections: updated_connections,
          created_count: pool.created_count,
          destroyed_count: pool.destroyed_count,
          hit_count: pool.hit_count + 1,
          miss_count: pool.miss_count
        }, Some(updated_conn.id))
      }
      None => {
        if pool.connections.length() < pool.max_connections {
          let new_conn = {
            id: "conn-" + pool.created_count.to_string(),
            created_at: current_time,
            last_used: current_time,
            in_use: true,
            use_count: 1
          }
          let updated_connections = pool.connections.push(new_conn)
          
          ({
            max_connections: pool.max_connections,
            min_connections: pool.min_connections,
            connections: updated_connections,
            created_count: pool.created_count + 1,
            destroyed_count: pool.destroyed_count,
            hit_count: pool.hit_count,
            miss_count: pool.miss_count + 1
          }, Some(new_conn.id))
        } else {
          // Pool exhausted
          ({
            max_connections: pool.max_connections,
            min_connections: pool.min_connections,
            connections: pool.connections,
            created_count: pool.created_count,
            destroyed_count: pool.destroyed_count,
            hit_count: pool.hit_count,
            miss_count: pool.miss_count + 1
          }, None)
        }
      }
    }
  }
  
  // Release connection back to pool
  let release_connection = fn(pool: ConnectionPool, connection_id: String, current_time: Int) {
    match pool.connections.find_index(fn(conn) { conn.id == connection_id }) {
      Some(index) => {
        let conn = pool.connections[index]
        if conn.in_use {
          let updated_conn = {
            id: conn.id,
            created_at: conn.created_at,
            last_used: current_time,
            in_use: false,
            use_count: conn.use_count
          }
          let updated_connections = pool.connections.update(index, updated_conn)
          
          ({
            max_connections: pool.max_connections,
            min_connections: pool.min_connections,
            connections: updated_connections,
            created_count: pool.created_count,
            destroyed_count: pool.destroyed_count,
            hit_count: pool.hit_count,
            miss_count: pool.miss_count
          }, true)
        } else {
          // Connection already released
          (pool, false)
        }
      }
      None => {
        // Connection not found
        (pool, false)
      }
    }
  }
  
  // Cleanup idle connections
  let cleanup_idle_connections = fn(pool: ConnectionPool, current_time: Int, idle_timeout_ms: Int) {
    let mut updated_pool = pool
    let mut cleaned_count = 0
    
    // Don't clean below minimum connections
    if updated_pool.connections.length() > updated_pool.min_connections {
      let idle_connections = updated_pool.connections.filter(fn(conn) { 
        !conn.in_use && (current_time - conn.last_used) > idle_timeout_ms 
      })
      
      let connections_to_keep = updated_pool.connections.length() - idle_connections.length()
      let min_to_remove = updated_pool.connections.length() - updated_pool.min_connections
      
      if idle_connections.length() > 0 && connections_to_keep >= updated_pool.min_connections {
        let remove_count = if idle_connections.length() > min_to_remove { min_to_remove } else { idle_connections.length() }
        let connections_to_remove = idle_connections.slice(0, remove_count)
        
        updated_pool = {
          max_connections: updated_pool.max_connections,
          min_connections: updated_pool.min_connections,
          connections: updated_pool.connections.filter(fn(conn) { 
            !connections_to_remove.any(fn(remove_conn) { remove_conn.id == conn.id })
          }),
          created_count: updated_pool.created_count,
          destroyed_count: updated_pool.destroyed_count + remove_count,
          hit_count: updated_pool.hit_count,
          miss_count: updated_pool.miss_count
        }
        
        cleaned_count = remove_count
      }
    }
    
    (updated_pool, cleaned_count)
  }
  
  // Test connection pool creation
  let pool = create_connection_pool(2, 5)
  assert_eq(pool.min_connections, 2)
  assert_eq(pool.max_connections, 5)
  assert_eq(pool.connections.length(), 2)
  assert_eq(pool.created_count, 2)
  assert_eq(pool.destroyed_count, 0)
  
  // Test getting connection (hit)
  let (pool1, conn1_id) = get_connection(pool, 1640995201000)
  match conn1_id {
    Some(id) => {
      assert_eq(id, "conn-0")
      assert_eq(pool1.hit_count, 1)
      assert_eq(pool1.miss_count, 0)
    }
    None => assert_true(false)
  }
  
  // Test getting another connection (hit)
  let (pool2, conn2_id) = get_connection(pool1, 1640995201100)
  match conn2_id {
    Some(id) => {
      assert_eq(id, "conn-1")
      assert_eq(pool2.hit_count, 2)
      assert_eq(pool2.miss_count, 0)
    }
    None => assert_true(false)
  }
  
  // Test getting connection when pool is full (miss)
  let (pool3, conn3_id) = get_connection(pool2, 1640995201200)
  match conn3_id {
    Some(id) => {
      assert_eq(id, "conn-2")
      assert_eq(pool3.hit_count, 2)
      assert_eq(pool3.miss_count, 1)
      assert_eq(pool3.created_count, 3)
    }
    None => assert_true(false)
  }
  
  // Test releasing connection
  let (pool4, release_result1) = release_connection(pool3, "conn-0", 1640995201300)
  assert_true(release_result1)
  
  // Test getting released connection (hit)
  let (pool5, conn4_id) = get_connection(pool4, 1640995201400)
  match conn4_id {
    Some(id) => {
      assert_eq(id, "conn-0")
      assert_eq(pool5.hit_count, 3)
      assert_eq(pool5.miss_count, 1)
    }
    None => assert_true(false)
  }
  
  // Test pool exhaustion
  let (pool6, conn5_id) = get_connection(pool5, 1640995201500)
  let (pool7, conn6_id) = get_connection(pool6, 1640995201600)
  
  match conn5_id {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  match conn6_id {
    None => assert_true(true)  // Should be None, pool exhausted
    Some(_) => assert_true(false)
  }
  
  assert_eq(pool7.connections.length(), 5)  // Max connections reached
  assert_eq(pool7.miss_count, 3)  // 3 misses (conn-2, conn-3, conn-4, and exhausted)
  
  // Test cleanup idle connections
  let (pool8, cleaned_count) = cleanup_idle_connections(pool7, 1640995300000, 10000)  // 3 hours later
  assert_eq(cleaned_count, 2)  // Should clean 2 connections, keeping min 2
  assert_eq(pool8.connections.length(), 3)  // 5 - 2 = 3 (min connections)
  assert_eq(pool8.destroyed_count, 2)
}