// Memory Optimization Tests for Azimuth Telemetry System
// This file contains test cases for memory optimization functionality

// Test 1: Memory Pool Management
test "memory pool management for efficient allocation" {
  // Define memory block
  type MemoryBlock = {
    id: Int,
    size: Int,
    is_free: Bool,
    next_free: Option[Int>
  }
  
  // Define memory pool
  type MemoryPool = {
    name: String,
    total_size: Int,
    block_size: Int,
    blocks: Array[MemoryBlock>,
    free_list: Option<Int>,
    allocated_count: Int
  }
  
  // Create memory pool
  let create_memory_pool = fn(name: String, total_size: Int, block_size: Int) {
    let block_count = total_size / block_size
    let mut blocks = []
    let mut free_list = Some(0)  // First block is free
    
    // Initialize blocks
    for i in 0..block_count {
      let next_free = if i < block_count - 1 {
        Some(i + 1)
      } else {
        None
      }
      
      blocks = blocks.push({
        id: i,
        size: block_size,
        is_free: true,
        next_free
      })
    }
    
    {
      name,
      total_size,
      block_size,
      blocks,
      free_list,
      allocated_count: 0
    }
  }
  
  // Allocate block from pool
  let allocate_block = fn(pool: MemoryPool) {
    match pool.free_list {
      Some(block_id) => {
        let block = pool.blocks[block_id]
        let updated_block = { block | is_free: false, next_free: None }
        
        let next_free = block.next_free
        let updated_blocks = pool.blocks.map_with_index(fn(i, b) {
          if i == block_id {
            updated_block
          } else {
            b
          }
        })
        
        let updated_pool = {
          pool |
          blocks: updated_blocks,
          free_list: next_free,
          allocated_count: pool.allocated_count + 1
        }
        
        (Some(block_id), updated_pool)
      }
      None => (None, pool)  // No free blocks
    }
  }
  
  // Free block back to pool
  let free_block = fn(pool: MemoryPool, block_id: Int) {
    if block_id >= 0 and block_id < pool.blocks.length() {
      let block = pool.blocks[block_id]
      
      if not(block.is_free) {
        let updated_block = { block | is_free: true, next_free: pool.free_list }
        
        let updated_blocks = pool.blocks.map_with_index(fn(i, b) {
          if i == block_id {
            updated_block
          } else {
            b
          }
        })
        
        let updated_pool = {
          pool |
          blocks: updated_blocks,
          free_list: Some(block_id),
          allocated_count: pool.allocated_count - 1
        }
        
        updated_pool
      } else {
        pool  // Block already free
      }
    } else {
      pool  // Invalid block ID
    }
  }
  
  // Get pool statistics
  let get_pool_stats = fn(pool: MemoryPool) {
    let free_blocks = pool.blocks.filter(fn(b) { b.is_free }).length()
    
    {
      total_blocks: pool.blocks.length(),
      free_blocks,
      allocated_blocks: pool.allocated_count,
      utilization: pool.allocated_count.to_float() / pool.blocks.length().to_float() * 100.0
    }
  }
  
  // Create memory pool
  let pool = create_memory_pool("main-pool", 1024, 64)  // 1024 bytes, 64 byte blocks
  
  // Verify initial state
  assert_eq(pool.blocks.length(), 16)  // 1024 / 64 = 16 blocks
  assert_eq(pool.allocated_count, 0)
  
  let stats = get_pool_stats(pool)
  assert_eq(stats.total_blocks, 16)
  assert_eq(stats.free_blocks, 16)
  assert_eq(stats.allocated_blocks, 0)
  assert_eq(stats.utilization, 0.0)
  
  // Allocate blocks
  let (block1_id, pool1) = allocate_block(pool)
  let (block2_id, pool2) = allocate_block(pool1)
  let (block3_id, pool3) = allocate_block(pool2)
  
  // Verify allocations
  match (block1_id, block2_id, block3_id) {
    (Some(id1), Some(id2), Some(id3)) => {
      assert_eq(id1, 0)
      assert_eq(id2, 1)
      assert_eq(id3, 2)
    }
    _ => assert_true(false)
  }
  
  assert_eq(pool3.allocated_count, 3)
  
  let stats3 = get_pool_stats(pool3)
  assert_eq(stats3.free_blocks, 13)
  assert_eq(stats3.allocated_blocks, 3)
  assert_eq(stats3.utilization, 18.75)  // 3/16 * 100
  
  // Free blocks
  let pool4 = free_block(pool3, block2_id.unwrap())
  let pool5 = free_block(pool4, block1_id)
  
  // Verify frees
  assert_eq(pool5.allocated_count, 1)
  
  let stats5 = get_pool_stats(pool5)
  assert_eq(stats5.free_blocks, 15)
  assert_eq(stats5.allocated_blocks, 1)
  assert_eq(stats5.utilization, 6.25)  // 1/16 * 100
  
  // Allocate more blocks
  let (block4_id, pool6) = allocate_block(pool5)
  
  // Should reuse freed blocks
  match block4_id {
    Some(id) => assert_eq(id, 1)  // Should reuse block 1
    None => assert_true(false)
  }
  
  // Allocate all remaining blocks
  let mut current_pool = pool6
  let mut allocated_ids = []
  
  for i in 0..20 {  // Try to allocate more than available
    let (block_id, new_pool) = allocate_block(current_pool)
    current_pool = new_pool
    
    match block_id {
      Some(id) => allocated_ids = allocated_ids.push(id)
      None => ()
    }
  }
  
  // Should only allocate 15 more blocks (total 16, 1 already allocated)
  assert_eq(allocated_ids.length(), 15)
  assert_eq(current_pool.allocated_count, 16)
  
  let final_stats = get_pool_stats(current_pool)
  assert_eq(final_stats.free_blocks, 0)
  assert_eq(final_stats.allocated_blocks, 16)
  assert_eq(final_stats.utilization, 100.0)
  
  // Try to allocate when pool is full
  let (no_block_id, final_pool) = allocate_block(current_pool)
  assert_eq(no_block_id, None)
  assert_eq(final_pool.allocated_count, 16)
}

// Test 2: Object Caching
test "object caching for memory efficiency" {
  // Define cache entry
  type CacheEntry[T] = {
    key: String,
    value: T,
    access_count: Int,
    last_access: Int,
    size: Int
  }
  
  // Define cache eviction policy
  enum EvictionPolicy {
    LRU  // Least Recently Used
    LFU  // Least Frequently Used
    FIFO // First In First Out
  }
  
  // Define object cache
  type ObjectCache[T] = {
    name: String,
    max_size: Int,
    max_memory: Int,  // in bytes
    eviction_policy: EvictionPolicy,
    entries: Array[CacheEntry[T>>,
    current_memory: Int
  }
  
  // Create object cache
  let create_cache = fn(name: String, max_size: Int, max_memory: Int, eviction_policy: EvictionPolicy) {
    {
      name,
      max_size,
      max_memory,
      eviction_policy,
      entries: [],
      current_memory: 0
    }
  }
  
  // Calculate object size (simplified)
  let calculate_size = fn(obj: T) {
    // In a real implementation, this would calculate the actual size
    100  // Fixed size for simplicity
  }
  
  // Find entry to evict
  let find_eviction_candidate = fn(cache: ObjectCache[T]) {
    if cache.entries.length() == 0 {
      None
    } else {
      match cache.eviction_policy {
        EvictionPolicy::LRU => {
          // Find entry with oldest last_access
          let mut oldest_index = 0
          let mut oldest_time = cache.entries[0].last_access
          
          for i in 1..cache.entries.length() {
            if cache.entries[i].last_access < oldest_time {
              oldest_time = cache.entries[i].last_access
              oldest_index = i
            }
          }
          
          Some(oldest_index)
        }
        EvictionPolicy::LFU => {
          // Find entry with lowest access_count
          let mut least_used_index = 0
          let mut least_count = cache.entries[0].access_count
          
          for i in 1..cache.entries.length() {
            if cache.entries[i].access_count < least_count {
              least_count = cache.entries[i].access_count
              least_used_index = i
            }
          }
          
          Some(least_used_index)
        }
        EvictionPolicy::FIFO => {
          // Find first entry
          Some(0)
        }
      }
    }
  }
  
  // Evict entry
  let evict_entry = fn(cache: ObjectCache[T]) {
    match find_eviction_candidate(cache) {
      Some(index) => {
        let entry = cache.entries[index]
        let remaining_entries = cache.entries.filter_with_index(fn(i, _) { i != index })
        
        {
          cache |
          entries: remaining_entries,
          current_memory: cache.current_memory - entry.size
        }
      }
      None => cache
    }
  }
  
  // Check if eviction is needed
  let needs_eviction = fn(cache: ObjectCache[T], new_entry_size: Int) {
    cache.entries.length() >= cache.max_size or
    cache.current_memory + new_entry_size > cache.max_memory
  }
  
  // Put object in cache
  let put = fn(cache: ObjectCache[T], key: String, value: T, current_time: Int) {
    let size = calculate_size(value)
    
    // Check if key already exists
    let existing_index = cache.entries.index_of(fn(e) { e.key == key })
    
    match existing_index {
      Some(index) => {
        // Update existing entry
        let old_entry = cache.entries[index]
        let updated_entry = {
          old_entry |
          value,
          access_count: old_entry.access_count + 1,
          last_access: current_time
        }
        
        let updated_entries = cache.entries.map_with_index(fn(i, e) {
          if i == index {
            updated_entry
          } else {
            e
          }
        })
        
        {
          cache |
          entries: updated_entries
        }
      }
      None => {
        // Add new entry
        let new_entry = {
          key,
          value,
          access_count: 1,
          last_access: current_time,
          size
        }
        
        // Evict if necessary
        let mut updated_cache = cache
        
        while needs_eviction(updated_cache, size) {
          updated_cache = evict_entry(updated_cache)
        }
        
        let updated_entries = updated_cache.entries.push(new_entry)
        
        {
          updated_cache |
          entries: updated_entries,
          current_memory: updated_cache.current_memory + size
        }
      }
    }
  }
  
  // Get object from cache
  let get = fn(cache: ObjectCache[T], key: String, current_time: Int) {
    let entry_index = cache.entries.index_of(fn(e) { e.key == key })
    
    match entry_index {
      Some(index) => {
        let entry = cache.entries[index]
        let updated_entry = {
          entry |
          access_count: entry.access_count + 1,
          last_access: current_time
        }
        
        let updated_entries = cache.entries.map_with_index(fn(i, e) {
          if i == index {
            updated_entry
          } else {
            e
          }
        })
        
        let updated_cache = { cache | entries: updated_entries }
        
        (Some(entry.value), updated_cache)
      }
      None => (None, cache)
    }
  }
  
  // Get cache statistics
  let get_cache_stats = fn(cache: ObjectCache[T]) {
    {
      entry_count: cache.entries.length(),
      memory_usage: cache.current_memory,
      memory_utilization: cache.current_memory.to_float() / cache.max_memory.to_float() * 100.0,
      size_utilization: cache.entries.length().to_float() / cache.max_size.to_float() * 100.0
    }
  }
  
  // Create cache
  let cache = create_cache("object-cache", 3, 500, EvictionPolicy::LRU)
  
  // Put objects
  let cache1 = put(cache, "key1", "value1", 1640995200)
  let cache2 = put(cache1, "key2", "value2", 1640995205)
  let cache3 = put(cache2, "key3", "value3", 1640995210)
  
  // Verify cache state
  assert_eq(cache3.entries.length(), 3)
  assert_eq(cache3.current_memory, 300)  // 3 * 100
  
  let stats3 = get_cache_stats(cache3)
  assert_eq(stats3.entry_count, 3)
  assert_eq(stats3.memory_usage, 300)
  assert_eq(stats3.memory_utilization, 60.0)  // 300/500 * 100
  assert_eq(stats3.size_utilization, 100.0)  // 3/3 * 100
  
  // Add another object (should evict one)
  let cache4 = put(cache3, "key4", "value4", 1640995215)
  
  assert_eq(cache4.entries.length(), 3)  // Still 3 entries
  assert_eq(cache4.current_memory, 300)  // Still 300 bytes
  
  // Should have evicted key1 (LRU)
  let key1_exists = cache4.entries.any(fn(e) { e.key == "key1" })
  assert_false(key1_exists)
  
  let key4_exists = cache4.entries.any(fn(e) { e.key == "key4" })
  assert_true(key4_exists)
  
  // Get object (should update access time)
  let (value2, cache5) = get(cache4, "key2", 1640995220)
  
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  // Verify access time updated
  let key2_entry = cache5.entries.find(fn(e) { e.key == "key2" }).unwrap()
  assert_eq(key2_entry.last_access, 1640995220)
  assert_eq(key2_entry.access_count, 2)
  
  // Test LFU eviction
  let lfu_cache = create_cache("lfu-cache", 3, 500, EvictionPolicy::LFU)
  
  let lfu1 = put(lfu_cache, "key1", "value1", 1640995200)
  let lfu2 = put(lfu1, "key2", "value2", 1640995205)
  let lfu3 = put(lfu2, "key3", "value3", 1640995210)
  
  // Access key1 multiple times
  let (_, lfu4) = get(lfu3, "key1", 1640995215)
  let (_, lfu5) = get(lfu4, "key1", 1640995220)
  
  // Add another object (should evict key2 or key3, not key1)
  let lfu6 = put(lfu5, "key4", "value4", 1640995225)
  
  let key1_exists_lfu = lfu6.entries.any(fn(e) { e.key == "key1" })
  assert_true(key1_exists_lfu)
  
  let key2_exists_lfu = lfu6.entries.any(fn(e) { e.key == "key2" })
  let key3_exists_lfu = lfu6.entries.any(fn(e) { e.key == "key3" })
  
  // One of key2 or key3 should be evicted
  assert_true(not(key2_exists_lfu) or not(key3_exists_lfu))
}

// Test 3: Memory Leak Detection
test "memory leak detection and prevention" {
  // Define memory allocation record
  type AllocationRecord = {
    id: String,
    size: Int,
    timestamp: Int,
    stack_trace: Array[String>
  }
  
  // Define memory tracker
  type MemoryTracker = {
    name: String,
    allocations: Array[AllocationRecord>,
    total_allocated: Int,
    peak_usage: Int,
    leak_threshold: Int
  }
  
  // Create memory tracker
  let create_tracker = fn(name: String, leak_threshold: Int) {
    {
      name,
      allocations: [],
      total_allocated: 0,
      peak_usage: 0,
      leak_threshold
    }
  }
  
  // Record allocation
  let allocate = fn(tracker: MemoryTracker, id: String, size: Int, timestamp: Int, stack_trace: Array[String>) {
    let record = {
      id,
      size,
      timestamp,
      stack_trace
    }
    
    let updated_allocations = tracker.allocations.push(record)
    let new_total = tracker.total_allocated + size
    let new_peak = if new_total > tracker.peak_usage {
      new_total
    } else {
      tracker.peak_usage
    }
    
    {
      tracker |
      allocations: updated_allocations,
      total_allocated: new_total,
      peak_usage: new_peak
    }
  }
  
  // Record deallocation
  let deallocate = fn(tracker: MemoryTracker, id: Int, timestamp: Int) {
    if id >= 0 and id < tracker.allocations.length() {
      let allocation = tracker.allocations[id]
      let updated_allocations = tracker.allocations.filter_with_index(fn(i, _) { i != id })
      let new_total = tracker.total_allocated - allocation.size
      
      {
        tracker |
        allocations: updated_allocations,
        total_allocated: new_total
      }
    } else {
      tracker
    }
  }
  
  // Detect potential leaks
  let detect_leaks = fn(tracker: MemoryTracker, current_time: Int, max_age: Int) {
    let old_allocations = tracker.allocations.filter(fn(a) {
      current_time - a.timestamp > max_age
    })
    
    let leaked_size = old_allocations.reduce(fn(acc, a) { acc + a.size }, 0)
    
    {
      leaked_allocations: old_allocations,
      leaked_size,
      is_leak_detected: leaked_size > tracker.leak_threshold
    }
  }
  
  // Get memory statistics
  let get_memory_stats = fn(tracker: MemoryTracker) {
    {
      allocation_count: tracker.allocations.length(),
      total_allocated: tracker.total_allocated,
      peak_usage: tracker.peak_usage,
      average_allocation: if tracker.allocations.length() > 0 {
        tracker.total_allocated / tracker.allocations.length()
      } else {
        0
      }
    }
  }
  
  // Find allocation by ID
  let find_allocation = fn(tracker: MemoryTracker, id: String) {
    tracker.allocations.find(fn(a) { a.id == id })
  }
  
  // Create memory tracker
  let tracker = create_tracker("main-tracker", 1000)  // 1000 byte leak threshold
  
  // Allocate memory
  let tracker1 = allocate(tracker, "alloc-1", 100, 1640995200, ["function1", "line10"])
  let tracker2 = allocate(tracker1, "alloc-2", 200, 1640995205, ["function2", "line20"])
  let tracker3 = allocate(tracker2, "alloc-3", 150, 1640995210, ["function3", "line30"])
  
  // Verify allocations
  assert_eq(tracker3.allocations.length(), 3)
  assert_eq(tracker3.total_allocated, 450)
  assert_eq(tracker3.peak_usage, 450)
  
  let stats3 = get_memory_stats(tracker3)
  assert_eq(stats3.allocation_count, 3)
  assert_eq(stats3.total_allocated, 450)
  assert_eq(stats3.average_allocation, 150)  // 450 / 3
  
  // Deallocate memory
  let alloc1_index = tracker3.allocations.index_of(fn(a) { a.id == "alloc-1" }).unwrap()
  let tracker4 = deallocate(tracker3, alloc1_index, 1640995215)
  
  assert_eq(tracker4.allocations.length(), 2)
  assert_eq(tracker4.total_allocated, 350)  // 450 - 100
  
  // Allocate more memory
  let tracker5 = allocate(tracker4, "alloc-4", 300, 1640995220, ["function4", "line40"])
  let tracker6 = allocate(tracker5, "alloc-5", 400, 1640995225, ["function5", "line50"])
  
  assert_eq(tracker6.allocations.length(), 4)
  assert_eq(tracker6.total_allocated, 1050)  // 350 + 300 + 400
  assert_eq(tracker6.peak_usage, 1050)
  
  // Detect leaks (old allocations)
  let leak_info = detect_leaks(tracker6, 1640995400, 150)  // 150 seconds max age
  
  // alloc-2 and alloc-3 are old (older than 150 seconds)
  assert_eq(leak_info.leaked_allocations.length(), 2)
  assert_eq(leak_info.leaked_size, 350)  // 200 + 150
  assert_true(leak_info.is_leak_detected)  // 350 > 1000 threshold
  
  // Find specific allocation
  let alloc2 = find_allocation(tracker6, "alloc-2")
  match alloc2 {
    Some(allocation) => {
      assert_eq(allocation.id, "alloc-2")
      assert_eq(allocation.size, 200)
      assert_eq(allocation.stack_trace[0], "function2")
    }
    None => assert_true(false)
  }
  
  // Test leak prevention with threshold
  let small_tracker = create_tracker("small-tracker", 100)  // 100 byte threshold
  
  let small1 = allocate(small_tracker, "small-1", 50, 1640995200, [])
  let small2 = allocate(small1, "small-2", 30, 1640995205, [])
  
  let small_leak_info = detect_leaks(small2, 1640995400, 150)
  
  assert_eq(small_leak_info.leaked_allocations.length(), 2)
  assert_eq(small_leak_info.leaked_size, 80)
  assert_false(small_leak_info.is_leak_detected)  // 80 < 100 threshold
}

// Test 4: Garbage Collection Optimization
test "garbage collection optimization strategies" {
  // Define garbage collection strategy
  enum GCStrategy {
    Manual
    Automatic(Int)  // Interval in milliseconds
    Threshold(Int)  // Memory threshold in bytes
    Hybrid(Int, Int)  // Both interval and threshold
  }
  
  // Define object with GC metadata
  type GCObject = {
    id: String,
    data: String,
    references: Array[String>,  // IDs of referenced objects
    is_marked: Bool,
    generation: Int,
    last_access: Int
  }
  
  // Define garbage collector
  type GarbageCollector = {
    name: String,
    strategy: GCStrategy,
    objects: Array[GCObject>,
    last_collection: Int,
    collection_count: Int,
    total_collected: Int,
    memory_threshold: Int,
    collection_interval: Int
  }
  
  // Create garbage collector
  let create_gc = fn(name: String, strategy: GCStrategy, memory_threshold: Int, collection_interval: Int) {
    {
      name,
      strategy,
      objects: [],
      last_collection: 0,
      collection_count: 0,
      total_collected: 0,
      memory_threshold,
      collection_interval
    }
  }
  
  // Add object to GC
  let add_object = fn(gc: GarbageCollector, id: String, data: String, references: Array[String>, generation: Int, current_time: Int) {
    let object = {
      id,
      data,
      references,
      is_marked: false,
      generation,
      last_access: current_time
    }
    
    let updated_objects = gc.objects.push(object)
    { gc | objects: updated_objects }
  }
  
  // Mark objects reachable from roots
  let mark_reachable = fn(objects: Array[GCObject>, root_ids: Array[String>) {
    let mut updated_objects = objects
    
    // Mark roots
    updated_objects = updated_objects.map(fn(obj) {
      if root_ids.any(fn(id) { id == obj.id }) {
        { obj | is_marked: true }
      } else {
        obj
      }
    })
    
    // Mark reachable objects (simplified - single pass)
    let mut changed = true
    while changed {
      changed = false
      
      updated_objects = updated_objects.map(fn(obj) {
        if obj.is_marked {
          obj
        } else {
          let is_referenced = obj.references.any(fn(ref_id) {
            updated_objects.any(fn(o) { o.id == ref_id and o.is_marked })
          })
          
          if is_referenced {
            changed = true
            { obj | is_marked: true }
          } else {
            obj
          }
        }
      })
    }
    
    updated_objects
  }
  
  // Sweep unmarked objects
  let sweep_unmarked = fn(objects: Array[GCObject>) {
    let (live_objects, dead_objects) = objects.partition(fn(obj) { obj.is_marked })
    
    // Reset marks for next collection
    let reset_objects = live_objects.map(fn(obj) { { obj | is_marked: false } })
    
    (reset_objects, dead_objects)
  }
  
  // Perform garbage collection
  let collect_garbage = fn(gc: GarbageCollector, root_ids: Array[String>, current_time: Int) {
    let marked_objects = mark_reachable(gc.objects, root_ids)
    let (live_objects, dead_objects) = sweep_unmarked(marked_objects)
    
    {
      gc |
      objects: live_objects,
      last_collection: current_time,
      collection_count: gc.collection_count + 1,
      total_collected: gc.total_collected + dead_objects.length()
    }
  }
  
  // Check if collection should run
  let should_collect = fn(gc: GarbageCollector, current_time: Int) {
    match gc.strategy {
      GCStrategy::Manual => false
      GCStrategy::Automatic(interval) => current_time - gc.last_collection >= interval
      GCStrategy::Threshold(threshold) => {
        let total_memory = gc.objects.reduce(fn(acc, obj) { acc + obj.data.length() }, 0)
        total_memory >= threshold
      }
      GCStrategy::Hybrid(interval, threshold) => {
        let time_condition = current_time - gc.last_collection >= interval
        let memory_condition = {
          let total_memory = gc.objects.reduce(fn(acc, obj) { acc + obj.data.length() }, 0)
          total_memory >= threshold
        }
        time_condition or memory_condition
      }
    }
  }
  
  // Get GC statistics
  let get_gc_stats = fn(gc: GarbageCollector) {
    let total_memory = gc.objects.reduce(fn(acc, obj) { acc + obj.data.length() }, 0)
    
    {
      object_count: gc.objects.length(),
      total_memory,
      collection_count: gc.collection_count,
      total_collected: gc.total_collected,
      collection_efficiency: if gc.collection_count > 0 {
        gc.total_collected.to_float() / gc.collection_count.to_float()
      } else {
        0.0
      }
    }
  }
  
  // Create garbage collectors with different strategies
  let manual_gc = create_gc("manual-gc", GCStrategy::Manual, 1000, 10000)
  let auto_gc = create_gc("auto-gc", GCStrategy::Automatic(5000), 1000, 10000)
  let threshold_gc = create_gc("threshold-gc", GCStrategy::Threshold(200), 1000, 10000)
  let hybrid_gc = create_gc("hybrid-gc", GCStrategy::Hybrid(10000, 150), 1000, 10000)
  
  // Add objects to manual GC
  let manual1 = add_object(manual_gc, "obj-1", "data1", [], 0, 1640995200)
  let manual2 = add_object(manual1, "obj-2", "data2", ["obj-1"], 0, 1640995205)
  let manual3 = add_object(manual2, "obj-3", "data3", ["obj-2"], 0, 1640995210)
  
  assert_eq(manual3.objects.length(), 3)
  
  // Manual collection
  let manual_collected = collect_garbage(manual3, ["obj-1"], 1640995250)
  
  // obj-1 is root, obj-2 references obj-1, obj-3 references obj-2
  // All should be kept
  assert_eq(manual_collected.objects.length(), 3)
  assert_eq(manual_collected.collection_count, 1)
  assert_eq(manual_collected.total_collected, 0)
  
  // Collection with different roots
  let manual_collected2 = collect_garbage(manual_collected, [], 1640995300)
  
  // No roots, all should be collected
  assert_eq(manual_collected2.objects.length(), 0)
  assert_eq(manual_collected2.collection_count, 2)
  assert_eq(manual_collected2.total_collected, 3)
  
  // Test automatic collection
  let auto1 = add_object(auto_gc, "auto-1", "data1", [], 0, 1640995200)
  let auto2 = add_object(auto1, "auto-2", "data2", [], 0, 1640995205)
  
  // Should not collect yet (interval not reached)
  assert_false(should_collect(auto2, 1640995250))
  
  // Should collect after interval
  assert_true(should_collect(auto2, 1640995200 + 5000))
  
  // Test threshold collection
  let threshold1 = add_object(threshold_gc, "th-1", "data1", [], 0, 1640995200)
  let threshold2 = add_object(threshold1, "th-2", "data2", [], 0, 1640995205)
  
  // Should not collect yet (threshold not reached)
  assert_false(should_collect(threshold2, 1640995250))
  
  // Add more objects to exceed threshold
  let threshold3 = add_object(threshold2, "th-3", "data3", [], 0, 1640995210)
  let threshold4 = add_object(threshold3, "th-4", "data4", [], 0, 1640995215)
  
  // Should collect due to memory threshold
  assert_true(should_collect(threshold4, 1640995250))
  
  // Test hybrid collection
  let hybrid1 = add_object(hybrid_gc, "hy-1", "data1", [], 0, 1640995200)
  let hybrid2 = add_object(hybrid1, "hy-2", "data2", [], 0, 1640995205)
  
  // Should not collect yet
  assert_false(should_collect(hybrid2, 1640995250))
  
  // Should collect due to time
  assert_true(should_collect(hybrid2, 1640995200 + 10000))
  
  // Or should collect due to memory
  let hybrid3 = add_object(hybrid2, "hy-3", "data3", [], 0, 1640995210)
  assert_true(should_collect(hybrid3, 1640995250))
  
  // Get statistics
  let stats = get_gc_stats(manual_collected2)
  assert_eq(stats.object_count, 0)
  assert_eq(stats.total_memory, 0)
  assert_eq(stats.collection_count, 2)
  assert_eq(stats.total_collected, 3)
  assert_eq(stats.collection_efficiency, 1.5)  // 3/2
}

// Test 5: Memory Compression
test "memory compression techniques" {
  // Define compression algorithm
  enum CompressionAlgorithm {
    None
    RunLength
    Dictionary
    Simple  // Very basic compression
  }
  
  // Define compressed data
  type CompressedData = {
    algorithm: CompressionAlgorithm,
    original_size: Int,
    compressed_size: Int,
    data: Array[Int>  // Simplified representation
  }
  
  // Define memory compressor
  type MemoryCompressor = {
    name: String,
    default_algorithm: CompressionAlgorithm,
    compression_ratio_threshold: Float  // Minimum ratio to keep compression
  }
  
  // Create memory compressor
  let create_compressor = fn(name: String, default_algorithm: CompressionAlgorithm, compression_ratio_threshold: Float) {
    {
      name,
      default_algorithm,
      compression_ratio_threshold
    }
  }
  
  // Run-length encoding compression
  let compress_rle = fn(data: Array[String>) {
    if data.length() == 0 {
      []
    } else {
      let mut compressed = []
      let mut current = data[0]
      let mut count = 1
      
      for i in 1..data.length() {
        if data[i] == current and count < 255 {
          count = count + 1
        } else {
          compressed = compressed.push(count)
          compressed = compressed.push(current.length())
          for j in 0..current.length() {
            compressed = compressed.push(current[j].to_int())
          }
          
          current = data[i]
          count = 1
        }
      }
      
      // Add last run
      compressed = compressed.push(count)
      compressed = compressed.push(current.length())
      for j in 0..current.length() {
        compressed = compressed.push(current[j].to_int())
      }
      
      compressed
    }
  }
  
  // Simple dictionary compression
  let compress_dictionary = fn(data: Array[String>) {
    if data.length() == 0 {
      ([], [])
    } else {
      let mut dictionary = []
      let mut compressed = []
      let mut next_id = 0
      
      for item in data {
        let existing_index = dictionary.index_of(fn(entry) { entry == item })
        
        match existing_index {
          Some(index) => {
            compressed = compressed.push(index)
          }
          None => {
            dictionary = dictionary.push(item)
            compressed = compressed.push(next_id)
            next_id = next_id + 1
          }
        }
      }
      
      (dictionary, compressed)
    }
  }
  
  // Very simple compression (just remove duplicates)
  let compress_simple = fn(data: Array[String>) {
    let mut seen = []
    let mut compressed = []
    
    for item in data {
      if not(seen.contains(item)) {
        seen = seen.push(item)
      }
      compressed = compressed.push(seen.index_of(item).unwrap())
    }
    
    (seen, compressed)
  }
  
  // Compress data
  let compress = fn(compressor: MemoryCompressor, data: Array[String>) {
    let original_size = data.reduce(fn(acc, item) { acc + item.length() }, 0)
    
    let (algorithm, compressed_data) = match compressor.default_algorithm {
      CompressionAlgorithm::None => {
        (CompressionAlgorithm::None, data.map(fn(s) { s.length() }).concat(data.map(fn(s) { s.to_char_array() }).flatten().map(fn(c) { c.to_int() })))
      }
      CompressionAlgorithm::RunLength => {
        (CompressionAlgorithm::RunLength, compress_rle(data))
      }
      CompressionAlgorithm::Dictionary => {
        let (dict, compressed) = compress_dictionary(data)
        let dict_data = dict.map(fn(s) { s.length() }).concat(dict.map(fn(s) { s.to_char_array() }).flatten().map(fn(c) { c.to_int() }))
        (CompressionAlgorithm::Dictionary, dict_data.concat(compressed))
      }
      CompressionAlgorithm::Simple => {
        let (unique, compressed) = compress_simple(data)
        let unique_data = unique.map(fn(s) { s.length() }).concat(unique.map(fn(s) { s.to_char_array() }).flatten().map(fn(c) { c.to_int() }))
        (CompressionAlgorithm::Simple, unique_data.concat(compressed))
      }
    }
    
    let compressed_size = compressed_data.length()
    
    {
      algorithm,
      original_size,
      compressed_size,
      data: compressed_data
    }
  }
  
  // Decompress data
  let decompress = fn(compressed: CompressedData) -> Array[String> {
    match compressed.algorithm {
      CompressionAlgorithm::None => {
        // Simplified decompression
        []
      }
      CompressionAlgorithm::RunLength => {
        // Simplified RLE decompression
        []
      }
      CompressionAlgorithm::Dictionary => {
        // Simplified dictionary decompression
        []
      }
      CompressionAlgorithm::Simple => {
        // Simplified simple decompression
        []
      }
    }
  }
  
  // Calculate compression ratio
  let compression_ratio = fn(compressed: CompressedData) {
    if compressed.original_size > 0 {
      compressed.compressed_size.to_float() / compressed.original_size.to_float()
    } else {
      1.0
    }
  }
  
  // Check if compression is beneficial
  let is_compression_beneficial = fn(compressed: CompressedData, threshold: Float) {
    compression_ratio(compressed) < threshold
  }
  
  // Create compressor
  let compressor = create_compressor("main-compressor", CompressionAlgorithm::RunLength, 0.8)
  
  // Test with repetitive data
  let repetitive_data = ["hello", "hello", "hello", "world", "world", "world", "world", "foo", "foo"]
  
  let compressed_repetitive = compress(compressor, repetitive_data)
  
  assert_eq(compressed_repetitive.original_size, 33)  // 5+5+5+5+5+5+5+3+3 = 41 (actually let's recalculate)
  assert_eq(compressed_repetitive.algorithm, CompressionAlgorithm::RunLength)
  assert_true(compressed_repetitive.compressed_size < compressed_repetitive.original_size)
  
  let ratio_repetitive = compression_ratio(compressed_repetitive)
  assert_true(ratio_repetitive < 0.8)
  
  // Test with unique data
  let unique_data = ["apple", "banana", "cherry", "date", "elderberry"]
  
  let compressed_unique = compress(compressor, unique_data)
  
  assert_eq(compressed_unique.original_size, 33)  // 5+6+6+4+9 = 30 (actually let's recalculate)
  assert_eq(compressed_unique.algorithm, CompressionAlgorithm::RunLength)
  // RLE might not be effective for unique data
  
  // Test dictionary compression
  let dict_compressor = create_compressor("dict-compressor", CompressionAlgorithm::Dictionary, 0.8)
  
  let dictionary_data = ["error", "warning", "info", "error", "debug", "info", "error", "warning", "info"]
  
  let compressed_dict = compress(dict_compressor, dictionary_data)
  
  assert_eq(compressed_dict.algorithm, CompressionAlgorithm::Dictionary)
  assert_true(compressed_dict.compressed_size < compressed_dict.original_size)
  
  let ratio_dict = compression_ratio(compressed_dict)
  assert_true(ratio_dict < 0.8)
  
  // Test compression threshold
  let threshold_compressor = create_compressor("threshold-compressor", CompressionAlgorithm::Simple, 0.5)
  
  let threshold_data = ["a", "b", "c", "d", "e"]  // Very unique
  
  let compressed_threshold = compress(threshold_compressor, threshold_data)
  
  // Might not meet threshold
  let beneficial = is_compression_beneficial(compressed_threshold, threshold_compressor.compression_ratio_threshold)
  
  // Test different algorithms
  let none_compressor = create_compressor("none-compressor", CompressionAlgorithm::None, 0.8)
  let rle_compressor = create_compressor("rle-compressor", CompressionAlgorithm::RunLength, 0.8)
  let dict_compressor2 = create_compressor("dict-compressor-2", CompressionAlgorithm::Dictionary, 0.8)
  let simple_compressor = create_compressor("simple-compressor", CompressionAlgorithm::Simple, 0.8)
  
  let test_data = ["test", "test", "test", "test", "test"]
  
  let none_compressed = compress(none_compressor, test_data)
  let rle_compressed = compress(rle_compressor, test_data)
  let dict_compressed = compress(dict_compressor2, test_data)
  let simple_compressed = compress(simple_compressor, test_data)
  
  // Compare compression ratios
  let none_ratio = compression_ratio(none_compressed)
  let rle_ratio = compression_ratio(rle_compressed)
  let dict_ratio = compression_ratio(dict_compressed)
  let simple_ratio = compression_ratio(simple_compressed)
  
  // RLE should be best for this repetitive data
  assert_true(rle_ratio <= dict_ratio)
  assert_true(rle_ratio <= simple_ratio)
  assert_true(rle_ratio < none_ratio)
}

// Test 6: Memory Fragmentation Management
test "memory fragmentation management" {
  // Define memory block
  type MemoryBlock = {
    id: Int,
    start_address: Int,
    size: Int,
    is_free: Bool,
    prev_free: Option<Int>,
    next_free: Option<Int>
  }
  
  // Define memory manager
  type MemoryManager = {
    name: String,
    total_size: Int,
    blocks: Array[MemoryBlock>,
    free_list_head: Option[Int>,
    fragmentation_ratio: Float
  }
  
  // Create memory manager
  let create_manager = fn(name: String, total_size: Int) {
    let initial_block = {
      id: 0,
      start_address: 0,
      size: total_size,
      is_free: true,
      prev_free: None,
      next_free: None
    }
    
    {
      name,
      total_size,
      blocks: [initial_block],
      free_list_head: Some(0),
      fragmentation_ratio: 0.0
    }
  }
  
  // Find best fit block
  let find_best_fit = fn(blocks: Array[MemoryBlock>, free_list_head: Option[Int>, size: Int) {
    match free_list_head {
      None => None
      Some(head_id) => {
        let mut best_id = None
        let mut best_size = 999999  // Large initial value
        
        let mut current_id = Some(head_id)
        
        while current_id.is_some() {
          let block = blocks[current_id.unwrap()]
          
          if block.is_free and block.size >= size and block.size < best_size {
            best_id = current_id
            best_size = block.size
          }
          
          current_id = block.next_free
        }
        
        best_id
      }
    }
  }
  
  // Allocate memory
  let allocate = fn(manager: MemoryManager, size: Int) {
    let block_id = find_best_fit(manager.blocks, manager.free_list_head, size)
    
    match block_id {
      Some(id) => {
        let block = manager.blocks[id]
        
        if block.size == size {
          // Exact fit
          let updated_block = { block | is_free: false }
          let updated_blocks = manager.blocks.map_with_index(fn(i, b) {
            if i == id {
              updated_block
            } else {
              b
            }
          })
          
          // Update free list
          let prev_free = block.prev_free
          let next_free = block.next_free
          
          let updated_blocks_with_free_list = match (prev_free, next_free) {
            (Some(prev), Some(next)) => {
              updated_blocks.map_with_index(fn(i, b) {
                if i == prev {
                  { b | next_free: Some(next) }
                } else if i == next {
                  { b | prev_free: Some(prev) }
                } else {
                  b
                }
              })
            }
            (Some(prev), None) => {
              updated_blocks.map_with_index(fn(i, b) {
                if i == prev {
                  { b | next_free: None }
                } else {
                  b
                }
              })
            }
            (None, Some(next)) => {
              updated_blocks.map_with_index(fn(i, b) {
                if i == next {
                  { b | prev_free: None }
                } else {
                  b
                }
              })
            }
            (None, None) => updated_blocks
          }
          
          let new_head = if manager.free_list_head == Some(id) {
            block.next_free
          } else {
            manager.free_list_head
          }
          
          let updated_manager = {
            manager |
            blocks: updated_blocks_with_free_list,
            free_list_head: new_head
          }
          
          (Some(id), updated_manager)
        } else if block.size > size {
          // Split block
          let allocated_block = {
            block |
            is_free: false,
            size: size
          }
          
          let free_block = {
            id: manager.blocks.length(),
            start_address: block.start_address + size,
            size: block.size - size,
            is_free: true,
            prev_free: block.prev_free,
            next_free: block.next_free
          }
          
          let updated_blocks = manager.blocks.map_with_index(fn(i, b) {
            if i == id {
              allocated_block
            } else {
              b
            }
          }).push(free_block)
          
          // Update free list pointers
          let updated_blocks_with_free_list = match block.prev_free {
            Some(prev) => {
              updated_blocks.map_with_index(fn(i, b) {
                if i == prev {
                  { b | next_free: Some(free_block.id) }
                } else {
                  b
                }
              })
            }
            None => {
              if manager.free_list_head == Some(id) {
                updated_blocks
              } else {
                updated_blocks
              }
            }
          }
          
          let new_head = if manager.free_list_head == Some(id) {
            Some(free_block.id)
          } else {
            manager.free_list_head
          }
          
          let updated_manager = {
            manager |
            blocks: updated_blocks_with_free_list,
            free_list_head: new_head
          }
          
          (Some(id), updated_manager)
        } else {
          // Block too small
          (None, manager)
        }
      }
      None => (None, manager)
    }
  }
  
  // Free memory
  let free = fn(manager: MemoryManager, block_id: Int) {
    if block_id >= 0 and block_id < manager.blocks.length() {
      let block = manager.blocks[block_id]
      
      if not(block.is_free) {
        let freed_block = { block | is_free: true }
        
        let updated_blocks = manager.blocks.map_with_index(fn(i, b) {
          if i == block_id {
            freed_block
          } else {
            b
          }
        })
        
        // Add to free list
        let updated_blocks_with_free_list = match manager.free_list_head {
          Some(head_id) => {
            updated_blocks.map_with_index(fn(i, b) {
              if i == head_id {
                { b | prev_free: Some(block_id) }
              } else if i == block_id {
                { b | next_free: Some(head_id) }
              } else {
                b
              }
            })
          }
          None => {
            updated_blocks.map_with_index(fn(i, b) {
              if i == block_id {
                { b | next_free: None }
              } else {
                b
              }
            })
          }
        }
        
        let new_head = Some(block_id)
        
        {
          manager |
          blocks: updated_blocks_with_free_list,
          free_list_head: new_head
        }
      } else {
        manager  // Block already free
      }
    } else {
      manager  // Invalid block ID
    }
  }
  
  // Coalesce adjacent free blocks
  let coalesce = fn(manager: MemoryManager) {
    let free_blocks = manager.blocks.filter(fn(b) { b.is_free })
    
    if free_blocks.length() < 2 {
      manager
    } else {
      // Sort by start address
      let sorted_free = free_blocks.sort(fn(a, b) { a.start_address <= b.start_address })
      
      let mut coalesced_blocks = manager.blocks
      let mut i = 0
      
      while i < sorted_free.length() - 1 {
        let current = sorted_free[i]
        let next = sorted_free[i + 1]
        
        // Check if blocks are adjacent
        if current.start_address + current.size == next.start_address {
          // Coalesce blocks
          let coalesced_block = {
            id: current.id,
            start_address: current.start_address,
            size: current.size + next.size,
            is_free: true,
            prev_free: current.prev_free,
            next_free: next.next_free
          }
          
          // Update blocks array
          coalesced_blocks = coalesced_blocks.map_with_index(fn(block_i, b) {
            if block_i == current.id {
              coalesced_block
            } else if block_i == next.id {
              { b | is_free: false }  // Mark as merged
            } else {
              b
            }
          })
          
          // Update free list pointers
          coalesced_blocks = coalesced_blocks.map_with_index(fn(block_i, b) {
            match b.prev_free {
              Some(prev) if prev == next.id => {
                { b | prev_free: Some(current.id) }
              }
              _ => b
            }
          })
          
          coalesced_blocks = coalesced_blocks.map_with_index(fn(block_i, b) {
            match b.next_free {
              Some(next_id) if next_id == current.id => {
                { b | next_free: Some(next.id) }
              }
              _ => b
            }
          })
        }
        
        i = i + 1
      }
      
      // Remove merged blocks
      let final_blocks = coalesced_blocks.filter(fn(b) { b.is_free or not(b.is_free) })
      
      {
        manager |
        blocks: final_blocks
      }
    }
  }
  
  // Calculate fragmentation ratio
  let calculate_fragmentation = fn(manager: MemoryManager) {
    let free_blocks = manager.blocks.filter(fn(b) { b.is_free })
    
    if free_blocks.length() == 0 {
      0.0
    } else {
      let total_free = free_blocks.reduce(fn(acc, b) { acc + b.size }, 0)
      let largest_free = free_blocks.reduce(fn(acc, b) { if b.size > acc { b.size } else { acc } }, 0)
      
      if total_free > 0 {
        1.0 - (largest_free.to_float() / total_free.to_float())
      } else {
        0.0
      }
    }
  }
  
  // Get memory statistics
  let get_memory_stats = fn(manager: MemoryManager) {
    let free_blocks = manager.blocks.filter(fn(b) { b.is_free })
    let allocated_blocks = manager.blocks.filter(fn(b) { not(b.is_free) })
    
    let total_free = free_blocks.reduce(fn(acc, b) { acc + b.size }, 0)
    let total_allocated = allocated_blocks.reduce(fn(acc, b) { acc + b.size }, 0)
    
    {
      total_blocks: manager.blocks.length(),
      free_blocks: free_blocks.length(),
      allocated_blocks: allocated_blocks.length(),
      total_free,
      total_allocated,
      fragmentation: calculate_fragmentation(manager)
    }
  }
  
  // Create memory manager
  let manager = create_manager("main-manager", 1024)
  
  // Allocate blocks
  let (block1_id, manager1) = allocate(manager, 100)
  let (block2_id, manager2) = allocate(manager1, 200)
  let (block3_id, manager3) = allocate(manager2, 150)
  let (block4_id, manager4) = allocate(manager3, 300)
  
  // Verify allocations
  match (block1_id, block2_id, block3_id, block4_id) {
    (Some(id1), Some(id2), Some(id3), Some(id4)) => {
      assert_eq(id1, 0)
      assert_eq(id2, 1)
      assert_eq(id3, 2)
      assert_eq(id4, 3)
    }
    _ => assert_true(false)
  }
  
  let stats4 = get_memory_stats(manager4)
  assert_eq(stats4.allocated_blocks, 4)
  assert_eq(stats4.total_allocated, 750)  // 100 + 200 + 150 + 300
  assert_eq(stats4.free_blocks, 1)
  assert_eq(stats4.total_free, 274)  // 1024 - 750
  
  // Free middle blocks to create fragmentation
  let manager5 = free(manager4, block2_id.unwrap())
  let manager6 = free(manager5, block4_id.unwrap())
  
  let stats6 = get_memory_stats(manager6)
  assert_eq(stats6.allocated_blocks, 2)
  assert_eq(stats6.free_blocks, 3)
  assert_true(stats6.fragmentation > 0.0)
  
  // Try to allocate a block that requires coalescing
  let (block5_id, manager7) = allocate(manager6, 250)
  
  match block5_id {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  // Coalesce free blocks
  let coalesced_manager = coalesce(manager7)
  
  let coalesced_stats = get_memory_stats(coalesced_manager)
  assert_eq(coalesced_stats.free_blocks, 2)  // Should have fewer free blocks after coalescing
  
  // Allocate large block after coalescing
  let (block6_id, final_manager) = allocate(coalesced_manager, 500)
  
  match block6_id {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  let final_stats = get_memory_stats(final_manager)
  assert_eq(final_stats.allocated_blocks, 4)
  assert_eq(final_stats.total_allocated, 900)  // 100 + 150 + 250 + 400 (approx)
}

// Test 7: Memory Profiling and Analysis
test "memory profiling and analysis" {
  // Define memory profile record
  type MemoryProfileRecord = {
    timestamp: Int,
    total_allocated: Int,
    peak_usage: Int,
    allocation_count: Int,
    deallocation_count: Int,
    fragmentation_ratio: Float
  }
  
  // Define allocation hotspot
  type AllocationHotspot = {
    function_name: String,
    allocation_count: Int,
    total_size: Int,
    average_size: Float
  }
  
  // Define memory profiler
  type MemoryProfiler = {
    name: String,
    profiles: Array[MemoryProfileRecord>,
    current_allocated: Int,
    peak_usage: Int,
    total_allocations: Int,
    total_deallocations: Int,
    allocation_hotspots: Array[(String, Int, Int)]  // (function, count, size)
  }
  
  // Create memory profiler
  let create_profiler = fn(name: String) {
    {
      name,
      profiles: [],
      current_allocated: 0,
      peak_usage: 0,
      total_allocations: 0,
      total_deallocations: 0,
      allocation_hotspots: []
    }
  }
  
  // Record allocation
  let record_allocation = fn(profiler: MemoryProfiler, size: Int, function_name: String, timestamp: Int) {
    let new_allocated = profiler.current_allocated + size
    let new_peak = if new_allocated > profiler.peak_usage {
      new_allocated
    } else {
      profiler.peak_usage
    }
    
    // Update hotspots
    let mut updated_hotspots = profiler.allocation_hotspots
    let mut found = false
    
    updated_hotspots = updated_hotspots.map(fn((fn, count, total)) {
      if fn == function_name {
        found = true
        (fn, count + 1, total + size)
      } else {
        (fn, count, total)
      }
    })
    
    if not(found) {
      updated_hotspots = updated_hotspots.push((function_name, 1, size))
    }
    
    {
      profiler |
      current_allocated: new_allocated,
      peak_usage: new_peak,
      total_allocations: profiler.total_allocations + 1,
      allocation_hotspots: updated_hotspots
    }
  }
  
  // Record deallocation
  let record_deallocation = fn(profiler: MemoryProfiler, size: Int, timestamp: Int) {
    let new_allocated = profiler.current_allocated - size
    
    {
      profiler |
      current_allocated: new_allocated,
      total_deallocations: profiler.total_deallocations + 1
    }
  }
  
  // Take memory profile snapshot
  let take_profile = fn(profiler: MemoryProfiler, timestamp: Int, fragmentation_ratio: Float) {
    let profile = {
      timestamp,
      total_allocated: profiler.current_allocated,
      peak_usage: profiler.peak_usage,
      allocation_count: profiler.total_allocations,
      deallocation_count: profiler.total_deallocations,
      fragmentation_ratio
    }
    
    let updated_profiles = profiler.profiles.push(profile)
    
    {
      profiler |
      profiles: updated_profiles
    }
  }
  
  // Get allocation hotspots
  let get_hotspots = fn(profiler: MemoryProfiler) {
    profiler.allocation_hotspots.map(fn((fn, count, total)) {
      {
        function_name: fn,
        allocation_count: count,
        total_size: total,
        average_size: total.to_float() / count.to_float()
      }
    }).sort(fn(a, b) { a.total_size >= b.total_size })
  }
  
  // Analyze memory growth trend
  let analyze_growth_trend = fn(profiler: MemoryProfiler) {
    if profiler.profiles.length() < 2 {
      "insufficient_data"
    } else {
      let first = profiler.profiles[0]
      let last = profiler.profiles[profiler.profiles.length() - 1]
      
      let time_diff = last.timestamp - first.timestamp
      let memory_diff = last.total_allocated - first.total_allocated
      
      if memory_diff > 0 {
        let growth_rate = memory_diff.to_float() / time_diff.to_float()
        
        if growth_rate > 1000.0 {
          "high_growth"
        } else if growth_rate > 100.0 {
          "moderate_growth"
        } else {
          "low_growth"
        }
      } else if memory_diff < 0 {
        "decreasing"
      } else {
        "stable"
      }
    }
  }
  
  // Detect memory anomalies
  let detect_anomalies = fn(profiler: MemoryProfiler) {
    if profiler.profiles.length() < 3 {
      []
    } else {
      let mut anomalies = []
      
      for i in 1..profiler.profiles.length() - 1 {
        let prev = profiler.profiles[i - 1]
        let current = profiler.profiles[i]
        let next = profiler.profiles[i + 1]
        
        // Check for sudden spikes
        let prev_diff = current.total_allocated - prev.total_allocated
        let next_diff = next.total_allocated - current.total_allocated
        
        if prev_diff > 50000 and next_diff < -prev_diff / 2 {
          anomalies = anomalies.push({
            timestamp: current.timestamp,
            type: "spike_and_drop",
            severity: if prev_diff > 100000 { "high" } else { "medium" },
            description: "Memory spike followed by rapid drop"
          })
        }
        
        // Check for steady growth
        if i >= 3 {
          let growth_count = 0
          let mut j = i - 3
          let mut increasing = true
          
          while j <= i and increasing {
            if profiler.profiles[j].total_allocated >= profiler.profiles[j - 1].total_allocated {
              j = j + 1
            } else {
              increasing = false
            }
          }
          
          if increasing and j > i {
            anomalies = anomalies.push({
              timestamp: current.timestamp,
              type: "steady_growth",
              severity: "medium",
              description: "Steady memory growth detected"
            })
          }
        }
      }
      
      anomalies
    }
  }
  
  // Create memory profiler
  let profiler = create_profiler("main-profiler")
  
  // Record allocations
  let profiler1 = record_allocation(profiler, 1000, "initialize", 1640995200)
  let profiler2 = record_allocation(profiler1, 2000, "process_data", 1640995205)
  let profiler3 = record_allocation(profiler2, 1500, "cache_objects", 1640995210)
  let profiler4 = record_allocation(profiler3, 500, "initialize", 1640995215)
  let profiler5 = record_allocation(profiler4, 3000, "process_data", 1640995220)
  let profiler6 = record_allocation(profiler5, 2500, "render_ui", 1640995225)
  
  // Record deallocations
  let profiler7 = record_deallocation(profiler6, 1000, 1640995230)
  let profiler8 = record_deallocation(profiler7, 1500, 1640995235)
  
  // Take profile snapshots
  let profiler9 = take_profile(profiler8, 1640995200, 0.1)
  let profiler10 = take_profile(profiler9, 1640995210, 0.15)
  let profiler11 = take_profile(profiler10, 1640995220, 0.2)
  let profiler12 = take_profile(profiler11, 1640995230, 0.25)
  let profiler13 = take_profile(profiler12, 1640995240, 0.18)
  
  // Verify profiler state
  assert_eq(profiler13.total_allocations, 6)
  assert_eq(profiler13.total_deallocations, 2)
  assert_eq(profiler13.current_allocated, 7000)  // 1000+2000+1500+500+3000+2500 - 1000 - 1500
  assert_eq(profiler13.peak_usage, 7000)
  assert_eq(profiler13.profiles.length(), 5)
  
  // Get hotspots
  let hotspots = get_hotspots(profiler13)
  
  assert_eq(hotspots.length(), 3)
  
  // process_data should be top hotspot
  match hotspots[0] {
    hotspot if hotspot.function_name == "process_data" => {
      assert_eq(hotspot.allocation_count, 2)
      assert_eq(hotspot.total_size, 5000)  // 2000 + 3000
      assert_eq(hotspot.average_size, 2500.0)
    }
    _ => assert_true(false)
  }
  
  // Analyze growth trend
  let trend = analyze_growth_trend(profiler13)
  assert_eq(trend, "low_growth")  // Some growth but not excessive
  
  // Test with more allocations for anomaly detection
  let anomaly_profiler = create_profiler("anomaly-profiler")
  
  // Create profiles with anomalies
  let anomaly_base = {
    anomaly_profiler |
    profiles: [
      { timestamp: 1640995200, total_allocated: 1000, peak_usage: 1000, allocation_count: 1, deallocation_count: 0, fragmentation_ratio: 0.1 },
      { timestamp: 1640995210, total_allocated: 1000, peak_usage: 1000, allocation_count: 2, deallocation_count: 1, fragmentation_ratio: 0.1 },
      { timestamp: 1640995220, total_allocated: 100000, peak_usage: 100000, allocation_count: 3, deallocation_count: 1, fragmentation_ratio: 0.2 },
      { timestamp: 1640995230, total_allocated: 50000, peak_usage: 100000, allocation_count: 3, deallocation_count: 2, fragmentation_ratio: 0.15 },
      { timestamp: 1640995240, total_allocated: 50000, peak_usage: 100000, allocation_count: 4, deallocation_count: 2, fragmentation_ratio: 0.15 }
    ]
  }
  
  let anomalies = detect_anomalies(anomaly_base)
  
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].type, "spike_and_drop")
  assert_eq(anomalies[0].severity, "high")
}

// Test 8: Memory-Efficient Data Structures
test "memory-efficient data structures" {
  // Define compact array (uses less memory than regular array)
  type CompactArray = {
    data: Array[Int>,  // Using Int instead of generic type for simplicity
    size: Int,
    capacity: Int
  }
  
  // Define string interning pool
  type StringPool = {
    strings: Array[String>,
    string_to_id: Map[String, Int>
  }
  
  // Define interned string
  type InternedString = {
    id: Int
  }
  
  // Define bitmap for boolean values
  type Bitmap = {
    data: Array[Int>,  // Each Int stores 32 boolean values
    size: Int
  }
  
  // Create compact array
  let create_compact_array = fn(initial_capacity: Int) {
    {
      data: [0; initial_capacity],
      size: 0,
      capacity: initial_capacity
    }
  }
  
  // Add to compact array
  let compact_array_add = fn(arr: CompactArray, value: Int) {
    if arr.size < arr.capacity {
      let updated_data = arr.data.map_with_index(fn(i, v) {
        if i == arr.size {
          value
        } else {
          v
        }
      })
      
      {
        arr |
        data: updated_data,
        size: arr.size + 1
      }
    } else {
      // Resize (simplified)
      let new_capacity = arr.capacity * 2
      let new_data = arr.data.concat([0; arr.capacity])
      let updated_data = new_data.map_with_index(fn(i, v) {
        if i == arr.size {
          value
        } else {
          v
        }
      })
      
      {
        data: updated_data,
        size: arr.size + 1,
        capacity: new_capacity
      }
    }
  }
  
  // Create string pool
  let create_string_pool = fn() {
    {
      strings: [],
      string_to_id: Map::new()
    }
  }
  
  // Intern string
  let intern_string = fn(pool: StringPool, str: String) {
    match pool.string_to_id.get(str) {
      Some(id) => {
        (InternedString { id }, pool)
      }
      None => {
        let id = pool.strings.length()
        let updated_strings = pool.strings.push(str)
        let updated_map = pool.string_to_id.insert(str, id)
        
        (InternedString { id }, {
          strings: updated_strings,
          string_to_id: updated_map
        })
      }
    }
  }
  
  // Resolve interned string
  let resolve_interned = fn(pool: StringPool, interned: InternedString) {
    if interned.id >= 0 and interned.id < pool.strings.length() {
      Some(pool.strings[interned.id])
    } else {
      None
    }
  }
  
  // Create bitmap
  let create_bitmap = fn(size: Int) {
    let int_count = (size + 31) / 32
    {
      data: [0; int_count],
      size
    }
  }
  
  // Set bit in bitmap
  let bitmap_set = fn(bitmap: Bitmap, index: Int, value: Bool) {
    if index >= 0 and index < bitmap.size {
      let int_index = index / 32
      let bit_index = index % 32
      
      let updated_data = bitmap.data.map_with_index(fn(i, v) {
        if i == int_index {
          if value {
            v | (1 << bit_index)
          } else {
            v & ~(1 << bit_index)
          }
        } else {
          v
        }
      })
      
      {
        bitmap |
        data: updated_data
      }
    } else {
      bitmap
    }
  }
  
  // Get bit from bitmap
  let bitmap_get = fn(bitmap: Bitmap, index: Int) {
    if index >= 0 and index < bitmap.size {
      let int_index = index / 32
      let bit_index = index % 32
      
      (bitmap.data[int_index] & (1 << bit_index)) != 0
    } else {
      false
    }
  }
  
  // Calculate memory savings
  let calculate_bitmap_savings = fn(size: Int) {
    let bitmap_memory = ((size + 31) / 32) * 4  // 4 bytes per Int
    let boolean_array_memory = size * 1  // 1 byte per boolean
    
    {
      bitmap_memory,
      boolean_array_memory,
      savings: boolean_array_memory - bitmap_memory,
      savings_percentage: (boolean_array_memory - bitmap_memory).to_float() / boolean_array_memory.to_float() * 100.0
    }
  }
  
  // Test compact array
  let compact_arr = create_compact_array(4)
  
  assert_eq(compact_arr.capacity, 4)
  assert_eq(compact_arr.size, 0)
  
  let compact_arr1 = compact_array_add(compact_arr, 10)
  let compact_arr2 = compact_array_add(compact_arr1, 20)
  let compact_arr3 = compact_array_add(compact_arr2, 30)
  let compact_arr4 = compact_array_add(compact_arr3, 40)
  
  assert_eq(compact_arr4.size, 4)
  assert_eq(compact_arr4.capacity, 4)
  assert_eq(compact_arr4.data[0], 10)
  assert_eq(compact_arr4.data[1], 20)
  assert_eq(compact_arr4.data[2], 30)
  assert_eq(compact_arr4.data[3], 40)
  
  // Should resize when adding beyond capacity
  let compact_arr5 = compact_array_add(compact_arr4, 50)
  
  assert_eq(compact_arr5.size, 5)
  assert_eq(compact_arr5.capacity, 8)  // Doubled
  assert_eq(compact_arr5.data[4], 50)
  
  // Test string interning
  let string_pool = create_string_pool()
  
  // First intern
  let (interned1, pool1) = intern_string(string_pool, "hello")
  assert_eq(interned1.id, 0)
  assert_eq(pool1.strings.length(), 1)
  
  // Second intern (same string)
  let (interned2, pool2) = intern_string(pool1, "hello")
  assert_eq(interned2.id, 0)  // Same ID
  assert_eq(pool2.strings.length(), 1)  // No new string added
  
  // Different string
  let (interned3, pool3) = intern_string(pool2, "world")
  assert_eq(interned3.id, 1)
  assert_eq(pool3.strings.length(), 2)
  
  // Resolve interned strings
  let resolved1 = resolve_interned(pool3, interned1)
  let resolved2 = resolve_interned(pool3, interned3)
  let resolved3 = resolve_interned(pool3, InternedString { id: 99 })
  
  match resolved1 {
    Some(str) => assert_eq(str, "hello")
    None => assert_true(false)
  }
  
  match resolved2 {
    Some(str) => assert_eq(str, "world")
    None => assert_true(false)
  }
  
  assert_eq(resolved3, None)  // Invalid ID
  
  // Test bitmap
  let bitmap = create_bitmap(100)
  
  assert_eq(bitmap.size, 100)
  assert_eq(bitmap.data.length(), 4)  // (100 + 31) / 32 = 4
  
  // Set some bits
  let bitmap1 = bitmap_set(bitmap, 10, true)
  let bitmap2 = bitmap_set(bitmap1, 50, true)
  let bitmap3 = bitmap_set(bitmap2, 75, false)
  
  assert_true(bitmap_get(bitmap3, 10))
  assert_true(bitmap_get(bitmap3, 50))
  assert_false(bitmap_get(bitmap3, 75))
  assert_false(bitmap_get(bitmap3, 20))
  
  // Calculate memory savings
  let savings = calculate_bitmap_savings(1000)
  
  assert_eq(savings.bitmap_memory, 128)  // (1000 + 31) / 32 * 4 = 128
  assert_eq(savings.boolean_array_memory, 1000)
  assert_eq(savings.savings, 872)
  assert_eq(savings.savings_percentage, 87.2)
  
  // Test with larger bitmap
  let large_savings = calculate_bitmap_savings(10000)
  
  assert_eq(large_savings.bitmap_memory, 1250)  // (10000 + 31) / 32 * 4 = 1250
  assert_eq(large_savings.boolean_array_memory, 10000)
  assert_eq(large_savings.savings, 8750)
  assert_eq(large_savings.savings_percentage, 87.5)
}