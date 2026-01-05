// Azimuth Memory Management Comprehensive Test Suite
// This file contains comprehensive test cases for memory management functionality

// Test 1: Memory Allocation and Deallocation
test "memory allocation and deallocation for telemetry objects" {
  // Define memory management types
  type MemoryBlock = {
    id: String,
    size: Int,
    allocated_at: Int,
    freed_at: Option[Int],
    in_use: Bool,
    data: Option[String]
  }
  
  type MemoryManager = {
    blocks: Array[MemoryBlock],
    total_allocated: Int,
    total_freed: Int,
    peak_usage: Int,
    allocation_strategy: String
  }
  
  // Memory management operations
  let create_memory_block = fn(id: String, size: Int, current_time: Int) {
    {
      id,
      size,
      allocated_at: current_time,
      freed_at: None,
      in_use: true,
      data: Some("x".repeat(size))
    }
  }
  
  let create_memory_manager = fn(allocation_strategy: String) {
    {
      blocks: [],
      total_allocated: 0,
      total_freed: 0,
      peak_usage: 0,
      allocation_strategy
    }
  }
  
  let allocate_memory = fn(manager: MemoryManager, size: Int, current_time: Int) {
    let block_id = "block-" + current_time.to_string() + "-" + manager.blocks.length().to_string()
    let block = create_memory_block(block_id, size, current_time)
    
    let new_blocks = manager.blocks.push(block)
    let new_total_allocated = manager.total_allocated + size
    let new_peak_usage = if new_total_allocated > manager.peak_usage {
      new_total_allocated
    } else {
      manager.peak_usage
    }
    
    (block, {
      blocks: new_blocks,
      total_allocated: new_total_allocated,
      total_freed: manager.total_freed,
      peak_usage: new_peak_usage,
      allocation_strategy: manager.allocation_strategy
    })
  }
  
  let deallocate_memory = fn(manager: MemoryManager, block_id: String, current_time: Int) {
    let mut updated_blocks = manager.blocks
    let mut freed_size = 0
    
    for i in 0..manager.blocks.length() {
      let block = manager.blocks[i]
      
      if block.id == block_id and block.in_use {
        freed_size = block.size
        
        updated_blocks = updated_blocks.set(i, {
          id: block.id,
          size: block.size,
          allocated_at: block.allocated_at,
          freed_at: Some(current_time),
          in_use: false,
          data: None
        })
        break
      }
    }
    
    {
      blocks: updated_blocks,
      total_allocated: manager.total_allocated,
      total_freed: manager.total_freed + freed_size,
      peak_usage: manager.peak_usage,
      allocation_strategy: manager.allocation_strategy
    }
  }
  
  let get_memory_usage = fn(manager: MemoryManager) {
    let current_usage = manager.total_allocated - manager.total_freed
    let active_blocks = manager.blocks.fold(0, fn(acc, block) {
      if block.in_use { acc + 1 } else { acc }
    })
    
    {
      current_usage,
      peak_usage: manager.peak_usage,
      total_allocated: manager.total_allocated,
      total_freed: manager.total_freed,
      active_blocks,
      total_blocks: manager.blocks.length()
    }
  }
  
  // Test memory manager creation
  let manager = create_memory_manager("first-fit")
  assert_eq(manager.allocation_strategy, "first-fit")
  assert_eq(manager.total_allocated, 0)
  assert_eq(manager.total_freed, 0)
  assert_eq(manager.peak_usage, 0)
  assert_eq(manager.blocks.length(), 0)
  
  // Test memory allocation
  let current_time = 1640995200
  let (block1, manager1) = allocate_memory(manager, 1024, current_time)
  assert_eq(block1.size, 1024)
  assert_true(block1.in_use)
  assert_eq(manager1.total_allocated, 1024)
  assert_eq(manager1.peak_usage, 1024)
  
  let (block2, manager2) = allocate_memory(manager1, 2048, current_time + 1)
  assert_eq(block2.size, 2048)
  assert_eq(manager2.total_allocated, 3072) // 1024 + 2048
  assert_eq(manager2.peak_usage, 3072)
  
  // Test memory deallocation
  let manager3 = deallocate_memory(manager2, block1.id, current_time + 10)
  assert_eq(manager3.total_freed, 1024)
  
  let block1_freed = manager3.blocks.find(fn(b) { b.id == block1.id }).unwrap()
  assert_false(block1_freed.in_use)
  assert_eq(block1_freed.freed_at, Some(current_time + 10))
  assert_eq(block1_freed.data, None)
  
  // Test memory usage calculation
  let usage = get_memory_usage(manager3)
  assert_eq(usage.current_usage, 2048) // 3072 - 1024
  assert_eq(usage.peak_usage, 3072)
  assert_eq(usage.total_allocated, 3072)
  assert_eq(usage.total_freed, 1024)
  assert_eq(usage.active_blocks, 1) // Only block2 is still active
  assert_eq(usage.total_blocks, 2)
}

// Test 2: Memory Pool Management
test "memory pool management for telemetry data" {
  // Define memory pool types
  type MemoryPool = {
    name: String,
    block_size: Int,
    total_blocks: Int,
    free_blocks: Array[Int], // Indices of free blocks
    used_blocks: Array[Int], // Indices of used blocks
    blocks: Array[MemoryBlock]
  }
  
  type MemoryPoolManager = {
    pools: Array[MemoryPool],
    total_memory: Int,
    used_memory: Int
  }
  
  // Memory pool operations
  let create_memory_pool = fn(name: String, block_size: Int, block_count: Int, current_time: Int) {
    let mut blocks = []
    let mut free_blocks = []
    
    for i in 0..block_count {
      let block = create_memory_block(name + "-block-" + i.to_string(), block_size, current_time)
      blocks = blocks.push(block)
      free_blocks = free_blocks.push(i)
    }
    
    {
      name,
      block_size,
      total_blocks: block_count,
      free_blocks,
      used_blocks: [],
      blocks
    }
  }
  
  let create_pool_manager = fn() {
    {
      pools: [],
      total_memory: 0,
      used_memory: 0
    }
  }
  
  let add_pool = fn(manager: MemoryPoolManager, pool: MemoryPool) {
    let new_total_memory = manager.total_memory + (pool.block_size * pool.total_blocks)
    
    {
      pools: manager.pools.push(pool),
      total_memory: new_total_memory,
      used_memory: manager.used_memory
    }
  }
  
  let allocate_from_pool = fn(manager: MemoryPoolManager, pool_name: String) {
    let mut pool_index = -1
    
    for i in 0..manager.pools.length() {
      if manager.pools[i].name == pool_name {
        pool_index = i
        break
      }
    }
    
    if pool_index >= 0 and manager.pools[pool_index].free_blocks.length() > 0 {
      let pool = manager.pools[pool_index]
      let block_index = pool.free_blocks[0]
      
      let new_free_blocks = pool.free_blocks.slice(1, pool.free_blocks.length())
      let new_used_blocks = pool.used_blocks.push(block_index)
      
      let updated_pool = {
        name: pool.name,
        block_size: pool.block_size,
        total_blocks: pool.total_blocks,
        free_blocks: new_free_blocks,
        used_blocks: new_used_blocks,
        blocks: pool.blocks
      }
      
      let updated_pools = manager.pools.set(pool_index, updated_pool)
      let new_used_memory = manager.used_memory + pool.block_size
      
      (Some(pool.blocks[block_index]), {
        pools: updated_pools,
        total_memory: manager.total_memory,
        used_memory: new_used_memory
      })
    } else {
      (None, manager)
    }
  }
  
  let deallocate_to_pool = fn(manager: MemoryPoolManager, pool_name: String, block_id: String) {
    let mut pool_index = -1
    let mut block_index = -1
    
    for i in 0..manager.pools.length() {
      if manager.pools[i].name == pool_name {
        pool_index = i
        
        for j in 0..manager.pools[i].used_blocks.length() {
          let used_block_index = manager.pools[i].used_blocks[j]
          if manager.pools[i].blocks[used_block_index].id == block_id {
            block_index = used_block_index
            break
          }
        }
        break
      }
    }
    
    if pool_index >= 0 and block_index >= 0 {
      let pool = manager.pools[pool_index]
      
      // Remove from used blocks
      let mut new_used_blocks = []
      for i in 0..pool.used_blocks.length() {
        if pool.used_blocks[i] != block_index {
          new_used_blocks = new_used_blocks.push(pool.used_blocks[i])
        }
      }
      
      // Add to free blocks
      let new_free_blocks = pool.free_blocks.push(block_index)
      
      let updated_pool = {
        name: pool.name,
        block_size: pool.block_size,
        total_blocks: pool.total_blocks,
        free_blocks: new_free_blocks,
        used_blocks: new_used_blocks,
        blocks: pool.blocks
      }
      
      let updated_pools = manager.pools.set(pool_index, updated_pool)
      let new_used_memory = manager.used_memory - pool.block_size
      
      {
        pools: updated_pools,
        total_memory: manager.total_memory,
        used_memory: new_used_memory
      }
    } else {
      manager
    }
  }
  
  // Test pool manager creation
  let manager = create_pool_manager()
  assert_eq(manager.pools.length(), 0)
  assert_eq(manager.total_memory, 0)
  assert_eq(manager.used_memory, 0)
  
  // Test adding pools
  let current_time = 1640995200
  let pool1 = create_memory_pool("small-pool", 1024, 10, current_time)
  let pool2 = create_memory_pool("large-pool", 4096, 5, current_time)
  
  let manager1 = add_pool(manager, pool1)
  let manager2 = add_pool(manager1, pool2)
  
  assert_eq(manager2.pools.length(), 2)
  assert_eq(manager2.total_memory, 1024 * 10 + 4096 * 5) // 10KB + 20KB = 30KB
  assert_eq(manager2.used_memory, 0)
  
  // Test allocation from pool
  let (block1, manager3) = allocate_from_pool(manager2, "small-pool")
  assert_true(block1.is_some())
  assert_eq(manager3.used_memory, 1024)
  assert_eq(manager3.pools[0].free_blocks.length(), 9)
  assert_eq(manager3.pools[0].used_blocks.length(), 1)
  
  let (block2, manager4) = allocate_from_pool(manager3, "large-pool")
  assert_true(block2.is_some())
  assert_eq(manager4.used_memory, 1024 + 4096)
  assert_eq(manager4.pools[1].free_blocks.length(), 4)
  assert_eq(manager4.pools[1].used_blocks.length(), 1)
  
  // Test deallocation to pool
  let manager5 = deallocate_to_pool(manager4, "small-pool", block1.unwrap().id)
  assert_eq(manager5.used_memory, 4096) // Only large block is still allocated
  assert_eq(manager5.pools[0].free_blocks.length(), 10) // Back to original
  assert_eq(manager5.pools[0].used_blocks.length(), 0)
  
  // Test allocation from exhausted pool
  let mut manager_exhausted = manager5
  let mut allocated_blocks = []
  
  // Allocate all large blocks
  for i in 0..5 {
    let (block, new_manager) = allocate_from_pool(manager_exhausted, "large-pool")
    assert_true(block.is_some())
    allocated_blocks = allocated_blocks.push(block.unwrap())
    manager_exhausted = new_manager
  }
  
  assert_eq(manager_exhausted.pools[1].free_blocks.length(), 0)
  assert_eq(manager_exhausted.pools[1].used_blocks.length(), 5)
  
  // Try to allocate one more (should fail)
  let (no_block, _) = allocate_from_pool(manager_exhausted, "large-pool")
  assert_eq(no_block, None)
}

// Test 3: Garbage Collection
test "garbage collection for telemetry objects" {
  // Define garbage collection types
  type GCObject = {
    id: String,
    size: Int,
    created_at: Int,
    last_accessed: Int,
    references: Int, // Reference count
    marked: Bool     // Marked for collection
  }
  
  type GCStats = {
    objects_collected: Int,
    memory_freed: Int,
    collection_time_ms: Int,
    objects_before: Int,
    objects_after: Int
  }
  
  type GarbageCollector = {
    objects: Array[GCObject],
    collection_threshold: Int,
    last_collection: Int,
    stats: GCStats
  }
  
  // Garbage collection operations
  let create_gc_object = fn(id: String, size: Int, current_time: Int) {
    {
      id,
      size,
      created_at: current_time,
      last_accessed: current_time,
      references: 1, // Initially referenced by creator
      marked: false
    }
  }
  
  let create_garbage_collector = fn(collection_threshold: Int) {
    {
      objects: [],
      collection_threshold,
      last_collection: 0,
      stats: {
        objects_collected: 0,
        memory_freed: 0,
        collection_time_ms: 0,
        objects_before: 0,
        objects_after: 0
      }
    }
  }
  
  let add_object = fn(gc: GarbageCollector, object: GCObject) {
    { gc | objects: gc.objects.push(object) }
  }
  
  let add_reference = fn(gc: GarbageCollector, object_id: String) {
    let mut updated_objects = gc.objects
    
    for i in 0..gc.objects.length() {
      let obj = gc.objects[i]
      if obj.id == object_id {
        updated_objects = updated_objects.set(i, {
          id: obj.id,
          size: obj.size,
          created_at: obj.created_at,
          last_accessed: obj.last_accessed,
          references: obj.references + 1,
          marked: obj.marked
        })
        break
      }
    }
    
    { gc | objects: updated_objects }
  }
  
  let remove_reference = fn(gc: GarbageCollector, object_id: String, current_time: Int) {
    let mut updated_objects = gc.objects
    
    for i in 0..gc.objects.length() {
      let obj = gc.objects[i]
      if obj.id == object_id {
        updated_objects = updated_objects.set(i, {
          id: obj.id,
          size: obj.size,
          created_at: obj.created_at,
          last_accessed: current_time,
          references: obj.references - 1,
          marked: obj.marked
        })
        break
      }
    }
    
    { gc | objects: updated_objects }
  }
  
  let mark_phase = fn(gc: GarbageCollector, root_objects: Array[String]) {
    let mut updated_objects = gc.objects
    
    // Reset all marks
    for i in 0..updated_objects.length() {
      let obj = updated_objects[i]
      updated_objects = updated_objects.set(i, {
        id: obj.id,
        size: obj.size,
        created_at: obj.created_at,
        last_accessed: obj.last_accessed,
        references: obj.references,
        marked: false
      })
    }
    
    // Mark reachable objects
    let mut to_mark = root_objects
    
    while to_mark.length() > 0 {
      let current_id = to_mark[0]
      to_mark = to_mark.slice(1, to_mark.length())
      
      for i in 0..updated_objects.length() {
        let obj = updated_objects[i]
        if obj.id == current_id and not(obj.marked) {
          updated_objects = updated_objects.set(i, {
            id: obj.id,
            size: obj.size,
            created_at: obj.created_at,
            last_accessed: obj.last_accessed,
            references: obj.references,
            marked: true
          })
          
          // In a real implementation, we would add referenced objects to to_mark
          // For this test, we'll just mark the object itself
        }
      }
    }
    
    { gc | objects: updated_objects }
  }
  
  let sweep_phase = fn(gc: GarbageCollector) {
    let mut remaining_objects = []
    let mut objects_collected = 0
    let mut memory_freed = 0
    
    for obj in gc.objects {
      if obj.marked or obj.references > 0 {
        remaining_objects = remaining_objects.push(obj)
      } else {
        objects_collected = objects_collected + 1
        memory_freed = memory_freed + obj.size
      }
    }
    
    {
      objects: remaining_objects,
      collection_threshold: gc.collection_threshold,
      last_collection: gc.last_collection,
      stats: {
        objects_collected,
        memory_freed,
        collection_time_ms: gc.stats.collection_time_ms,
        objects_before: gc.objects.length(),
        objects_after: remaining_objects.length()
      }
    }
  }
  
  let run_garbage_collection = fn(gc: GarbageCollector, root_objects: Array[String], current_time: Int) {
    let objects_before = gc.objects.length()
    let start_time = current_time
    
    let marked_gc = mark_phase(gc, root_objects)
    let collected_gc = sweep_phase(marked_gc)
    
    let end_time = start_time + 10 // Mock 10ms collection time
    
    {
      objects: collected_gc.objects,
      collection_threshold: collected_gc.collection_threshold,
      last_collection: end_time,
      stats: {
        objects_collected: collected_gc.stats.objects_collected,
        memory_freed: collected_gc.stats.memory_freed,
        collection_time_ms: end_time - start_time,
        objects_before,
        objects_after: collected_gc.objects.length()
      }
    }
  }
  
  // Test garbage collector creation
  let gc = create_garbage_collector(100)
  assert_eq(gc.collection_threshold, 100)
  assert_eq(gc.objects.length(), 0)
  assert_eq(gc.last_collection, 0)
  
  // Test adding objects
  let current_time = 1640995200
  let obj1 = create_gc_object("obj1", 1024, current_time)
  let obj2 = create_gc_object("obj2", 2048, current_time + 1)
  let obj3 = create_gc_object("obj3", 512, current_time + 2)
  
  let gc1 = add_object(gc, obj1)
  let gc2 = add_object(gc1, obj2)
  let gc3 = add_object(gc2, obj3)
  
  assert_eq(gc3.objects.length(), 3)
  
  // Test reference management
  let gc4 = add_reference(gc3, "obj1")
  let obj1_with_ref = gc4.objects.find(fn(o) { o.id == "obj1" }).unwrap()
  assert_eq(obj1_with_ref.references, 2)
  
  let gc5 = remove_reference(gc4, "obj2", current_time + 10)
  let obj2_deref = gc5.objects.find(fn(o) { o.id == "obj2" }).unwrap()
  assert_eq(obj2_deref.references, 0)
  
  // Test garbage collection
  let root_objects = ["obj1", "obj3"] // obj2 should be collected
  let gc6 = run_garbage_collection(gc5, root_objects, current_time + 20)
  
  assert_eq(gc6.objects.length(), 2) // obj2 should be collected
  assert_eq(gc6.stats.objects_collected, 1)
  assert_eq(gc6.stats.memory_freed, 2048) // obj2's size
  assert_eq(gc6.stats.objects_before, 3)
  assert_eq(gc6.stats.objects_after, 2)
  assert_eq(gc6.stats.collection_time_ms, 10)
  
  // Check that obj1 and obj3 are still present
  assert_true(gc6.objects.any(fn(o) { o.id == "obj1" }))
  assert_true(gc6.objects.any(fn(o) { o.id == "obj3" }))
  assert_false(gc6.objects.any(fn(o) { o.id == "obj2" }))
}

// Test 4: Memory Leak Detection
test "memory leak detection for telemetry system" {
  // Define memory leak detection types
  type MemoryLeak = {
    object_id: String,
    object_type: String,
    size: Int,
    allocated_at: Int,
    last_accessed: Int,
    age: Int, // How long it has been allocated
    suspicion_level: Int // 0-5, higher is more suspicious
  }
  
  type LeakDetector = {
    tracked_objects: Array[GCObject],
    leaks: Array[MemoryLeak],
    leak_threshold_age: Int, // Age in seconds to consider suspicious
    max_tracked_objects: Int
  }
  
  // Leak detection operations
  let create_leak_detector = fn(leak_threshold_age: Int, max_tracked_objects: Int) {
    {
      tracked_objects: [],
      leaks: [],
      leak_threshold_age,
      max_tracked_objects
    }
  }
  
  let track_object = fn(detector: LeakDetector, object: GCObject) {
    let updated_objects = detector.tracked_objects.push(object)
    { detector | tracked_objects: updated_objects }
  }
  
  let detect_leaks = fn(detector: LeakDetector, current_time: Int) {
    let mut detected_leaks = []
    
    for obj in detector.tracked_objects {
      let age = current_time - obj.created_at
      let access_age = current_time - obj.last_accessed
      
      // Calculate suspicion level based on age and access patterns
      let mut suspicion_level = 0
      
      if age > detector.leak_threshold_age {
        suspicion_level = suspicion_level + 1
      }
      
      if access_age > detector.leak_threshold_age {
        suspicion_level = suspicion_level + 2
      }
      
      if obj.references == 0 {
        suspicion_level = suspicion_level + 1
      }
      
      if suspicion_level >= 2 { // Only report suspicious objects
        detected_leaks = detected_leaks.push({
          object_id: obj.id,
          object_type: "telemetry-object",
          size: obj.size,
          allocated_at: obj.created_at,
          last_accessed: obj.last_accessed,
          age,
          suspicion_level
        })
      }
    }
    
    {
      tracked_objects: detector.tracked_objects,
      leaks: detected_leaks,
      leak_threshold_age: detector.leak_threshold_age,
      max_tracked_objects: detector.max_tracked_objects
    }
  }
  
  let get_leak_report = fn(detector: LeakDetector) {
    let total_leaked_memory = detector.leaks.fold(0, fn(acc, leak) { acc + leak.size })
    let high_suspicion_leaks = detector.leaks.fold(0, fn(acc, leak) {
      if leak.suspicion_level >= 4 { acc + 1 } else { acc }
    })
    
    {
      total_leaks: detector.leaks.length(),
      total_leaked_memory,
      high_suspicion_leaks,
      average_suspicion: if detector.leaks.length() > 0 {
        detector.leaks.fold(0, fn(acc, leak) { acc + leak.suspicion_level }) / detector.leaks.length()
      } else {
        0
      }
    }
  }
  
  // Test leak detector creation
  let detector = create_leak_detector(3600, 1000) // 1 hour threshold, max 1000 objects
  assert_eq(detector.leak_threshold_age, 3600)
  assert_eq(detector.max_tracked_objects, 1000)
  assert_eq(detector.tracked_objects.length(), 0)
  assert_eq(detector.leaks.length(), 0)
  
  // Test tracking objects
  let current_time = 1640995200
  let obj1 = create_gc_object("obj1", 1024, current_time - 7200) // 2 hours ago
  let obj2 = create_gc_object("obj2", 2048, current_time - 1800) // 30 minutes ago
  let obj3 = create_gc_object("obj3", 512, current_time - 10800) // 3 hours ago, never accessed
  let obj4 = create_gc_object("obj4", 1024, current_time - 100) // Recently created
  
  let detector1 = track_object(detector, obj1)
  let detector2 = track_object(detector1, obj2)
  let detector3 = track_object(detector2, obj3)
  let detector4 = track_object(detector3, obj4)
  
  assert_eq(detector4.tracked_objects.length(), 4)
  
  // Test leak detection
  let detector_with_leaks = detect_leaks(detector4, current_time)
  
  // obj1: 2 hours old, accessed recently, should have low suspicion
  // obj2: 30 minutes old, accessed recently, should not be reported
  // obj3: 3 hours old, never accessed, should have high suspicion
  // obj4: Recently created, should not be reported
  
  assert_eq(detector_with_leaks.leaks.length(), 2) // obj1 and obj3
  
  let obj3_leak = detector_with_leaks.leaks.find(fn(l) { l.object_id == "obj3" }).unwrap()
  assert_eq(obj3_leak.suspicion_level, 4) // Old + never accessed + no references
  
  let obj1_leak = detector_with_leaks.leaks.find(fn(l) { l.object_id == "obj1" }).unwrap()
  assert_eq(obj1_leak.suspicion_level, 1) // Old but accessed recently
  
  // Test leak report
  let report = get_leak_report(detector_with_leaks)
  assert_eq(report.total_leaks, 2)
  assert_eq(report.total_leaked_memory, 1024 + 512) // obj1 + obj3
  assert_eq(report.high_suspicion_leaks, 1) // Only obj3
  assert_eq(report.average_suspicion, (1 + 4) / 2) // Average of obj1 and obj3
}

// Test 5: Memory Fragmentation
test "memory fragmentation analysis and mitigation" {
  // Define fragmentation types
  type MemoryFragment = {
    start_address: Int,
    size: Int,
    is_free: Bool,
    allocated_to: Option[String]
  }
  
  type FragmentationAnalyzer = {
    fragments: Array[MemoryFragment],
    total_memory: Int,
    free_memory: Int,
    allocated_memory: Int,
    fragmentation_ratio: Float
  }
  
  // Fragmentation operations
  let create_fragment = fn(start_address: Int, size: Int, is_free: Bool, allocated_to: Option[String>) {
    {
      start_address,
      size,
      is_free,
      allocated_to
    }
  }
  
  let create_fragmentation_analyzer = fn(total_memory: Int) {
    {
      fragments: [create_fragment(0, total_memory, true, None)], // Start with all free memory
      total_memory,
      free_memory: total_memory,
      allocated_memory: 0,
      fragmentation_ratio: 0.0
    }
  }
  
  let allocate_fragment = fn(analyzer: FragmentationAnalyzer, size: Int, allocated_to: String) {
    let mut updated_fragments = analyzer.fragments
    let mut allocated = false
    
    for i in 0..analyzer.fragments.length() {
      let fragment = analyzer.fragments[i]
      
      if fragment.is_free and fragment.size >= size {
        // Allocate from this free fragment
        let allocated_fragment = create_fragment(fragment.start_address, size, false, Some(allocated_to))
        
        if fragment.size > size {
          // Split the fragment
          let remaining_fragment = create_fragment(
            fragment.start_address + size,
            fragment.size - size,
            true,
            None
          )
          
          updated_fragments = updated_fragments.slice(0, i) +
                          [allocated_fragment, remaining_fragment] +
                          updated_fragments.slice(i + 1, updated_fragments.length())
        } else {
          // Use the entire fragment
          updated_fragments = updated_fragments.set(i, allocated_fragment)
        }
        
        allocated = true
        break
      }
    }
    
    if allocated {
      let new_free_memory = analyzer.free_memory - size
      let new_allocated_memory = analyzer.allocated_memory + size
      
      // Calculate fragmentation ratio
      let free_fragments = updated_fragments.fold(0, fn(acc, f) {
        if f.is_free { acc + 1 } else { acc }
      })
      
      let fragmentation_ratio = if free_fragments > 1 {
        (free_fragments - 1).to_float() / free_fragments.to_float()
      } else {
        0.0
      }
      
      {
        fragments: updated_fragments,
        total_memory: analyzer.total_memory,
        free_memory: new_free_memory,
        allocated_memory: new_allocated_memory,
        fragmentation_ratio
      }
    } else {
      analyzer // Allocation failed
    }
  }
  
  let deallocate_fragment = fn(analyzer: FragmentationAnalyzer, allocated_to: String) {
    let mut updated_fragments = analyzer.fragments
    let mut deallocated = false
    
    for i in 0..analyzer.fragments.length() {
      let fragment = analyzer.fragments[i]
      
      if not(fragment.is_free) and fragment.allocated_to == Some(allocated_to) {
        // Free this fragment
        updated_fragments = updated_fragments.set(i, {
          start_address: fragment.start_address,
          size: fragment.size,
          is_free: true,
          allocated_to: None
        })
        
        deallocated = true
        break
      }
    }
    
    if deallocated {
      // Merge adjacent free fragments
      let mut merged_fragments = []
      let mut i = 0
      
      while i < updated_fragments.length() {
        let current = updated_fragments[i]
        
        if current.is_free and i < updated_fragments.length() - 1 {
          let next = updated_fragments[i + 1]
          
          if next.is_free {
            // Merge with next fragment
            let merged = create_fragment(
              current.start_address,
              current.size + next.size,
              true,
              None
            )
            
            merged_fragments = merged_fragments.push(merged)
            i = i + 2 // Skip the next fragment as it's merged
          } else {
            merged_fragments = merged_fragments.push(current)
            i = i + 1
          }
        } else {
          merged_fragments = merged_fragments.push(current)
          i = i + 1
        }
      }
      
      // Recalculate memory stats
      let new_free_memory = merged_fragments.fold(0, fn(acc, f) {
        if f.is_free { acc + f.size } else { acc }
      })
      
      let new_allocated_memory = merged_fragments.fold(0, fn(acc, f) {
        if not(f.is_free) { acc + f.size } else { acc }
      })
      
      // Calculate fragmentation ratio
      let free_fragments = merged_fragments.fold(0, fn(acc, f) {
        if f.is_free { acc + 1 } else { acc }
      })
      
      let fragmentation_ratio = if free_fragments > 1 {
        (free_fragments - 1).to_float() / free_fragments.to_float()
      } else {
        0.0
      }
      
      {
        fragments: merged_fragments,
        total_memory: analyzer.total_memory,
        free_memory: new_free_memory,
        allocated_memory: new_allocated_memory,
        fragmentation_ratio
      }
    } else {
      analyzer // Deallocation failed
    }
  }
  
  let get_largest_free_block = fn(analyzer: FragmentationAnalyzer) {
    let mut largest_size = 0
    
    for fragment in analyzer.fragments {
      if fragment.is_free and fragment.size > largest_size {
        largest_size = fragment.size
      }
    }
    
    largest_size
  }
  
  let defragment_memory = fn(analyzer: FragmentationAnalyzer) {
    let mut compacted_fragments = []
    let mut current_address = 0
    
    // First, add all allocated fragments
    for fragment in analyzer.fragments {
      if not(fragment.is_free) {
        compacted_fragments = compacted_fragments.push({
          start_address: current_address,
          size: fragment.size,
          is_free: false,
          allocated_to: fragment.allocated_to
        })
        current_address = current_address + fragment.size
      }
    }
    
    // Then add one large free fragment
    if current_address < analyzer.total_memory {
      compacted_fragments = compacted_fragments.push({
        start_address: current_address,
        size: analyzer.total_memory - current_address,
        is_free: true,
        allocated_to: None
      })
    }
    
    {
      fragments: compacted_fragments,
      total_memory: analyzer.total_memory,
      free_memory: analyzer.total_memory - analyzer.allocated_memory,
      allocated_memory: analyzer.allocated_memory,
      fragmentation_ratio: 0.0 // No fragmentation after compaction
    }
  }
  
  // Test fragmentation analyzer creation
  let analyzer = create_fragmentation_analyzer(10240) // 10KB
  assert_eq(analyzer.total_memory, 10240)
  assert_eq(analyzer.free_memory, 10240)
  assert_eq(analyzer.allocated_memory, 0)
  assert_eq(analyzer.fragments.length(), 1)
  assert_eq(analyzer.fragmentation_ratio, 0.0)
  
  // Test allocation
  let analyzer1 = allocate_fragment(analyzer, 2048, "alloc1")
  assert_eq(analyzer1.allocated_memory, 2048)
  assert_eq(analyzer1.free_memory, 8192)
  assert_eq(analyzer1.fragments.length(), 2) // One allocated, one free
  
  let analyzer2 = allocate_fragment(analyzer1, 1024, "alloc2")
  assert_eq(analyzer2.allocated_memory, 3072)
  assert_eq(analyzer2.free_memory, 7168)
  assert_eq(analyzer2.fragments.length(), 3) // Two allocated, one free
  
  // Test deallocation (creates fragmentation)
  let analyzer3 = deallocate_fragment(analyzer2, "alloc1")
  assert_eq(analyzer3.allocated_memory, 1024) // Only alloc2 is still allocated
  assert_eq(analyzer3.free_memory, 9216) // 2048 (alloc1 freed) + 7168 (original free)
  
  // Should have 3 fragments: free, allocated, free
  assert_eq(analyzer3.fragments.length(), 3)
  assert_true(analyzer3.fragments[0].is_free)
  assert_false(analyzer3.fragments[1].is_free)
  assert_true(analyzer3.fragments[2].is_free)
  
  // Fragmentation ratio should be > 0
  assert_true(analyzer3.fragmentation_ratio > 0.0)
  
  // Test largest free block
  let largest_free = get_largest_free_block(analyzer3)
  assert_eq(largest_free, 7168) // The original free block is larger
  
  // Test defragmentation
  let defragmented_analyzer = defragment_memory(analyzer3)
  assert_eq(defragmented_analyzer.fragmentation_ratio, 0.0)
  assert_eq(defragmented_analyzer.fragments.length(), 2) // One allocated, one free
  assert_false(defragmented_analyzer.fragments[0].is_free)
  assert_true(defragmented_analyzer.fragments[1].is_free)
  
  // The allocated fragment should be at the beginning
  assert_eq(defragmented_analyzer.fragments[0].start_address, 0)
  assert_eq(defragmented_analyzer.fragments[1].start_address, 1024)
}

// Test 6: Memory Usage Monitoring
test "memory usage monitoring and alerting" {
  // Define monitoring types
  type MemorySnapshot = {
    timestamp: Int,
    total_memory: Int,
    used_memory: Int,
    free_memory: Int,
    active_allocations: Int,
    fragmentation_ratio: Float
  }
  
  type MemoryAlert = {
    alert_type: String,
    message: String,
    timestamp: Int,
    severity: String // "low", "medium", "high", "critical"
  }
  
  type MemoryMonitor = {
    snapshots: Array[MemorySnapshot],
    alerts: Array[MemoryAlert],
    alert_thresholds: {
      usage_percent: Int,
      fragmentation_ratio: Float,
      allocation_rate: Int // allocations per second
    }
  }
  
  // Monitoring operations
  let create_memory_monitor = fn(usage_threshold: Int, fragmentation_threshold: Float, allocation_rate: Int) {
    {
      snapshots: [],
      alerts: [],
      alert_thresholds: {
        usage_percent: usage_threshold,
        fragmentation_ratio: fragmentation_threshold,
        allocation_rate: allocation_rate
      }
    }
  }
  
  let take_snapshot = fn(monitor: MemoryMonitor, total_memory: Int, used_memory: Int, 
                         active_allocations: Int, fragmentation_ratio: Float, current_time: Int) {
    let snapshot = {
      timestamp: current_time,
      total_memory,
      used_memory,
      free_memory: total_memory - used_memory,
      active_allocations,
      fragmentation_ratio
    }
    
    { monitor | snapshots: monitor.snapshots.push(snapshot) }
  }
  
  let check_alerts = fn(monitor: MemoryMonitor, current_time: Int) {
    if monitor.snapshots.length() < 2 {
      return monitor // Need at least 2 snapshots for trend analysis
    }
    
    let latest = monitor.snapshots[monitor.snapshots.length() - 1]
    let previous = monitor.snapshots[monitor.snapshots.length() - 2]
    
    let mut new_alerts = monitor.alerts
    
    // Check memory usage
    let usage_percent = (latest.used_memory.to_float() / latest.total_memory.to_float()) * 100.0
    
    if usage_percent > monitor.alert_thresholds.usage_percent.to_float() {
      let severity = if usage_percent > 90.0 {
        "critical"
      } else if usage_percent > 80.0 {
        "high"
      } else if usage_percent > 70.0 {
        "medium"
      } else {
        "low"
      }
      
      new_alerts = new_alerts.push({
        alert_type: "high_memory_usage",
        message: "Memory usage is " + usage_percent.to_int().to_string() + "%",
        timestamp: current_time,
        severity
      })
    }
    
    // Check fragmentation
    if latest.fragmentation_ratio > monitor.alert_thresholds.fragmentation_ratio {
      let severity = if latest.fragmentation_ratio > 0.8 {
        "high"
      } else if latest.fragmentation_ratio > 0.6 {
        "medium"
      } else {
        "low"
      }
      
      new_alerts = new_alerts.push({
        alert_type: "high_fragmentation",
        message: "Memory fragmentation ratio is " + (latest.fragmentation_ratio * 100.0).to_int().to_string() + "%",
        timestamp: current_time,
        severity
      })
    }
    
    // Check allocation rate
    let time_diff = latest.timestamp - previous.timestamp
    if time_diff > 0 {
      let allocation_rate = (latest.active_allocations - previous.active_allocations) * 1000 / time_diff
      
      if allocation_rate > monitor.alert_thresholds.allocation_rate {
        let severity = if allocation_rate > monitor.alert_thresholds.allocation_rate * 2 {
          "high"
        } else if allocation_rate > monitor.alert_thresholds.allocation_rate * 1.5 {
          "medium"
        } else {
          "low"
        }
        
        new_alerts = new_alerts.push({
          alert_type: "high_allocation_rate",
          message: "Allocation rate is " + allocation_rate.to_string() + " allocations/sec",
          timestamp: current_time,
          severity
        })
      }
    }
    
    {
      snapshots: monitor.snapshots,
      alerts: new_alerts,
      alert_thresholds: monitor.alert_thresholds
    }
  }
  
  let get_memory_trend = fn(monitor: MemoryMonitor) {
    if monitor.snapshots.length() < 2 {
      "insufficient_data"
    } else {
      let latest = monitor.snapshots[monitor.snapshots.length() - 1]
      let previous = monitor.snapshots[0] // Compare with first snapshot
      
      let usage_change = latest.used_memory - previous.used_memory
      let time_change = latest.timestamp - previous.timestamp
      
      if time_change > 0 {
        let rate = usage_change / time_change
        
        if rate > 1024 { // More than 1KB per second
          "increasing"
        } else if rate < -1024 { // Decreasing by more than 1KB per second
          "decreasing"
        } else {
          "stable"
        }
      } else {
        "stable"
      }
    }
  }
  
  // Test memory monitor creation
  let monitor = create_memory_monitor(80, 0.5, 100)
  assert_eq(monitor.alert_thresholds.usage_percent, 80)
  assert_eq(monitor.alert_thresholds.fragmentation_ratio, 0.5)
  assert_eq(monitor.alert_thresholds.allocation_rate, 100)
  assert_eq(monitor.snapshots.length(), 0)
  assert_eq(monitor.alerts.length(), 0)
  
  // Test taking snapshots
  let current_time = 1640995200
  let monitor1 = take_snapshot(monitor, 10240, 5120, 10, 0.2, current_time)
  assert_eq(monitor1.snapshots.length(), 1)
  assert_eq(monitor1.snapshots[0].used_memory, 5120)
  assert_eq(monitor1.snapshots[0].free_memory, 5120)
  
  let monitor2 = take_snapshot(monitor1, 10240, 8192, 15, 0.1, current_time + 10)
  assert_eq(monitor2.snapshots.length(), 2)
  
  // Test alert checking
  let monitor_with_alerts = check_alerts(monitor2, current_time + 10)
  
  // Memory usage is 8192/10240 = 80%, which should trigger an alert
  assert_true(monitor_with_alerts.alerts.length() > 0)
  
  let usage_alert = monitor_with_alerts.alerts.find(fn(a) { a.alert_type == "high_memory_usage" }).unwrap()
  assert_eq(usage_alert.severity, "medium") // 80% is medium severity
  
  // Test high memory usage
  let monitor3 = take_snapshot(monitor2, 10240, 9216, 20, 0.3, current_time + 20) // 90% usage
  let monitor4 = check_alerts(monitor3, current_time + 20)
  
  let high_usage_alert = monitor4.alerts.find(fn(a) { a.alert_type == "high_memory_usage" and a.timestamp == current_time + 20 }).unwrap()
  assert_eq(high_usage_alert.severity, "critical") // 90% is critical
  
  // Test fragmentation alert
  let monitor5 = take_snapshot(monitor3, 10240, 6144, 20, 0.6, current_time + 30) // 60% fragmentation
  let monitor6 = check_alerts(monitor5, current_time + 30)
  
  let fragmentation_alert = monitor6.alerts.find(fn(a) { a.alert_type == "high_fragmentation" }).unwrap()
  assert_eq(fragmentation_alert.severity, "medium") // 60% is medium
  
  // Test allocation rate alert
  let monitor7 = take_snapshot(monitor5, 10240, 6144, 30, 0.6, current_time + 31) // 10 allocations in 1 second
  let monitor8 = check_alerts(monitor7, current_time + 31)
  
  // 10 allocations per second is below the threshold of 100, so no alert
  let allocation_alert = monitor8.alerts.find(fn(a) { a.alert_type == "high_allocation_rate" })
  assert_eq(allocation_alert, None)
  
  // Test memory trend
  let trend = get_memory_trend(monitor8)
  assert_eq(trend, "increasing") // Memory usage increased from 5120 to 6144
}

// Test 7: Memory Optimization Strategies
test "memory optimization strategies for telemetry" {
  // Define optimization types
  type OptimizationStrategy = 
    | ObjectPooling
    | LazyLoading
    | DataCompression
    | MemoryMapping
  
  type OptimizationResult = {
    strategy: OptimizationStrategy,
    memory_saved: Int,
    performance_impact: Float, // -1.0 to 1.0, negative is performance loss
    implementation_complexity: Int // 1-10, higher is more complex
  }
  
  type MemoryOptimizer = {
    applied_strategies: Array[OptimizationResult],
    total_memory_saved: Int,
    net_performance_impact: Float
  }
  
  // Optimization operations
  let create_memory_optimizer = fn() {
    {
      applied_strategies: [],
      total_memory_saved: 0,
      net_performance_impact: 0.0
    }
  }
  
  let apply_strategy = fn(optimizer: MemoryOptimizer, strategy: OptimizationStrategy, 
                         memory_saved: Int, performance_impact: Float, implementation_complexity: Int) {
    let result = {
      strategy,
      memory_saved,
      performance_impact,
      implementation_complexity
    }
    
    let new_total_saved = optimizer.total_memory_saved + memory_saved
    let new_performance_impact = optimizer.net_performance_impact + performance_impact
    
    {
      applied_strategies: optimizer.applied_strategies.push(result),
      total_memory_saved: new_total_saved,
      net_performance_impact: new_performance_impact
    }
  }
  
  let calculate_object_pooling_benefit = fn(object_count: Int, object_size: Int, 
                                          allocation_frequency: Int, reuse_ratio: Float) {
    // Calculate memory saved by reusing objects instead of allocating new ones
    let allocations_without_pool = object_count * allocation_frequency
    let allocations_with_pool = allocations_without_pool * (1.0 - reuse_ratio)
    let saved_allocations = allocations_without_pool - allocations_with_pool
    
    let memory_saved = saved_allocations.to_int() * object_size
    
    // Performance impact: pooling improves performance for high allocation frequency
    let performance_impact = if allocation_frequency > 100 {
      0.3 // Significant performance gain
    } else if allocation_frequency > 10 {
      0.1 // Minor performance gain
    } else {
      -0.1 // Slight performance loss due to overhead
    }
    
    (memory_saved, performance_impact)
  }
  
  let calculate_lazy_loading_benefit = fn(total_data_size: Int, access_ratio: Float) {
    // Calculate memory saved by only loading data when accessed
    let loaded_data_size = (total_data_size.to_float() * access_ratio).to_int()
    let memory_saved = total_data_size - loaded_data_size
    
    // Performance impact: lazy loading can improve startup time but may cause delays
    let performance_impact = if access_ratio < 0.5 {
      0.2 // Significant memory savings with minor performance impact
    } else if access_ratio < 0.8 {
      0.1 // Minor memory savings
    } else {
      -0.1 // Not worth it for high access ratios
    }
    
    (memory_saved, performance_impact)
  }
  
  let calculate_compression_benefit = fn(data_size: Int, compression_ratio: Float, 
                                        access_frequency: Int) {
    // Calculate memory saved by compressing data
    let compressed_size = (data_size.to_float() / compression_ratio).to_int()
    let memory_saved = data_size - compressed_size
    
    // Performance impact: compression saves memory but adds CPU overhead
    let performance_impact = if access_frequency > 1000 {
      -0.3 // High performance impact for frequent access
    } else if access_frequency > 100 {
      -0.1 // Minor performance impact
    } else {
      0.2 // Good trade-off for infrequent access
    }
    
    (memory_saved, performance_impact)
  }
  
  let get_optimization_recommendation = fn(optimizer: MemoryOptimizer) {
    if optimizer.total_memory_saved > 10240 { // More than 10KB saved
      "High memory savings achieved with " + optimizer.applied_strategies.length().to_string() + " strategies"
    } else if optimizer.total_memory_saved > 1024 { // More than 1KB saved
      "Moderate memory savings achieved"
    } else {
      "Consider additional optimization strategies"
    }
  }
  
  // Test optimizer creation
  let optimizer = create_memory_optimizer()
  assert_eq(optimizer.applied_strategies.length(), 0)
  assert_eq(optimizer.total_memory_saved, 0)
  assert_eq(optimizer.net_performance_impact, 0.0)
  
  // Test object pooling strategy
  let (pool_memory_saved, pool_performance_impact) = calculate_object_pooling_benefit(100, 1024, 1000, 0.8)
  let optimizer1 = apply_strategy(optimizer, ObjectPooling, pool_memory_saved, pool_performance_impact, 5)
  
  assert_eq(optimizer1.total_memory_saved, pool_memory_saved)
  assert_eq(optimizer1.net_performance_impact, pool_performance_impact)
  assert_eq(optimizer1.applied_strategies.length(), 1)
  
  // Test lazy loading strategy
  let (lazy_memory_saved, lazy_performance_impact) = calculate_lazy_loading_benefit(10240, 0.3)
  let optimizer2 = apply_strategy(optimizer1, LazyLoading, lazy_memory_saved, lazy_performance_impact, 3)
  
  assert_eq(optimizer2.total_memory_saved, pool_memory_saved + lazy_memory_saved)
  assert_eq(optimizer2.net_performance_impact, pool_performance_impact + lazy_performance_impact)
  assert_eq(optimizer2.applied_strategies.length(), 2)
  
  // Test compression strategy
  let (compression_memory_saved, compression_performance_impact) = calculate_compression_benefit(5120, 2.0, 50)
  let optimizer3 = apply_strategy(optimizer2, DataCompression, compression_memory_saved, compression_performance_impact, 7)
  
  assert_eq(optimizer3.total_memory_saved, pool_memory_saved + lazy_memory_saved + compression_memory_saved)
  assert_eq(optimizer3.net_performance_impact, pool_performance_impact + lazy_performance_impact + compression_performance_impact)
  assert_eq(optimizer3.applied_strategies.length(), 3)
  
  // Test optimization recommendation
  let recommendation = get_optimization_recommendation(optimizer3)
  assert_true(recommendation.contains("memory savings"))
  
  // Test edge cases
  let (no_benefit_saved, no_benefit_impact) = calculate_lazy_loading_benefit(1024, 0.9) // High access ratio
  assert_eq(no_benefit_saved, 102) // 10% of data loaded
  assert_eq(no_benefit_impact, -0.1) // Not worth it
  
  let (high_impact_saved, high_impact_impact) = calculate_compression_benefit(10240, 3.0, 2000) // High access frequency
  assert_true(high_impact_saved > 0)
  assert_eq(high_impact_impact, -0.3) // High performance impact
}

// Test 8: Memory Profiling
test "memory profiling for telemetry applications" {
  // Define profiling types
  type MemoryProfile = {
    function_name: String,
    allocations: Int,
    total_allocated: Int,
    peak_usage: Int,
    allocation_frequency: Float,
    average_lifetime: Int
  }
  
  type MemoryProfiler = {
    profiles: Array[MemoryProfile],
    total_allocations: Int,
    total_allocated: Int,
    peak_memory_usage: Int,
    profiling_duration: Int
  }
  
  // Profiling operations
  let create_memory_profiler = fn() {
    {
      profiles: [],
      total_allocations: 0,
      total_allocated: 0,
      peak_memory_usage: 0,
      profiling_duration: 0
    }
  }
  
  let start_profiling_function = fn(profiler: MemoryProfiler, function_name: String) {
    // Check if function is already being profiled
    let existing_profile = profiler.profiles.find(fn(p) { p.function_name == function_name })
    
    match existing_profile {
      Some(_) => profiler // Already profiling this function
      None => {
        let new_profile = {
          function_name,
          allocations: 0,
          total_allocated: 0,
          peak_usage: 0,
          allocation_frequency: 0.0,
          average_lifetime: 0
        }
        
        { profiler | profiles: profiler.profiles.push(new_profile) }
      }
    }
  }
  
  let record_allocation = fn(profiler: MemoryProfiler, function_name: String, size: Int, current_time: Int) {
    let mut updated_profiles = profiler.profiles
    let mut found = false
    
    for i in 0..profiler.profiles.length() {
      let profile = profiler.profiles[i]
      
      if profile.function_name == function_name {
        let new_peak = if profile.total_allocated + size > profile.peak_usage {
          profile.total_allocated + size
        } else {
          profile.peak_usage
        }
        
        updated_profiles = updated_profiles.set(i, {
          function_name,
          allocations: profile.allocations + 1,
          total_allocated: profile.total_allocated + size,
          peak_usage: new_peak,
          allocation_frequency: profile.allocation_frequency,
          average_lifetime: profile.average_lifetime
        })
        
        found = true
        break
      }
    }
    
    if found {
      {
        profiles: updated_profiles,
        total_allocations: profiler.total_allocations + 1,
        total_allocated: profiler.total_allocated + size,
        peak_memory_usage: if profiler.total_allocated + size > profiler.peak_memory_usage {
          profiler.total_allocated + size
        } else {
          profiler.peak_memory_usage
        },
        profiling_duration: profiler.profiling_duration
      }
    } else {
      profiler // Function not being profiled
    }
  }
  
  let finalize_profiling = fn(profiler: MemoryProfiler, duration: Int) {
    let mut updated_profiles = profiler.profiles
    
    for i in 0..profiler.profiles.length() {
      let profile = profiler.profiles[i]
      
      // Calculate allocation frequency (allocations per second)
      let allocation_frequency = if duration > 0 {
        profile.allocations.to_float() / duration.to_float()
      } else {
        0.0
      }
      
      // Calculate average lifetime (simplified)
      let average_lifetime = if profile.allocations > 0 {
        duration / profile.allocations
      } else {
        0
      }
      
      updated_profiles = updated_profiles.set(i, {
        function_name: profile.function_name,
        allocations: profile.allocations,
        total_allocated: profile.total_allocated,
        peak_usage: profile.peak_usage,
        allocation_frequency,
        average_lifetime
      })
    }
    
    {
      profiles: updated_profiles,
      total_allocations: profiler.total_allocations,
      total_allocated: profiler.total_allocated,
      peak_memory_usage: profiler.peak_memory_usage,
      profiling_duration: duration
    }
  }
  
  let get_memory_hotspots = fn(profiler: MemoryProfiler, limit: Int) {
    let mut sorted_profiles = profiler.profiles
    
    // Sort by total allocated (descending)
    for i in 0..sorted_profiles.length() {
      for j in i+1..sorted_profiles.length() {
        if sorted_profiles[j].total_allocated > sorted_profiles[i].total_allocated {
          let temp = sorted_profiles[i]
          sorted_profiles = sorted_profiles.set(i, sorted_profiles[j])
          sorted_profiles = sorted_profiles.set(j, temp)
        }
      }
    }
    
    sorted_profiles.slice(0, if limit < sorted_profiles.length() { limit } else { sorted_profiles.length() })
  }
  
  let generate_profiling_report = fn(profiler: MemoryProfiler) {
    let hotspots = get_memory_hotspots(profiler, 5)
    
    "Memory Profiling Report\n" +
    "Total Allocations: " + profiler.total_allocations.to_string() + "\n" +
    "Total Memory Allocated: " + profiler.total_allocated.to_string() + " bytes\n" +
    "Peak Memory Usage: " + profiler.peak_memory_usage.to_string() + " bytes\n" +
    "Profiling Duration: " + profiler.profiling_duration.to_string() + " seconds\n" +
    "Top Memory Hotspots:\n" +
    hotspots.fold("", fn(acc, profile) {
      acc + "  " + profile.function_name + ": " + profile.total_allocated.to_string() + " bytes\n"
    })
  }
  
  // Test profiler creation
  let profiler = create_memory_profiler()
  assert_eq(profiler.profiles.length(), 0)
  assert_eq(profiler.total_allocations, 0)
  assert_eq(profiler.total_allocated, 0)
  assert_eq(profiler.peak_memory_usage, 0)
  
  // Test starting profiling
  let profiler1 = start_profiling_function(profiler, "process_telemetry")
  assert_eq(profiler1.profiles.length(), 1)
  assert_eq(profiler1.profiles[0].function_name, "process_telemetry")
  
  let profiler2 = start_profiling_function(profiler1, "serialize_data")
  assert_eq(profiler2.profiles.length(), 2)
  
  // Test recording allocations
  let current_time = 1640995200
  let profiler3 = record_allocation(profiler2, "process_telemetry", 1024, current_time)
  let profiler4 = record_allocation(profiler3, "process_telemetry", 2048, current_time + 1)
  let profiler5 = record_allocation(profiler4, "serialize_data", 512, current_time + 2)
  
  assert_eq(profiler5.total_allocations, 3)
  assert_eq(profiler5.total_allocated, 1024 + 2048 + 512)
  
  let telemetry_profile = profiler5.profiles.find(fn(p) { p.function_name == "process_telemetry" }).unwrap()
  assert_eq(telemetry_profile.allocations, 2)
  assert_eq(telemetry_profile.total_allocated, 3072)
  
  let serialize_profile = profiler5.profiles.find(fn(p) { p.function_name == "serialize_data" }).unwrap()
  assert_eq(serialize_profile.allocations, 1)
  assert_eq(serialize_profile.total_allocated, 512)
  
  // Test finalizing profiling
  let finalized_profiler = finalize_profiling(profiler5, 10) // 10 seconds
  
  let finalized_telemetry_profile = finalized_profiler.profiles.find(fn(p) { p.function_name == "process_telemetry" }).unwrap()
  assert_eq(finalized_telemetry_profile.allocation_frequency, 0.2) // 2 allocations / 10 seconds
  assert_eq(finalized_telemetry_profile.average_lifetime, 5) // 10 seconds / 2 allocations
  
  // Test memory hotspots
  let hotspots = get_memory_hotspots(finalized_profiler, 5)
  assert_eq(hotspots.length(), 2)
  assert_eq(hotspots[0].function_name, "process_telemetry") // Higher total allocated
  assert_eq(hotspots[1].function_name, "serialize_data")
  
  // Test profiling report
  let report = generate_profiling_report(finalized_profiler)
  assert_true(report.contains("Memory Profiling Report"))
  assert_true(report.contains("Total Allocations: 3"))
  assert_true(report.contains("process_telemetry: 3072 bytes"))
  assert_true(report.contains("serialize_data: 512 bytes"))
}