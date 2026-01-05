// Azimuth Premium Memory Management Tests
// This file contains high-quality test cases for memory management and resource cleanup

// Test 1: Memory Pool Management
test "memory pool management" {
  // Simple memory pool implementation
  type MemoryPool {
    blocks : Array[MemoryBlock]
    free_blocks : Array[Int]
    block_size : Int
    total_blocks : Int
  }
  
  type MemoryBlock {
    data : Array[Byte]
    allocated : Bool
  }
  
  let create_memory_pool = fn(block_size : Int, total_blocks : Int) -> MemoryPool {
    let mut blocks = []
    
    for i = 0; i < total_blocks; i = i + 1 {
      blocks.push(MemoryBlock { 
        data: [0; block_size], 
        allocated: false 
      })
    }
    
    MemoryPool { 
      blocks: blocks, 
      free_blocks: [0; total_blocks].map(|i| i), 
      block_size: block_size, 
      total_blocks: total_blocks 
    }
  }
  
  let allocate_block = fn(pool : MemoryPool) -> (Option[Int], MemoryPool) {
    if pool.free_blocks.length() == 0 {
      return (None, pool)
    }
    
    let block_index = pool.free_blocks[0]
    let mut new_blocks = pool.blocks
    let mut new_free_blocks = []
    
    // Mark block as allocated
    new_blocks[block_index] = MemoryBlock { 
      data: new_blocks[block_index].data, 
      allocated: true 
    }
    
    // Remove from free list
    for i = 1; i < pool.free_blocks.length(); i = i + 1 {
      new_free_blocks.push(pool.free_blocks[i])
    }
    
    let new_pool = MemoryPool {
      blocks: new_blocks,
      free_blocks: new_free_blocks,
      block_size: pool.block_size,
      total_blocks: pool.total_blocks
    }
    
    (Some(block_index), new_pool)
  }
  
  let deallocate_block = fn(pool : MemoryPool, block_index : Int) -> MemoryPool {
    if block_index < 0 || block_index >= pool.blocks.length() {
      return pool
    }
    
    if !pool.blocks[block_index].allocated {
      return pool
    }
    
    let mut new_blocks = pool.blocks
    let mut new_free_blocks = pool.free_blocks
    
    // Mark block as free
    new_blocks[block_index] = MemoryBlock { 
      data: new_blocks[block_index].data, 
      allocated: false 
    }
    
    // Clear block data
    for i = 0; i < new_blocks[block_index].data.length(); i = i + 1 {
      new_blocks[block_index].data[i] = 0
    }
    
    // Add to free list
    new_free_blocks.push(block_index)
    
    MemoryPool {
      blocks: new_blocks,
      free_blocks: new_free_blocks,
      block_size: pool.block_size,
      total_blocks: pool.total_blocks
    }
  }
  
  // Test memory pool operations
  let pool = create_memory_pool(1024, 10)
  assert_eq(pool.total_blocks, 10)
  assert_eq(pool.free_blocks.length(), 10)
  
  // Allocate blocks
  let (block1, pool1) = allocate_block(pool)
  let (block2, pool2) = allocate_block(pool1)
  let (block3, pool3) = allocate_block(pool2)
  
  match (block1, block2, block3) {
    (Some(b1), Some(b2), Some(b3)) => {
      assert_true(b1 >= 0 && b1 < 10)
      assert_true(b2 >= 0 && b2 < 10)
      assert_true(b3 >= 0 && b3 < 10)
      assert_not_eq(b1, b2)
      assert_not_eq(b2, b3)
      assert_not_eq(b1, b3)
    }
    _ => assert_true(false)
  }
  
  assert_eq(pool3.free_blocks.length(), 7)
  
  // Deallocate blocks
  let pool4 = deallocate_block(pool3, block1.unwrap())
  let pool5 = deallocate_block(pool4, block2.unwrap())
  
  assert_eq(pool5.free_blocks.length(), 9)
  
  // Reallocate blocks
  let (block4, pool6) = allocate_block(pool5)
  match block4 {
    Some(b) => assert_true(b >= 0 && b < 10)
    None => assert_true(false)
  }
  
  assert_eq(pool6.free_blocks.length(), 8)
}

// Test 2: Reference Counting
test "reference counting" {
  // Reference counted object
  type RefCounted {
    data : String
    ref_count : Int
  }
  
  let create_ref_counted = fn(data : String) -> RefCounted {
    RefCounted { 
      data: data, 
      ref_count: 1 
    }
  }
  
  let add_ref = fn(obj : RefCounted) -> RefCounted {
    RefCounted { 
      data: obj.data, 
      ref_count: obj.ref_count + 1 
    }
  }
  
  let release_ref = fn(obj : RefCounted) -> (Bool, RefCounted) {
    let new_count = obj.ref_count - 1
    let should_deallocate = new_count <= 0
    
    let new_obj = RefCounted { 
      data: obj.data, 
      ref_count: new_count 
    }
    
    (should_deallocate, new_obj)
  }
  
  // Test reference counting
  let obj = create_ref_counted("test data")
  assert_eq(obj.ref_count, 1)
  
  let obj2 = add_ref(obj)
  assert_eq(obj2.ref_count, 2)
  
  let obj3 = add_ref(obj2)
  assert_eq(obj3.ref_count, 3)
  
  // Release references
  let (should_delete1, obj4) = release_ref(obj3)
  assert_false(should_delete1)
  assert_eq(obj4.ref_count, 2)
  
  let (should_delete2, obj5) = release_ref(obj4)
  assert_false(should_delete2)
  assert_eq(obj5.ref_count, 1)
  
  let (should_delete3, obj6) = release_ref(obj5)
  assert_true(should_delete3)
  assert_eq(obj6.ref_count, 0)
}

// Test 3: Resource Management with RAII
test "resource management with raii" {
  // File handle resource
  type FileHandle {
    id : Int
    is_open : Bool
    path : String
  }
  
  type ResourceManager {
    handles : Array[FileHandle]
    next_id : Int
  }
  
  let create_resource_manager = fn() -> ResourceManager {
    ResourceManager { 
      handles: [], 
      next_id: 1 
    }
  }
  
  let open_file = fn(manager : ResourceManager, path : String) -> (FileHandle, ResourceManager) {
    let handle = FileHandle { 
      id: manager.next_id, 
      is_open: true, 
      path: path 
    }
    
    let new_manager = ResourceManager {
      handles: manager.handles.push(handle),
      next_id: manager.next_id + 1
    }
    
    (handle, new_manager)
  }
  
  let close_file = fn(manager : ResourceManager, handle : FileHandle) -> ResourceManager {
    let mut new_handles = []
    
    for h in manager.handles {
      if h.id == handle.id {
        new_handles.push(FileHandle { 
          id: h.id, 
          is_open: false, 
          path: h.path 
        })
      } else {
        new_handles.push(h)
      }
    }
    
    ResourceManager {
      handles: new_handles,
      next_id: manager.next_id
    }
  }
  
  let auto_close_file = fn(manager : ResourceManager, path : String, operation : FileHandle -> String) -> (String, ResourceManager) {
    let (handle, manager_with_handle) = open_file(manager, path)
    let result = operation(handle)
    let final_manager = close_file(manager_with_handle, handle)
    
    (result, final_manager)
  }
  
  // Test RAII pattern
  let manager = create_resource_manager()
  
  // Auto-close file after operation
  let (result, final_manager) = auto_close_file(manager, "test.txt", |handle| {
    "File " + handle.path.to_string() + " processed"
  })
  
  assert_eq(result, "File test.txt processed")
  
  // Verify file is closed
  let mut found_handle = false
  let mut is_closed = false
  
  for h in final_manager.handles {
    if h.path == "test.txt" {
      found_handle = true
      is_closed = !h.is_open
    }
  }
  
  assert_true(found_handle)
  assert_true(is_closed)
  
  // Test manual resource management
  let (handle2, manager2) = open_file(final_manager, "manual.txt")
  assert_true(handle2.is_open)
  
  let manager3 = close_file(manager2, handle2)
  
  // Verify file is closed
  let mut found_handle2 = false
  let mut is_closed2 = false
  
  for h in manager3.handles {
    if h.path == "manual.txt" {
      found_handle2 = true
      is_closed2 = !h.is_open
    }
  }
  
  assert_true(found_handle2)
  assert_true(is_closed2)
}

// Test 4: Memory Leak Detection
test "memory leak detection" {
  // Memory tracker
  type MemoryTracker {
    allocations : Array[(Int, Int)] // (id, size)
    deallocations : Array[Int] // id
    next_id : Int
    total_allocated : Int
    total_deallocated : Int
  }
  
  let create_tracker = fn() -> MemoryTracker {
    MemoryTracker { 
      allocations: [], 
      deallocations: [], 
      next_id: 1, 
      total_allocated: 0, 
      total_deallocated: 0 
    }
  }
  
  let allocate_memory = fn(tracker : MemoryTracker, size : Int) -> (Int, MemoryTracker) {
    let id = tracker.next_id
    let new_tracker = MemoryTracker {
      allocations: tracker.allocations.push((id, size)),
      deallocations: tracker.deallocations,
      next_id: tracker.next_id + 1,
      total_allocated: tracker.total_allocated + size,
      total_deallocated: tracker.total_deallocated
    }
    
    (id, new_tracker)
  }
  
  let deallocate_memory = fn(tracker : MemoryTracker, id : Int) -> MemoryTracker {
    let mut size = 0
    let mut found = false
    
    for (alloc_id, alloc_size) in tracker.allocations {
      if alloc_id == id {
        size = alloc_size
        found = true
        break
      }
    }
    
    if found {
      MemoryTracker {
        allocations: tracker.allocations,
        deallocations: tracker.deallocations.push(id),
        next_id: tracker.next_id,
        total_allocated: tracker.total_allocated,
        total_deallocated: tracker.total_deallocated + size
      }
    } else {
      tracker
    }
  }
  
  let detect_leaks = fn(tracker : MemoryTracker) -> Array[(Int, Int)] {
    let mut leaks = []
    
    for (alloc_id, alloc_size) in tracker.allocations {
      let mut deallocated = false
      
      for dealloc_id in tracker.deallocations {
        if dealloc_id == alloc_id {
          deallocated = true
          break
        }
      }
      
      if !deallocated {
        leaks.push((alloc_id, alloc_size))
      }
    }
    
    leaks
  }
  
  // Test memory leak detection
  let tracker = create_memory_tracker()
  
  // Allocate memory
  let (id1, tracker1) = allocate_memory(tracker, 100)
  let (id2, tracker2) = allocate_memory(tracker1, 200)
  let (id3, tracker3) = allocate_memory(tracker2, 150)
  
  assert_eq(tracker3.total_allocated, 450)
  assert_eq(tracker3.total_deallocated, 0)
  
  // Deallocate some memory
  let tracker4 = deallocate_memory(tracker3, id1)
  let tracker5 = deallocate_memory(tracker4, id3)
  
  assert_eq(tracker5.total_allocated, 450)
  assert_eq(tracker5.total_deallocated, 250) // 100 + 150
  
  // Detect leaks
  let leaks = detect_leaks(tracker5)
  assert_eq(leaks.length(), 1)
  assert_eq(leaks[0], (id2, 200))
  
  // Deallocate remaining memory
  let tracker6 = deallocate_memory(tracker5, id2)
  let leaks2 = detect_leaks(tracker6)
  assert_eq(leaks2.length(), 0)
  assert_eq(tracker6.total_deallocated, 450)
}

// Test 5: Buffer Management
test "buffer management" {
  // Dynamic buffer
  type Buffer {
    data : Array[Byte]
    capacity : Int
    length : Int
  }
  
  let create_buffer = fn(initial_capacity : Int) -> Buffer {
    Buffer { 
      data: [0; initial_capacity], 
      capacity: initial_capacity, 
      length: 0 
    }
  }
  
  let buffer_write = fn(buffer : Buffer, bytes : Array[Byte]) -> Buffer {
    let required_length = buffer.length + bytes.length()
    
    if required_length > buffer.capacity {
      // Need to resize
      let new_capacity = required_length * 2
      let mut new_data = [0; new_capacity]
      
      // Copy existing data
      for i = 0; i < buffer.length; i = i + 1 {
        new_data[i] = buffer.data[i]
      }
      
      // Copy new bytes
      for i = 0; i < bytes.length(); i = i + 1 {
        new_data[buffer.length + i] = bytes[i]
      }
      
      Buffer {
        data: new_data,
        capacity: new_capacity,
        length: required_length
      }
    } else {
      // No resize needed
      let mut new_data = buffer.data
      
      for i = 0; i < bytes.length(); i = i + 1 {
        new_data[buffer.length + i] = bytes[i]
      }
      
      Buffer {
        data: new_data,
        capacity: buffer.capacity,
        length: required_length
      }
    }
  }
  
  let buffer_read = fn(buffer : Buffer, offset : Int, length : Int) -> Array[Byte] {
    if offset < 0 || length < 0 || offset + length > buffer.length {
      return []
    }
    
    let mut result = []
    
    for i = 0; i < length; i = i + 1 {
      result.push(buffer.data[offset + i])
    }
    
    result
  }
  
  let buffer_clear = fn(buffer : Buffer) -> Buffer {
    Buffer {
      data: buffer.data,
      capacity: buffer.capacity,
      length: 0
    }
  }
  
  // Test buffer operations
  let buffer = create_buffer(10)
  assert_eq(buffer.capacity, 10)
  assert_eq(buffer.length, 0)
  
  // Write data
  let data1 = [1, 2, 3, 4, 5]
  let buffer1 = buffer_write(buffer, data1)
  assert_eq(buffer1.length, 5)
  assert_eq(buffer1.capacity, 10)
  
  // Read data
  let read1 = buffer_read(buffer1, 0, 5)
  assert_eq(read1, [1, 2, 3, 4, 5])
  
  // Write more data (triggers resize)
  let data2 = [6, 7, 8, 9, 10, 11, 12, 13]
  let buffer2 = buffer_write(buffer1, data2)
  assert_eq(buffer2.length, 13)
  assert_true(buffer2.capacity > 10) // Should have resized
  
  // Read all data
  let read2 = buffer_read(buffer2, 0, 13)
  assert_eq(read2, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13])
  
  // Read partial data
  let read3 = buffer_read(buffer2, 3, 5)
  assert_eq(read3, [4, 5, 6, 7, 8])
  
  // Clear buffer
  let buffer3 = buffer_clear(buffer2)
  assert_eq(buffer3.length, 0)
  assert_eq(buffer3.capacity, buffer2.capacity) // Capacity remains the same
  
  // Write new data after clear
  let data3 = [100, 200, 300]
  let buffer4 = buffer_write(buffer3, data3)
  assert_eq(buffer4.length, 3)
  
  let read4 = buffer_read(buffer4, 0, 3)
  assert_eq(read4, [100, 200, 300])
  
  // Test edge cases
  let read5 = buffer_read(buffer4, -1, 2) // Invalid offset
  assert_eq(read5.length(), 0)
  
  let read6 = buffer_read(buffer4, 0, 10) // Length beyond buffer
  assert_eq(read6.length(), 0)
}

// Test 6: Garbage Collection Simulation
test "garbage collection simulation" {
  // Object with references
  type GCObject {
    id : Int
    data : String
    references : Array[Int] // IDs of referenced objects
    marked : Bool
  }
  
  type GCSpace {
    objects : Array[GCObject]
    next_id : Int
  }
  
  let create_gc_space = fn() -> GCSpace {
    GCSpace { 
      objects: [], 
      next_id: 1 
    }
  }
  
  let allocate_object = fn(space : GCSpace, data : String, references : Array[Int]) -> (Int, GCSpace) {
    let id = space.next_id
    let obj = GCObject { 
      id: id, 
      data: data, 
      references: references, 
      marked: false 
    }
    
    let new_space = GCSpace {
      objects: space.objects.push(obj),
      next_id: space.next_id + 1
    }
    
    (id, new_space)
  }
  
  let mark_objects = fn(space : GCSpace, root_ids : Array[Int]) -> GCSpace {
    let mut new_objects = space.objects
    let mut to_mark = root_ids
    
    // Reset all marks
    for i = 0; i < new_objects.length(); i = i + 1 {
      new_objects[i] = GCObject {
        id: new_objects[i].id,
        data: new_objects[i].data,
        references: new_objects[i].references,
        marked: false
      }
    }
    
    // Mark reachable objects
    while to_mark.length() > 0 {
      let current_id = to_mark.shift()
      
      for i = 0; i < new_objects.length(); i = i + 1 {
        if new_objects[i].id == current_id && !new_objects[i].marked {
          new_objects[i] = GCObject {
            id: new_objects[i].id,
            data: new_objects[i].data,
            references: new_objects[i].references,
            marked: true
          }
          
          // Add references to mark list
          for ref_id in new_objects[i].references {
            to_mark.push(ref_id)
          }
          break
        }
      }
    }
    
    GCSpace {
      objects: new_objects,
      next_id: space.next_id
    }
  }
  
  let sweep_objects = fn(space : GCSpace) -> (Int, GCSpace) {
    let mut survivors = []
    let mut collected = 0
    
    for obj in space.objects {
      if obj.marked {
        survivors.push(GCObject {
          id: obj.id,
          data: obj.data,
          references: obj.references,
          marked: false // Reset mark for next GC
        })
      } else {
        collected = collected + 1
      }
    }
    
    (collected, GCSpace {
      objects: survivors,
      next_id: space.next_id
    })
  }
  
  let garbage_collect = fn(space : GCSpace, root_ids : Array[Int]) -> (Int, GCSpace) {
    let marked_space = mark_objects(space, root_ids)
    sweep_objects(marked_space)
  }
  
  // Test garbage collection
  let space = create_gc_space()
  
  // Allocate objects
  let (id1, space1) = allocate_object(space, "Root", [])
  let (id2, space2) = allocate_object(space1, "Child1", [id1])
  let (id3, space3) = allocate_object(space2, "Child2", [id1])
  let (id4, space4) = allocate_object(space3, "Grandchild1", [id2])
  let (id5, space5) = allocate_object(space4, "Grandchild2", [id2])
  let (id6, space6) = allocate_object(space5, "Orphan", [])
  
  assert_eq(space6.objects.length(), 6)
  
  // Run garbage collection with id1 as root
  let (collected, space7) = garbage_collect(space6, [id1])
  
  // Objects 1, 2, 3, 4, 5 should survive (reachable from root)
  // Object 6 should be collected (orphan)
  assert_eq(collected, 1)
  assert_eq(space7.objects.length(), 5)
  
  // Verify remaining objects
  let mut remaining_ids = []
  for obj in space7.objects {
    remaining_ids.push(obj.id)
  }
  
  assert_true(remaining_ids.contains(id1))
  assert_true(remaining_ids.contains(id2))
  assert_true(remaining_ids.contains(id3))
  assert_true(remaining_ids.contains(id4))
  assert_true(remaining_ids.contains(id5))
  assert_false(remaining_ids.contains(id6))
  
  // Run GC with no roots (collect everything)
  let (collected2, space8) = garbage_collect(space7, [])
  assert_eq(collected2, 5)
  assert_eq(space8.objects.length(), 0)
}