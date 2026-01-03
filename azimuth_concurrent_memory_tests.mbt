// Azimuth Concurrent Safety and Memory Management Tests
// This file contains comprehensive test cases for concurrent operations and memory management

test "thread-safe counter simulation" {
  // Simulate thread-safe counter using atomic operations (conceptual)
  type AtomicCounter {
    value : Int
    lock : Bool
  }
  
  let create_atomic_counter = fn(initial_value : Int) -> AtomicCounter {
    { value: initial_value, lock: false }
  }
  
  let atomic_increment = fn(counter : AtomicCounter) -> AtomicCounter {
    // Simulate atomic increment with lock
    if not(counter.lock) {
      let locked_counter = { value: counter.value, lock: true }
      let new_value = locked_counter.value + 1
      { value: new_value, lock: false }
    } else {
      counter  // Would wait in real implementation
    }
  }
  
  let counter = create_atomic_counter(0)
  
  // Simulate multiple increments
  let counter1 = atomic_increment(counter)
  let counter2 = atomic_increment(counter1)
  let counter3 = atomic_increment(counter2)
  
  assert_eq(counter3.value, 3)
  assert_false(counter3.lock)
}

test "shared resource access control" {
  // Simulate shared resource with access control
  type SharedResource {
    data : Array[String]
    readers : Int
    writers : Int
    lock : Bool
  }
  
  let create_shared_resource = fn() -> SharedResource {
    { data: [], readers: 0, writers: 0, lock: false }
  }
  
  let acquire_read_lock = fn(resource : SharedResource) -> SharedResource {
    if not(resource.lock) and resource.writers == 0 {
      { 
        data: resource.data, 
        readers: resource.readers + 1, 
        writers: resource.writers, 
        lock: false 
      }
    } else {
      resource  // Would wait in real implementation
    }
  }
  
  let release_read_lock = fn(resource : SharedResource) -> SharedResource {
    { 
      data: resource.data, 
      readers: resource.readers - 1, 
      writers: resource.writers, 
      lock: false 
    }
  }
  
  let acquire_write_lock = fn(resource : SharedResource) -> SharedResource {
    if not(resource.lock) and resource.readers == 0 and resource.writers == 0 {
      { 
        data: resource.data, 
        readers: resource.readers, 
        writers: resource.writers + 1, 
        lock: true 
      }
    } else {
      resource  // Would wait in real implementation
    }
  }
  
  let release_write_lock = fn(resource : SharedResource) -> SharedResource {
    { 
      data: resource.data, 
      readers: resource.readers, 
      writers: resource.writers - 1, 
      lock: false 
    }
  }
  
  // Test read lock acquisition
  let resource = create_shared_resource()
  let resource_with_reader = acquire_read_lock(resource)
  assert_eq(resource_with_reader.readers, 1)
  assert_eq(resource_with_reader.writers, 0)
  
  // Test multiple readers
  let resource_with_multiple_readers = acquire_read_lock(resource_with_reader)
  assert_eq(resource_with_multiple_readers.readers, 2)
  
  // Test write lock rejection when readers are present
  let write_attempt = acquire_write_lock(resource_with_multiple_readers)
  assert_eq(write_attempt.writers, 0)  // Should not acquire write lock
  assert_false(write_attempt.lock)
  
  // Release readers and acquire write lock
  let resource_without_readers = release_read_lock(release_read_lock(resource_with_multiple_readers))
  let resource_with_writer = acquire_write_lock(resource_without_readers)
  
  assert_eq(resource_with_writer.readers, 0)
  assert_eq(resource_with_writer.writers, 1)
  assert_true(resource_with_writer.lock)
}

test "deadlock prevention simulation" {
  // Simulate deadlock prevention using resource ordering
  type Resource {
    id : Int
    locked : Bool
  }
  
  let create_resource = fn(id : Int) -> Resource {
    { id: id, locked: false }
  }
  
  let acquire_resource = fn(resource : Resource) -> Resource {
    if not(resource.locked) {
      { id: resource.id, locked: true }
    } else {
      resource  // Would wait in real implementation
    }
  }
  
  let release_resource = fn(resource : Resource) -> Resource {
    { id: resource.id, locked: false }
  }
  
  let resource1 = create_resource(1)
  let resource2 = create_resource(2)
  
  // Safe acquisition: always acquire resources in order of ID
  let locked1 = acquire_resource(resource1)
  let locked2 = acquire_resource(resource2)
  
  assert_true(locked1.locked)
  assert_true(locked2.locked)
  
  // Release in reverse order
  let released2 = release_resource(locked2)
  let released1 = release_resource(locked1)
  
  assert_false(released1.locked)
  assert_false(released2.locked)
  
  // Test deadlock scenario (prevented by ordering)
  let another_resource1 = create_resource(1)
  let another_resource2 = create_resource(2)
  
  // Both threads try to acquire in the same order (1 then 2)
  let thread1_locked1 = acquire_resource(another_resource1)
  let thread1_locked2 = acquire_resource(thread1_locked1)
  
  // Thread 2 would wait for resource 1 instead of acquiring resource 2
  let thread2_locked1 = acquire_resource(another_resource2)  // This would wait in real scenario
  
  assert_true(thread1_locked1.locked)
  assert_true(thread1_locked2.locked)
}

test "memory allocation and deallocation simulation" {
  // Simulate memory pool management
  type MemoryPool {
    free_blocks : Array[Int]
    allocated_blocks : Array[Int]
    total_blocks : Int
  }
  
  let create_memory_pool = fn(size : Int) -> MemoryPool {
    let mut free_blocks = []
    for i in 0..size {
      free_blocks = free_blocks.push(i)
    }
    { 
      free_blocks: free_blocks, 
      allocated_blocks: [], 
      total_blocks: size 
    }
  }
  
  let allocate_block = fn(pool : MemoryPool) -> (MemoryPool, Option[Int]) {
    if pool.free_blocks.length() > 0 {
      let block_id = pool.free_blocks[0]
      let new_free_blocks = pool.free_blocks.slice(1, pool.free_blocks.length())
      let new_allocated_blocks = pool.allocated_blocks.push(block_id)
      
      ({ 
        free_blocks: new_free_blocks, 
        allocated_blocks: new_allocated_blocks, 
        total_blocks: pool.total_blocks 
      }, Some(block_id))
    } else {
      (pool, None)
    }
  }
  
  let deallocate_block = fn(pool : MemoryPool, block_id : Int) -> MemoryPool {
    // Remove from allocated blocks
    let mut new_allocated_blocks = []
    for block in pool.allocated_blocks {
      if block != block_id {
        new_allocated_blocks = new_allocated_blocks.push(block)
      }
    }
    
    // Add to free blocks
    let new_free_blocks = pool.free_blocks.push(block_id)
    
    { 
      free_blocks: new_free_blocks, 
      allocated_blocks: new_allocated_blocks, 
      total_blocks: pool.total_blocks 
    }
  }
  
  // Test memory pool operations
  let pool = create_memory_pool(10)
  assert_eq(pool.free_blocks.length(), 10)
  assert_eq(pool.allocated_blocks.length(), 0)
  
  // Allocate blocks
  let (pool1, block1) = allocate_block(pool)
  let (pool2, block2) = allocate_block(pool1)
  let (pool3, block3) = allocate_block(pool2)
  
  match block1 {
    Some(id) => assert_eq(id, 0)
    None => assert_true(false)
  }
  
  match block2 {
    Some(id) => assert_eq(id, 1)
    None => assert_true(false)
  }
  
  match block3 {
    Some(id) => assert_eq(id, 2)
    None => assert_true(false)
  }
  
  assert_eq(pool3.free_blocks.length(), 7)
  assert_eq(pool3.allocated_blocks.length(), 3)
  
  // Deallocate blocks
  let pool4 = deallocate_block(pool3, 1)
  let pool5 = deallocate_block(pool4, 0)
  
  assert_eq(pool5.free_blocks.length(), 9)
  assert_eq(pool5.allocated_blocks.length(), 1)
  
  // Test exhaustion
  let mut exhausted_pool = create_memory_pool(2)
  let (exhausted_pool1, _) = allocate_block(exhausted_pool)
  let (exhausted_pool2, _) = allocate_block(exhausted_pool1)
  let (_, exhausted_block) = allocate_block(exhausted_pool2)
  
  match exhausted_block {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

test "memory leak detection simulation" {
  // Simulate memory leak tracking
  type MemoryTracker {
    allocations : Array[(Int, String)]  // (id, description)
    deallocations : Array[Int]
    next_id : Int
  }
  
  let create_memory_tracker = fn() -> MemoryTracker {
    { allocations: [], deallocations: [], next_id: 0 }
  }
  
  let track_allocation = fn(tracker : MemoryTracker, description : String) -> (MemoryTracker, Int) {
    let id = tracker.next_id
    let new_allocations = tracker.allocations.push((id, description))
    
    ({ 
      allocations: new_allocations, 
      deallocations: tracker.deallocations, 
      next_id: id + 1 
    }, id)
  }
  
  let track_deallocation = fn(tracker : MemoryTracker, id : Int) -> MemoryTracker {
    let new_deallocations = tracker.deallocations.push(id)
    
    { 
      allocations: tracker.allocations, 
      deallocations: new_deallocations, 
      next_id: tracker.next_id 
    }
  }
  
  let detect_leaks = fn(tracker : MemoryTracker) -> Array[(Int, String)] {
    let mut leaks = []
    
    for allocation in tracker.allocations {
      match allocation {
        (id, description) => {
          let mut deallocated = false
          for deallocation_id in tracker.deallocations {
            if deallocation_id == id {
              deallocated = true
              break
            }
          }
          
          if not(deallocated) {
            leaks = leaks.push((id, description))
          }
        }
      }
    }
    
    leaks
  }
  
  // Test memory tracking
  let tracker = create_memory_tracker()
  
  // Allocate some memory
  let (tracker1, id1) = track_allocation(tracker, "buffer1")
  let (tracker2, id2) = track_allocation(tracker1, "buffer2")
  let (tracker3, id3) = track_allocation(tracker2, "buffer3")
  
  // Deallocate some memory
  let tracker4 = track_deallocation(tracker3, id2)
  
  // Check for leaks
  let leaks = detect_leaks(tracker4)
  assert_eq(leaks.length(), 2)  // buffer1 and buffer3 should still be allocated
  
  // Deallocate remaining memory
  let tracker5 = track_deallocation(tracker4, id1)
  let tracker6 = track_deallocation(tracker5, id3)
  
  // Check for leaks again
  let no_leaks = detect_leaks(tracker6)
  assert_eq(no_leaks.length(), 0)
}

test "garbage collection simulation" {
  // Simulate simple garbage collection
  type GCObject {
    id : Int
    references : Array[Int]  // IDs of objects this object references
    marked : Bool
  }
  
  type GCSpace {
    objects : Array[GCObject]
    roots : Array[Int]  // Root object IDs
    next_id : Int
  }
  
  let create_gc_space = fn() -> GCSpace {
    { objects: [], roots: [], next_id: 0 }
  }
  
  let allocate_object = fn(space : GCSpace, references : Array[Int]) -> (GCSpace, Int) {
    let id = space.next_id
    let object = { id: id, references: references, marked: false }
    let new_objects = space.objects.push(object)
    
    ({ 
      objects: new_objects, 
      roots: space.roots, 
      next_id: id + 1 
    }, id)
  }
  
  let add_root = fn(space : GCSpace, id : Int) -> GCSpace {
    { 
      objects: space.objects, 
      roots: space.roots.push(id), 
      next_id: space.next_id 
    }
  }
  
  let mark_phase = fn(space : GCSpace) -> GCSpace {
    // Reset all marks
    let mut reset_objects = []
    for object in space.objects {
      reset_objects = reset_objects.push({ 
        id: object.id, 
        references: object.references, 
        marked: false 
      })
    }
    
    // Mark from roots
    let mut to_mark = space.roots
    let mut marked_objects = reset_objects
    
    while to_mark.length() > 0 {
      let mark_id = to_mark[0]
      to_mark = to_mark.slice(1, to_mark.length())
      
      // Find and mark the object
      let mut new_marked_objects = []
      for object in marked_objects {
        if object.id == mark_id and not(object.marked) {
          // Add its references to the marking queue
          for ref_id in object.references {
            to_mark = to_mark.push(ref_id)
          }
          
          new_marked_objects = new_marked_objects.push({ 
            id: object.id, 
            references: object.references, 
            marked: true 
          })
        } else {
          new_marked_objects = new_marked_objects.push(object)
        }
      }
      marked_objects = new_marked_objects
    }
    
    { 
      objects: marked_objects, 
      roots: space.roots, 
      next_id: space.next_id 
    }
  }
  
  let sweep_phase = fn(space : GCSpace) -> (GCSpace, Array[Int]) {
    let mut surviving_objects = []
    let mut collected_ids = []
    
    for object in space.objects {
      if object.marked {
        surviving_objects = surviving_objects.push(object)
      } else {
        collected_ids = collected_ids.push(object.id)
      }
    }
    
    ({ 
      objects: surviving_objects, 
      roots: space.roots, 
      next_id: space.next_id 
    }, collected_ids)
  }
  
  // Test garbage collection
  let space = create_gc_space()
  
  // Create objects with references
  let (space1, obj1) = allocate_object(space, [])  // No references
  let (space2, obj2) = allocate_object(space1, [obj1])  // References obj1
  let (space3, obj3) = allocate_object(space2, [])  // No references, not reachable
  let (space4, obj4) = allocate_object(space3, [obj2])  // References obj2
  
  // Set roots
  let space_with_roots = add_root(space4, obj4)
  
  assert_eq(space_with_roots.objects.length(), 4)
  
  // Run garbage collection
  let marked_space = mark_phase(space_with_roots)
  let (final_space, collected) = sweep_phase(marked_space)
  
  // obj3 should be collected (not reachable from roots)
  assert_eq(collected.length(), 1)
  assert_eq(collected[0], obj3)
  assert_eq(final_space.objects.length(), 3)
}

test "buffer overflow protection simulation" {
  // Simulate buffer with overflow protection
  type SafeBuffer {
    data : Array[Char]
    capacity : Int
    size : Int
  }
  
  let create_safe_buffer = fn(capacity : Int) -> SafeBuffer {
    { data: [], capacity: capacity, size: 0 }
  }
  
  let safe_write = fn(buffer : SafeBuffer, data : Array[Char]) -> Result[SafeBuffer, String] {
    let required_space = buffer.size + data.length()
    
    if required_space <= buffer.capacity {
      let mut new_data = buffer.data
      for char in data {
        new_data = new_data.push(char)
      }
      
      Ok({ 
        data: new_data, 
        capacity: buffer.capacity, 
        size: required_space 
      })
    } else {
      Err("Buffer overflow: insufficient capacity")
    }
  }
  
  let safe_read = fn(buffer : SafeBuffer, offset : Int, length : Int) -> Result[Array[Char], String] {
    if offset >= 0 and length >= 0 and offset + length <= buffer.size {
      Ok(buffer.data.slice(offset, offset + length))
    } else {
      Err("Buffer read error: invalid offset or length")
    }
  }
  
  // Test buffer operations
  let buffer = create_safe_buffer(10)
  
  // Test safe write
  let write_result = safe_write(buffer, "hello".to_char_array())
  match write_result {
    Ok(new_buffer) => {
      assert_eq(new_buffer.size, 5)
      assert_eq(new_buffer.capacity, 10)
    }
    Err(_) => assert_true(false)
  }
  
  // Test overflow protection
  let large_data = "this is too long for the buffer".to_char_array()
  let overflow_result = safe_write(buffer, large_data)
  match overflow_result {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Buffer overflow: insufficient capacity")
  }
  
  // Test safe read
  match write_result {
    Ok(written_buffer) => {
      let read_result = safe_read(written_buffer, 1, 3)
      match read_result {
        Ok(data) => {
          assert_eq(data.length(), 3)
          assert_eq(data[0], 'e')
          assert_eq(data[1], 'l')
          assert_eq(data[2], 'l')
        }
        Err(_) => assert_true(false)
      }
      
      // Test read bounds checking
      let out_of_bounds_read = safe_read(written_buffer, 1, 10)
      match out_of_bounds_read {
        Ok(_) => assert_true(false)
        Err(error) => assert_eq(error, "Buffer read error: invalid offset or length")
      }
    }
    Err(_) => assert_true(false)
  }
}