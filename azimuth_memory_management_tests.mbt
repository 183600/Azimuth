// Azimuth Memory Management Tests
// This file contains tests for memory management patterns and resource handling

// Test 1: Resource Acquisition and Release
test "resource acquisition and release patterns" {
  type Resource = {
    id : Int
    is_acquired : Bool
    data : String
  }
  
  let create_resource = fn(id : Int) {
    Resource {
      id: id,
      is_acquired: true,
      data: "Resource data " + id.to_string()
    }
  }
  
  let release_resource = fn(resource : Resource) {
    { resource | is_acquired: false }
  }
  
  let use_resource = fn(resource : Resource, operation : (String) -> String) {
    if resource.is_acquired {
      operation(resource.data)
    } else {
      "Resource not available"
    }
  }
  
  // Test resource lifecycle
  let resource = create_resource(1)
  assert_true(resource.is_acquired)
  assert_eq(resource.data, "Resource data 1")
  
  let result = use_resource(resource, fn(data) { "Processed: " + data })
  assert_eq(result, "Processed: Resource data 1")
  
  let released = release_resource(resource)
  assert_false(released.is_acquired)
  
  let result_after_release = use_resource(released, fn(data) { "Processed: " + data })
  assert_eq(result_after_release, "Resource not available")
}

// Test 2: Memory Pool Management
test "memory pool management" {
  type PooledObject = {
    id : Int
    data : String
    in_use : Bool
  }
  
  type MemoryPool = {
    objects : [PooledObject]
    next_id : Int
  }
  
  let create_memory_pool = fn(initial_size : Int) {
    let mut objects = []
    for i in 0..<initial_size {
      objects = objects.push(PooledObject {
        id: i,
        data: "Object " + i.to_string(),
        in_use: false
      })
    }
    
    MemoryPool {
      objects: objects,
      next_id: initial_size
    }
  }
  
  let acquire_from_pool = fn(pool : MemoryPool) {
    let mut updated_pool = pool
    let mut acquired_object = None
    
    for i in 0..<updated_pool.objects.length() {
      if !updated_pool.objects[i].in_use {
        updated_pool.objects[i] = { updated_pool.objects[i] | in_use: true }
        acquired_object = Some(updated_pool.objects[i])
        break
      }
    }
    
    match acquired_object {
      Some(obj) => (Some(obj), updated_pool),
      None => {
        // Create new object if pool is exhausted
        let new_object = PooledObject {
          id: updated_pool.next_id,
          data: "Object " + updated_pool.next_id.to_string(),
          in_use: true
        }
        updated_pool.objects = updated_pool.objects.push(new_object)
        updated_pool.next_id = updated_pool.next_id + 1
        (Some(new_object), updated_pool)
      }
    }
  }
  
  let release_to_pool = fn(pool : MemoryPool, object_id : Int) {
    let mut updated_pool = pool
    for i in 0..<updated_pool.objects.length() {
      if updated_pool.objects[i].id == object_id {
        updated_pool.objects[i] = { updated_pool.objects[i] | in_use: false }
        break
      }
    }
    updated_pool
  }
  
  let get_pool_stats = fn(pool : MemoryPool) {
    let mut in_use_count = 0
    let mut available_count = 0
    
    for obj in pool.objects {
      if obj.in_use {
        in_use_count = in_use_count + 1
      } else {
        available_count = available_count + 1
      }
    }
    
    (in_use_count, available_count, pool.objects.length())
  }
  
  // Test memory pool operations
  let pool = create_memory_pool(3)
  let (in_use, available, total) = get_pool_stats(pool)
  assert_eq(in_use, 0)
  assert_eq(available, 3)
  assert_eq(total, 3)
  
  // Acquire objects
  let (obj1, pool1) = acquire_from_pool(pool)
  let (obj2, pool2) = acquire_from_pool(pool1)
  let (obj3, pool3) = acquire_from_pool(pool2)
  
  match obj1 {
    Some(o) => assert_true(o.in_use),
    None => assert_true(false)
  }
  
  let (in_use2, available2, total2) = get_pool_stats(pool3)
  assert_eq(in_use2, 3)
  assert_eq(available2, 0)
  assert_eq(total2, 3)
  
  // Acquire beyond pool capacity (should create new object)
  let (obj4, pool4) = acquire_from_pool(pool3)
  let (in_use3, available3, total3) = get_pool_stats(pool4)
  assert_eq(in_use3, 4)
  assert_eq(available3, 0)
  assert_eq(total3, 4)
  
  // Release objects
  match obj1 {
    Some(o) => {
      let pool5 = release_to_pool(pool4, o.id)
      let (in_use4, available4, total4) = get_pool_stats(pool5)
      assert_eq(in_use4, 3)
      assert_eq(available4, 1)
      assert_eq(total4, 4)
    }
    None => assert_true(false)
  }
}

// Test 3: Garbage Collection Simulation
test "garbage collection simulation" {
  type GCObject = {
    id : Int
    references : Int
    data : String
    marked : Bool
  }
  
  type GCSimulator = {
    objects : [GCObject]
    next_id : Int
  }
  
  let create_gc_simulator = fn() {
    GCSimulator {
      objects: [],
      next_id: 0
    }
  }
  
  let allocate_object = fn(simulator : GCSimulator, data : String) {
    let new_object = GCObject {
      id: simulator.next_id,
      references: 1,
      data: data,
      marked: false
    }
    
    {
      simulator |
      objects: simulator.objects.push(new_object),
      next_id: simulator.next_id + 1
    }
  }
  
  let add_reference = fn(simulator : GCSimulator, object_id : Int) {
    let mut updated_objects = simulator.objects
    for i in 0..<updated_objects.length() {
      if updated_objects[i].id == object_id {
        updated_objects[i] = { updated_objects[i] | references: updated_objects[i].references + 1 }
        break
      }
    }
    { simulator | objects: updated_objects }
  }
  
  let remove_reference = fn(simulator : GCSimulator, object_id : Int) {
    let mut updated_objects = simulator.objects
    for i in 0..<updated_objects.length() {
      if updated_objects[i].id == object_id {
        updated_objects[i] = { updated_objects[i] | references: updated_objects[i].references - 1 }
        break
      }
    }
    { simulator | objects: updated_objects }
  }
  
  let mark_objects = fn(simulator : GCSimulator, root_objects : [Int]) {
    let mut updated_objects = simulator.objects
    
    // Mark root objects
    for root_id in root_objects {
      for i in 0..<updated_objects.length() {
        if updated_objects[i].id == root_id {
          updated_objects[i] = { updated_objects[i] | marked: true }
          break
        }
      }
    }
    
    { simulator | objects: updated_objects }
  }
  
  let sweep_objects = fn(simulator : GCSimulator) {
    let mut remaining_objects = []
    let mut collected_count = 0
    
    for obj in simulator.objects {
      if obj.marked || obj.references > 0 {
        remaining_objects = remaining_objects.push({ obj | marked: false })
      } else {
        collected_count = collected_count + 1
      }
    }
    
    (collected_count, { simulator | objects: remaining_objects })
  }
  
  // Test garbage collection
  let simulator = create_gc_simulator()
  
  // Allocate objects
  let sim1 = allocate_object(simulator, "Object 1")
  let sim2 = allocate_object(sim1, "Object 2")
  let sim3 = allocate_object(sim2, "Object 3")
  
  assert_eq(sim3.objects.length(), 3)
  
  // Add references
  let sim4 = add_reference(sim3, 0) // Object 0 now has 2 references
  let sim5 = add_reference(sim4, 1) // Object 1 now has 2 references
  
  // Remove all references from object 2
  let sim6 = remove_reference(sim5, 2)
  
  // Mark root objects (0 and 1)
  let sim7 = mark_objects(sim6, [0, 1])
  
  // Sweep unmarked objects
  let (collected, final_sim) = sweep_objects(sim7)
  
  assert_eq(collected, 1) // Object 2 should be collected
  assert_eq(final_sim.objects.length(), 2)
}

// Test 4: Memory Leak Detection
test "memory leak detection" {
  type LeakTracker = {
    allocations : [(Int, String)] // (id, description)
    deallocations : [Int]
    next_id : Int
  }
  
  let create_leak_tracker = fn() {
    LeakTracker {
      allocations: [],
      deallocations: [],
      next_id: 0
    }
  }
  
  let track_allocation = fn(tracker : LeakTracker, description : String) {
    let allocation_id = tracker.next_id
    {
      tracker |
      allocations: tracker.allocations.push((allocation_id, description)),
      next_id: tracker.next_id + 1
    }
  }
  
  let track_deallocation = fn(tracker : LeakTracker, allocation_id : Int) {
    { tracker | deallocations: tracker.deallocations.push(allocation_id) }
  }
  
  let detect_leaks = fn(tracker : LeakTracker) {
    let mut leaked_allocations = []
    
    for (alloc_id, description) in tracker.allocations {
      let mut is_deallocated = false
      for dealloc_id in tracker.deallocations {
        if alloc_id == dealloc_id {
          is_deallocated = true
          break
        }
      }
      
      if !is_deallocated {
        leaked_allocations = leaked_allocations.push((alloc_id, description))
      }
    }
    
    leaked_allocations
  }
  
  // Test leak detection
  let tracker = create_leak_tracker()
  
  // Allocate resources
  let tracker1 = track_allocation(tracker, "Resource A")
  let tracker2 = track_allocation(tracker1, "Resource B")
  let tracker3 = track_allocation(tracker2, "Resource C")
  
  // Deallocate some resources
  let tracker4 = track_deallocation(tracker3, 1) // Deallocate Resource B
  
  // Detect leaks
  let leaks = detect_leaks(tracker4)
  assert_eq(leaks.length(), 2) // Resource A and C should be leaked
  
  // Deallocate remaining resources
  let tracker5 = track_deallocation(tracker4, 0) // Deallocate Resource A
  let tracker6 = track_deallocation(tracker5, 2) // Deallocate Resource C
  
  // No leaks should remain
  let no_leaks = detect_leaks(tracker6)
  assert_eq(no_leaks.length(), 0)
}

// Test 5: Buffer Management
test "buffer management and overflow protection" {
  type Buffer = {
    data : [Char]
    capacity : Int
    size : Int
  }
  
  let create_buffer = fn(capacity : Int) {
    Buffer {
      data: [],
      capacity: capacity,
      size: 0
    }
  }
  
  let buffer_write = fn(buffer : Buffer, chars : [Char]) {
    let available_space = buffer.capacity - buffer.size
    let chars_to_write = if chars.length() <= available_space {
      chars.length()
    } else {
      available_space
    }
    
    let new_data = buffer.data.concat(chars.slice(0, chars_to_write))
    
    {
      buffer |
      data: new_data,
      size: buffer.size + chars_to_write
    }
  }
  
  let buffer_read = fn(buffer : Buffer, count : Int) {
    let chars_to_read = if count <= buffer.size {
      count
    } else {
      buffer.size
    }
    
    let read_data = buffer.data.slice(0, chars_to_read)
    let remaining_data = buffer.data.slice(chars_to_read, buffer.size)
    
    (read_data, {
      buffer |
      data: remaining_data,
      size: buffer.size - chars_to_read
    })
  }
  
  let is_buffer_full = fn(buffer : Buffer) {
    buffer.size >= buffer.capacity
  }
  
  let is_buffer_empty = fn(buffer : Buffer) {
    buffer.size == 0
  }
  
  // Test buffer operations
  let buffer = create_buffer(5)
  assert_eq(buffer.capacity, 5)
  assert_eq(buffer.size, 0)
  assert_true(is_buffer_empty(buffer))
  assert_false(is_buffer_full(buffer))
  
  // Write to buffer
  let chars1 = ['H', 'e', 'l', 'l', 'o']
  let buffer1 = buffer_write(buffer, chars1)
  assert_eq(buffer1.size, 5)
  assert_true(is_buffer_full(buffer1))
  
  // Try to write beyond capacity
  let chars2 = ['W', 'o', 'r', 'l', 'd']
  let buffer2 = buffer_write(buffer1, chars2)
  assert_eq(buffer2.size, 5) // Should not exceed capacity
  
  // Read from buffer
  let (read_chars, buffer3) = buffer_read(buffer2, 3)
  assert_eq(read_chars, ['H', 'e', 'l'])
  assert_eq(buffer3.size, 2)
  assert_false(is_buffer_full(buffer3))
  assert_false(is_buffer_empty(buffer3))
  
  // Read remaining data
  let (remaining_chars, buffer4) = buffer_read(buffer3, 10) // Try to read more than available
  assert_eq(remaining_chars, ['l', 'o'])
  assert_true(is_buffer_empty(buffer4))
}

// Test 6: Stack vs Heap Allocation Simulation
test "stack vs heap allocation simulation" {
  type StackObject = {
    value : Int
    scope_level : Int
  }
  
  type HeapObject = {
    id : Int
    data : String
    reference_count : Int
  }
  
  type MemoryManager = {
    stack : [StackObject]
    heap : [HeapObject]
    current_scope : Int
    next_heap_id : Int
  }
  
  let create_memory_manager = fn() {
    MemoryManager {
      stack: [],
      heap: [],
      current_scope: 0,
      next_heap_id: 0
    }
  }
  
  let enter_scope = fn(manager : MemoryManager) {
    { manager | current_scope: manager.current_scope + 1 }
  }
  
  let exit_scope = fn(manager : MemoryManager) {
    let mut new_stack = []
    let current_scope = manager.current_scope
    
    // Remove stack objects from current scope
    for obj in manager.stack {
      if obj.scope_level < current_scope {
        new_stack = new_stack.push(obj)
      }
    }
    
    { manager | stack: new_stack, current_scope: current_scope - 1 }
  }
  
  let allocate_stack = fn(manager : MemoryManager, value : Int) {
    let stack_obj = StackObject {
      value: value,
      scope_level: manager.current_scope
    }
    
    { manager | stack: manager.stack.push(stack_obj) }
  }
  
  let allocate_heap = fn(manager : MemoryManager, data : String) {
    let heap_obj = HeapObject {
      id: manager.next_heap_id,
      data: data,
      reference_count: 1
    }
    
    {
      manager |
      heap: manager.heap.push(heap_obj),
      next_heap_id: manager.next_heap_id + 1
    }
  }
  
  let add_reference = fn(manager : MemoryManager, heap_id : Int) {
    let mut updated_heap = manager.heap
    for i in 0..<updated_heap.length() {
      if updated_heap[i].id == heap_id {
        updated_heap[i] = { updated_heap[i] | reference_count: updated_heap[i].reference_count + 1 }
        break
      }
    }
    { manager | heap: updated_heap }
  }
  
  let remove_reference = fn(manager : MemoryManager, heap_id : Int) {
    let mut updated_heap = manager.heap
    for i in 0..<updated_heap.length() {
      if updated_heap[i].id == heap_id {
        updated_heap[i] = { updated_heap[i] | reference_count: updated_heap[i].reference_count - 1 }
        break
      }
    }
    { manager | heap: updated_heap }
  }
  
  let garbage_collect_heap = fn(manager : MemoryManager) {
    let mut remaining_objects = []
    let mut collected_count = 0
    
    for obj in manager.heap {
      if obj.reference_count > 0 {
        remaining_objects = remaining_objects.push(obj)
      } else {
        collected_count = collected_count + 1
      }
    }
    
    (collected_count, { manager | heap: remaining_objects })
  }
  
  // Test stack and heap allocation
  let manager = create_memory_manager()
  
  // Enter scope 1
  let manager1 = enter_scope(manager)
  assert_eq(manager1.current_scope, 1)
  
  // Allocate stack objects
  let manager2 = allocate_stack(manager1, 10)
  let manager3 = allocate_stack(manager2, 20)
  assert_eq(manager3.stack.length(), 2)
  
  // Allocate heap objects
  let manager4 = allocate_heap(manager3, "Heap Data 1")
  let manager5 = allocate_heap(manager4, "Heap Data 2")
  assert_eq(manager5.heap.length(), 2)
  
  // Add reference to heap object
  let manager6 = add_reference(manager5, 0)
  
  // Enter scope 2
  let manager7 = enter_scope(manager6)
  assert_eq(manager7.current_scope, 2)
  
  // Allocate more stack objects
  let manager8 = allocate_stack(manager7, 30)
  assert_eq(manager8.stack.length(), 3)
  
  // Exit scope 2 (should remove scope 2 stack objects)
  let manager9 = exit_scope(manager8)
  assert_eq(manager9.stack.length(), 2) // Only scope 1 objects remain
  assert_eq(manager9.current_scope, 1)
  
  // Remove reference from heap object
  let manager10 = remove_reference(manager9, 0)
  
  // Garbage collect heap
  let (collected, final_manager) = garbage_collect_heap(manager10)
  assert_eq(collected, 1) // One object should be collected
  assert_eq(final_manager.heap.length(), 1)
}

// Test 7: Memory Fragmentation Simulation
test "memory fragmentation simulation" {
  type MemoryBlock = {
    id : Int
    size : Int
    is_free : Bool
    data : String
  }
  
  type MemoryHeap = {
    blocks : [MemoryBlock]
    total_size : Int
    free_size : Int
    next_id : Int
  }
  
  let create_memory_heap = fn(total_size : Int) {
    let initial_block = MemoryBlock {
      id: 0,
      size: total_size,
      is_free: true,
      data: ""
    }
    
    MemoryHeap {
      blocks: [initial_block],
      total_size: total_size,
      free_size: total_size,
      next_id: 1
    }
  }
  
  let allocate_block = fn(heap : MemoryHeap, requested_size : Int, data : String) {
    let mut updated_blocks = heap.blocks
    let mut allocated = false
    
    for i in 0..<updated_blocks.length() {
      if updated_blocks[i].is_free && updated_blocks[i].size >= requested_size {
        let block = updated_blocks[i]
        let remaining_size = block.size - requested_size
        
        if remaining_size > 0 {
          // Split the block
          let allocated_block = MemoryBlock {
            id: heap.next_id,
            size: requested_size,
            is_free: false,
            data: data
          }
          
          let remaining_block = MemoryBlock {
            id: block.id,
            size: remaining_size,
            is_free: true,
            data: ""
          }
          
          // Replace current block with allocated and remaining blocks
          let mut new_blocks = []
          for j in 0..<i {
            new_blocks = new_blocks.push(updated_blocks[j])
          }
          new_blocks = new_blocks.push(allocated_block)
          new_blocks = new_blocks.push(remaining_block)
          for j in (i+1)..<updated_blocks.length() {
            new_blocks = new_blocks.push(updated_blocks[j])
          }
          
          updated_blocks = new_blocks
        } else {
          // Use entire block
          updated_blocks[i] = {
            block |
            is_free: false,
            data: data,
            id: heap.next_id
          }
        }
        
        allocated = true
        break
      }
    }
    
    if allocated {
      let new_free_size = heap.free_size - requested_size
      (true, {
        heap |
        blocks: updated_blocks,
        free_size: new_free_size,
        next_id: heap.next_id + 1
      })
    } else {
      (false, heap)
    }
  }
  
  let free_block = fn(heap : MemoryHeap, block_id : Int) {
    let mut updated_blocks = heap.blocks
    let mut found = false
    
    for i in 0..<updated_blocks.length() {
      if updated_blocks[i].id == block_id {
        updated_blocks[i] = {
          updated_blocks[i] |
          is_free: true,
          data: ""
        }
        found = true
        break
      }
    }
    
    if found {
      // Coalesce adjacent free blocks
      let mut coalesced_blocks = []
      let i = 0
      while i < updated_blocks.length() {
        let current = updated_blocks[i]
        
        if current.is_free {
          let mut coalesced = current
          let mut j = i + 1
          
          while j < updated_blocks.length() && updated_blocks[j].is_free {
            coalesced = {
              coalesced |
              size: coalesced.size + updated_blocks[j].size
            }
            j = j + 1
          }
          
          coalesced_blocks = coalesced_blocks.push(coalesced)
          i = j
        } else {
          coalesced_blocks = coalesced_blocks.push(current)
          i = i + 1
        }
      }
      
      let freed_size = updated_blocks.find(fn(block) { block.id == block_id })
      match freed_size {
        Some(block) => {
          (true, {
            heap |
            blocks: coalesced_blocks,
            free_size: heap.free_size + block.size
          })
        }
        None => (false, heap)
      }
    } else {
      (false, heap)
    }
  }
  
  let get_fragmentation_count = fn(heap : MemoryHeap) {
    let mut free_blocks = 0
    for block in heap.blocks {
      if block.is_free {
        free_blocks = free_blocks + 1
      }
    }
    free_blocks
  }
  
  // Test memory fragmentation
  let heap = create_memory_heap(100)
  assert_eq(heap.total_size, 100)
  assert_eq(heap.free_size, 100)
  assert_eq(get_fragmentation_count(heap), 1)
  
  // Allocate blocks
  let (success1, heap1) = allocate_block(heap, 20, "Block 1")
  let (success2, heap2) = allocate_block(heap1, 30, "Block 2")
  let (success3, heap3) = allocate_block(heap2, 10, "Block 3")
  
  assert_true(success1)
  assert_true(success2)
  assert_true(success3)
  assert_eq(heap3.free_size, 40) // 100 - (20 + 30 + 10)
  
  // Free middle block (creates fragmentation)
  let (freed1, heap4) = free_block(heap3, 1) // Free Block 2
  assert_true(freed1)
  assert_eq(heap4.free_size, 70) // 40 + 30
  assert_eq(get_fragmentation_count(heap4), 2) // Two free blocks
  
  // Try to allocate a block that doesn't fit in fragmented space
  let (success4, heap5) = allocate_block(heap4, 50, "Block 4")
  assert_false(success4) // Should fail due to fragmentation
  
  // Allocate a smaller block that fits
  let (success5, heap6) = allocate_block(heap5, 25, "Block 4")
  assert_true(success5)
  
  // Free all blocks to coalesce
  let (freed2, heap7) = free_block(heap6, 0)
  let (freed3, heap8) = free_block(heap7, 2)
  let (freed4, heap9) = free_block(heap8, 3)
  
  assert_eq(heap9.free_size, 100)
  assert_eq(get_fragmentation_count(heap9), 1) // Should coalesce back to single block
}

// Test 8: Memory Usage Optimization
test "memory usage optimization" {
  type OptimizedArray[T] = {
    data : [T]
    capacity : Int
    size : Int
  }
  
  let create_optimized_array = fn[T](initial_capacity : Int) {
    OptimizedArray {
      data: [],
      capacity: initial_capacity,
      size: 0
    }
  }
  
  let optimized_push = fn[T](arr : OptimizedArray[T], item : T) {
    if arr.size < arr.capacity {
      {
        arr |
        data: arr.data.push(item),
        size: arr.size + 1
      }
    } else {
      // Need to resize - double capacity
      let new_capacity = arr.capacity * 2
      {
        arr |
        data: arr.data.push(item),
        capacity: new_capacity,
        size: arr.size + 1
      }
    }
  }
  
  let optimized_remove = fn[T](arr : OptimizedArray[T], index : Int) {
    if index >= 0 && index < arr.size {
      let mut new_data = []
      for i in 0..<arr.size {
        if i != index {
          new_data = new_data.push(arr.data[i])
        }
      }
      
      {
        arr |
        data: new_data,
        size: arr.size - 1
      }
    } else {
      arr
    }
  }
  
  let compact_if_needed = fn[T](arr : OptimizedArray[T]) {
    let utilization_ratio = arr.size.to_float() / arr.capacity.to_float()
    if utilization_ratio < 0.25 && arr.capacity > 4 {
      // Compact to reduce memory usage
      let new_capacity = arr.size * 2
      {
        arr |
        capacity: new_capacity
      }
    } else {
      arr
    }
  }
  
  // Test memory optimization
  let arr = create_optimized_array[String](2)
  assert_eq(arr.capacity, 2)
  assert_eq(arr.size, 0)
  
  // Add items (should trigger resize)
  let arr1 = optimized_push(arr, "item1")
  let arr2 = optimized_push(arr1, "item2")
  let arr3 = optimized_push(arr2, "item3") // Should resize to capacity 4
  
  assert_eq(arr3.capacity, 4)
  assert_eq(arr3.size, 3)
  
  // Remove items
  let arr4 = optimized_remove(arr3, 0)
  let arr5 = optimized_remove(arr4, 0)
  
  assert_eq(arr5.capacity, 4)
  assert_eq(arr5.size, 1)
  
  // Compact if needed
  let arr6 = compact_if_needed(arr5)
  assert_eq(arr6.capacity, 2) // Should compact from 4 to 2
  assert_eq(arr6.size, 1)
}

// Test 9: Reference Counting
test "reference counting for memory management" {
  type RefCountedObject = {
    id : Int
    data : String
    ref_count : Int
  }
  
  type RefCountManager = {
    objects : [RefCountedObject]
    next_id : Int
  }
  
  let create_ref_manager = fn() {
    RefCountManager {
      objects: [],
      next_id: 0
    }
  }
  
  let create_object = fn(manager : RefCountManager, data : String) {
    let new_object = RefCountedObject {
      id: manager.next_id,
      data: data,
      ref_count: 1
    }
    
    {
      manager |
      objects: manager.objects.push(new_object),
      next_id: manager.next_id + 1
    }
  }
  
  let add_reference = fn(manager : RefCountManager, object_id : Int) {
    let mut updated_objects = manager.objects
    for i in 0..<updated_objects.length() {
      if updated_objects[i].id == object_id {
        updated_objects[i] = { updated_objects[i] | ref_count: updated_objects[i].ref_count + 1 }
        break
      }
    }
    { manager | objects: updated_objects }
  }
  
  let remove_reference = fn(manager : RefCountManager, object_id : Int) {
    let mut updated_objects = manager.objects
    for i in 0..<updated_objects.length() {
      if updated_objects[i].id == object_id {
        updated_objects[i] = { updated_objects[i] | ref_count: updated_objects[i].ref_count - 1 }
        break
      }
    }
    { manager | objects: updated_objects }
  }
  
  let cleanup_zero_refs = fn(manager : RefCountManager) {
    let mut remaining_objects = []
    let mut cleaned_count = 0
    
    for obj in manager.objects {
      if obj.ref_count > 0 {
        remaining_objects = remaining_objects.push(obj)
      } else {
        cleaned_count = cleaned_count + 1
      }
    }
    
    (cleaned_count, { manager | objects: remaining_objects })
  }
  
  let get_ref_count = fn(manager : RefCountManager, object_id : Int) {
    let mut count = 0
    for obj in manager.objects {
      if obj.id == object_id {
        count = obj.ref_count
        break
      }
    }
    count
  }
  
  // Test reference counting
  let manager = create_ref_manager()
  
  // Create objects
  let manager1 = create_object(manager, "Object 1")
  let manager2 = create_object(manager1, "Object 2")
  
  assert_eq(get_ref_count(manager2, 0), 1)
  assert_eq(get_ref_count(manager2, 1), 1)
  
  // Add references
  let manager3 = add_reference(manager2, 0)
  let manager4 = add_reference(manager3, 0)
  
  assert_eq(get_ref_count(manager4, 0), 3)
  assert_eq(get_ref_count(manager4, 1), 1)
  
  // Remove references
  let manager5 = remove_reference(manager4, 0)
  let manager6 = remove_reference(manager5, 0)
  
  assert_eq(get_ref_count(manager6, 0), 1)
  assert_eq(get_ref_count(manager6, 1), 1)
  
  // Remove all references from object 1
  let manager7 = remove_reference(manager6, 1)
  
  // Cleanup zero reference objects
  let (cleaned, manager8) = cleanup_zero_refs(manager7)
  assert_eq(cleaned, 1) // Object 1 should be cleaned up
  assert_eq(manager8.objects.length(), 1)
  
  // Verify remaining object
  assert_eq(get_ref_count(manager8, 0), 1)
}

// Test 10: Memory Pool with Different Sizes
test "memory pool with different sized blocks" {
  type BlockSize {
    Small
    Medium
    Large
  }
  
  type PooledBlock = {
    id : Int
    size : BlockSize
    in_use : Bool
    data : String
  }
  
  type MultiSizePool = {
    small_blocks : [PooledBlock]
    medium_blocks : [PooledBlock]
    large_blocks : [PooledBlock]
    next_id : Int
  }
  
  let create_multi_size_pool = fn(small_count : Int, medium_count : Int, large_count : Int) {
    let mut small_blocks = []
    let mut medium_blocks = []
    let mut large_blocks = []
    let mut next_id = 0
    
    for i in 0..<small_count {
      small_blocks = small_blocks.push(PooledBlock {
        id: next_id,
        size: BlockSize::Small,
        in_use: false,
        data: ""
      })
      next_id = next_id + 1
    }
    
    for i in 0..<medium_count {
      medium_blocks = medium_blocks.push(PooledBlock {
        id: next_id,
        size: BlockSize::Medium,
        in_use: false,
        data: ""
      })
      next_id = next_id + 1
    }
    
    for i in 0..<large_count {
      large_blocks = large_blocks.push(PooledBlock {
        id: next_id,
        size: BlockSize::Large,
        in_use: false,
        data: ""
      })
      next_id = next_id + 1
    }
    
    MultiSizePool {
      small_blocks: small_blocks,
      medium_blocks: medium_blocks,
      large_blocks: large_blocks,
      next_id: next_id
    }
  }
  
  let acquire_block = fn(pool : MultiSizePool, size : BlockSize, data : String) {
    match size {
      BlockSize::Small => {
        let mut updated_pool = pool
        let mut acquired_block = None
        
        for i in 0..<updated_pool.small_blocks.length() {
          if !updated_pool.small_blocks[i].in_use {
            updated_pool.small_blocks[i] = {
              updated_pool.small_blocks[i] |
              in_use: true,
              data: data
            }
            acquired_block = Some(updated_pool.small_blocks[i])
            break
          }
        }
        
        match acquired_block {
          Some(block) => (Some(block), updated_pool),
          None => (None, pool)
        }
      }
      BlockSize::Medium => {
        let mut updated_pool = pool
        let mut acquired_block = None
        
        for i in 0..<updated_pool.medium_blocks.length() {
          if !updated_pool.medium_blocks[i].in_use {
            updated_pool.medium_blocks[i] = {
              updated_pool.medium_blocks[i] |
              in_use: true,
              data: data
            }
            acquired_block = Some(updated_pool.medium_blocks[i])
            break
          }
        }
        
        match acquired_block {
          Some(block) => (Some(block), updated_pool),
          None => (None, pool)
        }
      }
      BlockSize::Large => {
        let mut updated_pool = pool
        let mut acquired_block = None
        
        for i in 0..<updated_pool.large_blocks.length() {
          if !updated_pool.large_blocks[i].in_use {
            updated_pool.large_blocks[i] = {
              updated_pool.large_blocks[i] |
              in_use: true,
              data: data
            }
            acquired_block = Some(updated_pool.large_blocks[i])
            break
          }
        }
        
        match acquired_block {
          Some(block) => (Some(block), updated_pool),
          None => (None, pool)
        }
      }
    }
  }
  
  let release_block = fn(pool : MultiSizePool, block_id : Int) {
    let mut updated_pool = pool
    
    // Check small blocks
    for i in 0..<updated_pool.small_blocks.length() {
      if updated_pool.small_blocks[i].id == block_id {
        updated_pool.small_blocks[i] = {
          updated_pool.small_blocks[i] |
          in_use: false,
          data: ""
        }
        return updated_pool
      }
    }
    
    // Check medium blocks
    for i in 0..<updated_pool.medium_blocks.length() {
      if updated_pool.medium_blocks[i].id == block_id {
        updated_pool.medium_blocks[i] = {
          updated_pool.medium_blocks[i] |
          in_use: false,
          data: ""
        }
        return updated_pool
      }
    }
    
    // Check large blocks
    for i in 0..<updated_pool.large_blocks.length() {
      if updated_pool.large_blocks[i].id == block_id {
        updated_pool.large_blocks[i] = {
          updated_pool.large_blocks[i] |
          in_use: false,
          data: ""
        }
        return updated_pool
      }
    }
    
    updated_pool
  }
  
  let get_pool_stats = fn(pool : MultiSizePool) {
    let small_in_use = pool.small_blocks.filter(fn(block) { block.in_use }).length()
    let medium_in_use = pool.medium_blocks.filter(fn(block) { block.in_use }).length()
    let large_in_use = pool.large_blocks.filter(fn(block) { block.in_use }).length()
    
    (small_in_use, medium_in_use, large_in_use)
  }
  
  // Test multi-size pool
  let pool = create_multi_size_pool(2, 2, 2)
  let (small_used, medium_used, large_used) = get_pool_stats(pool)
  assert_eq(small_used, 0)
  assert_eq(medium_used, 0)
  assert_eq(large_used, 0)
  
  // Acquire blocks of different sizes
  let (small_block1, pool1) = acquire_block(pool, BlockSize::Small, "Small Data 1")
  let (small_block2, pool2) = acquire_block(pool1, BlockSize::Small, "Small Data 2")
  let (medium_block1, pool3) = acquire_block(pool2, BlockSize::Medium, "Medium Data 1")
  let (large_block1, pool4) = acquire_block(pool3, BlockSize::Large, "Large Data 1")
  
  match small_block1 {
    Some(block) => assert_true(block.in_use),
    None => assert_true(false)
  }
  
  let (small_used2, medium_used2, large_used2) = get_pool_stats(pool4)
  assert_eq(small_used2, 2)
  assert_eq(medium_used2, 1)
  assert_eq(large_used2, 1)
  
  // Try to acquire when pool is exhausted
  let (small_block3, pool5) = acquire_block(pool4, BlockSize::Small, "Small Data 3")
  match small_block3 {
    Some(_) => assert_true(false), // Should fail
    None => assert_true(true)
  }
  
  // Release a block
  match small_block1 {
    Some(block) => {
      let pool6 = release_block(pool5, block.id)
      let (small_used3, medium_used3, large_used3) = get_pool_stats(pool6)
      assert_eq(small_used3, 1)
      assert_eq(medium_used3, 1)
      assert_eq(large_used3, 1)
      
      // Should be able to acquire now
      let (small_block4, pool7) = acquire_block(pool6, BlockSize::Small, "Small Data 3")
      match small_block4 {
        Some(_) => assert_true(true),
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}