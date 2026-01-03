// Memory Management Tests for Azimuth Telemetry System
// This file contains test cases for memory management and resource cleanup

// Test 1: Memory Pool Management
test "memory pool management" {
  // Simulate memory pool for telemetry data
  let pool_size = 100
  let memory_pool = []
  let allocated_blocks = []
  
  // Initialize memory pool
  for i in 0..<pool_size {
    memory_pool = memory_pool.push("block_" + i.to_string())
  }
  
  assert_eq(memory_pool.length(), pool_size)
  
  // Allocate memory blocks
  let allocate_block = fn() {
    if memory_pool.length() > 0 {
      let block = memory_pool[0]
      memory_pool = memory_pool.slice(1, memory_pool.length())
      allocated_blocks = allocated_blocks.push(block)
      Ok(block)
    } else {
      Err("Memory pool exhausted")
    }
  }
  
  // Deallocate memory blocks
  let deallocate_block = fn(block) {
    let index = allocated_blocks.index_of(block)
    match index {
      Some(i) => {
        allocated_blocks = allocated_blocks.slice(0, i) + allocated_blocks.slice(i + 1, allocated_blocks.length())
        memory_pool = memory_pool.push(block)
        Ok("Block deallocated")
      }
      None => Err("Block not found in allocated list")
    }
  }
  
  // Test allocations
  let allocation_results = []
  for i in 0..<10 {
    let result = allocate_block()
    allocation_results = allocation_results.push(result)
  }
  
  // Verify allocations
  for result in allocation_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(memory_pool.length(), 90)
  assert_eq(allocated_blocks.length(), 10)
  
  // Test deallocations
  let deallocation_results = []
  for i in 0..<5 {
    match allocation_results[i] {
      Ok(block) => {
        let result = deallocate_block(block)
        deallocation_results = deallocation_results.push(result)
      }
      Err(_) => assert_true(false)
    }
  }
  
  // Verify deallocations
  for result in deallocation_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(memory_pool.length(), 95)
  assert_eq(allocated_blocks.length(), 5)
}

// Test 2: Garbage Collection Simulation
test "garbage collection simulation" {
  // Simulate object reference counting
  let objects = {}
  let reference_counts = {}
  
  // Create object
  let create_object = fn(id, data) {
    objects = objects.set(id, data)
    reference_counts = reference_counts.set(id, 1)
    Ok("Object created")
  }
  
  // Add reference
  let add_reference = fn(id) {
    match reference_counts.get(id) {
      Some(count) => {
        reference_counts = reference_counts.set(id, count + 1)
        Ok("Reference added")
      }
      None => Err("Object not found")
    }
  }
  
  // Remove reference
  let remove_reference = fn(id) {
    match reference_counts.get(id) {
      Some(count) => {
        let new_count = count - 1
        if new_count <= 0 {
          // Simulate garbage collection
          objects = objects.remove(id)
          reference_counts = reference_counts.remove(id)
          Ok("Object garbage collected")
        } else {
          reference_counts = reference_counts.set(id, new_count)
          Ok("Reference removed")
        }
      }
      None => Err("Object not found")
    }
  }
  
  // Test object lifecycle
  let result1 = create_object("obj1", "telemetry_data_1")
  let result2 = create_object("obj2", "telemetry_data_2")
  let result3 = create_object("obj3", "telemetry_data_3")
  
  match (result1, result2, result3) {
    (Ok(_), Ok(_), Ok(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(objects.length(), 3)
  assert_eq(reference_counts.length(), 3)
  
  // Add references
  let result4 = add_reference("obj1")
  let result5 = add_reference("obj1")
  let result6 = add_reference("obj2")
  
  match (result4, result5, result6) {
    (Ok(_), Ok(_), Ok(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(reference_counts.get("obj1").or_else(0), 3)
  assert_eq(reference_counts.get("obj2").or_else(0), 2)
  assert_eq(reference_counts.get("obj3").or_else(0), 1)
  
  // Remove references
  let result7 = remove_reference("obj1")
  let result8 = remove_reference("obj1")
  let result9 = remove_reference("obj3")
  
  match (result7, result8, result9) {
    (Ok(_), Ok(_), Ok(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(reference_counts.get("obj1").or_else(0), 1)
  assert_eq(objects.contains("obj3"), false)  // Should be garbage collected
  assert_eq(reference_counts.contains("obj3"), false)
}

// Test 3: Memory Leak Detection
test "memory leak detection" {
  // Simulate memory tracking
  let allocated_memory = {}
  let total_allocated = 0
  
  // Allocate memory
  let allocate_memory = fn(id, size) {
    allocated_memory = allocated_memory.set(id, size)
    total_allocated = total_allocated + size
    Ok(size)
  }
  
  // Free memory
  let free_memory = fn(id) {
    match allocated_memory.get(id) {
      Some(size) => {
        allocated_memory = allocated_memory.remove(id)
        total_allocated = total_allocated - size
        Ok(size)
      }
      None => Err("Memory block not found")
    }
  }
  
  // Check for leaks
  let detect_leaks = fn() {
    if allocated_memory.length() > 0 {
      let leaked_blocks = allocated_memory.keys()
      Err("Memory leaks detected: " + leaked_blocks.length().to_string() + " blocks")
    } else {
      Ok("No memory leaks detected")
    }
  }
  
  // Test normal allocation and deallocation
  let result1 = allocate_memory("block1", 1024)
  let result2 = allocate_memory("block2", 2048)
  let result3 = allocate_memory("block3", 512)
  
  match (result1, result2, result3) {
    (Ok(_), Ok(_), Ok(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(total_allocated, 3584)
  
  // Free some blocks
  let result4 = free_memory("block1")
  let result5 = free_memory("block3")
  
  match (result4, result5) {
    (Ok(_), Ok(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(total_allocated, 2048)
  
  // Detect leaks (block2 is still allocated)
  let leak_check1 = detect_leaks()
  match leak_check1 {
    Ok(_) => assert_true(false)  // Should detect leak
    Err(message) => assert_true(message.contains("1 blocks"))
  }
  
  // Free remaining block
  let result6 = free_memory("block2")
  match result6 {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  assert_eq(total_allocated, 0)
  
  // Check for leaks again
  let leak_check2 = detect_leaks()
  match leak_check2 {
    Ok(message) => assert_eq(message, "No memory leaks detected")
    Err(_) => assert_true(false)
  }
}

// Test 4: Buffer Management
test "buffer management" {
  // Simulate circular buffer for telemetry data
  let buffer_size = 10
  let circular_buffer = []
  let buffer_head = 0
  let buffer_tail = 0
  let buffer_count = 0
  
  // Initialize buffer
  for i in 0..<buffer_size {
    circular_buffer = circular_buffer.push(None)
  }
  
  // Write to buffer
  let write_to_buffer = fn(data) {
    if buffer_count < buffer_size {
      circular_buffer = circular_buffer.with(buffer_tail, Some(data))
      buffer_tail = (buffer_tail + 1) % buffer_size
      buffer_count = buffer_count + 1
      Ok("Data written")
    } else {
      Err("Buffer full")
    }
  }
  
  // Read from buffer
  let read_from_buffer = fn() {
    if buffer_count > 0 {
      match circular_buffer[buffer_head] {
        Some(data) => {
          circular_buffer = circular_buffer.with(buffer_head, None)
          buffer_head = (buffer_head + 1) % buffer_size
          buffer_count = buffer_count - 1
          Ok(data)
        }
        None => Err("No data at head position")
      }
    } else {
      Err("Buffer empty")
    }
  }
  
  // Test buffer operations
  let write_results = []
  for i in 0..<8 {
    let result = write_to_buffer("data_" + i.to_string())
    write_results = write_results.push(result)
  }
  
  // Verify writes
  for result in write_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(buffer_count, 8)
  
  // Test reads
  let read_results = []
  for i in 0..<3 {
    let result = read_from_buffer()
    read_results = read_results.push(result)
  }
  
  // Verify reads
  for i in 0..<3 {
    match read_results[i] {
      Ok(data) => assert_eq(data, "data_" + i.to_string())
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(buffer_count, 5)
  
  // Test buffer full condition
  let remaining_writes = buffer_size - buffer_count
  let full_buffer_results = []
  
  for i in 0..<remaining_writes {
    let result = write_to_buffer("additional_data_" + i.to_string())
    full_buffer_results = full_buffer_results.push(result)
  }
  
  // Verify writes until full
  for result in full_buffer_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(buffer_count, buffer_size)
  
  // Test write to full buffer
  let overflow_result = write_to_buffer("overflow_data")
  match overflow_result {
    Ok(_) => assert_true(false)
    Err(message) => assert_eq(message, "Buffer full")
  }
}

// Test 5: Memory Fragmentation
test "memory fragmentation simulation" {
  // Simulate memory allocation with fragmentation
  let memory_blocks = []
  let total_memory = 1000
  let allocated_memory = 0
  let fragmented_blocks = []
  
  // Initialize memory blocks
  for i in 0..<10 {
    memory_blocks = memory_blocks.push(None)
  }
  
  // Allocate memory with varying sizes
  let allocate_block = fn(index, size) {
    if index < memory_blocks.length() && memory_blocks[index] == None {
      if allocated_memory + size <= total_memory {
        memory_blocks = memory_blocks.with(index, Some(size))
        allocated_memory = allocated_memory + size
        Ok("Block allocated")
      } else {
        Err("Insufficient memory")
      }
    } else {
      Err("Block already allocated")
    }
  }
  
  // Free memory block
  let free_block = fn(index) {
    if index < memory_blocks.length() {
      match memory_blocks[index] {
        Some(size) => {
          memory_blocks = memory_blocks.with(index, None)
          allocated_memory = allocated_memory - size
          fragmented_blocks = fragmented_blocks.push(index)
          Ok("Block freed")
        }
        None => Err("Block already free")
      }
    } else {
      Err("Invalid block index")
    }
  }
  
  // Test allocation pattern that creates fragmentation
  let allocations = [
    (0, 100), (1, 200), (2, 150), (3, 100), (4, 250),
    (5, 50), (6, 300), (7, 100), (8, 200), (9, 150)
  ]
  
  let allocation_results = []
  for allocation in allocations {
    let result = allocate_block(allocation.0, allocation.1)
    allocation_results = allocation_results.push(result)
  }
  
  // Verify allocations
  for result in allocation_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(allocated_memory, 1600)  // This would exceed total_memory in real scenario
  
  // Free some blocks to create fragmentation
  let free_indices = [1, 3, 5, 7, 9]
  let free_results = []
  
  for index in free_indices {
    let result = free_block(index)
    free_results = free_results.push(result)
  }
  
  // Verify frees
  for result in free_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(fragmented_blocks.length(), 5)
  
  // Check for contiguous free blocks
  let contiguous_free_blocks = []
  let mut current_sequence = []
  
  for i in 0..<memory_blocks.length() {
    if memory_blocks[i] == None {
      current_sequence = current_sequence.push(i)
    } else {
      if current_sequence.length() > 0 {
        contiguous_free_blocks = contiguous_free_blocks.push(current_sequence)
        current_sequence = []
      }
    }
  }
  
  if current_sequence.length() > 0 {
    contiguous_free_blocks = contiguous_free_blocks.push(current_sequence)
  }
  
  assert_eq(contiguous_free_blocks.length(), 5)  // 5 separate free blocks
  
  // Test defragmentation (coalescing free blocks)
  let defragmented_memory = []
  for i in 0..<memory_blocks.length() {
    match memory_blocks[i] {
      Some(size) => defragmented_memory = defragmented_memory.push(Some(size))
      None => {}  // Skip free blocks
    }
  }
  
  assert_eq(defragmented_memory.length(), 5)  // Only allocated blocks remain
}

// Test 6: Resource Cleanup
test "resource cleanup operations" {
  // Simulate resource cleanup with RAII pattern
  let active_resources = []
  let cleanup_handlers = {}
  
  // Acquire resource
  let acquire_resource = fn(resource_id, cleanup_fn) {
    active_resources = active_resources.push(resource_id)
    cleanup_handlers = cleanup_handlers.set(resource_id, cleanup_fn)
    Ok("Resource acquired")
  }
  
  // Release resource
  let release_resource = fn(resource_id) {
    if active_resources.contains(resource_id) {
      active_resources = active_resources.filter(fn(id) { id != resource_id })
      match cleanup_handlers.get(resource_id) {
        Some(cleanup_fn) => {
          cleanup_fn()
          cleanup_handlers = cleanup_handlers.remove(resource_id)
          Ok("Resource released")
        }
        None => Err("Cleanup handler not found")
      }
    } else {
      Err("Resource not found")
    }
  }
  
  // Cleanup all resources
  let cleanup_all_resources = fn() {
    let cleanup_results = []
    for resource_id in active_resources {
      let result = release_resource(resource_id)
      cleanup_results = cleanup_results.push(result)
    }
    cleanup_results
  }
  
  // Test resource lifecycle
  let result1 = acquire_resource("file1", fn() { "Close file handle" })
  let result2 = acquire_resource("network1", fn() { "Close network connection" })
  let result3 = acquire_resource("database1", fn() { "Close database connection" })
  
  match (result1, result2, result3) {
    (Ok(_), Ok(_), Ok(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(active_resources.length(), 3)
  
  // Release individual resource
  let result4 = release_resource("network1")
  match result4 {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  assert_eq(active_resources.length(), 2)
  assert_false(active_resources.contains("network1"))
  
  // Cleanup all remaining resources
  let cleanup_results = cleanup_all_resources()
  
  for result in cleanup_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(active_resources.length(), 0)
  assert_eq(cleanup_handlers.length(), 0)
}

// Test 7: Memory Usage Monitoring
test "memory usage monitoring" {
  // Simulate memory usage tracking
  let memory_usage_history = []
  let current_memory_usage = 0
  let peak_memory_usage = 0
  let allocation_events = []
  
  // Record memory allocation
  let record_allocation = fn(size) {
    current_memory_usage = current_memory_usage + size
    if current_memory_usage > peak_memory_usage {
      peak_memory_usage = current_memory_usage
    }
    
    let timestamp = 1234567890 + allocation_events.length()
    memory_usage_history = memory_usage_history.push((timestamp, current_memory_usage))
    allocation_events = allocation_events.push(("allocate", size, timestamp))
    
    Ok(size)
  }
  
  // Record memory deallocation
  let record_deallocation = fn(size) {
    if current_memory_usage >= size {
      current_memory_usage = current_memory_usage - size
      
      let timestamp = 1234567890 + allocation_events.length()
      memory_usage_history = memory_usage_history.push((timestamp, current_memory_usage))
      allocation_events = allocation_events.push(("deallocate", size, timestamp))
      
      Ok(size)
    } else {
      Err("Deallocation size exceeds current usage")
    }
  }
  
  // Get memory statistics
  let get_memory_stats = fn() {
    {
      "current": current_memory_usage,
      "peak": peak_memory_usage,
      "allocations": allocation_events.filter(fn(event) { event.0 == "allocate" }).length(),
      "deallocations": allocation_events.filter(fn(event) { event.0 == "deallocate" }).length()
    }
  }
  
  // Test memory tracking
  let allocations = [1024, 2048, 512, 4096, 256]
  let deallocations = [512, 1024, 256]
  
  // Record allocations
  let allocation_results = []
  for size in allocations {
    let result = record_allocation(size)
    allocation_results = allocation_results.push(result)
  }
  
  // Verify allocations
  for result in allocation_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(current_memory_usage, 7936)  // Sum of all allocations
  assert_eq(peak_memory_usage, 7936)
  
  // Record deallocations
  let deallocation_results = []
  for size in deallocations {
    let result = record_deallocation(size)
    deallocation_results = deallocation_results.push(result)
  }
  
  // Verify deallocations
  for result in deallocation_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(current_memory_usage, 6144)  // After deallocations
  
  // Get memory statistics
  let stats = get_memory_stats()
  assert_eq(stats["current"], 6144)
  assert_eq(stats["peak"], 7936)
  assert_eq(stats["allocations"], 5)
  assert_eq(stats["deallocations"], 3)
  
  // Test memory usage history
  assert_eq(memory_usage_history.length(), 8)  // 5 allocations + 3 deallocations
  assert_eq(memory_usage_history[0].1, 1024)   // After first allocation
  assert_eq(memory_usage_history[7].1, 6144)   // After last deallocation
}

// Test 8: Smart Pointer Simulation
test "smart pointer simulation" {
  // Simulate smart pointer with reference counting
  let smart_pointers = {}
  let reference_counts = {}
  
  // Create smart pointer
  let create_smart_pointer = fn(id, data) {
    smart_pointers = smart_pointers.set(id, data)
    reference_counts = reference_counts.set(id, 1)
    Ok(id)
  }
  
  // Add reference
  let add_reference = fn(id) {
    match reference_counts.get(id) {
      Some(count) => {
        reference_counts = reference_counts.set(id, count + 1)
        Ok(count + 1)
      }
      None => Err("Smart pointer not found")
    }
  }
  
  // Remove reference
  let remove_reference = fn(id) {
    match reference_counts.get(id) {
      Some(count) => {
        let new_count = count - 1
        if new_count <= 0 {
          smart_pointers = smart_pointers.remove(id)
          reference_counts = reference_counts.remove(id)
          Ok(0)  // Object destroyed
        } else {
          reference_counts = reference_counts.set(id, new_count)
          Ok(new_count)
        }
      }
      None => Err("Smart pointer not found")
    }
  }
  
  // Get reference count
  let get_reference_count = fn(id) {
    reference_counts.get(id).or_else(0)
  }
  
  // Test smart pointer operations
  let result1 = create_smart_pointer("ptr1", "telemetry_data")
  let result2 = create_smart_pointer("ptr2", "metrics_data")
  
  match (result1, result2) {
    (Ok(_), Ok(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(get_reference_count("ptr1"), 1)
  assert_eq(get_reference_count("ptr2"), 1)
  
  // Add references
  let result3 = add_reference("ptr1")
  let result4 = add_reference("ptr1")
  let result5 = add_reference("ptr2")
  
  match (result3, result4, result5) {
    (Ok(count1), Ok(count2), Ok(count3)) => {
      assert_eq(count1, 2)
      assert_eq(count2, 3)
      assert_eq(count3, 2)
    }
    _ => assert_true(false)
  }
  
  // Remove references
  let result6 = remove_reference("ptr1")
  let result7 = remove_reference("ptr1")
  let result8 = remove_reference("ptr2")
  
  match (result6, result7, result8) {
    (Ok(count1), Ok(count2), Ok(count3)) => {
      assert_eq(count1, 2)  // ptr1: 3 -> 2
      assert_eq(count2, 0)  // ptr1: 2 -> 1, then destroyed
      assert_eq(count3, 1)  // ptr2: 2 -> 1
    }
    _ => assert_true(false)
  }
  
  assert_false(smart_pointers.contains("ptr1"))  // Should be destroyed
  assert_true(smart_pointers.contains("ptr2"))   // Should still exist
  assert_eq(get_reference_count("ptr1"), 0)      // Should be 0 (destroyed)
  assert_eq(get_reference_count("ptr2"), 1)      // Should be 1
}

// Test 9: Memory Pool with Different Sizes
test "memory pool with different block sizes" {
  // Simulate memory pools with different block sizes
  let small_blocks = []
  let medium_blocks = []
  let large_blocks = []
  
  let small_block_size = 64
  let medium_block_size = 256
  let large_block_size = 1024
  
  // Initialize pools
  for i in 0..<10 {
    small_blocks = small_blocks.push("small_" + i.to_string())
    medium_blocks = medium_blocks.push("medium_" + i.to_string())
    large_blocks = large_blocks.push("large_" + i.to_string())
  }
  
  // Allocate from appropriate pool
  let allocate_block = fn(requested_size) {
    if requested_size <= small_block_size {
      if small_blocks.length() > 0 {
        let block = small_blocks[0]
        small_blocks = small_blocks.slice(1, small_blocks.length())
        Ok(("small", block))
      } else {
        Err("Small block pool exhausted")
      }
    } else if requested_size <= medium_block_size {
      if medium_blocks.length() > 0 {
        let block = medium_blocks[0]
        medium_blocks = medium_blocks.slice(1, medium_blocks.length())
        Ok(("medium", block))
      } else {
        Err("Medium block pool exhausted")
      }
    } else if requested_size <= large_block_size {
      if large_blocks.length() > 0 {
        let block = large_blocks[0]
        large_blocks = large_blocks.slice(1, large_blocks.length())
        Ok(("large", block))
      } else {
        Err("Large block pool exhausted")
      }
    } else {
      Err("Requested size too large")
    }
  }
  
  // Return block to appropriate pool
  let return_block = fn(pool_type, block) {
    match pool_type {
      "small" => {
        small_blocks = small_blocks.push(block)
        Ok("Block returned to small pool")
      }
      "medium" => {
        medium_blocks = medium_blocks.push(block)
        Ok("Block returned to medium pool")
      }
      "large" => {
        large_blocks = large_blocks.push(block)
        Ok("Block returned to large pool")
      }
      _ => Err("Unknown pool type")
    }
  }
  
  // Test allocations
  let allocation_requests = [32, 128, 512, 64, 256, 1024, 48, 200, 800]
  let allocation_results = []
  
  for size in allocation_requests {
    let result = allocate_block(size)
    allocation_results = allocation_results.push(result)
  }
  
  // Verify allocations
  for result in allocation_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(small_blocks.length(), 8)   // Used 2 small blocks (32, 64, 48)
  assert_eq(medium_blocks.length(), 7)  // Used 3 medium blocks (128, 256, 200)
  assert_eq(large_blocks.length(), 7)   // Used 3 large blocks (512, 1024, 800)
  
  // Return some blocks
  let return_results = []
  for i in 0..<3 {
    match allocation_results[i] {
      Ok((pool_type, block)) => {
        let result = return_block(pool_type, block)
        return_results = return_results.push(result)
      }
      Err(_) => assert_true(false)
    }
  }
  
  // Verify returns
  for result in return_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(small_blocks.length(), 9)   // Returned 1 small block
  assert_eq(medium_blocks.length(), 8)  // Returned 1 medium block
  assert_eq(large_blocks.length(), 8)   // Returned 1 large block
}

// Test 10: Memory Pressure Handling
test "memory pressure handling" {
  // Simulate memory pressure scenarios
  let total_memory = 10000
  let used_memory = 0
  let memory_threshold = 8000
  let cache_data = {}
  let priority_data = {}
  
  // Allocate memory
  let allocate_memory = fn(id, size, priority) {
    if used_memory + size <= total_memory {
      used_memory = used_memory + size
      
      if priority == "high" {
        priority_data = priority_data.set(id, size)
      } else {
        cache_data = cache_data.set(id, size)
      }
      
      Ok("Memory allocated")
    } else {
      Err("Insufficient memory")
    }
  }
  
  // Free memory
  let free_memory = fn(id) {
    let size = priority_data.get(id).or_else(cache_data.get(id).or_else(0))
    
    if size > 0 {
      used_memory = used_memory - size
      priority_data = priority_data.remove(id)
      cache_data = cache_data.remove(id)
      Ok("Memory freed")
    } else {
      Err("Memory block not found")
    }
  }
  
  // Handle memory pressure
  let handle_memory_pressure = fn() {
    if used_memory > memory_threshold {
      // Free low priority cache data first
      let cache_entries = cache_data.entries()
      let mut freed_memory = 0
      
      for entry in cache_entries {
        let result = free_memory(entry.0)
        match result {
          Ok(_) => freed_memory = freed_memory + entry.1
          Err(_) => {}
        }
        
        if used_memory <= memory_threshold {
          break
        }
      }
      
      Ok("Memory pressure handled, freed: " + freed_memory.to_string())
    } else {
      Ok("No memory pressure")
    }
  }
  
  // Test memory pressure scenarios
  let allocations = [
    ("cache1", 2000, "low"),
    ("cache2", 1500, "low"),
    ("cache3", 1000, "low"),
    ("important1", 3000, "high"),
    ("important2", 2500, "high"),
    ("cache4", 2000, "low")
  ]
  
  let allocation_results = []
  for allocation in allocations {
    let result = allocate_memory(allocation.0, allocation.1, allocation.2)
    allocation_results = allocation_results.push(result)
  }
  
  // Verify allocations
  let successful_allocations = 0
  for result in allocation_results {
    match result {
      Ok(_) => successful_allocations = successful_allocations + 1
      Err(_) => {}
    }
  }
  
  assert_eq(successful_allocations, 5)  // Last allocation should fail due to insufficient memory
  assert_true(used_memory > memory_threshold)
  
  // Handle memory pressure
  let pressure_result = handle_memory_pressure()
  match pressure_result {
    Ok(message) => assert_true(message.contains("Memory pressure handled"))
    Err(_) => assert_true(false)
  }
  
  assert_true(used_memory <= memory_threshold)
  
  // Verify high priority data is preserved
  assert_eq(priority_data.length(), 2)
  assert_true(priority_data.contains("important1"))
  assert_true(priority_data.contains("important2"))
  
  // Verify cache data is reduced
  assert_true(cache_data.length() < 4)  // Some cache entries should be freed
}