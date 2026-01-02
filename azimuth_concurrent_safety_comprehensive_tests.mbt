// Azimuth Concurrent Safety Comprehensive Test Suite
// This file contains test cases for concurrent safety mechanisms

// Test 1: Shared State Simulation
test "shared state simulation" {
  // Simulate a shared counter with atomic operations
  type SharedCounter = {
    value: Int,
    version: Int  // Version number for optimistic locking
  }
  
  let create_counter = fn(initial_value: Int) -> SharedCounter {
    { value: initial_value, version: 0 }
  }
  
  let atomic_increment = fn(counter: SharedCounter) -> SharedCounter {
    { value: counter.value + 1, version: counter.version + 1 }
  }
  
  let atomic_decrement = fn(counter: SharedCounter) -> SharedCounter {
    { value: counter.value - 1, version: counter.version + 1 }
  }
  
  let atomic_add = fn(counter: SharedCounter, delta: Int) -> SharedCounter {
    { value: counter.value + delta, version: counter.version + 1 }
  }
  
  // Test basic operations
  let counter1 = create_counter(0)
  assert_eq(counter1.value, 0)
  assert_eq(counter1.version, 0)
  
  let counter2 = atomic_increment(counter1)
  assert_eq(counter2.value, 1)
  assert_eq(counter2.version, 1)
  
  let counter3 = atomic_decrement(counter2)
  assert_eq(counter3.value, 0)
  assert_eq(counter3.version, 2)
  
  let counter4 = atomic_add(counter3, 5)
  assert_eq(counter4.value, 5)
  assert_eq(counter4.version, 3)
  
  // Test optimistic locking simulation
  let compare_and_swap = fn(old_counter: SharedCounter, expected_version: Int, new_value: Int) -> Result<SharedCounter, String> {
    if old_counter.version == expected_version {
      Ok({ value: new_value, version: old_counter.version + 1 })
    } else {
      Err("Version mismatch")
    }
  }
  
  let counter5 = create_counter(10)
  let cas_result1 = compare_and_swap(counter5, 0, 20)
  match cas_result1 {
    Ok(new_counter) => {
      assert_eq(new_counter.value, 20)
      assert_eq(new_counter.version, 1)
    }
    Err(_) => assert_true(false)
  }
  
  let cas_result2 = compare_and_swap(counter5, 0, 30)  // Using original counter, version mismatch
  match cas_result2 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Version mismatch")
  }
}

// Test 2: Lock-Free Data Structure Simulation
test "lock-free data structure simulation" {
  // Simulate a lock-free stack using versioned nodes
  type StackNode = Option<(Int, Int)>  // (value, version)
  
  let empty_stack: StackNode = None
  
  let stack_push = fn(stack: StackNode, value: Int, version: Int) -> StackNode {
    Some((value, version))
  }
  
  let stack_pop = fn(stack: StackNode) -> (Option<Int>, StackNode) {
    match stack {
      Some((value, _)) => (Some(value), None)
      None => (None, None)
    }
  }
  
  let stack_peek = fn(stack: StackNode) -> Option<Int> {
    match stack {
      Some((value, _)) => Some(value)
      None => None
    }
  }
  
  // Test stack operations
  let stack1 = empty_stack
  assert_eq(stack_peek(stack1), None)
  
  let stack2 = stack_push(stack1, 10, 1)
  assert_eq(stack_peek(stack2), Some(10))
  
  let (value1, stack3) = stack_pop(stack2)
  match value1 {
    Some(v) => assert_eq(v, 10)
    None => assert_true(false)
  }
  assert_eq(stack_peek(stack3), None)
  
  // Test multiple pushes and pops
  let stack4 = stack_push(stack3, 20, 2)
  let stack5 = stack_push(stack4, 30, 3)
  
  assert_eq(stack_peek(stack5), Some(30))
  
  let (value2, stack6) = stack_pop(stack5)
  match value2 {
    Some(v) => assert_eq(v, 30)
    None => assert_true(false)
  }
  
  assert_eq(stack_peek(stack6), Some(20))
  
  let (value3, stack7) = stack_pop(stack6)
  match value3 {
    Some(v) => assert_eq(v, 20)
    None => assert_true(false)
  }
  
  assert_eq(stack_peek(stack7), None)
}

// Test 3: Producer-Consumer Pattern Simulation
test "producer-consumer pattern simulation" {
  // Simulate a bounded buffer with circular buffer
  type BoundedBuffer = {
    buffer: Array<Option<Int>>,
    head: Int,
    tail: Int,
    count: Int,
    capacity: Int
  }
  
  let create_buffer = fn(capacity: Int) -> BoundedBuffer {
    let mut buffer = []
    for i in 0..<capacity {
      buffer = buffer.push(None)
    }
    { buffer: buffer, head: 0, tail: 0, count: 0, capacity: capacity }
  }
  
  let buffer_is_empty = fn(buf: BoundedBuffer) -> Bool {
    buf.count == 0
  }
  
  let buffer_is_full = fn(buf: BoundedBuffer) -> Bool {
    buf.count == buf.capacity
  }
  
  let buffer_produce = fn(buf: BoundedBuffer, item: Int) -> Result<BoundedBuffer, String> {
    if buffer_is_full(buf) {
      Err("Buffer is full")
    } else {
      let new_buffer = buf.buffer.with(buf.tail, Some(item))
      let new_tail = (buf.tail + 1) % buf.capacity
      Ok({ 
        buffer: new_buffer, 
        head: buf.head, 
        tail: new_tail, 
        count: buf.count + 1, 
        capacity: buf.capacity 
      })
    }
  }
  
  let buffer_consume = fn(buf: BoundedBuffer) -> (Option<Int>, Result<BoundedBuffer, String>) {
    if buffer_is_empty(buf) {
      (None, Err("Buffer is empty"))
    } else {
      match buf.buffer[buf.head] {
        Some(item) => {
          let new_buffer = buf.buffer.with(buf.head, None)
          let new_head = (buf.head + 1) % buf.capacity
          (Some(item), Ok({ 
            buffer: new_buffer, 
            head: new_head, 
            tail: buf.tail, 
            count: buf.count - 1, 
            capacity: buf.capacity 
          }))
        }
        None => (None, Err("Invalid buffer state"))
      }
    }
  }
  
  // Test buffer operations
  let buffer1 = create_buffer(3)
  assert_true(buffer_is_empty(buffer1))
  assert_false(buffer_is_full(buffer1))
  
  let buffer2_result = buffer_produce(buffer1, 10)
  match buffer2_result {
    Ok(buffer2) => {
      assert_false(buffer_is_empty(buffer2))
      assert_false(buffer_is_full(buffer2))
      
      let buffer3_result = buffer_produce(buffer2, 20)
      match buffer3_result {
        Ok(buffer3) => {
          assert_false(buffer_is_empty(buffer3))
          assert_false(buffer_is_full(buffer3))
          
          let buffer4_result = buffer_produce(buffer3, 30)
          match buffer4_result {
            Ok(buffer4) => {
              assert_false(buffer_is_empty(buffer4))
              assert_true(buffer_is_full(buffer4))
              
              // Try to produce when full
              let buffer5_result = buffer_produce(buffer4, 40)
              match buffer5_result {
                Ok(_) => assert_true(false)
                Err(msg) => assert_eq(msg, "Buffer is full")
              }
              
              // Consume items
              let (item1, buffer6_result) = buffer_consume(buffer4)
              match item1 {
                Some(value) => assert_eq(value, 10)
                None => assert_true(false)
              }
              
              match buffer6_result {
                Ok(buffer6) => {
                  assert_false(buffer_is_empty(buffer6))
                  assert_false(buffer_is_full(buffer6))
                  
                  let (item2, buffer7_result) = buffer_consume(buffer6)
                  match item2 {
                    Some(value) => assert_eq(value, 20)
                    None => assert_true(false)
                  }
                  
                  match buffer7_result {
                    Ok(buffer7) => {
                      let (item3, buffer8_result) = buffer_consume(buffer7)
                      match item3 {
                        Some(value) => assert_eq(value, 30)
                        None => assert_true(false)
                      }
                      
                      match buffer8_result {
                        Ok(buffer8) => {
                          assert_true(buffer_is_empty(buffer8))
                          assert_false(buffer_is_full(buffer8))
                          
                          // Try to consume when empty
                          let (item4, buffer9_result) = buffer_consume(buffer8)
                          match item4 {
                            Some(_) => assert_true(false)
                            None => assert_true(true)
                          }
                          
                          match buffer9_result {
                            Ok(_) => assert_true(false)
                            Err(msg) => assert_eq(msg, "Buffer is empty")
                          }
                        }
                        Err(_) => assert_true(false)
                      }
                    }
                    Err(_) => assert_true(false)
                  }
                }
                Err(_) => assert_true(false)
              }
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 4: Read-Write Lock Simulation
test "read-write lock simulation" {
  // Simulate a read-write lock with counters
  type ReadWriteLock = {
    readers: Int,
    writers: Int,
    waiting_writers: Int
  }
  
  let create_lock = fn() -> ReadWriteLock {
    { readers: 0, writers: 0, waiting_writers: 0 }
  }
  
  let can_read = fn(lock: ReadWriteLock) -> Bool {
    lock.writers == 0 && lock.waiting_writers == 0
  }
  
  let can_write = fn(lock: ReadWriteLock) -> Bool {
    lock.readers == 0 && lock.writers == 0
  }
  
  let acquire_read = fn(lock: ReadWriteLock) -> Result<ReadWriteLock, String> {
    if can_read(lock) {
      Ok({ readers: lock.readers + 1, writers: lock.writers, waiting_writers: lock.waiting_writers })
    } else {
      Err("Cannot acquire read lock")
    }
  }
  
  let release_read = fn(lock: ReadWriteLock) -> ReadWriteLock {
    { readers: lock.readers - 1, writers: lock.writers, waiting_writers: lock.waiting_writers }
  }
  
  let acquire_write = fn(lock: ReadWriteLock) -> Result<ReadWriteLock, String> {
    if can_write(lock) {
      Ok({ readers: lock.readers, writers: lock.writers + 1, waiting_writers: lock.waiting_writers })
    } else {
      Ok({ readers: lock.readers, writers: lock.writers, waiting_writers: lock.waiting_writers + 1 })
    }
  }
  
  let release_write = fn(lock: ReadWriteLock) -> ReadWriteLock {
    { readers: lock.readers, writers: lock.writers - 1, waiting_writers: lock.waiting_writers }
  }
  
  // Test lock operations
  let lock1 = create_lock()
  assert_true(can_read(lock1))
  assert_true(can_write(lock1))
  
  // Acquire read lock
  let lock2_result = acquire_read(lock1)
  match lock2_result {
    Ok(lock2) => {
      assert_true(can_read(lock2))
      assert_false(can_write(lock2))
      
      // Acquire another read lock
      let lock3_result = acquire_read(lock2)
      match lock3_result {
        Ok(lock3) => {
          assert_true(can_read(lock3))
          assert_false(can_write(lock3));
          
          // Try to acquire write lock (should fail)
          let lock4_result = acquire_write(lock3)
          match lock4_result {
            Ok(lock4) => {
              assert_false(can_read(lock4))
              assert_false(can_write(lock4));
              assert_eq(lock4.waiting_writers, 1)
              
              // Release read locks
              let lock5 = release_read(lock3)
              assert_true(can_read(lock5))
              assert_false(can_write(lock5));
              
              let lock6 = release_read(lock5)
              assert_true(can_read(lock6))
              assert_true(can_write(lock6));
              
              // Now write lock should be acquirable
              let lock7_result = acquire_write(lock6)
              match lock7_result {
                Ok(lock7) => {
                  assert_false(can_read(lock7))
                  assert_false(can_write(lock7));
                  assert_eq(lock7.writers, 1)
                  
                  // Release write lock
                  let lock8 = release_write(lock7)
                  assert_true(can_read(lock8))
                  assert_true(can_write(lock8));
                  assert_eq(lock8.writers, 0)
                }
                Err(_) => assert_true(false)
              }
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 5: Thread-Safe Reference Simulation
test "thread-safe reference simulation" {
  // Simulate a thread-safe reference with versioning
  type SafeRef = {
    value: String,
    version: Int
  }
  
  let create_ref = fn(initial_value: String) -> SafeRef {
    { value: initial_value, version: 0 }
  }
  
  let ref_read = fn(ref: SafeRef) -> (String, Int) {
    (ref.value, ref.version)
  }
  
  let ref_write = fn(ref: SafeRef, new_value: String) -> SafeRef {
    { value: new_value, version: ref.version + 1 }
  }
  
  let ref_compare_and_swap = fn(ref: SafeRef, expected_version: Int, new_value: String) -> Result<SafeRef, String> {
    if ref.version == expected_version {
      Ok({ value: new_value, version: ref.version + 1 })
    } else {
      Err("Version mismatch")
    }
  }
  
  // Test reference operations
  let ref1 = create_ref("initial")
  let (value1, version1) = ref_read(ref1)
  assert_eq(value1, "initial")
  assert_eq(version1, 0)
  
  let ref2 = ref_write(ref1, "updated")
  let (value2, version2) = ref_read(ref2)
  assert_eq(value2, "updated")
  assert_eq(version2, 1)
  
  // Test compare-and-swap
  let cas_result1 = ref_compare_and_swap(ref2, 1, "cas_updated")
  match cas_result1 {
    Ok(ref3) => {
      let (value3, version3) = ref_read(ref3)
      assert_eq(value3, "cas_updated")
      assert_eq(version3, 2)
    }
    Err(_) => assert_true(false)
  }
  
  // Test CAS with wrong version
  let cas_result2 = ref_compare_and_swap(ref2, 0, "should_fail")
  match cas_result2 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Version mismatch")
  }
}

// Test 6: Concurrent Map Simulation
test "concurrent map simulation" {
  // Simulate a concurrent map with versioned entries
  type ConcurrentMapEntry = {
    value: Int,
    version: Int
  }
  
  type ConcurrentMap = Array<(String, Option<ConcurrentMapEntry>)>
  
  let create_map = fn() -> ConcurrentMap {
    []
  }
  
  let map_get = fn(map: ConcurrentMap, key: String) -> Option<Int> {
    for (k, entry) in map {
      if k == key {
        match entry {
          Some(e) => return Some(e.value)
          None => return None
        }
      }
    }
    None
  }
  
  let map_put = fn(map: ConcurrentMap, key: String, value: Int) -> ConcurrentMap {
    let mut result = []
    let mut found = false
    
    for (k, entry) in map {
      if k == key {
        match entry {
          Some(e) => {
            result = result.push((k, Some({ value: value, version: e.version + 1 })))
          }
          None => {
            result = result.push((k, Some({ value: value, version: 0 })))
          }
        }
        found = true
      } else {
        result = result.push((k, entry))
      }
    }
    
    if !found {
      result = result.push((key, Some({ value: value, version: 0 })))
    }
    
    result
  }
  
  let map_remove = fn(map: ConcurrentMap, key: String) -> ConcurrentMap {
    let mut result = []
    
    for (k, entry) in map {
      if k == key {
        result = result.push((k, None))
      } else {
        result = result.push((k, entry))
      }
    }
    
    result
  }
  
  // Test map operations
  let map1 = create_map()
  assert_eq(map_get(map1, "key1"), None)
  
  let map2 = map_put(map1, "key1", 10)
  match map_get(map2, "key1") {
    Some(value) => assert_eq(value, 10)
    None => assert_true(false)
  }
  
  let map3 = map_put(map2, "key2", 20)
  match map_get(map3, "key1") {
    Some(value) => assert_eq(value, 10)
    None => assert_true(false)
  }
  
  match map_get(map3, "key2") {
    Some(value) => assert_eq(value, 20)
    None => assert_true(false)
  }
  
  let map4 = map_put(map3, "key1", 30)  // Update existing key
  match map_get(map4, "key1") {
    Some(value) => assert_eq(value, 30)
    None => assert_true(false)
  }
  
  match map_get(map4, "key2") {
    Some(value) => assert_eq(value, 20)
    None => assert_true(false)
  }
  
  let map5 = map_remove(map4, "key1")
  assert_eq(map_get(map5, "key1"), None)
  
  match map_get(map5, "key2") {
    Some(value) => assert_eq(value, 20)
    None => assert_true(false)
  }
}

// Test 7: Concurrent Queue Simulation
test "concurrent queue simulation" {
  // Simulate a concurrent queue with head and tail pointers
  type ConcurrentQueue = {
    nodes: Array<Option<Int>>,
    head: Int,
    tail: Int,
    capacity: Int
  }
  
  let create_queue = fn(capacity: Int) -> ConcurrentQueue {
    let mut nodes = []
    for i in 0..<capacity {
      nodes = nodes.push(None)
    }
    { nodes: nodes, head: 0, tail: 0, capacity: capacity }
  }
  
  let queue_is_empty = fn(queue: ConcurrentQueue) -> Bool {
    queue.head == queue.tail
  }
  
  let queue_is_full = fn(queue: ConcurrentQueue) -> Bool {
    (queue.tail + 1) % queue.capacity == queue.head
  }
  
  let queue_enqueue = fn(queue: ConcurrentQueue, item: Int) -> Result<ConcurrentQueue, String> {
    if queue_is_full(queue) {
      Err("Queue is full")
    } else {
      let new_nodes = queue.nodes.with(queue.tail, Some(item))
      let new_tail = (queue.tail + 1) % queue.capacity
      Ok({ nodes: new_nodes, head: queue.head, tail: new_tail, capacity: queue.capacity })
    }
  }
  
  let queue_dequeue = fn(queue: ConcurrentQueue) -> (Option<Int>, Result<ConcurrentQueue, String>) {
    if queue_is_empty(queue) {
      (None, Err("Queue is empty"))
    } else {
      match queue.nodes[queue.head] {
        Some(item) => {
          let new_nodes = queue.nodes.with(queue.head, None)
          let new_head = (queue.head + 1) % queue.capacity
          (Some(item), Ok({ nodes: new_nodes, head: new_head, tail: queue.tail, capacity: queue.capacity }))
        }
        None => (None, Err("Invalid queue state"))
      }
    }
  }
  
  // Test queue operations
  let queue1 = create_queue(3)
  assert_true(queue_is_empty(queue1))
  assert_false(queue_is_full(queue1))
  
  let queue2_result = queue_enqueue(queue1, 10)
  match queue2_result {
    Ok(queue2) => {
      assert_false(queue_is_empty(queue2))
      assert_false(queue_is_full(queue2))
      
      let queue3_result = queue_enqueue(queue2, 20)
      match queue3_result {
        Ok(queue3) => {
          assert_false(queue_is_empty(queue3))
          assert_false(queue_is_full(queue3))
          
          let queue4_result = queue_enqueue(queue3, 30)
          match queue4_result {
            Ok(queue4) => {
              assert_false(queue_is_empty(queue4))
              assert_true(queue_is_full(queue4))
              
              // Try to enqueue when full
              let queue5_result = queue_enqueue(queue4, 40)
              match queue5_result {
                Ok(_) => assert_true(false)
                Err(msg) => assert_eq(msg, "Queue is full")
              }
              
              // Dequeue items
              let (item1, queue6_result) = queue_dequeue(queue4)
              match item1 {
                Some(value) => assert_eq(value, 10)
                None => assert_true(false)
              }
              
              match queue6_result {
                Ok(queue6) => {
                  assert_false(queue_is_empty(queue6))
                  assert_false(queue_is_full(queue6))
                  
                  let (item2, queue7_result) = queue_dequeue(queue6)
                  match item2 {
                    Some(value) => assert_eq(value, 20)
                    None => assert_true(false)
                  }
                  
                  match queue7_result {
                    Ok(queue7) => {
                      let (item3, queue8_result) = queue_dequeue(queue7)
                      match item3 {
                        Some(value) => assert_eq(value, 30)
                        None => assert_true(false)
                      }
                      
                      match queue8_result {
                        Ok(queue8) => {
                          assert_true(queue_is_empty(queue8))
                          assert_false(queue_is_full(queue8))
                          
                          // Try to dequeue when empty
                          let (item4, queue9_result) = queue_dequeue(queue8)
                          match item4 {
                            Some(_) => assert_true(false)
                            None => assert_true(true)
                          }
                          
                          match queue9_result {
                            Ok(_) => assert_true(false)
                            Err(msg) => assert_eq(msg, "Queue is empty")
                          }
                        }
                        Err(_) => assert_true(false)
                      }
                    }
                    Err(_) => assert_true(false)
                  }
                }
                Err(_) => assert_true(false)
              }
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 8: Semaphore Simulation
test "semaphore simulation" {
  // Simulate a semaphore with a counter
  type Semaphore = {
    permits: Int,
    max_permits: Int
  }
  
  let create_semaphore = fn(max_permits: Int) -> Semaphore {
    { permits: max_permits, max_permits: max_permits }
  }
  
  let semaphore_acquire = fn(sem: Semaphore) -> Result<Semaphore, String> {
    if sem.permits > 0 {
      Ok({ permits: sem.permits - 1, max_permits: sem.max_permits })
    } else {
      Err("No permits available")
    }
  }
  
  let semaphore_release = fn(sem: Semaphore) -> Result<Semaphore, String> {
    if sem.permits < sem.max_permits {
      Ok({ permits: sem.permits + 1, max_permits: sem.max_permits })
    } else {
      Err("Maximum permits reached")
    }
  }
  
  // Test semaphore operations
  let sem1 = create_semaphore(3)
  assert_eq(sem1.permits, 3)
  
  let sem2_result = semaphore_acquire(sem1)
  match sem2_result {
    Ok(sem2) => {
      assert_eq(sem2.permits, 2)
      
      let sem3_result = semaphore_acquire(sem2)
      match sem3_result {
        Ok(sem3) => {
          assert_eq(sem3.permits, 1)
          
          let sem4_result = semaphore_acquire(sem3)
          match sem4_result {
            Ok(sem4) => {
              assert_eq(sem4.permits, 0)
              
              // Try to acquire when no permits available
              let sem5_result = semaphore_acquire(sem4)
              match sem5_result {
                Ok(_) => assert_true(false)
                Err(msg) => assert_eq(msg, "No permits available")
              }
              
              // Release permits
              let sem6_result = semaphore_release(sem4)
              match sem6_result {
                Ok(sem6) => {
                  assert_eq(sem6.permits, 1)
                  
                  let sem7_result = semaphore_release(sem6)
                  match sem7_result {
                    Ok(sem7) => {
                      assert_eq(sem7.permits, 2)
                      
                      let sem8_result = semaphore_release(sem7)
                      match sem8_result {
                        Ok(sem8) => {
                          assert_eq(sem8.permits, 3)
                          
                          // Try to release when max permits reached
                          let sem9_result = semaphore_release(sem8)
                          match sem9_result {
                            Ok(_) => assert_true(false)
                            Err(msg) => assert_eq(msg, "Maximum permits reached")
                          }
                        }
                        Err(_) => assert_true(false)
                      }
                    }
                    Err(_) => assert_true(false)
                  }
                }
                Err(_) => assert_true(false)
              }
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}