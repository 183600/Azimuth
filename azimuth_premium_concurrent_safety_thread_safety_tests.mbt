// Azimuth Premium Concurrent Safety and Thread Safety Tests
// This file contains comprehensive test cases for concurrent safety and thread safety

// Test 1: Thread-Safe Counter
test "thread-safe counter implementation" {
  // Define atomic counter structure
  type AtomicCounter = {
    value: Int,
    lock_acquired: Bool,
    waiting_threads: Int,
    operations_count: Int
  }
  
  // Create atomic counter
  let create_atomic_counter = fn(initial_value: Int) {
    {
      value: initial_value,
      lock_acquired: false,
      waiting_threads: 0,
      operations_count: 0
    }
  }
  
  // Simulate acquiring lock
  let acquire_lock = fn(counter: AtomicCounter, thread_id: String) {
    if !counter.lock_acquired {
      ({
        value: counter.value,
        lock_acquired: true,
        waiting_threads: counter.waiting_threads,
        operations_count: counter.operations_count + 1
      }, true)
    } else {
      ({
        value: counter.value,
        lock_acquired: counter.lock_acquired,
        waiting_threads: counter.waiting_threads + 1,
        operations_count: counter.operations_count
      }, false)
    }
  }
  
  // Simulate releasing lock
  let release_lock = fn(counter: AtomicCounter) {
    if counter.lock_acquired {
      ({
        value: counter.value,
        lock_acquired: false,
        waiting_threads: if counter.waiting_threads > 0 { counter.waiting_threads - 1 } else { 0 },
        operations_count: counter.operations_count
      }, true)
    } else {
      (counter, false)
    }
  }
  
  // Atomic increment
  let atomic_increment = fn(counter: AtomicCounter, increment: Int) {
    let (locked_counter, acquired) = acquire_lock(counter, "thread-1")
    if acquired {
      let incremented = {
        value: locked_counter.value + increment,
        lock_acquired: locked_counter.lock_acquired,
        waiting_threads: locked_counter.waiting_threads,
        operations_count: locked_counter.operations_count + 1
      }
      let (unlocked_counter, _) = release_lock(incremented)
      unlocked_counter
    } else {
      counter  // Could not acquire lock
    }
  }
  
  // Atomic decrement
  let atomic_decrement = fn(counter: AtomicCounter, decrement: Int) {
    let (locked_counter, acquired) = acquire_lock(counter, "thread-1")
    if acquired {
      let decremented = {
        value: locked_counter.value - decrement,
        lock_acquired: locked_counter.lock_acquired,
        waiting_threads: locked_counter.waiting_threads,
        operations_count: locked_counter.operations_count + 1
      }
      let (unlocked_counter, _) = release_lock(decremented)
      unlocked_counter
    } else {
      counter  // Could not acquire lock
    }
  }
  
  // Test counter creation
  let counter = create_atomic_counter(0)
  assert_eq(counter.value, 0)
  assert_false(counter.lock_acquired)
  assert_eq(counter.waiting_threads, 0)
  assert_eq(counter.operations_count, 0)
  
  // Test atomic increment
  let counter1 = atomic_increment(counter, 5)
  assert_eq(counter1.value, 5)
  assert_false(counter1.lock_acquired)
  assert_eq(counter1.operations_count, 3)  // acquire + increment + release
  
  // Test atomic decrement
  let counter2 = atomic_decrement(counter1, 2)
  assert_eq(counter2.value, 3)
  assert_false(counter2.lock_acquired)
  assert_eq(counter2.operations_count, 6)  // previous 3 + acquire + decrement + release
  
  // Test concurrent operations simulation
  let counter3 = atomic_increment(counter2, 10)
  let counter4 = atomic_decrement(counter3, 5)
  let counter5 = atomic_increment(counter4, 2)
  
  assert_eq(counter5.value, 10)  // 3 + 10 - 5 + 2 = 10
  assert_false(counter5.lock_acquired)
}

// Test 2: Thread-Safe Queue
test "thread-safe queue implementation" {
  // Define queue node
  type QueueNode[T] = {
    value: T,
    next: Option[QueueNode[T]]
  }
  
  // Define thread-safe queue
  type ThreadSafeQueue[T] = {
    head: Option[QueueNode[T]],
    tail: Option[QueueNode[T]],
    size: Int,
    lock_acquired: Bool,
    enqueue_count: Int,
    dequeue_count: Int
  }
  
  // Create thread-safe queue
  let create_thread_safe_queue = fn() {
    {
      head: None,
      tail: None,
      size: 0,
      lock_acquired: false,
      enqueue_count: 0,
      dequeue_count: 0
    }
  }
  
  // Simulate lock acquisition
  let acquire_queue_lock = fn(queue: ThreadSafeQueue[T], thread_id: String) {
    if !queue.lock_acquired {
      ({
        head: queue.head,
        tail: queue.tail,
        size: queue.size,
        lock_acquired: true,
        enqueue_count: queue.enqueue_count,
        dequeue_count: queue.dequeue_count
      }, true)
    } else {
      (queue, false)
    }
  }
  
  // Simulate lock release
  let release_queue_lock = fn(queue: ThreadSafeQueue[T]) {
    if queue.lock_acquired {
      ({
        head: queue.head,
        tail: queue.tail,
        size: queue.size,
        lock_acquired: false,
        enqueue_count: queue.enqueue_count,
        dequeue_count: queue.dequeue_count
      }, true)
    } else {
      (queue, false)
    }
  }
  
  // Thread-safe enqueue
  let thread_safe_enqueue = fn(queue: ThreadSafeQueue[T], value: T, thread_id: String) {
    let (locked_queue, acquired) = acquire_queue_lock(queue, thread_id)
    if acquired {
      let new_node = { value, next: None }
      
      let (updated_head, updated_tail, updated_size) = match queue.tail {
        None => {
          // Empty queue
          (Some(new_node), Some(new_node), queue.size + 1)
        }
        Some(tail) => {
          // Non-empty queue
          let updated_tail = { tail | next: Some(new_node) }
          (queue.head, Some(new_node), queue.size + 1)
        }
      }
      
      let updated_queue = {
        head: updated_head,
        tail: updated_tail,
        size: updated_size,
        lock_acquired: locked_queue.lock_acquired,
        enqueue_count: locked_queue.enqueue_count + 1,
        dequeue_count: locked_queue.dequeue_count
      }
      
      let (unlocked_queue, _) = release_queue_lock(updated_queue)
      unlocked_queue
    } else {
      queue  // Could not acquire lock
    }
  }
  
  // Thread-safe dequeue
  let thread_safe_dequeue = fn(queue: ThreadSafeQueue[T], thread_id: String) {
    let (locked_queue, acquired) = acquire_queue_lock(queue, thread_id)
    if acquired {
      match locked_queue.head {
        None => {
          // Empty queue
          let (unlocked_queue, _) = release_queue_lock(locked_queue)
          (unlocked_queue, None)
        }
        Some(head) => {
          let updated_head = head.next
          let updated_tail = if updated_head == None { None } else { locked_queue.tail }
          let updated_size = locked_queue.size - 1
          
          let updated_queue = {
            head: updated_head,
            tail: updated_tail,
            size: updated_size,
            lock_acquired: locked_queue.lock_acquired,
            enqueue_count: locked_queue.enqueue_count,
            dequeue_count: locked_queue.dequeue_count + 1
          }
          
          let (unlocked_queue, _) = release_queue_lock(updated_queue)
          (unlocked_queue, Some(head.value))
        }
      }
    } else {
      (queue, None)  // Could not acquire lock
    }
  }
  
  // Test queue creation
  let queue = create_thread_safe_queue()
  assert_eq(queue.size, 0)
  assert_false(queue.lock_acquired)
  assert_eq(queue.enqueue_count, 0)
  assert_eq(queue.dequeue_count, 0)
  
  // Test enqueue operations
  let queue1 = thread_safe_enqueue(queue, "item1", "thread-1")
  assert_eq(queue1.size, 1)
  assert_eq(queue1.enqueue_count, 2)  // acquire + enqueue
  
  let queue2 = thread_safe_enqueue(queue1, "item2", "thread-2")
  assert_eq(queue2.size, 2)
  assert_eq(queue2.enqueue_count, 4)  // previous 2 + acquire + enqueue
  
  let queue3 = thread_safe_enqueue(queue2, "item3", "thread-1")
  assert_eq(queue3.size, 3)
  assert_eq(queue3.enqueue_count, 6)
  
  // Test dequeue operations
  let (queue4, dequeued1) = thread_safe_dequeue(queue3, "thread-3")
  match dequeued1 {
    Some(value) => assert_eq(value, "item1")
    None => assert_true(false)
  }
  assert_eq(queue4.size, 2)
  assert_eq(queue4.dequeue_count, 2)  // acquire + dequeue
  
  let (queue5, dequeued2) = thread_safe_dequeue(queue4, "thread-1")
  match dequeued2 {
    Some(value) => assert_eq(value, "item2")
    None => assert_true(false)
  }
  assert_eq(queue5.size, 1)
  assert_eq(queue5.dequeue_count, 4)
  
  let (queue6, dequeued3) = thread_safe_dequeue(queue5, "thread-2")
  match dequeued3 {
    Some(value) => assert_eq(value, "item3")
    None => assert_true(false)
  }
  assert_eq(queue6.size, 0)
  assert_eq(queue6.dequeue_count, 6)
  
  // Test dequeue from empty queue
  let (queue7, dequeued_empty) = thread_safe_dequeue(queue6, "thread-1")
  match dequeued_empty {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  assert_eq(queue7.size, 0)
}

// Test 3: Read-Write Lock Implementation
test "read-write lock implementation" {
  // Define read-write lock state
  enum LockState {
    Unlocked
    ReadLocked(Int)  // Number of readers
    WriteLocked(String)  // Writer thread ID
  }
  
  // Define read-write lock
  type ReadWriteLock = {
    state: LockState,
    waiting_readers: Int,
    waiting_writers: Int,
    lock_operations: Int
  }
  
  // Create read-write lock
  let create_read_write_lock = fn() {
    {
      state: LockState::Unlocked,
      waiting_readers: 0,
      waiting_writers: 0,
      lock_operations: 0
    }
  }
  
  // Acquire read lock
  let acquire_read_lock = fn(rw_lock: ReadWriteLock, thread_id: String) {
    match rw_lock.state {
      LockState::Unlocked => {
        ({
          state: LockState::ReadLocked(1),
          waiting_readers: rw_lock.waiting_readers,
          waiting_writers: rw_lock.waiting_writers,
          lock_operations: rw_lock.lock_operations + 1
        }, true)
      }
      LockState::ReadLocked(count) => {
        ({
          state: LockState::ReadLocked(count + 1),
          waiting_readers: rw_lock.waiting_readers,
          waiting_writers: rw_lock.waiting_writers,
          lock_operations: rw_lock.lock_operations + 1
        }, true)
      }
      LockState::WriteLocked(_) => {
        ({
          state: rw_lock.state,
          waiting_readers: rw_lock.waiting_readers + 1,
          waiting_writers: rw_lock.waiting_writers,
          lock_operations: rw_lock.lock_operations
        }, false)
      }
    }
  }
  
  // Release read lock
  let release_read_lock = fn(rw_lock: ReadWriteLock, thread_id: String) {
    match rw_lock.state {
      LockState::ReadLocked(count) => {
        if count > 1 {
          ({
            state: LockState::ReadLocked(count - 1),
            waiting_readers: if rw_lock.waiting_readers > 0 { rw_lock.waiting_readers - 1 } else { 0 },
            waiting_writers: rw_lock.waiting_writers,
            lock_operations: rw_lock.lock_operations + 1
          }, true)
        } else {
          ({
            state: LockState::Unlocked,
            waiting_readers: if rw_lock.waiting_readers > 0 { rw_lock.waiting_readers - 1 } else { 0 },
            waiting_writers: rw_lock.waiting_writers,
            lock_operations: rw_lock.lock_operations + 1
          }, true)
        }
      }
      _ => (rw_lock, false)
    }
  }
  
  // Acquire write lock
  let acquire_write_lock = fn(rw_lock: ReadWriteLock, thread_id: String) {
    match rw_lock.state {
      LockState::Unlocked => {
        ({
          state: LockState::WriteLocked(thread_id),
          waiting_readers: rw_lock.waiting_readers,
          waiting_writers: rw_lock.waiting_writers,
          lock_operations: rw_lock.lock_operations + 1
        }, true)
      }
      _ => {
        ({
          state: rw_lock.state,
          waiting_readers: rw_lock.waiting_readers,
          waiting_writers: rw_lock.waiting_writers + 1,
          lock_operations: rw_lock.lock_operations
        }, false)
      }
    }
  }
  
  // Release write lock
  let release_write_lock = fn(rw_lock: ReadWriteLock, thread_id: String) {
    match rw_lock.state {
      LockState::WriteLocked(writer_id) => {
        if writer_id == thread_id {
          ({
            state: LockState::Unlocked,
            waiting_readers: rw_lock.waiting_readers,
            waiting_writers: if rw_lock.waiting_writers > 0 { rw_lock.waiting_writers - 1 } else { 0 },
            lock_operations: rw_lock.lock_operations + 1
          }, true)
        } else {
          (rw_lock, false)
        }
      }
      _ => (rw_lock, false)
    }
  }
  
  // Test read-write lock creation
  let rw_lock = create_read_write_lock()
  match rw_lock.state {
    LockState::Unlocked => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(rw_lock.waiting_readers, 0)
  assert_eq(rw_lock.waiting_writers, 0)
  
  // Test multiple read locks
  let (rw_lock1, acquired1) = acquire_read_lock(rw_lock, "reader-1")
  assert_true(acquired1)
  match rw_lock1.state {
    LockState::ReadLocked(count) => assert_eq(count, 1)
    _ => assert_true(false)
  }
  
  let (rw_lock2, acquired2) = acquire_read_lock(rw_lock1, "reader-2")
  assert_true(acquired2)
  match rw_lock2.state {
    LockState::ReadLocked(count) => assert_eq(count, 2)
    _ => assert_true(false)
  }
  
  let (rw_lock3, acquired3) = acquire_read_lock(rw_lock2, "reader-3")
  assert_true(acquired3)
  match rw_lock3.state {
    LockState::ReadLocked(count) => assert_eq(count, 3)
    _ => assert_true(false)
  }
  
  // Test write lock blocked by read locks
  let (rw_lock4, acquired4) = acquire_write_lock(rw_lock3, "writer-1")
  assert_false(acquired4)
  assert_eq(rw_lock4.waiting_writers, 1)
  match rw_lock4.state {
    LockState::ReadLocked(count) => assert_eq(count, 3)
    _ => assert_true(false)
  }
  
  // Release read locks
  let (rw_lock5, released1) = release_read_lock(rw_lock4, "reader-1")
  assert_true(released1)
  match rw_lock5.state {
    LockState::ReadLocked(count) => assert_eq(count, 2)
    _ => assert_true(false)
  }
  
  let (rw_lock6, released2) = release_read_lock(rw_lock5, "reader-2")
  assert_true(released2)
  match rw_lock6.state {
    LockState::ReadLocked(count) => assert_eq(count, 1)
    _ => assert_true(false)
  }
  
  let (rw_lock7, released3) = release_read_lock(rw_lock6, "reader-3")
  assert_true(released3)
  match rw_lock7.state {
    LockState::Unlocked => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test write lock acquisition
  let (rw_lock8, acquired5) = acquire_write_lock(rw_lock7, "writer-1")
  assert_true(acquired5)
  match rw_lock8.state {
    LockState::WriteLocked(writer_id) => assert_eq(writer_id, "writer-1")
    _ => assert_true(false)
  }
  
  // Test read lock blocked by write lock
  let (rw_lock9, acquired6) = acquire_read_lock(rw_lock8, "reader-4")
  assert_false(acquired6)
  assert_eq(rw_lock9.waiting_readers, 1)
  match rw_lock9.state {
    LockState::WriteLocked(writer_id) => assert_eq(writer_id, "writer-1")
    _ => assert_true(false)
  }
  
  // Release write lock
  let (rw_lock10, released4) = release_write_lock(rw_lock9, "writer-1")
  assert_true(released4)
  match rw_lock10.state {
    LockState::Unlocked => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 4: Deadlock Detection and Prevention
test "deadlock detection and prevention" {
  // Define resource request
  type ResourceRequest = {
    thread_id: String,
    resource_id: String,
    timestamp: Int
  }
  
  // Define deadlock detector
  type DeadlockDetector = {
    resource_graph: Array[(String, String)],  // (thread_id, resource_id)
    wait_graph: Array[(String, String)],     // (thread_id, resource_id) - waiting for
    detected_deadlocks: Int,
    prevented_deadlocks: Int
  }
  
  // Create deadlock detector
  let create_deadlock_detector = fn() {
    {
      resource_graph: [],
      wait_graph: [],
      detected_deadlocks: 0,
      prevented_deadlocks: 0
    }
  }
  
  // Request resource
  let request_resource = fn(detector: DeadlockDetector, thread_id: String, resource_id: String, timestamp: Int) {
    let resource_owned = detector.resource_graph.any(fn(pair) { pair.1 == resource_id })
    
    if resource_owned {
      // Resource is owned, add to wait graph
      let updated_wait_graph = detector.wait_graph.push((thread_id, resource_id))
      
      ({
        resource_graph: detector.resource_graph,
        wait_graph: updated_wait_graph,
        detected_deadlocks: detector.detected_deadlocks,
        prevented_deadlocks: detector.prevented_deadlocks
      }, false)
    } else {
      // Resource is free, acquire it
      let updated_resource_graph = detector.resource_graph.push((thread_id, resource_id))
      
      ({
        resource_graph: updated_resource_graph,
        wait_graph: detector.wait_graph,
        detected_deadlocks: detector.detected_deadlocks,
        prevented_deadlocks: detector.prevented_deadlocks
      }, true)
    }
  }
  
  // Release resource
  let release_resource = fn(detector: DeadlockDetector, thread_id: String, resource_id: String) {
    // Remove from resource graph
    let updated_resource_graph = detector.resource_graph.filter(fn(pair) { 
      !(pair.0 == thread_id && pair.1 == resource_id) 
    })
    
    // Remove from wait graph
    let updated_wait_graph = detector.wait_graph.filter(fn(pair) { 
      !(pair.0 == thread_id && pair.1 == resource_id) 
    })
    
    ({
      resource_graph: updated_resource_graph,
      wait_graph: updated_wait_graph,
      detected_deadlocks: detector.detected_deadlocks,
      prevented_deadlocks: detector.prevented_deadlocks
    })
  }
  
  // Detect deadlock using cycle detection
  let detect_deadlock = fn(detector: DeadlockDetector) {
    // Build thread dependency graph
    let mut thread_dependencies = []
    
    for (thread, resource) in detector.wait_graph {
      match detector.resource_graph.find(fn(pair) { pair.1 == resource }) {
        Some((owner_thread, _)) => {
          thread_dependencies = thread_dependencies.push((thread, owner_thread))
        }
        None => ()
      }
    }
    
    // Simple cycle detection
    let has_cycle = thread_dependencies.length() > 0 && thread_dependencies.any(fn(dep) {
      thread_dependencies.any(fn(other_dep) {
        dep.0 == other_dep.1 && other_dep.0 == dep.1
      })
    })
    
    if has_cycle {
      ({
        resource_graph: detector.resource_graph,
        wait_graph: detector.wait_graph,
        detected_deadlocks: detector.detected_deadlocks + 1,
        prevented_deadlocks: detector.prevented_deadlocks
      }, true)
    } else {
      (detector, false)
    }
  }
  
  // Prevent deadlock using resource ordering
  let prevent_deadlock = fn(detector: DeadlockDetector, thread_id: String, resource_id: String) {
    // Simple prevention: if thread is waiting for any resource, don't allow new requests
    let thread_waiting = detector.wait_graph.any(fn(pair) { pair.0 == thread_id })
    
    if thread_waiting {
      ({
        resource_graph: detector.resource_graph,
        wait_graph: detector.wait_graph,
        detected_deadlocks: detector.detected_deadlocks,
        prevented_deadlocks: detector.prevented_deadlocks + 1
      }, false)
    } else {
      (detector, true)
    }
  }
  
  // Test deadlock detector creation
  let detector = create_deadlock_detector()
  assert_eq(detector.resource_graph.length(), 0)
  assert_eq(detector.wait_graph.length(), 0)
  assert_eq(detector.detected_deadlocks, 0)
  assert_eq(detector.prevented_deadlocks, 0)
  
  // Test resource acquisition
  let (detector1, acquired1) = request_resource(detector, "thread-1", "resource-1", 1640995200000)
  assert_true(acquired1)
  assert_eq(detector1.resource_graph.length(), 1)
  assert_eq(detector1.wait_graph.length(), 0)
  
  let (detector2, acquired2) = request_resource(detector1, "thread-2", "resource-2", 1640995200100)
  assert_true(acquired2)
  assert_eq(detector2.resource_graph.length(), 2)
  assert_eq(detector2.wait_graph.length(), 0)
  
  // Test resource waiting
  let (detector3, acquired3) = request_resource(detector2, "thread-1", "resource-2", 1640995200200)
  assert_false(acquired3)
  assert_eq(detector3.resource_graph.length(), 2)
  assert_eq(detector3.wait_graph.length(), 1)
  
  let (detector4, acquired4) = request_resource(detector3, "thread-2", "resource-1", 1640995200300)
  assert_false(acquired4)
  assert_eq(detector4.resource_graph.length(), 2)
  assert_eq(detector4.wait_graph.length(), 2)
  
  // Test deadlock detection
  let (detector5, has_deadlock) = detect_deadlock(detector4)
  assert_true(has_deadlock)
  assert_eq(detector5.detected_deadlocks, 1)
  
  // Test deadlock prevention
  let (detector6, can_acquire) = prevent_deadlock(detector5, "thread-3", "resource-3")
  assert_true(can_acquire)  // thread-3 is not waiting
  
  let (detector7, can_acquire2) = prevent_deadlock(detector6, "thread-1", "resource-3")
  assert_false(can_acquire2)  // thread-1 is already waiting
  assert_eq(detector7.prevented_deadlocks, 1)
  
  // Test resource release
  let detector8 = release_resource(detector7, "thread-1", "resource-1")
  assert_eq(detector8.resource_graph.length(), 1)
  
  let detector9 = release_resource(detector8, "thread-2", "resource-2")
  assert_eq(detector9.resource_graph.length(), 0)
}

// Test 5: Concurrent Data Structure Operations
test "concurrent data structure operations" {
  // Define concurrent map entry
  type MapEntry[K, V] = {
    key: K,
    value: V,
    version: Int,
    locked: Bool
  }
  
  // Define concurrent map
  type ConcurrentMap[K, V] = {
    entries: Array[MapEntry[K, V]],
    lock_table: Array[(K, Bool)],
    operations_count: Int,
    conflicts_count: Int
  }
  
  // Create concurrent map
  let create_concurrent_map = fn() {
    {
      entries: [],
      lock_table: [],
      operations_count: 0,
      conflicts_count: 0
    }
  }
  
  // Acquire lock for key
  let acquire_key_lock = fn(map: ConcurrentMap[K, V], key: K) {
    match map.lock_table.find_index(fn(pair) { pair.0 == key }) {
      Some(index) => {
        let (locked_key, is_locked) = map.lock_table[index]
        if !is_locked {
          let updated_lock_table = map.lock_table.update(index, (locked_key, true))
          ({
            entries: map.entries,
            lock_table: updated_lock_table,
            operations_count: map.operations_count + 1,
            conflicts_count: map.conflicts_count
          }, true)
        } else {
          ({
            entries: map.entries,
            lock_table: map.lock_table,
            operations_count: map.operations_count + 1,
            conflicts_count: map.conflicts_count + 1
          }, false)
        }
      }
      None => {
        // Key not in lock table, add it
        let updated_lock_table = map.lock_table.push((key, true))
        ({
          entries: map.entries,
          lock_table: updated_lock_table,
          operations_count: map.operations_count + 1,
          conflicts_count: map.conflicts_count
        }, true)
      }
    }
  }
  
  // Release lock for key
  let release_key_lock = fn(map: ConcurrentMap[K, V], key: K) {
    match map.lock_table.find_index(fn(pair) { pair.0 == key }) {
      Some(index) => {
        let (locked_key, _) = map.lock_table[index]
        let updated_lock_table = map.lock_table.update(index, (locked_key, false))
        ({
          entries: map.entries,
          lock_table: updated_lock_table,
          operations_count: map.operations_count + 1,
          conflicts_count: map.conflicts_count
        }, true)
      }
      None => {
        (map, false)
      }
    }
  }
  
  // Concurrent put operation
  let concurrent_put = fn(map: ConcurrentMap[K, V], key: K, value: V) {
    let (locked_map, acquired) = acquire_key_lock(map, key)
    if acquired {
      match locked_map.entries.find_index(fn(entry) { entry.key == key }) {
        Some(index) => {
          let entry = locked_map.entries[index]
          let updated_entry = {
            key: entry.key,
            value,
            version: entry.version + 1,
            locked: true
          }
          let updated_entries = locked_map.entries.update(index, updated_entry)
          
          let updated_map = {
            entries: updated_entries,
            lock_table: locked_map.lock_table,
            operations_count: locked_map.operations_count,
            conflicts_count: locked_map.conflicts_count
          }
          
          let (unlocked_map, _) = release_key_lock(updated_map, key)
          unlocked_map
        }
        None => {
          let new_entry = {
            key,
            value,
            version: 1,
            locked: true
          }
          let updated_entries = locked_map.entries.push(new_entry)
          
          let updated_map = {
            entries: updated_entries,
            lock_table: locked_map.lock_table,
            operations_count: locked_map.operations_count,
            conflicts_count: locked_map.conflicts_count
          }
          
          let (unlocked_map, _) = release_key_lock(updated_map, key)
          unlocked_map
        }
      }
    } else {
      map  // Could not acquire lock
    }
  }
  
  // Concurrent get operation
  let concurrent_get = fn(map: ConcurrentMap[K, V], key: K) {
    let (locked_map, acquired) = acquire_key_lock(map, key)
    if acquired {
      match locked_map.entries.find(fn(entry) { entry.key == key }) {
        Some(entry) => {
          let (unlocked_map, _) = release_key_lock(locked_map, key)
          (unlocked_map, Some(entry.value))
        }
        None => {
          let (unlocked_map, _) = release_key_lock(locked_map, key)
          (unlocked_map, None)
        }
      }
    } else {
      (map, None)  // Could not acquire lock
    }
  }
  
  // Test concurrent map creation
  let map = create_concurrent_map()
  assert_eq(map.entries.length(), 0)
  assert_eq(map.lock_table.length(), 0)
  assert_eq(map.operations_count, 0)
  assert_eq(map.conflicts_count, 0)
  
  // Test concurrent put operations
  let map1 = concurrent_put(map, "key1", "value1")
  assert_eq(map1.entries.length(), 1)
  assert_eq(map1.lock_table.length(), 1)
  assert_eq(map1.operations_count, 3)  // acquire + put + release
  assert_eq(map1.conflicts_count, 0)
  
  let map2 = concurrent_put(map1, "key2", "value2")
  assert_eq(map2.entries.length(), 2)
  assert_eq(map2.lock_table.length(), 2)
  
  // Test concurrent get operations
  let (map3, value1) = concurrent_get(map2, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  let (map4, value2) = concurrent_get(map3, "key2")
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  let (map5, value3) = concurrent_get(map4, "key3")
  match value3 {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test update operation
  let map6 = concurrent_put(map5, "key1", "updated_value1")
  match map6.entries.find(fn(entry) { entry.key == "key1" }) {
    Some(entry) => {
      assert_eq(entry.value, "updated_value1")
      assert_eq(entry.version, 2)
    }
    None => assert_true(false)
  }
  
  // Test conflict simulation
  let (conflict_map, acquired) = acquire_key_lock(map6, "key1")
  if acquired {
    // Try to acquire the same lock again (simulating concurrent access)
    let (_, acquired2) = acquire_key_lock(conflict_map, "key1")
    assert_false(acquired2)  // Should not acquire
    
    // Check conflict count
    assert_eq(conflict_map.conflicts_count, 1)
  }
}