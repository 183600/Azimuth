// Azimuth Concurrent Safety Test Suite
// This file contains comprehensive test cases for concurrent safety and thread safety

// Test 1: Mutex-like Lock Implementation
test "mutex-like lock implementation" {
  // Simulate a mutex lock with atomic operations
  
  type Mutex {
    locked : Bool
    owner : Option[Int]
    wait_queue : Array[Int]
  }
  
  let create_mutex = fn() {
    { locked: false, owner: None, wait_queue: [] }
  }
  
  let try_lock = fn(mutex, thread_id) {
    if not mutex.locked {
      ({ locked: true, owner: Some(thread_id), wait_queue: mutex.wait_queue }, true)
    } else {
      (mutex, false)
    }
  }
  
  let lock = fn(mutex, thread_id) {
    if not mutex.locked {
      ({ locked: true, owner: Some(thread_id), wait_queue: mutex.wait_queue }, true)
    } else {
      // Add to wait queue
      let updated_wait_queue = mutex.wait_queue @ [thread_id]
      ({ locked: true, owner: mutex.owner, wait_queue: updated_wait_queue }, false)
    }
  }
  
  let unlock = fn(mutex, thread_id) {
    match mutex.owner {
      Some(owner_id) => {
        if owner_id == thread_id {
          if mutex.wait_queue.length() > 0 {
            // Transfer lock to next waiting thread
            let next_owner = mutex.wait_queue[0]
            let remaining_queue = mutex.wait_queue[1:mutex.wait_queue.length()]
            ({ locked: true, owner: Some(next_owner), wait_queue: remaining_queue }, true)
          } else {
            // No waiting threads, release lock
            ({ locked: false, owner: None, wait_queue: mutex.wait_queue }, true)
          }
        } else {
          // Not the owner, cannot unlock
          (mutex, false)
        }
      }
      None => (mutex, false)  // Not locked
    }
  }
  
  // Test basic lock/unlock
  let mutex = create_mutex()
  assert_false(mutex.locked)
  assert_eq(mutex.owner, None)
  
  let (locked_mutex, acquired) = try_lock(mutex, 1)
  assert_true(acquired)
  assert_true(locked_mutex.locked)
  assert_eq(locked_mutex.owner, Some(1))
  
  let (unlocked_mutex, unlocked) = unlock(locked_mutex, 1)
  assert_true(unlocked)
  assert_false(unlocked_mutex.locked)
  assert_eq(unlocked_mutex.owner, None)
  
  // Test failed unlock (not owner)
  let (locked_mutex2, _) = try_lock(unlocked_mutex, 2)
  let (_, failed_unlock) = unlock(locked_mutex2, 1)
  assert_false(failed_unlock)
  
  // Test successful unlock (owner)
  let (_, success_unlock) = unlock(locked_mutex2, 2)
  assert_true(success_unlock)
  
  // Test wait queue
  let (locked_mutex3, _) = try_lock(unlocked_mutex, 3)
  let (queued_mutex, _) = lock(locked_mutex3, 4)
  let (queued_mutex2, _) = lock(queued_mutex, 5)
  
  assert_eq(queued_mutex2.wait_queue.length(), 2)
  assert_eq(queued_mutex2.wait_queue[0], 4)
  assert_eq(queued_mutex2.wait_queue[1], 5)
  
  // Unlock should transfer to next waiting thread
  let (transferred_mutex, _) = unlock(queued_mutex2, 3)
  assert_eq(transferred_mutex.owner, Some(4))
  assert_eq(transferred_mutex.wait_queue.length(), 1)
  assert_eq(transferred_mutex.wait_queue[0], 5)
}

// Test 2: Atomic Counter Implementation
test "atomic counter implementation" {
  // Simulate atomic operations on a counter
  
  type AtomicCounter {
    value : Int
    pending_operations : Array[(Int, String)]  // (thread_id, operation)
  }
  
  let create_counter = fn(initial_value) {
    { value: initial_value, pending_operations: [] }
  }
  
  let atomic_increment = fn(counter, thread_id) {
    // Simulate atomic increment
    { value: counter.value + 1, pending_operations: counter.pending_operations @ [(thread_id, "increment")] }
  }
  
  let atomic_decrement = fn(counter, thread_id) {
    // Simulate atomic decrement
    { value: counter.value - 1, pending_operations: counter.pending_operations @ [(thread_id, "decrement")] }
  }
  
  let atomic_add = fn(counter, thread_id, amount) {
    // Simulate atomic add
    { value: counter.value + amount, pending_operations: counter.pending_operations @ [(thread_id, "add:" + amount.to_string())] }
  }
  
  let atomic_get = fn(counter) {
    counter.value
  }
  
  // Test atomic operations
  let counter = create_counter(10)
  assert_eq(atomic_get(counter), 10)
  
  let counter1 = atomic_increment(counter, 1)
  assert_eq(atomic_get(counter1), 11)
  
  let counter2 = atomic_decrement(counter1, 2)
  assert_eq(atomic_get(counter2), 10)
  
  let counter3 = atomic_add(counter2, 3, 5)
  assert_eq(atomic_get(counter3), 15)
  
  // Verify operation history
  assert_eq(counter3.pending_operations.length(), 3)
  assert_eq(counter3.pending_operations[0], (1, "increment"))
  assert_eq(counter3.pending_operations[1], (2, "decrement"))
  assert_eq(counter3.pending_operations[2], (3, "add:5"))
  
  // Test concurrent operations simulation
  let simulate_concurrent_operations = fn(initial_counter, operations) {
    let mut result_counter = initial_counter
    
    for (thread_id, op_type, value) in operations {
      match op_type {
        "increment" => result_counter = atomic_increment(result_counter, thread_id)
        "decrement" => result_counter = atomic_decrement(result_counter, thread_id)
        "add" => result_counter = atomic_add(result_counter, thread_id, value)
        _ => ()
      }
    }
    
    result_counter
  }
  
  let concurrent_ops = [
    (1, "increment", 0),
    (2, "add", 3),
    (3, "increment", 0),
    (4, "decrement", 0),
    (5, "add", 10)
  ]
  
  let final_counter = simulate_concurrent_operations(counter3, concurrent_ops)
  assert_eq(atomic_get(final_counter), 28)  // 15 + 1 + 3 + 1 - 1 + 10 = 29
}

// Test 3: Thread-Safe Queue Implementation
test "thread-safe queue implementation" {
  // Simulate a thread-safe queue with proper synchronization
  
  type ThreadSafeQueue {
    items : Array[Int]
    head : Int
    tail : Int
    size : Int
    capacity : Int
    pending_enqueues : Array[(Int, Int)]  // (thread_id, value)
    pending_dequeues : Array[Int]         // thread_id
  }
  
  let create_queue = fn(capacity) {
    let items = []
    for i in 0..capacity {
      items = items @ [0]
    }
    { items: items, head: 0, tail: 0, size: 0, capacity: capacity, pending_enqueues: [], pending_dequeues: [] }
  }
  
  let enqueue = fn(queue, thread_id, value) {
    if queue.size < queue.capacity {
      let mut new_items = queue.items
      new_items[queue.tail] = value
      let new_tail = (queue.tail + 1) % queue.capacity
      
      ({
        items: new_items,
        head: queue.head,
        tail: new_tail,
        size: queue.size + 1,
        capacity: queue.capacity,
        pending_enqueues: queue.pending_enqueues,
        pending_dequeues: queue.pending_dequeues
      }, true)
    } else {
      // Queue is full, add to pending operations
      ({
        items: queue.items,
        head: queue.head,
        tail: queue.tail,
        size: queue.size,
        capacity: queue.capacity,
        pending_enqueues: queue.pending_enqueues @ [(thread_id, value)],
        pending_dequeues: queue.pending_dequeues
      }, false)
    }
  }
  
  let dequeue = fn(queue, thread_id) {
    if queue.size > 0 {
      let value = queue.items[queue.head]
      let new_head = (queue.head + 1) % queue.capacity
      
      ({
        items: queue.items,
        head: new_head,
        tail: queue.tail,
        size: queue.size - 1,
        capacity: queue.capacity,
        pending_enqueues: queue.pending_enqueues,
        pending_dequeues: queue.pending_dequeues
      }, Some(value))
    } else {
      // Queue is empty, add to pending operations
      ({
        items: queue.items,
        head: queue.head,
        tail: queue.tail,
        size: queue.size,
        capacity: queue.capacity,
        pending_enqueues: queue.pending_enqueues,
        pending_dequeues: queue.pending_dequeues @ [thread_id]
      }, None)
    }
  }
  
  // Test queue operations
  let queue = create_queue(3)
  assert_eq(queue.size, 0)
  
  // Enqueue items
  let (queue1, success1) = enqueue(queue, 1, 10)
  assert_true(success1)
  assert_eq(queue1.size, 1)
  
  let (queue2, success2) = enqueue(queue1, 2, 20)
  assert_true(success2)
  assert_eq(queue2.size, 2)
  
  let (queue3, success3) = enqueue(queue2, 3, 30)
  assert_true(success3)
  assert_eq(queue3.size, 3)
  
  // Queue is now full
  let (queue4, success4) = enqueue(queue3, 4, 40)
  assert_false(success4)
  assert_eq(queue4.size, 3)
  assert_eq(queue4.pending_enqueues.length(), 1)
  assert_eq(queue4.pending_enqueues[0], (4, 40))
  
  // Dequeue items
  let (queue5, value1) = dequeue(queue4, 5)
  match value1 {
    Some(v) => assert_eq(v, 10)
    None => assert_true(false)
  }
  assert_eq(queue5.size, 2)
  
  let (queue6, value2) = dequeue(queue5, 6)
  match value2 {
    Some(v) => assert_eq(v, 20)
    None => assert_true(false)
  }
  assert_eq(queue6.size, 1)
  
  // Queue has space now, pending enqueue should be processed
  let (queue7, success5) = enqueue(queue6, 7, 50)
  assert_true(success5)
  assert_eq(queue7.size, 2)
  
  // Dequeue remaining items
  let (queue8, value3) = dequeue(queue7, 8)
  match value3 {
    Some(v) => assert_eq(v, 30)
    None => assert_true(false)
  }
  
  let (queue9, value4) = dequeue(queue8, 9)
  match value4 {
    Some(v) => assert_eq(v, 50)
    None => assert_true(false)
  }
  
  // Queue is empty now
  assert_eq(queue9.size, 0)
  
  // Dequeue from empty queue
  let (queue10, value5) = dequeue(queue9, 10)
  match value5 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  assert_eq(queue10.pending_dequeues.length(), 1)
  assert_eq(queue10.pending_dequeues[0], 10)
}

// Test 4: Read-Write Lock Implementation
test "read-write lock implementation" {
  // Simulate a read-write lock with multiple readers and exclusive writers
  
  type ReadWriteLock {
    state : String  // "unlocked", "reading", "writing"
    readers : Int
    writer : Option[Int]
    waiting_readers : Array[Int]
    waiting_writers : Array[Int]
  }
  
  let create_rwlock = fn() {
    { state: "unlocked", readers: 0, writer: None, waiting_readers: [], waiting_writers: [] }
  }
  
  let acquire_read_lock = fn(rwlock, thread_id) {
    match rwlock.state {
      "unlocked" => {
        ({ state: "reading", readers: 1, writer: None, waiting_readers: [], waiting_writers: rwlock.waiting_writers }, true)
      }
      "reading" => {
        ({ state: "reading", readers: rwlock.readers + 1, writer: None, waiting_readers: [], waiting_writers: rwlock.waiting_writers }, true)
      }
      "writing" => {
        // Add to waiting readers
        ({ state: "writing", readers: rwlock.readers, writer: rwlock.writer, waiting_readers: rwlock.waiting_readers @ [thread_id], waiting_writers: rwlock.waiting_writers }, false)
      }
      _ => (rwlock, false)
    }
  }
  
  let acquire_write_lock = fn(rwlock, thread_id) {
    match rwlock.state {
      "unlocked" => {
        ({ state: "writing", readers: 0, writer: Some(thread_id), waiting_readers: rwlock.waiting_readers, waiting_writers: [] }, true)
      }
      "reading" | "writing" => {
        // Add to waiting writers
        ({ state: rwlock.state, readers: rwlock.readers, writer: rwlock.writer, waiting_readers: rwlock.waiting_readers, waiting_writers: rwlock.waiting_writers @ [thread_id] }, false)
      }
      _ => (rwlock, false)
    }
  }
  
  let release_read_lock = fn(rwlock, thread_id) {
    if rwlock.state == "reading" and rwlock.readers > 0 {
      let new_readers = rwlock.readers - 1
      
      if new_readers == 0 {
        // No more readers, check if there are waiting writers
        if rwlock.waiting_writers.length() > 0 {
          let next_writer = rwlock.waiting_writers[0]
          let remaining_writers = rwlock.waiting_writers[1:rwlock.waiting_writers.length()]
          ({ state: "writing", readers: 0, writer: Some(next_writer), waiting_readers: rwlock.waiting_readers, waiting_writers: remaining_writers }, true)
        } else {
          ({ state: "unlocked", readers: 0, writer: None, waiting_readers: rwlock.waiting_readers, waiting_writers: rwlock.waiting_writers }, true)
        }
      } else {
        ({ state: "reading", readers: new_readers, writer: None, waiting_readers: rwlock.waiting_readers, waiting_writers: rwlock.waiting_writers }, true)
      }
    } else {
      (rwlock, false)
    }
  }
  
  let release_write_lock = fn(rwlock, thread_id) {
    if rwlock.state == "writing" and rwlock.writer == Some(thread_id) {
      // Check if there are waiting readers or writers
      if rwlock.waiting_readers.length() > 0 {
        // Grant read locks to all waiting readers
        let new_state = "reading"
        let new_readers = rwlock.waiting_readers.length()
        ({ state: new_state, readers: new_readers, writer: None, waiting_readers: [], waiting_writers: rwlock.waiting_writers }, true)
      } else if rwlock.waiting_writers.length() > 0 {
        // Grant write lock to next waiting writer
        let next_writer = rwlock.waiting_writers[0]
        let remaining_writers = rwlock.waiting_writers[1:rwlock.waiting_writers.length()]
        ({ state: "writing", readers: 0, writer: Some(next_writer), waiting_readers: rwlock.waiting_readers, waiting_writers: remaining_writers }, true)
      } else {
        ({ state: "unlocked", readers: 0, writer: None, waiting_readers: rwlock.waiting_readers, waiting_writers: rwlock.waiting_writers }, true)
      }
    } else {
      (rwlock, false)
    }
  }
  
  // Test read-write lock
  let rwlock = create_rwlock()
  assert_eq(rwlock.state, "unlocked")
  
  // Acquire read locks
  let (rwlock1, acquired1) = acquire_read_lock(rwlock, 1)
  assert_true(acquired1)
  assert_eq(rwlock1.state, "reading")
  assert_eq(rwlock1.readers, 1)
  
  let (rwlock2, acquired2) = acquire_read_lock(rwlock1, 2)
  assert_true(acquired2)
  assert_eq(rwlock2.state, "reading")
  assert_eq(rwlock2.readers, 2)
  
  // Try to acquire write lock (should fail)
  let (rwlock3, acquired3) = acquire_write_lock(rwlock2, 3)
  assert_false(acquired3)
  assert_eq(rwlock3.waiting_writers.length(), 1)
  assert_eq(rwlock3.waiting_writers[0], 3)
  
  // Release read locks
  let (rwlock4, released1) = release_read_lock(rwlock3, 1)
  assert_true(released1)
  assert_eq(rwlock4.state, "reading")
  assert_eq(rwlock4.readers, 1)
  
  let (rwlock5, released2) = release_read_lock(rwlock4, 2)
  assert_true(released2)
  // Should transition to writing state for waiting writer
  assert_eq(rwlock5.state, "writing")
  assert_eq(rwlock5.writer, Some(3))
  
  // Release write lock
  let (rwlock6, released3) = release_write_lock(rwlock5, 3)
  assert_true(released3)
  assert_eq(rwlock6.state, "unlocked")
  assert_eq(rwlock6.writer, None)
}

// Test 5: Thread-Safe Cache Implementation
test "thread-safe cache implementation" {
  // Simulate a thread-safe cache with proper synchronization
  
  type CacheEntry {
    key : String
    value : Int
    last_accessed : Int
    access_count : Int
  }
  
  type ThreadSafeCache {
    entries : Array[CacheEntry]
    capacity : Int
    lock : Bool
    pending_operations : Array[(Int, String, String)]  // (thread_id, operation, key)
  }
  
  let create_cache = fn(capacity) {
    { entries: [], capacity: capacity, lock: false, pending_operations: [] }
  }
  
  let cache_lock = fn(cache, thread_id) {
    if not cache.lock {
      ({ entries: cache.entries, capacity: cache.capacity, lock: true, pending_operations: cache.pending_operations }, true)
    } else {
      ({ entries: cache.entries, capacity: cache.capacity, lock: true, pending_operations: cache.pending_operations @ [(thread_id, "lock", "")] }, false)
    }
  }
  
  let cache_unlock = fn(cache, thread_id) {
    if cache.lock {
      ({ entries: cache.entries, capacity: cache.capacity, lock: false, pending_operations: cache.pending_operations }, true)
    } else {
      (cache, false)
    }
  }
  
  let cache_get = fn(cache, thread_id, key) {
    if cache.lock {
      let mut found_value = None
      let mut found_index = -1
      
      for i in 0..cache.entries.length() {
        if cache.entries[i].key == key {
          found_value = Some(cache.entries[i].value)
          found_index = i
          break
        }
      }
      
      // Update access information
      if found_index >= 0 {
        let mut updated_entries = cache.entries
        updated_entries[found_index] = {
          key: key,
          value: found_value.unwrap(),
          last_accessed: 1000,  // Simulated timestamp
          access_count: cache.entries[found_index].access_count + 1
        }
        
        ({
          entries: updated_entries,
          capacity: cache.capacity,
          lock: cache.lock,
          pending_operations: cache.pending_operations
        }, found_value)
      } else {
        (cache, found_value)
      }
    } else {
      (cache, None)
    }
  }
  
  let cache_put = fn(cache, thread_id, key, value) {
    if cache.lock {
      let mut existing_index = -1
      
      for i in 0..cache.entries.length() {
        if cache.entries[i].key == key {
          existing_index = i
          break
        }
      }
      
      if existing_index >= 0 {
        // Update existing entry
        let mut updated_entries = cache.entries
        updated_entries[existing_index] = {
          key: key,
          value: value,
          last_accessed: 1000,  // Simulated timestamp
          access_count: cache.entries[existing_index].access_count + 1
        }
        
        ({ entries: updated_entries, capacity: cache.capacity, lock: cache.lock, pending_operations: cache.pending_operations }, true)
      } else {
        // Add new entry
        let new_entry = { key: key, value: value, last_accessed: 1000, access_count: 1 }
        
        if cache.entries.length() < cache.capacity {
          // Space available
          ({ entries: cache.entries @ [new_entry], capacity: cache.capacity, lock: cache.lock, pending_operations: cache.pending_operations }, true)
        } else {
          // Find least recently used entry
          let mut lru_index = 0
          let mut lru_time = cache.entries[0].last_accessed
          
          for i in 1..cache.entries.length() {
            if cache.entries[i].last_accessed < lru_time {
              lru_index = i
              lru_time = cache.entries[i].last_accessed
            }
          }
          
          // Replace LRU entry
          let mut updated_entries = cache.entries
          updated_entries[lru_index] = new_entry
          
          ({ entries: updated_entries, capacity: cache.capacity, lock: cache.lock, pending_operations: cache.pending_operations }, true)
        }
      }
    } else {
      (cache, false)
    }
  }
  
  // Test thread-safe cache
  let cache = create_cache(2)
  
  // Acquire lock
  let (locked_cache, acquired) = cache_lock(cache, 1)
  assert_true(acquired)
  assert_true(locked_cache.lock)
  
  // Try to acquire lock again (should fail)
  let (_, failed_acquire) = cache_lock(locked_cache, 2)
  assert_false(failed_acquire)
  
  // Put values
  let (cache1, put1) = cache_put(locked_cache, 1, "key1", 100)
  assert_true(put1)
  assert_eq(cache1.entries.length(), 1)
  
  let (cache2, put2) = cache_put(cache1, 1, "key2", 200)
  assert_true(put2)
  assert_eq(cache2.entries.length(), 2)
  
  // Get values
  let (cache3, get1) = cache_get(cache2, 1, "key1")
  match get1 {
    Some(v) => assert_eq(v, 100)
    None => assert_true(false)
  }
  
  // Add third value (should evict LRU)
  let (cache4, put3) = cache_put(cache3, 1, "key3", 300)
  assert_true(put3)
  assert_eq(cache4.entries.length(), 2)
  
  // key1 should still be in cache (recently accessed)
  let (cache5, get2) = cache_get(cache4, 1, "key1")
  match get2 {
    Some(v) => assert_eq(v, 100)
    None => assert_true(false)
  }
  
  // key2 should be evicted
  let (_, get3) = cache_get(cache5, 1, "key2")
  match get3 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Release lock
  let (unlocked_cache, released) = cache_unlock(cache5, 1)
  assert_true(released)
  assert_false(unlocked_cache.lock)
}

// Test 6: Deadlock Detection
test "deadlock detection" {
  // Simulate deadlock detection in a resource allocation system
  
  type ResourceRequest {
    thread_id : Int
    resource_id : Int
    timestamp : Int
  }
  
  type DeadlockDetector {
    resources : Array[Int]  // Available resources
    allocated : Array[(Int, Int)]  // (thread_id, resource_id)
    waiting : Array[ResourceRequest]
    timestamp : Int
  }
  
  let create_detector = fn(resources) {
    { resources: resources, allocated: [], waiting: [], timestamp: 0 }
  }
  
  let request_resource = fn(detector, thread_id, resource_id) {
    // Check if resource is available
    let mut resource_available = true
    for (_, allocated_resource) in detector.allocated {
      if allocated_resource == resource_id {
        resource_available = false
        break
      }
    }
    
    if resource_available {
      // Allocate resource
      let new_allocated = detector.allocated @ [(thread_id, resource_id)]
      ({ resources: detector.resources, allocated: new_allocated, waiting: detector.waiting, timestamp: detector.timestamp + 1 }, true)
    } else {
      // Add to waiting queue
      let request = { thread_id: thread_id, resource_id: resource_id, timestamp: detector.timestamp }
      let new_waiting = detector.waiting @ [request]
      ({ resources: detector.resources, allocated: detector.allocated, waiting: new_waiting, timestamp: detector.timestamp + 1 }, false)
    }
  }
  
  let release_resource = fn(detector, thread_id, resource_id) {
    // Remove from allocated
    let mut new_allocated = []
    let mut found = false
    
    for (tid, rid) in detector.allocated {
      if tid == thread_id and rid == resource_id {
        found = true
      } else {
        new_allocated = new_allocated @ [(tid, rid)]
      }
    }
    
    if found {
      // Check if any waiting threads can now be served
      let mut new_waiting = detector.waiting
      let mut updated_waiting = []
      
      for request in new_waiting {
        if request.resource_id == resource_id {
          // Allocate to this waiting thread
          new_allocated = new_allocated @ [(request.thread_id, request.resource_id)]
        } else {
          updated_waiting = updated_waiting @ [request]
        }
      }
      
      ({ resources: detector.resources, allocated: new_allocated, waiting: updated_waiting, timestamp: detector.timestamp + 1 }, true)
    } else {
      (detector, false)
    }
  }
  
  let detect_deadlock = fn(detector) {
    // Simplified deadlock detection: check for circular wait
    let mut deadlocked_threads = []
    
    // Build wait graph
    let mut wait_graph = []  // Array of (waiting_thread, holding_thread)
    
    for waiting_request in detector.waiting {
      let waiting_thread = waiting_request.thread_id
      let wanted_resource = waiting_request.resource_id
      
      // Find which thread holds this resource
      for (holding_thread, held_resource) in detector.allocated {
        if held_resource == wanted_resource {
          wait_graph = wait_graph @ [(waiting_thread, holding_thread)]
          break
        }
      }
    }
    
    // Check for cycles (simplified)
    for (waiting, holding) in wait_graph {
      for (waiting2, holding2) in wait_graph {
        if holding == waiting2 and holding2 == waiting {
          deadlocked_threads = deadlocked_threads @ [waiting, waiting2]
        }
      }
    }
    
    deadlocked_threads
  }
  
  // Test deadlock detection
  let detector = create_detector([1, 2])
  
  // Allocate resources
  let (detector1, allocated1) = request_resource(detector, 1, 1)
  assert_true(allocated1)
  
  let (detector2, allocated2) = request_resource(detector1, 2, 2)
  assert_true(allocated2)
  
  // Create deadlock situation
  let (detector3, waiting1) = request_resource(detector2, 1, 2)
  assert_false(waiting1)  // Thread 1 waits for resource 2
  
  let (detector4, waiting2) = request_resource(detector3, 2, 1)
  assert_false(waiting2)  // Thread 2 waits for resource 1
  
  // Detect deadlock
  let deadlocked_threads = detect_deadlock(detector4)
  assert_eq(deadlocked_threads.length(), 2)
  assert_true(deadlocked_threads.contains(1))
  assert_true(deadlocked_threads.contains(2))
  
  // Resolve deadlock by having thread 1 release resource 1
  let (detector5, released1) = release_resource(detector4, 1, 1)
  assert_true(released1)
  
  // Now thread 2 should get resource 1
  assert_eq(detector5.allocated.length(), 2)
  
  // Check if deadlock is resolved
  let deadlocked_threads2 = detect_deadlock(detector5)
  assert_eq(deadlocked_threads2.length(), 0)
}

// Test 7: Race Condition Detection
test "race condition detection" {
  // Simulate race condition detection in shared data access
  
  type SharedData {
    value : Int
    access_log : Array[(Int, String, Int)]  // (thread_id, operation, timestamp)
  }
  
  type RaceConditionDetector {
    shared_data : SharedData
    operations : Array[(Int, String, Int, Int)]  // (thread_id, operation, old_value, new_value)
  }
  
  let create_detector = fn(initial_value) {
    { 
      shared_data: { value: initial_value, access_log: [] },
      operations: []
    }
  }
  
  let read_shared_data = fn(detector, thread_id) {
    let value = detector.shared_data.value
    let new_log = detector.shared_data.access_log @ [(thread_id, "read", 1000)]
    let updated_shared_data = { value: value, access_log: new_log }
    
    ({
      shared_data: updated_shared_data,
      operations: detector.operations @ [(thread_id, "read", value, value)]
    }, value)
  }
  
  let write_shared_data = fn(detector, thread_id, new_value) {
    let old_value = detector.shared_data.value
    let new_log = detector.shared_data.access_log @ [(thread_id, "write", 1000)]
    let updated_shared_data = { value: new_value, access_log: new_log }
    
    ({
      shared_data: updated_shared_data,
      operations: detector.operations @ [(thread_id, "write", old_value, new_value)]
    }, true)
  }
  
  let detect_race_conditions = fn(detector) {
    let mut race_conditions = []
    
    // Look for read-modify-write patterns without proper synchronization
    for i in 0..detector.operations.length() {
      for j in i + 1..detector.operations.length() {
        let op1 = detector.operations[i]
        let op2 = detector.operations[j]
        
        // Check if different threads accessed the same data
        if op1.0 != op2.0 {
          // Check for read-write race
          if op1.1 == "read" and op2.1 == "write" and op1.2 == op2.2 {
            race_conditions = race_conditions @ ["Read-write race between thread " + op1.0.to_string() + " and thread " + op2.0.to_string()]
          }
          
          // Check for write-write race
          if op1.1 == "write" and op2.1 == "write" and op1.2 != op2.2 {
            race_conditions = race_conditions @ ["Write-write race between thread " + op1.0.to_string() + " and thread " + op2.0.to_string()]
          }
        }
      }
    }
    
    race_conditions
  }
  
  // Test race condition detection
  let detector = create_detector(10)
  
  // Simulate concurrent operations
  let (detector1, read1) = read_shared_data(detector, 1)
  assert_eq(read1, 10)
  
  let (detector2, read2) = read_shared_data(detector1, 2)
  assert_eq(read2, 10)
  
  // Both threads read the same value, now both try to write
  let (detector3, write1) = write_shared_data(detector2, 1, 20)
  assert_true(write1)
  
  let (detector4, write2) = write_shared_data(detector3, 2, 30)
  assert_true(write2)
  
  // Detect race conditions
  let race_conditions = detect_race_conditions(detector4)
  assert_eq(race_conditions.length(), 2)  // Two race conditions detected
  
  // Test with proper synchronization simulation
  let synchronized_operations = fn(initial_detector) {
    let mut result_detector = initial_detector
    
    // Simulate proper synchronized access
    let (temp_detector, _) = read_shared_data(result_detector, 1)
    let (temp_detector2, _) = write_shared_data(temp_detector, 1, 20)
    result_detector = temp_detector2
    
    let (temp_detector3, _) = read_shared_data(result_detector, 2)
    let (temp_detector4, _) = write_shared_data(temp_detector3, 2, 30)
    result_detector = temp_detector4
    
    result_detector
  }
  
  let sync_detector = synchronized_operations(create_detector(10))
  let sync_race_conditions = detect_race_conditions(sync_detector)
  assert_eq(sync_race_conditions.length(), 0)  // No race conditions with proper synchronization
}

// Test 8: Concurrent Data Structure Integrity
test "concurrent data structure integrity" {
  // Test data structure integrity under concurrent operations
  
  type ConcurrentCounter {
    value : Int
    operations : Array[(Int, String, Int, Int)]  // (thread_id, operation, old_value, new_value)
  }
  
  let create_concurrent_counter = fn(initial_value) {
    { value: initial_value, operations: [] }
  }
  
  let concurrent_increment = fn(counter, thread_id) {
    let old_value = counter.value
    let new_value = old_value + 1
    let new_operations = counter.operations @ [(thread_id, "increment", old_value, new_value)]
    
    ({ value: new_value, operations: new_operations }, new_value)
  }
  
  let concurrent_decrement = fn(counter, thread_id) {
    let old_value = counter.value
    let new_value = old_value - 1
    let new_operations = counter.operations @ [(thread_id, "decrement", old_value, new_value)]
    
    ({ value: new_value, operations: new_operations }, new_value)
  }
  
  let verify_integrity = fn(counter) {
    let mut expected_value = 0
    let mut integrity_issues = []
    
    // Calculate expected value based on operations
    for (thread_id, operation, old_value, new_value) in counter.operations {
      match operation {
        "increment" => {
          if new_value != old_value + 1 {
            integrity_issues = integrity_issues @ ["Increment operation integrity issue: " + old_value.to_string() + " -> " + new_value.to_string()]
          }
          expected_value = expected_value + 1
        }
        "decrement" => {
          if new_value != old_value - 1 {
            integrity_issues = integrity_issues @ ["Decrement operation integrity issue: " + old_value.to_string() + " -> " + new_value.to_string()]
          }
          expected_value = expected_value - 1
        }
        _ => ()
      }
    }
    
    // Check if final value matches expected
    if counter.value != expected_value {
      integrity_issues = integrity_issues @ ["Final value mismatch: expected " + expected_value.to_string() + ", got " + counter.value.to_string()]
    }
    
    integrity_issues
  }
  
  // Test concurrent operations
  let counter = create_concurrent_counter(0)
  
  // Simulate concurrent operations
  let (counter1, _) = concurrent_increment(counter, 1)
  let (counter2, _) = concurrent_increment(counter1, 2)
  let (counter3, _) = concurrent_decrement(counter2, 3)
  let (counter4, _) = concurrent_increment(counter3, 1)
  let (counter5, _) = concurrent_decrement(counter4, 2)
  
  // Verify integrity
  let integrity_issues = verify_integrity(counter5)
  assert_eq(integrity_issues.length(), 0)
  assert_eq(counter5.value, 0)  // +1 +1 -1 +1 -1 = 1
  
  // Test with corrupted operations (simulating race conditions)
  let corrupted_operations = [
    (1, "increment", 0, 1),
    (2, "increment", 1, 3),  // Skipped value 2 (race condition)
    (3, "decrement", 3, 1),  // Decrement from wrong value
    (1, "increment", 1, 2)
  ]
  
  let corrupted_counter = {
    value: 2,
    operations: corrupted_operations
  }
  
  let corrupted_issues = verify_integrity(corrupted_counter)
  assert_eq(corrupted_issues.length(), 2)  // Two integrity issues detected
}

// Test 9: Memory Consistency Model
test "memory consistency model" {
  // Test memory consistency in concurrent operations
  
  type MemoryCell {
    value : Int
    version : Int
    access_history : Array[(Int, String, Int, Int)]  // (thread_id, operation, value, version)
  }
  
  type MemoryModel {
    cells : Array[MemoryCell]
    global_version : Int
  }
  
  let create_memory_model = fn(num_cells) {
    let mut cells = []
    for i in 0..num_cells {
      cells = cells @ [{ value: 0, version: 0, access_history: [] }]
    }
    { cells: cells, global_version: 0 }
  }
  
  let memory_write = fn(model, thread_id, cell_index, value) {
    if cell_index < model.cells.length() {
      let new_version = model.global_version + 1
      let old_cell = model.cells[cell_index]
      let new_history = old_cell.access_history @ [(thread_id, "write", value, new_version)]
      let new_cell = { value: value, version: new_version, access_history: new_history }
      
      let mut new_cells = model.cells
      new_cells[cell_index] = new_cell
      
      ({ cells: new_cells, global_version: new_version }, true)
    } else {
      (model, false)
    }
  }
  
  let memory_read = fn(model, thread_id, cell_index) {
    if cell_index < model.cells.length() {
      let cell = model.cells[cell_index]
      let new_history = cell.access_history @ [(thread_id, "read", cell.value, cell.version)]
      let updated_cell = { value: cell.value, version: cell.version, access_history: new_history }
      
      let mut new_cells = model.cells
      new_cells[cell_index] = updated_cell
      
      ({ cells: new_cells, global_version: model.global_version }, Some(cell.value))
    } else {
      (model, None)
    }
  }
  
  let verify_memory_consistency = fn(model) {
    let mut consistency_issues = []
    
    // Check for stale reads
    for i in 0..model.cells.length() {
      let cell = model.cells[i]
      let mut write_versions = []
      
      // Collect all write versions
      for (thread_id, operation, value, version) in cell.access_history {
        if operation == "write" {
          write_versions = write_versions @ [(thread_id, version)]
        }
      }
      
      // Check for reads that see older versions
      for (thread_id, operation, value, version) in cell.access_history {
        if operation == "read" {
          for (writer_thread, write_version) in write_versions {
            if write_version > version {
              consistency_issues = consistency_issues @ ["Stale read detected: thread " + thread_id.to_string() + " read version " + version.to_string() + " after thread " + writer_thread.to_string() + " wrote version " + write_version.to_string()]
            }
          }
        }
      }
    }
    
    consistency_issues
  }
  
  // Test memory consistency
  let model = create_memory_model(2)
  
  // Perform operations
  let (model1, wrote1) = memory_write(model, 1, 0, 10)
  assert_true(wrote1)
  
  let (model2, wrote2) = memory_write(model1, 2, 1, 20)
  assert_true(wrote2)
  
  let (model3, read1) = memory_read(model2, 3, 0)
  match read1 {
    Some(value) => assert_eq(value, 10)
    None => assert_true(false)
  }
  
  let (model4, read2) = memory_read(model3, 4, 1)
  match read2 {
    Some(value) => assert_eq(value, 20)
    None => assert_true(false)
  }
  
  // Verify consistency
  let consistency_issues = verify_memory_consistency(model4)
  assert_eq(consistency_issues.length(), 0)
  
  // Test with inconsistent operations
  let inconsistent_operations = [
    (1, "write", 10, 1),
    (2, "write", 15, 2),
    (3, "read", 10, 1)  // Read sees older version after newer write
  ]
  
  let inconsistent_cell = {
    value: 15,
    version: 2,
    access_history: inconsistent_operations
  }
  
  let inconsistent_model = {
    cells: [inconsistent_cell],
    global_version: 2
  }
  
  let inconsistent_issues = verify_memory_consistency(inconsistent_model)
  assert_eq(inconsistent_issues.length(), 1)  // One consistency issue detected
}

// Test 10: Concurrent Map Operations
test "concurrent map operations" {
  // Test thread-safe map operations
  
  type MapEntry {
    key : String
    value : Int
    version : Int
  }
  
  type ConcurrentMap {
    entries : Array[MapEntry]
    lock : Bool
    operations : Array[(Int, String, String, Int)]  // (thread_id, operation, key, version)
  }
  
  let create_concurrent_map = fn() {
    { entries: [], lock: false, operations: [] }
  }
  
  let map_lock = fn(map, thread_id) {
    if not map.lock {
      ({ entries: map.entries, lock: true, operations: map.operations @ [(thread_id, "lock", "", 0)] }, true)
    } else {
      (map, false)
    }
  }
  
  let map_unlock = fn(map, thread_id) {
    if map.lock {
      ({ entries: map.entries, lock: false, operations: map.operations @ [(thread_id, "unlock", "", 0)] }, true)
    } else {
      (map, false)
    }
  }
  
  let map_put = fn(map, thread_id, key, value) {
    if map.lock {
      let mut existing_index = -1
      
      for i in 0..map.entries.length() {
        if map.entries[i].key == key {
          existing_index = i
          break
        }
      }
      
      let new_version = if existing_index >= 0 {
        map.entries[existing_index].version + 1
      } else {
        1
      }
      
      let new_entry = { key: key, value: value, version: new_version }
      
      if existing_index >= 0 {
        // Update existing entry
        let mut new_entries = map.entries
        new_entries[existing_index] = new_entry
        
        ({
          entries: new_entries,
          lock: map.lock,
          operations: map.operations @ [(thread_id, "put", key, new_version)]
        }, true)
      } else {
        // Add new entry
        ({
          entries: map.entries @ [new_entry],
          lock: map.lock,
          operations: map.operations @ [(thread_id, "put", key, new_version)]
        }, true)
      }
    } else {
      (map, false)
    }
  }
  
  let map_get = fn(map, thread_id, key) {
    let mut found_value = None
    let mut found_version = 0
    
    for entry in map.entries {
      if entry.key == key {
        found_value = Some(entry.value)
        found_version = entry.version
        break
      }
    }
    
    let updated_operations = map.operations @ [(thread_id, "get", key, found_version)]
    
    ({
      entries: map.entries,
      lock: map.lock,
      operations: updated_operations
    }, found_value)
  }
  
  let map_remove = fn(map, thread_id, key) {
    if map.lock {
      let mut new_entries = []
      let mut found = false
      
      for entry in map.entries {
        if entry.key == key {
          found = true
        } else {
          new_entries = new_entries @ [entry]
        }
      }
      
      if found {
        ({
          entries: new_entries,
          lock: map.lock,
          operations: map.operations @ [(thread_id, "remove", key, 0)]
        }, true)
      } else {
        (map, false)
      }
    } else {
      (map, false)
    }
  }
  
  // Test concurrent map operations
  let map = create_concurrent_map()
  
  // Acquire lock
  let (locked_map, acquired) = map_lock(map, 1)
  assert_true(acquired)
  assert_true(locked_map.lock)
  
  // Put values
  let (map1, put1) = map_put(locked_map, 1, "key1", 100)
  assert_true(put1)
  assert_eq(map1.entries.length(), 1)
  
  let (map2, put2) = map_put(map1, 1, "key2", 200)
  assert_true(put2)
  assert_eq(map2.entries.length(), 2)
  
  // Get values
  let (map3, get1) = map_get(map2, 1, "key1")
  match get1 {
    Some(v) => assert_eq(v, 100)
    None => assert_true(false)
  }
  
  // Update existing key
  let (map4, put3) = map_put(map3, 1, "key1", 150)
  assert_true(put3)
  assert_eq(map4.entries.length(), 2)
  
  // Verify updated value
  let (map5, get2) = map_get(map4, 1, "key1")
  match get2 {
    Some(v) => assert_eq(v, 150)
    None => assert_true(false)
  }
  
  // Remove key
  let (map6, removed) = map_remove(map5, 1, "key2")
  assert_true(removed)
  assert_eq(map6.entries.length(), 1)
  
  // Verify removal
  let (map7, get3) = map_get(map6, 1, "key2")
  match get3 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Release lock
  let (unlocked_map, released) = map_unlock(map7, 1)
  assert_true(released)
  assert_false(unlocked_map.lock)
  
  // Verify operation history
  assert_eq(unlocked_map.operations.length(), 8)  // lock, put, put, get, put, get, remove, unlock
  assert_eq(unlocked_map.operations[0], (1, "lock", "", 0))
  assert_eq(unlocked_map.operations[1], (1, "put", "key1", 1))
  assert_eq(unlocked_map.operations[7], (1, "unlock", "", 0))
}