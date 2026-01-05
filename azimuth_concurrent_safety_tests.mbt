// Azimuth Concurrent Safety Tests
// This file contains test cases for concurrent safety functionality

// Test 1: Thread-Safe Counter
test "thread-safe counter implementation" {
  // Define atomic counter structure
  type AtomicCounter = {
    value: Int,
    lock: Bool  // Simplified lock representation
  }
  
  // Create atomic counter
  let create_atomic_counter = fn(initial_value: Int) {
    {
      value: initial_value,
      lock: false
    }
  }
  
  // Acquire lock
  let acquire_lock = fn(counter: AtomicCounter) {
    if counter.lock {
      None  // Lock already held
    } else {
      Some({ counter | lock: true })  // Lock acquired
    }
  }
  
  // Release lock
  let release_lock = fn(counter: AtomicCounter) {
    { counter | lock: false }
  }
  
  // Atomic increment
  let atomic_increment = fn(counter: AtomicCounter) {
    match acquire_lock(counter) {
      Some(locked_counter) => {
        let new_value = locked_counter.value + 1
        let updated_counter = { locked_counter | value: new_value }
        let unlocked_counter = release_lock(updated_counter)
        Some(unlocked_counter)
      }
      None => None  // Could not acquire lock
    }
  }
  
  // Atomic decrement
  let atomic_decrement = fn(counter: AtomicCounter) {
    match acquire_lock(counter) {
      Some(locked_counter) => {
        let new_value = locked_counter.value - 1
        let updated_counter = { locked_counter | value: new_value }
        let unlocked_counter = release_lock(updated_counter)
        Some(unlocked_counter)
      }
      None => None  // Could not acquire lock
    }
  }
  
  // Atomic compare and swap
  let atomic_compare_and_swap = fn(counter: AtomicCounter, expected: Int, new_value: Int) {
    match acquire_lock(counter) {
      Some(locked_counter) => {
        if locked_counter.value == expected {
          let updated_counter = { locked_counter | value: new_value }
          let unlocked_counter = release_lock(updated_counter)
          Some((true, unlocked_counter))
        } else {
          let unlocked_counter = release_lock(locked_counter)
          Some((false, unlocked_counter))
        }
      }
      None => None  // Could not acquire lock
    }
  }
  
  // Test counter creation
  let counter = create_atomic_counter(0)
  assert_eq(counter.value, 0)
  assert_false(counter.lock)
  
  // Test atomic increment
  match atomic_increment(counter) {
    Some(updated_counter) => {
      assert_eq(updated_counter.value, 1)
      assert_false(updated_counter.lock)  // Lock should be released
    }
    None => assert_true(false)
  }
  
  // Test atomic decrement
  let counter_after_increment = match atomic_increment(counter) {
    Some(c) => c
    None => counter
  }
  
  match atomic_decrement(counter_after_increment) {
    Some(updated_counter) => {
      assert_eq(updated_counter.value, 1)  // 2 - 1 = 1
      assert_false(updated_counter.lock)
    }
    None => assert_true(false)
  }
  
  // Test compare and swap
  let counter_for_cas = create_atomic_counter(10)
  
  // Successful CAS
  match atomic_compare_and_swap(counter_for_cas, 10, 20) {
    Some((success, updated_counter)) => {
      assert_true(success)
      assert_eq(updated_counter.value, 20)
    }
    None => assert_true(false)
  }
  
  // Failed CAS
  let counter_after_cas = match atomic_compare_and_swap(counter_for_cas, 10, 20) {
    Some((_, c)) => c
    None => counter_for_cas
  }
  
  match atomic_compare_and_swap(counter_after_cas, 10, 30) {
    Some((success, updated_counter)) => {
      assert_false(success)  // Value is now 20, not 10
      assert_eq(updated_counter.value, 20)  // Value unchanged
    }
    None => assert_true(false)
  }
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
    lock: Bool
  }
  
  // Create empty queue
  let create_queue = fn() {
    {
      head: None,
      tail: None,
      size: 0,
      lock: false
    }
  }
  
  // Acquire lock
  let acquire_queue_lock = fn[T](queue: ThreadSafeQueue[T]) {
    if queue.lock {
      None
    } else {
      Some({ queue | lock: true })
    }
  }
  
  // Release lock
  let release_queue_lock = fn[T](queue: ThreadSafeQueue[T]) {
    { queue | lock: false }
  }
  
  // Enqueue operation
  let enqueue = fn[T](queue: ThreadSafeQueue[T], value: T) {
    match acquire_queue_lock(queue) {
      Some(locked_queue) => {
        let new_node = { value, next: None }
        
        let updated_queue = match locked_queue.tail {
          None => {
            // Queue is empty
            { 
              head: Some(new_node), 
              tail: Some(new_node), 
              size: 1, 
              lock: true 
            }
          }
          Some(current_tail) => {
            // Queue has elements
            let updated_tail = { current_tail | next: Some(new_node) }
            { 
              head: locked_queue.head, 
              tail: Some(new_node), 
              size: locked_queue.size + 1, 
              lock: true 
            }
          }
        }
        
        let unlocked_queue = release_queue_lock(updated_queue)
        Some(unlocked_queue)
      }
      None => None
    }
  }
  
  // Dequeue operation
  let dequeue = fn[T](queue: ThreadSafeQueue[T]) {
    match acquire_queue_lock(queue) {
      Some(locked_queue) => {
        match locked_queue.head {
          None => {
            // Queue is empty
            let unlocked_queue = release_queue_lock(locked_queue)
            Some((None, unlocked_queue))
          }
          Some(current_head) => {
            // Queue has elements
            let value = current_head.value
            let new_head = current_head.next
            
            let updated_queue = match new_head {
              None => {
                // Queue becomes empty
                { 
                  head: None, 
                  tail: None, 
                  size: 0, 
                  lock: true 
                }
              }
              Some(_) => {
                // Queue still has elements
                { 
                  head: new_head, 
                  tail: locked_queue.tail, 
                  size: locked_queue.size - 1, 
                  lock: true 
                }
              }
            }
            
            let unlocked_queue = release_queue_lock(updated_queue)
            Some((Some(value), unlocked_queue))
          }
        }
      }
      None => None
    }
  }
  
  // Test queue creation
  let queue = create_queue()
  assert_eq(queue.size, 0)
  assert_eq(queue.head, None)
  assert_eq(queue.tail, None)
  
  // Test enqueue
  let queue_with_one = match enqueue(queue, "first") {
    Some(q) => q
    None => queue
  }
  
  assert_eq(queue_with_one.size, 1)
  
  let queue_with_two = match enqueue(queue_with_one, "second") {
    Some(q) => q
    None => queue_with_one
  }
  
  assert_eq(queue_with_two.size, 2)
  
  let queue_with_three = match enqueue(queue_with_two, "third") {
    Some(q) => q
    None => queue_with_two
  }
  
  assert_eq(queue_with_three.size, 3)
  
  // Test dequeue
  match dequeue(queue_with_three) {
    Some((Some(value), updated_queue)) => {
      assert_eq(value, "first")
      assert_eq(updated_queue.size, 2)
    }
    _ => assert_true(false)
  }
  
  let queue_after_first_dequeue = match dequeue(queue_with_three) {
    Some((_, q)) => q
    None => queue_with_three
  }
  
  match dequeue(queue_after_first_dequeue) {
    Some((Some(value), updated_queue)) => {
      assert_eq(value, "second")
      assert_eq(updated_queue.size, 1)
    }
    _ => assert_true(false)
  }
  
  let queue_after_second_dequeue = match dequeue(queue_after_first_dequeue) {
    Some((_, q)) => q
    None => queue_after_first_dequeue
  }
  
  match dequeue(queue_after_second_dequeue) {
    Some((Some(value), updated_queue)) => {
      assert_eq(value, "third")
      assert_eq(updated_queue.size, 0)
    }
    _ => assert_true(false)
  }
  
  let queue_after_third_dequeue = match dequeue(queue_after_second_dequeue) {
    Some((_, q)) => q
    None => queue_after_second_dequeue
  }
  
  // Test dequeue from empty queue
  match dequeue(queue_after_third_dequeue) {
    Some((None, updated_queue)) => {
      assert_eq(updated_queue.size, 0)
    }
    _ => assert_true(false)
  }
}

// Test 3: Thread-Safe Map
test "thread-safe map implementation" {
  // Define map entry
  type MapEntry[K, V] = {
    key: K,
    value: V,
    next: Option[MapEntry[K, V]]
  }
  
  // Define thread-safe map
  type ThreadSafeMap[K, V] = {
    buckets: Array[Option[MapEntry[K, V]]>,
    size: Int,
    lock: Bool
  }
  
  // Simple hash function for strings
  let string_hash = fn(key: String, bucket_count: Int) {
    let mut hash = 0
    for i in 0 .. key.length() {
      hash = (hash + key.to_char_array()[i].to_int()) % bucket_count
    }
    hash
  }
  
  // Create map with specified bucket count
  let create_map = fn(bucket_count: Int) {
    let mut buckets = []
    for i in 0 .. bucket_count {
      buckets = buckets.push(None)
    }
    {
      buckets,
      size: 0,
      lock: false
    }
  }
  
  // Acquire lock
  let acquire_map_lock = fn[K, V](map: ThreadSafeMap[K, V]) {
    if map.lock {
      None
    } else {
      Some({ map | lock: true })
    }
  }
  
  // Release lock
  let release_map_lock = fn[K, V](map: ThreadSafeMap[K, V]) {
    { map | lock: false }
  }
  
  // Put key-value pair
  let put = fn(map: ThreadSafeMap[String, String], key: String, value: String) {
    match acquire_map_lock(map) {
      Some(locked_map) => {
        let bucket_index = string_hash(key, locked_map.buckets.length())
        let bucket = locked_map.buckets[bucket_index]
        
        let updated_bucket = match bucket {
          None => {
            // Bucket is empty
            Some({ key, value, next: None })
          }
          Some(entry) => {
            // Check if key already exists
            let mut found = false
            let mut updated_entry = entry
            
            let mut current = entry
            while match current.next { Some(_) => true, None => false } {
              match current.next {
                Some(next_entry) => {
                  if next_entry.key == key {
                    found = true
                    updated_entry = { next_entry | value }
                    break
                  }
                  current = next_entry
                }
                None => break
              }
            }
            
            if found {
              bucket  // Entry updated
            } else if entry.key == key {
              Some({ entry | value })  // First entry updated
            } else {
              // Add new entry to the front
              Some({ key, value, next: Some(entry) })
            }
          }
        }
        
        let mut updated_buckets = locked_map.buckets
        updated_buckets[bucket_index] = updated_bucket
        
        let new_size = match bucket {
          None => locked_map.size + 1
          Some(entry) => {
            // Check if key already existed
            let mut key_existed = false
            let mut current = entry
            
            while match current.next { Some(_) => true, None => false } {
              match current.next {
                Some(next_entry) => {
                  if next_entry.key == key {
                    key_existed = true
                    break
                  }
                  current = next_entry
                }
                None => break
              }
            }
            
            if key_existed or entry.key == key {
              locked_map.size  // Key existed, size unchanged
            } else {
              locked_map.size + 1  // New key added
            }
          }
        }
        
        let updated_map = {
          buckets: updated_buckets,
          size: new_size,
          lock: true
        }
        
        let unlocked_map = release_map_lock(updated_map)
        Some(unlocked_map)
      }
      None => None
    }
  }
  
  // Get value by key
  let get = fn(map: ThreadSafeMap[String, String], key: String) {
    match acquire_map_lock(map) {
      Some(locked_map) => {
        let bucket_index = string_hash(key, locked_map.buckets.length())
        let bucket = locked_map.buckets[bucket_index]
        
        let result = match bucket {
          None => None
          Some(entry) => {
            if entry.key == key {
              Some(entry.value)
            } else {
              let mut current = entry
              let mut found = None
              
              while match current.next { Some(_) => true, None => false } {
                match current.next {
                  Some(next_entry) => {
                    if next_entry.key == key {
                      found = Some(next_entry.value)
                      break
                    }
                    current = next_entry
                  }
                  None => break
                }
              }
              
              found
            }
          }
        }
        
        let unlocked_map = release_map_lock(locked_map)
        Some((result, unlocked_map))
      }
      None => None
    }
  }
  
  // Test map creation
  let map = create_map(4)
  assert_eq(map.size, 0)
  assert_eq(map.buckets.length(), 4)
  
  // Test put operations
  let map_with_one = match put(map, "key1", "value1") {
    Some(m) => m
    None => map
  }
  
  assert_eq(map_with_one.size, 1)
  
  let map_with_two = match put(map_with_one, "key2", "value2") {
    Some(m) => m
    None => map_with_one
  }
  
  assert_eq(map_with_two.size, 2)
  
  let map_with_three = match put(map_with_two, "key3", "value3") {
    Some(m) => m
    None => map_with_two
  }
  
  assert_eq(map_with_three.size, 3)
  
  // Test get operations
  match get(map_with_three, "key1") {
    Some((Some(value), _)) => assert_eq(value, "value1")
    _ => assert_true(false)
  }
  
  match get(map_with_three, "key2") {
    Some((Some(value), _)) => assert_eq(value, "value2")
    _ => assert_true(false)
  }
  
  match get(map_with_three, "key3") {
    Some((Some(value), _)) => assert_eq(value, "value3")
    _ => assert_true(false)
  }
  
  match get(map_with_three, "nonexistent") {
    Some((None, _)) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test update existing key
  let map_with_update = match put(map_with_three, "key1", "new_value1") {
    Some(m) => m
    None => map_with_three
  }
  
  assert_eq(map_with_update.size, 3)  // Size unchanged
  
  match get(map_with_update, "key1") {
    Some((Some(value), _)) => assert_eq(value, "new_value1")
    _ => assert_true(false)
  }
}

// Test 4: Thread Pool Implementation
test "thread pool implementation" {
  // Define task
  type Task = {
    id: Int,
    work: () -> String,
    status: String  // "pending", "running", "completed"
    result: Option[String>
  }
  
  // Define worker
  type Worker = {
    id: Int,
    current_task: Option[Int>,
    busy: Bool
  }
  
  // Define thread pool
  type ThreadPool = {
    workers: Array[Worker],
    task_queue: Array[Task>,
    completed_tasks: Array[Task>,
    lock: Bool
  }
  
  // Create thread pool
  let create_thread_pool = fn(worker_count: Int) {
    let mut workers = []
    for i in 0 .. worker_count {
      workers = workers.push({
        id: i,
        current_task: None,
        busy: false
      })
    }
    
    {
      workers,
      task_queue: [],
      completed_tasks: [],
      lock: false
    }
  }
  
  // Acquire lock
  let acquire_pool_lock = fn(pool: ThreadPool) {
    if pool.lock {
      None
    } else {
      Some({ pool | lock: true })
    }
  }
  
  // Release lock
  let release_pool_lock = fn(pool: ThreadPool) {
    { pool | lock: false }
  }
  
  // Submit task
  let submit_task = fn(pool: ThreadPool, work: () -> String) {
    match acquire_pool_lock(pool) {
      Some(locked_pool) => {
        let task_id = locked_pool.task_queue.length() + 
                      locked_pool.completed_tasks.length() + 
                      locked_pool.workers.filter(fn(w) { match w.current_task { Some(_) => true, None => false } }).length()
        
        let new_task = {
          id: task_id,
          work,
          status: "pending",
          result: None
        }
        
        let updated_pool = {
          workers: locked_pool.workers,
          task_queue: locked_pool.task_queue.push(new_task),
          completed_tasks: locked_pool.completed_tasks,
          lock: true
        }
        
        let unlocked_pool = release_pool_lock(updated_pool)
        Some(unlocked_pool)
      }
      None => None
    }
  }
  
  // Assign task to worker
  let assign_task_to_worker = fn(pool: ThreadPool) {
    match acquire_pool_lock(pool) {
      Some(locked_pool) => {
        // Find available worker
        let available_worker = locked_pool.workers.find(fn(w) { not(w.busy) })
        
        match available_worker {
          Some(worker) => {
            // Get next task from queue
            if locked_pool.task_queue.length() > 0 {
              let task = locked_pool.task_queue[0]
              let remaining_tasks = locked_pool.task_queue.slice(1)
              
              // Update worker
              let updated_workers = locked_pool.workers.map(fn(w) {
                if w.id == worker.id {
                  { w | current_task: Some(task.id), busy: true }
                } else {
                  w
                }
              })
              
              // Update task status
              let updated_task = { task | status: "running" }
              let updated_queue = remaining_tasks.push(updated_task)
              
              let updated_pool = {
                workers: updated_workers,
                task_queue: updated_queue,
                completed_tasks: locked_pool.completed_tasks,
                lock: true
              }
              
              let unlocked_pool = release_pool_lock(updated_pool)
              Some(unlocked_pool)
            } else {
              // No tasks in queue
              let unlocked_pool = release_pool_lock(locked_pool)
              Some(unlocked_pool)
            }
          }
          None => {
            // No available workers
            let unlocked_pool = release_pool_lock(locked_pool)
            Some(unlocked_pool)
          }
        }
      }
      None => None
    }
  }
  
  // Complete task
  let complete_task = fn(pool: ThreadPool, task_id: Int) {
    match acquire_pool_lock(pool) {
      Some(locked_pool) => {
        // Find the task
        let task_option = locked_pool.task_queue.find(fn(t) { t.id == task_id && t.status == "running" })
        
        match task_option {
          Some(task) => {
            // Execute the task
            let result = task.work()
            
            // Create completed task
            let completed_task = {
              id: task.id,
              work: task.work,
              status: "completed",
              result: Some(result)
            }
            
            // Find and update worker
            let updated_workers = locked_pool.workers.map(fn(w) {
              match w.current_task {
                Some(current_task_id) if current_task_id == task_id => {
                  { w | current_task: None, busy: false }
                }
                _ => w
              }
            })
            
            // Remove task from queue and add to completed
            let filtered_queue = locked_pool.task_queue.filter(fn(t) { t.id != task_id })
            let updated_completed = locked_pool.completed_tasks.push(completed_task)
            
            let updated_pool = {
              workers: updated_workers,
              task_queue: filtered_queue,
              completed_tasks: updated_completed,
              lock: true
            }
            
            let unlocked_pool = release_pool_lock(updated_pool)
            Some(unlocked_pool)
          }
          None => {
            // Task not found
            let unlocked_pool = release_pool_lock(locked_pool)
            Some(unlocked_pool)
          }
        }
      }
      None => None
    }
  }
  
  // Test thread pool creation
  let pool = create_thread_pool(3)
  assert_eq(pool.workers.length(), 3)
  assert_eq(pool.task_queue.length(), 0)
  assert_eq(pool.completed_tasks.length(), 0)
  
  // Test task submission
  let pool_with_task = match submit_task(pool, fn() { "Task 1 result" }) {
    Some(p) => p
    None => pool
  }
  
  assert_eq(pool_with_task.task_queue.length(), 1)
  assert_eq(pool_with_task.task_queue[0].status, "pending")
  
  let pool_with_more_tasks = pool_with_task
    |> fn(p) { match submit_task(p, fn() { "Task 2 result" }) { Some(p) => p, None => p } }
    |> fn(p) { match submit_task(p, fn() { "Task 3 result" }) { Some(p) => p, None => p } }
  
  assert_eq(pool_with_more_tasks.task_queue.length(), 3)
  
  // Test task assignment
  let pool_with_assigned = match assign_task_to_worker(pool_with_more_tasks) {
    Some(p) => p
    None => pool_with_more_tasks
  }
  
  // Check if a worker is now busy
  let busy_workers = pool_with_assigned.workers.filter(fn(w) { w.busy })
  assert_eq(busy_workers.length(), 1)
  
  // Check if a task is now running
  let running_tasks = pool_with_assigned.task_queue.filter(fn(t) { t.status == "running" })
  assert_eq(running_tasks.length(), 1)
  
  // Test task completion
  let running_task_id = match running_tasks[0] {
    task => task.id
  }
  
  let pool_with_completed = match complete_task(pool_with_assigned, running_task_id) {
    Some(p) => p
    None => pool_with_assigned
  }
  
  // Check if task is completed
  assert_eq(pool_with_completed.completed_tasks.length(), 1)
  assert_eq(pool_with_completed.completed_tasks[0].status, "completed")
  assert_eq(pool_with_completed.completed_tasks[0].result, Some("Task 1 result"))
  
  // Check if worker is no longer busy
  let busy_workers_after_completion = pool_with_completed.workers.filter(fn(w) { w.busy })
  assert_eq(busy_workers_after_completion.length(), 0)
}

// Test 5: Read-Write Lock
test "read-write lock implementation" {
  // Define read-write lock state
  enum RWLockState {
    Unlocked
    ReadLocked(Int)  // Number of readers
    WriteLocked
  }
  
  // Define read-write lock
  type RWLock = {
    state: RWLockState,
    waiting_writers: Int,
    waiting_readers: Int
  }
  
  // Create read-write lock
  let create_rw_lock = fn() {
    {
      state: RWLockState::Unlocked,
      waiting_writers: 0,
      waiting_readers: 0
    }
  }
  
  // Acquire read lock
  let acquire_read_lock = fn(lock: RWLock) {
    match lock.state {
      RWLockState::Unlocked => {
        // Can acquire read lock
        Some({ lock | state: RWLockState::ReadLocked(1) })
      }
      RWLockState::ReadLocked(count) => {
        // Can acquire read lock if no waiting writers
        if lock.waiting_writers == 0 {
          Some({ lock | state: RWLockState::ReadLocked(count + 1) })
        } else {
          None  // Must wait for writers
        }
      }
      RWLockState::WriteLocked => {
        // Cannot acquire read lock when write locked
        None
      }
    }
  }
  
  // Acquire write lock
  let acquire_write_lock = fn(lock: RWLock) {
    match lock.state {
      RWLockState::Unlocked => {
        // Can acquire write lock
        Some({ lock | state: RWLockState::WriteLocked })
      }
      RWLockState::ReadLocked(_) => {
        // Cannot acquire write lock when read locked
        None
      }
      RWLockState::WriteLocked => {
        // Cannot acquire write lock when already write locked
        None
      }
    }
  }
  
  // Release read lock
  let release_read_lock = fn(lock: RWLock) {
    match lock.state {
      RWLockState::ReadLocked(1) => {
        // Last reader, unlock
        { lock | state: RWLockState::Unlocked }
      }
      RWLockState::ReadLocked(count) => {
        // Still more readers
        { lock | state: RWLockState::ReadLocked(count - 1) }
      }
      _ => lock  // Invalid state
    }
  }
  
  // Release write lock
  let release_write_lock = fn(lock: RWLock) {
    match lock.state {
      RWLockState::WriteLocked => {
        { lock | state: RWLockState::Unlocked }
      }
      _ => lock  // Invalid state
    }
  }
  
  // Test lock creation
  let rw_lock = create_rw_lock()
  assert_eq(rw_lock.state, RWLockState::Unlocked)
  assert_eq(rw_lock.waiting_writers, 0)
  assert_eq(rw_lock.waiting_readers, 0)
  
  // Test acquiring read lock
  match acquire_read_lock(rw_lock) {
    Some(locked) => {
      match locked.state {
        RWLockState::ReadLocked(count) => assert_eq(count, 1)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let rw_lock_with_read = match acquire_read_lock(rw_lock) {
    Some(l) => l
    None => rw_lock
  }
  
  // Test acquiring multiple read locks
  match acquire_read_lock(rw_lock_with_read) {
    Some(locked) => {
      match locked.state {
        RWLockState::ReadLocked(count) => assert_eq(count, 2)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let rw_lock_with_two_reads = match acquire_read_lock(rw_lock_with_read) {
    Some(l) => l
    None => rw_lock_with_read
  }
  
  // Test releasing read lock
  let rw_lock_after_release = release_read_lock(rw_lock_with_two_reads)
  match rw_lock_after_release.state {
    RWLockState::ReadLocked(count) => assert_eq(count, 1)
    _ => assert_true(false)
  }
  
  // Test acquiring write lock (should fail with read locks)
  match acquire_write_lock(rw_lock_after_release) {
    Some(_) => assert_true(false)  // Should not succeed
    None => assert_true(true)      // Should fail
  }
  
  // Release remaining read lock
  let rw_lock_unlocked = release_read_lock(rw_lock_after_release)
  assert_eq(rw_lock_unlocked.state, RWLockState::Unlocked)
  
  // Test acquiring write lock
  match acquire_write_lock(rw_lock_unlocked) {
    Some(locked) => {
      assert_eq(locked.state, RWLockState::WriteLocked)
      
      // Test acquiring read lock (should fail with write lock)
      match acquire_read_lock(locked) {
        Some(_) => assert_true(false)  // Should not succeed
        None => assert_true(true)      // Should fail
      }
      
      // Test acquiring another write lock (should fail)
      match acquire_write_lock(locked) {
        Some(_) => assert_true(false)  // Should not succeed
        None => assert_true(true)      // Should fail
      }
      
      // Release write lock
      let unlocked = release_write_lock(locked)
      assert_eq(unlocked.state, RWLockState::Unlocked)
    }
    None => assert_true(false)
  }
}

// Test 6: Deadlock Detection
test "deadlock detection and prevention" {
  // Define resource request
  type ResourceRequest = {
    process_id: Int,
    resource_id: Int,
    timestamp: Int
  }
  
  // Define resource allocation
  type ResourceAllocation = {
    process_id: Int,
    resource_id: Int,
    timestamp: Int
  }
  
  // Define system state
  type SystemState = {
    processes: Array[Int>,
    resources: Array[Int>,
    allocations: Array[ResourceAllocation>,
    requests: Array[ResourceRequest>
  }
  
  // Create initial system state
  let create_system_state = fn(processes: Array[Int>, resources: Array[Int>) {
    {
      processes,
      resources,
      allocations: [],
      requests: []
    }
  }
  
  // Request resource
  let request_resource = fn(state: SystemState, process_id: Int, resource_id: Int, timestamp: Int) {
    let request = { process_id, resource_id, timestamp }
    { state | requests: state.requests.push(request) }
  }
  
  // Allocate resource
  let allocate_resource = fn(state: SystemState, process_id: Int, resource_id: Int, timestamp: Int) {
    // Remove request
    let filtered_requests = state.requests.filter(fn(r) { 
      not(r.process_id == process_id && r.resource_id == resource_id) 
    })
    
    // Add allocation
    let allocation = { process_id, resource_id, timestamp }
    { 
      state | 
      allocations: state.allocations.push(allocation),
      requests: filtered_requests
    }
  }
  
  // Release resource
  let release_resource = fn(state: SystemState, process_id: Int, resource_id: Int) {
    // Remove allocation
    let filtered_allocations = state.allocations.filter(fn(a) { 
      not(a.process_id == process_id && a.resource_id == resource_id) 
    })
    
    { state | allocations: filtered_allocations }
  }
  
  // Detect deadlock using wait-for graph
  let detect_deadlock = fn(state: SystemState) {
    // Build wait-for graph
    let mut edges = []
    
    // For each request, add edge from process to the process holding the resource
    for request in state.requests {
      let holders = state.allocations.filter(fn(a) { a.resource_id == request.resource_id })
      
      for holder in holders {
        edges = edges.push((request.process_id, holder.process_id))
      }
    }
    
    // Detect cycles in the graph
    let has_cycle = fn(edges: Array<(Int, Int)>, nodes: Array[Int>) {
      for start_node in nodes {
        let mut visited = []
        let mut stack = [start_node]
        let mut found_cycle = false
        
        while stack.length() > 0 and not(found_cycle) {
          let current = stack[0]
          stack = stack.slice(1)
          
          if visited.contains(current) {
            found_cycle = true
            break
          }
          
          visited = visited.push(current)
          
          // Find all nodes that current is waiting for
          let outgoing = edges.filter(fn(e) { e.0 == current })
          for edge in outgoing {
            stack = stack.push(edge.1)
          }
        }
        
        if found_cycle {
          return true
        }
      }
      
      false
    }
    
    let processes_in_requests = state.requests.map(fn(r) { r.process_id })
    let processes_in_allocations = state.allocations.map(fn(a) { a.process_id })
    let all_processes = (processes_in_requests + processes_in_allocations)
      .sort(fn(a, b) { a <= b })
      .reduce(fn(acc, p) { 
        if acc.contains(p) { acc } else { acc.push(p) } 
      }, [])
    
    has_cycle(edges, all_processes)
  }
  
  // Create system state
  let state = create_system_state([1, 2, 3], [101, 102])
  
  // Allocate resources
  let state_with_allocations = state
    |> fn(s) { allocate_resource(s, 1, 101, 1640995200) }
    |> fn(s) { allocate_resource(s, 2, 102, 1640995210) }
  
  assert_eq(state_with_allocations.allocations.length(), 2)
  
  // Request resources (no deadlock yet)
  let state_with_requests = state_with_allocations
    |> fn(s) { request_resource(s, 1, 102, 1640995220) }  // Process 1 wants resource 102
    |> fn(s) { request_resource(s, 2, 101, 1640995230) }  // Process 2 wants resource 101
  
  assert_eq(state_with_requests.requests.length(), 2)
  
  // Check for deadlock
  assert_true(detect_deadlock(state_with_requests))  // Deadlock detected
  
  // Resolve deadlock by releasing a resource
  let state_after_release = release_resource(state_with_requests, 1, 101)
  
  // Check if deadlock is resolved
  assert_false(detect_deadlock(state_after_release))
  
  // Allocate the requested resource
  let state_resolved = allocate_resource(state_after_release, 2, 101, 1640995240)
  
  assert_eq(state_resolved.allocations.length(), 2)
  assert_eq(state_resolved.requests.length(), 1)  // Only one request remaining
  
  // No deadlock
  assert_false(detect_deadlock(state_resolved))
}