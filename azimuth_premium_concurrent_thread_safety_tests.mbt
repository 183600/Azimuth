// Azimuth Premium Test Suite - Concurrent Safety and Thread Operations
// This file contains comprehensive test cases for concurrent operations, thread safety, and synchronization

// Test 1: Atomic Operations and Compare-And-Swap
test "atomic operations and compare-and-swap" {
  // Simulate atomic integer operations
  let atomic_integer = |initial_value| {
    let mut value = initial_value
    let mut operations_log = []
    
    let atomic_add = |delta| {
      let old_value = value
      value = value + delta
      operations_log.push(("add", delta, old_value, value))
      value
    }
    
    let atomic_sub = |delta| {
      let old_value = value
      value = value - delta
      operations_log.push(("sub", delta, old_value, value))
      value
    }
    
    let atomic_compare_and_swap = |expected, new_value| {
      let old_value = value
      if old_value == expected {
        value = new_value
        operations_log.push(("cas", expected, old_value, new_value))
        true
      } else {
        operations_log.push(("cas_failed", expected, old_value, value))
        false
      }
    }
    
    let get_value = || {
      value
    }
    
    let get_operations_log = || {
      operations_log
    }
    
    (atomic_add, atomic_sub, atomic_compare_and_swap, get_value, get_operations_log)
  }
  
  let (add, sub, cas, get_value, get_log) = atomic_integer(0)
  
  // Test atomic add
  assert_eq(add(5), 5)
  assert_eq(add(3), 8)
  assert_eq(get_value(), 8)
  
  // Test atomic sub
  assert_eq(sub(2), 6)
  assert_eq(get_value(), 6)
  
  // Test successful compare-and-swap
  assert_true(cas(6, 10))
  assert_eq(get_value(), 10)
  
  // Test failed compare-and-swap
  assert_false(cas(6, 20))  // Value is 10, not 6
  assert_eq(get_value(), 10)
  
  // Test successful compare-and-swap after failure
  assert_true(cas(10, 20))
  assert_eq(get_value(), 20)
  
  // Verify operations log
  let log = get_log()
  assert_eq(log.length(), 6)
  assert_eq(log[0], ("add", 5, 0, 5))
  assert_eq(log[1], ("add", 3, 5, 8))
  assert_eq(log[2], ("sub", 2, 8, 6))
  assert_eq(log[3], ("cas", 6, 6, 10))
  assert_eq(log[4], ("cas_failed", 6, 10, 10))
  assert_eq(log[5], ("cas", 10, 10, 20))
  
  // Test atomic increment with overflow handling
  let atomic_increment_with_overflow = |max_value| {
    let mut counter = 0
    
    let increment = || {
      if counter >= max_value {
        counter = 0  // Wrap around
      } else {
        counter = counter + 1
      }
      counter
    }
    
    let get_counter = || {
      counter
    }
    
    (increment, get_counter)
  }
  
  let (inc, get_counter) = atomic_increment_with_overflow(100)
  
  for i = 0; i < 105; i = i + 1 {
    let value = inc()
    if i < 100 {
      assert_eq(value, i + 1)
    } else {
      assert_eq(value, i - 99)  // Wrapped around
    }
  }
  
  assert_eq(get_counter(), 5)  // 105 % 100 = 5
}

// Test 2: Mutex and Lock Mechanisms
test "mutex and lock mechanisms" {
  // Simulate mutex behavior
  let mutex = |initial_data| {
    let mut data = initial_data
    let mut locked_by = None
    let mut lock_count = 0
    let mut wait_queue = []
    
    let acquire = |thread_id| {
      if locked_by.is_none() {
        locked_by = Some(thread_id)
        lock_count = lock_count + 1
        true  // Successfully acquired
      } else if locked_by == Some(thread_id) {
        // Already owned by this thread (reentrant lock)
        lock_count = lock_count + 1
        true
      } else {
        // Lock is held by another thread, add to wait queue
        wait_queue.push(thread_id)
        false  // Failed to acquire
      }
    }
    
    let release = |thread_id| {
      if locked_by != Some(thread_id) {
        return false  // Not owned by this thread
      }
      
      lock_count = lock_count - 1
      
      if lock_count == 0 {
        // Release the lock
        locked_by = None
        
        // Give lock to next waiting thread
        if wait_queue.length() > 0 {
          let next_thread = wait_queue.shift()
          locked_by = Some(next_thread)
          lock_count = 1
        }
      }
      
      true  // Successfully released
    }
    
    let try_lock = |thread_id| {
      if locked_by.is_none() || locked_by == Some(thread_id) {
        acquire(thread_id)
      } else {
        false
      }
    }
    
    let read_data = |thread_id| {
      if locked_by == Some(thread_id) {
        data
      } else {
        None  // Access denied
      }
    }
    
    let write_data = |thread_id, new_data| {
      if locked_by == Some(thread_id) {
        data = new_data
        true
      } else {
        false  // Access denied
      }
    }
    
    let get_lock_info = || {
      { owner: locked_by, count: lock_count, waiting: wait_queue.length() }
    }
    
    (acquire, release, try_lock, read_data, write_data, get_lock_info)
  }
  
  let (acquire, release, try_lock, read_data, write_data, get_lock_info) = mutex("initial_data")
  
  // Test basic lock acquisition
  assert_true(acquire("thread1"))
  assert_eq(get_lock_info().owner, Some("thread1"))
  assert_eq(get_lock_info().count, 1)
  
  // Test reentrant locking
  assert_true(acquire("thread1"))
  assert_eq(get_lock_info().count, 2)
  
  // Test lock failure for other thread
  assert_false(acquire("thread2"))
  assert_eq(get_lock_info().waiting, 1)
  
  // Test data access with lock
  match read_data("thread1") {
    Some(data) => assert_eq(data, "initial_data")
    None => assert_true(false)
  }
  
  match read_data("thread2") {
    Some(_) => assert_true(false)
    None => {}  // Expected
  }
  
  // Test data modification with lock
  assert_true(write_data("thread1", "modified_data"))
  match read_data("thread1") {
    Some(data) => assert_eq(data, "modified_data")
    None => assert_true(false)
  }
  
  assert_false(write_data("thread2", "should_fail"))
  
  // Test lock release
  assert_true(release("thread1"))
  assert_eq(get_lock_info().count, 1)
  assert_eq(get_lock_info().owner, Some("thread1"))
  
  assert_true(release("thread1"))
  assert_eq(get_lock_info().count, 0)
  assert_eq(get_lock_info().owner, None)
  assert_eq(get_lock_info().waiting, 0)
  
  // Test try_lock
  assert_true(try_lock("thread3"))
  assert_eq(get_lock_info().owner, Some("thread3"))
  
  assert_false(try_lock("thread4"))
  assert_eq(get_lock_info().waiting, 1)
  
  // Test deadlock detection simulation
  let deadlock_detector = |threads| {
    let mut resource_graph = {}
    let mut wait_graph = {}
    
    let request_resource = |thread_id, resource_id| {
      if !resource_graph.contains(resource_id) {
        // Resource is free
        resource_graph[resource_id] = thread_id
        true
      } else {
        // Resource is held by another thread
        let holder = resource_graph[resource_id]
        if holder != thread_id {
          // Add to wait graph
          let current_waiters = wait_graph.get(thread_id, [])
          wait_graph[thread_id] = current_waiters + [resource_id]
          
          // Check for deadlock (cycle detection)
          let mut visited = []
          let mut stack = [thread_id]
          
          while stack.length() > 0 {
            let current = stack.pop()
            
            if visited.contains(current) {
              return false  // Deadlock detected
            }
            
            visited.push(current)
            
            if wait_graph.contains(current) {
              for waiting_resource in wait_graph[current] {
                if resource_graph.contains(waiting_resource) {
                  let holder = resource_graph[waiting_resource]
                  stack.push(holder)
                }
              }
            }
          }
        }
        false
      }
    }
    
    let release_resource = |thread_id, resource_id| {
      if resource_graph.contains(resource_id) && resource_graph[resource_id] == thread_id {
        resource_graph.remove(resource_id)
        
        // Remove from wait graph if present
        if wait_graph.contains(thread_id) {
          let mut new_waiters = []
          for waiting_resource in wait_graph[thread_id] {
            if waiting_resource != resource_id {
              new_waiters.push(waiting_resource)
            }
          }
          
          if new_waiters.length() > 0 {
            wait_graph[thread_id] = new_waiters
          } else {
            wait_graph.remove(thread_id)
          }
        }
        
        true
      } else {
        false
      }
    }
    
    (request_resource, release_resource)
  }
  
  let (request_resource, release_resource) = deadlock_detector(["thread1", "thread2"])
  
  // Normal operation
  assert_true(request_resource("thread1", "resource1"))
  assert_true(request_resource("thread2", "resource2"))
  assert_true(release_resource("thread1", "resource1"))
  assert_true(request_resource("thread1", "resource2"))  // Wait for thread2
  
  // Deadlock scenario
  assert_true(request_resource("thread1", "resource1"))
  assert_true(request_resource("thread2", "resource2"))
  
  // In a real implementation, this would detect deadlock
  // Our simplified simulation might not catch all cases
  assert_false(request_resource("thread1", "resource2"))  // thread1 waits for resource2
  assert_false(request_resource("thread2", "resource1"))  // thread2 waits for resource1
}

// Test 3: Thread-Safe Data Structures
test "thread-safe data structures" {
  // Thread-safe counter
  let thread_safe_counter = |initial_value| {
    let mut counter = initial_value
    let mut operations = []
    
    let increment = |thread_id| {
      counter = counter + 1
      operations.push(("increment", thread_id, counter))
      counter
    }
    
    let decrement = |thread_id| {
      counter = counter - 1
      operations.push(("decrement", thread_id, counter))
      counter
    }
    
    let get_value = || {
      counter
    }
    
    let get_operations = || {
      operations
    }
    
    (increment, decrement, get_value, get_operations)
  }
  
  let (inc, dec, get_value, get_operations) = thread_safe_counter(0)
  
  // Simulate concurrent operations
  let value1 = inc("thread1")
  let value2 = inc("thread2")
  let value3 = dec("thread3")
  let value4 = inc("thread4")
  let value5 = dec("thread5")
  
  assert_eq(value1, 1)
  assert_eq(value2, 2)
  assert_eq(value3, 1)
  assert_eq(value4, 2)
  assert_eq(value5, 1)
  assert_eq(get_value(), 1)
  
  let operations = get_operations()
  assert_eq(operations.length(), 5)
  
  // Thread-safe queue
  let thread_safe_queue = || {
    let mut queue = []
    let mut enqueue_operations = []
    let mut dequeue_operations = []
    
    let enqueue = |thread_id, item| {
      queue.push(item)
      enqueue_operations.push((thread_id, item))
      true
    }
    
    let dequeue = |thread_id| {
      if queue.length() > 0 {
        let item = queue.shift()
        dequeue_operations.push((thread_id, item))
        Some(item)
      } else {
        None
      }
    }
    
    let is_empty = || {
      queue.length() == 0
    }
    
    let size = || {
      queue.length()
    }
    
    let get_operations = || {
      { enqueues: enqueue_operations, dequeues: dequeue_operations }
    }
    
    (enqueue, dequeue, is_empty, size, get_operations)
  }
  
  let (enqueue, dequeue, is_empty, size, get_operations) = thread_safe_queue()
  
  assert_true(is_empty())
  assert_eq(size(), 0)
  
  // Simulate concurrent enqueue operations
  assert_true(enqueue("thread1", "item1"))
  assert_true(enqueue("thread2", "item2"))
  assert_true(enqueue("thread3", "item3"))
  
  assert_false(is_empty())
  assert_eq(size(), 3)
  
  // Simulate concurrent dequeue operations
  match dequeue("thread4") {
    Some(item) => assert_eq(item, "item1")
    None => assert_true(false)
  }
  
  match dequeue("thread5") {
    Some(item) => assert_eq(item, "item2")
    None => assert_true(false)
  }
  
  assert_eq(size(), 1)
  
  let operations = get_operations()
  assert_eq(operations.enqueues.length(), 3)
  assert_eq(operations.dequeues.length(), 2)
  
  // Thread-safe map
  let thread_safe_map = || {
    let mut map = {}
    let mut operations = []
    
    let put = |thread_id, key, value| {
      let old_value = map.get(key)
      map[key] = value
      operations.push(("put", thread_id, key, value, old_value))
      old_value
    }
    
    let get = |thread_id, key| {
      operations.push(("get", thread_id, key))
      map.get(key)
    }
    
    let remove = |thread_id, key| {
      let old_value = map.get(key)
      if old_value.is_some() {
        map.remove(key)
      }
      operations.push(("remove", thread_id, key, old_value))
      old_value
    }
    
    let contains_key = |thread_id, key| {
      operations.push(("contains_key", thread_id, key))
      map.contains(key)
    }
    
    let size = || {
      map.keys().length()
    }
    
    let get_operations = || {
      operations
    }
    
    (put, get, remove, contains_key, size, get_operations)
  }
  
  let (put, get, remove, contains_key, size, get_operations) = thread_safe_map()
  
  assert_eq(size(), 0)
  
  // Simulate concurrent put operations
  assert_eq(put("thread1", "key1", "value1"), None)
  assert_eq(put("thread2", "key2", "value2"), None)
  assert_eq(put("thread3", "key1", "new_value1"), Some("value1"))
  
  assert_eq(size(), 2)
  
  // Simulate concurrent get operations
  assert_eq(get("thread4", "key1"), Some("new_value1"))
  assert_eq(get("thread5", "key2"), Some("value2"))
  assert_eq(get("thread6", "key3"), None)
  
  // Simulate concurrent contains_key operations
  assert_true(contains_key("thread7", "key1"))
  assert_true(contains_key("thread8", "key2"))
  assert_false(contains_key("thread9", "key3"))
  
  // Simulate concurrent remove operations
  assert_eq(remove("thread10", "key1"), Some("new_value1"))
  assert_eq(remove("thread11", "key3"), None)
  
  assert_eq(size(), 1)
  
  let operations = get_operations()
  assert_eq(operations.length(), 10)
}

// Test 4: Read-Write Locks
test "read-write locks" {
  // Simulate read-write lock behavior
  let read_write_lock = || {
    let mut readers = []
    let mut writer = None
    let mut waiting_writers = []
    let mut waiting_readers = []
    let mut operations = []
    
    let acquire_read_lock = |thread_id| {
      if writer.is_none() && waiting_writers.length() == 0 {
        // No writer, can acquire read lock
        readers.push(thread_id)
        operations.push(("acquire_read", thread_id, readers.length()))
        true
      } else {
        // Writer active or waiting, must wait
        if !waiting_readers.contains(thread_id) {
          waiting_readers.push(thread_id)
        }
        operations.push(("wait_read", thread_id))
        false
      }
    }
    
    let release_read_lock = |thread_id| {
      if readers.contains(thread_id) {
        let mut new_readers = []
        for reader in readers {
          if reader != thread_id {
            new_readers.push(reader)
          }
        }
        readers = new_readers
        operations.push(("release_read", thread_id, readers.length()))
        
        // If no more readers and waiting writers, let a writer proceed
        if readers.length() == 0 && waiting_writers.length() > 0 {
          let next_writer = waiting_writers.shift()
          writer = Some(next_writer)
          operations.push(("writer_promoted", next_writer))
        }
        
        true
      } else {
        false
      }
    }
    
    let acquire_write_lock = |thread_id| {
      if writer.is_none() && readers.length() == 0 {
        // No writer and no readers, can acquire write lock
        writer = Some(thread_id)
        operations.push(("acquire_write", thread_id))
        true
      } else {
        // Writer active or readers active, must wait
        if !waiting_writers.contains(thread_id) {
          waiting_writers.push(thread_id)
        }
        operations.push(("wait_write", thread_id))
        false
      }
    }
    
    let release_write_lock = |thread_id| {
      if writer == Some(thread_id) {
        writer = None
        operations.push(("release_write", thread_id))
        
        // Prioritize waiting writers over readers
        if waiting_writers.length() > 0 {
          let next_writer = waiting_writers.shift()
          writer = Some(next_writer)
          operations.push(("writer_promoted", next_writer))
        } else if waiting_readers.length() > 0 {
          // Let all waiting readers proceed
          let new_readers = waiting_readers
          waiting_readers = []
          readers = readers + new_readers
          operations.push(("readers_promoted", new_readers.length()))
        }
        
        true
      } else {
        false
      }
    }
    
    let get_state = || {
      {
        readers: readers,
        writer: writer,
        waiting_readers: waiting_readers.length(),
        waiting_writers: waiting_writers.length()
      }
    }
    
    let get_operations = || {
      operations
    }
    
    (
      acquire_read_lock, release_read_lock,
      acquire_write_lock, release_write_lock,
      get_state, get_operations
    )
  }
  
  let (
    acquire_read, release_read,
    acquire_write, release_write,
    get_state, get_operations
  ) = read_write_lock()
  
  // Test multiple concurrent readers
  assert_true(acquire_read("thread1"))
  assert_true(acquire_read("thread2"))
  assert_true(acquire_read("thread3"))
  
  let state = get_state()
  assert_eq(state.readers.length(), 3)
  assert_eq(state.writer, None)
  assert_eq(state.waiting_writers, 0)
  
  // Writer must wait when readers are active
  assert_false(acquire_write("thread4"))
  assert_eq(get_state().waiting_writers, 1)
  
  // New readers must wait when writer is waiting
  assert_false(acquire_read("thread5"))
  assert_eq(get_state().waiting_readers, 1)
  
  // Release readers
  assert_true(release_read("thread1"))
  assert_eq(get_state().readers.length(), 2)
  assert_eq(get_state().writer, None)  // Still waiting for all readers to release
  
  assert_true(release_read("thread2"))
  assert_eq(get_state().readers.length(), 1)
  assert_eq(get_state().writer, None)  // Still waiting for all readers to release
  
  assert_true(release_read("thread3"))
  assert_eq(get_state().readers.length(), 0)
  assert_eq(get_state().writer, Some("thread4"))  // Writer promoted
  
  // Test exclusive write access
  assert_false(acquire_read("thread6"))  // Must wait for writer
  assert_false(acquire_write("thread7"))  // Must wait for writer
  
  assert_true(release_write("thread4"))
  
  // After writer releases, waiting writer gets priority
  assert_eq(get_state().writer, Some("thread7"))  // Next waiting writer promoted
  
  // Test reader promotion after all writers done
  assert_true(release_write("thread7"))
  assert_eq(get_state().readers.length(), 2)  // Waiting readers promoted
  assert_true(get_state().readers.contains("thread5"))
  assert_true(get_state().readers.contains("thread6"))
  
  // Verify operations log
  let operations = get_operations()
  assert_eq(operations.length(), 15)
}

// Test 5: Semaphore and Counting Semaphores
test "semaphore and counting semaphores" {
  // Simulate semaphore behavior
  let semaphore = |initial_permits| {
    let mut permits = initial_permits
    let mut waiting_threads = []
    let mut operations = []
    
    let acquire = |thread_id| {
      if permits > 0 {
        permits = permits - 1
        operations.push(("acquire", thread_id, permits))
        true
      } else {
        if !waiting_threads.contains(thread_id) {
          waiting_threads.push(thread_id)
        }
        operations.push(("wait", thread_id))
        false
      }
    }
    
    let release = |thread_id| {
      if waiting_threads.length() > 0 {
        // Give permit to first waiting thread
        let next_thread = waiting_threads.shift()
        operations.push(("transfer", thread_id, next_thread))
        true
      } else {
        permits = permits + 1
        operations.push(("release", thread_id, permits))
        true
      }
    }
    
    let try_acquire = |thread_id| {
      if permits > 0 {
        permits = permits - 1
        operations.push(("try_acquire", thread_id, permits))
        true
      } else {
        operations.push(("try_acquire_failed", thread_id))
        false
      }
    }
    
    let get_available_permits = || {
      permits
    }
    
    let get_waiting_threads = || {
      waiting_threads
    }
    
    let get_operations = || {
      operations
    }
    
    (acquire, release, try_acquire, get_available_permits, get_waiting_threads, get_operations)
  }
  
  let (acquire, release, try_acquire, get_permits, get_waiting, get_operations) = semaphore(3)
  
  // Test initial permits
  assert_eq(get_permits(), 3)
  assert_eq(get_waiting().length(), 0)
  
  // Test successful acquisitions
  assert_true(acquire("thread1"))
  assert_eq(get_permits(), 2)
  
  assert_true(acquire("thread2"))
  assert_eq(get_permits(), 1)
  
  assert_true(acquire("thread3"))
  assert_eq(get_permits(), 0)
  
  // Test acquisition when no permits available
  assert_false(acquire("thread4"))
  assert_eq(get_waiting().length(), 1)
  assert_true(get_waiting().contains("thread4"))
  
  assert_false(acquire("thread5"))
  assert_eq(get_waiting().length(), 2)
  assert_true(get_waiting().contains("thread5"))
  
  // Test try_acquire
  assert_false(try_acquire("thread6"))
  assert_eq(get_waiting().length(), 3)
  assert_true(get_waiting().contains("thread6"))
  
  // Test release with waiting threads
  assert_true(release("thread1"))
  assert_eq(get_permits(), 0)  // Permit transferred to waiting thread
  assert_eq(get_waiting().length(), 2)  // One waiting thread got the permit
  
  // Test release when no waiting threads
  assert_true(release("thread2"))
  assert_eq(get_permits(), 1)  // No waiting threads, permit added back
  assert_eq(get_waiting().length(), 2)
  
  // Test try_acquire with available permits
  assert_true(try_acquire("thread7"))
  assert_eq(get_permits(), 0)
  
  // Test binary semaphore (mutex)
  let binary_semaphore = semaphore(1)
  let (b_acquire, b_release, b_try_acquire, b_get_permits, b_get_waiting, _) = binary_semaphore
  
  assert_eq(b_get_permits(), 1)
  
  assert_true(b_acquire("thread1"))
  assert_eq(b_get_permits(), 0)
  
  assert_false(b_acquire("thread2"))
  assert_eq(b_get_waiting().length(), 1)
  
  assert_false(b_try_acquire("thread3"))
  assert_eq(b_get_waiting().length(), 2)
  
  assert_true(b_release("thread1"))
  assert_eq(b_get_waiting().length(), 1)  // Permit transferred to waiting thread
  
  // Verify operations log
  let operations = get_operations()
  assert_eq(operations.length(), 12)
}

// Test 6: Condition Variables and Waiting
test "condition variables and waiting" {
  // Simulate condition variable behavior
  let condition_variable = || {
    let mut waiting_threads = []
    let mut notifications = []
    let mut operations = []
    
    let wait = |thread_id, condition| {
      if condition() {
        operations.push(("wait_skip", thread_id, "condition already true"))
        true
      } else {
        waiting_threads.push((thread_id, condition))
        operations.push(("wait_start", thread_id))
        false
      }
    }
    
    let notify_one = |thread_id| {
      if waiting_threads.length() > 0 {
        let (waiting_thread, condition) = waiting_threads.shift()
        
        if condition() {
          operations.push(("notify_success", thread_id, waiting_thread))
          true
        } else {
          // Condition not met, put back in queue
          waiting_threads.push((waiting_thread, condition))
          operations.push(("notify_failed", thread_id, waiting_thread))
          false
        }
      } else {
        operations.push(("notify_empty", thread_id))
        false
      }
    }
    
    let notify_all = |thread_id| {
      if waiting_threads.length() > 0 {
        let mut notified_count = 0
        let mut still_waiting = []
        
        for (waiting_thread, condition) in waiting_threads {
          if condition() {
            notified_count = notified_count + 1
            operations.push(("notify_all_success", thread_id, waiting_thread))
          } else {
            still_waiting.push((waiting_thread, condition))
          }
        }
        
        waiting_threads = still_waiting
        operations.push(("notify_all_complete", thread_id, notified_count))
        notified_count
      } else {
        operations.push(("notify_all_empty", thread_id))
        0
      }
    }
    
    let get_waiting_count = || {
      waiting_threads.length()
    }
    
    let get_operations = || {
      operations
    }
    
    (wait, notify_one, notify_all, get_waiting_count, get_operations)
  }
  
  let (wait, notify_one, notify_all, get_waiting_count, get_operations) = condition_variable()
  
  // Test condition satisfaction
  let condition1 = || true
  let condition2 = || false
  
  assert_true(wait("thread1", condition1))  // Condition already true
  assert_eq(get_waiting_count(), 0)
  
  assert_false(wait("thread2", condition2))  // Condition false, thread waits
  assert_eq(get_waiting_count(), 1)
  
  assert_false(wait("thread3", condition2))  // Condition false, thread waits
  assert_eq(get_waiting_count(), 2)
  
  // Test notify one
  assert_false(notify_one("notifier1"))  // Condition still false
  assert_eq(get_waiting_count(), 2)
  
  // Change condition and notify
  let condition3 = || true
  let mut waiting_threads = [("thread2", condition2), ("thread3", condition3)]
  
  // Simulate condition change for thread3
  assert_true(notify_one("notifier2"))  // Thread3's condition is now true
  assert_eq(get_waiting_count(), 1)
  
  // Test notify all
  let condition4 = || true
  // Update thread2's condition
  waiting_threads = [("thread2", condition4)]
  
  assert_eq(notify_all("notifier3"), 1)  // Thread2's condition is now true
  assert_eq(get_waiting_count(), 0)
  
  // Test producer-consumer pattern
  let producer_consumer_condition = || {
    let mut buffer = []
    let buffer_size = 5
    let mut producers_waiting = []
    let mut consumers_waiting = []
    
    let produce = |thread_id, item| {
      if buffer.length() < buffer_size {
        buffer.push(item)
        true
      } else {
        producers_waiting.push((thread_id, item))
        false
      }
    }
    
    let consume = |thread_id| {
      if buffer.length() > 0 {
        let item = buffer.shift()
        Some(item)
      } else {
        consumers_waiting.push(thread_id)
        None
      }
    }
    
    let get_buffer_state = || {
      {
        buffer: buffer,
        size: buffer.length(),
        producers_waiting: producers_waiting.length(),
        consumers_waiting: consumers_waiting.length()
      }
    }
    
    (produce, consume, get_buffer_state)
  }
  
  let (produce, consume, get_buffer_state) = producer_consumer_condition()
  
  // Fill buffer
  assert_true(produce("producer1", "item1"))
  assert_true(produce("producer2", "item2"))
  assert_true(produce("producer3", "item3"))
  
  let state = get_buffer_state()
  assert_eq(state.size, 3)
  assert_eq(state.producers_waiting, 0)
  assert_eq(state.consumers_waiting, 0)
  
  // Consume items
  match consume("consumer1") {
    Some(item) => assert_eq(item, "item1")
    None => assert_true(false)
  }
  
  match consume("consumer2") {
    Some(item) => assert_eq(item, "item2")
    None => assert_true(false)
  }
  
  // Verify operations log
  let operations = get_operations()
  assert_eq(operations.length(), 10)
}

// Test 7: Thread Pools and Task Execution
test "thread pools and task execution" {
  // Simulate thread pool behavior
  let thread_pool = |pool_size| {
    let mut threads = []
    let mut task_queue = []
    let mut completed_tasks = []
    let mut task_id_counter = 0
    
    // Initialize threads
    for i = 0; i < pool_size; i = i + 1 {
      threads.push({
        id: "thread_" + i.to_string(),
        status: "idle",
        current_task: None
      })
    }
    
    let submit_task = |task| {
      let task_id = "task_" + task_id_counter.to_string()
      task_id_counter = task_id_counter + 1
      
      let task_info = {
        id: task_id,
        task: task,
        status: "queued",
        submitted_at: 1640995200000L + task_id_counter
      }
      
      task_queue.push(task_info)
      task_id
    }
    
    let execute_next_task = || {
      // Find idle thread
      let mut idle_thread_index = -1
      for i = 0; i < threads.length(); i = i + 1 {
        if threads[i].status == "idle" {
          idle_thread_index = i
          break
        }
      }
      
      if idle_thread_index >= 0 && task_queue.length() > 0 {
        let task = task_queue.shift()
        threads[idle_thread_index].status = "busy"
        threads[idle_thread_index].current_task = Some(task.id)
        
        // Simulate task execution
        let completed_task = {
          id: task.id,
          task: task.task,
          status: "completed",
          completed_at: 1640995200000L + task_id_counter + 1000,
          executed_by: threads[idle_thread_index].id
        }
        
        completed_tasks.push(completed_task)
        threads[idle_thread_index].status = "idle"
        threads[idle_thread_index].current_task = None
        
        Some(completed_task)
      } else {
        None
      }
    }
    
    let get_pool_status = || {
      let mut idle_count = 0
      let mut busy_count = 0
      
      for thread in threads {
        if thread.status == "idle" {
          idle_count = idle_count + 1
        } else {
          busy_count = busy_count + 1
        }
      }
      
      {
        total_threads: threads.length(),
        idle_threads: idle_count,
        busy_threads: busy_count,
        queued_tasks: task_queue.length(),
        completed_tasks: completed_tasks.length()
      }
    }
    
    let get_completed_tasks = || {
      completed_tasks
    }
    
    (submit_task, execute_next_task, get_pool_status, get_completed_tasks)
  }
  
  let (submit_task, execute_next_task, get_pool_status, get_completed_tasks) = thread_pool(3)
  
  // Test initial pool status
  let status = get_pool_status()
  assert_eq(status.total_threads, 3)
  assert_eq(status.idle_threads, 3)
  assert_eq(status.busy_threads, 0)
  assert_eq(status.queued_tasks, 0)
  
  // Submit tasks
  let task1 = submit_task("compute factorial")
  let task2 = submit_task("sort array")
  let task3 = submit_task("fetch data")
  let task4 = submit_task("process image")
  let task5 = submit_task("send email")
  
  let status = get_pool_status()
  assert_eq(status.queued_tasks, 5)
  
  // Execute tasks
  let completed1 = execute_next_task()
  assert_true(completed1.is_some())
  
  let completed2 = execute_next_task()
  assert_true(completed2.is_some())
  
  let completed3 = execute_next_task()
  assert_true(completed3.is_some())
  
  let status = get_pool_status()
  assert_eq(status.busy_threads, 0)  // All tasks completed immediately in our simulation
  assert_eq(status.queued_tasks, 2)
  assert_eq(status.completed_tasks, 3)
  
  // Execute remaining tasks
  let completed4 = execute_next_task()
  let completed5 = execute_next_task()
  
  let status = get_pool_status()
  assert_eq(status.queued_tasks, 0)
  assert_eq(status.completed_tasks, 5)
  
  // Test task execution beyond pool capacity
  let task6 = submit_task("backup database")
  let task7 = submit_task("generate report")
  
  // Execute with no available threads (all idle in our simulation)
  let completed6 = execute_next_task()
  let completed7 = execute_next_task()
  
  let status = get_pool_status()
  assert_eq(status.completed_tasks, 7)
  
  // Verify completed tasks
  let completed_tasks = get_completed_tasks()
  assert_eq(completed_tasks.length(), 7)
  
  // Test task priority simulation
  let priority_thread_pool = |pool_size| {
    let mut threads = []
    let mut high_priority_queue = []
    let mut normal_priority_queue = []
    let mut low_priority_queue = []
    let mut completed_tasks = []
    
    // Initialize threads
    for i = 0; i < pool_size; i = i + 1 {
      threads.push({
        id: "thread_" + i.to_string(),
        status: "idle",
        current_task: None
      })
    }
    
    let submit_task = |task, priority| {
      let task_info = {
        id: "task_" + completed_tasks.length().to_string(),
        task: task,
        priority: priority,
        status: "queued"
      }
      
      match priority {
        "high" => high_priority_queue.push(task_info),
        "normal" => normal_priority_queue.push(task_info),
        "low" => low_priority_queue.push(task_info),
        _ => normal_priority_queue.push(task_info)
      }
      
      task_info.id
    }
    
    let execute_next_task = || {
      // Find idle thread
      let mut idle_thread_index = -1
      for i = 0; i < threads.length(); i = i + 1 {
        if threads[i].status == "idle" {
          idle_thread_index = i
          break
        }
      }
      
      if idle_thread_index >= 0 {
        let task = if high_priority_queue.length() > 0 {
          high_priority_queue.shift()
        } else if normal_priority_queue.length() > 0 {
          normal_priority_queue.shift()
        } else if low_priority_queue.length() > 0 {
          low_priority_queue.shift()
        } else {
          None
        }
        
        match task {
          Some(task_info) => {
            threads[idle_thread_index].status = "busy"
            threads[idle_thread_index].current_task = Some(task_info.id)
            
            // Simulate task execution
            let completed_task = {
              id: task_info.id,
              task: task_info.task,
              priority: task_info.priority,
              status: "completed",
              executed_by: threads[idle_thread_index].id
            }
            
            completed_tasks.push(completed_task)
            threads[idle_thread_index].status = "idle"
            threads[idle_thread_index].current_task = None
            
            Some(completed_task)
          }
          None => None
        }
      } else {
        None
      }
    }
    
    let get_queue_status = || {
      {
        high_priority: high_priority_queue.length(),
        normal_priority: normal_priority_queue.length(),
        low_priority: low_priority_queue.length()
      }
    }
    
    (submit_task, execute_next_task, get_queue_status)
  }
  
  let (p_submit, p_execute, get_queue_status) = priority_thread_pool(2)
  
  // Submit tasks with different priorities
  p_submit("low priority task", "low")
  p_submit("high priority task", "high")
  p_submit("normal priority task", "normal")
  p_submit("another high priority task", "high")
  p_submit("another low priority task", "low")
  
  let queue_status = get_queue_status()
  assert_eq(queue_status.high_priority, 2)
  assert_eq(queue_status.normal_priority, 1)
  assert_eq(queue_status.low_priority, 2)
  
  // Execute tasks (high priority should execute first)
  let completed1 = p_execute()
  let completed2 = p_execute()
  let completed3 = p_execute()
  
  match completed1 {
    Some(task) => assert_eq(task.priority, "high")
    None => assert_true(false)
  }
  
  match completed2 {
    Some(task) => assert_eq(task.priority, "high")
    None => assert_true(false)
  }
  
  match completed3 {
    Some(task) => assert_eq(task.priority, "normal")
    None => assert_true(false)
  }
}

// Test 8: Concurrent Collections and Lock-Free Data Structures
test "concurrent collections and lock-free data structures" {
  // Simulate lock-free stack using compare-and-swap
  let lock_free_stack = || {
    let mut head = None
    let mut operations = []
    
    let push = |thread_id, value| {
      let new_node = { value: value, next: head }
      let old_head = head
      
      // Simulate CAS operation
      head = Some(new_node)
      operations.push(("push", thread_id, value))
      true
    }
    
    let pop = |thread_id| {
      match head {
        Some(node) => {
          head = node.next
          operations.push(("pop", thread_id, node.value))
          Some(node.value)
        }
        None => {
          operations.push(("pop_empty", thread_id))
          None
        }
      }
    }
    
    let is_empty = || {
      head.is_none()
    }
    
    let size = || {
      let mut count = 0
      let mut current = head
      
      while current.is_some() {
        match current {
          Some(node) => {
            count = count + 1
            current = node.next
          }
          None => break
        }
      }
      
      count
    }
    
    let get_operations = || {
      operations
    }
    
    (push, pop, is_empty, size, get_operations)
  }
  
  let (push, pop, is_empty, size, get_operations) = lock_free_stack()
  
  // Test stack operations
  assert_true(is_empty())
  assert_eq(size(), 0)
  
  assert_true(push("thread1", "item1"))
  assert_true(push("thread2", "item2"))
  assert_true(push("thread3", "item3"))
  
  assert_false(is_empty())
  assert_eq(size(), 3)
  
  match pop("thread4") {
    Some(value) => assert_eq(value, "item3")
    None => assert_true(false)
  }
  
  match pop("thread5") {
    Some(value) => assert_eq(value, "item2")
    None => assert_true(false)
  }
  
  assert_eq(size(), 1)
  
  // Simulate lock-free queue using atomic operations
  let lock_free_queue = || {
    let mut head = None
    let mut tail = None
    let mut operations = []
    
    let enqueue = |thread_id, value| {
      let new_node = { value: value, next: None }
      
      if tail.is_none() {
        // Queue is empty
        head = Some(new_node)
        tail = Some(new_node)
      } else {
        // Add to tail
        match tail {
          Some(old_tail) => {
            old_tail.next = Some(new_node)
            tail = Some(new_node)
          }
          None => {}
        }
      }
      
      operations.push(("enqueue", thread_id, value))
      true
    }
    
    let dequeue = |thread_id| {
      match head {
        Some(old_head) => {
          head = old_head.next
          
          if head.is_none() {
            tail = None
          }
          
          operations.push(("dequeue", thread_id, old_head.value))
          Some(old_head.value)
        }
        None => {
          operations.push(("dequeue_empty", thread_id))
          None
        }
      }
    }
    
    let is_empty = || {
      head.is_none()
    }
    
    let size = || {
      let mut count = 0
      let mut current = head
      
      while current.is_some() {
        match current {
          Some(node) => {
            count = count + 1
            current = node.next
          }
          None => break
        }
      }
      
      count
    }
    
    let get_operations = || {
      operations
    }
    
    (enqueue, dequeue, is_empty, size, get_operations)
  }
  
  let (enqueue, dequeue, is_empty, size, get_operations) = lock_free_queue()
  
  // Test queue operations
  assert_true(is_empty())
  assert_eq(size(), 0)
  
  assert_true(enqueue("thread1", "item1"))
  assert_true(enqueue("thread2", "item2"))
  assert_true(enqueue("thread3", "item3"))
  
  assert_false(is_empty())
  assert_eq(size(), 3)
  
  match dequeue("thread4") {
    Some(value) => assert_eq(value, "item1")
    None => assert_true(false)
  }
  
  match dequeue("thread5") {
    Some(value) => assert_eq(value, "item2")
    None => assert_true(false)
  }
  
  assert_eq(size(), 1)
  
  // Simulate concurrent hash map with atomic operations
  let concurrent_hash_map = |initial_capacity| {
    let mut buckets = []
    let mut operations = []
    
    // Initialize buckets
    for i = 0; i < initial_capacity; i = i + 1 {
      buckets.push([])
    }
    
    let hash_key = |key| {
      let mut hash = 0
      for i = 0; i < key.length(); i = i + 1 {
        hash = (hash + key.char_code_at(i)) % initial_capacity
      }
      hash
    }
    
    let put = |thread_id, key, value| {
      let bucket_index = hash_key(key)
      let bucket = buckets[bucket_index]
      
      // Check if key already exists
      let mut found = false
      let mut new_bucket = []
      
      for (k, v) in bucket {
        if k == key {
          new_bucket.push((key, value))
          found = true
        } else {
          new_bucket.push((k, v))
        }
      }
      
      if !found {
        new_bucket.push((key, value))
      }
      
      buckets[bucket_index] = new_bucket
      operations.push(("put", thread_id, key, value))
      found
    }
    
    let get = |thread_id, key| {
      let bucket_index = hash_key(key)
      let bucket = buckets[bucket_index]
      
      operations.push(("get", thread_id, key))
      
      for (k, v) in bucket {
        if k == key {
          return Some(v)
        }
      }
      
      None
    }
    
    let remove = |thread_id, key| {
      let bucket_index = hash_key(key)
      let bucket = buckets[bucket_index]
      
      let mut found = false
      let mut new_bucket = []
      
      for (k, v) in bucket {
        if k == key {
          found = true
        } else {
          new_bucket.push((k, v))
        }
      }
      
      if found {
        buckets[bucket_index] = new_bucket
      }
      
      operations.push(("remove", thread_id, key, found))
      found
    }
    
    let size = || {
      let mut count = 0
      for bucket in buckets {
        count = count + bucket.length()
      }
      count
    }
    
    let get_operations = || {
      operations
    }
    
    (put, get, remove, size, get_operations)
  }
  
  let (put, get, remove, size, get_operations) = concurrent_hash_map(5)
  
  // Test hash map operations
  assert_eq(size(), 0)
  
  assert_false(put("thread1", "key1", "value1"))  // New key
  assert_false(put("thread2", "key2", "value2"))  // New key
  assert_true(put("thread3", "key1", "new_value1"))  // Existing key
  
  assert_eq(size(), 2)
  
  match get("thread4", "key1") {
    Some(value) => assert_eq(value, "new_value1")
    None => assert_true(false)
  }
  
  match get("thread5", "key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match get("thread6", "key3") {
    Some(_) => assert_true(false)
    None => {}  // Expected
  }
  
  assert_true(remove("thread7", "key1"))
  assert_false(remove("thread8", "key3"))  // Non-existent key
  
  assert_eq(size(), 1)
  
  // Verify operations log
  let operations = get_operations()
  assert_eq(operations.length(), 8)
}

// Test 9: Concurrent Algorithms and Coordination Patterns
test "concurrent algorithms and coordination patterns" {
  // Simulate map-reduce pattern
  let map_reduce = |data, map_function, reduce_function, num_workers| {
    let mut map_results = []
    let chunk_size = (data.length() + num_workers - 1) / num_workers
    
    // Map phase
    for i = 0; i < num_workers; i = i + 1 {
      let start = i * chunk_size
      let end = if (i + 1) * chunk_size < data.length() {
        (i + 1) * chunk_size
      } else {
        data.length()
      }
      
      if start < data.length() {
        let chunk = data.slice(start, end)
        let mapped_chunk = chunk.map(map_function)
        map_results.push(mapped_chunk)
      }
    }
    
    // Reduce phase
    let mut flattened = []
    for result in map_results {
      flattened = flattened + result
    }
    
    if flattened.length() > 0 {
      flattened.reduce(reduce_function)
    } else {
      None
    }
  }
  
  let data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Test sum using map-reduce
  let sum_result = map_reduce(
    data,
    |x| x,
    |acc, val| acc + val,
    3
  )
  
  match sum_result {
    Some(sum) => assert_eq(sum, 55)
    None => assert_true(false)
  }
  
  // Test product using map-reduce
  let product_result = map_reduce(
    data,
    |x| x,
    |acc, val| acc * val,
    4
  )
  
  match product_result {
    Some(product) => assert_eq(product, 3628800)  // 10!
    None => assert_true(false)
  }
  
  // Test square and sum using map-reduce
  let square_sum_result = map_reduce(
    data,
    |x| x * x,
    |acc, val| acc + val,
    2
  )
  
  match square_sum_result {
    Some(square_sum) => assert_eq(square_sum, 385)  // 1² + 2² + ... + 10²
    None => assert_true(false)
  }
  
  // Simulate fork-join pattern
  let fork_join = |task, num_forks| {
    let mut subtasks = []
    
    // Fork phase
    for i = 0; i < num_forks; i = i + 1 {
      let subtask = task(i)
      subtasks.push(subtask)
    }
    
    // Join phase
    let mut results = []
    for subtask in subtasks {
      results.push(subtask)
    }
    
    results
  }
  
  // Test fork-join with factorial calculation
  let factorial_task = |n| {
    if n <= 1 {
      1
    } else {
      n * factorial_task(n - 1)
    }
  }
  
  let fork_join_factorial = || {
    let task = |i| {
      factorial_task(i + 1)  // Calculate factorial of (i+1)
    }
    
    fork_join(task, 5)
  }
  
  let factorials = fork_join_factorial()
  assert_eq(factorials, [1, 2, 6, 24, 120])  // 1!, 2!, 3!, 4!, 5!
  
  // Simulate barrier synchronization
  let barrier = |num_threads| {
    let mut waiting_threads = []
    let mut generation = 0
    
    let await_barrier = |thread_id| {
      waiting_threads.push(thread_id)
      
      if waiting_threads.length() >= num_threads {
        generation = generation + 1
        let released_threads = waiting_threads
        waiting_threads = []
        (true, generation, released_threads)
      } else {
        (false, generation, [])
      }
    }
    
    let reset = || {
      waiting_threads = []
      generation = 0
    }
    
    (await_barrier, reset)
  }
  
  let (await_barrier, reset) = barrier(3)
  
  // Test barrier synchronization
  let (released1, gen1, threads1) = await_barrier("thread1")
  assert_false(released1)
  assert_eq(gen1, 0)
  assert_eq(threads1.length(), 0)
  
  let (released2, gen2, threads2) = await_barrier("thread2")
  assert_false(released2)
  assert_eq(gen2, 0)
  assert_eq(threads2.length(), 0)
  
  let (released3, gen3, threads3) = await_barrier("thread3")
  assert_true(released3)
  assert_eq(gen3, 1)
  assert_eq(threads3.length(), 3)
  assert_true(threads3.contains("thread1"))
  assert_true(threads3.contains("thread2"))
  assert_true(threads3.contains("thread3"))
  
  // Test barrier reset
  reset()
  
  let (released4, gen4, threads4) = await_barrier("thread4")
  assert_false(released4)
  assert_eq(gen4, 0)
  
  // Simulate work-stealing queue
  let work_stealing_queue = || {
    let mut local_queue = []
    let mut stolen_tasks = []
    
    let add_task = |thread_id, task| {
      local_queue.push((thread_id, task))
    }
    
    let get_task = |thread_id| {
      let mut i = 0
      while i < local_queue.length() {
        let (owner_thread, task) = local_queue[i]
        if owner_thread == thread_id {
          local_queue.remove(i)
          return Some(task)
        }
        i = i + 1
      }
      None
    }
    
    let steal_task = |thief_thread_id| {
      if local_queue.length() > 0 {
        let (owner_thread, task) = local_queue.shift()
        stolen_tasks.push((thief_thread_id, owner_thread, task))
        Some(task)
      } else {
        None
      }
    }
    
    let get_queue_state = || {
      {
        queue_size: local_queue.length(),
        stolen_tasks: stolen_tasks.length()
      }
    }
    
    (add_task, get_task, steal_task, get_queue_state)
  }
  
  let (add_task, get_task, steal_task, get_queue_state) = work_stealing_queue()
  
  // Add tasks to queue
  add_task("thread1", "task1")
  add_task("thread2", "task2")
  add_task("thread1", "task3")
  add_task("thread3", "task4")
  
  let state = get_queue_state()
  assert_eq(state.queue_size, 4)
  
  // Get own tasks
  match get_task("thread1") {
    Some(task) => assert_eq(task, "task1")
    None => assert_true(false)
  }
  
  match get_task("thread1") {
    Some(task) => assert_eq(task, "task3")
    None => assert_true(false)
  }
  
  match get_task("thread1") {
    Some(_) => assert_true(false)
    None => {}  // Expected - no more tasks for thread1
  }
  
  // Steal tasks
  match steal_task("thread4") {
    Some(task) => assert_eq(task, "task2")
    None => assert_true(false)
  }
  
  match steal_task("thread5") {
    Some(task) => assert_eq(task, "task4")
    None => assert_true(false)
  }
  
  let final_state = get_queue_state()
  assert_eq(final_state.queue_size, 0)
  assert_eq(final_state.stolen_tasks, 2)
}

// Test 10: Concurrent Performance and Scalability
test "concurrent performance and scalability" {
  // Simulate concurrent counter with different contention levels
  let concurrent_counter = |num_threads, increments_per_thread| {
    let mut counter = 0
    let mut operations = []
    
    for thread_id = 0; thread_id < num_threads; thread_id = thread_id + 1 {
      for i = 0; i < increments_per_thread; i = i + 1 {
        counter = counter + 1
        operations.push(("increment", "thread_" + thread_id.to_string(), counter))
      }
    }
    
    counter
  }
  
  // Test with different thread counts
  let counter1 = concurrent_counter(1, 1000)
  let counter2 = concurrent_counter(2, 500)
  let counter4 = concurrent_counter(4, 250)
  let counter8 = concurrent_counter(8, 125)
  
  assert_eq(counter1, 1000)
  assert_eq(counter2, 1000)
  assert_eq(counter4, 1000)
  assert_eq(counter8, 1000)
  
  // Simulate work distribution
  let work_distribution = |total_work, num_workers| {
    let mut work_per_worker = []
    let base_work = total_work / num_workers
    let remainder = total_work % num_workers
    
    for i = 0; i < num_workers; i = i + 1 {
      let work = if i < remainder {
        base_work + 1
      } else {
        base_work
      }
      work_per_worker.push(work)
    }
    
    work_per_worker
  }
  
  let distribution1 = work_distribution(1000, 4)
  assert_eq(distribution1, [250, 250, 250, 250])
  
  let distribution2 = work_distribution(1003, 4)
  assert_eq(distribution2, [251, 251, 251, 250])
  
  let distribution3 = work_distribution(1000, 7)
  assert_eq(distribution3, [143, 143, 143, 143, 143, 142, 142])
  
  // Simulate Amdahl's Law
  let amdahls_law = |total_work, parallel_fraction, num_processors| {
    let serial_fraction = 1.0 - parallel_fraction
    let parallel_speedup = 1.0 / (serial_fraction + parallel_fraction / num_processors.to_float())
    parallel_speedup
  }
  
  // Test with different parallel fractions
  let speedup_90 = amdahls_law(1000, 0.9, 4)
  let speedup_75 = amdahls_law(1000, 0.75, 4)
  let speedup_50 = amdahls_law(1000, 0.5, 4)
  let speedup_25 = amdahls_law(1000, 0.25, 4)
  
  assert_true(speedup_90 > 3.0)  // 90% parallel should get good speedup
  assert_true(speedup_75 > 2.0)  // 75% parallel should get moderate speedup
  assert_true(speedup_50 > 1.5)  // 50% parallel should get some speedup
  assert_true(speedup_25 > 1.0)  // 25% parallel should get minimal speedup
  
  // Simulate scalability analysis
  let scalability_analysis = |base_problem_size, max_processors| {
    let mut results = []
    
    for processors = 1; processors <= max_processors; processors = processors + 1 {
      let problem_size = base_problem_size * processors
      let efficiency = amdahls_law(problem_size, 0.8, processors) / processors.to_float()
      
      results.push({
        processors: processors,
        problem_size: problem_size,
        efficiency: efficiency
      })
    }
    
    results
  }
  
  let scalability_results = scalability_analysis(1000, 8)
  
  // Verify results
  assert_eq(scalability_results.length(), 8)
  assert_eq(scalability_results[0].processors, 1)
  assert_eq(scalability_results[0].problem_size, 1000)
  assert_eq(scalability_results[7].processors, 8)
  assert_eq(scalability_results[7].problem_size, 8000)
  
  // Efficiency should generally decrease as processors increase
  assert_true(scalability_results[0].efficiency > scalability_results[7].efficiency)
  
  // Simulate contention analysis
  let contention_analysis = |num_threads, operations_per_thread, lock_hold_time| {
    let mut total_wait_time = 0
    let mut operations = []
    
    for thread_id = 0; thread_id < num_threads; thread_id = thread_id + 1 {
      for op_id = 0; op_id < operations_per_thread; op_id = op_id + 1 {
        // Simulate lock acquisition with contention
        let wait_time = (num_threads - 1) * lock_hold_time / 2  // Simplified contention model
        total_wait_time = total_wait_time + wait_time
        
        operations.push({
          thread: "thread_" + thread_id.to_string(),
          operation: op_id,
          wait_time: wait_time
        })
      }
    }
    
    let avg_wait_time = total_wait_time.to_float() / (num_threads * operations_per_thread).to_float()
    {
      total_operations: num_threads * operations_per_thread,
      total_wait_time: total_wait_time,
      avg_wait_time: avg_wait_time,
      contention_factor: avg_wait_time / lock_hold_time.to_float()
    }
  }
  
  let analysis1 = contention_analysis(2, 100, 10)
  let analysis2 = contention_analysis(4, 100, 10)
  let analysis4 = contention_analysis(8, 100, 10)
  let analysis8 = contention_analysis(16, 100, 10)
  
  // Contention should increase with more threads
  assert_true(analysis1.contention_factor < analysis2.contention_factor)
  assert_true(analysis2.contention_factor < analysis4.contention_factor)
  assert_true(analysis4.contention_factor < analysis8.contention_factor)
  
  // Average wait time should increase with more threads
  assert_true(analysis1.avg_wait_time < analysis2.avg_wait_time)
  assert_true(analysis2.avg_wait_time < analysis4.avg_wait_time)
  assert_true(analysis4.avg_wait_time < analysis8.avg_wait_time)
  
  // Simulate load balancing
  let load_balancing = |total_work, num_workers| {
    let work_per_worker = work_distribution(total_work, num_workers)
    let max_work = work_per_worker.reduce(|acc, val| if val > acc { val } else { acc }, 0)
    let min_work = work_per_worker.reduce(|acc, val| if val < acc { val } else { acc }, max_work)
    
    let avg_work = total_work / num_workers
    let imbalance = if avg_work > 0 {
      (max_work - min_work).to_float() / avg_work.to_float()
    } else {
      0.0
    }
    
    {
      work_per_worker: work_per_worker,
      max_work: max_work,
      min_work: min_work,
      avg_work: avg_work,
      imbalance: imbalance
    }
  }
  
  let balance1 = load_balancing(1000, 4)
  let balance2 = load_balancing(1003, 4)
  let balance3 = load_balancing(1000, 7)
  
  // Perfect balance when work divides evenly
  assert_eq(balance1.max_work, 250)
  assert_eq(balance1.min_work, 250)
  assert_eq(balance1.imbalance, 0.0)
  
  // Small imbalance when work doesn't divide evenly
  assert_eq(balance2.max_work, 251)
  assert_eq(balance2.min_work, 250)
  assert_true(balance2.imbalance > 0.0 && balance2.imbalance < 0.1)
  
  // Larger imbalance with more workers
  assert_eq(balance3.max_work, 143)
  assert_eq(balance3.min_work, 142)
  assert_true(balance3.imbalance > 0.0 && balance3.imbalance < 0.1)
}