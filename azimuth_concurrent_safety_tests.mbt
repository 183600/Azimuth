// Azimuth Telemetry System - Concurrent Safety Tests
// This file contains comprehensive test cases for concurrent safety functionality

// Test 1: Thread-Safe Counter
test "thread-safe counter" {
  let counter = ConcurrentCounter::new(0)
  let num_threads = 10
  let increments_per_thread = 1000
  
  // Create threads to increment counter concurrently
  let threads = []
  for i in 0..=num_threads - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=increments_per_thread - 1 {
        ConcurrentCounter::increment(counter)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify final count
  let final_count = ConcurrentCounter::get_value(counter)
  assert_eq(final_count, num_threads * increments_per_thread)
  
  // Test atomic compare and swap
  let old_value = ConcurrentCounter::get_value(counter)
  let new_value = old_value + 100
  let cas_result = ConcurrentCounter::compare_and_swap(counter, old_value, new_value)
  assert_true(cas_result)
  assert_eq(ConcurrentCounter::get_value(counter), new_value)
  
  // Test failed CAS
  let cas_result = ConcurrentCounter::compare_and_swap(counter, old_value, new_value + 100)
  assert_false(cas_result)
  assert_eq(ConcurrentCounter::get_value(counter), new_value) // Should be unchanged
}

// Test 2: Thread-Safe Map
test "thread-safe map" {
  let map = ConcurrentMap::new()
  let num_threads = 10
  let entries_per_thread = 100
  
  // Create threads to insert entries concurrently
  let threads = []
  for i in 0..=num_threads - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=entries_per_thread - 1 {
        let key = "key_" + i.to_string() + "_" + j.to_string()
        let value = "value_" + i.to_string() + "_" + j.to_string()
        ConcurrentMap::insert(map, key, value)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify all entries were inserted
  assert_eq(ConcurrentMap::size(map), num_threads * entries_per_thread)
  
  // Test concurrent reads
  let read_threads = []
  for i in 0..=num_threads - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=entries_per_thread - 1 {
        let key = "key_" + i.to_string() + "_" + j.to_string()
        let expected_value = "value_" + i.to_string() + "_" + j.to_string()
        let actual_value = ConcurrentMap::get(map, key)
        match actual_value {
          Some(value) => assert_eq(value, expected_value)
          None => assert_true(false)
        }
      }
    })
    read_threads.push(thread)
  }
  
  // Wait for all read threads to complete
  for thread in read_threads {
    Thread::join(thread)
  }
  
  // Test concurrent updates
  let update_threads = []
  for i in 0..=num_threads - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=entries_per_thread - 1 {
        let key = "key_" + i.to_string() + "_" + j.to_string()
        let new_value = "updated_value_" + i.to_string() + "_" + j.to_string()
        ConcurrentMap::insert(map, key, new_value)
      }
    })
    update_threads.push(thread)
  }
  
  // Wait for all update threads to complete
  for thread in update_threads {
    Thread::join(thread)
  }
  
  // Verify all entries were updated
  for i in 0..=num_threads - 1 {
    for j in 0..=entries_per_thread - 1 {
      let key = "key_" + i.to_string() + "_" + j.to_string()
      let expected_value = "updated_value_" + i.to_string() + "_" + j.to_string()
      let actual_value = ConcurrentMap::get(map, key)
      match actual_value {
        Some(value) => assert_eq(value, expected_value)
        None => assert_true(false)
      }
    }
  }
}

// Test 3: Thread-Safe Queue
test "thread-safe queue" {
  let queue = ConcurrentQueue::new()
  let num_producers = 5
  let num_consumers = 3
  let items_per_producer = 100
  
  // Create producer threads
  let producer_threads = []
  for i in 0..=num_producers - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=items_per_producer - 1 {
        let item = "item_" + i.to_string() + "_" + j.to_string()
        ConcurrentQueue::enqueue(queue, item)
      }
    })
    producer_threads.push(thread)
  }
  
  // Create consumer threads
  let consumed_items = ConcurrentMap::new()
  let consumer_threads = []
  for i in 0..=num_consumers - 1 {
    let thread = Thread::spawn(fn() {
      let mut local_count = 0
      while local_count < (num_producers * items_per_producer) / num_consumers {
        match ConcurrentQueue::dequeue(queue) {
          Some(item) => {
            ConcurrentMap::insert(consumed_items, item, true)
            local_count = local_count + 1
          }
          None => Thread::sleep(1) // Brief pause if queue is empty
        }
      }
    })
    consumer_threads.push(thread)
  }
  
  // Wait for all producer threads to complete
  for thread in producer_threads {
    Thread::join(thread)
  }
  
  // Wait for all consumer threads to complete
  for thread in consumer_threads {
    Thread::join(thread)
  }
  
  // Verify all items were consumed
  assert_eq(ConcurrentMap::size(consumed_items), num_producers * items_per_producer)
  assert_eq(ConcurrentQueue::size(queue), 0)
}

// Test 4: Read-Write Lock
test "read-write lock" {
  let rw_lock = ReadWriteLock::new()
  let shared_data = ConcurrentCounter::new(0)
  let num_readers = 10
  let num_writers = 3
  let reads_per_reader = 100
  let writes_per_writer = 10
  
  // Create reader threads
  let reader_threads = []
  for i in 0..=num_readers - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=reads_per_reader - 1 {
        ReadWriteLock::read_lock(rw_lock)
        let value = ConcurrentCounter::get_value(shared_data)
        // Simulate read operation
        Thread::sleep(1)
        ReadWriteLock::read_unlock(rw_lock)
        
        // Value should be consistent during read
        assert_true(value >= 0)
      }
    })
    reader_threads.push(thread)
  }
  
  // Create writer threads
  let writer_threads = []
  for i in 0..=num_writers - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=writes_per_writer - 1 {
        ReadWriteLock::write_lock(rw_lock)
        let old_value = ConcurrentCounter::get_value(shared_data)
        ConcurrentCounter::increment(shared_data)
        let new_value = ConcurrentCounter::get_value(shared_data)
        // Simulate write operation
        Thread::sleep(5)
        ReadWriteLock::write_unlock(rw_lock)
        
        // Value should have increased by exactly 1
        assert_eq(new_value, old_value + 1)
      }
    })
    writer_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in reader_threads {
    Thread::join(thread)
  }
  for thread in writer_threads {
    Thread::join(thread)
  }
  
  // Verify final value
  let final_value = ConcurrentCounter::get_value(shared_data)
  assert_eq(final_value, num_writers * writes_per_writer)
}

// Test 5: Mutex and Critical Sections
test "mutex and critical sections" {
  let mutex = Mutex::new()
  let shared_list = ConcurrentList::new()
  let num_threads = 10
  let items_per_thread = 100
  
  // Create threads to add items to the list
  let threads = []
  for i in 0..=num_threads - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=items_per_thread - 1 {
        Mutex::lock(mutex)
        // Critical section - add item to list
        ConcurrentList::push_back(shared_list, "item_" + i.to_string() + "_" + j.to_string())
        Mutex::unlock(mutex)
        
        // Non-critical section
        Thread::sleep(1)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify all items were added
  assert_eq(ConcurrentList::size(shared_list), num_threads * items_per_thread)
  
  // Test with RAII-style lock
  let raii_list = ConcurrentList::new()
  let raii_threads = []
  for i in 0..=num_threads - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=items_per_thread - 1 {
        let _lock = MutexGuard::new(mutex)
        // Critical section - RAII lock automatically released
        ConcurrentList::push_back(raii_list, "raii_item_" + i.to_string() + "_" + j.to_string())
      }
    })
    raii_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in raii_threads {
    Thread::join(thread)
  }
  
  // Verify all items were added
  assert_eq(ConcurrentList::size(raii_list), num_threads * items_per_thread)
}

// Test 6: Atomic Operations
test "atomic operations" {
  // Test atomic integer
  let atomic_int = AtomicInteger::new(0)
  let num_threads = 10
  let increments_per_thread = 1000
  
  let threads = []
  for i in 0..=num_threads - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=increments_per_thread - 1 {
        AtomicInteger::add(atomic_int, 1)
      }
    })
    threads.push(thread)
  }
  
  for thread in threads {
    Thread::join(thread)
  }
  
  assert_eq(AtomicInteger::get(atomic_int), num_threads * increments_per_thread)
  
  // Test atomic boolean
  let atomic_bool = AtomicBoolean::new(false)
  
  let threads = []
  for i in 0..=num_threads - 1 {
    let thread = Thread::spawn(fn() {
      // Try to set to true, but only one should succeed
      let result = AtomicBoolean::compare_and_set(atomic_bool, false, true)
      if result {
        // This thread successfully set the value
        Thread::sleep(10) // Hold the "lock" for a bit
        AtomicBoolean::set(atomic_bool, false) // Release the "lock"
      }
    })
    threads.push(thread)
  }
  
  for thread in threads {
    Thread::join(thread)
  }
  
  // Final value should be false
  assert_false(AtomicBoolean::get(atomic_bool))
  
  // Test atomic reference
  let atomic_ref = AtomicReference::new("initial_value")
  
  let threads = []
  for i in 0..=num_threads - 1 {
    let thread = Thread::spawn(fn() {
      let new_value = "thread_" + i.to_string() + "_value"
      AtomicReference::set(atomic_ref, new_value)
    })
    threads.push(thread)
  }
  
  for thread in threads {
    Thread::join(thread)
  }
  
  // Value should be one of the thread values
  let final_value = AtomicReference::get(atomic_ref)
  assert_true(final_value.contains("thread_"))
  assert_true(final_value.contains("_value"))
}

// Test 7: Concurrent Data Structure Consistency
test "concurrent data structure consistency" {
  let concurrent_set = ConcurrentSet::new()
  let num_threads = 10
  let items_per_thread = 100
  
  // Test concurrent insertions
  let threads = []
  for i in 0..=num_threads - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=items_per_thread - 1 {
        let item = "item_" + i.to_string() + "_" + j.to_string()
        ConcurrentSet::add(concurrent_set, item)
      }
    })
    threads.push(thread)
  }
  
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify all items were added
  assert_eq(ConcurrentSet::size(concurrent_set), num_threads * items_per_thread)
  
  // Test concurrent removals
  let remove_threads = []
  for i in 0..=num_threads - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=items_per_thread - 1 {
        if j % 2 == 0 { // Remove even items
          let item = "item_" + i.to_string() + "_" + j.to_string()
          ConcurrentSet::remove(concurrent_set, item)
        }
      }
    })
    remove_threads.push(thread)
  }
  
  for thread in remove_threads {
    Thread::join(thread)
  }
  
  // Verify half of the items were removed
  assert_eq(ConcurrentSet::size(concurrent_set), num_threads * (items_per_thread / 2))
  
  // Test concurrent contains operations
  let contains_threads = []
  let found_items = ConcurrentMap::new()
  for i in 0..=num_threads - 1 {
    let thread = Thread::spawn(fn() {
      for j in 0..=items_per_thread - 1 {
        if j % 2 == 1 { // Check odd items (should still be in set)
          let item = "item_" + i.to_string() + "_" + j.to_string()
          if ConcurrentSet::contains(concurrent_set, item) {
            ConcurrentMap::insert(found_items, item, true)
          }
        }
      }
    })
    contains_threads.push(thread)
  }
  
  for thread in contains_threads {
    Thread::join(thread)
  }
  
  // Verify all odd items were found
  assert_eq(ConcurrentMap::size(found_items), num_threads * (items_per_thread / 2))
}

// Test 8: Deadlock Detection and Prevention
test "deadlock detection and prevention" {
  let lock1 = Mutex::new()
  let lock2 = Mutex::new()
  let deadlock_detector = DeadlockDetector::new()
  
  // Register locks with detector
  DeadlockDetector::register_lock(deadlock_detector, lock1, "lock1")
  DeadlockDetector::register_lock(deadlock_detector, lock2, "lock2")
  
  // Test safe lock ordering (no deadlock)
  let safe_threads = []
  for i in 0..=5 {
    let thread = Thread::spawn(fn() {
      // Always acquire locks in the same order
      DeadlockDetector::acquire_lock(deadlock_detector, "lock1")
      Mutex::lock(lock1)
      
      Thread::sleep(1) // Simulate work
      
      DeadlockDetector::acquire_lock(deadlock_detector, "lock2")
      Mutex::lock(lock2)
      
      Thread::sleep(1) // Simulate work
      
      Mutex::unlock(lock2)
      DeadlockDetector::release_lock(deadlock_detector, "lock2")
      
      Mutex::unlock(lock1)
      DeadlockDetector::release_lock(deadlock_detector, "lock1")
    })
    safe_threads.push(thread)
  }
  
  for thread in safe_threads {
    Thread::join(thread)
  }
  
  // Verify no deadlocks occurred
  assert_false(DeadlockDetector::has_deadlock(deadlock_detector))
  
  // Test potential deadlock detection
  let unsafe_threads = []
  for i in 0..=2 {
    let thread = Thread::spawn(fn() {
      if i % 2 == 0 {
        // Thread 0: lock1 -> lock2
        DeadlockDetector::acquire_lock(deadlock_detector, "lock1")
        Mutex::lock(lock1)
        Thread::sleep(10)
        
        DeadlockDetector::acquire_lock(deadlock_detector, "lock2")
        Mutex::lock(lock2)
        
        Mutex::unlock(lock2)
        DeadlockDetector::release_lock(deadlock_detector, "lock2")
        
        Mutex::unlock(lock1)
        DeadlockDetector::release_lock(deadlock_detector, "lock1")
      } else {
        // Thread 1: lock2 -> lock1 (potential deadlock)
        DeadlockDetector::acquire_lock(deadlock_detector, "lock2")
        Mutex::lock(lock2)
        Thread::sleep(10)
        
        // This would cause a deadlock, but the detector should prevent it
        let can_acquire = DeadlockDetector::try_acquire_lock(deadlock_detector, "lock1")
        if can_acquire {
          Mutex::lock(lock1)
          
          Mutex::unlock(lock1)
          DeadlockDetector::release_lock(deadlock_detector, "lock1")
        }
        
        Mutex::unlock(lock2)
        DeadlockDetector::release_lock(deadlock_detector, "lock2")
      }
    })
    unsafe_threads.push(thread)
  }
  
  for thread in unsafe_threads {
    Thread::join(thread)
  }
  
  // The detector should have detected and prevented the deadlock
  assert_true(DeadlockDetector::prevented_deadlock(deadlock_detector))
}