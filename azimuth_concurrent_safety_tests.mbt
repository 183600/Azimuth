// Azimuth Telemetry System - Concurrent Safety Tests
// This file contains comprehensive test cases for concurrent safety functionality

// Test 1: Thread-Safe Counter Operations
test "thread-safe counter operations" {
  let counter = ThreadSafeCounter::new()
  
  // Test basic increment/decrement operations
  assert_eq(ThreadSafeCounter::get(counter), 0)
  
  ThreadSafeCounter::increment(counter)
  assert_eq(ThreadSafeCounter::get(counter), 1)
  
  ThreadSafeCounter::add(counter, 5)
  assert_eq(ThreadSafeCounter::get(counter), 6)
  
  ThreadSafeCounter::decrement(counter)
  assert_eq(ThreadSafeCounter::get(counter), 5)
  
  ThreadSafeCounter::subtract(counter, 2)
  assert_eq(ThreadSafeCounter::get(counter), 3)
  
  // Test atomic compare and swap
  let cas_result = ThreadSafeCounter::compare_and_swap(counter, 3, 10)
  assert_true(cas_result)
  assert_eq(ThreadSafeCounter::get(counter), 10)
  
  let cas_result2 = ThreadSafeCounter::compare_and_swap(counter, 3, 20)
  assert_false(cas_result2)
  assert_eq(ThreadSafeCounter::get(counter), 10)
  
  // Test concurrent access simulation
  let threads = []
  
  for i in 0..=10 {
    let thread_data = {
      counter: counter,
      iterations: 1000
    }
    threads.push(thread_data)
  }
  
  // Simulate concurrent increments
  for thread in threads {
    for i in 0..=thread.iterations {
      ThreadSafeCounter::increment(thread.counter)
    }
  }
  
  // Verify final count
  let expected_count = 10 + (11 * 1001) // Initial 10 + 11 threads * 1001 operations
  assert_eq(ThreadSafeCounter::get(counter), expected_count)
}

// Test 2: Thread-Safe Map Operations
test "thread-safe map operations" {
  let map = ThreadSafeMap::new()
  
  // Test basic operations
  ThreadSafeMap::put(map, "key1", "value1")
  ThreadSafeMap::put(map, "key2", "value2")
  ThreadSafeMap::put(map, "key3", "value3")
  
  match ThreadSafeMap::get(map, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match ThreadSafeMap::get(map, "key4") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test contains key
  assert_true(ThreadSafeMap::contains_key(map, "key2"))
  assert_false(ThreadSafeMap::contains_key(map, "key5"))
  
  // Test remove operation
  let removed_value = ThreadSafeMap::remove(map, "key2")
  match removed_value {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  assert_false(ThreadSafeMap::contains_key(map, "key2"))
  
  // Test put if absent
  let put_result = ThreadSafeMap::put_if_absent(map, "key3", "new_value3")
  match put_result {
    Some(existing_value) => assert_eq(existing_value, "value3")
    None => assert_true(false)
  }
  
  let put_result2 = ThreadSafeMap::put_if_absent(map, "key6", "value6")
  match put_result2 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test size operation
  assert_eq(ThreadSafeMap::size(map), 4) // key1, key3, key6, and maybe others
  
  // Test concurrent access simulation
  let threads = []
  
  for i in 0..=10 {
    let thread_data = {
      map: map,
      thread_id: i,
      operations: 100
    }
    threads.push(thread_data)
  }
  
  // Simulate concurrent operations
  for thread in threads {
    for i in 0..=thread.operations {
      let key = "thread_" + thread.thread_id.to_string() + "_key_" + i.to_string()
      let value = "thread_" + thread.thread_id.to_string() + "_value_" + i.to_string()
      
      ThreadSafeMap::put(thread.map, key, value)
      
      // Try to read some existing keys
      let read_key = "key1"
      match ThreadSafeMap::get(thread.map, read_key) {
        Some(_) => assert_true(true)
        None => assert_true(false)
      }
    }
  }
  
  // Verify map contains all expected keys
  let final_size = ThreadSafeMap::size(map)
  assert_true(final_size >= 4) // At least our original keys
}

// Test 3: Thread-Safe Queue Operations
test "thread-safe queue operations" {
  let queue = ThreadSafeQueue::new()
  
  // Test basic operations
  assert_true(ThreadSafeQueue::is_empty(queue))
  assert_eq(ThreadSafeQueue::size(queue), 0)
  
  ThreadSafeQueue::enqueue(queue, "item1")
  ThreadSafeQueue::enqueue(queue, "item2")
  ThreadSafeQueue::enqueue(queue, "item3")
  
  assert_false(ThreadSafeQueue::is_empty(queue))
  assert_eq(ThreadSafeQueue::size(queue), 3)
  
  // Test dequeue operations
  let item1 = ThreadSafeQueue::dequeue(queue)
  match item1 {
    Some(value) => assert_eq(value, "item1")
    None => assert_true(false)
  }
  
  let item2 = ThreadSafeQueue::dequeue(queue)
  match item2 {
    Some(value) => assert_eq(value, "item2")
    None => assert_true(false)
  }
  
  assert_eq(ThreadSafeQueue::size(queue), 1)
  
  // Test peek operation
  let peeked_item = ThreadSafeQueue::peek(queue)
  match peeked_item {
    Some(value) => assert_eq(value, "item3")
    None => assert_true(false)
  }
  
  // Size should remain the same after peek
  assert_eq(ThreadSafeQueue::size(queue), 1)
  
  // Clear the queue
  let item3 = ThreadSafeQueue::dequeue(queue)
  match item3 {
    Some(value) => assert_eq(value, "item3")
    None => assert_true(false)
  }
  
  assert_true(ThreadSafeQueue::is_empty(queue))
  
  // Test concurrent enqueue/dequeue
  let producers = []
  let consumers = []
  
  // Create producer threads
  for i in 0..=5 {
    let producer_data = {
      queue: queue,
      producer_id: i,
      items_to_produce: 100
    }
    producers.push(producer_data)
  }
  
  // Simulate producers
  for producer in producers {
    for i in 0..=producer.items_to_produce {
      let item = "producer_" + producer.producer_id.to_string() + "_item_" + i.to_string()
      ThreadSafeQueue::enqueue(producer.queue, item)
    }
  }
  
  // Verify all items were enqueued
  assert_eq(ThreadSafeQueue::size(queue), 606) // 6 producers * 101 items
  
  // Simulate consumers
  let total_consumed = { mut count: 0 }
  for i in 0..=605 {
    let item = ThreadSafeQueue::dequeue(queue)
    match item {
      Some(_) => total_consumed.count = total_consumed.count + 1
      None => assert_true(false)
    }
  }
  
  assert_eq(total_consumed.count, 606)
  assert_true(ThreadSafeQueue::is_empty(queue))
}

// Test 4: Thread-Safe Lock Management
test "thread-safe lock management" {
  let lock_manager = LockManager::new()
  
  // Test basic lock operations
  let lock1 = LockManager::create_lock(lock_manager, "resource1")
  let lock2 = LockManager::create_lock(lock_manager, "resource2")
  
  // Test acquiring and releasing locks
  assert_true(LockManager::acquire_lock(lock_manager, lock1, 1000L)) // 1s timeout
  assert_true(LockManager::is_locked(lock_manager, lock1))
  
  LockManager::release_lock(lock_manager, lock1)
  assert_false(LockManager::is_locked(lock_manager, lock1))
  
  // Test lock timeout
  assert_true(LockManager::acquire_lock(lock_manager, lock1, 1000L))
  
  // Try to acquire the same lock with a short timeout
  assert_false(LockManager::acquire_lock(lock_manager, lock1, 100L)) // 100ms timeout
  
  LockManager::release_lock(lock_manager, lock1)
  
  // Test multiple locks
  assert_true(LockManager::acquire_lock(lock_manager, lock1, 1000L))
  assert_true(LockManager::acquire_lock(lock_manager, lock2, 1000L))
  
  assert_true(LockManager::is_locked(lock_manager, lock1))
  assert_true(LockManager::is_locked(lock_manager, lock2))
  
  // Test lock contention simulation
  let threads = []
  
  for i in 0..=10 {
    let thread_data = {
      lock_manager: lock_manager,
      lock: lock1,
      thread_id: i,
      iterations: 10
    }
    threads.push(thread_data)
  }
  
  // Simulate concurrent lock access
  for thread in threads {
    for i in 0..=thread.iterations {
      if LockManager::acquire_lock(thread.lock_manager, thread.lock, 500L) {
        // Simulate some work while holding the lock
        let mut result = 1
        for j in 1..=100 {
          result = result * j
        }
        
        LockManager::release_lock(thread.lock_manager, thread.lock)
      }
    }
  }
  
  // Verify lock is not held
  assert_false(LockManager::is_locked(lock_manager, lock1))
}

// Test 5: Thread-Safe Resource Pool
test "thread-safe resource pool" {
  let resource_pool = ThreadSafeResourcePool::new(|| { "resource" }, 5) // Max 5 resources
  
  // Test resource checkout and checkin
  let resource1 = ResourcePool::checkout(resource_pool)
  match resource1 {
    Some(r) => assert_eq(r, "resource")
    None => assert_true(false)
  }
  
  assert_eq(ResourcePool::available_count(resource_pool), 4)
  assert_eq(ResourcePool::checked_out_count(resource_pool), 1)
  
  // Check in the resource
  ResourcePool::checkin(resource_pool, resource1)
  assert_eq(ResourcePool::available_count(resource_pool), 5)
  assert_eq(ResourcePool::checked_out_count(resource_pool), 0)
  
  // Test pool exhaustion
  let resources = []
  
  for i in 0..=6 {
    let resource = ResourcePool::checkout(resource_pool)
    match resource {
      Some(r) => resources.push(r)
      None => {
        // Should fail on the 6th attempt (pool size is 5)
        assert_eq(i, 5)
      }
    }
  }
  
  assert_eq(resources.length(), 5)
  assert_eq(ResourcePool::available_count(resource_pool), 0)
  assert_eq(ResourcePool::checked_out_count(resource_pool), 5)
  
  // Check in all resources
  for resource in resources {
    ResourcePool::checkin(resource_pool, Some(resource))
  }
  
  assert_eq(ResourcePool::available_count(resource_pool), 5)
  assert_eq(ResourcePool::checked_out_count(resource_pool), 0)
  
  // Test concurrent resource access
  let threads = []
  
  for i in 0..=10 {
    let thread_data = {
      pool: resource_pool,
      thread_id: i,
      operations: 20
    }
    threads.push(thread_data)
  }
  
  // Simulate concurrent resource usage
  for thread in threads {
    for i in 0..=thread.operations {
      let resource = ResourcePool::checkout(thread.pool)
      match resource {
        Some(r) => {
          // Simulate resource usage
          Time::sleep(1L) // 1ms
          ResourcePool::checkin(thread.pool, Some(r))
        }
        None => {
          // Pool exhausted, should wait or handle gracefully
          Time::sleep(1L) // 1ms
        }
      }
    }
  }
  
  // Verify all resources are returned
  assert_eq(ResourcePool::available_count(resource_pool), 5)
  assert_eq(ResourcePool::checked_out_count(resource_pool), 0)
}

// Test 6: Thread-Safe Atomic Operations
test "thread-safe atomic operations" {
  let atomic_int = AtomicInt::new(0)
  let atomic_bool = AtomicBool::new(false)
  let atomic_float = AtomicFloat::new(0.0)
  
  // Test atomic integer operations
  assert_eq(AtomicInt::get(atomic_int), 0)
  
  AtomicInt::set(atomic_int, 10)
  assert_eq(AtomicInt::get(atomic_int), 10)
  
  AtomicInt::add(atomic_int, 5)
  assert_eq(AtomicInt::get(atomic_int), 15)
  
  AtomicInt::subtract(atomic_int, 3)
  assert_eq(AtomicInt::get(atomic_int), 12)
  
  let old_value = AtomicInt::increment(atomic_int)
  assert_eq(old_value, 12)
  assert_eq(AtomicInt::get(atomic_int), 13)
  
  let old_value2 = AtomicInt::decrement(atomic_int)
  assert_eq(old_value2, 13)
  assert_eq(AtomicInt::get(atomic_int), 12)
  
  // Test atomic boolean operations
  assert_false(AtomicBool::get(atomic_bool))
  
  AtomicBool::set(atomic_bool, true)
  assert_true(AtomicBool::get(atomic_bool))
  
  let old_bool = AtomicBool::compare_and_swap(atomic_bool, true, false)
  assert_true(old_bool)
  assert_false(AtomicBool::get(atomic_bool))
  
  let old_bool2 = AtomicBool::compare_and_swap(atomic_bool, true, false)
  assert_false(old_bool2)
  assert_false(AtomicBool::get(atomic_bool))
  
  // Test atomic float operations
  assert_eq(AtomicFloat::get(atomic_float), 0.0)
  
  AtomicFloat::set(atomic_float, 3.14)
  assert_eq(AtomicFloat::get(atomic_float), 3.14)
  
  AtomicFloat::add(atomic_float, 2.0)
  assert_eq(AtomicFloat::get(atomic_float), 5.14)
  
  AtomicFloat::subtract(atomic_float, 1.14)
  assert_eq(AtomicFloat::get(atomic_float), 4.0)
  
  // Test concurrent atomic operations
  let threads = []
  
  for i in 0..=10 {
    let thread_data = {
      atomic_int: atomic_int,
      atomic_float: atomic_float,
      thread_id: i,
      iterations: 1000
    }
    threads.push(thread_data)
  }
  
  // Simulate concurrent atomic operations
  for thread in threads {
    for i in 0..=thread.iterations {
      AtomicInt::add(thread.atomic_int, 1)
      AtomicFloat::add(thread.atomic_float, 0.001)
    }
  }
  
  // Verify final values
  let expected_int = 12 + (11 * 1000) // Initial 12 + 11 threads * 1000 operations
  assert_eq(AtomicInt::get(atomic_int), expected_int)
  
  let expected_float = 4.0 + (11 * 1000 * 0.001) // Initial 4.0 + 11 threads * 1000 operations * 0.001
  assert_eq(AtomicFloat::get(atomic_float), expected_float)
}

// Test 7: Thread-Safe Reference Counting
test "thread-safe reference counting" {
  let ref_counted = ThreadSafeRefCounted::new("test_data")
  
  // Test basic reference counting
  assert_eq(RefCounted::ref_count(ref_counted), 1)
  
  let ref1 = RefCounted::clone(ref_counted)
  assert_eq(RefCounted::ref_count(ref_counted), 2)
  
  let ref2 = RefCounted::clone(ref_counted)
  assert_eq(RefCounted::ref_count(ref_counted), 3)
  
  // Test data access
  match RefCounted::get_data(ref_counted) {
    Some(data) => assert_eq(data, "test_data")
    None => assert_true(false)
  }
  
  match RefCounted::get_data(ref1) {
    Some(data) => assert_eq(data, "test_data")
    None => assert_true(false)
  }
  
  // Test reference dropping
  RefCounted::drop(ref1)
  assert_eq(RefCounted::ref_count(ref_counted), 2)
  
  RefCounted::drop(ref2)
  assert_eq(RefCounted::ref_count(ref_counted), 1)
  
  // Test concurrent reference counting
  let threads = []
  
  for i in 0..=10 {
    let thread_data = {
      ref_counted: ref_counted,
      thread_id: i,
      operations: 100
    }
    threads.push(thread_data)
  }
  
  // Simulate concurrent reference operations
  for thread in threads {
    let refs = []
    
    for i in 0..=thread.operations {
      let ref_clone = RefCounted::clone(thread.ref_counted)
      refs.push(ref_clone)
      
      // Access data
      match RefCounted::get_data(ref_clone) {
        Some(data) => assert_eq(data, "test_data")
        None => assert_true(false)
      }
    }
    
    // Drop all references
    for ref_clone in refs {
      RefCounted::drop(ref_clone)
    }
  }
  
  // Should be back to the original reference
  assert_eq(RefCounted::ref_count(ref_counted), 1)
}

// Test 8: Thread-Safe Condition Variables
test "thread-safe condition variables" {
  let condition = ThreadSafeCondition::new()
  let mutex = ThreadSafeMutex::new()
  let shared_data = ThreadSafeMutex::new(0)
  
  // Test basic condition variable operations
  ThreadSafeMutex::lock(mutex)
  
  // Wait with timeout (should timeout since no one will signal)
  let wait_result = ThreadSafeCondition::wait_timeout(condition, mutex, 100L) // 100ms timeout
  assert_false(wait_result) // Should timeout
  
  ThreadSafeMutex::unlock(mutex)
  
  // Test signal and wait
  let signal_thread_data = {
    condition: condition,
    mutex: mutex,
    signaled: { mut value: false }
  }
  
  // Simulate signaling thread
  ThreadSafeMutex::lock(signal_thread_data.mutex)
  signal_thread_data.signaled.value = true
  ThreadSafeCondition::signal(signal_thread_data.condition)
  ThreadSafeMutex::unlock(signal_thread_data.mutex)
  
  // Simulate waiting thread
  ThreadSafeMutex::lock(mutex)
  let wait_result = ThreadSafeCondition::wait_timeout(condition, mutex, 1000L) // 1s timeout
  assert_true(wait_result) // Should be signaled
  ThreadSafeMutex::unlock(mutex)
  
  // Test producer-consumer pattern
  let buffer = ThreadSafeQueue::new()
  let buffer_not_empty = ThreadSafeCondition::new()
  let buffer_not_full = ThreadSafeCondition::new()
  let buffer_mutex = ThreadSafeMutex::new()
  
  // Producer
  ThreadSafeMutex::lock(buffer_mutex)
  ThreadSafeQueue::enqueue(buffer, "item1")
  ThreadSafeCondition::signal(buffer_not_empty)
  ThreadSafeMutex::unlock(buffer_mutex)
  
  // Consumer
  ThreadSafeMutex::lock(buffer_mutex)
  
  if ThreadSafeQueue::is_empty(buffer) {
    ThreadSafeCondition::wait(buffer_not_empty, buffer_mutex)
  }
  
  let item = ThreadSafeQueue::dequeue(buffer)
  match item {
    Some(value) => assert_eq(value, "item1")
    None => assert_true(false)
  }
  
  ThreadSafeCondition::signal(buffer_not_full)
  ThreadSafeMutex::unlock(buffer_mutex)
}

// Test 9: Thread-Safe Read-Write Locks
test "thread-safe read-write locks" {
  let rw_lock = ThreadSafeRWLock::new()
  let shared_data = ThreadSafeRWLock::new(0)
  
  // Test basic read lock operations
  assert_true(RWLock::read_lock(rw_lock, 1000L)) // 1s timeout
  assert_eq(RWLock::reader_count(rw_lock), 1)
  assert_eq(RWLock::writer_count(rw_lock), 0)
  
  RWLock::read_unlock(rw_lock)
  assert_eq(RWLock::reader_count(rw_lock), 0)
  
  // Test basic write lock operations
  assert_true(RWLock::write_lock(rw_lock, 1000L)) // 1s timeout
  assert_eq(RWLock::reader_count(rw_lock), 0)
  assert_eq(RWLock::writer_count(rw_lock), 1)
  
  RWLock::write_unlock(rw_lock)
  assert_eq(RWLock::writer_count(rw_lock), 0)
  
  // Test multiple readers
  assert_true(RWLock::read_lock(rw_lock, 1000L))
  assert_true(RWLock::read_lock(rw_lock, 1000L))
  assert_true(RWLock::read_lock(rw_lock, 1000L))
  
  assert_eq(RWLock::reader_count(rw_lock), 3)
  assert_eq(RWLock::writer_count(rw_lock), 0)
  
  // Writer should not be able to acquire lock while readers exist
  assert_false(RWLock::write_lock(rw_lock, 100L)) // 100ms timeout
  
  RWLock::read_unlock(rw_lock)
  assert_eq(RWLock::reader_count(rw_lock), 2)
  
  RWLock::read_unlock(rw_lock)
  assert_eq(RWLock::reader_count(rw_lock), 1)
  
  RWLock::read_unlock(rw_lock)
  assert_eq(RWLock::reader_count(rw_lock), 0)
  
  // Now writer should be able to acquire lock
  assert_true(RWLock::write_lock(rw_lock, 1000L))
  assert_eq(RWLock::writer_count(rw_lock), 1)
  
  // Readers should not be able to acquire lock while writer exists
  assert_false(RWLock::read_lock(rw_lock, 100L)) // 100ms timeout
  
  RWLock::write_unlock(rw_lock)
  assert_eq(RWLock::writer_count(rw_lock), 0)
  
  // Test concurrent readers and writers
  let reader_threads = []
  let writer_threads = []
  
  // Create reader threads
  for i in 0..=5 {
    let reader_data = {
      rw_lock: rw_lock,
      shared_data: shared_data,
      thread_id: i,
      operations: 100
    }
    reader_threads.push(reader_data)
  }
  
  // Create writer threads
  for i in 0..=2 {
    let writer_data = {
      rw_lock: rw_lock,
      shared_data: shared_data,
      thread_id: i,
      operations: 50
    }
    writer_threads.push(writer_data)
  }
  
  // Simulate concurrent readers
  for reader in reader_threads {
    for i in 0..=reader.operations {
      if RWLock::read_lock(reader.rw_lock, 500L) {
        // Read data
        match RWLock::read_data(reader.shared_data) {
          Some(value) => assert_true(value >= 0)
          None => assert_true(false)
        }
        
        RWLock::read_unlock(reader.rw_lock)
      }
    }
  }
  
  // Simulate concurrent writers
  for writer in writer_threads {
    for i in 0..=writer.operations {
      if RWLock::write_lock(writer.rw_lock, 500L) {
        // Write data
        let current_value = match RWLock::read_data(writer.shared_data) {
          Some(value) => value
          None => 0
        }
        
        RWLock::write_data(writer.shared_data, current_value + 1)
        RWLock::write_unlock(writer.rw_lock)
      }
    }
  }
  
  // Verify final state
  match RWLock::read_data(shared_data) {
    Some(value) => assert_eq(value, 150) // 3 writers * 50 operations
    None => assert_true(false)
  }
}

// Test 10: Thread-Safe Deadlock Detection
test "thread-safe deadlock detection" {
  let deadlock_detector = DeadlockDetector::new()
  
  // Create multiple locks
  let lock_a = DeadlockDetector::create_lock(deadlock_detector, "lock_a")
  let lock_b = DeadlockDetector::create_lock(deadlock_detector, "lock_b")
  let lock_c = DeadlockDetector::create_lock(deadlock_detector, "lock_c")
  
  // Test normal locking sequence (no deadlock)
  assert_true(DeadlockDetector::acquire_lock(deadlock_detector, lock_a, "thread1"))
  assert_true(DeadlockDetector::acquire_lock(deadlock_detector, lock_b, "thread1"))
  
  DeadlockDetector::release_lock(deadlock_detector, lock_b)
  DeadlockDetector::release_lock(deadlock_detector, lock_a)
  
  // Test potential deadlock detection
  assert_true(DeadlockDetector::acquire_lock(deadlock_detector, lock_a, "thread1"))
  assert_true(DeadlockDetector::acquire_lock(deadlock_detector, lock_b, "thread2"))
  
  // This should detect potential deadlock
  assert_false(DeadlockDetector::acquire_lock(deadlock_detector, lock_a, "thread2"))
  
  // Clean up
  DeadlockDetector::release_lock(deadlock_detector, lock_b)
  DeadlockDetector::release_lock(deadlock_detector, lock_a)
  
  // Test deadlock detection with circular waiting
  assert_true(DeadlockDetector::acquire_lock(deadlock_detector, lock_a, "thread1"))
  assert_true(DeadlockDetector::acquire_lock(deadlock_detector, lock_b, "thread2"))
  assert_true(DeadlockDetector::acquire_lock(deadlock_detector, lock_c, "thread3"))
  
  // Try to create a circular wait
  assert_false(DeadlockDetector::acquire_lock(deadlock_detector, lock_b, "thread1"))
  assert_false(DeadlockDetector::acquire_lock(deadlock_detector, lock_c, "thread2"))
  assert_false(DeadlockDetector::acquire_lock(deadlock_detector, lock_a, "thread3"))
  
  // Clean up
  DeadlockDetector::release_lock(deadlock_detector, lock_c)
  DeadlockDetector::release_lock(deadlock_detector, lock_b)
  DeadlockDetector::release_lock(deadlock_detector, lock_a)
  
  // Test deadlock detection statistics
  let stats = DeadlockDetector::get_statistics(deadlock_detector)
  match stats {
    DeadlockStats(total_acquisitions, potential_deadlocks_detected, actual_deadlocks_detected) => {
      assert_true(total_acquisitions > 0)
      assert_true(potential_deadlocks_detected > 0)
      assert_eq(actual_deadlocks_detected, 0) // No actual deadlocks should occur
    }
    _ => assert_true(false)
  }
  
  // Test deadlock detection with timeout
  let timeout_detector = DeadlockDetector::with_timeout(1000L) // 1s timeout
  
  assert_true(DeadlockDetector::acquire_lock(timeout_detector, lock_a, "thread1"))
  
  // This should timeout rather than deadlock
  assert_false(DeadlockDetector::acquire_lock(timeout_detector, lock_a, "thread2"))
  
  DeadlockDetector::release_lock(timeout_detector, lock_a)
}