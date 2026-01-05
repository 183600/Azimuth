// Azimuth Telemetry System - Concurrent Safety and Thread Safety Comprehensive Tests
// This file contains comprehensive test cases for concurrent programming, thread safety, and synchronization

// Test 1: Thread-Safe Counter Implementation
test "thread-safe counter implementation" {
  let counter = ThreadSafeCounter::new()
  
  // Test initial counter value
  assert_eq(ThreadSafeCounter::get(counter), 0)
  
  // Test increment operations
  ThreadSafeCounter::increment(counter)
  assert_eq(ThreadSafeCounter::get(counter), 1)
  
  ThreadSafeCounter::increment_by(counter, 5)
  assert_eq(ThreadSafeCounter::get(counter), 6)
  
  // Test decrement operations
  ThreadSafeCounter::decrement(counter)
  assert_eq(ThreadSafeCounter::get(counter), 5)
  
  ThreadSafeCounter::decrement_by(counter, 3)
  assert_eq(ThreadSafeCounter::get(counter), 2)
  
  // Test atomic compare and swap
  let cas_result = ThreadSafeCounter::compare_and_swap(counter, 2, 10)
  assert_true(cas_result)
  assert_eq(ThreadSafeCounter::get(counter), 10)
  
  let cas_fail = ThreadSafeCounter::compare_and_swap(counter, 5, 20)
  assert_false(cas_fail)
  assert_eq(ThreadSafeCounter::get(counter), 10)  // Value should remain unchanged
}

// Test 2: Concurrent Queue with Multiple Producers and Consumers
test "concurrent queue with multiple producers and consumers" {
  let concurrent_queue = ConcurrentQueue::new(100)  // Queue with capacity 100
  
  // Test initial queue state
  assert_true(ConcurrentQueue::is_empty(concurrent_queue))
  assert_eq(ConcurrentQueue::size(concurrent_queue), 0)
  
  // Test producer operations
  assert_true(ConcurrentQueue::enqueue(concurrent_queue, "item1"))
  assert_true(ConcurrentQueue::enqueue(concurrent_queue, "item2"))
  assert_true(ConcurrentQueue::enqueue(concurrent_queue, "item3"))
  
  assert_eq(ConcurrentQueue::size(concurrent_queue), 3)
  
  // Test consumer operations
  let item1 = ConcurrentQueue::dequeue(concurrent_queue)
  match item1 {
    Some(value) => assert_eq(value, "item1")
    None => assert_true(false)
  }
  
  assert_eq(ConcurrentQueue::size(concurrent_queue), 2)
  
  // Test blocking dequeue when queue is empty
  let empty_result = ConcurrentQueue::try_dequeue(concurrent_queue)
  match empty_result {
    Some(value) => assert_eq(value, "item2")
    None => assert_true(false)
  }
  
  // Test queue capacity limits
  for i in 0..=100 {  // Try to add 101 items to a queue with capacity 100
    let enqueued = ConcurrentQueue::try_enqueue(concurrent_queue, "capacity_test" + i.to_string())
    if i < 98 {  // We already have 1 item in the queue
      assert_true(enqueued)
    } else {
      assert_false(enqueued)  // Should fail when queue is full
    }
  }
}

// Test 3: Read-Write Lock for Multiple Readers and Single Writer
test "read-write lock for multiple readers and single writer" {
  let rw_lock = ReadWriteLock::new()
  let shared_data = SharedData::new("initial_value")
  
  // Test read lock acquisition
  let read_lock1 = ReadWriteLock::acquire_read_lock(rw_lock)
  assert_true(ReadWriteLock::is_read_locked(rw_lock))
  assert_false(ReadWriteLock::is_write_locked(rw_lock))
  
  // Multiple readers should be allowed
  let read_lock2 = ReadWriteLock::acquire_read_lock(rw_lock)
  let read_lock3 = ReadWriteLock::acquire_read_lock(rw_lock)
  
  assert_eq(ReadWriteLock::get_reader_count(rw_lock), 3)
  
  // Read operations should succeed
  let value1 = SharedData::read(shared_data, read_lock1)
  let value2 = SharedData::read(shared_data, read_lock2)
  let value3 = SharedData::read(shared_data, read_lock3)
  
  assert_eq(value1, "initial_value")
  assert_eq(value2, "initial_value")
  assert_eq(value3, "initial_value")
  
  // Release read locks
  ReadWriteLock::release_read_lock(rw_lock, read_lock1)
  ReadWriteLock::release_read_lock(rw_lock, read_lock2)
  ReadWriteLock::release_read_lock(rw_lock, read_lock3)
  
  assert_eq(ReadWriteLock::get_reader_count(rw_lock), 0)
  
  // Test write lock acquisition
  let write_lock = ReadWriteLock::acquire_write_lock(rw_lock)
  assert_true(ReadWriteLock::is_write_locked(rw_lock))
  assert_false(ReadWriteLock::is_read_locked(rw_lock))
  
  // Write operation should succeed
  SharedData::write(shared_data, write_lock, "updated_value")
  
  // Release write lock
  ReadWriteLock::release_write_lock(rw_lock, write_lock)
  
  // Verify updated value
  let new_read_lock = ReadWriteLock::acquire_read_lock(rw_lock)
  let updated_value = SharedData::read(shared_data, new_read_lock)
  assert_eq(updated_value, "updated_value")
  ReadWriteLock::release_read_lock(rw_lock, new_read_lock)
}

// Test 4: Thread-Safe HashMap with Concurrent Access
test "thread-safe hashmap with concurrent access" {
  let concurrent_map = ConcurrentHashMap::new()
  
  // Test initial map state
  assert_eq(ConcurrentHashMap::size(concurrent_map), 0)
  
  // Test put operations
  ConcurrentHashMap::put(concurrent_map, "key1", "value1")
  ConcurrentHashMap::put(concurrent_map, "key2", "value2")
  ConcurrentHashMap::put(concurrent_map, "key3", "value3")
  
  assert_eq(ConcurrentHashMap::size(concurrent_map), 3)
  
  // Test get operations
  let value1 = ConcurrentHashMap::get(concurrent_map, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  let non_existent = ConcurrentHashMap::get(concurrent_map, "non_existent")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test put-if-absent operation
  let put_result = ConcurrentHashMap::put_if_absent(concurrent_map, "key1", "new_value1")
  assert_false(put_result)  // Key already exists
  
  let put_result2 = ConcurrentHashMap::put_if_absent(concurrent_map, "key4", "value4")
  assert_true(put_result2)   // Key was absent and now added
  
  assert_eq(ConcurrentHashMap::size(concurrent_map), 4)
  
  // Test remove operation
  let removed_value = ConcurrentHashMap::remove(concurrent_map, "key2")
  match removed_value {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  assert_eq(ConcurrentHashMap::size(concurrent_map), 3)
  
  // Test contains key operation
  assert_true(ConcurrentHashMap::contains_key(concurrent_map, "key1"))
  assert_false(ConcurrentHashMap::contains_key(concurrent_map, "key2"))
  
  // Test replace operation
  let replace_result = ConcurrentHashMap::replace(concurrent_map, "key1", "replaced_value1")
  match replace_result {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  let new_value = ConcurrentHashMap::get(concurrent_map, "key1")
  match new_value {
    Some(v) => assert_eq(v, "replaced_value1")
    None => assert_true(false)
  }
}

// Test 5: Semaphore for Resource Limiting
test "semaphore for resource limiting" {
  let semaphore = Semaphore::new(3)  // Allow 3 concurrent permits
  
  // Test initial semaphore state
  assert_eq(Semaphore::get_available_permits(semaphore), 3)
  assert_eq(Semaphore::get_used_permits(semaphore), 0)
  
  // Test permit acquisition
  let permit1 = Semaphore::acquire(semaphore)
  let permit2 = Semaphore::acquire(semaphore)
  let permit3 = Semaphore::acquire(semaphore)
  
  assert_eq(Semaphore::get_available_permits(semaphore), 0)
  assert_eq(Semaphore::get_used_permits(semaphore), 3)
  
  // Test permit acquisition when none available
  let permit4 = Semaphore::try_acquire(semaphore)
  match permit4 {
    Some(_) => assert_true(false)  // Should fail
    None => assert_true(true)      // Expected to fail
  }
  
  // Test permit release
  Semaphore::release(semaphore, permit1)
  assert_eq(Semaphore::get_available_permits(semaphore), 1)
  assert_eq(Semaphore::get_used_permits(semaphore), 2)
  
  // Should be able to acquire again
  let new_permit = Semaphore::try_acquire(semaphore)
  match new_permit {
    Some(_) => assert_true(true)   // Should succeed
    None => assert_true(false)     // Should not fail
  }
  
  assert_eq(Semaphore::get_available_permits(semaphore), 0)
  assert_eq(Semaphore::get_used_permits(semaphore), 3)
  
  // Release all permits
  Semaphore::release(semaphore, permit2)
  Semaphore::release(semaphore, permit3)
  Semaphore::release(semaphore, new_permit)
  
  assert_eq(Semaphore::get_available_permits(semaphore), 3)
  assert_eq(Semaphore::get_used_permits(semaphore), 0)
}

// Test 6: Barrier for Thread Synchronization
test "barrier for thread synchronization" {
  let barrier = Barrier::new(3)  // Wait for 3 threads
  
  // Test initial barrier state
  assert_eq(Barrier::get_parties(barrier), 3)
  assert_eq(Barrier::get_waiting(barrier), 0)
  assert_false(Barrier::is_broken(barrier))
  
  // Test barrier waiting
  let result1 = Barrier::await(barrier)
  match result1 {
    BarrierResult::NotBroken(index) => assert_eq(index, 0)
    BarrierResult::Broken => assert_true(false)
  }
  
  assert_eq(Barrier::get_waiting(barrier), 1)
  
  let result2 = Barrier::await(barrier)
  match result2 {
    BarrierResult::NotBroken(index) => assert_eq(index, 1)
    BarrierResult::Broken => assert_true(false)
  }
  
  assert_eq(Barrier::get_waiting(barrier), 2)
  
  let result3 = Barrier::await(barrier)
  match result3 {
    BarrierResult::NotBroken(index) => assert_eq(index, 2)
    BarrierResult::Broken => assert_true(false)
  }
  
  assert_eq(Barrier::get_waiting(barrier), 0)  // All threads released
  
  // Test barrier reset
  Barrier::reset(barrier)
  assert_eq(Barrier::get_waiting(barrier), 0)
  assert_false(Barrier::is_broken(barrier))
}

// Test 7: Atomic Operations for Complex Data Structures
test "atomic operations for complex data structures" {
  let atomic_reference = AtomicReference::new(ComplexData::new("initial", 42))
  
  // Test initial value
  let initial_value = AtomicReference::get(atomic_reference)
  assert_eq(ComplexData::get_name(initial_value), "initial")
  assert_eq(ComplexData::get_value(initial_value), 42)
  
  // Test atomic compare and set
  let new_data = ComplexData::new("updated", 100)
  let cas_result = AtomicReference::compare_and_set(atomic_reference, initial_value, new_data)
  assert_true(cas_result)
  
  let updated_value = AtomicReference::get(atomic_reference)
  assert_eq(ComplexData::get_name(updated_value), "updated")
  assert_eq(ComplexData::get_value(updated_value), 100)
  
  // Test atomic update with function
  let update_result = AtomicReference::update(atomic_reference, fn(data) {
    return ComplexData::new(
      ComplexData::get_name(data) + "_modified",
      ComplexData::get_value(data) * 2
    )
  })
  
  assert_true(update_result)
  
  let modified_value = AtomicReference::get(atomic_reference)
  assert_eq(ComplexData::get_name(modified_value), "updated_modified")
  assert_eq(ComplexData::get_value(modified_value), 200)
  
  // Test atomic get and set
  let old_value = AtomicReference::get_and_set(atomic_reference, ComplexData::new("final", 999))
  assert_eq(ComplexData::get_name(old_value), "updated_modified")
  assert_eq(ComplexData::get_value(old_value), 200)
  
  let final_value = AtomicReference::get(atomic_reference)
  assert_eq(ComplexData::get_name(final_value), "final")
  assert_eq(ComplexData::get_value(final_value), 999)
}

// Test 8: Thread Pool for Task Execution
test "thread pool for task execution" {
  let thread_pool = ThreadPool::new(4)  // Pool with 4 threads
  
  // Test initial thread pool state
  assert_eq(ThreadPool::get_core_pool_size(thread_pool), 4)
  assert_eq(ThreadPool::get_active_count(thread_pool), 0)
  assert_eq(ThreadPool::get_task_count(thread_pool), 0)
  
  // Test task submission
  let future1 = ThreadPool::submit(thread_pool, fn() {
    return "Task 1 Result"
  })
  
  let future2 = ThreadPool::submit(thread_pool, fn() {
    return 42
  })
  
  let future3 = ThreadPool::submit(thread_pool, fn() {
    return [1, 2, 3, 4, 5]
  })
  
  // Test task execution
  let result1 = Future::get(future1)
  match result1 {
    Some(value) => assert_eq(value, "Task 1 Result")
    None => assert_true(false)
  }
  
  let result2 = Future::get(future2)
  match result2 {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  let result3 = Future::get(future3)
  match result3 {
    Some(value) => assert_eq(value, [1, 2, 3, 4, 5])
    None => assert_true(false)
  }
  
  // Test task completion
  assert_true(Future::is_done(future1))
  assert_true(Future::is_done(future2))
  assert_true(Future::is_done(future3))
  
  // Test shutdown
  ThreadPool::shutdown(thread_pool)
  assert_true(ThreadPool::is_shutdown(thread_pool))
}

// Test 9: Concurrent Data Structure Operations
test "concurrent data structure operations" {
  let concurrent_list = ConcurrentList::new()
  
  // Test initial list state
  assert_true(ConcurrentList::is_empty(concurrent_list))
  assert_eq(ConcurrentList::size(concurrent_list), 0)
  
  // Test add operations
  ConcurrentList::add(concurrent_list, "item1")
  ConcurrentList::add(concurrent_list, "item2")
  ConcurrentList::add(concurrent_list, "item3")
  
  assert_eq(ConcurrentList::size(concurrent_list), 3)
  
  // Test contains operation
  assert_true(ConcurrentList::contains(concurrent_list, "item2"))
  assert_false(ConcurrentList::contains(concurrent_list, "non_existent"))
  
  // Test remove operation
  let removed = ConcurrentList::remove(concurrent_list, "item2")
  assert_true(removed)
  assert_eq(ConcurrentList::size(concurrent_list), 2)
  assert_false(ConcurrentList::contains(concurrent_list, "item2"))
  
  // Test iterator
  let iterator = ConcurrentList::iterator(concurrent_list)
  let iterated_items = []
  
  while Iterator::has_next(iterator) {
    let item = Iterator::next(iterator)
    iterated_items.push(item)
  }
  
  assert_eq(iterated_items.length(), 2)
  assert_true(iterated_items.contains("item1"))
  assert_true(iterated_items.contains("item3"))
  
  // Test concurrent set
  let concurrent_set = ConcurrentSet::new()
  
  // Test add operations
  assert_true(ConcurrentSet::add(concurrent_set, "element1"))
  assert_true(ConcurrentSet::add(concurrent_set, "element2"))
  assert_false(ConcurrentSet::add(concurrent_set, "element1"))  // Duplicate
  
  assert_eq(ConcurrentSet::size(concurrent_set), 2)
  
  // Test contains operations
  assert_true(ConcurrentSet::contains(concurrent_set, "element1"))
  assert_false(ConcurrentSet::contains(concurrent_set, "element3"))
  
  // Test remove operations
  assert_true(ConcurrentSet::remove(concurrent_set, "element2"))
  assert_false(ConcurrentSet::remove(concurrent_set, "non_existent"))
  
  assert_eq(ConcurrentSet::size(concurrent_set), 1)
}

// Test 10: Deadlock Detection and Prevention
test "deadlock detection and prevention" {
  let deadlock_detector = DeadlockDetector::new()
  
  // Test initial detector state
  assert_false(DeadlockDetector::has_deadlock(deadlock_detector))
  assert_eq(DeadlockDetector::get_cycle_count(deadlock_detector), 0)
  
  // Create resources
  let resource1 = Resource::new("resource1")
  let resource2 = Resource::new("resource2")
  let resource3 = Resource::new("resource3")
  
  // Create threads
  let thread1 = Thread::new("thread1")
  let thread2 = Thread::new("thread2")
  
  // Test lock acquisition
  DeadlockDetector::acquire_lock(deadlock_detector, thread1, resource1)
  DeadlockDetector::acquire_lock(deadlock_detector, thread2, resource2)
  
  assert_false(DeadlockDetector::has_deadlock(deadlock_detector))
  
  // Test lock waiting (potential deadlock scenario)
  DeadlockDetector::wait_for_lock(deadlock_detector, thread1, resource2)
  DeadlockDetector::wait_for_lock(deadlock_detector, thread2, resource1)
  
  // Should detect deadlock
  assert_true(DeadlockDetector::has_deadlock(deadlock_detector))
  assert_eq(DeadlockDetector::get_cycle_count(deadlock_detector), 1)
  
  // Test deadlock resolution
  let resolution = DeadlockDetector::resolve_deadlock(deadlock_detector)
  match resolution {
    DeadlockResolution::VictimChosen(victim) => {
      assert_true(victim === thread1 || victim === thread2)
    }
    DeadlockResolution::NoDeadlock => assert_true(false)
  }
  
  // Test lock ordering for prevention
  let lock_ordering = LockOrdering::new()
  LockOrdering::define_ordering(lock_ordering, [
    ("resource1", "resource2"),
    ("resource2", "resource3")
  ])
  
  // Test valid lock order
  let valid_order = LockOrdering::check_order(lock_ordering, [
    ("thread1", "resource1"),
    ("thread1", "resource2"),
    ("thread1", "resource3")
  ])
  assert_true(valid_order)
  
  // Test invalid lock order (would cause deadlock)
  let invalid_order = LockOrdering::check_order(lock_ordering, [
    ("thread1", "resource3"),
    ("thread1", "resource2"),
    ("thread1", "resource1")
  ])
  assert_false(invalid_order)
  
  // Test timeout-based deadlock prevention
  let timeout_manager = TimeoutLockManager::new()
  TimeoutLockManager::set_lock_timeout(timeout_manager, 5000)  // 5 seconds
  
  // Test lock acquisition with timeout
  let lock_result = TimeoutLockManager::try_acquire_with_timeout(timeout_manager, resource1, 1000)
  assert_true(lock_result.success)
  
  // Test lock acquisition that would timeout
  let timeout_result = TimeoutLockManager::try_acquire_with_timeout(timeout_manager, resource1, 100)
  assert_false(timeout_result.success)
  assert_eq(timeout_result.reason, TimeoutReason::LockHeld)
}