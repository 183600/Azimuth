// Azimuth Premium Concurrent Safety Tests
// This file contains high-quality test cases for concurrent operations and thread safety

// Test 1: Thread-Safe Counter Operations
test "thread-safe counter operations" {
  let counter = AtomicCounter::new(0)
  
  assert_eq(AtomicCounter::get(counter), 0)
  
  // Test increment operations
  AtomicCounter::increment(counter)
  assert_eq(AtomicCounter::get(counter), 1)
  
  AtomicCounter::add(counter, 5)
  assert_eq(AtomicCounter::get(counter), 6)
  
  // Test decrement operations
  AtomicCounter::decrement(counter)
  assert_eq(AtomicCounter::get(counter), 5)
  
  AtomicCounter::subtract(counter, 2)
  assert_eq(AtomicCounter::get(counter), 3)
  
  // Test compare and swap
  let success = AtomicCounter::compare_and_swap(counter, 3, 10)
  assert_true(success)
  assert_eq(AtomicCounter::get(counter), 10)
  
  // Failed compare and swap
  let failed = AtomicCounter::compare_and_swap(counter, 3, 20)
  assert_false(failed)
  assert_eq(AtomicCounter::get(counter), 10)
  
  // Test fetch and add
  let old_value = AtomicCounter::fetch_and_add(counter, 5)
  assert_eq(old_value, 10)
  assert_eq(AtomicCounter::get(counter), 15)
}

// Test 2: Thread-Safe Queue Operations
test "thread-safe queue operations" {
  let queue = ConcurrentQueue::new()
  
  // Test empty queue
  assert_true(ConcurrentQueue::is_empty(queue))
  assert_eq(ConcurrentQueue::size(queue), 0)
  
  // Test enqueue operations
  assert_true(ConcurrentQueue::enqueue(queue, "item1"))
  assert_true(ConcurrentQueue::enqueue(queue, "item2"))
  assert_true(ConcurrentQueue::enqueue(queue, "item3"))
  
  assert_false(ConcurrentQueue::is_empty(queue))
  assert_eq(ConcurrentQueue::size(queue), 3)
  
  // Test dequeue operations
  match ConcurrentQueue::dequeue(queue) {
    Some(item) => assert_eq(item, "item1")
    None => assert_true(false)
  }
  
  assert_eq(ConcurrentQueue::size(queue), 2)
  
  // Test blocking dequeue with timeout
  match ConcurrentQueue::dequeue_with_timeout(queue, 100) {
    Some(item) => assert_eq(item, "item2")
    None => assert_true(false)
  }
  
  // Test empty queue timeout
  ConcurrentQueue::dequeue(queue)  // Remove last item
  match ConcurrentQueue::dequeue_with_timeout(queue, 50) {
    Some(_) => assert_true(false)
    None => assert_true(true)  // Timeout expected
  }
}

// Test 3: Thread-Safe Hash Map Operations
test "thread-safe hash map operations" {
  let map = ConcurrentHashMap::new()
  
  // Test empty map
  assert_eq(ConcurrentHashMap::size(map), 0)
  assert_true(ConcurrentHashMap::is_empty(map))
  
  // Test put operations
  match ConcurrentHashMap::put(map, "key1", "value1") {
    None => assert_true(true)  // Expected for new key
    Some(_) => assert_true(false)
  }
  
  match ConcurrentHashMap::put(map, "key2", "value2") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  assert_eq(ConcurrentHashMap::size(map), 2)
  
  // Test get operations
  match ConcurrentHashMap::get(map, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // Test put if absent
  match ConcurrentHashMap::put_if_absent(map, "key1", "new_value1") {
    Some(old_value) => assert_eq(old_value, "value1")
    None => assert_true(false)
  }
  
  match ConcurrentHashMap::put_if_absent(map, "key3", "value3") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test remove operations
  match ConcurrentHashMap::remove(map, "key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  assert_false(ConcurrentHashMap::contains_key(map, "key2"))
  assert_eq(ConcurrentHashMap::size(map), 2)
  
  // Test compute if absent
  let result = ConcurrentHashMap::compute_if_absent(map, "key4", || "computed_value")
  assert_eq(result, "computed_value")
  assert_eq(ConcurrentHashMap::size(map), 3)
}

// Test 4: Read-Write Lock Operations
test "read-write lock operations" {
  let lock = ReadWriteLock::new()
  let shared_data = Ref::new(0)
  
  // Test read lock
  {
    let _read_guard = ReadWriteLock::read_lock(lock)
    assert_eq(*Ref::borrow(shared_data), 0)
    // Multiple read locks should be allowed
    let _read_guard2 = ReadWriteLock::read_lock(lock)
    assert_eq(*Ref::borrow(shared_data), 0)
  }
  
  // Test write lock
  {
    let mut write_guard = ReadWriteLock::write_lock(lock)
    *Ref::borrow_mut(write_guard) = 42
    assert_eq(*Ref::borrow(write_guard), 42)
  }
  
  // Test try read lock
  match ReadWriteLock::try_read_lock(lock) {
    Some(_guard) => assert_eq(*Ref::borrow(shared_data), 42)
    None => assert_true(false)
  }
  
  // Test try write lock
  match ReadWriteLock::try_write_lock(lock) {
    Some(mut _guard) => {
      *Ref::borrow_mut(_guard) = 100
      assert_eq(*Ref::borrow(_guard), 100)
    }
    None => assert_true(false)
  }
}

// Test 5: Thread Pool Operations
test "thread pool operations" {
  let pool = ThreadPool::new(4)
  
  // Test submit task
  let future = ThreadPool::submit(pool, || {
    let mut sum = 0
    for i in 1..=10 {
      sum = sum + i
    }
    sum
  })
  
  // Test get result
  match ThreadPool::get_result(future) {
    Some(result) => assert_eq(result, 55)  // Sum of 1 to 10
    None => assert_true(false)
  }
  
  // Test submit multiple tasks
  let futures = []
  for i in 0..5 {
    let future = ThreadPool::submit(pool, || {
      Thread::sleep(10)  // Simulate work
      i * 2
    })
    futures.push(future)
  }
  
  // Collect results
  let results = []
  for future in futures {
    match ThreadPool::get_result(future) {
      Some(result) => results.push(result)
      None => assert_true(false)
    }
  }
  
  assert_eq(results.length(), 5)
  assert_eq(results[0], 0)
  assert_eq(results[1], 2)
  assert_eq(results[2], 4)
  assert_eq(results[3], 6)
  assert_eq(results[4], 8)
  
  // Test shutdown
  ThreadPool::shutdown(pool)
}

// Test 6: Barrier Synchronization
test "barrier synchronization" {
  let barrier = Barrier::new(3)
  let counter = AtomicCounter::new(0)
  
  // Test barrier wait
  let futures = []
  for i in 0..3 {
    let future = ThreadPool::submit(ThreadPool::new(1), || {
      AtomicCounter::increment(counter)
      Barrier::wait(barrier)
      AtomicCounter::increment(counter)
      i
    })
    futures.push(future)
  }
  
  // Collect results
  let results = []
  for future in futures {
    match ThreadPool::get_result(future) {
      Some(result) => results.push(result)
      None => assert_true(false)
    }
  }
  
  // All threads should have passed the barrier
  assert_eq(AtomicCounter::get(counter), 6)  // 3 threads * 2 increments each
  assert_eq(results.length(), 3)
}

// Test 7: Semaphore Operations
test "semaphore operations" {
  let semaphore = Semaphore::new(2)
  let counter = AtomicCounter::new(0)
  
  // Test acquire and release
  assert_true(Semaphore::try_acquire(semaphore))
  assert_true(Semaphore::try_acquire(semaphore))
  assert_false(Semaphore::try_acquire(semaphore))  // Should fail
  
  Semaphore::release(semaphore)
  assert_true(Semaphore::try_acquire(semaphore))  // Should succeed now
  
  // Test acquire with timeout
  Semaphore::release(semaphore)  // Release one more permit
  match Semaphore::acquire_with_timeout(semaphore, 50) {
    true => assert_true(true)
    false => assert_true(false)
  }
  
  assert_false(Semaphore::try_acquire(semaphore))  // No permits left
  
  // Test concurrent access
  let futures = []
  for i in 0..5 {
    let future = ThreadPool::submit(ThreadPool::new(1), || {
      Semaphore::acquire(semaphore)
      AtomicCounter::increment(counter)
      Thread::sleep(10)  // Simulate work
      AtomicCounter::increment(counter)
      Semaphore::release(semaphore)
      i
    })
    futures.push(future)
  }
  
  // Collect results
  let results = []
  for future in futures {
    match ThreadPool::get_result(future) {
      Some(result) => results.push(result)
      None => assert_true(false)
    }
  }
  
  assert_eq(AtomicCounter::get(counter), 10)  // 5 threads * 2 increments each
  assert_eq(results.length(), 5)
}

// Test 8: Atomic Reference Operations
test "atomic reference operations" {
  let atomic_ref = AtomicRef::new("initial_value")
  
  // Test get operations
  match AtomicRef::get(atomic_ref) {
    Some(value) => assert_eq(value, "initial_value")
    None => assert_true(false)
  }
  
  // Test compare and swap
  let old_value = AtomicRef::compare_and_swap(atomic_ref, "initial_value", "new_value")
  match old_value {
    Some(value) => assert_eq(value, "initial_value")
    None => assert_true(false)
  }
  
  match AtomicRef::get(atomic_ref) {
    Some(value) => assert_eq(value, "new_value")
    None => assert_true(false)
  }
  
  // Failed compare and swap
  let failed = AtomicRef::compare_and_swap(atomic_ref, "initial_value", "another_value")
  match failed {
    Some(value) => assert_eq(value, "new_value")
    None => assert_true(false)
  }
  
  match AtomicRef::get(atomic_ref) {
    Some(value) => assert_eq(value, "new_value")
    None => assert_true(false)
  }
  
  // Test swap
  let swapped = AtomicRef::swap(atomic_ref, "swapped_value")
  match swapped {
    Some(value) => assert_eq(value, "new_value")
    None => assert_true(false)
  }
  
  match AtomicRef::get(atomic_ref) {
    Some(value) => assert_eq(value, "swapped_value")
    None => assert_true(false)
  }
  
  // Test update
  AtomicRef::update(atomic_ref, |value| value + "_updated")
  match AtomicRef::get(atomic_ref) {
    Some(value) => assert_eq(value, "swapped_value_updated")
    None => assert_true(false)
  }
}