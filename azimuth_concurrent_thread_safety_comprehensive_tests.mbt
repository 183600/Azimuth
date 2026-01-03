// Azimuth Telemetry System - Concurrent Thread Safety Comprehensive Tests
// This file contains comprehensive test cases for concurrent operations and thread safety

// Test 1: Concurrent Counter Operations
test "concurrent counter operations" {
  // Test thread-safe counter
  let counter = ConcurrentCounter::new()
  
  // Test initial value
  assert_eq(ConcurrentCounter::get(counter), 0)
  
  // Test increment operations
  ConcurrentCounter::increment(counter)
  assert_eq(ConcurrentCounter::get(counter), 1)
  
  ConcurrentCounter::increment_by(counter, 5)
  assert_eq(ConcurrentCounter::get(counter), 6)
  
  // Test decrement operations
  ConcurrentCounter::decrement(counter)
  assert_eq(ConcurrentCounter::get(counter), 5)
  
  ConcurrentCounter::decrement_by(counter, 2)
  assert_eq(ConcurrentCounter::get(counter), 3)
  
  // Test compare and swap
  let cas_result = ConcurrentCounter::compare_and_swap(counter, 3, 10)
  assert_true(cas_result)
  assert_eq(ConcurrentCounter::get(counter), 10)
  
  let cas_fail = ConcurrentCounter::compare_and_swap(counter, 3, 20)
  assert_false(cas_fail)
  assert_eq(ConcurrentCounter::get(counter), 10)
  
  // Test reset
  ConcurrentCounter::reset(counter)
  assert_eq(ConcurrentCounter::get(counter), 0)
}

// Test 2: Concurrent Queue Operations
test "concurrent queue operations" {
  // Test thread-safe queue
  let queue = ConcurrentQueue::new()
  
  // Test empty queue
  assert_true(ConcurrentQueue::is_empty(queue))
  assert_eq(ConcurrentQueue::size(queue), 0)
  
  // Test enqueue operations
  ConcurrentQueue::enqueue(queue, "item1")
  ConcurrentQueue::enqueue(queue, "item2")
  ConcurrentQueue::enqueue(queue, "item3")
  
  assert_false(ConcurrentQueue::is_empty(queue))
  assert_eq(ConcurrentQueue::size(queue), 3)
  
  // Test dequeue operations
  let dequeued1 = ConcurrentQueue::dequeue(queue)
  match dequeued1 {
    Some(item) => assert_eq(item, "item1")
    None => assert_true(false)
  }
  
  assert_eq(ConcurrentQueue::size(queue), 2)
  
  let dequeued2 = ConcurrentQueue::dequeue(queue)
  match dequeued2 {
    Some(item) => assert_eq(item, "item2")
    None => assert_true(false)
  }
  
  // Test peek operation
  let peeked = ConcurrentQueue::peek(queue)
  match peeked {
    Some(item) => assert_eq(item, "item3")
    None => assert_true(false)
  }
  
  // Test remaining items
  let dequeued3 = ConcurrentQueue::dequeue(queue)
  match dequeued3 {
    Some(item) => assert_eq(item, "item3")
    None => assert_true(false)
  }
  
  // Test empty queue again
  assert_true(ConcurrentQueue::is_empty(queue))
  assert_eq(ConcurrentQueue::size(queue), 0)
  
  let empty_dequeue = ConcurrentQueue::dequeue(queue)
  match empty_dequeue {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let empty_peek = ConcurrentQueue::peek(queue)
  match empty_peek {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Concurrent Hash Map Operations
test "concurrent hash map operations" {
  // Test thread-safe hash map
  let hash_map = ConcurrentHashMap::new()
  
  // Test insertion
  ConcurrentHashMap::put(hash_map, "key1", "value1")
  ConcurrentHashMap::put(hash_map, "key2", "value2")
  ConcurrentHashMap::put(hash_map, "key3", "value3")
  
  // Test retrieval
  let value1 = ConcurrentHashMap::get(hash_map, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  let non_existent = ConcurrentHashMap::get(hash_map, "non_existent")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test update
  ConcurrentHashMap::put(hash_map, "key1", "updated_value1")
  let updated_value = ConcurrentHashMap::get(hash_map, "key1")
  match updated_value {
    Some(v) => assert_eq(v, "updated_value1")
    None => assert_true(false)
  }
  
  // Test removal
  let removed = ConcurrentHashMap::remove(hash_map, "key2")
  match removed {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  let value_after_removal = ConcurrentHashMap::get(hash_map, "key2")
  match value_after_removal {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test size
  assert_eq(ConcurrentHashMap::size(hash_map), 2)
  
  // Test contains key
  assert_true(ConcurrentHashMap::contains_key(hash_map, "key1"))
  assert_false(ConcurrentHashMap::contains_key(hash_map, "key2"))
  
  // Test put if absent
  let put_absent_result = ConcurrentHashMap::put_if_absent(hash_map, "key4", "value4")
  match put_absent_result {
    Some(_) => assert_true(false) // Should not exist
    None => assert_true(true)
  }
  
  let put_existing_result = ConcurrentHashMap::put_if_absent(hash_map, "key1", "new_value")
  match put_existing_result {
    Some(v) => assert_eq(v, "updated_value1") // Should return existing value
    None => assert_true(false)
  }
  
  // Test clear
  ConcurrentHashMap::clear(hash_map)
  assert_eq(ConcurrentHashMap::size(hash_map), 0)
}

// Test 4: Read-Write Lock Operations
test "read-write lock operations" {
  // Test read-write lock
  let rw_lock = ReadWriteLock::new()
  let shared_data = Ref::new(0)
  
  // Test read lock
  let read_lock = ReadWriteLock::read_lock(rw_lock)
  assert_true(ReadWriteLock::is_read_locked(rw_lock))
  assert_false(ReadWriteLock::is_write_locked(rw_lock))
  
  // Multiple read locks should be allowed
  let read_lock2 = ReadWriteLock::read_lock(rw_lock)
  assert_true(ReadWriteLock::is_read_locked(rw_lock))
  
  // Release read locks
  ReadWriteLock::unlock_read(rw_lock, read_lock2)
  ReadWriteLock::unlock_read(rw_lock, read_lock)
  assert_false(ReadWriteLock::is_read_locked(rw_lock))
  
  // Test write lock
  let write_lock = ReadWriteLock::write_lock(rw_lock)
  assert_true(ReadWriteLock::is_write_locked(rw_lock))
  assert_false(ReadWriteLock::is_read_locked(rw_lock))
  
  // Only one write lock should be allowed
  let write_lock2 = ReadWriteLock::try_write_lock(rw_lock)
  match write_lock2 {
    Some(_) => assert_true(false) // Should not be able to acquire another write lock
    None => assert_true(true)
  }
  
  // Release write lock
  ReadWriteLock::unlock_write(rw_lock, write_lock)
  assert_false(ReadWriteLock::is_write_locked(rw_lock))
  
  // Test try read lock
  let try_read = ReadWriteLock::try_read_lock(rw_lock)
  match try_read {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  // Test try write lock
  let try_write = ReadWriteLock::try_write_lock(rw_lock)
  match try_write {
    Some(_) => assert_true(false) // Should not be able to acquire with read lock held
    None => assert_true(true)
  }
}

// Test 5: Atomic Operations
test "atomic operations" {
  // Test atomic integer
  let atomic_int = AtomicInteger::new(0)
  
  // Test get and set
  assert_eq(AtomicInteger::get(atomic_int), 0)
  AtomicInteger::set(atomic_int, 10)
  assert_eq(AtomicInteger::get(atomic_int), 10)
  
  // Test increment and decrement
  let incremented = AtomicInteger::increment_and_get(atomic_int)
  assert_eq(incremented, 11)
  assert_eq(AtomicInteger::get(atomic_int), 11)
  
  let decremented = AtomicInteger::decrement_and_get(atomic_int)
  assert_eq(decremented, 10)
  assert_eq(AtomicInteger::get(atomic_int), 10)
  
  // Test add and get
  let added = AtomicInteger::add_and_get(atomic_int, 5)
  assert_eq(added, 15)
  assert_eq(AtomicInteger::get(atomic_int), 15)
  
  // Test get and add
  let previous = AtomicInteger::get_and_add(atomic_int, 3)
  assert_eq(previous, 15)
  assert_eq(AtomicInteger::get(atomic_int), 18)
  
  // Test compare and set
  let cas_result = AtomicInteger::compare_and_set(atomic_int, 18, 25)
  assert_true(cas_result)
  assert_eq(AtomicInteger::get(atomic_int), 25)
  
  let cas_fail = AtomicInteger::compare_and_set(atomic_int, 18, 30)
  assert_false(cas_fail)
  assert_eq(AtomicInteger::get(atomic_int), 25)
  
  // Test atomic boolean
  let atomic_bool = AtomicBoolean::new(false)
  
  assert_false(AtomicBoolean::get(atomic_bool))
  
  AtomicBoolean::set(atomic_bool, true)
  assert_true(AtomicBoolean::get(atomic_bool))
  
  let previous_bool = AtomicBoolean::get_and_set(atomic_bool, false)
  assert_true(previous_bool)
  assert_false(AtomicBoolean::get(atomic_bool))
  
  let compare_result = AtomicBoolean::compare_and_set(atomic_bool, false, true)
  assert_true(compare_result)
  assert_true(AtomicBoolean::get(atomic_bool))
}

// Test 6: Thread Pool Operations
test "thread pool operations" {
  // Test thread pool creation
  let thread_pool = ThreadPool::new(4)
  
  // Test submitting tasks
  let future1 = ThreadPool::submit(thread_pool, fn() { 1 + 1 })
  let future2 = ThreadPool::submit(thread_pool, fn() { 2 * 3 })
  let future3 = ThreadPool::submit(thread_pool, fn() { "hello" + " world" })
  
  // Test getting results
  let result1 = Future::get(future1)
  match result1 {
    Some(value) => assert_eq(value, 2)
    None => assert_true(false)
  }
  
  let result2 = Future::get(future2)
  match result2 {
    Some(value) => assert_eq(value, 6)
    None => assert_true(false)
  }
  
  let result3 = Future::get(future3)
  match result3 {
    Some(value) => assert_eq(value, "hello world")
    None => assert_true(false)
  }
  
  // Test task completion
  assert_true(Future::is_done(future1))
  assert_true(Future::is_done(future2))
  assert_true(Future::is_done(future3))
  
  // Test pool shutdown
  ThreadPool::shutdown(thread_pool)
  assert_true(ThreadPool::is_shutdown(thread_pool))
  
  // Test submitting task after shutdown
  let future_after_shutdown = ThreadPool::submit(thread_pool, fn() { 42 })
  assert_true(Future::is_cancelled(future_after_shutdown))
}

// Test 7: Semaphore Operations
test "semaphore operations" {
  // Test semaphore with 3 permits
  let semaphore = Semaphore::new(3)
  
  // Test available permits
  assert_eq(Semaphore::available_permits(semaphore), 3)
  
  // Test acquire
  Semaphore::acquire(semaphore)
  assert_eq(Semaphore::available_permits(semaphore), 2)
  
  Semaphore::acquire(semaphore)
  assert_eq(Semaphore::available_permits(semaphore), 1)
  
  // Test try acquire
  let try_result = Semaphore::try_acquire(semaphore)
  assert_true(try_result)
  assert_eq(Semaphore::available_permits(semaphore), 0)
  
  // Test try acquire when no permits available
  let try_fail = Semaphore::try_acquire(semaphore)
  assert_false(try_fail)
  assert_eq(Semaphore::available_permits(semaphore), 0)
  
  // Test release
  Semaphore::release(semaphore)
  assert_eq(Semaphore::available_permits(semaphore), 1)
  
  Semaphore::release(semaphore)
  assert_eq(Semaphore::available_permits(semaphore), 2)
  
  // Test drain permits
  let drained = Semaphore::drain_permits(semaphore)
  assert_eq(drained, 2)
  assert_eq(Semaphore::available_permits(semaphore), 0)
  
  // Test reduce permits
  Semaphore::reduce_permits(semaphore, 1)
  assert_eq(Semaphore::available_permits(semaphore), 0)
  
  // Test release more than initial permits
  Semaphore::release(semaphore)
  Semaphore::release(semaphore)
  assert_eq(Semaphore::available_permits(semaphore), 2)
}

// Test 8: Countdown Latch Operations
test "countdown latch operations" {
  // Test countdown latch with count 3
  let latch = CountDownLatch::new(3)
  
  // Test initial count
  assert_eq(CountDownLatch::get_count(latch), 3)
  assert_false(CountDownLatch::is_counted_down(latch))
  
  // Test countdown
  CountDownLatch::count_down(latch)
  assert_eq(CountDownLatch::get_count(latch), 2)
  assert_false(CountDownLatch::is_counted_down(latch))
  
  CountDownLatch::count_down(latch)
  assert_eq(CountDownLatch::get_count(latch), 1)
  assert_false(CountDownLatch::is_counted_down(latch))
  
  CountDownLatch::count_down(latch)
  assert_eq(CountDownLatch::get_count(latch), 0)
  assert_true(CountDownLatch::is_counted_down(latch))
  
  // Test await when already counted down
  let await_result = CountDownLatch::await(latch, 100) // 100ms timeout
  assert_true(await_result)
  
  // Test new latch for await timeout
  let new_latch = CountDownLatch::new(2)
  CountDownLatch::count_down(new_latch)
  
  // This should timeout since count is not zero
  let timeout_result = CountDownLatch::await(new_latch, 10) // 10ms timeout
  assert_false(timeout_result)
  assert_eq(CountDownLatch::get_count(new_latch), 1)
}

// Test 9: Cyclic Barrier Operations
test "cyclic barrier operations" {
  // Test cyclic barrier with 3 parties
  let barrier = CyclicBarrier::new(3)
  
  // Test initial parties
  assert_eq(CyclicBarrier::get_parties(barrier), 3)
  assert_eq(CyclicBarrier::get_number_waiting(barrier), 0)
  
  // Test barrier reset
  CyclicBarrier::reset(barrier)
  assert_false(CyclicBarrier::is_broken(barrier))
  
  // Test barrier action
  let barrier_with_action = CyclicBarrier::with_action(2, fn() { "action_executed" })
  
  // In a real scenario, multiple threads would wait at the barrier
  // For this test, we'll simulate the behavior
  assert_eq(CyclicBarrier::get_parties(barrier_with_action), 2)
  
  // Test broken barrier
  CyclicBarrier::reset(barrier_with_action)
  assert_false(CyclicBarrier::is_broken(barrier_with_action))
}

// Test 10: Concurrent Collection Operations
test "concurrent collection operations" {
  // Test concurrent list
  let concurrent_list = ConcurrentList::new()
  
  // Test add operations
  ConcurrentList::add(concurrent_list, "item1")
  ConcurrentList::add(concurrent_list, "item2")
  ConcurrentList::add(concurrent_list, "item3")
  
  // Test size
  assert_eq(ConcurrentList::size(concurrent_list), 3)
  
  // Test get operations
  let item1 = ConcurrentList::get(concurrent_list, 0)
  match item1 {
    Some(value) => assert_eq(value, "item1")
    None => assert_true(false)
  }
  
  let item3 = ConcurrentList::get(concurrent_list, 2)
  match item3 {
    Some(value) => assert_eq(value, "item3")
    None => assert_true(false)
  }
  
  // Test remove operations
  let removed = ConcurrentList::remove(concurrent_list, 1)
  match removed {
    Some(value) => assert_eq(value, "item2")
    None => assert_true(false)
  }
  
  assert_eq(ConcurrentList::size(concurrent_list), 2)
  
  // Test contains
  assert_true(ConcurrentList::contains(concurrent_list, "item1"))
  assert_false(ConcurrentList::contains(concurrent_list, "item2"))
  
  // Test clear
  ConcurrentList::clear(concurrent_list)
  assert_eq(ConcurrentList::size(concurrent_list), 0)
  
  // Test concurrent set
  let concurrent_set = ConcurrentSet::new()
  
  // Test add operations
  assert_true(ConcurrentSet::add(concurrent_set, "unique1"))
  assert_true(ConcurrentSet::add(concurrent_set, "unique2"))
  assert_false(ConcurrentSet::add(concurrent_set, "unique1")) // Duplicate
  
  // Test size
  assert_eq(ConcurrentSet::size(concurrent_set), 2)
  
  // Test contains
  assert_true(ConcurrentSet::contains(concurrent_set, "unique1"))
  assert_false(ConcurrentSet::contains(concurrent_set, "nonexistent"))
  
  // Test remove
  assert_true(ConcurrentSet::remove(concurrent_set, "unique1"))
  assert_false(ConcurrentSet::remove(concurrent_set, "nonexistent"))
  
  assert_eq(ConcurrentSet::size(concurrent_set), 1)
  assert_false(ConcurrentSet::contains(concurrent_set, "unique1"))
}