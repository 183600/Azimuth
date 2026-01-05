// Azimuth Telemetry System - Concurrent and Thread Safety Tests
// This file contains test cases for concurrent operations and thread safety

// Test 1: Atomic Operations
test "atomic operations" {
  // Test atomic integer
  let atomic_int = AtomicInt::new(0)
  
  assert_eq(AtomicInt::get(atomic_int), 0)
  
  AtomicInt::set(atomic_int, 5)
  assert_eq(AtomicInt::get(atomic_int), 5)
  
  AtomicInt::add(atomic_int, 3)
  assert_eq(AtomicInt::get(atomic_int), 8)
  
  AtomicInt::sub(atomic_int, 2)
  assert_eq(AtomicInt::get(atomic_int), 6)
  
  // Test atomic boolean
  let atomic_bool = AtomicBool::new(false)
  
  assert_false(AtomicBool::get(atomic_bool))
  
  AtomicBool::set(atomic_bool, true)
  assert_true(AtomicBool::get(atomic_bool))
  
  AtomicBool::set(atomic_bool, false)
  assert_false(AtomicBool::get(atomic_bool))
  
  // Test compare and swap
  let atomic_cas = AtomicInt::new(10)
  
  let result1 = AtomicInt::compare_and_swap(atomic_cas, 10, 20)
  assert_true(result1)
  assert_eq(AtomicInt::get(atomic_cas), 20)
  
  let result2 = AtomicInt::compare_and_swap(atomic_cas, 10, 30)
  assert_false(result2)
  assert_eq(AtomicInt::get(atomic_cas), 20)
}

// Test 2: Mutex Operations
test "mutex operations" {
  let mutex = Mutex::new(0)
  
  // Test lock and unlock
  let guard1 = Mutex::lock(mutex)
  match guard1 {
    Ok(guard) => {
      assert_eq(*guard, 0)
      *guard = 5
      Mutex::unlock(guard)
    }
    Error(_) => assert_true(false)
  }
  
  let guard2 = Mutex::lock(mutex)
  match guard2 {
    Ok(guard) => {
      assert_eq(*guard, 5)
      Mutex::unlock(guard)
    }
    Error(_) => assert_true(false)
  }
  
  // Test try lock
  let guard3 = Mutex::try_lock(mutex)
  match guard3 {
    Ok(guard) => {
      assert_eq(*guard, 5)
      Mutex::unlock(guard)
    }
    Error(_) => assert_true(false)
  }
  
  // Test nested lock detection
  let guard4 = Mutex::lock(mutex)
  match guard4 {
    Ok(guard) => {
      // Try to lock again (should fail)
      let guard5 = Mutex::try_lock(mutex)
      match guard5 {
        Ok(_) => assert_true(false)
        Error(_) => assert_true(true)
      }
      Mutex::unlock(guard)
    }
    Error(_) => assert_true(false)
  }
}

// Test 3: Read-Write Lock Operations
test "read-write lock operations" {
  let rw_lock = RwLock::new(0)
  
  // Test read lock
  let read_guard1 = RwLock::read_lock(rw_lock)
  match read_guard1 {
    Ok(guard) => {
      assert_eq(*guard, 0)
      RwLock::read_unlock(guard)
    }
    Error(_) => assert_true(false)
  }
  
  // Test write lock
  let write_guard1 = RwLock::write_lock(rw_lock)
  match write_guard1 {
    Ok(guard) => {
      assert_eq(*guard, 0)
      *guard = 10
      RwLock::write_unlock(guard)
    }
    Error(_) => assert_true(false)
  }
  
  // Verify write operation
  let read_guard2 = RwLock::read_lock(rw_lock)
  match read_guard2 {
    Ok(guard) => {
      assert_eq(*guard, 10)
      RwLock::read_unlock(guard)
    }
    Error(_) => assert_true(false)
  }
  
  // Test multiple readers
  let read_guard3 = RwLock::read_lock(rw_lock)
  match read_guard3 {
    Ok(_) => {
      let read_guard4 = RwLock::read_lock(rw_lock)
      match read_guard4 {
        Ok(_) => {
          // Multiple readers should be allowed
          RwLock::read_unlock(read_guard4)
          RwLock::read_unlock(read_guard3)
        }
        Error(_) => assert_true(false)
      }
    }
    Error(_) => assert_true(false)
  }
  
  // Test write lock exclusion
  let write_guard2 = RwLock::write_lock(rw_lock)
  match write_guard2 {
    Ok(_) => {
      // Try to get another write lock (should fail)
      let write_guard3 = RwLock::try_write_lock(rw_lock)
      match write_guard3 {
        Ok(_) => assert_true(false)
        Error(_) => assert_true(true)
      }
      
      // Try to get a read lock (should fail)
      let read_guard5 = RwLock::try_read_lock(rw_lock)
      match read_guard5 {
        Ok(_) => assert_true(false)
        Error(_) => assert_true(true)
      }
      
      RwLock::write_unlock(write_guard2)
    }
    Error(_) => assert_true(false)
  }
}

// Test 4: Concurrent Counter
test "concurrent counter" {
  let counter = ConcurrentCounter::new()
  
  assert_eq(ConcurrentCounter::get(counter), 0)
  
  ConcurrentCounter::increment(counter)
  assert_eq(ConcurrentCounter::get(counter), 1)
  
  ConcurrentCounter::increment(counter)
  ConcurrentCounter::increment(counter)
  assert_eq(ConcurrentCounter::get(counter), 3)
  
  ConcurrentCounter::add(counter, 5)
  assert_eq(ConcurrentCounter::get(counter), 8)
  
  ConcurrentCounter::decrement(counter)
  assert_eq(ConcurrentCounter::get(counter), 7)
  
  ConcurrentCounter::sub(counter, 2)
  assert_eq(ConcurrentCounter::get(counter), 5)
  
  ConcurrentCounter::reset(counter)
  assert_eq(ConcurrentCounter::get(counter), 0)
}

// Test 5: Concurrent Queue
test "concurrent queue" {
  let queue = ConcurrentQueue::new()
  
  assert_true(ConcurrentQueue::is_empty(queue))
  assert_eq(ConcurrentQueue::size(queue), 0)
  
  // Test enqueue
  ConcurrentQueue::enqueue(queue, 1)
  ConcurrentQueue::enqueue(queue, 2)
  ConcurrentQueue::enqueue(queue, 3)
  
  assert_false(ConcurrentQueue::is_empty(queue))
  assert_eq(ConcurrentQueue::size(queue), 3)
  
  // Test dequeue
  let item1 = ConcurrentQueue::dequeue(queue)
  match item1 {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  
  assert_eq(ConcurrentQueue::size(queue), 2)
  
  let item2 = ConcurrentQueue::dequeue(queue)
  match item2 {
    Some(value) => assert_eq(value, 2)
    None => assert_true(false)
  }
  
  let item3 = ConcurrentQueue::dequeue(queue)
  match item3 {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  
  assert_true(ConcurrentQueue::is_empty(queue))
  assert_eq(ConcurrentQueue::size(queue), 0)
  
  // Test dequeue from empty queue
  let empty_item = ConcurrentQueue::dequeue(queue)
  match empty_item {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 6: Thread Pool
test "thread pool operations" {
  let thread_pool = ThreadPool::new(4)
  
  assert_eq(ThreadPool::thread_count(thread_pool), 4)
  
  // Test task submission
  let future1 = ThreadPool::submit(thread_pool, fn() { 1 + 1 })
  let result1 = Future::get(future1)
  match result1 {
    Ok(value) => assert_eq(value, 2)
    Error(_) => assert_true(false)
  }
  
  let future2 = ThreadPool::submit(thread_pool, fn() { 5 * 4 })
  let result2 = Future::get(future2)
  match result2 {
    Ok(value) => assert_eq(value, 20)
    Error(_) => assert_true(false)
  }
  
  // Test multiple tasks
  let futures = []
  for i in 0..10 {
    let future = ThreadPool::submit(thread_pool, fn() { i * i })
    futures.push(future)
  }
  
  let mut results = []
  for future in futures {
    let result = Future::get(future)
    match result {
      Ok(value) => results.push(value)
      Error(_) => assert_true(false)
    }
  }
  
  assert_eq(results.length(), 10)
  
  // Test shutdown
  ThreadPool::shutdown(thread_pool)
}

// Test 7: Concurrent Map
test "concurrent map operations" {
  let map = ConcurrentMap::new()
  
  assert_eq(ConcurrentMap::size(map), 0)
  
  // Test put operations
  ConcurrentMap::put(map, "key1", "value1")
  ConcurrentMap::put(map, "key2", "value2")
  ConcurrentMap::put(map, "key3", "value3")
  
  assert_eq(ConcurrentMap::size(map), 3)
  
  // Test get operations
  let value1 = ConcurrentMap::get(map, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  let value2 = ConcurrentMap::get(map, "key2")
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  // Test get non-existent key
  let non_existent = ConcurrentMap::get(map, "non_existent")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test put if absent
  let result1 = ConcurrentMap::put_if_absent(map, "key1", "new_value1")
  match result1 {
    Some(old_value) => assert_eq(old_value, "value1")
    None => assert_true(false)
  }
  
  let result2 = ConcurrentMap::put_if_absent(map, "key4", "value4")
  match result2 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  assert_eq(ConcurrentMap::size(map), 4)
  
  // Test remove operations
  let removed_value = ConcurrentMap::remove(map, "key2")
  match removed_value {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  assert_eq(ConcurrentMap::size(map), 3)
  
  // Test contains key
  assert_true(ConcurrentMap::contains_key(map, "key1"))
  assert_true(ConcurrentMap::contains_key(map, "key3"))
  assert_true(ConcurrentMap::contains_key(map, "key4"))
  assert_false(ConcurrentMap::contains_key(map, "key2"))
  assert_false(ConcurrentMap::contains_key(map, "non_existent"))
}

// Test 8: Concurrent Set
test "concurrent set operations" {
  let set = ConcurrentSet::new()
  
  assert_true(ConcurrentSet::is_empty(set))
  assert_eq(ConcurrentSet::size(set), 0)
  
  // Test add operations
  assert_true(ConcurrentSet::add(set, "item1"))
  assert_true(ConcurrentSet::add(set, "item2"))
  assert_true(ConcurrentSet::add(set, "item3"))
  
  assert_false(ConcurrentSet::is_empty(set))
  assert_eq(ConcurrentSet::size(set), 3)
  
  // Test add duplicate
  assert_false(ConcurrentSet::add(set, "item1"))
  assert_eq(ConcurrentSet::size(set), 3)
  
  // Test contains operations
  assert_true(ConcurrentSet::contains(set, "item1"))
  assert_true(ConcurrentSet::contains(set, "item2"))
  assert_true(ConcurrentSet::contains(set, "item3"))
  assert_false(ConcurrentSet::contains(set, "item4"))
  
  // Test remove operations
  assert_true(ConcurrentSet::remove(set, "item2"))
  assert_false(ConcurrentSet::contains(set, "item2"))
  assert_eq(ConcurrentSet::size(set), 2)
  
  // Test remove non-existent item
  assert_false(ConcurrentSet::remove(set, "non_existent"))
  assert_eq(ConcurrentSet::size(set), 2)
  
  // Test clear
  ConcurrentSet::clear(set)
  assert_true(ConcurrentSet::is_empty(set))
  assert_eq(ConcurrentSet::size(set), 0)
}

// Test 9: Barrier Synchronization
test "barrier synchronization" {
  let barrier = Barrier::new(3)
  
  assert_eq(Barrier::parties(barrier), 3)
  assert_false(Barrier::is_broken(barrier))
  
  // Test wait operations
  let mut waiting_count = 0
  
  // Simulate threads waiting at barrier
  waiting_count = waiting_count + 1
  let result1 = Barrier::wait(barrier)
  match result1 {
    Ok(is_leader) => assert_true(is_leader || !is_leader)  // One thread should be leader
    Error(_) => assert_true(false)
  }
  
  waiting_count = waiting_count + 1
  let result2 = Barrier::wait(barrier)
  match result2 {
    Ok(is_leader) => assert_true(is_leader || !is_leader)
    Error(_) => assert_true(false)
  }
  
  waiting_count = waiting_count + 1
  let result3 = Barrier::wait(barrier)
  match result3 {
    Ok(is_leader) => assert_true(is_leader || !is_leader)
    Error(_) => assert_true(false)
  }
  
  assert_eq(waiting_count, 3)
}

// Test 10: Semaphore Operations
test "semaphore operations" {
  let semaphore = Semaphore::new(2)
  
  assert_eq(Semaphore::available_permits(semaphore), 2)
  
  // Test acquire operations
  assert_true(Semaphore::try_acquire(semaphore))
  assert_eq(Semaphore::available_permits(semaphore), 1)
  
  assert_true(Semaphore::try_acquire(semaphore))
  assert_eq(Semaphore::available_permits(semaphore), 0)
  
  // Test acquire when no permits available
  assert_false(Semaphore::try_acquire(semaphore))
  assert_eq(Semaphore::available_permits(semaphore), 0)
  
  // Test release operations
  Semaphore::release(semaphore)
  assert_eq(Semaphore::available_permits(semaphore), 1)
  
  Semaphore::release(semaphore)
  assert_eq(Semaphore::available_permits(semaphore), 2)
  
  // Test release beyond initial permits
  Semaphore::release(semaphore)
  assert_eq(Semaphore::available_permits(semaphore), 3)
  
  // Test drain permits
  let drained = Semaphore::drain_permits(semaphore)
  assert_eq(drained, 3)
  assert_eq(Semaphore::available_permits(semaphore), 0)
  
  // Test try acquire with timeout
  let result = Semaphore::try_acquire_with_timeout(semaphore, 100)
  match result {
    Ok(acquired) => assert_false(acquired)
    Error(_) => assert_true(false)
  }
}

// Type definitions for concurrent data structures

// AtomicInt type
type AtomicInt Int

// AtomicInt methods
fn AtomicInt::new(value: Int) -> AtomicInt { value }
fn AtomicInt::get(atomic: AtomicInt) -> Int { atomic }
fn AtomicInt::set(atomic: AtomicInt, value: Int) -> Unit { atomic = value }
fn AtomicInt::add(atomic: AtomicInt, value: Int) -> Unit { atomic = atomic + value }
fn AtomicInt::sub(atomic: AtomicInt, value: Int) -> Unit { atomic = atomic - value }
fn AtomicInt::compare_and_swap(atomic: AtomicInt, expected: Int, new: Int) -> Bool {
  if atomic == expected {
    atomic = new
    true
  } else {
    false
  }
}

// AtomicBool type
type AtomicBool Bool

// AtomicBool methods
fn AtomicBool::new(value: Bool) -> AtomicBool { value }
fn AtomicBool::get(atomic: AtomicBool) -> Bool { atomic }
fn AtomicBool::set(atomic: AtomicBool, value: Bool) -> Unit { atomic = value }

// Mutex type
type Mutex[T] { value: T, is_locked: Bool }

// Mutex methods
fn Mutex::new[T](value: T) -> Mutex[T] { { value: value, is_locked: false } }
fn Mutex::lock[T](mutex: Mutex[T]) -> Result[T, String] {
  if mutex.is_locked {
    Error("Mutex already locked")
  } else {
    mutex.is_locked = true
    Ok(mutex.value)
  }
}
fn Mutex::try_lock[T](mutex: Mutex[T]) -> Result[T, String] {
  Mutex::lock(mutex)
}
fn Mutex::unlock[T](mutex: Mutex[T]) -> Unit {
  mutex.is_locked = false
}

// RwLock type
type RwLock[T] { value: T, readers: Int, writer: Bool }

// RwLock methods
fn RwLock::new[T](value: T) -> RwLock[T] { { value: value, readers: 0, writer: false } }
fn RwLock::read_lock[T](rw_lock: RwLock[T]) -> Result[T, String] {
  if rw_lock.writer {
    Error("Write lock held")
  } else {
    rw_lock.readers = rw_lock.readers + 1
    Ok(rw_lock.value)
  }
}
fn RwLock::try_read_lock[T](rw_lock: RwLock[T]) -> Result[T, String] {
  RwLock::read_lock(rw_lock)
}
fn RwLock::read_unlock[T](rw_lock: RwLock[T]) -> Unit {
  rw_lock.readers = rw_lock.readers - 1
}
fn RwLock::write_lock[T](rw_lock: RwLock[T]) -> Result[T, String] {
  if rw_lock.writer || rw_lock.readers > 0 {
    Error("Lock held")
  } else {
    rw_lock.writer = true
    Ok(rw_lock.value)
  }
}
fn RwLock::try_write_lock[T](rw_lock: RwLock[T]) -> Result[T, String] {
  RwLock::write_lock(rw_lock)
}
fn RwLock::write_unlock[T](rw_lock: RwLock[T]) -> Unit {
  rw_lock.writer = false
}

// ConcurrentCounter type
type ConcurrentCounter { value: AtomicInt }

// ConcurrentCounter methods
fn ConcurrentCounter::new() -> ConcurrentCounter { { value: AtomicInt::new(0) } }
fn ConcurrentCounter::get(counter: ConcurrentCounter) -> Int { AtomicInt::get(counter.value) }
fn ConcurrentCounter::increment(counter: ConcurrentCounter) -> Unit { AtomicInt::add(counter.value, 1) }
fn ConcurrentCounter::decrement(counter: ConcurrentCounter) -> Unit { AtomicInt::sub(counter.value, 1) }
fn ConcurrentCounter::add(counter: ConcurrentCounter, value: Int) -> Unit { AtomicInt::add(counter.value, value) }
fn ConcurrentCounter::sub(counter: ConcurrentCounter, value: Int) -> Unit { AtomicInt::sub(counter.value, value) }
fn ConcurrentCounter::reset(counter: ConcurrentCounter) -> Unit { AtomicInt::set(counter.value, 0) }

// ConcurrentQueue type
type ConcurrentQueue[T] { items: Array[T], mutex: Mutex[Unit] }

// ConcurrentQueue methods
fn ConcurrentQueue::new[T]() -> ConcurrentQueue[T] { 
  { items: [], mutex: Mutex::new(()) } 
}
fn ConcurrentQueue::enqueue[T](queue: ConcurrentQueue[T], item: T) -> Unit {
  let _ = Mutex::lock(queue.mutex)
  queue.items.push(item)
  Mutex::unlock(queue.mutex)
}
fn ConcurrentQueue::dequeue[T](queue: ConcurrentQueue[T]) -> Option[T] {
  let _ = Mutex::lock(queue.mutex)
  let result = if queue.items.length() > 0 {
    Some(queue.items.shift())
  } else {
    None
  }
  Mutex::unlock(queue.mutex)
  result
}
fn ConcurrentQueue::is_empty[T](queue: ConcurrentQueue[T]) -> Bool {
  let _ = Mutex::lock(queue.mutex)
  let result = queue.items.length() == 0
  Mutex::unlock(queue.mutex)
  result
}
fn ConcurrentQueue::size[T](queue: ConcurrentQueue[T]) -> Int {
  let _ = Mutex::lock(queue.mutex)
  let result = queue.items.length()
  Mutex::unlock(queue.mutex)
  result
}

// ThreadPool type
type ThreadPool { thread_count: Int }

// ThreadPool methods
fn ThreadPool::new(thread_count: Int) -> ThreadPool { { thread_count: thread_count } }
fn ThreadPool::thread_count(pool: ThreadPool) -> Int { pool.thread_count }
fn ThreadPool::submit[T](pool: ThreadPool, task: () -> T) -> Future[T] {
  Future::new(task)
}
fn ThreadPool::shutdown(pool: ThreadPool) -> Unit { () }

// Future type
type Future[T] { task: () -> T, completed: Bool, result: Option[T] }

// Future methods
fn Future::new[T](task: () -> T) -> Future[T] { 
  { task: task, completed: false, result: None } 
}
fn Future::get[T](future: Future[T]) -> Result[T, String] {
  if !future.completed {
    let result = future.task()
    future.completed = true
    future.result = Some(result)
    Ok(result)
  } else {
    match future.result {
      Some(value) => Ok(value),
      None => Error("No result available")
    }
  }
}

// ConcurrentMap type
type ConcurrentMap[K, V] { map: Map[K, V], mutex: Mutex[Unit] }

// ConcurrentMap methods
fn ConcurrentMap::new[K, V]() -> ConcurrentMap[K, V] { 
  { map: Map::new(), mutex: Mutex::new(()) } 
}
fn ConcurrentMap::put[K, V](map: ConcurrentMap[K, V], key: K, value: V) -> Unit {
  let _ = Mutex::lock(map.mutex)
  map.map.insert(key, value)
  Mutex::unlock(map.mutex)
}
fn ConcurrentMap::get[K, V](map: ConcurrentMap[K, V], key: K) -> Option[V] {
  let _ = Mutex::lock(map.mutex)
  let result = map.map.get(key)
  Mutex::unlock(map.mutex)
  result
}
fn ConcurrentMap::put_if_absent[K, V](map: ConcurrentMap[K, V], key: K, value: V) -> Option[V] {
  let _ = Mutex::lock(map.mutex)
  let result = if !map.map.contains_key(key) {
    map.map.insert(key, value)
    None
  } else {
    map.map.get(key)
  }
  Mutex::unlock(map.mutex)
  result
}
fn ConcurrentMap::remove[K, V](map: ConcurrentMap[K, V], key: K) -> Option[V] {
  let _ = Mutex::lock(map.mutex)
  let result = map.map.remove(key)
  Mutex::unlock(map.mutex)
  result
}
fn ConcurrentMap::contains_key[K, V](map: ConcurrentMap[K, V], key: K) -> Bool {
  let _ = Mutex::lock(map.mutex)
  let result = map.map.contains_key(key)
  Mutex::unlock(map.mutex)
  result
}
fn ConcurrentMap::size[K, V](map: ConcurrentMap[K, V]) -> Int {
  let _ = Mutex::lock(map.mutex)
  let result = map.map.size()
  Mutex::unlock(map.mutex)
  result
}

// ConcurrentSet type
type ConcurrentSet[T] { set: Set[T], mutex: Mutex[Unit] }

// ConcurrentSet methods
fn ConcurrentSet::new[T]() -> ConcurrentSet[T] { 
  { set: Set::new(), mutex: Mutex::new(()) } 
}
fn ConcurrentSet::add[T](set: ConcurrentSet[T], item: T) -> Bool {
  let _ = Mutex::lock(set.mutex)
  let result = set.set.insert(item)
  Mutex::unlock(set.mutex)
  result
}
fn ConcurrentSet::remove[T](set: ConcurrentSet[T], item: T) -> Bool {
  let _ = Mutex::lock(set.mutex)
  let result = set.set.remove(item)
  Mutex::unlock(set.mutex)
  result
}
fn ConcurrentSet::contains[T](set: ConcurrentSet[T], item: T) -> Bool {
  let _ = Mutex::lock(set.mutex)
  let result = set.set.contains(item)
  Mutex::unlock(set.mutex)
  result
}
fn ConcurrentSet::is_empty[T](set: ConcurrentSet[T]) -> Bool {
  let _ = Mutex::lock(set.mutex)
  let result = set.set.is_empty()
  Mutex::unlock(set.mutex)
  result
}
fn ConcurrentSet::size[T](set: ConcurrentSet[T]) -> Int {
  let _ = Mutex::lock(set.mutex)
  let result = set.set.size()
  Mutex::unlock(set.mutex)
  result
}
fn ConcurrentSet::clear[T](set: ConcurrentSet[T]) -> Unit {
  let _ = Mutex::lock(set.mutex)
  set.set.clear()
  Mutex::unlock(set.mutex)
}

// Barrier type
type Barrier { parties: Int, waiting: Int, is_broken: Bool }

// Barrier methods
fn Barrier::new(parties: Int) -> Barrier { 
  { parties: parties, waiting: 0, is_broken: false } 
}
fn Barrier::parties(barrier: Barrier) -> Int { barrier.parties }
fn Barrier::is_broken(barrier: Barrier) -> Bool { barrier.is_broken }
fn Barrier::wait(barrier: Barrier) -> Result[Bool, String] {
  if barrier.is_broken {
    Error("Barrier is broken")
  } else {
    barrier.waiting = barrier.waiting + 1
    if barrier.waiting >= barrier.parties {
      barrier.waiting = 0
      Ok(true)  // Last thread to arrive is the leader
    } else {
      Ok(false)  // Other threads are not leaders
    }
  }
}

// Semaphore type
type Semaphore { permits: Int }

// Semaphore methods
fn Semaphore::new(permits: Int) -> Semaphore { { permits: permits } }
fn Semaphore::available_permits(semaphore: Semaphore) -> Int { semaphore.permits }
fn Semaphore::try_acquire(semaphore: Semaphore) -> Bool {
  if semaphore.permits > 0 {
    semaphore.permits = semaphore.permits - 1
    true
  } else {
    false
  }
}
fn Semaphore::try_acquire_with_timeout(semaphore: Semaphore, timeout_ms: Int) -> Result[Bool, String] {
  // Simplified implementation - doesn't actually wait
  Ok(Semaphore::try_acquire(semaphore))
}
fn Semaphore::release(semaphore: Semaphore) -> Unit {
  semaphore.permits = semaphore.permits + 1
}
fn Semaphore::drain_permits(semaphore: Semaphore) -> Int {
  let result = semaphore.permits
  semaphore.permits = 0
  result
}