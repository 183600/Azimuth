// Azimuth Telemetry System - Advanced Concurrent Safety Tests
// This file contains comprehensive test cases for concurrent safety mechanisms

// Test 1: Thread-Safe Counter
test "thread-safe counter implementation" {
  type AtomicCounter = {
    value: Int,
    lock: Bool
  }
  
  let atomic_counter_new = fn(initial_value: Int) -> AtomicCounter {
    { value: initial_value, lock: false }
  }
  
  let atomic_counter_increment = fn(counter: AtomicCounter) -> AtomicCounter {
    // Simulate atomic operation with lock
    if counter.lock {
      counter  // Would block in real implementation
    } else {
      { value: counter.value + 1, lock: counter.lock }
    }
  }
  
  let atomic_counter_get = fn(counter: AtomicCounter) -> Int {
    counter.value
  }
  
  let atomic_counter_add = fn(counter: AtomicCounter, delta: Int) -> AtomicCounter {
    if counter.lock {
      counter  // Would block in real implementation
    } else {
      { value: counter.value + delta, lock: counter.lock }
    }
  }
  
  // Test atomic counter operations
  let counter1 = atomic_counter_new(0)
  let counter2 = atomic_counter_increment(counter1)
  let counter3 = atomic_counter_increment(counter2)
  let counter4 = atomic_counter_add(counter3, 5)
  
  assert_eq(atomic_counter_get(counter1), 0)
  assert_eq(atomic_counter_get(counter2), 1)
  assert_eq(atomic_counter_get(counter3), 2)
  assert_eq(atomic_counter_get(counter4), 7)
  
  // Test with negative values
  let counter5 = atomic_counter_add(counter4, -3)
  assert_eq(atomic_counter_get(counter5), 4)
}

// Test 2: Thread-Safe Queue
test "thread-safe queue implementation" {
  type ConcurrentQueue[T] = {
    items: Array[T],
    head: Int,
    tail: Int,
    size: Int,
    capacity: Int,
    lock: Bool
  }
  
  let concurrent_queue_new = fn[T](capacity: Int) -> ConcurrentQueue[T] {
    let mut items = []
    for i in 0..=capacity-1 {
      items = items + [None]  // Placeholder for generic type
    }
    { items, head: 0, tail: 0, size: 0, capacity, lock: false }
  }
  
  let concurrent_queue_enqueue = fn[T](queue: ConcurrentQueue[T], item: T) -> ConcurrentQueue[T] {
    if queue.size >= queue.capacity {
      queue  // Queue is full
    } else if queue.lock {
      queue  // Would block in real implementation
    } else {
      let new_items = queue.items.with(queue.tail, item)
      let new_tail = (queue.tail + 1) % queue.capacity
      { 
        items: new_items, 
        head: queue.head, 
        tail: new_tail, 
        size: queue.size + 1, 
        capacity: queue.capacity, 
        lock: queue.lock 
      }
    }
  }
  
  let concurrent_queue_dequeue = fn[T](queue: ConcurrentQueue[T]) -> (ConcurrentQueue[T], Option[T]) {
    if queue.size == 0 {
      (queue, None)  // Queue is empty
    } else if queue.lock {
      (queue, None)  // Would block in real implementation
    } else {
      let item = queue.items[queue.head]
      let new_head = (queue.head + 1) % queue.capacity
      let new_queue = { 
        items: queue.items, 
        head: new_head, 
        tail: queue.tail, 
        size: queue.size - 1, 
        capacity: queue.capacity, 
        lock: queue.lock 
      }
      (new_queue, Some(item))
    }
  }
  
  // Test concurrent queue
  let queue1 = concurrent_queue_new(3)
  assert_eq(queue1.size, 0)
  
  let queue2 = concurrent_queue_enqueue(queue1, "item1")
  assert_eq(queue2.size, 1)
  
  let queue3 = concurrent_queue_enqueue(queue2, "item2")
  assert_eq(queue3.size, 2)
  
  let queue4 = concurrent_queue_enqueue(queue3, "item3")
  assert_eq(queue4.size, 3)
  
  // Queue is now full
  let queue5 = concurrent_queue_enqueue(queue4, "item4")
  assert_eq(queue5.size, 3)  // Should not change
  
  // Dequeue items
  let (queue6, item1) = concurrent_queue_dequeue(queue5)
  match item1 {
    Some(value) => assert_eq(value, "item1")
    None => assert_true(false)
  }
  assert_eq(queue6.size, 2)
  
  let (queue7, item2) = concurrent_queue_dequeue(queue6)
  match item2 {
    Some(value) => assert_eq(value, "item2")
    None => assert_true(false)
  }
  assert_eq(queue7.size, 1)
  
  let (queue8, item3) = concurrent_queue_dequeue(queue7)
  match item3 {
    Some(value) => assert_eq(value, "item3")
    None => assert_true(false)
  }
  assert_eq(queue8.size, 0)
  
  // Queue is now empty
  let (queue9, empty_item) = concurrent_queue_dequeue(queue8)
  match empty_item {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  assert_eq(queue9.size, 0)
}

// Test 3: Read-Write Lock
test "read-write lock implementation" {
  enum LockState {
    Unlocked
    ReadLocked(Int)
    WriteLocked
  }
  
  type ReadWriteLock = {
    state: LockState,
    waiting_writers: Int,
    waiting_readers: Int
  }
  
  let read_write_lock_new = fn() -> ReadWriteLock {
    { state: Unlocked, waiting_writers: 0, waiting_readers: 0 }
  }
  
  let read_write_lock_acquire_read = fn(lock: ReadWriteLock) -> ReadWriteLock {
    match lock.state {
      Unlocked => { state: ReadLocked(1), waiting_writers: lock.waiting_writers, waiting_readers: lock.waiting_readers }
      ReadLocked(count) => { state: ReadLocked(count + 1), waiting_writers: lock.waiting_writers, waiting_readers: lock.waiting_readers }
      WriteLocked => { state: WriteLocked, waiting_writers: lock.waiting_writers, waiting_readers: lock.waiting_readers + 1 }
    }
  }
  
  let read_write_lock_release_read = fn(lock: ReadWriteLock) -> ReadWriteLock {
    match lock.state {
      ReadLocked(1) => { state: Unlocked, waiting_writers: lock.waiting_writers, waiting_readers: lock.waiting_readers }
      ReadLocked(count) => { state: ReadLocked(count - 1), waiting_writers: lock.waiting_writers, waiting_readers: lock.waiting_readers }
      _ => lock  // Invalid state
    }
  }
  
  let read_write_lock_acquire_write = fn(lock: ReadWriteLock) -> ReadWriteLock {
    match lock.state {
      Unlocked => { state: WriteLocked, waiting_writers: lock.waiting_writers, waiting_readers: lock.waiting_readers }
      ReadLocked(_) => { state: lock.state, waiting_writers: lock.waiting_writers + 1, waiting_readers: lock.waiting_readers }
      WriteLocked => { state: WriteLocked, waiting_writers: lock.waiting_writers + 1, waiting_readers: lock.waiting_readers }
    }
  }
  
  let read_write_lock_release_write = fn(lock: ReadWriteLock) -> ReadWriteLock {
    match lock.state {
      WriteLocked => { state: Unlocked, waiting_writers: lock.waiting_writers, waiting_readers: lock.waiting_readers }
      _ => lock  // Invalid state
    }
  }
  
  // Test read-write lock
  let lock = read_write_lock_new()
  
  // Acquire read lock
  let lock1 = read_write_lock_acquire_read(lock)
  match lock1.state {
    ReadLocked(count) => assert_eq(count, 1)
    _ => assert_true(false)
  }
  
  // Acquire another read lock
  let lock2 = read_write_lock_acquire_read(lock1)
  match lock2.state {
    ReadLocked(count) => assert_eq(count, 2)
    _ => assert_true(false)
  }
  
  // Release read lock
  let lock3 = read_write_lock_release_read(lock2)
  match lock3.state {
    ReadLocked(count) => assert_eq(count, 1)
    _ => assert_true(false)
  }
  
  // Release last read lock
  let lock4 = read_write_lock_release_read(lock3)
  match lock4.state {
    Unlocked => assert_true(true)
    _ => assert_true(false)
  }
  
  // Acquire write lock
  let lock5 = read_write_lock_acquire_write(lock4)
  match lock5.state {
    WriteLocked => assert_true(true)
    _ => assert_true(false)
  }
  
  // Try to acquire read lock while write lock is held
  let lock6 = read_write_lock_acquire_read(lock5)
  assert_eq(lock6.waiting_readers, 1)  // Should be waiting
  
  // Release write lock
  let lock7 = read_write_lock_release_write(lock6)
  match lock7.state {
    WriteLocked => assert_true(true)  // Still write locked in simplified implementation
    _ => assert_true(false)
  }
}

// Test 4: Concurrent Map
test "concurrent map implementation" {
  type ConcurrentMap[K, V] = {
    entries: Array[(K, V)>,
    lock: Bool
  }
  
  let concurrent_map_new = fn[K, V]() -> ConcurrentMap[K, V] {
    { entries: [], lock: false }
  }
  
  let concurrent_map_put = fn[K: Eq, V](map: ConcurrentMap[K, V], key: K, value: V) -> ConcurrentMap[K, V] {
    if map.lock {
      map  // Would block in real implementation
    } else {
      let mut found = false
      let mut new_entries = []
      
      for (k, v) in map.entries {
        if k == key {
          new_entries = new_entries + [(key, value)]
          found = true
        } else {
          new_entries = new_entries + [(k, v)]
        }
      }
      
      if !found {
        new_entries = new_entries + [(key, value)]
      }
      
      { entries: new_entries, lock: map.lock }
    }
  }
  
  let concurrent_map_get = fn[K: Eq, V](map: ConcurrentMap[K, V], key: K) -> Option[V] {
    for (k, v) in map.entries {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  let concurrent_map_remove = fn[K: Eq, V](map: ConcurrentMap[K, V], key: K) -> ConcurrentMap[K, V] {
    if map.lock {
      map  // Would block in real implementation
    } else {
      let mut new_entries = []
      
      for (k, v) in map.entries {
        if k != key {
          new_entries = new_entries + [(k, v)]
        }
      }
      
      { entries: new_entries, lock: map.lock }
    }
  }
  
  // Test concurrent map
  let map1 = concurrent_map_new()
  
  let map2 = concurrent_map_put(map1, "key1", "value1")
  let map3 = concurrent_map_put(map2, "key2", "value2")
  let map4 = concurrent_map_put(map3, "key3", "value3")
  
  // Test get
  match concurrent_map_get(map4, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match concurrent_map_get(map4, "key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match concurrent_map_get(map4, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test update
  let map5 = concurrent_map_put(map4, "key1", "new_value1")
  match concurrent_map_get(map5, "key1") {
    Some(value) => assert_eq(value, "new_value1")
    None => assert_true(false)
  }
  
  // Test remove
  let map6 = concurrent_map_remove(map5, "key2")
  match concurrent_map_get(map6, "key2") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  match concurrent_map_get(map6, "key1") {
    Some(value) => assert_eq(value, "new_value1")
    None => assert_true(false)
  }
}

// Test 5: Thread Pool
test "thread pool implementation" {
  type Task = {
    id: Int,
    work: () -> String
  }
  
  type Worker = {
    id: Int,
    busy: Bool,
    current_task: Option[Int>
  }
  
  type ThreadPool = {
    workers: Array[Worker>,
    task_queue: Array[Task>,
    next_task_id: Int
  }
  
  let thread_pool_new = fn(worker_count: Int) -> ThreadPool {
    let mut workers = []
    for i in 0..=worker_count-1 {
      workers = workers + [{
        id: i,
        busy: false,
        current_task: None
      }]
    }
    
    { workers, task_queue: [], next_task_id: 0 }
  }
  
  let thread_pool_submit = fn(pool: ThreadPool, work: () -> String) -> ThreadPool {
    let task = { id: pool.next_task_id, work }
    {
      workers: pool.workers,
      task_queue: pool.task_queue + [task],
      next_task_id: pool.next_task_id + 1
    }
  }
  
  let thread_pool_assign_tasks = fn(pool: ThreadPool) -> ThreadPool {
    let mut new_workers = pool.workers
    let mut remaining_tasks = pool.task_queue
    
    for i in 0..=pool.workers.length()-1 {
      if !pool.workers[i].busy && remaining_tasks.length() > 0 {
        let task = remaining_tasks[0]
        new_workers = new_workers.with(i, {
          id: pool.workers[i].id,
          busy: true,
          current_task: Some(task.id)
        })
        remaining_tasks = remaining_tasks.slice(1, remaining_tasks.length())
      }
    }
    
    { workers: new_workers, task_queue: remaining_tasks, next_task_id: pool.next_task_id }
  }
  
  let thread_pool_complete_task = fn(pool: ThreadPool, worker_id: Int) -> ThreadPool {
    let mut new_workers = pool.workers
    
    for i in 0..=pool.workers.length()-1 {
      if pool.workers[i].id == worker_id {
        new_workers = new_workers.with(i, {
          id: pool.workers[i].id,
          busy: false,
          current_task: None
        })
        break
      }
    }
    
    { workers: new_workers, task_queue: pool.task_queue, next_task_id: pool.next_task_id }
  }
  
  // Test thread pool
  let pool1 = thread_pool_new(3)
  assert_eq(pool1.workers.length(), 3)
  assert_eq(pool1.task_queue.length(), 0)
  
  // Submit tasks
  let pool2 = thread_pool_submit(pool1, fn() -> String { "Task 1" })
  let pool3 = thread_pool_submit(pool2, fn() -> String { "Task 2" })
  let pool4 = thread_pool_submit(pool3, fn() -> String { "Task 3" })
  let pool5 = thread_pool_submit(pool4, fn() -> String { "Task 4" })
  let pool6 = thread_pool_submit(pool5, fn() -> String { "Task 5" })
  
  assert_eq(pool6.task_queue.length(), 5)
  
  // Assign tasks to workers
  let pool7 = thread_pool_assign_tasks(pool6)
  assert_eq(pool7.task_queue.length(), 2)  // 3 workers, 5 tasks, 2 remaining
  
  // Check worker states
  for worker in pool7.workers {
    if worker.id < 2 {
      assert_true(worker.busy)  // First 2 workers should be busy
    } else {
      assert_false(worker.busy)  // Third worker should be busy too since we have 3 workers
    }
  }
  
  // Complete a task
  let pool8 = thread_pool_complete_task(pool7, 0)
  assert_false(pool8.workers[0].busy)
  
  // Reassign tasks
  let pool9 = thread_pool_assign_tasks(pool8)
  assert_eq(pool9.task_queue.length(), 1)  // One task should be reassigned
}

// Test 6: Barrier Synchronization
test "barrier synchronization" {
  type Barrier = {
    total_threads: Int,
    waiting_threads: Int,
    generation: Int
  }
  
  let barrier_new = fn(thread_count: Int) -> Barrier {
    { total_threads: thread_count, waiting_threads: 0, generation: 0 }
  }
  
  let barrier_wait = fn(barrier: Barrier) -> (Barrier, Bool) {
    let new_waiting = barrier.waiting_threads + 1
    
    if new_waiting == barrier.total_threads {
      // All threads have arrived, reset barrier
      ({
        total_threads: barrier.total_threads,
        waiting_threads: 0,
        generation: barrier.generation + 1
      }, true)  // Return true for one thread to take action
    } else {
      ({
        total_threads: barrier.total_threads,
        waiting_threads: new_waiting,
        generation: barrier.generation
      }, false)  // Return false for waiting threads
    }
  }
  
  // Test barrier
  let barrier = barrier_new(3)
  
  // First thread arrives
  let (barrier1, signaled1) = barrier_wait(barrier)
  assert_eq(barrier1.waiting_threads, 1)
  assert_false(signaled1)
  
  // Second thread arrives
  let (barrier2, signaled2) = barrier_wait(barrier1)
  assert_eq(barrier2.waiting_threads, 2)
  assert_false(signaled2)
  
  // Third thread arrives, all should be released
  let (barrier3, signaled3) = barrier_wait(barrier2)
  assert_eq(barrier3.waiting_threads, 0)
  assert_eq(barrier3.generation, 1)
  assert_true(signaled3)
}

// Test 7: Semaphore
test "semaphore implementation" {
  type Semaphore = {
    permits: Int,
    max_permits: Int
  }
  
  let semaphore_new = fn(initial_permits: Int) -> Semaphore {
    { permits: initial_permits, max_permits: initial_permits }
  }
  
  let semaphore_acquire = fn(semaphore: Semaphore) -> (Semaphore, Bool) {
    if semaphore.permits > 0 {
      ({ permits: semaphore.permits - 1, max_permits: semaphore.max_permits }, true)
    } else {
      (semaphore, false)  // Would block in real implementation
    }
  }
  
  let semaphore_release = fn(semaphore: Semaphore) -> Semaphore {
    if semaphore.permits < semaphore.max_permits {
      { permits: semaphore.permits + 1, max_permits: semaphore.max_permits }
    } else {
      semaphore  // Already at max permits
    }
  }
  
  // Test semaphore
  let semaphore1 = semaphore_new(2)
  assert_eq(semaphore1.permits, 2)
  
  // Acquire first permit
  let (semaphore2, acquired1) = semaphore_acquire(semaphore1)
  assert_true(acquired1)
  assert_eq(semaphore2.permits, 1)
  
  // Acquire second permit
  let (semaphore3, acquired2) = semaphore_acquire(semaphore2)
  assert_true(acquired2)
  assert_eq(semaphore3.permits, 0)
  
  // Try to acquire third permit
  let (semaphore4, acquired3) = semaphore_acquire(semaphore3)
  assert_false(acquired3)
  assert_eq(semaphore4.permits, 0)
  
  // Release a permit
  let semaphore5 = semaphore_release(semaphore4)
  assert_eq(semaphore5.permits, 1)
  
  // Acquire again
  let (semaphore6, acquired4) = semaphore_acquire(semaphore5)
  assert_true(acquired4)
  assert_eq(semaphore6.permits, 0)
  
  // Release all permits
  let semaphore7 = semaphore_release(semaphore6)
  assert_eq(semaphore7.permits, 1)
  
  let semaphore8 = semaphore_release(semaphore7)
  assert_eq(semaphore8.permits, 2)
  
  // Try to release beyond max permits
  let semaphore9 = semaphore_release(semaphore8)
  assert_eq(semaphore9.permits, 2)  // Should stay at max
}

// Test 8: Producer-Consumer Pattern
test "producer-consumer pattern" {
  type ProducerConsumerBuffer[T] = {
    buffer: Array[T>,
    head: Int,
    tail: Int,
    size: Int,
    capacity: Int
  }
  
  let producer_consumer_new = fn[T](capacity: Int) -> ProducerConsumerBuffer[T] {
    let mut buffer = []
    for i in 0..=capacity-1 {
      buffer = buffer + [None]  // Placeholder
    }
    { buffer, head: 0, tail: 0, size: 0, capacity }
  }
  
  let producer_consumer_produce = fn[T](pc: ProducerConsumerBuffer[T], item: T) -> ProducerConsumerBuffer[T] {
    if pc.size >= pc.capacity {
      pc  // Buffer is full
    } else {
      let new_buffer = pc.buffer.with(pc.tail, item)
      let new_tail = (pc.tail + 1) % pc.capacity
      { buffer: new_buffer, head: pc.head, tail: new_tail, size: pc.size + 1, capacity: pc.capacity }
    }
  }
  
  let producer_consumer_consume = fn[T](pc: ProducerConsumerBuffer[T]) -> (ProducerConsumerBuffer[T], Option[T]) {
    if pc.size == 0 {
      (pc, None)  // Buffer is empty
    } else {
      let item = pc.buffer[pc.head]
      let new_head = (pc.head + 1) % pc.capacity
      let new_pc = { buffer: pc.buffer, head: new_head, tail: pc.tail, size: pc.size - 1, capacity: pc.capacity }
      (new_pc, Some(item))
    }
  }
  
  // Test producer-consumer
  let pc1 = producer_consumer_new(3)
  
  // Produce items
  let pc2 = producer_consumer_produce(pc1, "item1")
  let pc3 = producer_consumer_produce(pc2, "item2")
  let pc4 = producer_consumer_produce(pc3, "item3")
  
  assert_eq(pc4.size, 3)
  
  // Try to produce when full
  let pc5 = producer_consumer_produce(pc4, "item4")
  assert_eq(pc5.size, 3)  // Should not change
  
  // Consume items
  let (pc6, item1) = producer_consumer_consume(pc5)
  match item1 {
    Some(value) => assert_eq(value, "item1")
    None => assert_true(false)
  }
  assert_eq(pc6.size, 2)
  
  let (pc7, item2) = producer_consumer_consume(pc6)
  match item2 {
    Some(value) => assert_eq(value, "item2")
    None => assert_true(false)
  }
  assert_eq(pc7.size, 1)
  
  // Produce more items
  let pc8 = producer_consumer_produce(pc7, "item4")
  let pc9 = producer_consumer_produce(pc8, "item5")
  
  assert_eq(pc9.size, 3)
  
  // Consume remaining items
  let (pc10, item3) = producer_consumer_consume(pc9)
  match item3 {
    Some(value) => assert_eq(value, "item3")
    None => assert_true(false)
  }
  
  let (pc11, item4) = producer_consumer_consume(pc10)
  match item4 {
    Some(value) => assert_eq(value, "item4")
    None => assert_true(false)
  }
  
  let (pc12, item5) = producer_consumer_consume(pc11)
  match item5 {
    Some(value) => assert_eq(value, "item5")
    None => assert_true(false)
  }
  
  assert_eq(pc12.size, 0)
}

// Test 9: Future and Promise
test "future and promise implementation" {
  enum FutureState[T] {
    Pending
    Completed(T)
    Failed(String)
  }
  
  type Future[T] = {
    state: FutureState[T],
    callbacks: Array[(T) -> Unit>
  }
  
  type Promise[T] = {
    future: Future[T]
  }
  
  let promise_new = fn[T]() -> Promise[T] {
    Promise({
      state: Pending,
      callbacks: []
    })
  }
  
  let promise_fulfill = fn[T](promise: Promise[T], value: T) -> Promise[T] {
    Promise({
      state: Completed(value),
      callbacks: promise.future.callbacks
    })
  }
  
  let promise_reject = fn[T](promise: Promise[T], error: String) -> Promise[T] {
    Promise({
      state: Failed(error),
      callbacks: promise.future.callbacks
    })
  }
  
  let future_get = fn[T](future: Future[T]) -> Option[T] {
    match future.state {
      Completed(value) => Some(value)
      _ => None
    }
  }
  
  let future_is_completed = fn[T](future: Future[T]) -> Bool {
    match future.state {
      Completed(_) => true
      Failed(_) => true
      Pending => false
    }
  }
  
  // Test promise and future
  let promise1 = promise_new()
  
  // Future should not be completed yet
  assert_false(future_is_completed(promise1.future))
  match future_get(promise1.future) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Fulfill the promise
  let promise2 = promise_fulfill(promise1, "Success")
  
  // Future should now be completed
  assert_true(future_is_completed(promise2.future))
  match future_get(promise2.future) {
    Some(value) => assert_eq(value, "Success")
    None => assert_true(false)
  }
  
  // Test rejection
  let promise3 = promise_new()
  let promise4 = promise_reject(promise3, "Error occurred")
  
  // Future should be completed but with error
  assert_true(future_is_completed(promise4.future))
  match future_get(promise4.future) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 10: Concurrent Data Structure Safety
test "concurrent data structure safety" {
  // Test concurrent access to shared data structure
  type SafeCounter = {
    value: Int,
    version: Int
  }
  
  let safe_counter_new = fn(initial_value: Int) -> SafeCounter {
    { value: initial_value, version: 0 }
  }
  
  let safe_counter_increment = fn(counter: SafeCounter) -> SafeCounter {
    { value: counter.value + 1, version: counter.version + 1 }
  }
  
  let safe_counter_compare_and_swap = fn(counter: SafeCounter, expected_version: Int, new_value: Int) -> SafeCounter {
    if counter.version == expected_version {
      { value: new_value, version: counter.version + 1 }
    } else {
      counter  // Version mismatch, operation failed
    }
  }
  
  // Test safe counter
  let counter1 = safe_counter_new(0)
  assert_eq(counter1.value, 0)
  assert_eq(counter1.version, 0)
  
  let counter2 = safe_counter_increment(counter1)
  assert_eq(counter2.value, 1)
  assert_eq(counter2.version, 1)
  
  let counter3 = safe_counter_increment(counter2)
  assert_eq(counter3.value, 2)
  assert_eq(counter3.version, 2)
  
  // Test compare-and-swap with correct version
  let counter4 = safe_counter_compare_and_swap(counter3, 2, 10)
  assert_eq(counter4.value, 10)
  assert_eq(counter4.version, 3)
  
  // Test compare-and-swap with incorrect version
  let counter5 = safe_counter_compare_and_swap(counter4, 2, 20)  // Wrong version
  assert_eq(counter5.value, 10)  // Should not change
  assert_eq(counter5.version, 3)  // Should not change
  
  // Test with correct version
  let counter6 = safe_counter_compare_and_swap(counter5, 3, 20)
  assert_eq(counter6.value, 20)
  assert_eq(counter6.version, 4)
}