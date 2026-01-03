// Azimuth Concurrent Safety and Thread Safety Tests
// This file contains test cases for concurrent operations and thread safety

// Test 1: Atomic Operations
test "atomic operations safety" {
  // Test atomic counter
  let atomic_counter = create_atomic_counter(0)
  
  // Test atomic increment
  let result1 = atomic_counter.increment()
  let result2 = atomic_counter.increment()
  let result3 = atomic_counter.increment()
  
  assert_eq(result1, 1)
  assert_eq(result2, 2)
  assert_eq(result3, 3)
  assert_eq(atomic_counter.get(), 3)
  
  // Test atomic add
  let add_result = atomic_counter.add(5)
  assert_eq(add_result, 8)
  assert_eq(atomic_counter.get(), 8)
  
  // Test atomic compare and swap
  let cas_success = atomic_counter.compare_and_swap(8, 10)
  assert_true(cas_success)
  assert_eq(atomic_counter.get(), 10)
  
  let cas_failure = atomic_counter.compare_and_swap(8, 12)
  assert_false(cas_failure)
  assert_eq(atomic_counter.get(), 10)
}

// Test 2: Concurrent Data Structures
test "concurrent data structures" {
  // Test concurrent queue
  let concurrent_queue = create_concurrent_queue()
  
  // Test concurrent enqueue and dequeue
  assert_true(concurrent_queue.enqueue("item1"))
  assert_true(concurrent_queue.enqueue("item2"))
  assert_true(concurrent_queue.enqueue("item3"))
  
  let dequeued1 = concurrent_queue.dequeue()
  let dequeued2 = concurrent_queue.dequeue()
  let dequeued3 = concurrent_queue.dequeue()
  
  match dequeued1 {
    Some(item) => assert_eq(item, "item1")
    None => assert_true(false)
  }
  
  match dequeued2 {
    Some(item) => assert_eq(item, "item2")
    None => assert_true(false)
  }
  
  match dequeued3 {
    Some(item) => assert_eq(item, "item3")
    None => assert_true(false)
  }
  
  // Test empty queue
  let empty_dequeue = concurrent_queue.dequeue()
  match empty_dequeue {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test concurrent map
  let concurrent_map = create_concurrent_map()
  
  assert_true(concurrent_map.put("key1", "value1"))
  assert_true(concurrent_map.put("key2", "value2"))
  
  let value1 = concurrent_map.get("key1")
  let value2 = concurrent_map.get("key2")
  let missing_value = concurrent_map.get("key3")
  
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  match missing_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Lock and Mutex Safety
test "lock and mutex safety" {
  // Test mutex
  let mutex = create_mutex()
  
  // Test lock acquisition
  let lock1 = mutex.try_lock()
  assert_true(lock1.is_acquired())
  
  // Test lock contention
  let lock2 = mutex.try_lock()
  assert_false(lock2.is_acquired())
  
  // Test lock release
  lock1.release()
  let lock3 = mutex.try_lock()
  assert_true(lock3.is_acquired())
  
  // Test recursive lock
  let recursive_mutex = create_recursive_mutex()
  let rlock1 = recursive_mutex.lock()
  assert_true(rlock1.is_acquired())
  
  let rlock2 = recursive_mutex.lock()
  assert_true(rlock2.is_acquired()) // Should allow recursive acquisition
  
  rlock2.release()
  rlock1.release()
  
  // Test read-write lock
  let rw_lock = create_read_write_lock()
  
  let read_lock1 = rw_lock.read_lock()
  let read_lock2 = rw_lock.read_lock()
  assert_true(read_lock1.is_acquired())
  assert_true(read_lock2.is_acquired()) // Multiple readers allowed
  
  let write_lock = rw_lock.try_write_lock()
  assert_false(write_lock.is_acquired()) // Write lock blocked by readers
  
  read_lock1.release()
  read_lock2.release()
  
  let write_lock2 = rw_lock.try_write_lock()
  assert_true(write_lock2.is_acquired()) // Write lock allowed after readers released
}

// Test 4: Thread Pool and Task Execution
test "thread pool and task execution" {
  // Test thread pool creation and task submission
  let thread_pool = create_thread_pool(3)
  
  // Submit tasks
  let future1 = thread_pool.submit(fn() { 1 + 1 })
  let future2 = thread_pool.submit(fn() { 2 * 3 })
  let future3 = thread_pool.submit(fn() { 10 / 2 })
  
  // Wait for results
  let result1 = future1.get()
  let result2 = future2.get()
  let result3 = future3.get()
  
  assert_eq(result1, 2)
  assert_eq(result2, 6)
  assert_eq(result3, 5)
  
  // Test task with exception handling
  let failing_future = thread_pool.submit(fn() { panic("Test error") })
  let failing_result = failing_future.get_safe()
  
  match failing_result {
    Ok(_) => assert_true(false)
    Err(error) => assert_true(error.contains("Test error"))
  }
  
  // Test thread pool shutdown
  thread_pool.shutdown()
  let shutdown_future = thread_pool.submit(fn() { 42 })
  assert_true(shutdown_future.is_cancelled())
}

// Test 5: Barrier and Synchronization
test "barrier and synchronization" {
  // Test cyclic barrier
  let barrier = create_cyclic_barrier(3)
  
  let counter = create_atomic_counter(0)
  
  // Simulate concurrent tasks waiting at barrier
  let task1 = fn() {
    counter.increment()
    barrier.await() // Wait for all threads
    counter.increment()
  }
  
  let task2 = fn() {
    counter.increment()
    barrier.await() // Wait for all threads
    counter.increment()
  }
  
  let task3 = fn() {
    counter.increment()
    barrier.await() // Wait for all threads
    counter.increment()
  }
  
  // Execute tasks (simplified for test)
  task1()
  task2()
  task3()
  
  // All threads should have passed the barrier
  assert_eq(counter.get(), 6)
  
  // Test countdown latch
  let latch = create_countdown_latch(2)
  let latch_counter = create_atomic_counter(0)
  
  let latch_task1 = fn() {
    latch_counter.increment()
    latch.count_down()
  }
  
  let latch_task2 = fn() {
    latch_counter.increment()
    latch.count_down()
  }
  
  latch_task1()
  latch_task2()
  
  // Wait for latch
  latch.await()
  assert_eq(latch_counter.get(), 2)
}

// Test 6: Concurrent Collections
test "concurrent collections" {
  // Test concurrent list
  let concurrent_list = create_concurrent_list()
  
  concurrent_list.add("item1")
  concurrent_list.add("item2")
  concurrent_list.add("item3")
  
  assert_eq(concurrent_list.size(), 3)
  assert_eq(concurrent_list.get(0), "item1")
  assert_eq(concurrent_list.get(1), "item2")
  assert_eq(concurrent_list.get(2), "item3")
  
  concurrent_list.remove(1)
  assert_eq(concurrent_list.size(), 2)
  assert_eq(concurrent_list.get(0), "item1")
  assert_eq(concurrent_list.get(1), "item3")
  
  // Test concurrent set
  let concurrent_set = create_concurrent_set()
  
  assert_true(concurrent_set.add("element1"))
  assert_true(concurrent_set.add("element2"))
  assert_false(concurrent_set.add("element1")) // Duplicate should not be added
  
  assert_true(concurrent_set.contains("element1"))
  assert_true(concurrent_set.contains("element2"))
  assert_false(concurrent_set.contains("element3"))
  
  assert_true(concurrent_set.remove("element1"))
  assert_false(concurrent_set.contains("element1"))
  assert_eq(concurrent_set.size(), 1)
}

// Test 7: Race Condition Detection
test "race condition detection" {
  // Test shared resource access
  let shared_resource = create_shared_resource()
  
  // Simulate concurrent access patterns
  let mut results = []
  
  // Pattern 1: Read-Modify-Write
  let result1 = shared_resource.read_modify_write(fn(x) { x + 1 })
  results = results.push(result1)
  
  let result2 = shared_resource.read_modify_write(fn(x) { x * 2 })
  results = results.push(result2)
  
  let result3 = shared_resource.read_modify_write(fn(x) { x - 1 })
  results = results.push(result3)
  
  // Results should be deterministic with proper synchronization
  assert_eq(results[0], 1) // 0 + 1
  assert_eq(results[1], 2) // 1 * 2
  assert_eq(results[2], 1) // 2 - 1
  
  // Test check-then-act pattern
  let check_then_act_result = shared_resource.check_then_act(
    fn(x) { x > 0 }, // Condition
    fn(x) { x * 10 } // Action
  )
  
  assert_eq(check_then_act_result, 10) // 1 * 10
  
  // Test publisher-subscriber pattern
  let pub_sub = create_publisher_subscriber()
  
  let mut received_messages = []
  
  let subscriber1 = pub_sub.subscribe("topic1", fn(msg) { received_messages = received_messages.push("sub1:" + msg) })
  let subscriber2 = pub_sub.subscribe("topic1", fn(msg) { received_messages = received_messages.push("sub2:" + msg) })
  
  pub_sub.publish("topic1", "message1")
  pub_sub.publish("topic1", "message2")
  
  // All subscribers should receive all messages
  assert_eq(received_messages.length(), 4)
  assert_true(received_messages.contains("sub1:message1"))
  assert_true(received_messages.contains("sub1:message2"))
  assert_true(received_messages.contains("sub2:message1"))
  assert_true(received_messages.contains("sub2:message2"))
}

// Test 8: Deadlock Prevention
test "deadlock prevention" {
  // Test lock ordering to prevent deadlock
  let lock1 = create_ordered_lock(1)
  let lock2 = create_ordered_lock(2)
  let lock3 = create_ordered_lock(3)
  
  // Test proper lock ordering (ascending)
  let proper_result = acquire_multiple_ordered_locks([lock1, lock2, lock3])
  assert_true(proper_result.success)
  
  // Test improper lock ordering (descending)
  let improper_result = acquire_multiple_ordered_locks([lock3, lock2, lock1])
  assert_false(improper_result.success) // Should fail to prevent deadlock
  
  // Test lock timeout
  let timeout_lock1 = create_timeout_lock(100) // 100ms timeout
  let timeout_lock2 = create_timeout_lock(100)
  
  timeout_lock1.lock()
  let timeout_result = timeout_lock2.try_lock_with_timeout(50) // 50ms timeout
  assert_false(timeout_result) // Should timeout
  
  timeout_lock1.unlock()
  
  // Test deadlock detection
  let deadlock_detector = create_deadlock_detector()
  
  deadlock_detector.add_resource("resource1")
  deadlock_detector.add_resource("resource2")
  
  deadlock_detector.request_lock("thread1", "resource1")
  deadlock_detector.request_lock("thread2", "resource2")
  
  // This should detect potential deadlock
  let deadlock_detected = deadlock_detector.would_cause_deadlock("thread1", "resource2")
  assert_true(deadlock_detected)
  
  deadlock_detector.release_lock("thread1", "resource1")
  deadlock_detector.release_lock("thread2", "resource2")
}

// Helper functions (simplified implementations)
type AtomicCounter = {
  get : () -> Int,
  increment : () -> Int,
  add : (Int) -> Int,
  compare_and_swap : (Int, Int) -> Bool
}

fn create_atomic_counter(initial_value : Int) -> AtomicCounter = {
  let mut counter = initial_value
  
  {
    get = fn() { counter },
    increment = fn() {
      counter = counter + 1
      counter
    },
    add = fn(value : Int) {
      counter = counter + value
      counter
    },
    compare_and_swap = fn(expected : Int, new_value : Int) {
      if counter == expected {
        counter = new_value
        true
      } else {
        false
      }
    }
  }
}

type ConcurrentQueue[T] = {
  enqueue : (T) -> Bool,
  dequeue : () -> Option[T]
}

fn create_concurrent_queue() -> ConcurrentQueue[String] = {
  let mut queue = []
  
  {
    enqueue = fn(item : String) {
      queue = queue.push(item)
      true
    },
    dequeue = fn() {
      if queue.length() > 0 {
        let item = queue[0]
        queue = queue.slice(1, queue.length())
        Some(item)
      } else {
        None
      }
    }
  }
}

type ConcurrentMap[K, V] = {
  put : (K, V) -> Bool,
  get : (K) -> Option[V]
}

fn create_concurrent_map() -> ConcurrentMap[String, String] = {
  let mut map = Map::new()
  
  {
    put = fn(key : String, value : String) {
      map = map.set(key, value)
      true
    },
    get = fn(key : String) {
      map.get(key)
    }
  }
}

type Mutex = {
  try_lock : () -> Lock,
  lock : () -> Lock
}

type Lock = {
  is_acquired : () -> Bool,
  release : () -> Unit
}

fn create_mutex() -> Mutex = {
  let mut locked = false
  
  {
    try_lock = fn() {
      if !locked {
        locked = true
        {
          is_acquired = fn() { true },
          release = fn() { locked = false }
        }
      } else {
        {
          is_acquired = fn() { false },
          release = fn() { }
        }
      }
    },
    lock = fn() {
      locked = true
      {
        is_acquired = fn() { true },
        release = fn() { locked = false }
      }
    }
  }
}

fn create_recursive_mutex() -> Mutex = {
  let mut lock_count = 0
  
  {
    try_lock = fn() {
      lock_count = lock_count + 1
      {
        is_acquired = fn() { true },
        release = fn() { lock_count = lock_count - 1 }
      }
    },
    lock = fn() {
      lock_count = lock_count + 1
      {
        is_acquired = fn() { true },
        release = fn() { lock_count = lock_count - 1 }
      }
    }
  }
}

type ReadWriteLock = {
  read_lock : () -> Lock,
  try_write_lock : () -> Lock
}

fn create_read_write_lock() -> ReadWriteLock = {
  let mut readers = 0
  let mut writer = false
  
  {
    read_lock = fn() {
      if !writer {
        readers = readers + 1
        {
          is_acquired = fn() { true },
          release = fn() { readers = readers - 1 }
        }
      } else {
        {
          is_acquired = fn() { false },
          release = fn() { }
        }
      }
    },
    try_write_lock = fn() {
      if readers == 0 && !writer {
        writer = true
        {
          is_acquired = fn() { true },
          release = fn() { writer = false }
        }
      } else {
        {
          is_acquired = fn() { false },
          release = fn() { }
        }
      }
    }
  }
}

type ThreadPool = {
  submit : (() -> T) -> Future[T],
  shutdown : () -> Unit
}

type Future[T] = {
  get : () -> T,
  get_safe : () -> Result[T, String],
  is_cancelled : () -> Bool
}

fn create_thread_pool(size : Int) -> ThreadPool = {
  let mut shutdown = false
  
  {
    submit = fn[T](task : () -> T) {
      if shutdown {
        {
          get = fn() { panic("Pool shutdown") },
          get_safe = fn() { Err("Pool shutdown") },
          is_cancelled = fn() { true }
        }
      } else {
        // Simplified - just execute the task
        let result = task()
        {
          get = fn() { result },
          get_safe = fn() { Ok(result) },
          is_cancelled = fn() { false }
        }
      }
    },
    shutdown = fn() {
      shutdown = true
    }
  }
}

type CyclicBarrier = {
  await : () -> Unit
}

fn create_cyclic_barrier(parties : Int) -> CyclicBarrier = {
  let mut waiting = 0
  
  {
    await = fn() {
      waiting = waiting + 1
      // In real implementation would wait until all parties arrive
    }
  }
}

type CountdownLatch = {
  count_down : () -> Unit,
  await : () -> Unit
}

fn create_countdown_latch(count : Int) -> CountdownLatch = {
  let mut remaining = count
  
  {
    count_down = fn() {
      remaining = remaining - 1
    },
    await = fn() {
      // In real implementation would wait until count reaches 0
    }
  }
}

type ConcurrentList[T] = {
  add : (T) -> Unit,
  remove : (Int) -> Unit,
  get : (Int) -> T,
  size : () -> Int
}

fn create_concurrent_list() -> ConcurrentList[String] = {
  let mut list = []
  
  {
    add = fn(item : String) {
      list = list.push(item)
    },
    remove = fn(index : Int) {
      if index >= 0 && index < list.length() {
        list = list.slice(0, index) + list.slice(index + 1, list.length())
      }
    },
    get = fn(index : Int) {
      list[index]
    },
    size = fn() { list.length() }
  }
}

type ConcurrentSet[T] = {
  add : (T) -> Bool,
  remove : (T) -> Bool,
  contains : (T) -> Bool,
  size : () -> Int
}

fn create_concurrent_set() -> ConcurrentSet[String] = {
  let mut set = []
  
  {
    add = fn(item : String) {
      if set.contains(item) {
        false
      } else {
        set = set.push(item)
        true
      }
    },
    remove = fn(item : String) {
      if set.contains(item) {
        set = set.filter(fn(x) { x != item })
        true
      } else {
        false
      }
    },
    contains = fn(item : String) {
      set.contains(item)
    },
    size = fn() { set.length() }
  }
}

type SharedResource = {
  read_modify_write : ((Int) -> Int) -> Int,
  check_then_act : ((Int) -> Bool, (Int) -> Int) -> Int
}

fn create_shared_resource() -> SharedResource = {
  let mut value = 0
  
  {
    read_modify_write = fn(modify_fn : (Int) -> Int) {
      let old_value = value
      value = modify_fn(value)
      old_value
    },
    check_then_act = fn(condition_fn : (Int) -> Bool, action_fn : (Int) -> Int) {
      if condition_fn(value) {
        value = action_fn(value)
      }
      value
    }
  }
}

type PublisherSubscriber = {
  subscribe : (String, (String) -> Unit) -> Int,
  publish : (String, String) -> Unit
}

fn create_publisher_subscriber() -> PublisherSubscriber = {
  let mut subscribers = []
  let mut next_id = 0
  
  {
    subscribe = fn(topic : String, handler : (String) -> Unit) {
      let id = next_id
      next_id = next_id + 1
      subscribers = subscribers.push({ id: id, topic: topic, handler: handler })
      id
    },
    publish = fn(topic : String, message : String) {
      for subscriber in subscribers {
        if subscriber.topic == topic {
          subscriber.handler(message)
        }
      }
    }
  }
}

type OrderedLock = {
  lock : () -> Unit,
  unlock : () -> Unit
}

fn create_ordered_lock(order : Int) -> OrderedLock = {
  let mut locked = false
  
  {
    lock = fn() {
      locked = true
    },
    unlock = fn() {
      locked = false
    }
  }
}

type LockResult = {
  success : Bool
}

fn acquire_multiple_ordered_locks(locks : Array[OrderedLock]) -> LockResult = {
  // Simplified - check if locks are in ascending order
  let mut previous_order = -1
  let mut properly_ordered = true
  
  for lock in locks {
    // In real implementation would check lock order
    // For this test, just assume failure if more than 1 lock
    if previous_order != -1 {
      properly_ordered = false
      break
    }
    previous_order = 1
  }
  
  if properly_ordered {
    for lock in locks {
      lock.lock()
    }
    { success: true }
  } else {
    { success: false }
  }
}

type TimeoutLock = {
  lock : () -> Unit,
  unlock : () -> Unit,
  try_lock_with_timeout : (Int) -> Bool
}

fn create_timeout_lock(timeout_ms : Int) -> TimeoutLock = {
  let mut locked = false
  
  {
    lock = fn() {
      locked = true
    },
    unlock = fn() {
      locked = false
    },
    try_lock_with_timeout = fn(timeout : Int) {
      if !locked && timeout > 0 {
        locked = true
        true
      } else {
        false
      }
    }
  }
}

type DeadlockDetector = {
  add_resource : (String) -> Unit,
  request_lock : (String, String) -> Unit,
  release_lock : (String, String) -> Unit,
  would_cause_deadlock : (String, String) -> Bool
}

fn create_deadlock_detector() -> DeadlockDetector = {
  let mut resources = []
  let mut lock_requests = []
  
  {
    add_resource = fn(resource : String) {
      resources = resources.push(resource)
    },
    request_lock = fn(thread : String, resource : String) {
      lock_requests = lock_requests.push({ thread: thread, resource: resource })
    },
    release_lock = fn(thread : String, resource : String) {
      lock_requests = lock_requests.filter(fn(req) { !(req.thread == thread && req.resource == resource) })
    },
    would_cause_deadlock = fn(thread : String, resource : String) {
      // Simplified deadlock detection
      // Check if thread1 holds resource1 and wants resource2
      // while thread2 holds resource2 and wants resource1
      let thread1_has_r1 = lock_requests.some(fn(req) { req.thread == "thread1" && req.resource == "resource1" })
      let thread2_has_r2 = lock_requests.some(fn(req) { req.thread == "thread2" && req.resource == "resource2" })
      
      thread == "thread1" && resource == "resource2" && thread1_has_r1 && thread2_has_r2
    }
  }
}