// Azimuth Telemetry System - Concurrent Safety Tests
// This file contains comprehensive test cases for concurrent safety functionality

// Test 1: Thread Safety Tests
test "thread safety operations" {
  // Test atomic operations
  let atomic_counter = AtomicInt::new(0)
  
  // Create multiple threads that increment the counter
  let threads = []
  for i in 0..10 {
    let thread = ThreadUtil::spawn(() -> {
      for j in 0..1000 {
        AtomicInt::increment(atomic_counter)
      }
    })
    ArrayUtil::push(threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    ThreadUtil::join(thread)
  }
  
  // Verify final value
  assert_eq(AtomicInt::get(atomic_counter), 10000)
  
  // Test atomic compare and swap
  let atomic_value = AtomicInt::new(5)
  
  // Successful CAS
  let cas_result1 = AtomicInt::compare_and_swap(atomic_value, 5, 10)
  assert_true(cas_result1)
  assert_eq(AtomicInt::get(atomic_value), 10)
  
  // Failed CAS
  let cas_result2 = AtomicInt::compare_and_swap(atomic_value, 5, 15)
  assert_false(cas_result2)
  assert_eq(AtomicInt::get(atomic_value), 10)
  
  // Test atomic reference
  let atomic_ref = AtomicRef::new("initial_value")
  
  // Update reference
  let old_ref = AtomicRef::swap(atomic_ref, "new_value")
  assert_eq(old_ref, "initial_value")
  assert_eq(AtomicRef::get(atomic_ref), "new_value")
  
  // Test atomic array operations
  let atomic_array = AtomicArray::new(100)
  
  // Create threads that update array elements
  let array_threads = []
  for i in 0..10 {
    let thread = ThreadUtil::spawn(() -> {
      for j in 0..100 {
        let index = (i * 10 + j) % 100
        AtomicArray::set(atomic_array, index, i * 1000 + j)
      }
    })
    ArrayUtil::push(array_threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in array_threads {
    ThreadUtil::join(thread)
  }
  
  // Verify all elements have been set
  for i in 0..100 {
    let value = AtomicArray::get(atomic_array, i)
    assert_true(value >= 0)
  }
}

// Test 2: Mutex and Lock Tests
test "mutex and lock operations" {
  // Test basic mutex
  let mutex = Mutex::new()
  let shared_counter = Ref::new(0)
  
  // Create threads that increment counter with mutex protection
  let mutex_threads = []
  for i in 0..10 {
    let thread = ThreadUtil::spawn(() -> {
      for j in 0..100 {
        Mutex::lock(mutex)
        let current = Ref::get(shared_counter)
        Ref::set(shared_counter, current + 1)
        Mutex::unlock(mutex)
      }
    })
    ArrayUtil::push(mutex_threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in mutex_threads {
    ThreadUtil::join(thread)
  }
  
  // Verify final value
  assert_eq(Ref::get(shared_counter), 1000)
  
  // Test reentrant mutex
  let reentrant_mutex = ReentrantMutex::new()
  let nested_counter = Ref::new(0)
  
  let reentrant_thread = ThreadUtil::spawn(() -> {
    ReentrantMutex::lock(reentrant_mutex)
    ReentrantMutex::lock(reentrant_mutex)  // Should not deadlock
    
    let current = Ref::get(nested_counter)
    Ref::set(nested_counter, current + 1)
    
    ReentrantMutex::unlock(reentrant_mutex)
    ReentrantMutex::unlock(reentrant_mutex)
  })
  
  ThreadUtil::join(reentrant_thread)
  assert_eq(Ref::get(nested_counter), 1)
  
  // Test try lock
  let try_mutex = Mutex::new()
  let try_result = Mutex::try_lock(try_mutex)
  assert_true(try_result)
  
  // Second try should fail
  let try_result2 = Mutex::try_lock(try_mutex)
  assert_false(try_result2)
  
  Mutex::unlock(try_mutex)
  
  // Test timed lock
  let timed_mutex = Mutex::new()
  
  // Lock in one thread
  let timed_thread = ThreadUtil::spawn(() -> {
    Mutex::lock(timed_mutex)
    TimeUtil::sleep(100)  // Hold lock for 100ms
    Mutex::unlock(timed_mutex)
  })
  
  TimeUtil::sleep(10)  // Give thread time to acquire lock
  
  // Try to lock with timeout in main thread
  let start_time = TimeUtil::current_time_millis()
  let timed_result = Mutex::try_lock_for(timed_mutex, 200)  // 200ms timeout
  let elapsed = TimeUtil::current_time_millis() - start_time
  
  assert_true(timed_result)
  assert_true(elapsed >= 90 && elapsed <= 200)  // Should wait at least 90ms but less than 200ms
  
  Mutex::unlock(timed_mutex)
  ThreadUtil::join(timed_thread)
  
  // Test read-write lock
  let rw_lock = RWLock::new()
  let shared_data = Ref::new(0)
  
  // Create reader threads
  let reader_threads = []
  for i in 0..5 {
    let thread = ThreadUtil::spawn(() -> {
      RWLock::read_lock(rw_lock)
      let value = Ref::get(shared_data)
      TimeUtil::sleep(50)  // Simulate reading
      RWLock::read_unlock(rw_lock)
      return value
    })
    ArrayUtil::push(reader_threads, thread)
  }
  
  // Create writer thread
  let writer_thread = ThreadUtil::spawn(() -> {
    RWLock::write_lock(rw_lock)
    let current = Ref::get(shared_data)
    Ref::set(shared_data, current + 100)
    TimeUtil::sleep(100)  // Simulate writing
    RWLock::write_unlock(rw_lock)
  })
  
  // Wait for all threads to complete
  let reader_results = []
  for thread in reader_threads {
    let result = ThreadUtil::join(thread)
    ArrayUtil::push(reader_results, result)
  }
  
  ThreadUtil::join(writer_thread)
  
  // Verify final value
  assert_eq(Ref::get(shared_data), 100)
  
  // All readers should see either 0 or 100, never intermediate states
  for result in reader_results {
    assert_true(result == 0 || result == 100)
  }
}

// Test 3: Concurrent Data Structure Tests
test "concurrent data structure operations" {
  // Test concurrent queue
  let concurrent_queue = ConcurrentQueue::new()
  
  // Producer threads
  let producer_threads = []
  for i in 0..3 {
    let thread = ThreadUtil::spawn(() -> {
      for j in 0..100 {
        ConcurrentQueue::enqueue(concurrent_queue, "item_" + (i * 100 + j).to_string())
      }
    })
    ArrayUtil::push(producer_threads, thread)
  }
  
  // Consumer threads
  let consumer_threads = []
  let consumed_items = Ref::new([])
  
  for i in 0..2 {
    let thread = ThreadUtil::spawn(() -> {
      let mut local_items = []
      for j in 0..150 {
        match ConcurrentQueue::dequeue(concurrent_queue) {
          Some(item) => ArrayUtil::push(local_items, item),
          None => TimeUtil::sleep(1)  // Wait if queue is empty
        }
      }
      
      // Merge local items with global
      Mutex::lock(Mutex::new())
      let global_items = Ref::get(consumed_items)
      Ref::set(consumed_items, ArrayUtil::concat(global_items, local_items))
      Mutex::unlock(Mutex::new())
    })
    ArrayUtil::push(consumer_threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in producer_threads {
    ThreadUtil::join(thread)
  }
  
  for thread in consumer_threads {
    ThreadUtil::join(thread)
  }
  
  // Verify all items were consumed
  let final_items = Ref::get(consumed_items)
  assert_eq(final_items.length(), 300)
  
  // Test concurrent map
  let concurrent_map = ConcurrentMap::new()
  
  // Writer threads
  let map_writer_threads = []
  for i in 0..5 {
    let thread = ThreadUtil::spawn(() -> {
      for j in 0..20 {
        let key = "key_" + (i * 20 + j).to_string()
        let value = "value_" + (i * 20 + j).to_string()
        ConcurrentMap::put(concurrent_map, key, value)
      }
    })
    ArrayUtil::push(map_writer_threads, thread)
  }
  
  // Reader threads
  let map_reader_threads = []
  for i in 0..3 {
    let thread = ThreadUtil::spawn(() -> {
      let mut local_size = 0
      for j in 0..50 {
        let keys = ConcurrentMap::keys(concurrent_map)
        local_size = keys.length()
        TimeUtil::sleep(1)
      }
      return local_size
    })
    ArrayUtil::push(map_reader_threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in map_writer_threads {
    ThreadUtil::join(thread)
  }
  
  let reader_results = []
  for thread in map_reader_threads {
    let result = ThreadUtil::join(thread)
    ArrayUtil::push(reader_results, result)
  }
  
  // Verify final map size
  assert_eq(ConcurrentMap::size(concurrent_map), 100)
  
  // Test concurrent set
  let concurrent_set = ConcurrentSet::new()
  
  // Adder threads
  let set_adder_threads = []
  for i in 0..5 {
    let thread = ThreadUtil::spawn(() -> {
      for j in 0..20 {
        ConcurrentSet::add(concurrent_set, i * 20 + j)
      }
    })
    ArrayUtil::push(set_adder_threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in set_adder_threads {
    ThreadUtil::join(thread)
  }
  
  // Verify set size
  assert_eq(ConcurrentSet::size(concurrent_set), 100)
  
  // Test contains operation
  for i in 0..100 {
    assert_true(ConcurrentSet::contains(concurrent_set, i))
  }
  assert_false(ConcurrentSet::contains(concurrent_set, 100))
}

// Test 4: Synchronization Primitive Tests
test "synchronization primitive operations" {
  // Test semaphore
  let semaphore = Semaphore::new(3)  // Allow 3 concurrent permits
  
  let semaphore_threads = []
  let completion_times = Ref::new([])
  
  // Create threads that acquire semaphore
  for i in 0..10 {
    let thread = ThreadUtil::spawn(() -> {
      let start_time = TimeUtil::current_time_millis()
      
      Semaphore::acquire(semaphore)
      TimeUtil::sleep(100)  // Hold permit for 100ms
      Semaphore::release(semaphore)
      
      let end_time = TimeUtil::current_time_millis()
      let completion_time = end_time - start_time
      
      Mutex::lock(Mutex::new())
      let times = Ref::get(completion_times)
      Ref::set(completion_times, ArrayUtil::append(times, [completion_time]))
      Mutex::unlock(Mutex::new())
    })
    ArrayUtil::push(semaphore_threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in semaphore_threads {
    ThreadUtil::join(thread)
  }
  
  // Verify completion times
  let times = Ref::get(completion_times)
  assert_eq(times.length(), 10)
  
  // With 3 permits, we should have approximately 4 "waves" of completion
  // First 3 should complete around 100ms, next 3 around 200ms, etc.
  let sorted_times = ArrayUtil::sort(times)
  assert_true(sorted_times[2] < 150)  // First 3 should complete before 150ms
  assert_true(sorted_times[5] < 250)  // First 6 should complete before 250ms
  assert_true(sorted_times[8] < 350)  // First 9 should complete before 350ms
  
  // Test barrier
  let barrier = Barrier::new(5)  // Wait for 5 threads
  let barrier_reached = Ref::new([])
  
  let barrier_threads = []
  for i in 0..5 {
    let thread = ThreadUtil::spawn(() -> {
      TimeUtil::sleep(i * 10)  // Stagger thread starts
      
      let reach_time = TimeUtil::current_time_millis()
      
      // Wait at barrier
      Barrier::wait(barrier)
      
      let pass_time = TimeUtil::current_time_millis()
      
      Mutex::lock(Mutex::new())
      let reached = Ref::get(barrier_reached)
      Ref::set(barrier_reached, ArrayUtil::append(reached, [(reach_time, pass_time)]))
      Mutex::unlock(Mutex::new())
    })
    ArrayUtil::push(barrier_threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in barrier_threads {
    ThreadUtil::join(thread)
  }
  
  // Verify barrier behavior
  let reached_times = Ref::get(barrier_reached)
  assert_eq(reached_times.length(), 5)
  
  // All threads should pass the barrier around the same time
  let pass_times = ArrayUtil::map(reached_times, (pair) -> { pair[1] })
  let min_pass = ArrayUtil::min(pass_times)
  let max_pass = ArrayUtil::max(pass_times)
  
  // All threads should pass within 50ms of each other
  assert_true(max_pass - min_pass < 50)
  
  // Test condition variable
  let condition_mutex = Mutex::new()
  let condition = ConditionVariable::new()
  let shared_flag = Ref::new(false)
  let condition_results = Ref::new([])
  
  // Waiter thread
  let waiter_thread = ThreadUtil::spawn(() -> {
    Mutex::lock(condition_mutex)
    
    while !Ref::get(shared_flag) {
      ConditionVariable::wait(condition, condition_mutex)
    }
    
    let wait_time = TimeUtil::current_time_millis()
    
    Mutex::unlock(condition_mutex)
    
    Mutex::lock(Mutex::new())
    let results = Ref::get(condition_results)
    Ref::set(condition_results, ArrayUtil::append(results, [("waiter", wait_time)]))
    Mutex::unlock(Mutex::new())
  })
  
  // Notifier thread
  let notifier_thread = ThreadUtil::spawn(() -> {
    TimeUtil::sleep(100)  // Wait before notifying
    
    Mutex::lock(condition_mutex)
    Ref::set(shared_flag, true)
    ConditionVariable::notify_all(condition)
    Mutex::unlock(condition_mutex)
    
    let notify_time = TimeUtil::current_time_millis()
    
    Mutex::lock(Mutex::new())
    let results = Ref::get(condition_results)
    Ref::set(condition_results, ArrayUtil::append(results, [("notifier", notify_time)]))
    Mutex::unlock(Mutex::new())
  })
  
  // Wait for threads to complete
  ThreadUtil::join(waiter_thread)
  ThreadUtil::join(notifier_thread)
  
  // Verify condition variable behavior
  let results = Ref::get(condition_results)
  assert_eq(results.length(), 2)
  
  let waiter_time = ArrayUtil::find(results, (pair) -> { pair[0] == "waiter" })[1]
  let notifier_time = ArrayUtil::find(results, (pair) -> { pair[0] == "notifier" })[1]
  
  // Waiter should wake up after notifier
  assert_true(waiter_time >= notifier_time)
  
  // Test countdown latch
  let latch = CountDownLatch::new(3)
  let latch_results = Ref::new([])
  
  // Worker threads
  let worker_threads = []
  for i in 0..3 {
    let thread = ThreadUtil::spawn(() -> {
      TimeUtil::sleep(i * 50)  // Stagger completion times
      
      // Do some work
      TimeUtil::sleep(100)
      
      // Count down
      CountDownLatch::count_down(latch)
      
      let completion_time = TimeUtil::current_time_millis()
      
      Mutex::lock(Mutex::new())
      let results = Ref::get(latch_results)
      Ref::set(latch_results, ArrayUtil::append(results, [("worker_" + i.to_string(), completion_time)]))
      Mutex::unlock(Mutex::new())
    })
    ArrayUtil::push(worker_threads, thread)
  }
  
  // Waiter thread
  let latch_waiter_thread = ThreadUtil::spawn(() -> {
    let start_time = TimeUtil::current_time_millis()
    
    CountDownLatch::wait(latch)  // Wait for all workers to complete
    
    let end_time = TimeUtil::current_time_millis()
    
    Mutex::lock(Mutex::new())
    let results = Ref::get(latch_results)
    Ref::set(latch_results, ArrayUtil::append(results, [("latch_waiter", end_time)]))
    Mutex::unlock(Mutex::new())
  })
  
  // Wait for all threads to complete
  for thread in worker_threads {
    ThreadUtil::join(thread)
  }
  
  ThreadUtil::join(latch_waiter_thread)
  
  // Verify latch behavior
  let latch_results_final = Ref::get(latch_results)
  assert_eq(latch_results_final.length(), 4)
  
  let latch_waiter_time = ArrayUtil::find(latch_results_final, (pair) -> { pair[0] == "latch_waiter" })[1]
  let worker_times = ArrayUtil::filter(latch_results_final, (pair) -> { pair[0] != "latch_waiter" })
  let latest_worker_time = ArrayUtil::max(ArrayUtil::map(worker_times, (pair) -> { pair[1] }))
  
  // Latch waiter should complete after the latest worker
  assert_true(latch_waiter_time >= latest_worker_time)
}

// Test 5: Memory Ordering and Visibility Tests
test "memory ordering and visibility operations" {
  // Test volatile behavior
  let volatile_flag = VolatileBool::new(false)
  let volatile_data = VolatileInt::new(0)
  
  // Writer thread
  let writer_thread = ThreadUtil::spawn(() -> {
    VolatileInt::set(volatile_data, 42)
    VolatileBool::set(volatile_flag, true)
  })
  
  // Reader thread
  let reader_thread = ThreadUtil::spawn(() -> {
    while !VolatileBool::get(volatile_flag) {
      // Busy wait
    }
    
    let data_value = VolatileInt::get(volatile_data)
    return data_value
  })
  
  ThreadUtil::join(writer_thread)
  let reader_result = ThreadUtil::join(reader_thread)
  
  // Reader should see the written data
  assert_eq(reader_result, 42)
  
  // Test memory fence
  let fence_data1 = Ref::new(0)
  let fence_data2 = Ref::new(0)
  let fence_flag = Ref::new(false)
  
  // Thread 1
  let fence_thread1 = ThreadUtil::spawn(() -> {
    Ref::set(fence_data1, 1)
    MemoryFence::store()
    Ref::set(fence_flag, true)
  })
  
  // Thread 2
  let fence_thread2 = ThreadUtil::spawn(() -> {
    while !Ref::get(fence_flag) {
      // Busy wait
    }
    
    MemoryFence::load()
    let data2_value = Ref::get(fence_data2)
    let data1_value = Ref::get(fence_data1)
    
    return (data1_value, data2_value)
  })
  
  ThreadUtil::join(fence_thread1)
  let fence_result = ThreadUtil::join(fence_thread2)
  
  // With memory fences, data1 should be visible
  assert_eq(fence_result[0], 1)
  
  // Test double-checked locking
  let lazy_instance = Ref::new(None)
  let instance_mutex = Mutex::new()
  
  let instance_threads = []
  
  // Create multiple threads that try to get instance
  for i in 0..10 {
    let thread = ThreadUtil::spawn(() -> {
      // First check without lock
      match Ref::get(lazy_instance) {
        Some(instance) => return instance,
        None => {
          // Double-checked locking
          Mutex::lock(instance_mutex)
          
          // Check again with lock
          match Ref::get(lazy_instance) {
            Some(instance) => {
              Mutex::unlock(instance_mutex)
              return instance
            }
            None => {
              // Create instance
              let new_instance = "ExpensiveInstance_" + TimeUtil::current_time_millis().to_string()
              Ref::set(lazy_instance, Some(new_instance))
              Mutex::unlock(instance_mutex)
              return new_instance
            }
          }
        }
      }
    })
    ArrayUtil::push(instance_threads, thread)
  }
  
  // Wait for all threads to complete
  let instance_results = []
  for thread in instance_threads {
    let result = ThreadUtil::join(thread)
    ArrayUtil::push(instance_results, result)
  }
  
  // All threads should get the same instance
  let first_instance = instance_results[0]
  for instance in instance_results {
    assert_eq(instance, first_instance)
  }
  
  // Test happens-before relationship
  let happens_before_data = Ref::new([])
  let happens_before_mutex = Mutex::new()
  
  // Thread A
  let thread_a = ThreadUtil::spawn(() -> {
    Mutex::lock(happens_before_mutex)
    
    // Write data
    Ref::set(happens_before_data, ArrayUtil::append(Ref::get(happens_before_data), ["A"]))
    
    Mutex::unlock(happens_before_mutex)
    
    // Signal completion
    let completion_flag = Ref::new(false)
    Ref::set(completion_flag, true)
  })
  
  // Thread B
  let thread_b = ThreadUtil::spawn(() -> {
    // Wait for thread A to complete
    let completion_flag = Ref::new(false)
    while !Ref::get(completion_flag) {
      TimeUtil::sleep(1)
    }
    
    Mutex::lock(happens_before_mutex)
    
    // Read data
    let current_data = Ref::get(happens_before_data)
    
    // Write more data
    Ref::set(happens_before_data, ArrayUtil::append(current_data, ["B"]))
    
    Mutex::unlock(happens_before_mutex)
  })
  
  ThreadUtil::join(thread_a)
  ThreadUtil::join(thread_b)
  
  // Verify happens-before relationship
  let final_data = Ref::get(happens_before_data)
  assert_eq(final_data[0], "A")
  assert_eq(final_data[1], "B")
}

// Test 6: Deadlock Detection and Prevention Tests
test "deadlock detection and prevention operations" {
  // Test deadlock detection
  let deadlock_detector = DeadlockDetector::new()
  
  let mutex1 = Mutex::new()
  let mutex2 = Mutex::new()
  
  // Register mutexes with detector
  DeadlockDetector::register_mutex(deadlock_detector, mutex1, "mutex1")
  DeadlockDetector::register_mutex(deadlock_detector, mutex2, "mutex2")
  
  // Thread 1: Lock mutex1 then mutex2
  let deadlock_thread1 = ThreadUtil::spawn(() -> {
    DeadlockDetector::acquire(deadlock_detector, mutex1)
    TimeUtil::sleep(50)
    DeadlockDetector::acquire(deadlock_detector, mutex2)
    
    // Do work
    TimeUtil::sleep(50)
    
    DeadlockDetector::release(deadlock_detector, mutex2)
    DeadlockDetector::release(deadlock_detector, mutex1)
  })
  
  // Thread 2: Lock mutex2 then mutex1 (potential deadlock)
  let deadlock_thread2 = ThreadUtil::spawn(() -> {
    TimeUtil::sleep(25)  // Let thread1 acquire mutex1 first
    
    DeadlockDetector::acquire(deadlock_detector, mutex2)
    TimeUtil::sleep(50)
    
    // This should detect potential deadlock
    let deadlock_detected = DeadlockDetector::try_acquire(deadlock_detector, mutex1)
    assert_true(deadlock_detected)  // Should detect deadlock
    
    // Release acquired lock
    DeadlockDetector::release(deadlock_detector, mutex2)
  })
  
  ThreadUtil::join(deadlock_thread1)
  ThreadUtil::join(deadlock_thread2)
  
  // Test lock ordering to prevent deadlock
  let lock_orderer = LockOrderer::new()
  
  // Define lock ordering
  LockOrderer::define_order(lock_orderer, ["A", "B", "C"])
  
  let ordered_mutex_a = Mutex::new()
  let ordered_mutex_b = Mutex::new()
  let ordered_mutex_c = Mutex::new()
  
  // Register with orderer
  LockOrderer::register_lock(lock_orderer, ordered_mutex_a, "A")
  LockOrderer::register_lock(lock_orderer, ordered_mutex_b, "B")
  LockOrderer::register_lock(lock_orderer, ordered_mutex_c, "C")
  
  // Thread 1: Lock in correct order A -> B -> C
  let ordered_thread1 = ThreadUtil::spawn(() -> {
    LockOrderer::acquire_ordered(lock_orderer, ordered_mutex_a)
    LockOrderer::acquire_ordered(lock_orderer, ordered_mutex_b)
    LockOrderer::acquire_ordered(lock_orderer, ordered_mutex_c)
    
    // Do work
    TimeUtil::sleep(50)
    
    LockOrderer::release_ordered(lock_orderer, ordered_mutex_c)
    LockOrderer::release_ordered(lock_orderer, ordered_mutex_b)
    LockOrderer::release_ordered(lock_orderer, ordered_mutex_a)
  })
  
  // Thread 2: Lock in incorrect order C -> B -> A (should be prevented)
  let ordered_thread2 = ThreadUtil::spawn(() -> {
    TimeUtil::sleep(25)
    
    LockOrderer::acquire_ordered(lock_orderer, ordered_mutex_c)
    
    // This should violate lock ordering
    let order_violation = LockOrderer::try_acquire_ordered(lock_orderer, ordered_mutex_b)
    assert_false(order_violation)  // Should detect violation
    
    LockOrderer::release_ordered(lock_orderer, ordered_mutex_c)
  })
  
  ThreadUtil::join(ordered_thread1)
  ThreadUtil::join(ordered_thread2)
  
  // Test timeout-based deadlock prevention
  let timeout_mutex1 = Mutex::new()
  let timeout_mutex2 = Mutex::new()
  
  // Thread 1: Lock mutex1 then try mutex2 with timeout
  let timeout_thread1 = ThreadUtil::spawn(() -> {
    Mutex::lock(timeout_mutex1)
    TimeUtil::sleep(100)
    
    // Try to lock mutex2 with timeout
    let timeout_result = Mutex::try_lock_for(timeout_mutex2, 50)
    assert_false(timeout_result)  // Should timeout
    
    Mutex::unlock(timeout_mutex1)
  })
  
  // Thread 2: Lock mutex2 then try mutex1 with timeout
  let timeout_thread2 = ThreadUtil::spawn(() -> {
    Mutex::lock(timeout_mutex2)
    TimeUtil::sleep(100)
    
    // Try to lock mutex1 with timeout
    let timeout_result = Mutex::try_lock_for(timeout_mutex1, 50)
    assert_false(timeout_result)  // Should timeout
    
    Mutex::unlock(timeout_mutex2)
  })
  
  ThreadUtil::join(timeout_thread1)
  ThreadUtil::join(timeout_thread2)
}

// Test 7: Concurrent Collection Performance Tests
test "concurrent collection performance operations" {
  // Test concurrent hash map performance
  let concurrent_map = ConcurrentMap::new()
  let map_size = 10000
  
  // Measure insertion performance
  let start_time = TimeUtil::current_time_millis()
  
  let map_threads = []
  for i in 0..10 {
    let thread = ThreadUtil::spawn(() -> {
      for j in 0..(map_size / 10) {
        let key = "key_" + (i * (map_size / 10) + j).to_string()
        let value = "value_" + (i * (map_size / 10) + j).to_string()
        ConcurrentMap::put(concurrent_map, key, value)
      }
    })
    ArrayUtil::push(map_threads, thread)
  }
  
  for thread in map_threads {
    ThreadUtil::join(thread)
  }
  
  let insertion_time = TimeUtil::current_time_millis() - start_time
  
  // Verify all items were inserted
  assert_eq(ConcurrentMap::size(concurrent_map), map_size)
  
  // Measure lookup performance
  start_time = TimeUtil::current_time_millis()
  
  let lookup_threads = []
  for i in 0..10 {
    let thread = ThreadUtil::spawn(() -> {
      for j in 0..(map_size / 10) {
        let key = "key_" + (i * (map_size / 10) + j).to_string()
        let value = ConcurrentMap::get(concurrent_map, key)
        assert_true(value != None)
      }
    })
    ArrayUtil::push(lookup_threads, thread)
  }
  
  for thread in lookup_threads {
    ThreadUtil::join(thread)
  }
  
  let lookup_time = TimeUtil::current_time_millis() - start_time
  
  // Performance should be reasonable
  assert_true(insertion_time < 5000)  // Should complete within 5 seconds
  assert_true(lookup_time < 3000)    // Should complete within 3 seconds
  
  // Test concurrent queue performance
  let concurrent_queue = ConcurrentQueue::new()
  let queue_size = 10000
  
  // Measure enqueue performance
  start_time = TimeUtil::current_time_millis()
  
  let enqueue_threads = []
  for i in 0..5 {
    let thread = ThreadUtil::spawn(() -> {
      for j in 0..(queue_size / 5) {
        ConcurrentQueue::enqueue(concurrent_queue, "item_" + (i * (queue_size / 5) + j).to_string())
      }
    })
    ArrayUtil::push(enqueue_threads, thread)
  }
  
  for thread in enqueue_threads {
    ThreadUtil::join(thread)
  }
  
  let enqueue_time = TimeUtil::current_time_millis() - start_time
  
  // Measure dequeue performance
  start_time = TimeUtil::current_time_millis()
  
  let dequeue_threads = []
  for i in 0..5 {
    let thread = ThreadUtil::spawn(() -> {
      let mut count = 0
      while count < (queue_size / 5) {
        match ConcurrentQueue::dequeue(concurrent_queue) {
          Some(_) => count = count + 1,
          None => TimeUtil::sleep(1)
        }
      }
    })
    ArrayUtil::push(dequeue_threads, thread)
  }
  
  for thread in dequeue_threads {
    ThreadUtil::join(thread)
  }
  
  let dequeue_time = TimeUtil::current_time_millis() - start_time
  
  // Verify queue is empty
  assert_true(ConcurrentQueue::is_empty(concurrent_queue))
  
  // Performance should be reasonable
  assert_true(enqueue_time < 3000)  // Should complete within 3 seconds
  assert_true(dequeue_time < 3000)  // Should complete within 3 seconds
}

// Test 8: Concurrent Algorithm Tests
test "concurrent algorithm operations" {
  // Test parallel quicksort
  let unsorted_array = ArrayUtil::shuffle(ArrayUtil::range(1, 10000))
  
  // Sequential quicksort
  let start_time = TimeUtil::current_time_millis()
  let sequential_sorted = QuickSort::sequential(unsorted_array)
  let sequential_time = TimeUtil::current_time_millis() - start_time
  
  // Parallel quicksort
  start_time = TimeUtil::current_time_millis()
  let parallel_sorted = QuickSort::parallel(unsorted_array, 4)  // 4 threads
  let parallel_time = TimeUtil::current_time_millis() - start_time
  
  // Verify both produce correct results
  assert_eq(sequential_sorted.length(), parallel_sorted.length())
  for i in 0..sequential_sorted.length() {
    assert_eq(sequential_sorted[i], parallel_sorted[i])
  }
  
  // Parallel should be faster for large arrays
  assert_true(parallel_time < sequential_time)
  
  // Test parallel map-reduce
  let data = ArrayUtil::range(1, 10000)
  
  // Sequential map-reduce
  start_time = TimeUtil::current_time_millis()
  let sequential_map = ArrayUtil::map(data, (x) -> { x * x })
  let sequential_reduce = ArrayUtil::reduce(sequential_map, 0, (acc, x) -> { acc + x })
  let sequential_map_reduce_time = TimeUtil::current_time_millis() - start_time
  
  // Parallel map-reduce
  start_time = TimeUtil::current_time_millis()
  let parallel_map = ParallelUtil::map(data, (x) -> { x * x })
  let parallel_reduce = ParallelUtil::reduce(parallel_map, 0, (acc, x) -> { acc + x })
  let parallel_map_reduce_time = TimeUtil::current_time_millis() - start_time
  
  // Verify both produce correct results
  assert_eq(sequential_reduce, parallel_reduce)
  
  // Parallel should be faster for large datasets
  assert_true(parallel_map_reduce_time < sequential_map_reduce_time)
  
  // Test parallel merge sort
  let unsorted_for_merge = ArrayUtil::shuffle(ArrayUtil::range(1, 10000))
  
  // Sequential merge sort
  start_time = TimeUtil::current_time_millis()
  let sequential_merge_sorted = MergeSort::sequential(unsorted_for_merge)
  let sequential_merge_time = TimeUtil::current_time_millis() - start_time
  
  // Parallel merge sort
  start_time = TimeUtil::current_time_millis()
  let parallel_merge_sorted = MergeSort::parallel(unsorted_for_merge, 4)  // 4 threads
  let parallel_merge_time = TimeUtil::current_time_millis() - start_time
  
  // Verify both produce correct results
  assert_eq(sequential_merge_sorted.length(), parallel_merge_sorted.length())
  for i in 0..sequential_merge_sorted.length() {
    assert_eq(sequential_merge_sorted[i], parallel_merge_sorted[i])
  }
  
  // Parallel should be faster for large arrays
  assert_true(parallel_merge_time < sequential_merge_time)
  
  // Test parallel matrix multiplication
  let matrix_a = [
    [1, 2, 3, 4],
    [5, 6, 7, 8],
    [9, 10, 11, 12],
    [13, 14, 15, 16]
  ]
  
  let matrix_b = [
    [17, 18, 19, 20],
    [21, 22, 23, 24],
    [25, 26, 27, 28],
    [29, 30, 31, 32]
  ]
  
  // Sequential matrix multiplication
  start_time = TimeUtil::current_time_millis()
  let sequential_result = MatrixUtil::multiply_sequential(matrix_a, matrix_b)
  let sequential_matrix_time = TimeUtil::current_time_millis() - start_time
  
  // Parallel matrix multiplication
  start_time = TimeUtil::current_time_millis()
  let parallel_result = MatrixUtil::multiply_parallel(matrix_a, matrix_b, 4)  // 4 threads
  let parallel_matrix_time = TimeUtil::current_time_millis() - start_time
  
  // Verify both produce correct results
  assert_eq(sequential_result.length(), parallel_result.length())
  for i in 0..sequential_result.length() {
    assert_eq(sequential_result[i].length(), parallel_result[i].length())
    for j in 0..sequential_result[i].length() {
      assert_eq(sequential_result[i][j], parallel_result[i][j])
    }
  }
  
  // Parallel should be faster for matrix operations
  assert_true(parallel_matrix_time < sequential_matrix_time)
}

// Test 9: Concurrent Resource Pool Tests
test "concurrent resource pool operations" {
  // Test thread pool
  let thread_pool = ThreadPool::new(4)
  let task_results = Ref::new([])
  
  // Submit tasks
  let futures = []
  for i in 0..20 {
    let future = ThreadPool::submit(thread_pool, () -> {
      TimeUtil::sleep(50)  // Simulate work
      return "Task_" + i.to_string() + "_completed"
    })
    ArrayUtil::push(futures, future)
  }
  
  // Collect results
  for future in futures {
    let result = Future::get(future)
    
    Mutex::lock(Mutex::new())
    let results = Ref::get(task_results)
    Ref::set(task_results, ArrayUtil::append(results, [result]))
    Mutex::unlock(Mutex::new())
  }
  
  // Verify all tasks completed
  let final_results = Ref::get(task_results)
  assert_eq(final_results.length(), 20)
  
  for i in 0..20 {
    assert_true(final_results.contains("Task_" + i.to_string() + "_completed"))
  }
  
  // Test connection pool
  let connection_pool = ConnectionPool::new("test_db_url", 5)
  let connection_results = Ref::new([])
  
  // Acquire connections
  let connection_threads = []
  for i in 0..10 {
    let thread = ThreadUtil::spawn(() -> {
      let connection = ConnectionPool::acquire(connection_pool)
      
      // Simulate using connection
      TimeUtil::sleep(100)
      
      let result = "Used_connection_" + ConnectionUtil::get_id(connection)
      
      ConnectionPool::release(connection_pool, connection)
      
      Mutex::lock(Mutex::new())
      let results = Ref::get(connection_results)
      Ref::set(connection_results, ArrayUtil::append(results, [result]))
      Mutex::unlock(Mutex::new())
    })
    ArrayUtil::push(connection_threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in connection_threads {
    ThreadUtil::join(thread)
  }
  
  // Verify all connections were used
  let connection_final_results = Ref::get(connection_results)
  assert_eq(connection_final_results.length(), 10)
  
  // Verify pool size is maintained
  assert_eq(ConnectionPool::get_available_connections(connection_pool), 5)
  
  // Test object pool
  let object_pool = ObjectPool::new(() -> { ExpensiveObject::new() }, 10)
  let object_results = Ref::new([])
  
  // Acquire objects
  let object_threads = []
  for i in 0..20 {
    let thread = ThreadUtil::spawn(() -> {
      let obj = ObjectPool::acquire(object_pool)
      
      // Use object
      let result = ExpensiveObject::process(obj, i)
      
      ObjectPool::release(object_pool, obj)
      
      Mutex::lock(Mutex::new())
      let results = Ref::get(object_results)
      Ref::set(object_results, ArrayUtil::append(results, [result]))
      Mutex::unlock(Mutex::new())
    })
    ArrayUtil::push(object_threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in object_threads {
    ThreadUtil::join(thread)
  }
  
  // Verify all objects were used
  let object_final_results = Ref::get(object_results)
  assert_eq(object_final_results.length(), 20)
  
  // Verify pool size is maintained
  assert_eq(ObjectPool::get_available_objects(object_pool), 10)
}

// Test 10: Concurrent Exception Handling Tests
test "concurrent exception handling operations" {
  // Test exception propagation across threads
  let exception_thread = ThreadUtil::spawn(() -> {
    TimeUtil::sleep(50)
    raise RuntimeError("Test exception from thread")
  })
  
  let exception_result = ThreadUtil::join_with_exception_handling(exception_thread)
  
  match exception_result {
    Error(err) => assert_eq(err, "Test exception from thread"),
    _ => assert_true(false)
  }
  
  // Test exception handling in thread pool
  let thread_pool = ThreadPool::new(2)
  let exception_futures = []
  
  // Submit tasks that throw exceptions
  for i in 0..5 {
    let future = ThreadPool::submit(thread_pool, () -> {
      TimeUtil::sleep(50)
      if i % 2 == 0 {
        raise RuntimeError("Exception from task " + i.to_string())
      } else {
        return "Success from task " + i.to_string()
      }
    })
    ArrayUtil::push(exception_futures, future)
  }
  
  // Collect results
  let exception_results = Ref::new([])
  for future in exception_futures {
    match Future::get_with_exception_handling(future) {
      Ok(result) => {
        Mutex::lock(Mutex::new())
        let results = Ref::get(exception_results)
        Ref::set(exception_results, ArrayUtil::append(results, [("success", result)]))
        Mutex::unlock(Mutex::new())
      }
      Error(err) => {
        Mutex::lock(Mutex::new())
        let results = Ref::get(exception_results)
        Ref::set(exception_results, ArrayUtil::append(results, [("error", err)]))
        Mutex::unlock(Mutex::new())
      }
    }
  }
  
  // Verify exception handling
  let final_exception_results = Ref::get(exception_results)
  assert_eq(final_exception_results.length(), 5)
  
  let success_count = ArrayUtil::filter(final_exception_results, (pair) -> { pair[0] == "success" }).length()
  let error_count = ArrayUtil::filter(final_exception_results, (pair) -> { pair[0] == "error" }).length()
  
  assert_eq(success_count, 2)
  assert_eq(error_count, 3)
  
  // Test concurrent exception aggregation
  let exception_aggregator = ExceptionAggregator::new()
  
  let aggregator_threads = []
  for i in 0..10 {
    let thread = ThreadUtil::spawn(() -> {
      try {
        TimeUtil::sleep(i * 10)
        if i % 3 == 0 {
          raise RuntimeError("Exception " + i.to_string())
        }
        return "Success " + i.to_string()
      } catch {
        RuntimeError(msg) => {
          ExceptionAggregator::add_exception(exception_aggregator, msg)
          return "Handled exception " + i.to_string()
        }
      }
    })
    ArrayUtil::push(aggregator_threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in aggregator_threads {
    ThreadUtil::join(thread)
  }
  
  // Verify exception aggregation
  let exceptions = ExceptionAggregator::get_exceptions(exception_aggregator)
  assert_eq(exceptions.length(), 4)  // 0, 3, 6, 9
  
  assert_true(exceptions.contains("Exception 0"))
  assert_true(exceptions.contains("Exception 3"))
  assert_true(exceptions.contains("Exception 6"))
  assert_true(exceptions.contains("Exception 9"))
  
  // Test concurrent error recovery
  let error_recovery = ErrorRecovery::new()
  
  ErrorRecovery::add_recovery_strategy(error_recovery, "RuntimeError", () -> {
    "Recovered from runtime error"
  })
  
  let recovery_threads = []
  let recovery_results = Ref::new([])
  
  for i in 0..10 {
    let thread = ThreadUtil::spawn(() -> {
      try {
        TimeUtil::sleep(i * 5)
        if i % 4 == 0 {
          raise RuntimeError("Recoverable error " + i.to_string())
        }
        return "Normal execution " + i.to_string()
      } catch {
        RuntimeError(msg) => {
          let recovery_result = ErrorRecovery::recover(error_recovery, msg)
          Mutex::lock(Mutex::new())
          let results = Ref::get(recovery_results)
          Ref::set(recovery_results, ArrayUtil::append(results, [("recovered", recovery_result)]))
          Mutex::unlock(Mutex::new())
          return recovery_result
        }
      }
    })
    ArrayUtil::push(recovery_threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in recovery_threads {
    ThreadUtil::join(thread)
  }
  
  // Verify error recovery
  let final_recovery_results = Ref::get(recovery_results)
  assert_eq(final_recovery_results.length(), 3)  // 0, 4, 8
  
  for result in final_recovery_results {
    assert_eq(result[0], "recovered")
    assert_eq(result[1], "Recovered from runtime error")
  }
}