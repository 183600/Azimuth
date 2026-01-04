// Azimuth High-Quality Concurrency and Thread Safety Tests
// This file contains comprehensive test cases for concurrency and thread safety

// Test 1: Thread Pool Management
test "thread pool management" {
  let thread_pool = ThreadPool::new(4) // 4 worker threads
  
  // Test basic task submission
  let task1 = thread_pool.submit(|| {
    sleep(Duration::from_millis(100))
    42
  })
  
  let result1 = task1.wait()
  assert_eq(result1, 42)
  
  // Test multiple concurrent tasks
  let tasks = []
  for i in 0..=10 {
    let task = thread_pool.submit(move || {
      sleep(Duration::from_millis(50 + i * 10))
      i * 2
    })
    tasks.push(task)
  }
  
  // Wait for all tasks to complete
  let results = []
  for task in tasks {
    let result = task.wait()
    results.push(result)
  }
  
  assert_eq(results.length(), 11)
  
  // Verify results
  for i in 0..=10 {
    assert_eq(results[i], i * 2)
  }
  
  // Test thread pool metrics
  let metrics = thread_pool.get_metrics()
  
  assert_true(metrics.active_threads >= 0)
  assert_true(metrics.active_threads <= 4)
  assert_true(metrics.completed_tasks > 0)
  assert_true(metrics.queue_length >= 0)
  assert_true(metrics.total_tasks_submitted > 0)
  
  // Test thread pool scaling
  thread_pool.resize(8) // Increase to 8 threads
  
  let scaling_metrics = thread_pool.get_metrics()
  assert_true(scaling_metrics.active_threads <= 8)
  
  // Test thread pool shutdown
  thread_pool.shutdown()
  
  // After shutdown, new tasks should be rejected
  let shutdown_result = thread_pool.submit(|| {
    "should_not_execute"
  })
  
  assert_true(shutdown_result.is_error())
}

// Test 2: Mutex and Lock Safety
test "mutex and lock safety" {
  let counter = AtomicCounter::new(0)
  let mutex = Mutex::new()
  
  // Test mutex protection of shared resource
  let threads = []
  for i in 0..=10 {
    let thread = Thread::spawn(move || {
      for j in 0..=100 {
        let _lock = mutex.lock()
        counter.increment()
        // Lock is automatically released when it goes out of scope
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    thread.join()
  }
  
  // Verify counter value
  assert_eq(counter.get_value(), 1100) // 11 threads * 100 increments each
  
  // Test try_lock functionality
  let try_lock_result = mutex.try_lock()
  assert_true(try_lock_result.is_success())
  
  let _lock = try_lock_result.unwrap()
  
  // Try to lock again (should fail)
  let second_try_result = mutex.try_lock()
  assert_true(second_try_result.is_error())
  
  // Test lock timeout
  let timeout_result = mutex.lock_with_timeout(Duration::from_millis(100))
  assert_true(timeout_result.is_error()) // Should timeout since lock is held
  
  // Test recursive mutex
  let recursive_mutex = RecursiveMutex::new()
  
  let recursive_thread = Thread::spawn(|| {
    let _lock1 = recursive_mutex.lock()
    let _lock2 = recursive_mutex.lock() // Should succeed in recursive mutex
    let _lock3 = recursive_mutex.lock() // Should also succeed
    // All locks will be released when they go out of scope
  })
  
  recursive_thread.join()
  
  // Test read-write lock
  let rw_lock = ReadWriteLock::new()
  let shared_data = SharedData::new(0)
  
  // Test multiple readers
  let reader_threads = []
  for i in 0..=5 {
    let thread = Thread::spawn(move || {
      let _read_lock = rw_lock.read_lock()
      let value = shared_data.get_value()
      assert_eq(value, 0) // Should still be 0 since no writers yet
    })
    reader_threads.push(thread)
  }
  
  // Wait for all readers to complete
  for thread in reader_threads {
    thread.join()
  }
  
  // Test exclusive writer
  let writer_thread = Thread::spawn(|| {
    let _write_lock = rw_lock.write_lock()
    shared_data.set_value(42)
  })
  
  writer_thread.join()
  
  // Verify data was written
  let _read_lock = rw_lock.read_lock()
  assert_eq(shared_data.get_value(), 42)
  
  // Test lock contention metrics
  let lock_metrics = mutex.get_contention_metrics()
  
  assert_true(lock_metrics.total_acquisitions > 0)
  assert_true(lock_metrics.contention_count >= 0)
  assert_true(lock_metrics.average_wait_time_ms >= 0)
}

// Test 3: Atomic Operations
test "atomic operations" {
  // Test atomic integer
  let atomic_int = AtomicInteger::new(0)
  
  // Test atomic increment
  let old_value = atomic_int.fetch_add(5)
  assert_eq(old_value, 0)
  assert_eq(atomic_int.get(), 5)
  
  // Test atomic compare and swap
  let cas_result = atomic_int.compare_and_swap(5, 10)
  assert_true(cas_result) // Should succeed since current value is 5
  assert_eq(atomic_int.get(), 10)
  
  let cas_fail_result = atomic_int.compare_and_swap(5, 15)
  assert_false(cas_fail_result) // Should fail since current value is 10
  assert_eq(atomic_int.get(), 10)
  
  // Test atomic operations from multiple threads
  let atomic_counter = AtomicInteger::new(0)
  let threads = []
  
  for i in 0..=10 {
    let thread = Thread::spawn(move || {
      for j in 0..=1000 {
        atomic_counter.fetch_add(1)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    thread.join()
  }
  
  // Verify final value
  assert_eq(atomic_counter.get(), 11000) // 11 threads * 1000 increments each
  
  // Test atomic boolean
  let atomic_bool = AtomicBoolean::new(false)
  
  assert_false(atomic_bool.get())
  
  atomic_bool.set(true)
  assert_true(atomic_bool.get())
  
  let old_bool = atomic_bool.compare_and_swap(true, false)
  assert_true(old_bool) // Should succeed
  assert_false(atomic_bool.get())
  
  // Test atomic reference
  let atomic_ref = AtomicReference::new("initial_value")
  
  assert_eq(atomic_ref.get(), "initial_value")
  
  let old_ref = atomic_ref.swap("new_value")
  assert_eq(old_ref, "initial_value")
  assert_eq(atomic_ref.get(), "new_value")
  
  // Test atomic floating point operations
  let atomic_float = AtomicFloat::new(0.0)
  
  atomic_float.fetch_add(1.5)
  assert_eq(atomic_float.get(), 1.5)
  
  atomic_float.fetch_sub(0.5)
  assert_eq(atomic_float.get(), 1.0)
}

// Test 4: Concurrent Data Structures
test "concurrent data structures" {
  // Test concurrent queue
  let concurrent_queue = ConcurrentQueue::new()
  
  // Test producer-consumer pattern
  let producer_threads = []
  let consumer_threads = []
  
  // Start producers
  for i in 0..=3 {
    let thread = Thread::spawn(move || {
      for j in 0..=100 {
        let item = "item_" + i.to_string() + "_" + j.to_string()
        concurrent_queue.enqueue(item)
      }
    })
    producer_threads.push(thread)
  }
  
  // Start consumers
  let consumed_items = Arc::new(Mutex::new([]))
  for i in 0..=2 {
    let consumed_items_clone = consumed_items.clone()
    let thread = Thread::spawn(move || {
      let mut local_items = []
      for j in 0..=133 { // 4 producers * 100 items / 3 consumers ≈ 133 each
        if let Some(item) = concurrent_queue.dequeue_timeout(Duration::from_millis(100)) {
          local_items.push(item)
        }
      }
      
      let _lock = consumed_items_clone.lock()
      for item in local_items {
        consumed_items_clone.value.push(item)
      }
    })
    consumer_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in producer_threads {
    thread.join()
  }
  
  for thread in consumer_threads {
    thread.join()
  }
  
  // Verify all items were consumed
  let _lock = consumed_items.lock()
  assert_eq(consumed_items.value.length(), 400) // 4 producers * 100 items each
  
  // Test concurrent hashmap
  let concurrent_map = ConcurrentHashMap::new()
  
  // Test concurrent writes
  let writer_threads = []
  for i in 0..=5 {
    let thread = Thread::spawn(move || {
      for j in 0..=50 {
        let key = "key_" + i.to_string() + "_" + j.to_string()
        let value = "value_" + i.to_string() + "_" + j.to_string()
        concurrent_map.insert(key, value)
      }
    })
    writer_threads.push(thread)
  }
  
  // Wait for all writers to complete
  for thread in writer_threads {
    thread.join()
  }
  
  // Verify all entries were added
  assert_eq(concurrent_map.size(), 300) // 6 threads * 50 entries each
  
  // Test concurrent reads
  let reader_threads = []
  let read_results = Arc::new(Mutex::new([]))
  
  for i in 0..=5 {
    let read_results_clone = read_results.clone()
    let thread = Thread::spawn(move || {
      let mut local_results = []
      for j in 0..=50 {
        let key = "key_" + i.to_string() + "_" + j.to_string()
        if let Some(value) = concurrent_map.get(key) {
          local_results.push(value)
        }
      }
      
      let _lock = read_results_clone.lock()
      for result in local_results {
        read_results_clone.value.push(result)
      }
    })
    reader_threads.push(thread)
  }
  
  // Wait for all readers to complete
  for thread in reader_threads {
    thread.join()
  }
  
  // Verify reads
  let _lock = read_results.lock()
  assert_eq(read_results.value.length(), 300) // All keys should be found
  
  // Test concurrent set
  let concurrent_set = ConcurrentSet::new()
  
  // Test concurrent additions
  let add_threads = []
  for i in 0..=4 {
    let thread = Thread::spawn(move || {
      for j in 0..=40 {
        let item = "item_" + i.to_string() + "_" + j.to_string()
        concurrent_set.add(item)
      }
    })
    add_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in add_threads {
    thread.join()
  }
  
  // Verify set size (should be 250 unique items)
  assert_eq(concurrent_set.size(), 250)
  
  // Test concurrent contains operation
  let contains_threads = []
  let contains_results = Arc::new(Mutex::new([]))
  
  for i in 0..=4 {
    let contains_results_clone = contains_results.clone()
    let thread = Thread::spawn(move || {
      let mut local_results = []
      for j in 0..=40 {
        let item = "item_" + i.to_string() + "_" + j.to_string()
        let contains = concurrent_set.contains(item)
        local_results.push(contains)
      }
      
      let _lock = contains_results_clone.lock()
      for result in local_results {
        contains_results_clone.value.push(result)
      }
    })
    contains_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in contains_threads {
    thread.join()
  }
  
  // Verify all items are found
  let _lock = contains_results.lock()
  for result in contains_results.value {
    assert_true(result)
  }
}

// Test 5: Thread-Safe Reference Counting
test "thread-safe reference counting" {
  // Test Arc (Atomic Reference Counting)
  let shared_data = Arc::new("shared_string")
  
  assert_eq(Arc::strong_count(&shared_data), 1)
  
  // Create multiple references
  let ref1 = shared_data.clone()
  let ref2 = shared_data.clone()
  let ref3 = shared_data.clone()
  
  assert_eq(Arc::strong_count(&shared_data), 4)
  
  // Verify all references point to the same data
  assert_eq(*shared_data, "shared_string")
  assert_eq(*ref1, "shared_string")
  assert_eq(*ref2, "shared_string")
  assert_eq(*ref3, "shared_string")
  
  // Test Arc in multi-threaded environment
  let arc_shared = Arc::new(Mutex::new(0))
  let threads = []
  
  for i in 0..=10 {
    let arc_clone = arc_shared.clone()
    let thread = Thread::spawn(move || {
      let _lock = arc_clone.lock()
      *arc_clone.value += 1
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    thread.join()
  }
  
  // Verify final value
  let _lock = arc_shared.lock()
  assert_eq(*arc_shared.value, 11)
  
  // Drop some references
  drop(ref1);
  drop(ref2);
  
  assert_eq(Arc::strong_count(&shared_data), 2)
  
  // Test Weak references
  let weak_ref = Arc::downgrade(&shared_data)
  
  assert_eq(Arc::strong_count(&shared_data), 2)
  assert_eq(Arc::weak_count(&shared_data), 1)
  
  // Upgrade weak reference
  let upgraded = weak_ref.upgrade()
  assert_true(upgraded.is_some())
  assert_eq(*upgraded.unwrap(), "shared_string")
  
  // Drop last strong reference
  drop(shared_data);
  drop(ref3);
  
  // Now weak reference should no longer upgrade
  let upgraded_fail = weak_ref.upgrade()
  assert_true(upgraded_fail.is_none())
}

// Test 6: Condition Variables
test "condition variables" {
  let mutex = Mutex::new()
  let condition = ConditionVariable::new()
  let shared_data = SharedData::new(0)
  let ready = AtomicBoolean::new(false)
  
  // Producer thread
  let producer = Thread::spawn(|| {
    let _lock = mutex.lock()
    
    // Simulate work
    sleep(Duration::from_millis(100))
    
    // Update shared data
    shared_data.set_value(42)
    ready.set(true)
    
    // Notify waiting threads
    condition.notify_all()
  })
  
  // Consumer thread
  let consumer = Thread::spawn(|| {
    let _lock = mutex.lock()
    
    // Wait for data to be ready
    while !ready.get() {
      condition.wait(_lock)
    }
    
    // Data should now be ready
    assert_eq(shared_data.get_value(), 42)
  })
  
  // Wait for threads to complete
  producer.join()
  consumer.join()
  
  // Test timeout with condition variable
  let timeout_mutex = Mutex::new()
  let timeout_condition = ConditionVariable::new()
  let timeout_data = SharedData::new(0)
  
  let timeout_thread = Thread::spawn(|| {
    let _lock = timeout_mutex.lock()
    
    // Wait with timeout
    let wait_result = timeout_condition.wait_timeout(_lock, Duration::from_millis(100))
    
    // Should timeout
    assert_false(wait_result)
    assert_eq(timeout_data.get_value(), 0) // Value should remain unchanged
  })
  
  timeout_thread.join()
  
  // Test notify_one
  let notify_mutex = Mutex::new()
  let notify_condition = ConditionVariable::new()
  let notify_data = SharedData::new(0)
  let notified = AtomicBoolean::new(false)
  
  // Create multiple waiting threads
  let waiting_threads = []
  for i in 0..=3 {
    let thread = Thread::spawn(move || {
      let _lock = notify_mutex.lock()
      
      while !notified.get() {
        notify_condition.wait(_lock)
      }
      
      // Only first notified thread should modify data
      if notify_data.get_value() == 0 {
        notify_data.set_value(i + 1)
      }
    })
    waiting_threads.push(thread)
  }
  
  // Notify only one thread
  sleep(Duration::from_millis(50))
  let _lock = notify_mutex.lock()
  notified.set(true)
  notify_condition.notify_one();
  
  // Wait for all threads to complete
  for thread in waiting_threads {
    thread.join()
  }
  
  // Verify only one thread modified the data
  assert_true(notify_data.get_value() > 0 && notify_data.get_value() <= 4)
}

// Test 7: Futures and Promises
test "futures and promises" {
  // Test basic future/promise
  let (promise, future) = create_future_promise_pair()
  
  // Spawn a thread to fulfill the promise
  let promise_thread = Thread::spawn(move || {
    sleep(Duration::from_millis(100))
    promise.set("fulfilled_value")
  })
  
  // Wait for the future
  let result = future.wait()
  assert_eq(result, "fulfilled_value")
  
  promise_thread.join()
  
  // Test future chaining
  let initial_future = Future::from_value(10)
  
  let chained_future = initial_future.then(|x| {
    x * 2
  }).then(|x| {
    x + 5
  }).then(|x| {
    x.to_string()
  })
  
  let chained_result = chained_future.wait()
  assert_eq(chained_result, "25") // (10 * 2) + 5 = 25
  
  // Test future combinators
  let future1 = Future::from_value(1)
  let future2 = Future::from_value(2)
  let future3 = Future::from_value(3)
  
  // Test join all
  let joined_future = Future::join_all([future1, future2, future3])
  let joined_result = joined_future.wait()
  assert_eq(joined_result, [1, 2, 3])
  
  // Test select (first completed)
  let slow_future = Future::lazy(|| {
    sleep(Duration::from_millis(200))
    "slow"
  })
  
  let fast_future = Future::lazy(|| {
    sleep(Duration::from_millis(50))
    "fast"
  })
  
  let selected_future = Future::select([slow_future, fast_future])
  let selected_result = selected_future.wait()
  assert_eq(selected_result, "fast") // Fast future should complete first
  
  // Test async operations with futures
  let async_executor = AsyncExecutor::new(4) // 4 worker threads
  
  let async_future = async_executor.spawn_async(|| {
    sleep(Duration::from_millis(100))
    "async_result"
  })
  
  let async_result = async_future.wait()
  assert_eq(async_result, "async_result")
  
  // Test error handling with futures
  let error_future = Future::lazy(|| {
    sleep(Duration::from_millis(50))
    raise TestError::new("Simulated error")
  })
  
  let error_result = error_future.wait()
  match error_result {
    Ok(_) => {
      assert_true(false) // Should have failed
    }
    Err(TestError::TestError(message)) => {
      assert_eq(message, "Simulated error")
    }
    Err(_) => {
      assert_true(false) // Unexpected error type
    }
  }
  
  // Test timeout with futures
  let timeout_future = Future::lazy(|| {
    sleep(Duration::from_millis(200))
    "should_timeout"
  })
  
  let timeout_result = timeout_future.wait_timeout(Duration::from_millis(100))
  assert_true(timeout_result.is_none()) // Should timeout
}

// Test 8: Lock-Free Data Structures
test "lock-free data structures" {
  // Test lock-free stack
  let lock_free_stack = LockFreeStack::new()
  
  // Push elements
  for i in 0..=100 {
    lock_free_stack.push(i)
  }
  
  // Pop elements concurrently
  let threads = []
  let popped_values = Arc::new(Mutex::new([]))
  
  for i in 0..=5 {
    let popped_values_clone = popped_values.clone()
    let thread = Thread::spawn(move || {
      let mut local_values = []
      
      for j in 0..=20 {
        if let Some(value) = lock_free_stack.pop() {
          local_values.push(value)
        }
      }
      
      let _lock = popped_values_clone.lock()
      for value in local_values {
        popped_values_clone.value.push(value)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    thread.join()
  }
  
  // Verify popped values
  let _lock = popped_values.lock()
  assert_eq(popped_values.value.length(), 101) // All elements should be popped
  
  // Verify all values are unique and in range
  let mut sorted_values = popped_values.value.clone()
  sorted_values.sort()
  
  for i in 0..=100 {
    assert_eq(sorted_values[i], i)
  }
  
  // Test lock-free queue
  let lock_free_queue = LockFreeQueue::new()
  
  // Enqueue elements
  for i in 0..=100 {
    lock_free_queue.enqueue(i)
  }
  
  // Dequeue elements concurrently
  let queue_threads = []
  let dequeued_values = Arc::new(Mutex::new([]))
  
  for i in 0..=5 {
    let dequeued_values_clone = dequeued_values.clone()
    let thread = Thread::spawn(move || {
      let mut local_values = []
      
      for j in 0..=20 {
        if let Some(value) = lock_free_queue.dequeue() {
          local_values.push(value)
        }
      }
      
      let _lock = dequeued_values_clone.lock()
      for value in local_values {
        dequeued_values_clone.value.push(value)
      }
    })
    queue_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in queue_threads {
    thread.join()
  }
  
  // Verify dequeued values
  let _lock = dequeued_values.lock()
  assert_eq(dequeued_values.value.length(), 101) // All elements should be dequeued
  
  // Test ABA problem prevention
  let aba_stack = LockFreeStack::new()
  
  // Push initial element
  aba_stack.push("initial")
  
  let aba_thread = Thread::spawn(|| {
    // Pop initial element
    let popped = aba_stack.pop()
    assert_eq(popped, Some("initial"))
    
    // Modify element and push back
    aba_stack.push("modified")
    
    // Pop and push again to simulate ABA
    let popped_again = aba_stack.pop()
    assert_eq(popped_again, Some("modified"))
    aba_stack.push("modified_again")
  })
  
  aba_thread.join()
  
  // Final pop should get the last pushed element
  let final_pop = aba_stack.pop()
  assert_eq(final_pop, Some("modified_again"))
}

// Test 9: Concurrent Algorithms
test "concurrent algorithms" {
  // Test parallel merge sort
  let unsorted_data = generate_random_array(10000)
  let parallel_sorter = ParallelMergeSort::new(4) // 4 threads
  
  let sorted_data = parallel_sorter.sort(unsorted_data.clone())
  
  // Verify sorting
  for i in 0..=sorted_data.length() - 2 {
    assert_true(sorted_data[i] <= sorted_data[i + 1])
  }
  
  // Compare with sequential sort
  let sequential_sorted = unsorted_data.clone()
  sequential_sorted.sort()
  
  assert_eq(sorted_data, sequential_sorted)
  
  // Test parallel map-reduce
  let map_reduce = ParallelMapReduce::new(8) // 8 threads
  
  let data = (0..=10000).map(|i| i).collect()
  
  // Map operation: square each number
  let mapped = map_reduce.map(data, |x| x * x)
  
  // Reduce operation: sum all squared numbers
  let reduced = map_reduce.reduce(mapped, |a, b| a + b)
  
  // Verify result (sum of squares from 0 to 10000)
  let expected = (0..=10000).map(|i| i * i).reduce(0, |a, b| a + b)
  assert_eq(reduced, expected)
  
  // Test parallel prefix sum
  let prefix_sum = ParallelPrefixSum::new(4) // 4 threads
  
  let input_data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let prefix_result = prefix_sum.compute(input_data.to_array())
  
  // Verify prefix sum
  assert_eq(prefix_result, [1, 3, 6, 10, 15, 21, 28, 36, 45, 55])
  
  // Test parallel matrix multiplication
  let matrix_a = generate_random_matrix(100, 100)
  let matrix_b = generate_random_matrix(100, 100)
  
  let parallel_matrix_mult = ParallelMatrixMultiplication::new(4) // 4 threads
  let result_matrix = parallel_matrix_mult.multiply(matrix_a.clone(), matrix_b.clone())
  
  // Verify matrix dimensions
  assert_eq(result_matrix.rows, 100)
  assert_eq(result_matrix.cols, 100)
  
  // Verify matrix multiplication correctness
  for i in 0..=10 { // Check first 10 rows
    for j in 0..=10 { // Check first 10 columns
      let mut expected_value = 0
      for k in 0..=99 {
        expected_value += matrix_a.get(i, k) * matrix_b.get(k, j)
      }
      assert_eq(result_matrix.get(i, j), expected_value)
    }
  }
  
  // Test parallel quicksort
  let parallel_quicksort = ParallelQuickSort::new(6) // 6 threads
  
  let unsorted_for_quicksort = generate_random_array(10000)
  let quicksorted = parallel_quicksort.sort(unsorted_for_quicksort.clone())
  
  // Verify sorting
  for i in 0..=quicksorted.length() - 2 {
    assert_true(quicksorted[i] <= quicksorted[i + 1])
  }
  
  // Compare with built-in sort
  let builtin_sorted = unsorted_for_quicksort.clone()
  builtin_sorted.sort()
  
  assert_eq(quicksorted, builtin_sorted)
}

// Test 10: Concurrent Resource Management
test "concurrent resource management" {
  // Test connection pool
  let connection_pool = ConcurrentConnectionPool::new(10) // 10 connections
  
  // Test concurrent connection acquisition
  let threads = []
  let acquired_connections = Arc::new(Mutex::new([]))
  
  for i in 0..=15 {
    let acquired_connections_clone = acquired_connections.clone()
    let thread = Thread::spawn(move || {
      if let Some(conn) = connection_pool.acquire() {
        // Simulate using the connection
        sleep(Duration::from_millis(50 + i * 10))
        
        let _lock = acquired_connections_clone.lock()
        acquired_connections_clone.value.push(conn.id)
        
        // Return connection to pool
        connection_pool.release(conn)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    thread.join()
  }
  
  // Verify connections were acquired and released
  let _lock = acquired_connections.lock()
  assert_eq(acquired_connections.value.length(), 15) // All threads should get a connection
  assert_eq(connection_pool.get_available_connections(), 10) // All connections returned
  
  // Test semaphore
  let semaphore = Semaphore::new(3) // 3 permits
  
  let semaphore_threads = []
  let semaphore_results = Arc::new(Mutex::new([]))
  
  for i in 0..=8 {
    let semaphore_results_clone = semaphore_results.clone()
    let thread = Thread::spawn(move || {
      let start_time = get_current_timestamp()
      
      // Acquire permit
      let _permit = semaphore.acquire()
      
      let acquire_time = get_current_timestamp()
      
      // Hold permit for a while
      sleep(Duration::from_millis(100))
      
      let release_time = get_current_timestamp()
      
      // Permit is automatically released when it goes out of scope
      
      let _lock = semaphore_results_clone.lock()
      semaphore_results_clone.value.push({
        "thread_id": i,
        "acquire_time_ms": (acquire_time - start_time).to_millis(),
        "hold_time_ms": (release_time - acquire_time).to_millis()
      })
    })
    semaphore_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in semaphore_threads {
    thread.join()
  }
  
  // Verify semaphore behavior
  let _lock = semaphore_results.lock()
  assert_eq(semaphore_results.value.length(), 8)
  
  // Test barrier
  let barrier = Barrier::new(5) // 5 threads must wait
  
  let barrier_threads = []
  let barrier_results = Arc::new(Mutex::new([]))
  
  for i in 0..=4 {
    let barrier_results_clone = barrier_results.clone()
    let thread = Thread::spawn(move || {
      let start_time = get_current_timestamp()
      
      // Do some work
      sleep(Duration::from_millis(50 + i * 10))
      
      // Wait at barrier
      barrier.wait()
      
      let end_time = get_current_timestamp()
      
      let _lock = barrier_results_clone.lock()
      barrier_results_clone.value.push({
        "thread_id": i,
        "wait_time_ms": (end_time - start_time).to_millis()
      })
    })
    barrier_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in barrier_threads {
    thread.join()
  }
  
  // Verify barrier behavior
  let _lock = barrier_results.lock()
  assert_eq(barrier_results.value.length(), 5)
  
  // All threads should have waited approximately the same amount of time
  let wait_times = barrier_results.value.map(|r| r.wait_time_ms)
  let max_wait = wait_times.max()
  let min_wait = wait_times.min()
  
  // The difference should be relatively small (within 100ms)
  assert_true(max_wait - min_wait < 100)
  
  // Test concurrent resource cleanup
  let resource_manager = ConcurrentResourceManager::new()
  
  let cleanup_threads = []
  
  for i in 0..=10 {
    let thread = Thread::spawn(move || {
      // Allocate resources
      let resources = []
      for j in 0..=10 {
        let resource = resource_manager.allocate_resource("resource_" + i.to_string() + "_" + j.to_string())
        resources.push(resource)
      }
      
      // Use resources
      sleep(Duration::from_millis(50))
      
      // Clean up resources
      for resource in resources {
        resource_manager.deallocate_resource(resource)
      }
    })
    cleanup_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in cleanup_threads {
    thread.join()
  }
  
  // Verify all resources were cleaned up
  assert_eq(resource_manager.get_allocated_resources_count(), 0)
  
  // Test resource leak detection
  let leak_detector = ConcurrentResourceLeakDetector::new()
  
  // Intentionally leak some resources
  let leaked_resources = []
  for i in 0..=5 {
    let resource = resource_manager.allocate_resource("leaked_" + i.to_string())
    leaked_resources.push(resource)
    leak_detector.track_resource(resource)
  }
  
  let leak_report = leak_detector.detect_leaks()
  assert_true(leak_report.leaked_resources.length() > 0)
  
  // Clean up leaked resources
  for resource in leaked_resources {
    resource_manager.deallocate_resource(resource)
    leak_detector.untrack_resource(resource)
  }
  
  let final_leak_report = leak_detector.detect_leaks()
  assert_eq(final_leak_report.leaked_resources.length(), 0)
}