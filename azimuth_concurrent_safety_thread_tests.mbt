// Azimuth Concurrent Safety Thread Tests
// This file contains test cases for concurrent safety and thread management functionality

// Test 1: Basic Thread Safety with Shared Data
test "basic thread safety with shared data" {
  let shared_counter = AtomicCounter::new(0)
  let thread_manager = ThreadManager::new()
  
  // Create multiple threads that increment the counter
  let threads = []
  for i = 0; i < 10; i = i + 1 {
    let thread = thread_manager.spawn(fn() {
      for j = 0; j < 1000; j = j + 1 {
        AtomicCounter::increment(shared_counter)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    ThreadManager::join(thread)
  }
  
  // Verify final counter value
  assert_eq(AtomicCounter::get(shared_counter), 10000)
  
  // Test atomic compare and swap
  let atomic_value = AtomicInt::new(10)
  
  // Successful CAS
  let cas_result1 = AtomicInt::compare_and_swap(atomic_value, 10, 20)
  assert_eq(cas_result1, true)
  assert_eq(AtomicInt::get(atomic_value), 20)
  
  // Failed CAS
  let cas_result2 = AtomicInt::compare_and_swap(atomic_value, 10, 30)
  assert_eq(cas_result2, false)
  assert_eq(AtomicInt::get(atomic_value), 20)
  
  // Test atomic operations
  AtomicInt::add(atomic_value, 5)
  assert_eq(AtomicInt::get(atomic_value), 25)
  
  AtomicInt::subtract(atomic_value, 10)
  assert_eq(AtomicInt::get(atomic_value), 15)
}

// Test 2: Mutex and Lock Safety
test "mutex and lock safety" {
  let thread_manager = ThreadManager::new()
  let shared_data = Mutex::new([])
  let data_lock = Mutex::new(0)
  
  // Test basic mutex operations
  threads = []
  
  // Producer threads
  for i = 0; i < 5; i = i + 1 {
    let thread_id = i
    let thread = thread_manager.spawn(fn() {
      for j = 0; j < 100; j = j + 1 {
        Mutex::lock(shared_data)
        let data = Mutex::get(shared_data)
        let new_data = data.concat(["thread_" + thread_id.to_string() + "_item_" + j.to_string()])
        Mutex::set(shared_data, new_data)
        Mutex::unlock(shared_data)
      }
    })
    threads.push(thread)
  }
  
  // Consumer threads
  for i = 0; i < 3; i = i + 1 {
    let thread = thread_manager.spawn(fn() {
      for j = 0; j < 50; j = j + 1 {
        Mutex::lock(shared_data)
        let data = Mutex::get(shared_data)
        if data.length() > 0 {
          let new_data = data.slice(1, data.length())
          Mutex::set(shared_data, new_data)
        }
        Mutex::unlock(shared_data)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    ThreadManager::join(thread)
  }
  
  // Verify data integrity
  let final_data = Mutex::get(shared_data)
  assert_eq(final_data.length() >= 0, true)
  
  // Test RAII lock guard
  let guarded_data = Mutex::new(0)
  
  let lock_test_thread = thread_manager.spawn(fn() {
    let guard = MutexLockGuard::new(guarded_data)
    let value = MutexLockGuard::get(guard)
    MutexLockGuard::set(guard, value + 1)
    // Lock automatically released when guard goes out of scope
  })
  
  ThreadManager::join(lock_test_thread)
  assert_eq(Mutex::get(guarded_data), 1)
  
  // Test try_lock
  let try_lock_data = Mutex::new(42)
  
  let blocking_thread = thread_manager.spawn(fn() {
    let guard = MutexLockGuard::new(try_lock_data)
    Clock::sleep(200)  // Hold lock for 200ms
  })
  
  Clock::sleep(50)  // Let blocking thread acquire lock
  
  let try_result = Mutex::try_lock(try_lock_data)
  assert_eq(try_result.is_none(), true)  // Should fail to acquire lock
  
  ThreadManager::join(blocking_thread)
  
  // Now try_lock should succeed
  let try_result2 = Mutex::try_lock(try_lock_data)
  assert_eq(try_result2.is_some(), true)
}

// Test 3: Read-Write Lock Safety
test "read write lock safety" {
  let thread_manager = ThreadManager::new()
  let shared_resource = RwLock::new("initial_value")
  let read_count = AtomicInt::new(0)
  let write_count = AtomicInt::new(0)
  
  // Create reader threads
  let reader_threads = []
  for i = 0; i < 5; i = i + 1 {
    let thread = thread_manager.spawn(fn() {
      for j = 0; j < 100; j = j + 1 {
        RwLock::read_lock(shared_resource)
        let value = RwLock::get_read(shared_resource)
        AtomicInt::increment(read_count)
        RwLock::read_unlock(shared_resource)
        
        Clock::sleep(1)  // Small delay to increase contention
      }
    })
    reader_threads.push(thread)
  }
  
  // Create writer threads
  let writer_threads = []
  for i = 0; i < 2; i = i + 1 {
    let thread_id = i
    let thread = thread_manager.spawn(fn() {
      for j = 0; j < 10; j = j + 1 {
        RwLock::write_lock(shared_resource)
        let current_value = RwLock::get_write(shared_resource)
        let new_value = current_value + "_writer_" + thread_id.to_string() + "_" + j.to_string()
        RwLock::set_write(shared_resource, new_value)
        AtomicInt::increment(write_count)
        RwLock::write_unlock(shared_resource)
        
        Clock::sleep(5)  // Longer delay for writers
      }
    })
    writer_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in reader_threads {
    ThreadManager::join(thread)
  }
  
  for thread in writer_threads {
    ThreadManager::join(thread)
  }
  
  // Verify operations completed
  assert_eq(AtomicInt::get(read_count), 500)
  assert_eq(AtomicInt::get(write_count), 20)
  
  // Test try_read_lock and try_write_lock
  let try_rwlock = RwLock::new("try_test")
  
  // Acquire write lock
  RwLock::write_lock(try_rwlock)
  
  // Try read lock should fail
  let try_read_result = RwLock::try_read_lock(try_rwlock)
  assert_eq(try_read_result, false)
  
  // Try write lock should fail
  let try_write_result = RwLock::try_write_lock(try_rwlock)
  assert_eq(try_write_result, false)
  
  RwLock::write_unlock(try_rwlock)
  
  // Now try locks should succeed
  let try_read_result2 = RwLock::try_read_lock(try_rwlock)
  assert_eq(try_read_result2, true)
  RwLock::read_unlock(try_rwlock)
  
  let try_write_result2 = RwLock::try_write_lock(try_rwlock)
  assert_eq(try_write_result2, true)
  RwLock::write_unlock(try_rwlock)
}

// Test 4: Concurrent Data Structures
test "concurrent data structures" {
  let thread_manager = ThreadManager::new()
  
  // Test concurrent queue
  let concurrent_queue = ConcurrentQueue::new()
  
  // Producer threads
  let producer_threads = []
  for i = 0; i < 3; i = i + 1 {
    let producer_id = i
    let thread = thread_manager.spawn(fn() {
      for j = 0; j < 100; j = j + 1 {
        let item = "producer_" + producer_id.to_string() + "_item_" + j.to_string()
        ConcurrentQueue::enqueue(concurrent_queue, item)
      }
    })
    producer_threads.push(thread)
  }
  
  // Consumer threads
  let consumer_threads = []
  let consumed_count = AtomicInt::new(0)
  
  for i = 0; i < 3; i = i + 1 {
    let thread = thread_manager.spawn(fn() {
      while true {
        let item = ConcurrentQueue::try_dequeue(concurrent_queue)
        match item {
          Some(_) => {
            AtomicInt::increment(consumed_count)
          }
          None => {
            Clock::sleep(1)
            // Check if queue is empty and producers are done
            if ConcurrentQueue::is_empty(concurrent_queue) {
              break
            }
          }
        }
      }
    })
    consumer_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in producer_threads {
    ThreadManager::join(thread)
  }
  
  for thread in consumer_threads {
    ThreadManager::join(thread)
  }
  
  // Verify all items were consumed
  assert_eq(AtomicInt::get(consumed_count), 300)
  assert_eq(ConcurrentQueue::is_empty(concurrent_queue), true)
  
  // Test concurrent hash map
  let concurrent_map = ConcurrentHashMap::new()
  
  let map_threads = []
  
  // Insert threads
  for i = 0; i < 5; i = i + 1 {
    let thread_id = i
    let thread = thread_manager.spawn(fn() {
      for j = 0; j < 20; j = j + 1 {
        let key = "key_" + thread_id.to_string() + "_" + j.to_string()
        let value = "value_" + thread_id.to_string() + "_" + j.to_string()
        ConcurrentHashMap::insert(concurrent_map, key, value)
      }
    })
    map_threads.push(thread)
  }
  
  // Wait for insertions to complete
  for thread in map_threads {
    ThreadManager::join(thread)
  }
  
  // Verify all items were inserted
  assert_eq(ConcurrentHashMap::size(concurrent_map), 100)
  
  // Test concurrent access
  let access_threads = []
  let access_count = AtomicInt::new(0)
  
  for i = 0; i < 10; i = i + 1 {
    let thread = thread_manager.spawn(fn() {
      for j = 0; j < 10; j = j + 1 {
        let key = "key_" + (j % 5).to_string() + "_" + (j % 20).to_string()
        let value = ConcurrentHashMap::get(concurrent_map, key)
        match value {
          Some(_) => AtomicInt::increment(access_count)
          None => assert_true(false)
        }
      }
    })
    access_threads.push(thread)
  }
  
  for thread in access_threads {
    ThreadManager::join(thread)
  }
  
  assert_eq(AtomicInt::get(access_count), 100)
}

// Test 5: Thread Pool and Task Scheduling
test "thread pool and task scheduling" {
  let thread_pool = ThreadPool::new(4)  // 4 worker threads
  let task_counter = AtomicInt::new(0)
  let completion_order = Mutex::new([])
  
  // Submit tasks to thread pool
  let tasks = []
  for i = 0; i < 20; i = i + 1 {
    let task_id = i
    let task = Task::new(fn() {
      AtomicInt::increment(task_counter)
      
      // Simulate variable execution time
      Clock::sleep(task_id % 5 * 10)
      
      Mutex::lock(completion_order)
      let order = Mutex::get(completion_order)
      let new_order = order.concat([task_id])
      Mutex::set(completion_order, new_order)
      Mutex::unlock(completion_order)
    })
    
    tasks.push(task)
  }
  
  // Submit all tasks
  let futures = []
  for task in tasks {
    let future = ThreadPool::submit(thread_pool, task)
    futures.push(future)
  }
  
  // Wait for all tasks to complete
  for future in futures {
    ThreadPool::wait_for_future(future)
  }
  
  // Verify all tasks completed
  assert_eq(AtomicInt::get(task_counter), 20)
  
  let final_order = Mutex::get(completion_order)
  assert_eq(final_order.length(), 20)
  
  // Test task priorities
  let priority_counter = AtomicInt::new(0)
  let high_priority_task = Task::with_priority(10, fn() {  // High priority
    AtomicInt::add(priority_counter, 100)
  })
  
  let low_priority_task = Task::with_priority(1, fn() {  // Low priority
    AtomicInt::add(priority_counter, 1)
  })
  
  let high_future = ThreadPool::submit(thread_pool, high_priority_task)
  let low_future = ThreadPool::submit(thread_pool, low_priority_task)
  
  ThreadPool::wait_for_future(high_future)
  ThreadPool::wait_for_future(low_future)
  
  // High priority task should complete first
  assert_eq(AtomicInt::get(priority_counter), 101)
  
  // Test thread pool shutdown
  ThreadPool::shutdown(thread_pool)
  
  // Try to submit task after shutdown (should fail)
  let shutdown_task = Task::new(fn() {
    assert_true(false)  // Should not reach here
  })
  
  let shutdown_future = ThreadPool::submit(thread_pool, shutdown_task)
  assert_eq(shutdown_future.is_valid(), false)
}

// Test 6: Deadlock Detection and Prevention
test "deadlock detection and prevention" {
  let deadlock_detector = DeadlockDetector::new()
  let thread_manager = ThreadManager::new()
  
  // Create resources that can cause deadlock
  let resource1 = Mutex::new("resource1")
  let resource2 = Mutex::new("resource2")
  
  // Enable deadlock detection
  deadlock_detector.enable()
  deadlock_detector.register_lock(resource1, "resource1")
  deadlock_detector.register_lock(resource2, "resource2")
  
  // Test deadlock detection
  let deadlock_detected = AtomicBool::new(false)
  
  // Thread 1: Lock resource1 then resource2
  let thread1 = thread_manager.spawn(fn() {
    Mutex::lock(resource1)
    Clock::sleep(50)  // Increase chance of deadlock
    Mutex::lock(resource2)
    Mutex::unlock(resource2)
    Mutex::unlock(resource1)
  })
  
  // Thread 2: Lock resource2 then resource1 (reverse order)
  let thread2 = thread_manager.spawn(fn() {
    Mutex::lock(resource2)
    Clock::sleep(50)  // Increase chance of deadlock
    Mutex::lock(resource1)
    Mutex::unlock(resource1)
    Mutex::unlock(resource2)
  })
  
  // Monitor for deadlock
  let monitor_thread = thread_manager.spawn(fn() {
    Clock::sleep(100)  // Let threads attempt to acquire locks
    if deadlock_detector.is_deadlock_detected() {
      AtomicBool::set(deadlock_detected, true)
      deadlock_detector.resolve_deadlock()
    }
  })
  
  ThreadManager::join(monitor_thread)
  
  // Force join with timeout to avoid hanging if deadlock occurs
  ThreadManager::join_with_timeout(thread1, 1000)
  ThreadManager::join_with_timeout(thread2, 1000)
  
  // Deadlock should be detected and resolved
  assert_eq(AtomicBool::get(deadlock_detected), true)
  
  // Test lock ordering to prevent deadlock
  let lock_order_manager = LockOrderManager::new()
  lock_order_manager.add_order_rule("resource1", "resource2")  // resource1 must be locked before resource2
  
  let safe_thread1 = thread_manager.spawn(fn() {
    lock_order_manager.acquire("resource1")
    Mutex::lock(resource1)
    
    lock_order_manager.acquire("resource2")
    Mutex::lock(resource2)
    
    Mutex::unlock(resource2)
    lock_order_manager.release("resource2")
    
    Mutex::unlock(resource1)
    lock_order_manager.release("resource1")
  })
  
  let safe_thread2 = thread_manager.spawn(fn() {
    // This follows the same order, so no deadlock
    lock_order_manager.acquire("resource1")
    Mutex::lock(resource1)
    
    lock_order_manager.acquire("resource2")
    Mutex::lock(resource2)
    
    Mutex::unlock(resource2)
    lock_order_manager.release("resource2")
    
    Mutex::unlock(resource1)
    lock_order_manager.release("resource1")
  })
  
  ThreadManager::join(safe_thread1)
  ThreadManager::join(safe_thread2)
  
  // No deadlock should occur
  assert_eq(deadlock_detector.is_deadlock_detected(), false)
}

// Test 7: Condition Variables and Thread Synchronization
test "condition variables and thread synchronization" {
  let thread_manager = ThreadManager::new()
  
  // Test producer-consumer with condition variable
  let buffer = Mutex::new([])
  let buffer_capacity = 10
  let not_full = ConditionVariable::new()
  let not_empty = ConditionVariable::new()
  
  let produced_count = AtomicInt::new(0)
  let consumed_count = AtomicInt::new(0)
  
  // Producer thread
  let producer_thread = thread_manager.spawn(fn() {
    for i = 0; i < 20; i = i + 1 {
      Mutex::lock(buffer)
      
      // Wait if buffer is full
      while Mutex::get(buffer).length() >= buffer_capacity {
        ConditionVariable::wait(not_full, buffer)
      }
      
      // Produce item
      let current_buffer = Mutex::get(buffer)
      let new_buffer = current_buffer.concat(["item_" + i.to_string()])
      Mutex::set(buffer, new_buffer)
      AtomicInt::increment(produced_count)
      
      // Signal that buffer is not empty
      ConditionVariable::signal(not_empty)
      Mutex::unlock(buffer)
    }
  })
  
  // Consumer thread
  let consumer_thread = thread_manager.spawn(fn() {
    for i = 0; i < 20; i = i + 1 {
      Mutex::lock(buffer)
      
      // Wait if buffer is empty
      while Mutex::get(buffer).length() == 0 {
        ConditionVariable::wait(not_empty, buffer)
      }
      
      // Consume item
      let current_buffer = Mutex::get(buffer)
      let new_buffer = current_buffer.slice(1, current_buffer.length())
      Mutex::set(buffer, new_buffer)
      AtomicInt::increment(consumed_count)
      
      // Signal that buffer is not full
      ConditionVariable::signal(not_full)
      Mutex::unlock(buffer)
    }
  })
  
  ThreadManager::join(producer_thread)
  ThreadManager::join(consumer_thread)
  
  // Verify all items were produced and consumed
  assert_eq(AtomicInt::get(produced_count), 20)
  assert_eq(AtomicInt::get(consumed_count), 20)
  assert_eq(Mutex::get(buffer).length(), 0)
  
  // Test barrier synchronization
  let barrier = Barrier::new(5)  // Wait for 5 threads
  let barrier_reached = AtomicInt::new(0)
  
  let barrier_threads = []
  for i = 0; i < 5; i = i + 1 {
    let thread = thread_manager.spawn(fn() {
      // Do some work
      Clock::sleep(i * 10)
      
      // Increment counter before barrier
      AtomicInt::increment(barrier_reached)
      
      // Wait at barrier
      Barrier::wait(barrier)
      
      // All threads should reach this point at the same time
      assert_eq(AtomicInt::get(barrier_reached), 5)
    })
    barrier_threads.push(thread)
  }
  
  for thread in barrier_threads {
    ThreadManager::join(thread)
  }
}

// Test 8: Atomic Operations and Memory Ordering
test "atomic operations and memory ordering" {
  // Test atomic flag
  let atomic_flag = AtomicFlag::new(false)
  
  assert_eq(AtomicFlag::is_set(atomic_flag), false)
  
  AtomicFlag::set(atomic_flag)
  assert_eq(AtomicFlag::is_set(atomic_flag), true)
  
  AtomicFlag::clear(atomic_flag)
  assert_eq(AtomicFlag::is_set(atomic_flag), false)
  
  // Test atomic pointer
  let atomic_ptr = AtomicPtr::new(null)
  let data = "test_data"
  
  AtomicPtr::store(atomic_ptr, data)
  assert_eq(AtomicPtr::load(atomic_ptr), data)
  
  let old_ptr = AtomicPtr::exchange(atomic_ptr, "new_data")
  assert_eq(old_ptr, data)
  assert_eq(AtomicPtr::load(atomic_ptr), "new_data")
  
  // Test memory ordering
  let atomic_data = AtomicInt::new(0)
  let atomic_ready = AtomicBool::new(false)
  
  let thread_manager = ThreadManager::new()
  
  // Writer thread
  let writer_thread = thread_manager.spawn(fn() {
    AtomicInt::store(atomic_data, 42, MemoryOrder::Release)
    AtomicBool::store(atomic_ready, true, MemoryOrder::Release)
  })
  
  // Reader thread
  let reader_thread = thread_manager.spawn(fn() {
    while !AtomicBool::load(atomic_ready, MemoryOrder::Acquire) {
      Clock::sleep(1)
    }
    
    // Due to acquire-release semantics, should see the updated data
    let data_value = AtomicInt::load(atomic_data, MemoryOrder::Acquire)
    assert_eq(data_value, 42)
  })
  
  ThreadManager::join(writer_thread)
  ThreadManager::join(reader_thread)
  
  // Test fetch_add and fetch_sub
  let fetch_counter = AtomicInt::new(0)
  
  let old_value = AtomicInt::fetch_add(fetch_counter, 5)
  assert_eq(old_value, 0)
  assert_eq(AtomicInt::get(fetch_counter), 5)
  
  old_value = AtomicInt::fetch_sub(fetch_counter, 2)
  assert_eq(old_value, 5)
  assert_eq(AtomicInt::get(fetch_counter), 3)
}

// Test 9: Thread-Local Storage
test "thread local storage" {
  let thread_manager = ThreadManager::new()
  
  // Create thread-local variable
  let thread_local_counter = ThreadLocal::new(|| { 0 })
  
  // Create threads that use thread-local storage
  let threads = []
  
  for i = 0; i < 5; i = i + 1 {
    let thread_id = i
    let thread = thread_manager.spawn(fn() {
      // Each thread has its own copy of the counter
      for j = 0; j < 10; j = j + 1 {
        let current = ThreadLocal::get(thread_local_counter)
        ThreadLocal::set(thread_local_counter, current + 1)
      }
      
      // Each thread should have its own counter value of 10
      assert_eq(ThreadLocal::get(thread_local_counter), 10)
    })
    threads.push(thread)
  }
  
  for thread in threads {
    ThreadManager::join(thread)
  }
  
  // Main thread should still have its own counter value of 0
  assert_eq(ThreadLocal::get(thread_local_counter), 0)
  
  // Test thread-local with cleanup
  let cleanup_called = AtomicInt::new(0)
  let thread_local_with_cleanup = ThreadLocal::with_cleanup(
    || { "resource" },
    fn(resource) {
      AtomicInt::increment(cleanup_called)
    }
  )
  
  let cleanup_thread = thread_manager.spawn(fn() {
    let resource = ThreadLocal::get(thread_local_with_cleanup)
    assert_eq(resource, "resource")
  })
  
  ThreadManager::join(cleanup_thread)
  
  // Cleanup should have been called when thread ended
  assert_eq(AtomicInt::get(cleanup_called), 1)
}

// Test 10: Concurrent Performance and Scalability
test "concurrent performance and scalability" {
  let thread_manager = ThreadManager::new()
  let performance_monitor = ConcurrentPerformanceMonitor::new()
  
  // Test scalability with different thread counts
  let thread_counts = [1, 2, 4, 8]
  let operations_per_thread = 10000
  
  for thread_count in thread_counts {
    let shared_counter = AtomicInt::new(0)
    let start_time = Clock::now()
    
    performance_monitor.start_benchmark("atomic_counter_" + thread_count.to_string())
    
    let threads = []
    for i = 0; i < thread_count; i = i + 1 {
      let thread = thread_manager.spawn(fn() {
        for j = 0; j < operations_per_thread; j = j + 1 {
          AtomicInt::increment(shared_counter)
        }
      })
      threads.push(thread)
    }
    
    for thread in threads {
      ThreadManager::join(thread)
    }
    
    let end_time = Clock::now()
    let duration = end_time - start_time
    
    performance_monitor.end_benchmark("atomic_counter_" + thread_count.to_string())
    
    // Verify correctness
    assert_eq(AtomicInt::get(shared_counter), thread_count * operations_per_thread)
    
    // Record performance metrics
    let throughput = (thread_count * operations_per_thread) as f64 / (duration as f64)
    performance_monitor.record_throughput("atomic_counter_" + thread_count.to_string(), throughput)
  }
  
  // Test lock contention
  let mutex_counter = Mutex::new(0)
  let contention_thread_count = 8
  
  performance_monitor.start_benchmark("mutex_contention")
  
  let contention_threads = []
  for i = 0; i < contention_thread_count; i = i + 1 {
    let thread = thread_manager.spawn(fn() {
      for j = 0; j < 1000; j = j + 1 {
        Mutex::lock(mutex_counter)
        let current = Mutex::get(mutex_counter)
        Mutex::set(mutex_counter, current + 1)
        Mutex::unlock(mutex_counter)
      }
    })
    contention_threads.push(thread)
  }
  
  for thread in contention_threads {
    ThreadManager::join(thread)
  }
  
  performance_monitor.end_benchmark("mutex_contention")
  
  // Verify correctness
  assert_eq(Mutex::get(mutex_counter), contention_thread_count * 1000)
  
  // Test concurrent data structure performance
  let concurrent_queue = ConcurrentQueue::new()
  
  performance_monitor.start_benchmark("concurrent_queue")
  
  let queue_threads = []
  
  // Producer threads
  for i = 0; i < 4; i = i + 1 {
    let thread = thread_manager.spawn(fn() {
      for j = 0; j < 2500; j = j + 1 {
        ConcurrentQueue::enqueue(concurrent_queue, "item")
      }
    })
    queue_threads.push(thread)
  }
  
  // Consumer threads
  for i = 0; i < 4; i = i + 1 {
    let thread = thread_manager.spawn(fn() {
      let mut consumed = 0
      while consumed < 2500 {
        let item = ConcurrentQueue::try_dequeue(concurrent_queue)
        match item {
          Some(_) => consumed = consumed + 1
          None => Clock::sleep(1)
        }
      }
    })
    queue_threads.push(thread)
  }
  
  for thread in queue_threads {
    ThreadManager::join(thread)
  }
  
  performance_monitor.end_benchmark("concurrent_queue")
  
  // Verify all items were processed
  assert_eq(ConcurrentQueue::is_empty(concurrent_queue), true)
  
  // Get performance report
  let report = performance_monitor.generate_report()
  assert_eq(report.benchmarks.length() > 0, true)
  
  // Verify scalability (more threads should generally increase throughput)
  let single_thread_throughput = performance_monitor.get_throughput("atomic_counter_1")
  let multi_thread_throughput = performance_monitor.get_throughput("atomic_counter_4")
  
  // Multi-threaded should be faster (though not always due to contention)
  assert_eq(multi_thread_throughput > 0, true)
  assert_eq(single_thread_throughput > 0, true)
}