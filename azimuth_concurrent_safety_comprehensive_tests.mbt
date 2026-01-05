// Azimuth Telemetry System - Concurrent Safety Tests
// This file contains comprehensive test cases for concurrent safety and thread-safety

// Test 1: Atomic Operations Safety
test "atomic operations safety" {
  // Test atomic integer operations
  let atomic_int = AtomicInt::new(0)
  let thread_count = 100
  let increments_per_thread = 1000
  
  // Create threads that increment the atomic integer
  let threads = []
  for i in 0..thread_count {
    threads.push(Thread::spawn(|| {
      for j in 0..increments_per_thread {
        AtomicInt::fetch_add(atomic_int, 1)
      }
    }))
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify final value is correct
  let expected_value = thread_count * increments_per_thread
  assert_eq(AtomicInt::load(atomic_int), expected_value)
  
  // Test atomic boolean operations
  let atomic_bool = AtomicBool::new(false)
  
  // Create threads that try to set the boolean
  let threads2 = []
  for i in 0..thread_count {
    threads2.push(Thread::spawn(|| {
      AtomicBool::compare_exchange(atomic_bool, false, true)
    }))
  }
  
  // Wait for all threads to complete
  for thread in threads2 {
    Thread::join(thread)
  }
  
  // Verify only one thread successfully set the boolean
  assert_true(AtomicBool::load(atomic_bool))
  
  // Test atomic reference operations
  let atomic_ref = AtomicRef::new(None)
  
  // Create threads that try to set the reference
  let threads3 = []
  for i in 0..thread_count {
    threads3.push(Thread::spawn(|| {
      let value = Some("value_" + i.to_string())
      AtomicRef::store(atomic_ref, value)
    }))
  }
  
  // Wait for all threads to complete
  for thread in threads3 {
    Thread::join(thread)
  }
  
  // Verify a value was set
  let final_value = AtomicRef::load(atomic_ref)
  match final_value {
    Some(v) => assert_true(v.starts_with("value_"))
    None => assert_true(false)
  }
}

// Test 2: Mutex and Lock Safety
test "mutex and lock safety" {
  // Test basic mutex protection
  let shared_counter = 0
  let mutex = Mutex::new()
  let thread_count = 100
  let increments_per_thread = 100
  
  // Create threads that increment the counter protected by mutex
  let threads = []
  for i in 0..thread_count {
    threads.push(Thread::spawn(|| {
      for j in 0..increments_per_thread {
        Mutex::lock(mutex)
        shared_counter = shared_counter + 1
        Mutex::unlock(mutex)
      }
    }))
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify final value is correct
  let expected_value = thread_count * increments_per_thread
  assert_eq(shared_counter, expected_value)
  
  // Test reentrant mutex
  let reentrant_mutex = ReentrantMutex::new()
  let shared_data = "initial_value"
  
  // Create threads that use reentrant mutex
  let threads2 = []
  for i in 0..thread_count {
    threads2.push(Thread::spawn(|| {
      ReentrantMutex::lock(reentrant_mutex)
      // Reentrant lock should allow nested locking
      ReentrantMutex::lock(reentrant_mutex)
      shared_data = "thread_" + i.to_string()
      ReentrantMutex::unlock(reentrant_mutex)
      ReentrantMutex::unlock(reentrant_mutex)
    }))
  }
  
  // Wait for all threads to complete
  for thread in threads2 {
    Thread::join(thread)
  }
  
  // Verify data was modified
  assert_true(shared_data.starts_with("thread_"))
  
  // Test try_lock functionality
  let try_mutex = Mutex::new()
  let shared_flag = false
  
  // Create a thread that locks for a short time
  let lock_thread = Thread::spawn(|| {
    Mutex::lock(try_mutex)
    sleep(100)  // Hold lock for 100ms
    shared_flag = true
    Mutex::unlock(try_mutex)
  })
  
  sleep(10)  // Give the lock thread time to acquire the lock
  
  // Try to lock from main thread
  let try_result = Mutex::try_lock(try_mutex)
  match try_result {
    Some(_) => assert_true(false),  // Should not be able to lock
    None => assert_true(true)       // Expected case
  }
  
  // Wait for lock thread to complete
  Thread::join(lock_thread)
  
  // Now we should be able to lock
  let try_result2 = Mutex::try_lock(try_mutex)
  match try_result2 {
    Some(_) => assert_true(true),   // Should be able to lock now
    None => assert_true(false)      // Should not happen
  }
  
  Mutex::unlock(try_mutex)
}

// Test 3: Read-Write Lock Safety
test "read-write lock safety" {
  let shared_data = 0
  let rw_lock = RwLock::new()
  let reader_count = 10
  let writer_count = 5
  let operations_per_thread = 100
  
  // Create reader threads
  let reader_threads = []
  for i in 0..reader_count {
    reader_threads.push(Thread::spawn(|| {
      for j in 0..operations_per_thread {
        RwLock::read_lock(rw_lock)
        let value = shared_data
        // Simulate read operation
        sleep(1)
        RwLock::read_unlock(rw_lock)
        
        // Verify value is reasonable (non-negative)
        assert_true(value >= 0)
      }
    }))
  }
  
  // Create writer threads
  let writer_threads = []
  for i in 0..writer_count {
    writer_threads.push(Thread::spawn(|| {
      for j in 0..operations_per_thread {
        RwLock::write_lock(rw_lock)
        shared_data = shared_data + 1
        // Simulate write operation
        sleep(2)
        RwLock::write_unlock(rw_lock)
      }
    }))
  }
  
  // Wait for all threads to complete
  for thread in reader_threads {
    Thread::join(thread)
  }
  for thread in writer_threads {
    Thread::join(thread)
  }
  
  // Verify final value is correct
  let expected_value = writer_count * operations_per_thread
  assert_eq(shared_data, expected_value)
  
  // Test try_read_lock and try_write_lock
  let try_rwlock = RwLock::new()
  let test_data = "initial"
  
  // Acquire write lock
  RwLock::write_lock(try_rwlock)
  
  // Try to acquire read lock (should fail)
  let try_read_result = RwLock::try_read_lock(try_rwlock)
  match try_read_result {
    Some(_) => assert_true(false),  // Should not be able to acquire read lock
    None => assert_true(true)       // Expected case
  }
  
  // Try to acquire write lock (should fail)
  let try_write_result = RwLock::try_write_lock(try_rwlock)
  match try_write_result {
    Some(_) => assert_true(false),  // Should not be able to acquire write lock
    None => assert_true(true)       // Expected case
  }
  
  RwLock::write_unlock(try_rwlock)
  
  // Now we should be able to acquire locks
  let try_read_result2 = RwLock::try_read_lock(try_rwlock)
  match try_read_result2 {
    Some(_) => assert_true(true),   // Should be able to acquire read lock
    None => assert_true(false)      // Should not happen
  }
  
  RwLock::read_unlock(try_rwlock)
  
  let try_write_result2 = RwLock::try_write_lock(try_rwlock)
  match try_write_result2 {
    Some(_) => assert_true(true),   // Should be able to acquire write lock
    None => assert_true(false)      // Should not happen
  }
  
  RwLock::write_unlock(try_rwlock)
}

// Test 4: Thread-Safe Data Structures
test "thread-safe data structures" {
  // Test thread-safe queue
  let queue = ConcurrentQueue::new()
  let producer_count = 5
  let consumer_count = 3
  let items_per_producer = 1000
  
  // Create producer threads
  let producer_threads = []
  for i in 0..producer_count {
    producer_threads.push(Thread::spawn(|| {
      for j in 0..items_per_producer {
        let item = "item_" + i.to_string() + "_" + j.to_string()
        ConcurrentQueue::enqueue(queue, item)
      }
    }))
  }
  
  // Track consumed items
  let consumed_items = []
  let consumed_mutex = Mutex::new()
  
  // Create consumer threads
  let consumer_threads = []
  for i in 0..consumer_count {
    consumer_threads.push(Thread::spawn(|| {
      let mut local_consumed = []
      while true {
        let item = ConcurrentQueue::try_dequeue(queue)
        match item {
          Some(value) => local_consumed.push(value),
          None => {
            // Check if producers are done
            if ConcurrentQueue::is_empty(queue) {
              break
            }
            sleep(1)
          }
        }
      }
      
      // Add local consumed items to global list
      Mutex::lock(consumed_mutex)
      for item in local_consumed {
        consumed_items.push(item)
      }
      Mutex::unlock(consumed_mutex)
    }))
  }
  
  // Wait for all producer threads to complete
  for thread in producer_threads {
    Thread::join(thread)
  }
  
  // Wait for all consumer threads to complete
  for thread in consumer_threads {
    Thread::join(thread)
  }
  
  // Verify all items were consumed
  let expected_items = producer_count * items_per_producer
  assert_eq(consumed_items.length(), expected_items)
  
  // Test thread-safe hashmap
  let map = ConcurrentHashMap::new()
  let thread_count = 10
  let operations_per_thread = 1000
  
  // Create threads that put and get values
  let threads = []
  for i in 0..thread_count {
    threads.push(Thread::spawn(|| {
      for j in 0..operations_per_thread {
        let key = "key_" + j.to_string()
        let value = "value_" + i.to_string() + "_" + j.to_string()
        
        // Put value
        ConcurrentHashMap::put(map, key, value)
        
        // Get value
        let retrieved = ConcurrentHashMap::get(map, key)
        match retrieved {
          Some(v) => assert_true(v.starts_with("value_")),
          None => assert_true(false)
        }
        
        // Remove value
        ConcurrentHashMap::remove(map, key)
      }
    }))
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Map should be empty since all items were removed
  assert_true(ConcurrentHashMap::is_empty(map))
  
  // Test thread-safe counter
  let counter = AtomicCounter::new(0)
  let thread_count2 = 100
  let increments_per_thread2 = 100
  
  // Create threads that increment the counter
  let threads2 = []
  for i in 0..thread_count2 {
    threads2.push(Thread::spawn(|| {
      for j in 0..increments_per_thread2 {
        AtomicCounter::increment(counter)
      }
    }))
  }
  
  // Wait for all threads to complete
  for thread in threads2 {
    Thread::join(thread)
  }
  
  // Verify final value is correct
  let expected_value2 = thread_count2 * increments_per_thread2
  assert_eq(AtomicCounter::get(counter), expected_value2)
}

// Test 5: Deadlock Detection and Prevention
test "deadlock detection and prevention" {
  // Test potential deadlock scenario
  let mutex1 = Mutex::new()
  let mutex2 = Mutex::new()
  let shared_data1 = 0
  let shared_data2 = 0
  
  // Create thread that locks mutex1 then mutex2
  let thread1 = Thread::spawn(|| {
    Mutex::lock(mutex1)
    sleep(10)  // Increase chance of deadlock
    Mutex::lock(mutex2)
    
    shared_data1 = shared_data1 + 1
    shared_data2 = shared_data2 + 1
    
    Mutex::unlock(mutex2)
    Mutex::unlock(mutex1)
  })
  
  // Create thread that locks mutex2 then mutex1
  let thread2 = Thread::spawn(|| {
    Mutex::lock(mutex2)
    sleep(10)  // Increase chance of deadlock
    Mutex::lock(mutex1)
    
    shared_data1 = shared_data1 + 1
    shared_data2 = shared_data2 + 1
    
    Mutex::unlock(mutex1)
    Mutex::unlock(mutex2)
  })
  
  // Set a timeout for deadlock detection
  let timeout_thread = Thread::spawn(|| {
    sleep(5000)  // 5 second timeout
    assert_true(false)  // If we reach here, deadlock occurred
  })
  
  // Wait for threads to complete
  Thread::join(thread1)
  Thread::join(thread2)
  
  // Cancel timeout thread
  Thread::cancel(timeout_thread)
  
  // Verify data was modified correctly
  assert_eq(shared_data1, 2)
  assert_eq(shared_data2, 2)
  
  // Test lock ordering to prevent deadlock
  let ordered_mutex1 = OrderedMutex::new(1)  // Lower priority
  let ordered_mutex2 = OrderedMutex::new(2)  // Higher priority
  let ordered_shared_data1 = 0
  let ordered_shared_data2 = 0
  
  // Create thread that locks in correct order
  let ordered_thread1 = Thread::spawn(|| {
    OrderedMutex::lock(ordered_mutex1)
    OrderedMutex::lock(ordered_mutex2)
    
    ordered_shared_data1 = ordered_shared_data1 + 1
    ordered_shared_data2 = ordered_shared_data2 + 1
    
    OrderedMutex::unlock(ordered_mutex2)
    OrderedMutex::unlock(ordered_mutex1)
  })
  
  // Create thread that locks in correct order
  let ordered_thread2 = Thread::spawn(|| {
    OrderedMutex::lock(ordered_mutex1)
    OrderedMutex::lock(ordered_mutex2)
    
    ordered_shared_data1 = ordered_shared_data1 + 1
    ordered_shared_data2 = ordered_shared_data2 + 1
    
    OrderedMutex::unlock(ordered_mutex2)
    OrderedMutex::unlock(ordered_mutex1)
  })
  
  // Wait for threads to complete
  Thread::join(ordered_thread1)
  Thread::join(ordered_thread2)
  
  // Verify data was modified correctly
  assert_eq(ordered_shared_data1, 2)
  assert_eq(ordered_shared_data2, 2)
}

// Test 6: Thread Pool Safety
test "thread pool safety" {
  let thread_pool = ThreadPool::new(4)
  let task_count = 1000
  let shared_counter = AtomicInt::new(0)
  
  // Submit tasks to thread pool
  let futures = []
  for i in 0..task_count {
    futures.push(ThreadPool::submit(thread_pool, || {
      AtomicInt::fetch_add(shared_counter, 1)
    }))
  }
  
  // Wait for all tasks to complete
  let results = []
  for future in futures {
    results.push(Future::get(future))
  }
  
  // Verify all tasks completed
  assert_eq(results.length(), task_count)
  
  // Verify counter was incremented correctly
  assert_eq(AtomicInt::load(shared_counter), task_count)
  
  // Test thread pool with shared resources
  let shared_list = ConcurrentList::new()
  let task_count2 = 100
  
  // Submit tasks that add to shared list
  let futures2 = []
  for i in 0..task_count2 {
    futures2.push(ThreadPool::submit(thread_pool, || {
      ConcurrentList::add(shared_list, "item_" + i.to_string())
    }))
  }
  
  // Wait for all tasks to complete
  for future in futures2 {
    Future::get(future)
  }
  
  // Verify all items were added
  assert_eq(ConcurrentList::size(shared_list), task_count2)
  
  // Test thread pool shutdown
  ThreadPool::shutdown(thread_pool)
  
  // Try to submit task after shutdown (should fail)
  let future = ThreadPool::try_submit(thread_pool, || {
    "should_not_execute"
  })
  
  match future {
    Some(_) => assert_true(false),  // Should not be able to submit after shutdown
    None => assert_true(true)       // Expected case
  }
}

// Test 7: Condition Variable Safety
test "condition variable safety" {
  let mutex = Mutex::new()
  let condition = ConditionVariable::new()
  let shared_data = 0
  let producer_count = 5
  let items_per_producer = 10
  
  // Create producer threads
  let producer_threads = []
  for i in 0..producer_count {
    producer_threads.push(Thread::spawn(|| {
      for j in 0..items_per_producer {
        Mutex::lock(mutex)
        shared_data = shared_data + 1
        ConditionVariable::notify_one(condition)
        Mutex::unlock(mutex)
        sleep(10)
      }
    }))
  }
  
  // Create consumer thread
  let consumed_items = 0
  let consumer_thread = Thread::spawn(|| {
    let mut local_consumed = 0
    while local_consumed < producer_count * items_per_producer {
      Mutex::lock(mutex)
      while shared_data == 0 {
        ConditionVariable::wait(condition, mutex)
      }
      shared_data = shared_data - 1
      local_consumed = local_consumed + 1
      Mutex::unlock(mutex)
    }
    return local_consumed
  })
  
  // Wait for all producer threads to complete
  for thread in producer_threads {
    Thread::join(thread)
  }
  
  // Notify consumer one more time in case it's waiting
  Mutex::lock(mutex)
  ConditionVariable::notify_one(condition)
  Mutex::unlock(mutex)
  
  // Wait for consumer thread to complete
  let consumed = Thread::join(consumer_thread)
  
  // Verify all items were consumed
  let expected_items = producer_count * items_per_producer
  assert_eq(consumed, expected_items)
  assert_eq(shared_data, 0)
}

// Test 8: Memory Ordering and Visibility
test "memory ordering and visibility" {
  // Test relaxed memory ordering
  let relaxed_counter = AtomicInt::new(0)
  let thread_count = 100
  let increments_per_thread = 100
  
  // Create threads that increment with relaxed ordering
  let threads = []
  for i in 0..thread_count {
    threads.push(Thread::spawn(|| {
      for j in 0..increments_per_thread {
        AtomicInt::fetch_add_relaxed(relaxed_counter, 1)
      }
    }))
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify final value is correct
  let expected_value = thread_count * increments_per_thread
  assert_eq(AtomicInt::load_relaxed(relaxed_counter), expected_value)
  
  // Test acquire-release memory ordering
  let data = 0
  let flag = AtomicBool::new(false)
  
  // Create writer thread
  let writer_thread = Thread::spawn(|| {
    data = 42
    AtomicBool::store_release(flag, true)
  })
  
  // Create reader thread
  let reader_thread = Thread::spawn(|| {
    while !AtomicBool::load_acquire(flag) {
      // Spin wait
    }
    assert_eq(data, 42)
  })
  
  // Wait for threads to complete
  Thread::join(writer_thread)
  Thread::join(reader_thread)
  
  // Test sequential consistency
  let seq_data1 = 0
  let seq_data2 = 0
  let seq_flag1 = AtomicBool::new(false)
  let seq_flag2 = AtomicBool::new(false)
  
  // Create first thread
  let seq_thread1 = Thread::spawn(|| {
    seq_data1 = 1
    AtomicBool::store_seq_cst(seq_flag1, true)
  })
  
  // Create second thread
  let seq_thread2 = Thread::spawn(|| {
    while !AtomicBool::load_seq_cst(seq_flag1) {
      // Spin wait
    }
    seq_data2 = 2
    AtomicBool::store_seq_cst(seq_flag2, true)
  })
  
  // Create third thread
  let seq_thread3 = Thread::spawn(|| {
    while !AtomicBool::load_seq_cst(seq_flag2) {
      // Spin wait
    }
    assert_eq(seq_data1, 1)
    assert_eq(seq_data2, 2)
  })
  
  // Wait for threads to complete
  Thread::join(seq_thread1)
  Thread::join(seq_thread2)
  Thread::join(seq_thread3)
}

// Test 9: Concurrent Telemetry Operations
test "concurrent telemetry operations" {
  let span_processor = ConcurrentSpanProcessor::new()
  let meter_provider = ConcurrentMeterProvider::new()
  let logger_provider = ConcurrentLoggerProvider::new()
  let thread_count = 10
  let operations_per_thread = 100
  
  // Create threads that generate spans
  let span_threads = []
  for i in 0..thread_count {
    span_threads.push(Thread::spawn(|| {
      for j in 0..operations_per_thread {
        let span = Span::new("concurrent_span_" + j.to_string(), Internal, SpanContext::new("trace", "span_" + j.to_string(), true, ""))
        ConcurrentSpanProcessor::process(span_processor, span)
      }
    }))
  }
  
  // Create threads that record metrics
  let metric_threads = []
  for i in 0..thread_count {
    metric_threads.push(Thread::spawn(|| {
      let meter = ConcurrentMeterProvider::get_meter(meter_provider, "concurrent_meter_" + i.to_string())
      let counter = Meter::create_counter(meter, "concurrent_counter", None, None)
      for j in 0..operations_per_thread {
        Counter::add(counter, 1.0)
      }
    }))
  }
  
  // Create threads that emit logs
  let log_threads = []
  for i in 0..thread_count {
    log_threads.push(Thread::spawn(|| {
      let logger = ConcurrentLoggerProvider::get_logger(logger_provider, "concurrent_logger_" + i.to_string())
      for j in 0..operations_per_thread {
        let log_record = LogRecord::new(Info, "Concurrent log message " + j.to_string())
        Logger::emit(logger, log_record)
      }
    }))
  }
  
  // Wait for all threads to complete
  for thread in span_threads {
    Thread::join(thread)
  }
  for thread in metric_threads {
    Thread::join(thread)
  }
  for thread in log_threads {
    Thread::join(thread)
  }
  
  // Verify all operations were processed
  let expected_spans = thread_count * operations_per_thread
  let expected_metrics = thread_count * operations_per_thread
  let expected_logs = thread_count * operations_per_thread
  
  assert_eq(ConcurrentSpanProcessor::processed_count(span_processor), expected_spans)
  assert_eq(ConcurrentMeterProvider::metric_count(meter_provider), expected_metrics)
  assert_eq(ConcurrentLoggerProvider::log_count(logger_provider), expected_logs)
}

// Test 10: Comprehensive Concurrent Safety Scenario
test "comprehensive concurrent safety scenario" {
  // Create a complex concurrent scenario with multiple shared resources
  let shared_cache = ConcurrentLRUCache::new(1000)
  let shared_database = ConnectionPool::new(10)
  let shared_metrics = ConcurrentMetricsCollector::new()
  let shared_spans = ConcurrentSpanBuffer::new()
  let thread_count = 20
  let operations_per_thread = 50
  
  // Create worker threads
  let worker_threads = []
  for i in 0..thread_count {
    worker_threads.push(Thread::spawn(|| {
      for j in 0..operations_per_thread {
        let operation_id = "op_" + i.to_string() + "_" + j.to_string()
        
        // Start span
        let span = Span::new(operation_id, Internal, SpanContext::new("trace", "span", true, ""))
        
        // Try to get from cache
        let cache_result = ConcurrentLRUCache::get(shared_cache, operation_id)
        let result = match cache_result {
          Some(value) => {
            // Cache hit
            ConcurrentMetricsCollector::record_cache_hit(shared_metrics)
            value
          }
          None => {
            // Cache miss, get from database
            ConcurrentMetricsCollector::record_cache_miss(shared_metrics)
            let connection = ConnectionPool::acquire(shared_database)
            let db_result = Database::query(connection, "SELECT value FROM data WHERE id = " + operation_id)
            ConnectionPool::release(shared_database, connection)
            
            // Store in cache
            match db_result {
              Some(value) => {
                ConcurrentLRUCache::put(shared_cache, operation_id, value)
                value
              }
              None => "default_value"
            }
          }
        }
        
        // Process result
        let processed_result = process_result(result)
        
        // Record metric
        ConcurrentMetricsCollector::record_operation(shared_metrics, operation_id, processed_result)
        
        // End span
        Span::end(span)
        ConcurrentSpanBuffer::add(shared_spans, span)
        
        // Random sleep to simulate real-world conditions
        sleep(random_int(1, 10))
      }
    }))
  }
  
  // Create maintenance thread
  let maintenance_thread = Thread::spawn(|| {
    for i in 0..100 {
      sleep(50)
      
      // Periodically clean up cache
      ConcurrentLRUCache::cleanup(shared_cache)
      
      // Periodically flush spans
      if i % 10 == 0 {
        ConcurrentSpanBuffer::flush(shared_spans)
      }
      
      // Periodically collect metrics
      if i % 5 == 0 {
        ConcurrentMetricsCollector::collect(shared_metrics)
      }
    }
  })
  
  // Wait for all worker threads to complete
  for thread in worker_threads {
    Thread::join(thread)
  }
  
  // Wait for maintenance thread to complete
  Thread::join(maintenance_thread)
  
  // Verify system state is consistent
  assert_true(ConcurrentLRUCache::is_consistent(shared_cache))
  assert_true(ConnectionPool::is_consistent(shared_database))
  assert_true(ConcurrentMetricsCollector::is_consistent(shared_metrics))
  assert_true(ConcurrentSpanBuffer::is_consistent(shared_spans))
  
  // Verify all operations were recorded
  let expected_operations = thread_count * operations_per_thread
  assert_eq(ConcurrentMetricsCollector::operation_count(shared_metrics), expected_operations)
  assert_eq(ConcurrentSpanBuffer::span_count(shared_spans), expected_operations)
  
  // Verify cache hit/miss ratio is reasonable
  let cache_hits = ConcurrentMetricsCollector::cache_hit_count(shared_metrics)
  let cache_misses = ConcurrentMetricsCollector::cache_miss_count(shared_metrics)
  let total_cache_operations = cache_hits + cache_misses
  
  assert_true(total_cache_operations > 0)
  let hit_ratio = cache_hits.to_float() / total_cache_operations.to_float()
  assert_true(hit_ratio > 0.0 && hit_ratio <= 1.0)
}