// Azimuth Telemetry System - Concurrent Safety and Thread Safety Tests
// This file contains comprehensive test cases for concurrent operations and thread safety

// Test 1: Thread-Safe Span Operations
test "thread-safe span operations" {
  let span_manager = ThreadSafeSpanManager::new()
  
  // Test concurrent span creation
  let threads = []
  for i in 0..=10 {
    let thread_id = "thread_" + i.to_string()
    let thread = Thread::spawn(fn() {
      for j in 0..=100 {
        let span_ctx = SpanContext::new("trace123", thread_id + "_span_" + j.to_string(), true, "active")
        let span = ThreadSafeSpanManager::create_span(span_manager, "concurrent_operation", Internal, span_ctx)
        
        // Concurrent attribute setting
        ThreadSafeSpanManager::set_attribute(span_manager, span, "thread.id", StringValue(thread_id))
        ThreadSafeSpanManager::set_attribute(span_manager, span, "iteration", IntValue(j))
        
        // Concurrent event addition
        ThreadSafeSpanManager::add_event(span_manager, span, "operation_step", Some([
          ("step", IntValue(j)),
          ("thread", StringValue(thread_id))
        ]))
        
        ThreadSafeSpanManager::end_span(span_manager, span)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify all spans were created and processed correctly
  let stats = ThreadSafeSpanManager::get_stats(span_manager)
  assert_eq(stats.total_spans_created, 1100) // 11 threads * 100 spans each
  assert_eq(stats.total_spans_ended, 1100)
  assert_eq(stats.concurrent_operations, 11)
}

// Test 2: Thread-Safe Metrics Collection
test "thread-safe metrics collection" {
  let metrics_manager = ThreadSafeMetricsManager::new()
  
  // Create shared metrics
  let counter = ThreadSafeMetricsManager::create_counter(metrics_manager, "concurrent_operations", 
    Some("Concurrent operations count"), Some("operations"))
  let histogram = ThreadSafeMetricsManager::create_histogram(metrics_manager, "operation_duration",
    Some("Operation duration"), Some("milliseconds"))
  
  // Test concurrent metric updates
  let threads = []
  for i in 0..=20 {
    let thread_id = "metrics_thread_" + i.to_string()
    let thread = Thread::spawn(fn() {
      for j in 0..=50 {
        // Concurrent counter increments
        ThreadSafeMetricsManager::counter_add(metrics_manager, counter, 1.0, Some([
          ("thread.id", StringValue(thread_id)),
          ("operation.type", StringValue("concurrent_test"))
        ]))
        
        // Concurrent histogram recordings
        let duration = (j % 10 + 1).to_float() * 10.0 // 10ms to 100ms
        ThreadSafeMetricsManager::histogram_record(metrics_manager, histogram, duration, Some([
          ("thread.id", StringValue(thread_id))
        ]))
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify metrics integrity
  let counter_snapshot = ThreadSafeMetricsManager::get_counter_snapshot(metrics_manager, counter)
  assert_eq(counter_snapshot.total_value, 1050.0) // 21 threads * 50 operations
  
  let histogram_snapshot = ThreadSafeMetricsManager::get_histogram_snapshot(metrics_manager, histogram)
  assert_eq(histogram_snapshot.total_count, 1050)
  assert_true(histogram_snapshot.sum > 0.0)
  
  // Verify no data corruption occurred
  let metrics_stats = ThreadSafeMetricsManager::get_stats(metrics_manager)
  assert_eq(metrics_stats.total_metric_updates, 2100) // 1050 counter + 1050 histogram
  assert_eq(metrics_stats.concurrent_writers, 21)
}

// Test 3: Thread-Safe Log Emission
test "thread-safe log emission" {
  let log_manager = ThreadSafeLogManager::new()
  
  // Test concurrent log emission
  let threads = []
  for i in 0..=15 {
    let thread_id = "log_thread_" + i.to_string()
    let thread = Thread::spawn(fn() {
      for j in 0..=30 {
        let severity = match j % 4 {
          0 => Info,
          1 => Warn,
          2 => Error,
          _ => Debug
        }
        
        let log_record = LogRecord::new_with_attributes(
          severity,
          "Log message from " + thread_id + " iteration " + j.to_string(),
          [
            ("thread.id", StringValue(thread_id)),
            ("iteration", IntValue(j)),
            ("timestamp", IntValue(1234567890 + j))
          ]
        )
        
        ThreadSafeLogManager::emit_log(log_manager, log_record)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify log integrity
  let log_stats = ThreadSafeLogManager::get_stats(log_manager)
  assert_eq(log_stats.total_logs_emitted, 480) // 16 threads * 30 logs each
  assert_eq(log_stats.concurrent_emitters, 16)
  
  // Verify log count by severity
  let info_count = ThreadSafeLogManager::get_log_count_by_severity(log_manager, Info)
  let warn_count = ThreadSafeLogManager::get_log_count_by_severity(log_manager, Warn)
  let error_count = ThreadSafeLogManager::get_log_count_by_severity(log_manager, Error)
  let debug_count = ThreadSafeLogManager::get_log_count_by_severity(log_manager, Debug)
  
  assert_eq(info_count, 120)
  assert_eq(warn_count, 120)
  assert_eq(error_count, 120)
  assert_eq(debug_count, 120)
}

// Test 4: Thread-Safe Context Propagation
test "thread-safe context propagation" {
  let context_manager = ThreadSafeContextManager::new()
  
  // Create initial context with baggage
  let root_ctx = Context::root()
  let correlation_key = ContextKey::new("correlation_id")
  let initial_ctx = Context::with_value(root_ctx, correlation_key, "global_correlation_123")
  
  // Test concurrent context propagation
  let threads = []
  for i in 0..=8 {
    let thread_id = "ctx_thread_" + i.to_string()
    let thread = Thread::spawn(fn() {
      // Each thread gets a copy of the initial context
      let thread_ctx = ThreadSafeContextManager::copy_context(context_manager, initial_ctx)
      
      // Add thread-specific baggage
      let thread_key = ContextKey::new("thread_id")
      let thread_ctx = Context::with_value(thread_ctx, thread_key, thread_id)
      
      for j in 0..=20 {
        // Create nested context for each operation
        let operation_key = ContextKey::new("operation_id")
        let operation_ctx = Context::with_value(thread_ctx, operation_key, "op_" + j.to_string())
        
        // Verify all context values are accessible
        let correlation_value = Context::get(operation_ctx, correlation_key)
        match correlation_value {
          Some(value) => assert_eq(value, "global_correlation_123")
          None => assert_true(false)
        }
        
        let thread_value = Context::get(operation_ctx, thread_key)
        match thread_value {
          Some(value) => assert_eq(value, thread_id)
          None => assert_true(false)
        }
        
        let operation_value = Context::get(operation_ctx, operation_key)
        match operation_value {
          Some(value) => assert_eq(value, "op_" + j.to_string())
          None => assert_true(false)
        }
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify context manager statistics
  let ctx_stats = ThreadSafeContextManager::get_stats(context_manager)
  assert_eq(ctx_stats.total_context_copies, 9)
  assert_eq(ctx_stats.total_context_creations, 189) // 9 threads * 21 contexts each
  assert_eq(ctx_stats.concurrent_context_operations, 9)
}

// Test 5: Thread-Safe Resource Pool
test "thread-safe resource pool" {
  let resource_pool = ThreadSafeResourcePool::new(10) // 10 resources in pool
  
  // Test concurrent resource acquisition and release
  let threads = []
  for i in 0..=20 {
    let thread_id = "pool_thread_" + i.to_string()
    let thread = Thread::spawn(fn() {
      for j in 0..=10 {
        // Acquire resource from pool
        let resource = ThreadSafeResourcePool::acquire(resource_pool, 5000) // 5s timeout
        match resource {
          Some(res) => {
            // Simulate resource usage
            Thread::sleep(10) // 10ms operation
            
            // Verify resource is valid
            assert_true(ThreadSafeResourcePool::is_resource_valid(res))
            
            // Release resource back to pool
            ThreadSafeResourcePool::release(resource_pool, res)
          }
          None => {
            // Handle timeout case
            assert_true(true) // Expected under high contention
          }
        }
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify pool integrity
  let pool_stats = ThreadSafeResourcePool::get_stats(resource_pool)
  assert_eq(pool_stats.total_acquisitions, 210) // 21 threads * 10 acquisitions
  assert_eq(pool_stats.total_releases, pool_stats.total_acquisitions - pool_stats.timeouts)
  assert_eq(pool_stats.available_resources, 10) // All resources should be returned
  assert_true(pool_stats.max_concurrent_acquisitions <= 10)
}

// Test 6: Thread-Safe Batch Processing
test "thread-safe batch processing" {
  let batch_processor = ThreadSafeBatchProcessor::new(100, 1000) // 100 items max, 1s timeout
  
  // Test concurrent batch submission
  let threads = []
  for i in 0..=12 {
    let thread_id = "batch_thread_" + i.to_string()
    let thread = Thread::spawn(fn() {
      for j in 0..=25 {
        let item = BatchItem::new(thread_id + "_item_" + j.to_string(), [
          ("thread.id", StringValue(thread_id)),
          ("item.index", IntValue(j)),
          ("timestamp", IntValue(1234567890 + j))
        ])
        
        ThreadSafeBatchProcessor::submit_item(batch_processor, item)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Force process remaining items
  ThreadSafeBatchProcessor::flush(batch_processor)
  
  // Verify batch processing integrity
  let batch_stats = ThreadSafeBatchProcessor::get_stats(batch_processor)
  assert_eq(batch_stats.total_items_submitted, 325) // 13 threads * 25 items
  assert_eq(batch_stats.total_items_processed, 325)
  assert_eq(batch_stats.total_batches_created, 4) // 325 items / 100 per batch = 4 batches
  assert_true(batch_stats.concurrent_submitters == 13)
}

// Test 7: Thread-Safe Cache Operations
test "thread-safe cache operations" {
  let cache = ThreadSafeCache::new(1000) // 1000 item capacity
  
  // Test concurrent cache operations
  let threads = []
  for i in 0..=16 {
    let thread_id = "cache_thread_" + i.to_string()
    let thread = Thread::spawn(fn() {
      for j in 0..=50 {
        let key = thread_id + "_key_" + j.to_string()
        let value = thread_id + "_value_" + j.to_string()
        
        // Concurrent cache put
        ThreadSafeCache::put(cache, key, value)
        
        // Concurrent cache get
        let retrieved_value = ThreadSafeCache::get(cache, key)
        match retrieved_value {
          Some(v) => assert_eq(v, value)
          None => assert_true(false)
        }
        
        // Test cache size consistency
        if j % 10 == 0 {
          let cache_size = ThreadSafeCache::size(cache)
          assert_true(cache_size > 0)
          assert_true(cache_size <= 1000)
        }
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify cache integrity
  let cache_stats = ThreadSafeCache::get_stats(cache)
  assert_eq(cache_stats.total_puts, 850) // 17 threads * 50 puts
  assert_eq(cache_stats.total_gets, 850)
  assert_eq(cache_stats.cache_hits, 850) // All gets should hit since we put then get immediately
  assert_true(cache_stats.concurrent_operations == 17)
}

// Test 8: Thread-Safe Queue Operations
test "thread-safe queue operations" {
  let queue = ThreadSafeQueue::new()
  
  // Test concurrent enqueue and dequeue
  let producer_threads = []
  let consumer_threads = []
  
  // Start producer threads
  for i in 0..=5 {
    let thread_id = "producer_" + i.to_string()
    let thread = Thread::spawn(fn() {
      for j in 0..=40 {
        let item = QueueItem::new(thread_id + "_item_" + j.to_string(), [
          ("producer", StringValue(thread_id)),
          ("index", IntValue(j))
        ])
        ThreadSafeQueue::enqueue(queue, item)
      }
    })
    producer_threads.push(thread)
  }
  
  // Start consumer threads
  let consumed_count = AtomicInt::new(0)
  for i in 0..=3 {
    let thread = Thread::spawn(fn() {
      let local_count = 0
      while local_count < 60 {
        let item = ThreadSafeQueue::dequeue(queue, 1000) // 1s timeout
        match item {
          Some(queue_item) => {
            local_count = local_count + 1
            AtomicInt::increment(consumed_count)
          }
          None => {
            // No item available, continue trying
          }
        }
      }
    })
    consumer_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in producer_threads {
    Thread::join(thread)
  }
  
  for thread in consumer_threads {
    Thread::join(thread)
  }
  
  // Verify queue integrity
  let queue_stats = ThreadSafeQueue::get_stats(queue)
  assert_eq(queue_stats.total_enqueued, 240) // 6 producers * 40 items
  assert_eq(queue_stats.total_dequeued, AtomicInt::get(consumed_count))
  assert_eq(queue_stats.remaining_items, queue_stats.total_enqueued - queue_stats.total_dequeued)
  assert_true(queue_stats.concurrent_producers == 6)
  assert_true(queue_stats.concurrent_consumers == 4)
}

// Test 9: Thread-Safe Atomic Operations
test "thread-safe atomic operations" {
  let atomic_counter = ThreadSafeAtomicCounter::new(0)
  let atomic_map = ThreadSafeAtomicMap::new()
  
  // Test concurrent atomic counter operations
  let counter_threads = []
  for i in 0..=10 {
    let thread = Thread::spawn(fn() {
      for j in 0..=100 {
        // Concurrent increments
        ThreadSafeAtomicCounter::increment(atomic_counter)
        
        // Concurrent adds
        ThreadSafeAtomicCounter::add(atomic_counter, j)
        
        // Concurrent compare and swap
        let current = ThreadSafeAtomicCounter::get(atomic_counter)
        if current % 2 == 0 {
          ThreadSafeAtomicCounter::compare_and_swap(atomic_counter, current, current + 10)
        }
      }
    })
    counter_threads.push(thread)
  }
  
  // Test concurrent map operations
  let map_threads = []
  for i in 0..=8 {
    let thread_id = "map_thread_" + i.to_string()
    let thread = Thread::spawn(fn() {
      for j in 0..=50 {
        let key = thread_id + "_key_" + j.to_string()
        let value = "value_" + j.to_string()
        
        // Concurrent map put
        ThreadSafeAtomicMap::put(atomic_map, key, value)
        
        // Concurrent map get
        let retrieved_value = ThreadSafeAtomicMap::get(atomic_map, key)
        match retrieved_value {
          Some(v) => assert_eq(v, value)
          None => assert_true(false)
        }
        
        // Test map size
        if j % 10 == 0 {
          let map_size = ThreadSafeAtomicMap::size(atomic_map)
          assert_true(map_size > 0)
        }
      }
    })
    map_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in counter_threads {
    Thread::join(thread)
  }
  
  for thread in map_threads {
    Thread::join(thread)
  }
  
  // Verify atomic operations integrity
  let final_counter_value = ThreadSafeAtomicCounter::get(atomic_counter)
  assert_true(final_counter_value > 0)
  
  let map_size = ThreadSafeAtomicMap::size(atomic_map)
  assert_eq(map_size, 450) // 9 threads * 50 keys
  
  let atomic_stats = ThreadSafeAtomicOperations::get_stats(atomic_counter, atomic_map)
  assert_eq(atomic_stats.total_counter_operations, 2310) // 11 threads * 101 increments + 11 threads * 101 adds
  assert_eq(atomic_stats.total_map_operations, 900) // 9 threads * 50 puts + 9 threads * 50 gets
}

// Test 10: Thread-Safe Lock-Free Data Structures
test "thread-safe lock-free data structures" {
  let lock_free_stack = LockFreeStack::new()
  let lock_free_queue = LockFreeQueue::new()
  
  // Test concurrent lock-free stack operations
  let stack_threads = []
  for i in 0..=12 {
    let thread_id = "stack_thread_" + i.to_string()
    let thread = Thread::spawn(fn() {
      // Push operations
      for j in 0..=30 {
        let item = StackItem::new(thread_id + "_item_" + j.to_string())
        LockFreeStack::push(lock_free_stack, item)
      }
      
      // Pop operations
      let popped_count = 0
      for j in 0..=20 {
        let item = LockFreeStack::pop(lock_free_stack)
        match item {
          Some(_) => popped_count = popped_count + 1
          None => break // Stack empty
        }
      }
    })
    stack_threads.push(thread)
  }
  
  // Test concurrent lock-free queue operations
  let queue_threads = []
  for i in 0..=8 {
    let thread_id = "lf_queue_thread_" + i.to_string()
    let thread = Thread::spawn(fn() {
      // Enqueue operations
      for j in 0..=25 {
        let item = QueueItem::new(thread_id + "_item_" + j.to_string())
        LockFreeQueue::enqueue(lock_free_queue, item)
      }
      
      // Dequeue operations
      let dequeued_count = 0
      for j in 0..=15 {
        let item = LockFreeQueue::dequeue(lock_free_queue)
        match item {
          Some(_) => dequeued_count = dequeued_count + 1
          None => break // Queue empty
        }
      }
    })
    queue_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in stack_threads {
    Thread::join(thread)
  }
  
  for thread in queue_threads {
    Thread::join(thread)
  }
  
  // Verify lock-free data structures integrity
  let stack_stats = LockFreeStack::get_stats(lock_free_stack)
  assert_eq(stack_stats.total_pushes, 390) // 13 threads * 30 pushes
  assert_true(stack_stats.total_pops > 0)
  assert_true(stack_stats.total_pops <= stack_stats.total_pushes)
  assert_eq(stack_stats.remaining_items, stack_stats.total_pushes - stack_stats.total_pops)
  
  let queue_stats = LockFreeQueue::get_stats(lock_free_queue)
  assert_eq(queue_stats.total_enqueues, 225) // 9 threads * 25 enqueues
  assert_true(queue_stats.total_dequeues > 0)
  assert_true(queue_stats.total_dequeues <= queue_stats.total_enqueues)
  assert_eq(queue_stats.remaining_items, queue_stats.total_enqueues - queue_stats.total_dequeues)
  
  // Verify no memory corruption or data races occurred
  let lock_free_stats = LockFreeDataStructures::get_combined_stats(lock_free_stack, lock_free_queue)
  assert_true(lock_free_stats.total_operations > 0)
  assert_eq(lock_free_stats.concurrent_threads, 22) // 13 stack + 9 queue threads
  assert_true(lock_free_stats.no_corruption_detected)
}