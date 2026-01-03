// Azimuth Telemetry System - Concurrent Safety Tests
// This file contains test cases for concurrent safety functionality

// Test 1: Thread-Safe Span Operations
test "thread-safe span operations" {
  let span_manager = ThreadSafeSpanManager::new()
  
  // Create spans from multiple threads
  let threads = []
  let span_ids = []
  
  for i in 0..=9 {
    let thread_id = i
    let thread = Thread::spawn(|| {
      let created_spans = []
      
      for j in 0..=9 {
        let span_id = "span_" + thread_id.to_string() + "_" + j.to_string()
        let span_ctx = SpanContext::new("trace_" + thread_id.to_string(), span_id, true, "")
        let span = ThreadSafeSpanManager::create_span(span_manager, "concurrent_test_span", Internal, span_ctx)
        
        // Add events concurrently
        ThreadSafeSpanManager::add_event(span_manager, span, "thread_event", Some([("thread_id", StringValue(thread_id.to_string()))]))
        
        // Set status concurrently
        ThreadSafeSpanManager::set_status(span_manager, span, Ok, Some("Thread operation completed"))
        
        created_spans.push(span)
      }
      
      // End spans concurrently
      for span in created_spans {
        ThreadSafeSpanManager::end_span(span_manager, span)
      }
      
      created_spans.map(|span| ThreadSafeSpanManager::get_span_id(span_manager, span))
    })
    
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    let thread_spans = Thread::join(thread)
    for span_id in thread_spans {
      span_ids.push(span_id)
    }
  }
  
  // Verify all spans were created and processed correctly
  assert_eq(span_ids.length(), 100) // 10 threads × 10 spans each
  
  // Check that all spans have correct attributes
  for span_id in span_ids {
    let span = ThreadSafeSpanManager::get_span_by_id(span_manager, span_id)
    match span {
      Some(s) => {
        assert_eq(ThreadSafeSpanManager::get_name(span_manager, s), "concurrent_test_span")
        assert_eq(ThreadSafeSpanManager::get_status(span_manager, s), Ok)
      }
      None => assert_true(false)
    }
  }
}

// Test 2: Concurrent Metric Operations
test "concurrent metric operations" {
  let metric_manager = ThreadSafeMetricManager::new()
  
  // Create metric
  let counter = ThreadSafeMetricManager::create_counter(metric_manager, "concurrent_counter", None, None)
  
  // Increment counter from multiple threads
  let threads = []
  
  for i in 0..=9 {
    let thread = Thread::spawn(|| {
      for j in 0..=99 {
        ThreadSafeMetricManager::add(metric_manager, counter, 1.0)
        ThreadSafeMetricManager::add(metric_manager, counter, 1.0, Some(Attributes::new()))
      }
    })
    
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify final counter value
  let final_value = ThreadSafeMetricManager::get_value(metric_manager, counter)
  assert_eq(final_value, 2000.0) // 10 threads × 200 increments each
  
  // Test concurrent histogram operations
  let histogram = ThreadSafeMetricManager::create_histogram(metric_manager, "concurrent_histogram", None, None)
  
  let histogram_threads = []
  
  for i in 0..=9 {
    let thread = Thread::spawn(|| {
      for j in 0..=99 {
        ThreadSafeMetricManager::record(metric_manager, histogram, j.to_float())
      }
    })
    
    histogram_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in histogram_threads {
    Thread::join(thread)
  }
  
  // Verify histogram statistics
  let histogram_stats = ThreadSafeMetricManager::get_stats(metric_manager, histogram)
  assert_eq(histogram_stats.count, 1000) // 10 threads × 100 recordings each
  assert_eq(histogram_stats.min, 0.0)
  assert_eq(histogram_stats.max, 99.0)
}

// Test 3: Concurrent Log Operations
test "concurrent log operations" {
  let log_manager = ThreadSafeLogManager::new()
  
  let logger = ThreadSafeLogManager::get_logger(log_manager, "concurrent_logger")
  
  // Emit logs from multiple threads
  let threads = []
  
  for i in 0..=9 {
    let thread_id = i
    let thread = Thread::spawn(|| {
      for j in 0..=99 {
        let log_record = LogRecord::new(Info, "Thread " + thread_id.to_string() + " log message " + j.to_string())
        ThreadSafeLogManager::emit(log_manager, logger, log_record)
      }
    })
    
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify all logs were recorded
  let log_count = ThreadSafeLogManager::get_log_count(log_manager, logger)
  assert_eq(log_count, 1000) // 10 threads × 100 logs each
  
  // Verify log content
  let logs = ThreadSafeLogManager::get_logs(log_manager, logger)
  assert_eq(logs.length(), 1000)
  
  // Check that logs from all threads are present
  let mut thread_counts = [0; 10]
  
  for log in logs {
    match LogRecord::body(log) {
      Some(message) => {
        for i in 0..=9 {
          if message.contains("Thread " + i.to_string()) {
            thread_counts[i] = thread_counts[i] + 1
            break
          }
        }
      }
      None => assert_true(false)
    }
  }
  
  // Each thread should have 100 logs
  for i in 0..=9 {
    assert_eq(thread_counts[i], 100)
  }
}

// Test 4: Concurrent Resource Access with Locks
test "concurrent resource access with locks" {
  let resource_manager = ThreadSafeResourceManager::new()
  
  // Create shared resource
  let shared_resource = ThreadSafeResourceManager::create_resource(resource_manager, "shared_counter", 0)
  
  // Multiple threads increment the counter
  let threads = []
  
  for i in 0..=9 {
    let thread = Thread::spawn(|| {
      for j in 0..=99 {
        // Acquire lock
        ThreadSafeResourceManager::acquire_lock(resource_manager, shared_resource)
        
        // Read current value
        let current_value = ThreadSafeResourceManager::get_int_value(resource_manager, shared_resource)
        
        // Increment and update
        let new_value = current_value + 1
        ThreadSafeResourceManager::set_int_value(resource_manager, shared_resource, new_value)
        
        // Release lock
        ThreadSafeResourceManager::release_lock(resource_manager, shared_resource)
      }
    })
    
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify final value
  let final_value = ThreadSafeResourceManager::get_int_value(resource_manager, shared_resource)
  assert_eq(final_value, 1000) // 10 threads × 100 increments each
  
  // Test with timeout on lock acquisition
  let timeout_resource = ThreadSafeResourceManager::create_resource(resource_manager, "timeout_resource", 0)
  
  // Thread that holds the lock for a long time
  let holder_thread = Thread::spawn(|| {
    ThreadSafeResourceManager::acquire_lock(resource_manager, timeout_resource)
    Thread::sleep(2000) // Hold for 2 seconds
    ThreadSafeResourceManager::release_lock(resource_manager, timeout_resource)
  })
  
  // Thread that tries to acquire with timeout
  let waiter_thread = Thread::spawn(|| {
    let acquired = ThreadSafeResourceManager::try_acquire_lock_with_timeout(resource_manager, timeout_resource, 1000) // 1 second timeout
    assert_false(acquired) // Should fail due to timeout
  })
  
  Thread::join(holder_thread)
  Thread::join(waiter_thread)
}

// Test 5: Concurrent Context Propagation
test "concurrent context propagation" {
  let context_manager = ThreadSafeContextManager::new()
  
  // Create root context
  let root_ctx = ThreadSafeContextManager::create_root_context(context_manager)
  
  // Create contexts in multiple threads
  let threads = []
  let context_ids = []
  
  for i in 0..=9 {
    let thread_id = i
    let thread = Thread::spawn(|| {
      // Create child context
      let child_ctx = ThreadSafeContextManager::create_child_context(context_manager, root_ctx)
      
      // Set values in context
      let key1 = ContextKey::new("thread_id")
      ThreadSafeContextManager::set_value(context_manager, child_ctx, key1, thread_id.to_string())
      
      let key2 = ContextKey::new("operation")
      ThreadSafeContextManager::set_value(context_manager, child_ctx, key2, "concurrent_test")
      
      // Create grandchild context
      let grandchild_ctx = ThreadSafeContextManager::create_child_context(context_manager, child_ctx)
      
      // Add more values
      let key3 = ContextKey::new("step")
      ThreadSafeContextManager::set_value(context_manager, grandchild_ctx, key3, "processing")
      
      // Return context ID for verification
      ThreadSafeContextManager::get_context_id(context_manager, grandchild_ctx)
    })
    
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    let context_id = Thread::join(thread)
    context_ids.push(context_id)
  }
  
  // Verify all contexts were created
  assert_eq(context_ids.length(), 10)
  
  // Check context values
  for context_id in context_ids {
    let context = ThreadSafeContextManager::get_context_by_id(context_manager, context_id)
    match context {
      Some(ctx) => {
        // Check inherited values
        let key2 = ContextKey::new("operation")
        match ThreadSafeContextManager::get_value(context_manager, ctx, key2) {
          Some(value) => assert_eq(value, "concurrent_test")
          None => assert_true(false)
        }
        
        // Check direct values
        let key3 = ContextKey::new("step")
        match ThreadSafeContextManager::get_value(context_manager, ctx, key3) {
          Some(value) => assert_eq(value, "processing")
          None => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
}

// Test 6: Concurrent Baggage Operations
test "concurrent baggage operations" {
  let baggage_manager = ThreadSafeBaggageManager::new()
  
  // Create initial baggage
  let initial_baggage = ThreadSafeBaggageManager::create_baggage(baggage_manager)
  
  // Multiple threads add entries to baggage
  let threads = []
  
  for i in 0..=9 {
    let thread_id = i
    let thread = Thread::spawn(|| {
      let thread_baggage = ThreadSafeBaggageManager::create_baggage(baggage_manager)
      
      // Copy entries from initial baggage
      ThreadSafeBaggageManager::copy_from(baggage_manager, thread_baggage, initial_baggage)
      
      // Add thread-specific entries
      ThreadSafeBaggageManager::set_entry(baggage_manager, thread_baggage, "thread_id", thread_id.to_string())
      ThreadSafeBaggageManager::set_entry(baggage_manager, thread_baggage, "operation", "concurrent_baggage_test")
      
      // Add more entries
      for j in 0..=9 {
        let key = "key_" + thread_id.to_string() + "_" + j.to_string()
        let value = "value_" + thread_id.to_string() + "_" + j.to_string()
        ThreadSafeBaggageManager::set_entry(baggage_manager, thread_baggage, key, value)
      }
      
      thread_baggage
    })
    
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  let baggage_list = []
  for thread in threads {
    let baggage = Thread::join(thread)
    baggage_list.push(baggage)
  }
  
  // Verify all baggage instances
  assert_eq(baggage_list.length(), 10)
  
  for baggage in baggage_list {
    // Check thread-specific entries
    match ThreadSafeBaggageManager::get_entry(baggage_manager, baggage, "operation") {
      Some(value) => assert_eq(value, "concurrent_baggage_test")
      None => assert_true(false)
    }
    
    // Check that each baggage has 12 entries (2 common + 10 thread-specific)
    let entries = ThreadSafeBaggageManager::get_all_entries(baggage_manager, baggage)
    assert_eq(entries.length(), 12)
  }
}

// Test 7: Race Condition Detection
test "race condition detection" {
  let race_detector = RaceConditionDetector::new()
  
  // Enable race detection
  RaceConditionDetector::enable_detection(race_detector)
  
  // Create shared resource
  let shared_counter = RaceConditionDetector::create_monitored_resource(race_detector, "counter", 0)
  
  // Create threads that will cause race conditions
  let threads = []
  
  for i in 0..=9 {
    let thread = Thread::spawn(|| {
      for j in 0..=99 {
        // Read-modify-write without proper synchronization (intentional race condition)
        let current_value = RaceConditionDetector::get_int_value(race_detector, shared_counter)
        let new_value = current_value + 1
        RaceConditionDetector::set_int_value(race_detector, shared_counter, new_value)
      }
    })
    
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Check for race conditions
  let race_conditions = RaceConditionDetector::get_detected_races(race_detector)
  assert_true(race_conditions.length() > 0) // Should detect race conditions
  
  // Verify final value is less than expected due to race conditions
  let final_value = RaceConditionDetector::get_int_value(race_detector, shared_counter)
  assert_true(final_value < 1000) // Should be less than 1000 due to lost updates
  
  // Test with proper synchronization
  let synchronized_counter = RaceConditionDetector::create_monitored_resource(race_detector, "sync_counter", 0)
  
  let sync_threads = []
  
  for i in 0..=9 {
    let thread = Thread::spawn(|| {
      for j in 0..=99 {
        // Use atomic operations
        RaceConditionDetector::atomic_increment(race_detector, synchronized_counter, 1)
      }
    })
    
    sync_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in sync_threads {
    Thread::join(thread)
  }
  
  // Check for race conditions in synchronized version
  let sync_race_conditions = RaceConditionDetector::get_detected_races_for_resource(race_detector, "sync_counter")
  assert_eq(sync_race_conditions.length(), 0) // Should have no race conditions
  
  // Verify final value is correct
  let sync_final_value = RaceConditionDetector::get_int_value(race_detector, synchronized_counter)
  assert_eq(sync_final_value, 1000) // Should be exactly 1000
}

// Test 8: Deadlock Detection and Prevention
test "deadlock detection and prevention" {
  let deadlock_manager = DeadlockManager::new()
  
  // Enable deadlock detection
  DeadlockManager::enable_detection(deadlock_manager)
  
  // Create resources
  let resource1 = DeadlockManager::create_resource(deadlock_manager, "resource1")
  let resource2 = DeadlockManager::create_resource(deadlock_manager, "resource2")
  
  // Create threads that will cause deadlock
  let deadlock_detected = AtomicBool::new(false)
  
  let thread1 = Thread::spawn(|| {
    // Acquire resources in order 1 -> 2
    DeadlockManager::acquire_resource(deadlock_manager, resource1)
    Thread::sleep(100) // Give time for other thread to acquire resource2
    DeadlockManager::acquire_resource(deadlock_manager, resource2)
    
    // Do work
    Thread::sleep(100)
    
    // Release resources
    DeadlockManager::release_resource(deadlock_manager, resource2)
    DeadlockManager::release_resource(deadlock_manager, resource1)
  })
  
  let thread2 = Thread::spawn(|| {
    // Acquire resources in order 2 -> 1 (will cause deadlock)
    DeadlockManager::acquire_resource(deadlock_manager, resource2)
    Thread::sleep(100) // Give time for other thread to acquire resource1
    
    // This should detect deadlock
    let acquired = DeadlockManager::try_acquire_with_deadlock_detection(deadlock_manager, resource1)
    if !acquired {
      deadlock_detected.store(true)
    }
    
    // Release resource2
    DeadlockManager::release_resource(deadlock_manager, resource2)
  })
  
  // Wait for threads to complete
  Thread::join(thread1)
  Thread::join(thread2)
  
  // Check if deadlock was detected
  assert_true(deadlock_detected.load())
  
  // Test deadlock prevention
  DeadlockManager::enable_prevention(deadlock_manager)
  
  let prevention_success = AtomicBool::new(true)
  
  let thread3 = Thread::spawn(|| {
    // Acquire resources in order 1 -> 2
    DeadlockManager::acquire_with_prevention(deadlock_manager, resource1)
    DeadlockManager::acquire_with_prevention(deadlock_manager, resource2)
    
    // Do work
    Thread::sleep(100)
    
    // Release resources
    DeadlockManager::release_resource(deadlock_manager, resource2)
    DeadlockManager::release_resource(deadlock_manager, resource1)
  })
  
  let thread4 = Thread::spawn(|| {
    // Try to acquire in order 2 -> 1 (prevention should reorder or block)
    let acquired1 = DeadlockManager::try_acquire_with_prevention(deadlock_manager, resource2)
    if acquired1 {
      let acquired2 = DeadlockManager::try_acquire_with_prevention(deadlock_manager, resource1)
      if acquired2 {
        // Do work
        Thread::sleep(100)
        
        // Release resources
        DeadlockManager::release_resource(deadlock_manager, resource1)
        DeadlockManager::release_resource(deadlock_manager, resource2)
      } else {
        DeadlockManager::release_resource(deadlock_manager, resource2)
      }
    }
  })
  
  // Wait for threads to complete
  Thread::join(thread3)
  Thread::join(thread4)
  
  // Check if prevention was successful
  assert_true(prevention_success.load())
  
  // No deadlocks should be detected when prevention is enabled
  let detected_deadlocks = DeadlockManager::get_detected_deadlocks(deadlock_manager)
  assert_eq(detected_deadlocks.length(), 0)
}

// Test 9: Concurrent Data Structure Operations
test "concurrent data structure operations" {
  let data_structure_manager = ConcurrentDataManager::new()
  
  // Create concurrent data structures
  let concurrent_map = ConcurrentDataManager::create_map(data_structure_manager)
  let concurrent_queue = ConcurrentDataManager::create_queue(data_structure_manager)
  let concurrent_set = ConcurrentDataManager::create_set(data_structure_manager)
  
  // Test concurrent map operations
  let map_threads = []
  
  for i in 0..=9 {
    let thread_id = i
    let thread = Thread::spawn(|| {
      // Insert key-value pairs
      for j in 0..=99 {
        let key = "key_" + thread_id.to_string() + "_" + j.to_string()
        let value = "value_" + thread_id.to_string() + "_" + j.to_string()
        ConcurrentDataManager::map_insert(data_structure_manager, concurrent_map, key, value)
      }
      
      // Read values
      for j in 0..=99 {
        let key = "key_" + thread_id.to_string() + "_" + j.to_string()
        match ConcurrentDataManager::map_get(data_structure_manager, concurrent_map, key) {
          Some(value) => assert_eq(value, "value_" + thread_id.to_string() + "_" + j.to_string())
          None => assert_true(false)
        }
      }
    })
    
    map_threads.push(thread)
  }
  
  // Test concurrent queue operations
  let queue_producer_threads = []
  let queue_consumer_threads = []
  
  // Producer threads
  for i in 0..=4 {
    let thread_id = i
    let thread = Thread::spawn(|| {
      for j in 0..=199 {
        let item = "item_" + thread_id.to_string() + "_" + j.to_string()
        ConcurrentDataManager::queue_enqueue(data_structure_manager, concurrent_queue, item)
      }
    })
    
    queue_producer_threads.push(thread)
  }
  
  // Consumer threads
  for i in 0..=4 {
    let thread = Thread::spawn(|| {
      let consumed_items = []
      
      for j in 0..=199 {
        match ConcurrentDataManager::queue_dequeue(data_structure_manager, concurrent_queue) {
          Some(item) => consumed_items.push(item),
          None => {} // Queue might be empty
        }
      }
      
      consumed_items
    })
    
    queue_consumer_threads.push(thread)
  }
  
  // Test concurrent set operations
  let set_threads = []
  
  for i in 0..=9 {
    let thread_id = i
    let thread = Thread::spawn(|| {
      // Insert items
      for j in 0..=99 {
        let item = "item_" + thread_id.to_string() + "_" + j.to_string()
        ConcurrentDataManager::set_insert(data_structure_manager, concurrent_set, item)
      }
      
      // Check if items exist
      for j in 0..=99 {
        let item = "item_" + thread_id.to_string() + "_" + j.to_string()
        assert_true(ConcurrentDataManager::set_contains(data_structure_manager, concurrent_set, item))
      }
    })
    
    set_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in map_threads {
    Thread::join(thread)
  }
  
  for thread in queue_producer_threads {
    Thread::join(thread)
  }
  
  let consumed_items_list = []
  for thread in queue_consumer_threads {
    let consumed_items = Thread::join(thread)
    consumed_items_list.push(consumed_items)
  }
  
  for thread in set_threads {
    Thread::join(thread)
  }
  
  // Verify map operations
  let map_size = ConcurrentDataManager::map_size(data_structure_manager, concurrent_map)
  assert_eq(map_size, 1000) // 10 threads × 100 items each
  
  // Verify queue operations
  let total_consumed = 0
  for consumed_items in consumed_items_list {
    total_consumed = total_consumed + consumed_items.length()
  }
  
  // Should have consumed most items (some might still be in queue)
  assert_true(total_consumed > 900)
  
  // Verify set operations
  let set_size = ConcurrentDataManager::set_size(data_structure_manager, concurrent_set)
  assert_eq(set_size, 1000) // 10 threads × 100 items each
}

// Test 10: Concurrent Telemetry Pipeline
test "concurrent telemetry pipeline" {
  let pipeline_manager = ConcurrentPipelineManager::new()
  
  // Create pipeline stages
  let ingestion_stage = PipelineManager::create_stage(pipeline_manager, "ingestion", 4) // 4 workers
  let processing_stage = PipelineManager::create_stage(pipeline_manager, "processing", 2) // 2 workers
  let storage_stage = PipelineManager::create_stage(pipeline_manager, "storage", 3) // 3 workers
  
  // Connect stages
  PipelineManager::connect_stages(pipeline_manager, ingestion_stage, processing_stage)
  PipelineManager::connect_stages(pipeline_manager, processing_stage, storage_stage)
  
  // Start pipeline
  PipelineManager::start_pipeline(pipeline_manager)
  
  // Generate telemetry data concurrently
  let producer_threads = []
  
  for i in 0..=9 {
    let thread_id = i
    let thread = Thread::spawn(|| {
      for j in 0..=99 {
        let span_ctx = SpanContext::new("trace_" + thread_id.to_string(), "span_" + j.to_string(), true, "")
        let span = Span::new("operation_" + thread_id.to_string(), Internal, span_ctx)
        Span::add_event(span, "event", Some([("producer", StringValue(thread_id.to_string()))]))
        
        // Submit to pipeline
        PipelineManager::submit_to_pipeline(pipeline_manager, ingestion_stage, span)
      }
    })
    
    producer_threads.push(thread)
  }
  
  // Wait for all producers to complete
  for thread in producer_threads {
    Thread::join(thread)
  }
  
  // Wait for pipeline to process all items
  PipelineManager::wait_for_completion(pipeline_manager)
  
  // Stop pipeline
  PipelineManager::stop_pipeline(pipeline_manager)
  
  // Verify processing statistics
  let ingestion_stats = PipelineManager::get_stage_stats(pipeline_manager, ingestion_stage)
  let processing_stats = PipelineManager::get_stage_stats(pipeline_manager, processing_stage)
  let storage_stats = PipelineManager::get_stage_stats(pipeline_manager, storage_stage)
  
  // All stages should have processed the same number of items
  assert_eq(ingestion_stats.processed_items, 1000) // 10 threads × 100 items each
  assert_eq(processing_stats.processed_items, 1000)
  assert_eq(storage_stats.processed_items, 1000)
  
  // Check for errors
  assert_eq(ingestion_stats.error_count, 0)
  assert_eq(processing_stats.error_count, 0)
  assert_eq(storage_stats.error_count, 0)
  
  // Verify throughput
  let total_time = PipelineManager::get_total_processing_time(pipeline_manager)
  let throughput = 1000.0 / total_time * 1000.0 // items per second
  
  assert_true(throughput > 100.0) // Should process at least 100 items per second
  
  // Check resource utilization
  let resource_stats = PipelineManager::get_resource_stats(pipeline_manager)
  assert_true(resource_stats.cpu_usage < 90.0) // CPU usage should be reasonable
  assert_true(resource_stats.memory_usage < 100 * 1024 * 1024) // Memory usage should be reasonable
}