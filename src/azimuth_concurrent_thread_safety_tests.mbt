// Azimuth Concurrent and Thread Safety Tests
// This file contains comprehensive test cases for concurrent operations and thread safety

// Test 1: Concurrent Span Creation and Management
test "concurrent span creation and management" {
  let concurrent_span_manager = ConcurrentSpanManager::new()
  let thread_count = 10
  let spans_per_thread = 100
  
  // Create spans concurrently
  let threads = []
  let span_ids = ConcurrentArray::new()
  
  for i in 0..=thread_count {
    let thread = Thread::spawn(|| => {
      let thread_span_ids = []
      for j in 0..=spans_per_thread {
        let span_name = "span_" + i.to_string() + "_" + j.to_string()
        let span_id = ConcurrentSpanManager::create_span(concurrent_span_manager, span_name)
        thread_span_ids.push(span_id)
      }
      thread_span_ids
    })
    threads.push(thread)
  }
  
  // Collect all span IDs
  for thread in threads {
    let thread_span_ids = Thread::join(thread)
    for span_id in thread_span_ids {
      ConcurrentArray::push(span_ids, span_id)
    }
  }
  
  // Verify all spans were created
  assert_eq(ConcurrentArray::size(span_ids), thread_count * spans_per_thread)
  
  // Verify all span IDs are unique
  let unique_span_ids = ConcurrentHashSet::new()
  for span_id in ConcurrentArray::iter(span_ids) {
    assert_false(ConcurrentHashSet::contains(unique_span_ids, span_id))
    ConcurrentHashSet::insert(unique_span_ids, span_id)
  }
  
  assert_eq(ConcurrentHashSet::size(unique_span_ids), thread_count * spans_per_thread)
  
  // Test concurrent span operations
  let operation_threads = []
  for span_id in ConcurrentArray::iter(span_ids) {
    let thread = Thread::spawn(|| => {
      ConcurrentSpanManager::add_attribute(concurrent_span_manager, span_id, "concurrent_attr", "test_value")
      ConcurrentSpanManager::add_event(concurrent_span_manager, span_id, "concurrent_event")
      ConcurrentSpanManager::set_status(concurrent_span_manager, span_id, Ok)
    })
    operation_threads.push(thread)
  }
  
  // Wait for all operations to complete
  for thread in operation_threads {
    Thread::join(thread)
  }
  
  // Verify operations completed successfully
  for span_id in ConcurrentArray::iter(span_ids) {
    let attributes = ConcurrentSpanManager::get_attributes(concurrent_span_manager, span_id)
    assert_true(attributes.contains_key("concurrent_attr"))
    
    let events = ConcurrentSpanManager::get_events(concurrent_span_manager, span_id)
    assert_true(events.length() > 0)
    
    let status = ConcurrentSpanManager::get_status(concurrent_span_manager, span_id)
    assert_eq(status, Ok)
  }
}

// Test 2: Thread-Safe Metrics Collection
test "thread-safe metrics collection" {
  let metrics_collector = ThreadSafeMetricsCollector::new()
  let thread_count = 20
  let operations_per_thread = 500
  
  // Create counters for concurrent testing
  let request_counter = ThreadSafeMetricsCollector::create_counter(metrics_collector, "concurrent_requests")
  let error_counter = ThreadSafeMetricsCollector::create_counter(metrics_collector, "concurrent_errors")
  let response_histogram = ThreadSafeMetricsCollector::create_histogram(metrics_collector, "response_time")
  
  // Simulate concurrent metrics recording
  let threads = []
  
  for i in 0..=thread_count {
    let thread = Thread::spawn(|| => {
      for j in 0..=operations_per_thread {
        // Record request
        ThreadSafeCounter::add(request_counter, 1.0)
        
        // Simulate some processing time
        let processing_time = Random::float_range(10.0, 500.0)
        ThreadSafeHistogram::record(response_histogram, processing_time)
        
        // Randomly record errors (10% chance)
        if Random::float_range(0.0, 1.0) < 0.1 {
          ThreadSafeCounter::add(error_counter, 1.0)
        }
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify metrics accuracy
  let total_requests = ThreadSafeCounter::get_value(request_counter)
  let total_errors = ThreadSafeCounter::get_value(error_counter)
  let response_stats = ThreadSafeHistogram::get_stats(response_histogram)
  
  assert_eq(total_requests, (thread_count + 1) * operations_per_thread)
  assert_true(total_errors > 0 && total_errors < total_requests)
  assert_eq(response_stats.count, total_requests)
  assert_true(response_stats.min >= 10.0 && response_stats.max <= 500.0)
  
  // Test concurrent metric creation
  let creation_threads = []
  for i in 0..=10 {
    let thread = Thread::spawn(|| => {
      let metric_name = "dynamic_metric_" + i.to_string()
      ThreadSafeMetricsCollector::create_counter(metrics_collector, metric_name)
    })
    creation_threads.push(thread)
  }
  
  for thread in creation_threads {
    Thread::join(thread)
  }
  
  // Verify all metrics were created
  let all_metrics = ThreadSafeMetricsCollector::get_all_metrics(metrics_collector)
  assert_true(all_metrics.length() >= 13) // 3 original + 10 dynamic
}

// Test 3: Concurrent Context Propagation
test "concurrent context propagation" {
  let context_propagator = ConcurrentContextPropagator::new()
  let thread_count = 15
  
  // Create a parent context
  let parent_context = ConcurrentContextPropagator::create_context(context_propagator, [
    ("trace_id", "0af7651916cd43dd8448eb211c80319c"),
    ("user_id", "user123"),
    ("request_id", "req456")
  ])
  
  // Test concurrent context propagation
  let threads = []
  let propagated_contexts = ConcurrentArray::new()
  
  for i in 0..=thread_count {
    let thread = Thread::spawn(|| => {
      // Extract context from parent
      let child_context = ConcurrentContextPropagator::extract_context(context_propagator, parent_context)
      
      // Add child-specific data
      ConcurrentContextPropagator::set_value(context_propagator, child_context, "thread_id", i.to_string())
      ConcurrentContextPropagator::set_value(context_propagator, child_context, "operation", "concurrent_test")
      
      // Simulate some work
      Thread::sleep(Random::int_range(10, 100))
      
      // Create grandchild context
      let grandchild_context = ConcurrentContextPropagator::create_child_context(context_propagator, child_context, [
        ("step", "grandchild_operation")
      ])
      
      grandchild_context
    })
    threads.push(thread)
  }
  
  // Collect all propagated contexts
  for thread in threads {
    let context = Thread::join(thread)
    ConcurrentArray::push(propagated_contexts, context)
  }
  
  // Verify context propagation
  for context in ConcurrentArray::iter(propagated_contexts) {
    // Verify parent context is preserved
    let trace_id = ConcurrentContextPropagator::get_value(context_propagator, context, "trace_id")
    assert_eq(trace_id, Some("0af7651916cd43dd8448eb211c80319c"))
    
    let user_id = ConcurrentContextPropagator::get_value(context_propagator, context, "user_id")
    assert_eq(user_id, Some("user123"))
    
    let request_id = ConcurrentContextPropagator::get_value(context_propagator, context, "request_id")
    assert_eq(request_id, Some("req456"))
    
    // Verify child-specific data
    let thread_id = ConcurrentContextPropagator::get_value(context_propagator, context, "thread_id")
    assert_true(thread_id.is_some())
    
    let operation = ConcurrentContextPropagator::get_value(context_propagator, context, "operation")
    assert_eq(operation, Some("concurrent_test"))
    
    // Verify grandchild data
    let step = ConcurrentContextPropagator::get_value(context_propagator, context, "step")
    assert_eq(step, Some("grandchild_operation"))
  }
}

// Test 4: Concurrent Resource Pool Access
test "concurrent resource pool access" {
  let resource_pool = ConcurrentResourcePool::new(50, || => SharedResource::new())
  let thread_count = 20
  let operations_per_thread = 25
  
  // Test concurrent resource acquisition and release
  let threads = []
  let operation_results = ConcurrentArray::new()
  
  for i in 0..=thread_count {
    let thread = Thread::spawn(|| => {
      let thread_results = []
      
      for j in 0..=operations_per_thread {
        // Acquire resource
        let resource = ConcurrentResourcePool::acquire(resource_pool, 1000L) // 1s timeout
        match resource {
          Some(r) => {
            // Use resource
            let result = SharedResource::perform_operation(r, "operation_" + j.to_string())
            thread_results.push(result)
            
            // Simulate some work
            Thread::sleep(Random::int_range(1, 10))
            
            // Release resource
            ConcurrentResourcePool::release(resource_pool, r)
          }
          None => {
            thread_results.push("timeout")
          }
        }
      }
      
      thread_results
    })
    threads.push(thread)
  }
  
  // Collect all operation results
  for thread in threads {
    let thread_results = Thread::join(thread)
    for result in thread_results {
      ConcurrentArray::push(operation_results, result)
    }
  }
  
  // Verify operations completed successfully
  let timeout_count = 0
  let success_count = 0
  
  for result in ConcurrentArray::iter(operation_results) {
    match result {
      "timeout" => timeout_count = timeout_count + 1
      _ => success_count = success_count + 1
    }
  }
  
  let total_operations = (thread_count + 1) * operations_per_thread
  assert_eq(success_count + timeout_count, total_operations)
  
  // Most operations should succeed
  let success_rate = success_count.to_float() / total_operations.to_float()
  assert_true(success_rate > 0.9)
  
  // Verify pool integrity
  assert_eq(ConcurrentResourcePool::available_count(resource_pool), 50)
  
  // Test pool exhaustion handling
  let exhaustion_threads = []
  let exhaustion_results = ConcurrentArray::new()
  
  for i in 0..=60 { // More threads than resources
    let thread = Thread::spawn(|| => {
      let resource = ConcurrentResourcePool::acquire(resource_pool, 100L) // Short timeout
      match resource {
        Some(r) => {
          ConcurrentResourcePool::release(resource_pool, r)
          "success"
        }
        None => "exhausted"
      }
    })
    exhaustion_threads.push(thread)
  }
  
  for thread in exhaustion_threads {
    let result = Thread::join(thread)
    ConcurrentArray::push(exhaustion_results, result)
  }
  
  let exhaustion_count = 0
  for result in ConcurrentArray::iter(exhaustion_results) {
    if result == "exhausted" {
      exhaustion_count = exhaustion_count + 1
    }
  }
  
  assert_true(exhaustion_count > 0) // Some threads should experience exhaustion
  assert_eq(ConcurrentResourcePool::available_count(resource_pool), 50)
}

// Test 5: Concurrent Batch Processing
test "concurrent batch processing" {
  let batch_processor = ConcurrentBatchProcessor::new(100) // Batch size of 100
  let thread_count = 8
  let items_per_thread = 1000
  
  // Generate test data
  let all_data = []
  for i in 0..=thread_count {
    for j in 0..=items_per_thread {
      all_data.push("data_" + i.to_string() + "_" + j.to_string())
    }
  }
  
  // Test concurrent batch processing
  let threads = []
  let processed_items = ConcurrentHashSet::new()
  
  for i in 0..=thread_count {
    let start_index = i * items_per_thread
    let end_index = start_index + items_per_thread
    let thread_data = all_data.slice(start_index, end_index)
    
    let thread = Thread::spawn(|| => {
      let batch_results = ConcurrentBatchProcessor::process(batch_processor, thread_data, |batch| {
        let processed_batch = []
        for item in batch {
          let processed = "processed_" + item
          processed_batch.push(processed)
        }
        processed_batch
      })
      batch_results
    })
    threads.push(thread)
  }
  
  // Collect all processed items
  for thread in threads {
    let thread_results = Thread::join(thread)
    for batch in thread_results {
      for item in batch {
        assert_false(ConcurrentHashSet::contains(processed_items, item))
        ConcurrentHashSet::insert(processed_items, item)
      }
    }
  }
  
  // Verify all items were processed
  assert_eq(ConcurrentHashSet::size(processed_items), (thread_count + 1) * items_per_thread)
  
  // Verify processing correctness
  for i in 0..=thread_count {
    for j in 0..=items_per_thread {
      let expected_item = "processed_data_" + i.to_string() + "_" + j.to_string()
      assert_true(ConcurrentHashSet::contains(processed_items, expected_item))
    }
  }
}

// Test 6: Lock-Free Data Structures
test "lock-free data structures" {
  let lock_free_queue = LockFreeQueue::new()
  let lock_free_stack = LockFreeStack::new()
  let lock_free_map = LockFreeMap::new()
  
  let producer_count = 10
  let consumer_count = 5
  let items_per_producer = 1000
  
  // Test lock-free queue
  let producer_threads = []
  let consumer_threads = []
  
  // Producers
  for i in 0..=producer_count {
    let thread = Thread::spawn(|| => {
      for j in 0..=items_per_producer {
        let item = "queue_item_" + i.to_string() + "_" + j.to_string()
        LockFreeQueue::enqueue(lock_free_queue, item)
      }
    })
    producer_threads.push(thread)
  }
  
  // Consumers
  let consumed_items = ConcurrentArray::new()
  for i in 0..=consumer_count {
    let thread = Thread::spawn(|| => {
      let mut local_items = []
      while local_items.length() < (producer_count + 1) * items_per_producer / (consumer_count + 1) {
        match LockFreeQueue::dequeue(lock_free_queue) {
          Some(item) => local_items.push(item),
          None => Thread::sleep(1) // Brief pause if queue is empty
        }
      }
      local_items
    })
    consumer_threads.push(thread)
  }
  
  // Wait for producers
  for thread in producer_threads {
    Thread::join(thread)
  }
  
  // Wait for consumers
  for thread in consumer_threads {
    let thread_items = Thread::join(thread)
    for item in thread_items {
      ConcurrentArray::push(consumed_items, item)
    }
  }
  
  // Verify queue operations
  assert_eq(ConcurrentArray::size(consumed_items), (producer_count + 1) * items_per_producer)
  
  // Test lock-free stack
  let stack_threads = []
  for i in 0..=producer_count {
    let thread = Thread::spawn(|| => {
      for j in 0..=items_per_producer {
        let item = "stack_item_" + i.to_string() + "_" + j.to_string()
        LockFreeStack::push(lock_free_stack, item)
      }
    })
    stack_threads.push(thread)
  }
  
  for thread in stack_threads {
    Thread::join(thread)
  }
  
  // Pop all items
  let popped_items = []
  while true {
    match LockFreeStack::pop(lock_free_stack) {
      Some(item) => popped_items.push(item),
      None => break
    }
  }
  
  assert_eq(popped_items.length(), (producer_count + 1) * items_per_producer)
  
  // Test lock-free map
  let map_threads = []
  for i in 0..=producer_count {
    let thread = Thread::spawn(|| => {
      for j in 0..=items_per_producer {
        let key = "key_" + i.to_string() + "_" + j.to_string()
        let value = "value_" + i.to_string() + "_" + j.to_string()
        LockFreeMap::insert(lock_free_map, key, value)
      }
    })
    map_threads.push(thread)
  }
  
  for thread in map_threads {
    Thread::join(thread)
  }
  
  // Verify map operations
  let map_size = LockFreeMap::size(lock_free_map)
  assert_eq(map_size, (producer_count + 1) * items_per_producer)
  
  // Test concurrent map access
  let access_threads = []
  for i in 0..=consumer_count {
    let thread = Thread::spawn(|| => {
      let mut found_items = 0
      for j in 0..=items_per_producer {
        let key = "key_" + j.to_string() + "_0"
        match LockFreeMap::get(lock_free_map, key) {
          Some(_) => found_items = found_items + 1,
          None => ()
        }
      }
      found_items
    })
    access_threads.push(thread)
  }
  
  for thread in access_threads {
    let found = Thread::join(thread)
    assert_true(found > 0)
  }
}

// Test 7: Concurrent Telemetry Collection
test "concurrent telemetry collection" {
  let telemetry_collector = ConcurrentTelemetryCollector::new()
  let service_count = 5
  let spans_per_service = 200
  
  // Simulate concurrent telemetry collection from multiple services
  let service_threads = []
  
  for i in 0..=service_count {
    let thread = Thread::spawn(|| => {
      let service_name = "service_" + i.to_string()
      
      for j in 0..=spans_per_service {
        // Create span
        let span_id = ConcurrentTelemetryCollector::create_span(
          telemetry_collector,
          service_name,
          "operation_" + j.to_string()
        )
        
        // Add attributes
        ConcurrentTelemetryCollector::add_attribute(
          telemetry_collector,
          span_id,
          "service_name",
          service_name
        )
        
        ConcurrentTelemetryCollector::add_attribute(
          telemetry_collector,
          span_id,
          "operation_index",
          j.to_string()
        )
        
        // Add events
        ConcurrentTelemetryCollector::add_event(
          telemetry_collector,
          span_id,
          "event_" + j.to_string()
        )
        
        // Record metrics
        let metric_name = service_name + "_operations"
        ConcurrentTelemetryCollector::record_metric(
          telemetry_collector,
          metric_name,
          Random::float_range(10.0, 100.0)
        )
        
        // End span
        ConcurrentTelemetryCollector::end_span(telemetry_collector, span_id)
      }
    })
    service_threads.push(thread)
  }
  
  // Wait for all services to complete
  for thread in service_threads {
    Thread::join(thread)
  }
  
  // Verify telemetry collection
  let all_spans = ConcurrentTelemetryCollector::get_all_spans(telemetry_collector)
  assert_eq(all_spans.length(), (service_count + 1) * spans_per_service)
  
  // Verify service-specific data
  for i in 0..=service_count {
    let service_name = "service_" + i.to_string()
    let service_spans = ConcurrentTelemetryCollector::get_spans_by_service(telemetry_collector, service_name)
    assert_eq(service_spans.length(), spans_per_service)
    
    let metric_name = service_name + "_operations"
    let metric_value = ConcurrentTelemetryCollector::get_metric_value(telemetry_collector, metric_name)
    assert_true(metric_value > 0.0)
  }
  
  // Test concurrent aggregation
  let aggregation_threads = []
  let aggregation_results = ConcurrentArray::new()
  
  for i in 0..=service_count {
    let thread = Thread::spawn(|| => {
      let service_name = "service_" + i.to_string()
      let service_metrics = ConcurrentTelemetryCollector::aggregate_metrics(
        telemetry_collector,
        service_name
      )
      service_metrics
    })
    aggregation_threads.push(thread)
  }
  
  for thread in aggregation_threads {
    let metrics = Thread::join(thread)
    ConcurrentArray::push(aggregation_results, metrics)
  }
  
  assert_eq(ConcurrentArray::size(aggregation_results), service_count + 1)
  
  // Verify aggregation results
  for metrics in ConcurrentArray::iter(aggregation_results) {
    assert_true(metrics.contains_key("operation_count"))
    assert_true(metrics.contains_key("avg_duration"))
    assert_true(metrics.contains_key("error_count"))
  }
}

// Test 8: Race Condition Detection
test "race condition detection" {
  let race_detector = RaceConditionDetector::new()
  RaceConditionDetector::enable_monitoring(race_detector, true)
  
  let shared_counter = AtomicInt::new(0)
  let thread_count = 20
  let increments_per_thread = 1000
  
  // Test with proper synchronization (should not have race conditions)
  let synchronized_threads = []
  
  for i in 0..=thread_count {
    let thread = Thread::spawn(|| => {
      for j in 0..=increments_per_thread {
        AtomicInt::increment(shared_counter)
      }
    })
    synchronized_threads.push(thread)
  }
  
  for thread in synchronized_threads {
    Thread::join(thread)
  }
  
  let synchronized_result = AtomicInt::get(shared_counter)
  assert_eq(synchronized_result, (thread_count + 1) * increments_per_thread)
  
  let synchronized_races = RaceConditionDetector::get_detected_races(race_detector)
  assert_eq(synchronized_races.length(), 0) // No race conditions with atomic operations
  
  // Reset for unsynchronized test
  RaceConditionDetector::reset(race_detector)
  AtomicInt::set(shared_counter, 0)
  
  // Test without proper synchronization (may have race conditions)
  let unsynchronized_threads = []
  
  for i in 0..=thread_count {
    let thread = Thread::spawn(|| => {
      for j in 0..=increments_per_thread {
        // This is intentionally unsynchronized to potentially trigger race conditions
        let current = AtomicInt::get(shared_counter)
        Thread::sleep(0) // Brief pause to increase race condition likelihood
        AtomicInt::set(shared_counter, current + 1)
      }
    })
    unsynchronized_threads.push(thread)
  }
  
  for thread in unsynchronized_threads {
    Thread::join(thread)
  }
  
  let unsynchronized_result = AtomicInt::get(shared_counter)
  let unsynchronized_races = RaceConditionDetector::get_detected_races(race_detector)
  
  // Result may be less than expected due to race conditions
  assert_true(unsynchronized_result <= (thread_count + 1) * increments_per_thread)
  
  // Race conditions may or may not be detected depending on timing
  // but the detector should be monitoring for them
  assert_true(RaceConditionDetector::is_monitoring_active(race_detector))
  
  // Test deadlock detection
  let deadlock_detector = DeadlockDetector::new()
  DeadlockDetector::enable_monitoring(deadlock_detector, true)
  
  let resource1 = Mutex::new()
  let resource2 = Mutex::new()
  
  let deadlock_threads = []
  
  // Thread 1: Lock resource1 then resource2
  let thread1 = Thread::spawn(|| => {
    Mutex::lock(resource1)
    Thread::sleep(100) // Increase deadlock likelihood
    Mutex::lock(resource2)
    Mutex::unlock(resource2)
    Mutex::unlock(resource1)
  })
  deadlock_threads.push(thread1)
  
  // Thread 2: Lock resource2 then resource1
  let thread2 = Thread::spawn(|| => {
    Mutex::lock(resource2)
    Thread::sleep(100) // Increase deadlock likelihood
    Mutex::lock(resource1)
    Mutex::unlock(resource1)
    Mutex::unlock(resource2)
  })
  deadlock_threads.push(thread2)
  
  // Wait with timeout to detect potential deadlock
  let start_time = Time::now()
  for thread in deadlock_threads {
    Thread::join_with_timeout(thread, 2000L) // 2 second timeout
  }
  let end_time = Time::now()
  
  // If deadlock occurred, the operation would take longer than expected
  let execution_time = end_time - start_time
  let potential_deadlock = execution_time > 1500L
  
  if potential_deadlock {
    let deadlocks = DeadlockDetector::get_detected_deadlocks(deadlock_detector)
    assert_true(deadlocks.length() > 0)
  }
}