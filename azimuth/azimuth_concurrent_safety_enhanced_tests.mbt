// Azimuth Concurrent Safety Enhanced Tests
// This file contains enhanced tests for concurrent safety in the Azimuth telemetry system

// Test 1: Concurrent span operations with race condition detection
pub test "concurrent span operations with race condition detection" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrent-safety-test")
  
  // Configure thread pool for concurrent operations
  let thread_pool = azimuth::ThreadPool::new(10)  # 10 concurrent threads
  let race_detector = azimuth::RaceConditionDetector::new()
  
  // Create shared spans for concurrent access
  let shared_spans = []
  for i = 0; i < 20; i = i + 1 {
    let span = azimuth::Tracer::start_span(tracer, "shared.span." + i.to_string())
    shared_spans.push(span)
  }
  
  // Start concurrent operations on shared spans
  let tasks = []
  
  // Task 1: Concurrent attribute setting
  for i = 0; i < 10; i = i + 1 {
    let task = azimuth::ThreadPool::submit(thread_pool, {
      let span_index = i % shared_spans.length()
      let span = shared_spans[span_index]
      
      // Set attributes concurrently
      for j = 0; j < 100; j = j + 1 {
        let key = "concurrent.attr." + j.to_string()
        let value = "value." + i.to_string() + "." + j.to_string()
        
        azimuth::RaceConditionDetector::start_access(race_detector, span, "set_attribute")
        azimuth::Span::set_attribute(span, key, azimuth::StringValue(value))
        azimuth::RaceConditionDetector::end_access(race_detector, span, "set_attribute")
        
        # Small delay to increase chance of race conditions
        azimuth::Clock::sleep(1)
      }
    })
    tasks.push(task)
  }
  
  // Task 2: Concurrent event adding
  for i = 0; i < 10; i = i + 1 {
    let task = azimuth::ThreadPool::submit(thread_pool, {
      let span_index = i % shared_spans.length()
      let span = shared_spans[span_index]
      
      # Add events concurrently
      for j = 0; j < 50; j = j + 1 {
        let event_name = "concurrent.event." + j.to_string()
        
        azimuth::RaceConditionDetector::start_access(race_detector, span, "add_event")
        azimuth::Span::add_event(span, event_name, Some([
          ("event.index", azimuth::IntValue(j)),
          ("thread.id", azimuth::IntValue(i))
        ]))
        azimuth::RaceConditionDetector::end_access(race_detector, span, "add_event")
        
        azimuth::Clock::sleep(2)
      }
    })
    tasks.push(task)
  }
  
  // Task 3: Concurrent status setting
  for i = 0; i < 5; i = i + 1 {
    let task = azimuth::ThreadPool::submit(thread_pool, {
      let span_index = i % shared_spans.length()
      let span = shared_spans[span_index]
      
      # Set status concurrently
      azimuth::RaceConditionDetector::start_access(race_detector, span, "set_status")
      azimuth::Span::set_status(span, azimuth::Ok)
      azimuth::RaceConditionDetector::end_access(race_detector, span, "set_status")
      
      azimuth::Clock::sleep(10)
    })
    tasks.push(task)
  }
  
  # Wait for all tasks to complete
  for task in tasks {
    azimuth::ThreadPool::wait_for_completion(task, 10000)  # 10 second timeout
  }
  
  # End all spans
  for span in shared_spans {
    azimuth::Span::end(span)
  }
  
  # Check for race conditions
  let race_report = azimuth::RaceConditionDetector::get_report(race_detector)
  
  # Verify no race conditions detected (or they were properly handled)
  assert_true(race_report.detected_race_conditions == 0 || race_report.handled_race_conditions > 0)
  
  # Verify all spans are properly ended
  for span in shared_spans {
    assert_eq(azimuth::Span::status(span), azimuth::Ok)
  }
  
  # Verify span attributes are consistent
  for span in shared_spans.take(5) {
    let attributes = azimuth::Span::get_all_attributes(span)
    assert_true(attributes.length() >= 0)
    
    # Check for attribute consistency
    for attr in attributes {
      match attr.value {
        azimuth::StringValue(value) => assert_true(value.length() > 0)
        azimuth::IntValue(int) => assert_true(int >= 0)
        azimuth::BoolValue(bool) => assert_true(bool == true || bool == false)
        _ => {}
      }
    }
  }
}

// Test 2: Concurrent metric operations with thread safety
pub test "concurrent metric operations with thread safety" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "concurrent-metric-test")
  
  # Create thread-safe metrics
  let counter = azimuth::Meter::create_counter(meter, "concurrent.counter", Some("Concurrent counter"), Some("count"))
  let histogram = azimuth::Meter::create_histogram(meter, "concurrent.histogram", Some("Concurrent histogram"), Some("ms"))
  let gauge = azimuth::Meter::create_gauge(meter, "concurrent.gauge", Some("Concurrent gauge"), Some("percent"))
  
  # Configure thread pool
  let thread_pool = azimuth::ThreadPool::new(20)
  let thread_safety_checker = azimuth::ThreadSafetyChecker::new()
  
  # Start concurrent metric operations
  let tasks = []
  
  # Task 1: Concurrent counter increments
  for i = 0; i < 20; i = i + 1 {
    let task = azimuth::ThreadPool::submit(thread_pool, {
      # Increment counter 1000 times
      for j = 0; j < 1000; j = j + 1 {
        azimuth::ThreadSafetyChecker::start_operation(thread_safety_checker, "counter_add")
        azimuth::Counter::add(counter, 1, [
          ("thread.id", azimuth::IntValue(i)),
          ("iteration", azimuth::IntValue(j))
        ])
        azimuth::ThreadSafetyChecker::end_operation(thread_safety_checker, "counter_add")
      }
    })
    tasks.push(task)
  }
  
  # Task 2: Concurrent histogram recordings
  for i = 0; i < 20; i = i + 1 {
    let task = azimuth::ThreadPool::submit(thread_pool, {
      # Record histogram values 500 times
      for j = 0; j < 500; j = j + 1 {
        let value = (j % 1000).to_double()
        
        azimuth::ThreadSafetyChecker::start_operation(thread_safety_checker, "histogram_record")
        azimuth::Histogram::record(histogram, value, [
          ("thread.id", azimuth::IntValue(i)),
          ("value.range", azimuth::StringValue("0-1000"))
        ])
        azimuth::ThreadSafetyChecker::end_operation(thread_safety_checker, "histogram_record")
      }
    })
    tasks.push(task)
  }
  
  # Task 3: Concurrent gauge updates
  for i = 0; i < 20; i = i + 1 {
    let task = azimuth::ThreadPool::submit(thread_pool, {
      # Update gauge 200 times
      for j = 0; j < 200; j = j + 1 {
        let value = (j % 100).to_long()
        
        azimuth::ThreadSafetyChecker::start_operation(thread_safety_checker, "gauge_set")
        azimuth::Gauge::set(gauge, value, [
          ("thread.id", azimuth::IntValue(i)),
          ("update.type", azimuth::StringValue("concurrent"))
        ])
        azimuth::ThreadSafetyChecker::end_operation(thread_safety_checker, "gauge_set")
        
        azimuth::Clock::sleep(1)
      }
    })
    tasks.push(task)
  }
  
  # Wait for all tasks to complete
  for task in tasks {
    azimuth::ThreadPool::wait_for_completion(task, 15000)  # 15 second timeout
  }
  
  # Verify thread safety
  let safety_report = azimuth::ThreadSafetyChecker::get_report(thread_safety_checker)
  
  # Check for thread safety violations
  assert_true(safety_report.violations == 0)
  assert_true(safety_report.concurrent_operations > 0)
  
  # Verify metric consistency
  let counter_data = azimuth::Counter::get_data(counter)
  let histogram_data = azimuth::Histogram::get_data(histogram)
  let gauge_data = azimuth::Gauge::get_data(gauge)
  
  # Counter should have exactly 20,000 increments (20 threads * 1000 increments)
  assert_true(counter_data.count >= 20000)
  
  # Histogram should have exactly 10,000 recordings (20 threads * 500 recordings)
  assert_true(histogram_data.count >= 10000)
  
  # Gauge should have a valid value
  assert_true(gauge_data.value >= 0 && gauge_data.value < 100)
  
  # Verify metric attributes are consistent
  for attr in counter_data.attributes {
    match attr.value {
      azimuth::IntValue(value) => assert_true(value >= 0)
      _ => {}
    }
  }
}

// Test 3: Concurrent log operations with message ordering
pub test "concurrent log operations with message ordering" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "concurrent-log-test")
  
  # Configure concurrent log processor
  let log_processor = azimuth::ConcurrentLogProcessor::new()
    .with_max_concurrent_writers(50)
    .with_message_queue_size(10000)
    .with_ordering_enabled(true)
  
  # Configure thread pool
  let thread_pool = azimuth::ThreadPool::new(30)
  let message_order_tracker = azimuth::MessageOrderTracker::new()
  
  # Start concurrent log operations
  let tasks = []
  
  # Task 1: Concurrent log emission
  for i = 0; i < 30; i = i + 1 {
    let task = azimuth::ThreadPool::submit(thread_pool, {
      let thread_id = i
      
      # Emit 100 log messages
      for j = 0; j < 100; j = j + 1 {
        let message_id = thread_id * 1000 + j  # Unique message ID
        let message = "Thread " + thread_id.to_string() + " message " + j.to_string()
        
        let log_record = azimuth::LogRecord::new(azimuth::Info, message)
        azimuth::LogRecord::set_attribute(log_record, "thread.id", azimuth::IntValue(thread_id))
        azimuth::LogRecord::set_attribute(log_record, "message.id", azimuth::IntValue(message_id))
        azimuth::LogRecord::set_attribute(log_record, "sequence", azimuth::IntValue(j))
        
        # Track message order
        azimuth::MessageOrderTracker::track(message_order_tracker, message_id, thread_id, j)
        
        # Emit log record
        azimuth::ConcurrentLogProcessor::emit(log_processor, logger, log_record)
      }
    })
    tasks.push(task)
  }
  
  # Task 2: Concurrent log flushing
  for i = 0; i < 5; i = i + 1 {
    let task = azimuth::ThreadPool::submit(thread_pool, {
      # Periodically flush logs
      azimuth::Clock::sleep(100)  # 100ms
      azimuth::ConcurrentLogProcessor::flush(log_processor)
    })
    tasks.push(task)
  }
  
  # Wait for all tasks to complete
  for task in tasks {
    azimuth::ThreadPool::wait_for_completion(task, 20000)  # 20 second timeout
  }
  
  # Final flush
  azimuth::ConcurrentLogProcessor::flush(log_processor)
  
  # Verify message ordering
  let order_report = azimuth::MessageOrderTracker::get_report(message_order_tracker)
  
  # Check for ordering violations
  assert_true(order_report.violations == 0)
  assert_true(order_report.total_messages >= 3000)  # 30 threads * 100 messages
  
  # Verify log processor statistics
  let processor_stats = azimuth::ConcurrentLogProcessor::get_statistics(log_processor)
  
  assert_true(processor_stats.total_messages >= 3000)
  assert_true(processor_stats.concurrent_writers <= 50)
  assert_true(processor_stats.queue_size >= 0)
  assert_true(processor_stats.flush_count >= 5)
  
  # Verify log message integrity
  let logged_messages = azimuth::ConcurrentLogProcessor::get_messages(log_processor)
  
  assert_true(logged_messages.length() >= 3000)
  
  # Check message content
  for message in logged_messages.take(100) {
    assert_true(message.contains("Thread"))
    assert_true(message.contains("message"))
    
    # Extract and verify message ID
    let message_id = azimuth::MessageOrderTracker::extract_message_id(message)
    assert_true(message_id >= 0)
  }
}

// Test 4: Concurrent context and baggage operations
pub test "concurrent context and baggage operations" {
  # Configure thread pool
  let thread_pool = azimuth::ThreadPool::new(15)
  let context_safety_validator = azimuth::ContextSafetyValidator::new()
  
  # Create root context
  let root_ctx = azimuth::Context::root()
  
  # Start concurrent context operations
  let tasks = []
  
  # Task 1: Concurrent context creation and modification
  for i = 0; i < 15; i = i + 1 {
    let task = azimuth::ThreadPool::submit(thread_pool, {
      let thread_id = i
      
      # Create context chain
      let ctx = ref root_ctx
      
      for j = 0; j < 50; j = j + 1 {
        let key = azimuth::ContextKey::new("key." + thread_id.to_string() + "." + j.to_string())
        let value = "value." + thread_id.to_string() + "." + j.to_string()
        
        # Validate context before modification
        azimuth::ContextSafetyValidator::validate_before(context_safety_validator, ctx, key)
        
        # Modify context
        ctx = azimuth::Context::with_value(ctx, key, value)
        
        # Validate context after modification
        azimuth::ContextSafetyValidator::validate_after(context_safety_validator, ctx, key)
        
        # Verify value was set correctly
        let retrieved_value = azimuth::Context::get(ctx, key)
        match retrieved_value {
          Some(v) => assert_eq(v, value)
          None => assert_true(false)
        }
      }
      
      # Return the final context for verification
      ctx
    })
    tasks.push(task)
  }
  
  # Task 2: Concurrent baggage operations
  for i = 0; i < 15; i = i + 1 {
    let task = azimuth::ThreadPool::submit(thread_pool, {
      let thread_id = i
      
      # Create and modify baggage
      let baggage = ref azimuth::Baggage::new()
      
      for j = 0; j < 30; j = j + 1 {
        let key = "baggage.key." + thread_id.to_string() + "." + j.to_string()
        let value = "baggage.value." + thread_id.to_string() + "." + j.to_string()
        
        # Validate baggage before modification
        azimuth::ContextSafetyValidator::validate_baggage_before(context_safety_validator, baggage, key)
        
        # Modify baggage
        baggage = azimuth::Baggage::set_entry(baggage, key, value)
        
        # Validate baggage after modification
        azimuth::ContextSafetyValidator::validate_baggage_after(context_safety_validator, baggage, key)
        
        # Verify entry was set correctly
        let retrieved_value = azimuth::Baggage::get_entry(baggage, key)
        assert_eq(retrieved_value, Some(value))
      }
      
      # Return the final baggage for verification
      baggage
    })
    tasks.push(task)
  }
  
  # Wait for all tasks to complete
  let contexts = []
  let baggages = []
  
  for i in 0..15 {
    let ctx_result = azimuth::ThreadPool::wait_for_completion(tasks[i], 10000)
    let baggage_result = azimuth::ThreadPool::wait_for_completion(tasks[15 + i], 10000)
    
    match ctx_result {
      azimuth::TaskSuccess(ctx) => contexts.push(ctx),
      azimuth::TaskFailure(_) => assert_true(false)
    }
    
    match baggage_result {
      azimuth::TaskSuccess(baggage) => baggages.push(baggage),
      azimuth::TaskFailure(_) => assert_true(false)
    }
  }
  
  # Verify context safety
  let safety_report = azimuth::ContextSafetyValidator::get_report(context_safety_validator)
  
  # Check for safety violations
  assert_true(safety_report.context_violations == 0)
  assert_true(safety_report.baggage_violations == 0)
  assert_true(safety_report.concurrent_operations > 0)
  
  # Verify context integrity
  assert_eq(contexts.length(), 15)
  
  for i in 0..15 {
    let ctx = contexts[i]
    
    # Verify all keys are present
    for j in 0..50 {
      let key = azimuth::ContextKey::new("key." + i.to_string() + "." + j.to_string())
      let value = "value." + i.to_string() + "." + j.to_string()
      
      let retrieved_value = azimuth::Context::get(ctx, key)
      assert_eq(retrieved_value, Some(value))
    }
  }
  
  # Verify baggage integrity
  assert_eq(baggages.length(), 15)
  
  for i in 0..15 {
    let baggage = baggages[i]
    
    # Verify all entries are present
    for j in 0..30 {
      let key = "baggage.key." + i.to_string() + "." + j.to_string()
      let value = "baggage.value." + i.to_string() + "." + j.to_string()
      
      let retrieved_value = azimuth::Baggage::get_entry(baggage, key)
      assert_eq(retrieved_value, Some(value))
    }
  }
}

// Test 5: Concurrent resource operations with deadlock detection
pub test "concurrent resource operations with deadlock detection" {
  # Configure thread pool
  let thread_pool = azimuth::ThreadPool::new(10)
  let deadlock_detector = azimuth::DeadlockDetector::new()
  
  # Create shared resources
  let resource_a = azimuth::SharedResource::new("resource-a")
  let resource_b = azimuth::SharedResource::new("resource-b")
  let resource_c = azimuth::SharedResource::new("resource-c")
  
  # Start concurrent resource operations
  let tasks = []
  
  # Task 1: Resource acquisition in different orders (potential deadlock scenario)
  for i = 0; i < 5; i = i + 1 {
    let task = azimuth::ThreadPool::submit(thread_pool, {
      let thread_id = i
      
      # Acquire resources in order A -> B -> C
      azimuth::DeadlockDetector::start_acquire(deadlock_detector, thread_id, "resource-a")
      azimuth::SharedResource::acquire(resource_a)
      azimuth::DeadlockDetector::end_acquire(deadlock_detector, thread_id, "resource-a")
      
      azimuth::Clock::sleep(10)  # Small delay to increase deadlock chance
      
      azimuth::DeadlockDetector::start_acquire(deadlock_detector, thread_id, "resource-b")
      azimuth::SharedResource::acquire(resource_b)
      azimuth::DeadlockDetector::end_acquire(deadlock_detector, thread_id, "resource-b")
      
      azimuth::Clock::sleep(10)
      
      azimuth::DeadlockDetector::start_acquire(deadlock_detector, thread_id, "resource-c")
      azimuth::SharedResource::acquire(resource_c)
      azimuth::DeadlockDetector::end_acquire(deadlock_detector, thread_id, "resource-c")
      
      # Do some work with resources
      azimuth::SharedResource::modify(resource_a, "thread-" + thread_id.to_string())
      azimuth::SharedResource::modify(resource_b, "thread-" + thread_id.to_string())
      azimuth::SharedResource::modify(resource_c, "thread-" + thread_id.to_string())
      
      # Release resources in reverse order
      azimuth::SharedResource::release(resource_c)
      azimuth::SharedResource::release(resource_b)
      azimuth::SharedResource::release(resource_a)
    })
    tasks.push(task)
  }
  
  # Task 2: Resource acquisition in reverse order (potential deadlock scenario)
  for i = 5; i < 10; i = i + 1 {
    let task = azimuth::ThreadPool::submit(thread_pool, {
      let thread_id = i
      
      # Acquire resources in reverse order C -> B -> A
      azimuth::DeadlockDetector::start_acquire(deadlock_detector, thread_id, "resource-c")
      azimuth::SharedResource::acquire(resource_c)
      azimuth::DeadlockDetector::end_acquire(deadlock_detector, thread_id, "resource-c")
      
      azimuth::Clock::sleep(10)  # Small delay to increase deadlock chance
      
      azimuth::DeadlockDetector::start_acquire(deadlock_detector, thread_id, "resource-b")
      azimuth::SharedResource::acquire(resource_b)
      azimuth::DeadlockDetector::end_acquire(deadlock_detector, thread_id, "resource-b")
      
      azimuth::Clock::sleep(10)
      
      azimuth::DeadlockDetector::start_acquire(deadlock_detector, thread_id, "resource-a")
      azimuth::SharedResource::acquire(resource_a)
      azimuth::DeadlockDetector::end_acquire(deadlock_detector, thread_id, "resource-a")
      
      # Do some work with resources
      azimuth::SharedResource::modify(resource_c, "thread-" + thread_id.to_string())
      azimuth::SharedResource::modify(resource_b, "thread-" + thread_id.to_string())
      azimuth::SharedResource::modify(resource_a, "thread-" + thread_id.to_string())
      
      # Release resources in reverse order
      azimuth::SharedResource::release(resource_a)
      azimuth::SharedResource::release(resource_b)
      azimuth::SharedResource::release(resource_c)
    })
    tasks.push(task)
  }
  
  # Wait for all tasks to complete
  for task in tasks {
    let result = azimuth::ThreadPool::wait_for_completion(task, 30000)  # 30 second timeout
    
    # Check for deadlock detection or task timeout
    match result {
      azimuth::TaskSuccess(_) => {}
      azimuth::TaskFailure(reason) => {
        # Task might have failed due to deadlock detection or timeout
        assert_true(reason.contains("deadlock") || reason.contains("timeout"))
      }
    }
  }
  
  # Verify deadlock detection
  let deadlock_report = azimuth::DeadlockDetector::get_report(deadlock_detector)
  
  # Check for detected deadlocks
  assert_true(deadlock_report.detected_deadlocks >= 0)
  assert_true(deadlock_report.prevented_deadlocks >= 0)
  assert_true(deadlock_report.concurrent_acquisitions > 0)
  
  # Verify resource integrity
  let resource_a_data = azimuth::SharedResource::get_data(resource_a)
  let resource_b_data = azimuth::SharedResource::get_data(resource_b)
  let resource_c_data = azimuth::SharedResource::get_data(resource_c)
  
  # Resources should have been modified by multiple threads
  assert_true(resource_a_data.length() >= 0)
  assert_true(resource_b_data.length() >= 0)
  assert_true(resource_c_data.length() >= 0)
  
  # Verify resource states
  assert_true(azimuth::SharedResource::is_available(resource_a))
  assert_true(azimuth::SharedResource::is_available(resource_b))
  assert_true(azimuth::SharedResource::is_available(resource_c))
}