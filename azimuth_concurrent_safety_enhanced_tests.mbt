// Azimuth Telemetry System - Concurrent Safety Enhanced Tests
// This file contains comprehensive test cases for concurrent safety functionality

// Test 1: Thread-Safe Attributes Operations
test "thread-safe attributes operations" {
  let attrs = ThreadSafeAttributes::new()
  
  // Test concurrent attribute setting
  let mut handles = []
  for i in 0..=10 {
    let handle = Thread::spawn(func() {
      let key = "key." + i.to_string()
      let value = StringValue("value." + i.to_string())
      ThreadSafeAttributes::set(attrs, key, value)
    })
    handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in handles {
    Thread::join(handle)
  }
  
  // Verify all attributes were set correctly
  for i in 0..=10 {
    let key = "key." + i.to_string()
    let result = ThreadSafeAttributes::get(attrs, key)
    match result {
      Some(StringValue(v)) => assert_eq(v, "value." + i.to_string())
      _ => assert_true(false)
    }
  }
  
  // Test concurrent attribute reading
  let mut read_handles = []
  for i in 0..=20 {
    let handle = Thread::spawn(func() {
      let key = "key." + (i % 11).to_string()
      ThreadSafeAttributes::get(attrs, key)
    })
    read_handles.push(handle)
  }
  
  // Wait for all read threads to complete
  for handle in read_handles {
    let result = Thread::join(handle)
    match result {
      Some(StringValue(_)) => assert_true(true)
      None => assert_true(false)
    }
  }
}

// Test 2: Thread-Safe Span Operations
test "thread-safe span operations" {
  let span_manager = ThreadSafeSpanManager::new()
  
  // Test concurrent span creation
  let mut handles = []
  for i in 0..=5 {
    let handle = Thread::spawn(func() {
      let span_name = "span_" + i.to_string()
      let span_ctx = SpanContext::new("trace_" + i.to_string(), "span_" + i.to_string(), true, "")
      let span = Span::new(span_name, Internal, span_ctx)
      ThreadSafeSpanManager::add_span(span_manager, span)
    })
    handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in handles {
    Thread::join(handle)
  }
  
  // Verify all spans were added
  let spans = ThreadSafeSpanManager::get_all_spans(span_manager)
  assert_eq(spans.length(), 6)
  
  // Test concurrent span operations
  let mut operation_handles = []
  for span in spans {
    let handle = Thread::spawn(func() {
      Span::add_event(span, "concurrent_event", Some([("thread_id", StringValue(Thread::current_id().to_string()))]))
      Span::set_status(span, Ok, Some("Operation completed"))
    })
    operation_handles.push(handle)
  }
  
  // Wait for all operations to complete
  for handle in operation_handles {
    Thread::join(handle)
  }
  
  // Verify operations completed successfully
  for span in spans {
    assert_eq(Span::status(span), Ok)
  }
}

// Test 3: Thread-Safe Metrics Operations
test "thread-safe metrics operations" {
  let metrics_manager = ThreadSafeMetricsManager::new()
  
  // Test concurrent counter operations
  let counter = ThreadSafeMetricsManager::create_counter(metrics_manager, "concurrent_counter", Some("Test counter"), Some("count"))
  
  let mut handles = []
  for i in 0..=100 {
    let handle = Thread::spawn(func() {
      Counter::add(counter, 1.0)
    })
    handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in handles {
    Thread::join(handle)
  }
  
  // Verify counter value
  let value = Counter::get_value(counter)
  assert_eq(value, 101.0)
  
  // Test concurrent histogram operations
  let histogram = ThreadSafeMetricsManager::create_histogram(metrics_manager, "concurrent_histogram", Some("Test histogram"), Some("ms"))
  
  let mut histogram_handles = []
  for i in 0..=50 {
    let handle = Thread::spawn(func() {
      let value = (i % 10 + 1) * 10.0
      Histogram::record(histogram, value)
    })
    histogram_handles.push(handle)
  }
  
  // Wait for all histogram operations to complete
  for handle in histogram_handles {
    Thread::join(handle)
  }
  
  // Verify histogram data
  let histogram_data = Histogram::get_data(histogram)
  assert_eq(histogram_data.count, 51)
}

// Test 4: Thread-Safe Log Operations
test "thread-safe log operations" {
  let log_manager = ThreadSafeLogManager::new()
  let logger = ThreadSafeLogManager::get_logger(log_manager, "concurrent_logger")
  
  // Test concurrent log operations
  let mut handles = []
  for i in 0..=50 {
    let handle = Thread::spawn(func() {
      let log_record = LogRecord::new(Info, "Log message " + i.to_string())
      Logger::emit(logger, log_record)
    })
    handles.push(handle)
  }
  
  // Wait for all log operations to complete
  for handle in handles {
    Thread::join(handle)
  }
  
  // Verify all log records were emitted
  let log_count = Logger::get_log_count(logger)
  assert_eq(log_count, 51)
  
  // Test concurrent log filtering
  let mut filter_handles = []
  for i in 0..=10 {
    let handle = Thread::spawn(func() {
      let filter = LogFilter::new(Info, Error)
      Logger::get_filtered_logs(logger, filter)
    })
    filter_handles.push(handle)
  }
  
  // Wait for all filter operations to complete
  for handle in filter_handles {
    let filtered_logs = Thread::join(handle)
    assert_eq(filtered_logs.length(), 51)
  }
}

// Test 5: Thread-Safe Resource Operations
test "thread-safe resource operations" {
  let resource_manager = ThreadSafeResourceManager::new()
  
  // Test concurrent resource creation
  let mut handles = []
  for i in 0..=5 {
    let handle = Thread::spawn(func() {
      let attrs = [
        ("service.name", StringValue("service_" + i.to_string())),
        ("service.version", StringValue("1.0." + i.to_string())),
        ("service.instance.id", StringValue("instance-" + i.to_string()))
      ]
      let resource = Resource::with_attributes(Resource::new(), attrs)
      ThreadSafeResourceManager::add_resource(resource_manager, "resource_" + i.to_string(), resource)
    })
    handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in handles {
    Thread::join(handle)
  }
  
  // Verify all resources were added
  let resources = ThreadSafeResourceManager::get_all_resources(resource_manager)
  assert_eq(resources.length(), 6)
  
  // Test concurrent resource merging
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("environment", StringValue("production")),
    ("region", StringValue("us-west-2"))
  ])
  
  let mut merge_handles = []
  for resource in resources {
    let handle = Thread::spawn(func() {
      ThreadSafeResourceManager::merge_with_base(resource_manager, resource, base_resource)
    })
    merge_handles.push(handle)
  }
  
  // Wait for all merge operations to complete
  for handle in merge_handles {
    Thread::join(handle)
  }
}
  
// Test 6: Thread-Safe Context Propagation
test "thread-safe context propagation" {
  let context_manager = ThreadSafeContextManager::new()
  
  // Create initial context
  let key = ContextKey::new("trace_id")
  let initial_ctx = Context::with_value(Context::root(), key, "initial_trace_id")
  
  // Test concurrent context propagation
  let mut handles = []
  for i in 0..=10 {
    let handle = Thread::spawn(func() {
      // Propagate context to new thread
      let propagated_ctx = ThreadSafeContextManager::propagate_context(context_manager, initial_ctx)
      
      // Add thread-specific data
      let thread_key = ContextKey::new("thread_id")
      let updated_ctx = Context::with_value(propagated_ctx, thread_key, i.to_string())
      
      // Store updated context
      ThreadSafeContextManager::store_context(context_manager, "thread_" + i.to_string(), updated_ctx)
    })
    handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in handles {
    Thread::join(handle)
  }
  
  // Verify context propagation
  for i in 0..=10 {
    let ctx = ThreadSafeContextManager::get_context(context_manager, "thread_" + i.to_string())
    match ctx {
      Some(context) => {
        // Verify original context is preserved
        match Context::get(context, key) {
          Some(trace_id) => assert_eq(trace_id, "initial_trace_id")
          None => assert_true(false)
        }
        
        // Verify thread-specific data
        let thread_key = ContextKey::new("thread_id")
        match Context::get(context, thread_key) {
          Some(thread_id) => assert_eq(thread_id, i.to_string())
          None => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
}

// Test 7: Thread-Safe Baggage Operations
test "thread-safe baggage operations" {
  let baggage_manager = ThreadSafeBaggageManager::new()
  
  // Test concurrent baggage operations
  let mut handles = []
  for i in 0..=10 {
    let handle = Thread::spawn(func() {
      let baggage = Baggage::new()
      let updated_baggage = Baggage::set_entry(baggage, "key_" + i.to_string(), "value_" + i.to_string())
      ThreadSafeBaggageManager::store_baggage(baggage_manager, "baggage_" + i.to_string(), updated_baggage)
    })
    handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in handles {
    Thread::join(handle)
  }
  
  // Verify all baggage entries
  for i in 0..=10 {
    let baggage = ThreadSafeBaggageManager::get_baggage(baggage_manager, "baggage_" + i.to_string())
    match baggage {
      Some(b) => {
        let value = Baggage::get_entry(b, "key_" + i.to_string())
        match value {
          Some(v) => assert_eq(v, "value_" + i.to_string())
          None => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
  
  // Test concurrent baggage merging
  let base_baggage = Baggage::new()
  let base_updated = Baggage::set_entry(base_baggage, "base_key", "base_value")
  
  let mut merge_handles = []
  for i in 0..=5 {
    let handle = Thread::spawn(func() {
      let baggage = ThreadSafeBaggageManager::get_baggage(baggage_manager, "baggage_" + i.to_string())
      match baggage {
        Some(b) => {
          let merged = Baggage::merge(base_updated, b)
          ThreadSafeBaggageManager::store_baggage(baggage_manager, "merged_" + i.to_string(), merged)
        }
        None => assert_true(false)
      }
    })
    merge_handles.push(handle)
  }
  
  // Wait for all merge operations to complete
  for handle in merge_handles {
    Thread::join(handle)
  }
  
  // Verify merged baggage
  for i in 0..=5 {
    let merged_baggage = ThreadSafeBaggageManager::get_baggage(baggage_manager, "merged_" + i.to_string())
    match merged_baggage {
      Some(b) => {
        // Verify base entry
        let base_value = Baggage::get_entry(b, "base_key")
        match base_value {
          Some(v) => assert_eq(v, "base_value")
          None => assert_true(false)
        }
        
        // Verify original entry
        let original_value = Baggage::get_entry(b, "key_" + i.to_string())
        match original_value {
          Some(v) => assert_eq(v, "value_" + i.to_string())
          None => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
}

// Test 8: Thread-Safe Cache Operations
test "thread-safe cache operations" {
  let cache = ThreadSafeCache::new(100) // Max 100 entries
  
  // Test concurrent cache writes
  let mut handles = []
  for i in 0..=50 {
    let handle = Thread::spawn(func() {
      let key = "key_" + i.to_string()
      let value = "value_" + i.to_string()
      ThreadSafeCache::put(cache, key, value)
    })
    handles.push(handle)
  }
  
  // Wait for all writes to complete
  for handle in handles {
    Thread::join(handle)
  }
  
  // Verify all entries were added
  for i in 0..=50 {
    let key = "key_" + i.to_string()
    let value = ThreadSafeCache::get(cache, key)
    match value {
      Some(v) => assert_eq(v, "value_" + i.to_string())
      None => assert_true(false)
    }
  }
  
  // Test concurrent cache reads
  let mut read_handles = []
  for i in 0..=100 {
    let handle = Thread::spawn(func() {
      let key = "key_" + (i % 51).to_string()
      ThreadSafeCache::get(cache, key)
    })
    read_handles.push(handle)
  }
  
  // Wait for all reads to complete
  for handle in read_handles {
    let result = Thread::join(handle)
    match result {
      Some(_) => assert_true(true)
      None => assert_true(false)
    }
  }
  
  // Test cache size limits
  for i in 100..=200 {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string()
    ThreadSafeCache::put(cache, key, value)
  }
  
  // Verify cache respects size limits
  let cache_size = ThreadSafeCache::size(cache)
  assert_true(cache_size <= 100)
}

// Test 9: Thread-Safe Queue Operations
test "thread-safe queue operations" {
  let queue = ThreadSafeQueue::new()
  
  // Test concurrent queue operations
  let mut producer_handles = []
  for i in 0..=5 {
    let handle = Thread::spawn(func() {
      for j in 0..=10 {
        let item = "item_" + i.to_string() + "_" + j.to_string()
        ThreadSafeQueue::enqueue(queue, item)
      }
    })
    producer_handles.push(handle)
  }
  
  let mut consumer_handles = []
  for i in 0..=3 {
    let handle = Thread::spawn(func() {
      let mut items = []
      for j in 0..=15 {
        let item = ThreadSafeQueue::dequeue(queue)
        match item {
          Some(value) => items.push(value)
          None => break
        }
      }
      items
    })
    consumer_handles.push(handle)
  }
  
  // Wait for all producers to complete
  for handle in producer_handles {
    Thread::join(handle)
  }
  
  // Wait for all consumers to complete
  let mut all_items = []
  for handle in consumer_handles {
    let items = Thread::join(handle)
    for item in items {
      all_items.push(item)
    }
  }
  
  // Verify items were processed
  assert_eq(all_items.length(), 60) // 6 producers * 10 items each
}

// Test 10: Thread-Safe Lock-Free Operations
test "thread-safe lock-free operations" {
  let counter = LockFreeCounter::new(0)
  
  // Test concurrent counter increments
  let mut handles = []
  for i in 0..=100 {
    let handle = Thread::spawn(func() {
      LockFreeCounter::increment(counter)
    })
    handles.push(handle)
  }
  
  // Wait for all increments to complete
  for handle in handles {
    Thread::join(handle)
  }
  
  // Verify counter value
  let value = LockFreeCounter::get_value(counter)
  assert_eq(value, 101)
  
  // Test concurrent counter adds
  let mut add_handles = []
  for i in 0..=50 {
    let handle = Thread::spawn(func() {
      LockFreeCounter::add(counter, i)
    })
    add_handles.push(handle)
  }
  
  // Wait for all adds to complete
  for handle in add_handles {
    Thread::join(handle)
  }
  
  // Verify final counter value
  let final_value = LockFreeCounter::get_value(counter)
  assert_eq(final_value, 1821) // 101 + sum(0..50)
  
  // Test lock-free stack operations
  let stack = LockFreeStack::new()
  
  // Test concurrent push operations
  let mut push_handles = []
  for i in 0..=50 {
    let handle = Thread::spawn(func() {
      LockFreeStack::push(stack, i)
    })
    push_handles.push(handle)
  }
  
  // Wait for all pushes to complete
  for handle in push_handles {
    Thread::join(handle)
  }
  
  // Test concurrent pop operations
  let mut pop_handles = []
  for i in 0..=25 {
    let handle = Thread::spawn(func() {
      let mut items = []
      for j in 0..=2 {
        let item = LockFreeStack::pop(stack)
        match item {
          Some(value) => items.push(value)
          None => break
        }
      }
      items
    })
    pop_handles.push(handle)
  }
  
  // Wait for all pops to complete
  let mut popped_items = []
  for handle in pop_handles {
    let items = Thread::join(handle)
    for item in items {
      popped_items.push(item)
    }
  }
  
  // Verify items were popped
  assert_eq(popped_items.length(), 78) // 26 consumers * 3 items each, but some may get None
}