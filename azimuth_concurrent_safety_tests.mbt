// Azimuth Concurrent Safety Tests
// This file contains test cases for concurrent safety and thread safety

// Test 1: Thread-Safe Counter
test "thread-safe counter operations" {
  let counter = ThreadSafeCounter::new(0)
  
  // Test single-threaded operations
  ThreadSafeCounter::increment(counter)
  ThreadSafeCounter::increment(counter)
  ThreadSafeCounter::add(counter, 5)
  
  assert_eq(ThreadSafeCounter::value(counter), 7)
  
  // Test multi-threaded operations
  let threads = []
  for i = 0; i < 10; i = i + 1 {
    let thread = Thread::spawn({
      for j = 0; j < 100; j = j + 1 {
        ThreadSafeCounter::increment(counter)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Should be 7 + (10 * 100) = 1007
  assert_eq(ThreadSafeCounter::value(counter), 1007)
}

// Test 2: Thread-Safe Attributes
test "thread-safe attributes operations" {
  let attributes = ThreadSafeAttributes::new()
  
  // Test single-threaded operations
  ThreadSafeAttributes::set(attributes, "key1", StringValue("value1"))
  ThreadSafeAttributes::set(attributes, "key2", IntValue(42))
  
  let value1 = ThreadSafeAttributes::get(attributes, "key1")
  match value1 {
    Some(StringValue(v)) => assert_eq(v, "value1")
    _ => assert_true(false)
  }
  
  // Test concurrent read/write operations
  let threads = []
  
  // Writer threads
  for i = 0; i < 5; i = i + 1 {
    let thread = Thread::spawn({
      let key = "concurrent_key_" + i.to_string()
      let value = "concurrent_value_" + i.to_string()
      ThreadSafeAttributes::set(attributes, key, StringValue(value))
    })
    threads.push(thread)
  }
  
  // Reader threads
  for i = 0; i < 5; i = i + 1 {
    let thread = Thread::spawn({
      let key = "key1"
      let value = ThreadSafeAttributes::get(attributes, key)
      match value {
        Some(StringValue(v)) => assert_eq(v, "value1")
        _ => assert_true(false)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify all values are present
  assert_eq(ThreadSafeAttributes::size(attributes), 7) // Original 2 + 5 new ones
}

// Test 3: Thread-Safe Span Operations
test "thread-safe span operations" {
  let span_processor = ThreadSafeSpanProcessor::new()
  
  // Test concurrent span creation and ending
  let threads = []
  
  for i = 0; i < 10; i = i + 1 {
    let thread = Thread::spawn({
      let span_name = "test_span_" + i.to_string()
      let span = ThreadSafeSpanProcessor::start_span(span_processor, span_name, Internal)
      
      // Add some attributes
      ThreadSafeSpan::set_attribute(span, "thread_id", IntValue(i))
      ThreadSafeSpan::add_event(span, "test_event")
      
      // Simulate some work
      Thread::sleep(10)
      
      // End the span
      ThreadSafeSpanProcessor::end_span(span_processor, span)
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify all spans were processed
  assert_eq(ThreadSafeSpanProcessor::processed_count(span_processor), 10)
}

// Test 4: Thread-Safe Metrics Collection
test "thread-safe metrics collection" {
  let metrics_collector = ThreadSafeMetricsCollector::new()
  
  // Test concurrent metric recording
  let threads = []
  
  for i = 0; i < 10; i = i + 1 {
    let thread = Thread::spawn({
      let counter = ThreadSafeMetricsCollector::get_counter(metrics_collector, "test_counter")
      
      for j = 0; j < 100; j = j + 1 {
        ThreadSafeCounter::add(counter, 1.0)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify metric value
  let counter = ThreadSafeMetricsCollector::get_counter(metrics_collector, "test_counter")
  assert_eq(ThreadSafeCounter::value(counter), 1000.0)
}

// Test 5: Thread-Safe Queue Operations
test "thread-safe queue operations" {
  let queue = ThreadSafeQueue::new(100) // Max capacity 100
  
  // Test concurrent producer-consumer pattern
  let producer_threads = []
  let consumer_threads = []
  
  // Create producer threads
  for i = 0; i < 5; i = i + 1 {
    let thread = Thread::spawn({
      for j = 0; j < 20; j = j + 1 {
        let item = "item_" + i.to_string() + "_" + j.to_string()
        ThreadSafeQueue::enqueue(queue, item)
        Thread::sleep(1) // Small delay to simulate work
      }
    })
    producer_threads.push(thread)
  }
  
  // Create consumer threads
  let consumed_items = ThreadSafeCounter::new(0)
  for i = 0; i < 3; i = i + 1 {
    let thread = Thread::spawn({
      for j = 0; j < 33; j = j + 1 { // 33 * 3 = 99, one less than total produced
        let item = ThreadSafeQueue::dequeue(queue)
        match item {
          Some(_) => ThreadSafeCounter::increment(consumed_items)
          None => Thread::sleep(1) // Wait if queue is empty
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
  
  // Verify queue operations
  assert_eq(ThreadSafeCounter::value(consumed_items), 99)
  assert_eq(ThreadSafeQueue::size(queue), 1) // One item remaining
}

// Test 6: Thread-Safe Resource Pool
test "thread-safe resource pool" {
  let resource_pool = ThreadSafeResourcePool::new(5) // Pool size 5
  
  // Test concurrent resource acquisition and release
  let threads = []
  
  for i = 0; i < 10; i = i + 1 {
    let thread = Thread::spawn({
      // Try to acquire a resource
      let resource = ThreadSafeResourcePool::acquire(resource_pool)
      match resource {
        Some(res) => {
          // Use the resource
          Thread::sleep(10) // Simulate work
          
          // Release the resource
          ThreadSafeResourcePool::release(resource_pool, res)
        }
        None => {
          // Pool exhausted, this is acceptable
          assert_true(true)
        }
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify all resources are returned to pool
  assert_eq(ThreadSafeResourcePool::available_count(resource_pool), 5)
}

// Test 7: Thread-Safe Cache Operations
test "thread-safe cache operations" {
  let cache = ThreadSafeCache::new(100) // Max 100 entries
  
  // Pre-populate cache
  for i = 0; i < 50; i = i + 1 {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string()
    ThreadSafeCache::put(cache, key, value)
  }
  
  // Test concurrent cache operations
  let threads = []
  let hit_counter = ThreadSafeCounter::new(0)
  let miss_counter = ThreadSafeCounter::new(0)
  
  for i = 0; i < 10; i = i + 1 {
    let thread = Thread::spawn({
      for j = 0; j < 20; j = j + 1 {
        let key = "key_" + (Random::int() % 60).to_string() // Random access to existing and non-existing keys
        
        let value = ThreadSafeCache::get(cache, key)
        match value {
          Some(_) => ThreadSafeCounter::increment(hit_counter),
          None => ThreadSafeCounter::increment(miss_counter)
        }
        
        // Occasionally add new entries
        if j % 5 == 0 {
          let new_key = "new_key_" + i.to_string() + "_" + j.to_string()
          let new_value = "new_value_" + i.to_string() + "_" + j.to_string()
          ThreadSafeCache::put(cache, new_key, new_value)
        }
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify cache operations
  let total_accesses = ThreadSafeCounter::value(hit_counter) + ThreadSafeCounter::value(miss_counter)
  assert_eq(total_accesses, 200) // 10 threads * 20 operations each
  assert_true(ThreadSafeCounter::value(hit_counter) > 0) // Should have some cache hits
  assert_true(ThreadSafeCache::size(cache) >= 50) // Should have at least original entries
}

// Test 8: Thread-Safe Log Buffer
test "thread-safe log buffer" {
  let log_buffer = ThreadSafeLogBuffer::new(1000) // Buffer size 1000
  
  // Test concurrent logging
  let threads = []
  
  for i = 0; i < 10; i = i + 1 {
    let thread = Thread::spawn({
      for j = 0; j < 50; j = j + 1 {
        let message = "Thread " + i.to_string() + " Message " + j.to_string()
        let log_entry = LogEntry::new(Info, message)
        ThreadSafeLogBuffer::add(log_buffer, log_entry)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify all log entries are captured
  assert_eq(ThreadSafeLogBuffer::size(log_buffer), 500)
  
  // Test concurrent consumption
  let consumed_count = ThreadSafeCounter::new(0)
  let consumer_threads = []
  
  for i = 0; i < 5; i = i + 1 {
    let thread = Thread::spawn({
      let entries = ThreadSafeLogBuffer::drain(log_buffer, 100) // Drain up to 100 entries
      for entry in entries {
        ThreadSafeCounter::increment(consumed_count)
      }
    })
    consumer_threads.push(thread)
  }
  
  // Wait for all consumers to complete
  for thread in consumer_threads {
    Thread::join(thread)
  }
  
  // Verify consumption
  assert_eq(ThreadSafeCounter::value(consumed_count), 500)
  assert_eq(ThreadSafeLogBuffer::size(log_buffer), 0)
}

// Test 9: Thread-Safe Configuration Manager
test "thread-safe configuration manager" {
  let config_manager = ThreadSafeConfigManager::new()
  
  // Initialize with some configuration
  ThreadSafeConfigManager::set(config_manager, "max_connections", IntValue(100))
  ThreadSafeConfigManager::set(config_manager, "timeout_ms", IntValue(5000))
  ThreadSafeConfigManager::set(config_manager, "enable_compression", BoolValue(true))
  
  // Test concurrent configuration updates
  let threads = []
  
  for i = 0; i < 10; i = i + 1 {
    let thread = Thread::spawn({
      // Update configuration
      let key = "dynamic_key_" + i.to_string()
      let value = "dynamic_value_" + i.to_string()
      ThreadSafeConfigManager::set(config_manager, key, StringValue(value))
      
      // Read configuration
      let timeout = ThreadSafeConfigManager::get(config_manager, "timeout_ms")
      match timeout {
        Some(IntValue(t)) => assert_true(t >= 1000 && t <= 10000) // Should be reasonable
        _ => assert_true(false)
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify configuration integrity
  let max_connections = ThreadSafeConfigManager::get(config_manager, "max_connections")
  match max_connections {
    Some(IntValue(conn)) => assert_eq(conn, 100)
    _ => assert_true(false)
  }
  
  // Verify new configuration entries
  assert_eq(ThreadSafeConfigManager::size(config_manager), 13) // Original 3 + 10 new ones
}

// Test 10: Thread-Safe Reference Counting
test "thread-safe reference counting" {
  let ref_counted = ThreadSafeRefCounted::new("test_data")
  
  // Test initial reference count
  assert_eq(ThreadSafeRefCounted::ref_count(ref_counted), 1)
  
  // Test concurrent reference counting
  let threads = []
  let refs = ThreadSafeCounter::new(0)
  
  for i = 0; i < 10; i = i + 1 {
    let thread = Thread::spawn({
      // Create references
      let local_refs = []
      for j = 0; j < 5; j = j + 1 {
        let ref_clone = ThreadSafeRefCounted::clone(ref_counted)
        local_refs.push(ref_clone)
        ThreadSafeCounter::increment(refs)
      }
      
      // Use references
      for local_ref in local_refs {
        let data = ThreadSafeRefCounted::get(local_ref)
        match data {
          Some(value) => assert_eq(value, "test_data")
          None => assert_true(false)
        }
      }
      
      // References are automatically dropped when they go out of scope
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify reference count
  // Should be back to 1 (original) after all thread-local references are dropped
  assert_eq(ThreadSafeRefCounted::ref_count(ref_counted), 1)
  assert_eq(ThreadSafeCounter::value(refs), 50) // 10 threads * 5 references each
}