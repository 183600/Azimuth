// Azimuth Memory Leak Protection Comprehensive Tests
// This file contains comprehensive test cases for memory leak detection, prevention, and resource cleanup

// Test 1: Span Lifecycle Memory Management
test "span lifecycle memory management" {
  let leak_detector = MemoryLeakDetector::new()
  let initial_memory = MemoryMonitor::get_current_usage()
  
  // Create and properly manage spans
  let managed_spans = []
  
  for i in 0..=1000 {
    let span = Span::new("test_span_" + i.to_string(), Internal, SpanContext::empty())
    Span::add_event(span, "test_event", None)
    Span::set_attribute(span, "test_attr", StringValue("test_value"))
    managed_spans.push(span)
  }
  
  let after_creation_memory = MemoryMonitor::get_current_usage()
  let creation_memory_increase = after_creation_memory - initial_memory
  
  // Properly end and cleanup spans
  for span in managed_spans {
    Span::end(span)
    Span::cleanup(span)
  }
  
  // Force garbage collection
  MemoryMonitor::force_gc()
  let after_cleanup_memory = MemoryMonitor::get_current_usage()
  
  // Memory should be released after cleanup
  let memory_released = after_creation_memory - after_cleanup_memory
  assert_true(memory_released > creation_memory_increase * 0.8) // At least 80% should be released
  
  // Verify no span leaks detected
  let leak_report = MemoryLeakDetector::analyze(leak_detector)
  assert_eq(leak_report.span_leaks, 0)
}

// Test 2: Attribute Collection Memory Management
test "attribute collection memory management" {
  let leak_detector = MemoryLeakDetector::new()
  
  // Test attribute creation and cleanup
  let attribute_collections = []
  
  for i in 0..=500 {
    let attrs = Attributes::new()
    
    // Add various types of attributes
    Attributes::set(attrs, "string_attr", StringValue("value_" + i.to_string()))
    Attributes::set(attrs, "int_attr", IntValue(i))
    Attributes::set(attrs, "float_attr", FloatValue(i.to_float() * 3.14))
    Attributes::set(attrs, "bool_attr", BoolValue(i % 2 == 0))
    
    // Add array attributes
    let string_array = Array::new(10)
    for j in 0..=9 {
      string_array.push("array_value_" + j.to_string())
    }
    Attributes::set(attrs, "string_array", ArrayStringValue(string_array))
    
    attribute_collections.push(attrs)
  }
  
  // Verify attributes are properly stored
  for attrs in attribute_collections {
    let string_attr = Attributes::get(attrs, "string_attr")
    match string_attr {
      Some(StringValue(_)) => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  // Cleanup attribute collections
  for attrs in attribute_collections {
    Attributes::cleanup(attrs)
  }
  
  // Force garbage collection
  MemoryMonitor::force_gc()
  
  // Verify no attribute leaks detected
  let leak_report = MemoryLeakDetector::analyze(leak_detector)
  assert_eq(leak_report.attribute_leaks, 0)
}

// Test 3: Metrics Memory Management
test "metrics memory management" {
  let leak_detector = MemoryLeakDetector::new()
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test_meter")
  
  // Create various metric instruments
  let metrics = []
  
  for i in 0..=100 {
    let counter = Meter::create_counter(
      meter,
      "counter_" + i.to_string(),
      Some("Test counter " + i.to_string()),
      Some("count")
    )
    
    let histogram = Meter::create_histogram(
      meter,
      "histogram_" + i.to_string(),
      Some("Test histogram " + i.to_string()),
      Some("ms")
    )
    
    let gauge = Meter::create_gauge(
      meter,
      "gauge_" + i.to_string(),
      Some("Test gauge " + i.to_string()),
      Some("value")
    )
    
    metrics.push((counter, histogram, gauge))
  }
  
  // Record metrics
  for (counter, histogram, gauge) in metrics {
    Counter::add(counter, 1.0)
    Histogram::record(histogram, 100.0)
    
    // Simulate metric updates
    for j in 0..=10 {
      Counter::add(counter, j.to_float())
      Histogram::record(histogram, j.to_float() * 10.0)
    }
  }
  
  // Cleanup metrics
  for (counter, histogram, gauge) in metrics {
    Counter::cleanup(counter)
    Histogram::cleanup(histogram)
    Gauge::cleanup(gauge)
  }
  
  // Force garbage collection
  MemoryMonitor::force_gc()
  
  // Verify no metric leaks detected
  let leak_report = MemoryLeakDetector::analyze(leak_detector)
  assert_eq(leak_report.metric_leaks, 0)
}

// Test 4: Log Record Memory Management
test "log record memory management" {
  let leak_detector = MemoryLeakDetector::new()
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test_logger")
  
  // Create and emit log records
  let log_records = []
  
  for i in 0..=1000 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "log_id", IntValue(i))
    Attributes::set(attrs, "component", StringValue("test_component"))
    
    let log_record = LogRecord::new_with_context(
      if i % 4 == 0 { Error } else if i % 2 == 0 { Warn } else { Info },
      Some("Test log message " + i.to_string()),
      Some(attrs),
      Some(1609459200000L + (i * 1000L)), // Timestamp
      Some(1609459200000L + (i * 1000L) + 100L), // Observed timestamp
      Some("trace_id_" + (i % 10).to_string()),
      Some("span_id_" + i.to_string()),
      None
    )
    
    log_records.push(log_record)
  }
  
  // Emit log records
  for log_record in log_records {
    Logger::emit(logger, log_record)
  }
  
  // Cleanup log records
  for log_record in log_records {
    LogRecord::cleanup(log_record)
  }
  
  // Force garbage collection
  MemoryMonitor::force_gc()
  
  // Verify no log record leaks detected
  let leak_report = MemoryLeakDetector::analyze(leak_detector)
  assert_eq(leak_report.log_leaks, 0)
}

// Test 5: Resource Pool Memory Management
test "resource pool memory management" {
  let leak_detector = MemoryLeakDetector::new()
  
  // Create resource pools
  let span_pool = ResourcePool::new(|| => Span::new("pooled_span", Internal, SpanContext::empty()), 100)
  let attribute_pool = ResourcePool::new(|| => Attributes::new(), 200)
  
  // Acquire and release resources
  let acquired_resources = []
  
  // Test normal acquire/release cycle
  for i in 0..=500 {
    // Acquire resources
    let span = ResourcePool::acquire(span_pool)
    let attrs = ResourcePool::acquire(attribute_pool)
    
    // Use resources
    Span::add_event(span, "test_event", None)
    Attributes::set(attrs, "test_attr", StringValue("test_value"))
    
    acquired_resources.push((span, attrs))
    
    // Release some resources
    if i % 3 == 0 {
      let (release_span, release_attrs) = acquired_resources.remove(0)
      ResourcePool::release(span_pool, release_span)
      ResourcePool::release(attribute_pool, release_attrs)
    }
  }
  
  // Release all remaining resources
  for (span, attrs) in acquired_resources {
    ResourcePool::release(span_pool, span)
    ResourcePool::release(attribute_pool, attrs)
  }
  
  // Cleanup pools
  ResourcePool::cleanup(span_pool)
  ResourcePool::cleanup(attribute_pool)
  
  // Force garbage collection
  MemoryMonitor::force_gc()
  
  // Verify no resource pool leaks detected
  let leak_report = MemoryLeakDetector::analyze(leak_detector)
  assert_eq(leak_report.resource_pool_leaks, 0)
}

// Test 6: Circular Reference Detection and Prevention
test "circular reference detection and prevention" {
  let leak_detector = MemoryLeakDetector::new()
  
  // Create objects with potential circular references
  let span_a = Span::new("span_a", Internal, SpanContext::empty())
  let span_b = Span::new("span_b", Internal, SpanContext::empty())
  
  // Create circular reference through links
  Span::add_link(span_a, SpanLink::new(Span::span_context(span_b), None, None))
  Span::add_link(span_b, SpanLink::new(Span::span_context(span_a), None, None))
  
  // Create circular reference through events
  let event_attrs_a = Attributes::new()
  Attributes::set(event_attrs_a, "linked_span", StringValue("span_b"))
  Span::add_event(span_a, "linked_to_b", Some(event_attrs_a))
  
  let event_attrs_b = Attributes::new()
  Attributes::set(event_attrs_b, "linked_span", StringValue("span_a"))
  Span::add_event(span_b, "linked_to_a", Some(event_attrs_b))
  
  // Test circular reference detection
  let circular_refs = MemoryLeakDetector::detect_circular_references(leak_detector)
  assert_true(circular_refs.length() >= 2)
  
  // Test circular reference cleanup
  Span::break_circular_references(span_a)
  Span::break_circular_references(span_b)
  
  // Verify circular references are broken
  let circular_refs_after_cleanup = MemoryLeakDetector::detect_circular_references(leak_detector)
  assert_true(circular_refs_after_cleanup.length() < circular_refs.length())
  
  // End and cleanup spans
  Span::end(span_a)
  Span::end(span_b)
  Span::cleanup(span_a)
  Span::cleanup(span_b)
  
  // Force garbage collection
  MemoryMonitor::force_gc()
  
  // Verify no leaks from circular references
  let leak_report = MemoryLeakDetector::analyze(leak_detector)
  assert_eq(leak_report.circular_reference_leaks, 0)
}

// Test 7: Large Object Memory Management
test "large object memory management" {
  let leak_detector = MemoryLeakDetector::new()
  let initial_memory = MemoryMonitor::get_current_usage()
  
  // Create large objects
  let large_objects = []
  
  for i in 0..=10 {
    // Create span with many attributes
    let span = Span::new("large_span_" + i.to_string(), Internal, SpanContext::empty())
    
    // Add many attributes
    for j in 0..=1000 {
      Attributes::set(
        Span::attributes(span),
        "attr_" + j.to_string(),
        StringValue("large_value_" + j.to_string() + "_with_additional_data_to_increase_size")
      )
    }
    
    // Add many events
    for j in 0..=500 {
      let event_attrs = Attributes::new()
      for k in 0..=10 {
        Attributes::set(
          event_attrs,
          "event_attr_" + k.to_string(),
          StringValue("event_value_" + k.to_string())
        )
      }
      Span::add_event(span, "event_" + j.to_string(), Some(event_attrs))
    }
    
    large_objects.push(span)
  }
  
  let after_creation_memory = MemoryMonitor::get_current_usage()
  let memory_increase = after_creation_memory - initial_memory
  
  // Verify significant memory usage
  assert_true(memory_increase > 100 * 1024 * 1024) // At least 100MB increase
  
  // Cleanup large objects
  for span in large_objects {
    Span::end(span)
    Span::cleanup(span)
  }
  
  // Force garbage collection
  MemoryMonitor::force_gc()
  let after_cleanup_memory = MemoryMonitor::get_current_usage()
  
  // Verify most memory is released
  let memory_released = after_creation_memory - after_cleanup_memory
  assert_true(memory_released > memory_increase * 0.8) // At least 80% released
  
  // Verify no large object leaks
  let leak_report = MemoryLeakDetector::analyze(leak_detector)
  assert_eq(leak_report.large_object_leaks, 0)
}

// Test 8: Memory Pressure Response
test "memory pressure response" {
  let memory_monitor = MemoryMonitor::new()
  let leak_detector = MemoryLeakDetector::new()
  
  // Configure memory pressure handling
  MemoryMonitor::set_threshold(memory_monitor, 100 * 1024 * 1024) // 100MB threshold
  MemoryMonitor::enable_auto_cleanup(memory_monitor, true)
  
  // Create memory pressure
  let pressure_objects = []
  
  for i in 0..=1000 {
    let span = Span::new("pressure_span_" + i.to_string(), Internal, SpanContext::empty())
    
    // Add significant data to each span
    for j in 0..=100 {
      Attributes::set(
        Span::attributes(span),
        "pressure_attr_" + j.to_string(),
        StringValue("pressure_value_" + j.to_string() + "_with_lots_of_data_to_increase_memory_usage")
      )
    }
    
    pressure_objects.push(span)
    
    // Check if memory pressure is detected
    if MemoryMonitor::is_under_pressure(memory_monitor) {
      // Verify automatic cleanup is triggered
      let cleanup_triggered = MemoryMonitor::auto_cleanup_triggered(memory_monitor)
      assert_true(cleanup_triggered)
      
      // Break out of the loop
      break
    }
  }
  
  // Verify memory pressure was detected
  assert_true(MemoryMonitor::is_under_pressure(memory_monitor))
  
  // Cleanup remaining objects
  for span in pressure_objects {
    Span::end(span)
    Span::cleanup(span)
  }
  
  // Force garbage collection
  MemoryMonitor::force_gc()
  
  // Verify memory pressure is resolved
  assert_false(MemoryMonitor::is_under_pressure(memory_monitor))
  
  // Verify no leaks during pressure handling
  let leak_report = MemoryLeakDetector::analyze(leak_detector)
  assert_true(leak_report.total_leaks() < 10) // Allow for minimal leaks during pressure
}

// Test 9: Weak Reference Memory Management
test "weak reference memory management" {
  let leak_detector = MemoryLeakDetector::new()
  
  // Create objects with weak references
  let strong_refs = []
  let weak_refs = []
  
  for i in 0..=100 {
    let span = Span::new("weak_ref_span_" + i.to_string(), Internal, SpanContext::empty())
    strong_refs.push(span)
    
    // Create weak reference
    let weak_ref = WeakReference::new(span)
    weak_refs.push(weak_ref)
  }
  
  // Verify weak references are valid
  for weak_ref in weak_refs {
    assert_true(WeakReference::is_valid(weak_ref))
    
    match WeakReference::upgrade(weak_ref) {
      Some(_) => assert_true(true)
      None => assert_true(false)
    }
  }
  
  // Clear strong references
  strong_refs.clear()
  
  // Force garbage collection
  MemoryMonitor::force_gc()
  
  // Verify weak references are invalidated
  let mut invalid_count = 0
  for weak_ref in weak_refs {
    if !WeakReference::is_valid(weak_ref) {
      invalid_count = invalid_count + 1
    }
    
    match WeakReference::upgrade(weak_ref) {
      Some(_) => assert_true(false) // Should not be able to upgrade
      None => assert_true(true)
    }
  }
  
  // Most weak references should be invalidated
  assert_true(invalid_count > 90)
  
  // Cleanup weak references
  for weak_ref in weak_refs {
    WeakReference::cleanup(weak_ref)
  }
  
  // Verify no weak reference leaks
  let leak_report = MemoryLeakDetector::analyze(leak_detector)
  assert_eq(leak_report.weak_reference_leaks, 0)
}

// Test 10: Cache Memory Management
test "cache memory management" {
  let leak_detector = MemoryLeakDetector::new()
  
  // Create caches with different eviction policies
  let lru_cache = LRUCache::new(1000)
  let lfu_cache = LFUCache::new(1000)
  let ttl_cache = TTLCache::new(1000, 60000L) // 1 minute TTL
  
  // Fill caches with data
  for i in 0..=2000 {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string() + "_with_additional_data_to_increase_size"
    
    LRUCache::put(lru_cache, key, value)
    LFUCache::put(lfu_cache, key, value)
    TTLCache::put(ttl_cache, key, value)
  }
  
  // Verify cache sizes are maintained
  assert_eq(LRUCache::size(lru_cache), 1000)
  assert_eq(LFUCache::size(lfu_cache), 1000)
  assert_true(TTLCache::size(ttl_cache) <= 1000)
  
  // Test cache eviction
  for i in 0..=500 {
    let key = "eviction_key_" + i.to_string()
    let value = "eviction_value_" + i.to_string()
    
    LRUCache::put(lru_cache, key, value)
    LFUCache::put(lfu_cache, key, value)
    TTLCache::put(ttl_cache, key, value)
  }
  
  // Verify cache sizes are still maintained
  assert_eq(LRUCache::size(lru_cache), 1000)
  assert_eq(LFUCache::size(lfu_cache), 1000)
  
  // Test TTL expiration
  Time::sleep(61000L) // Wait for TTL to expire
  
  // Access caches to trigger cleanup
  for i in 0..=100 {
    let key = "key_" + i.to_string()
    LRUCache::get(lru_cache, key)
    LFUCache::get(lfu_cache, key)
    TTLCache::get(ttl_cache, key)
  }
  
  // TTL cache should have expired entries
  assert_true(TTLCache::size(ttl_cache) < 1000)
  
  // Cleanup caches
  LRUCache::cleanup(lru_cache)
  LFUCache::cleanup(lfu_cache)
  TTLCache::cleanup(ttl_cache)
  
  // Force garbage collection
  MemoryMonitor::force_gc()
  
  // Verify no cache leaks
  let leak_report = MemoryLeakDetector::analyze(leak_detector)
  assert_eq(leak_report.cache_leaks, 0)
}

// Helper functions
fn min(a : Float, b : Float, c : Float) -> Float {
  if a <= b && a <= c { a }
  else if b <= a && b <= c { b }
  else { c }
}