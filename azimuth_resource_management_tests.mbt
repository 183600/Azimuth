// Azimuth Resource Management Tests
// This file contains test cases for resource allocation, cleanup, and lifecycle management

test "span lifecycle resource management" {
  let span_ctx = SpanContext::new("lifecycle_trace", "lifecycle_span", true, "")
  
  // Create span
  let span = Span::new("lifecycle_test", Internal, span_ctx)
  assert_true(Span::is_recording(span))
  assert_eq(Span::status(span), Unset)
  
  // Add events and attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "test_attr", StringValue("test_value"))
  Span::add_event(span, "test_event", Some(attrs))
  
  // Modify span
  Span::set_status(span, Ok, Some("Test completed"))
  assert_eq(Span::status(span), Ok)
  
  // End span
  Span::end(span)
  assert_false(Span::is_recording(span))
  
  // Verify span is still accessible after ending
  assert_eq(Span::name(span), "lifecycle_test")
  assert_eq(Span::status(span), Ok)
  
  // Test resource cleanup
  let cleanup_result = Span::cleanup(span)
  assert_true(cleanup_result)
}

test "attribute resource management" {
  let attrs = Attributes::new()
  
  // Test initial state
  assert_eq(Attributes::count(attrs), 0)
  
  // Add attributes
  Attributes::set(attrs, "string_attr", StringValue("string_value"))
  Attributes::set(attrs, "int_attr", IntValue(42))
  Attributes::set(attrs, "float_attr", FloatValue(3.14))
  Attributes::set(attrs, "bool_attr", BoolValue(true))
  
  assert_eq(Attributes::count(attrs), 4)
  
  // Retrieve attributes
  let string_result = Attributes::get(attrs, "string_attr")
  match string_result {
    StringValue(v) => assert_eq(v, "string_value")
    _ => assert_true(false)
  }
  
  let int_result = Attributes::get(attrs, "int_attr")
  match int_result {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Remove attributes
  Attributes::remove(attrs, "string_attr")
  assert_eq(Attributes::count(attrs), 3)
  
  let removed_result = Attributes::get(attrs, "string_attr")
  match removed_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Clear all attributes
  Attributes::clear(attrs)
  assert_eq(Attributes::count(attrs), 0)
  
  // Test resource cleanup
  let cleanup_result = Attributes::cleanup(attrs)
  assert_true(cleanup_result)
}

test "metrics resource management" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "resource_meter")
  
  // Create metrics
  let counter = Meter::create_counter(meter, "resource_counter", Some("Resource counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "resource_histogram", Some("Resource histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "resource_updown", Some("Resource updown"), Some("value"))
  let gauge = Meter::create_gauge(meter, "resource_gauge", Some("Resource gauge"), Some("value"))
  
  // Verify metrics are valid
  assert_true(Counter::is_valid(counter))
  assert_true(Histogram::is_valid(histogram))
  assert_true(UpDownCounter::is_valid(updown_counter))
  assert_true(Gauge::is_valid(gauge))
  
  // Record values
  Counter::add(counter, 10.0)
  Histogram::record(histogram, 100.0)
  UpDownCounter::add(updown_counter, 5.0)
  Gauge::record(gauge, 42.0)
  
  // Verify metrics are still valid after recording
  assert_true(Counter::is_valid(counter))
  assert_true(Histogram::is_valid(histogram))
  assert_true(UpDownCounter::is_valid(updown_counter))
  assert_true(Gauge::is_valid(gauge))
  
  // Test metric cleanup
  let counter_cleanup = Counter::cleanup(counter)
  let histogram_cleanup = Histogram::cleanup(histogram)
  let updown_cleanup = UpDownCounter::cleanup(updown_counter)
  let gauge_cleanup = Gauge::cleanup(gauge)
  
  assert_true(counter_cleanup)
  assert_true(histogram_cleanup)
  assert_true(updown_cleanup)
  assert_true(gauge_cleanup)
  
  // Test meter provider cleanup
  let provider_cleanup = MeterProvider::cleanup(provider)
  assert_true(provider_cleanup)
}

test "log record resource management" {
  // Create log record
  let attrs = Attributes::new()
  Attributes::set(attrs, "log_attr", StringValue("log_value"))
  
  let log_record = LogRecord::new(
    Error,
    Some("Resource management test log"),
    Some(attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some("resource_trace"),
    Some("resource_span")
  )
  
  // Verify log record is valid
  assert_true(LogRecord::is_valid(log_record))
  assert_eq(LogRecord::severity_number(log_record), Error)
  
  // Modify log record
  LogRecord::update_severity(log_record, Warn)
  assert_eq(LogRecord::severity_number(log_record), Warn)
  
  // Emit log record
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "resource_logger")
  let emit_result = Logger::emit(logger, log_record)
  assert_true(emit_result)
  
  // Test log record cleanup
  let cleanup_result = LogRecord::cleanup(log_record)
  assert_true(cleanup_result)
  
  // Test logger provider cleanup
  let provider_cleanup = LoggerProvider::cleanup(provider)
  assert_true(provider_cleanup)
}

test "context resource management" {
  let root_ctx = Context::root()
  
  // Create context with values
  let key1 = ContextKey::new("resource_key1")
  let key2 = ContextKey::new("resource_key2")
  let key3 = ContextKey::new("resource_key3")
  
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  let ctx3 = Context::with_value(ctx2, key3, "value3")
  
  // Verify context values
  let value1 = Context::get(ctx3, key1)
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  let value2 = Context::get(ctx3, key2)
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  let value3 = Context::get(ctx3, key3)
  match value3 {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  // Test context cleanup
  let cleanup_result = Context::cleanup(ctx3)
  assert_true(cleanup_result)
}

test "resource pool management" {
  let pool = ResourcePool::new(10)
  
  // Verify initial pool state
  assert_eq(ResourcePool::available_count(pool), 10)
  assert_eq(ResourcePool::used_count(pool), 0)
  
  // Acquire resources
  let resources = []
  
  for i in 0..5 {
    let resource = ResourcePool::acquire(pool)
    assert_true(ResourcePool::is_valid(resource))
    resources.push(resource)
  }
  
  // Verify pool state after acquiring
  assert_eq(ResourcePool::available_count(pool), 5)
  assert_eq(ResourcePool::used_count(pool), 5)
  
  // Release resources
  for resource in resources {
    ResourcePool::release(pool, resource)
  }
  
  // Verify pool state after releasing
  assert_eq(ResourcePool::available_count(pool), 10)
  assert_eq(ResourcePool::used_count(pool), 0)
  
  // Test pool exhaustion
  let exhausted_resources = []
  
  for i in 0..15 {
    let resource = ResourcePool::acquire(pool)
    
    if ResourcePool::is_valid(resource) {
      exhausted_resources.push(resource)
    }
  }
  
  // Verify pool was exhausted
  assert_eq(exhausted_resources.length(), 10)
  assert_eq(ResourcePool::available_count(pool), 0)
  assert_eq(ResourcePool::used_count(pool), 10)
  
  // Release all resources
  for resource in exhausted_resources {
    ResourcePool::release(pool, resource)
  }
  
  // Verify pool state
  assert_eq(ResourcePool::available_count(pool), 10)
  assert_eq(ResourcePool::used_count(pool), 0)
  
  // Test pool cleanup
  let cleanup_result = ResourcePool::cleanup(pool)
  assert_true(cleanup_result)
}

test "memory resource management" {
  // Test memory allocation with many objects
  let objects = []
  
  // Allocate memory
  for i in 0..1000 {
    let attrs = Attributes::new()
    
    for j in 0..10 {
      let key = "memory_attr_" + j.to_string()
      let value = "memory_value_" + j.to_string()
      Attributes::set(attrs, key, StringValue(value))
    }
    
    objects.push(attrs)
  }
  
  // Verify all objects are valid
  for attrs in objects {
    assert_eq(Attributes::count(attrs), 10)
    
    for j in 0..10 {
      let key = "memory_attr_" + j.to_string()
      let expected_value = "memory_value_" + j.to_string()
      
      let result = Attributes::get(attrs, key)
      match result {
        StringValue(v) => assert_eq(v, expected_value)
        _ => assert_true(false)
      }
    }
  }
  
  // Free memory
  for attrs in objects {
    Attributes::cleanup(attrs)
  }
  
  // Verify memory was freed
  let memory_after_cleanup = Memory::get_usage()
  assert_true(memory_after_cleanup < Memory::get_limit())
}

test "resource leak detection" {
  // Enable leak detection
  LeakDetector::enable()
  
  // Create resources
  let spans = []
  let attributes = []
  
  for i in 0..100 {
    let span_ctx = SpanContext::new("leak_trace_" + i.to_string(), "leak_span_" + i.to_string(), true, "")
    let span = Span::new("leak_span_" + i.to_string(), Internal, span_ctx)
    spans.push(span)
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "leak_attr", StringValue("leak_value_" + i.to_string()))
    attributes.push(attrs)
  }
  
  // Check for leaks before cleanup
  let leaks_before = LeakDetector::detect()
  assert_true(leaks_before > 0)
  
  // Clean up resources
  for span in spans {
    Span::end(span)
    Span::cleanup(span)
  }
  
  for attrs in attributes {
    Attributes::cleanup(attrs)
  }
  
  // Check for leaks after cleanup
  let leaks_after = LeakDetector::detect()
  assert_eq(leaks_after, 0)
  
  // Disable leak detection
  LeakDetector::disable()
}

test "resource quota management" {
  // Set resource quotas
  let quotas = ResourceQuotas::new()
  ResourceQuotas::set_max_spans(quotas, 100)
  ResourceQuotas::set_max_attributes(quotas, 1000)
  ResourceQuotas::set_max_memory(quotas, 10 * 1024 * 1024) // 10MB
  
  // Use resources within quotas
  let spans = []
  
  for i in 0..50 {
    let span_ctx = SpanContext::new("quota_trace_" + i.to_string(), "quota_span_" + i.to_string(), true, "")
    let span = Span::new("quota_span_" + i.to_string(), Internal, span_ctx)
    spans.push(span)
  }
  
  // Verify quotas are not exceeded
  assert_true(ResourceQuotas::spans_used(quotas) <= ResourceQuotas::max_spans(quotas))
  assert_true(ResourceQuotas::memory_used(quotas) <= ResourceQuotas::max_memory(quotas))
  
  // Test quota enforcement
  let quota_result = ResourceQuotas::enforce(quotas)
  assert_true(quota_result)
  
  // Try to exceed quotas
  for i in 50..150 {
    let span_ctx = SpanContext::new("quota_trace_" + i.to_string(), "quota_span_" + i.to_string(), true, "")
    let span = Span::new("quota_span_" + i.to_string(), Internal, span_ctx)
    
    if ResourceQuotas::can_create_span(quotas) {
      spans.push(span)
    } else {
      // Quota exceeded, should not create span
      Span::cleanup(span)
    }
  }
  
  // Verify quotas are enforced
  assert_true(ResourceQuotas::spans_used(quotas) <= ResourceQuotas::max_spans(quotas))
  
  // Clean up
  for span in spans {
    Span::end(span)
    Span::cleanup(span)
  }
  
  // Verify quotas are updated after cleanup
  assert_eq(ResourceQuotas::spans_used(quotas), 0)
}

test "resource priority management" {
  // Create resource manager with priorities
  let manager = ResourceManager::new()
  
  // Create high priority resources
  let high_priority_spans = []
  
  for i in 0..10 {
    let span_ctx = SpanContext::new("high_trace_" + i.to_string(), "high_span_" + i.to_string(), true, "")
    let span = Span::new("high_span_" + i.to_string(), Internal, span_ctx)
    ResourceManager::set_priority(manager, span, High)
    high_priority_spans.push(span)
  }
  
  // Create low priority resources
  let low_priority_spans = []
  
  for i in 0..10 {
    let span_ctx = SpanContext::new("low_trace_" + i.to_string(), "low_span_" + i.to_string(), true, "")
    let span = Span::new("low_span_" + i.to_string(), Internal, span_ctx)
    ResourceManager::set_priority(manager, span, Low)
    low_priority_spans.push(span)
  }
  
  // Verify priorities are set
  for span in high_priority_spans {
    assert_eq(ResourceManager::get_priority(manager, span), High)
  }
  
  for span in low_priority_spans {
    assert_eq(ResourceManager::get_priority(manager, span), Low)
  }
  
  // Simulate resource pressure
  ResourceManager::simulate_pressure(manager)
  
  // Verify low priority resources are cleaned up first
  let high_remaining = ResourceManager::count_by_priority(manager, High)
  let low_remaining = ResourceManager::count_by_priority(manager, Low)
  
  assert_true(high_remaining >= low_remaining)
  
  // Clean up remaining resources
  for span in high_priority_spans {
    Span::end(span)
    Span::cleanup(span)
  }
  
  for span in low_priority_spans {
    if Span::is_recording(span) {
      Span::end(span)
      Span::cleanup(span)
    }
  }
  
  // Verify all resources are cleaned up
  assert_eq(ResourceManager::count_by_priority(manager, High), 0)
  assert_eq(ResourceManager::count_by_priority(manager, Low), 0)
}

test "resource reuse and recycling" {
  // Create resource recycler
  let recycler = ResourceRecycler::new()
  
  // Create and recycle spans
  let recycled_spans = []
  
  for i in 0..10 {
    let span_ctx = SpanContext::new("recycle_trace_" + i.to_string(), "recycle_span_" + i.to_string(), true, "")
    let span = Span::new("recycle_span_" + i.to_string(), Internal, span_ctx)
    
    // Use span
    Span::set_status(span, Ok, Some("Used span"))
    Span::end(span)
    
    // Recycle span
    ResourceRecycler::recycle(recycler, span)
  }
  
  // Verify resources are available for reuse
  assert_eq(ResourceRecycler::available_count(recycler), 10)
  
  // Reuse recycled resources
  for i in 0..10 {
    let reused_span = ResourceRecycler::get_span(recycler)
    
    // Verify span is clean and ready for reuse
    assert_eq(Span::status(reused_span), Unset)
    assert_true(Span::is_recording(reused_span))
    
    // Use reused span
    Span::set_status(reused_span, Ok, Some("Reused span"))
    Span::end(reused_span)
    
    recycled_spans.push(reused_span)
  }
  
  // Verify all recycled spans were reused
  assert_eq(ResourceRecycler::available_count(recycler), 0)
  
  // Clean up
  for span in recycled_spans {
    Span::cleanup(span)
  }
  
  // Test recycler cleanup
  let cleanup_result = ResourceRecycler::cleanup(recycler)
  assert_true(cleanup_result)
}