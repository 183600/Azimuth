// Azimuth Telemetry System - Resource Management and Lifecycle Tests
// This file contains comprehensive resource management and lifecycle test cases

// Test 1: Provider Lifecycle Management
test "provider lifecycle management" {
  // Create and initialize providers
  let tracer_provider = TracerProvider::new()
  let meter_provider = MeterProvider::new()
  let logger_provider = LoggerProvider::new()
  
  // Verify providers are initialized
  assert_true(TracerProvider::is_initialized(tracer_provider))
  assert_true(MeterProvider::is_initialized(meter_provider))
  assert_true(LoggerProvider::is_initialized(logger_provider))
  
  // Get instruments from providers
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle_test_tracer")
  let meter = MeterProvider::get_meter(meter_provider, "lifecycle_test_meter")
  let logger = LoggerProvider::get_logger(logger_provider, "lifecycle_test_logger")
  
  // Verify instruments are created
  assert_true(Tracer::is_valid(tracer))
  assert_true(Meter::is_valid(meter))
  assert_true(Logger::is_valid(logger))
  
  // Use instruments to create telemetry objects
  let span = Tracer::start_span(tracer, "lifecycle_test_span")
  let counter = Meter::create_counter(meter, "lifecycle_counter", None, None)
  let log_record = LogRecord::new(Info, "Lifecycle test log message")
  
  // Verify objects are created
  assert_true(Span::is_valid(span))
  assert_true(Counter::is_valid(counter))
  assert_true(LogRecord::is_valid(log_record))
  
  // Clean up objects
  Span::end(span)
  Logger::emit(logger, log_record)
  
  // Shutdown providers
  TracerProvider::shutdown(tracer_provider)
  MeterProvider::shutdown(meter_provider)
  LoggerProvider::shutdown(logger_provider)
  
  // Verify providers are shut down
  assert_false(TracerProvider::is_initialized(tracer_provider))
  assert_false(MeterProvider::is_initialized(meter_provider))
  assert_false(LoggerProvider::is_initialized(logger_provider))
}

// Test 2: Span Lifecycle Management
test "span lifecycle management" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "span_lifecycle_tracer")
  
  // Create span
  let span = Tracer::start_span(tracer, "lifecycle_span")
  
  // Verify span is in created state
  assert_eq(Span::lifecycle_state(span), Created)
  assert_true(Span::is_recording(span))
  
  // Start span
  Span::start(span)
  assert_eq(Span::lifecycle_state(span), Started)
  assert_true(Span::is_recording(span))
  
  // Add attributes and events
  Span::set_attribute(span, "test.attribute", StringValue("test_value"))
  Span::add_event(span, "test_event", None)
  
  // Verify span is still in started state
  assert_eq(Span::lifecycle_state(span), Started)
  assert_true(Span::is_recording(span))
  
  // Pause span
  Span::pause(span)
  assert_eq(Span::lifecycle_state(span), Paused)
  assert_false(Span::is_recording(span))
  
  // Resume span
  Span::resume(span)
  assert_eq(Span::lifecycle_state(span), Started)
  assert_true(Span::is_recording(span))
  
  // End span
  Span::end(span)
  assert_eq(Span::lifecycle_state(span), Ended)
  assert_false(Span::is_recording(span))
  
  // Verify span data is still accessible after ending
  assert_eq(Span::name(span), "lifecycle_span")
  
  // Test span destruction
  Span::destroy(span)
  assert_eq(Span::lifecycle_state(span), Destroyed)
  
  // Shutdown provider
  TracerProvider::shutdown(provider)
}

// Test 3: Metric Lifecycle Management
test "metric lifecycle management" {
  let provider = MeterProvider::new()
  let meter = MeterProvider::get_meter(provider, "metric_lifecycle_meter")
  
  // Create metrics
  let counter = Meter::create_counter(meter, "lifecycle_counter", None, None)
  let histogram = Meter::create_histogram(meter, "lifecycle_histogram", None, None)
  let gauge = Meter::create_gauge(meter, "lifecycle_gauge", None, None)
  
  // Verify metrics are created
  assert_eq(Counter::lifecycle_state(counter), Created)
  assert_eq(Histogram::lifecycle_state(histogram), Created)
  assert_eq(Gauge::lifecycle_state(gauge), Created)
  
  // Activate metrics
  Counter::activate(counter)
  Histogram::activate(histogram)
  Gauge::activate(gauge)
  
  // Verify metrics are active
  assert_eq(Counter::lifecycle_state(counter), Active)
  assert_eq(Histogram::lifecycle_state(histogram), Active)
  assert_eq(Gauge::lifecycle_state(gauge), Active)
  
  // Use metrics
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 100.0)
  Gauge::set(gauge, 42.0)
  
  // Verify metrics are still active
  assert_eq(Counter::lifecycle_state(counter), Active)
  assert_eq(Histogram::lifecycle_state(histogram), Active)
  assert_eq(Gauge::lifecycle_state(gauge), Active)
  
  // Deactivate metrics
  Counter::deactivate(counter)
  Histogram::deactivate(histogram)
  Gauge::deactivate(gauge)
  
  // Verify metrics are deactivated
  assert_eq(Counter::lifecycle_state(counter), Inactive)
  assert_eq(Histogram::lifecycle_state(histogram), Inactive)
  assert_eq(Gauge::lifecycle_state(gauge), Inactive)
  
  // Try to use deactivated metrics (should be no-op)
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 100.0)
  Gauge::set(gauge, 24.0)
  
  // Verify metrics are still deactivated
  assert_eq(Counter::lifecycle_state(counter), Inactive)
  assert_eq(Histogram::lifecycle_state(histogram), Inactive)
  assert_eq(Gauge::lifecycle_state(gauge), Inactive)
  
  // Destroy metrics
  Counter::destroy(counter)
  Histogram::destroy(histogram)
  Gauge::destroy(gauge)
  
  // Verify metrics are destroyed
  assert_eq(Counter::lifecycle_state(counter), Destroyed)
  assert_eq(Histogram::lifecycle_state(histogram), Destroyed)
  assert_eq(Gauge::lifecycle_state(gauge), Destroyed)
  
  // Shutdown provider
  MeterProvider::shutdown(provider)
}

// Test 4: Logger Lifecycle Management
test "logger lifecycle management" {
  let provider = LoggerProvider::new()
  let logger = LoggerProvider::get_logger(provider, "logger_lifecycle_logger")
  
  // Verify logger is created
  assert_eq(Logger::lifecycle_state(logger), Created)
  
  // Activate logger
  Logger::activate(logger)
  assert_eq(Logger::lifecycle_state(logger), Active)
  
  // Create and emit log records
  let log_record1 = LogRecord::new(Info, "Active log message")
  let log_record2 = LogRecord::new(Warn, "Warning message")
  let log_record3 = LogRecord::new(Error, "Error message")
  
  Logger::emit(logger, log_record1)
  Logger::emit(logger, log_record2)
  Logger::emit(logger, log_record3)
  
  // Verify logger is still active
  assert_eq(Logger::lifecycle_state(logger), Active)
  
  // Pause logger
  Logger::pause(logger)
  assert_eq(Logger::lifecycle_state(logger), Paused)
  
  // Try to emit log record while paused (should be buffered)
  let paused_log_record = LogRecord::new(Info, "Paused log message")
  Logger::emit(logger, paused_log_record)
  
  // Verify logger is still paused
  assert_eq(Logger::lifecycle_state(logger), Paused)
  
  // Resume logger
  Logger::resume(logger)
  assert_eq(Logger::lifecycle_state(logger), Active)
  
  // Verify buffered log records are emitted
  let buffered_count = Logger::get_buffered_count(logger)
  assert_eq(buffered_count, 0) // All buffered records should be emitted
  
  // Deactivate logger
  Logger::deactivate(logger)
  assert_eq(Logger::lifecycle_state(logger), Inactive)
  
  // Try to emit log record while inactive (should be ignored)
  let inactive_log_record = LogRecord::new(Info, "Inactive log message")
  Logger::emit(logger, inactive_log_record)
  
  // Verify logger is still inactive
  assert_eq(Logger::lifecycle_state(logger), Inactive)
  
  // Destroy logger
  Logger::destroy(logger)
  assert_eq(Logger::lifecycle_state(logger), Destroyed)
  
  // Shutdown provider
  LoggerProvider::shutdown(provider)
}

// Test 5: Resource Pool Management
test "resource pool management" {
  // Create resource pools
  let span_pool = ResourcePool::new("span_pool", 10)
  let metric_pool = ResourcePool::new("metric_pool", 20)
  let log_pool = ResourcePool::new("log_pool", 15)
  
  // Verify pools are created
  assert_eq(ResourcePool::size(span_pool), 10)
  assert_eq(ResourcePool::size(metric_pool), 20)
  assert_eq(ResourcePool::size(log_pool), 15)
  
  assert_eq(ResourcePool::available_count(span_pool), 10)
  assert_eq(ResourcePool::available_count(metric_pool), 20)
  assert_eq(ResourcePool::available_count(log_pool), 15)
  
  // Acquire resources from pools
  let span1 = ResourcePool::acquire(span_pool)
  let span2 = ResourcePool::acquire(span_pool)
  let span3 = ResourcePool::acquire(span_pool)
  
  let metric1 = ResourcePool::acquire(metric_pool)
  let metric2 = ResourcePool::acquire(metric_pool)
  
  let log1 = ResourcePool::acquire(log_pool)
  let log2 = ResourcePool::acquire(log_pool)
  let log3 = ResourcePool::acquire(log_pool)
  
  // Verify available counts decreased
  assert_eq(ResourcePool::available_count(span_pool), 7)
  assert_eq(ResourcePool::available_count(metric_pool), 18)
  assert_eq(ResourcePool::available_count(log_pool), 12)
  
  // Use resources
  match span1 {
    Some(span) => Span::set_attribute(span, "test", StringValue("value1"))
    None => assert_true(false)
  }
  
  match metric1 {
    Some(metric) => Counter::add(metric, 1.0)
    None => assert_true(false)
  }
  
  match log1 {
    Some(log) => Logger::emit(log, LogRecord::new(Info, "Test message"))
    None => assert_true(false)
  }
  
  // Release resources back to pools
  ResourcePool::release(span_pool, span1)
  ResourcePool::release(span_pool, span2)
  
  ResourcePool::release(metric_pool, metric1)
  
  ResourcePool::release(log_pool, log1)
  ResourcePool::release(log_pool, log2)
  
  // Verify available counts increased
  assert_eq(ResourcePool::available_count(span_pool), 9)
  assert_eq(ResourcePool::available_count(metric_pool), 19)
  assert_eq(ResourcePool::available_count(log_pool), 14)
  
  // Test pool exhaustion
  let mut acquired_resources = []
  for i in 0..=12 {
    let resource = ResourcePool::acquire(span_pool)
    acquired_resources.push(resource)
  }
  
  // Last acquisition should fail (pool exhausted)
  match acquired_resources[12] {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Release all acquired resources
  for resource in acquired_resources {
    match resource {
      Some(r) => ResourcePool::release(span_pool, r)
      None => () // Skip None values
    }
  }
  
  // Verify all resources are available
  assert_eq(ResourcePool::available_count(span_pool), 10)
  
  // Destroy pools
  ResourcePool::destroy(span_pool)
  ResourcePool::destroy(metric_pool)
  ResourcePool::destroy(log_pool)
}

// Test 6: Memory Management
test "memory management" {
  let initial_memory = Memory::get_allocated_bytes()
  
  // Create a large number of telemetry objects
  let mut spans = []
  let mut metrics = []
  let mut logs = []
  
  for i in 0..=1000 {
    // Create spans
    let provider = TracerProvider::new()
    let tracer = TracerProvider::get_tracer(provider, "memory_test_tracer")
    let span = Tracer::start_span(tracer, "memory_test_span_" + i.to_string())
    spans.push(span)
    
    // Create metrics
    let meter_provider = MeterProvider::new()
    let meter = MeterProvider::get_meter(meter_provider, "memory_test_meter")
    let counter = Meter::create_counter(meter, "memory_test_counter", None, None)
    metrics.push(counter)
    
    // Create logs
    let logger_provider = LoggerProvider::new()
    let logger = LoggerProvider::get_logger(logger_provider, "memory_test_logger")
    let log_record = LogRecord::new(Info, "Memory test log " + i.to_string())
    logs.push((logger, log_record))
  }
  
  let after_allocation_memory = Memory::get_allocated_bytes()
  let memory_increase = after_allocation_memory - initial_memory
  
  // Verify memory usage is reasonable
  assert_true(memory_increase > 0)
  
  // Clean up objects
  for span in spans {
    Span::end(span)
  }
  
  for (logger, log_record) in logs {
    Logger::emit(logger, log_record)
  }
  
  // Force garbage collection
  Memory::force_gc()
  
  let after_cleanup_memory = Memory::get_allocated_bytes()
  let memory_decrease = after_allocation_memory - after_cleanup_memory
  
  // Verify memory was freed
  assert_true(memory_decrease > 0)
}

// Test 7: Resource Reference Counting
test "resource reference counting" {
  // Create a shared resource
  let shared_resource = SharedResource::new("shared_test_resource")
  
  // Verify initial reference count
  assert_eq(SharedResource::ref_count(shared_resource), 1)
  
  // Create references to the shared resource
  let ref1 = SharedResource::add_ref(shared_resource)
  let ref2 = SharedResource::add_ref(shared_resource)
  let ref3 = SharedResource::add_ref(shared_resource)
  
  // Verify reference count increased
  assert_eq(SharedResource::ref_count(shared_resource), 4)
  
  // Release references
  SharedResource::release(ref1)
  assert_eq(SharedResource::ref_count(shared_resource), 3)
  
  SharedResource::release(ref2)
  assert_eq(SharedResource::ref_count(shared_resource), 2)
  
  SharedResource::release(ref3)
  assert_eq(SharedResource::ref_count(shared_resource), 1)
  
  // Release original reference
  SharedResource::release(shared_resource)
  
  // Resource should be destroyed when reference count reaches 0
  assert_true(SharedResource::is_destroyed(shared_resource))
}

// Test 8: Context Lifecycle Management
test "context lifecycle management" {
  // Create root context
  let root_ctx = Context::root()
  assert_eq(Context::lifecycle_state(root_ctx), Active)
  
  // Create child contexts
  let child_ctx1 = Context::with_value(root_ctx, ContextKey::new("key1"), "value1")
  let child_ctx2 = Context::with_value(child_ctx1, ContextKey::new("key2"), "value2")
  let child_ctx3 = Context::with_value(child_ctx2, ContextKey::new("key3"), "value3")
  
  // Verify contexts are active
  assert_eq(Context::lifecycle_state(root_ctx), Active)
  assert_eq(Context::lifecycle_state(child_ctx1), Active)
  assert_eq(Context::lifecycle_state(child_ctx2), Active)
  assert_eq(Context::lifecycle_state(child_ctx3), Active)
  
  // Verify context hierarchy
  assert_eq(Context::parent(child_ctx1), Some(root_ctx))
  assert_eq(Context::parent(child_ctx2), Some(child_ctx1))
  assert_eq(Context::parent(child_ctx3), Some(child_ctx2))
  assert_eq(Context::parent(root_ctx), None)
  
  // Verify context values
  let value1 = Context::get(child_ctx3, ContextKey::new("key1"))
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  let value2 = Context::get(child_ctx3, ContextKey::new("key2"))
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  let value3 = Context::get(child_ctx3, ContextKey::new("key3"))
  match value3 {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  // Deactivate child contexts
  Context::deactivate(child_ctx3)
  assert_eq(Context::lifecycle_state(child_ctx3), Inactive)
  
  Context::deactivate(child_ctx2)
  assert_eq(Context::lifecycle_state(child_ctx2), Inactive)
  
  Context::deactivate(child_ctx1)
  assert_eq(Context::lifecycle_state(child_ctx1), Inactive)
  
  // Root context should still be active
  assert_eq(Context::lifecycle_state(root_ctx), Active)
  
  // Deactivate root context
  Context::deactivate(root_ctx)
  assert_eq(Context::lifecycle_state(root_ctx), Inactive)
}

// Test 9: Resource Cleanup on Exception
test "resource cleanup on exception" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "exception_test_tracer")
  
  // Create span
  let span = Tracer::start_span(tracer, "exception_test_span")
  
  // Verify span is created
  assert_eq(Span::lifecycle_state(span), Created)
  
  // Simulate exception during span operation
  let result = ExceptionHandling::safe_execute({
    Span::set_attribute(span, "test.attr", StringValue("test_value"))
    Span::add_event(span, "test_event", None)
    
    // Simulate exception
    ExceptionHandling::raise_exception("Simulated exception")
    
    // This code should not be reached
    Span::end(span)
    Success(())
  })
  
  // Verify exception was caught
  match result {
    Error(err) => assert_eq(err.message, "Simulated exception")
    _ => assert_true(false)
  }
  
  // Verify span was properly cleaned up despite exception
  assert_eq(Span::lifecycle_state(span), Ended)
  
  // Shutdown provider
  TracerProvider::shutdown(provider)
}

// Test 10: Resource Leak Detection
test "resource leak detection" {
  // Enable resource leak detection
  LeakDetector::enable()
  
  // Create resources
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "leak_test_tracer")
  
  // Create spans without ending them (simulating leaks)
  let span1 = Tracer::start_span(tracer, "leaked_span1")
  let span2 = Tracer::start_span(tracer, "leaked_span2")
  let span3 = Tracer::start_span(tracer, "leaked_span3")
  
  // End only one span
  Span::end(span2)
  
  // Check for leaks
  let leaks = LeakDetector::detect_leaks()
  
  // Verify leaks are detected
  assert_eq(leaks.length(), 2) // span1 and span3 are leaked
  
  // Verify leak details
  for leak in leaks {
    match leak.resource_type {
      "Span" => assert_true(leak.resource_name.contains("leaked_span"))
      _ => assert_true(false)
    }
  }
  
  // Clean up leaked resources
  Span::end(span1)
  Span::end(span3)
  
  // Check for leaks again
  let leaks_after_cleanup = LeakDetector::detect_leaks()
  assert_eq(leaks_after_cleanup.length(), 0)
  
  // Disable leak detection
  LeakDetector::disable()
  
  // Shutdown provider
  TracerProvider::shutdown(provider)
}