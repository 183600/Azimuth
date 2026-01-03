// Azimuth Telemetry System - Memory Leak Detection and Resource Cleanup Tests
// This file contains comprehensive test cases for memory leak detection and resource cleanup

// Test 1: Span Memory Leak Detection
test "span memory leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  let initial_memory = leak_detector.get_memory_usage()
  
  // Create and destroy spans repeatedly
  for i in 0..1000 {
    let span = Span::new("test-span-" + i.to_string(), Internal, SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    
    // Add events and attributes
    Span::add_event(span, "test-event", Some([("event.key", StringValue("event-value"))]))
    Span::set_attribute(span, "span.attr", StringValue("span-value"))
    Span::set_status(span, Ok, Some("Test operation"))
    
    // End span
    Span::end(span)
    
    // Explicitly destroy span (in a real implementation, this would be handled by GC)
    Span::destroy(span)
  }
  
  // Force garbage collection
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be minimal (less than 1MB)
  assert_true(memory_increase < 1024 * 1024)
  
  // Check for span leaks
  let leaked_spans = leak_detector.check_for_span_leaks()
  assert_eq(leaked_spans.length(), 0)
}

// Test 2: Metric Memory Leak Detection
test "metric memory leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  let initial_memory = leak_detector.get_memory_usage()
  
  // Create and destroy metrics repeatedly
  for i in 0..500 {
    let provider = MeterProvider::new()
    let meter = MeterProvider::get_meter(provider, "test-meter-" + i.to_string())
    
    // Create different metric types
    let counter = Meter::create_counter(meter, "test-counter-" + i.to_string(), None, None)
    let histogram = Meter::create_histogram(meter, "test-histogram-" + i.to_string(), None, None)
    let updown_counter = Meter::create_updown_counter(meter, "test-updown-" + i.to_string(), None, None)
    let gauge = Meter::create_gauge(meter, "test-gauge-" + i.to_string(), None, None)
    
    // Record metrics
    Counter::add(counter, 1.0)
    Histogram::record(histogram, 100.0)
    UpDownCounter::add(updown_counter, 1.0)
    Gauge::set(gauge, 42.0)
    
    // Destroy metrics (in a real implementation, this would be handled by GC)
    Metric::destroy(counter)
    Metric::destroy(histogram)
    Metric::destroy(updown_counter)
    Metric::destroy(gauge)
    MeterProvider::destroy(provider)
  }
  
  // Force garbage collection
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be minimal
  assert_true(memory_increase < 1024 * 1024)
  
  // Check for metric leaks
  let leaked_metrics = leak_detector.check_for_metric_leaks()
  assert_eq(leaked_metrics.length(), 0)
}

// Test 3: Log Record Memory Leak Detection
test "log record memory leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  let initial_memory = leak_detector.get_memory_usage()
  
  // Create and destroy log records repeatedly
  for i in 0..2000 {
    let provider = LoggerProvider::new()
    let logger = LoggerProvider::get_logger(provider, "test-logger-" + i.to_string())
    
    // Create log records with different sizes
    let small_log = LogRecord::new(Info, "Small log message")
    let medium_log = LogRecord::new(Warn, "Medium log message with more content " + i.to_string())
    let large_log = LogRecord::new(Error, "Large log message with much more content " + i.to_string() + " and additional data to increase size")
    
    // Add attributes
    LogRecord::add_attribute(small_log, "small.attr", StringValue("small-value"))
    LogRecord::add_attribute(medium_log, "medium.attr", StringValue("medium-value-" + i.to_string()))
    LogRecord::add_attribute(large_log, "large.attr", StringValue("large-value-" + i.to_string()))
    
    // Emit log records
    Logger::emit(logger, small_log)
    Logger::emit(logger, medium_log)
    Logger::emit(logger, large_log)
    
    // Destroy log records (in a real implementation, this would be handled by GC)
    LogRecord::destroy(small_log)
    LogRecord::destroy(medium_log)
    LogRecord::destroy(large_log)
    LoggerProvider::destroy(provider)
  }
  
  // Force garbage collection
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be minimal
  assert_true(memory_increase < 2 * 1024 * 1024) // Allow more for logs
  
  // Check for log record leaks
  let leaked_logs = leak_detector.check_for_log_leaks()
  assert_eq(leaked_logs.length(), 0)
}

// Test 4: Resource Memory Leak Detection
test "resource memory leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  let initial_memory = leak_detector.get_memory_usage()
  
  // Create and destroy resources repeatedly
  for i in 0..100 {
    let resource = Resource::new()
    
    // Add many attributes
    for j in 0..50 {
      let key = "attr-" + j.to_string()
      let value = "value-" + i.to_string() + "-" + j.to_string()
      Resource::set_attribute(resource, key, StringValue(value))
    }
    
    // Create resource with many attributes
    let large_resource = Resource::with_attributes(Resource::new(), create_large_attribute_list(100))
    
    // Merge resources
    let merged_resource = Resource::merge(resource, large_resource)
    
    // Destroy resources (in a real implementation, this would be handled by GC)
    Resource::destroy(resource)
    Resource::destroy(large_resource)
    Resource::destroy(merged_resource)
  }
  
  // Force garbage collection
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be minimal
  assert_true(memory_increase < 1024 * 1024)
  
  // Check for resource leaks
  let leaked_resources = leak_detector.check_for_resource_leaks()
  assert_eq(leaked_resources.length(), 0)
}

// Test 5: Context Memory Leak Detection
test "context memory leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  let initial_memory = leak_detector.get_memory_usage()
  
  // Create and destroy contexts repeatedly
  for i in 0..1000 {
    let root_ctx = Context::root()
    
    // Create deep context chain
    let mut current_ctx = root_ctx
    for j in 0..10 {
      let key = ContextKey::new("key-" + j.to_string())
      let value = "value-" + i.to_string() + "-" + j.to_string()
      current_ctx = Context::with_value(current_ctx, key, value)
    }
    
    // Create context with many values
    let wide_ctx = Context::root()
    for j in 0..50 {
      let key = ContextKey::new("wide-key-" + j.to_string())
      let value = "wide-value-" + i.to_string() + "-" + j.to_string()
      wide_ctx = Context::with_value(wide_ctx, key, value)
    }
    
    // Destroy contexts (in a real implementation, this would be handled by GC)
    Context::destroy(root_ctx)
    Context::destroy(current_ctx)
    Context::destroy(wide_ctx)
  }
  
  // Force garbage collection
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be minimal
  assert_true(memory_increase < 1024 * 1024)
  
  // Check for context leaks
  let leaked_contexts = leak_detector.check_for_context_leaks()
  assert_eq(leaked_contexts.length(), 0)
}

// Test 6: Baggage Memory Leak Detection
test "baggage memory leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  let initial_memory = leak_detector.get_memory_usage()
  
  // Create and destroy baggage repeatedly
  for i in 0..500 {
    let baggage = Baggage::new()
    
    // Add many entries
    for j in 0..20 {
      let key = "baggage-key-" + j.to_string()
      let value = "baggage-value-" + i.to_string() + "-" + j.to_string()
      baggage = Baggage::set_entry(baggage, key, value)
    }
    
    // Create baggage with large values
    let large_baggage = Baggage::new()
    for j in 0..10 {
      let key = "large-key-" + j.to_string()
      let value = create_large_string(1000) // 1KB value
      large_baggage = Baggage::set_entry(large_baggage, key, value)
    }
    
    // Merge baggage
    let merged_baggage = Baggage::merge(baggage, large_baggage)
    
    // Destroy baggage (in a real implementation, this would be handled by GC)
    Baggage::destroy(baggage)
    Baggage::destroy(large_baggage)
    Baggage::destroy(merged_baggage)
  }
  
  // Force garbage collection
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be minimal
  assert_true(memory_increase < 2 * 1024 * 1024) // Allow more for large baggage
  
  // Check for baggage leaks
  let leaked_baggage = leak_detector.check_for_baggage_leaks()
  assert_eq(leaked_baggage.length(), 0)
}

// Test 7: Attribute Memory Leak Detection
test "attribute memory leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  let initial_memory = leak_detector.get_memory_usage()
  
  // Create and destroy attributes repeatedly
  for i in 0..2000 {
    let attrs = Attributes::new()
    
    // Add attributes of different types
    Attributes::set(attrs, "string.attr", StringValue("string-value-" + i.to_string()))
    Attributes::set(attrs, "int.attr", IntValue(i))
    Attributes::set(attrs, "float.attr", FloatValue(i.to_float() + 0.5))
    Attributes::set(attrs, "bool.attr", BoolValue(i % 2 == 0))
    
    // Add array attributes
    let string_array = ArrayStringValue(["item1", "item2", "item3"])
    Attributes::set(attrs, "array.string.attr", string_array)
    
    let int_array = ArrayIntValue([1, 2, 3, 4, 5])
    Attributes::set(attrs, "array.int.attr", int_array)
    
    // Add many attributes
    for j in 0..10 {
      let key = "multi.attr." + j.to_string()
      let value = StringValue("multi-value-" + i.to_string() + "-" + j.to_string())
      Attributes::set(attrs, key, value)
    }
    
    // Destroy attributes (in a real implementation, this would be handled by GC)
    Attributes::destroy(attrs)
  }
  
  // Force garbage collection
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be minimal
  assert_true(memory_increase < 1024 * 1024)
  
  // Check for attribute leaks
  let leaked_attributes = leak_detector.check_for_attribute_leaks()
  assert_eq(leaked_attributes.length(), 0)
}

// Test 8: Telemetry System Resource Cleanup
test "telemetry system resource cleanup" {
  let leak_detector = MemoryLeakDetector::new()
  let initial_memory = leak_detector.get_memory_usage()
  
  // Create and destroy telemetry systems repeatedly
  for i in 0..100 {
    let telemetry_system = TelemetrySystem::new()
    
    // Configure system
    TelemetrySystem::configure(telemetry_system, create_test_configuration())
    
    // Create resources
    let tracer = TelemetrySystem::get_tracer(telemetry_system)
    let meter = TelemetrySystem::get_meter(telemetry_system)
    let logger = TelemetrySystem::get_logger(telemetry_system)
    
    // Use resources
    for j in 0..10 {
      let span = Tracer::start_span(tracer, "test-span-" + j.to_string())
      Span::add_event(span, "test-event", None)
      Span::end(span)
      
      let counter = Meter::get_counter(meter, "test-counter")
      Counter::add(counter, 1.0)
      
      let log_record = LogRecord::new(Info, "Test log message " + j.to_string())
      Logger::emit(logger, log_record)
    }
    
    // Shutdown system
    TelemetrySystem::shutdown(telemetry_system)
    
    // Destroy system (in a real implementation, this would be handled by GC)
    TelemetrySystem::destroy(telemetry_system)
  }
  
  // Force garbage collection
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be minimal
  assert_true(memory_increase < 5 * 1024 * 1024) // Allow more for complex system
  
  // Check for system leaks
  let leaked_systems = leak_detector.check_for_system_leaks()
  assert_eq(leaked_systems.length(), 0)
}

// Test 9: Long-running Resource Leak Detection
test "long-running resource leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  let initial_memory = leak_detector.get_memory_usage()
  
  // Simulate long-running application
  let telemetry_system = TelemetrySystem::new()
  TelemetrySystem::configure(telemetry_system, create_test_configuration())
  
  let tracer = TelemetrySystem::get_tracer(telemetry_system)
  let meter = TelemetrySystem::get_meter(telemetry_system)
  let logger = TelemetrySystem::get_logger(telemetry_system)
  
  // Run for many iterations
  for i in 0..10000 {
    // Create short-lived resources
    let span = Tracer::start_span(tracer, "short-lived-span")
    Span::add_event(span, "short-event", None)
    Span::end(span)
    
    let counter = Meter::get_counter(meter, "short-lived-counter")
    Counter::add(counter, 1.0)
    
    let log_record = LogRecord::new(Info, "Short-lived log " + i.to_string())
    Logger::emit(logger, log_record)
    
    // Periodically check memory usage
    if i % 1000 == 0 {
      let current_memory = leak_detector.get_memory_usage()
      let memory_increase = current_memory - initial_memory
      
      // Memory increase should be reasonable
      assert_true(memory_increase < 10 * 1024 * 1024) // Less than 10MB
      
      // Check for leaks
      let leaked_spans = leak_detector.check_for_span_leaks()
      let leaked_metrics = leak_detector.check_for_metric_leaks()
      let leaked_logs = leak_detector.check_for_log_leaks()
      
      assert_eq(leaked_spans.length(), 0)
      assert_eq(leaked_metrics.length(), 0)
      assert_eq(leaked_logs.length(), 0)
    }
  }
  
  // Shutdown system
  TelemetrySystem::shutdown(telemetry_system)
  TelemetrySystem::destroy(telemetry_system)
  
  // Force garbage collection
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be minimal after cleanup
  assert_true(memory_increase < 5 * 1024 * 1024)
}

// Test 10: Resource Cleanup Under Error Conditions
test "resource cleanup under error conditions" {
  let leak_detector = MemoryLeakDetector::new()
  let initial_memory = leak_detector.get_memory_usage()
  
  // Test cleanup under various error conditions
  
  // Test with invalid configuration
  let result = TelemetrySystem::try_configure_with_invalid_config()
  match result {
    Success(_) => assert_true(false) // Should fail
    Error(_) => {
      // System should still be clean after failed configuration
      let leaked_resources = leak_detector.check_for_system_leaks()
      assert_eq(leaked_resources.length(), 0)
    }
  }
  
  // Test with resource creation failures
  for i in 0..100 {
    let result = Span::try_create_with_invalid_context()
    match result {
      Success(_) => assert_true(false) // Should fail
      Error(_) => {
        // No leaks should occur
        let leaked_spans = leak_detector.check_for_span_leaks()
        assert_eq(leaked_spans.length(), 0)
      }
    }
    
    let result = Metric::try_create_with_invalid_name()
    match result {
      Success(_) => assert_true(false) // Should fail
      Error(_) => {
        // No leaks should occur
        let leaked_metrics = leak_detector.check_for_metric_leaks()
        assert_eq(leaked_metrics.length(), 0)
      }
    }
    
    let result = LogRecord::try_create_with_invalid_severity()
    match result {
      Success(_) => assert_true(false) // Should fail
      Error(_) => {
        // No leaks should occur
        let leaked_logs = leak_detector.check_for_log_leaks()
        assert_eq(leaked_logs.length(), 0)
      }
    }
  }
  
  // Test with partial failures during shutdown
  let telemetry_system = TelemetrySystem::new()
  TelemetrySystem::configure(telemetry_system, create_test_configuration())
  
  // Simulate partial shutdown failure
  let shutdown_result = TelemetrySystem::try_shutdown_with_failure(telemetry_system)
  match shutdown_result {
    Success(_) => assert_true(false) // Should fail
    Error(_) => {
      // Resources should still be cleaned up as much as possible
      let leaked_systems = leak_detector.check_for_system_leaks()
      assert_true(leaked_systems.length() < 5) // Some leaks might be expected under failure
    }
  }
  
  // Force garbage collection
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be minimal even under error conditions
  assert_true(memory_increase < 10 * 1024 * 1024)
}

// Helper functions
fn create_large_attribute_list(count : Int) -> Array[(String, AttributeValue)] {
  let mut attributes = []
  
  for i in 0..count {
    let key = "large-attr-" + i.to_string()
    let value = StringValue("large-value-" + i.to_string())
    attributes = attributes + [(key, value)]
  }
  
  attributes
}

fn create_large_string(size : Int) -> String {
  let mut result = ""
  let base = "a"
  
  while result.length() < size {
    result = result + base
  }
  
  result
}

fn create_test_configuration() -> Configuration {
  let config = Configuration::new()
  Configuration::set_value(config, "service.name", "test-service")
  Configuration::set_value(config, "service.version", "1.0.0")
  Configuration::set_value(config, "telemetry.enabled", "true")
  Configuration::set_value(config, "telemetry.exporter.type", "stdout")
  config
}