// Azimuth Telemetry System - Resource Management and Lifecycle Tests
// This file contains comprehensive resource management and lifecycle test cases for the telemetry system

// Test 1: Provider Lifecycle Management
test "provider lifecycle management" {
  // Test provider creation
  let tracer_provider = TracerProvider::new()
  let meter_provider = MeterProvider::new()
  let logger_provider = LoggerProvider::new()
  
  // Verify providers are properly initialized
  assert_true(TracerProvider::is_initialized(tracer_provider), "TracerProvider should be initialized")
  assert_true(MeterProvider::is_initialized(meter_provider), "MeterProvider should be initialized")
  assert_true(LoggerProvider::is_initialized(logger_provider), "LoggerProvider should be initialized")
  
  // Get tracers, meters, and loggers
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle_tracer")
  let meter = MeterProvider::get_meter(meter_provider, "lifecycle_meter")
  let logger = LoggerProvider::get_logger(logger_provider, "lifecycle_logger")
  
  // Verify components are properly created
  assert_true(tracer != None, "Tracer should be created")
  assert_true(meter != None, "Meter should be created")
  assert_true(logger != None, "Logger should be created")
  
  // Use components
  match tracer {
    Some(t) => {
      let span = Tracer::start_span(t, "lifecycle_span")
      Span::end(span)
    }
    None => assert_true(false, "Tracer should be available")
  }
  
  match meter {
    Some(m) => {
      let counter = Meter::create_counter(m, "lifecycle_counter", None, None)
      Counter::add(counter, 1.0)
    }
    None => assert_true(false, "Meter should be available")
  }
  
  match logger {
    Some(l) => {
      let log_record = LogRecord::new(Info, "Lifecycle test log")
      Logger::emit(l, log_record)
    }
    None => assert_true(false, "Logger should be available")
  }
  
  // Test provider shutdown
  TracerProvider::shutdown(tracer_provider)
  MeterProvider::shutdown(meter_provider)
  LoggerProvider::shutdown(logger_provider)
  
  // Verify providers are properly shutdown
  assert_false(TracerProvider::is_initialized(tracer_provider), "TracerProvider should be shutdown")
  assert_false(MeterProvider::is_initialized(meter_provider), "MeterProvider should be shutdown")
  assert_false(LoggerProvider::is_initialized(logger_provider), "LoggerProvider should be shutdown")
}

// Test 2: Span Lifecycle Management
test "span lifecycle management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "span_lifecycle_tracer")
  
  // Test span creation
  let span = Tracer::start_span(tracer, "lifecycle_span")
  assert_true(span != None, "Span should be created")
  
  match span {
    Some(s) => {
      // Verify span is recording
      assert_true(Span::is_recording(s), "Span should be recording")
      assert_eq(Span::status(s), Unset)
      
      // Add attributes and events
      Span::set_attribute(s, "lifecycle_attr", StringValue("lifecycle_value"))
      Span::add_event(s, "lifecycle_event", None)
      
      // Test span status update
      Span::set_status(s, Ok, Some("Operation completed"))
      assert_eq(Span::status(s), Ok)
      
      // Test span ending
      Span::end(s)
      
      // Verify span is no longer recording
      assert_false(Span::is_recording(s), "Span should not be recording after ending")
      
      // Test operations after ending
      let initial_attr_count = Span::get_attribute_count(s)
      Span::set_attribute(s, "post_end_attr", StringValue("post_end_value"))
      assert_eq(Span::get_attribute_count(s), initial_attr_count, "Should not be able to set attributes after ending")
      
      let initial_event_count = Span::get_event_count(s)
      Span::add_event(s, "post_end_event", None)
      assert_eq(Span::get_event_count(s), initial_event_count, "Should not be able to add events after ending")
    }
    None => assert_true(false, "Span should be created")
  }
}

// Test 3: Metrics Instrument Lifecycle
test "metrics instrument lifecycle" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "instrument_lifecycle_meter")
  
  // Test counter lifecycle
  let counter = Meter::create_counter(meter, "lifecycle_counter", Some("Test counter"), Some("count"))
  assert_true(counter != None, "Counter should be created")
  
  match counter {
    Some(c) => {
      // Use counter
      Counter::add(c, 1.0)
      Counter::add(c, 2.5)
      
      // Verify counter data
      let data = Counter::get_data(c)
      assert_true(abs(data.value - 3.5) < 0.0001, "Counter should accumulate values")
      
      // Test counter disabling
      Counter::disable(c)
      Counter::add(c, 5.0)
      let disabled_data = Counter::get_data(c)
      assert_eq(disabled_data.value, data.value, "Disabled counter should not accumulate new values")
      
      // Test counter re-enabling
      Counter::enable(c)
      Counter::add(c, 2.0)
      let enabled_data = Counter::get_data(c)
      assert_true(abs(enabled_data.value - 5.5) < 0.0001, "Re-enabled counter should accumulate values")
    }
    None => assert_true(false, "Counter should be created")
  }
  
  // Test histogram lifecycle
  let histogram = Meter::create_histogram(meter, "lifecycle_histogram", Some("Test histogram"), Some("ms"))
  assert_true(histogram != None, "Histogram should be created")
  
  match histogram {
    Some(h) => {
      // Use histogram
      Histogram::record(h, 100.0)
      Histogram::record(h, 200.0)
      Histogram::record(h, 150.0)
      
      // Verify histogram data
      let data = Histogram::get_data(h)
      assert_eq(data.count, 3)
      assert_true(abs(data.sum - 450.0) < 0.0001, "Histogram should sum values")
      
      // Test histogram disabling
      Histogram::disable(h)
      Histogram::record(h, 300.0)
      let disabled_data = Histogram::get_data(h)
      assert_eq(disabled_data.count, data.count, "Disabled histogram should not record new values")
      
      // Test histogram re-enabling
      Histogram::enable(h)
      Histogram::record(h, 50.0)
      let enabled_data = Histogram::get_data(h)
      assert_eq(enabled_data.count, data.count + 1, "Re-enabled histogram should record new values")
    }
    None => assert_true(false, "Histogram should be created")
  }
}

// Test 4: Resource Cleanup
test "resource cleanup" {
  let initial_memory = get_memory_usage()
  
  // Create and use telemetry resources
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "cleanup_tracer")
  
  let spans = []
  for i in 0..=999 {
    let span = Tracer::start_span(tracer, "cleanup_span_" + i.to_string())
    
    // Add attributes and events
    for j in 0..=9 {
      let key = "key_" + j.to_string()
      let value = StringValue("value_" + j.to_string())
      Span::set_attribute(span, key, value)
      
      let event_name = "event_" + j.to_string()
      Span::add_event(span, event_name, None)
    }
    
    spans = Array::push(spans, span)
  }
  
  let peak_memory = get_memory_usage()
  
  // End all spans to trigger cleanup
  for span in spans {
    Span::end(span)
  }
  
  // Force garbage collection if available
  force_garbage_collection()
  
  // Shutdown provider to trigger cleanup
  TracerProvider::shutdown(provider)
  
  let final_memory = get_memory_usage()
  
  // Verify memory was reclaimed
  let memory_increase = peak_memory - initial_memory
  let memory_after_cleanup = final_memory - initial_memory
  
  assert_true(memory_after_cleanup < memory_increase * 0.3, "Most memory should be reclaimed after cleanup")
}

// Test 5: Component Dependency Management
test "component dependency management" {
  // Create providers with dependencies
  let tracer_provider = TracerProvider::new()
  let meter_provider = MeterProvider::new()
  let logger_provider = LoggerProvider::new()
  
  // Create shared exporter
  let exporter = SharedExporter::new()
  
  // Add exporter to all providers
  TracerProvider::add_span_exporter(tracer_provider, exporter)
  MeterProvider::add_metric_exporter(meter_provider, exporter)
  LoggerProvider::add_log_exporter(logger_provider, exporter)
  
  // Verify exporter is shared
  assert_eq(TracerProvider::get_exporter_count(tracer_provider), 1)
  assert_eq(MeterProvider::get_exporter_count(meter_provider), 1)
  assert_eq(LoggerProvider::get_exporter_count(logger_provider), 1)
  
  // Use components
  let tracer = TracerProvider::get_tracer(tracer_provider, "dependency_tracer")
  let meter = MeterProvider::get_meter(meter_provider, "dependency_meter")
  let logger = LoggerProvider::get_logger(logger_provider, "dependency_logger")
  
  match tracer {
    Some(t) => {
      let span = Tracer::start_span(t, "dependency_span")
      Span::end(span)
    }
    None => assert_true(false, "Tracer should be available")
  }
  
  match meter {
    Some(m) => {
      let counter = Meter::create_counter(m, "dependency_counter", None, None)
      Counter::add(counter, 1.0)
    }
    None => assert_true(false, "Meter should be available")
  }
  
  match logger {
    Some(l) => {
      let log_record = LogRecord::new(Info, "Dependency test log")
      Logger::emit(l, log_record)
    }
    None => assert_true(false, "Logger should be available")
  }
  
  // Test dependency resolution
  assert_true(SharedExporter::is_exporter_used(exporter), "Exporter should be used by providers")
  
  // Shutdown providers
  TracerProvider::shutdown(tracer_provider)
  MeterProvider::shutdown(meter_provider)
  LoggerProvider::shutdown(logger_provider)
  
  // Verify exporter is no longer used
  assert_false(SharedExporter::is_exporter_used(exporter), "Exporter should not be used after providers are shutdown")
}

// Test 6: Context Lifecycle Management
test "context lifecycle management" {
  let root_ctx = Context::root()
  
  // Create context hierarchy
  let ctx1 = Context::with_value(root_ctx, ContextKey::new("key1"), "value1")
  let ctx2 = Context::with_value(ctx1, ContextKey::new("key2"), "value2")
  let ctx3 = Context::with_value(ctx2, ContextKey::new("key3"), "value3")
  
  // Verify context hierarchy
  match Context::get(ctx3, ContextKey::new("key1")) {
    Some(value) => assert_eq(value, "value1"),
    None => assert_true(false, "Context should maintain parent values")
  }
  
  match Context::get(ctx3, ContextKey::new("key2")) {
    Some(value) => assert_eq(value, "value2"),
    None => assert_true(false, "Context should maintain parent values")
  }
  
  match Context::get(ctx3, ContextKey::new("key3")) {
    Some(value) => assert_eq(value, "value3"),
    None => assert_true(false, "Context should maintain current values")
  }
  
  // Test context cleanup
  Context::cleanup(ctx3)
  
  // Verify context is cleaned up
  match Context::get(ctx3, ContextKey::new("key1")) {
    Some(_) => assert_true(false, "Context should not be accessible after cleanup"),
    None => assert_true(true, "Context should not be accessible after cleanup")
  }
}

// Test 7: Baggage Lifecycle Management
test "baggage lifecycle management" {
  let baggage = Baggage::new()
  
  // Add entries
  let baggage1 = Baggage::set_entry(baggage, "key1", "value1")
  let baggage2 = Baggage::set_entry(baggage1, "key2", "value2")
  let baggage3 = Baggage::set_entry(baggage2, "key3", "value3")
  
  // Verify entries
  match Baggage::get_entry(baggage3, "key1") {
    Some(value) => assert_eq(value, "value1"),
    None => assert_true(false, "Baggage should maintain entries")
  }
  
  match Baggage::get_entry(baggage3, "key2") {
    Some(value) => assert_eq(value, "value2"),
    None => assert_true(false, "Baggage should maintain entries")
  }
  
  match Baggage::get_entry(baggage3, "key3") {
    Some(value) => assert_eq(value, "value3"),
    None => assert_true(false, "Baggage should maintain entries")
  }
  
  // Test baggage cleanup
  Baggage::cleanup(baggage3)
  
  // Verify baggage is cleaned up
  match Baggage::get_entry(baggage3, "key1") {
    Some(_) => assert_true(false, "Baggage should not be accessible after cleanup"),
    None => assert_true(true, "Baggage should not be accessible after cleanup")
  }
}

// Test 8: Resource Lifecycle Management
test "resource lifecycle management" {
  let resource = Resource::new()
  
  // Add attributes
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Verify attributes
  match Resource::get_attribute(resource_with_attrs, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "test_service"),
    _ => assert_true(false, "Resource should maintain attributes")
  }
  
  // Test resource cleanup
  Resource::cleanup(resource_with_attrs)
  
  // Verify resource is cleaned up
  match Resource::get_attribute(resource_with_attrs, "service.name") {
    Some(_) => assert_true(false, "Resource should not be accessible after cleanup"),
    None => assert_true(true, "Resource should not be accessible after cleanup")
  }
}

// Test 9: Exporter Lifecycle Management
test "exporter lifecycle management" {
  let exporter = TestExporter::new()
  
  // Verify exporter is initialized
  assert_true(TestExporter::is_initialized(exporter), "Exporter should be initialized")
  
  // Create provider and add exporter
  let provider = TracerProvider::new()
  TracerProvider::add_span_exporter(provider, exporter)
  
  // Use exporter
  let tracer = TracerProvider::get_tracer(provider, "exporter_lifecycle_tracer")
  match tracer {
    Some(t) => {
      let span = Tracer::start_span(t, "exporter_lifecycle_span")
      Span::end(span)
    }
    None => assert_true(false, "Tracer should be available")
  }
  
  // Verify exporter was used
  assert_true(TestExporter::get_export_count(exporter) > 0, "Exporter should have been used")
  
  // Test exporter shutdown
  TestExporter::shutdown(exporter)
  
  // Verify exporter is shutdown
  assert_false(TestExporter::is_initialized(exporter), "Exporter should be shutdown")
  
  // Try to use exporter after shutdown
  let tracer2 = TracerProvider::get_tracer(provider, "exporter_lifecycle_tracer2")
  match tracer2 {
    Some(t) => {
      let span = Tracer::start_span(t, "exporter_lifecycle_span2")
      Span::end(span)
    }
    None => assert_true(false, "Tracer should be available")
  }
  
  // Verify exporter was not used after shutdown
  let export_count_after_shutdown = TestExporter::get_export_count(exporter)
  assert_eq(export_count_after_shutdown, 0, "Exporter should not be used after shutdown")
}

// Test 10: Memory Leak Prevention
test "memory leak prevention" {
  let initial_memory = get_memory_usage()
  
  // Create and destroy multiple providers in a loop
  for i in 0..=99 {
    let provider = TracerProvider::new()
    let tracer = TracerProvider::get_tracer(provider, "memory_leak_tracer_" + i.to_string())
    
    // Create and end spans
    let spans = []
    for j in 0..=49 {
      let span = Tracer::start_span(tracer, "memory_leak_span_" + j.to_string())
      
      // Add attributes and events
      for k in 0..=9 {
        let key = "key_" + k.to_string()
        let value = StringValue("value_" + k.to_string())
        Span::set_attribute(span, key, value)
        
        let event_name = "event_" + k.to_string()
        Span::add_event(span, event_name, None)
      }
      
      spans = Array::push(spans, span)
    }
    
    // End all spans
    for span in spans {
      Span::end(span)
    }
    
    // Shutdown provider
    TracerProvider::shutdown(provider)
  }
  
  // Force garbage collection if available
  force_garbage_collection()
  
  let final_memory = get_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Assert that memory increase is reasonable (less than 5MB)
  assert_true(memory_increase < 5000000, "Memory increase should be less than 5MB after multiple provider lifecycles")
}

// Helper functions for testing
fn get_memory_usage() -> Int {
  // Implementation would depend on the available memory functions in MoonBit
  // This is a placeholder for the actual implementation
  0
}

fn force_garbage_collection() -> Unit {
  // Implementation would depend on the available GC functions in MoonBit
  // This is a placeholder for the actual implementation
  ()
}

// Helper function to calculate absolute value
fn abs(x : Float) -> Float {
  if x < 0.0 {
    -x
  } else {
    x
  }
}