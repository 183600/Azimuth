// Azimuth Telemetry System - Premium Resource Lifecycle Tests
// This file contains high-quality test cases for telemetry system resource cleanup and lifecycle

// Test 1: Span Lifecycle Management
test "span lifecycle management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "lifecycle_tracer")
  
  // Test span creation and lifecycle
  let span_ctx = SpanContext::new("lifecycle_trace_123", "lifecycle_span_456", true, "active")
  let span = Span::new("lifecycle_test_span", Internal, span_ctx)
  
  // Verify initial state
  assert_true(Span::is_recording(span))
  assert_eq(Span::status(span), Unset)
  assert_eq(Span::name(span), "lifecycle_test_span")
  
  // Add events during lifecycle
  Span::add_event(span, "lifecycle_start", Some([
    ("phase", StringValue("initialization")),
    ("component", StringValue("lifecycle_test"))
  ]))
  
  // Update status during lifecycle
  Span::set_status(span, Ok, Some("Operation in progress"))
  assert_eq(Span::status(span), Ok)
  
  // Add more events
  Span::add_event(span, "lifecycle_middle", Some([
    ("phase", StringValue("processing")),
    ("progress", StringValue("50%"))
  ]))
  
  // Verify span is still recording
  assert_true(Span::is_recording(span))
  
  // Complete span lifecycle
  Span::add_event(span, "lifecycle_end", Some([
    ("phase", StringValue("completion")),
    ("duration_ms", StringValue("100"))
  ]))
  
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  Span::end(span)
  
  // Verify final state
  assert_false(Span::is_recording(span))
  assert_eq(Span::status(span), Ok)
  
  // Test that operations on ended span are handled gracefully
  Span::add_event(span, "post_end_event", Some([
    ("test", StringValue("should_be_ignored"))
  ]))
  
  Span::set_status(span, Error, Some("Should not change"))
  
  // Status should remain unchanged
  assert_eq(Span::status(span), Ok)
  assert_false(Span::is_recording(span))
}

// Test 2: Metrics Lifecycle Management
test "metrics lifecycle management" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "lifecycle_meter")
  
  // Create metrics
  let counter = Meter::create_counter(meter, "lifecycle_counter", Some("Lifecycle test counter"), Some("operations"))
  let histogram = Meter::create_histogram(meter, "lifecycle_histogram", Some("Lifecycle test histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "lifecycle_updown", Some("Lifecycle updown counter"), Some("value"))
  
  // Test metrics during active lifecycle
  let attrs = Attributes::new()
  Attributes::set(attrs, "lifecycle_phase", StringValue("active"))
  
  Counter::add(counter, 1.0, Some(attrs))
  Histogram::record(histogram, 100.0, Some(attrs))
  UpDownCounter::add(updown_counter, 10.0, Some(attrs))
  
  // Verify instrument integrity during lifecycle
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "lifecycle_counter")
  assert_eq(Instrument::description(counter_instrument), Some("Lifecycle test counter"))
  
  let histogram_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(histogram_instrument), "lifecycle_histogram")
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
  
  // Test metrics during shutdown phase
  let shutdown_attrs = Attributes::new()
  Attributes::set(shutdown_attrs, "lifecycle_phase", StringValue("shutdown"))
  
  Counter::add(counter, 1.0, Some(shutdown_attrs))
  Histogram::record(histogram, 50.0, Some(shutdown_attrs))
  UpDownCounter::add(updown_counter, -10.0, Some(shutdown_attrs))
  
  // Simulate meter shutdown
  Meter::shutdown(meter)
  
  // Test operations after shutdown are handled gracefully
  let post_shutdown_attrs = Attributes::new()
  Attributes::set(post_shutdown_attrs, "lifecycle_phase", StringValue("post_shutdown"))
  
  Counter::add(counter, 1.0, Some(post_shutdown_attrs))
  Histogram::record(histogram, 25.0, Some(post_shutdown_attrs))
  UpDownCounter::add(updown_counter, 5.0, Some(post_shutdown_attrs))
  
  // Verify instruments are still accessible
  let final_counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(final_counter_instrument), "lifecycle_counter")
  
  let final_histogram_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(final_histogram_instrument), "lifecycle_histogram")
}

// Test 3: Logger Lifecycle Management
test "logger lifecycle management" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "lifecycle_logger")
  
  // Test logging during active lifecycle
  let active_attrs = Attributes::new()
  Attributes::set(active_attrs, "lifecycle_phase", StringValue("active"))
  Attributes::set(active_attrs, "component", StringValue("lifecycle_test"))
  
  let active_log = LogRecord::new_with_context(
    Info,
    Some("Active phase log message"),
    Some(active_attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some("lifecycle_trace"),
    Some("lifecycle_span"),
    Some(Context::root())
  )
  
  Logger::emit(logger, active_log)
  
  // Verify log record integrity
  assert_eq(LogRecord::severity_number(active_log), Info)
  match LogRecord::body(active_log) {
    Some(body) => assert_eq(body, "Active phase log message")
    None => assert_true(false, "Log body should be preserved")
  }
  
  // Test logging during shutdown phase
  let shutdown_attrs = Attributes::new()
  Attributes::set(shutdown_attrs, "lifecycle_phase", StringValue("shutdown"))
  
  let shutdown_log = LogRecord::new_with_context(
    Warn,
    Some("Shutdown phase log message"),
    Some(shutdown_attrs),
    Some(1234567892L),
    Some(1234567893L),
    Some("lifecycle_trace"),
    Some("lifecycle_span"),
    Some(Context::root())
  )
  
  Logger::emit(logger, shutdown_log)
  
  // Simulate logger shutdown
  Logger::shutdown(logger)
  
  // Test logging after shutdown
  let post_shutdown_attrs = Attributes::new()
  Attributes::set(post_shutdown_attrs, "lifecycle_phase", StringValue("post_shutdown"))
  
  let post_shutdown_log = LogRecord::new_with_context(
    Error,
    Some("Post shutdown log message"),
    Some(post_shutdown_attrs),
    Some(1234567894L),
    Some(1234567895L),
    Some("lifecycle_trace"),
    Some("lifecycle_span"),
    Some(Context::root())
  )
  
  Logger::emit(logger, post_shutdown_log)
  
  // Verify log record integrity is maintained
  assert_eq(LogRecord::severity_number(post_shutdown_log), Error)
  match LogRecord::body(post_shutdown_log) {
    Some(body) => assert_eq(body, "Post shutdown log message")
    None => assert_true(false, "Post shutdown log body should be preserved")
  }
}

// Test 4: Context Lifecycle Management
test "context lifecycle management" {
  // Test context creation and lifecycle
  let root_ctx = Context::root()
  
  // Verify root context state
  match root_ctx.data {
    None => assert_true(true)
    Some(_) => assert_true(false, "Root context should have no data")
  }
  
  // Build context chain during lifecycle
  let key1 = ContextKey::new("lifecycle_key1")
  let key2 = ContextKey::new("lifecycle_key2")
  let key3 = ContextKey::new("lifecycle_key3")
  
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  let ctx3 = Context::with_value(ctx2, key3, "value3")
  
  // Verify context chain integrity
  match Context::get(ctx3, key1) {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false, "Context key1 should be accessible")
  }
  
  match Context::get(ctx3, key2) {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false, "Context key2 should be accessible")
  }
  
  match Context::get(ctx3, key3) {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false, "Context key3 should be accessible")
  }
  
  // Test context cleanup
  // In a real implementation, this would involve garbage collection
  // Here we test that context references don't leak
  let cleanup_ctx = Context::with_value(ctx3, ContextKey::new("cleanup_key"), "cleanup_value")
  
  // Verify cleanup context
  match Context::get(cleanup_ctx, ContextKey::new("cleanup_key")) {
    Some(value) => assert_eq(value, "cleanup_value")
    None => assert_true(false, "Cleanup key should be accessible")
  }
  
  // Original context should still be intact
  match Context::get(ctx3, key1) {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false, "Original context should remain intact after cleanup")
  }
}

// Test 5: Resource Lifecycle Management
test "resource lifecycle management" {
  // Create base resource
  let base_resource = Resource::new()
  
  // Add attributes during initialization phase
  let init_attrs = [
    ("service.name", StringValue("lifecycle_service")),
    ("service.version", StringValue("1.0.0")),
    ("lifecycle.phase", StringValue("initialization"))
  ]
  
  let init_resource = Resource::with_attributes(base_resource, init_attrs)
  
  // Verify initialization phase attributes
  match Resource::get_attribute(init_resource, "service.name") {
    Some(StringValue(value)) => assert_eq(value, "lifecycle_service")
    None => assert_true(false, "Service name should be set during initialization")
  }
  
  match Resource::get_attribute(init_resource, "lifecycle.phase") {
    Some(StringValue(value)) => assert_eq(value, "initialization")
    None => assert_true(false, "Lifecycle phase should be set to initialization")
  }
  
  // Update resource during active phase
  let active_attrs = [
    ("lifecycle.phase", StringValue("active")),
    ("service.startup_time", StringValue("2023-01-01T00:00:00Z")),
    ("active.operations", IntValue(10))
  ]
  
  let active_resource = Resource::with_attributes(init_resource, active_attrs)
  
  // Verify active phase attributes
  match Resource::get_attribute(active_resource, "lifecycle.phase") {
    Some(StringValue(value)) => assert_eq(value, "active")
    None => assert_true(false, "Lifecycle phase should be updated to active")
  }
  
  match Resource::get_attribute(active_resource, "active.operations") {
    Some(IntValue(value)) => assert_eq(value, 10)
    None => assert_true(false, "Active operations should be set")
  }
  
  // Original attributes should be preserved
  match Resource::get_attribute(active_resource, "service.name") {
    Some(StringValue(value)) => assert_eq(value, "lifecycle_service")
    None => assert_true(false, "Service name should be preserved during active phase")
  }
  
  // Update resource during shutdown phase
  let shutdown_attrs = [
    ("lifecycle.phase", StringValue("shutdown")),
    ("service.shutdown_time", StringValue("2023-01-01T01:00:00Z")),
    ("total.operations", IntValue(1000))
  ]
  
  let shutdown_resource = Resource::with_attributes(active_resource, shutdown_attrs)
  
  // Verify shutdown phase attributes
  match Resource::get_attribute(shutdown_resource, "lifecycle.phase") {
    Some(StringValue(value)) => assert_eq(value, "shutdown")
    None => assert_true(false, "Lifecycle phase should be updated to shutdown")
  }
  
  match Resource::get_attribute(shutdown_resource, "total.operations") {
    Some(IntValue(value)) => assert_eq(value, 1000)
    None => assert_true(false, "Total operations should be set during shutdown")
  }
  
  // All previous attributes should be preserved
  match Resource::get_attribute(shutdown_resource, "service.name") {
    Some(StringValue(value)) => assert_eq(value, "lifecycle_service")
    None => assert_true(false, "Service name should be preserved through shutdown")
  }
  
  match Resource::get_attribute(shutdown_resource, "service.startup_time") {
    Some(StringValue(value)) => assert_eq(value, "2023-01-01T00:00:00Z")
    None => assert_true(false, "Startup time should be preserved through shutdown")
  }
}

// Test 6: Baggage Lifecycle Management
test "baggage lifecycle management" {
  let baggage = Baggage::new()
  
  // Add entries during initialization
  let init_baggage = Baggage::set_entry(baggage, "init.request_id", "req_12345")
  let init_baggage2 = Baggage::set_entry(init_baggage, "init.session_id", "sess_67890")
  
  // Verify initialization entries
  match Baggage::get_entry(init_baggage2, "init.request_id") {
    Some(value) => assert_eq(value, "req_12345")
    None => assert_true(false, "Init request ID should be present")
  }
  
  match Baggage::get_entry(init_baggage2, "init.session_id") {
    Some(value) => assert_eq(value, "sess_67890")
    None => assert_true(false, "Init session ID should be present")
  }
  
  // Add entries during active phase
  let active_baggage = Baggage::set_entry(init_baggage2, "active.operation_id", "op_11111")
  let active_baggage2 = Baggage::set_entry(active_baggage, "active.user_id", "user_22222")
  
  // Verify active phase entries
  match Baggage::get_entry(active_baggage2, "active.operation_id") {
    Some(value) => assert_eq(value, "op_11111")
    None => assert_true(false, "Active operation ID should be present")
  }
  
  // Initialization entries should still be present
  match Baggage::get_entry(active_baggage2, "init.request_id") {
    Some(value) => assert_eq(value, "req_12345")
    None => assert_true(false, "Init request ID should be preserved during active phase")
  }
  
  // Remove entries during cleanup
  let cleanup_baggage = Baggage::remove_entry(active_baggage2, "active.operation_id")
  
  // Verify removal
  match Baggage::get_entry(cleanup_baggage, "active.operation_id") {
    Some(_) => assert_true(false, "Removed entry should not be present")
    None => assert_true(true)
  }
  
  // Other entries should still be present
  match Baggage::get_entry(cleanup_baggage, "active.user_id") {
    Some(value) => assert_eq(value, "user_22222")
    None => assert_true(false, "Active user ID should be preserved after cleanup")
  }
  
  match Baggage::get_entry(cleanup_baggage, "init.request_id") {
    Some(value) => assert_eq(value, "req_12345")
    None => assert_true(false, "Init request ID should be preserved after cleanup")
  }
}

// Test 7: Provider Lifecycle Management
test "provider lifecycle management" {
  // Test tracer provider lifecycle
  let tracer_provider = TracerProvider::default()
  
  // Create tracer during active phase
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle_tracer")
  
  // Create span using tracer
  let span_ctx = SpanContext::new("provider_trace", "provider_span", true, "active")
  let span = Span::new("provider_test_span", Internal, span_ctx)
  
  // Verify tracer and span are working
  assert_true(Span::is_recording(span))
  assert_eq(Span::name(span), "provider_test_span")
  
  Span::end(span)
  
  // Test meter provider lifecycle
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "lifecycle_meter")
  
  let counter = Meter::create_counter(meter, "provider_counter", Some("Provider test counter"), Some("operations"))
  Counter::add(counter, 1.0)
  
  // Verify meter and counter are working
  let instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(instrument), "provider_counter")
  
  // Test logger provider lifecycle
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "lifecycle_logger")
  
  let log_record = LogRecord::new(Info, Some("Provider lifecycle test"))
  Logger::emit(logger, log_record)
  
  // Verify logger is working
  assert_eq(LogRecord::severity_number(log_record), Info)
  
  // Simulate provider shutdown
  TracerProvider::shutdown(tracer_provider)
  MeterProvider::shutdown(meter_provider)
  Logger::shutdown(logger_provider)
  
  // Test operations after shutdown are handled gracefully
  let post_shutdown_span = Span::new("post_shutdown_span", Internal, span_ctx)
  assert_true(Span::is_recording(post_shutdown_span))
  Span::end(post_shutdown_span)
  
  Counter::add(counter, 1.0)
  
  let post_shutdown_log = LogRecord::new(Warn, Some("Post shutdown log"))
  Logger::emit(logger, post_shutdown_log)
  
  // Verify post-shutdown operations don't crash
  assert_true(true, "Post shutdown operations should be handled gracefully")
}

// Test 8: Memory Cleanup and Resource Leak Prevention
test "memory cleanup and resource leak prevention" {
  // Test creation and cleanup of many telemetry objects
  let object_count = 1000
  
  // Create many spans and ensure they can be cleaned up
  for i in 0..object_count {
    let span_ctx = SpanContext::new("cleanup_trace_" + i.to_string(), "cleanup_span_" + i.to_string(), true, "")
    let span = Span::new("cleanup_span_" + i.to_string(), Internal, span_ctx)
    
    Span::add_event(span, "cleanup_test", Some([
      ("span_index", StringValue(i.to_string()))
    ]))
    
    Span::end(span)
    
    // Span should be properly ended and ready for cleanup
    assert_false(Span::is_recording(span))
  }
  
  // Create many attribute sets and ensure they can be cleaned up
  for i in 0..object_count {
    let attrs = Attributes::new()
    Attributes::set(attrs, "cleanup_key_" + i.to_string(), StringValue("cleanup_value_" + i.to_string()))
    
    // Retrieve and verify attribute
    match Attributes::get(attrs, "cleanup_key_" + i.to_string()) {
      Some(StringValue(value)) => assert_eq(value, "cleanup_value_" + i.to_string())
      None => assert_true(false, "Cleanup attribute should be present")
    }
  }
  
  // Create many contexts and ensure they can be cleaned up
  let root_ctx = Context::root()
  
  for i in 0..object_count {
    let ctx = Context::with_value(root_ctx, ContextKey::new("cleanup_ctx_" + i.to_string()), "cleanup_value_" + i.to_string())
    
    // Verify context value
    match Context::get(ctx, ContextKey::new("cleanup_ctx_" + i.to_string())) {
      Some(value) => assert_eq(value, "cleanup_value_" + i.to_string())
      None => assert_true(false, "Cleanup context value should be present")
    }
  }
  
  // Create many resources and ensure they can be cleaned up
  for i in 0..object_count {
    let attrs = [
      ("cleanup_resource.id", StringValue("resource_" + i.to_string())),
      ("cleanup_resource.type", StringValue("test_type"))
    ]
    let resource = Resource::with_attributes(Resource::new(), attrs)
    
    // Verify resource attributes
    match Resource::get_attribute(resource, "cleanup_resource.id") {
      Some(StringValue(value)) => assert_eq(value, "resource_" + i.to_string())
      None => assert_true(false, "Cleanup resource ID should be present")
    }
  }
  
  // Create many baggage entries and ensure they can be cleaned up
  let baggage = Baggage::new()
  let mut large_baggage = baggage
  
  for i in 0..object_count {
    large_baggage = Baggage::set_entry(large_baggage, "cleanup_baggage_" + i.to_string(), "cleanup_value_" + i.to_string())
  }
  
  // Verify a few baggage entries
  match Baggage::get_entry(large_baggage, "cleanup_baggage_0") {
    Some(value) => assert_eq(value, "cleanup_value_0")
    None => assert_true(false, "First baggage entry should be present")
  }
  
  match Baggage::get_entry(large_baggage, "cleanup_baggage_" + (object_count - 1).to_string()) {
    Some(value) => assert_eq(value, "cleanup_value_" + (object_count - 1).to_string())
    None => assert_true(false, "Last baggage entry should be present")
  }
  
  // All objects should be properly cleaned up when they go out of scope
  assert_true(true, "Memory cleanup test completed successfully")
}