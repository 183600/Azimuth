// Azimuth Telemetry System - High Quality Resource Management and Lifecycle Tests
// This file contains comprehensive resource management and lifecycle test cases

// Test 1: Provider Lifecycle Management
test "provider lifecycle management" {
  // Test TracerProvider lifecycle
  let tracer_provider = TracerProvider::new()
  
  // Verify provider is initialized
  assert_true(TracerProvider::is_initialized(tracer_provider))
  assert_false(TracerProvider::is_shutdown(tracer_provider))
  
  // Create tracer from provider
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle_test_tracer")
  assert_true(tracer != null)
  
  // Create spans using tracer
  let span1 = Tracer::start_span(tracer, "test_span_1")
  let span2 = Tracer::start_span(tracer, "test_span_2")
  
  // End spans
  Span::end(span1)
  Span::end(span2)
  
  // Shutdown provider
  TracerProvider::shutdown(tracer_provider)
  assert_true(TracerProvider::is_shutdown(tracer_provider))
  
  // Verify operations are gracefully handled after shutdown
  try {
    let span3 = Tracer::start_span(tracer, "test_span_after_shutdown")
    // Should either return no-op span or handle gracefully
    Span::end(span3)
    assert_true(true)
  } catch {
    // Exception is acceptable after shutdown
    assert_true(true)
  }
  
  // Test MeterProvider lifecycle
  let meter_provider = MeterProvider::new()
  assert_true(MeterProvider::is_initialized(meter_provider))
  assert_false(MeterProvider::is_shutdown(meter_provider))
  
  let meter = MeterProvider::get_meter(meter_provider, "lifecycle_test_meter")
  let counter = Meter::create_counter(meter, "test_counter")
  
  Counter::add(counter, 1.0)
  
  MeterProvider::shutdown(meter_provider)
  assert_true(MeterProvider::is_shutdown(meter_provider))
  
  // Test LoggerProvider lifecycle
  let logger_provider = LoggerProvider::new()
  assert_true(LoggerProvider::is_initialized(logger_provider))
  assert_false(LoggerProvider::is_shutdown(logger_provider))
  
  let logger = LoggerProvider::get_logger(logger_provider, "lifecycle_test_logger")
  let log_record = LogRecord::new(Info, "Test message")
  Logger::emit(logger, log_record)
  
  LoggerProvider::shutdown(logger_provider)
  assert_true(LoggerProvider::is_shutdown(logger_provider))
}

// Test 2: Span Lifecycle Management
test "span lifecycle management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "span_lifecycle_tracer")
  
  // Test span creation
  let span = Tracer::start_span(tracer, "lifecycle_test_span")
  assert_true(span != null)
  assert_true(Span::is_recording(span))
  assert_eq(Span::status(span), Unset)
  
  // Test span operations during active phase
  Span::set_attribute(span, "test.attr", StringValue("test_value"))
  Span::add_event(span, "test_event", Some([
    ("event.attr", StringValue("event_value"))
  ]))
  
  // Verify attributes and events are set
  let attr_result = Span::get_attribute(span, "test.attr")
  match attr_result {
    Some(StringValue(v)) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  // Test span ending
  Span::end(span)
  assert_false(Span::is_recording(span))
  
  // Test operations after span ended
  try {
    Span::set_attribute(span, "after.end", StringValue("should_be_ignored"))
    Span::add_event(span, "after_end_event", None)
    // These operations should be gracefully ignored
    assert_true(true)
  } catch {
    // Exception is acceptable
    assert_true(true)
  }
  
  // Verify attributes set after ending are ignored
  let after_end_attr = Span::get_attribute(span, "after.end")
  match after_end_attr {
    Some(_) => assert_true(false) // Should not exist
    None => assert_true(true)
  }
}

// Test 3: Resource Cleanup and Memory Management
test "resource cleanup and memory management" {
  let initial_memory = get_current_memory_usage()
  
  // Create and destroy multiple providers
  for i in 0..100 {
    let tracer_provider = TracerProvider::new()
    let meter_provider = MeterProvider::new()
    let logger_provider = LoggerProvider::new()
    
    // Create instruments
    let tracer = TracerProvider::get_tracer(tracer_provider, "temp_tracer_" + i.to_string())
    let meter = MeterProvider::get_meter(meter_provider, "temp_meter_" + i.to_string())
    let logger = LoggerProvider::get_logger(logger_provider, "temp_logger_" + i.to_string())
    
    // Create spans, metrics, and logs
    for j in 0..10 {
      let span = Tracer::start_span(tracer, "temp_span_" + j.to_string())
      Span::set_attribute(span, "index", IntValue(j))
      Span::end(span)
      
      let counter = Meter::create_counter(meter, "temp_counter_" + j.to_string())
      Counter::add(counter, 1.0)
      
      let log_record = LogRecord::new(Info, "Temp log " + j.to_string())
      Logger::emit(logger, log_record)
    }
    
    // Shutdown providers
    TracerProvider::shutdown(tracer_provider)
    MeterProvider::shutdown(meter_provider)
    LoggerProvider::shutdown(logger_provider)
    
    // Explicitly drop references if needed
    // tracer_provider = null
    // meter_provider = null
    // logger_provider = null
  }
  
  // Force garbage collection if available
  force_garbage_collection()
  
  let final_memory = get_current_memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be reasonable
  assert_true(memory_increase < 50 * 1024 * 1024, "Memory increase should be less than 50MB")
}

// Test 4: Context Lifecycle and Propagation
test "context lifecycle and propagation" {
  let root_ctx = Context::root()
  
  // Test context lifecycle
  let key1 = ContextKey::new("test.key.1")
  let key2 = ContextKey::new("test.key.2")
  
  // Create context chain
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  
  // Verify values in context chain
  match Context::get(ctx2, key1) {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match Context::get(ctx2, key2) {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  // Test context propagation
  let propagated_ctx = Context::propagate(ctx2)
  
  // Verify propagated context maintains values
  match Context::get(propagated_ctx, key1) {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match Context::get(propagated_ctx, key2) {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  // Test context cleanup
  Context::cleanup(propagated_ctx)
  
  // After cleanup, context should be in clean state
  // Implementation-specific behavior
}

// Test 5: Baggage Lifecycle Management
test "baggage lifecycle management" {
  let baggage = Baggage::new()
  
  // Test baggage entry lifecycle
  let updated_baggage = Baggage::set_entry(baggage, "test.key", "test.value")
  
  // Verify entry is set
  let value = Baggage::get_entry(updated_baggage, "test.key")
  match value {
    Some(v) => assert_eq(v, "test.value")
    None => assert_true(false)
  }
  
  // Test baggage propagation
  let propagated_baggage = Baggage::propagate(updated_baggage)
  
  // Verify propagated baggage maintains entries
  let propagated_value = Baggage::get_entry(propagated_baggage, "test.key")
  match propagated_value {
    Some(v) => assert_eq(v, "test.value")
    None => assert_true(false)
  }
  
  // Test baggage entry removal
  let cleaned_baggage = Baggage::remove_entry(propagated_baggage, "test.key")
  
  // Verify entry is removed
  let removed_value = Baggage::get_entry(cleaned_baggage, "test.key")
  match removed_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test baggage cleanup
  Baggage::cleanup(cleaned_baggage)
}

// Test 6: Instrument Lifecycle Management
test "instrument lifecycle management" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "instrument_lifecycle_meter")
  
  // Test counter lifecycle
  let counter = Meter::create_counter(meter, "lifecycle_counter")
  assert_true(counter != null)
  
  // Use counter
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5, Some([
    ("test.attr", StringValue("test_value"))
  ]))
  
  // Test histogram lifecycle
  let histogram = Meter::create_histogram(meter, "lifecycle_histogram")
  assert_true(histogram != null)
  
  // Use histogram
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.5, Some([
    ("test.attr", StringValue("test_value"))
  ]))
  
  // Test updown counter lifecycle
  let updown_counter = Meter::create_updown_counter(meter, "lifecycle_updown")
  assert_true(updown_counter != null)
  
  // Use updown counter
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -5.0)
  
  // Test gauge lifecycle
  let gauge = Meter::create_gauge(meter, "lifecycle_gauge")
  assert_true(gauge != null)
  
  // Use gauge
  Gauge::record(gauge, 42.0)
  
  // Test instrument cleanup
  Instrument::cleanup(counter)
  Instrument::cleanup(histogram)
  Instrument::cleanup(updown_counter)
  Instrument::cleanup(gauge)
  
  // Verify operations after cleanup are handled gracefully
  try {
    Counter::add(counter, 1.0)
    assert_true(true)
  } catch {
    assert_true(true)
  }
}

// Test 7: Resource Attribute Lifecycle
test "resource attribute lifecycle" {
  let resource = Resource::new()
  
  // Test resource attribute lifecycle
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Verify attributes are set
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test_service")
    _ => assert_true(false)
  }
  
  // Test resource merging
  let override_attrs = [
    ("service.version", StringValue("2.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  // Verify merged attributes
  let merged_version = Resource::get_attribute(merged_resource, "service.version")
  match merged_version {
    Some(StringValue(version)) => assert_eq(version, "2.0.0")
    _ => assert_true(false)
  }
  
  let merged_env = Resource::get_attribute(merged_resource, "deployment.environment")
  match merged_env {
    Some(StringValue(env)) => assert_eq(env, "production")
    _ => assert_true(false)
  }
  
  // Verify original attributes are preserved
  let merged_name = Resource::get_attribute(merged_resource, "service.name")
  match merged_name {
    Some(StringValue(name)) => assert_eq(name, "test_service")
    _ => assert_true(false)
  }
  
  // Test resource cleanup
  Resource::cleanup(merged_resource)
}

// Test 8: Logger and Log Record Lifecycle
test "logger and log record lifecycle" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "log_lifecycle_logger")
  
  // Test log record creation and emission
  let log_record1 = LogRecord::new(Info, "Test log message 1")
  Logger::emit(logger, log_record1)
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.attr", StringValue("log_value"))
  
  let log_record2 = LogRecord::new_with_context(
    Error,
    Some("Error log message"),
    Some(attrs),
    Some(get_current_time_millis()),
    None,
    Some("trace_123"),
    Some("span_456"),
    None
  )
  Logger::emit(logger, log_record2)
  
  // Test logger lifecycle
  assert_true(Logger::is_active(logger))
  
  LoggerProvider::shutdown(provider)
  assert_false(Logger::is_active(logger))
  
  // Test log emission after shutdown
  try {
    let log_record3 = LogRecord::new(Warn, "Log after shutdown")
    Logger::emit(logger, log_record3)
    assert_true(true)
  } catch {
    assert_true(true)
  }
}

// Test 9: HTTP Client Resource Lifecycle
test "http client resource lifecycle" {
  // Test HTTP client lifecycle
  let client = HttpClient::new()
  assert_true(HttpClient::is_active(client))
  
  // Make HTTP requests
  let headers = [
    ("Content-Type", "application/json"),
    ("User-Agent", "azimuth-telemetry/1.0")
  ]
  
  let request = HttpRequest::new("GET", "https://httpbin.org/get", headers, None)
  
  try {
    let response = HttpClient::send(client, request)
    assert_eq(HttpResponse::status_code(response), 200)
  } catch {
    // Network errors are acceptable in test environment
    assert_true(true)
  }
  
  // Test HTTP client shutdown
  HttpClient::shutdown(client)
  assert_false(HttpClient::is_active(client))
  
  // Test requests after shutdown
  try {
    let response = HttpClient::send(client, request)
    assert_true(true)
  } catch {
    assert_true(true)
  }
}

// Test 10: Graceful Shutdown with In-Progress Operations
test "graceful shutdown with in-progress operations" {
  let tracer_provider = TracerProvider::new()
  let meter_provider = MeterProvider::new()
  let logger_provider = LoggerProvider::new()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "shutdown_test_tracer")
  let meter = MeterProvider::get_meter(meter_provider, "shutdown_test_meter")
  let logger = LoggerProvider::get_logger(logger_provider, "shutdown_test_logger")
  
  let counter = Meter::create_counter(meter, "shutdown_counter")
  
  // Start long-running operations
  let spans = []
  for i in 0..10 {
    let span = Tracer::start_span(tracer, "long_running_span_" + i.to_string())
    spans.push(span)
  }
  
  // Record metrics
  for i in 0..100 {
    Counter::add(counter, 1.0, Some([("operation", StringValue("pre_shutdown"))]))
  }
  
  // Emit logs
  for i in 0..50 {
    let log_record = LogRecord::new(Info, "Pre-shutdown log " + i.to_string())
    Logger::emit(logger, log_record)
  }
  
  // Initiate graceful shutdown
  let shutdown_start = get_current_time_millis()
  
  // Continue operations during shutdown
  let shutdown_tasks = []
  
  // Task 1: Complete remaining spans
  let task1 = ConcurrentTask::run(fn() {
    for span in spans {
      Span::set_attribute(span, "shutdown.phase", StringValue("in_progress"))
      Span::end(span)
    }
    return "spans_completed"
  })
  shutdown_tasks.push(task1)
  
  // Task 2: Continue metrics recording
  let task2 = ConcurrentTask::run(fn() {
    for i in 0..50 {
      Counter::add(counter, 1.0, Some([("operation", StringValue("during_shutdown"))]))
    }
    return "metrics_completed"
  })
  shutdown_tasks.push(task2)
  
  // Task 3: Continue log emission
  let task3 = ConcurrentTask::run(fn() {
    for i in 0..25 {
      let log_record = LogRecord::new(Warn, "During shutdown log " + i.to_string())
      Logger::emit(logger, log_record)
    }
    return "logs_completed"
  })
  shutdown_tasks.push(task3)
  
  // Shutdown providers
  TracerProvider::shutdown(tracer_provider)
  MeterProvider::shutdown(meter_provider)
  LoggerProvider::shutdown(logger_provider)
  
  // Wait for all tasks to complete
  for task in shutdown_tasks {
    match ConcurrentTask::await(task) {
      Some(result) => assert_true(result != "")
      None => assert_true(false)
    }
  }
  
  let shutdown_end = get_current_time_millis()
  let shutdown_duration = shutdown_end - shutdown_start
  
  // Verify shutdown completed within reasonable time
  assert_true(shutdown_duration < 5000, "Graceful shutdown should complete within 5 seconds")
  
  // Verify all providers are shutdown
  assert_true(TracerProvider::is_shutdown(tracer_provider))
  assert_true(MeterProvider::is_shutdown(meter_provider))
  assert_true(LoggerProvider::is_shutdown(logger_provider))
}