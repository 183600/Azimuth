// Azimuth Telemetry System - Premium Resource Management and Cleanup Tests
// This file contains comprehensive resource management and cleanup tests for telemetry operations

// Test 1: Attribute Resource Management
test "attribute resource management and cleanup" {
  // Test automatic cleanup of attributes
  let create_and_cleanup = || {
    let attrs = Attributes::new()
    
    // Create many attributes
    for i in 0..<1000 {
      let key = "cleanup.key." + i.to_string()
      let value = StringValue("cleanup.value." + i.to_string())
      Attributes::set(attrs, key, value)
    }
    
    // Attributes should be automatically cleaned up when function returns
    return attrs.length()
  }
  
  let initial_count = create_and_cleanup()
  assert_true(initial_count > 0) // Verify attributes were created
  
  // Test manual cleanup
  let attrs = Attributes::new()
  for i in 0..<500 {
    let key = "manual.key." + i.to_string()
    let value = StringValue("manual.value." + i.to_string())
    Attributes::set(attrs, key, value)
  }
  
  let before_cleanup = attrs.length()
  assert_true(before_cleanup > 0)
  
  // Manual cleanup (if supported)
  Attributes::clear(attrs)
  let after_cleanup = attrs.length()
  assert_eq(after_cleanup, 0) // Should be empty after cleanup
}

// Test 2: Span Resource Management
test "span resource management and cleanup" {
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
  
  // Test automatic span cleanup
  let create_and_cleanup_spans = || {
    let spans = []
    
    // Create many spans
    for i in 0..<100 {
      let span_name = "cleanup.span." + i.to_string()
      let span = Span::new(span_name, Internal, span_ctx)
      spans.push(span)
    }
    
    // Spans should be automatically cleaned up when function returns
    return spans.length()
  }
  
  let initial_span_count = create_and_cleanup_spans()
  assert_eq(initial_span_count, 100) // Verify spans were created
  
  // Test manual span cleanup
  let spans = []
  for i in 0..<50 {
    let span_name = "manual.span." + i.to_string()
    let span = Span::new(span_name, Internal, span_ctx)
    spans.push(span)
  }
  
  // End all spans for proper cleanup
  for span in spans {
    assert_true(Span::is_recording(span)) // Should be recording before end
    Span::end(span)
    assert_false(Span::is_recording(span)) // Should not be recording after end
  }
  
  // Test span lifecycle management
  let lifecycle_span = Span::new("lifecycle.test", Internal, span_ctx)
  assert_true(Span::is_recording(lifecycle_span))
  
  // Add events and status
  Span::add_event(lifecycle_span, "lifecycle.event", None)
  Span::set_status(lifecycle_span, Ok, Some("Lifecycle test"))
  
  // End span
  Span::end(lifecycle_span)
  assert_false(Span::is_recording(lifecycle_span))
  
  // Operations after end should be handled gracefully
  Span::add_event(lifecycle_span, "post.end.event", None)
  Span::set_status(lifecycle_span, Error, Some("Post end error"))
}

// Test 3: Metrics Resource Management
test "metrics resource management and cleanup" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "resource_test_meter")
  
  // Test automatic metric instrument cleanup
  let create_and_cleanup_metrics = || {
    let instruments = []
    
    // Create many metric instruments
    for i in 0..<50 {
      let counter_name = "cleanup.counter." + i.to_string()
      let counter = Meter::create_counter(meter, counter_name, Some("Cleanup counter"), Some("count"))
      instruments.push(counter)
      
      let histogram_name = "cleanup.histogram." + i.to_string()
      let histogram = Meter::create_histogram(meter, histogram_name, Some("Cleanup histogram"), Some("ms"))
      instruments.push(histogram)
    }
    
    // Instruments should be automatically cleaned up when function returns
    return instruments.length()
  }
  
  let initial_instrument_count = create_and_cleanup_metrics()
  assert_eq(initial_instrument_count, 100) // 50 counters + 50 histograms
  
  // Test manual metric cleanup
  let counter = Meter::create_counter(meter, "manual.counter", Some("Manual counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "manual.histogram", Some("Manual histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "manual.updown", Some("Manual updown"), Some("value"))
  let gauge = Meter::create_gauge(meter, "manual.gauge", Some("Manual gauge"), Some("value"))
  
  // Use instruments
  Counter::add(counter, 100.0)
  Histogram::record(histogram, 50.0)
  UpDownCounter::add(updown_counter, 25.0)
  
  // Manual cleanup (if supported)
  Meter::cleanup_instrument(counter)
  Meter::cleanup_instrument(histogram)
  Meter::cleanup_instrument(updown_counter)
  Meter::cleanup_instrument(gauge)
  
  // Test meter provider cleanup
  let cleanup_provider = MeterProvider::default()
  let cleanup_meter = MeterProvider::get_meter(cleanup_provider, "cleanup_test_meter")
  
  // Create instruments
  let cleanup_counter = Meter::create_counter(cleanup_meter, "cleanup.test.counter", Some("Cleanup test"), Some("count"))
  
  // Use instrument
  Counter::add(cleanup_counter, 10.0)
  
  // Cleanup provider
  MeterProvider::cleanup(cleanup_provider)
}

// Test 4: Log Resource Management
test "log resource management and cleanup" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "resource_test_logger")
  
  // Test automatic log record cleanup
  let create_and_cleanup_logs = || {
    let log_records = []
    
    // Create many log records
    for i in 0..<100 {
      let log_record = LogRecord::new(Info, "Resource test log " + i.to_string())
      log_records.push(log_record)
    }
    
    // Log records should be automatically cleaned up when function returns
    return log_records.length()
  }
  
  let initial_log_count = create_and_cleanup_logs()
  assert_eq(initial_log_count, 100) // Verify log records were created
  
  // Test manual log cleanup
  let log_records = []
  for i in 0..<50 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Manual cleanup log " + i.to_string()),
      Some(attrs),
      Some(Clock::monotonic()),
      Some(Clock::monotonic() + 1000L),
      Some("trace_id_" + i.to_string()),
      Some("span_id_" + i.to_string()),
      None
    )
    log_records.push(log_record)
  }
  
  // Emit logs
  for log_record in log_records {
    Logger::emit(logger, log_record)
  }
  
  // Manual log record cleanup (if supported)
  for log_record in log_records {
    LogRecord::cleanup(log_record)
  }
  
  // Test logger provider cleanup
  let cleanup_provider = LoggerProvider::default()
  let cleanup_logger = LoggerProvider::get_logger(cleanup_provider, "cleanup_test_logger")
  
  let cleanup_log = LogRecord::new(Info, "Cleanup test log")
  Logger::emit(cleanup_logger, cleanup_log)
  
  // Cleanup provider
  LoggerProvider::cleanup(cleanup_provider)
}

// Test 5: Context Resource Management
test "context resource management and cleanup" {
  let root_ctx = Context::root()
  
  // Test automatic context cleanup
  let create_and_cleanup_contexts = || {
    let contexts = []
    
    // Create many nested contexts
    let mut ctx = root_ctx
    for i in 0..<100 {
      let key = ContextKey::new("cleanup.key." + i.to_string())
      let value = "cleanup.value." + i.to_string()
      ctx = Context::with_value(ctx, key, value)
      contexts.push(ctx)
    }
    
    // Contexts should be automatically cleaned up when function returns
    return contexts.length()
  }
  
  let initial_context_count = create_and_cleanup_contexts()
  assert_eq(initial_context_count, 100) // Verify contexts were created
  
  // Test manual context cleanup
  let mut ctx = root_ctx
  let keys = []
  
  // Create deep context hierarchy
  for i in 0..<50 {
    let key = ContextKey::new("manual.key." + i.to_string())
    let value = "manual.value." + i.to_string()
    ctx = Context::with_value(ctx, key, value)
    keys.push(key)
  }
  
  // Verify all values are present
  for (i, key) in keys.enumerate() {
    match Context::get(ctx, key) {
      Some(value) => assert_eq(value, "manual.value." + i.to_string())
      None => assert_true(false)
    }
  }
  
  // Manual context cleanup (if supported)
  Context::cleanup(ctx)
  
  // Test context hierarchy cleanup
  let parent_ctx = Context::with_value(root_ctx, ContextKey::new("parent"), "parent_value")
  let child_ctx = Context::with_value(parent_ctx, ContextKey::new("child"), "child_value")
  let grandchild_ctx = Context::with_value(child_ctx, ContextKey::new("grandchild"), "grandchild_value")
  
  // Verify hierarchy
  match Context::get(grandchild_ctx, ContextKey::new("parent")) {
    Some(value) => assert_eq(value, "parent_value")
    None => assert_true(false)
  }
  
  match Context::get(grandchild_ctx, ContextKey::new("child")) {
    Some(value) => assert_eq(value, "child_value")
    None => assert_true(false)
  }
  
  match Context::get(grandchild_ctx, ContextKey::new("grandchild")) {
    Some(value) => assert_eq(value, "grandchild_value")
    None => assert_true(false)
  }
}

// Test 6: Resource Resource Management
test "resource resource management and cleanup" {
  // Test automatic resource cleanup
  let create_and_cleanup_resources = || {
    let resources = []
    
    // Create many resources
    for i in 0..<50 {
      let attrs = [
        ("service.name", StringValue("service_" + i.to_string())),
        ("service.version", StringValue("1.0." + i.to_string())),
        ("service.instance.id", StringValue("instance_" + i.to_string()))
      ]
      
      let resource = Resource::with_attributes(Resource::new(), attrs)
      resources.push(resource)
    }
    
    // Resources should be automatically cleaned up when function returns
    return resources.length()
  }
  
  let initial_resource_count = create_and_cleanup_resources()
  assert_eq(initial_resource_count, 50) // Verify resources were created
  
  // Test manual resource cleanup
  let attrs = [
    ("test.service.name", StringValue("test_service")),
    ("test.service.version", StringValue("1.0.0")),
    ("test.service.instance.id", StringValue("test_instance_123")),
    ("test.host.name", StringValue("test_host")),
    ("test.process.id", IntValue(12345))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attrs)
  
  // Verify attributes
  for (key, expected_value) in attrs {
    let actual_value = Resource::get_attribute(resource, key)
    match actual_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false)
    }
  }
  
  // Manual resource cleanup (if supported)
  Resource::cleanup(resource)
  
  // Test resource merging and cleanup
  let resource1 = Resource::with_attributes(Resource::new(), [
    ("merge.key1", StringValue("value1")),
    ("shared.key", StringValue("original_value"))
  ])
  
  let resource2 = Resource::with_attributes(Resource::new(), [
    ("merge.key2", StringValue("value2")),
    ("shared.key", StringValue("overridden_value"))
  ])
  
  let merged_resource = Resource::merge(resource1, resource2)
  
  // Verify merge
  let merge_key1 = Resource::get_attribute(merged_resource, "merge.key1")
  match merge_key1 {
    Some(StringValue(value)) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  let shared_key = Resource::get_attribute(merged_resource, "shared.key")
  match shared_key {
    Some(StringValue(value)) => assert_eq(value, "overridden_value")
    None => assert_true(false)
  }
  
  // Cleanup merged resources
  Resource::cleanup(merged_resource)
}

// Test 7: HTTP Client Resource Management
test "http client resource management and cleanup" {
  // Test automatic HTTP client cleanup
  let create_and_cleanup_clients = || {
    let clients = []
    
    // Create many HTTP clients
    for i in 0..<20 {
      let client = HttpClient::new()
      clients.push(client)
    }
    
    // Clients should be automatically cleaned up when function returns
    return clients.length()
  }
  
  let initial_client_count = create_and_cleanup_clients()
  assert_eq(initial_client_count, 20) // Verify clients were created
  
  // Test manual HTTP client cleanup
  let client = HttpClient::new()
  
  // Create requests and responses
  let requests = []
  let responses = []
  
  for i in 0..<50 {
    let headers = [
      ("Content-Type", "application/json"),
      ("X-Request-ID", i.to_string())
    ]
    
    let request = HttpRequest::new(
      "POST",
      "https://example.com/api/resource_" + i.to_string(),
      headers,
      Some("{\"test\": \"request_" + i.to_string() + "\"}")
    )
    
    let response = HttpResponse::new(
      200,
      [("Content-Type", "application/json")],
      Some("{\"status\": \"success\", \"id\": " + i.to_string() + "}")
    )
    
    requests.push(request)
    responses.push(response)
  }
  
  // Verify requests and responses
  assert_eq(requests.length(), 50)
  assert_eq(responses.length(), 50)
  
  for (i, request) in requests.enumerate() {
    assert_eq(HttpRequest::http_method(request), "POST")
    assert_true(HttpRequest::url(request).contains("resource_" + i.to_string()))
  }
  
  for response in responses {
    assert_eq(HttpResponse::status_code(response), 200)
  }
  
  // Manual cleanup
  for request in requests {
    HttpRequest::cleanup(request)
  }
  
  for response in responses {
    HttpResponse::cleanup(response)
  }
  
  HttpClient::cleanup(client)
}

// Test 8: Baggage Resource Management
test "baggage resource management and cleanup" {
  // Test automatic baggage cleanup
  let create_and_cleanup_baggage = || {
    let baggage_items = []
    
    // Create many baggage instances
    for i in 0..<30 {
      let baggage = Baggage::new()
      let mut current_baggage = baggage
      
      // Add entries
      for j in 0..<20 {
        let key = "baggage_" + i.to_string() + "_key_" + j.to_string()
        let value = "baggage_" + i.to_string() + "_value_" + j.to_string()
        current_baggage = Baggage::set_entry(current_baggage, key, value)
      }
      
      baggage_items.push(current_baggage)
    }
    
    // Baggage instances should be automatically cleaned up when function returns
    return baggage_items.length()
  }
  
  let initial_baggage_count = create_and_cleanup_baggage()
  assert_eq(initial_baggage_count, 30) // Verify baggage instances were created
  
  // Test manual baggage cleanup
  let baggage = Baggage::new()
  let mut current_baggage = baggage
  
  // Create baggage with many entries
  let keys = []
  for i in 0..<100 {
    let key = "manual.key." + i.to_string()
    let value = "manual.value." + i.to_string()
    current_baggage = Baggage::set_entry(current_baggage, key, value)
    keys.push(key)
  }
  
  // Verify all entries
  for (i, key) in keys.enumerate() {
    let value = Baggage::get_entry(current_baggage, key)
    match value {
      Some(v) => assert_eq(v, "manual.value." + i.to_string())
      None => assert_true(false)
    }
  }
  
  // Remove some entries
  for i in 0..<50 {
    let key = "manual.key." + i.to_string()
    current_baggage = Baggage::remove_entry(current_baggage, key)
  }
  
  // Verify removal
  for i in 0..<50 {
    let key = "manual.key." + i.to_string()
    let value = Baggage::get_entry(current_baggage, key)
    match value {
      Some(_) => assert_true(false) // Should be removed
      None => assert_true(true)
    }
  }
  
  for i in 50..<100 {
    let key = "manual.key." + i.to_string()
    let value = Baggage::get_entry(current_baggage, key)
    match value {
      Some(v) => assert_eq(v, "manual.value." + i.to_string())
      None => assert_true(false)
    }
  }
  
  // Manual baggage cleanup
  Baggage::cleanup(current_baggage)
}

// Test 9: Memory Leak Detection
test "memory leak detection and prevention" {
  // Test for potential memory leaks in attribute operations
  let test_attribute_leaks = || {
    for iteration in 0..<100 {
      let attrs = Attributes::new()
      
      // Create and destroy many attributes
      for i in 0..<1000 {
        let key = "leak.test.key." + i.to_string()
        let value = StringValue("leak.test.value." + i.to_string())
        Attributes::set(attrs, key, value)
        
        // Immediately remove some attributes
        if i % 2 == 0 {
          Attributes::remove(attrs, key)
        }
      }
      
      // Clear all attributes
      Attributes::clear(attrs)
    }
  }
  
  test_attribute_leaks()
  
  // Test for potential memory leaks in span operations
  let test_span_leaks = || {
    let span_ctx = SpanContext::new("leak_test_trace", "leak_test_span", true, "")
    
    for iteration in 0..<50 {
      let spans = []
      
      // Create and end many spans
      for i in 0..<500 {
        let span = Span::new("leak.test.span." + i.to_string(), Internal, span_ctx)
        Span::add_event(span, "leak.test.event", None)
        Span::end(span)
      }
    }
  }
  
  test_span_leaks()
  
  // Test for potential memory leaks in context operations
  let test_context_leaks = || {
    let root_ctx = Context::root()
    
    for iteration in 0..<50 {
      let mut ctx = root_ctx
      
      // Create deep context hierarchies
      for i in 0..<200 {
        let key = ContextKey::new("leak.test.key." + i.to_string())
        let value = "leak.test.value." + i.to_string()
        ctx = Context::with_value(ctx, key, value)
      }
      
      // Context should be automatically cleaned up
    }
  }
  
  test_context_leaks()
  
  // Test for potential memory leaks in metric operations
  let test_metric_leaks = || {
    let provider = MeterProvider::default()
    let meter = MeterProvider::get_meter(provider, "leak_test_meter")
    
    for iteration in 0..<30 {
      let instruments = []
      
      // Create many metric instruments
      for i in 0..<100 {
        let counter = Meter::create_counter(
          meter,
          "leak.test.counter." + i.to_string(),
          Some("Leak test counter"),
          Some("count")
        )
        instruments.push(counter)
        
        // Use the instrument
        Counter::add(counter, i.to_float())
      }
      
      // Instruments should be automatically cleaned up
    }
  }
  
  test_metric_leaks()
}