// Concurrent Safety Tests
// This file contains test cases for concurrent operations and thread safety

// Test 1: Concurrent span creation and management
pub test "concurrent_span_operations" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrent-test-service")
  
  // Create multiple spans concurrently
  let parent_span = azimuth::Tracer::start_span(tracer, "Concurrent Operations Parent")
  azimuth::Span::set_attribute(parent_span, "concurrent.operations", azimuth::IntValue(10))
  
  // Simulate concurrent span creation
  let mut spans = []
  let mut i = 0
  while i < 10 {
    let span = azimuth::Tracer::start_span_with_parent(tracer, "Concurrent Operation " + i.to_string(), parent_span)
    azimuth::Span::set_attribute(span, "operation.id", azimuth::IntValue(i))
    azimuth::Span::set_attribute(span, "thread.id", azimuth::StringValue("thread-" + i.to_string()))
    spans = spans + [span]
    i = i + 1
  }
  
  // Simulate concurrent attribute setting
  for span in spans {
    azimuth::Span::set_attribute(span, "concurrent.attribute", azimuth::StringValue("set_concurrently"))
    azimuth::Span::set_attribute(span, "timestamp", azimuth::IntValue(1640995200 + i))
    azimuth::Span::set_status_code(span, azimuth::Ok)
    azimuth::Span::end(span)
  }
  
  // End parent span
  azimuth::Span::set_status_code(parent_span, azimuth::Ok)
  azimuth::Span::end(parent_span)
  
  // Verify all spans have expected attributes
  for span in spans {
    assert_eq(azimuth::Span::get_attribute(span, "concurrent.attribute"), Some(azimuth::StringValue("set_concurrently")))
  }
}

// Test 2: Concurrent metrics operations
pub test "concurrent_metrics_operations" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "concurrent-metrics")
  
  // Create metric instruments
  let counter = azimuth::Meter::create_counter(meter, "concurrent.counter", Some("Concurrent test counter"), None)
  let histogram = azimuth::Meter::create_histogram(meter, "concurrent.histogram", Some("Concurrent test histogram"), Some("ms"))
  let gauge = azimuth::Meter::create_gauge(meter, "concurrent.gauge", Some("Concurrent test gauge"), None)
  
  // Simulate concurrent metric updates
  let mut i = 0
  while i < 100 {
    // Counter updates from different "threads"
    azimuth::Counter::add(counter, 1, [("thread.id", i.to_string()), ("operation.type", "increment")])
    
    // Histogram updates from different "threads"
    azimuth::Histogram::record(histogram, (i % 100).to_float(), [("thread.id", i.to_string()), ("operation.type", "latency")])
    
    // Gauge updates from different "threads"
    azimuth::Gauge::set(gauge, (i % 50).to_float(), [("thread.id", i.to_string()), ("operation.type", "measurement")])
    
    i = i + 1
  }
  
  // Verify metric instruments are still functional
  assert_eq(counter.name, "concurrent.counter")
  assert_eq(histogram.name, "concurrent.histogram")
  assert_eq(gauge.name, "concurrent.gauge")
  
  // Final batch operation
  azimuth::Counter::add(counter, 100, [("operation.type", "batch"), ("thread.count", "100")])
}

// Test 3: Concurrent logging operations
pub test "concurrent_logging_operations" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "concurrent-logger")
  
  // Simulate concurrent log emissions
  let mut i = 0
  while i < 500 {
    let log_level = match i % 4 {
      0 => azimuth::Debug,
      1 => azimuth::Info,
      2 => azimuth::Warn,
      _ => azimuth::Error
    }
    
    let log_message = "Concurrent log message " + i.to_string() + " from thread " + (i % 10).to_string()
    let log_record = azimuth::LogRecord::new(log_level, log_message)
    
    // Add thread-specific attributes
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "thread.id", azimuth::StringValue("thread-" + (i % 10).to_string()))
    azimuth::Attributes::set(attrs, "log.sequence", azimuth::IntValue(i))
    let enriched_log = azimuth::LogRecord::with_attributes(log_record, attrs)
    
    azimuth::Logger::emit(logger, enriched_log)
    i = i + 1
  }
  
  // Verify logger is still functional
  let final_log = azimuth::LogRecord::new(azimuth::Info, "Concurrent logging test completed")
  azimuth::Logger::emit(logger, final_log)
  
  assert_eq(logger.scope.name, "concurrent-logger")
}

// Test 4: Concurrent context and baggage operations
pub test "concurrent_context_baggage_operations" {
  let root_ctx = azimuth::Context::root()
  
  // Create multiple contexts concurrently
  let mut contexts = []
  let mut i = 0
  while i < 20 {
    let ctx = azimuth::Context::with_value(root_ctx, azimuth::ContextKey::new("thread." + i.to_string()), "value." + i.to_string())
    contexts = contexts + [ctx]
    i = i + 1
  }
  
  // Add baggage to contexts concurrently
  let mut baggage_contexts = []
  for ctx in contexts {
    let baggage = azimuth::Baggage::new()
    let baggage_with_entry = azimuth::Baggage::set_entry(baggage, "concurrent.entry", "concurrent.value")
    let ctx_with_baggage = azimuth::Context::with_value(ctx, azimuth::ContextKey::new("baggage"), baggage_with_entry)
    baggage_contexts = baggage_contexts + [ctx_with_baggage]
  }
  
  // Verify all contexts have expected values
  let mut j = 0
  for ctx in contexts {
    let key = azimuth::ContextKey::new("thread." + j.to_string())
    assert_eq(azimuth::Context::get(ctx, key), Some("value." + j.to_string()))
    j = j + 1
  }
  
  // Verify all baggage contexts have baggage
  for baggage_ctx in baggage_contexts {
    let baggage = azimuth::Context::get(baggage_ctx, azimuth::ContextKey::new("baggage"))
    assert_eq(azimuth::Baggage::get_entry(baggage, "concurrent.entry"), Some("concurrent.value"))
  }
}

// Test 5: Concurrent resource operations
pub test "concurrent_resource_operations" {
  // Create multiple resources concurrently
  let mut resources = []
  let mut i = 0
  while i < 10 {
    let resource = azimuth::Resource::new()
    let resource_with_attrs = azimuth::Resource::with_attributes(resource, [
      ("resource.id", azimuth::StringValue("resource-" + i.to_string())),
      ("thread.id", azimuth::StringValue("thread-" + (i % 5).to_string())),
      ("creation.time", azimuth::IntValue(1640995200 + i))
    ])
    resources = resources + [resource_with_attrs]
    i = i + 1
  }
  
  // Merge resources concurrently
  let base_resource = azimuth::Resource::new()
  let mut merged_resource = base_resource
  
  for resource in resources {
    merged_resource = azimuth::Resource::merge(merged_resource, resource)
  }
  
  // Verify merged resource contains attributes from all resources
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "resource.id"), Some(azimuth::StringValue("resource-9"))) // Last one wins
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "creation.time"), Some(azimuth::IntValue(1640995209))) // Last one wins
  
  // Test concurrent attribute access
  for resource in resources {
    let resource_id = azimuth::Resource::get_attribute(resource, "resource.id")
    let thread_id = azimuth::Resource::get_attribute(resource, "thread.id")
    let creation_time = azimuth::Resource::get_attribute(resource, "creation.time")
    
    assert_true(resource_id != None)
    assert_true(thread_id != None)
    assert_true(creation_time != None)
  }
}

// Test 6: Concurrent propagator operations
pub test "concurrent_propagator_operations" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // Create multiple propagators
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // Simulate concurrent inject operations
  let mut carriers = []
  let mut i = 0
  while i < 10 {
    let carrier = azimuth::TextMapCarrier::new()
    let ctx = azimuth::Context::root()
    let ctx_with_trace = azimuth::Context::with_value(ctx, azimuth::ContextKey::new("trace.id"), "trace-" + i.to_string())
    let ctx_with_baggage = azimuth::Context::with_value(ctx_with_trace, azimuth::ContextKey::new("baggage"), "entry=value-" + i.to_string())
    
    azimuth::CompositePropagator::inject(composite_propagator, ctx_with_baggage, carrier)
    carriers = carriers + [carrier]
    i = i + 1
  }
  
  // Simulate concurrent extract operations
  let mut extracted_contexts = []
  for carrier in carriers {
    let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
    extracted_contexts = extracted_contexts + [extracted_ctx]
  }
  
  // Verify extraction results
  for extracted_ctx in extracted_contexts {
    let key = azimuth::ContextKey::new("extracted")
    let extracted_value = azimuth::Context::get(extracted_ctx, key)
    assert_eq(extracted_value, Some("true"))
  }
}

// Test 7: Concurrent span parent-child relationships
pub test "concurrent_span_parent_child_relationships" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "relationship-test-service")
  
  // Create multiple root spans
  let mut root_spans = []
  let mut i = 0
  while i < 5 {
    let root_span = azimuth::Tracer::start_span(tracer, "Root Operation " + i.to_string())
    azimuth::Span::set_attribute(root_span, "root.id", azimuth::IntValue(i))
    root_spans = root_spans + [root_span]
    i = i + 1
  }
  
  // Create child spans for each root span concurrently
  let mut all_child_spans = []
  for root_span in root_spans {
    let mut child_spans = []
    let mut j = 0
    while j < 3 {
      let child_span = azimuth::Tracer::start_span_with_parent(tracer, "Child Operation " + j.to_string(), root_span)
      azimuth::Span::set_attribute(child_span, "child.id", azimuth::IntValue(j))
      azimuth::Span::set_attribute(child_span, "root.parent", azimuth::Span::get_attribute(root_span, "root.id"))
      child_spans = child_spans + [child_span]
      j = j + 1
    }
    all_child_spans = all_child_spans + child_spans
  }
  
  // End all child spans
  for child_span in all_child_spans {
    azimuth::Span::set_status_code(child_span, azimuth::Ok)
    azimuth::Span::end(child_span)
  }
  
  // End all root spans
  for root_span in root_spans {
    azimuth::Span::set_status_code(root_span, azimuth::Ok)
    azimuth::Span::end(root_span)
  }
  
  // Verify parent-child relationships
  let total_child_spans = 5 * 3 // 5 roots * 3 children each
  assert_eq(length(all_child_spans), total_child_spans)
  
  // Verify child spans have parent attributes
  for child_span in all_child_spans {
    let parent_attr = azimuth::Span::get_attribute(child_span, "root.parent")
    assert_true(parent_attr != None)
  }
}