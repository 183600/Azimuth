// Azimuth Telemetry System - Comprehensive Memory Management and Resource Cleanup Tests
// This file contains comprehensive test cases for memory management and resource cleanup

// Test 1: Span Memory Management
test "span memory management" {
  let initial_memory = Performance::get_memory_usage()
  
  // Create spans
  let spans = []
  let iterations = 100
  let trace_ctx = SpanContext::new("memory_trace", "memory_root", true, "")
  
  for i in 0..iterations {
    let span = Span::new("memory_span_" + i.to_string(), Internal, trace_ctx)
    Span::add_attribute(span, "iteration", IntValue(i))
    Span::add_attribute(span, "operation.type", StringValue("memory_test"))
    Span::add_event(span, "test_event", Some([
      ("event.data", StringValue("test_data_" + i.to_string())),
      ("event.timestamp", IntValue(1234567890 + i))
    ]))
    spans.push(span)
  }
  
  let after_creation_memory = Performance::get_memory_usage()
  let memory_increase = after_creation_memory - initial_memory
  
  // Verify memory increase is reasonable
  assert_true(memory_increase > 0, "Memory should increase when creating spans")
  assert_true(memory_increase < 10 * 1024 * 1024, "Memory increase should be reasonable (< 10MB for 100 spans)")
  
  // End all spans to trigger cleanup
  for span in spans {
    Span::end(span)
  }
  
  // Force garbage collection if available
  Performance::force_gc()
  
  let after_cleanup_memory = Performance::get_memory_usage()
  let memory_after_cleanup = after_cleanup_memory - initial_memory
  
  // Verify memory is cleaned up
  assert_true(memory_after_cleanup < memory_increase, "Memory should be cleaned up after ending spans")
}

// Test 2: Attributes Memory Management
test "attributes memory management" {
  let initial_memory = Performance::get_memory_usage()
  
  // Create attributes with large data
  let attrs_list = []
  let iterations = 50
  
  for i in 0..iterations {
    let attrs = Attributes::new()
    
    // Add attributes with various data sizes
    Attributes::set(attrs, "small.key", StringValue("small_value_" + i.to_string()))
    Attributes::set(attrs, "medium.key", StringValue("x".repeat(1000)))
    Attributes::set(attrs, "large.key", StringValue("x".repeat(10000)))
    Attributes::set(attrs, "int.key", IntValue(i))
    Attributes::set(attrs, "float.key", FloatValue(i * 3.14))
    Attributes::set(attrs, "bool.key", BoolValue(i % 2 == 0))
    
    // Add array attributes
    let string_array = Array::init(100, fn(j) { "array_item_" + j.to_string() })
    let int_array = Array::init(100, fn(j) { j })
    Attributes::set(attrs, "string.array", ArrayStringValue(string_array))
    Attributes::set(attrs, "int.array", ArrayIntValue(int_array))
    
    attrs_list.push(attrs)
  }
  
  let after_creation_memory = Performance::get_memory_usage()
  let memory_increase = after_creation_memory - initial_memory
  
  // Verify memory increase is reasonable
  assert_true(memory_increase > 0, "Memory should increase when creating attributes")
  assert_true(memory_increase < 50 * 1024 * 1024, "Memory increase should be reasonable (< 50MB for large attributes)")
  
  // Clear references to allow cleanup
  attrs_list.clear()
  
  // Force garbage collection
  Performance::force_gc()
  
  let after_cleanup_memory = Performance::get_memory_usage()
  let memory_after_cleanup = after_cleanup_memory - initial_memory
  
  // Verify memory is cleaned up
  assert_true(memory_after_cleanup < memory_increase, "Memory should be cleaned up after clearing attributes")
}

// Test 3: Metrics Memory Management
test "metrics memory management" {
  let initial_memory = Performance::get_memory_usage()
  
  // Create metrics and record data
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "memory_meter")
  
  let instruments = []
  let iterations = 50
  
  for i in 0..iterations {
    let counter = Meter::create_counter(meter, "memory_counter_" + i.to_string(), Some("Memory counter"), Some("count"))
    let histogram = Meter::create_histogram(meter, "memory_histogram_" + i.to_string(), Some("Memory histogram"), Some("ms"))
    let updown_counter = Meter::create_updown_counter(meter, "memory_updown_" + i.to_string(), Some("Memory updown"), Some("value"))
    
    instruments.push((counter, histogram, updown_counter))
    
    // Record data
    for j in 0..100 {
      Counter::add(counter, 1.0, Some([
        ("iteration", IntValue(i)),
        ("record", IntValue(j)),
        ("data", StringValue("test_data_" + j.to_string()))
      ]))
      
      Histogram::record(histogram, j.to_float(), Some([
        ("iteration", IntValue(i)),
        ("record", IntValue(j))
      ]))
      
      UpDownCounter::add(updown_counter, 1.0, Some([
        ("iteration", IntValue(i)),
        ("record", IntValue(j))
      ]))
    }
  }
  
  let after_creation_memory = Performance::get_memory_usage()
  let memory_increase = after_creation_memory - initial_memory
  
  // Verify memory increase is reasonable
  assert_true(memory_increase > 0, "Memory should increase when creating metrics")
  assert_true(memory_increase < 20 * 1024 * 1024, "Memory increase should be reasonable (< 20MB for metrics)")
  
  // Collect metrics
  let metric_data = Meter::collect(meter)
  
  let after_collection_memory = Performance::get_memory_usage()
  let memory_after_collection = after_collection_memory - initial_memory
  
  // Clear references
  instruments.clear()
  
  // Force garbage collection
  Performance::force_gc()
  
  let after_cleanup_memory = Performance::get_memory_usage()
  let memory_after_cleanup = after_cleanup_memory - initial_memory
  
  // Verify memory is cleaned up
  assert_true(memory_after_cleanup < memory_after_collection, "Memory should be cleaned up after clearing metrics")
}

// Test 4: Context Memory Management
test "context memory management" {
  let initial_memory = Performance::get_memory_usage()
  
  // Create contexts
  let contexts = []
  let iterations = 200
  
  for i in 0..iterations {
    let trace_id = "memory_trace_" + i.to_string()
    let span_id = "memory_span_" + i.to_string()
    let ctx = SpanContext::new(trace_id, span_id, true, "memory_state_" + i.to_string())
    contexts.push(ctx)
  }
  
  let after_creation_memory = Performance::get_memory_usage()
  let memory_increase = after_creation_memory - initial_memory
  
  // Verify memory increase is reasonable
  assert_true(memory_increase > 0, "Memory should increase when creating contexts")
  assert_true(memory_increase < 5 * 1024 * 1024, "Memory increase should be reasonable (< 5MB for 200 contexts)")
  
  // Clear references
  contexts.clear()
  
  // Force garbage collection
  Performance::force_gc()
  
  let after_cleanup_memory = Performance::get_memory_usage()
  let memory_after_cleanup = after_cleanup_memory - initial_memory
  
  // Verify memory is cleaned up
  assert_true(memory_after_cleanup < memory_increase, "Memory should be cleaned up after clearing contexts")
}

// Test 5: Baggage Memory Management
test "baggage memory management" {
  let initial_memory = Performance::get_memory_usage()
  
  // Create baggage with large data
  let baggage_instances = []
  let iterations = 50
  
  for i in 0..iterations {
    let baggage = Baggage::new()
    
    // Add baggage entries with various sizes
    baggage = Baggage::set_entry(baggage, "small.entry", "small_value_" + i.to_string())
    baggage = Baggage::set_entry(baggage, "medium.entry", "x".repeat(1000))
    baggage = Baggage::set_entry(baggage, "large.entry", "x".repeat(10000))
    baggage = Baggage::set_entry(baggage, "unicode.entry", "Unicode测试数据_" + i.to_string())
    
    baggage_instances.push(baggage)
  }
  
  let after_creation_memory = Performance::get_memory_usage()
  let memory_increase = after_creation_memory - initial_memory
  
  // Verify memory increase is reasonable
  assert_true(memory_increase > 0, "Memory should increase when creating baggage")
  assert_true(memory_increase < 30 * 1024 * 1024, "Memory increase should be reasonable (< 30MB for baggage)")
  
  // Clear references
  baggage_instances.clear()
  
  // Force garbage collection
  Performance::force_gc()
  
  let after_cleanup_memory = Performance::get_memory_usage()
  let memory_after_cleanup = after_cleanup_memory - initial_memory
  
  // Verify memory is cleaned up
  assert_true(memory_after_cleanup < memory_increase, "Memory should be cleaned up after clearing baggage")
}

// Test 6: Resource Memory Management
test "resource memory management" {
  let initial_memory = Performance::get_memory_usage()
  
  // Create resources with large attributes
  let resources = []
  let iterations = 50
  
  for i in 0..iterations {
    let attrs = [
      ("service.name", StringValue("memory_test_service_" + i.to_string())),
      ("service.version", StringValue("1.0." + i.to_string())),
      ("service.instance.id", StringValue("instance_" + i.to_string())),
      ("large.attribute", StringValue("x".repeat(5000))),
      ("unicode.attribute", StringValue("Unicode属性测试_" + i.to_string())),
      ("numeric.attribute", IntValue(i)),
      ("float.attribute", FloatValue(i * 3.14159))
    ]
    
    let resource = Resource::with_attributes(Resource::new(), attrs)
    resources.push(resource)
  }
  
  let after_creation_memory = Performance::get_memory_usage()
  let memory_increase = after_creation_memory - initial_memory
  
  // Verify memory increase is reasonable
  assert_true(memory_increase > 0, "Memory should increase when creating resources")
  assert_true(memory_increase < 20 * 1024 * 1024, "Memory increase should be reasonable (< 20MB for resources)")
  
  // Clear references
  resources.clear()
  
  // Force garbage collection
  Performance::force_gc()
  
  let after_cleanup_memory = Performance::get_memory_usage()
  let memory_after_cleanup = after_cleanup_memory - initial_memory
  
  // Verify memory is cleaned up
  assert_true(memory_after_cleanup < memory_increase, "Memory should be cleaned up after clearing resources")
}

// Test 7: Log Records Memory Management
test "log records memory management" {
  let initial_memory = Performance::get_memory_usage()
  
  // Create log records with large data
  let log_records = []
  let iterations = 100
  
  for i in 0..iterations {
    let attrs = Attributes::new()
    Attributes::set(attrs, "log.id", IntValue(i))
    Attributes::set(attrs, "log.service", StringValue("memory_test_service"))
    Attributes::set(attrs, "log.large.data", StringValue("x".repeat(2000)))
    
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Memory test log message " + i.to_string() + " with additional data: " + "x".repeat(1000)),
      Some(attrs),
      Some(1234567890L + i),
      Some(1234567891L + i),
      Some("memory_log_trace_" + i.to_string()),
      Some("memory_log_span_" + i.to_string()),
      Some(Context::root())
    )
    
    log_records.push(log_record)
  }
  
  let after_creation_memory = Performance::get_memory_usage()
  let memory_increase = after_creation_memory - initial_memory
  
  // Verify memory increase is reasonable
  assert_true(memory_increase > 0, "Memory should increase when creating log records")
  assert_true(memory_increase < 15 * 1024 * 1024, "Memory increase should be reasonable (< 15MB for log records)")
  
  // Clear references
  log_records.clear()
  
  // Force garbage collection
  Performance::force_gc()
  
  let after_cleanup_memory = Performance::get_memory_usage()
  let memory_after_cleanup = after_cleanup_memory - initial_memory
  
  // Verify memory is cleaned up
  assert_true(memory_after_cleanup < memory_increase, "Memory should be cleaned up after clearing log records")
}

// Test 8: Serialization Memory Management
test "serialization memory management" {
  let initial_memory = Performance::get_memory_usage()
  
  // Create and serialize large data structures
  let serialized_data = []
  let iterations = 50
  
  for i in 0..iterations {
    let attrs = Attributes::new()
    Attributes::set(attrs, "serialization.id", IntValue(i))
    Attributes::set(attrs, "serialization.data", StringValue("x".repeat(5000)))
    Attributes::set(attrs, "serialization.array", ArrayIntValue(Array::init(100, fn(j) { j })))
    
    let span_ctx = SpanContext::new("serialization_trace_" + i.to_string(), "serialization_span_" + i.to_string(), true, "")
    let span = Span::new("serialization_test_" + i.to_string(), Internal, span_ctx)
    Span::add_attribute(span, "test.data", StringValue("x".repeat(3000)))
    
    // Serialize data
    let serialized_attrs = Attributes::serialize(attrs)
    let serialized_span = Span::serialize(span)
    
    serialized_data.push((serialized_attrs, serialized_span))
  }
  
  let after_serialization_memory = Performance::get_memory_usage()
  let memory_increase = after_serialization_memory - initial_memory
  
  // Verify memory increase is reasonable
  assert_true(memory_increase > 0, "Memory should increase when serializing data")
  assert_true(memory_increase < 25 * 1024 * 1024, "Memory increase should be reasonable (< 25MB for serialized data)")
  
  // Deserialize data
  let deserialized_data = []
  for i in 0..serialized_data.length() {
    let (serialized_attrs, serialized_span) = serialized_data[i]
    let deserialized_attrs = Attributes::deserialize(serialized_attrs)
    let deserialized_span = Span::deserialize(serialized_span)
    deserialized_data.push((deserialized_attrs, deserialized_span))
  }
  
  let after_deserialization_memory = Performance::get_memory_usage()
  let memory_after_deserialization = after_deserialization_memory - initial_memory
  
  // Clear references
  serialized_data.clear()
  deserialized_data.clear()
  
  // Force garbage collection
  Performance::force_gc()
  
  let after_cleanup_memory = Performance::get_memory_usage()
  let memory_after_cleanup = after_cleanup_memory - initial_memory
  
  // Verify memory is cleaned up
  assert_true(memory_after_cleanup < memory_after_deserialization, "Memory should be cleaned up after clearing serialized data")
}

// Test 9: HTTP Client Memory Management
test "http client memory management" {
  let initial_memory = Performance::get_memory_usage()
  
  // Create HTTP requests and responses with large data
  let requests = []
  let responses = []
  let iterations = 50
  
  for i in 0..iterations {
    let headers = [
      ("Content-Type", "application/json"),
      ("X-Request-ID", "req_" + i.to_string()),
      ("X-Large-Header", "x".repeat(1000))
    ]
    
    let large_body = "x".repeat(10000)
    let request = HttpRequest::new("POST", "https://example.com/api/" + i.to_string(), headers, Some(large_body))
    requests.push(request)
    
    let response_headers = [
      ("Content-Type", "application/json"),
      ("X-Response-ID", "resp_" + i.to_string()),
      ("X-Large-Response-Header", "y".repeat(1000))
    ]
    
    let large_response_body = "y".repeat(15000)
    let response = HttpResponse::new(200, response_headers, Some(large_response_body))
    responses.push(response)
  }
  
  let after_creation_memory = Performance::get_memory_usage()
  let memory_increase = after_creation_memory - initial_memory
  
  // Verify memory increase is reasonable
  assert_true(memory_increase > 0, "Memory should increase when creating HTTP objects")
  assert_true(memory_increase < 20 * 1024 * 1024, "Memory increase should be reasonable (< 20MB for HTTP objects)")
  
  // Clear references
  requests.clear()
  responses.clear()
  
  // Force garbage collection
  Performance::force_gc()
  
  let after_cleanup_memory = Performance::get_memory_usage()
  let memory_after_cleanup = after_cleanup_memory - initial_memory
  
  // Verify memory is cleaned up
  assert_true(memory_after_cleanup < memory_increase, "Memory should be cleaned up after clearing HTTP objects")
}

// Test 10: Comprehensive Resource Cleanup Scenario
test "comprehensive resource cleanup scenario" {
  let initial_memory = Performance::get_memory_usage()
  
  // Create a complex scenario with multiple resource types
  let spans = []
  let attributes = []
  let contexts = []
  let baggage_instances = []
  let resources = []
  let log_records = []
  let http_requests = []
  let http_responses = []
  
  let iterations = 30  // Reduced for comprehensive test
  
  // Create spans
  for i in 0..iterations {
    let trace_ctx = SpanContext::new("cleanup_trace_" + i.to_string(), "cleanup_root_" + i.to_string(), true, "")
    let span = Span::new("cleanup_span_" + i.to_string(), Internal, trace_ctx)
    Span::add_attribute(span, "cleanup.id", IntValue(i))
    Span::add_event(span, "cleanup_event", Some([("event.data", StringValue("x".repeat(1000)))]))
    spans.push(span)
    contexts.push(trace_ctx)
  }
  
  // Create attributes
  for i in 0..iterations {
    let attrs = Attributes::new()
    Attributes::set(attrs, "cleanup.id", IntValue(i))
    Attributes::set(attrs, "cleanup.data", StringValue("x".repeat(2000)))
    Attributes::set(attrs, "cleanup.array", ArrayIntValue(Array::init(50, fn(j) { j })))
    attributes.push(attrs)
  }
  
  // Create baggage
  for i in 0..iterations {
    let baggage = Baggage::new()
    baggage = Baggage::set_entry(baggage, "cleanup.id", i.to_string())
    baggage = Baggage::set_entry(baggage, "cleanup.data", "x".repeat(1500))
    baggage_instances.push(baggage)
  }
  
  // Create resources
  for i in 0..iterations {
    let attrs = [
      ("cleanup.service", StringValue("cleanup_service_" + i.to_string())),
      ("cleanup.data", StringValue("x".repeat(1000))),
      ("cleanup.id", IntValue(i))
    ]
    let resource = Resource::with_attributes(Resource::new(), attrs)
    resources.push(resource)
  }
  
  // Create log records
  for i in 0..iterations {
    let attrs = Attributes::new()
    Attributes::set(attrs, "log.cleanup.id", IntValue(i))
    Attributes::set(attrs, "log.cleanup.data", StringValue("x".repeat(800)))
    
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Cleanup log message " + i.to_string() + " " + "x".repeat(500)),
      Some(attrs),
      Some(1234567890L + i),
      Some(1234567891L + i),
      Some("cleanup_log_trace_" + i.to_string()),
      Some("cleanup_log_span_" + i.to_string()),
      Some(Context::root())
    )
    log_records.push(log_record)
  }
  
  // Create HTTP objects
  for i in 0..iterations {
    let headers = [("X-Cleanup-ID", i.to_string())]
    let body = "x".repeat(2000)
    let request = HttpRequest::new("POST", "https://cleanup.example.com/" + i.to_string(), headers, Some(body))
    http_requests.push(request)
    
    let response = HttpResponse::new(200, headers, Some("y".repeat(2500)))
    http_responses.push(response)
  }
  
  let after_creation_memory = Performance::get_memory_usage()
  let memory_increase = after_creation_memory - initial_memory
  
  // Verify memory increase is reasonable
  assert_true(memory_increase > 0, "Memory should increase when creating comprehensive resources")
  assert_true(memory_increase < 50 * 1024 * 1024, "Memory increase should be reasonable (< 50MB for comprehensive test)")
  
  // End spans to trigger cleanup
  for span in spans {
    Span::end(span)
  }
  
  // Clear all references
  spans.clear()
  attributes.clear()
  contexts.clear()
  baggage_instances.clear()
  resources.clear()
  log_records.clear()
  http_requests.clear()
  http_responses.clear()
  
  // Force garbage collection
  Performance::force_gc()
  
  let after_cleanup_memory = Performance::get_memory_usage()
  let memory_after_cleanup = after_cleanup_memory - initial_memory
  
  // Verify memory is cleaned up
  assert_true(memory_after_cleanup < memory_increase, "Memory should be cleaned up after comprehensive cleanup")
  
  // Verify system is still functional after cleanup
  let test_span = Span::new("post_cleanup_test", Internal, SpanContext::new("test", "test", true, ""))
  Span::add_attribute(test_span, "cleanup.test", StringValue("successful"))
  Span::end(test_span)
  
  assert_false(Span::is_recording(test_span))
  
  // Verify final memory usage is reasonable
  let final_memory = Performance::get_memory_usage()
  let final_memory_increase = final_memory - initial_memory
  
  assert_true(final_memory_increase < memory_increase * 0.3, "Final memory usage should be significantly reduced")
}