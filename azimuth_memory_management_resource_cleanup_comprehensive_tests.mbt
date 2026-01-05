// Azimuth Telemetry System - Memory Management Resource Cleanup Comprehensive Tests
// This file contains comprehensive test cases for memory management and resource cleanup

// Test 1: Span Memory Management
test "span memory management" {
  let initial_memory = Memory::allocated()
  let span_count = 1000
  
  // Create spans
  let spans = []
  for i in 0..span_count {
    let trace_id = "memory_test_trace_" + i.to_string()
    let span_id = "memory_test_span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("memory_test_operation", Internal, span_ctx)
    
    // Add events and attributes to increase memory usage
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    Attributes::set(attrs, "data", StringValue("test_data_" + i.to_string()))
    
    Span::add_event(span, "test_event", Some(attrs))
    spans.push(span)
  }
  
  let after_create_memory = Memory::allocated()
  let memory_per_span = (after_create_memory - initial_memory) / span_count
  
  // Verify reasonable memory usage per span
  assert_true(memory_per_span < 2000)  // Less than 2KB per span
  
  // End spans to trigger cleanup
  for span in spans {
    Span::end(span)
  }
  
  // Force garbage collection if available
  Memory::gc()
  
  let after_cleanup_memory = Memory::allocated()
  let memory_recovered = after_create_memory - after_cleanup_memory
  
  // Verify most memory is recovered
  let recovery_percentage = (memory_recovered * 100) / (after_create_memory - initial_memory)
  assert_true(recovery_percentage > 70)  // At least 70% memory recovered
}

// Test 2: Attributes Memory Management
test "attributes memory management" {
  let initial_memory = Memory::allocated()
  let attr_count = 500
  
  // Create attributes with various data types
  let attrs = Attributes::new()
  for i in 0..attr_count {
    let string_key = "string_key_" + i.to_string()
    let string_value = StringValue("string_value_" + i.to_string())
    Attributes::set(attrs, string_key, string_value)
    
    let int_key = "int_key_" + i.to_string()
    let int_value = IntValue(i)
    Attributes::set(attrs, int_key, int_value)
    
    let float_key = "float_key_" + i.to_string()
    let float_value = FloatValue(i.to_float() + 0.5)
    Attributes::set(attrs, float_key, float_value)
    
    let bool_key = "bool_key_" + i.to_string()
    let bool_value = BoolValue(i % 2 == 0)
    Attributes::set(attrs, bool_key, bool_value)
  }
  
  let after_create_memory = Memory::allocated()
  let memory_per_attr = (after_create_memory - initial_memory) / (attr_count * 4)
  
  // Verify reasonable memory usage per attribute
  assert_true(memory_per_attr < 100)  // Less than 100 bytes per attribute
  
  // Clear attributes
  Attributes::clear(attrs)
  
  // Force garbage collection if available
  Memory::gc()
  
  let after_cleanup_memory = Memory::allocated()
  let memory_recovered = after_create_memory - after_cleanup_memory
  
  // Verify most memory is recovered
  let recovery_percentage = (memory_recovered * 100) / (after_create_memory - initial_memory)
  assert_true(recovery_percentage > 60)  // At least 60% memory recovered
}

// Test 3: Log Records Memory Management
test "log records memory management" {
  let initial_memory = Memory::allocated()
  let log_count = 800
  
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "memory_test_logger")
  
  // Create log records
  let log_records = []
  for i in 0..log_count {
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    Attributes::set(attrs, "message", StringValue("Log message " + i.to_string()))
    
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Memory test log message " + i.to_string()),
      Some(attrs),
      Some(Time::now()),
      Some(Time::now()),
      Some("trace_" + i.to_string()),
      Some("span_" + i.to_string()),
      Some(Context::root())
    )
    
    log_records.push(log_record)
  }
  
  let after_create_memory = Memory::allocated()
  let memory_per_log = (after_create_memory - initial_memory) / log_count
  
  // Verify reasonable memory usage per log record
  assert_true(memory_per_log < 500)  // Less than 500 bytes per log record
  
  // Clear references to log records
  log_records.clear()
  
  // Force garbage collection if available
  Memory::gc()
  
  let after_cleanup_memory = Memory::allocated()
  let memory_recovered = after_create_memory - after_cleanup_memory
  
  // Verify most memory is recovered
  let recovery_percentage = (memory_recovered * 100) / (after_create_memory - initial_memory)
  assert_true(recovery_percentage > 65)  // At least 65% memory recovered
}

// Test 4: Metrics Memory Management
test "metrics memory management" {
  let initial_memory = Memory::allocated()
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "memory_test_meter")
  
  // Create multiple metric instruments
  let instruments = []
  for i in 0..100 {
    let counter = Meter::create_counter(meter, "counter_" + i.to_string(), Some("Test counter"), Some("count"))
    let histogram = Meter::create_histogram(meter, "histogram_" + i.to_string(), Some("Test histogram"), Some("ms"))
    let gauge = Meter::create_gauge(meter, "gauge_" + i.to_string(), Some("Test gauge"), Some("value"))
    
    instruments.push(counter)
    instruments.push(histogram)
    instruments.push(gauge)
  }
  
  // Add measurements to instruments
  for i in 0..100 {
    let counter = instruments[i * 3]
    let histogram = instruments[i * 3 + 1]
    let gauge = instruments[i * 3 + 2]
    
    for j in 0..10 {
      Counter::add(counter, j.to_float())
      Histogram::record(histogram, j.to_float())
    }
  }
  
  let after_create_memory = Memory::allocated()
  let memory_per_instrument = (after_create_memory - initial_memory) / instruments.length()
  
  // Verify reasonable memory usage per instrument
  assert_true(memory_per_instrument < 1000)  // Less than 1KB per instrument
  
  // Clear references to instruments
  instruments.clear()
  
  // Force garbage collection if available
  Memory::gc()
  
  let after_cleanup_memory = Memory::allocated()
  let memory_recovered = after_create_memory - after_cleanup_memory
  
  // Verify most memory is recovered
  let recovery_percentage = (memory_recovered * 100) / (after_create_memory - initial_memory)
  assert_true(recovery_percentage > 60)  // At least 60% memory recovered
}

// Test 5: Serialization Buffer Management
test "serialization buffer management" {
  let initial_memory = Memory::allocated()
  let buffer_count = 50
  
  // Create spans for serialization
  let spans = []
  for i in 0..buffer_count {
    let trace_id = "buffer_test_trace_" + i.to_string()
    let span_id = "buffer_test_span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("buffer_test_operation", Internal, span_ctx)
    
    // Add events and attributes
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    Attributes::set(attrs, "data", StringValue("test_data_" + i.to_string()))
    
    Span::add_event(span, "test_event", Some(attrs))
    spans.push(span)
  }
  
  // Serialize spans to create buffers
  let buffers = []
  for span in spans {
    let buffer = SpanSerializer::serialize_to_buffer(span)
    buffers.push(buffer)
  }
  
  let after_serialize_memory = Memory::allocated()
  let memory_per_buffer = (after_serialize_memory - initial_memory) / buffer_count
  
  // Verify reasonable memory usage per buffer
  assert_true(memory_per_buffer < 5000)  // Less than 5KB per buffer
  
  // Deserialize from buffers
  let deserialized_spans = []
  for buffer in buffers {
    let span = SpanSerializer::deserialize_from_buffer(buffer)
    deserialized_spans.push(span)
  }
  
  let after_deserialize_memory = Memory::allocated()
  
  // Clear buffers and deserialized spans
  buffers.clear()
  deserialized_spans.clear()
  
  // End original spans
  for span in spans {
    Span::end(span)
  }
  
  // Force garbage collection if available
  Memory::gc()
  
  let after_cleanup_memory = Memory::allocated()
  let memory_recovered = after_deserialize_memory - after_cleanup_memory
  
  // Verify most memory is recovered
  let recovery_percentage = (memory_recovered * 100) / (after_deserialize_memory - initial_memory)
  assert_true(recovery_percentage > 70)  // At least 70% memory recovered
}

// Test 6: Context Memory Management
test "context memory management" {
  let initial_memory = Memory::allocated()
  let context_count = 300
  
  // Create contexts with baggage
  let contexts = []
  for i in 0..context_count {
    let baggage = Baggage::new()
    let updated_baggage = Baggage::set_entry(baggage, "key1_" + i.to_string(), "value1_" + i.to_string())
    let final_baggage = Baggage::set_entry(updated_baggage, "key2_" + i.to_string(), "value2_" + i.to_string())
    
    let ctx = Context::with_baggage(Context::root(), final_baggage)
    contexts.push(ctx)
  }
  
  let after_create_memory = Memory::allocated()
  let memory_per_context = (after_create_memory - initial_memory) / context_count
  
  // Verify reasonable memory usage per context
  assert_true(memory_per_context < 800)  // Less than 800 bytes per context
  
  // Clear references to contexts
  contexts.clear()
  
  // Force garbage collection if available
  Memory::gc()
  
  let after_cleanup_memory = Memory::allocated()
  let memory_recovered = after_create_memory - after_cleanup_memory
  
  // Verify most memory is recovered
  let recovery_percentage = (memory_recovered * 100) / (after_create_memory - initial_memory)
  assert_true(recovery_percentage > 65)  // At least 65% memory recovered
}

// Test 7: Resource Memory Management
test "resource memory management" {
  let initial_memory = Memory::allocated()
  let resource_count = 100
  
  // Create resources with attributes
  let resources = []
  for i in 0..resource_count {
    let attrs = [
      ("service.name", StringValue("service_" + i.to_string())),
      ("service.version", StringValue("1.0." + i.to_string())),
      ("service.instance.id", StringValue("instance_" + i.to_string())),
      ("environment", StringValue("test")),
      ("region", StringValue("us-west-2"))
    ]
    
    let resource = Resource::with_attributes(Resource::new(), attrs)
    resources.push(resource)
  }
  
  let after_create_memory = Memory::allocated()
  let memory_per_resource = (after_create_memory - initial_memory) / resource_count
  
  // Verify reasonable memory usage per resource
  assert_true(memory_per_resource < 1000)  // Less than 1KB per resource
  
  // Clear references to resources
  resources.clear()
  
  // Force garbage collection if available
  Memory::gc()
  
  let after_cleanup_memory = Memory::allocated()
  let memory_recovered = after_create_memory - after_cleanup_memory
  
  // Verify most memory is recovered
  let recovery_percentage = (memory_recovered * 100) / (after_create_memory - initial_memory)
  assert_true(recovery_percentage > 60)  // At least 60% memory recovered
}

// Test 8: Large Object Memory Management
test "large object memory management" {
  let initial_memory = Memory::allocated()
  let large_object_count = 20
  
  // Create spans with large attribute values
  let spans = []
  for i in 0..large_object_count {
    let trace_id = "large_object_trace_" + i.to_string()
    let span_id = "large_object_span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("large_object_operation", Internal, span_ctx)
    
    // Add large string attribute (10KB each)
    let large_string = "x".repeat(10240)  // 10KB string
    let attrs = Attributes::new()
    Attributes::set(attrs, "large_data", StringValue(large_string))
    
    // Add multiple events with large data
    for j in 0..5 {
      Span::add_event(span, "large_event_" + j.to_string(), Some(attrs))
    }
    
    spans.push(span)
  }
  
  let after_create_memory = Memory::allocated()
  let memory_per_large_object = (after_create_memory - initial_memory) / large_object_count
  
  // Verify reasonable memory usage per large object
  assert_true(memory_per_large_object < 60000)  // Less than 60KB per large object
  
  // End spans to trigger cleanup
  for span in spans {
    Span::end(span)
  }
  
  // Force garbage collection if available
  Memory::gc()
  
  let after_cleanup_memory = Memory::allocated()
  let memory_recovered = after_create_memory - after_cleanup_memory
  
  // Verify most memory is recovered
  let recovery_percentage = (memory_recovered * 100) / (after_create_memory - initial_memory)
  assert_true(recovery_percentage > 75)  // At least 75% memory recovered
}

// Test 9: Memory Leak Detection
test "memory leak detection" {
  let initial_memory = Memory::allocated()
  let iterations = 10
  let objects_per_iteration = 100
  
  // Perform multiple iterations of creating and cleaning up objects
  for iteration in 0..iterations {
    // Create spans
    let spans = []
    for i in 0..objects_per_iteration {
      let trace_id = "leak_test_trace_" + iteration.to_string() + "_" + i.to_string()
      let span_id = "leak_test_span_" + iteration.to_string() + "_" + i.to_string()
      let span_ctx = SpanContext::new(trace_id, span_id, true, "")
      let span = Span::new("leak_test_operation", Internal, span_ctx)
      
      // Add attributes and events
      let attrs = Attributes::new()
      Attributes::set(attrs, "iteration", IntValue(iteration))
      Attributes::set(attrs, "index", IntValue(i))
      
      Span::add_event(span, "leak_test_event", Some(attrs))
      spans.push(span)
    }
    
    // End spans
    for span in spans {
      Span::end(span)
    }
    
    // Clear references
    spans.clear()
    
    // Force garbage collection every few iterations
    if iteration % 3 == 0 {
      Memory::gc()
    }
  }
  
  // Final garbage collection
  Memory::gc()
  
  let final_memory = Memory::allocated()
  let memory_increase = final_memory - initial_memory
  
  // Verify memory increase is minimal (indicating no significant leaks)
  let memory_increase_per_object = memory_increase / (iterations * objects_per_iteration)
  assert_true(memory_increase_per_object < 50)  // Less than 50 bytes per object
}

// Test 10: Memory Pressure Handling
test "memory pressure handling" {
  let initial_memory = Memory::allocated()
  let memory_limit = Memory::available() / 4  // Use 25% of available memory
  let objects_created = 0
  
  // Create objects until near memory limit
  let spans = []
  while Memory::allocated() - initial_memory < memory_limit && objects_created < 10000 {
    let trace_id = "pressure_test_trace_" + objects_created.to_string()
    let span_id = "pressure_test_span_" + objects_created.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("pressure_test_operation", Internal, span_ctx)
    
    // Add attributes
    let attrs = Attributes::new()
    Attributes::set(attrs, "index", IntValue(objects_created))
    Attributes::set(attrs, "data", StringValue("pressure_test_data_" + objects_created.to_string()))
    
    Span::add_event(span, "pressure_test_event", Some(attrs))
    spans.push(span)
    
    objects_created = objects_created + 1
  }
  
  let peak_memory = Memory::allocated()
  let memory_used = peak_memory - initial_memory
  
  // Verify we used significant memory but didn't exceed limit
  assert_true(memory_used > memory_limit * 8 / 10)  // At least 80% of limit
  assert_true(memory_used < memory_limit * 12 / 10)  // Less than 120% of limit
  
  // Test cleanup under memory pressure
  for span in spans {
    Span::end(span)
  }
  
  // Clear references
  spans.clear()
  
  // Force garbage collection
  Memory::gc()
  
  let after_cleanup_memory = Memory::allocated()
  let memory_recovered = peak_memory - after_cleanup_memory
  
  // Verify most memory is recovered even under pressure
  let recovery_percentage = (memory_recovered * 100) / memory_used
  assert_true(recovery_percentage > 70)  // At least 70% memory recovered
}