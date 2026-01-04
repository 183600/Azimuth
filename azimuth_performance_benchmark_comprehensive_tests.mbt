// Azimuth Telemetry System - Comprehensive Performance Benchmark Tests
// This file contains comprehensive test cases for performance benchmarking

// Test 1: Span Creation Performance
test "span creation performance benchmark" {
  // Benchmark span creation with different attribute counts
  let iterations = 1000
  let trace_ctx = SpanContext::new("perf_trace_001", "perf_root", true, "")
  
  // Benchmark 1: Minimal span creation
  let start_time = Performance::current_time_millis()
  for i in 0..iterations {
    let span = Span::new("minimal_span", Internal, trace_ctx)
    Span::end(span)
  }
  let minimal_time = Performance::current_time_millis() - start_time
  
  // Verify minimal span creation is within reasonable time (should be < 100ms for 1000 iterations)
  assert_true(minimal_time < 100, "Minimal span creation should be fast")
  
  // Benchmark 2: Span creation with attributes
  let start_time_attrs = Performance::current_time_millis()
  for i in 0..iterations {
    let span = Span::new("attr_span", Internal, trace_ctx)
    Span::add_attribute(span, "iteration", IntValue(i))
    Span::add_attribute(span, "operation.type", StringValue("benchmark"))
    Span::add_attribute(span, "service.name", StringValue("performance_test"))
    Span::end(span)
  }
  let attrs_time = Performance::current_time_millis() - start_time_attrs
  
  // Verify span creation with attributes is within reasonable time
  assert_true(attrs_time < 200, "Span creation with attributes should be reasonably fast")
  
  // Benchmark 3: Span creation with events
  let start_time_events = Performance::current_time_millis()
  for i in 0..iterations {
    let span = Span::new("event_span", Internal, trace_ctx)
    Span::add_event(span, "operation_started", Some([("iteration", IntValue(i))]))
    Span::add_event(span, "operation_completed", Some([("duration", IntValue(i * 2))]))
    Span::end(span)
  }
  let events_time = Performance::current_time_millis() - start_time_events
  
  // Verify span creation with events is within reasonable time
  assert_true(events_time < 300, "Span creation with events should be reasonably fast")
  
  // Performance comparison
  let attrs_overhead = attrs_time - minimal_time
  let events_overhead = events_time - minimal_time
  
  // Verify overhead is reasonable
  assert_true(attrs_overhead < minimal_time * 2, "Attribute overhead should be reasonable")
  assert_true(events_overhead < minimal_time * 3, "Event overhead should be reasonable")
}

// Test 2: Attribute Operations Performance
test "attribute operations performance benchmark" {
  let iterations = 1000
  let attrs = Attributes::new()
  
  // Benchmark 1: Attribute setting performance
  let start_time_set = Performance::current_time_millis()
  for i in 0..iterations {
    Attributes::set(attrs, "key_" + i.to_string(), IntValue(i))
  }
  let set_time = Performance::current_time_millis() - start_time_set
  
  // Verify attribute setting is within reasonable time
  assert_true(set_time < 100, "Attribute setting should be fast")
  
  // Benchmark 2: Attribute getting performance
  let start_time_get = Performance::current_time_millis()
  for i in 0..iterations {
    let result = Attributes::get(attrs, "key_" + i.to_string())
    match result {
      Some(IntValue(value)) => assert_eq(value, i)
      _ => assert_true(false)
    }
  }
  let get_time = Performance::current_time_millis() - start_time_get
  
  // Verify attribute getting is within reasonable time
  assert_true(get_time < 100, "Attribute getting should be fast")
  
  // Benchmark 3: Mixed attribute operations
  let mixed_attrs = Attributes::new()
  let start_time_mixed = Performance::current_time_millis()
  for i in 0..iterations {
    if i % 3 == 0 {
      Attributes::set(mixed_attrs, "mixed_key_" + i.to_string(), StringValue("value_" + i.to_string()))
    } else if i % 3 == 1 {
      Attributes::set(mixed_attrs, "mixed_key_" + i.to_string(), FloatValue(i * 3.14))
    } else {
      Attributes::set(mixed_attrs, "mixed_key_" + i.to_string(), BoolValue(i % 2 == 0))
    }
    
    // Get some existing attributes
    if i > 10 {
      let result = Attributes::get(mixed_attrs, "mixed_key_" + (i - 10).to_string())
      assert_true(result.is_some())
    }
  }
  let mixed_time = Performance::current_time_millis() - start_time_mixed
  
  // Verify mixed operations are within reasonable time
  assert_true(mixed_time < 200, "Mixed attribute operations should be reasonably fast")
}

// Test 3: Metrics Collection Performance
test "metrics collection performance benchmark" {
  let iterations = 1000
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance_meter")
  
  // Create instruments
  let counter = Meter::create_counter(meter, "performance_counter", Some("Performance counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "performance_histogram", Some("Performance histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "performance_updown", Some("Performance updown counter"), Some("value"))
  
  // Benchmark 1: Counter operations
  let start_time_counter = Performance::current_time_millis()
  for i in 0..iterations {
    Counter::add(counter, 1.0)
    Counter::add(counter, i.to_float(), Some([("iteration", IntValue(i))]))
  }
  let counter_time = Performance::current_time_millis() - start_time_counter
  
  // Verify counter operations are within reasonable time
  assert_true(counter_time < 100, "Counter operations should be fast")
  
  // Benchmark 2: Histogram operations
  let start_time_histogram = Performance::current_time_millis()
  for i in 0..iterations {
    Histogram::record(histogram, i.to_float())
    Histogram::record(histogram, i * 2.5, Some([("iteration", IntValue(i))]))
  }
  let histogram_time = Performance::current_time_millis() - start_time_histogram
  
  // Verify histogram operations are within reasonable time
  assert_true(histogram_time < 150, "Histogram operations should be reasonably fast")
  
  // Benchmark 3: UpDownCounter operations
  let start_time_updown = Performance::current_time_millis()
  for i in 0..iterations {
    UpDownCounter::add(updown_counter, 1.0)
    UpDownCounter::add(updown_counter, -0.5, Some([("iteration", IntValue(i))]))
  }
  let updown_time = Performance::current_time_millis() - start_time_updown
  
  // Verify updown counter operations are within reasonable time
  assert_true(updown_time < 100, "UpDownCounter operations should be fast")
  
  // Benchmark 4: Metrics collection
  let start_time_collect = Performance::current_time_millis()
  let metric_data = Meter::collect(meter)
  let collect_time = Performance::current_time_millis() - start_time_collect
  
  // Verify metrics collection is within reasonable time
  assert_true(collect_time < 50, "Metrics collection should be fast")
  
  // Verify collected metrics contain expected data
  let metrics = MetricData::metrics(metric_data)
  assert_true(metrics.length() >= 3)
}

// Test 4: Serialization Performance
test "serialization performance benchmark" {
  let iterations = 100
  
  // Create test data
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.string.key", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int.key", ArrayIntValue([1, 2, 3, 4, 5]))
  
  let span_ctx = SpanContext::new("serialization_trace", "serialization_span", true, "")
  let span = Span::new("serialization_test", Internal, span_ctx)
  Span::add_attribute(span, "test.attr", StringValue("test_value"))
  Span::add_event(span, "test_event", Some([("event.attr", IntValue(123))]))
  
  // Benchmark 1: Attribute serialization
  let start_time_attr_serialization = Performance::current_time_millis()
  for i in 0..iterations {
    let serialized = Attributes::serialize(attrs)
    let deserialized = Attributes::deserialize(serialized)
  }
  let attr_serialization_time = Performance::current_time_millis() - start_time_attr_serialization
  
  // Verify attribute serialization is within reasonable time
  assert_true(attr_serialization_time < 100, "Attribute serialization should be reasonably fast")
  
  // Benchmark 2: Span serialization
  let start_time_span_serialization = Performance::current_time_millis()
  for i in 0..iterations {
    let serialized = Span::serialize(span)
    let deserialized = Span::deserialize(serialized)
  }
  let span_serialization_time = Performance::current_time_millis() - start_time_span_serialization
  
  // Verify span serialization is within reasonable time
  assert_true(span_serialization_time < 200, "Span serialization should be reasonably fast")
  
  // Benchmark 3: Span context serialization
  let start_time_ctx_serialization = Performance::current_time_millis()
  for i in 0..iterations {
    let serialized = SpanContext::serialize(span_ctx)
    let deserialized = SpanContext::deserialize(serialized)
  }
  let ctx_serialization_time = Performance::current_time_millis() - start_time_ctx_serialization
  
  // Verify span context serialization is within reasonable time
  assert_true(ctx_serialization_time < 50, "Span context serialization should be fast")
  
  // Performance comparison
  let avg_attr_time = attr_serialization_time.to_float() / iterations.to_float()
  let avg_span_time = span_serialization_time.to_float() / iterations.to_float()
  let avg_ctx_time = ctx_serialization_time.to_float() / iterations.to_float()
  
  // Verify average operation times are reasonable
  assert_true(avg_attr_time < 1.0, "Average attribute serialization time should be < 1ms")
  assert_true(avg_span_time < 2.0, "Average span serialization time should be < 2ms")
  assert_true(avg_ctx_time < 0.5, "Average span context serialization time should be < 0.5ms")
}

// Test 5: Context Propagation Performance
test "context propagation performance benchmark" {
  let iterations = 1000
  let root_ctx = SpanContext::new("context_perf_trace", "context_perf_root", true, "")
  let baggage = Baggage::new()
  let baggage_with_data = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage_final = Baggage::set_entry(baggage_with_data, "request.id", "req456")
  
  // Benchmark 1: Context creation
  let start_time_ctx_creation = Performance::current_time_millis()
  for i in 0..iterations {
    let ctx = SpanContext::new("trace_" + i.to_string(), "span_" + i.to_string(), true, "")
  }
  let ctx_creation_time = Performance::current_time_millis() - start_time_ctx_creation
  
  // Verify context creation is within reasonable time
  assert_true(ctx_creation_time < 100, "Context creation should be fast")
  
  // Benchmark 2: Context propagation
  let start_time_ctx_propagation = Performance::current_time_millis()
  for i in 0..iterations {
    let child_ctx = SpanContext::new(
      SpanContext::trace_id(root_ctx),
      "child_span_" + i.to_string(),
      SpanContext::is_sampled(root_ctx),
      ""
    )
  }
  let ctx_propagation_time = Performance::current_time_millis() - start_time_ctx_propagation
  
  // Verify context propagation is within reasonable time
  assert_true(ctx_propagation_time < 100, "Context propagation should be fast")
  
  // Benchmark 3: Baggage operations
  let start_time_baggage_ops = Performance::current_time_millis()
  for i in 0..iterations {
    let updated_baggage = Baggage::set_entry(baggage_final, "iteration", i.to_string())
    let value = Baggage::get_entry(updated_baggage, "user.id")
    match value {
      Some(v) => assert_eq(v, "user123")
      None => assert_true(false)
    }
  }
  let baggage_ops_time = Performance::current_time_millis() - start_time_baggage_ops
  
  // Verify baggage operations are within reasonable time
  assert_true(baggage_ops_time < 150, "Baggage operations should be reasonably fast")
}

// Test 6: Memory Usage Performance
test "memory usage performance benchmark" {
  let initial_memory = Performance::get_memory_usage()
  
  // Create a large number of spans
  let spans = []
  let iterations = 100
  let trace_ctx = SpanContext::new("memory_trace", "memory_root", true, "")
  
  for i in 0..iterations {
    let span = Span::new("memory_test_span_" + i.to_string(), Internal, trace_ctx)
    Span::add_attribute(span, "iteration", IntValue(i))
    Span::add_attribute(span, "operation.type", StringValue("memory_test"))
    Span::add_event(span, "test_event", Some([("event_data", StringValue("test_data_" + i.to_string()))]))
    spans.push(span)
  }
  
  let after_creation_memory = Performance::get_memory_usage()
  let memory_increase = after_creation_memory - initial_memory
  
  // Verify memory increase is reasonable (should be < 10MB for 100 spans)
  assert_true(memory_increase < 10 * 1024 * 1024, "Memory usage should be reasonable")
  
  // Clean up spans
  for span in spans {
    Span::end(span)
  }
  
  // Force garbage collection if available
  Performance::force_gc()
  
  let after_cleanup_memory = Performance::get_memory_usage()
  let memory_after_cleanup = after_cleanup_memory - initial_memory
  
  // Verify memory is properly cleaned up (should be significantly reduced)
  assert_true(memory_after_cleanup < memory_increase * 0.5, "Memory should be properly cleaned up")
}

// Test 7: Concurrent Operations Performance
test "concurrent operations performance benchmark" {
  let iterations = 100
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent_meter")
  let counter = Meter::create_counter(meter, "concurrent_counter", Some("Concurrent counter"), Some("count"))
  
  // Benchmark concurrent span creation
  let start_time_concurrent_spans = Performance::current_time_millis()
  
  // Simulate concurrent operations
  for i in 0..iterations {
    let trace_ctx = SpanContext::new("concurrent_trace_" + i.to_string(), "concurrent_root_" + i.to_string(), true, "")
    let span = Span::new("concurrent_span_" + i.to_string(), Internal, trace_ctx)
    Span::add_attribute(span, "thread.id", IntValue(i))
    Span::end(span)
  }
  
  let concurrent_spans_time = Performance::current_time_millis() - start_time_concurrent_spans
  
  // Verify concurrent span creation is within reasonable time
  assert_true(concurrent_spans_time < 200, "Concurrent span creation should be reasonably fast")
  
  // Benchmark concurrent metrics operations
  let start_time_concurrent_metrics = Performance::current_time_millis()
  
  for i in 0..iterations {
    Counter::add(counter, 1.0, Some([("thread.id", IntValue(i))]))
  }
  
  let concurrent_metrics_time = Performance::current_time_millis() - start_time_concurrent_metrics
  
  // Verify concurrent metrics operations are within reasonable time
  assert_true(concurrent_metrics_time < 100, "Concurrent metrics operations should be fast")
}

// Test 8: Large Payload Performance
test "large payload performance benchmark" {
  let iterations = 10
  let large_string_size = 10000
  let large_string = "x".repeat(large_string_size)
  let large_array = Array::init(large_string_size, fn(i) { i })
  
  // Benchmark 1: Large string attribute
  let start_time_large_string = Performance::current_time_millis()
  for i in 0..iterations {
    let attrs = Attributes::new()
    Attributes::set(attrs, "large.string", StringValue(large_string))
    let serialized = Attributes::serialize(attrs)
    let deserialized = Attributes::deserialize(serialized)
  }
  let large_string_time = Performance::current_time_millis() - start_time_large_string
  
  // Verify large string attribute handling is within reasonable time
  assert_true(large_string_time < 1000, "Large string attribute handling should be reasonably fast")
  
  // Benchmark 2: Large array attribute
  let start_time_large_array = Performance::current_time_millis()
  for i in 0..iterations {
    let attrs = Attributes::new()
    Attributes::set(attrs, "large.array", ArrayIntValue(large_array))
    let serialized = Attributes::serialize(attrs)
    let deserialized = Attributes::deserialize(serialized)
  }
  let large_array_time = Performance::current_time_millis() - start_time_large_array
  
  // Verify large array attribute handling is within reasonable time
  assert_true(large_array_time < 1000, "Large array attribute handling should be reasonably fast")
  
  // Benchmark 3: Large span with many events
  let trace_ctx = SpanContext::new("large_payload_trace", "large_payload_root", true, "")
  let start_time_large_span = Performance::current_time_millis()
  
  for i in 0..iterations {
    let span = Span::new("large_span", Internal, trace_ctx)
    
    // Add many events
    for j in 0..100 {
      Span::add_event(span, "event_" + j.to_string(), Some([
        ("event.data", StringValue("large_event_data_" + j.to_string())),
        ("iteration", IntValue(j))
      ]))
    }
    
    let serialized = Span::serialize(span)
    let deserialized = Span::deserialize(serialized)
  }
  
  let large_span_time = Performance::current_time_millis() - start_time_large_span
  
  // Verify large span handling is within reasonable time
  assert_true(large_span_time < 2000, "Large span handling should be reasonably fast")
}

// Test 9: High Frequency Operations Performance
test "high frequency operations performance benchmark" {
  let high_frequency_iterations = 10000
  let trace_ctx = SpanContext::new("high_freq_trace", "high_freq_root", true, "")
  
  // Benchmark high frequency span creation
  let start_time_high_freq = Performance::current_time_millis()
  
  for i in 0..high_frequency_iterations {
    let span = Span::new("high_freq_span", Internal, trace_ctx)
    Span::add_attribute(span, "iteration", IntValue(i))
    Span::end(span)
  }
  
  let high_freq_time = Performance::current_time_millis() - start_time_high_freq
  
  // Verify high frequency operations are within reasonable time
  assert_true(high_freq_time < 1000, "High frequency operations should be fast")
  
  // Calculate operations per second
  let ops_per_second = high_frequency_iterations.to_float() / (high_freq_time.to_float() / 1000.0)
  
  // Verify we can handle at least 1000 operations per second
  assert_true(ops_per_second >= 1000.0, "Should handle at least 1000 ops/sec")
}

// Test 10: Resource Usage Under Load
test "resource usage under load benchmark" {
  let initial_memory = Performance::get_memory_usage()
  let initial_cpu = Performance::get_cpu_usage()
  
  let load_iterations = 500
  let trace_ctx = SpanContext::new("load_trace", "load_root", true, "")
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "load_meter")
  let counter = Meter::create_counter(meter, "load_counter", Some("Load counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "load_histogram", Some("Load histogram"), Some("ms"))
  
  // Apply load
  for i in 0..load_iterations {
    // Create spans
    let span = Span::new("load_span_" + i.to_string(), Internal, trace_ctx)
    Span::add_attribute(span, "iteration", IntValue(i))
    Span::add_attribute(span, "operation.type", StringValue("load_test"))
    Span::add_event(span, "load_event", Some([("event_data", StringValue("load_test_data"))]))
    
    // Record metrics
    Counter::add(counter, 1.0, Some([("iteration", IntValue(i))]))
    Histogram::record(histogram, i.to_float(), Some([("iteration", IntValue(i))]))
    
    // Create and manipulate attributes
    let attrs = Attributes::new()
    Attributes::set(attrs, "load.iteration", IntValue(i))
    Attributes::set(attrs, "load.operation", StringValue("test"))
    Attributes::set(attrs, "load.timestamp", IntValue(Performance::current_time_millis()))
    
    // Serialize/deserialize
    let serialized_attrs = Attributes::serialize(attrs)
    let deserialized_attrs = Attributes::deserialize(serialized_attrs)
    
    Span::end(span)
  }
  
  let final_memory = Performance::get_memory_usage()
  let final_cpu = Performance::get_cpu_usage()
  
  let memory_increase = final_memory - initial_memory
  let cpu_increase = final_cpu - initial_cpu
  
  // Verify resource usage is reasonable
  assert_true(memory_increase < 50 * 1024 * 1024, "Memory increase should be < 50MB under load")
  assert_true(cpu_increase < 80.0, "CPU usage should be reasonable under load")
  
  // Collect metrics
  let metric_data = Meter::collect(meter)
  let metrics = MetricData::metrics(metric_data)
  
  // Verify metrics were collected correctly
  assert_true(metrics.length() >= 2)
  
  // Check if we can still operate efficiently after load
  let post_load_start = Performance::current_time_millis()
  let post_load_span = Span::new("post_load_span", Internal, trace_ctx)
  Span::add_attribute(post_load_span, "test.type", StringValue("post_load"))
  Span::end(post_load_span)
  let post_load_time = Performance::current_time_millis() - post_load_start
  
  // Verify operations are still efficient after load
  assert_true(post_load_time < 10, "Operations should remain efficient after load")
}