// Performance Benchmark Tests
// This file contains test cases for performance benchmarking of telemetry operations

test "high-frequency span creation performance" {
  // Test performance of creating many spans rapidly
  let start_time = azimuth::Time::now()
  let span_count = 10000
  
  for i = 0; i < span_count; i = i + 1 {
    let trace_id = "trace-" + azimuth::Int::to_string(i)
    let span_id = "span-" + azimuth::Int::to_string(i)
    let span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "")
    let span = azimuth::Span::new("high-frequency-operation", azimuth::Internal, span_ctx)
    
    // Add some attributes
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "iteration", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "operation.type", azimuth::StringValue("benchmark"))
    
    // End the span
    azimuth::Span::end(span)
  }
  
  let end_time = azimuth::Time::now()
  let duration = azimuth::Time::difference(end_time, start_time)
  let spans_per_second = (span_count * 1000) / duration
  
  // Performance assertion: should handle at least 1000 spans per second
  assert_true(spans_per_second >= 1000, "Span creation performance below threshold: " + azimuth::Int::to_string(spans_per_second) + " spans/sec")
}

test "metric aggregation performance" {
  // Test performance of metric aggregation with large datasets
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "performance-test")
  
  let histogram = azimuth::Meter::create_histogram(meter, "latency.histogram", "Latency histogram", "ms")
  let counter = azimuth::Meter::create_counter(meter, "operation.counter", "Operation counter", "1")
  let gauge = azimuth::Meter::create_gauge(meter, "memory.usage", "Memory usage gauge", "bytes")
  
  let start_time = azimuth::Time::now()
  let measurement_count = 50000
  
  // Record large number of measurements
  for i = 0; i < measurement_count; i = i + 1 {
    // Record histogram measurements
    let latency = azimuth::Float::from_int(i % 1000) * 0.1
    azimuth::Histogram::record(histogram, latency, [("operation", azimuth::StringValue("op-" + azimuth::Int::to_string(i % 10)))])
    
    // Record counter increments
    azimuth::Counter::add(counter, 1, [("type", azimuth::StringValue("benchmark"))])
    
    // Update gauge values
    if i % 100 == 0 {
      let memory_value = 1024 * 1024 * (i / 100)
      azimuth::Gauge::set(gauge, memory_value, [("instance", azimuth::StringValue("server-" + azimuth::Int::to_string(i / 100)))])
    }
  }
  
  let recording_time = azimuth::Time::now()
  let recording_duration = azimuth::Time::difference(recording_time, start_time)
  
  // Test aggregation performance
  let aggregation_start = azimuth::Time::now()
  let histogram_metrics = azimuth::Histogram::get_measurements(histogram)
  let counter_metrics = azimuth::Counter::get_value(counter)
  let gauge_metrics = azimuth::Gauge::get_value(gauge)
  
  let aggregation_end = azimuth::Time::now()
  let aggregation_duration = azimuth::Time::difference(aggregation_end, aggregation_start)
  
  // Performance assertions
  let measurements_per_second = (measurement_count * 1000) / recording_duration
  assert_true(measurements_per_second >= 10000, "Metric recording performance below threshold: " + azimuth::Int::to_string(measurements_per_second) + " measurements/sec")
  
  assert_true(aggregation_duration < 100, "Metric aggregation too slow: " + azimuth::Int::to_string(aggregation_duration) + "ms")
  
  // Verify data integrity
  assert_eq(counter_metrics, measurement_count)
  assert_true(histogram_metrics.length > 0)
  assert_true(gauge_metrics.length > 0)
}

test "attribute operations performance" {
  // Test performance of attribute operations with various data types
  let start_time = azimuth::Time::now()
  let operation_count = 100000
  
  for i = 0; i < operation_count; i = i + 1 {
    let attrs = azimuth::Attributes::new()
    
    // Set various attribute types
    azimuth::Attributes::set(attrs, "string.key", azimuth::StringValue("value-" + azimuth::Int::to_string(i)))
    azimuth::Attributes::set(attrs, "int.key", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "float.key", azimuth::FloatValue(azimuth::Float::from_int(i) * 0.1))
    azimuth::Attributes::set(attrs, "bool.key", azimuth::BoolValue(i % 2 == 0))
    
    // Array attributes
    if i % 10 == 0 {
      azimuth::Attributes::set(attrs, "array.string", azimuth::ArrayStringValue(["a", "b", "c"]))
      azimuth::Attributes::set(attrs, "array.int", azimuth::ArrayIntValue([i, i + 1, i + 2]))
    }
    
    // Get operations
    let string_val = azimuth::Attributes::get(attrs, "string.key")
    let int_val = azimuth::Attributes::get(attrs, "int.key")
    let float_val = azimuth::Attributes::get(attrs, "float.key")
    let bool_val = azimuth::Attributes::get(attrs, "bool.key")
    
    // Verify values
    assert_eq(string_val, Some(azimuth::StringValue("value-" + azimuth::Int::to_string(i))))
    assert_eq(int_val, Some(azimuth::IntValue(i)))
    assert_eq(float_val, Some(azimuth::FloatValue(azimuth::Float::from_int(i) * 0.1)))
    assert_eq(bool_val, Some(azimuth::BoolValue(i % 2 == 0)))
  }
  
  let end_time = azimuth::Time::now()
  let duration = azimuth::Time::difference(end_time, start_time)
  let operations_per_second = (operation_count * 1000) / duration
  
  // Performance assertion: should handle at least 5000 attribute operations per second
  assert_true(operations_per_second >= 5000, "Attribute operations performance below threshold: " + azimuth::Int::to_string(operations_per_second) + " ops/sec")
}

test "context propagation performance" {
  // Test performance of context propagation operations
  let root_ctx = azimuth::Context::root()
  let start_time = azimuth::Time::now()
  let propagation_count = 50000
  
  // Build deep context hierarchy
  let current_ctx = root_ctx
  for i = 0; i < propagation_count; i = i + 1 {
    let key = azimuth::ContextKey::new("key-" + azimuth::Int::to_string(i))
    current_ctx = azimuth::Context::with_value(current_ctx, key, "value-" + azimuth::Int::to_string(i))
    
    // Test retrieval performance
    if i % 1000 == 0 {
      let retrieval_key = azimuth::ContextKey::new("key-" + azimuth::Int::to_string(i / 2))
      let retrieved_value = azimuth::Context::get(current_ctx, retrieval_key)
      assert_eq(retrieved_value, Some("value-" + azimuth::Int::to_string(i / 2)))
    }
  }
  
  let end_time = azimuth::Time::now()
  let duration = azimuth::Time::difference(end_time, start_time)
  let propagations_per_second = (propagation_count * 1000) / duration
  
  // Performance assertion: should handle at least 10000 context propagations per second
  assert_true(propagations_per_second >= 10000, "Context propagation performance below threshold: " + azimuth::Int::to_string(propagations_per_second) + " propagations/sec")
}

test "memory usage under load" {
  // Test memory usage patterns under high load
  let initial_memory = azimuth::Memory::get_usage()
  let span_count = 10000
  
  // Create spans and hold references
  let spans = []
  for i = 0; i < span_count; i = i + 1 {
    let trace_id = "trace-" + azimuth::Int::to_string(i)
    let span_id = "span-" + azimuth::Int::to_string(i)
    let span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "")
    let span = azimuth::Span::new("memory-test-operation", azimuth::Internal, span_ctx)
    
    // Add attributes
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "index", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "data", azimuth::StringValue("sample-data-" + azimuth::Int::to_string(i)))
    
    spans = azimuth::List::append(spans, span)
  }
  
  let peak_memory = azimuth::Memory::get_usage()
  let memory_increase = peak_memory - initial_memory
  let memory_per_span = memory_increase / span_count
  
  // Clear references
  spans = []
  
  let final_memory = azimuth::Memory::get_usage()
  let memory_recovered = peak_memory - final_memory
  
  // Memory efficiency assertions
  assert_true(memory_per_span < 1024, "Memory per span too high: " + azimuth::Int::to_string(memory_per_span) + " bytes")
  assert_true(memory_recovered > (memory_increase * 0.8), "Memory not properly recovered: " + azimuth::Int::to_string(memory_recovered) + " bytes recovered")
}