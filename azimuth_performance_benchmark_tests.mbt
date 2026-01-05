// Azimuth Telemetry System - Performance Benchmark Tests
// This file contains test cases for performance benchmarking of the telemetry system

// Test 1: Span Creation Performance
test "span creation performance" {
  let iterations = 10000
  let start_time = Time::now()
  
  // Create spans
  for i in 0..=iterations {
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("performance_test_span", Internal, span_ctx)
    
    // Add some attributes
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    Span::set_attributes(span, attrs)
    
    // Add an event
    Span::add_event(span, "test_event", Some([("event.iteration", IntValue(i))]))
    
    // End the span
    Span::end(span)
  }
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // Calculate spans per second
  let spans_per_second = (iterations as Float) / (duration as Float)
  
  // Verify performance meets expectations
  assert_true(spans_per_second > 1000.0) // Should create at least 1000 spans per second
}

// Test 2: Metric Recording Performance
test "metric recording performance" {
  let iterations = 10000
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance_meter")
  let counter = Meter::create_counter(meter, "performance_counter", Some("Performance counter"), Some("count"))
  
  let start_time = Time::now()
  
  // Record metrics
  for i in 0..=iterations {
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    Counter::add(counter, 1.0, Some(attrs))
  }
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // Calculate metrics per second
  let metrics_per_second = (iterations as Float) / (duration as Float)
  
  // Verify performance meets expectations
  assert_true(metrics_per_second > 10000.0) // Should record at least 10000 metrics per second
}

// Test 3: Context Propagation Performance
test "context propagation performance" {
  let iterations = 10000
  let propagator = TraceContextPropagator::new()
  
  let start_time = Time::now()
  
  // Inject and extract context
  for i in 0..=iterations {
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    
    // Inject context
    let carrier = TextMapCarrier::new()
    Propagator::inject(propagator, span_ctx, carrier)
    
    // Extract context
    let extracted_ctx = Propagator::extract(propagator, carrier)
    
    // Verify extracted context
    assert_eq(SpanContext::trace_id(extracted_ctx), trace_id)
    assert_eq(SpanContext::span_id(extracted_ctx), span_id)
  }
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // Calculate context propagations per second
  let propagations_per_second = (iterations as Float) / (duration as Float)
  
  // Verify performance meets expectations
  assert_true(propagations_per_second > 5000.0) // Should propagate at least 5000 contexts per second
}

// Test 4: Serialization Performance
test "serialization performance" {
  let iterations = 1000
  let spans = []
  
  // Create spans for serialization
  for i in 0..=iterations {
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("serialization_test_span", Internal, span_ctx)
    
    // Add attributes
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    Attributes::set(attrs, "test.attribute", StringValue("test_value_" + i.to_string()))
    Span::set_attributes(span, attrs)
    
    // Add events
    for j in 0..=5 {
      Span::add_event(span, "event_" + j.to_string(), Some([("event.iteration", IntValue(j))]))
    }
    
    spans.push(span)
  }
  
  // Test JSON serialization
  let start_time = Time::now()
  
  for span in spans {
    let json_str = SpanSerializer::to_json(span)
    assert_true(json_str.length() > 0)
  }
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // Calculate serializations per second
  let serializations_per_second = (iterations as Float) / (duration as Float)
  
  // Verify performance meets expectations
  assert_true(serializations_per_second > 100.0) // Should serialize at least 100 spans per second
}

// Test 5: Deserialization Performance
test "deserialization performance" {
  let iterations = 1000
  let json_strings = []
  
  // Create JSON strings for deserialization
  for i in 0..=iterations {
    let json_str = "{"
      + "\"trace_id\":\"trace_" + i.to_string() + "\","
      + "\"span_id\":\"span_" + i.to_string() + "\","
      + "\"name\":\"deserialization_test_span\","
      + "\"kind\":\"INTERNAL\","
      + "\"attributes\":{\"iteration\":" + i.to_string() + ",\"test.attribute\":\"test_value_" + i.to_string() + "\"},"
      + "\"events\":[{\"name\":\"event_0\",\"attributes\":{\"event.iteration\":0}}],"
      + "\"start_time\":1234567890,"
      + "\"end_time\":1234567895"
      + "}"
    json_strings.push(json_str)
  }
  
  // Test JSON deserialization
  let start_time = Time::now()
  
  for json_str in json_strings {
    let span = SpanDeserializer::from_json(json_str)
    assert_eq(Span::name(span), "deserialization_test_span")
  }
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // Calculate deserializations per second
  let deserializations_per_second = (iterations as Float) / (duration as Float)
  
  // Verify performance meets expectations
  assert_true(deserializations_per_second > 100.0) // Should deserialize at least 100 spans per second
}

// Test 6: Memory Usage Benchmark
test "memory usage benchmark" {
  let initial_memory = Memory::usage()
  
  // Create a large number of spans
  let spans = []
  let iterations = 10000
  
  for i in 0..=iterations {
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("memory_test_span", Internal, span_ctx)
    
    // Add attributes
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    Attributes::set(attrs, "large.data", StringValue("x".repeat(100))) // 100 character string
    Span::set_attributes(span, attrs)
    
    spans.push(span)
  }
  
  let peak_memory = Memory::usage()
  
  // Clear spans to trigger garbage collection
  spans.clear()
  Memory::collect()
  
  let final_memory = Memory::usage()
  
  // Calculate memory usage per span
  let memory_per_span = (peak_memory - initial_memory) / (iterations as Int)
  
  // Verify memory usage is reasonable
  assert_true(memory_per_span < 10000) // Should use less than 10KB per span
  
  // Verify memory is properly cleaned up
  assert_true((final_memory - initial_memory) < (peak_memory - initial_memory) / 2)
}

// Test 7: Concurrent Span Creation Performance
test "concurrent span creation performance" {
  let iterations = 1000
  let threads = 4
  let iterations_per_thread = iterations / threads
  
  let start_time = Time::now()
  
  // Create spans concurrently
  let handles = []
  
  for t in 0..=threads-1 {
    let handle = Thread::spawn(|thread_id| {
      for i in 0..=iterations_per_thread {
        let trace_id = "trace_" + thread_id.to_string() + "_" + i.to_string()
        let span_id = "span_" + thread_id.to_string() + "_" + i.to_string()
        let span_ctx = SpanContext::new(trace_id, span_id, true, "")
        let span = Span::new("concurrent_test_span", Internal, span_ctx)
        
        // Add attributes
        let attrs = Attributes::new()
        Attributes::set(attrs, "thread_id", IntValue(thread_id))
        Attributes::set(attrs, "iteration", IntValue(i))
        Span::set_attributes(span, attrs)
        
        Span::end(span)
      }
    })
    handles.push(handle)
  }
  
  // Wait for all threads to complete
  for handle in handles {
    Thread::join(handle)
  }
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // Calculate spans per second
  let spans_per_second = (iterations as Float) / (duration as Float)
  
  // Verify performance meets expectations
  assert_true(spans_per_second > 500.0) // Should create at least 500 spans per second with concurrency
}

// Test 8: Aggregation Performance
test "aggregation performance" {
  let iterations = 10000
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_meter")
  let histogram = Meter::create_histogram(meter, "performance_histogram", Some("Performance histogram"), Some("ms"))
  
  // Record measurements
  for i in 0..=iterations {
    Histogram::record(histogram, (i % 1000) as Float)
  }
  
  // Test aggregation performance
  let start_time = Time::now()
  
  let aggregator = MetricAggregator::new()
  let aggregated = MetricAggregator::aggregate_histogram(aggregator, histogram)
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // Verify aggregation result
  match aggregated {
    AggregatedMetric::Histogram(stats) => {
      assert_eq(stats.count, iterations + 1)
      assert_eq(stats.min, 0.0)
      assert_eq(stats.max, 999.0)
    }
    _ => assert_true(false)
  }
  
  // Calculate aggregation time
  let aggregation_time_ms = (duration as Float) * 1000.0
  
  // Verify performance meets expectations
  assert_true(aggregation_time_ms < 100.0) // Should aggregate in less than 100ms
}

// Test 9: Filtering Performance
test "filtering performance" {
  let iterations = 10000
  let spans = []
  
  // Create spans with different attributes
  for i in 0..=iterations {
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    Attributes::set(attrs, "category", StringValue(if i % 2 == 0 { "even" } else { "odd" }))
    Attributes::set(attrs, "priority", StringValue(if i % 3 == 0 { "high" } else { "low" }))
    
    let ctx = SpanContext::new("trace_" + i.to_string(), "span_" + i.to_string(), true, "")
    let span = Span::name("filter_test_span", Internal, ctx)
    Span::set_attributes(span, attrs)
    spans.push(span)
  }
  
  // Test filtering performance
  let start_time = Time::now()
  
  let filter = SpanFilter::attribute_equals("category", "even")
  let filtered_spans = SpanFilter::filter(filter, spans)
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // Verify filtering result
  assert_eq(filtered_spans.length(), (iterations / 2) + 1)
  
  // Calculate filtering time
  let filtering_time_ms = (duration as Float) * 1000.0
  
  // Verify performance meets expectations
  assert_true(filtering_time_ms < 50.0) // Should filter in less than 50ms
}

// Test 10: End-to-End Performance
test "end to end performance" {
  let iterations = 1000
  let start_time = Time::now()
  
  // End-to-end telemetry flow
  for i in 0..=iterations {
    // 1. Create span
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("e2e_test_span", Internal, span_ctx)
    
    // 2. Add attributes
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    Attributes::set(attrs, "operation", StringValue("test_operation"))
    Span::set_attributes(span, attrs)
    
    // 3. Add events
    Span::add_event(span, "start_event", Some([("event.type", StringValue("system"))]))
    Span::add_event(span, "end_event", Some([("event.type", StringValue("system"))]))
    
    // 4. Record metrics
    let provider = MeterProvider::default()
    let meter = MeterProvider::get_meter(provider, "e2e_meter")
    let counter = Meter::create_counter(meter, "e2e_counter", Some("E2E counter"), Some("count"))
    Counter::add(counter, 1.0, Some(attrs))
    
    // 5. Serialize span
    let json_str = SpanSerializer::to_json(span)
    
    // 6. Deserialize span
    let deserialized_span = SpanDeserializer::from_json(json_str)
    
    // 7. Filter spans
    let filter = SpanFilter::attribute_equals("operation", "test_operation")
    let filtered_spans = SpanFilter::filter(filter, [deserialized_span])
    
    // 8. Aggregate metrics
    let aggregator = MetricAggregator::new()
    let aggregated = MetricAggregator::aggregate_counter(aggregator, counter)
    
    // 9. End span
    Span::end(span)
    
    // Verify results
    assert_eq(filtered_spans.length(), 1)
    match aggregated {
      AggregatedMetric::Sum(value) => assert_eq(value, (i + 1) as Float)
      _ => assert_true(false)
    }
  }
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // Calculate operations per second
  let operations_per_second = (iterations as Float) / (duration as Float)
  
  // Verify performance meets expectations
  assert_true(operations_per_second > 10.0) // Should complete at least 10 end-to-end operations per second
}