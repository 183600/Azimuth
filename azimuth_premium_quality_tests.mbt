// Azimuth Telemetry System - Premium Quality Test Suite
// This file contains high-quality test cases covering critical telemetry functionality

// Test 1: Distributed Tracing Consistency Across Service Boundaries
test "distributed tracing consistency across service boundaries" {
  // Create trace context with proper validation
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "b7ad6b7169203331"
  let parent_span_ctx = SpanContext::new(trace_id, parent_span_id, true, "")
  
  // Service A: Create parent span
  let tracer_provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(tracer_provider_a, "service-a")
  let parent_span = Tracer::start_span(tracer_a, "service-a-operation")
  
  // Validate parent span context
  assert_eq(SpanContext::trace_id(parent_span.span_context()), trace_id)
  assert_eq(SpanContext::span_id(parent_span.span_context()), parent_span_id)
  assert_true(SpanContext::is_sampled(parent_span.span_context()))
  
  // Service B: Create child span with proper parent relationship
  let tracer_provider_b = TracerProvider::default()
  let tracer_b = TracerProvider::get_tracer(tracer_provider_b, "service-b")
  let child_span = Tracer::start_span_with_parent(
    tracer_b, 
    "service-b-operation", 
    Some(parent_span.span_context())
  )
  
  // Validate child span maintains trace consistency
  assert_eq(SpanContext::trace_id(child_span.span_context()), trace_id)
  assert_not_eq(SpanContext::span_id(child_span.span_context()), parent_span_id)
  assert_true(SpanContext::is_sampled(child_span.span_context()))
  
  // Service C: Create grandchild span
  let tracer_provider_c = TracerProvider::default()
  let tracer_c = TracerProvider::get_tracer(tracer_provider_c, "service-c")
  let grandchild_span = Tracer::start_span_with_parent(
    tracer_c, 
    "service-c-operation", 
    Some(child_span.span_context())
  )
  
  // Validate grandchild span maintains trace consistency
  assert_eq(SpanContext::trace_id(grandchild_span.span_context()), trace_id)
  assert_not_eq(SpanContext::span_id(grandchild_span.span_context()), child_span.span_context())
  assert_true(SpanContext::is_sampled(grandchild_span.span_context()))
  
  // Add events to demonstrate operation flow
  parent_span.add_event("service-a.start", Some([("operation", "process-request")]))
  child_span.add_event("service-b.start", Some([("operation", "validate-data")]))
  grandchild_span.add_event("service-c.start", Some([("operation", "execute-business-logic")]))
  
  // Set status for each span
  parent_span.set_status(Ok, Some("Request processed successfully"))
  child_span.set_status(Ok, Some("Data validation completed"))
  grandchild_span.set_status(Ok, Some("Business logic executed"))
  
  // End spans in proper order (grandchild first)
  grandchild_span.end()
  child_span.end()
  parent_span.end()
  
  // Validate all spans are properly ended
  assert_false(parent_span.is_recording())
  assert_false(child_span.is_recording())
  assert_false(grandchild_span.is_recording())
}

// Test 2: Telemetry Data Serialization and Deserialization Integrity
test "telemetry data serialization and deserialization integrity" {
  // Create complex telemetry data structure
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "fedcba0987654321"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Create span with rich attributes
  let span = Span::new("serialization-test", Internal, span_ctx)
  
  // Add diverse attribute types
  let attributes = [
    ("string.attr", StringValue("test-value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("string.array.attr", ArrayStringValue(["a", "b", "c"])),
    ("int.array.attr", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  for (key, value) in attributes {
    span.set_attribute(key, value)
  }
  
  // Add events with timestamps
  let base_time = 1640995200000L
  span.add_event_with_timestamp("event.1", base_time, Some([("event.type", "start")]))
  span.add_event_with_timestamp("event.2", base_time + 1000L, Some([("event.type", "progress")]))
  span.add_event_with_timestamp("event.3", base_time + 2000L, Some([("event.type", "complete")]))
  
  // Set span status
  span.set_status(Ok, Some("Operation completed successfully"))
  
  // Serialize span to JSON-like structure
  let serialized = Span::serialize(span)
  
  // Validate serialized structure contains all expected fields
  assert_true(SerializedData::has_field(serialized, "trace_id"))
  assert_true(SerializedData::has_field(serialized, "span_id"))
  assert_true(SerializedData::has_field(serialized, "name"))
  assert_true(SerializedData::has_field(serialized, "status"))
  assert_true(SerializedData::has_field(serialized, "attributes"))
  assert_true(SerializedData::has_field(serialized, "events"))
  
  // Validate specific field values
  assert_eq(SerializedData::get_string_field(serialized, "trace_id"), trace_id)
  assert_eq(SerializedData::get_string_field(serialized, "span_id"), span_id)
  assert_eq(SerializedData::get_string_field(serialized, "name"), "serialization-test")
  
  // Deserialize back to span object
  let deserialized_span = Span::deserialize(serialized)
  
  // Validate deserialized span matches original
  assert_eq(Span::name(deserialized_span), Span::name(span))
  assert_eq(SpanContext::trace_id(deserialized_span.span_context()), trace_id)
  assert_eq(SpanContext::span_id(deserialized_span.span_context()), span_id)
  assert_eq(Span::status(deserialized_span), Span::status(span))
  
  // Validate attributes are preserved
  let original_string_attr = Span::get_attribute(span, "string.attr")
  let deserialized_string_attr = Span::get_attribute(deserialized_span, "string.attr")
  
  match (original_string_attr, deserialized_string_attr) {
    (Some(StringValue(orig)), Some(StringValue(deser))) => assert_eq(orig, deser)
    _ => assert_true(false)
  }
  
  // Validate events are preserved
  let original_events = Span::events(span)
  let deserialized_events = Span::events(deserialized_span)
  assert_eq(original_events.length(), deserialized_events.length())
  
  // Validate event timestamps
  for i = 0; i < original_events.length(); i = i + 1 {
    assert_eq(
      Event::timestamp(original_events[i]), 
      Event::timestamp(deserialized_events[i])
    )
    assert_eq(
      Event::name(original_events[i]), 
      Event::name(deserialized_events[i])
    )
  }
}

// Test 3: High-Concurrency Resource Management and Cleanup
test "high-concurrency resource management and cleanup" {
  // Create resource pool for telemetry components
  let resource_pool = ResourcePool::new(100) // Max 100 concurrent resources
  
  // Simulate high-concurrency scenario with multiple "threads"
  let concurrent_operations = 50
  let mut operation_results = []
  
  // Create tracer provider for concurrent operations
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrency-test")
  
  // Simulate concurrent span creation and management
  for i = 0; i < concurrent_operations; i = i + 1 {
    // Acquire resource from pool
    let resource = ResourcePool::acquire(resource_pool)
    assert_true(ResourcePool::is_valid(resource))
    
    // Create span with unique name
    let span_name = "concurrent-span-" + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    
    // Simulate work by adding attributes and events
    span.set_attribute("operation.id", IntValue(i))
    span.set_attribute("thread.id", IntValue(i % 10)) // Simulate 10 threads
    span.add_event("operation.start", Some([("timestamp", (1640995200000L + i * 1000L).to_string())]))
    
    // Simulate processing time
    let processing_steps = [10, 20, 30, 40, 50]
    for step in processing_steps {
      span.set_attribute("processing.step", IntValue(step))
      // Simulate some work
      let result = step * i
      assert_true(result >= 0)
    }
    
    span.add_event("operation.complete", Some([("result", (i * 100).to_string())]))
    span.set_status(Ok, Some("Operation completed successfully"))
    
    // Record result
    operation_results.push((i, Span::name(span), SpanContext::span_id(span.span_context())))
    
    // End span
    span.end()
    
    // Release resource back to pool
    ResourcePool::release(resource_pool, resource)
  }
  
  // Validate all operations completed successfully
  assert_eq(operation_results.length(), concurrent_operations)
  
  // Validate all spans have unique names and IDs
  let mut span_names = []
  let mut span_ids = []
  
  for (_, name, id) in operation_results {
    span_names.push(name)
    span_ids.push(id)
  }
  
  // All span names should be unique
  for i = 0; i < span_names.length(); i = i + 1 {
    for j = i + 1; j < span_names.length(); j = j + 1 {
      assert_not_eq(span_names[i], span_names[j])
    }
  }
  
  // All span IDs should be unique
  for i = 0; i < span_ids.length(); i = i + 1 {
    for j = i + 1; j < span_ids.length(); j = j + 1 {
      assert_not_eq(span_ids[i], span_ids[j])
    }
  }
  
  // Validate resource pool integrity
  assert_eq(ResourcePool::available_count(resource_pool), 100)
  assert_eq(ResourcePool::acquired_count(resource_pool), 0)
  
  // Test resource cleanup
  ResourcePool::cleanup(resource_pool)
  assert_true(ResourcePool::is_clean(resource_pool))
}

// Test 4: Cross-Service Context Propagation with Complex Scenarios
test "cross-service context propagation with complex scenarios" {
  // Create composite propagator with multiple propagation formats
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let custom_propagator = CustomPropagator::new("x-custom-trace")
  
  let composite_propagator = CompositePropagator::new([
    trace_propagator,
    baggage_propagator,
    custom_propagator
  ])
  
  // Create source context with rich information
  let trace_id = "abcdef1234567890abcdef1234567890"
  let span_id = "0987654321fedcba"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "rojo=00f067aa0ba902b7")
  
  // Create baggage with complex values
  let baggage_items = [
    ("user.id", "user-12345"),
    ("session.id", "session-abcdef"),
    ("request.id", "req-123456789"),
    ("tenant.id", "tenant-001"),
    ("correlation.id", "corr-xyz789")
  ]
  
  let mut source_context = Context::with_value(
    Context::root(),
    ContextKey::new("trace-context"),
    span_ctx
  )
  
  // Add baggage items to context
  for (key, value) in baggage_items {
    source_context = Context::with_value(
      source_context,
      ContextKey::new("baggage." + key),
      value
    )
  }
  
  // Add custom context data
  source_context = Context::with_value(
    source_context,
    ContextKey::new("custom-data"),
    "custom-value-12345"
  )
  
  // Inject context into carrier
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, source_context, carrier)
  
  // Validate injected headers
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  match traceparent {
    Some(value) => {
      // Validate traceparent format: version-trace_id-span_id-flags
      let parts = value.split("-")
      assert_eq(parts.length(), 4)
      assert_eq(parts[0], "00") // Version
      assert_eq(parts[1], trace_id) // Trace ID
      assert_eq(parts[2], span_id) // Span ID
      assert_eq(parts[3], "01") // Flags (sampled)
    }
    None => assert_true(false) // Should have traceparent header
  }
  
  let baggage = TextMapCarrier::get(carrier, "baggage")
  match baggage {
    Some(value) => {
      // Validate baggage contains all items
      assert_true(value.contains("user.id=user-12345"))
      assert_true(value.contains("session.id=session-abcdef"))
      assert_true(value.contains("request.id=req-123456789"))
      assert_true(value.contains("tenant.id=tenant-001"))
      assert_true(value.contains("correlation.id=corr-xyz789"))
    }
    None => assert_true(false) // Should have baggage header
  }
  
  let custom_header = TextMapCarrier::get(carrier, "x-custom-trace")
  match custom_header {
    Some(value) => assert_eq(value, "custom-value-12345")
    None => assert_true(false) // Should have custom header
  }
  
  // Extract context from carrier
  let extracted_context = CompositePropagator::extract(composite_propagator, carrier)
  
  // Validate extracted trace context
  let extracted_span_ctx = Context::get(extracted_context, ContextKey::new("trace-context"))
  match extracted_span_ctx {
    Some(ctx) => {
      assert_eq(SpanContext::trace_id(ctx), trace_id)
      assert_eq(SpanContext::span_id(ctx), span_id)
      assert_true(SpanContext::is_sampled(ctx))
    }
    None => assert_true(false) // Should have extracted span context
  }
  
  // Validate extracted baggage items
  for (key, expected_value) in baggage_items {
    let extracted_value = Context::get(extracted_context, ContextKey::new("baggage." + key))
    match extracted_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false) // Should have extracted baggage item
    }
  }
  
  // Validate extracted custom data
  let extracted_custom = Context::get(extracted_context, ContextKey::new("custom-data"))
  match extracted_custom {
    Some(value) => assert_eq(value, "custom-value-12345")
    None => assert_true(false) // Should have extracted custom data
  }
}

// Test 5: Advanced Telemetry Data Aggregation and Analytics
test "advanced telemetry data aggregation and analytics" {
  // Create meter provider for metrics collection
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "analytics-test")
  
  // Create various metric instruments
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let response_time_histogram = Meter::create_histogram(meter, "http.response.time")
  let active_connections_gauge = Meter::create_gauge(meter, "http.connections.active")
  let queue_size_updown = Meter::create_updown_counter(meter, "http.queue.size")
  
  // Simulate metric collection over time
  let time_series_data = [
    // (timestamp, request_count, avg_response_time, active_connections, queue_size)
    (1640995200000L, 100, 50.5, 10, 5),
    (1640995260000L, 150, 75.2, 15, 8),
    (1640995320000L, 120, 60.8, 12, 6),
    (1640995380000L, 200, 95.3, 20, 12),
    (1640995440000L, 180, 85.7, 18, 10),
    (1640995500000L, 160, 70.4, 16, 9),
    (1640995560000L, 140, 65.1, 14, 7),
    (1640995620000L, 170, 80.9, 17, 11)
  ]
  
  // Record metrics for each time point
  for (timestamp, request_count, avg_response_time, active_connections, queue_size) in time_series_data {
    // Record request count
    Counter::add(request_counter, request_count.to_float())
    
    // Record response time distribution
    for i = 0; i < request_count; i = i + 1 {
      // Simulate individual response times around the average
      let variation = (i % 20 - 10).to_float() // -10 to +10 variation
      let response_time = avg_response_time + variation
      Histogram::record(response_time_histogram, response_time)
    }
    
    // Record active connections
    // In a real implementation, this would set the gauge value
    // For testing, we validate the value
    assert_true(active_connections >= 0)
    
    // Record queue size changes
    if queue_size > 5 {
      UpDownCounter::add(queue_size_updown, (queue_size - 5).to_float())
    } else {
      UpDownCounter::add(queue_size_updown, (queue_size - 5).to_float())
    }
  }
  
  // Calculate analytics from collected data
  let total_requests = time_series_data.reduce(|acc, (_, count, _, _, _)| acc + count, 0)
  let avg_response_time = time_series_data.reduce(|acc, (_, _, avg, _, _)| acc + avg, 0.0) / time_series_data.length().to_float()
  let max_active_connections = time_series_data.reduce(|acc, (_, _, _, active, _)| if active > acc { active } else { acc }, 0)
  let avg_queue_size = time_series_data.reduce(|acc, (_, _, _, _, queue)| acc + queue, 0) / time_series_data.length()
  
  // Validate calculated analytics
  assert_eq(total_requests, 1220)
  assert_true(avg_response_time > 70.0 && avg_response_time < 80.0)
  assert_eq(max_active_connections, 20)
  assert_eq(avg_queue_size, 8.5)
  
  // Test percentile calculations for response times
  let all_response_times = []
  for (_, count, avg, _, _) in time_series_data {
    for i = 0; i < count; i = i + 1 {
      let variation = (i % 20 - 10).to_float()
      all_response_times.push(avg + variation)
    }
  }
  
  // Sort response times for percentile calculation
  all_response_times.sort()
  
  // Calculate percentiles
  let p50_index = all_response_times.length() / 2
  let p95_index = (all_response_times.length() * 95) / 100
  let p99_index = (all_response_times.length() * 99) / 100
  
  let p50_response_time = all_response_times[p50_index]
  let p95_response_time = all_response_times[p95_index]
  let p99_response_time = all_response_times[p99_index]
  
  // Validate percentile relationships
  assert_true(p50_response_time <= p95_response_time)
  assert_true(p95_response_time <= p99_response_time)
  assert_true(p50_response_time > 40.0 && p50_response_time < 90.0)
  assert_true(p95_response_time > 80.0 && p95_response_time < 110.0)
  assert_true(p99_response_time > 85.0 && p99_response_time < 120.0)
  
  // Test rate calculations
  let time_window = 60000L * 7 // 7 minutes in milliseconds
  let requests_per_second = total_requests.to_float() / (time_window / 1000L).to_float()
  assert_true(requests_per_second > 2.5 && requests_per_second < 3.5)
}

// Test 6: Error Recovery and Fault Tolerance Mechanisms
test "error recovery and fault tolerance mechanisms" {
  // Create fault-injection enabled tracer
  let tracer_provider = TracerProvider::with_fault_injection(0.2) // 20% fault injection rate
  let tracer = TracerProvider::get_tracer(tracer_provider, "fault-tolerance-test")
  
  // Test span creation with fault injection
  let mut successful_spans = []
  let mut failed_spans = []
  
  for i = 0; i < 100; i = i + 1 {
    match Tracer::start_span_safe(tracer, "fault-test-span-" + i.to_string()) {
      Ok(span) => successful_spans.push(span),
      Error(error) => failed_spans.push(error)
    }
  }
  
  // With 20% fault injection, we should have some successful and some failed spans
  assert_true(successful_spans.length() > 60) // At least 60% success
  assert_true(failed_spans.length() > 10)    // At least 10% failure
  assert_eq(successful_spans.length() + failed_spans.length(), 100)
  
  // Test error recovery in span operations
  for span in successful_spans {
    // Test attribute setting with error handling
    match Span::set_attribute_safe(span, "test.attr", StringValue("test-value")) {
      Ok(_) => assert_true(true), // Success
      Error(_) => {
        // On error, span should still be usable
        assert_true(Span::is_recording(span))
      }
    }
    
    // Test event addition with error handling
    match Span::add_event_safe(span, "test.event", Some([("event.data", "test")])) {
      Ok(_) => assert_true(true), // Success
      Error(_) => {
        // On error, span should still be usable
        assert_true(Span::is_recording(span))
      }
    }
    
    // Test span ending with error handling
    match Span::end_safe(span) {
      Ok(_) => assert_true(true), // Success
      Error(_) => {
        // On error, span should still be ended
        assert_false(Span::is_recording(span))
      }
    }
  }
  
  // Test circuit breaker pattern for telemetry operations
  let circuit_breaker = CircuitBreaker::new(5, 10000L) // 5 failures, 10s timeout
  
  // Test circuit breaker with failing operations
  for i = 0; i < 10; i = i + 1 {
    match CircuitBreaker::execute(circuit_breaker, || {
      // Simulate operation that fails 50% of the time
      if i % 2 == 0 {
        Error("Simulated failure")
      } else {
        Ok("Success")
      }
    }) {
      Ok(_) => assert_true(true),
      Error(_) => assert_true(true)
    }
  }
  
  // Circuit breaker should be open after sufficient failures
  assert_true(CircuitBreaker::is_open(circuit_breaker))
  
  // Test retry mechanism with exponential backoff
  let retry_policy = RetryPolicy::exponential_backoff(3, 100L, 1000L) // 3 retries, 100ms to 1s
  let mut retry_attempts = 0
  
  match RetryPolicy::execute(retry_policy, || {
    retry_attempts = retry_attempts + 1
    if retry_attempts < 3 {
      Error("Temporary failure")
    } else {
      Ok("Success after retries")
    }
  }) {
    Ok(result) => {
      assert_eq(result, "Success after retries")
      assert_eq(retry_attempts, 3)
    }
    Error(_) => assert_true(false) // Should succeed after retries
  }
  
  // Test graceful degradation when telemetry system is overloaded
  let telemetry_health = TelemetryHealthMonitor::new()
  
  // Simulate system overload
  TelemetryHealthMonitor::set_load_level(telemetry_health, 0.9) // 90% load
  
  // Test adaptive sampling under load
  let adaptive_sampler = AdaptiveSampler::new()
  let sampling_decision = AdaptiveSampler::should_sample(adaptive_sampler, telemetry_health)
  
  // Under high load, sampling should be reduced
  assert_true(sampling_decision.sampled_rate < 0.8)
  
  // Test recovery when load reduces
  TelemetryHealthMonitor::set_load_level(telemetry_health, 0.3) // 30% load
  let recovery_sampling_decision = AdaptiveSampler::should_sample(adaptive_sampler, telemetry_health)
  
  // Under normal load, sampling should be normal
  assert_true(recovery_sampling_decision.sampled_rate > 0.8)
}

// Test 7: Performance Benchmarking and Resource Optimization
test "performance benchmarking and resource optimization" {
  // Create performance benchmark suite
  let benchmark_suite = PerformanceBenchmark::new()
  
  // Benchmark span creation performance
  let span_creation_time = PerformanceBenchmark::measure(benchmark_suite, || {
    let tracer_provider = TracerProvider::default()
    let tracer = TracerProvider::get_tracer(tracer_provider, "benchmark-test")
    
    for i = 0; i < 1000; i = i + 1 {
      let span = Tracer::start_span(tracer, "benchmark-span-" + i.to_string())
      span.end()
    }
  })
  
  // Span creation should be reasonably fast
  assert_true(span_creation_time < 1000000000L) // Less than 1 second for 1000 spans
  
  // Benchmark attribute setting performance
  let attribute_time = PerformanceBenchmark::measure(benchmark_suite, || {
    let tracer_provider = TracerProvider::default()
    let tracer = TracerProvider::get_tracer(tracer_provider, "benchmark-test")
    let span = Tracer::start_span(tracer, "attribute-benchmark")
    
    for i = 0; i < 100; i = i + 1 {
      span.set_attribute("attr." + i.to_string(), StringValue("value-" + i.to_string()))
    }
    
    span.end()
  })
  
  // Attribute setting should be fast
  assert_true(attribute_time < 100000000L) // Less than 100ms for 100 attributes
  
  // Benchmark event addition performance
  let event_time = PerformanceBenchmark::measure(benchmark_suite, || {
    let tracer_provider = TracerProvider::default()
    let tracer = TracerProvider::get_tracer(tracer_provider, "benchmark-test")
    let span = Tracer::start_span(tracer, "event-benchmark")
    
    for i = 0; i < 50; i = i + 1 {
      span.add_event("event-" + i.to_string(), Some([("event.id", i.to_string())]))
    }
    
    span.end()
  })
  
  // Event addition should be fast
  assert_true(event_time < 50000000L) // Less than 50ms for 50 events
  
  // Benchmark serialization performance
  let serialization_time = PerformanceBenchmark::measure(benchmark_suite, || {
    let tracer_provider = TracerProvider::default()
    let tracer = TracerProvider::get_tracer(tracer_provider, "benchmark-test")
    let span = Tracer::start_span(tracer, "serialization-benchmark")
    
    // Add rich data
    span.set_attribute("string.attr", StringValue("test-value"))
    span.set_attribute("int.attr", IntValue(42))
    span.set_attribute("float.attr", FloatValue(3.14159))
    span.set_attribute("bool.attr", BoolValue(true))
    
    for i = 0; i < 10; i = i + 1 {
      span.add_event("event-" + i.to_string(), Some([("event.data", "test-" + i.to_string())]))
    }
    
    // Serialize span
    let serialized = Span::serialize(span)
    span.end()
    
    serialized
  })
  
  // Serialization should be fast
  assert_true(serialization_time < 10000000L) // Less than 10ms for complex span
  
  // Test memory usage optimization
  let memory_tracker = MemoryTracker::new()
  
  // Measure memory usage with many spans
  MemoryTracker::start_tracking(memory_tracker)
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "memory-test")
  let spans = []
  
  for i = 0; i < 100; i = i + 1 {
    let span = Tracer::start_span(tracer, "memory-test-span-" + i.to_string())
    span.set_attribute("test.attr", StringValue("test-value"))
    spans.push(span)
  }
  
  let memory_usage = MemoryTracker::get_current_usage(memory_tracker)
  
  // End all spans and check memory cleanup
  for span in spans {
    span.end()
  }
  
  // Force garbage collection if available
  MemoryTracker::force_gc(memory_tracker)
  
  let memory_after_cleanup = MemoryTracker::get_current_usage(memory_tracker)
  
  // Memory should be released after span cleanup
  assert_true(memory_after_cleanup < memory_usage)
  
  // Test resource pool efficiency
  let resource_pool = ResourcePool::new(50)
  let resource_acquire_time = PerformanceBenchmark::measure(benchmark_suite, || {
    for i = 0; i < 1000; i = i + 1 {
      let resource = ResourcePool::acquire(resource_pool)
      ResourcePool::release(resource_pool, resource)
    }
  })
  
  // Resource pool operations should be very fast
  assert_true(resource_acquire_time < 50000000L) // Less than 50ms for 1000 acquire/release cycles
}

// Test 8: Memory Management and Resource Lifecycle
test "memory management and resource lifecycle" {
  // Create memory-managed telemetry components
  let memory_manager = MemoryManager::new()
  
  // Test span lifecycle with proper cleanup
  let span_lifecycle_test = || {
    let tracer_provider = TracerProvider::with_memory_manager(memory_manager)
    let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle-test")
    
    // Create spans with different lifetimes
    let short_lived_span = Tracer::start_span(tracer, "short-lived")
    short_lived_span.set_attribute("lifecycle", "short")
    short_lived_span.end()
    
    let medium_lived_span = Tracer::start_span(tracer, "medium-lived")
    medium_lived_span.set_attribute("lifecycle", "medium")
    
    // Create nested spans
    let child_span = Tracer::start_span_with_parent(
      tracer, 
      "child-span", 
      Some(medium_lived_span.span_context())
    )
    child_span.set_attribute("lifecycle", "child")
    child_span.end()
    
    medium_lived_span.end()
    
    // Create long-lived span
    let long_lived_span = Tracer::start_span(tracer, "long-lived")
    long_lived_span.set_attribute("lifecycle", "long")
    
    // Add many attributes to test memory usage
    for i = 0; i < 100; i = i + 1 {
      long_lived_span.set_attribute("attr." + i.to_string(), StringValue("value-" + i.to_string()))
    }
    
    // Add many events to test memory usage
    for i = 0; i < 50; i = i + 1 {
      long_lived_span.add_event("event-" + i.to_string(), Some([("event.id", i.to_string())]))
    }
    
    long_lived_span.end()
  }
  
  // Measure memory before and after lifecycle test
  let memory_before = MemoryManager::get_allocated_memory(memory_manager)
  span_lifecycle_test()
  let memory_after = MemoryManager::get_allocated_memory(memory_manager)
  
  // Force garbage collection
  MemoryManager::force_gc(memory_manager)
  let memory_after_gc = MemoryManager::get_allocated_memory(memory_manager)
  
  // Memory should be properly managed
  assert_true(memory_after_gc <= memory_after)
  
  // Test resource leak detection
  let leak_detector = ResourceLeakDetector::new()
  LeakDetector::start_monitoring(leak_detector)
  
  // Create resources with intentional leaks for testing
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "leak-test")
  
  // Create spans without ending them (simulating leaks)
  let leaked_spans = []
  for i = 0; i < 10; i = i + 1 {
    let span = Tracer::start_span(tracer, "leaked-span-" + i.to_string())
    span.set_attribute("leaked", "true")
    leaked_spans.push(span)
    // Intentionally not ending these spans
  }
  
  // Check for detected leaks
  let detected_leaks = LeakDetector::detect_leaks(leak_detector)
  assert_true(detected_leaks.length() >= 10)
  
  // Clean up leaked spans
  for span in leaked_spans {
    span.end()
  }
  
  // Verify leaks are resolved
  let resolved_leaks = LeakDetector::detect_leaks(leak_detector)
  assert_eq(resolved_leaks.length(), 0)
  
  // Test memory pool allocation efficiency
  let memory_pool = MemoryPool::new(1024, 100) // 1KB blocks, max 100 blocks
  
  let allocations = []
  
  // Allocate many small blocks
  for i = 0; i < 50; i = i + 1 {
    let block = MemoryPool::allocate(memory_pool, 64) // 64 bytes
    assert_true(MemoryPool::is_valid_block(block))
    allocations.push(block)
  }
  
  // Verify pool statistics
  assert_eq(MemoryPool::allocated_count(memory_pool), 50)
  assert_true(MemoryPool::available_count(memory_pool) < 50)
  
  // Free all allocated blocks
  for block in allocations {
    MemoryPool::deallocate(memory_pool, block)
  }
  
  // Verify all blocks are returned to pool
  assert_eq(MemoryPool::allocated_count(memory_pool), 0)
  assert_eq(MemoryPool::available_count(memory_pool), 100)
  
  // Test large allocation handling
  let large_block = MemoryPool::allocate(memory_pool, 2048) // 2KB (larger than block size)
  assert_true(MemoryPool::is_valid_block(large_block))
  
  MemoryPool::deallocate(memory_pool, large_block)
  
  // Test memory pressure handling
  let pressure_test_blocks = []
  
  // Allocate until pool is exhausted
  for i = 0; i < 150; i = i + 1 {
    match MemoryPool::try_allocate(memory_pool, 64) {
      Some(block) => pressure_test_blocks.push(block),
      None => {
        // Pool should be exhausted at this point
        assert_true(i >= 100)
        break
      }
    }
  }
  
  // Clean up pressure test blocks
  for block in pressure_test_blocks {
    MemoryPool::deallocate(memory_pool, block)
  }
  
  // Verify pool is fully recovered
  assert_eq(MemoryPool::available_count(memory_pool), 100)
}

// Test 9: Multi-Tenant Isolation and Security
test "multi-tenant isolation and security" {
  // Create multi-tenant telemetry system
  let tenant_manager = TenantManager::new()
  
  // Create tenants with different isolation levels
  let tenant_a = TenantManager::create_tenant(tenant_manager, "tenant-a", "high")
  let tenant_b = TenantManager::create_tenant(tenant_manager, "tenant-b", "medium")
  let tenant_c = TenantManager::create_tenant(tenant_manager, "tenant-c", "low")
  
  // Validate tenant creation
  assert_true(Tenant::is_valid(tenant_a))
  assert_true(Tenant::is_valid(tenant_b))
  assert_true(Tenant::is_valid(tenant_c))
  assert_eq(Tenant::id(tenant_a), "tenant-a")
  assert_eq(Tenant::isolation_level(tenant_a), "high")
  
  // Create isolated tracer providers for each tenant
  let tracer_provider_a = TracerProvider::with_tenant(tenant_a)
  let tracer_provider_b = TracerProvider::with_tenant(tenant_b)
  let tracer_provider_c = TracerProvider::with_tenant(tenant_c)
  
  let tracer_a = TracerProvider::get_tracer(tracer_provider_a, "service-a")
  let tracer_b = TracerProvider::get_tracer(tracer_provider_b, "service-b")
  let tracer_c = TracerProvider::get_tracer(tracer_provider_c, "service-c")
  
  // Create spans for each tenant
  let span_a = Tracer::start_span(tracer_a, "tenant-a-operation")
  let span_b = Tracer::start_span(tracer_b, "tenant-b-operation")
  let span_c = Tracer::start_span(tracer_c, "tenant-c-operation")
  
  // Add tenant-specific attributes
  span_a.set_attribute("tenant.id", StringValue("tenant-a"))
  span_a.set_attribute("user.id", StringValue("user-a-123"))
  span_a.set_attribute("sensitive.data", StringValue("secret-a"))
  
  span_b.set_attribute("tenant.id", StringValue("tenant-b"))
  span_b.set_attribute("user.id", StringValue("user-b-456"))
  span_b.set_attribute("sensitive.data", StringValue("secret-b"))
  
  span_c.set_attribute("tenant.id", StringValue("tenant-c"))
  span_c.set_attribute("user.id", StringValue("user-c-789"))
  span_c.set_attribute("sensitive.data", StringValue("secret-c"))
  
  // Test tenant isolation - spans should not be accessible across tenants
  let tenant_a_spans = TenantManager::get_spans(tenant_manager, tenant_a)
  let tenant_b_spans = TenantManager::get_spans(tenant_manager, tenant_b)
  let tenant_c_spans = TenantManager::get_spans(tenant_manager, tenant_c)
  
  assert_eq(tenant_a_spans.length(), 1)
  assert_eq(tenant_b_spans.length(), 1)
  assert_eq(tenant_c_spans.length(), 1)
  
  // Verify span isolation
  assert_eq(Span::name(tenant_a_spans[0]), "tenant-a-operation")
  assert_eq(Span::name(tenant_b_spans[0]), "tenant-b-operation")
  assert_eq(Span::name(tenant_c_spans[0]), "tenant-c-operation")
  
  // Test cross-tenant data access prevention
  let cross_tenant_access = || {
    // Try to access tenant B's spans from tenant A context
    TenantManager::get_spans_with_context(tenant_manager, tenant_a, tenant_b)
  }
  
  match cross_tenant_access() {
    Ok(_) => assert_true(false), // Should not succeed
    Error(access_error) => assert_true(access_error.contains("access denied"))
  }
  
  // Test data encryption for high-security tenants
  if Tenant::isolation_level(tenant_a) == "high" {
    let encrypted_data = Tenant::encrypt_data(tenant_a, "sensitive information")
    assert_not_eq(encrypted_data, "sensitive information")
    
    let decrypted_data = Tenant::decrypt_data(tenant_a, encrypted_data)
    assert_eq(decrypted_data, "sensitive information")
  }
  
  // Test resource quotas per tenant
  let quota_manager = QuotaManager::new()
  
  // Set quotas for each tenant
  QuotaManager::set_span_quota(quota_manager, tenant_a, 100)
  QuotaManager::set_span_quota(quota_manager, tenant_b, 50)
  QuotaManager::set_span_quota(quota_manager, tenant_c, 25)
  
  // Test quota enforcement
  let mut tenant_a_span_count = 0
  for i = 0; i < 120; i = i + 1 {
    match Tracer::start_span_with_quota_check(tracer_a, "quota-test-" + i.to_string()) {
      Ok(_) => tenant_a_span_count = tenant_a_span_count + 1,
      Error(quota_error) => {
        assert_true(quota_error.contains("quota exceeded"))
        break
      }
    }
  }
  
  // Tenant A should not exceed its quota of 100 spans
  assert_true(tenant_a_span_count <= 100)
  
  // Test audit logging for security events
  let audit_logger = AuditLogger::new()
  
  // Log security events
  AuditLogger::log_access_attempt(audit_logger, tenant_a, "read", "tenant-b-data", false)
  AuditLogger::log_access_attempt(audit_logger, tenant_b, "read", "tenant-b-data", true)
  AuditLogger::log_access_attempt(audit_logger, tenant_c, "write", "tenant-c-data", true)
  
  // Verify audit log entries
  let audit_entries = AuditLogger::get_entries(audit_logger, tenant_a)
  assert_eq(audit_entries.length(), 1)
  assert_eq(AuditEntry::operation(audit_entries[0]), "read")
  assert_eq(AuditEntry::resource(audit_entries[0]), "tenant-b-data")
  assert_false(AuditEntry::success(audit_entries[0]))
  
  // End spans
  span_a.end()
  span_b.end()
  span_c.end()
  
  // Test tenant cleanup
  TenantManager::cleanup_tenant(tenant_manager, tenant_c)
  assert_false(Tenant::is_active(tenant_c))
  assert_eq(TenantManager::get_spans(tenant_manager, tenant_c).length(), 0)
}

// Test 10: Internationalization and Localization Support
test "internationalization and localization support" {
  // Create internationalization manager
  let i18n_manager = I18nManager::new()
  
  // Add supported locales
  I18nManager::add_locale(i18n_manager, "en-US")
  I18nManager::add_locale(i18n_manager, "zh-CN")
  I18nManager::add_locale(i18n_manager, "es-ES")
  I18nManager::add_locale(i18n_manager, "ja-JP")
  I18nManager::add_locale(i18n_manager, "fr-FR")
  
  // Add localized messages
  I18nManager::add_message(i18n_manager, "en-US", "span.created", "Span created successfully")
  I18nManager::add_message(i18n_manager, "zh-CN", "span.created", "Span创建成功")
  I18nManager::add_message(i18n_manager, "es-ES", "span.created", "Span creado exitosamente")
  I18nManager::add_message(i18n_manager, "ja-JP", "span.created", "Spanが正常に作成されました")
  I18nManager::add_message(i18n_manager, "fr-FR", "span.created", "Span créé avec succès")
  
  I18nManager::add_message(i18n_manager, "en-US", "error.connection", "Connection failed")
  I18nManager::add_message(i18n_manager, "zh-CN", "error.connection", "连接失败")
  I18nManager::add_message(i18n_manager, "es-ES", "error.connection", "Conexión fallida")
  I18nManager::add_message(i18n_manager, "ja-JP", "error.connection", "接続に失敗しました")
  I18nManager::add_message(i18n_manager, "fr-FR", "error.connection", "Échec de connexion")
  
  // Test message localization
  let en_message = I18nManager::get_message(i18n_manager, "en-US", "span.created")
  let zh_message = I18nManager::get_message(i18n_manager, "zh-CN", "span.created")
  let es_message = I18nManager::get_message(i18n_manager, "es-ES", "span.created")
  let ja_message = I18nManager::get_message(i18n_manager, "ja-JP", "span.created")
  let fr_message = I18nManager::get_message(i18n_manager, "fr-FR", "span.created")
  
  assert_eq(en_message, "Span created successfully")
  assert_eq(zh_message, "Span创建成功")
  assert_eq(es_message, "Span creado exitosamente")
  assert_eq(ja_message, "Spanが正常に作成されました")
  assert_eq(fr_message, "Span créé avec succès")
  
  // Test locale fallback mechanism
  let fallback_message = I18nManager::get_message_with_fallback(
    i18n_manager, 
    "de-DE", // German not supported
    "span.created", 
    "en-US" // Fallback to English
  )
  assert_eq(fallback_message, "Span created successfully")
  
  // Test localized telemetry components
  let localized_tracer_provider = TracerProvider::with_localization(i18n_manager)
  let tracer_en = TracerProvider::get_tracer_with_locale(localized_tracer_provider, "localized-test", "en-US")
  let tracer_zh = TracerProvider::get_tracer_with_locale(localized_tracer_provider, "localized-test", "zh-CN")
  
  // Create localized spans
  let span_en = Tracer::start_span(tracer_en, "localized-operation")
  let span_zh = Tracer::start_span(tracer_zh, "localized-operation")
  
  // Add localized events
  span_en.add_event("operation.start", Some([("message", I18nManager::get_message(i18n_manager, "en-US", "span.created"))]))
  span_zh.add_event("operation.start", Some([("message", I18nManager::get_message(i18n_manager, "zh-CN", "span.created"))]))
  
  // Test localized error messages
  span_en.set_status(Error, Some(I18nManager::get_message(i18n_manager, "en-US", "error.connection")))
  span_zh.set_status(Error, Some(I18nManager::get_message(i18n_manager, "zh-CN", "error.connection")))
  
  // Verify localized events
  let en_events = Span::events(span_en)
  let zh_events = Span::events(span_zh)
  
  assert_eq(en_events.length(), 1)
  assert_eq(zh_events.length(), 1)
  
  let en_event_message = Event::get_attribute(en_events[0], "message")
  let zh_event_message = Event::get_attribute(zh_events[0], "message")
  
  match (en_event_message, zh_event_message) {
    (Some(StringValue(en_msg)), Some(StringValue(zh_msg))) => {
      assert_eq(en_msg, "Span created successfully")
      assert_eq(zh_msg, "Span创建成功")
    }
    _ => assert_true(false)
  }
  
  // Test localized metrics
  let localized_meter_provider = MeterProvider::with_localization(i18n_manager)
  let meter_en = MeterProvider::get_meter_with_locale(localized_meter_provider, "localized-test", "en-US")
  let meter_zh = MeterProvider::get_meter_with_locale(localized_meter_provider, "localized-test", "zh-CN")
  
  let counter_en = Meter::create_counter_with_description(
    meter_en, 
    "localized.counter", 
    Some(I18nManager::get_message(i18n_manager, "en-US", "span.created")),
    Some("counts")
  )
  
  let counter_zh = Meter::create_counter_with_description(
    meter_zh, 
    "localized.counter", 
    Some(I18nManager::get_message(i18n_manager, "zh-CN", "span.created")),
    Some("次数")
  )
  
  // Verify localized descriptions
  assert_eq(Metric::description(counter_en), "Span created successfully")
  assert_eq(Metric::description(counter_zh), "Span创建成功")
  
  // Test localized logging
  let localized_logger_provider = LoggerProvider::with_localization(i18n_manager)
  let logger_en = LoggerProvider::get_logger_with_locale(localized_logger_provider, "localized-test", "en-US")
  let logger_zh = LoggerProvider::get_logger_with_locale(localized_logger_provider, "localized-test", "zh-CN")
  
  let log_en = LogRecord::new(Info, I18nManager::get_message(i18n_manager, "en-US", "span.created"))
  let log_zh = LogRecord::new(Info, I18nManager::get_message(i18n_manager, "zh-CN", "span.created"))
  
  // Verify localized log messages
  match (LogRecord::body(log_en), LogRecord::body(log_zh)) {
    (Some(en_msg), Some(zh_msg)) => {
      assert_eq(en_msg, "Span created successfully")
      assert_eq(zh_msg, "Span创建成功")
    }
    _ => assert_true(false)
  }
  
  // Test right-to-left language support
  I18nManager::add_locale(i18n_manager, "ar-SA")
  I18nManager::add_message(i18n_manager, "ar-SA", "span.created", "تم إنشاء Span بنجاح")
  
  let ar_message = I18nManager::get_message(i18n_manager, "ar-SA", "span.created")
  assert_eq(ar_message, "تم إنشاء Span بنجاح")
  
  // Test bidirectional text handling
  let mixed_text = "English " + ar_message + " 中文"
  assert_true(mixed_text.contains("English"))
  assert_true(mixed_text.contains("تم إنشاء Span بنجاح"))
  assert_true(mixed_text.contains("中文"))
  
  // End spans
  span_en.end()
  span_zh.end()
}