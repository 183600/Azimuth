// Telemetry Performance Tests for Azimuth Telemetry System
// This file contains test cases for telemetry performance benchmarks

// Test 1: High-Throughput Span Creation and Processing
test "high-throughput span creation and processing" {
  let processor = SpanProcessor::new(BatchSpanProcessor::new(1000, 5000)) // 1000 batch size, 5s timeout
  
  // Measure span creation performance
  let start_time = Time::now()
  
  let spans = []
  for i in 1..=10000 {
    let span_ctx = SpanContext::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      true,
      "test_state"
    )
    
    let span = Span::new("high_throughput_test", Server, span_ctx)
    
    // Add some attributes
    Span::set_attribute(span, "iteration", IntValue(i))
    Span::set_attribute(span, "service", StringValue("performance_test"))
    Span::set_attribute(span, "operation", StringValue("benchmark"))
    
    // Add events
    if i % 100 == 0 {
      Span::add_event(span, "milestone_reached", Some([
        ("milestone", StringValue((i / 100).to_string()))
      ]))
    }
    
    spans.push(span)
  }
  
  let creation_time = Time::now()
  let creation_duration = Time::duration_between(start_time, creation_time)
  
  // Should create 10000 spans in reasonable time (less than 1 second)
  assert_true(creation_duration < 1000L)
  
  // Measure span processing performance
  let processing_start = Time::now()
  
  for span in spans {
    SpanProcessor::on_start(processor, span)
    Span::end(span)
    SpanProcessor::on_end(processor, span)
  }
  
  let processing_end = Time::now()
  let processing_duration = Time::duration_between(processing_start, processing_end)
  
  // Should process 10000 spans in reasonable time (less than 2 seconds)
  assert_true(processing_duration < 2000L)
  
  // Force flush and measure
  let flush_start = Time::now()
  SpanProcessor::force_flush(processor)
  let flush_end = Time::now()
  let flush_duration = Time::duration_between(flush_start, flush_end)
  
  // Flush should complete quickly
  assert_true(flush_duration < 100L)
  
  // Calculate performance metrics
  let spans_per_second = (10000.0 * 1000.0) / (creation_duration as Float)
  let processing_spans_per_second = (10000.0 * 1000.0) / (processing_duration as Float)
  
  // Should achieve reasonable throughput
  assert_true(spans_per_second > 10000.0) // At least 10k spans/second
  assert_true(processing_spans_per_second > 5000.0) // At least 5k spans/second processing
}

// Test 2: Metric Collection Performance
test "metric collection performance" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance_test")
  
  // Create various metric instruments
  let counter = Meter::create_counter(meter, "performance_counter", Some("Performance counter"), Some("operations"))
  let histogram = Meter::create_histogram(meter, "performance_histogram", Some("Performance histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "performance_updown", Some("Performance updown counter"), Some("value"))
  let gauge = Meter::create_gauge(meter, "performance_gauge", Some("Performance gauge"), Some("value"))
  
  // Measure metric recording performance
  let start_time = Time::now()
  
  for i in 1..=50000 {
    // Record counter measurement
    Counter::add(counter, 1.0)
    
    // Record histogram measurement
    Histogram::record(histogram, (i % 1000) as Float)
    
    // Record updown counter measurement
    if i % 2 == 0 {
      UpDownCounter::add(updown_counter, 1.0)
    } else {
      UpDownCounter::add(updown_counter, -1.0)
    }
    
    // Record gauge measurement
    Gauge::record(gauge, (i % 100) as Float)
    
    // Every 1000 iterations, record with attributes
    if i % 1000 == 0 {
      let attrs = Attributes::new()
      Attributes::set(attrs, "batch", StringValue((i / 1000).to_string()))
      
      Counter::add(counter, 10.0, Some(attrs))
      Histogram::record(histogram, 500.0, Some(attrs))
    }
  }
  
  let end_time = Time::now()
  let duration = Time::duration_between(start_time, end_time)
  
  // Should record 200000 measurements (4 per iteration * 50000) in reasonable time
  assert_true(duration < 2000L) // Less than 2 seconds
  
  // Calculate performance metrics
  let measurements_per_second = (200000.0 * 1000.0) / (duration as Float)
  
  // Should achieve reasonable throughput
  assert_true(measurements_per_second > 100000.0) // At least 100k measurements/second
}

// Test 3: Memory Usage Under Load
test "memory usage under load" {
  let initial_memory = Memory::get_allocated_bytes()
  
  // Create large amount of telemetry data
  let telemetry_data = []
  
  for i in 1..=100000 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "trace_id", StringValue("trace_" + i.to_string()))
    Attributes::set(attrs, "span_id", StringValue("span_" + i.to_string()))
    Attributes::set(attrs, "service", StringValue("memory_test_service"))
    Attributes::set(attrs, "operation", StringValue("memory_test_operation"))
    Attributes::set(attrs, "iteration", IntValue(i))
    
    telemetry_data.push(TelemetryData::new(
      (i * 0.1),
      attrs,
      1234567890L + i
    ))
  }
  
  let peak_memory = Memory::get_allocated_bytes()
  let memory_increase = peak_memory - initial_memory
  
  // Memory usage should be reasonable (less than 100MB for 100k telemetry records)
  assert_true(memory_increase < 100 * 1024 * 1024) // Less than 100MB
  
  // Process the data
  let processor = BatchProcessor::new(10000)
  let processed_batches = []
  
  for batch in BatchProcessor::process(processor, telemetry_data) {
    let aggregated = BatchAggregator::aggregate(batch)
    processed_batches.push(aggregated)
  }
  
  let processed_memory = Memory::get_allocated_bytes()
  
  // Clear references to allow garbage collection
  telemetry_data = []
  
  // Force garbage collection if available
  Memory::collect()
  
  let final_memory = Memory::get_allocated_bytes()
  let final_increase = final_memory - initial_memory
  
  // Memory should be released after processing and cleanup
  assert_true(final_increase < memory_increase / 2) // At least 50% memory should be released
}

// Test 4: Concurrent Telemetry Operations
test "concurrent telemetry operations" {
  let processor = ConcurrentSpanProcessor::new(1000, 1000) // 1000 batch size, 1s timeout
  
  // Simulate concurrent operations
  let concurrent_tasks = 10
  let operations_per_task = 1000
  
  let start_time = Time::now()
  
  // Create concurrent tasks
  let tasks = []
  for task_id in 1..=concurrent_tasks {
    let task = ConcurrentTask::new({
      for i in 1..=operations_per_task {
        let span_ctx = SpanContext::new(
          "concurrent_trace_" + task_id.to_string(),
          "concurrent_span_" + task_id.to_string() + "_" + i.to_string(),
          true,
          "concurrent_state"
        )
        
        let span = Span::new("concurrent_test", Server, span_ctx)
        
        // Add attributes
        Span::set_attribute(span, "task_id", IntValue(task_id))
        Span::set_attribute(span, "operation_id", IntValue(i))
        Span::set_attribute(span, "thread", StringValue("thread_" + task_id.to_string()))
        
        // Process span
        SpanProcessor::on_start(processor, span)
        Span::end(span)
        SpanProcessor::on_end(processor, span)
      }
    })
    
    tasks.push(task)
  }
  
  // Execute all tasks concurrently
  ConcurrentTask::execute_all(tasks)
  
  let end_time = Time::now()
  let duration = Time::duration_between(start_time, end_time)
  
  // Should complete 10000 operations (10 tasks * 1000 ops) in reasonable time
  assert_true(duration < 5000L) // Less than 5 seconds
  
  // Force flush all processors
  SpanProcessor::force_flush(processor)
  
  // Calculate concurrent throughput
  let total_operations = concurrent_tasks * operations_per_task
  let operations_per_second = (total_operations as Float * 1000.0) / (duration as Float)
  
  // Should achieve reasonable concurrent throughput
  assert_true(operations_per_second > 2000.0) // At least 2k operations/second concurrently
}

// Test 5: Attribute Processing Performance
test "attribute processing performance" {
  // Test performance with large attribute sets
  let start_time = Time::now()
  
  let spans_with_large_attrs = []
  
  for i in 1..=5000 {
    let span_ctx = SpanContext::new(
      "attr_test_trace_" + i.to_string(),
      "attr_test_span_" + i.to_string(),
      true,
      "attr_test_state"
    )
    
    let span = Span::new("attribute_performance_test", Server, span_ctx)
    
    // Add large number of attributes
    for j in 1..=100 {
      Span::set_attribute(span, "attr_" + j.to_string(), StringValue("value_" + j.to_string()))
    }
    
    // Add array attributes
    let array_attr = ArrayStringValue([])
    for k in 1..=50 {
      array_attr.push("array_value_" + k.to_string())
    }
    Span::set_attribute(span, "large_array", array_attr)
    
    spans_with_large_attrs.push(span)
  }
  
  let creation_time = Time::now()
  let creation_duration = Time::duration_between(start_time, creation_time)
  
  // Should create 5000 spans with 100+ attributes each in reasonable time
  assert_true(creation_duration < 3000L) // Less than 3 seconds
  
  // Test attribute retrieval performance
  let retrieval_start = Time::now()
  
  for span in spans_with_large_attrs {
    // Retrieve various attributes
    let attr_1 = Span::get_attribute(span, "attr_1")
    let attr_50 = Span::get_attribute(span, "attr_50")
    let attr_100 = Span::get_attribute(span, "attr_100")
    let array_attr = Span::get_attribute(span, "large_array")
    
    // Verify attributes exist
    assert_true(attr_1.is_some())
    assert_true(attr_50.is_some())
    assert_true(attr_100.is_some())
    assert_true(array_attr.is_some())
  }
  
  let retrieval_end = Time::now()
  let retrieval_duration = Time::duration_between(retrieval_start, retrieval_end)
  
  // Should retrieve attributes quickly
  assert_true(retrieval_duration < 1000L) // Less than 1 second
  
  // Calculate performance metrics
  let spans_per_second = (5000.0 * 1000.0) / (creation_duration as Float)
  let retrievals_per_second = (5000.0 * 4.0 * 1000.0) / (retrieval_duration as Float) // 4 retrievals per span
  
  assert_true(spans_per_second > 1500.0) // At least 1.5k spans/second with large attrs
  assert_true(retrievals_per_second > 20000.0) // At least 20k attribute retrievals/second
}

// Test 6: Serialization Performance
test "serialization performance" {
  // Create large telemetry dataset
  let telemetry_data = []
  
  for i in 1..=10000 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "trace_id", StringValue("serialization_test_" + i.to_string()))
    Attributes::set(attrs, "span_id", StringValue("span_" + i.to_string()))
    Attributes::set(attrs, "service", StringValue("serialization_service"))
    Attributes::set(attrs, "operation", StringValue("serialization_operation"))
    Attributes::set(attrs, "iteration", IntValue(i))
    
    telemetry_data.push(TelemetryData::new(
      (i * 0.5),
      attrs,
      1234567890L + i
    ))
  }
  
  // Test JSON serialization performance
  let json_start = Time::now()
  let json_serialized = TelemetrySerializer::serialize_to_json(telemetry_data)
  let json_end = Time::now()
  let json_duration = Time::duration_between(json_start, json_end)
  
  // Should serialize 10000 records in reasonable time
  assert_true(json_duration < 2000L) // Less than 2 seconds
  
  // Test JSON deserialization performance
  let json_deserialize_start = Time::now()
  let json_deserialized = TelemetrySerializer::deserialize_from_json(json_serialized)
  let json_deserialize_end = Time::now()
  let json_deserialize_duration = Time::duration_between(json_deserialize_start, json_deserialize_end)
  
  // Should deserialize quickly
  assert_true(json_deserialize_duration < 2000L) // Less than 2 seconds
  
  // Verify data integrity
  assert_eq(json_deserialized.length(), 10000)
  
  // Test binary serialization performance
  let binary_start = Time::now()
  let binary_serialized = TelemetrySerializer::serialize_to_binary(telemetry_data)
  let binary_end = Time::now()
  let binary_duration = Time::duration_between(binary_start, binary_end)
  
  // Binary serialization should be faster
  assert_true(binary_duration < json_duration)
  
  // Test binary deserialization performance
  let binary_deserialize_start = Time::now()
  let binary_deserialized = TelemetrySerializer::deserialize_from_binary(binary_serialized)
  let binary_deserialize_end = Time::now()
  let binary_deserialize_duration = Time::duration_between(binary_deserialize_start, binary_deserialize_end)
  
  // Binary deserialization should be faster
  assert_true(binary_deserialize_duration < json_deserialize_duration)
  
  // Verify data integrity
  assert_eq(binary_deserialized.length(), 10000)
  
  // Calculate performance metrics
  let json_serialization_rate = (10000.0 * 1000.0) / (json_duration as Float)
  let json_deserialization_rate = (10000.0 * 1000.0) / (json_deserialize_duration as Float)
  let binary_serialization_rate = (10000.0 * 1000.0) / (binary_duration as Float)
  let binary_deserialization_rate = (10000.0 * 1000.0) / (binary_deserialize_duration as Float)
  
  assert_true(json_serialization_rate > 5000.0) // At least 5k records/second
  assert_true(json_deserialization_rate > 5000.0) // At least 5k records/second
  assert_true(binary_serialization_rate > 10000.0) // At least 10k records/second
  assert_true(binary_deserialization_rate > 10000.0) // At least 10k records/second
}

// Test 7: Context Propagation Performance
test "context propagation performance" {
  let propagator = TraceContextPropagator::new()
  let carrier = TextMapCarrier::new()
  
  // Test context injection performance
  let injection_start = Time::now()
  
  for i in 1..=10000 {
    let span_ctx = SpanContext::new(
      "propagation_trace_" + i.to_string(),
      "propagation_span_" + i.to_string(),
      true,
      "propagation_state"
    )
    
    let ctx = Context::with_value(
      Context::root(),
      ContextKey::new("span_context"),
      span_ctx
    )
    
    // Add baggage
    let baggage = Baggage::new()
    let baggage_with_entry = Baggage::set_entry(baggage, "user_id", "user_" + i.to_string())
    let ctx_with_baggage = Context::with_value(
      ctx,
      ContextKey::new("baggage"),
      baggage_with_entry
    )
    
    Propagator::inject(propagator, ctx_with_baggage, carrier)
  }
  
  let injection_end = Time::now()
  let injection_duration = Time::duration_between(injection_start, injection_end)
  
  // Should inject 10000 contexts in reasonable time
  assert_true(injection_duration < 2000L) // Less than 2 seconds
  
  // Test context extraction performance
  let extraction_start = Time::now()
  
  for i in 1..=10000 {
    let extracted_ctx = Propagator::extract(propagator, carrier)
    
    // Get span context from extracted context
    let span_ctx = Context::get(extracted_ctx, ContextKey::new("span_context"))
    assert_true(span_ctx.is_some())
    
    // Get baggage from extracted context
    let baggage = Context::get(extracted_ctx, ContextKey::new("baggage"))
    assert_true(baggage.is_some())
  }
  
  let extraction_end = Time::now()
  let extraction_duration = Time::duration_between(extraction_start, extraction_end)
  
  // Should extract 10000 contexts in reasonable time
  assert_true(extraction_duration < 2000L) // Less than 2 seconds
  
  // Calculate performance metrics
  let injection_rate = (10000.0 * 1000.0) / (injection_duration as Float)
  let extraction_rate = (10000.0 * 1000.0) / (extraction_duration as Float)
  
  assert_true(injection_rate > 5000.0) // At least 5k injections/second
  assert_true(extraction_rate > 5000.0) // At least 5k extractions/second
}

// Test 8: Resource Management Performance
test "resource management performance" {
  // Test resource creation and merging performance
  let creation_start = Time::now()
  
  let resources = []
  for i in 1..=1000 {
    let attrs = []
    for j in 1..=50 {
      attrs.push(("resource_attr_" + j.to_string(), StringValue("value_" + i.to_string() + "_" + j.to_string())))
    }
    
    let resource = Resource::with_attributes(Resource::new(), attrs)
    resources.push(resource)
  }
  
  let creation_end = Time::now()
  let creation_duration = Time::duration_between(creation_start, creation_end)
  
  // Should create 1000 resources with 50 attributes each in reasonable time
  assert_true(creation_duration < 1000L) // Less than 1 second
  
  // Test resource merging performance
  let merge_start = Time::now()
  
  let base_resource = Resource::new()
  for resource in resources {
    base_resource = Resource::merge(base_resource, resource)
  }
  
  let merge_end = Time::now()
  let merge_duration = Time::duration_between(merge_start, merge_end)
  
  // Should merge 1000 resources in reasonable time
  assert_true(merge_duration < 2000L) // Less than 2 seconds
  
  // Test resource attribute retrieval performance
  let retrieval_start = Time::now()
  
  for i in 1..=1000 {
    for j in 1..=50 {
      let attr_name = "resource_attr_" + j.to_string()
      let attr_value = Resource::get_attribute(base_resource, attr_name)
      
      // Should find the attribute (from the last resource that set it)
      assert_true(attr_value.is_some())
    }
  }
  
  let retrieval_end = Time::now()
  let retrieval_duration = Time::duration_between(retrieval_start, retrieval_end)
  
  // Should retrieve 50000 attributes in reasonable time
  assert_true(retrieval_duration < 1000L) // Less than 1 second
  
  // Calculate performance metrics
  let creation_rate = (1000.0 * 1000.0) / (creation_duration as Float)
  let merge_rate = (1000.0 * 1000.0) / (merge_duration as Float)
  let retrieval_rate = (50000.0 * 1000.0) / (retrieval_duration as Float)
  
  assert_true(creation_rate > 1000.0) // At least 1k resources/second
  assert_true(merge_rate > 500.0) // At least 500 merges/second
  assert_true(retrieval_rate > 50000.0) // At least 50k attribute retrievals/second
}