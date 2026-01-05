// Azimuth Performance Benchmark High-Quality Tests
// This file contains comprehensive performance benchmark test cases

// Test 1: Span Creation and End Performance
test "span creation and end performance benchmark" {
  let tracer = TracerProvider::get_tracer("performance_benchmark")
  let benchmark = PerformanceBenchmark::new()
  
  // Benchmark span creation performance
  let span_creation_result = PerformanceBenchmark::measure(benchmark, || {
    let mut spans = []
    for i in 0..=10000 {
      let span = Tracer::start_span(tracer, "benchmark_span_" + i.to_string())
      spans.push(span)
    }
    spans.length()
  })
  
  // Verify span creation performance (should be < 1ms per span)
  let creation_time_per_span = span_creation_result.duration / 10000.0
  assert_true(creation_time_per_span < 1.0, "Span creation should be < 1ms per span")
  
  // Benchmark span ending performance
  let spans = Array::make(10000, Tracer::start_span(tracer, "end_benchmark_span"))
  let span_end_result = PerformanceBenchmark::measure(benchmark, || {
    for span in spans {
      Span::end(span)
    }
    true
  })
  
  // Verify span ending performance (should be < 0.5ms per span)
  let end_time_per_span = span_end_result.duration / 10000.0
  assert_true(end_time_per_span < 0.5, "Span ending should be < 0.5ms per span")
  
  // Benchmark span with attributes performance
  let span_with_attrs_result = PerformanceBenchmark::measure(benchmark, || {
    let mut spans = []
    for i in 0..=5000 {
      let span = Tracer::start_span(tracer, "attr_benchmark_span_" + i.to_string())
      Span::set_attribute(span, "iteration", IntValue(i))
      Span::set_attribute(span, "service", StringValue("benchmark_service"))
      Span::set_attribute(span, "timestamp", IntValue(1609459200 + i))
      Span::add_event(span, "test_event", Some([
        ("event_type", StringValue("benchmark")),
        ("event_data", FloatValue(i as Float))
      ]))
      spans.push(span)
    }
    spans.length()
  })
  
  // Verify span with attributes performance
  let attrs_time_per_span = span_with_attrs_result.duration / 5000.0
  assert_true(attrs_time_per_span < 2.0, "Span with attributes should be < 2ms per span")
}

// Test 2: Metrics Collection Performance
test "metrics collection performance benchmark" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "benchmark_meter")
  let benchmark = PerformanceBenchmark::new()
  
  // Benchmark counter performance
  let counter = Meter::create_counter(meter, "benchmark_counter", Some("Benchmark counter"), Some("count"))
  
  let counter_result = PerformanceBenchmark::measure(benchmark, || {
    for i in 0..=100000 {
      Counter::add(counter, 1.0)
    }
    true
  })
  
  // Verify counter performance (should be > 100k operations per second)
  let counter_ops_per_second = 100000.0 / (counter_result.duration / 1000.0)
  assert_true(counter_ops_per_second > 100000.0, "Counter should support > 100k ops/sec")
  
  // Benchmark histogram performance
  let histogram = Meter::create_histogram(meter, "benchmark_histogram", Some("Benchmark histogram"), Some("ms"))
  
  let histogram_result = PerformanceBenchmark::measure(benchmark, || {
    for i in 0..=50000 {
      Histogram::record(histogram, (i as Float) % 1000.0)
    }
    true
  })
  
  // Verify histogram performance
  let histogram_ops_per_second = 50000.0 / (histogram_result.duration / 1000.0)
  assert_true(histogram_ops_per_second > 50000.0, "Histogram should support > 50k ops/sec")
  
  // Benchmark gauge performance
  let gauge = Meter::create_gauge(meter, "benchmark_gauge", Some("Benchmark gauge"), Some("value"))
  
  let gauge_result = PerformanceBenchmark::measure(benchmark, || {
    for i in 0..=25000 {
      Gauge::record(gauge, (i as Float) % 100.0)
    }
    true
  })
  
  // Verify gauge performance
  let gauge_ops_per_second = 25000.0 / (gauge_result.duration / 1000.0)
  assert_true(gauge_ops_per_second > 25000.0, "Gauge should support > 25k ops/sec")
  
  // Benchmark metrics with attributes performance
  let attr_counter = Meter::create_counter(meter, "attr_benchmark_counter", Some("Attr benchmark counter"), Some("count"))
  
  let attr_counter_result = PerformanceBenchmark::measure(benchmark, || {
    for i in 0..=25000 {
      Counter::add(attr_counter, 1.0, Some(Attributes::with_data([
        ("service", StringValue("benchmark_service")),
        ("operation", StringValue("test_operation")),
        ("status", StringValue(if i % 10 == 0 { "error" } else { "success" })),
        ("iteration", IntValue(i))
      ])))
    }
    true
  })
  
  // Verify metrics with attributes performance
  let attr_counter_ops_per_second = 25000.0 / (attr_counter_result.duration / 1000.0)
  assert_true(attr_counter_ops_per_second > 10000.0, "Counter with attributes should support > 10k ops/sec")
}

// Test 3: Context Propagation Performance
test "context propagation performance benchmark" {
  let tracer = TracerProvider::get_tracer("context_benchmark")
  let propagator = TraceContextPropagator::new()
  let benchmark = PerformanceBenchmark::new()
  
  // Create root context
  let root_span = Tracer::start_span(tracer, "root_context")
  let root_context = Span::span_context(root_span)
  
  // Benchmark context injection performance
  let injection_result = PerformanceBenchmark::measure(benchmark, || {
    let mut headers = []
    for i in 0..=10000 {
      headers = []
      TraceContextPropagator::inject(propagator, root_context, headers)
    }
    headers.length()
  })
  
  // Verify injection performance
  let injection_time_per_op = injection_result.duration / 10000.0
  assert_true(injection_time_per_op < 0.1, "Context injection should be < 0.1ms per operation")
  
  // Benchmark context extraction performance
  let headers = []
  TraceContextPropagator::inject(propagator, root_context, headers)
  
  let extraction_result = PerformanceBenchmark::measure(benchmark, || {
    let mut contexts = []
    for i in 0..=10000 {
      let extracted = TraceContextPropagator::extract(propagator, headers)
      contexts.push(extracted)
    }
    contexts.length()
  })
  
  // Verify extraction performance
  let extraction_time_per_op = extraction_result.duration / 10000.0
  assert_true(extraction_time_per_op < 0.1, "Context extraction should be < 0.1ms per operation")
  
  // Benchmark round-trip propagation performance
  let roundtrip_result = PerformanceBenchmark::measure(benchmark, || {
    let mut successful_extractions = 0
    for i in 0..=5000 {
      let headers = []
      TraceContextPropagator::inject(propagator, root_context, headers)
      let extracted = TraceContextPropagator::extract(propagator, headers)
      
      match extracted {
        Some(context) => {
          if SpanContext::trace_id(context) == SpanContext::trace_id(root_context) {
            successful_extractions = successful_extractions + 1
          }
        }
        None => {}
      }
    }
    successful_extractions
  })
  
  // Verify all extractions were successful
  assert_eq(roundtrip_result.value, 5000)
  
  Span::end(root_span)
}

// Test 4: Memory Usage Benchmark
test "memory usage benchmark" {
  let memory_profiler = MemoryProfiler::new()
  let tracer = TracerProvider::get_tracer("memory_benchmark")
  
  // Baseline memory measurement
  let baseline_memory = MemoryProfiler::get_current_usage(memory_profiler)
  
  // Create many spans and measure memory growth
  let mut spans = []
  for i in 0..=10000 {
    let span = Tracer::start_span(tracer, "memory_benchmark_span_" + i.to_string())
    Span::set_attribute(span, "iteration", IntValue(i))
    Span::set_attribute(span, "data", StringValue("test_data_" + i.to_string()))
    Span::add_event(span, "memory_test_event", Some([
      ("event_iteration", IntValue(i)),
      ("event_data", FloatValue(i as Float))
    ]))
    spans.push(span)
  }
  
  let peak_memory = MemoryProfiler::get_current_usage(memory_profiler)
  
  // Calculate memory per span
  let memory_per_span = (peak_memory.heap_used - baseline_memory.heap_used) / 10000
  
  // Verify memory usage is reasonable (< 1KB per span)
  assert_true(memory_per_span < 1024, "Each span should use < 1KB memory")
  
  // End all spans and measure memory recovery
  for span in spans {
    Span::end(span)
  }
  
  // Force garbage collection if available
  MemoryProfiler::force_gc(memory_profiler)
  
  let final_memory = MemoryProfiler::get_current_usage(memory_profiler)
  
  // Verify most memory is recovered
  let memory_recovered = (peak_memory.heap_used - final_memory.heap_used)
  let memory_growth = final_memory.heap_used - baseline_memory.heap_used
  
  assert_true(memory_recovered > (peak_memory.heap_used - baseline_memory.heap_used) * 0.8, 
    "Should recover at least 80% of memory after ending spans")
  assert_true(memory_growth < baseline_memory.heap_used * 0.1, 
    "Memory growth should be < 10% of baseline")
}

// Test 5: Batch Processing Performance
test "batch processing performance benchmark" {
  let batch_processor = BatchProcessor::new(BatchConfig {
    max_batch_size: 1000,
    max_delay_ms: 100,
    max_export_batch_size: 500
  })
  
  let benchmark = PerformanceBenchmark::new()
  
  // Create batch of telemetry data
  let batch_data = Array::make(10000, TelemetryData {
    timestamp: 1609459200L,
    metric_name: "batch_test_metric",
    value: FloatValue(50.0),
    attributes: [
      ("service", StringValue("batch_test_service")),
      ("operation", StringValue("batch_test_operation"))
    ]
  })
  
  // Benchmark batch processing performance
  let batch_result = PerformanceBenchmark::measure(benchmark, || {
    BatchProcessor::process_batch(batch_processor, batch_data)
  })
  
  // Verify batch processing performance
  let throughput = 10000.0 / (batch_result.duration / 1000.0)
  assert_true(throughput > 5000.0, "Batch processing should handle > 5000 items/sec")
  
  // Benchmark different batch sizes
  let batch_sizes = [100, 500, 1000, 2000, 5000]
  let mut performance_results = []
  
  for batch_size in batch_sizes {
    let test_data = Array::make(batch_size, TelemetryData {
      timestamp: 1609459200L,
      metric_name: "size_test_metric",
      value: FloatValue(50.0),
      attributes: [
        ("batch_size", IntValue(batch_size))
      ]
    })
    
    let size_result = PerformanceBenchmark::measure(benchmark, || {
      BatchProcessor::process_batch(batch_processor, test_data)
    })
    
    performance_results.push((batch_size, size_result.duration))
  }
  
  // Verify performance scales reasonably with batch size
  assert_true(performance_results[4].2 < performance_results[0].2 * 25.0, 
    "5000-item batch should not take 25x longer than 100-item batch")
}

// Test 6: Serialization Performance
test "serialization performance benchmark" {
  let serializer = JsonSerializer::new()
  let benchmark = PerformanceBenchmark::new()
  
  // Create complex telemetry data for serialization
  let complex_data = TelemetryData {
    timestamp: 1609459200L,
    metric_name: "complex_metric",
    value: FloatValue(42.5),
    attributes: [
      ("string_attr", StringValue("test_string_value")),
      ("int_attr", IntValue(123)),
      ("float_attr", FloatValue(3.14159)),
      ("bool_attr", BoolValue(true)),
      ("array_attr", ArrayStringValue(["item1", "item2", "item3"]))
    ]
  }
  
  // Benchmark serialization performance
  let serialization_result = PerformanceBenchmark::measure(benchmark, || {
    let mut serialized = []
    for i in 0..=10000 {
      let serialized_item = JsonSerializer::serialize(serializer, complex_data)
      serialized.push(serialized_item)
    }
    serialized.length()
  })
  
  // Verify serialization performance
  let serialization_ops_per_second = 10000.0 / (serialization_result.duration / 1000.0)
  assert_true(serialization_ops_per_second > 10000.0, "Serialization should handle > 10k ops/sec")
  
  // Benchmark deserialization performance
  let serialized_data = JsonSerializer::serialize(serializer, complex_data)
  
  let deserialization_result = PerformanceBenchmark::measure(benchmark, || {
    let mut deserialized = []
    for i in 0..=10000 {
      let deserialized_item = JsonSerializer::deserialize(serializer, serialized_data)
      deserialized.push(deserialized_item)
    }
    deserialized.length()
  })
  
  // Verify deserialization performance
  let deserialization_ops_per_second = 10000.0 / (deserialization_result.duration / 1000.0)
  assert_true(deserialization_ops_per_second > 5000.0, "Deserialization should handle > 5k ops/sec")
}

// Test 7: Concurrent Operations Performance
test "concurrent operations performance benchmark" {
  let concurrent_tracer = TracerProvider::get_tracer("concurrent_benchmark")
  let benchmark = PerformanceBenchmark::new()
  
  // Benchmark concurrent span creation
  let concurrent_result = PerformanceBenchmark::measure(benchmark, || {
    let mut handles = []
    
    // Create 10 concurrent threads, each creating 1000 spans
    for thread_id in 0..=10 {
      let handle = Thread::spawn(|| {
        let mut spans = []
        for i in 0..=1000 {
          let span = Tracer::start_span(concurrent_tracer, 
            "concurrent_span_" + thread_id.to_string() + "_" + i.to_string())
          spans.push(span)
        }
        spans.length()
      })
      handles.push(handle)
    }
    
    // Wait for all threads to complete
    let mut total_spans = 0
    for handle in handles {
      total_spans = total_spans + Thread::join(handle)
    }
    
    total_spans
  })
  
  // Verify all spans were created
  assert_eq(concurrent_result.value, 11000)
  
  // Verify concurrent performance (should be close to sequential performance)
  let concurrent_ops_per_second = 11000.0 / (concurrent_result.duration / 1000.0)
  assert_true(concurrent_ops_per_second > 5000.0, "Concurrent operations should handle > 5000 ops/sec")
  
  // Benchmark concurrent metrics operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent_meter")
  let counter = Meter::create_counter(meter, "concurrent_counter", Some("Concurrent counter"), Some("count"))
  
  let concurrent_metrics_result = PerformanceBenchmark::measure(benchmark, || {
    let mut handles = []
    
    // Create 10 concurrent threads, each performing 5000 counter operations
    for thread_id in 0..=10 {
      let handle = Thread::spawn(|| {
        for i in 0..=5000 {
          Counter::add(counter, 1.0, Some(Attributes::with_data([
            ("thread_id", IntValue(thread_id)),
            ("iteration", IntValue(i))
          ])))
        }
        5000
      })
      handles.push(handle)
    }
    
    // Wait for all threads to complete
    let mut total_operations = 0
    for handle in handles {
      total_operations = total_operations + Thread::join(handle)
    }
    
    total_operations
  })
  
  // Verify all operations were performed
  assert_eq(concurrent_metrics_result.value, 55000)
  
  // Verify concurrent metrics performance
  let concurrent_metrics_ops_per_second = 55000.0 / (concurrent_metrics_result.duration / 1000.0)
  assert_true(concurrent_metrics_ops_per_second > 25000.0, "Concurrent metrics should handle > 25000 ops/sec")
}