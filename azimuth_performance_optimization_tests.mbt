// Azimuth Performance Optimization Test Suite
// This file contains comprehensive test cases for performance optimization scenarios

// Test 1: High-Volume Span Creation Performance
test "high-volume span creation performance" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "performance_test_tracer")
  let span_count = 10000
  
  // Measure span creation performance
  let start_time = Time::now()
  
  let mut spans = []
  for i in 0..span_count {
    let span_name = "perf_span_" + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    spans = spans.push(span)
  }
  
  let creation_time = Time::now() - start_time
  
  // Measure span ending performance
  let end_start_time = Time::now()
  
  for span in spans {
    Span::end(span)
  }
  
  let end_time = Time::now() - end_start_time
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Verify all spans were created and exported
  let collected_spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(collected_spans.length(), span_count)
  
  // Performance assertions (adjust thresholds based on environment)
  let creation_rate = span_count.to_float() / creation_time.to_seconds()
  let end_rate = span_count.to_float() / end_time.to_seconds()
  
  // Should be able to create at least 1000 spans per second
  assert_true(creation_rate > 1000.0)
  assert_true(end_rate > 1000.0)
  
  // Log performance metrics
  let meter = TelemetryProvider::get_meter(provider, "performance_metrics")
  let creation_rate_gauge = Meter::create_gauge(meter, "span_creation_rate")
  let end_rate_gauge = Meter::create_gauge(meter, "span_end_rate")
  
  Gauge::set(creation_rate_gauge, creation_rate)
  Gauge::set(end_rate_gauge, end_rate)
}

// Test 2: Batch Processing Performance
test "batch processing performance" {
  // Configure batch processing with different sizes
  let batch_sizes = [10, 50, 100, 500, 1000]
  let operations_per_batch = 1000
  
  for batch_size in batch_sizes {
    let batch_config = {
      max_batch_size: batch_size,
      max_export_timeout_ms: 5000,
      scheduled_delay_ms: 100
    }
    
    let provider = TelemetryProvider::with_batch_config(batch_config)
    let tracer = TelemetryProvider::get_tracer(provider, "batch_perf_tracer")
    
    // Measure batch processing performance
    let start_time = Time::now()
    
    for i in 0..operations_per_batch {
      let span = Tracer::start_span(tracer, "batch_span_" + i.to_string())
      Span::end(span)
    }
    
    // Force batch export
    TelemetryProvider::force_flush(provider)
    
    let total_time = Time::now() - start_time
    let throughput = operations_per_batch.to_float() / total_time.to_seconds()
    
    // Verify all spans were exported
    let spans = TelemetryProvider::get_collected_spans(provider)
    assert_eq(spans.length(), operations_per_batch)
    
    // Log performance metrics
    let meter = TelemetryProvider::get_meter(provider, "batch_performance")
    let throughput_gauge = Meter::create_gauge(meter, "batch_throughput")
    Gauge::set(throughput_gauge, throughput)
    
    // Larger batch sizes should generally provide better throughput
    if batch_size > 10 {
      assert_true(throughput > 100.0) // At least 100 operations per second
    }
  }
}

// Test 3: Memory Usage Optimization
test "memory usage optimization" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "memory_test_tracer")
  
  // Measure memory usage before creating spans
  let initial_memory = Memory::get_usage()
  
  // Create a large number of spans
  let span_count = 5000
  let mut spans = []
  
  for i in 0..span_count {
    let span = Tracer::start_span(tracer, "memory_span_" + i.to_string())
    
    // Add some attributes to increase memory usage
    Span::add_attribute(span, "iteration", i.to_string())
    Span::add_attribute(span, "data", "x".repeat(100)) // 100 character string
    
    spans = spans.push(span)
  }
  
  let peak_memory = Memory::get_usage()
  
  // End all spans
  for span in spans {
    Span::end(span)
  }
  
  // Force export and cleanup
  TelemetryProvider::force_flush(provider)
  
  // Measure memory after cleanup
  let final_memory = Memory::get_usage()
  
  // Verify memory usage is reasonable
  let memory_per_span = (peak_memory - initial_memory) / span_count
  assert_true(memory_per_span < 10000) // Less than 10KB per span
  
  // Memory should be released after export
  let memory_released = peak_memory - final_memory
  let release_percentage = memory_released.to_float() / (peak_memory - initial_memory).to_float()
  assert_true(release_percentage > 0.8) // At least 80% of memory should be released
  
  // Log memory metrics
  let meter = TelemetryProvider::get_meter(provider, "memory_metrics")
  let memory_gauge = Meter::create_gauge(meter, "memory_per_span")
  let release_gauge = Meter::create_gauge(meter, "memory_release_percentage")
  
  Gauge::set(memory_gauge, memory_per_span.to_float())
  Gauge::set(release_gauge, release_percentage * 100.0)
}

// Test 4: Attribute Processing Performance
test "attribute processing performance" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "attribute_test_tracer")
  
  // Test different attribute counts
  let attribute_counts = [1, 5, 10, 25, 50, 100]
  
  for attr_count in attribute_counts {
    let span = Tracer::start_span(tracer, "attribute_test_span")
    
    // Measure attribute addition performance
    let start_time = Time::now()
    
    for i in 0..attr_count {
      let key = "attr_" + i.to_string()
      let value = "value_" + i.to_string()
      Span::add_attribute(span, key, value)
    }
    
    let add_time = Time::now() - start_time
    
    // Measure attribute retrieval performance
    let retrieval_start_time = Time::now()
    
    for i in 0..attr_count {
      let key = "attr_" + i.to_string()
      let _value = Span::get_attribute(span, key)
    }
    
    let retrieval_time = Time::now() - retrieval_start_time
    
    Span::end(span)
    
    // Performance should be reasonable even with many attributes
    let add_rate = attr_count.to_float() / add_time.to_seconds()
    let retrieval_rate = attr_count.to_float() / retrieval_time.to_seconds()
    
    assert_true(add_rate > 1000.0) // At least 1000 attributes per second
    assert_true(retrieval_rate > 5000.0) // At least 5000 attribute retrievals per second
    
    // Log performance metrics
    let meter = TelemetryProvider::get_meter(provider, "attribute_performance")
    let add_rate_gauge = Meter::create_gauge(meter, "attribute_add_rate")
    let retrieval_rate_gauge = Meter::create_gauge(meter, "attribute_retrieval_rate")
    
    Gauge::set(add_rate_gauge, add_rate)
    Gauge::set(retrieval_rate_gauge, retrieval_rate)
  }
}

// Test 5: Metric Aggregation Performance
test "metric aggregation performance" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "aggregation_test_meter")
  
  // Create metrics
  let counter = Meter::create_counter(meter, "performance_counter")
  let histogram = Meter::create_histogram(meter, "performance_histogram")
  
  // Test different data volumes
  let data_volumes = [100, 1000, 5000, 10000]
  
  for volume in data_volumes {
    // Measure counter performance
    let counter_start_time = Time::now()
    
    for i in 0..volume {
      let attrs = Attributes::new()
      Attributes::set(attrs, "iteration", i.to_string())
      Counter::add(counter, 1.0, Some(attrs))
    }
    
    let counter_time = Time::now() - counter_start_time
    
    // Measure histogram performance
    let histogram_start_time = Time::now()
    
    for i in 0..volume {
      let attrs = Attributes::new()
      Attributes::set(attrs, "iteration", i.to_string())
      Histogram::record(histogram, i.to_float(), Some(attrs))
    }
    
    let histogram_time = Time::now() - histogram_start_time
    
    // Force export
    TelemetryProvider::force_flush(provider)
    
    // Verify metrics were recorded
    let metrics = TelemetryProvider::get_collected_metrics(provider)
    let counter_metrics = metrics.filter(fn(m) { m.name == "performance_counter" })
    let histogram_metrics = metrics.filter(fn(m) { m.name == "performance_histogram" })
    
    assert_eq(counter_metrics.length(), volume)
    assert_eq(histogram_metrics.length(), volume)
    
    // Performance assertions
    let counter_rate = volume.to_float() / counter_time.to_seconds()
    let histogram_rate = volume.to_float() / histogram_time.to_seconds()
    
    assert_true(counter_rate > 1000.0) // At least 1000 counter operations per second
    assert_true(histogram_rate > 1000.0) // At least 1000 histogram operations per second
    
    // Log performance metrics
    let perf_meter = TelemetryProvider::get_meter(provider, "performance_metrics")
    let counter_rate_gauge = Meter::create_gauge(perf_meter, "counter_operation_rate")
    let histogram_rate_gauge = Meter::create_gauge(perf_meter, "histogram_operation_rate")
    
    Gauge::set(counter_rate_gauge, counter_rate)
    Gauge::set(histogram_rate_gauge, histogram_rate)
  }
}

// Test 6: Sampling Performance Impact
test "sampling performance impact" {
  // Test different sampling rates
  let sampling_rates = [0.1, 0.5, 0.9, 1.0]
  let operation_count = 1000
  
  for rate in sampling_rates {
    let sampling_config = {
      sampling_enabled: true,
      sampling_rate: rate,
      max_spans_per_second: 10000
    }
    
    let provider = TelemetryProvider::with_config(sampling_config)
    let tracer = TelemetryProvider::get_tracer(provider, "sampling_test_tracer")
    
    // Measure performance with sampling
    let start_time = Time::now()
    
    let mut sampled_count = 0
    for i in 0..operation_count {
      let span = Tracer::start_span(tracer, "sampling_span_" + i.to_string())
      if Span::is_sampled(span) {
        sampled_count = sampled_count + 1
      }
      Span::end(span)
    }
    
    let total_time = Time::now() - start_time
    let throughput = operation_count.to_float() / total_time.to_seconds()
    
    // Force export
    TelemetryProvider::force_flush(provider)
    
    // Verify sampling worked correctly
    let spans = TelemetryProvider::get_collected_spans(provider)
    assert_eq(spans.length(), sampled_count)
    
    // Verify sampling rate is approximately correct
    let actual_rate = sampled_count.to_float() / operation_count.to_float()
    assert_true((actual_rate - rate).abs() < 0.1) // Within 10% of expected rate
    
    // Performance should not be significantly impacted by sampling
    assert_true(throughput > 1000.0) // At least 1000 operations per second
    
    // Log performance metrics
    let meter = TelemetryProvider::get_meter(provider, "sampling_performance")
    let throughput_gauge = Meter::create_gauge(meter, "sampling_throughput")
    let actual_rate_gauge = Meter::create_gauge(meter, "actual_sampling_rate")
    
    Gauge::set(throughput_gauge, throughput)
    Gauge::set(actual_rate_gauge, actual_rate * 100.0)
  }
}

// Test 7: Concurrent Processing Performance
test "concurrent processing performance" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "concurrent_test_tracer")
  let thread_counts = [1, 2, 4, 8, 16]
  let operations_per_thread = 500
  
  for thread_count in thread_counts {
    // Measure concurrent processing performance
    let start_time = Time::now()
    
    // Simulate concurrent operations
    let mut spans = []
    for thread_id in 0..thread_count {
      for op_id in 0..operations_per_thread {
        let span_name = "concurrent_span_" + thread_id.to_string() + "_" + op_id.to_string()
        let span = Tracer::start_span(tracer, span_name)
        Span::add_attribute(span, "thread_id", thread_id.to_string())
        Span::add_attribute(span, "operation_id", op_id.to_string())
        spans = spans.push(span)
      }
    }
    
    // End all spans
    for span in spans {
      Span::end(span)
    }
    
    let total_time = Time::now() - start_time
    let total_operations = thread_count * operations_per_thread
    let throughput = total_operations.to_float() / total_time.to_seconds()
    
    // Force export
    TelemetryProvider::force_flush(provider)
    
    // Verify all spans were processed
    let collected_spans = TelemetryProvider::get_collected_spans(provider)
    assert_eq(collected_spans.length(), total_operations)
    
    // Performance should scale reasonably with thread count
    assert_true(throughput > 500.0) // At least 500 operations per second
    
    // Log performance metrics
    let meter = TelemetryProvider::get_meter(provider, "concurrent_performance")
    let throughput_gauge = Meter::create_gauge(meter, "concurrent_throughput")
    let thread_count_gauge = Meter::create_gauge(meter, "thread_count")
    
    Gauge::set(throughput_gauge, throughput)
    Gauge::set(thread_count_gauge, thread_count.to_float())
  }
}

// Test 8: Serialization Performance
test "serialization performance" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "serialization_test_tracer")
  
  // Create spans with varying complexity
  let span_complexities = [
    { name: "simple_span", attr_count: 5, event_count: 0 },
    { name: "medium_span", attr_count: 20, event_count: 5 },
    { name: "complex_span", attr_count: 50, event_count: 20 }
  ]
  
  for complexity in span_complexities {
    let span = Tracer::start_span(tracer, complexity.name)
    
    // Add attributes
    for i in 0..complexity.attr_count {
      let key = "attr_" + i.to_string()
      let value = "value_" + i.to_string()
      Span::add_attribute(span, key, value)
    }
    
    // Add events
    for i in 0..complexity.event_count {
      let event_name = "event_" + i.to_string()
      Span::add_event(span, event_name, Some([
        ("event_attr", "event_value_" + i.to_string())
      ]))
    }
    
    // Measure serialization performance
    let serialization_start_time = Time::now()
    
    let serialized = Span::serialize(span)
    
    let serialization_time = Time::now() - serialization_start_time
    
    // Measure deserialization performance
    let deserialization_start_time = Time::now()
    
    let deserialized = Span::deserialize(serialized)
    
    let deserialization_time = Time::now() - deserialization_start_time
    
    Span::end(span)
    
    // Verify serialization/deserialization worked
    assert_true(deserialized.is_some())
    
    // Performance assertions
    let serialization_size = serialized.length()
    let serialization_rate = serialization_size.to_float() / serialization_time.to_seconds()
    let deserialization_rate = serialization_size.to_float() / deserialization_time.to_seconds()
    
    assert_true(serialization_rate > 1000.0) // At least 1KB per second
    assert_true(deserialization_rate > 1000.0) // At least 1KB per second
    
    // Log performance metrics
    let meter = TelemetryProvider::get_meter(provider, "serialization_performance")
    let size_gauge = Meter::create_gauge(meter, "serialized_size")
    let serialization_rate_gauge = Meter::create_gauge(meter, "serialization_rate")
    let deserialization_rate_gauge = Meter::create_gauge(meter, "deserialization_rate")
    
    Gauge::set(size_gauge, serialization_size.to_float())
    Gauge::set(serialization_rate_gauge, serialization_rate)
    Gauge::set(deserialization_rate_gauge, deserialization_rate)
  }
}

// Test 9: Resource Cleanup Performance
test "resource cleanup performance" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "cleanup_test_tracer")
  
  // Create a large number of spans
  let span_count = 5000
  let mut spans = []
  
  for i in 0..span_count {
    let span = Tracer::start_span(tracer, "cleanup_span_" + i.to_string())
    Span::add_attribute(span, "iteration", i.to_string())
    spans = spans.push(span)
  }
  
  // Measure memory usage before cleanup
  let pre_cleanup_memory = Memory::get_usage()
  
  // Measure cleanup performance
  let cleanup_start_time = Time::now()
  
  // End all spans and force cleanup
  for span in spans {
    Span::end(span)
  }
  
  TelemetryProvider::force_flush(provider)
  TelemetryProvider::cleanup(provider)
  
  let cleanup_time = Time::now() - cleanup_start_time
  
  // Measure memory usage after cleanup
  let post_cleanup_memory = Memory::get_usage()
  
  // Verify cleanup performance
  let cleanup_rate = span_count.to_float() / cleanup_time.to_seconds()
  assert_true(cleanup_rate > 100.0) // At least 100 spans cleaned up per second
  
  // Verify memory was released
  let memory_released = pre_cleanup_memory - post_cleanup_memory
  let release_percentage = memory_released.to_float() / pre_cleanup_memory.to_float()
  assert_true(release_percentage > 0.7) // At least 70% of memory should be released
  
  // Log performance metrics
  let meter = TelemetryProvider::get_meter(provider, "cleanup_performance")
  let cleanup_rate_gauge = Meter::create_gauge(meter, "cleanup_rate")
  let memory_release_gauge = Meter::create_gauge(meter, "memory_release_percentage")
  
  Gauge::set(cleanup_rate_gauge, cleanup_rate)
  Gauge::set(memory_release_gauge, release_percentage * 100.0)
}

// Test 10: End-to-End Performance Benchmark
test "end-to-end performance benchmark" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "benchmark_tracer")
  let meter = TelemetryProvider::get_meter(provider, "benchmark_meter")
  let logger = TelemetryProvider::get_logger(provider, "benchmark_logger")
  
  // Create metrics for benchmarking
  let span_counter = Meter::create_counter(meter, "spans_created")
  let metric_counter = Meter::create_counter(meter, "metrics_created")
  let log_counter = Meter::create_counter(meter, "logs_created")
  let end_to_end_timer = Meter::create_histogram(meter, "end_to_end_duration")
  
  // Benchmark configuration
  let benchmark_iterations = 100
  let spans_per_iteration = 50
  let metrics_per_iteration = 20
  let logs_per_iteration = 10
  
  // Run benchmark
  let benchmark_start_time = Time::now()
  
  for iteration in 0..benchmark_iterations {
    let iteration_start_time = Time::now()
    
    // Create spans
    for i in 0..spans_per_iteration {
      let span = Tracer::start_span(tracer, "benchmark_span_" + i.to_string())
      Span::add_attribute(span, "iteration", iteration.to_string())
      Span::add_attribute(span, "span_index", i.to_string())
      
      // Add events to some spans
      if i % 5 == 0 {
        Span::add_event(span, "benchmark_event", Some([
          ("event_iteration", iteration.to_string()),
          ("event_span_index", i.to_string())
        ]))
      }
      
      Span::end(span)
      Counter::add(span_counter, 1.0)
    }
    
    // Create metrics
    for i in 0..metrics_per_iteration {
      let attrs = Attributes::new()
      Attributes::set(attrs, "iteration", iteration.to_string())
      Attributes::set(attrs, "metric_index", i.to_string())
      
      Counter::add(span_counter, 1.0, Some(attrs))
      Histogram::record(end_to_end_timer, (iteration * 10 + i).to_float(), Some(attrs))
    }
    
    // Create logs
    for i in 0..logs_per_iteration {
      let severity = match i % 4 {
        0 => Debug
        1 => Info
        2 => Warning
        _ => Error
      }
      
      let log_record = Logger::create_log_record(logger, severity, "Benchmark log " + i.to_string())
      LogRecord::add_attribute(log_record, "iteration", iteration.to_string())
      LogRecord::add_attribute(log_record, "log_index", i.to_string())
      
      Logger::emit(logger, log_record)
      Counter::add(log_counter, 1.0)
    }
    
    let iteration_time = Time::now() - iteration_start_time
    Histogram::record(end_to_end_timer, iteration_time.to_milliseconds().to_float())
    
    // Force export every 10 iterations
    if iteration % 10 == 0 {
      TelemetryProvider::force_flush(provider)
    }
  }
  
  // Final export
  TelemetryProvider::force_flush(provider)
  
  let benchmark_total_time = Time::now() - benchmark_start_time
  
  // Verify all telemetry data was created
  let spans = TelemetryProvider::get_collected_spans(provider)
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  let logs = TelemetryProvider::get_collected_logs(provider)
  
  assert_eq(spans.length(), benchmark_iterations * spans_per_iteration)
  assert_true(metrics.length() >= benchmark_iterations * metrics_per_iteration) // May include internal metrics
  assert_eq(logs.length(), benchmark_iterations * logs_per_iteration)
  
  // Calculate overall performance metrics
  let total_operations = benchmark_iterations * (spans_per_iteration + metrics_per_iteration + logs_per_iteration)
  let overall_throughput = total_operations.to_float() / benchmark_total_time.to_seconds()
  
  // Performance assertions
  assert_true(overall_throughput > 100.0) // At least 100 operations per second overall
  
  // Log performance metrics
  let perf_meter = TelemetryProvider::get_meter(provider, "benchmark_metrics")
  let throughput_gauge = Meter::create_gauge(perf_meter, "overall_throughput")
  let total_time_gauge = Meter::create_gauge(perf_meter, "total_benchmark_time")
  let total_operations_gauge = Meter::create_gauge(perf_meter, "total_operations")
  
  Gauge::set(throughput_gauge, overall_throughput)
  Gauge::set(total_time_gauge, benchmark_total_time.to_milliseconds().to_float())
  Gauge::set(total_operations_gauge, total_operations.to_float())
  
  // Calculate average times for each operation type
  let avg_span_time = benchmark_total_time.to_milliseconds().to_float() / (benchmark_iterations * spans_per_iteration).to_float()
  let avg_metric_time = benchmark_total_time.to_milliseconds().to_float() / (benchmark_iterations * metrics_per_iteration).to_float()
  let avg_log_time = benchmark_total_time.to_milliseconds().to_float() / (benchmark_iterations * logs_per_iteration).to_float()
  
  let span_time_gauge = Meter::create_gauge(perf_meter, "avg_span_time_ms")
  let metric_time_gauge = Meter::create_gauge(perf_meter, "avg_metric_time_ms")
  let log_time_gauge = Meter::create_gauge(perf_meter, "avg_log_time_ms")
  
  Gauge::set(span_time_gauge, avg_span_time)
  Gauge::set(metric_time_gauge, avg_metric_time)
  Gauge::set(log_time_gauge, avg_log_time)
}