// Azimuth Telemetry System - Performance Benchmark Comprehensive Tests
// This file contains comprehensive performance benchmark tests for the telemetry system

// Test 1: Telemetry Collection Performance
test "telemetry collection performance benchmark" {
  let benchmark_runner = BenchmarkRunner::new()
  
  // Benchmark telemetry collection with different data sizes
  let data_sizes = [100, 1000, 10000, 100000]
  
  for size in data_sizes {
    let result = benchmark_runner.run_benchmark("telemetry_collection_" + size.to_string(), fn() {
      let collector = TelemetryCollector::new()
      
      for i in 0..=size {
        let metric = Metric::counter("test_metric", "Test metric")
        Metric::add(metric, i as Float)
        TelemetryCollector::collect_metrics(collector)
      }
    })
    
    // Verify performance metrics
    assert_true(result.execution_time_ms > 0)
    assert_true(result.memory_usage_bytes > 0)
    
    // Performance should degrade gracefully
    let ops_per_second = size as Float / (result.execution_time_ms as Float / 1000.0)
    assert_true(ops_per_second > 1000)  // Minimum 1000 ops/sec
    
    println!("Collection size: " + size.to_string() + 
            ", Time: " + result.execution_time_ms.to_string() + "ms" +
            ", Ops/sec: " + ops_per_second.to_string())
  }
}

// Test 2: Span Creation Performance
test "span creation performance benchmark" {
  let benchmark_runner = BenchmarkRunner::new()
  
  // Benchmark span creation with different attributes
  let attribute_counts = [0, 5, 10, 25, 50]
  
  for attr_count in attribute_counts {
    let result = benchmark_runner.run_benchmark("span_creation_" + attr_count.to_string() + "_attrs", fn() {
      let trace_id = "0af7651916cd43dd8448eb211c80319c"
      let span_ctx = SpanContext::new(trace_id, "span_id", true, "")
      
      for i in 0..=1000 {
        let span = Span::new("test_span_" + i.to_string(), Internal, span_ctx)
        
        // Add attributes
        for j in 0..=attr_count {
          Span::set_attribute(span, "attr_" + j.to_string(), "value_" + j.to_string())
        }
        
        Span::end(span)
      }
    })
    
    // Verify performance metrics
    assert_true(result.execution_time_ms > 0)
    
    let spans_per_second = 1000.0 / (result.execution_time_ms as Float / 1000.0)
    
    // Performance should be reasonable even with many attributes
    if attr_count <= 10 {
      assert_true(spans_per_second > 10000)  // 10000+ spans/sec for <=10 attributes
    } else {
      assert_true(spans_per_second > 1000)   // 1000+ spans/sec for >10 attributes
    }
    
    println!("Span creation with " + attr_count.to_string() + " attrs: " +
            spans_per_second.to_string() + " spans/sec")
  }
}

// Test 3: Metric Aggregation Performance
test "metric aggregation performance benchmark" {
  let benchmark_runner = BenchmarkRunner::new()
  
  // Benchmark different aggregation strategies
  let aggregation_strategies = ["sum", "avg", "min", "max", "histogram"]
  
  for strategy in aggregation_strategies {
    let result = benchmark_runner.run_benchmark("aggregation_" + strategy, fn() {
      let aggregator = TelemetryAggregator::new()
      
      // Add data points
      for i in 0..=10000 {
        let data_point = DataPoint::new("test_metric", (i % 100) as Float, i)
        TelemetryAggregator::add_point(aggregator, data_point)
      }
      
      // Perform aggregation
      match strategy {
        "sum" => TelemetryAggregator::sum(aggregator, "test_metric"),
        "avg" => TelemetryAggregator::average(aggregator, "test_metric"),
        "min" => TelemetryAggregator::min(aggregator, "test_metric"),
        "max" => TelemetryAggregator::max(aggregator, "test_metric"),
        "histogram" => TelemetryAggregator::histogram(aggregator, "test_metric", 10),
        _ => None
      }
    })
    
    // Verify performance metrics
    assert_true(result.execution_time_ms > 0)
    
    let ops_per_second = 10000.0 / (result.execution_time_ms as Float / 1000.0)
    assert_true(ops_per_second > 5000)  // Minimum 5000 ops/sec
    
    println!("Aggregation strategy " + strategy + ": " + 
            ops_per_second.to_string() + " ops/sec")
  }
}

// Test 4: Trace Propagation Performance
test "trace propagation performance benchmark" {
  let benchmark_runner = BenchmarkRunner::new()
  
  // Benchmark trace context injection and extraction
  let propagation_scenarios = ["http_headers", "grpc_metadata", "messaging_headers"]
  
  for scenario in propagation_scenarios {
    let result = benchmark_runner.run_benchmark("propagation_" + scenario, fn() {
      let propagator = TracePropagator::new()
      let span_ctx = SpanContext::new("trace_id", "span_id", true, "trace_state")
      
      for i in 0..=5000 {
        let headers = []
        
        // Inject context
        let injected = match scenario {
          "http_headers" => TracePropagator::inject_to_http(propagator, span_ctx, headers),
          "grpc_metadata" => TracePropagator::inject_to_grpc(propagator, span_ctx, headers),
          "messaging_headers" => TracePropagator::inject_to_messaging(propagator, span_ctx, headers),
          _ => headers
        }
        
        // Extract context
        let _ = match scenario {
          "http_headers" => TracePropagator::extract_from_http(propagator, injected),
          "grpc_metadata" => TracePropagator::extract_from_grpc(propagator, injected),
          "messaging_headers" => TracePropagator::extract_from_messaging(propagator, injected),
          _ => span_ctx
        }
      }
    })
    
    // Verify performance metrics
    assert_true(result.execution_time_ms > 0)
    
    let ops_per_second = 5000.0 / (result.execution_time_ms as Float / 1000.0)
    assert_true(ops_per_second > 10000)  // Minimum 10000 ops/sec
    
    println!("Trace propagation (" + scenario + "): " + 
            ops_per_second.to_string() + " ops/sec")
  }
}

// Test 5: Memory Usage Under Load
test "memory usage under load benchmark" {
  let memory_profiler = MemoryProfiler::new()
  
  // Profile memory usage with increasing load
  let load_levels = [1000, 5000, 10000, 50000]
  
  for load in load_levels {
    let initial_memory = memory_profiler.get_current_memory_usage()
    
    // Generate load
    let collector = TelemetryCollector::new()
    for i in 0..=load {
      let span = Span::new("load_test_span_" + i.to_string(), Internal, 
        SpanContext::new("trace_id", "span_id", true, ""))
      
      // Add events and attributes
      for j in 0..=10 {
        Span::add_event(span, "event_" + j.to_string(), Some([
          ("iteration", i.to_string()),
          ("event_count", j.to_string())
        ]))
      }
      
      TelemetryCollector::add_span(collector, span)
    }
    
    let peak_memory = memory_profiler.get_peak_memory_usage()
    
    // Force garbage collection if available
    memory_profiler.force_gc()
    let final_memory = memory_profiler.get_current_memory_usage()
    
    // Calculate memory metrics
    let memory_per_operation = (peak_memory - initial_memory) as Float / load as Float
    let memory_leak = final_memory - initial_memory
    
    // Memory usage should be reasonable
    assert_true(memory_per_operation < 1024)  // Less than 1KB per operation
    
    // Memory leak should be minimal
    let leak_percentage = memory_leak as Float / (peak_memory - initial_memory) as Float * 100.0
    assert_true(leak_percentage < 10.0)  // Less than 10% leak
    
    println!("Load: " + load.to_string() + 
            ", Memory/op: " + memory_per_operation.to_string() + " bytes" +
            ", Leak: " + leak_percentage.to_string() + "%")
  }
}

// Test 6: Concurrent Performance
test "concurrent performance benchmark" {
  let concurrent_benchmark = ConcurrentBenchmark::new()
  
  // Benchmark with different thread counts
  let thread_counts = [1, 2, 4, 8, 16]
  
  for thread_count in thread_counts {
    let result = concurrent_benchmark.run_concurrent_benchmark(
      "concurrent_ops_" + thread_count.to_string(), 
      thread_count, 
      fn() {
        // Simulate telemetry operations
        let span = Span::new("concurrent_span", Internal, 
          SpanContext::new("trace_id", "span_id", true, ""))
        
        // Add metrics
        let counter = Metric::counter("concurrent_counter", "Concurrent counter")
        Metric::add(counter, 1.0)
        
        // Add events
        Span::add_event(span, "concurrent_event", Some([
          ("thread_id", "thread_" + thread_count.to_string())
        ]))
        
        Span::end(span)
      }
    )
    
    // Verify performance metrics
    assert_true(result.total_execution_time_ms > 0)
    
    let total_ops = thread_count * 1000
    let throughput = total_ops as Float / (result.total_execution_time_ms as Float / 1000.0)
    
    // Throughput should scale reasonably with thread count
    let expected_min_throughput = thread_count as Float * 500.0  // 500 ops/sec per thread
    assert_true(throughput > expected_min_throughput)
    
    // Check for contention
    let efficiency = throughput / (thread_count as Float * 1000.0) * 100.0
    assert_true(efficiency > 20.0)  // At least 20% efficiency
    
    println!("Threads: " + thread_count.to_string() + 
            ", Throughput: " + throughput.to_string() + " ops/sec" +
            ", Efficiency: " + efficiency.to_string() + "%")
  }
}

// Test 7: Data Serialization Performance
test "data serialization performance benchmark" {
  let benchmark_runner = BenchmarkRunner::new()
  
  // Benchmark different serialization formats
  let serialization_formats = ["json", "protobuf", "binary", "csv"]
  
  for format in serialization_formats {
    let result = benchmark_runner.run_benchmark("serialization_" + format, fn() {
      // Create test data
      let telemetry_data = []
      for i in 0..=1000 {
        let data = TelemetryData::new(
          "service_" + (i % 10).to_string(),
          "operation_" + (i % 5).to_string(),
          200 + (i % 300),
          50 + (i % 200),
          i % 10 != 0
        )
        telemetry_data = telemetry_data.push(data)
      }
      
      // Serialize data
      let _ = match format {
        "json" => TelemetrySerializer::to_json(telemetry_data),
        "protobuf" => TelemetrySerializer::to_protobuf(telemetry_data),
        "binary" => TelemetrySerializer::to_binary(telemetry_data),
        "csv" => TelemetrySerializer::to_csv(telemetry_data),
        _ => ""
      }
    })
    
    // Verify performance metrics
    assert_true(result.execution_time_ms > 0)
    
    let ops_per_second = 1000.0 / (result.execution_time_ms as Float / 1000.0)
    assert_true(ops_per_second > 100)  // Minimum 100 ops/sec
    
    println!("Serialization (" + format + "): " + 
            ops_per_second.to_string() + " ops/sec")
  }
}

// Test 8: Batch Processing Performance
test "batch processing performance benchmark" {
  let benchmark_runner = BenchmarkRunner::new()
  
  // Benchmark different batch sizes
  let batch_sizes = [10, 50, 100, 500, 1000]
  
  for batch_size in batch_sizes {
    let result = benchmark_runner.run_benchmark("batch_processing_" + batch_size.to_string(), fn() {
      let batch_processor = BatchProcessor::new(batch_size)
      
      // Process multiple batches
      for batch_idx in 0..=100 {
        let batch = []
        for i in 0..=batch_size {
          let data = TelemetryData::new(
            "service",
            "operation",
            200,
            50,
            true
          )
          batch = batch.push(data)
        }
        
        BatchProcessor::process(batch_processor, batch)
      }
    })
    
    // Verify performance metrics
    assert_true(result.execution_time_ms > 0)
    
    let total_records = batch_size * 100
    let records_per_second = total_records as Float / (result.execution_time_ms as Float / 1000.0)
    
    // Batch processing should be efficient
    assert_true(records_per_second > 10000)  // Minimum 10000 records/sec
    
    println!("Batch size " + batch_size.to_string() + ": " + 
            records_per_second.to_string() + " records/sec")
  }
}

// Test 9: Filtering and Querying Performance
test "filtering and querying performance benchmark" {
  let benchmark_runner = BenchmarkRunner::new()
  
  // Create large dataset for filtering
  let large_dataset = []
  for i in 0..=50000 {
    let data = TelemetryData::new(
      "service_" + (i % 20).to_string(),
      "operation_" + (i % 10).to_string(),
      200 + (i % 300),
      50 + (i % 200),
      i % 10 != 0
    )
    large_dataset = large_dataset.push(data)
  }
  
  // Benchmark different filter types
  let filter_types = ["service_name", "status_code", "response_time", "complex"]
  
  for filter_type in filter_types {
    let result = benchmark_runner.run_benchmark("filtering_" + filter_type, fn() {
      let filter = TelemetryFilter::new()
      
      for i in 0..=100 {
        let _ = match filter_type {
          "service_name" => TelemetryFilter::by_service(filter, large_dataset, "service_5"),
          "status_code" => TelemetryFilter::by_status(filter, large_dataset, 200),
          "response_time" => TelemetryFilter::by_response_time(filter, large_dataset, 100),
          "complex" => {
            let filtered = TelemetryFilter::by_service(filter, large_dataset, "service_5")
            TelemetryFilter::by_status(filter, filtered, 200)
          }
          _ => large_dataset
        }
      }
    })
    
    // Verify performance metrics
    assert_true(result.execution_time_ms > 0)
    
    let ops_per_second = 100.0 / (result.execution_time_ms as Float / 1000.0)
    assert_true(ops_per_second > 1000)  // Minimum 1000 ops/sec
    
    println!("Filtering (" + filter_type + "): " + 
            ops_per_second.to_string() + " ops/sec")
  }
}

// Test 10: End-to-End Performance
test "end-to-end performance benchmark" {
  let benchmark_runner = BenchmarkRunner::new()
  
  // Benchmark complete telemetry pipeline
  let result = benchmark_runner.run_benchmark("end_to_end_pipeline", fn() {
    // Initialize components
    let collector = TelemetryCollector::new()
    let processor = TelemetryProcessor::new()
    let aggregator = TelemetryAggregator::new()
    let exporter = TelemetryExporter::new()
    
    // Simulate realistic workload
    for i in 0..=1000 {
      // Create span with attributes and events
      let span = Span::new("operation_" + (i % 10).to_string(), Internal, 
        SpanContext::new("trace_" + (i % 100).to_string(), "span_" + i.to_string(), true, ""))
      
      // Add attributes
      Span::set_attribute(span, "service.name", "service_" + (i % 5).to_string())
      Span::set_attribute(span, "operation.type", "type_" + (i % 3).to_string())
      
      // Add events
      for j in 0..=5 {
        Span::add_event(span, "event_" + j.to_string(), Some([
          ("event_data", (i * j).to_string())
        ]))
      }
      
      // Record metrics
      let counter = Metric::counter("operation_count", "Operation counter")
      Metric::add(counter, 1.0)
      
      let histogram = Metric::histogram("operation_duration", "Operation duration", "ms")
      Metric::record(histogram, 50.0 + (i % 100) as Float)
      
      // Process through pipeline
      TelemetryCollector::add_span(collector, span)
      TelemetryProcessor::process_span(processor, span)
      
      // Periodic aggregation
      if i % 100 == 0 {
        let metrics = TelemetryCollector::collect_metrics(collector)
        TelemetryAggregator::add_metrics(aggregator, metrics)
        
        // Export data
        let telemetry_data = TelemetryCollector::get_all_data(collector)
        let _ = TelemetryExporter::to_json(exporter, telemetry_data)
      }
    }
    
    // Final aggregation and export
    let final_metrics = TelemetryCollector::collect_metrics(collector)
    TelemetryAggregator::add_metrics(aggregator, final_metrics)
    
    let final_data = TelemetryCollector::get_all_data(collector)
    let _ = TelemetryExporter::to_json(exporter, final_data)
  })
  
  // Verify performance metrics
  assert_true(result.execution_time_ms > 0)
  
  let ops_per_second = 1000.0 / (result.execution_time_ms as Float / 1000.0)
  assert_true(ops_per_second > 100)  // Minimum 100 ops/sec for end-to-end
  
  // Memory usage should be reasonable
  assert_true(result.memory_usage_bytes < 100 * 1024 * 1024)  // Less than 100MB
  
  println!("End-to-end pipeline: " + ops_per_second.to_string() + " ops/sec" +
          ", Memory: " + (result.memory_usage_bytes / 1024 / 1024).to_string() + "MB")
}