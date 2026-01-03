// Azimuth Telemetry System - Enhanced Performance Benchmark Tests
// This file contains comprehensive test cases for performance benchmarking

// Test 1: Span Creation Performance Benchmark
test "span creation performance benchmark" {
  let benchmark = PerformanceBenchmark::new("span_creation")
  
  // Benchmark span creation with different attribute counts
  let attribute_counts = [0, 5, 10, 25, 50, 100]
  
  for attr_count in attribute_counts {
    let timer = Timer::start()
    
    // Create spans with varying attribute counts
    for i in 0..=1000 {
      let trace_id = "trace_" + Int::to_string(i)
      let span_id = "span_" + Int::to_string(i)
      let span_ctx = SpanContext::new(trace_id, span_id, true, "benchmark_service")
      let span = Span::new("benchmark_operation", Server, span_ctx)
      
      // Add attributes
      for j in 0..=attr_count {
        let key = "attr_" + Int::to_string(j)
        let value = "value_" + Int::to_string(j)
        Span::set_attribute(span, key, StringValue(value))
      }
      
      Span::end(span)
    }
    
    let elapsed = Timer::elapsed(timer)
    PerformanceBenchmark::record_metric(benchmark, "span_creation_" + Int::to_string(attr_count) + "_attrs", elapsed)
    
    // Performance assertions
    let avg_time_per_span = elapsed / 1000.0
    if attr_count <= 10 {
      assert_true(avg_time_per_span < 0.001) // Less than 1ms per span for low attribute count
    } else if attr_count <= 50 {
      assert_true(avg_time_per_span < 0.005) // Less than 5ms per span for medium attribute count
    } else {
      assert_true(avg_time_per_span < 0.01) // Less than 10ms per span for high attribute count
    }
  }
  
  // Verify performance doesn't degrade significantly with more attributes
  let baseline_time = PerformanceBenchmark::get_metric(benchmark, "span_creation_0_attrs")
  let high_attr_time = PerformanceBenchmark::get_metric(benchmark, "span_creation_100_attrs")
  
  match (baseline_time, high_attr_time) {
    (Some(base), Some(high)) => {
      let degradation_ratio = high / base
      assert_true(degradation_ratio < 10.0) // Should not be more than 10x slower
    }
    _ => assert_true(false)
  }
}

// Test 2: Metrics Recording Performance Benchmark
test "metrics recording performance benchmark" {
  let benchmark = PerformanceBenchmark::new("metrics_recording")
  
  // Benchmark different metric types
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "benchmark_meter")
  
  // Create different metric instruments
  let counter = Meter::create_counter(meter, "benchmark_counter", Some("Benchmark counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "benchmark_histogram", Some("Benchmark histogram"), Some("ms"))
  let gauge = Meter::create_gauge(meter, "benchmark_gauge", Some("Benchmark gauge"), Some("value"))
  let updown_counter = Meter::create_updown_counter(meter, "benchmark_updown", Some("Benchmark updown"), Some("value"))
  
  // Benchmark counter operations
  let counter_timer = Timer::start()
  for i in 0..=10000 {
    Counter::add(counter, 1.0)
  }
  let counter_elapsed = Timer::elapsed(counter_timer)
  PerformanceBenchmark::record_metric(benchmark, "counter_operations", counter_elapsed)
  
  // Benchmark histogram operations
  let histogram_timer = Timer::start()
  for i in 0..=10000 {
    Histogram::record(histogram, Int::to_float(i % 1000))
  }
  let histogram_elapsed = Timer::elapsed(histogram_timer)
  PerformanceBenchmark::record_metric(benchmark, "histogram_operations", histogram_elapsed)
  
  // Benchmark gauge operations
  let gauge_timer = Timer::start()
  for i in 0..=10000 {
    Gauge::set(gauge, Int::to_float(i % 100))
  }
  let gauge_elapsed = Timer::elapsed(gauge_timer)
  PerformanceBenchmark::record_metric(benchmark, "gauge_operations", gauge_elapsed)
  
  // Benchmark updown counter operations
  let updown_timer = Timer::start()
  for i in 0..=10000 {
    if i % 2 == 0 {
      UpDownCounter::add(updown_counter, 1.0)
    } else {
      UpDownCounter::add(updown_counter, -1.0)
    }
  }
  let updown_elapsed = Timer::elapsed(updown_timer)
  PerformanceBenchmark::record_metric(benchmark, "updown_operations", updown_elapsed)
  
  // Performance assertions
  let counter_avg = counter_elapsed / 10000.0
  let histogram_avg = histogram_elapsed / 10000.0
  let gauge_avg = gauge_elapsed / 10000.0
  let updown_avg = updown_elapsed / 10000.0
  
  // All operations should be fast
  assert_true(counter_avg < 0.0001) // Less than 0.1ms per operation
  assert_true(histogram_avg < 0.0001) // Less than 0.1ms per operation
  assert_true(gauge_avg < 0.0001) // Less than 0.1ms per operation
  assert_true(updown_avg < 0.0001) // Less than 0.1ms per operation
}

// Test 3: Trace Context Propagation Performance Benchmark
test "trace context propagation performance benchmark" {
  let benchmark = PerformanceBenchmark::new("context_propagation")
  
  // Create parent context
  let trace_id = "5af7651916cd43dd8448eb211c8031a1"
  let parent_span_id = "r7ad6b7169203341"
  let parent_ctx = SpanContext::new(trace_id, parent_span_id, true, "parent_service")
  
  // Benchmark context injection
  let injection_timer = Timer::start()
  for i in 0..=10000 {
    let headers = TracePropagator::inject(parent_ctx)
    // Simulate header processing
    let header_count = headers.length()
    assert_true(header_count >= 1)
  }
  let injection_elapsed = Timer::elapsed(injection_timer)
  PerformanceBenchmark::record_metric(benchmark, "context_injection", injection_elapsed)
  
  // Benchmark context extraction
  let headers = [
    ("traceparent", "00-" + trace_id + "-" + parent_span_id + "-01"),
    ("tracestate", "key1=value1,key2=value2")
  ]
  
  let extraction_timer = Timer::start()
  for i in 0..=10000 {
    let extracted_ctx = TracePropagator::extract(headers)
    assert_eq(SpanContext::trace_id(extracted_ctx), trace_id)
    assert_eq(SpanContext::span_id(extracted_ctx), parent_span_id)
  }
  let extraction_elapsed = Timer::elapsed(extraction_timer)
  PerformanceBenchmark::record_metric(benchmark, "context_extraction", extraction_elapsed)
  
  // Performance assertions
  let injection_avg = injection_elapsed / 10000.0
  let extraction_avg = extraction_elapsed / 10000.0
  
  assert_true(injection_avg < 0.0001) // Less than 0.1ms per injection
  assert_true(extraction_avg < 0.0001) // Less than 0.1ms per extraction
}

// Test 4: Memory Usage Benchmark
test "memory usage benchmark" {
  let benchmark = PerformanceBenchmark::new("memory_usage")
  
  // Measure baseline memory
  let baseline_memory = MemoryProfiler::get_current_usage()
  
  // Create spans and measure memory growth
  let spans = []
  for i in 0..=1000 {
    let trace_id = "trace_" + Int::to_string(i)
    let span_id = "span_" + Int::to_string(i)
    let span_ctx = SpanContext::new(trace_id, span_id, true, "memory_test_service")
    let span = Span::new("memory_test_operation", Server, span_ctx)
    
    // Add attributes and events
    for j in 0..=10 {
      let key = "attr_" + Int::to_string(j)
      let value = "value_" + Int::to_string(j)
      Span::set_attribute(span, key, StringValue(value))
    }
    
    for j in 0..=5 {
      let event_name = "event_" + Int::to_string(j)
      Span::add_event(span, event_name, None)
    }
    
    spans.push(span)
  }
  
  let after_creation_memory = MemoryProfiler::get_current_usage()
  let memory_increase = after_creation_memory - baseline_memory
  PerformanceBenchmark::record_metric(benchmark, "span_creation_memory", Int::to_float(memory_increase))
  
  // End all spans and measure memory after cleanup
  for span in spans {
    Span::end(span)
  }
  
  // Force garbage collection if available
  MemoryProfiler::force_gc()
  
  let after_cleanup_memory = MemoryProfiler::get_current_usage()
  let final_memory_increase = after_cleanup_memory - baseline_memory
  PerformanceBenchmark::record_metric(benchmark, "after_cleanup_memory", Int::to_float(final_memory_increase))
  
  // Memory usage assertions
  // Each span should use reasonable amount of memory
  let memory_per_span = Int::to_float(memory_increase) / 1000.0
  assert_true(memory_per_span < 1024.0) // Less than 1KB per span
  
  // Memory should be significantly reduced after cleanup
  let cleanup_efficiency = 1.0 - (Int::to_float(final_memory_increase) / Int::to_float(memory_increase))
  assert_true(cleanup_efficiency > 0.7) // At least 70% of memory should be reclaimed
}

// Test 5: Concurrent Operations Performance Benchmark
test "concurrent operations performance benchmark" {
  let benchmark = PerformanceBenchmark::new("concurrent_operations")
  
  // Benchmark concurrent span creation
  let concurrent_span_timer = Timer::start()
  
  // Simulate concurrent operations
  let concurrent_tasks = []
  for task_id in 0..=10 {
    let task = ConcurrentTask::spawn({
      let task_timer = Timer::start()
      
      for i in 0..=100 {
        let trace_id = "trace_" + Int::to_string(task_id) + "_" + Int::to_string(i)
        let span_id = "span_" + Int::to_string(task_id) + "_" + Int::to_string(i)
        let span_ctx = SpanContext::new(trace_id, span_id, true, "concurrent_service")
        let span = Span::new("concurrent_operation", Server, span_ctx)
        
        // Add some attributes
        Span::set_attribute(span, "task_id", IntValue(task_id))
        Span::set_attribute(span, "iteration", IntValue(i))
        
        Span::end(span)
      }
      
      Timer::elapsed(task_timer)
    })
    
    concurrent_tasks.push(task)
  }
  
  // Wait for all tasks to complete
  let task_times = []
  for task in concurrent_tasks {
    let task_time = ConcurrentTask::join(task)
    task_times.push(task_time)
  }
  
  let concurrent_span_elapsed = Timer::elapsed(concurrent_span_timer)
  PerformanceBenchmark::record_metric(benchmark, "concurrent_span_creation", concurrent_span_elapsed)
  
  // Calculate statistics
  let total_spans = 11 * 100 // 11 tasks * 100 spans each
  let avg_time_per_span = concurrent_span_elapsed / Int::to_float(total_spans)
  let max_task_time = Array::max(task_times)
  let min_task_time = Array::min(task_times)
  
  // Performance assertions
  assert_true(avg_time_per_span < 0.002) // Less than 2ms per span under concurrency
  
  // Check load balancing - tasks should complete in similar timeframes
  let time_variance = max_task_time - min_task_time
  let time_variance_ratio = time_variance / max_task_time
  assert_true(time_variance_ratio < 0.5) // Variance should be less than 50% of max time
  
  // Benchmark concurrent metrics operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent_benchmark_meter")
  let counter = Meter::create_counter(meter, "concurrent_counter", Some("Concurrent counter"), Some("count"))
  
  let concurrent_metrics_timer = Timer::start()
  
  let metrics_tasks = []
  for task_id in 0..=10 {
    let task = ConcurrentTask::spawn({
      for i in 0..=1000 {
        Counter::add(counter, 1.0)
      }
    })
    
    metrics_tasks.push(task)
  }
  
  // Wait for all metrics tasks to complete
  for task in metrics_tasks {
    ConcurrentTask::join(task)
  }
  
  let concurrent_metrics_elapsed = Timer::elapsed(concurrent_metrics_timer)
  PerformanceBenchmark::record_metric(benchmark, "concurrent_metrics_operations", concurrent_metrics_elapsed)
  
  // Performance assertions for metrics
  let total_operations = 11 * 1000 // 11 tasks * 1000 operations each
  let avg_time_per_operation = concurrent_metrics_elapsed / Int::to_float(total_operations)
  assert_true(avg_time_per_operation < 0.00001) // Less than 0.01ms per operation under concurrency
}

// Test 6: High Throughput Scenario Benchmark
test "high throughput scenario benchmark" {
  let benchmark = PerformanceBenchmark::new("high_throughput")
  
  // Simulate high-throughput scenario
  let scenario_timer = Timer::start()
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "throughput_meter")
  
  // Create instruments
  let request_counter = Meter::create_counter(meter, "requests", Some("Request count"), Some("count"))
  let latency_histogram = Meter::create_histogram(meter, "latency", Some("Request latency"), Some("ms"))
  let active_connections_gauge = Meter::create_gauge(meter, "active_connections", Some("Active connections"), Some("connections"))
  
  // Simulate high throughput
  let total_requests = 100000
  let concurrent_connections = 100
  
  // Set initial active connections
  Gauge::set(active_connections_gauge, Int::to_float(concurrent_connections))
  
  // Simulate requests
  for i in 0..=total_requests {
    // Increment request counter
    Counter::add(request_counter, 1.0)
    
    // Record latency (simulate varying response times)
    let latency = 10.0 + (Int::to_float(i % 1000) / 10.0)
    Histogram::record(latency_histogram, latency)
    
    // Create span for request
    let trace_id = "trace_" + Int::to_string(i)
    let span_id = "span_" + Int::to_string(i)
    let span_ctx = SpanContext::new(trace_id, span_id, i % 10 == 0, "throughput_service")
    let span = Span::new("request_processing", Server, span_ctx)
    
    // Add request attributes
    Span::set_attribute(span, "request.id", IntValue(i))
    Span::set_attribute(span, "request.latency", FloatValue(latency))
    
    // End span
    Span::end(span)
    
    // Simulate connection churn
    if i % 1000 == 0 {
      let new_connections = concurrent_connections + (i % 20 - 10)
      Gauge::set(active_connections_gauge, Int::to_float(new_connections))
    }
  }
  
  let scenario_elapsed = Timer::elapsed(scenario_timer)
  PerformanceBenchmark::record_metric(benchmark, "high_throughput_scenario", scenario_elapsed)
  
  // Calculate throughput metrics
  let requests_per_second = Int::to_float(total_requests) / scenario_elapsed
  let avg_time_per_request = scenario_elapsed / Int::to_float(total_requests)
  
  // Performance assertions
  assert_true(requests_per_second > 10000.0) // Should handle at least 10k requests per second
  assert_true(avg_time_per_request < 0.0001) // Less than 0.1ms per request
  
  // Verify metrics were recorded correctly
  let counter_value = Counter::get_value(request_counter)
  match counter_value {
    Some(value) => assert_eq(value, Int::to_float(total_requests))
    None => assert_true(false)
  }
  
  let histogram_count = Histogram::get_count(latency_histogram)
  match histogram_count {
    Some(count) => assert_eq(count, total_requests)
    None => assert_true(false)
  }
}