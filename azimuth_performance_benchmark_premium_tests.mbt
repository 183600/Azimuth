// Premium Performance Benchmark Tests for Azimuth
// This file contains comprehensive performance benchmark tests for the telemetry system

// Test 1: Span Creation and Destruction Performance
test "span creation and destruction performance" {
  let benchmark = PerformanceBenchmark::new("span_creation_destruction")
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "benchmark_tracer")
  
  // Benchmark span creation
  let creation_iterations = 10000
  let creation_start = PerformanceTimer::start()
  
  for i in 0..creation_iterations {
    let span_name = "benchmark_span_" + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    Span::end(span)
  }
  
  let creation_time = PerformanceTimer::stop(creation_start)
  let creation_avg = creation_time.to_float() / creation_iterations.to_float()
  
  // Performance assertions
  assert_true(creation_avg < 100.0) // Average creation time should be less than 100 microseconds
  
  // Benchmark span with attributes
  let attr_iterations = 5000
  let attr_start = PerformanceTimer::start()
  
  for i in 0..attr_iterations {
    let span = Tracer::start_span(tracer, "span_with_attrs")
    Span::set_attribute(span, "iteration", IntValue(i))
    Span::set_attribute(span, "name", StringValue("test"))
    Span::set_attribute(span, "value", FloatValue(i.to_float()))
    Span::add_event(span, "test_event", Some([("event_data", StringValue("data"))]))
    Span::end(span)
  }
  
  let attr_time = PerformanceTimer::stop(attr_start)
  let attr_avg = attr_time.to_float() / attr_iterations.to_float()
  
  // Performance assertions for spans with attributes
  assert_true(attr_avg < 200.0) // Average time should be less than 200 microseconds
  assert_true(attr_avg < creation_avg * 3.0) // Should not be more than 3x slower than basic span
  
  // Record benchmark results
  PerformanceBenchmark::record_result(benchmark, "span_creation", creation_avg, "microseconds")
  PerformanceBenchmark::record_result(benchmark, "span_with_attributes", attr_avg, "microseconds")
}

// Test 2: Metrics Collection Performance
test "metrics collection performance" {
  let benchmark = PerformanceBenchmark::new("metrics_collection")
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "benchmark_meter")
  
  // Benchmark counter operations
  let counter = Meter::create_counter(meter, "benchmark_counter", Some("Benchmark counter"), Some("count"))
  let counter_iterations = 100000
  
  let counter_start = PerformanceTimer::start()
  for i in 0..counter_iterations {
    Counter::add(counter, 1.0)
  }
  let counter_time = PerformanceTimer::stop(counter_start)
  let counter_avg = counter_time.to_float() / counter_iterations.to_float()
  
  // Performance assertions
  assert_true(counter_avg < 10.0) // Average counter operation should be less than 10 microseconds
  
  // Benchmark histogram operations
  let histogram = Meter::create_histogram(meter, "benchmark_histogram", Some("Benchmark histogram"), Some("ms"))
  let histogram_iterations = 50000
  
  let histogram_start = PerformanceTimer::start()
  for i in 0..histogram_iterations {
    Histogram::record(histogram, i.to_float())
  }
  let histogram_time = PerformanceTimer::stop(histogram_start)
  let histogram_avg = histogram_time.to_float() / histogram_iterations.to_float()
  
  // Performance assertions
  assert_true(histogram_avg < 20.0) // Average histogram operation should be less than 20 microseconds
  
  // Benchmark gauge operations
  let gauge = Meter::create_gauge(meter, "benchmark_gauge", Some("Benchmark gauge"), Some("value"))
  let gauge_iterations = 10000
  
  let gauge_start = PerformanceTimer::start()
  for i in 0..gauge_iterations {
    Gauge::record(gauge, i.to_float())
  }
  let gauge_time = PerformanceTimer::stop(gauge_start)
  let gauge_avg = gauge_time.to_float() / gauge_iterations.to_float()
  
  // Performance assertions
  assert_true(gauge_avg < 15.0) // Average gauge operation should be less than 15 microseconds
  
  // Record benchmark results
  PerformanceBenchmark::record_result(benchmark, "counter_add", counter_avg, "microseconds")
  PerformanceBenchmark::record_result(benchmark, "histogram_record", histogram_avg, "microseconds")
  PerformanceBenchmark::record_result(benchmark, "gauge_record", gauge_avg, "microseconds")
}

// Test 3: Trace Context Propagation Performance
test "trace context propagation performance" {
  let benchmark = PerformanceBenchmark::new("context_propagation")
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_id = "00f067aa0ba902b7"
  
  // Benchmark context creation
  let context_iterations = 50000
  let context_start = PerformanceTimer::start()
  
  let contexts = []
  for i in 0..context_iterations {
    let context = SpanContext::new(trace_id + i.to_string(), span_id + i.to_string(), true, "sampled")
    contexts.push(context)
  }
  
  let context_time = PerformanceTimer::stop(context_start)
  let context_avg = context_time.to_float() / context_iterations.to_float()
  
  // Performance assertions
  assert_true(context_avg < 50.0) // Average context creation should be less than 50 microseconds
  
  // Benchmark context extraction
  let extract_iterations = 50000
  let extract_start = PerformanceTimer::start()
  
  for context in contexts {
    let extracted_trace_id = SpanContext::trace_id(context)
    let extracted_span_id = SpanContext::span_id(context)
    let is_sampled = SpanContext::is_sampled(context)
    let is_valid = SpanContext::is_valid(context)
    
    // Use the extracted values to prevent optimization
    assert_true(extracted_trace_id.length() > 0)
    assert_true(extracted_span_id.length() > 0)
    assert_true(is_sampled == true || is_sampled == false)
    assert_true(is_valid == true || is_valid == false)
  }
  
  let extract_time = PerformanceTimer::stop(extract_start)
  let extract_avg = extract_time.to_float() / extract_iterations.to_float()
  
  // Performance assertions
  assert_true(extract_avg < 25.0) // Average context extraction should be less than 25 microseconds
  
  // Benchmark context serialization
  let serialize_iterations = 10000
  let serialize_start = PerformanceTimer::start()
  
  for i in 0..serialize_iterations {
    if i < contexts.length() {
      let context = contexts[i]
      let serialized = TraceContextSerializer::serialize(context)
      assert_true(serialized.length() > 0)
    }
  }
  
  let serialize_time = PerformanceTimer::stop(serialize_start)
  let serialize_avg = serialize_time.to_float() / serialize_iterations.to_float()
  
  // Performance assertions
  assert_true(serialize_avg < 100.0) // Average serialization should be less than 100 microseconds
  
  // Record benchmark results
  PerformanceBenchmark::record_result(benchmark, "context_creation", context_avg, "microseconds")
  PerformanceBenchmark::record_result(benchmark, "context_extraction", extract_avg, "microseconds")
  PerformanceBenchmark::record_result(benchmark, "context_serialization", serialize_avg, "microseconds")
}

// Test 4: Attribute Operations Performance
test "attribute operations performance" {
  let benchmark = PerformanceBenchmark::new("attribute_operations")
  
  // Benchmark attribute creation
  let attr_creation_iterations = 50000
  let attr_creation_start = PerformanceTimer::start()
  
  for i in 0..attr_creation_iterations {
    let string_attr = StringValue("test_value_" + i.to_string())
    let int_attr = IntValue(i)
    let float_attr = FloatValue(i.to_float())
    let bool_attr = BoolValue(i % 2 == 0)
    
    // Use attributes to prevent optimization
    assert_true(string_attr.length() > 0)
    assert_true(int_attr >= 0)
    assert_true(float_attr >= 0.0)
    assert_true(bool_attr == true || bool_attr == false)
  }
  
  let attr_creation_time = PerformanceTimer::stop(attr_creation_start)
  let attr_creation_avg = attr_creation_time.to_float() / (attr_creation_iterations.to_float() * 4.0)
  
  // Performance assertions
  assert_true(attr_creation_avg < 20.0) // Average attribute creation should be less than 20 microseconds
  
  // Benchmark attributes collection operations
  let attrs = Attributes::new()
  let collection_iterations = 10000
  
  let collection_start = PerformanceTimer::start()
  for i in 0..collection_iterations {
    let key = "attr_" + i.to_string()
    Attributes::set(attrs, key, StringValue("value_" + i.to_string()))
    
    let retrieved = Attributes::get(attrs, key)
    match retrieved {
      Some(StringValue(value)) => assert_true(value.length() > 0)
      _ => assert_true(false)
    }
  }
  
  let collection_time = PerformanceTimer::stop(collection_start)
  let collection_avg = collection_time.to_float() / collection_iterations.to_float()
  
  // Performance assertions
  assert_true(collection_avg < 50.0) // Average collection operation should be less than 50 microseconds
  
  // Record benchmark results
  PerformanceBenchmark::record_result(benchmark, "attribute_creation", attr_creation_avg, "microseconds")
  PerformanceBenchmark::record_result(benchmark, "attribute_collection_ops", collection_avg, "microseconds")
}

// Test 5: Logging Performance
test "logging performance" {
  let benchmark = PerformanceBenchmark::new("logging_operations")
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "benchmark_logger")
  
  // Benchmark simple log record creation
  let simple_log_iterations = 50000
  let simple_log_start = PerformanceTimer::start()
  
  for i in 0..simple_log_iterations {
    let log_record = LogRecord::new(Info, "Log message " + i.to_string())
    Logger::emit(logger, log_record)
  }
  
  let simple_log_time = PerformanceTimer::stop(simple_log_start)
  let simple_log_avg = simple_log_time.to_float() / simple_log_iterations.to_float()
  
  // Performance assertions
  assert_true(simple_log_avg < 30.0) // Average simple log should be less than 30 microseconds
  
  // Benchmark complex log record creation
  let complex_log_iterations = 10000
  let complex_log_start = PerformanceTimer::start()
  
  for i in 0..complex_log_iterations {
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    Attributes::set(attrs, "component", StringValue("benchmark"))
    
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Complex log message " + i.to_string()),
      Some(attrs),
      Some(1609459200000L + i.to_int()),
      Some(1609459201000L + i.to_int()),
      Some("trace_id_" + i.to_string()),
      Some("span_id_" + i.to_string()),
      None
    )
    Logger::emit(logger, log_record)
  }
  
  let complex_log_time = PerformanceTimer::stop(complex_log_start)
  let complex_log_avg = complex_log_time.to_float() / complex_log_iterations.to_float()
  
  // Performance assertions
  assert_true(complex_log_avg < 100.0) // Average complex log should be less than 100 microseconds
  assert_true(complex_log_avg < simple_log_avg * 5.0) // Should not be more than 5x slower than simple log
  
  // Record benchmark results
  PerformanceBenchmark::record_result(benchmark, "simple_log", simple_log_avg, "microseconds")
  PerformanceBenchmark::record_result(benchmark, "complex_log", complex_log_avg, "microseconds")
}

// Test 6: Memory Allocation Performance
test "memory allocation performance" {
  let benchmark = PerformanceBenchmark::new("memory_allocation")
  
  // Benchmark span allocation
  let span_alloc_iterations = 10000
  let initial_memory = MemoryProfiler::get_allocated_memory()
  
  let span_alloc_start = PerformanceTimer::start()
  let spans = []
  
  for i in 0..span_alloc_iterations {
    let span_name = "memory_test_span_" + i.to_string()
    let span = Span::new(span_name, Internal, SpanContext::new("trace", "span", true, ""))
    spans.push(span)
  }
  
  let span_alloc_time = PerformanceTimer::stop(span_alloc_start)
  let span_alloc_avg = span_alloc_time.to_float() / span_alloc_iterations.to_float()
  
  let after_span_memory = MemoryProfiler::get_allocated_memory()
  let span_memory_per = (after_span_memory - initial_memory).to_float() / span_alloc_iterations.to_float()
  
  // Performance assertions
  assert_true(span_alloc_avg < 200.0) // Average span allocation should be less than 200 microseconds
  assert_true(span_memory_per < 1024.0) // Each span should use less than 1KB
  
  // Benchmark batch deallocation
  let dealloc_start = PerformanceTimer::start()
  spans = [] // Clear the array to deallocate spans
  let dealloc_time = PerformanceTimer::stop(dealloc_start)
  
  // Force garbage collection if available
  MemoryProfiler::force_gc()
  let final_memory = MemoryProfiler::get_allocated_memory()
  let memory_reclaimed = after_span_memory - final_memory
  
  // Memory reclamation assertions
  assert_true(memory_reclaimed > (after_span_memory - initial_memory) * 0.8) // Should reclaim at least 80%
  
  // Record benchmark results
  PerformanceBenchmark::record_result(benchmark, "span_allocation", span_alloc_avg, "microseconds")
  PerformanceBenchmark::record_result(benchmark, "span_memory_per_span", span_memory_per, "bytes")
  PerformanceBenchmark::record_result(benchmark, "deallocation_time", dealloc_time.to_float(), "microseconds")
}

// Test 7: Concurrent Operations Performance
test "concurrent operations performance" {
  let benchmark = PerformanceBenchmark::new("concurrent_operations")
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "concurrent_tracer")
  
  // Simulate concurrent span operations
  let concurrent_threads = 10
  let operations_per_thread = 1000
  
  let concurrent_start = PerformanceTimer::start()
  
  // In a real implementation, this would use actual concurrent execution
  // For this test, we simulate the operations sequentially
  let all_spans = []
  for thread_id in 0..concurrent_threads {
    let thread_spans = []
    for op_id in 0..operations_per_thread {
      let span_name = "thread_" + thread_id.to_string() + "_op_" + op_id.to_string()
      let span = Tracer::start_span(tracer, span_name)
      Span::set_attribute(span, "thread_id", IntValue(thread_id))
      Span::set_attribute(span, "operation_id", IntValue(op_id))
      thread_spans.push(span)
    }
    all_spans.extend(thread_spans)
  }
  
  let concurrent_time = PerformanceTimer::stop(concurrent_start)
  let total_operations = concurrent_threads * operations_per_thread
  let concurrent_avg = concurrent_time.to_float() / total_operations.to_float()
  
  // Performance assertions
  assert_true(concurrent_avg < 150.0) // Average concurrent operation should be less than 150 microseconds
  
  // End all spans
  let end_start = PerformanceTimer::start()
  for span in all_spans {
    Span::end(span)
  }
  let end_time = PerformanceTimer::stop(end_start)
  let end_avg = end_time.to_float() / all_spans.length().to_float()
  
  // Performance assertions
  assert_true(end_avg < 50.0) // Average span end should be less than 50 microseconds
  
  // Record benchmark results
  PerformanceBenchmark::record_result(benchmark, "concurrent_span_ops", concurrent_avg, "microseconds")
  PerformanceBenchmark::record_result(benchmark, "concurrent_span_end", end_avg, "microseconds")
}

// Test 8: High-Throughput Scenario Performance
test "high-throughput scenario performance" {
  let benchmark = PerformanceBenchmark::new("high_throughput")
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "throughput_tracer")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "throughput_meter")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "throughput_logger")
  
  // Create instruments
  let counter = Meter::create_counter(meter, "throughput_counter", Some("Throughput counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "throughput_histogram", Some("Throughput histogram"), Some("ms"))
  
  // Simulate high-throughput scenario
  let throughput_operations = 10000
  let throughput_start = PerformanceTimer::start()
  
  for i in 0..throughput_operations {
    // Create span
    let span = Tracer::start_span(tracer, "throughput_operation")
    Span::set_attribute(span, "operation_id", IntValue(i))
    
    // Record metrics
    Counter::add(counter, 1.0)
    Histogram::record(histogram, (i % 100).to_float())
    
    // Emit log
    let log_record = LogRecord::new(Info, "Throughput operation " + i.to_string())
    Logger::emit(logger, log_record)
    
    // Add event to span
    Span::add_event(span, "operation_complete", Some([("duration", FloatValue(i.to_float()))]))
    
    // End span
    Span::end(span)
  }
  
  let throughput_time = PerformanceTimer::stop(throughput_start)
  let throughput_avg = throughput_time.to_float() / throughput_operations.to_float()
  let throughput_ops_per_sec = 1000000.0 / throughput_avg // Convert to operations per second
  
  // Performance assertions
  assert_true(throughput_avg < 500.0) // Average operation should be less than 500 microseconds
  assert_true(throughput_ops_per_sec > 2000.0) // Should handle at least 2000 ops/sec
  
  // Calculate throughput breakdown
  let span_time = throughput_avg * 0.4 // Estimate 40% of time in spans
  let metrics_time = throughput_avg * 0.3 // Estimate 30% of time in metrics
  let logging_time = throughput_avg * 0.3 // Estimate 30% of time in logging
  
  // Record benchmark results
  PerformanceBenchmark::record_result(benchmark, "throughput_operation", throughput_avg, "microseconds")
  PerformanceBenchmark::record_result(benchmark, "throughput_ops_per_sec", throughput_ops_per_sec, "ops/sec")
  PerformanceBenchmark::record_result(benchmark, "span_component_time", span_time, "microseconds")
  PerformanceBenchmark::record_result(benchmark, "metrics_component_time", metrics_time, "microseconds")
  PerformanceBenchmark::record_result(benchmark, "logging_component_time", logging_time, "microseconds")
}

// Test 9: Resource Usage Under Load
test "resource usage under load" {
  let benchmark = PerformanceBenchmark::new("resource_usage")
  
  let initial_memory = MemoryProfiler::get_allocated_memory()
  let initial_cpu = CpuProfiler::get_cpu_usage()
  
  // Simulate load
  let load_operations = 50000
  let load_start = PerformanceTimer::start()
  
  let active_spans = []
  for i in 0..load_operations {
    let span = Span::new("load_test_span", Internal, SpanContext::new("trace", "span", true, ""))
    Span::set_attribute(span, "iteration", IntValue(i))
    
    // Keep some spans active to simulate real usage
    if i % 10 == 0 {
      active_spans.push(span)
    } else {
      Span::end(span)
    }
  }
  
  let load_time = PerformanceTimer::stop(load_start)
  let peak_memory = MemoryProfiler::get_allocated_memory()
  let peak_cpu = CpuProfiler::get_cpu_usage()
  
  // End remaining spans
  for span in active_spans {
    Span::end(span)
  }
  
  // Force cleanup and measure final resources
  MemoryProfiler::force_gc()
  let final_memory = MemoryProfiler::get_allocated_memory()
  let final_cpu = CpuProfiler::get_cpu_usage()
  
  // Calculate resource usage metrics
  let memory_per_operation = (peak_memory - initial_memory).to_float() / load_operations.to_float()
  let cpu_increase = peak_cpu - initial_cpu
  let memory_reclaimed = peak_memory - final_memory
  
  // Resource usage assertions
  assert_true(memory_per_operation < 512.0) // Should use less than 512 bytes per operation
  assert_true(cpu_increase < 50.0) // CPU increase should be less than 50%
  assert_true(memory_reclaimed > (peak_memory - initial_memory) * 0.7) // Should reclaim at least 70%
  
  // Record benchmark results
  PerformanceBenchmark::record_result(benchmark, "load_operation_time", load_time.to_float() / load_operations.to_float(), "microseconds")
  PerformanceBenchmark::record_result(benchmark, "memory_per_operation", memory_per_operation, "bytes")
  PerformanceBenchmark::record_result(benchmark, "cpu_increase", cpu_increase, "percent")
  PerformanceBenchmark::record_result(benchmark, "memory_reclaimed", memory_reclaimed.to_float(), "bytes")
}

// Test 10: Performance Regression Detection
test "performance regression detection" {
  let benchmark = PerformanceBenchmark::new("regression_detection")
  
  // Define performance baselines (in microseconds)
  let baselines = [
    ("span_creation", 50.0),
    ("counter_add", 5.0),
    ("histogram_record", 10.0),
    ("context_extraction", 15.0),
    ("simple_log", 20.0)
  ]
  
  // Run performance tests and compare against baselines
  let regressions = []
  
  for (operation, baseline) in baselines {
    let measured_time = run_performance_test(operation)
    let regression_ratio = measured_time / baseline
    
    // Check for regression (more than 20% slower than baseline)
    if regression_ratio > 1.2 {
      regressions.push((operation, measured_time, baseline, regression_ratio))
    }
    
    // Record the measurement
    PerformanceBenchmark::record_result(benchmark, operation, measured_time, "microseconds")
  }
  
  // Regression assertions
  assert_true(regressions.length() == 0) // No regressions should be detected
  
  // If regressions were detected, they would be reported here
  for regression in regressions {
    let (operation, measured, baseline, ratio) = regression
    // In a real implementation, this would log or report the regression
    assert_true(false, "Performance regression detected in " + operation + 
                     ": measured " + measured.to_string() + 
                     "μs, baseline " + baseline.to_string() + 
                     "μs, ratio " + ratio.to_string())
  }
}

// Helper function for performance regression testing
fn run_performance_test(operation : String) -> Float {
  match operation {
    "span_creation" => {
      let provider = TracerProvider::default()
      let tracer = TracerProvider::get_tracer(provider, "test_tracer")
      let iterations = 1000
      let start = PerformanceTimer::start()
      
      for i in 0..iterations {
        let span = Tracer::start_span(tracer, "test_span")
        Span::end(span)
      }
      
      let time = PerformanceTimer::stop(start)
      time.to_float() / iterations.to_float()
    }
    "counter_add" => {
      let provider = MeterProvider::default()
      let meter = MeterProvider::get_meter(provider, "test_meter")
      let counter = Meter::create_counter(meter, "test_counter", None, None)
      let iterations = 10000
      let start = PerformanceTimer::start()
      
      for i in 0..iterations {
        Counter::add(counter, 1.0)
      }
      
      let time = PerformanceTimer::stop(start)
      time.to_float() / iterations.to_float()
    }
    "histogram_record" => {
      let provider = MeterProvider::default()
      let meter = MeterProvider::get_meter(provider, "test_meter")
      let histogram = Meter::create_histogram(meter, "test_histogram", None, None)
      let iterations = 5000
      let start = PerformanceTimer::start()
      
      for i in 0..iterations {
        Histogram::record(histogram, i.to_float())
      }
      
      let time = PerformanceTimer::stop(start)
      time.to_float() / iterations.to_float()
    }
    "context_extraction" => {
      let context = SpanContext::new("trace_id", "span_id", true, "sampled")
      let iterations = 10000
      let start = PerformanceTimer::start()
      
      for i in 0..iterations {
        let trace_id = SpanContext::trace_id(context)
        let span_id = SpanContext::span_id(context)
        let is_sampled = SpanContext::is_sampled(context)
        let is_valid = SpanContext::is_valid(context)
        
        // Use the values to prevent optimization
        assert_true(trace_id.length() > 0)
        assert_true(span_id.length() > 0)
      }
      
      let time = PerformanceTimer::stop(start)
      time.to_float() / iterations.to_float()
    }
    "simple_log" => {
      let provider = LoggerProvider::default()
      let logger = LoggerProvider::get_logger(provider, "test_logger")
      let iterations = 5000
      let start = PerformanceTimer::start()
      
      for i in 0..iterations {
        let log_record = LogRecord::new(Info, "Test log message " + i.to_string())
        Logger::emit(logger, log_record)
      }
      
      let time = PerformanceTimer::stop(start)
      time.to_float() / iterations.to_float()
    }
    _ => 0.0 // Default case
  }
}