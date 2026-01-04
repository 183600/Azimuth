// Performance Benchmark Tests
// This file contains comprehensive performance benchmark tests for the Azimuth telemetry system

// Test 1: Span Creation and Management Performance
test "span creation and management performance" {
  let benchmark = PerformanceBenchmark::new("span_creation", 1000)
  
  // Benchmark span creation
  let start_time = PerformanceBenchmark::start_timer(benchmark)
  
  let mut spans = []
  for i in 0..1000 {
    let span_name = "test-span-" + i.to_string()
    let span = Span::new(span_name, Internal, SpanContext::new("trace_id", "span_id", true, ""))
    spans = spans + [span]
  }
  
  let end_time = PerformanceBenchmark::end_timer(benchmark)
  let creation_time = PerformanceBenchmark::elapsed_ms(benchmark, start_time, end_time)
  
  // Verify performance - should create 1000 spans in reasonable time
  assert_true(creation_time < 1000.0) // Less than 1 second for 1000 spans
  assert_eq(spans.length(), 1000)
  
  // Benchmark span operations
  let operation_start = PerformanceBenchmark::start_timer(benchmark)
  
  for span in spans {
    Span::add_event(span, "test_event", None)
    Span::set_status(span, Ok, None)
  }
  
  let operation_end = PerformanceBenchmark::end_timer(benchmark)
  let operation_time = PerformanceBenchmark::elapsed_ms(benchmark, operation_start, operation_end)
  
  // Verify operations performance
  assert_true(operation_time < 500.0) // Less than 500ms for 1000 operations
  
  // Report benchmark results
  let report = PerformanceBenchmark::generate_report(benchmark)
  assert_true(PerformanceReport::contains_metric(report, "span_creation"))
  assert_true(PerformanceReport::contains_metric(report, "span_operations"))
}

// Test 2: Metrics Collection Performance
test "metrics collection performance" {
  let benchmark = PerformanceBenchmark::new("metrics_collection", 10000)
  
  // Setup metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "benchmark_meter")
  
  let counter = Meter::create_counter(meter, "benchmark_counter", None, None)
  let histogram = Meter::create_histogram(meter, "benchmark_histogram", None, None)
  let updown_counter = Meter::create_updown_counter(meter, "benchmark_updown", None, None)
  
  // Benchmark counter operations
  let counter_start = PerformanceBenchmark::start_timer(benchmark)
  
  for i in 0..10000 {
    Counter::add(counter, i.to_int() as Float)
  }
  
  let counter_end = PerformanceBenchmark::end_timer(benchmark)
  let counter_time = PerformanceBenchmark::elapsed_ms(benchmark, counter_start, counter_end)
  
  // Verify counter performance
  assert_true(counter_time < 1000.0) // Less than 1 second for 10000 operations
  
  // Benchmark histogram operations
  let histogram_start = PerformanceBenchmark::start_timer(benchmark)
  
  for i in 0..10000 {
    let value = (i % 100) as Float
    Histogram::record(histogram, value)
  }
  
  let histogram_end = PerformanceBenchmark::end_timer(benchmark)
  let histogram_time = PerformanceBenchmark::elapsed_ms(benchmark, histogram_start, histogram_end)
  
  // Verify histogram performance
  assert_true(histogram_time < 1500.0) // Less than 1.5 seconds for 10000 operations
  
  // Benchmark updown counter operations
  let updown_start = PerformanceBenchmark::start_timer(benchmark)
  
  for i in 0..10000 {
    let value = if i % 2 == 0 { 1.0 } else { -1.0 }
    UpDownCounter::add(updown_counter, value)
  }
  
  let updown_end = PerformanceBenchmark::end_timer(benchmark)
  let updown_time = PerformanceBenchmark::elapsed_ms(benchmark, updown_start, updown_end)
  
  // Verify updown counter performance
  assert_true(updown_time < 1000.0) // Less than 1 second for 10000 operations
  
  // Generate performance report
  let report = PerformanceBenchmark::generate_report(benchmark)
  assert_true(PerformanceReport::contains_metric(report, "counter_operations"))
  assert_true(PerformanceReport::contains_metric(report, "histogram_operations"))
  assert_true(PerformanceReport::contains_metric(report, "updown_counter_operations"))
}

// Test 3: Trace Context Propagation Performance
test "trace context propagation performance" {
  let benchmark = PerformanceBenchmark::new("context_propagation", 5000)
  
  // Create initial context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_ctx = SpanContext::new(trace_id, "root_span", true, "test_state")
  
  // Benchmark context injection
  let injection_start = PerformanceBenchmark::start_timer(benchmark)
  
  let mut injected_contexts = []
  for i in 0..5000 {
    let child_ctx = SpanContext::create_child(span_ctx, "child_span_" + i.to_string())
    let injected = TracePropagator::inject(child_ctx)
    injected_contexts = injected_contexts + [injected]
  }
  
  let injection_end = PerformanceBenchmark::end_timer(benchmark)
  let injection_time = PerformanceBenchmark::elapsed_ms(benchmark, injection_start, injection_end)
  
  // Verify injection performance
  assert_true(injection_time < 2000.0) // Less than 2 seconds for 5000 injections
  assert_eq(injected_contexts.length(), 5000)
  
  // Benchmark context extraction
  let extraction_start = PerformanceBenchmark::start_timer(benchmark)
  
  let mut extracted_contexts = []
  for injected in injected_contexts {
    let extracted = TracePropagator::extract(injected)
    extracted_contexts = extracted_contexts + [extracted]
  }
  
  let extraction_end = PerformanceBenchmark::end_timer(benchmark)
  let extraction_time = PerformanceBenchmark::elapsed_ms(benchmark, extraction_start, extraction_end)
  
  // Verify extraction performance
  assert_true(extraction_time < 2000.0) // Less than 2 seconds for 5000 extractions
  assert_eq(extracted_contexts.length(), 5000)
  
  // Verify trace ID consistency
  for ctx in extracted_contexts {
    assert_eq(SpanContext::trace_id(ctx), trace_id)
  }
  
  // Generate performance report
  let report = PerformanceBenchmark::generate_report(benchmark)
  assert_true(PerformanceReport::contains_metric(report, "context_injection"))
  assert_true(PerformanceReport::contains_metric(report, "context_extraction"))
}

// Test 4: Attribute Operations Performance
test "attribute operations performance" {
  let benchmark = PerformanceBenchmark::new("attribute_operations", 10000)
  
  // Benchmark attribute creation and setting
  let creation_start = PerformanceBenchmark::start_timer(benchmark)
  
  let mut attributes_list = []
  for i in 0..10000 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "attr_" + i.to_string(), StringValue("value_" + i.to_string()))
    Attributes::set(attrs, "int_attr_" + i.to_string(), IntValue(i))
    Attributes::set(attrs, "float_attr_" + i.to_string(), FloatValue(i.to_int() as Float * 1.5))
    Attributes::set(attrs, "bool_attr_" + i.to_string(), BoolValue(i % 2 == 0))
    attributes_list = attributes_list + [attrs]
  }
  
  let creation_end = PerformanceBenchmark::end_timer(benchmark)
  let creation_time = PerformanceBenchmark::elapsed_ms(benchmark, creation_start, creation_end)
  
  // Verify creation performance
  assert_true(creation_time < 3000.0) // Less than 3 seconds for 10000 attribute sets
  assert_eq(attributes_list.length(), 10000)
  
  // Benchmark attribute retrieval
  let retrieval_start = PerformanceBenchmark::start_timer(benchmark)
  
  let mut retrieved_values = 0
  for i in 0..10000 {
    let attrs = attributes_list[i]
    let string_value = Attributes::get(attrs, "attr_" + i.to_string())
    let int_value = Attributes::get(attrs, "int_attr_" + i.to_string())
    let float_value = Attributes::get(attrs, "float_attr_" + i.to_string())
    let bool_value = Attributes::get(attrs, "bool_attr_" + i.to_string())
    
    // Count successful retrievals
    if string_value.is_some() && int_value.is_some() && float_value.is_some() && bool_value.is_some() {
      retrieved_values = retrieved_values + 1
    }
  }
  
  let retrieval_end = PerformanceBenchmark::end_timer(benchmark)
  let retrieval_time = PerformanceBenchmark::elapsed_ms(benchmark, retrieval_start, retrieval_end)
  
  // Verify retrieval performance
  assert_true(retrieval_time < 2000.0) // Less than 2 seconds for 40000 retrievals
  assert_eq(retrieved_values, 10000) // All values should be retrieved successfully
  
  // Generate performance report
  let report = PerformanceBenchmark::generate_report(benchmark)
  assert_true(PerformanceReport::contains_metric(report, "attribute_creation"))
  assert_true(PerformanceReport::contains_metric(report, "attribute_retrieval"))
}

// Test 5: Log Record Processing Performance
test "log record processing performance" {
  let benchmark = PerformanceBenchmark::new("log_processing", 5000)
  
  // Setup logger
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "benchmark_logger")
  
  // Benchmark log record creation
  let creation_start = PerformanceBenchmark::start_timer(benchmark)
  
  let mut log_records = []
  for i in 0..5000 {
    let severity = if i % 4 == 0 { Info } else if i % 4 == 1 { Warn } else if i % 4 == 2 { Error } else { Debug }
    let message = "Log message " + i.to_string()
    let log_record = LogRecord::new(severity, message)
    log_records = log_records + [log_record]
  }
  
  let creation_end = PerformanceBenchmark::end_timer(benchmark)
  let creation_time = PerformanceBenchmark::elapsed_ms(benchmark, creation_start, creation_end)
  
  // Verify creation performance
  assert_true(creation_time < 1000.0) // Less than 1 second for 5000 log records
  assert_eq(log_records.length(), 5000)
  
  // Benchmark log emission
  let emission_start = PerformanceBenchmark::start_timer(benchmark)
  
  for log_record in log_records {
    Logger::emit(logger, log_record)
  }
  
  let emission_end = PerformanceBenchmark::end_timer(benchmark)
  let emission_time = PerformanceBenchmark::elapsed_ms(benchmark, emission_start, emission_end)
  
  // Verify emission performance
  assert_true(emission_time < 2000.0) // Less than 2 seconds for 5000 emissions
  
  // Generate performance report
  let report = PerformanceBenchmark::generate_report(benchmark)
  assert_true(PerformanceReport::contains_metric(report, "log_creation"))
  assert_true(PerformanceReport::contains_metric(report, "log_emission"))
}

// Test 6: Memory Usage and Allocation Performance
test "memory usage and allocation performance" {
  let memory_profiler = MemoryProfiler::new()
  
  // Baseline memory measurement
  let baseline_memory = MemoryProfiler::get_current_usage(memory_profiler)
  
  // Create a large number of telemetry objects
  let mut telemetry_objects = []
  for i in 0..10000 {
    let span = Span::new("memory-test-span-" + i.to_string(), Internal, 
      SpanContext::new("trace_id", "span_id", true, ""))
    let attrs = Attributes::new()
    Attributes::set(attrs, "index", IntValue(i))
    Span::add_event(span, "test_event", Some([("index", IntValue(i))]))
    telemetry_objects = telemetry_objects + [(span, attrs)]
  }
  
  // Measure memory after allocation
  let after_allocation = MemoryProfiler::get_current_usage(memory_profiler)
  let memory_increase = MemoryProfiler::calculate_increase(baseline_memory, after_allocation)
  
  // Verify memory usage is reasonable
  let memory_per_object = memory_increase / 10000
  assert_true(memory_per_object < 5000) // Less than 5KB per object
  
  // Test memory deallocation
  telemetry_objects = [] // Clear references
  
  // Force garbage collection if available
  MemoryProfiler::force_gc(memory_profiler)
  
  // Measure memory after deallocation
  let after_deallocation = MemoryProfiler::get_current_usage(memory_profiler)
  let memory_reclaimed = MemoryProfiler::calculate_decrease(after_allocation, after_deallocation)
  
  // Verify memory is properly reclaimed
  let reclamation_rate = (memory_reclaimed as Float) / (memory_increase as Float)
  assert_true(reclamation_rate > 0.8) // At least 80% of memory should be reclaimed
  
  // Generate memory report
  let memory_report = MemoryProfiler::generate_report(memory_profiler)
  assert_true(MemoryReport::contains_metric(memory_report, "baseline_memory"))
  assert_true(MemoryReport::contains_metric(memory_report, "peak_memory"))
  assert_true(MemoryReport::contains_metric(memory_report, "memory_reclamation_rate"))
}

// Test 7: Concurrent Operations Performance
test "concurrent operations performance" {
  let concurrency_benchmark = ConcurrencyBenchmark::new("concurrent_operations", 4, 1000)
  
  // Create shared telemetry resources
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent_meter")
  let counter = Meter::create_counter(meter, "concurrent_counter", None, None)
  
  // Benchmark concurrent counter operations
  let concurrent_start = ConcurrencyBenchmark::start_timer(concurrency_benchmark)
  
  // Simulate concurrent operations (simplified for this test)
  let mut thread_results = []
  for thread_id in 0..4 {
    let mut thread_count = 0
    for i in 0..1000 {
      Counter::add(counter, 1.0)
      thread_count = thread_count + 1
    }
    thread_results = thread_results + [thread_count]
  }
  
  let concurrent_end = ConcurrencyBenchmark::end_timer(concurrency_benchmark)
  let concurrent_time = ConcurrencyBenchmark::elapsed_ms(concurrency_benchmark, concurrent_start, concurrent_end)
  
  // Verify concurrent operations performance
  assert_true(concurrent_time < 2000.0) // Less than 2 seconds for 4000 operations
  assert_eq(thread_results.length(), 4)
  
  // Verify all threads completed their work
  for result in thread_results {
    assert_eq(result, 1000)
  }
  
  // Benchmark concurrent span operations
  let span_start = ConcurrencyBenchmark::start_timer(concurrency_benchmark)
  
  let mut span_results = []
  for thread_id in 0..4 {
    let mut thread_spans = []
    for i in 0..250 {
      let span_name = "thread-" + thread_id.to_string() + "-span-" + i.to_string()
      let span = Span::new(span_name, Internal, SpanContext::new("trace_id", "span_id", true, ""))
      Span::add_event(span, "thread_event", Some([("thread_id", IntValue(thread_id))]))
      thread_spans = thread_spans + [span]
    }
    span_results = span_results + [thread_spans]
  }
  
  let span_end = ConcurrencyBenchmark::end_timer(concurrency_benchmark)
  let span_time = ConcurrencyBenchmark::elapsed_ms(concurrency_benchmark, span_start, span_end)
  
  // Verify concurrent span operations performance
  assert_true(span_time < 3000.0) // Less than 3 seconds for 1000 spans
  assert_eq(span_results.length(), 4)
  
  // Generate concurrency report
  let report = ConcurrencyBenchmark::generate_report(concurrency_benchmark)
  assert_true(ConcurrencyReport::contains_metric(report, "concurrent_counter_operations"))
  assert_true(ConcurrencyReport::contains_metric(report, "concurrent_span_operations"))
}

// Test 8: Serialization Performance
test "serialization performance" {
  let serialization_benchmark = PerformanceBenchmark::new("serialization", 1000)
  
  // Create test data
  let mut test_spans = []
  for i in 0..1000 {
    let span = Span::new("serialization-test-" + i.to_string(), Internal, 
      SpanContext::new("trace_id", "span_id", true, ""))
    Span::add_attribute(span, "index", IntValue(i))
    Span::add_event(span, "test_event", Some([("index", IntValue(i))]))
    test_spans = test_spans + [span]
  }
  
  // Benchmark JSON serialization
  let json_start = PerformanceBenchmark::start_timer(serialization_benchmark)
  
  let mut json_serialized = []
  for span in test_spans {
    let json = SpanSerializer::to_json(span)
    json_serialized = json_serialized + [json]
  }
  
  let json_end = PerformanceBenchmark::end_timer(serialization_benchmark)
  let json_time = PerformanceBenchmark::elapsed_ms(serialization_benchmark, json_start, json_end)
  
  // Verify JSON serialization performance
  assert_true(json_time < 2000.0) // Less than 2 seconds for 1000 spans
  assert_eq(json_serialized.length(), 1000)
  
  // Benchmark binary serialization
  let binary_start = PerformanceBenchmark::start_timer(serialization_benchmark)
  
  let mut binary_serialized = []
  for span in test_spans {
    let binary = SpanSerializer::to_binary(span)
    binary_serialized = binary_serialized + [binary]
  }
  
  let binary_end = PerformanceBenchmark::end_timer(serialization_benchmark)
  let binary_time = PerformanceBenchmark::elapsed_ms(serialization_benchmark, binary_start, binary_end)
  
  // Verify binary serialization performance
  assert_true(binary_time < 1500.0) // Less than 1.5 seconds for 1000 spans
  assert_eq(binary_serialized.length(), 1000)
  
  // Compare serialization efficiency
  let json_total_size = json_serialized.fold_left(0, fn(acc, s) { acc + s.length() })
  let binary_total_size = binary_serialized.fold_left(0, fn(acc, b) { acc + b.length() })
  
  // Binary should be more compact than JSON
  assert_true(binary_total_size < json_total_size)
  
  // Generate serialization report
  let report = PerformanceBenchmark::generate_report(serialization_benchmark)
  assert_true(PerformanceReport::contains_metric(report, "json_serialization"))
  assert_true(PerformanceReport::contains_metric(report, "binary_serialization"))
  assert_true(PerformanceReport::contains_metric(report, "serialization_efficiency"))
}

// Test 9: Resource Cleanup Performance
test "resource cleanup performance" {
  let cleanup_benchmark = PerformanceBenchmark::new("resource_cleanup", 1000)
  
  // Create resources to clean up
  let mut resources = []
  for i in 0..1000 {
    let provider = MeterProvider::default()
    let meter = MeterProvider::get_meter(provider, "cleanup_meter_" + i.to_string())
    let span = Span::new("cleanup_span_" + i.to_string(), Internal, 
      SpanContext::new("trace_id", "span_id", true, ""))
    resources = resources + [(provider, meter, span)]
  }
  
  // Benchmark resource cleanup
  let cleanup_start = PerformanceBenchmark::start_timer(cleanup_benchmark)
  
  // Clean up resources
  resources = [] // Clear references
  
  let cleanup_end = PerformanceBenchmark::end_timer(cleanup_benchmark)
  let cleanup_time = PerformanceBenchmark::elapsed_ms(cleanup_benchmark, cleanup_start, cleanup_end)
  
  // Verify cleanup performance
  assert_true(cleanup_time < 1000.0) // Less than 1 second for 1000 resources
  assert_eq(resources.length(), 0)
  
  // Generate cleanup report
  let report = PerformanceBenchmark::generate_report(cleanup_benchmark)
  assert_true(PerformanceReport::contains_metric(report, "resource_cleanup"))
}

// Test 10: End-to-End Performance Scenario
test "end-to-end performance scenario" {
  let e2e_benchmark = PerformanceBenchmark::new("end_to_end", 100)
  
  // Simulate a complete telemetry workflow
  let e2e_start = PerformanceBenchmark::start_timer(e2e_benchmark)
  
  let mut trace_results = []
  for trace_id in 0..100 {
    let trace_start = PerformanceBenchmark::start_timer(e2e_benchmark)
    
    // Create trace context
    let trace_ctx = SpanContext::new("trace_" + trace_id.to_string(), "root_span", true, "")
    
    // Create spans
    let mut spans = []
    for span_id in 0..5 {
      let span_name = "span_" + span_id.to_string()
      let span = Span::new(span_name, Internal, trace_ctx)
      
      // Add attributes
      Span::add_attribute(span, "trace_id", StringValue("trace_" + trace_id.to_string()))
      Span::add_attribute(span, "span_id", StringValue("span_" + span_id.to_string()))
      
      // Add events
      for event_id in 0..3 {
        Span::add_event(span, "event_" + event_id.to_string(), None)
      }
      
      spans = spans + [span]
    }
    
    // Create metrics
    let provider = MeterProvider::default()
    let meter = MeterProvider::get_meter(provider, "trace_meter")
    let counter = Meter::create_counter(meter, "trace_counter", None, None)
    
    for i in 0..10 {
      Counter::add(counter, 1.0)
    }
    
    // Create logs
    let logger_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(logger_provider, "trace_logger")
    
    for i in 0..5 {
      let log_record = LogRecord::new(Info, "Trace log message " + i.to_string())
      Logger::emit(logger, log_record)
    }
    
    // Serialize spans
    let mut serialized_spans = []
    for span in spans {
      let json = SpanSerializer::to_json(span)
      serialized_spans = serialized_spans + [json]
    }
    
    let trace_end = PerformanceBenchmark::end_timer(e2e_benchmark)
    let trace_time = PerformanceBenchmark::elapsed_ms(e2e_benchmark, trace_start, trace_end)
    
    trace_results = trace_results + [trace_time]
  }
  
  let e2e_end = PerformanceBenchmark::end_timer(e2e_benchmark)
  let total_time = PerformanceBenchmark::elapsed_ms(e2e_benchmark, e2e_start, e2e_end)
  
  // Calculate statistics
  let avg_trace_time = trace_results.fold_left(0.0, fn(acc, time) { acc + time }) / trace_results.length() as Float
  let max_trace_time = trace_results.fold_left(0.0, fn(acc, time) { if time > acc { time } else { acc } })
  let min_trace_time = trace_results.fold_left(999999.0, fn(acc, time) { if time < acc { time } else { acc } })
  
  // Verify end-to-end performance
  assert_true(total_time < 10000.0) // Less than 10 seconds for 100 traces
  assert_true(avg_trace_time < 100.0) // Average less than 100ms per trace
  assert_true(max_trace_time < 200.0) // Maximum less than 200ms per trace
  assert_eq(trace_results.length(), 100)
  
  // Generate end-to-end report
  let report = PerformanceBenchmark::generate_report(e2e_benchmark)
  assert_true(PerformanceReport::contains_metric(report, "end_to_end"))
  assert_true(PerformanceReport::contains_statistic(report, "average_trace_time"))
  assert_true(PerformanceReport::contains_statistic(report, "max_trace_time"))
  assert_true(PerformanceReport::contains_statistic(report, "min_trace_time"))
}