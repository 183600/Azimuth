// Azimuth Telemetry System - Premium Performance Benchmark Tests
// This file contains high-quality performance benchmark tests for critical telemetry operations

// Test 1: Attribute Operations Performance
test "attribute operations performance benchmark" {
  let attrs = Attributes::new()
  let iterations = 10000
  
  // Benchmark attribute setting performance
  let start_time = Clock::monotonic()
  for i in 0..<iterations {
    let key = "perf.key." + i.to_string()
    let value = StringValue("performance.test.value." + i.to_string())
    Attributes::set(attrs, key, value)
  }
  let set_duration = Clock::monotonic() - start_time
  
  // Benchmark attribute getting performance
  start_time = Clock::monotonic()
  for i in 0..<iterations {
    let key = "perf.key." + i.to_string()
    let _result = Attributes::get(attrs, key)
  }
  let get_duration = Clock::monotonic() - start_time
  
  // Performance assertions - these should be adjusted based on actual requirements
  assert_true(set_duration < 1000000L) // Set operations should complete within 1ms
  assert_true(get_duration < 500000L)  // Get operations should complete within 0.5ms
  assert_true(set_duration > get_duration) // Get should be faster than set
}

// Test 2: Span Creation and Lifecycle Performance
test "span lifecycle performance benchmark" {
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
  let iterations = 5000
  let spans = []
  
  // Benchmark span creation performance
  let start_time = Clock::monotonic()
  for i in 0..<iterations {
    let span_name = "performance.span." + i.to_string()
    let span = Span::new(span_name, Internal, span_ctx)
    spans.push(span)
  }
  let creation_duration = Clock::monotonic() - start_time
  
  // Benchmark span operations performance
  start_time = Clock::monotonic()
  for span in spans {
    Span::add_event(span, "performance.event", Some([("iteration", IntValue(1))]))
    Span::set_status(span, Ok, Some("Performance test"))
  }
  let operations_duration = Clock::monotonic() - start_time
  
  // Benchmark span ending performance
  start_time = Clock::monotonic()
  for span in spans {
    Span::end(span)
  }
  let end_duration = Clock::monotonic() - start_time
  
  // Performance assertions
  assert_true(creation_duration < 2000000L)  // Creation should complete within 2ms
  assert_true(operations_duration < 1500000L) // Operations should complete within 1.5ms
  assert_true(end_duration < 1000000L)       // Ending should complete within 1ms
}

// Test 3: Metrics Collection Performance
test "metrics collection performance benchmark" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance_meter")
  let counter = Meter::create_counter(meter, "performance.counter", Some("Performance counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "performance.histogram", Some("Performance histogram"), Some("ms"))
  let iterations = 10000
  
  // Benchmark counter operations
  let start_time = Clock::monotonic()
  for i in 0..<iterations {
    Counter::add(counter, i.to_float())
  }
  let counter_duration = Clock::monotonic() - start_time
  
  // Benchmark histogram operations
  start_time = Clock::monotonic()
  for i in 0..<iterations {
    Histogram::record(histogram, i.to_float())
  }
  let histogram_duration = Clock::monotonic() - start_time
  
  // Performance assertions
  assert_true(counter_duration < 1000000L)   // Counter operations should complete within 1ms
  assert_true(histogram_duration < 2000000L) // Histogram operations should complete within 2ms
}

// Test 4: Log Record Emission Performance
test "log record emission performance benchmark" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "performance_logger")
  let iterations = 5000
  
  // Benchmark simple log record creation and emission
  let start_time = Clock::monotonic()
  for i in 0..<iterations {
    let log_record = LogRecord::new(Info, "Performance test log message " + i.to_string())
    Logger::emit(logger, log_record)
  }
  let simple_log_duration = Clock::monotonic() - start_time
  
  // Benchmark complex log record creation and emission
  start_time = Clock::monotonic()
  for i in 0..<iterations {
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    Attributes::set(attrs, "component", StringValue("performance_test"))
    
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Complex performance test log message " + i.to_string()),
      Some(attrs),
      Some(Clock::monotonic()),
      Some(Clock::monotonic() + 1000L),
      Some("trace_id_" + i.to_string()),
      Some("span_id_" + i.to_string()),
      None
    )
    Logger::emit(logger, log_record)
  }
  let complex_log_duration = Clock::monotonic() - start_time
  
  // Performance assertions
  assert_true(simple_log_duration < 2000000L)   // Simple logs should complete within 2ms
  assert_true(complex_log_duration < 5000000L)  // Complex logs should complete within 5ms
  assert_true(complex_log_duration > simple_log_duration) // Complex logs should be slower
}

// Test 5: Context Propagation Performance
test "context propagation performance benchmark" {
  let root_ctx = Context::root()
  let iterations = 10000
  
  // Benchmark context creation and value setting
  let start_time = Clock::monotonic()
  let mut ctx = root_ctx
  for i in 0..<iterations {
    let key = ContextKey::new("context.key." + i.to_string())
    ctx = Context::with_value(ctx, key, "context.value." + i.to_string())
  }
  let context_creation_duration = Clock::monotonic() - start_time
  
  // Benchmark context value retrieval
  start_time = Clock::monotonic()
  for i in 0..<iterations {
    let key = ContextKey::new("context.key." + i.to_string())
    let _value = Context::get(ctx, key)
  }
  let context_retrieval_duration = Clock::monotonic() - start_time
  
  // Performance assertions
  assert_true(context_creation_duration < 3000000L)  // Context creation should complete within 3ms
  assert_true(context_retrieval_duration < 2000000L) // Context retrieval should complete within 2ms
}

// Test 6: Memory Allocation and Cleanup Performance
test "memory allocation and cleanup performance benchmark" {
  let iterations = 1000
  
  // Benchmark attribute creation and cleanup
  let start_time = Clock::monotonic()
  for i in 0..<iterations {
    let attrs = Attributes::new()
    for j in 0..<100 {
      let key = "memory.key." + j.to_string()
      let value = StringValue("memory.value." + j.to_string())
      Attributes::set(attrs, key, value)
    }
    // Attributes should be automatically cleaned up when going out of scope
  }
  let memory_cleanup_duration = Clock::monotonic() - start_time
  
  // Benchmark span creation and cleanup
  start_time = Clock::monotonic()
  for i in 0..<iterations {
    let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
    let span = Span::new("memory.test.span", Internal, span_ctx)
    Span::add_event(span, "memory.test.event", None)
    Span::end(span)
    // Span should be automatically cleaned up when going out of scope
  }
  let span_cleanup_duration = Clock::monotonic() - start_time
  
  // Performance assertions
  assert_true(memory_cleanup_duration < 10000000L) // Memory cleanup should complete within 10ms
  assert_true(span_cleanup_duration < 5000000L)    // Span cleanup should complete within 5ms
}

// Test 7: Serialization and Deserialization Performance
test "serialization and deserialization performance benchmark" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("test.value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  let iterations = 1000
  
  // Benchmark serialization performance
  let start_time = Clock::monotonic()
  let mut serialized_data = []
  for i in 0..<iterations {
    let data = Attributes::serialize(attrs)
    serialized_data.push(data)
  }
  let serialization_duration = Clock::monotonic() - start_time
  
  // Benchmark deserialization performance
  start_time = Clock::monotonic()
  for data in serialized_data {
    let _deserialized_attrs = Attributes::deserialize(data)
  }
  let deserialization_duration = Clock::monotonic() - start_time
  
  // Performance assertions
  assert_true(serialization_duration < 5000000L)   // Serialization should complete within 5ms
  assert_true(deserialization_duration < 5000000L) // Deserialization should complete within 5ms
}

// Test 8: Concurrent Operations Performance
test "concurrent operations performance benchmark" {
  let attrs = Attributes::new()
  let iterations_per_thread = 1000
  let thread_count = 4
  
  // Benchmark concurrent attribute setting
  let start_time = Clock::monotonic()
  let threads = []
  for thread_id in 0..<thread_count {
    let thread = Thread::spawn(|| {
      for i in 0..<iterations_per_thread {
        let key = "concurrent.key." + thread_id.to_string() + "." + i.to_string()
        let value = StringValue("concurrent.value." + thread_id.to_string() + "." + i.to_string())
        Attributes::set(attrs, key, value)
      }
    })
    threads.push(thread)
  }
  for thread in threads {
    Thread::join(thread)
  }
  let concurrent_set_duration = Clock::monotonic() - start_time
  
  // Benchmark concurrent attribute getting
  start_time = Clock::monotonic()
  let get_threads = []
  for thread_id in 0..<thread_count {
    let thread = Thread::spawn(|| {
      for i in 0..<iterations_per_thread {
        let key = "concurrent.key." + thread_id.to_string() + "." + i.to_string()
        let _value = Attributes::get(attrs, key)
      }
    })
    get_threads.push(thread)
  }
  for thread in get_threads {
    Thread::join(thread)
  }
  let concurrent_get_duration = Clock::monotonic() - start_time
  
  // Performance assertions
  assert_true(concurrent_set_duration < 10000000L) // Concurrent set should complete within 10ms
  assert_true(concurrent_get_duration < 10000000L) // Concurrent get should complete within 10ms
}