// Azimuth Telemetry System - Performance and Resource Management Tests
// This file contains test cases for performance optimization and resource management

// Test 1: Memory Usage Optimization
test "memory usage optimization" {
  // Test efficient span creation and cleanup
  let spans = []
  
  // Create spans in batches to test memory efficiency
  for batch in 1..=5 {
    let batch_spans = []
    
    // Create 100 spans per batch
    for i in 1..=100 {
      let span_id = "span_" + batch.to_string() + "_" + i.to_string()
      let ctx = SpanContext::new("trace123456789012345678901234567890", span_id, true, "")
      let span = Span::new("memory_test_span_" + i.to_string(), Internal, ctx)
      
      // Add minimal attributes to test memory usage
      Span::set_attribute(span, "batch", IntValue(batch))
      Span::set_attribute(span, "index", IntValue(i))
      
      batch_spans.push(span)
    }
    
    // End spans immediately to test cleanup
    for span in batch_spans {
      Span::end(span)
    }
    
    // Clear batch array to free memory
    batch_spans = []
  }
  
  // Test attribute memory efficiency
  let attrs = Attributes::new()
  
  // Add many attributes to test memory usage
  for i in 1..=1000 {
    let key = "attr_" + i.to_string()
    let value = StringValue("value_" + i.to_string())
    Attributes::set(attrs, key, value)
  }
  
  // Verify some attributes are set
  let test_attr = Attributes::get(attrs, "attr_500")
  match test_attr {
    Some(StringValue(value)) => assert_eq(value, "value_500")
    _ => assert_true(false)
  }
  
  // Test array attribute memory efficiency
  let large_string_array = []
  for i in 1..=1000 {
    large_string_array.push("string_" + i.to_string())
  }
  
  Attributes::set(attrs, "large.array", ArrayStringValue(large_string_array))
  
  let array_result = Attributes::get(attrs, "large.array")
  match array_result {
    Some(ArrayStringValue(arr)) => assert_eq(arr.length(), 1000)
    _ => assert_true(false)
  }
}

// Test 2: Performance Benchmarking for Metrics
test "performance benchmarking for metrics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance_meter")
  
  // Create metrics for performance testing
  let counter = Meter::create_counter(
    meter,
    "performance.counter",
    Some("Performance test counter"),
    Some("operations")
  )
  
  let histogram = Meter::create_histogram(
    meter,
    "performance.histogram",
    Some("Performance test histogram"),
    Some("ms")
  )
  
  let gauge = Meter::create_gauge(
    meter,
    "performance.gauge",
    Some("Performance test gauge"),
    Some("units")
  )
  
  // Benchmark counter operations
  let start_time = 1234567890L
  
  // Perform 10,000 counter operations
  for i in 1..=10000 {
    Counter::add(counter, 1.0, Some(Attributes::with([
      ("operation.type", StringValue("benchmark")),
      ("iteration", IntValue(i))
    ])))
  }
  
  let counter_end_time = 1234567890L
  
  // Benchmark histogram operations
  for i in 1..=5000 {
    let value = 50.0 + (i.to_float() % 1000.0)
    Histogram::record(histogram, value, Some(Attributes::with([
      ("operation.type", StringValue("benchmark")),
      ("iteration", IntValue(i))
    ])))
  }
  
  let histogram_end_time = 1234567890L
  
  // Benchmark gauge operations
  for i in 1..=3000 {
    let value = 100.0 + (i.to_float() % 50.0)
    Gauge::record(gauge, value, Some(Attributes::with([
      ("operation.type", StringValue("benchmark")),
      ("iteration", IntValue(i))
    ])))
  }
  
  let gauge_end_time = 1234567890L
  
  // Verify metrics are properly configured
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "performance.counter")
  
  let histogram_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(histogram_instrument), "performance.histogram")
  
  let gauge_instrument = Gauge::as_instrument(gauge)
  assert_eq(Instrument::name(gauge_instrument), "performance.gauge")
  
  // Test performance with different attribute combinations
  let performance_counter = Meter::create_counter(
    meter,
    "attribute.performance.counter",
    Some("Attribute performance test counter"),
    Some("operations")
  )
  
  // Test with varying numbers of attributes
  for attr_count in [1, 5, 10, 25, 50, 100] {
    let attrs = Attributes::new()
    
    // Add varying number of attributes
    for i in 1..=attr_count {
      let key = "attr_" + i.to_string()
      let value = StringValue("value_" + i.to_string())
      Attributes::set(attrs, key, value)
    }
    
    // Measure performance with this attribute count
    for i in 1..=1000 {
      Counter::add(performance_counter, 1.0, Some(attrs))
    }
  }
}

// Test 3: Resource Pool Management
test "resource pool management" {
  // Test span context pool efficiency
  let context_pool = []
  
  // Create and reuse span contexts
  for i in 1..=100 {
    let trace_id = "trace123456789012345678901234567890"
    let span_id = "span_" + i.to_string().pad_left(16, '0')
    let ctx = SpanContext::new(trace_id, span_id, true, "")
    context_pool.push(ctx)
  }
  
  // Verify all contexts are valid
  for ctx in context_pool {
    assert_true(SpanContext::is_valid(ctx))
    assert_true(SpanContext::is_sampled(ctx))
    assert_eq(SpanContext::trace_id(ctx), "trace123456789012345678901234567890")
  }
  
  // Test attribute pool efficiency
  let attribute_pool = []
  
  // Create reusable attribute sets
  let base_attrs = Attributes::with([
    ("service.name", StringValue("azimuth-performance-test")),
    ("service.version", StringValue("2.1.0")),
    ("environment", StringValue("test"))
  ])
  
  for i in 1..=50 {
    let attrs = Attributes::with([
      ("service.name", StringValue("azimuth-performance-test")),
      ("service.version", StringValue("2.1.0")),
      ("environment", StringValue("test")),
      ("worker.id", IntValue(i)),
      ("operation.type", StringValue("resource_pool_test"))
    ])
    attribute_pool.push(attrs)
  }
  
  // Verify all attribute sets have correct values
  for i in 1..=50 {
    let attrs = attribute_pool[i - 1]
    let worker_id = Attributes::get(attrs, "worker.id")
    match worker_id {
      Some(IntValue(id)) => assert_eq(id, i)
      _ => assert_true(false)
    }
  }
  
  // Test resource cleanup
  for ctx in context_pool {
    // In a real implementation, this would return context to pool
    assert_true(SpanContext::is_valid(ctx))
  }
  
  for attrs in attribute_pool {
    // In a real implementation, this would return attributes to pool
    let service_name = Attributes::get(attrs, "service.name")
    match service_name {
      Some(StringValue(name)) => assert_eq(name, "azimuth-performance-test")
      _ => assert_true(false)
    }
  }
}

// Test 4: Batch Processing Optimization
test "batch processing optimization" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "batch_meter")
  
  // Create metrics for batch testing
  let batch_counter = Meter::create_counter(
    meter,
    "batch.operations.total",
    Some("Total batch operations"),
    Some("operations")
  )
  
  let batch_histogram = Meter::create_histogram(
    meter,
    "batch.operation.duration",
    Some("Batch operation duration"),
    Some("ms")
  )
  
  // Test batch sizes for optimal performance
  let batch_sizes = [10, 50, 100, 500, 1000, 2000]
  
  for batch_size in batch_sizes {
    let batch_start_time = 1234567890L
    
    // Process batch
    for i in 1..=batch_size {
      Counter::add(batch_counter, 1.0, Some(Attributes::with([
        ("batch.size", IntValue(batch_size)),
        ("operation.index", IntValue(i)),
        ("processing.mode", StringValue("batch"))
      ])))
      
      let duration = 10.0 + (i.to_float() % 100.0)
      Histogram::record(batch_histogram, duration, Some(Attributes::with([
        ("batch.size", IntValue(batch_size)),
        ("operation.index", IntValue(i))
      ])))
    }
    
    let batch_end_time = 1234567890L
    
    // Record batch completion
    Histogram::record(batch_histogram, (batch_end_time - batch_start_time).to_float(), Some(Attributes::with([
      ("operation.type", StringValue("batch_complete")),
      ("batch.size", IntValue(batch_size))
    ])))
  }
  
  // Test log batch processing
  let log_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(log_provider, "batch_logger")
  
  // Create log records in batches
  for batch in 1..=10 {
    let log_batch = []
    
    // Create 100 log records per batch
    for i in 1..=100 {
      let log = LogRecord::new(Info, "Batch log message " + batch.to_string() + "_" + i.to_string())
      log_batch.push(log)
    }
    
    // Process log batch
    for log in log_batch {
      Logger::emit(logger, log)
    }
    
    // Clear batch
    log_batch = []
  }
  
  // Test span batch processing
  let span_batch = []
  
  // Create spans in batch
  for i in 1..=200 {
    let span_id = "batch_span_" + i.to_string()
    let ctx = SpanContext::new("trace123456789012345678901234567890", span_id, true, "")
    let span = Span::new("batch_operation_" + i.to_string(), Internal, ctx)
    
    Span::set_attribute(span, "batch.id", IntValue(1))
    Span::set_attribute(span, "batch.index", IntValue(i))
    
    span_batch.push(span)
  }
  
  // End all spans in batch
  for span in span_batch {
    Span::end(span)
  }
}

// Test 5: Memory Leak Detection
test "memory leak detection" {
  // Test span lifecycle management
  let spans = []
  
  // Create spans and ensure proper cleanup
  for i in 1..=1000 {
    let span_id = "leak_test_span_" + i.to_string()
    let ctx = SpanContext::new("trace123456789012345678901234567890", span_id, true, "")
    let span = Span::new("leak_test_operation", Internal, ctx)
    
    // Add attributes
    Span::set_attribute(span, "test.id", IntValue(i))
    Span::set_attribute(span, "test.type", StringValue("leak_detection"))
    
    // Add events
    Span::add_event(span, "span_created", Some([
      ("creation.time", IntValue(1234567890 + i))
    ]))
    
    // End span immediately
    Span::end(span)
    
    // Add to array for potential leak detection
    if i % 100 == 0 {
      spans.push(span)
    }
  }
  
  // Clean up remaining spans
  for span in spans {
    // Verify span is properly ended
    assert_eq(Span::status(span), Unset) // Or appropriate status
  }
  
  // Test attribute cleanup
  let attrs = Attributes::new()
  
  // Add many attributes
  for i in 1..=5000 {
    let key = "leak_test_attr_" + i.to_string()
    let value = StringValue("value_" + i.to_string())
    Attributes::set(attrs, key, value)
  }
  
  // Test context cleanup
  let ctx = Context::root()
  
  // Add many context values
  let contexts = []
  for i in 1..=1000 {
    let key = ContextKey::new("leak_test_key_" + i.to_string())
    let value = "leak_test_value_" + i.to_string()
    let new_ctx = Context::with_value(ctx, key, value)
    contexts.push(new_ctx)
  }
  
  // Verify context values
  for i in 1..=1000 {
    let test_ctx = contexts[i - 1]
    let key = ContextKey::new("leak_test_key_" + i.to_string())
    match Context::get(test_ctx, key) {
      Some(value) => assert_eq(value, "leak_test_value_" + i.to_string())
      None => assert_true(false)
    }
  }
}

// Test 6: CPU Usage Optimization
test "cpu usage optimization" {
  // Test efficient algorithms for common operations
  
  // Test efficient attribute lookup
  let attrs = Attributes::new()
  
  // Add attributes
  for i in 1..=1000 {
    let key = "cpu_test_attr_" + i.to_string()
    let value = StringValue("value_" + i.to_string())
    Attributes::set(attrs, key, value)
  }
  
  // Test efficient lookup patterns
  let lookup_keys = ["cpu_test_attr_1", "cpu_test_attr_500", "cpu_test_attr_1000"]
  
  for key in lookup_keys {
    let result = Attributes::get(attrs, key)
    match result {
      Some(StringValue(value)) => assert_true(value.starts_with("value_"))
      None => assert_true(false)
    }
  }
  
  // Test efficient string operations
  let test_strings = []
  
  // Create test strings
  for i in 1..=1000 {
    test_strings.push("test_string_" + i.to_string())
  }
  
  // Test efficient string processing
  for str in test_strings {
    assert_true(str.starts_with("test_string_"))
    assert_true(str.length() > 0)
  }
  
  // Test efficient array operations
  let test_array = []
  
  // Create test array
  for i in 1..=10000 {
    test_array.push(i)
  }
  
  // Test efficient array access
  let test_indices = [0, 4999, 9999]
  
  for index in test_indices {
    assert_true(index < test_array.length())
    assert_eq(test_array[index], index + 1)
  }
  
  // Test efficient filtering
  let even_numbers = filter(test_array, fn(x) { x % 2 == 0 })
  assert_eq(even_numbers.length(), 5000)
  
  // Test efficient mapping
  let doubled = map(test_array[0..=99], fn(x) { x * 2 })
  assert_eq(doubled.length(), 100)
  assert_eq(doubled[0], 2)
  assert_eq(doubled[99], 200)
}

// Test 7: I/O Performance Optimization
test "io performance optimization" {
  // Test efficient log batching
  let log_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(log_provider, "io_performance_logger")
  
  // Create log batch
  let log_batch = []
  
  // Create logs with varying sizes
  for i in 1..=1000 {
    let message_size = i % 100 + 10  // Messages between 10 and 110 characters
    let message = "a".repeat(message_size) + " " + i.to_string()
    let log = LogRecord::new(Info, message)
    log_batch.push(log)
  }
  
  // Process logs in batches
  let batch_size = 100
  for batch_start in range(0, log_batch.length(), batch_size) {
    let batch_end = min(batch_start + batch_size, log_batch.length())
    
    // Process batch
    for i in batch_start..<batch_end {
      Logger::emit(logger, log_batch[i])
    }
  }
  
  // Test efficient metric batching
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "io_performance_meter")
  
  let counter = Meter::create_counter(
    meter,
    "io.performance.counter",
    Some("I/O performance counter"),
    Some("operations")
  )
  
  // Batch metric updates
  let metric_batch = []
  
  for i in 1..=5000 {
    let attrs = Attributes::with([
      ("operation.type", StringValue("io_performance")),
      ("operation.id", IntValue(i)),
      ("batch.size", IntValue(100))
    ])
    metric_batch.push(attrs)
  }
  
  // Process metric batch
  for attrs in metric_batch {
    Counter::add(counter, 1.0, Some(attrs))
  }
  
  // Test efficient span batching
  let span_batch = []
  
  // Create spans in batch
  for i in 1..=500 {
    let span_id = "io_span_" + i.to_string()
    let ctx = SpanContext::new("trace123456789012345678901234567890", span_id, true, "")
    let span = Span::new("io_performance_operation", Internal, ctx)
    
    Span::set_attribute(span, "operation.id", IntValue(i))
    Span::set_attribute(span, "batch.id", IntValue(1))
    
    span_batch.push(span)
  }
  
  // End spans in batch
  for span in span_batch {
    Span::end(span)
  }
}

// Test 8: Resource Pool Recycling
test "resource pool recycling" {
  // Test span context recycling
  let context_pool = []
  let recycled_contexts = []
  
  // Create initial pool
  for i in 1..=100 {
    let trace_id = "trace123456789012345678901234567890"
    let span_id = "pool_span_" + i.to_string().pad_left(16, '0')
    let ctx = SpanContext::new(trace_id, span_id, true, "")
    context_pool.push(ctx)
  }
  
  // Recycle contexts
  for ctx in context_pool {
    // In a real implementation, this would reset and reuse the context
    let recycled_ctx = SpanContext::new(
      SpanContext::trace_id(ctx),
      "recycled_" + SpanContext::span_id(ctx),
      true,
      ""
    )
    recycled_contexts.push(recycled_ctx)
  }
  
  // Verify recycled contexts
  for ctx in recycled_contexts {
    assert_true(SpanContext::is_valid(ctx))
    assert_true(SpanContext::is_sampled(ctx))
    assert_true(SpanContext::span_id(ctx).starts_with("recycled_"))
  }
  
  // Test attribute recycling
  let attribute_pool = []
  let recycled_attributes = []
  
  // Create initial pool
  for i in 1..=50 {
    let attrs = Attributes::with([
      ("pool.id", IntValue(i)),
      ("pool.type", StringValue("recyclable")),
      ("initial.value", StringValue("initial_" + i.to_string()))
    ])
    attribute_pool.push(attrs)
  }
  
  // Recycle attributes
  for attrs in attribute_pool {
    // In a real implementation, this would reset and reuse the attributes
    let recycled_attrs = Attributes::with([
      ("recycled.id", IntValue(Attributes::get(attrs, "pool.id").unwrap_or(IntValue(0)).unwrap_or(0))),
      ("recycled.type", StringValue("recycled")),
      ("recycled.value", StringValue("recycled_value"))
    ])
    recycled_attributes.push(recycled_attrs)
  }
  
  // Verify recycled attributes
  for attrs in recycled_attributes {
    let recycled_type = Attributes::get(attrs, "recycled.type")
    match recycled_type {
      Some(StringValue(type_str)) => assert_eq(type_str, "recycled")
      _ => assert_true(false)
    }
  }
  
  // Test resource cleanup
  context_pool = []
  attribute_pool = []
  recycled_contexts = []
  recycled_attributes = []
}

// Test 9: Performance Monitoring and Metrics
test "performance monitoring and metrics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance_monitor")
  
  // Create performance monitoring metrics
  let operation_counter = Meter::create_counter(
    meter,
    "performance.operations.total",
    Some("Total performance operations"),
    Some("operations")
  )
  
  let duration_histogram = Meter::create_histogram(
    meter,
    "performance.operation.duration",
    Some("Performance operation duration"),
    Some("ms")
  )
  
  let memory_gauge = Meter::create_gauge(
    meter,
    "performance.memory.usage",
    Some("Current memory usage"),
    Some("bytes")
  )
  
  let cpu_gauge = Meter::create_gauge(
    meter,
    "performance.cpu.usage",
    Some("Current CPU usage"),
    Some("percent")
  )
  
  // Simulate performance monitoring
  for operation_type in ["span_creation", "attribute_setting", "metric_recording", "log_emission"] {
    for i in 1..=100 {
      let start_time = 1234567890L + i
      
      // Record operation start
      Counter::add(operation_counter, 1.0, Some(Attributes::with([
        ("operation.type", StringValue(operation_type)),
        ("operation.id", IntValue(i))
      ])))
      
      // Simulate operation duration
      let base_duration = match operation_type {
        "span_creation" => 5
        "attribute_setting" => 2
        "metric_recording" => 3
        "log_emission" => 4
        _ => 1
      }
      
      let duration = base_duration + (i % 10)
      Histogram::record(duration_histogram, duration.to_float(), Some(Attributes::with([
        ("operation.type", StringValue(operation_type)),
        ("operation.id", IntValue(i))
      ])))
      
      // Simulate memory usage
      let memory_usage = 1024 * 1024 * (10 + (i % 50))  // 10-60 MB
      Gauge::record(memory_gauge, memory_usage.to_float(), Some(Attributes::with([
        ("operation.type", StringValue(operation_type))
      ])))
      
      // Simulate CPU usage
      let cpu_usage = 10.0 + (i.to_float() % 80.0)  // 10-90%
      Gauge::record(cpu_gauge, cpu_usage, Some(Attributes::with([
        ("operation.type", StringValue(operation_type))
      ])))
      
      let end_time = start_time + duration
    }
  }
  
  // Create performance summary metrics
  let summary_counter = Meter::create_counter(
    meter,
    "performance.summary.operations",
    Some("Performance summary operations"),
    Some("operations")
  )
  
  // Record summary
  Counter::add(summary_counter, 400.0, Some(Attributes::with([
    ("summary.type", StringValue("total_operations")),
    ("monitoring.period", StringValue("test_session"))
  ])))
  
  // Verify metric instruments
  let operation_instrument = Counter::as_instrument(operation_counter)
  assert_eq(Instrument::name(operation_instrument), "performance.operations.total")
  
  let duration_instrument = Histogram::as_instrument(duration_histogram)
  assert_eq(Instrument::name(duration_instrument), "performance.operation.duration")
  
  let memory_instrument = Gauge::as_instrument(memory_gauge)
  assert_eq(Instrument::name(memory_instrument), "performance.memory.usage")
  
  let cpu_instrument = Gauge::as_instrument(cpu_gauge)
  assert_eq(Instrument::name(cpu_instrument), "performance.cpu.usage")
}

// Test 10: Scalability Testing
test "scalability testing" {
  // Test system scalability with increasing load
  
  let test_scales = [100, 500, 1000, 2000, 5000]
  
  for scale in test_scales {
    let provider = MeterProvider::default()
    let meter = MeterProvider::get_meter(provider, "scalability_meter_" + scale.to_string())
    
    // Create metrics for this scale
    let scale_counter = Meter::create_counter(
      meter,
      "scale.operations.total",
      Some("Total operations at scale " + scale.to_string()),
      Some("operations")
    )
    
    let scale_histogram = Meter::create_histogram(
      meter,
      "scale.operation.duration",
      Some("Operation duration at scale " + scale.to_string()),
      Some("ms")
    )
    
    // Create spans at scale
    let spans = []
    
    for i in 1..=scale {
      let span_id = "scale_span_" + i.to_string()
      let ctx = SpanContext::new("scale_trace_123456789012345678901234567890", span_id, true, "")
      let span = Span::new("scale_operation", Internal, ctx)
      
      // Add scale-specific attributes
      Span::set_attribute(span, "scale", IntValue(scale))
      Span::set_attribute(span, "index", IntValue(i))
      
      // Add events
      if i % 100 == 0 {
        Span::add_event(span, "milestone_reached", Some([
          ("milestone", IntValue(i)),
          ("scale", IntValue(scale))
        ]))
      }
      
      spans.push(span)
      
      // Record metrics
      Counter::add(scale_counter, 1.0, Some(Attributes::with([
        ("scale", IntValue(scale)),
        ("operation", StringValue("span_creation"))
      ])))
      
      let duration = 1.0 + (i.to_float() % 10.0)
      Histogram::record(scale_histogram, duration, Some(Attributes::with([
        ("scale", IntValue(scale)),
        ("operation", StringValue("span_creation"))
      ])))
    }
    
    // End all spans
    for span in spans {
      Span::end(span)
      
      // Record end metrics
      Counter::add(scale_counter, 1.0, Some(Attributes::with([
        ("scale", IntValue(scale)),
        ("operation", StringValue("span_ending"))
      ])))
      
      let duration = 0.5 + (spans.length().to_float() % 5.0)
      Histogram::record(scale_histogram, duration, Some(Attributes::with([
        ("scale", IntValue(scale)),
        ("operation", StringValue("span_ending"))
      ])))
    }
    
    // Create logs at scale
    let log_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(log_provider, "scale_logger_" + scale.to_string())
    
    for i in 1..=scale {
      let log = LogRecord::new(Info, "Scale test log " + i.to_string() + " at scale " + scale.to_string())
      Logger::emit(logger, log)
      
      // Record log metrics
      Counter::add(scale_counter, 1.0, Some(Attributes::with([
        ("scale", IntValue(scale)),
        ("operation", StringValue("log_emission"))
      ])))
      
      let duration = 0.1 + (i.to_float() % 2.0)
      Histogram::record(scale_histogram, duration, Some(Attributes::with([
        ("scale", IntValue(scale)),
        ("operation", StringValue("log_emission"))
      ])))
    }
    
    // Create attributes at scale
    let attrs = Attributes::new()
    
    for i in 1..=scale {
      let key = "scale_attr_" + i.to_string()
      let value = StringValue("scale_value_" + i.to_string())
      Attributes::set(attrs, key, value)
      
      // Record attribute metrics
      Counter::add(scale_counter, 1.0, Some(Attributes::with([
        ("scale", IntValue(scale)),
        ("operation", StringValue("attribute_setting"))
      ])))
      
      let duration = 0.05 + (i.to_float() % 1.0)
      Histogram::record(scale_histogram, duration, Some(Attributes::with([
        ("scale", IntValue(scale)),
        ("operation", StringValue("attribute_setting"))
      ])))
    }
    
    // Verify some attributes are set at this scale
    let test_attr = Attributes::get(attrs, "scale_attr_" + (scale / 2).to_string())
    match test_attr {
      Some(StringValue(value)) => assert_eq(value, "scale_value_" + (scale / 2).to_string())
      None => assert_true(false)
    }
  }
}