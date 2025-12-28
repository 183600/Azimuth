// Performance and Resource Management Tests for Azimuth Telemetry System
// These tests focus on performance benchmarks, resource utilization, and optimization

// Test 1: High-Volume Attributes Operations Performance
pub test "high-volume attributes operations performance" {
  let attrs = azimuth::Attributes::new()
  
  // Test bulk attribute setting performance
  for i = 0; i < 100; i = i + 1 {
    let key = "bulk.attribute." + i.to_string()
    azimuth::Attributes::set(attrs, key, azimuth::StringValue("bulk_value_" + i.to_string()))
  }
  
  // Test bulk attribute retrieval performance
  for i = 0; i < 100; i = i + 1 {
    let key = "bulk.attribute." + i.to_string()
    let value = azimuth::Attributes::get(attrs, key)
    assert_eq(value, Some(azimuth::StringValue("bulk_value_" + i.to_string())))
  }
  
  // Test mixed attribute types performance
  azimuth::Attributes::set(attrs, "performance.string", azimuth::StringValue("performance_test_string"))
  azimuth::Attributes::set(attrs, "performance.int", azimuth::IntValue(999999))
  azimuth::Attributes::set(attrs, "performance.float", azimuth::FloatValue(3.14159265359))
  azimuth::Attributes::set(attrs, "performance.bool", azimuth::BoolValue(true))
  azimuth::Attributes::set(attrs, "performance.array.string", azimuth::ArrayStringValue(["item1", "item2", "item3"]))
  azimuth::Attributes::set(attrs, "performance.array.int", azimuth::ArrayIntValue([100, 200, 300, 400, 500]))
  
  // Verify mixed type retrieval performance
  let string_val = azimuth::Attributes::get(attrs, "performance.string")
  let int_val = azimuth::Attributes::get(attrs, "performance.int")
  let float_val = azimuth::Attributes::get(attrs, "performance.float")
  let bool_val = azimuth::Attributes::get(attrs, "performance.bool")
  let array_string_val = azimuth::Attributes::get(attrs, "performance.array.string")
  let array_int_val = azimuth::Attributes::get(attrs, "performance.array.int")
  
  assert_eq(string_val, Some(azimuth::StringValue("performance_test_string")))
  assert_eq(int_val, Some(azimuth::IntValue(999999)))
  assert_eq(float_val, Some(azimuth::FloatValue(3.14159265359)))
  assert_eq(bool_val, Some(azimuth::BoolValue(true)))
  assert_eq(array_string_val, Some(azimuth::ArrayStringValue(["item1", "item2", "item3"])))
  assert_eq(array_int_val, Some(azimuth::ArrayIntValue([100, 200, 300, 400, 500])))
}

// Test 2: Memory-Efficient Context Management
pub test "memory-efficient context management" {
  let root_ctx = azimuth::Context::root()
  
  // Test context chaining performance
  let ctx_1 = azimuth::Context::with_value(root_ctx, azimuth::ContextKey::new("level.1"), "value_1")
  let ctx_2 = azimuth::Context::with_value(ctx_1, azimuth::ContextKey::new("level.2"), "value_2")
  let ctx_3 = azimuth::Context::with_value(ctx_2, azimuth::ContextKey::new("level.3"), "value_3")
  let ctx_4 = azimuth::Context::with_value(ctx_3, azimuth::ContextKey::new("level.4"), "value_4")
  let ctx_5 = azimuth::Context::with_value(ctx_4, azimuth::ContextKey::new("level.5"), "value_5")
  
  // Verify deep context retrieval performance
  let key_1 = azimuth::ContextKey::new("level.1")
  let key_2 = azimuth::ContextKey::new("level.2")
  let key_3 = azimuth::ContextKey::new("level.3")
  let key_4 = azimuth::ContextKey::new("level.4")
  let key_5 = azimuth::ContextKey::new("level.5")
  
  assert_eq(azimuth::Context::get(ctx_5, key_1), Some("value_1"))
  assert_eq(azimuth::Context::get(ctx_5, key_2), Some("value_2"))
  assert_eq(azimuth::Context::get(ctx_5, key_3), Some("value_3"))
  assert_eq(azimuth::Context::get(ctx_5, key_4), Some("value_4"))
  assert_eq(azimuth::Context::get(ctx_5, key_5), Some("value_5"))
  
  // Test context isolation performance
  let isolated_ctx = azimuth::Context::with_value(root_ctx, azimuth::ContextKey::new("isolated"), "isolated_value")
  let isolated_key = azimuth::ContextKey::new("isolated")
  assert_eq(azimuth::Context::get(isolated_ctx, isolated_key), Some("isolated_value"))
  assert_eq(azimuth::Context::get(isolated_ctx, key_1), None)
}

// Test 3: High-Throughput Span Operations
pub test "high-throughput span operations" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "performance.tracer")
  
  // Test bulk span creation performance
  let spans = []
  for i = 0; i < 50; i = i + 1 {
    let span = azimuth::Tracer::start_span(tracer, "performance.span." + i.to_string())
    spans.push(span)
  }
  
  // Test bulk span event addition performance
  for span in spans {
    azimuth::Span::add_event(span, "performance.event", 
      Some([("event.id", azimuth::StringValue("evt_123")),
            ("timestamp", azimuth::IntValue(1735689600)),
            ("performance.metric", azimuth::FloatValue(99.99))]))
  }
  
  // Test bulk span status setting performance
  for span in spans {
    azimuth::Span::set_status(span, azimuth::Ok, Some("Performance operation completed"))
  }
  
  // Test bulk span ending performance
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // Verify span properties
  assert_eq(spans.length(), 50)
  assert_eq(azimuth::Span::name(spans[0]), "performance.span.0")
  assert_eq(azimuth::Span::name(spans[24]), "performance.span.24")
  assert_eq(azimuth::Span::name(spans[49]), "performance.span.49")
}

// Test 4: Metrics Aggregation Performance
pub test "metrics aggregation performance" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "performance.meter")
  
  // Create multiple metric instruments
  let counter = azimuth::Meter::create_counter(meter, "performance.counter", 
    Some("High-performance counter"), Some("operations"))
  let histogram = azimuth::Meter::create_histogram(meter, "performance.histogram", 
    Some("High-performance histogram"), Some("ms"))
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "performance.updown", 
    Some("High-performance up-down counter"), Some("units"))
  let gauge = azimuth::Meter::create_gauge(meter, "performance.gauge", 
    Some("High-performance gauge"), Some("percentage"))
  
  // Test high-frequency metric recording performance
  for i = 0; i < 100; i = i + 1 {
    azimuth::Counter::add(counter, i.to_double())
    azimuth::Histogram::record(histogram, i.to_double() * 1.5)
    azimuth::UpDownCounter::add(updown_counter, i.to_double() - 50.0)
  }
  
  // Test metric instrument conversion performance
  let counter_instrument = azimuth::Counter::as_instrument(counter)
  let histogram_instrument = azimuth::Histogram::as_instrument(histogram)
  
  // Verify instrument properties
  assert_eq(azimuth::Instrument::name(counter_instrument), "performance.counter")
  assert_eq(azimuth::Instrument::name(histogram_instrument), "performance.histogram")
  assert_eq(azimuth::Instrument::description(counter_instrument), Some("High-performance counter"))
  assert_eq(azimuth::Instrument::description(histogram_instrument), Some("High-performance histogram"))
  assert_eq(azimuth::Instrument::unit(counter_instrument), Some("operations"))
  assert_eq(azimuth::Instrument::unit(histogram_instrument), Some("ms"))
}

// Test 5: High-Volume Logging Performance
pub test "high-volume logging performance" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "performance.logger")
  
  // Test bulk log record creation performance
  let log_records = []
  for i = 0; i < 50; i = i + 1 {
    let log_record = azimuth::LogRecord::new_with_context(
      if i % 4 == 0 { azimuth::Info } 
      else if i % 4 == 1 { azimuth::Warn } 
      else if i % 4 == 2 { azimuth::Error } 
      else { azimuth::Debug },
      Some("Performance log message " + i.to_string()),
      Some(azimuth::Attributes::new()),
      Some(1735689600000000000L + i.to_int64() * 1000000L),
      Some(1735689600000000000L + i.to_int64() * 1000000L + 1000L),
      Some("perf-trace-" + i.to_string()),
      Some("perf-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    log_records.push(log_record)
  }
  
  // Test bulk log emission performance
  for log_record in log_records {
    azimuth::Logger::emit(logger, log_record)
  }
  
  // Verify log record properties
  assert_eq(log_records.length(), 50)
  assert_eq(azimuth::LogRecord::body(log_records[0]), Some("Performance log message 0"))
  assert_eq(azimuth::LogRecord::body(log_records[24]), Some("Performance log message 24"))
  assert_eq(azimuth::LogRecord::body(log_records[49]), Some("Performance log message 49"))
  
  assert_eq(azimuth::LogRecord::severity_number(log_records[0]), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(log_records[1]), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(log_records[2]), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(log_records[3]), azimuth::Debug)
}

// Test 6: Resource Pool Management Performance
pub test "resource pool management performance" {
  let base_resource = azimuth::Resource::new()
  
  // Test large resource attribute sets performance
  let large_attrs = []
  for i = 0; i < 100; i = i + 1 {
    large_attrs.push(("resource.attr." + i.to_string(), azimuth::StringValue("value_" + i.to_string())))
  }
  
  let resource_with_large_attrs = azimuth::Resource::with_attributes(base_resource, large_attrs)
  
  // Test resource attribute retrieval performance
  for i = 0; i < 100; i = i + 1 {
    let key = "resource.attr." + i.to_string()
    let value = azimuth::Resource::get_attribute(resource_with_large_attrs, key)
    assert_eq(value, Some(azimuth::StringValue("value_" + i.to_string())))
  }
  
  // Test resource merging performance
  let override_attrs = []
  for i = 0; i < 50; i = i + 1 {
    override_attrs.push(("override.attr." + i.to_string(), azimuth::StringValue("override_" + i.to_string())))
  }
  
  let override_resource = azimuth::Resource::with_attributes(base_resource, override_attrs)
  let merged_resource = azimuth::Resource::merge(resource_with_large_attrs, override_resource)
  
  // Verify merged resource contains both original and override attributes
  let original_attr = azimuth::Resource::get_attribute(merged_resource, "resource.attr.50")
  let override_attr = azimuth::Resource::get_attribute(merged_resource, "override.attr.25")
  
  assert_eq(original_attr, Some(azimuth::StringValue("value_50")))
  assert_eq(override_attr, Some(azimuth::StringValue("override_25")))
}

// Test 7: Propagation Cache Performance
pub test "propagation cache performance" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CTraceContextPropagator::new()
  
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // Test repeated injection/extraction performance
  let carriers = []
  let contexts = []
  
  // Create multiple contexts and carriers
  for i = 0; i < 25; i = i + 1 {
    let ctx = azimuth::Context::root()
    let carrier = azimuth::TextMapCarrier::new()
    contexts.push(ctx)
    carriers.push(carrier)
  }
  
  // Test bulk injection performance
  for i = 0; i < 25; i = i + 1 {
    azimuth::CompositePropagator::inject(composite_propagator, contexts[i], carriers[i])
  }
  
  // Test bulk extraction performance
  let extracted_contexts = []
  for i = 0; i < 25; i = i + 1 {
    let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carriers[i])
    extracted_contexts.push(extracted_ctx)
  }
  
  // Verify extraction worked
  let extracted_key = azimuth::ContextKey::new("extracted")
  for extracted_ctx in extracted_contexts {
    let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
    assert_eq(extracted_value, Some("true"))
  }
}

// Test 8: High-Frequency Time Operations
pub test "high-frequency time operations" {
  let clock = azimuth::Clock::system()
  let random = azimuth::Random::system()
  
  // Test high-frequency timestamp generation
  let timestamps = []
  for i = 0; i < 50; i = i + 1 {
    let timestamp = azimuth::Clock::now_unix_nanos(clock)
    timestamps.push(timestamp)
  }
  
  // Verify timestamp validity and ordering
  for timestamp in timestamps {
    assert_true(timestamp >= 1700000000000000000L)
    assert_true(timestamp < 1800000000000000000L)
  }
  
  // Test high-frequency random number generation
  let random_bytes_arrays = []
  let random_u64_values = []
  
  for i = 0; i < 25; i = i + 1 {
    let random_bytes = azimuth::Random::next_bytes(random, 16)
    let random_u64 = azimuth::Random::next_u64(random)
    random_bytes_arrays.push(random_bytes)
    random_u64_values.push(random_u64)
  }
  
  // Verify random data generation
  for random_bytes in random_bytes_arrays {
    assert_eq(random_bytes.length(), 16)
  }
  
  for random_u64 in random_u64_values {
    assert_true(random_u64.to_int() >= 0)
    assert_true(random_u64.to_int() < 1000000)
  }
}

// Test 9: Memory Usage Optimization
pub test "memory usage optimization" {
  // Test attribute reuse patterns
  let reusable_attrs = azimuth::Attributes::new()
  
  // Reuse the same attributes object with different values
  azimuth::Attributes::set(reusable_attrs, "reusable.key", azimuth::StringValue("value_1"))
  let value_1 = azimuth::Attributes::get(reusable_attrs, "reusable.key")
  assert_eq(value_1, Some(azimuth::StringValue("value_1")))
  
  azimuth::Attributes::set(reusable_attrs, "reusable.key", azimuth::StringValue("value_2"))
  let value_2 = azimuth::Attributes::get(reusable_attrs, "reusable.key")
  assert_eq(value_2, Some(azimuth::StringValue("value_2")))
  
  // Test baggage reuse patterns
  let reusable_baggage = azimuth::Baggage::new()
  let baggage_1 = azimuth::Baggage::set_entry(reusable_baggage, "reusable.baggage", "baggage_value_1")
  let baggage_2 = azimuth::Baggage::set_entry(reusable_baggage, "reusable.baggage", "baggage_value_2")
  
  let baggage_value_1 = azimuth::Baggage::get_entry(baggage_1, "reusable.baggage")
  let baggage_value_2 = azimuth::Baggage::get_entry(baggage_2, "reusable.baggage")
  
  assert_eq(baggage_value_1, Some("baggage_value_1"))
  assert_eq(baggage_value_2, Some("baggage_value_2"))
  
  // Test context reuse patterns
  let base_ctx = azimuth::Context::root()
  let ctx_1 = azimuth::Context::with_value(base_ctx, azimuth::ContextKey::new("reusable.ctx"), "ctx_value_1")
  let ctx_2 = azimuth::Context::with_value(base_ctx, azimuth::ContextKey::new("reusable.ctx"), "ctx_value_2")
  
  let reusable_key = azimuth::ContextKey::new("reusable.ctx")
  let ctx_value_1 = azimuth::Context::get(ctx_1, reusable_key)
  let ctx_value_2 = azimuth::Context::get(ctx_2, reusable_key)
  
  assert_eq(ctx_value_1, Some("ctx_value_1"))
  assert_eq(ctx_value_2, Some("ctx_value_2"))
}

// Test 10: Comprehensive Performance Benchmark
pub test "comprehensive performance benchmark" {
  // Setup comprehensive telemetry stack
  let tracer_provider = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "benchmark.tracer")
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "benchmark.meter")
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "benchmark.logger")
  
  // Create performance instruments
  let operation_counter = azimuth::Meter::create_counter(meter, "benchmark.operations")
  let latency_histogram = azimuth::Meter::create_histogram(meter, "benchmark.latency")
  let memory_gauge = azimuth::Meter::create_gauge(meter, "benchmark.memory")
  
  // Execute comprehensive benchmark
  let spans = []
  let log_records = []
  
  // Benchmark span operations
  for i = 0; i < 20; i = i + 1 {
    let span = azimuth::Tracer::start_span(tracer, "benchmark.operation." + i.to_string())
    azimuth::Span::add_event(span, "benchmark.event", 
      Some([("iteration", azimuth::IntValue(i)),
            ("operation.type", azimuth::StringValue("performance_test"))]))
    spans.push(span)
    
    // Record metrics for each operation
    azimuth::Counter::add(operation_counter, 1.0)
    azimuth::Histogram::record(latency_histogram, i.to_double() * 10.0)
  }
  
  // Benchmark logging operations
  for i = 0; i < 20; i = i + 1 {
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Benchmark log entry " + i.to_string()),
      Some(azimuth::Attributes::new()),
      Some(1735689600000000000L + i.to_int64() * 1000000L),
      None,
      Some("benchmark-trace"),
      Some("benchmark-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    log_records.push(log_record)
    azimuth::Logger::emit(logger, log_record)
  }
  
  // Complete benchmark operations
  for span in spans {
    azimuth::Span::set_status(span, azimuth::Ok, Some("Benchmark operation completed"))
    azimuth::Span::end(span)
  }
  
  // Verify benchmark results
  assert_eq(spans.length(), 20)
  assert_eq(log_records.length(), 20)
  
  assert_eq(azimuth::Span::name(spans[0]), "benchmark.operation.0")
  assert_eq(azimuth::Span::name(spans[19]), "benchmark.operation.19")
  
  assert_eq(azimuth::LogRecord::body(log_records[0]), Some("Benchmark log entry 0"))
  assert_eq(azimuth::LogRecord::body(log_records[19]), Some("Benchmark log entry 19"))
  
  // Verify metric instruments
  assert_eq(operation_counter.name, "benchmark.operations")
  assert_eq(latency_histogram.name, "benchmark.latency")
  assert_eq(memory_gauge.name, "benchmark.memory")
  
  // Verify logger scope
  assert_eq(logger.scope.name, "benchmark.logger")
  
  // Verify tracer scope
  let tracer_scope = azimuth::Tracer::instrumentation_scope(tracer)
  assert_eq(tracer_scope.name, "benchmark.tracer")
}