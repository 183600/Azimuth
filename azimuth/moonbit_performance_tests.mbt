// MoonBit performance and concurrency tests for Azimuth telemetry system
// These tests focus on performance benchmarks and concurrent operations

// Test 1: Span creation and operations performance
pub test "Span创建和操作性能测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "performance-test")
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create many spans
  let spans = []
  for i in 0..1000 {
    let span = azimuth::Tracer::start_span(tracer, "perf-span-" + i.to_string())
    spans.push(span)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Perform operations on spans
  for span in spans {
    azimuth::Span::add_event(span, "performance.event", Some([("iteration", azimuth::StringValue(i.to_string()))]))
    azimuth::Span::set_status(span, azimuth::Ok)
  }
  
  let operations_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // End all spans
  for span in spans {
    azimuth::Span::end(span)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let creation_duration = creation_time - start_time
  let operations_duration = operations_time - creation_time
  let end_duration = end_time - operations_time
  let total_duration = end_time - start_time
  
  // Verify performance is within reasonable bounds
  assert_true(creation_duration < 10000000000L)  // Less than 10 seconds for creation
  assert_true(operations_duration < 5000000000L)  // Less than 5 seconds for operations
  assert_true(end_duration < 5000000000L)  // Less than 5 seconds for ending
  assert_true(total_duration < 20000000000L)  // Less than 20 seconds total
  
  // Verify all spans were created
  assert_eq(spans.length(), 1000)
}

// Test 2: Metrics operations performance
pub test "度量操作性能测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "perf-meter")
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create instruments
  let counter = azimuth::Meter::create_counter(meter, "perf-counter")
  let histogram = azimuth::Meter::create_histogram(meter, "perf-histogram")
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "perf-updown-counter")
  let gauge = azimuth::Meter::create_gauge(meter, "perf-gauge")
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Perform many metric operations
  for i in 0..10000 {
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double())
    azimuth::UpDownCounter::add(updown_counter, 1.0)
    // Gauge doesn't have a set operation in simplified implementation
  }
  
  let operations_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let creation_duration = creation_time - start_time
  let operations_duration = operations_time - creation_time
  let total_duration = operations_time - start_time
  
  // Verify performance is within reasonable bounds
  assert_true(creation_duration < 1000000000L)  // Less than 1 second for creation
  assert_true(operations_duration < 5000000000L)  // Less than 5 seconds for operations
  assert_true(total_duration < 6000000000L)  // Less than 6 seconds total
  
  // Verify instrument properties
  assert_eq(counter.name, "perf-counter")
  assert_eq(histogram.name, "perf-histogram")
  assert_eq(updown_counter.name, "perf-updown-counter")
  assert_eq(gauge.name, "perf-gauge")
}

// Test 3: Attributes operations performance
pub test "属性操作性能测试" {
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create many attributes
  let attrs_list = []
  for i in 0..1000 {
    let attrs = azimuth::Attributes::new()
    attrs_list.push(attrs)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Set many attribute values
  for i in 0..attrs_list.length() {
    let attrs = attrs_list[i]
    azimuth::Attributes::set(attrs, "string.key", azimuth::StringValue("value" + i.to_string()))
    azimuth::Attributes::set(attrs, "int.key", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "float.key", azimuth::FloatValue(i.to_double()))
    azimuth::Attributes::set(attrs, "bool.key", azimuth::BoolValue(i % 2 == 0))
  }
  
  let set_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Get many attribute values
  for i in 0..attrs_list.length() {
    let attrs = attrs_list[i]
    let _ = azimuth::Attributes::get(attrs, "string.key")
    let _ = azimuth::Attributes::get(attrs, "int.key")
    let _ = azimuth::Attributes::get(attrs, "float.key")
    let _ = azimuth::Attributes::get(attrs, "bool.key")
    let _ = azimuth::Attributes::get(attrs, "nonexistent.key")
  }
  
  let get_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let creation_duration = creation_time - start_time
  let set_duration = set_time - creation_time
  let get_duration = get_time - set_time
  let total_duration = get_time - start_time
  
  // Verify performance is within reasonable bounds
  assert_true(creation_duration < 1000000000L)  // Less than 1 second for creation
  assert_true(set_duration < 2000000000L)  // Less than 2 seconds for setting
  assert_true(get_duration < 2000000000L)  // Less than 2 seconds for getting
  assert_true(total_duration < 5000000000L)  // Less than 5 seconds total
  
  // Verify all attributes were created
  assert_eq(attrs_list.length(), 1000)
}

// Test 4: Context operations performance
pub test "上下文操作性能测试" {
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create many context keys
  let keys = []
  for i in 0..1000 {
    let key = azimuth::ContextKey::new("key" + i.to_string())
    keys.push(key)
  }
  
  let key_creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create many contexts with values
  let contexts = []
  let root_ctx = azimuth::Context::root()
  for i in 0..keys.length() {
    let ctx = azimuth::Context::with_value(root_ctx, keys[i], "value" + i.to_string())
    contexts.push(ctx)
  }
  
  let context_creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Get many values from contexts
  for i in 0..contexts.length() {
    let _ = azimuth::Context::get(contexts[i], keys[i])
    let _ = azimuth::Context::get(contexts[i], azimuth::ContextKey::new("nonexistent.key"))
  }
  
  let get_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let key_creation_duration = key_creation_time - start_time
  let context_creation_duration = context_creation_time - key_creation_time
  let get_duration = get_time - context_creation_time
  let total_duration = get_time - start_time
  
  // Verify performance is within reasonable bounds
  assert_true(key_creation_duration < 1000000000L)  // Less than 1 second for key creation
  assert_true(context_creation_duration < 2000000000L)  // Less than 2 seconds for context creation
  assert_true(get_duration < 2000000000L)  // Less than 2 seconds for getting
  assert_true(total_duration < 5000000000L)  // Less than 5 seconds total
  
  // Verify all keys and contexts were created
  assert_eq(keys.length(), 1000)
  assert_eq(contexts.length(), 1000)
}

// Test 5: Log record operations performance
pub test "日志记录操作性能测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "perf-logger")
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create many log records
  let log_records = []
  for i in 0..5000 {
    let log_record = azimuth::LogRecord::new(azimuth::Info, "Log message " + i.to_string())
    log_records.push(log_record)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create many detailed log records
  let detailed_log_records = []
  for i in 0..1000 {
    let attrs = azimuth::Attributes::new()
    let detailed_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Detailed log message " + i.to_string()),
      Some(attrs),
      Some(1735689600000000000L + i.to_int64()),
      Some(1735689600000000001L + i.to_int64()),
      Some("trace" + i.to_string()),
      Some("span" + i.to_string()),
      Some(azimuth::Context::root())
    )
    detailed_log_records.push(detailed_record)
  }
  
  let detailed_creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Emit all log records
  for record in log_records {
    azimuth::Logger::emit(logger, record)
  }
  
  for record in detailed_log_records {
    azimuth::Logger::emit(logger, record)
  }
  
  let emit_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let creation_duration = creation_time - start_time
  let detailed_creation_duration = detailed_creation_time - creation_time
  let emit_duration = emit_time - detailed_creation_time
  let total_duration = emit_time - start_time
  
  // Verify performance is within reasonable bounds
  assert_true(creation_duration < 2000000000L)  // Less than 2 seconds for creation
  assert_true(detailed_creation_duration < 2000000000L)  // Less than 2 seconds for detailed creation
  assert_true(emit_duration < 3000000000L)  // Less than 3 seconds for emitting
  assert_true(total_duration < 7000000000L)  // Less than 7 seconds total
  
  // Verify all log records were created
  assert_eq(log_records.length(), 5000)
  assert_eq(detailed_log_records.length(), 1000)
}

// Test 6: Resource operations performance
pub test "资源操作性能测试" {
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create many resources
  let resources = []
  for i in 0..1000 {
    let resource = azimuth::Resource::new()
    resources.push(resource)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create resources with many attributes
  let resources_with_attrs = []
  for i in 0..resources.length() {
    let attrs = [
      ("service.name", azimuth::StringValue("service" + i.to_string())),
      ("service.version", azimuth::StringValue("1.0." + i.to_string())),
      ("service.instance.id", azimuth::StringValue("instance" + i.to_string())),
      ("environment", azimuth::StringValue("test")),
      ("host.name", azimuth::StringValue("host" + i.to_string()))
    ]
    let resource_with_attrs = azimuth::Resource::with_attributes(resources[i], attrs)
    resources_with_attrs.push(resource_with_attrs)
  }
  
  let attrs_creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Get many attribute values
  for i in 0..resources_with_attrs.length() {
    let _ = azimuth::Resource::get_attribute(resources_with_attrs[i], "service.name")
    let _ = azimuth::Resource::get_attribute(resources_with_attrs[i], "service.version")
    let _ = azimuth::Resource::get_attribute(resources_with_attrs[i], "service.instance.id")
    let _ = azimuth::Resource::get_attribute(resources_with_attrs[i], "environment")
    let _ = azimuth::Resource::get_attribute(resources_with_attrs[i], "host.name")
    let _ = azimuth::Resource::get_attribute(resources_with_attrs[i], "nonexistent.attr")
  }
  
  let get_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Merge many resources
  let merged_resources = []
  for i in 0..resources_with_attrs.length() {
    let override_attrs = [("merge.attr", azimuth::StringValue("merge" + i.to_string()))]
    let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), override_attrs)
    let merged_resource = azimuth::Resource::merge(resources_with_attrs[i], override_resource)
    merged_resources.push(merged_resource)
  }
  
  let merge_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let creation_duration = creation_time - start_time
  let attrs_creation_duration = attrs_creation_time - creation_time
  let get_duration = get_time - attrs_creation_time
  let merge_duration = merge_time - get_time
  let total_duration = merge_time - start_time
  
  // Verify performance is within reasonable bounds
  assert_true(creation_duration < 1000000000L)  // Less than 1 second for creation
  assert_true(attrs_creation_duration < 2000000000L)  // Less than 2 seconds for attributes creation
  assert_true(get_duration < 2000000000L)  // Less than 2 seconds for getting
  assert_true(merge_duration < 2000000000L)  // Less than 2 seconds for merging
  assert_true(total_duration < 7000000000L)  // Less than 7 seconds total
  
  // Verify all resources were created
  assert_eq(resources.length(), 1000)
  assert_eq(resources_with_attrs.length(), 1000)
  assert_eq(merged_resources.length(), 1000)
}

// Test 7: Baggage operations performance
pub test "Baggage操作性能测试" {
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create many baggage instances
  let baggage_list = []
  for i in 0..1000 {
    let baggage = azimuth::Baggage::new()
    baggage_list.push(baggage)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Set many baggage entries
  let baggage_with_entries = []
  for i in 0..baggage_list.length() {
    let baggage = baggage_list[i]
    let baggage_with_entry1 = azimuth::Baggage::set_entry(baggage, "key1", "value1-" + i.to_string())
    let baggage_with_entry2 = azimuth::Baggage::set_entry(baggage_with_entry1, "key2", "value2-" + i.to_string())
    let baggage_with_entry3 = azimuth::Baggage::set_entry(baggage_with_entry2, "key3", "value3-" + i.to_string())
    baggage_with_entries.push(baggage_with_entry3)
  }
  
  let set_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Get many baggage entries
  for i in 0..baggage_with_entries.length() {
    let _ = azimuth::Baggage::get_entry(baggage_with_entries[i], "key1")
    let _ = azimuth::Baggage::get_entry(baggage_with_entries[i], "key2")
    let _ = azimuth::Baggage::get_entry(baggage_with_entries[i], "key3")
    let _ = azimuth::Baggage::get_entry(baggage_with_entries[i], "nonexistent.key")
  }
  
  let get_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Remove many baggage entries
  let baggage_after_removal = []
  for i in 0..baggage_with_entries.length() {
    let baggage_after_remove1 = azimuth::Baggage::remove_entry(baggage_with_entries[i], "key1")
    let baggage_after_remove2 = azimuth::Baggage::remove_entry(baggage_after_remove1, "key2")
    baggage_after_removal.push(baggage_after_remove2)
  }
  
  let remove_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let creation_duration = creation_time - start_time
  let set_duration = set_time - creation_time
  let get_duration = get_time - set_time
  let remove_duration = remove_time - get_time
  let total_duration = remove_time - start_time
  
  // Verify performance is within reasonable bounds
  assert_true(creation_duration < 1000000000L)  // Less than 1 second for creation
  assert_true(set_duration < 2000000000L)  // Less than 2 seconds for setting
  assert_true(get_duration < 2000000000L)  // Less than 2 seconds for getting
  assert_true(remove_duration < 2000000000L)  // Less than 2 seconds for removal
  assert_true(total_duration < 7000000000L)  // Less than 7 seconds total
  
  // Verify all baggage instances were created
  assert_eq(baggage_list.length(), 1000)
  assert_eq(baggage_with_entries.length(), 1000)
  assert_eq(baggage_after_removal.length(), 1000)
}

// Test 8: Concurrency simulation test
pub test "并发模拟测试" {
  // Simulate concurrent operations by interleaving different operations
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrency-test")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "concurrency-meter")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "concurrency-logger")
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Simulate concurrent operations by interleaving
  for i in 0..1000 {
    // Create span
    let span = azimuth::Tracer::start_span(tracer, "concurrent-span-" + i.to_string())
    
    // Create metric
    let counter = azimuth::Meter::create_counter(meter, "concurrent-counter-" + i.to_string())
    azimuth::Counter::add(counter, 1.0)
    
    // Create log record
    let log_record = azimuth::LogRecord::new(azimuth::Info, "Concurrent log " + i.to_string())
    
    // Create attributes
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "iteration", azimuth::IntValue(i))
    
    // Create context
    let key = azimuth::ContextKey::new("iteration")
    let ctx = azimuth::Context::with_value(azimuth::Context::root(), key, i.to_string())
    
    // Create baggage
    let baggage = azimuth::Baggage::new()
    let baggage_with_entry = azimuth::Baggage::set_entry(baggage, "iteration", i.to_string())
    
    // Create resource
    let resource = azimuth::Resource::new()
    let resource_with_attrs = azimuth::Resource::with_attributes(resource, [
      ("iteration", azimuth::IntValue(i))
    ])
    
    // Perform operations
    azimuth::Span::add_event(span, "concurrent-event", Some([("iteration", azimuth::IntValue(i))]))
    azimuth::Span::set_status(span, azimuth::Ok)
    azimuth::Span::end(span)
    
    azimuth::Logger::emit(logger, log_record)
    
    // Get values
    let _ = azimuth::Attributes::get(attrs, "iteration")
    let _ = azimuth::Context::get(ctx, key)
    let _ = azimuth::Baggage::get_entry(baggage_with_entry, "iteration")
    let _ = azimuth::Resource::get_attribute(resource_with_attrs, "iteration")
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let total_duration = end_time - start_time
  
  // Verify performance is within reasonable bounds
  assert_true(total_duration < 10000000000L)  // Less than 10 seconds for all operations
}

// Test 9: Memory usage simulation test
pub test "内存使用模拟测试" {
  // Simulate memory usage by creating and retaining many objects
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create and retain many spans
  let spans = []
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "memory-test")
  
  for i in 0..5000 {
    let span = azimuth::Tracer::start_span(tracer, "memory-span-" + i.to_string())
    spans.push(span)
  }
  
  let span_creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create and retain many attributes
  let attrs_list = []
  for i in 0..5000 {
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "key1", azimuth::StringValue("value1-" + i.to_string()))
    azimuth::Attributes::set(attrs, "key2", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "key3", azimuth::FloatValue(i.to_double()))
    attrs_list.push(attrs)
  }
  
  let attrs_creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create and retain many contexts
  let contexts = []
  for i in 0..5000 {
    let key = azimuth::ContextKey::new("key" + i.to_string())
    let ctx = azimuth::Context::with_value(azimuth::Context::root(), key, "value" + i.to_string())
    contexts.push(ctx)
  }
  
  let context_creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create and retain many log records
  let log_records = []
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "memory-test")
  
  for i in 0..5000 {
    let log_record = azimuth::LogRecord::new(azimuth::Info, "Memory test log " + i.to_string())
    log_records.push(log_record)
  }
  
  let log_creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create and retain many resources
  let resources = []
  for i in 0..5000 {
    let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
      ("service.name", azimuth::StringValue("service" + i.to_string())),
      ("service.instance.id", azimuth::StringValue("instance" + i.to_string()))
    ])
    resources.push(resource)
  }
  
  let resource_creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Perform operations on all retained objects
  for i in 0..spans.length() {
    azimuth::Span::add_event(spans[i], "memory-test-event", Some([("index", azimuth::IntValue(i))]))
    azimuth::Span::set_status(spans[i], azimuth::Ok)
  }
  
  for i in 0..attrs_list.length() {
    let _ = azimuth::Attributes::get(attrs_list[i], "key1")
    let _ = azimuth::Attributes::get(attrs_list[i], "key2")
    let _ = azimuth::Attributes::get(attrs_list[i], "key3")
  }
  
  for i in 0..contexts.length() {
    let key = azimuth::ContextKey::new("key" + i.to_string())
    let _ = azimuth::Context::get(contexts[i], key)
  }
  
  for record in log_records {
    azimuth::Logger::emit(logger, record)
  }
  
  for i in 0..resources.length() {
    let _ = azimuth::Resource::get_attribute(resources[i], "service.name")
    let _ = azimuth::Resource::get_attribute(resources[i], "service.instance.id")
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let span_creation_duration = span_creation_time - start_time
  let attrs_creation_duration = attrs_creation_time - span_creation_time
  let context_creation_duration = context_creation_time - attrs_creation_time
  let log_creation_duration = log_creation_time - context_creation_time
  let resource_creation_duration = resource_creation_time - log_creation_time
  let operations_duration = end_time - resource_creation_time
  let total_duration = end_time - start_time
  
  // Verify performance is within reasonable bounds
  assert_true(span_creation_duration < 5000000000L)  // Less than 5 seconds for span creation
  assert_true(attrs_creation_duration < 5000000000L)  // Less than 5 seconds for attributes creation
  assert_true(context_creation_duration < 5000000000L)  // Less than 5 seconds for context creation
  assert_true(log_creation_duration < 5000000000L)  // Less than 5 seconds for log creation
  assert_true(resource_creation_duration < 5000000000L)  // Less than 5 seconds for resource creation
  assert_true(operations_duration < 10000000000L)  // Less than 10 seconds for operations
  assert_true(total_duration < 35000000000L)  // Less than 35 seconds total
  
  // Verify all objects were created
  assert_eq(spans.length(), 5000)
  assert_eq(attrs_list.length(), 5000)
  assert_eq(contexts.length(), 5000)
  assert_eq(log_records.length(), 5000)
  assert_eq(resources.length(), 5000)
}

// Test 10: Stress test with mixed operations
pub test "混合操作压力测试" {
  // Stress test with mixed operations to simulate real-world usage
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "stress-test")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "stress-test")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "stress-test")
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Create instruments
  let counter = azimuth::Meter::create_counter(meter, "stress-counter")
  let histogram = azimuth::Meter::create_histogram(meter, "stress-histogram")
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "stress-updown-counter")
  let gauge = azimuth::Meter::create_gauge(meter, "stress-gauge")
  
  // Perform mixed operations
  for i in 0..10000 {
    // Create span and perform operations
    let span = azimuth::Tracer::start_span(tracer, "stress-span-" + i.to_string())
    azimuth::Span::add_event(span, "stress-event", Some([("iteration", azimuth::IntValue(i))]))
    azimuth::Span::set_status(span, azimuth::Ok)
    
    // Perform metric operations
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double())
    azimuth::UpDownCounter::add(updown_counter, 1.0)
    
    // Create and emit log record
    let log_record = azimuth::LogRecord::new(azimuth::Info, "Stress log " + i.to_string())
    azimuth::Logger::emit(logger, log_record)
    
    // Create and manipulate attributes
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "iteration", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "string.value", azimuth::StringValue("value" + i.to_string()))
    let _ = azimuth::Attributes::get(attrs, "iteration")
    let _ = azimuth::Attributes::get(attrs, "string.value")
    
    // Create and manipulate context
    let key = azimuth::ContextKey::new("iteration")
    let ctx = azimuth::Context::with_value(azimuth::Context::root(), key, i.to_string())
    let _ = azimuth::Context::get(ctx, key)
    
    // Create and manipulate baggage
    let baggage = azimuth::Baggage::new()
    let baggage_with_entry = azimuth::Baggage::set_entry(baggage, "iteration", i.to_string())
    let _ = azimuth::Baggage::get_entry(baggage_with_entry, "iteration")
    
    // Create and manipulate resource
    let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
      ("iteration", azimuth::IntValue(i)),
      ("string.value", azimuth::StringValue("value" + i.to_string()))
    ])
    let _ = azimuth::Resource::get_attribute(resource, "iteration")
    let _ = azimuth::Resource::get_attribute(resource, "string.value")
    
    // End span
    azimuth::Span::end(span)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let total_duration = end_time - start_time
  
  // Verify performance is within reasonable bounds
  assert_true(total_duration < 30000000000L)  // Less than 30 seconds for all operations
  
  // Verify instrument properties
  assert_eq(counter.name, "stress-counter")
  assert_eq(histogram.name, "stress-histogram")
  assert_eq(updown_counter.name, "stress-updown-counter")
  assert_eq(gauge.name, "stress-gauge")
}