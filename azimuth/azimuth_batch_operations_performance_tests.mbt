// Azimuth Batch Operations Performance Tests
// This file contains tests for batch operations performance in the Azimuth telemetry system

// Test 1: Large-scale span batch operations
pub test "large-scale span batch operations" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "batch-performance-test")
  
  // Configure batch processor for performance testing
  let batch_config = azimuth::BatchSpanProcessorConfig::new()
    .with_max_queue_size(10000)
    .with_max_export_batch_size(1000)
    .with_schedule_delay(100)  # 100ms for faster testing
    .with_export_timeout(5000)  # 5 seconds
  
  let batch_processor = azimuth::BatchSpanProcessor::builder(batch_config)
    .build()
  
  // Performance test: Create and end 1000 spans
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let spans = []
  for i = 0; i < 1000; i = i + 1 {
    let span_name = "batch.span." + i.to_string()
    let span = azimuth::Tracer::start_span(tracer, span_name)
    
    # Add attributes
    azimuth::Span::set_attribute(span, "span.index", azimuth::IntValue(i))
    azimuth::Span::set_attribute(span, "batch.test", azimuth::BoolValue(true))
    
    # Add events for some spans
    if i % 10 == 0 {
      azimuth::Span::add_event(span, "batch.event", Some([
        ("event.index", azimuth::StringValue(i.to_string())),
        ("event.type", azimuth::StringValue("milestone"))
      ]))
    }
    
    spans.push(span)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # End all spans in batch
  for span in spans {
    azimuth::Span::end(span)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Calculate performance metrics
  let creation_duration = (creation_time - start_time).to_double() / 1000000.0  # Convert to milliseconds
  let end_duration = (end_time - creation_time).to_double() / 1000000.0
  let total_duration = (end_time - start_time).to_double() / 1000000.0
  
  # Verify performance expectations
  assert_true(creation_duration < 1000.0)  # Should create 1000 spans in less than 1 second
  assert_true(end_duration < 500.0)        # Should end 1000 spans in less than 500ms
  assert_true(total_duration < 1500.0)     # Total operation should be less than 1.5 seconds
  
  # Calculate spans per second
  let spans_per_second = 1000.0 / (total_duration / 1000.0)
  assert_true(spans_per_second > 500.0)    # Should handle at least 500 spans/second
  
  # Force flush and measure export performance
  let flush_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let flush_result = azimuth::BatchSpanProcessor::force_flush(batch_processor)
  let flush_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  assert_true(flush_result)
  
  let flush_duration = (flush_end - flush_start).to_double() / 1000000.0
  assert_true(flush_duration < 5000.0)     # Flush should complete in less than 5 seconds
  
  # Verify batch processor statistics
  let processor_stats = azimuth::BatchSpanProcessor::get_statistics(batch_processor)
  assert_true(processor_stats.total_spans >= 1000)
  assert_true(processor_stats.queue_size >= 0)
  assert_true(processor_stats.export_count >= 1)
  assert_true(processor_stats.average_export_time > 0.0)
}

// Test 2: Batch metric operations performance
pub test "batch metric operations performance" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "batch-metric-test")
  
  # Create various metrics for batch testing
  let counter = azimuth::Meter::create_counter(meter, "batch.counter", Some("Batch counter"), Some("count"))
  let histogram = azimuth::Meter::create_histogram(meter, "batch.histogram", Some("Batch histogram"), Some("ms"))
  let gauge = azimuth::Meter::create_gauge(meter, "batch.gauge", Some("Batch gauge"), Some("percent"))
  
  # Performance test: Batch metric recordings
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Batch counter operations
  for i = 0; i < 5000; i = i + 1 {
    azimuth::Counter::add(counter, 1, [
      ("batch.index", azimuth::IntValue(i)),
      ("batch.type", azimuth::StringValue("counter")),
      ("batch.size", azimuth::IntValue(100))
    ])
  }
  
  let counter_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Batch histogram operations
  for i = 0; i < 5000; i = i + 1 {
    let value = (i % 1000).to_double()
    azimuth::Histogram::record(histogram, value, [
      ("batch.index", azimuth::IntValue(i)),
      ("batch.type", azimuth::StringValue("histogram"))
    ])
  }
  
  let histogram_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Batch gauge operations
  for i = 0; i < 5000; i = i + 1 {
    let value = (i % 100).to_long()
    azimuth::Gauge::set(gauge, value, [
      ("batch.index", azimuth::IntValue(i)),
      ("batch.type", azimuth::StringValue("gauge"))
    ])
  }
  
  let gauge_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Calculate performance metrics
  let counter_duration = (counter_time - start_time).to_double() / 1000000.0
  let histogram_duration = (histogram_time - counter_time).to_double() / 1000000.0
  let gauge_duration = (gauge_time - histogram_time).to_double() / 1000000.0
  let total_duration = (gauge_time - start_time).to_double() / 1000000.0
  
  # Verify performance expectations
  assert_true(counter_duration < 1000.0)     # 5000 counter operations in < 1s
  assert_true(histogram_duration < 1000.0)  # 5000 histogram operations in < 1s
  assert_true(gauge_duration < 1000.0)      # 5000 gauge operations in < 1s
  assert_true(total_duration < 3000.0)      # Total operations in < 3s
  
  # Calculate operations per second
  let counter_ops_per_sec = 5000.0 / (counter_duration / 1000.0)
  let histogram_ops_per_sec = 5000.0 / (histogram_duration / 1000.0)
  let gauge_ops_per_sec = 5000.0 / (gauge_duration / 1000.0)
  
  assert_true(counter_ops_per_sec > 5000.0)    # At least 5000 counter ops/sec
  assert_true(histogram_ops_per_sec > 5000.0) # At least 5000 histogram ops/sec
  assert_true(gauge_ops_per_sec > 5000.0)     # At least 5000 gauge ops/sec
  
  # Test batch metric export performance
  let export_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let export_result = azimuth::MeterProvider::export(provider)
  let export_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let export_duration = (export_end - export_start).to_double() / 1000000.0
  assert_true(export_duration < 2000.0)  # Export should complete in < 2 seconds
}

// Test 3: Batch log operations performance
pub test "batch log operations performance" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "batch-log-test")
  
  # Configure batch log processor
  let log_batch_config = azimuth::BatchLogProcessorConfig::new()
    .with_max_queue_size(5000)
    .with_max_export_batch_size(500)
    .with_schedule_delay(50)   # 50ms for faster testing
    .with_export_timeout(3000) # 3 seconds
  
  let log_batch_processor = azimuth::BatchLogProcessor::new(log_batch_config)
  
  # Performance test: Create and emit 2000 log records
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let log_records = []
  let severity_levels = [azimuth::Trace, azimuth::Debug, azimuth::Info, azimuth::Warn, azimuth::Error, azimuth::Fatal]
  
  for i = 0; i < 2000; i = i + 1 {
    let severity = severity_levels[i % severity_levels.length()]
    let message = "Batch log message " + i.to_string()
    
    let log_record = azimuth::LogRecord::new(severity, message)
    azimuth::LogRecord::set_attribute(log_record, "log.index", azimuth::IntValue(i))
    azimuth::LogRecord::set_attribute(log_record, "batch.test", azimuth::BoolValue(true))
    
    # Add context for some logs
    if i % 20 == 0 {
      let trace_id = "trace-" + i.to_string()
      let span_id = "span-" + i.to_string()
      azimuth::LogRecord::set_trace_id(log_record, trace_id)
      azimuth::LogRecord::set_span_id(log_record, span_id)
    }
    
    log_records.push(log_record)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Emit all log records
  for log_record in log_records {
    azimuth::Logger::emit(logger, log_record)
  }
  
  let emit_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Calculate performance metrics
  let creation_duration = (creation_time - start_time).to_double() / 1000000.0
  let emit_duration = (emit_time - creation_time).to_double() / 1000000.0
  let total_duration = (emit_time - start_time).to_double() / 1000000.0
  
  # Verify performance expectations
  assert_true(creation_duration < 500.0)   # Create 2000 logs in < 500ms
  assert_true(emit_duration < 1000.0)      # Emit 2000 logs in < 1s
  assert_true(total_duration < 1500.0)     # Total operation in < 1.5s
  
  # Calculate logs per second
  let logs_per_second = 2000.0 / (total_duration / 1000.0)
  assert_true(logs_per_second > 1000.0)    # Should handle at least 1000 logs/second
  
  # Force flush and measure export performance
  let flush_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let flush_result = azimuth::BatchLogProcessor::force_flush(log_batch_processor)
  let flush_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  assert_true(flush_result)
  
  let flush_duration = (flush_end - flush_start).to_double() / 1000000.0
  assert_true(flush_duration < 3000.0)     # Flush should complete in < 3 seconds
  
  # Verify log processor statistics
  let processor_stats = azimuth::BatchLogProcessor::get_statistics(log_batch_processor)
  assert_true(processor_stats.total_logs >= 2000)
  assert_true(processor_stats.queue_size >= 0)
  assert_true(processor_stats.export_count >= 1)
  assert_true(processor_stats.average_export_time > 0.0)
}

// Test 4: Batch attribute operations performance
pub test "batch attribute operations performance" {
  # Test performance of batch attribute operations on spans and resources
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "batch-attr-test")
  
  # Create a span for attribute testing
  let span = azimuth::Tracer::start_span(tracer, "batch.attribute.test")
  
  # Performance test: Batch attribute setting
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Set 1000 attributes on the span
  for i = 0; i < 1000; i = i + 1 {
    let key = "attr." + i.to_string()
    let value = "value." + i.to_string()
    azimuth::Span::set_attribute(span, key, azimuth::StringValue(value))
  }
  
  let set_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Test batch attribute retrieval
  let retrieved_values = []
  for i = 0; i < 1000; i = i + 1 {
    let key = "attr." + i.to_string()
    let value = azimuth::Span::get_attribute(span, key)
    retrieved_values.push(value)
  }
  
  let get_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Calculate performance metrics
  let set_duration = (set_time - start_time).to_double() / 1000000.0
  let get_duration = (get_time - set_time).to_double() / 1000000.0
  let total_duration = (get_time - start_time).to_double() / 1000000.0
  
  # Verify performance expectations
  assert_true(set_duration < 500.0)   # Set 1000 attributes in < 500ms
  assert_true(get_duration < 200.0)   # Get 1000 attributes in < 200ms
  assert_true(total_duration < 700.0) # Total operation in < 700ms
  
  # Calculate operations per second
  let set_ops_per_sec = 1000.0 / (set_duration / 1000.0)
  let get_ops_per_sec = 1000.0 / (get_duration / 1000.0)
  
  assert_true(set_ops_per_sec > 2000.0)  # At least 2000 attribute sets/sec
  assert_true(get_ops_per_sec > 5000.0)  # At least 5000 attribute gets/sec
  
  # Verify all values were retrieved correctly
  assert_eq(retrieved_values.length(), 1000)
  for i in 0..1000 {
    match retrieved_values[i] {
      Some(azimuth::StringValue(value)) => assert_eq(value, "value." + i.to_string())
      _ => assert_true(false)
    }
  }
  
  azimuth::Span::end(span)
  
  # Test resource attribute batch operations
  let resource = azimuth::Resource::new()
  
  let resource_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Set 500 resource attributes
  let resource_attrs = []
  for i = 0; i < 500; i = i + 1 {
    let key = "resource.attr." + i.to_string()
    let value = "resource.value." + i.to_string()
    resource_attrs.push((key, azimuth::StringValue(value)))
  }
  
  let enriched_resource = azimuth::Resource::with_attributes(resource, resource_attrs)
  
  let resource_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let resource_duration = (resource_end - resource_start).to_double() / 1000000.0
  assert_true(resource_duration < 300.0)  # Set 500 resource attributes in < 300ms
  
  # Test batch resource attribute retrieval
  let retrieval_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let retrieved_resource_attrs = []
  for i = 0; i < 500; i = i + 1 {
    let key = "resource.attr." + i.to_string()
    let value = azimuth::Resource::get_attribute(enriched_resource, key)
    retrieved_resource_attrs.push(value)
  }
  
  let retrieval_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let retrieval_duration = (retrieval_end - retrieval_start).to_double() / 1000000.0
  assert_true(retrieval_duration < 150.0)  # Get 500 resource attributes in < 150ms
  
  # Verify resource attribute retrieval
  assert_eq(retrieved_resource_attrs.length(), 500)
  for i in 0..500 {
    match retrieved_resource_attrs[i] {
      Some(azimuth::StringValue(value)) => assert_eq(value, "resource.value." + i.to_string())
      _ => assert_true(false)
    }
  }
}

// Test 5: Batch context and baggage operations performance
pub test "batch context and baggage operations performance" {
  # Test performance of batch context and baggage operations
  
  let root_ctx = azimuth::Context::root()
  
  # Performance test: Batch context operations
  let context_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Create context with 1000 key-value pairs
  let ctx_with_values = ref root_ctx
  for i = 0; i < 1000; i = i + 1 {
    let key = azimuth::ContextKey::new("context.key." + i.to_string())
    let value = "context.value." + i.to_string()
    ctx_with_values = azimuth::Context::with_value(ctx_with_values, key, value)
  }
  
  let context_set_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Test batch context retrieval
  let retrieved_context_values = []
  for i = 0; i < 1000; i = i + 1 {
    let key = azimuth::ContextKey::new("context.key." + i.to_string())
    let value = azimuth::Context::get(ctx_with_values, key)
    retrieved_context_values.push(value)
  }
  
  let context_get_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Calculate context performance metrics
  let context_set_duration = (context_set_time - context_start).to_double() / 1000000.0
  let context_get_duration = (context_get_time - context_set_time).to_double() / 1000000.0
  let context_total_duration = (context_get_time - context_start).to_double() / 1000000.0
  
  # Verify context performance expectations
  assert_true(context_set_duration < 1000.0)  # Set 1000 context values in < 1s
  assert_true(context_get_duration < 500.0)   # Get 1000 context values in < 500ms
  assert_true(context_total_duration < 1500.0) # Total operation in < 1.5s
  
  # Calculate context operations per second
  let context_set_ops_per_sec = 1000.0 / (context_set_duration / 1000.0)
  let context_get_ops_per_sec = 1000.0 / (context_get_duration / 1000.0)
  
  assert_true(context_set_ops_per_sec > 1000.0)  # At least 1000 context sets/sec
  assert_true(context_get_ops_per_sec > 2000.0)  # At least 2000 context gets/sec
  
  # Verify all context values were retrieved correctly
  assert_eq(retrieved_context_values.length(), 1000)
  for i in 0..1000 {
    match retrieved_context_values[i] {
      Some(value) => assert_eq(value, "context.value." + i.to_string())
      None => assert_true(false)
    }
  }
  
  # Performance test: Batch baggage operations
  let baggage_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Create baggage with 500 entries
  let baggage_with_entries = ref azimuth::Baggage::new()
  for i = 0; i < 500; i = i + 1 {
    let key = "baggage.key." + i.to_string()
    let value = "baggage.value." + i.to_string()
    baggage_with_entries = azimuth::Baggage::set_entry(baggage_with_entries, key, value)
  }
  
  let baggage_set_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Test batch baggage retrieval
  let retrieved_baggage_values = []
  for i = 0; i < 500; i = i + 1 {
    let key = "baggage.key." + i.to_string()
    let value = azimuth::Baggage::get_entry(baggage_with_entries, key)
    retrieved_baggage_values.push(value)
  }
  
  let baggage_get_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Calculate baggage performance metrics
  let baggage_set_duration = (baggage_set_time - baggage_start).to_double() / 1000000.0
  let baggage_get_duration = (baggage_get_time - baggage_set_time).to_double() / 1000000.0
  let baggage_total_duration = (baggage_get_time - baggage_start).to_double() / 1000000.0
  
  # Verify baggage performance expectations
  assert_true(baggage_set_duration < 800.0)   # Set 500 baggage entries in < 800ms
  assert_true(baggage_get_duration < 400.0)   # Get 500 baggage entries in < 400ms
  assert_true(baggage_total_duration < 1200.0) # Total operation in < 1.2s
  
  # Calculate baggage operations per second
  let baggage_set_ops_per_sec = 500.0 / (baggage_set_duration / 1000.0)
  let baggage_get_ops_per_sec = 500.0 / (baggage_get_duration / 1000.0)
  
  assert_true(baggage_set_ops_per_sec > 600.0)   # At least 600 baggage sets/sec
  assert_true(baggage_get_ops_per_sec > 1200.0)  # At least 1200 baggage gets/sec
  
  # Verify all baggage values were retrieved correctly
  assert_eq(retrieved_baggage_values.length(), 500)
  for i in 0..500 {
    match retrieved_baggage_values[i] {
      Some(value) => assert_eq(value, "baggage.value." + i.to_string())
      None => assert_true(false)
    }
  }
  
  # Test batch baggage serialization/deserialization performance
  let serialization_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Serialize baggage to string
  let serialized_baggage = azimuth::Baggage::serialize(baggage_with_entries)
  
  let serialization_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Deserialize baggage from string
  let deserialization_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let deserialized_baggage = azimuth::Baggage::deserialize(serialized_baggage)
  
  let deserialization_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # Calculate serialization performance metrics
  let serialization_duration = (serialization_end - serialization_start).to_double() / 1000000.0
  let deserialization_duration = (deserialization_end - deserialization_start).to_double() / 1000000.0
  
  # Verify serialization performance expectations
  assert_true(serialization_duration < 200.0)      # Serialize 500 entries in < 200ms
  assert_true(deserialization_duration < 300.0)   # Deserialize 500 entries in < 300ms
  
  # Verify serialization/deserialization correctness
  assert_true(serialized_baggage.length() > 0)
  
  # Check that deserialized baggage has the same number of entries
  let deserialized_count = azimuth::Baggage::get_entry_count(deserialized_baggage)
  assert_eq(deserialized_count, 500)
}