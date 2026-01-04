// Azimuth Serialization Format Test Suite
// This file contains comprehensive test cases for serialization format scenarios

// Test 1: Basic Span Serialization
test "basic span serialization" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "serialization_test_tracer")
  
  // Create a span with basic attributes
  let span = Tracer::start_span(tracer, "test_span")
  Span::add_attribute(span, "service.name", "test_service")
  Span::add_attribute(span, "service.version", "1.0.0")
  Span::add_attribute(span, "environment", "test")
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::end(span)
  
  // Serialize the span
  let serialized = Span::serialize(span)
  
  // Verify serialization produced data
  assert_true(serialized.length() > 0)
  
  // Deserialize the span
  let deserialized = Span::deserialize(serialized)
  
  // Verify deserialization worked
  assert_true(deserialized.is_some())
  
  match deserialized {
    Some(deserialized_span) => {
      // Verify basic properties
      assert_eq(deserialized_span.name, "test_span")
      assert_eq(deserialized_span.status, Ok)
      assert_eq(deserialized_span.status_message, Some("Operation completed"))
      
      // Verify attributes
      assert_true(deserialized_span.attributes.contains(("service.name", "test_service")))
      assert_true(deserialized_span.attributes.contains(("service.version", "1.0.0")))
      assert_true(deserialized_span.attributes.contains(("environment", "test")))
    }
    None => assert_true(false)
  }
}

// Test 2: Complex Span Serialization
test "complex span serialization" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "complex_serialization_tracer")
  
  // Create a complex span with events and links
  let span = Tracer::start_span(tracer, "complex_span")
  Span::add_attribute(span, "service.name", "complex_service")
  Span::add_attribute(span, "user.id", "user123")
  Span::add_attribute(span, "request.id", "req456")
  
  // Add events
  Span::add_event(span, "db.query.start", Some([
    ("db.statement", "SELECT * FROM users"),
    ("db.type", "postgresql")
  ]))
  
  Span::add_event(span, "db.query.end", Some([
    ("db.rows_affected", "10"),
    ("db.duration_ms", "150")
  ]))
  
  // Add another event with different attributes
  Span::add_event(span, "cache.hit", Some([
    ("cache.key", "user123"),
    ("cache.size", "1024")
  ]))
  
  // Set status
  Span::set_status(span, Ok, Some("Complex operation completed"))
  
  // End the span
  Span::end(span)
  
  // Serialize the span
  let serialized = Span::serialize(span)
  
  // Verify serialization produced data
  assert_true(serialized.length() > 0)
  
  // Deserialize the span
  let deserialized = Span::deserialize(serialized)
  
  // Verify deserialization worked
  assert_true(deserialized.is_some())
  
  match deserialized {
    Some(deserialized_span) => {
      // Verify basic properties
      assert_eq(deserialized_span.name, "complex_span")
      assert_eq(deserialized_span.status, Ok)
      assert_eq(deserialized_span.status_message, Some("Complex operation completed"))
      
      // Verify attributes
      assert_true(deserialized_span.attributes.contains(("service.name", "complex_service")))
      assert_true(deserialized_span.attributes.contains(("user.id", "user123")))
      assert_true(deserialized_span.attributes.contains(("request.id", "req456")))
      
      // Verify events
      assert_eq(deserialized_span.events.length(), 3)
      
      let db_start_event = deserialized_span.events.find(fn(e) { e.name == "db.query.start" })
      match db_start_event {
        Some(event) => {
          assert_true(event.attributes.contains(("db.statement", "SELECT * FROM users")))
          assert_true(event.attributes.contains(("db.type", "postgresql")))
        }
        None => assert_true(false)
      }
      
      let db_end_event = deserialized_span.events.find(fn(e) { e.name == "db.query.end" })
      match db_end_event {
        Some(event) => {
          assert_true(event.attributes.contains(("db.rows_affected", "10")))
          assert_true(event.attributes.contains(("db.duration_ms", "150")))
        }
        None => assert_true(false)
      }
      
      let cache_event = deserialized_span.events.find(fn(e) { e.name == "cache.hit" })
      match cache_event {
        Some(event) => {
          assert_true(event.attributes.contains(("cache.key", "user123")))
          assert_true(event.attributes.contains(("cache.size", "1024")))
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 3: Metric Serialization
test "metric serialization" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "metric_serialization_meter")
  
  // Create metrics with different types
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  let gauge = Meter::create_gauge(meter, "test_gauge", Some("Test gauge"), Some("value"))
  
  // Record metric values with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", "test_service")
  Attributes::set(attrs, "environment", "test")
  
  Counter::add(counter, 42.0, Some(attrs))
  Histogram::record(histogram, 123.45, Some(attrs))
  Gauge::set(gauge, 67.89)
  
  // Force metric export
  TelemetryProvider::force_flush(provider)
  
  // Get metrics for serialization
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  assert_eq(metrics.length(), 3)
  
  // Serialize each metric
  let mut serialized_metrics = []
  for metric in metrics {
    let serialized = Metric::serialize(metric)
    assert_true(serialized.length() > 0)
    serialized_metrics = serialized_metrics.push(serialized)
  }
  
  // Deserialize metrics
  for i in 0..serialized_metrics.length() {
    let serialized = serialized_metrics[i]
    let deserialized = Metric::deserialize(serialized)
    
    assert_true(deserialized.is_some())
    
    match deserialized {
      Some(deserialized_metric) => {
        let original_metric = metrics[i]
        
        // Verify basic properties
        assert_eq(deserialized_metric.name, original_metric.name)
        assert_eq(deserialized_metric.value, original_metric.value)
        assert_eq(deserialized_metric.unit, original_metric.unit)
        
        // Verify attributes
        if original_metric.attributes.length() > 0 {
          assert_eq(deserialized_metric.attributes.length(), original_metric.attributes.length())
          
          for attr in original_metric.attributes {
            assert_true(deserialized_metric.attributes.contains(attr))
          }
        }
      }
      None => assert_true(false)
    }
  }
}

// Test 4: Log Record Serialization
test "log record serialization" {
  let provider = TelemetryProvider::new()
  let logger = TelemetryProvider::get_logger(provider, "log_serialization_logger")
  
  // Create log records with different severity levels
  let debug_log = Logger::create_log_record(logger, Debug, "Debug message")
  LogRecord::add_attribute(debug_log, "component", "test")
  LogRecord::add_attribute(debug_log, "version", "1.0.0")
  
  let info_log = Logger::create_log_record(logger, Info, "Info message")
  LogRecord::add_attribute(info_log, "component", "test")
  LogRecord::add_attribute(info_log, "user.id", "user123")
  
  let warning_log = Logger::create_log_record(logger, Warning, "Warning message")
  LogRecord::add_attribute(warning_log, "component", "test")
  LogRecord::add_attribute(warning_log, "warning.code", "DEPRECATED_API")
  
  let error_log = Logger::create_log_record(logger, Error, "Error message")
  LogRecord::add_attribute(error_log, "component", "test")
  LogRecord::add_attribute(error_log, "error.code", "VALIDATION_FAILED")
  LogRecord::add_attribute(error_log, "error.message", "Invalid input parameter")
  
  // Emit logs
  Logger::emit(logger, debug_log)
  Logger::emit(logger, info_log)
  Logger::emit(logger, warning_log)
  Logger::emit(logger, error_log)
  
  // Force log export
  TelemetryProvider::force_flush(provider)
  
  // Get logs for serialization
  let logs = TelemetryProvider::get_collected_logs(provider)
  assert_eq(logs.length(), 4)
  
  // Serialize each log
  let mut serialized_logs = []
  for log in logs {
    let serialized = LogRecord::serialize(log)
    assert_true(serialized.length() > 0)
    serialized_logs = serialized_logs.push(serialized)
  }
  
  // Deserialize logs
  for i in 0..serialized_logs.length() {
    let serialized = serialized_logs[i]
    let deserialized = LogRecord::deserialize(serialized)
    
    assert_true(deserialized.is_some())
    
    match deserialized {
      Some(deserialized_log) => {
        let original_log = logs[i]
        
        // Verify basic properties
        assert_eq(deserialized_log.severity, original_log.severity)
        assert_eq(deserialized_log.message, original_log.message)
        
        // Verify attributes
        if original_log.attributes.length() > 0 {
          assert_eq(deserialized_log.attributes.length(), original_log.attributes.length())
          
          for attr in original_log.attributes {
            assert_true(deserialized_log.attributes.contains(attr))
          }
        }
      }
      None => assert_true(false)
    }
  }
}

// Test 5: Resource Serialization
test "resource serialization" {
  // Create a resource with attributes
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", "test_service"),
    ("service.version", "1.0.0"),
    ("service.instance.id", "instance-12345"),
    ("deployment.environment", "test"),
    ("host.name", "test-host"),
    ("process.id", "1234"),
    ("process.executable.name", "test_app"),
    ("process.command_args", "test_app --config=test.conf")
  ])
  
  // Serialize the resource
  let serialized = Resource::serialize(resource)
  
  // Verify serialization produced data
  assert_true(serialized.length() > 0)
  
  // Deserialize the resource
  let deserialized = Resource::deserialize(serialized)
  
  // Verify deserialization worked
  assert_true(deserialized.is_some())
  
  match deserialized {
    Some(deserialized_resource) => {
      // Verify all attributes are present
      let expected_attributes = [
        ("service.name", "test_service"),
        ("service.version", "1.0.0"),
        ("service.instance.id", "instance-12345"),
        ("deployment.environment", "test"),
        ("host.name", "test-host"),
        ("process.id", "1234"),
        ("process.executable.name", "test_app"),
        ("process.command_args", "test_app --config=test.conf")
      ]
      
      for attr in expected_attributes {
        assert_true(Resource::get_attribute(deserialized_resource, attr.0) == Some(StringValue(attr.1)))
      }
    }
    None => assert_true(false)
  }
}

// Test 6: Context Serialization
test "context serialization" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "context_serialization_tracer")
  
  // Create a span with context
  let span = Tracer::start_span(tracer, "context_test_span")
  let context = Span::span_context(span)
  
  // Serialize the context
  let serialized = SpanContext::serialize(context)
  
  // Verify serialization produced data
  assert_true(serialized.length() > 0)
  
  // Deserialize the context
  let deserialized = SpanContext::deserialize(serialized)
  
  // Verify deserialization worked
  assert_true(deserialized.is_some())
  
  match deserialized {
    Some(deserialized_context) => {
      // Verify context properties
      assert_eq(SpanContext::trace_id(deserialized_context), SpanContext::trace_id(context))
      assert_eq(SpanContext::span_id(deserialized_context), SpanContext::span_id(context))
      assert_eq(SpanContext::is_sampled(deserialized_context), SpanContext::is_sampled(context))
      assert_eq(SpanContext::is_valid(deserialized_context), SpanContext::is_valid(context))
    }
    None => assert_true(false)
  }
  
  Span::end(span)
}

// Test 7: Baggage Serialization
test "baggage serialization" {
  // Create baggage with entries
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "user123")
  let updated_baggage2 = Baggage::set_entry(updated_baggage, "request.id", "req456")
  let final_baggage = Baggage::set_entry(updated_baggage2, "session.id", "session789")
  
  // Serialize the baggage
  let serialized = Baggage::serialize(final_baggage)
  
  // Verify serialization produced data
  assert_true(serialized.length() > 0)
  
  // Deserialize the baggage
  let deserialized = Baggage::deserialize(serialized)
  
  // Verify deserialization worked
  assert_true(deserialized.is_some())
  
  match deserialized {
    Some(deserialized_baggage) => {
      // Verify baggage entries
      let user_id = Baggage::get_entry(deserialized_baggage, "user.id")
      let request_id = Baggage::get_entry(deserialized_baggage, "request.id")
      let session_id = Baggage::get_entry(deserialized_baggage, "session.id")
      
      match user_id {
        Some(value) => assert_eq(value, "user123")
        None => assert_true(false)
      }
      
      match request_id {
        Some(value) => assert_eq(value, "req456")
        None => assert_true(false)
      }
      
      match session_id {
        Some(value) => assert_eq(value, "session789")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 8: Batch Serialization
test "batch serialization" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "batch_serialization_tracer")
  let meter = TelemetryProvider::get_meter(provider, "batch_serialization_meter")
  let logger = TelemetryProvider::get_logger(provider, "batch_serialization_logger")
  
  // Create a batch of telemetry data
  let batch_size = 10
  
  // Create spans
  let mut spans = []
  for i in 0..batch_size {
    let span = Tracer::start_span(tracer, "batch_span_" + i.to_string())
    Span::add_attribute(span, "batch_index", i.to_string())
    Span::end(span)
    spans = spans.push(span)
  }
  
  // Create metrics
  let counter = Meter::create_counter(meter, "batch_counter")
  for i in 0..batch_size {
    let attrs = Attributes::new()
    Attributes::set(attrs, "batch_index", i.to_string())
    Counter::add(counter, i.to_float(), Some(attrs))
  }
  
  // Create logs
  for i in 0..batch_size {
    let log_record = Logger::create_log_record(logger, Info, "Batch log " + i.to_string())
    LogRecord::add_attribute(log_record, "batch_index", i.to_string())
    Logger::emit(logger, log_record)
  }
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Get all telemetry data
  let collected_spans = TelemetryProvider::get_collected_spans(provider)
  let collected_metrics = TelemetryProvider::get_collected_metrics(provider)
  let collected_logs = TelemetryProvider::get_collected_logs(provider)
  
  assert_eq(collected_spans.length(), batch_size)
  assert_eq(collected_metrics.length(), batch_size)
  assert_eq(collected_logs.length(), batch_size)
  
  // Create a batch
  let batch = Batch::new()
  Batch::add_spans(batch, collected_spans)
  Batch::add_metrics(batch, collected_metrics)
  Batch::add_logs(batch, collected_logs)
  
  // Serialize the batch
  let serialized = Batch::serialize(batch)
  
  // Verify serialization produced data
  assert_true(serialized.length() > 0)
  
  // Deserialize the batch
  let deserialized = Batch::deserialize(serialized)
  
  // Verify deserialization worked
  assert_true(deserialized.is_some())
  
  match deserialized {
    Some(deserialized_batch) => {
      // Verify batch contents
      let deserialized_spans = Batch::get_spans(deserialized_batch)
      let deserialized_metrics = Batch::get_metrics(deserialized_batch)
      let deserialized_logs = Batch::get_logs(deserialized_batch)
      
      assert_eq(deserialized_spans.length(), batch_size)
      assert_eq(deserialized_metrics.length(), batch_size)
      assert_eq(deserialized_logs.length(), batch_size)
      
      // Verify span properties
      for i in 0..deserialized_spans.length() {
        let span = deserialized_spans[i]
        let expected_name = "batch_span_" + i.to_string()
        assert_eq(span.name, expected_name)
        assert_true(span.attributes.contains(("batch_index", i.to_string())))
      }
      
      // Verify metric properties
      for i in 0..deserialized_metrics.length() {
        let metric = deserialized_metrics[i]
        assert_eq(metric.name, "batch_counter")
        assert_eq(metric.value, i.to_float())
        assert_true(metric.attributes.contains(("batch_index", i.to_string())))
      }
      
      // Verify log properties
      for i in 0..deserialized_logs.length() {
        let log = deserialized_logs[i]
        assert_eq(log.severity, Info)
        assert_eq(log.message, "Batch log " + i.to_string())
        assert_true(log.attributes.contains(("batch_index", i.to_string())))
      }
    }
    None => assert_true(false)
  }
}

// Test 9: Compression Serialization
test "compression serialization" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "compression_tracer")
  
  // Create a large span with many attributes and events
  let span = Tracer::start_span(tracer, "large_span")
  
  // Add many attributes
  for i in 0..100 {
    let key = "attr_" + i.to_string()
    let value = "value_" + i.to_string() + "_".repeat(20) // Make values longer
    Span::add_attribute(span, key, value)
  }
  
  // Add many events
  for i in 0..50 {
    let event_name = "event_" + i.to_string()
    let event_attrs = [
      ("event_index", i.to_string()),
      ("event_data", "data_".repeat(10) + i.to_string())
    ]
    Span::add_event(span, event_name, Some(event_attrs))
  }
  
  Span::end(span)
  
  // Serialize without compression
  let serialized_uncompressed = Span::serialize(span)
  
  // Serialize with compression
  let serialized_compressed = Span::serialize_with_compression(span, CompressionType::Gzip)
  
  // Verify both serializations produced data
  assert_true(serialized_uncompressed.length() > 0)
  assert_true(serialized_compressed.length() > 0)
  
  // Compression should reduce size
  assert_true(serialized_compressed.length() < serialized_uncompressed.length())
  
  // Deserialize without compression
  let deserialized_uncompressed = Span::deserialize(serialized_uncompressed)
  
  // Deserialize with compression
  let deserialized_compressed = Span::deserialize_with_compression(serialized_compressed, CompressionType::Gzip)
  
  // Verify both deserializations worked
  assert_true(deserialized_uncompressed.is_some())
  assert_true(deserialized_compressed.is_some())
  
  match (deserialized_uncompressed, deserialized_compressed) {
    (Some(uncompressed), Some(compressed)) => {
      // Verify both deserializations produce the same result
      assert_eq(uncompressed.name, compressed.name)
      assert_eq(uncompressed.status, compressed.status)
      assert_eq(uncompressed.attributes.length(), compressed.attributes.length())
      assert_eq(uncompressed.events.length(), compressed.events.length())
      
      // Verify all attributes are present
      for i in 0..100 {
        let key = "attr_" + i.to_string()
        let value = "value_" + i.to_string() + "_".repeat(20)
        assert_true(uncompressed.attributes.contains((key, value)))
        assert_true(compressed.attributes.contains((key, value)))
      }
      
      // Verify all events are present
      for i in 0..50 {
        let event_name = "event_" + i.to_string()
        let uncompressed_event = uncompressed.events.find(fn(e) { e.name == event_name })
        let compressed_event = compressed.events.find(fn(e) { e.name == event_name })
        
        assert_true(uncompressed_event.is_some())
        assert_true(compressed_event.is_some())
        
        match (uncompressed_event, compressed_event) {
          (Some(ue), Some(ce)) => {
            assert_eq(ue.attributes.length(), ce.attributes.length())
            assert_true(ue.attributes.contains(("event_index", i.to_string())))
            assert_true(ce.attributes.contains(("event_index", i.to_string())))
          }
          _ => assert_true(false)
        }
      }
    }
    _ => assert_true(false)
  }
}

// Test 10: Format Compatibility
test "format compatibility" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "compatibility_tracer")
  
  // Create a span with various data types
  let span = Tracer::start_span(tracer, "compatibility_span")
  
  // Add attributes with different value types
  Span::add_attribute(span, "string_attr", "string_value")
  Span::add_attribute(span, "int_attr", "42")
  Span::add_attribute(span, "float_attr", "3.14")
  Span::add_attribute(span, "bool_attr", "true")
  
  // Add events with different attribute types
  Span::add_event(span, "string_event", Some([
    ("string_key", "string_value")
  ]))
  
  Span::add_event(span, "numeric_event", Some([
    ("int_key", "123"),
    ("float_key", "456.789")
  ]))
  
  Span::add_event(span, "bool_event", Some([
    ("bool_key_true", "true"),
    ("bool_key_false", "false")
  ]))
  
  Span::end(span)
  
  // Serialize to different formats
  let json_serialized = Span::serialize_to_format(span, SerializationFormat::JSON)
  let protobuf_serialized = Span::serialize_to_format(span, SerializationFormat::Protobuf)
  let xml_serialized = Span::serialize_to_format(span, SerializationFormat::XML)
  
  // Verify all serializations produced data
  assert_true(json_serialized.length() > 0)
  assert_true(protobuf_serialized.length() > 0)
  assert_true(xml_serialized.length() > 0)
  
  // Deserialize from different formats
  let json_deserialized = Span::deserialize_from_format(json_serialized, SerializationFormat::JSON)
  let protobuf_deserialized = Span::deserialize_from_format(protobuf_serialized, SerializationFormat::Protobuf)
  let xml_deserialized = Span::deserialize_from_format(xml_serialized, SerializationFormat::XML)
  
  // Verify all deserializations worked
  assert_true(json_deserialized.is_some())
  assert_true(protobuf_deserialized.is_some())
  assert_true(xml_deserialized.is_some())
  
  match (json_deserialized, protobuf_deserialized, xml_deserialized) {
    (Some(json_span), Some(protobuf_span), Some(xml_span)) => {
      // Verify all deserializations produce equivalent results
      assert_eq(json_span.name, protobuf_span.name)
      assert_eq(json_span.name, xml_span.name)
      
      assert_eq(json_span.status, protobuf_span.status)
      assert_eq(json_span.status, xml_span.status)
      
      // Verify attributes are present in all formats
      assert_true(json_span.attributes.contains(("string_attr", "string_value")))
      assert_true(protobuf_span.attributes.contains(("string_attr", "string_value")))
      assert_true(xml_span.attributes.contains(("string_attr", "string_value")))
      
      assert_true(json_span.attributes.contains(("int_attr", "42")))
      assert_true(protobuf_span.attributes.contains(("int_attr", "42")))
      assert_true(xml_span.attributes.contains(("int_attr", "42")))
      
      assert_true(json_span.attributes.contains(("float_attr", "3.14")))
      assert_true(protobuf_span.attributes.contains(("float_attr", "3.14")))
      assert_true(xml_span.attributes.contains(("float_attr", "3.14")))
      
      assert_true(json_span.attributes.contains(("bool_attr", "true")))
      assert_true(protobuf_span.attributes.contains(("bool_attr", "true")))
      assert_true(xml_span.attributes.contains(("bool_attr", "true")))
      
      // Verify events are present in all formats
      assert_eq(json_span.events.length(), 3)
      assert_eq(protobuf_span.events.length(), 3)
      assert_eq(xml_span.events.length(), 3)
      
      // Verify specific events
      let json_string_event = json_span.events.find(fn(e) { e.name == "string_event" })
      let protobuf_string_event = protobuf_span.events.find(fn(e) { e.name == "string_event" })
      let xml_string_event = xml_span.events.find(fn(e) { e.name == "string_event" })
      
      assert_true(json_string_event.is_some())
      assert_true(protobuf_string_event.is_some())
      assert_true(xml_string_event.is_some())
      
      match (json_string_event, protobuf_string_event, xml_string_event) {
        (Some(json_e), Some(protobuf_e), Some(xml_e)) => {
          assert_true(json_e.attributes.contains(("string_key", "string_value")))
          assert_true(protobuf_e.attributes.contains(("string_key", "string_value")))
          assert_true(xml_e.attributes.contains(("string_key", "string_value")))
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}