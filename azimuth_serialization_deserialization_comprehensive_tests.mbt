// Azimuth Serialization and Deserialization Comprehensive Tests
// This file contains comprehensive test cases for serialization and deserialization

// Test 1: Span Serialization and Deserialization
test "span serialization and deserialization" {
  let tracer = TracerProvider::get_tracer("serialization_test")
  let json_serializer = JsonSerializer::new()
  let binary_serializer = BinarySerializer::new()
  
  // Create a complex span with various attributes and events
  let span = Tracer::start_span(tracer, "serialization_test_span")
  Span::set_attribute(span, "service.name", StringValue("test-service"))
  Span::set_attribute(span, "operation.name", StringValue("test-operation"))
  Span::set_attribute(span, "user.id", IntValue(12345))
  Span::set_attribute(span, "request.size", FloatValue(1024.5))
  Span::set_attribute(span, "authenticated", BoolValue(true))
  Span::set_attribute(span, "tags", ArrayStringValue(["tag1", "tag2", "tag3"]))
  Span::set_attribute(span, "retry.counts", ArrayIntValue([1, 2, 3]))
  
  // Add events
  Span::add_event(span, "request.received", Some([
    ("timestamp", IntValue(1609459200)),
    ("size", IntValue(1024))
  ]))
  
  Span::add_event(span, "request.processed", Some([
    ("timestamp", IntValue(1609459210)),
    ("duration", IntValue(10))
  ]))
  
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Test JSON serialization
  let json_serialized = JsonSerializer::serialize_span(json_serializer, span)
  assert_true(json_serialized.length() > 0, "JSON serialization should produce non-empty result")
  
  // Test JSON deserialization
  let json_deserialized = JsonSerializer::deserialize_span(json_serializer, json_serialized)
  match json_deserialized {
    Some(deserialized_span) => {
      assert_eq(Span::name(deserialized_span), Span::name(span))
      assert_eq(Span::status(deserialized_span), Span::status(span))
      
      // Verify attributes
      let service_name = Span::get_attribute(deserialized_span, "service.name")
      match service_name {
        Some(StringValue(name)) => assert_eq(name, "test-service")
        _ => assert_true(false)
      }
      
      let user_id = Span::get_attribute(deserialized_span, "user.id")
      match user_id {
        Some(IntValue(id)) => assert_eq(id, 12345)
        _ => assert_true(false)
      }
    }
    None => assert_true(false, "JSON deserialization should succeed")
  }
  
  // Test binary serialization
  let binary_serialized = BinarySerializer::serialize_span(binary_serializer, span)
  assert_true(binary_serialized.length() > 0, "Binary serialization should produce non-empty result")
  
  // Test binary deserialization
  let binary_deserialized = BinarySerializer::deserialize_span(binary_serializer, binary_serialized)
  match binary_deserialized {
    Some(deserialized_span) => {
      assert_eq(Span::name(deserialized_span), Span::name(span))
      assert_eq(Span::status(deserialized_span), Span::status(span))
      
      // Verify attributes
      let authenticated = Span::get_attribute(deserialized_span, "authenticated")
      match authenticated {
        Some(BoolValue(auth)) => assert_true(auth)
        _ => assert_true(false)
      }
    }
    None => assert_true(false, "Binary deserialization should succeed")
  }
  
  Span::end(span)
}

// Test 2: Metrics Serialization and Deserialization
test "metrics serialization and deserialization" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "serialization_metrics")
  let json_serializer = JsonSerializer::new()
  
  // Create metrics with various data points
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  let gauge = Meter::create_gauge(meter, "test_gauge", Some("Test gauge"), Some("value"))
  
  // Add data points
  for i in 0..=10 {
    Counter::add(counter, 1.0, Some(Attributes::with_data([
      ("operation", StringValue("test_op")),
      ("status", StringValue(if i % 3 == 0 { "error" } else { "success" }))
    ])))
    
    Histogram::record(histogram, (i as Float) * 10.0, Some(Attributes::with_data([
      ("percentile", StringValue("p" + (i * 10).to_string()))
    ])))
    
    if i % 2 == 0 {
      Gauge::record(gauge, (i as Float) * 5.0, Some(Attributes::with_data([
        ("instance", StringValue("instance_" + (i / 2).to_string()))
      ])))
    }
  }
  
  // Test metrics serialization
  let metrics_data = MetricsData::from_meter(meter)
  let json_serialized = JsonSerializer::serialize_metrics(json_serializer, metrics_data)
  assert_true(json_serialized.length() > 0, "Metrics JSON serialization should produce non-empty result")
  
  // Test metrics deserialization
  let json_deserialized = JsonSerializer::deserialize_metrics(json_serializer, json_serialized)
  match json_deserialized {
    Some(deserialized_metrics) => {
      assert_eq(deserialized_metrics.metrics.length(), 3, "Should have 3 metrics")
      
      // Verify counter data
      let counter_metric = deserialized_metrics.metrics.find(|m| m.name == "test_counter")
      match counter_metric {
        Some(metric) => {
          assert_eq(metric.data_type, Counter)
          assert_true(metric.data_points.length() > 0)
        }
        None => assert_true(false)
      }
      
      // Verify histogram data
      let histogram_metric = deserialized_metrics.metrics.find(|m| m.name == "test_histogram")
      match histogram_metric {
        Some(metric) => {
          assert_eq(metric.data_type, Histogram)
          assert_true(metric.data_points.length() > 0)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false, "Metrics JSON deserialization should succeed")
  }
}

// Test 3: Log Record Serialization and Deserialization
test "log record serialization and deserialization" {
  let json_serializer = JsonSerializer::new()
  
  // Create complex log record
  let log_record = LogRecord::new_with_context(
    Warn,
    Some("Complex log message with special characters: !@#$%^&*()"),
    Some(Attributes::with_data([
      ("service.name", StringValue("log-service")),
      ("operation.id", StringValue("op-12345")),
      ("user.id", IntValue(67890)),
      ("duration.ms", FloatValue(123.45)),
      ("success", BoolValue(true)),
      ("tags", ArrayStringValue(["log", "warning", "test"])),
      ("metadata", StringValue("{\"key\":\"value\",\"nested\":{\"data\":true}}"))
    ])),
    Some(1609459200L),
    Some(1609459200L + 100L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(Context::with_value(
      Context::root(),
      ContextKey::new("correlation_id"),
      "corr-abcde"
    ))
  )
  
  // Test log record JSON serialization
  let json_serialized = JsonSerializer::serialize_log_record(json_serializer, log_record)
  assert_true(json_serialized.length() > 0, "Log record JSON serialization should produce non-empty result")
  
  // Test log record JSON deserialization
  let json_deserialized = JsonSerializer::deserialize_log_record(json_serializer, json_serialized)
  match json_deserialized {
    Some(deserialized_record) => {
      assert_eq(LogRecord::severity_number(deserialized_record), Warn)
      
      match LogRecord::body(deserialized_record) {
        Some(body) => assert_eq(body, "Complex log message with special characters: !@#$%^&*()")
        None => assert_true(false)
      }
      
      // Verify attributes
      let service_name = LogRecord::get_attribute(deserialized_record, "service.name")
      match service_name {
        Some(StringValue(name)) => assert_eq(name, "log-service")
        _ => assert_true(false)
      }
      
      let success = LogRecord::get_attribute(deserialized_record, "success")
      match success {
        Some(BoolValue(s)) => assert_true(s)
        _ => assert_true(false)
      }
    }
    None => assert_true(false, "Log record JSON deserialization should succeed")
  }
}

// Test 4: Resource Serialization and Deserialization
test "resource serialization and deserialization" {
  let json_serializer = JsonSerializer::new()
  
  // Create resource with various attributes
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("telemetry-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("host.name", StringValue("production-server-01")),
    ("host.ip", StringValue("192.168.1.100")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("5.4.0-74-generic")),
    ("process.id", IntValue(1234)),
    ("process.executable.name", StringValue("telemetry-service")),
    ("process.command_line", StringValue("./telemetry-service --config prod.yaml")),
    ("deployment.environment", StringValue("production")),
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2")),
    ("cloud.availability_zone", StringValue("us-west-2a")),
    ("custom.tags", ArrayStringValue(["production", "critical", "monitored"]))
  ])
  
  // Test resource JSON serialization
  let json_serialized = JsonSerializer::serialize_resource(json_serializer, resource)
  assert_true(json_serialized.length() > 0, "Resource JSON serialization should produce non-empty result")
  
  // Test resource JSON deserialization
  let json_deserialized = JsonSerializer::deserialize_resource(json_serializer, json_serialized)
  match json_deserialized {
    Some(deserialized_resource) => {
      // Verify key attributes
      let service_name = Resource::get_attribute(deserialized_resource, "service.name")
      match service_name {
        Some(StringValue(name)) => assert_eq(name, "telemetry-service")
        _ => assert_true(false)
      }
      
      let process_id = Resource::get_attribute(deserialized_resource, "process.id")
      match process_id {
        Some(IntValue(pid)) => assert_eq(pid, 1234)
        _ => assert_true(false)
      }
      
      let custom_tags = Resource::get_attribute(deserialized_resource, "custom.tags")
      match custom_tags {
        Some(ArrayStringValue(tags)) => {
          assert_eq(tags.length(), 3)
          assert_true(tags.contains("production"))
          assert_true(tags.contains("critical"))
          assert_true(tags.contains("monitored"))
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false, "Resource JSON deserialization should succeed")
  }
}

// Test 5: Context and Baggage Serialization
test "context and baggage serialization" {
  let json_serializer = JsonSerializer::new()
  
  // Create context with baggage
  let baggage = Baggage::new()
  let baggage_with_data = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_more_data = Baggage::set_entry(baggage_with_data, "session.id", "session-67890")
  let baggage_with_even_more_data = Baggage::set_entry(baggage_with_more_data, "request.id", "req-abcde")
  
  let context = Context::with_value(
    Context::with_value(
      Context::root(),
      ContextKey::new("correlation_id"),
      "corr-12345"
    ),
    ContextKey::new("baggage"),
    baggage_with_even_more_data
  )
  
  // Test context JSON serialization
  let json_serialized = JsonSerializer::serialize_context(json_serializer, context)
  assert_true(json_serialized.length() > 0, "Context JSON serialization should produce non-empty result")
  
  // Test context JSON deserialization
  let json_deserialized = JsonSerializer::deserialize_context(json_serializer, json_serialized)
  match json_deserialized {
    Some(deserialized_context) => {
      // Verify correlation ID
      let correlation_id = Context::get(deserialized_context, ContextKey::new("correlation_id"))
      match correlation_id {
        Some(id) => assert_eq(id, "corr-12345")
        None => assert_true(false)
      }
      
      // Verify baggage
      let baggage = Context::get(deserialized_context, ContextKey::new("baggage"))
      match baggage {
        Some(b) => {
          let user_id = Baggage::get_entry(b, "user.id")
          match user_id {
            Some(id) => assert_eq(id, "user-12345")
            None => assert_true(false)
          }
          
          let session_id = Baggage::get_entry(b, "session.id")
          match session_id {
            Some(id) => assert_eq(id, "session-67890")
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false, "Context JSON deserialization should succeed")
  }
}

// Test 6: Batch Serialization and Deserialization
test "batch serialization and deserialization" {
  let json_serializer = JsonSerializer::new()
  let tracer = TracerProvider::get_tracer("batch_test")
  
  // Create batch of spans
  let mut spans = []
  for i in 0..=5 {
    let span = Tracer::start_span(tracer, "batch_span_" + i.to_string())
    Span::set_attribute(span, "batch.index", IntValue(i))
    Span::set_attribute(span, "batch.size", IntValue(6))
    
    if i % 2 == 0 {
      Span::add_event(span, "batch_event_" + i.to_string(), Some([
        ("event_type", StringValue("batch_processing")),
        ("event_index", IntValue(i))
      ]))
    }
    
    spans.push(span)
  }
  
  // Create batch data
  let batch_data = BatchData {
    spans: spans,
    resource: Resource::with_attributes(Resource::new(), [
      ("batch.id", StringValue("batch-12345")),
      ("batch.timestamp", IntValue(1609459200))
    ]),
    instrumentation_scope: InstrumentationScope {
      name: "batch_test_scope",
      version: Some("1.0.0"),
      attributes: []
    }
  }
  
  // Test batch JSON serialization
  let json_serialized = JsonSerializer::serialize_batch(json_serializer, batch_data)
  assert_true(json_serialized.length() > 0, "Batch JSON serialization should produce non-empty result")
  
  // Test batch JSON deserialization
  let json_deserialized = JsonSerializer::deserialize_batch(json_serializer, json_serialized)
  match json_deserialized {
    Some(deserialized_batch) => {
      assert_eq(deserialized_batch.spans.length(), 6, "Should have 6 spans in batch")
      
      // Verify batch resource
      let batch_id = Resource::get_attribute(deserialized_batch.resource, "batch.id")
      match batch_id {
        Some(StringValue(id)) => assert_eq(id, "batch-12345")
        _ => assert_true(false)
      }
      
      // Verify instrumentation scope
      assert_eq(deserialized_batch.instrumentation_scope.name, "batch_test_scope")
      assert_eq(deserialized_batch.instrumentation_scope.version, Some("1.0.0"))
      
      // Verify span attributes
      for i in 0..=deserialized_batch.spans.length() - 1 {
        let span = deserialized_batch.spans[i]
        let batch_index = Span::get_attribute(span, "batch.index")
        match batch_index {
          Some(IntValue(index)) => assert_eq(index, i)
          _ => assert_true(false)
        }
      }
    }
    None => assert_true(false, "Batch JSON deserialization should succeed")
  }
  
  // End all spans
  for span in spans {
    Span::end(span)
  }
}

// Test 7: Cross-Format Serialization
test "cross-format serialization" {
  let json_serializer = JsonSerializer::new()
  let binary_serializer = BinarySerializer::new()
  let xml_serializer = XmlSerializer::new()
  let tracer = TracerProvider::get_tracer("cross_format_test")
  
  // Create test span
  let span = Tracer::start_span(tracer, "cross_format_span")
  Span::set_attribute(span, "test.value", StringValue("cross-format-test"))
  Span::set_attribute(span, "test.number", IntValue(42))
  
  // Serialize to different formats
  let json_data = JsonSerializer::serialize_span(json_serializer, span)
  let binary_data = BinarySerializer::serialize_span(binary_serializer, span)
  let xml_data = XmlSerializer::serialize_span(xml_serializer, span)
  
  // Verify all formats produce data
  assert_true(json_data.length() > 0, "JSON serialization should produce data")
  assert_true(binary_data.length() > 0, "Binary serialization should produce data")
  assert_true(xml_data.length() > 0, "XML serialization should produce data")
  
  // Deserialize from different formats
  let json_deserialized = JsonSerializer::deserialize_span(json_serializer, json_data)
  let binary_deserialized = BinarySerializer::deserialize_span(binary_serializer, binary_data)
  let xml_deserialized = XmlSerializer::deserialize_span(xml_serializer, xml_data)
  
  // Verify all deserializations succeed
  match (json_deserialized, binary_deserialized, xml_deserialized) {
    (Some(json_span), Some(binary_span), Some(xml_span)) => {
      // Verify all deserialized spans have the same name
      assert_eq(Span::name(json_span), Span::name(binary_span))
      assert_eq(Span::name(binary_span), Span::name(xml_span))
      assert_eq(Span::name(xml_span), "cross_format_span")
      
      // Verify attributes are preserved across formats
      let json_value = Span::get_attribute(json_span, "test.value")
      let binary_value = Span::get_attribute(binary_span, "test.value")
      let xml_value = Span::get_attribute(xml_span, "test.value")
      
      match (json_value, binary_value, xml_value) {
        (Some(StringValue(json_v)), Some(StringValue(binary_v)), Some(StringValue(xml_v))) => {
          assert_eq(json_v, binary_v)
          assert_eq(binary_v, xml_v)
          assert_eq(xml_v, "cross-format-test")
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false, "All format deserializations should succeed")
  }
  
  Span::end(span)
}

// Test 8: Serialization Error Handling
test "serialization error handling" {
  let json_serializer = JsonSerializer::new()
  let error_handler = SerializationErrorHandler::new()
  
  // Test serialization with invalid data
  let invalid_span_data = SpanData {
    name: "", // Empty name might cause error
    attributes: [],
    events: [],
    status: Unset,
    start_time: -1L, // Invalid timestamp
    end_time: None
  }
  
  let error_result = SerializationErrorHandler::with_error_handling(error_handler, || {
    JsonSerializer::serialize_span_data(json_serializer, invalid_span_data)
  })
  
  match error_result {
    Ok(data) => assert_true(data.length() > 0, "Should handle invalid data gracefully")
    Error(error) => assert_true(error.contains("serialization"), "Should provide meaningful error message")
  }
  
  // Test deserialization with invalid JSON
  let invalid_json = "{ invalid json syntax }"
  
  let deserialization_error_result = SerializationErrorHandler::with_error_handling(error_handler, || {
    JsonSerializer::deserialize_span(json_serializer, invalid_json)
  })
  
  match deserialization_error_result {
    Ok(_) => assert_true(false, "Should not succeed with invalid JSON")
    Error(error) => assert_true(error.contains("deserialization"), "Should provide meaningful error message")
  }
  
  // Test partial recovery from serialization errors
  let partial_span_data = SpanData {
    name: "valid_name",
    attributes: [
      ("valid.attribute", StringValue("valid_value")),
      ("invalid.attribute", StringValue("")) // Empty value might cause error
    ],
    events: [],
    status: Ok,
    start_time: 1609459200L,
    end_time: Some(1609459210L)
  }
  
  let recovery_result = SerializationErrorHandler::with_recovery(error_handler, || {
    JsonSerializer::serialize_span_data(json_serializer, partial_span_data)
  }, || {
    // Fallback: serialize with only valid attributes
    let valid_span_data = SpanData {
      name: partial_span_data.name,
      attributes: [
        ("valid.attribute", StringValue("valid_value"))
      ],
      events: partial_span_data.events,
      status: partial_span_data.status,
      start_time: partial_span_data.start_time,
      end_time: partial_span_data.end_time
    }
    JsonSerializer::serialize_span_data(json_serializer, valid_span_data)
  })
  
  match recovery_result {
    Ok(data) => assert_true(data.length() > 0, "Should recover from partial serialization errors")
    Error(_) => assert_true(false, "Should recover from serialization errors")
  }
}