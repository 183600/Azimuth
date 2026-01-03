// Azimuth Telemetry System - Telemetry Data Serialization and Deserialization Tests
// This file contains comprehensive test cases for telemetry data serialization and deserialization

// Test 1: Span Serialization and Deserialization
test "span serialization and deserialization" {
  let tracer = TracerProvider::get_tracer("serialization_test_tracer")
  let span = Tracer::start_span(tracer, "test_serialization_span")
  
  // Add attributes and events to the span
  Span::set_attribute(span, "http.method", StringValue("GET"))
  Span::set_attribute(span, "http.url", StringValue("https://example.com/api"))
  Span::set_attribute(span, "http.status_code", IntValue(200))
  
  Span::add_event(span, "request_start", Some([
    ("timestamp", IntValue(1234567890L)),
    ("component", StringValue("http_client"))
  ]))
  
  Span::add_event(span, "request_end", Some([
    ("timestamp", IntValue(1234567895L)),
    ("duration_ms", IntValue(5000))
  ]))
  
  // Set span status
  Span::set_status(span, Ok, Some("Request completed successfully"))
  
  // Serialize the span
  let serializer = SpanSerializer::new()
  let serialized_data = SpanSerializer::serialize(serializer, span)
  
  // Verify serialization produced data
  assert_true(serialized_data.length() > 0)
  
  // Deserialize the span
  let deserializer = SpanDeserializer::new()
  let deserialized_span = SpanDeserializer::deserialize(deserializer, serialized_data)
  
  // Verify deserialized span matches original
  assert_eq(Span::name(deserialized_span), Span::name(span))
  assert_eq(Span::kind(deserialized_span), Span::kind(span))
  
  // Verify attributes
  let original_attrs = Span::attributes(span)
  let deserialized_attrs = Span::attributes(deserialized_span)
  
  let http_method = Attributes::get(original_attrs, "http.method")
  let deserialized_http_method = Attributes::get(deserialized_attrs, "http.method")
  
  match (http_method, deserialized_http_method) {
    (Some(StringValue(method)), Some(StringValue(deserialized_method))) => {
      assert_eq(method, deserialized_method)
    }
    _ => assert_true(false)
  }
  
  // Verify events
  let original_events = Span::events(span)
  let deserialized_events = Span::events(deserialized_span)
  assert_eq(original_events.length(), deserialized_events.length())
  
  // Verify status
  assert_eq(Span::status(deserialized_span), Span::status(span))
}

// Test 2: Metric Serialization and Deserialization
test "metric serialization and deserialization" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "serialization_test_meter")
  
  // Create a counter metric
  let counter = Meter::create_counter(meter, "http_requests_total", Some("Total HTTP requests"), Some("count"))
  Counter::add(counter, 100.0, Some(Attributes::with_data([
    ("method", StringValue("GET")),
    ("status", StringValue("200")),
    ("endpoint", StringValue("/api/users"))
  ])))
  
  // Serialize the counter
  let metric_serializer = MetricSerializer::new()
  let serialized_counter = MetricSerializer::serialize(metric_serializer, Counter::as_metric(counter))
  
  // Verify serialization produced data
  assert_true(serialized_counter.length() > 0)
  
  // Deserialize the counter
  let metric_deserializer = MetricDeserializer::new()
  let deserialized_counter = MetricDeserializer::deserialize(metric_deserializer, serialized_counter)
  
  // Verify deserialized metric matches original
  assert_eq(Metric::name(deserialized_counter), Metric::name(Counter::as_metric(counter)))
  assert_eq(Metric::description(deserialized_counter), Metric::description(Counter::as_metric(counter)))
  assert_eq(Metric::unit(deserialized_counter), Metric::unit(Counter::as_metric(counter)))
  
  // Create a histogram metric
  let histogram = Meter::create_histogram(meter, "request_duration_seconds", Some("Request duration"), Some("seconds"))
  Histogram::record(histogram, 0.1)
  Histogram::record(histogram, 0.2)
  Histogram::record(histogram, 0.15)
  
  // Serialize the histogram
  let serialized_histogram = MetricSerializer::serialize(metric_serializer, Histogram::as_metric(histogram))
  
  // Deserialize the histogram
  let deserialized_histogram = MetricDeserializer::deserialize(metric_deserializer, serialized_histogram)
  
  // Verify deserialized histogram matches original
  assert_eq(Metric::name(deserialized_histogram), Metric::name(Histogram::as_metric(histogram)))
  assert_eq(Metric::description(deserialized_histogram), Metric::description(Histogram::as_metric(histogram)))
}

// Test 3: Log Record Serialization and Deserialization
test "log record serialization and deserialization" {
  // Create a log record with attributes
  let log_record = LogRecord::new_with_context(
    Warn,
    Some("Database connection failed"),
    Some(Attributes::with_data([
      ("database.host", StringValue("db.example.com")),
      ("database.port", IntValue(5432)),
      ("error.code", StringValue("CONN_TIMEOUT")),
      ("retry_count", IntValue(3))
    ])),
    Some(1234567890L),
    Some(1234567895L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(Context::with_value(Context::root(), ContextKey::new("user_id"), "12345"))
  )
  
  // Serialize the log record
  let log_serializer = LogSerializer::new()
  let serialized_log = LogSerializer::serialize(log_serializer, log_record)
  
  // Verify serialization produced data
  assert_true(serialized_log.length() > 0)
  
  // Deserialize the log record
  let log_deserializer = LogDeserializer::new()
  let deserialized_log = LogDeserializer::deserialize(log_deserializer, serialized_log)
  
  // Verify deserialized log matches original
  assert_eq(LogRecord::severity_number(deserialized_log), LogRecord::severity_number(log_record))
  
  match (LogRecord::body(deserialized_log), LogRecord::body(log_record)) {
    (Some(deserialized_body), Some(original_body)) => {
      assert_eq(deserialized_body, original_body)
    }
    _ => assert_true(false)
  }
  
  // Verify attributes
  let original_attrs = LogRecord::attributes(log_record)
  let deserialized_attrs = LogRecord::attributes(deserialized_log)
  
  let db_host = Attributes::get(original_attrs, "database.host")
  let deserialized_db_host = Attributes::get(deserialized_attrs, "database.host")
  
  match (db_host, deserialized_db_host) {
    (Some(StringValue(host)), Some(StringValue(deserialized_host))) => {
      assert_eq(host, deserialized_host)
    }
    _ => assert_true(false)
  }
  
  // Verify trace and span IDs
  assert_eq(LogRecord::trace_id(deserialized_log), LogRecord::trace_id(log_record))
  assert_eq(LogRecord::span_id(deserialized_log), LogRecord::span_id(log_record))
}

// Test 4: Resource Serialization and Deserialization
test "resource serialization and deserialization" {
  // Create a resource with multiple attributes
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("payment-service-12345")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("prod-server-01")),
    ("host.id", StringValue("host-abc123")),
    ("process.pid", IntValue(1234)),
    ("process.executable.name", StringValue("payment-service")),
    ("process.command_args", ArrayStringValue(["--port", "8080", "--config", "/etc/payment/config.yaml"]))
  ])
  
  // Serialize the resource
  let resource_serializer = ResourceSerializer::new()
  let serialized_resource = ResourceSerializer::serialize(resource_serializer, resource)
  
  // Verify serialization produced data
  assert_true(serialized_resource.length() > 0)
  
  // Deserialize the resource
  let resource_deserializer = ResourceDeserializer::new()
  let deserialized_resource = ResourceDeserializer::deserialize(resource_deserializer, serialized_resource)
  
  // Verify deserialized resource matches original
  let original_attrs = Resource::attributes(resource)
  let deserialized_attrs = Resource::attributes(deserialized_resource)
  
  // Verify service name
  let service_name = Resource::get_attribute(resource, "service.name")
  let deserialized_service_name = Resource::get_attribute(deserialized_resource, "service.name")
  
  match (service_name, deserialized_service_name) {
    (Some(StringValue(name)), Some(StringValue(deserialized_name))) => {
      assert_eq(name, deserialized_name)
    }
    _ => assert_true(false)
  }
  
  // Verify process PID
  let process_pid = Resource::get_attribute(resource, "process.pid")
  let deserialized_process_pid = Resource::get_attribute(deserialized_resource, "process.pid")
  
  match (process_pid, deserialized_process_pid) {
    (Some(IntValue(pid)), Some(IntValue(deserialized_pid))) => {
      assert_eq(pid, deserialized_pid)
    }
    _ => assert_true(false)
  }
  
  // Verify array attribute
  let command_args = Resource::get_attribute(resource, "process.command_args")
  let deserialized_command_args = Resource::get_attribute(deserialized_resource, "process.command_args")
  
  match (command_args, deserialized_command_args) {
    (Some(ArrayStringValue(args)), Some(ArrayStringValue(deserialized_args))) => {
      assert_eq(args.length(), deserialized_args.length())
      for i in 0..=args.length() - 1 {
        assert_eq(args[i], deserialized_args[i])
      }
    }
    _ => assert_true(false)
  }
}

// Test 5: Context Serialization and Deserialization
test "context serialization and deserialization" {
  // Create a context with multiple values
  let root_ctx = Context::root()
  let ctx1 = Context::with_value(root_ctx, ContextKey::new("user_id"), "user123")
  let ctx2 = Context::with_value(ctx1, ContextKey::new("request_id"), "req456")
  let ctx3 = Context::with_value(ctx2, ContextKey::new("session_id"), "session789")
  
  // Create a span context
  let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "test_state")
  let final_ctx = Context::with_value(ctx3, ContextKey::new("span_context"), span_ctx)
  
  // Serialize the context
  let context_serializer = ContextSerializer::new()
  let serialized_context = ContextSerializer::serialize(context_serializer, final_ctx)
  
  // Verify serialization produced data
  assert_true(serialized_context.length() > 0)
  
  // Deserialize the context
  let context_deserializer = ContextDeserializer::new()
  let deserialized_context = ContextDeserializer::deserialize(context_deserializer, serialized_context)
  
  // Verify deserialized context matches original
  let user_id = Context::get(final_ctx, ContextKey::new("user_id"))
  let deserialized_user_id = Context::get(deserialized_context, ContextKey::new("user_id"))
  
  match (user_id, deserialized_user_id) {
    (Some(id), Some(deserialized_id)) => assert_eq(id, deserialized_id)
    _ => assert_true(false)
  }
  
  let request_id = Context::get(final_ctx, ContextKey::new("request_id"))
  let deserialized_request_id = Context::get(deserialized_context, ContextKey::new("request_id"))
  
  match (request_id, deserialized_request_id) {
    (Some(id), Some(deserialized_id)) => assert_eq(id, deserialized_id)
    _ => assert_true(false)
  }
  
  let session_id = Context::get(final_ctx, ContextKey::new("session_id"))
  let deserialized_session_id = Context::get(deserialized_context, ContextKey::new("session_id"))
  
  match (session_id, deserialized_session_id) {
    (Some(id), Some(deserialized_id)) => assert_eq(id, deserialized_id)
    _ => assert_true(false)
  }
  
  // Verify span context
  let span_context = Context::get(final_ctx, ContextKey::new("span_context"))
  let deserialized_span_context = Context::get(deserialized_context, ContextKey::new("span_context"))
  
  match (span_context, deserialized_span_context) {
    (Some(SpanContext(trace_id, span_id, sampled, state)), 
     Some(SpanContext(deserialized_trace_id, deserialized_span_id, deserialized_sampled, deserialized_state))) => {
      assert_eq(trace_id, deserialized_trace_id)
      assert_eq(span_id, deserialized_span_id)
      assert_eq(sampled, deserialized_sampled)
      assert_eq(state, deserialized_state)
    }
    _ => assert_true(false)
  }
}

// Test 6: Batch Telemetry Data Serialization and Deserialization
test "batch telemetry data serialization and deserialization" {
  // Create a batch of telemetry data
  let telemetry_batch = []
  
  // Add spans to the batch
  for i in 0..=5 {
    let span = Tracer::start_span(TracerProvider::get_tracer("batch_test"), "batch_span_" + i.to_string())
    Span::set_attribute(span, "batch_index", IntValue(i))
    Span::add_event(span, "batch_event", Some([
      ("event_type", StringValue("batch_processing")),
      ("event_id", StringValue("event_" + i.to_string()))
    ]))
    telemetry_batch.push(TelemetryData::Span(span))
  }
  
  // Add metrics to the batch
  let meter = MeterProvider::get_meter(MeterProvider::default(), "batch_test_meter")
  for i in 0..=5 {
    let counter = Meter::create_counter(meter, "batch_counter_" + i.to_string(), None, None)
    Counter::add(counter, i.to_float(), Some(Attributes::with_data([
      ("batch_index", IntValue(i))
    ])))
    telemetry_batch.push(TelemetryData::Metric(Counter::as_metric(counter)))
  }
  
  // Add logs to the batch
  for i in 0..=5 {
    let log = LogRecord::new_with_context(
      if i % 2 == 0 { Info } else { Warn },
      Some("Batch log message " + i.to_string()),
      Some(Attributes::with_data([
        ("batch_index", IntValue(i))
      ])),
      Some(1234567890L + i.to_int()),
      None,
      None,
      None,
      None
    )
    telemetry_batch.push(TelemetryData::Log(log))
  }
  
  // Serialize the batch
  let batch_serializer = BatchSerializer::new()
  let serialized_batch = BatchSerializer::serialize(batch_serializer, telemetry_batch)
  
  // Verify serialization produced data
  assert_true(serialized_batch.length() > 0)
  
  // Deserialize the batch
  let batch_deserializer = BatchDeserializer::new()
  let deserialized_batch = BatchDeserializer::deserialize(batch_deserializer, serialized_batch)
  
  // Verify deserialized batch matches original
  assert_eq(deserialized_batch.length(), telemetry_batch.length())
  
  // Verify each telemetry data item
  for i in 0..=deserialized_batch.length() - 1 {
    match (telemetry_batch[i], deserialized_batch[i]) {
      (TelemetryData::Span(original_span), TelemetryData::Span(deserialized_span)) => {
        assert_eq(Span::name(original_span), Span::name(deserialized_span))
        
        let original_attrs = Span::attributes(original_span)
        let deserialized_attrs = Span::attributes(deserialized_span)
        
        let batch_index = Attributes::get(original_attrs, "batch_index")
        let deserialized_batch_index = Attributes::get(deserialized_attrs, "batch_index")
        
        match (batch_index, deserialized_batch_index) {
          (Some(IntValue(index)), Some(IntValue(deserialized_index))) => {
            assert_eq(index, deserialized_index)
          }
          _ => assert_true(false)
        }
      }
      (TelemetryData::Metric(original_metric), TelemetryData::Metric(deserialized_metric)) => {
        assert_eq(Metric::name(original_metric), Metric::name(deserialized_metric))
      }
      (TelemetryData::Log(original_log), TelemetryData::Log(deserialized_log)) => {
        assert_eq(LogRecord::severity_number(original_log), LogRecord::severity_number(deserialized_log))
        
        match (LogRecord::body(original_log), LogRecord::body(deserialized_log)) {
          (Some(original_body), Some(deserialized_body)) => {
            assert_eq(original_body, deserialized_body)
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
}

// Test 7: Cross-Format Serialization and Deserialization
test "cross-format serialization and deserialization" {
  // Create a span for testing
  let span = Tracer::start_span(TracerProvider::get_tracer("cross_format_test"), "cross_format_span")
  Span::set_attribute(span, "format.test", StringValue("cross_format"))
  Span::add_event(span, "format_event", Some([
    ("format", StringValue("json")),
    ("version", StringValue("1.0"))
  ]))
  
  // Serialize to JSON
  let json_serializer = JsonSerializer::new()
  let json_data = JsonSerializer::serialize_span(json_serializer, span)
  
  // Serialize to Protocol Buffers
  let protobuf_serializer = ProtobufSerializer::new()
  let protobuf_data = ProtobufSerializer::serialize_span(protobuf_serializer, span)
  
  // Serialize to XML
  let xml_serializer = XmlSerializer::new()
  let xml_data = XmlSerializer::serialize_span(xml_serializer, span)
  
  // Verify all formats produced data
  assert_true(json_data.length() > 0)
  assert_true(protobuf_data.length() > 0)
  assert_true(xml_data.length() > 0)
  
  // Deserialize from JSON
  let json_deserializer = JsonDeserializer::new()
  let span_from_json = JsonDeserializer::deserialize_span(json_deserializer, json_data)
  
  // Deserialize from Protocol Buffers
  let protobuf_deserializer = ProtobufDeserializer::new()
  let span_from_protobuf = ProtobufDeserializer::deserialize_span(protobuf_deserializer, protobuf_data)
  
  // Deserialize from XML
  let xml_deserializer = XmlDeserializer::new()
  let span_from_xml = XmlDeserializer::deserialize_span(xml_deserializer, xml_data)
  
  // Verify all deserialized spans match original
  assert_eq(Span::name(span), Span::name(span_from_json))
  assert_eq(Span::name(span), Span::name(span_from_protobuf))
  assert_eq(Span::name(span), Span::name(span_from_xml))
  
  // Verify attributes are preserved across formats
  let original_attrs = Span::attributes(span)
  let json_attrs = Span::attributes(span_from_json)
  let protobuf_attrs = Span::attributes(span_from_protobuf)
  let xml_attrs = Span::attributes(span_from_xml)
  
  let format_test = Attributes::get(original_attrs, "format.test")
  let json_format_test = Attributes::get(json_attrs, "format.test")
  let protobuf_format_test = Attributes::get(protobuf_attrs, "format.test")
  let xml_format_test = Attributes::get(xml_attrs, "format.test")
  
  match (format_test, json_format_test, protobuf_format_test, xml_format_test) {
    (Some(StringValue(original)), Some(StringValue(json)), Some(StringValue(protobuf)), Some(StringValue(xml))) => {
      assert_eq(original, json)
      assert_eq(original, protobuf)
      assert_eq(original, xml)
    }
    _ => assert_true(false)
  }
}

// Test 8: Compression and Decompression of Serialized Telemetry Data
test "compression and decompression of serialized telemetry data" {
  // Create a large telemetry dataset
  let large_dataset = []
  
  // Add many spans to create a large dataset
  for i in 0..=100 {
    let span = Tracer::start_span(TracerProvider::get_tracer("compression_test"), "large_span_" + i.to_string())
    Span::set_attribute(span, "large_data", StringValue("x".repeat(1000))) // Large attribute value
    Span::set_attribute(span, "index", IntValue(i))
    
    // Add multiple events
    for j in 0..=10 {
      Span::add_event(span, "event_" + j.to_string(), Some([
        ("event_data", StringValue("y".repeat(500))), // Large event data
        ("event_index", IntValue(j))
      ]))
    }
    
    large_dataset.push(span)
  }
  
  // Serialize the dataset
  let batch_serializer = BatchSerializer::new()
  let serialized_data = BatchSerializer::serialize_spans(batch_serializer, large_dataset)
  
  // Compress the serialized data
  let compressor = TelemetryCompressor::new()
  let compressed_data = TelemetryCompressor::compress(compressor, serialized_data)
  
  // Verify compression reduced data size
  assert_true(compressed_data.length() < serialized_data.length())
  
  // Decompress the data
  let decompressed_data = TelemetryCompressor::decompress(compressor, compressed_data)
  
  // Verify decompressed data matches original serialized data
  assert_eq(decompressed_data.length(), serialized_data.length())
  
  // Deserialize the decompressed data
  let batch_deserializer = BatchDeserializer::new()
  let deserialized_spans = BatchDeserializer::deserialize_spans(batch_deserializer, decompressed_data)
  
  // Verify deserialized spans match original
  assert_eq(deserialized_spans.length(), large_dataset.length())
  
  // Verify specific span data
  for i in 0..=deserialized_spans.length() - 1 {
    let original_span = large_dataset[i]
    let deserialized_span = deserialized_spans[i]
    
    assert_eq(Span::name(original_span), Span::name(deserialized_span))
    
    let original_attrs = Span::attributes(original_span)
    let deserialized_attrs = Span::attributes(deserialized_span)
    
    let large_data = Attributes::get(original_attrs, "large_data")
    let deserialized_large_data = Attributes::get(deserialized_attrs, "large_data")
    
    match (large_data, deserialized_large_data) {
      (Some(StringValue(original)), Some(StringValue(deserialized))) => {
        assert_eq(original, deserialized)
        assert_eq(original.length(), 1000)
      }
      _ => assert_true(false)
    }
    
    // Verify events
    let original_events = Span::events(original_span)
    let deserialized_events = Span::events(deserialized_span)
    assert_eq(original_events.length(), deserialized_events.length())
  }
}