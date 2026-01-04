// Azimuth Telemetry System - Advanced Serialization and Deserialization Tests
// This file contains comprehensive test cases for data serialization and deserialization

// Test 1: JSON Serialization for Telemetry Data
test "json serialization for telemetry data" {
  let serializer = JsonSerializer::new()
  
  // Test span serialization
  let span_context = SpanContext::new("trace123", "span456", true, "active")
  let span = Span::new("test_operation", Internal, span_context)
  
  Span::add_event(span, "operation_started", Some([
    ("timestamp", IntValue(1234567890)),
    ("component", StringValue("auth"))
  ]))
  
  Span::set_attributes(span, [
    ("user.id", StringValue("user123")),
    ("operation.type", StringValue("authentication"))
  ])
  
  let serialized_span = JsonSerializer::serialize_span(serializer, span)
  assert_true(serialized_span.contains("\"trace_id\":\"trace123\""))
  assert_true(serialized_span.contains("\"span_id\":\"span456\""))
  assert_true(serialized_span.contains("\"name\":\"test_operation\""))
  
  // Test deserialization
  let deserialized_span = JsonSerializer::deserialize_span(serializer, serialized_span)
  assert_eq(Span::name(deserialized_span), "test_operation")
  assert_eq(SpanContext::trace_id(Span::span_context(deserialized_span)), "trace123")
  
  // Test metric serialization
  let metric = Metric::new_counter("http_requests", 42.0, [
    ("method", StringValue("GET")),
    ("status", StringValue("200"))
  ])
  
  let serialized_metric = JsonSerializer::serialize_metric(serializer, metric)
  assert_true(serialized_metric.contains("\"name\":\"http_requests\""))
  assert_true(serialized_metric.contains("\"value\":42.0"))
  
  let deserialized_metric = JsonSerializer::deserialize_metric(serializer, serialized_metric)
  assert_eq(Metric::name(deserialized_metric), "http_requests")
  assert_eq(Metric::value(deserialized_metric), 42.0)
}

// Test 2: Protocol Buffers Serialization
test "protocol buffers serialization for high-performance" {
  let protobuf_serializer = ProtobufSerializer::new()
  
  // Test batch span serialization
  let spans = []
  for i in 0..=10 {
    let span_ctx = SpanContext::new("batch_trace", "span_" + i.to_string(), true, "active")
    let span = Span::new("batch_operation_" + i.to_string(), Internal, span_ctx)
    spans.push(span)
  }
  
  let serialized_batch = ProtobufSerializer::serialize_span_batch(protobuf_serializer, spans)
  assert_true(serialized_batch.length() > 0)
  
  // Test deserialization
  let deserialized_spans = ProtobufSerializer::deserialize_span_batch(protobuf_serializer, serialized_batch)
  assert_eq(deserialized_spans.length(), 11)
  
  // Verify first span
  let first_span = deserialized_spans[0]
  assert_eq(Span::name(first_span), "batch_operation_0")
  assert_eq(SpanContext::trace_id(Span::span_context(first_span)), "batch_trace")
  
  // Test metric serialization
  let metrics = [
    Metric::new_histogram("response_time", 100.5, ["endpoint", "/api/users"]),
    Metric::new_counter("error_count", 3.0, ["error_type", "timeout"]),
    Metric::new_gauge("active_connections", 25.0, ["connection_type", "websocket"])
  ]
  
  let serialized_metrics = ProtobufSerializer::serialize_metric_batch(protobuf_serializer, metrics)
  let deserialized_metrics = ProtobufSerializer::deserialize_metric_batch(protobuf_serializer, serialized_metrics)
  
  assert_eq(deserialized_metrics.length(), 3)
  assert_eq(Metric::name(deserialized_metrics[0]), "response_time")
  assert_eq(Metric::value(deserialized_metrics[0]), 100.5)
}

// Test 3: Avro Schema-Based Serialization
test "avro schema-based serialization" {
  let avro_serializer = AvroSerializer::new()
  
  // Define schema for telemetry events
  let event_schema = AvroSchema::new_record("TelemetryEvent", [
    ("event_id", AvroType::String),
    ("timestamp", AvroType::Long),
    ("event_type", AvroType::String),
    ("trace_id", AvroType::String),
    ("attributes", AvroType::Map(AvroType::String))
  ])
  
  // Test event serialization with schema
  let event = TelemetryEvent::new(
    "event123",
    1234567890L,
    "span_started",
    "trace456",
    [
      ("service.name", StringValue("auth-service")),
      ("span.name", StringValue("user_login")),
      ("user.id", StringValue("user789"))
    ]
  )
  
  let serialized_event = AvroSerializer::serialize_with_schema(avro_serializer, event, event_schema)
  assert_true(serialized_event.length() > 0)
  
  // Test deserialization with schema validation
  let deserialized_event = AvroSerializer::deserialize_with_schema(avro_serializer, serialized_event, event_schema)
  assert_eq(TelemetryEvent::event_id(deserialized_event), "event123")
  assert_eq(TelemetryEvent::event_type(deserialized_event), "span_started")
  assert_eq(TelemetryEvent::trace_id(deserialized_event), "trace456")
  
  // Test schema evolution compatibility
  let evolved_schema = AvroSchema::new_record("TelemetryEvent", [
    ("event_id", AvroType::String),
    ("timestamp", AvroType::Long),
    ("event_type", AvroType::String),
    ("trace_id", AvroType::String),
    ("attributes", AvroType::Map(AvroType::String)),
    ("severity", AvroType::String) // New field with default
  ])
  
  // Should be able to deserialize old data with new schema
  let evolved_event = AvroSerializer::deserialize_with_schema(avro_serializer, serialized_event, evolved_schema)
  assert_eq(TelemetryEvent::event_id(evolved_event), "event123")
  // New field should have default value
  assert_eq(TelemetryEvent::severity(evolved_event), "INFO")
}

// Test 4: MessagePack Serialization for Compact Storage
test "messagepack serialization for compact storage" {
  let msgpack_serializer = MessagePackSerializer::new()
  
  // Test log record serialization
  let log_record = LogRecord::new_with_attributes(
    Error,
    "Database connection failed",
    [
      ("error.code", StringValue("CONN_TIMEOUT")),
      ("database.host", StringValue("db.example.com")),
      ("retry.count", IntValue(3)),
      ("connection.timeout", IntValue(5000))
    ]
  )
  
  let serialized_log = MessagePackSerializer::serialize_log_record(msgpack_serializer, log_record)
  
  // MessagePack should be more compact than JSON
  let json_serializer = JsonSerializer::new()
  let json_log = JsonSerializer::serialize_log_record(json_serializer, log_record)
  
  assert_true(serialized_log.length() < json_log.length())
  
  // Test deserialization
  let deserialized_log = MessagePackSerializer::deserialize_log_record(msgpack_serializer, serialized_log)
  assert_eq(LogRecord::severity_number(deserialized_log), Error)
  match LogRecord::body(deserialized_log) {
    Some(body) => assert_eq(body, "Database connection failed")
    None => assert_true(false)
  }
  
  // Test batch serialization
  let log_records = [
    log_record,
    LogRecord::new(Info, "Application started"),
    LogRecord::new(Warn, "High memory usage detected")
  ]
  
  let serialized_batch = MessagePackSerializer::serialize_log_batch(msgpack_serializer, log_records)
  let deserialized_batch = MessagePackSerializer::deserialize_log_batch(msgpack_serializer, serialized_batch)
  
  assert_eq(deserialized_batch.length(), 3)
  assert_eq(LogRecord::severity_number(deserialized_batch[0]), Error)
  assert_eq(LogRecord::severity_number(deserialized_batch[1]), Info)
  assert_eq(LogRecord::severity_number(deserialized_batch[2]), Warn)
}

// Test 5: Custom Binary Serialization Format
test "custom binary serialization format" {
  let binary_serializer = BinarySerializer::new()
  
  // Test custom format for high-frequency telemetry data
  let telemetry_data = TelemetryData::new(
    "high_frequency_metrics",
    1234567890L,
    [
      MetricPoint::new("cpu_usage", 75.5),
      MetricPoint::new("memory_usage", 60.2),
      MetricPoint::new("network_io", 1024.0)
    ]
  )
  
  let serialized_data = BinarySerializer::serialize_telemetry_data(binary_serializer, telemetry_data)
  
  // Custom binary format should include magic number and version
  assert_eq(serialized_data[0], 0xAE) // Magic number byte 1
  assert_eq(serialized_data[1], 0x42) // Magic number byte 2
  assert_eq(serialized_data[2], 0x01) // Version 1
  
  // Test deserialization
  let deserialized_data = BinarySerializer::deserialize_telemetry_data(binary_serializer, serialized_data)
  assert_eq(TelemetryData::source(deserialized_data), "high_frequency_metrics")
  assert_eq(TelemetryData::timestamp(deserialized_data), 1234567890L)
  
  let metrics = TelemetryData::metrics(deserialized_data)
  assert_eq(metrics.length(), 3)
  assert_eq(MetricPoint::name(metrics[0]), "cpu_usage")
  assert_eq(MetricPoint::value(metrics[0]), 75.5)
  
  // Test compression integration
  let compressed_data = BinarySerializer::compress_serialize(binary_serializer, telemetry_data)
  let decompressed_data = BinarySerializer::decompress_deserialize(binary_serializer, compressed_data)
  
  assert_eq(TelemetryData::source(decompressed_data), "high_frequency_metrics")
  assert_true(compressed_data.length() < serialized_data.length())
}

// Test 6: Streaming Serialization for Large Datasets
test "streaming serialization for large datasets" {
  let stream_serializer = StreamSerializer::new()
  
  // Test streaming span serialization
  let span_stream = stream_serializer.create_span_stream()
  
  // Write large number of spans to stream
  for i in 0..=10000 {
    let span_ctx = SpanContext::new("stream_trace", "span_" + i.to_string(), true, "active")
    let span = Span::new("stream_operation_" + i.to_string(), Internal, span_ctx)
    StreamSerializer::write_span(stream_serializer, span_stream, span)
  }
  
  // Test streaming deserialization
  let deserialized_count = 0
  let span_reader = stream_serializer.create_span_reader(span_stream)
  
  while StreamSerializer::has_next_span(span_reader) {
    let span = StreamSerializer::read_next_span(span_reader)
    deserialized_count = deserialized_count + 1
  }
  
  assert_eq(deserialized_count, 10001)
  
  // Test memory efficiency
  let memory_usage = StreamSerializer::get_memory_usage(span_stream)
  assert_true(memory_usage < 1000000) // Should be under 1MB for 10k spans
  
  // Test random access serialization
  let random_access_stream = stream_serializer.create_random_access_stream()
  
  for i in 0..=1000 {
    let metric = Metric::new_counter("metric_" + i.to_string(), i.to_float(), [])
    StreamSerializer::write_metric_at_position(random_access_stream, i, metric)
  }
  
  // Test random access read
  let metric_500 = StreamSerializer::read_metric_at_position(random_access_stream, 500)
  assert_eq(Metric::name(metric_500), "metric_500")
  assert_eq(Metric::value(metric_500), 500.0)
  
  let metric_999 = StreamSerializer::read_metric_at_position(random_access_stream, 999)
  assert_eq(Metric::name(metric_999), "metric_999")
  assert_eq(Metric::value(metric_999), 999.0)
}

// Test 7: Schema Validation and Migration
test "schema validation and migration" {
  let schema_manager = SchemaManager::new()
  
  // Define v1 schema
  let v1_schema = Schema::new("telemetry_span_v1", [
    Field::new("trace_id", FieldType::String, true),
    Field::new("span_id", FieldType::String, true),
    Field::new("name", FieldType::String, true),
    Field::new("start_time", FieldType::Long, true),
    Field::new("end_time", FieldType::Long, false)
  ])
  
  // Define v2 schema (with additional fields)
  let v2_schema = Schema::new("telemetry_span_v2", [
    Field::new("trace_id", FieldType::String, true),
    Field::new("span_id", FieldType::String, true),
    Field::new("name", FieldType::String, true),
    Field::new("start_time", FieldType::Long, true),
    Field::new("end_time", FieldType::Long, false),
    Field::new("status", FieldType::String, false), // New field
    Field::new("duration_ms", FieldType::Long, false) // Computed field
  ])
  
  // Register schemas
  SchemaManager::register_schema(schema_manager, v1_schema)
  SchemaManager::register_schema(schema_manager, v2_schema)
  
  // Create data with v1 schema
  let v1_data = SchemaManager::create_data(schema_manager, "telemetry_span_v1", [
    ("trace_id", StringValue("trace123")),
    ("span_id", StringValue("span456")),
    ("name", StringValue("test_operation")),
    ("start_time", LongValue(1234567890L)),
    ("end_time", LongValue(1234567895L))
  ])
  
  // Validate against v1 schema
  let v1_validation = SchemaManager::validate(schema_manager, v1_data, "telemetry_span_v1")
  assert_true(v1_validation.is_valid)
  
  // Test migration from v1 to v2
  let migrated_data = SchemaManager::migrate(schema_manager, v1_data, "telemetry_span_v1", "telemetry_span_v2")
  
  // Validate migrated data against v2 schema
  let v2_validation = SchemaManager::validate(schema_manager, migrated_data, "telemetry_span_v2")
  assert_true(v2_validation.is_valid)
  
  // Verify migration results
  let duration = SchemaManager::get_field_value(migrated_data, "duration_ms")
  match duration {
    LongValue(value) => assert_eq(value, 5L) // 1234567895 - 1234567890
    _ => assert_true(false)
  }
  
  let status = SchemaManager::get_field_value(migrated_data, "status")
  match status {
    StringValue(value) => assert_eq(value, "OK") // Default status
    _ => assert_true(false)
  }
}

// Test 8: Encryption and Secure Serialization
test "encryption and secure serialization" {
  let secure_serializer = SecureSerializer::new("encryption_key_12345")
  
  // Test sensitive data serialization
  let sensitive_data = SensitiveTelemetry::new(
    "user_authentication",
    [
      ("user.id", StringValue("user123")),
      ("user.email", StringValue("user@example.com")),
      ("auth.token", StringValue("secret_token_abc123")),
      ("credit_card", StringValue("4111-1111-1111-1111"))
    ]
  )
  
  // Serialize with encryption
  let encrypted_data = SecureSerializer::encrypt_serialize(secure_serializer, sensitive_data)
  
  // Verify data is encrypted (should not contain plain text values)
  assert_false(encrypted_data.contains("user123"))
  assert_false(encrypted_data.contains("user@example.com"))
  assert_false(encrypted_data.contains("secret_token_abc123"))
  assert_false(encrypted_data.contains("4111-1111-1111-1111"))
  
  // Test decryption and deserialization
  let decrypted_data = SecureSerializer::decrypt_deserialize(secure_serializer, encrypted_data)
  
  assert_eq(SensitiveTelemetry::operation_type(decrypted_data), "user_authentication")
  
  let user_id = SensitiveTelemetry::get_attribute(decrypted_data, "user.id")
  match user_id {
    StringValue(value) => assert_eq(value, "user123")
    _ => assert_true(false)
  }
  
  let auth_token = SensitiveTelemetry::get_attribute(decrypted_data, "auth.token")
  match auth_token {
    StringValue(value) => assert_eq(value, "secret_token_abc123")
    _ => assert_true(false)
  }
  
  // Test field-level encryption
  let field_encrypted_data = SecureSerializer::encrypt_fields(secure_serializer, sensitive_data, [
    "auth.token", "credit_card"
  ])
  
  // Non-sensitive fields should be visible
  assert_true(field_encrypted_data.contains("user123"))
  assert_true(field_encrypted_data.contains("user@example.com"))
  
  // Sensitive fields should be encrypted
  assert_false(field_encrypted_data.contains("secret_token_abc123"))
  assert_false(field_encrypted_data.contains("4111-1111-1111-1111"))
  
  // Test decryption of field-level encrypted data
  let field_decrypted_data = SecureSerializer::decrypt_fields(secure_serializer, field_encrypted_data, [
    "auth.token", "credit_card"
  ])
  
  let decrypted_token = SensitiveTelemetry::get_attribute(field_decrypted_data, "auth.token")
  match decrypted_token {
    StringValue(value) => assert_eq(value, "secret_token_abc123")
    _ => assert_true(false)
  }
}

// Test 9: Cross-Format Serialization Compatibility
test "cross-format serialization compatibility" {
  let format_converter = FormatConverter::new()
  
  // Create test data
  let test_span = Span::new("compatibility_test", Internal, 
    SpanContext::new("trace123", "span456", true, "active"))
  
  Span::add_event(test_span, "test_event", Some([
    ("test_attr", StringValue("test_value"))
  ]))
  
  // Convert JSON to Protocol Buffers
  let json_serializer = JsonSerializer::new()
  let json_data = JsonSerializer::serialize_span(json_serializer, test_span)
  
  let protobuf_data = FormatConverter::convert_json_to_protobuf(format_converter, json_data)
  assert_true(protobuf_data.length() > 0)
  
  // Convert Protocol Buffers back to JSON
  let converted_json = FormatConverter::convert_protobuf_to_json(format_converter, protobuf_data)
  assert_true(converted_json.contains("\"trace_id\":\"trace123\""))
  assert_true(converted_json.contains("\"name\":\"compatibility_test\""))
  
  // Verify round-trip conversion
  let protobuf_serializer = ProtobufSerializer::new()
  let converted_span = ProtobufSerializer::deserialize_span(protobuf_serializer, protobuf_data)
  assert_eq(Span::name(converted_span), "compatibility_test")
  
  // Test Avro to JSON conversion
  let avro_serializer = AvroSerializer::new()
  let avro_schema = AvroSchema::new_record("Span", [
    ("trace_id", AvroType::String),
    ("span_id", AvroType::String),
    ("name", AvroType::String),
    ("kind", AvroType::Int)
  ])
  
  let avro_data = AvroSerializer::serialize_with_schema(avro_serializer, test_span, avro_schema)
  let avro_to_json = FormatConverter::convert_avro_to_json(format_converter, avro_data, avro_schema)
  
  assert_true(avro_to_json.contains("\"trace_id\":\"trace123\""))
  assert_true(avro_to_json.contains("\"name\":\"compatibility_test\""))
  
  // Test MessagePack to JSON conversion
  let msgpack_serializer = MessagePackSerializer::new()
  let msgpack_data = MessagePackSerializer::serialize_span(msgpack_serializer, test_span)
  let msgpack_to_json = FormatConverter::convert_msgpack_to_json(format_converter, msgpack_data)
  
  assert_true(msgpack_to_json.contains("\"trace_id\":\"trace123\""))
  assert_true(msgpack_to_json.contains("\"name\":\"compatibility_test\""))
}

// Test 10: Serialization Performance Benchmarks
test "serialization performance benchmarks" {
  let performance_benchmark = SerializationBenchmark::new()
  
  // Create test dataset
  let test_spans = []
  for i in 0..=1000 {
    let span_ctx = SpanContext::new("perf_trace", "span_" + i.to_string(), true, "active")
    let span = Span::new("perf_operation_" + i.to_string(), Internal, span_ctx)
    
    // Add events and attributes
    Span::add_event(span, "event_" + i.to_string(), Some([
      ("iteration", IntValue(i))
    ]))
    
    Span::set_attributes(span, [
      ("batch.id", StringValue("batch123")),
      ("iteration", IntValue(i))
    ])
    
    test_spans.push(span)
  }
  
  // Benchmark JSON serialization
  let json_time = SerializationBenchmark::benchmark_json_serialization(performance_benchmark, test_spans)
  assert_true(json_time > 0)
  
  // Benchmark Protocol Buffers serialization
  let protobuf_time = SerializationBenchmark::benchmark_protobuf_serialization(performance_benchmark, test_spans)
  assert_true(protobuf_time > 0)
  
  // Benchmark Avro serialization
  let avro_time = SerializationBenchmark::benchmark_avro_serialization(performance_benchmark, test_spans)
  assert_true(avro_time > 0)
  
  // Benchmark MessagePack serialization
  let msgpack_time = SerializationBenchmark::benchmark_msgpack_serialization(performance_benchmark, test_spans)
  assert_true(msgpack_time > 0)
  
  // Verify performance expectations
  // Protocol Buffers and MessagePack should be faster than JSON
  assert_true(protobuf_time < json_time)
  assert_true(msgpack_time < json_time)
  
  // Compare serialization sizes
  let json_size = SerializationBenchmark::get_json_size(performance_benchmark, test_spans)
  let protobuf_size = SerializationBenchmark::get_protobuf_size(performance_benchmark, test_spans)
  let msgpack_size = SerializationBenchmark::get_msgpack_size(performance_benchmark, test_spans)
  
  // Binary formats should be more compact
  assert_true(protobuf_size < json_size)
  assert_true(msgpack_size < json_size)
  
  // Generate performance report
  let report = SerializationBenchmark::generate_report(performance_benchmark)
  assert_true(report.contains("JSON serialization time"))
  assert_true(report.contains("Protocol Buffers serialization time"))
  assert_true(report.contains("MessagePack serialization time"))
  assert_true(report.contains("Size comparison"))
}