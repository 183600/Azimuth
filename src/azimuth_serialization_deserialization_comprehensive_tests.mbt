// Azimuth Telemetry System - Serialization and Deserialization Comprehensive Tests
// This file contains comprehensive test cases for data serialization and deserialization

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  let json_serializer = JsonSerializer::new()
  
  // Create test telemetry data
  let telemetry_data = TelemetryData::new(
    "service_a",
    "operation_1",
    200,
    150,
    true
  )
  
  // Add attributes
  telemetry_data.attributes.set("user_id", "12345")
  telemetry_data.attributes.set("request_id", "req-67890")
  telemetry_data.attributes.set("region", "us-west-1")
  
  // Serialize to JSON
  let json_string = json_serializer.serialize(telemetry_data)
  assert_true(json_string.contains("service_a"))
  assert_true(json_string.contains("operation_1"))
  assert_true(json_string.contains("200"))
  assert_true(json_string.contains("12345"))
  
  // Deserialize from JSON
  let deserialized_data = json_serializer.deserialize(json_string)
  match deserialized_data {
    Some(data) => {
      assert_eq(data.service_name, telemetry_data.service_name)
      assert_eq(data.operation_name, telemetry_data.operation_name)
      assert_eq(data.status_code, telemetry_data.status_code)
      assert_eq(data.duration_ms, telemetry_data.duration_ms)
      assert_eq(data.success, telemetry_data.success)
      
      // Verify attributes
      let user_id = data.attributes.get("user_id")
      match user_id {
        Some(StringValue(id)) => assert_eq(id, "12345"),
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test batch serialization
  let batch_data = []
  for i in 0..=100 {
    let data = TelemetryData::new(
      "service_" + (i % 5).to_string(),
      "operation_" + (i % 3).to_string(),
      200 + (i % 300),
      50 + (i % 200),
      i % 10 != 0
    )
    batch_data = batch_data.push(data)
  }
  
  let batch_json = json_serializer.serialize_batch(batch_data)
  let deserialized_batch = json_serializer.deserialize_batch(batch_json)
  
  assert_eq(deserialized_batch.length(), batch_data.length())
  
  // Verify batch data integrity
  for i in 0..=batch_data.length() - 1 {
    assert_eq(deserialized_batch[i].service_name, batch_data[i].service_name)
    assert_eq(deserialized_batch[i].duration_ms, batch_data[i].duration_ms)
  }
}

// Test 2: Protocol Buffers Serialization
test "protocol buffers serialization" {
  let protobuf_serializer = ProtobufSerializer::new()
  
  // Create test span data
  let span_data = SpanData::new(
    "trace_12345",
    "span_67890",
    "service_operation",
    SpanKind::Server,
    1609459200,
    1609459350,
    SpanStatus::Ok
  )
  
  // Add events
  span_data.add_event(SpanEvent::new("start_event", 1609459200, [
    ("component", "client"),
    ("version", "1.0.0")
  ]))
  
  span_data.add_event(SpanEvent::new("end_event", 1609459350, [
    ("result", "success"),
    ("items_processed", "42")
  ]))
  
  // Serialize to protobuf
  let protobuf_bytes = protobuf_serializer.serialize_span(span_data)
  assert_true(protobuf_bytes.length() > 0)
  
  // Deserialize from protobuf
  let deserialized_span = protobuf_serializer.deserialize_span(protobuf_bytes)
  match deserialized_span {
    Some(span) => {
      assert_eq(span.trace_id, span_data.trace_id)
      assert_eq(span.span_id, span_data.span_id)
      assert_eq(span.operation_name, span_data.operation_name)
      assert_eq(span.start_time, span_data.start_time)
      assert_eq(span.end_time, span_data.end_time)
      assert_eq(span.events.length(), span_data.events.length())
    }
    None => assert_true(false)
  }
  
  // Test metric serialization
  let metric_data = MetricData::new(
    "cpu_usage",
    MetricType::Gauge,
    75.5,
    "percent",
    1609459200,
    [("host", "server-1"), ("region", "us-west-1")]
  )
  
  let metric_bytes = protobuf_serializer.serialize_metric(metric_data)
  let deserialized_metric = protobuf_serializer.deserialize_metric(metric_bytes)
  
  match deserialized_metric {
    Some(metric) => {
      assert_eq(metric.name, metric_data.name)
      assert_eq(metric.metric_type, metric_data.metric_type)
      assert_eq(metric.value, metric_data.value)
      assert_eq(metric.unit, metric_data.unit)
    }
    None => assert_true(false)
  }
}

// Test 3: Binary Serialization
test "binary serialization" {
  let binary_serializer = BinarySerializer::new()
  
  // Create test log data
  let log_data = LogData::new(
    LogLevel::Info,
    "Application started successfully",
    1609459200,
    "service-logger",
    [("process_id", "12345"), ("version", "2.1.0")]
  )
  
  // Serialize to binary
  let binary_data = binary_serializer.serialize(log_data)
  assert_true(binary_data.length() > 0)
  
  // Deserialize from binary
  let deserialized_log = binary_serializer.deserialize(binary_data)
  match deserialized_log {
    Some(log) => {
      assert_eq(log.level, log_data.level)
      assert_eq(log.message, log_data.message)
      assert_eq(log.timestamp, log_data.timestamp)
      assert_eq(log.source, log_data.source)
    }
    None => assert_true(false)
  }
  
  // Test binary serialization with compression
  let compressed_binary = binary_serializer.serialize_compressed(log_data)
  let deserialized_compressed = binary_serializer.deserialize_compressed(compressed_binary)
  
  match deserialized_compressed {
    Some(log) => {
      assert_eq(log.message, log_data.message)
      assert_eq(log.level, log_data.level)
    }
    None => assert_true(false)
  }
  
  // Verify compression ratio
  let compression_ratio = compressed_binary.length() as Float / binary_data.length() as Float
  assert_true(compression_ratio < 1.0)  // Compressed data should be smaller
  
  // Test binary stream serialization
  let stream_data = []
  for i in 0..=1000 {
    let data = LogData::new(
      if i % 4 == 0 { LogLevel::Debug }
      else if i % 4 == 1 { LogLevel::Info }
      else if i % 4 == 2 { LogLevel::Warning }
      else { LogLevel::Error },
      "Log message " + i.to_string(),
      1609459200 + i,
      "stream-logger",
      [("index", i.to_string())]
    )
    stream_data = stream_data.push(data)
  }
  
  let stream_binary = binary_serializer.serialize_stream(stream_data)
  let deserialized_stream = binary_serializer.deserialize_stream(stream_binary)
  
  assert_eq(deserialized_stream.length(), stream_data.length())
}

// Test 4: CSV Serialization
test "csv serialization" {
  let csv_serializer = CsvSerializer::new()
  
  // Create test metrics data
  let metrics_data = []
  for i in 0..=50 {
    let metric = MetricData::new(
      "metric_" + (i % 5).to_string(),
      if i % 2 == 0 { MetricType::Counter } else { MetricType::Gauge },
      (i * 1.5) as Float,
      if i % 3 == 0 { "count" } else { "percent" },
      1609459200 + i * 60,
      [
        ("host", "server-" + (i % 3).to_string()),
        ("region", if i % 2 == 0 { "us-east" } else { "us-west" })
      ]
    )
    metrics_data = metrics_data.push(metric)
  }
  
  // Serialize to CSV
  let csv_string = csv_serializer.serialize_metrics(metrics_data)
  
  // Verify CSV structure
  let csv_lines = csv_string.split("\n")
  assert_eq(csv_lines.length(), metrics_data.length() + 1)  // +1 for header
  
  // Verify header
  let header = csv_lines[0]
  assert_true(header.contains("name"))
  assert_true(header.contains("type"))
  assert_true(header.contains("value"))
  assert_true(header.contains("unit"))
  assert_true(header.contains("timestamp"))
  assert_true(header.contains("host"))
  assert_true(header.contains("region"))
  
  // Deserialize from CSV
  let deserialized_metrics = csv_serializer.deserialize_metrics(csv_string)
  assert_eq(deserialized_metrics.length(), metrics_data.length())
  
  // Verify data integrity
  for i in 0..=metrics_data.length() - 1 {
    assert_eq(deserialized_metrics[i].name, metrics_data[i].name)
    assert_eq(deserialized_metrics[i].value, metrics_data[i].value)
    assert_eq(deserialized_metrics[i].unit, metrics_data[i].unit)
  }
  
  // Test CSV with custom delimiter
  let custom_csv = csv_serializer.serialize_metrics_with_delimiter(metrics_data, ";")
  let deserialized_custom = csv_serializer.deserialize_metrics_with_delimiter(custom_csv, ";")
  assert_eq(deserialized_custom.length(), metrics_data.length())
  
  // Test CSV with quoted fields
  let metrics_with_quotes = []
  let metric_with_comma = MetricData::new(
    "metric_with,comma",
    MetricType::Counter,
    100.0,
    "count",
    1609459200,
    [("description", "This metric contains, commas in its description")]
  )
  metrics_with_quotes = metrics_with_quotes.push(metric_with_comma)
  
  let quoted_csv = csv_serializer.serialize_metrics(metrics_with_quotes)
  let deserialized_quoted = csv_serializer.deserialize_metrics(quoted_csv)
  assert_eq(deserialized_quoted.length(), 1)
  assert_eq(deserialized_quoted[0].name, "metric_with,comma")
}

// Test 5: XML Serialization
test "xml serialization" {
  let xml_serializer = XmlSerializer::new()
  
  // Create test trace data
  let trace_data = TraceData::new(
    "trace_12345",
    [
      SpanData::new(
        "trace_12345",
        "span_1",
        "service_a_operation",
        SpanKind::Server,
        1609459200,
        1609459250,
        SpanStatus::Ok
      ),
      SpanData::new(
        "trace_12345",
        "span_2",
        "service_b_operation",
        SpanKind::Client,
        1609459210,
        1609459280,
        SpanStatus::Error
      )
    ]
  )
  
  // Add attributes to spans
  trace_data.spans[0].attributes.set("service.name", "service_a")
  trace_data.spans[0].attributes.set("service.version", "1.2.3")
  
  trace_data.spans[1].attributes.set("service.name", "service_b")
  trace_data.spans[1].attributes.set("service.version", "2.1.0")
  
  // Serialize to XML
  let xml_string = xml_serializer.serialize_trace(trace_data)
  
  // Verify XML structure
  assert_true(xml_string.contains("<trace>"))
  assert_true(xml_string.contains("</trace>"))
  assert_true(xml_string.contains("<spans>"))
  assert_true(xml_string.contains("</spans>"))
  assert_true(xml_string.contains("<span>"))
  assert_true(xml_string.contains("</span>"))
  assert_true(xml_string.contains("trace_12345"))
  assert_true(xml_string.contains("span_1"))
  assert_true(xml_string.contains("service_a"))
  
  // Deserialize from XML
  let deserialized_trace = xml_serializer.deserialize_trace(xml_string)
  match deserialized_trace {
    Some(trace) => {
      assert_eq(trace.trace_id, trace_data.trace_id)
      assert_eq(trace.spans.length(), trace_data.spans.length())
      
      // Verify span data
      for i in 0..=trace.spans.length() - 1 {
        assert_eq(trace.spans[i].span_id, trace_data.spans[i].span_id)
        assert_eq(trace.spans[i].operation_name, trace_data.spans[i].operation_name)
        assert_eq(trace.spans[i].start_time, trace_data.spans[i].start_time)
      }
    }
    None => assert_true(false)
  }
  
  // Test XML with namespace support
  let namespaced_xml = xml_serializer.serialize_with_namespace(trace_data, "telemetry", "http://example.com/telemetry")
  assert_true(namespaced_xml.contains("xmlns:telemetry"))
  assert_true(namespaced_xml.contains("telemetry:trace"))
  
  let deserialized_namespaced = xml_serializer.deserialize_with_namespace(namespaced_xml, "telemetry")
  match deserialized_namespaced {
    Some(trace) => assert_eq(trace.trace_id, trace_data.trace_id),
    None => assert_true(false)
  }
}

// Test 6: Avro Serialization
test "avro serialization" {
  let avro_serializer = AvroSerializer::new()
  
  // Define Avro schema for telemetry data
  let schema = AvroSchema::new({
    "type": "record",
    "name": "TelemetryData",
    "fields": [
      {"name": "service_name", "type": "string"},
      {"name": "operation_name", "type": "string"},
      {"name": "status_code", "type": "int"},
      {"name": "duration_ms", "type": "int"},
      {"name": "success", "type": "boolean"},
      {"name": "timestamp", "type": "long"},
      {
        "name": "attributes",
        "type": {
          "type": "map",
          "values": "string"
        }
      }
    ]
  })
  
  // Create test data
  let telemetry_data = TelemetryData::new(
    "avro_service",
    "avro_operation",
    200,
    125,
    true
  )
  telemetry_data.timestamp = 1609459200
  telemetry_data.attributes.set("key1", "value1")
  telemetry_data.attributes.set("key2", "value2")
  
  // Serialize to Avro
  let avro_bytes = avro_serializer.serialize(telemetry_data, schema)
  assert_true(avro_bytes.length() > 0)
  
  // Deserialize from Avro
  let deserialized_data = avro_serializer.deserialize(avro_bytes, schema)
  match deserialized_data {
    Some(data) => {
      assert_eq(data.service_name, telemetry_data.service_name)
      assert_eq(data.operation_name, telemetry_data.operation_name)
      assert_eq(data.status_code, telemetry_data.status_code)
      assert_eq(data.duration_ms, telemetry_data.duration_ms)
      assert_eq(data.success, telemetry_data.success)
      assert_eq(data.timestamp, telemetry_data.timestamp)
    }
    None => assert_true(false)
  }
  
  // Test Avro schema evolution
  let evolved_schema = AvroSchema::new({
    "type": "record",
    "name": "TelemetryData",
    "fields": [
      {"name": "service_name", "type": "string"},
      {"name": "operation_name", "type": "string"},
      {"name": "status_code", "type": "int"},
      {"name": "duration_ms", "type": "int"},
      {"name": "success", "type": "boolean"},
      {"name": "timestamp", "type": "long"},
      {"name": "version", "type": "string", "default": "1.0.0"},  // New field with default
      {
        "name": "attributes",
        "type": {
          "type": "map",
          "values": "string"
        }
      }
    ]
  })
  
  // Deserialize with evolved schema
  let evolved_data = avro_serializer.deserialize(avro_bytes, evolved_schema)
  match evolved_data {
    Some(data) => {
      assert_eq(data.service_name, telemetry_data.service_name)
      assert_eq(data.version, "1.0.0")  // Should use default value
    }
    None => assert_true(false)
  }
}

// Test 7: MessagePack Serialization
test "messagepack serialization" {
  let msgpack_serializer = MessagePackSerializer::new()
  
  // Create test resource data
  let resource_data = ResourceData::new(
    "web_server",
    "server-01",
    [
      ResourceMetric::new("cpu_usage", 75.5, "percent"),
      ResourceMetric::new("memory_usage", 1024.0, "MB"),
      ResourceMetric::new("disk_usage", 50.0, "percent"),
      ResourceMetric::new("network_in", 1024.0, "KB/s"),
      ResourceMetric::new("network_out", 512.0, "KB/s")
    ],
    1609459200
  )
  
  // Serialize to MessagePack
  let msgpack_bytes = msgpack_serializer.serialize(resource_data)
  assert_true(msgpack_bytes.length() > 0)
  
  // Deserialize from MessagePack
  let deserialized_resource = msgpack_serializer.deserialize(msgpack_bytes)
  match deserialized_resource {
    Some(resource) => {
      assert_eq(resource.resource_type, resource_data.resource_type)
      assert_eq(resource.resource_id, resource_data.resource_id)
      assert_eq(resource.metrics.length(), resource_data.metrics.length())
      assert_eq(resource.timestamp, resource_data.timestamp)
      
      // Verify metrics
      for i in 0..=resource.metrics.length() - 1 {
        assert_eq(resource.metrics[i].name, resource_data.metrics[i].name)
        assert_eq(resource.metrics[i].value, resource_data.metrics[i].value)
        assert_eq(resource.metrics[i].unit, resource_data.metrics[i].unit)
      }
    }
    None => assert_true(false)
  }
  
  // Test MessagePack with streaming
  let resource_stream = []
  for i in 0..=100 {
    let resource = ResourceData::new(
      "resource_" + (i % 5).to_string(),
      "instance-" + i.to_string(),
      [ResourceMetric::new("metric_" + (i % 3).to_string(), (i * 2.5) as Float, "unit")],
      1609459200 + i * 10
    )
    resource_stream = resource_stream.push(resource)
  }
  
  let stream_bytes = msgpack_serializer.serialize_stream(resource_stream)
  let deserialized_stream = msgpack_serializer.deserialize_stream(stream_bytes)
  
  assert_eq(deserialized_stream.length(), resource_stream.length())
}

// Test 8: Custom Serialization Format
test "custom serialization format" {
  let custom_serializer = CustomFormatSerializer::new()
  
  // Define custom format specification
  let format_spec = CustomFormatSpec::new(
    magic_number: [0x54, 0x45, 0x4C, 0x45],  // "TELE"
    version: 1,
    compression: true,
    encryption: false,
    checksum: true
  )
  
  // Create test alert data
  let alert_data = AlertData::new(
    "high_cpu_usage",
    AlertSeverity::Warning,
    "CPU usage exceeded 80% threshold",
    1609459200,
    1609459500,  // Active for 5 minutes
    [("service", "web_server"), ("host", "server-01"), ("threshold", "80")]
  )
  
  // Serialize to custom format
  let custom_bytes = custom_serializer.serialize(alert_data, format_spec)
  assert_true(custom_bytes.length() > 4)  // Should be larger than magic number
  
  // Verify magic number
  assert_eq(custom_bytes[0], 0x54)
  assert_eq(custom_bytes[1], 0x45)
  assert_eq(custom_bytes[2], 0x4C)
  assert_eq(custom_bytes[3], 0x45)
  
  // Deserialize from custom format
  let deserialized_alert = custom_serializer.deserialize(custom_bytes)
  match deserialized_alert {
    Some(alert) => {
      assert_eq(alert.alert_id, alert_data.alert_id)
      assert_eq(alert.severity, alert_data.severity)
      assert_eq(alert.message, alert_data.message)
      assert_eq(alert.start_time, alert_data.start_time)
      assert_eq(alert.end_time, alert_data.end_time)
    }
    None => assert_true(false)
  }
  
  // Test format version compatibility
  let v2_spec = CustomFormatSpec::new(
    magic_number: [0x54, 0x45, 0x4C, 0x45],
    version: 2,
    compression: true,
    encryption: false,
    checksum: true
  )
  
  let v2_bytes = custom_serializer.serialize(alert_data, v2_spec)
  let v2_deserialized = custom_serializer.deserialize(v2_bytes)
  match v2_deserialized {
    Some(alert) => assert_eq(alert.alert_id, alert_data.alert_id),
    None => assert_true(false)
  }
  
  // Test corrupted data detection
  let mut corrupted_bytes = custom_bytes
  corrupted_bytes[10] = corrupted_bytes[10] ^ 0xFF  // Flip bits
  
  let corrupted_result = custom_serializer.deserialize(corrupted_bytes)
  match corrupted_result {
    Some(_) => assert_true(false),  // Should not succeed with corrupted data
    None => assert_true(true)
  }
}

// Test 9: Serialization Performance Comparison
test "serialization performance comparison" {
  let performance_tester = SerializationPerformanceTester::new()
  
  // Create large test dataset
  let test_data = []
  for i in 0..=10000 {
    let data = TelemetryData::new(
      "perf_service_" + (i % 10).to_string(),
      "perf_operation_" + (i % 5).to_string(),
      200 + (i % 300),
      50 + (i % 500),
      i % 10 != 0
    )
    data.timestamp = 1609459200 + i
    data.attributes.set("index", i.to_string())
    data.attributes.set("batch", (i % 100).to_string())
    test_data = test_data.push(data)
  }
  
  // Test JSON performance
  let json_result = performance_tester.test_format("json", fn() {
    let serializer = JsonSerializer::new()
    let serialized = serializer.serialize_batch(test_data)
    let _ = serializer.deserialize_batch(serialized)
  })
  
  // Test Protocol Buffers performance
  let protobuf_result = performance_tester.test_format("protobuf", fn() {
    let serializer = ProtobufSerializer::new()
    let serialized = serializer.serialize_batch(test_data)
    let _ = serializer.deserialize_batch(serialized)
  })
  
  // Test Binary performance
  let binary_result = performance_tester.test_format("binary", fn() {
    let serializer = BinarySerializer::new()
    let serialized = serializer.serialize_batch(test_data)
    let _ = serializer.deserialize_batch(serialized)
  })
  
  // Test MessagePack performance
  let msgpack_result = performance_tester.test_format("messagepack", fn() {
    let serializer = MessagePackSerializer::new()
    let serialized = serializer.serialize_batch(test_data)
    let _ = serializer.deserialize_batch(serialized)
  })
  
  // Verify all formats completed successfully
  assert_true(json_result.serialization_time_ms > 0)
  assert_true(json_result.deserialization_time_ms > 0)
  assert_true(protobuf_result.serialization_time_ms > 0)
  assert_true(protobuf_result.deserialization_time_ms > 0)
  assert_true(binary_result.serialization_time_ms > 0)
  assert_true(binary_result.deserialization_time_ms > 0)
  assert_true(msgpack_result.serialization_time_ms > 0)
  assert_true(msgpack_result.deserialization_time_ms > 0)
  
  // Compare serialization sizes
  assert_true(protobuf_result.serialized_size < json_result.serialized_size)
  assert_true(binary_result.serialized_size < json_result.serialized_size)
  assert_true(msgpack_result.serialized_size < json_result.serialized_size)
  
  // Compare performance
  let results = [json_result, protobuf_result, binary_result, msgpack_result]
  let sorted_by_speed = results.sort_by(fn(a, b) { 
    (a.serialization_time_ms + a.deserialization_time_ms) - 
    (b.serialization_time_ms + b.deserialization_time_ms) 
  })
  
  // Binary should be fastest for this test case
  assert_eq(sorted_by_speed[0].format, "binary")
  
  println!("Performance comparison results:")
  for result in results {
    println!(result.format + ": " + 
            "Serialize=" + result.serialization_time_ms.to_string() + "ms, " +
            "Deserialize=" + result.deserialization_time_ms.to_string() + "ms, " +
            "Size=" + result.serialized_size.to_string() + " bytes")
  }
}

// Test 10: Cross-Format Serialization and Migration
test "cross-format serialization and migration" {
  let format_migrator = FormatMigrator::new()
  
  // Create test configuration data
  let config_data = ConfigurationData::new(
    "telemetry_config",
    1,
    [
      ConfigItem::new("sampling.rate", 0.1, ConfigType::Float),
      ConfigItem::new("max.spans", 1000, ConfigType::Int),
      ConfigItem::new("export.interval", 5000, ConfigType::Int),
      ConfigItem::new("debug.enabled", false, ConfigType::Bool),
      ConfigItem::new("service.name", "telemetry_service", ConfigType::String)
    ]
  )
  
  // Serialize to JSON
  let json_serializer = JsonSerializer::new()
  let json_data = json_serializer.serialize_config(config_data)
  
  // Migrate from JSON to Protocol Buffers
  let protobuf_data = format_migrator.migrate(json_data, "json", "protobuf")
  assert_true(protobuf_data.length() > 0)
  
  // Verify migrated data
  let protobuf_serializer = ProtobufSerializer::new()
  let migrated_config = protobuf_serializer.deserialize_config(protobuf_data)
  match migrated_config {
    Some(config) => {
      assert_eq(config.name, config_data.name)
      assert_eq(config.version, config_data.version)
      assert_eq(config.items.length(), config_data.items.length())
      
      // Verify config items
      for i in 0..=config.items.length() - 1 {
        assert_eq(config.items[i].key, config_data.items[i].key)
        assert_eq(config.items[i].value, config_data.items[i].value)
        assert_eq(config.items[i].config_type, config_data.items[i].config_type)
      }
    }
    None => assert_true(false)
  }
  
  // Migrate from Protocol Buffers to XML
  let xml_data = format_migrator.migrate(protobuf_data, "protobuf", "xml")
  assert_true(xml_data.contains("<config>"))
  assert_true(xml_data.contains("telemetry_config"))
  
  // Verify XML migration
  let xml_serializer = XmlSerializer::new()
  let xml_config = xml_serializer.deserialize_config(xml_data)
  match xml_config {
    Some(config) => assert_eq(config.name, config_data.name),
    None => assert_true(false)
  }
  
  // Test batch migration
  let batch_configs = []
  for i in 0..=10 {
    let config = ConfigurationData::new(
      "config_" + i.to_string(),
      i,
      [ConfigItem::new("key", "value_" + i.to_string(), ConfigType::String)]
    )
    batch_configs = batch_configs.push(config)
  }
  
  let batch_json = json_serializer.serialize_config_batch(batch_configs)
  let batch_protobuf = format_migrator.migrate_batch(batch_json, "json", "protobuf")
  let migrated_batch = protobuf_serializer.deserialize_config_batch(batch_protobuf)
  
  assert_eq(migrated_batch.length(), batch_configs.length())
  
  // Test migration with transformation
  let transformer = ConfigTransformer::new()
  transformer.add_rule("sampling.rate", fn(value) {
    match value {
      FloatValue(f) => FloatValue(f * 100.0),  // Convert to percentage
      _ => value
    }
  })
  
  let transformed_protobuf = format_migrator.migrate_with_transform(
    json_data, "json", "protobuf", transformer)
  
  let transformed_config = protobuf_serializer.deserialize_config(transformed_protobuf)
  match transformed_config {
    Some(config) => {
      let sampling_rate = config.items.find(fn(item) { item.key == "sampling.rate" })
      match sampling_rate {
        Some(item) => {
          match item.value {
            FloatValue(f) => assert_float_eq(f, 10.0, 0.01),  // 0.1 * 100 = 10.0
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}