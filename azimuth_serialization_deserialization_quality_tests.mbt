// Azimuth Telemetry System - Serialization and Deserialization Quality Tests
// This file contains comprehensive test cases for data serialization and deserialization functionality

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Test basic JSON serialization
  let test_data = {
    "name": "test_span",
    "trace_id": "0af7651916cd43dd8448eb211c80319c",
    "span_id": "b7ad6b7169203331",
    "timestamp": 1234567890,
    "duration": 1500,
    "status": "ok"
  }
  
  // Serialize to JSON string
  let json_string = JsonSerializer::serialize(test_data)
  assert_true(json_string.length() > 0)
  assert_true(json_string.contains("test_span"))
  assert_true(json_string.contains("0af7651916cd43dd8448eb211c80319c"))
  
  // Deserialize from JSON string
  let deserialized_data = JsonSerializer::deserialize(json_string)
  match deserialized_data {
    Some(data) => {
      assert_eq(data["name"], "test_span")
      assert_eq(data["trace_id"], "0af7651916cd43dd8448eb211c80319c")
      assert_eq(data["span_id"], "b7ad6b7169203331")
      assert_eq(data["timestamp"], 1234567890)
      assert_eq(data["duration"], 1500)
      assert_eq(data["status"], "ok")
    }
    None => assert_true(false)
  }
}

// Test 2: Binary Serialization and Deserialization
test "binary serialization and deserialization" {
  // Test binary serialization of telemetry data
  let telemetry_data = TelemetryData::new(
    "service.name",
    "operation.name",
    1234567890L,
    1500L,
    Ok,
    [("http.method", StringValue("GET")), ("http.status_code", IntValue(200))]
  )
  
  // Serialize to binary format
  let binary_data = BinarySerializer::serialize(telemetry_data)
  assert_true(binary_data.length() > 0)
  
  // Deserialize from binary format
  let deserialized_data = BinarySerializer::deserialize(binary_data)
  match deserialized_data {
    Some(data) => {
      assert_eq(TelemetryData::service_name(data), "service.name")
      assert_eq(TelemetryData::operation_name(data), "operation.name")
      assert_eq(TelemetryData::timestamp(data), 1234567890L)
      assert_eq(TelemetryData::duration(data), 1500L)
      assert_eq(TelemetryData::status(data), Ok)
      
      let attributes = TelemetryData::attributes(data)
      let http_method = Attributes::get(attributes, "http.method")
      match http_method {
        Some(StringValue(method)) => assert_eq(method, "GET")
        _ => assert_true(false)
      }
      
      let status_code = Attributes::get(attributes, "http.status_code")
      match status_code {
        Some(IntValue(code)) => assert_eq(code, 200)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 3: Protocol Buffer Serialization and Deserialization
test "protocol buffer serialization and deserialization" {
  // Test protobuf serialization
  let span_data = SpanData::new(
    "trace_id_123",
    "span_id_456",
    "parent_span_id_789",
    "operation_name",
    Internal,
    1234567890L,
    1234569390L,
    [("service.name", StringValue("test_service"))],
    [Event::new("event1", 1234568000L, [("event.key", StringValue("event.value"))])],
    Ok,
    "Operation completed successfully"
  )
  
  // Serialize to protobuf
  let protobuf_data = ProtobufSerializer::serialize(span_data)
  assert_true(protobuf_data.length() > 0)
  
  // Deserialize from protobuf
  let deserialized_span = ProtobufSerializer::deserialize_span(protobuf_data)
  match deserialized_span {
    Some(span) => {
      assert_eq(SpanData::trace_id(span), "trace_id_123")
      assert_eq(SpanData::span_id(span), "span_id_456")
      assert_eq(SpanData::parent_span_id(span), "parent_span_id_789")
      assert_eq(SpanData::operation_name(span), "operation_name")
      assert_eq(SpanData::start_time(span), 1234567890L)
      assert_eq(SpanData::end_time(span), 1234569390L)
      assert_eq(SpanData::status(span), Ok)
      assert_eq(SpanData::status_message(span), "Operation completed successfully")
    }
    None => assert_true(false)
  }
}

// Test 4: MessagePack Serialization and Deserialization
test "messagepack serialization and deserialization" {
  // Test MessagePack serialization
  let metrics_data = {
    "counter_name": "request_count",
    "counter_value": 100.0,
    "histogram_name": "response_time",
    "histogram_values": [10.5, 20.3, 30.7, 40.1, 50.9],
    "gauge_name": "active_connections",
    "gauge_value": 25.0,
    "timestamp": 1234567890L
  }
  
  // Serialize to MessagePack
  let msgpack_data = MessagePackSerializer::serialize(metrics_data)
  assert_true(msgpack_data.length() > 0)
  
  // Deserialize from MessagePack
  let deserialized_metrics = MessagePackSerializer::deserialize(msgpack_data)
  match deserialized_metrics {
    Some(metrics) => {
      assert_eq(metrics["counter_name"], "request_count")
      assert_eq(metrics["counter_value"], 100.0)
      assert_eq(metrics["histogram_name"], "response_time")
      assert_eq(metrics["gauge_name"], "active_connections")
      assert_eq(metrics["gauge_value"], 25.0)
      assert_eq(metrics["timestamp"], 1234567890L)
      
      let histogram_values = metrics["histogram_values"]
      assert_eq(histogram_values.length(), 5)
      assert_eq(histogram_values[0], 10.5)
      assert_eq(histogram_values[4], 50.9)
    }
    None => assert_true(false)
  }
}

// Test 5: XML Serialization and Deserialization
test "xml serialization and deserialization" {
  // Test XML serialization
  let log_data = LogData::new(
    Info,
    "Application started successfully",
    1234567890L,
    [("logger.name", StringValue("app.logger")), ("thread.id", StringValue("main"))],
    Some("trace_id_123"),
    Some("span_id_456")
  )
  
  // Serialize to XML
  let xml_string = XmlSerializer::serialize(log_data)
  assert_true(xml_string.length() > 0)
  assert_true(xml_string.contains("<log>"))
  assert_true(xml_string.contains("Application started successfully"))
  assert_true(xml_string.contains("<severity>"))
  assert_true(xml_string.contains("</log>"))
  
  // Deserialize from XML
  let deserialized_log = XmlSerializer::deserialize_log(xml_string)
  match deserialized_log {
    Some(log) => {
      assert_eq(LogData::severity(log), Info)
      assert_eq(LogData::message(log), "Application started successfully")
      assert_eq(LogData::timestamp(log), 1234567890L)
      assert_eq(LogData::trace_id(log), Some("trace_id_123"))
      assert_eq(LogData::span_id(log), Some("span_id_456"))
      
      let attributes = LogData::attributes(log)
      let logger_name = Attributes::get(attributes, "logger.name")
      match logger_name {
        Some(StringValue(name)) => assert_eq(name, "app.logger")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 6: Custom Format Serialization and Deserialization
test "custom format serialization and deserialization" {
  // Test custom format serialization
  let custom_data = CustomTelemetryData::new(
    "custom.event",
    1234567890L,
    [("custom.field1", StringValue("value1")), ("custom.field2", IntValue(42))],
    [1, 2, 3, 4, 5]
  )
  
  // Serialize to custom format
  let custom_string = CustomSerializer::serialize(custom_data)
  assert_true(custom_string.length() > 0)
  
  // Deserialize from custom format
  let deserialized_custom = CustomSerializer::deserialize(custom_string)
  match deserialized_custom {
    Some(data) => {
      assert_eq(CustomTelemetryData::event_type(data), "custom.event")
      assert_eq(CustomTelemetryData::timestamp(data), 1234567890L)
      
      let attributes = CustomTelemetryData::attributes(data)
      let field1 = Attributes::get(attributes, "custom.field1")
      match field1 {
        Some(StringValue(value)) => assert_eq(value, "value1")
        _ => assert_true(false)
      }
      
      let field2 = Attributes::get(attributes, "custom.field2")
      match field2 {
        Some(IntValue(value)) => assert_eq(value, 42)
        _ => assert_true(false)
      }
      
      let array_data = CustomTelemetryData::array_data(data)
      assert_eq(array_data.length(), 5)
      assert_eq(array_data[0], 1)
      assert_eq(array_data[4], 5)
    }
    None => assert_true(false)
  }
}

// Test 7: Compression Serialization and Deserialization
test "compression serialization and deserialization" {
  // Test compression during serialization
  let large_data = LargeTelemetryData::new(
    "large.operation",
    1234567890L,
    // Create a large string to test compression
    "x".repeat(10000)
  )
  
  // Serialize with compression
  let compressed_data = CompressionSerializer::serialize(large_data)
  assert_true(compressed_data.length() > 0)
  
  // The compressed data should be smaller than the original
  let original_size = LargeTelemetryData::data_size(large_data)
  let compressed_size = compressed_data.length()
  assert_true(compressed_size < original_size)
  
  // Deserialize with decompression
  let deserialized_large = CompressionSerializer::deserialize(compressed_data)
  match deserialized_large {
    Some(data) => {
      assert_eq(LargeTelemetryData::operation_name(data), "large.operation")
      assert_eq(LargeTelemetryData::timestamp(data), 1234567890L)
      assert_eq(LargeTelemetryData::large_string(data), "x".repeat(10000))
    }
    None => assert_true(false)
  }
}

// Test 8: Error Handling in Serialization and Deserialization
test "error handling in serialization and deserialization" {
  // Test serialization of invalid data
  let invalid_data = {
    "valid_field": "valid_value",
    "invalid_field": null  // This might cause serialization issues
  }
  
  // Handle serialization errors gracefully
  let result = JsonSerializer::serialize_safe(invalid_data)
  match result {
    Ok(json_string) => assert_true(json_string.length() > 0)
    Err(error) => assert_true(error.contains("serialization"))
  }
  
  // Test deserialization of invalid JSON
  let invalid_json = "{ invalid json string }"
  let deserialization_result = JsonSerializer::deserialize_safe(invalid_json)
  match deserialization_result {
    Ok(data) => assert_true(false)  // Should not succeed with invalid JSON
    Err(error) => assert_true(error.contains("parse") || error.contains("invalid"))
  }
  
  // Test deserialization of valid JSON with missing fields
  let incomplete_json = "{\"name\": \"test\"}"
  let incomplete_result = JsonSerializer::deserialize_safe(incomplete_json)
  match incomplete_result {
    Ok(data) => {
      // Should succeed but with missing fields
      assert_eq(data["name"], "test")
      assert_eq(data.get("trace_id"), None)
    }
    Err(error) => assert_true(false)  // Should handle missing fields gracefully
  }
}

// Test 9: Performance of Serialization and Deserialization
test "performance of serialization and deserialization" {
  // Test serialization performance with large dataset
  let large_dataset = []
  for i in 0..=1000 {
    large_dataset.push({
      "id": i,
      "name": "item_" + i.to_string(),
      "value": i * 2.5,
      "timestamp": 1234567890L + i.to_long(),
      "metadata": "metadata_" + i.to_string()
    })
  }
  
  // Measure serialization time
  let start_time = Time::now()
  let serialized_data = JsonSerializer::serialize(large_dataset)
  let serialization_time = Time::now() - start_time
  
  // Serialization should complete within reasonable time
  assert_true(serialization_time < 5000L)  // Less than 5 seconds
  assert_true(serialized_data.length() > 0)
  
  // Measure deserialization time
  start_time = Time::now()
  let deserialized_dataset = JsonSerializer::deserialize(serialized_data)
  let deserialization_time = Time::now() - start_time
  
  // Deserialization should complete within reasonable time
  assert_true(deserialization_time < 5000L)  // Less than 5 seconds
  
  match deserialized_dataset {
    Some(data) => {
      assert_eq(data.length(), 1001)
      assert_eq(data[0]["id"], 0)
      assert_eq(data[1000]["id"], 1000)
    }
    None => assert_true(false)
  }
}

// Test 10: Cross-Format Conversion
test "cross-format conversion" {
  // Test conversion between different serialization formats
  let original_data = {
    "service": "test_service",
    "operation": "test_operation",
    "timestamp": 1234567890L,
    "attributes": [
      {"key": "attr1", "value": "value1"},
      {"key": "attr2", "value": 42}
    ]
  }
  
  // Serialize to JSON
  let json_data = JsonSerializer::serialize(original_data)
  
  // Convert JSON to XML
  let xml_data = FormatConverter::json_to_xml(json_data)
  assert_true(xml_data.contains("<service>test_service</service>"))
  assert_true(xml_data.contains("<operation>test_operation</operation>"))
  
  // Convert XML to MessagePack
  let msgpack_data = FormatConverter::xml_to_msgpack(xml_data)
  assert_true(msgpack_data.length() > 0)
  
  // Convert MessagePack back to JSON
  let final_json = FormatConverter::msgpack_to_json(msgpack_data)
  
  // Verify data integrity through all conversions
  let final_data = JsonSerializer::deserialize(final_json)
  match final_data {
    Some(data) => {
      assert_eq(data["service"], "test_service")
      assert_eq(data["operation"], "test_operation")
      assert_eq(data["timestamp"], 1234567890L)
    }
    None => assert_true(false)
  }
}