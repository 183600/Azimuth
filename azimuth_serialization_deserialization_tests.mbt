// Azimuth Telemetry System - Serialization and Deserialization Tests
// This file contains comprehensive test cases for serialization and deserialization functionality

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Create test telemetry data
  let telemetry_data = TelemetryData::with_attributes("response_time", 150.5, "ms", 1234567890L, [
    ("service", "auth"),
    ("endpoint", "/login"),
    ("status", "success")
  ])
  
  // Serialize to JSON
  let json_str = JsonSerializer::serialize(telemetry_data)
  
  // Verify JSON contains expected fields
  assert_true(json_str.contains("\"metric_name\":\"response_time\""))
  assert_true(json_str.contains("\"value\":150.5"))
  assert_true(json_str.contains("\"unit\":\"ms\""))
  assert_true(json_str.contains("\"timestamp\":1234567890"))
  assert_true(json_str.contains("\"service\":\"auth\""))
  assert_true(json_str.contains("\"endpoint\":\"/login\""))
  assert_true(json_str.contains("\"status\":\"success\""))
  
  // Deserialize from JSON
  let deserialized_data = JsonSerializer::deserialize(json_str)
  
  // Verify deserialized data matches original
  assert_eq(deserialized_data.metric_name, telemetry_data.metric_name)
  assert_eq(deserialized_data.value, telemetry_data.value)
  assert_eq(deserialized_data.unit, telemetry_data.unit)
  assert_eq(deserialized_data.timestamp, telemetry_data.timestamp)
  
  // Verify attributes were preserved
  let service_attr = TelemetryData::get_attribute(deserialized_data, "service")
  match service_attr {
    Some(StringValue(value)) => assert_eq(value, "auth")
    _ => assert_true(false)
  }
  
  let endpoint_attr = TelemetryData::get_attribute(deserialized_data, "endpoint")
  match endpoint_attr {
    Some(StringValue(value)) => assert_eq(value, "/login")
    _ => assert_true(false)
  }
  
  let status_attr = TelemetryData::get_attribute(deserialized_data, "status")
  match status_attr {
    Some(StringValue(value)) => assert_eq(value, "success")
    _ => assert_true(false)
  }
}

// Test 2: Binary Serialization and Deserialization
test "binary serialization and deserialization" {
  // Create test telemetry data
  let telemetry_data = TelemetryData::with_attributes("cpu_usage", 75.2, "percent", 1234567890L, [
    ("host", "server-01"),
    ("region", "us-west-1"),
    ("environment", "production")
  ])
  
  // Serialize to binary
  let binary_data = BinarySerializer::serialize(telemetry_data)
  
  // Verify binary data is not empty
  assert_true(binary_data.length() > 0)
  
  // Deserialize from binary
  let deserialized_data = BinarySerializer::deserialize(binary_data)
  
  // Verify deserialized data matches original
  assert_eq(deserialized_data.metric_name, telemetry_data.metric_name)
  assert_eq(deserialized_data.value, telemetry_data.value)
  assert_eq(deserialized_data.unit, telemetry_data.unit)
  assert_eq(deserialized_data.timestamp, telemetry_data.timestamp)
  
  // Verify attributes were preserved
  let host_attr = TelemetryData::get_attribute(deserialized_data, "host")
  match host_attr {
    Some(StringValue(value)) => assert_eq(value, "server-01")
    _ => assert_true(false)
  }
  
  let region_attr = TelemetryData::get_attribute(deserialized_data, "region")
  match region_attr {
    Some(StringValue(value)) => assert_eq(value, "us-west-1")
    _ => assert_true(false)
  }
  
  let env_attr = TelemetryData::get_attribute(deserialized_data, "environment")
  match env_attr {
    Some(StringValue(value)) => assert_eq(value, "production")
    _ => assert_true(false)
  }
}

// Test 3: Protocol Buffers Serialization and Deserialization
test "protocol buffers serialization and deserialization" {
  // Create test telemetry data
  let telemetry_data = TelemetryData::with_attributes("memory_usage", 1024.0, "MB", 1234567890L, [
    ("process", "azimuth-telemetry"),
    ("pid", "12345"),
    ("container", "app-container-01")
  ])
  
  // Serialize to Protocol Buffers
  let proto_data = ProtobufSerializer::serialize(telemetry_data)
  
  // Verify proto data is not empty
  assert_true(proto_data.length() > 0)
  
  // Deserialize from Protocol Buffers
  let deserialized_data = ProtobufSerializer::deserialize(proto_data)
  
  // Verify deserialized data matches original
  assert_eq(deserialized_data.metric_name, telemetry_data.metric_name)
  assert_eq(deserialized_data.value, telemetry_data.value)
  assert_eq(deserialized_data.unit, telemetry_data.unit)
  assert_eq(deserialized_data.timestamp, telemetry_data.timestamp)
  
  // Verify attributes were preserved
  let process_attr = TelemetryData::get_attribute(deserialized_data, "process")
  match process_attr {
    Some(StringValue(value)) => assert_eq(value, "azimuth-telemetry")
    _ => assert_true(false)
  }
  
  let pid_attr = TelemetryData::get_attribute(deserialized_data, "pid")
  match pid_attr {
    Some(StringValue(value)) => assert_eq(value, "12345")
    _ => assert_true(false)
  }
  
  let container_attr = TelemetryData::get_attribute(deserialized_data, "container")
  match container_attr {
    Some(StringValue(value)) => assert_eq(value, "app-container-01")
    _ => assert_true(false)
  }
}

// Test 4: Batch Serialization and Deserialization
test "batch serialization and deserialization" {
  // Create multiple telemetry data points
  let telemetry_data_points = [
    TelemetryData::new("metric1", 10.5, "ms", 1234567890L),
    TelemetryData::new("metric2", 100.0, "count", 1234567891L),
    TelemetryData::new("metric3", 0.95, "ratio", 1234567892L),
    TelemetryData::new("metric4", 42.0, "bytes", 1234567893L),
    TelemetryData::new("metric5", 3.14, "pi", 1234567894L)
  ]
  
  // Serialize batch to JSON
  let batch_json = JsonSerializer::serialize_batch(telemetry_data_points)
  
  // Verify JSON contains all metrics
  assert_true(batch_json.contains("\"metric_name\":\"metric1\""))
  assert_true(batch_json.contains("\"metric_name\":\"metric2\""))
  assert_true(batch_json.contains("\"metric_name\":\"metric3\""))
  assert_true(batch_json.contains("\"metric_name\":\"metric4\""))
  assert_true(batch_json.contains("\"metric_name\":\"metric5\""))
  
  // Deserialize batch from JSON
  let deserialized_batch = JsonSerializer::deserialize_batch(batch_json)
  
  // Verify all data points were deserialized
  assert_eq(deserialized_batch.length(), telemetry_data_points.length())
  
  // Verify each data point matches original
  for i in 0..=telemetry_data_points.length() - 1 {
    assert_eq(deserialized_batch[i].metric_name, telemetry_data_points[i].metric_name)
    assert_eq(deserialized_batch[i].value, telemetry_data_points[i].value)
    assert_eq(deserialized_batch[i].unit, telemetry_data_points[i].unit)
    assert_eq(deserialized_batch[i].timestamp, telemetry_data_points[i].timestamp)
  }
}

// Test 5: Cross-Format Conversion
test "cross-format conversion" {
  // Create test telemetry data
  let telemetry_data = TelemetryData::with_attributes("network_latency", 25.7, "ms", 1234567890L, [
    ("source", "client-01"),
    ("destination", "api-server"),
    ("protocol", "HTTP/2")
  ])
  
  // Serialize to JSON
  let json_data = JsonSerializer::serialize(telemetry_data)
  
  // Convert JSON to binary
  let binary_data = FormatConverter::json_to_binary(json_data)
  
  // Convert binary to Protocol Buffers
  let proto_data = FormatConverter::binary_to_protobuf(binary_data)
  
  // Deserialize from Protocol Buffers
  let deserialized_data = ProtobufSerializer::deserialize(proto_data)
  
  // Verify final data matches original
  assert_eq(deserialized_data.metric_name, telemetry_data.metric_name)
  assert_eq(deserialized_data.value, telemetry_data.value)
  assert_eq(deserialized_data.unit, telemetry_data.unit)
  assert_eq(deserialized_data.timestamp, telemetry_data.timestamp)
  
  // Verify attributes were preserved through all conversions
  let source_attr = TelemetryData::get_attribute(deserialized_data, "source")
  match source_attr {
    Some(StringValue(value)) => assert_eq(value, "client-01")
    _ => assert_true(false)
  }
  
  let dest_attr = TelemetryData::get_attribute(deserialized_data, "destination")
  match dest_attr {
    Some(StringValue(value)) => assert_eq(value, "api-server")
    _ => assert_true(false)
  }
  
  let protocol_attr = TelemetryData::get_attribute(deserialized_data, "protocol")
  match protocol_attr {
    Some(StringValue(value)) => assert_eq(value, "HTTP/2")
    _ => assert_true(false)
  }
}

// Test 6: Schema Validation and Evolution
test "schema validation and evolution" {
  // Create schema validator
  let schema_validator = SchemaValidator::new()
  
  // Define schema for telemetry data
  let schema = Schema::new("TelemetryData", [
    Field::new("metric_name", FieldType::String, true),
    Field::new("value", FieldType::Float, true),
    Field::new("unit", FieldType::String, true),
    Field::new("timestamp", FieldType::Long, true),
    Field::new("attributes", FieldType::Map(String, AttributeValue), false)
  ])
  
  // Register schema
  SchemaValidator::register_schema(schema_validator, "TelemetryData", schema)
  
  // Create valid telemetry data
  let valid_data = TelemetryData::new("valid_metric", 42.0, "count", 1234567890L)
  
  // Serialize valid data
  let valid_json = JsonSerializer::serialize(valid_data)
  
  // Validate against schema
  let validation_result = SchemaValidator::validate(schema_validator, "TelemetryData", valid_json)
  assert_true(validation_result.is_valid)
  
  // Create invalid telemetry data (missing required field)
  let invalid_json = "{\"metric_name\":\"invalid_metric\",\"value\":42.0}" // Missing unit and timestamp
  
  // Validate against schema
  let validation_result = SchemaValidator::validate(schema_validator, "TelemetryData", invalid_json)
  assert_false(validation_result.is_valid)
  assert_true(validation_result.errors.length() > 0)
  
  // Test schema evolution (add new optional field)
  let evolved_schema = Schema::new("TelemetryData", [
    Field::new("metric_name", FieldType::String, true),
    Field::new("value", FieldType::Float, true),
    Field::new("unit", FieldType::String, true),
    Field::new("timestamp", FieldType::Long, true),
    Field::new("attributes", FieldType::Map(String, AttributeValue), false),
    Field::new("metadata", FieldType::Map(String, String), false) // New optional field
  ])
  
  // Register evolved schema
  SchemaValidator::register_schema(schema_validator, "TelemetryData_v2", evolved_schema)
  
  // Old data should still be valid against new schema
  let validation_result = SchemaValidator::validate(schema_validator, "TelemetryData_v2", valid_json)
  assert_true(validation_result.is_valid)
  
  // New data with additional field should also be valid
  let new_data_json = "{\"metric_name\":\"new_metric\",\"value\":42.0,\"unit\":\"count\",\"timestamp\":1234567890,\"metadata\":{\"version\":\"1.0\"}}"
  let validation_result = SchemaValidator::validate(schema_validator, "TelemetryData_v2", new_data_json)
  assert_true(validation_result.is_valid)
}

// Test 7: Compression and Decompression
test "compression and decompression" {
  // Create large telemetry data batch
  let large_batch = []
  for i in 0..=1000 {
    large_batch.push(TelemetryData::with_attributes(
      "metric_" + i.to_string(),
      i.to_float(),
      "unit",
      1234567890L + i.to_long(),
      [
        ("attr1", "value1_" + i.to_string()),
        ("attr2", "value2_" + i.to_string()),
        ("attr3", "value3_" + i.to_string())
      ]
    ))
  }
  
  // Serialize batch to JSON
  let json_data = JsonSerializer::serialize_batch(large_batch)
  
  // Compress JSON data
  let compressed_data = CompressionUtil::compress(json_data, CompressionAlgorithm::Gzip)
  
  // Verify compressed data is smaller
  assert_true(compressed_data.length() < json_data.length())
  
  // Decompress data
  let decompressed_json = CompressionUtil::decompress(compressed_data, CompressionAlgorithm::Gzip)
  
  // Verify decompressed data matches original
  assert_eq(decompressed_json, json_data)
  
  // Deserialize decompressed data
  let deserialized_batch = JsonSerializer::deserialize_batch(decompressed_json)
  
  // Verify all data points were preserved
  assert_eq(deserialized_batch.length(), large_batch.length())
  
  for i in 0..=large_batch.length() - 1 {
    assert_eq(deserialized_batch[i].metric_name, large_batch[i].metric_name)
    assert_eq(deserialized_batch[i].value, large_batch[i].value)
    assert_eq(deserialized_batch[i].unit, large_batch[i].unit)
    assert_eq(deserialized_batch[i].timestamp, large_batch[i].timestamp)
  }
}

// Test 8: Error Handling in Serialization
test "error handling in serialization" {
  // Test serialization of null/invalid data
  let invalid_data = TelemetryData::new("", 0.0, "", 0L) // All fields empty/zero
  
  // Attempt to serialize
  let result = JsonSerializer::try_serialize(invalid_data)
  
  match result {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => assert_eq(error.message, "Invalid telemetry data: empty metric name")
  }
  
  // Test deserialization of malformed JSON
  let malformed_json = "{\"metric_name\":\"test\",\"value\":not_a_number,\"unit\":\"ms\",\"timestamp\":1234567890}"
  
  let result = JsonSerializer::try_deserialize(malformed_json)
  
  match result {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => assert_eq(error.message, "Invalid JSON: cannot parse value as number")
  }
  
  // Test deserialization of incomplete JSON
  let incomplete_json = "{\"metric_name\":\"test\",\"value\":42.0" // Missing closing braces and fields
  
  let result = JsonSerializer::try_deserialize(incomplete_json)
  
  match result {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => assert_eq(error.message, "Invalid JSON: unexpected end of input")
  }
  
  // Test handling of unknown fields during deserialization
  let json_with_unknown_fields = "{\"metric_name\":\"test\",\"value\":42.0,\"unit\":\"ms\",\"timestamp\":1234567890,\"unknown_field\":\"unknown_value\"}"
  
  // Should succeed with warning
  let result = JsonSerializer::try_deserialize_with_options(json_with_unknown_fields, DeserializationOptions::new().with_strict(false))
  
  match result {
    Ok(data) => {
      assert_eq(data.metric_name, "test")
      assert_eq(data.value, 42.0)
      assert_eq(data.unit, "ms")
      assert_eq(data.timestamp, 1234567890L)
    }
    Err(_) => assert_true(false)
  }
  
  // Should fail in strict mode
  let result = JsonSerializer::try_deserialize_with_options(json_with_unknown_fields, DeserializationOptions::new().with_strict(true))
  
  match result {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => assert_eq(error.message, "Unknown field: unknown_field")
  }
}