// Azimuth Telemetry System - Serialization Format Enhanced Tests
// This file contains comprehensive test cases for data serialization and deserialization

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Create complex telemetry data
  let telemetry_data = TelemetryData::new()
  
  // Add metrics
  TelemetryData::add_metric(telemetry_data, "cpu_usage", 75.5)
  TelemetryData::add_metric(telemetry_data, "memory_usage", 1024.0)
  TelemetryData::add_metric(telemetry_data, "disk_io", 256.7)
  
  // Add attributes
  TelemetryData::add_attribute(telemetry_data, "service", "auth_service")
  TelemetryData::add_attribute(telemetry_data, "environment", "production")
  TelemetryData::add_attribute(telemetry_data, "version", "1.2.3")
  
  // Add nested attributes
  let nested_attrs = Attributes::new()
  Attributes::set(nested_attrs, "region", "us-west")
  Attributes::set(nested_attrs, "zone", "us-west-2a")
  TelemetryData::add_nested_attribute(telemetry_data, "location", nested_attrs)
  
  // Serialize to JSON
  let json_string = TelemetryData::to_json(telemetry_data)
  assert_true(json_string.contains("cpu_usage"))
  assert_true(json_string.contains("75.5"))
  assert_true(json_string.contains("auth_service"))
  assert_true(json_string.contains("location"))
  
  // Deserialize from JSON
  let deserialized_data = TelemetryData::from_json(json_string)
  
  // Verify deserialized data
  let cpu_metric = TelemetryData::get_metric(deserialized_data, "cpu_usage")
  match cpu_metric {
    Some(value) => assert_eq(value, 75.5),
    None => assert_true(false)
  }
  
  let service_attr = TelemetryData::get_attribute(deserialized_data, "service")
  match service_attr {
    StringValue(value) => assert_eq(value, "auth_service"),
    _ => assert_true(false)
  }
  
  let location_nested = TelemetryData::get_nested_attribute(deserialized_data, "location")
  match location_nested {
    Attributes(attrs) => {
      let region = Attributes::get(attrs, "region")
      match region {
        StringValue(value) => assert_eq(value, "us-west"),
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// Test 2: Protocol Buffers Serialization
test "protocol buffers serialization" {
  // Create telemetry data for protobuf
  let telemetry_data = TelemetryData::new()
  
  // Add metrics with various types
  TelemetryData::add_metric(telemetry_data, "counter", 42.0)
  TelemetryData::add_metric(telemetry_data, "gauge", 3.14159)
  TelemetryData::add_metric(telemetry_data, "histogram", 100.0)
  
  // Add attributes
  TelemetryData::add_attribute(telemetry_data, "string_attr", "test_value")
  TelemetryData::add_attribute(telemetry_data, "int_attr", 123)
  TelemetryData::add_attribute(telemetry_data, "bool_attr", true)
  TelemetryData::add_attribute(telemetry_data, "float_attr", 45.67)
  
  // Serialize to Protocol Buffers
  let protobuf_data = TelemetryData::to_protobuf(telemetry_data)
  assert_true(protobuf_data.length() > 0)
  
  // Deserialize from Protocol Buffers
  let deserialized_data = TelemetryData::from_protobuf(protobuf_data)
  
  // Verify deserialized data
  let counter_metric = TelemetryData::get_metric(deserialized_data, "counter")
  match counter_metric {
    Some(value) => assert_eq(value, 42.0),
    None => assert_true(false)
  }
  
  let string_attr = TelemetryData::get_attribute(deserialized_data, "string_attr")
  match string_attr {
    StringValue(value) => assert_eq(value, "test_value"),
    _ => assert_true(false)
  }
  
  let bool_attr = TelemetryData::get_attribute(deserialized_data, "bool_attr")
  match bool_attr {
    BoolValue(value) => assert_eq(value, true),
    _ => assert_true(false)
  }
}

// Test 3: MessagePack Serialization
test "messagepack serialization" {
  // Create telemetry data for MessagePack
  let telemetry_data = TelemetryData::new()
  
  // Add array of metrics
  let metrics = [
    ("metric1", 10.5),
    ("metric2", 20.3),
    ("metric3", 30.7)
  ]
  
  for (name, value) in metrics {
    TelemetryData::add_metric(telemetry_data, name, value)
  }
  
  // Add array of attributes
  let attributes = [
    ("attr1", "value1"),
    ("attr2", "value2"),
    ("attr3", "value3")
  ]
  
  for (name, value) in attributes {
    TelemetryData::add_attribute(telemetry_data, name, value)
  }
  
  // Serialize to MessagePack
  let msgpack_data = TelemetryData::to_msgpack(telemetry_data)
  assert_true(msgpack_data.length() > 0)
  
  // Verify MessagePack is more compact than JSON
  let json_data = TelemetryData::to_json(telemetry_data)
  assert_true(msgpack_data.length() < json_data.length())
  
  // Deserialize from MessagePack
  let deserialized_data = TelemetryData::from_msgpack(msgpack_data)
  
  // Verify deserialized data
  for (name, value) in metrics {
    let metric = TelemetryData::get_metric(deserialized_data, name)
    match metric {
      Some(v) => assert_eq(v, value),
      None => assert_true(false)
    }
  }
  
  for (name, value) in attributes {
    let attr = TelemetryData::get_attribute(deserialized_data, name)
    match attr {
      StringValue(v) => assert_eq(v, value),
      _ => assert_true(false)
    }
  }
}

// Test 4: Binary Serialization with Custom Format
test "binary serialization with custom format" {
  // Create custom binary serializer
  let serializer = BinarySerializer::new()
  
  // Configure serializer with custom format
  BinarySerializer::set_magic_number(serializer, 0x415A494D) // "AZIM"
  BinarySerializer::set_version(serializer, 1)
  BinarySerializer::enable_compression(serializer, true)
  
  // Create complex telemetry data
  let telemetry_data = TelemetryData::new()
  
  // Add time-series data
  let time_series = []
  for i in 0..=100 {
    let timestamp = 1234567890L + (i * 1000)
    let value = 50.0 + (i.to_float() * 0.5)
    time_series.push(TimeSeriesPoint::new(timestamp, value))
  }
  TelemetryData::add_time_series(telemetry_data, "cpu_time_series", time_series)
  
  // Add histogram data
  let histogram_buckets = [
    (0.0, 10),
    (10.0, 25),
    (25.0, 50),
    (50.0, 100),
    (100.0, 15)
  ]
  
  for (boundary, count) in histogram_buckets {
    TelemetryData::add_histogram_bucket(telemetry_data, "response_time", boundary, count)
  }
  
  // Serialize to custom binary format
  let binary_data = BinarySerializer::serialize(serializer, telemetry_data)
  assert_true(binary_data.length() > 0)
  
  // Verify magic number and version
  assert_eq(binary_data[0], 0x41) // 'A'
  assert_eq(binary_data[1], 0x5A) // 'Z'
  assert_eq(binary_data[2], 0x49) // 'I'
  assert_eq(binary_data[3], 0x4D) // 'M'
  assert_eq(binary_data[4], 1)    // Version 1
  
  // Deserialize from custom binary format
  let deserializer = BinaryDeserializer::new()
  let deserialized_data = BinaryDeserializer::deserialize(deserializer, binary_data)
  
  // Verify deserialized time-series data
  let deserialized_time_series = TelemetryData::get_time_series(deserialized_data, "cpu_time_series")
  match deserialized_time_series {
    TimeSeries(points) => {
      assert_eq(points.length(), 101)
      
      // Verify first and last points
      match points[0] {
        TimeSeriesPoint(timestamp, value) => {
          assert_eq(timestamp, 1234567890L)
          assert_eq(value, 50.0)
        }
        _ => assert_true(false)
      }
      
      match points[100] {
        TimeSeriesPoint(timestamp, value) => {
          assert_eq(timestamp, 1234567890L + 100000)
          assert_eq(value, 100.0)
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Verify deserialized histogram data
  let deserialized_histogram = TelemetryData::get_histogram(deserialized_data, "response_time")
  match deserialized_histogram {
    Histogram(buckets) => {
      assert_eq(buckets.length(), 5)
      
      for (i, (boundary, count)) in histogram_buckets.enumerate() {
        match buckets[i] {
          HistogramBucket(b, c) => {
            assert_eq(b, boundary)
            assert_eq(c, count)
          }
          _ => assert_true(false)
        }
      }
    }
    _ => assert_true(false)
  }
}

// Test 5: Cross-Format Serialization and Conversion
test "cross-format serialization and conversion" {
  // Create telemetry data
  let telemetry_data = TelemetryData::new()
  
  // Add diverse data
  TelemetryData::add_metric(telemetry_data, "temperature", 23.5)
  TelemetryData::add_attribute(telemetry_data, "sensor_id", "temp_001")
  TelemetryData::add_attribute(telemetry_data, "location", "server_room")
  TelemetryData::add_attribute(telemetry_data, "active", true)
  
  // Serialize to JSON
  let json_data = TelemetryData::to_json(telemetry_data)
  
  // Convert JSON to Protocol Buffers
  let protobuf_data = SerializationConverter::json_to_protobuf(json_data)
  
  // Convert Protocol Buffers to MessagePack
  let msgpack_data = SerializationConverter::protobuf_to_msgpack(protobuf_data)
  
  // Convert MessagePack back to JSON
  let final_json_data = SerializationConverter::msgpack_to_json(msgpack_data)
  
  // Deserialize from final JSON
  let final_data = TelemetryData::from_json(final_json_data)
  
  // Verify data integrity through all conversions
  let temperature = TelemetryData::get_metric(final_data, "temperature")
  match temperature {
    Some(value) => assert_eq(value, 23.5),
    None => assert_true(false)
  }
  
  let sensor_id = TelemetryData::get_attribute(final_data, "sensor_id")
  match sensor_id {
    StringValue(value) => assert_eq(value, "temp_001"),
    _ => assert_true(false)
  }
  
  let active = TelemetryData::get_attribute(final_data, "active")
  match active {
    BoolValue(value) => assert_eq(value, true),
    _ => assert_true(false)
  }
  
  // Verify size differences between formats
  assert_true(protobuf_data.length() < json_data.length())
  assert_true(msgpack_data.length() < json_data.length())
  
  // Verify all formats contain the same data
  let json_deserialized = TelemetryData::from_json(json_data)
  let protobuf_deserialized = TelemetryData::from_protobuf(protobuf_data)
  let msgpack_deserialized = TelemetryData::from_msgpack(msgpack_data)
  
  assert_true(TelemetryData::equals(final_data, json_deserialized))
  assert_true(TelemetryData::equals(final_data, protobuf_deserialized))
  assert_true(TelemetryData::equals(final_data, msgpack_deserialized))
}

// Test 6: Serialization Error Handling and Validation
test "serialization error handling and validation" {
  // Test with invalid JSON
  let invalid_json = "{ invalid json }"
  
  let result = TelemetryData::from_json(invalid_json)
  match result {
    Ok(_) => assert_true(false), // Should not succeed
    Error(e) => {
      assert_true(Error::is_parse_error(e))
      assert_true(Error::message(e).contains("JSON"))
    }
  }
  
  // Test with corrupted Protocol Buffers data
  let corrupted_protobuf = [0x08, 0x96, 0x01, 0x12, 0x03, 0x74, 0x65] // Truncated data
  
  let protobuf_result = TelemetryData::from_protobuf(corrupted_protobuf)
  match protobuf_result {
    Ok(_) => assert_true(false), // Should not succeed
    Error(e) => {
      assert_true(Error::is_parse_error(e))
      assert_true(Error::message(e).contains("Protocol Buffers"))
    }
  }
  
  // Test with corrupted MessagePack data
  let corrupted_msgpack = [0x82, 0xA7, 0x6D, 0x65, 0x74, 0x72, 0x69, 0x63] // Truncated data
  
  let msgpack_result = TelemetryData::from_msgpack(corrupted_msgpack)
  match msgpack_result {
    Ok(_) => assert_true(false), // Should not succeed
    Error(e) => {
      assert_true(Error::is_parse_error(e))
      assert_true(Error::message(e).contains("MessagePack"))
    }
  }
  
  // Test with too large data
  let large_data = TelemetryData::new()
  
  // Add a very large string attribute
  let large_string = "x".repeat(10000000) // 10MB string
  TelemetryData::add_attribute(large_data, "large_attr", large_string)
  
  let large_result = TelemetryData::to_json(large_data)
  match large_result {
    Ok(_) => assert_true(true), // Should succeed
    Error(e) => {
      // Might fail due to memory constraints
      assert_true(Error::is_memory_error(e))
    }
  }
  
  // Test with circular references (if supported)
  let circular_data = TelemetryData::new()
  TelemetryData::add_attribute(circular_data, "self_ref", circular_data)
  
  let circular_result = TelemetryData::to_json(circular_data)
  match circular_result {
    Ok(_) => assert_true(false), // Should not succeed
    Error(e) => {
      assert_true(Error::is_circular_reference_error(e))
    }
  }
}

// Test 7: Serialization Performance and Optimization
test "serialization performance and optimization" {
  // Create large telemetry dataset
  let large_data = TelemetryData::new()
  
  // Add many metrics
  for i in 0..=1000 {
    TelemetryData::add_metric(large_data, "metric_" + i.to_string(), i.to_float())
  }
  
  // Add many attributes
  for i in 0..=1000 {
    TelemetryData::add_attribute(large_data, "attr_" + i.to_string(), "value_" + i.to_string())
  }
  
  // Test JSON serialization performance
  let json_start = Time::now()
  let json_data = TelemetryData::to_json(large_data)
  let json_end = Time::now()
  let json_time = json_end - json_start
  
  // Test JSON deserialization performance
  let json_deserialize_start = Time::now()
  let _ = TelemetryData::from_json(json_data)
  let json_deserialize_end = Time::now()
  let json_deserialize_time = json_deserialize_end - json_deserialize_start
  
  // Test Protocol Buffers serialization performance
  let protobuf_start = Time::now()
  let protobuf_data = TelemetryData::to_protobuf(large_data)
  let protobuf_end = Time::now()
  let protobuf_time = protobuf_end - protobuf_start
  
  // Test Protocol Buffers deserialization performance
  let protobuf_deserialize_start = Time::now()
  let _ = TelemetryData::from_protobuf(protobuf_data)
  let protobuf_deserialize_end = Time::now()
  let protobuf_deserialize_time = protobuf_deserialize_end - protobuf_deserialize_start
  
  // Test MessagePack serialization performance
  let msgpack_start = Time::now()
  let msgpack_data = TelemetryData::to_msgpack(large_data)
  let msgpack_end = Time::now()
  let msgpack_time = msgpack_end - msgpack_start
  
  // Test MessagePack deserialization performance
  let msgpack_deserialize_start = Time::now()
  let _ = TelemetryData::from_msgpack(msgpack_data)
  let msgpack_deserialize_end = Time::now()
  let msgpack_deserialize_time = msgpack_deserialize_end - msgpack_deserialize_start
  
  // Verify performance expectations
  // Protocol Buffers and MessagePack should be faster than JSON
  assert_true(protobuf_time < json_time)
  assert_true(msgpack_time < json_time)
  assert_true(protobuf_deserialize_time < json_deserialize_time)
  assert_true(msgpack_deserialize_time < json_deserialize_time)
  
  // Verify size expectations
  // Protocol Buffers and MessagePack should be more compact than JSON
  assert_true(protobuf_data.length() < json_data.length())
  assert_true(msgpack_data.length() < json_data.length())
  
  // Performance should be reasonable (less than 1 second for this dataset)
  assert_true(json_time < 1000000)  // Less than 1 second
  assert_true(protobuf_time < 1000000)  // Less than 1 second
  assert_true(msgpack_time < 1000000)  // Less than 1 second
}