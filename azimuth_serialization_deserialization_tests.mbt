// Azimuth Data Serialization and Deserialization Tests
// This file contains test cases for data serialization and deserialization

// Test 1: JSON Serialization
test "json serialization" {
  let telemetry_data = TelemetryData::new(
    "service-123",
    "operation-456",
    [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("/api/users")),
      ("http.status_code", IntValue(200)),
      ("duration_ms", FloatValue(125.5))
    ],
    1234567890L
  )
  
  // Test JSON serialization
  let serializer = JsonSerializer::new()
  let json_data = Serializer::serialize(serializer, telemetry_data)
  
  // Verify JSON structure
  assert_true(json_data.contains("\"service_name\":\"service-123\""))
  assert_true(json_data.contains("\"operation_name\":\"operation-456\""))
  assert_true(json_data.contains("\"http.method\":\"GET\""))
  assert_true(json_data.contains("\"http.status_code\":200"))
  assert_true(json_data.contains("\"duration_ms\":125.5"))
  
  // Test JSON deserialization
  let deserializer = JsonDeserializer::new()
  let deserialized_data = Deserializer::deserialize(deserializer, json_data)
  
  // Verify deserialized data
  assert_eq(TelemetryData::service_name(deserialized_data), "service-123")
  assert_eq(TelemetryData::operation_name(deserialized_data), "operation-456")
  assert_eq(TelemetryData::timestamp(deserialized_data), 1234567890L)
  
  let http_method = TelemetryData::get_attribute(deserialized_data, "http.method")
  match http_method {
    Some(StringValue(method)) => assert_eq(method, "GET")
    _ => assert_true(false)
  }
  
  let status_code = TelemetryData::get_attribute(deserialized_data, "http.status_code")
  match status_code {
    Some(IntValue(code)) => assert_eq(code, 200)
    _ => assert_true(false)
  }
}

// Test 2: XML Serialization
test "xml serialization" {
  let telemetry_data = TelemetryData::new(
    "service-123",
    "operation-456",
    [
      ("http.method", StringValue("POST")),
      ("http.url", StringValue("/api/data")),
      ("request.size", IntValue(1024)),
      ("response.size", IntValue(2048))
    ],
    1234567890L
  )
  
  // Test XML serialization
  let serializer = XmlSerializer::new()
  let xml_data = Serializer::serialize(serializer, telemetry_data)
  
  // Verify XML structure
  assert_true(xml_data.contains("<telemetry>"))
  assert_true(xml_data.contains("<service_name>service-123</service_name>"))
  assert_true(xml_data.contains("<operation_name>operation-456</operation_name>"))
  assert_true(xml_data.contains("<attributes>"))
  assert_true(xml_data.contains("<attribute name=\"http.method\" type=\"string\">POST</attribute>"))
  assert_true(xml_data.contains("<attribute name=\"request.size\" type=\"int\">1024</attribute>"))
  assert_true(xml_data.contains("</telemetry>"))
  
  // Test XML deserialization
  let deserializer = XmlDeserializer::new()
  let deserialized_data = Deserializer::deserialize(deserializer, xml_data)
  
  // Verify deserialized data
  assert_eq(TelemetryData::service_name(deserialized_data), "service-123")
  assert_eq(TelemetryData::operation_name(deserialized_data), "operation-456")
  
  let http_method = TelemetryData::get_attribute(deserialized_data, "http.method")
  match http_method {
    Some(StringValue(method)) => assert_eq(method, "POST")
    _ => assert_true(false)
  }
}

// Test 3: Binary Serialization
test "binary serialization" {
  let telemetry_data = TelemetryData::new(
    "service-123",
    "operation-456",
    [
      ("counter", IntValue(42)),
      ("ratio", FloatValue(3.14159)),
      ("enabled", BoolValue(true))
    ],
    1234567890L
  )
  
  // Test binary serialization
  let serializer = BinarySerializer::new()
  let binary_data = Serializer::serialize(serializer, telemetry_data)
  
  // Binary data should be non-empty
  assert_true(binary_data.length() > 0)
  
  // Test binary deserialization
  let deserializer = BinaryDeserializer::new()
  let deserialized_data = Deserializer::deserialize(deserializer, binary_data)
  
  // Verify deserialized data
  assert_eq(TelemetryData::service_name(deserialized_data), "service-123")
  assert_eq(TelemetryData::operation_name(deserialized_data), "operation-456")
  
  let counter = TelemetryData::get_attribute(deserialized_data, "counter")
  match counter {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  let ratio = TelemetryData::get_attribute(deserialized_data, "ratio")
  match ratio {
    Some(FloatValue(value)) => assert_true((value - 3.14159).abs() < 0.00001)
    _ => assert_true(false)
  }
  
  let enabled = TelemetryData::get_attribute(deserialized_data, "enabled")
  match enabled {
    Some(BoolValue(value)) => assert_true(value)
    _ => assert_true(false)
  }
}

// Test 4: Protocol Buffers Serialization
test "protocol buffers serialization" {
  let telemetry_data = TelemetryData::new(
    "service-123",
    "operation-456",
    [
      ("user.id", StringValue("user-789")),
      ("session.id", StringValue("session-abc")),
      ("event.count", IntValue(10))
    ],
    1234567890L
  )
  
  // Test Protocol Buffers serialization
  let serializer = ProtobufSerializer::new()
  let protobuf_data = Serializer::serialize(serializer, telemetry_data)
  
  // Protobuf data should be non-empty
  assert_true(protobuf_data.length() > 0)
  
  // Test Protocol Buffers deserialization
  let deserializer = ProtobufDeserializer::new()
  let deserialized_data = Deserializer::deserialize(deserializer, protobuf_data)
  
  // Verify deserialized data
  assert_eq(TelemetryData::service_name(deserialized_data), "service-123")
  assert_eq(TelemetryData::operation_name(deserialized_data), "operation-456")
  
  let user_id = TelemetryData::get_attribute(deserialized_data, "user.id")
  match user_id {
    Some(StringValue(id)) => assert_eq(id, "user-789")
    _ => assert_true(false)
  }
}

// Test 5: MessagePack Serialization
test "messagepack serialization" {
  let telemetry_data = TelemetryData::new(
    "service-123",
    "operation-456",
    [
      ("temperature", FloatValue(23.5)),
      ("humidity", FloatValue(65.2)),
      ("pressure", FloatValue(1013.25))
    ],
    1234567890L
  )
  
  // Test MessagePack serialization
  let serializer = MessagePackSerializer::new()
  let msgpack_data = Serializer::serialize(serializer, telemetry_data)
  
  // MessagePack data should be non-empty
  assert_true(msgpack_data.length() > 0)
  
  // Test MessagePack deserialization
  let deserializer = MessagePackDeserializer::new()
  let deserialized_data = Deserializer::deserialize(deserializer, msgpack_data)
  
  // Verify deserialized data
  assert_eq(TelemetryData::service_name(deserialized_data), "service-123")
  assert_eq(TelemetryData::operation_name(deserialized_data), "operation-456")
  
  let temperature = TelemetryData::get_attribute(deserialized_data, "temperature")
  match temperature {
    Some(FloatValue(value)) => assert_true((value - 23.5).abs() < 0.00001)
    _ => assert_true(false)
  }
}

// Test 6: CSV Serialization
test "csv serialization" {
  let telemetry_data_list = [
    TelemetryData::new("service-1", "op-1", [("value", IntValue(10))], 1234567890L),
    TelemetryData::new("service-1", "op-2", [("value", IntValue(20))], 1234567891L),
    TelemetryData::new("service-2", "op-1", [("value", IntValue(30))], 1234567892L)
  ]
  
  // Test CSV serialization
  let serializer = CsvSerializer::new()
  let csv_data = Serializer::serialize_list(serializer, telemetry_data_list)
  
  // Verify CSV structure
  assert_true(csv_data.contains("service_name,operation_name,timestamp,value"))
  assert_true(csv_data.contains("service-1,op-1,1234567890,10"))
  assert_true(csv_data.contains("service-1,op-2,1234567891,20"))
  assert_true(csv_data.contains("service-2,op-1,1234567892,30"))
  
  // Test CSV deserialization
  let deserializer = CsvDeserializer::new()
  let deserialized_list = Deserializer::deserialize_list(deserializer, csv_data)
  
  // Verify deserialized data
  assert_eq(deserialized_list.length(), 3)
  
  let first_item = deserialized_list[0]
  assert_eq(TelemetryData::service_name(first_item), "service-1")
  assert_eq(TelemetryData::operation_name(first_item), "op-1")
  
  let value = TelemetryData::get_attribute(first_item, "value")
  match value {
    Some(IntValue(v)) => assert_eq(v, 10)
    _ => assert_true(false)
  }
}

// Test 7: YAML Serialization
test "yaml serialization" {
  let telemetry_data = TelemetryData::new(
    "service-123",
    "operation-456",
    [
      ("database.host", StringValue("db.example.com")),
      ("database.port", IntValue(5432)),
      ("database.ssl", BoolValue(true))
    ],
    1234567890L
  )
  
  // Test YAML serialization
  let serializer = YamlSerializer::new()
  let yaml_data = Serializer::serialize(serializer, telemetry_data)
  
  // Verify YAML structure
  assert_true(yaml_data.contains("service_name: service-123"))
  assert_true(yaml_data.contains("operation_name: operation-456"))
  assert_true(yaml_data.contains("attributes:"))
  assert_true(yaml_data.contains("database.host: db.example.com"))
  assert_true(yaml_data.contains("database.port: 5432"))
  assert_true(yaml_data.contains("database.ssl: true"))
  
  // Test YAML deserialization
  let deserializer = YamlDeserializer::new()
  let deserialized_data = Deserializer::deserialize(deserializer, yaml_data)
  
  // Verify deserialized data
  assert_eq(TelemetryData::service_name(deserialized_data), "service-123")
  assert_eq(TelemetryData::operation_name(deserialized_data), "operation-456")
  
  let db_host = TelemetryData::get_attribute(deserialized_data, "database.host")
  match db_host {
    Some(StringValue(host)) => assert_eq(host, "db.example.com")
    _ => assert_true(false)
  }
}

// Test 8: Custom Serialization Format
test "custom serialization format" {
  let telemetry_data = TelemetryData::new(
    "service-123",
    "operation-456",
    [
      ("custom.field1", StringValue("value1")),
      ("custom.field2", IntValue(42)),
      ("custom.field3", BoolValue(false))
    ],
    1234567890L
  )
  
  // Test custom serialization (key-value pairs with type prefixes)
  let serializer = CustomSerializer::new()
  let custom_data = Serializer::serialize(serializer, telemetry_data)
  
  // Verify custom format
  assert_true(custom_data.contains("s:service_name=service-123"))
  assert_true(custom_data.contains("s:operation_name=operation-456"))
  assert_true(custom_data.contains("t:1234567890"))
  assert_true(custom_data.contains("a:custom.field1=s:value1"))
  assert_true(custom_data.contains("a:custom.field2=i:42"))
  assert_true(custom_data.contains("a:custom.field3=b:false"))
  
  // Test custom deserialization
  let deserializer = CustomDeserializer::new()
  let deserialized_data = Deserializer::deserialize(deserializer, custom_data)
  
  // Verify deserialized data
  assert_eq(TelemetryData::service_name(deserialized_data), "service-123")
  assert_eq(TelemetryData::operation_name(deserialized_data), "operation-456")
  
  let field1 = TelemetryData::get_attribute(deserialized_data, "custom.field1")
  match field1 {
    Some(StringValue(value)) => assert_eq(value, "value1")
    _ => assert_true(false)
  }
}

// Test 9: Serialization with Compression
test "serialization with compression" {
  let telemetry_data = TelemetryData::new(
    "service-123",
    "operation-456",
    [
      ("large.data", StringValue("x".repeat(1000))),
      ("another.large.data", StringValue("y".repeat(1000)))
    ],
    1234567890L
  )
  
  // Test JSON serialization with compression
  let json_serializer = JsonSerializer::new()
  let compressor = GzipCompressor::new()
  
  let json_data = Serializer::serialize(json_serializer, telemetry_data)
  let compressed_json = Compressor::compress(compressor, json_data.to_bytes())
  
  // Verify compression reduces size
  assert_true(compressed_json.length() < json_data.length())
  
  // Test decompression and deserialization
  let decompressor = GzipDecompressor::new()
  let json_deserializer = JsonDeserializer::new()
  
  let decompressed_json = Decompressor::decompress(decompressor, compressed_json)
  let deserialized_data = Deserializer::deserialize(json_deserializer, decompressed_json.to_string())
  
  // Verify deserialized data
  assert_eq(TelemetryData::service_name(deserialized_data), "service-123")
  assert_eq(TelemetryData::operation_name(deserialized_data), "operation-456")
  
  let large_data = TelemetryData::get_attribute(deserialized_data, "large.data")
  match large_data {
    Some(StringValue(data)) => assert_eq(data, "x".repeat(1000))
    _ => assert_true(false)
  }
}

// Test 10: Serialization Error Handling
test "serialization error handling" {
  // Test serialization of invalid data
  let invalid_data = TelemetryData::new("", "", [], -1L) // Invalid empty strings and negative timestamp
  
  let json_serializer = JsonSerializer::new()
  let result = Serializer::try_serialize(json_serializer, invalid_data)
  
  match result {
    Success(_) => assert_true(false) // Should fail with invalid data
    Error(error) => assert_eq(error.code, SerializationError)
  }
  
  // Test deserialization of malformed data
  let malformed_json = "{ invalid json }"
  let json_deserializer = JsonDeserializer::new()
  let result2 = Deserializer::try_deserialize(json_deserializer, malformed_json)
  
  match result2 {
    Success(_) => assert_true(false) // Should fail with malformed data
    Error(error) => assert_eq(error.code, DeserializationError)
  }
  
  // Test deserialization with missing required fields
  let incomplete_json = "{\"service_name\":\"test\"}" // Missing operation_name
  let result3 = Deserializer::try_deserialize(json_deserializer, incomplete_json)
  
  match result3 {
    Success(_) => assert_true(false) // Should fail with incomplete data
    Error(error) => assert_eq(error.code, ValidationError)
  }
  
  // Test deserialization with type mismatch
  let type_mismatch_json = "{\"service_name\":\"test\",\"operation_name\":\"op\",\"timestamp\":\"not_a_number\"}"
  let result4 = Deserializer::try_deserialize(json_deserializer, type_mismatch_json)
  
  match result4 {
    Success(_) => assert_true(false) // Should fail with type mismatch
    Error(error) => assert_eq(error.code, TypeMismatchError)
  }
}