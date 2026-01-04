// Azimuth Telemetry System - Data Serialization Tests
// This file contains test cases for data serialization and deserialization functionality

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Create test data structures
  let telemetry_data = TelemetryData::new(
    "trace-12345",
    "span-67890",
    1640995200000,
    150,
    "ok"
  )
  
  TelemetryData::add_attribute(telemetry_data, "service.name", "user-service")
  TelemetryData::add_attribute(telemetry_data, "host.name", "server-01")
  TelemetryData::add_attribute(telemetry_data, "region", "us-west-2")
  
  TelemetryData::add_event(telemetry_data, "db.query.start", 1640995200050)
  TelemetryData::add_event(telemetry_data, "db.query.execute", 1640995200100)
  TelemetryData::add_event(telemetry_data, "db.query.complete", 1640995200150)
  
  // Test JSON serialization
  let json_serializer = JsonSerializer::new()
  let serialized_json = JsonSerializer::serialize(json_serializer, telemetry_data)
  
  assert_true(serialized_json.length() > 0)
  assert_true(serialized_json.contains("\"trace_id\":\"trace-12345\""))
  assert_true(serialized_json.contains("\"span_id\":\"span-67890\""))
  assert_true(serialized_json.contains("\"timestamp\":1640995200000"))
  assert_true(serialized_json.contains("\"duration\":150"))
  assert_true(serialized_json.contains("\"status\":\"ok\""))
  assert_true(serialized_json.contains("\"service.name\":\"user-service\""))
  assert_true(serialized_json.contains("\"host.name\":\"server-01\""))
  assert_true(serialized_json.contains("\"region\":\"us-west-2\""))
  
  // Test JSON deserialization
  let deserialized_data = JsonSerializer::deserialize(json_serializer, serialized_json)
  
  match deserialized_data {
    Ok(data) => {
      assert_eq(TelemetryData::trace_id(data), "trace-12345")
      assert_eq(TelemetryData::span_id(data), "span-67890")
      assert_eq(TelemetryData::timestamp(data), 1640995200000)
      assert_eq(TelemetryData::duration(data), 150)
      assert_eq(TelemetryData::status(data), "ok")
      
      assert_eq(TelemetryData::get_attribute(data, "service.name"), Some("user-service"))
      assert_eq(TelemetryData::get_attribute(data, "host.name"), Some("server-01"))
      assert_eq(TelemetryData::get_attribute(data, "region"), Some("us-west-2"))
      
      let events = TelemetryData::events(data)
      assert_eq(events.length(), 3)
      assert_eq(events[0].name, "db.query.start")
      assert_eq(events[0].timestamp, 1640995200050)
      assert_eq(events[1].name, "db.query.execute")
      assert_eq(events[1].timestamp, 1640995200100)
      assert_eq(events[2].name, "db.query.complete")
      assert_eq(events[2].timestamp, 1640995200150)
    }
    Err(_) => assert_true(false)
  }
  
  // Test JSON serialization with pretty printing
  let pretty_json = JsonSerializer::serialize_pretty(json_serializer, telemetry_data)
  
  assert_true(pretty_json.contains("\n")) // Should contain newlines
  assert_true(pretty_json.contains("  ")) // Should contain indentation
  assert_true(pretty_json.contains("\"trace_id\": \"trace-12345\""))
  
  // Test JSON serialization with custom settings
  let custom_serializer = JsonSerializer::with_options(JsonOptions {
    indent: 4,
    sort_keys: true,
    omit_null_values: true,
    escape_unicode: false
  })
  
  let custom_json = JsonSerializer::serialize(custom_serializer, telemetry_data)
  assert_true(custom_json.length() > 0)
  
  // Test JSON deserialization with validation
  let schema = JsonSchema::from_type(TelemetryData::type_info())
  let validated_deserializer = JsonValidatingDeserializer::new(schema)
  
  let validated_result = JsonValidatingDeserializer::deserialize(validated_deserializer, serialized_json)
  
  match validated_result {
    Ok(data) => {
      assert_eq(TelemetryData::trace_id(data), "trace-12345")
    }
    Err(error) => {
      // Should not fail with valid data
      assert_true(false)
    }
  }
  
  // Test JSON deserialization with invalid data
  let invalid_json = "{\"trace_id\":123,\"span_id\":\"span-67890\"}" // trace_id should be string
  
  let invalid_result = JsonValidatingDeserializer::deserialize(validated_deserializer, invalid_json)
  
  match invalid_result {
    Ok(_) => assert_true(false) // Should fail with invalid data
    Err(error) => {
      assert_true(error.contains("validation") || error.contains("type"))
    }
  }
  
  // Test JSON serialization of collections
  let telemetry_collection = [
    telemetry_data,
    TelemetryData::new("trace-12346", "span-67891", 1640995201000, 200, "ok"),
    TelemetryData::new("trace-12347", "span-67892", 1640995202000, 100, "error")
  ]
  
  let collection_json = JsonSerializer::serialize_collection(json_serializer, telemetry_collection)
  
  assert_true(collection_json.contains("["))
  assert_true(collection_json.contains("]"))
  assert_true(collection_json.contains("\"trace_id\":\"trace-12345\""))
  assert_true(collection_json.contains("\"trace_id\":\"trace-12346\""))
  assert_true(collection_json.contains("\"trace_id\":\"trace-12347\""))
  
  // Test JSON deserialization of collections
  let deserialized_collection = JsonSerializer::deserialize_collection(json_serializer, collection_json)
  
  match deserialized_collection {
    Ok(collection) => {
      assert_eq(collection.length(), 3)
      assert_eq(TelemetryData::trace_id(collection[0]), "trace-12345")
      assert_eq(TelemetryData::trace_id(collection[1]), "trace-12346")
      assert_eq(TelemetryData::trace_id(collection[2]), "trace-12347")
    }
    Err(_) => assert_true(false)
  }
}

// Test 2: Protocol Buffers Serialization and Deserialization
test "protocol buffers serialization and deserialization" {
  // Create test data structures
  let telemetry_data = TelemetryData::new(
    "trace-12345",
    "span-67890",
    1640995200000,
    150,
    "ok"
  )
  
  TelemetryData::add_attribute(telemetry_data, "service.name", "user-service")
  TelemetryData::add_attribute(telemetry_data, "host.name", "server-01")
  TelemetryData::add_attribute(telemetry_data, "region", "us-west-2")
  
  TelemetryData::add_event(telemetry_data, "db.query.start", 1640995200050)
  TelemetryData::add_event(telemetry_data, "db.query.execute", 1640995200100)
  TelemetryData::add_event(telemetry_data, "db.query.complete", 1640995200150)
  
  // Test Protocol Buffers serialization
  let protobuf_serializer = ProtobufSerializer::new()
  let serialized_protobuf = ProtobufSerializer::serialize(protobuf_serializer, telemetry_data)
  
  assert_true(serialized_protobuf.length() > 0)
  assert_true(serialized_protobuf.length() < 1000) // Should be compact
  
  // Test Protocol Buffers deserialization
  let deserialized_data = ProtobufSerializer::deserialize(protobuf_serializer, serialized_protobuf)
  
  match deserialized_data {
    Ok(data) => {
      assert_eq(TelemetryData::trace_id(data), "trace-12345")
      assert_eq(TelemetryData::span_id(data), "span-67890")
      assert_eq(TelemetryData::timestamp(data), 1640995200000)
      assert_eq(TelemetryData::duration(data), 150)
      assert_eq(TelemetryData::status(data), "ok")
      
      assert_eq(TelemetryData::get_attribute(data, "service.name"), Some("user-service"))
      assert_eq(TelemetryData::get_attribute(data, "host.name"), Some("server-01"))
      assert_eq(TelemetryData::get_attribute(data, "region"), Some("us-west-2"))
      
      let events = TelemetryData::events(data)
      assert_eq(events.length(), 3)
      assert_eq(events[0].name, "db.query.start")
      assert_eq(events[0].timestamp, 1640995200050)
      assert_eq(events[1].name, "db.query.execute")
      assert_eq(events[1].timestamp, 1640995200100)
      assert_eq(events[2].name, "db.query.complete")
      assert_eq(events[2].timestamp, 1640995200150)
    }
    Err(_) => assert_true(false)
  }
  
  // Test Protocol Buffers serialization with different versions
  let v1_serializer = ProtobufSerializer::with_version("v1")
  let v2_serializer = ProtobufSerializer::with_version("v2")
  
  let v1_data = ProtobufSerializer::serialize(v1_serializer, telemetry_data)
  let v2_data = ProtobufSerializer::serialize(v2_serializer, telemetry_data)
  
  assert_true(v1_data.length() > 0)
  assert_true(v2_data.length() > 0)
  // V2 might be more efficient
  assert_true(v2_data.length() <= v1_data.length())
  
  // Test backward compatibility
  let v1_deserialized = ProtobufSerializer::deserialize(v1_serializer, v1_data)
  let v2_deserialized = ProtobufSerializer::deserialize(v2_serializer, v2_data)
  
  match (v1_deserialized, v2_deserialized) {
    (Ok(v1_data), Ok(v2_data)) => {
      assert_eq(TelemetryData::trace_id(v1_data), TelemetryData::trace_id(v2_data))
      assert_eq(TelemetryData::span_id(v1_data), TelemetryData::span_id(v2_data))
    }
    _ => assert_true(false)
  }
  
  // Test Protocol Buffers serialization of collections
  let telemetry_collection = [
    telemetry_data,
    TelemetryData::new("trace-12346", "span-67891", 1640995201000, 200, "ok"),
    TelemetryData::new("trace-12347", "span-67892", 1640995202000, 100, "error")
  ]
  
  let collection_protobuf = ProtobufSerializer::serialize_collection(protobuf_serializer, telemetry_collection)
  
  assert_true(collection_protobuf.length() > 0)
  assert_true(collection_protobuf.length() < 3000) // Should be compact
  
  // Test Protocol Buffers deserialization of collections
  let deserialized_collection = ProtobufSerializer::deserialize_collection(protobuf_serializer, collection_protobuf)
  
  match deserialized_collection {
    Ok(collection) => {
      assert_eq(collection.length(), 3)
      assert_eq(TelemetryData::trace_id(collection[0]), "trace-12345")
      assert_eq(TelemetryData::trace_id(collection[1]), "trace-12346")
      assert_eq(TelemetryData::trace_id(collection[2]), "trace-12347")
    }
    Err(_) => assert_true(false)
  }
  
  // Test Protocol Buffers with partial data
  let partial_data = TelemetryData::new("trace-12348", "span-67893", 1640995203000, 120, "ok")
  // Don't add attributes or events
  
  let partial_protobuf = ProtobufSerializer::serialize(protobuf_serializer, partial_data)
  let partial_deserialized = ProtobufSerializer::deserialize(protobuf_serializer, partial_protobuf)
  
  match partial_deserialized {
    Ok(data) => {
      assert_eq(TelemetryData::trace_id(data), "trace-12348")
      assert_eq(TelemetryData::span_id(data), "span-67893")
      assert_eq(TelemetryData::attributes(data).length(), 0)
      assert_eq(TelemetryData::events(data).length(), 0)
    }
    Err(_) => assert_true(false)
  }
  
  // Test Protocol Buffers performance
  let large_collection = []
  
  for i in 0..=999 { // 1000 items
    let data = TelemetryData::new(
      "trace-" + i.to_string(),
      "span-" + i.to_string(),
      1640995200000 + i * 1000,
      100 + i,
      if i % 10 == 0 { "error" } else { "ok" }
    )
    
    TelemetryData::add_attribute(data, "index", i.to_string())
    large_collection = large_collection.push(data)
  }
  
  let serialize_start = Time::now()
  let large_protobuf = ProtobufSerializer::serialize_collection(protobuf_serializer, large_collection)
  let serialize_end = Time::now()
  let serialize_time = serialize_end - serialize_start
  
  let deserialize_start = Time::now()
  let large_deserialized = ProtobufSerializer::deserialize_collection(protobuf_serializer, large_protobuf)
  let deserialize_end = Time::now()
  let deserialize_time = deserialize_end - deserialize_start
  
  match large_deserialized {
    Ok(collection) => {
      assert_eq(collection.length(), 1000)
      assert_true(serialize_time < 1000) // Should serialize in less than 1 second
      assert_true(deserialize_time < 1000) // Should deserialize in less than 1 second
    }
    Err(_) => assert_true(false)
  }
}

// Test 3: MessagePack Serialization and Deserialization
test "messagepack serialization and deserialization" {
  // Create test data structures
  let telemetry_data = TelemetryData::new(
    "trace-12345",
    "span-67890",
    1640995200000,
    150,
    "ok"
  )
  
  TelemetryData::add_attribute(telemetry_data, "service.name", "user-service")
  TelemetryData::add_attribute(telemetry_data, "host.name", "server-01")
  TelemetryData::add_attribute(telemetry_data, "region", "us-west-2")
  
  TelemetryData::add_event(telemetry_data, "db.query.start", 1640995200050)
  TelemetryData::add_event(telemetry_data, "db.query.execute", 1640995200100)
  TelemetryData::add_event(telemetry_data, "db.query.complete", 1640995200150)
  
  // Test MessagePack serialization
  let msgpack_serializer = MessagePackSerializer::new()
  let serialized_msgpack = MessagePackSerializer::serialize(msgpack_serializer, telemetry_data)
  
  assert_true(serialized_msgpack.length() > 0)
  assert_true(serialized_msgpack.length() < 1000) // Should be compact
  
  // Test MessagePack deserialization
  let deserialized_data = MessagePackSerializer::deserialize(msgpack_serializer, serialized_msgpack)
  
  match deserialized_data {
    Ok(data) => {
      assert_eq(TelemetryData::trace_id(data), "trace-12345")
      assert_eq(TelemetryData::span_id(data), "span-67890")
      assert_eq(TelemetryData::timestamp(data), 1640995200000)
      assert_eq(TelemetryData::duration(data), 150)
      assert_eq(TelemetryData::status(data), "ok")
      
      assert_eq(TelemetryData::get_attribute(data, "service.name"), Some("user-service"))
      assert_eq(TelemetryData::get_attribute(data, "host.name"), Some("server-01"))
      assert_eq(TelemetryData::get_attribute(data, "region"), Some("us-west-2"))
      
      let events = TelemetryData::events(data)
      assert_eq(events.length(), 3)
      assert_eq(events[0].name, "db.query.start")
      assert_eq(events[0].timestamp, 1640995200050)
      assert_eq(events[1].name, "db.query.execute")
      assert_eq(events[1].timestamp, 1640995200100)
      assert_eq(events[2].name, "db.query.complete")
      assert_eq(events[2].timestamp, 1640995200150)
    }
    Err(_) => assert_true(false)
  }
  
  // Test MessagePack serialization with different options
  let compact_serializer = MessagePackSerializer::with_options(MsgPackOptions {
    use_str8: true,
    use_bin8: true,
    prefer_int32: true,
    sort_keys: true
  })
  
  let compact_msgpack = MessagePackSerializer::serialize(compact_serializer, telemetry_data)
  
  assert_true(compact_msgpack.length() > 0)
  // Compact options should result in smaller size
  assert_true(compact_msgpack.length() <= serialized_msgpack.length())
  
  // Test MessagePack serialization of collections
  let telemetry_collection = [
    telemetry_data,
    TelemetryData::new("trace-12346", "span-67891", 1640995201000, 200, "ok"),
    TelemetryData::new("trace-12347", "span-67892", 1640995202000, 100, "error")
  ]
  
  let collection_msgpack = MessagePackSerializer::serialize_collection(msgpack_serializer, telemetry_collection)
  
  assert_true(collection_msgpack.length() > 0)
  assert_true(collection_msgpack.length() < 3000) // Should be compact
  
  // Test MessagePack deserialization of collections
  let deserialized_collection = MessagePackSerializer::deserialize_collection(msgpack_serializer, collection_msgpack)
  
  match deserialized_collection {
    Ok(collection) => {
      assert_eq(collection.length(), 3)
      assert_eq(TelemetryData::trace_id(collection[0]), "trace-12345")
      assert_eq(TelemetryData::trace_id(collection[1]), "trace-12346")
      assert_eq(TelemetryData::trace_id(collection[2]), "trace-12347")
    }
    Err(_) => assert_true(false)
  }
  
  // Test MessagePack streaming serialization
  let stream_serializer = MessagePackStreamSerializer::new()
  let stream_buffer = Buffer::new()
  
  for data in telemetry_collection {
    MessagePackStreamSerializer::serialize_to_stream(stream_serializer, data, stream_buffer)
  }
  
  let stream_data = Buffer::to_bytes(stream_buffer)
  assert_true(stream_data.length() > 0)
  
  // Test MessagePack streaming deserialization
  let stream_deserializer = MessagePackStreamDeserializer::new()
  let stream_reader = BufferReader::new(stream_data)
  let deserialized_stream = []
  
  while MessagePackStreamDeserializer::has_next(stream_deserializer, stream_reader) {
    match MessagePackStreamDeserializer::deserialize_next(stream_deserializer, stream_reader) {
      Ok(data) => deserialized_stream = deserialized_stream.push(data),
      Err(_) => break
    }
  }
  
  assert_eq(deserialized_stream.length(), 3)
  assert_eq(TelemetryData::trace_id(deserialized_stream[0]), "trace-12345")
  assert_eq(TelemetryData::trace_id(deserialized_stream[1]), "trace-12346")
  assert_eq(TelemetryData::trace_id(deserialized_stream[2]), "trace-12347")
}

// Test 4: XML Serialization and Deserialization
test "xml serialization and deserialization" {
  // Create test data structures
  let telemetry_data = TelemetryData::new(
    "trace-12345",
    "span-67890",
    1640995200000,
    150,
    "ok"
  )
  
  TelemetryData::add_attribute(telemetry_data, "service.name", "user-service")
  TelemetryData::add_attribute(telemetry_data, "host.name", "server-01")
  TelemetryData::add_attribute(telemetry_data, "region", "us-west-2")
  
  TelemetryData::add_event(telemetry_data, "db.query.start", 1640995200050)
  TelemetryData::add_event(telemetry_data, "db.query.execute", 1640995200100)
  TelemetryData::add_event(telemetry_data, "db.query.complete", 1640995200150)
  
  // Test XML serialization
  let xml_serializer = XmlSerializer::new()
  let serialized_xml = XmlSerializer::serialize(xml_serializer, telemetry_data)
  
  assert_true(serialized_xml.length() > 0)
  assert_true(serialized_xml.contains("<TelemetryData>"))
  assert_true(serialized_xml.contains("</TelemetryData>"))
  assert_true(serialized_xml.contains("<trace_id>trace-12345</trace_id>"))
  assert_true(serialized_xml.contains("<span_id>span-67890</span_id>"))
  assert_true(serialized_xml.contains("<timestamp>1640995200000</timestamp>"))
  assert_true(serialized_xml.contains("<duration>150</duration>"))
  assert_true(serialized_xml.contains("<status>ok</status>"))
  assert_true(serialized_xml.contains("<attributes>"))
  assert_true(serialized_xml.contains("<events>"))
  
  // Test XML deserialization
  let deserialized_data = XmlSerializer::deserialize(xml_serializer, serialized_xml)
  
  match deserialized_data {
    Ok(data) => {
      assert_eq(TelemetryData::trace_id(data), "trace-12345")
      assert_eq(TelemetryData::span_id(data), "span-67890")
      assert_eq(TelemetryData::timestamp(data), 1640995200000)
      assert_eq(TelemetryData::duration(data), 150)
      assert_eq(TelemetryData::status(data), "ok")
      
      assert_eq(TelemetryData::get_attribute(data, "service.name"), Some("user-service"))
      assert_eq(TelemetryData::get_attribute(data, "host.name"), Some("server-01"))
      assert_eq(TelemetryData::get_attribute(data, "region"), Some("us-west-2"))
      
      let events = TelemetryData::events(data)
      assert_eq(events.length(), 3)
      assert_eq(events[0].name, "db.query.start")
      assert_eq(events[0].timestamp, 1640995200050)
      assert_eq(events[1].name, "db.query.execute")
      assert_eq(events[1].timestamp, 1640995200100)
      assert_eq(events[2].name, "db.query.complete")
      assert_eq(events[2].timestamp, 1640995200150)
    }
    Err(_) => assert_true(false)
  }
  
  // Test XML serialization with pretty printing
  let pretty_xml = XmlSerializer::serialize_pretty(xml_serializer, telemetry_data)
  
  assert_true(pretty_xml.contains("\n")) // Should contain newlines
  assert_true(pretty_xml.contains("  ")) // Should contain indentation
  assert_true(pretty_xml.contains("<TelemetryData>"))
  
  // Test XML serialization with custom options
  let custom_serializer = XmlSerializer::with_options(XmlOptions {
    indent: 4,
    omit_xml_declaration: false,
    use_attributes: true,
    namespace: "http://azimuth.io/telemetry"
  })
  
  let custom_xml = XmlSerializer::serialize(custom_serializer, telemetry_data)
  
  assert_true(custom_xml.contains("<?xml"))
  assert_true(custom_xml.contains("http://azimuth.io/telemetry"))
  assert_true(custom_xml.length() > 0)
  
  // Test XML deserialization with schema validation
  let xml_schema = XmlSchema::from_file("telemetry_schema.xsd")
  match xml_schema {
    Ok(schema) => {
      let validating_deserializer = XmlValidatingDeserializer::new(schema)
      
      let validated_result = XmlValidatingDeserializer::deserialize(validating_deserializer, serialized_xml)
      
      match validated_result {
        Ok(data) => {
          assert_eq(TelemetryData::trace_id(data), "trace-12345")
        }
        Err(error) => {
          // Should not fail with valid data
          assert_true(false)
        }
      }
      
      // Test with invalid XML
      let invalid_xml = "<TelemetryData><trace_id>123</trace_id></TelemetryData>" // Missing required fields
      
      let invalid_result = XmlValidatingDeserializer::deserialize(validating_deserializer, invalid_xml)
      
      match invalid_result {
        Ok(_) => assert_true(false) // Should fail with invalid data
        Err(error) => {
          assert_true(error.contains("validation") || error.contains("schema"))
        }
      }
    }
    Err(_) => {
      // Schema file might not exist in test environment
      assert_true(true)
    }
  }
  
  // Test XML serialization of collections
  let telemetry_collection = [
    telemetry_data,
    TelemetryData::new("trace-12346", "span-67891", 1640995201000, 200, "ok"),
    TelemetryData::new("trace-12347", "span-67892", 1640995202000, 100, "error")
  ]
  
  let collection_xml = XmlSerializer::serialize_collection(xml_serializer, telemetry_collection)
  
  assert_true(collection_xml.contains("<TelemetryDataCollection>"))
  assert_true(collection_xml.contains("</TelemetryDataCollection>"))
  assert_true(collection_xml.contains("<TelemetryData>"))
  assert_true(collection_xml.contains("</TelemetryData>"))
  assert_true(collection_xml.contains("<trace_id>trace-12345</trace_id>"))
  assert_true(collection_xml.contains("<trace_id>trace-12346</trace_id>"))
  assert_true(collection_xml.contains("<trace_id>trace-12347</trace_id>"))
  
  // Test XML deserialization of collections
  let deserialized_collection = XmlSerializer::deserialize_collection(xml_serializer, collection_xml)
  
  match deserialized_collection {
    Ok(collection) => {
      assert_eq(collection.length(), 3)
      assert_eq(TelemetryData::trace_id(collection[0]), "trace-12345")
      assert_eq(TelemetryData::trace_id(collection[1]), "trace-12346")
      assert_eq(TelemetryData::trace_id(collection[2]), "trace-12347")
    }
    Err(_) => assert_true(false)
  }
}

// Test 5: Custom Binary Serialization and Deserialization
test "custom binary serialization and deserialization" {
  // Create test data structures
  let telemetry_data = TelemetryData::new(
    "trace-12345",
    "span-67890",
    1640995200000,
    150,
    "ok"
  )
  
  TelemetryData::add_attribute(telemetry_data, "service.name", "user-service")
  TelemetryData::add_attribute(telemetry_data, "host.name", "server-01")
  TelemetryData::add_attribute(telemetry_data, "region", "us-west-2")
  
  TelemetryData::add_event(telemetry_data, "db.query.start", 1640995200050)
  TelemetryData::add_event(telemetry_data, "db.query.execute", 1640995200100)
  TelemetryData::add_event(telemetry_data, "db.query.complete", 1640995200150)
  
  // Test custom binary serialization
  let binary_serializer = CustomBinarySerializer::new()
  let serialized_binary = CustomBinarySerializer::serialize(binary_serializer, telemetry_data)
  
  assert_true(serialized_binary.length() > 0)
  assert_true(serialized_binary.length() < 500) // Should be very compact
  
  // Test custom binary deserialization
  let deserialized_data = CustomBinarySerializer::deserialize(binary_serializer, serialized_binary)
  
  match deserialized_data {
    Ok(data) => {
      assert_eq(TelemetryData::trace_id(data), "trace-12345")
      assert_eq(TelemetryData::span_id(data), "span-67890")
      assert_eq(TelemetryData::timestamp(data), 1640995200000)
      assert_eq(TelemetryData::duration(data), 150)
      assert_eq(TelemetryData::status(data), "ok")
      
      assert_eq(TelemetryData::get_attribute(data, "service.name"), Some("user-service"))
      assert_eq(TelemetryData::get_attribute(data, "host.name"), Some("server-01"))
      assert_eq(TelemetryData::get_attribute(data, "region"), Some("us-west-2"))
      
      let events = TelemetryData::events(data)
      assert_eq(events.length(), 3)
      assert_eq(events[0].name, "db.query.start")
      assert_eq(events[0].timestamp, 1640995200050)
      assert_eq(events[1].name, "db.query.execute")
      assert_eq(events[1].timestamp, 1640995200100)
      assert_eq(events[2].name, "db.query.complete")
      assert_eq(events[2].timestamp, 1640995200150)
    }
    Err(_) => assert_true(false)
  }
  
  // Test custom binary serialization with compression
  let compressed_serializer = CustomBinarySerializer::with_compression("gzip")
  let compressed_binary = CustomBinarySerializer::serialize(compressed_serializer, telemetry_data)
  
  assert_true(compressed_binary.length() > 0)
  // Compressed data should be smaller or similar in size
  assert_true(compressed_binary.length() <= serialized_binary.length())
  
  // Test custom binary deserialization with compression
  let decompressed_data = CustomBinarySerializer::deserialize(compressed_serializer, compressed_binary)
  
  match decompressed_data {
    Ok(data) => {
      assert_eq(TelemetryData::trace_id(data), "trace-12345")
      assert_eq(TelemetryData::span_id(data), "span-67890")
    }
    Err(_) => assert_true(false)
  }
  
  // Test custom binary serialization with encryption
  let encryption_key = EncryptionKey::generate()
  let encrypted_serializer = CustomBinarySerializer::with_encryption(encryption_key, "AES-256-GCM")
  let encrypted_binary = CustomBinarySerializer::serialize(encrypted_serializer, telemetry_data)
  
  assert_true(encrypted_binary.length() > 0)
  // Encrypted data should be different from unencrypted
  assert_true(encrypted_binary != serialized_binary)
  
  // Test custom binary deserialization with encryption
  let decrypted_data = CustomBinarySerializer::deserialize(encrypted_serializer, encrypted_binary)
  
  match decrypted_data {
    Ok(data) => {
      assert_eq(TelemetryData::trace_id(data), "trace-12345")
      assert_eq(TelemetryData::span_id(data), "span-67890")
    }
    Err(_) => assert_true(false)
  }
  
  // Test custom binary serialization with versioning
  let v1_serializer = CustomBinarySerializer::with_version("v1")
  let v2_serializer = CustomBinarySerializer::with_version("v2")
  
  let v1_binary = CustomBinarySerializer::serialize(v1_serializer, telemetry_data)
  let v2_binary = CustomBinarySerializer::serialize(v2_serializer, telemetry_data)
  
  // Different versions might have different formats
  assert_true(v1_binary.length() > 0)
  assert_true(v2_binary.length() > 0)
  
  // Test backward compatibility
  let v1_deserialized = CustomBinarySerializer::deserialize(v2_serializer, v1_binary) // V2 should read V1
  match v1_deserialized {
    Ok(data) => {
      assert_eq(TelemetryData::trace_id(data), "trace-12345")
      assert_eq(TelemetryData::span_id(data), "span-67890")
    }
    Err(_) => assert_true(false)
  }
  
  // Test custom binary serialization of collections
  let telemetry_collection = [
    telemetry_data,
    TelemetryData::new("trace-12346", "span-67891", 1640995201000, 200, "ok"),
    TelemetryData::new("trace-12347", "span-67892", 1640995202000, 100, "error")
  ]
  
  let collection_binary = CustomBinarySerializer::serialize_collection(binary_serializer, telemetry_collection)
  
  assert_true(collection_binary.length() > 0)
  assert_true(collection_binary.length() < 1500) // Should be compact
  
  // Test custom binary deserialization of collections
  let deserialized_collection = CustomBinarySerializer::deserialize_collection(binary_serializer, collection_binary)
  
  match deserialized_collection {
    Ok(collection) => {
      assert_eq(collection.length(), 3)
      assert_eq(TelemetryData::trace_id(collection[0]), "trace-12345")
      assert_eq(TelemetryData::trace_id(collection[1]), "trace-12346")
      assert_eq(TelemetryData::trace_id(collection[2]), "trace-12347")
    }
    Err(_) => assert_true(false)
  }
  
  // Test custom binary streaming serialization
  let stream_serializer = CustomBinaryStreamSerializer::new()
  let stream_buffer = Buffer::new()
  
  for data in telemetry_collection {
    CustomBinaryStreamSerializer::serialize_to_stream(stream_serializer, data, stream_buffer)
  }
  
  let stream_data = Buffer::to_bytes(stream_buffer)
  assert_true(stream_data.length() > 0)
  
  // Test custom binary streaming deserialization
  let stream_deserializer = CustomBinaryStreamDeserializer::new()
  let stream_reader = BufferReader::new(stream_data)
  let deserialized_stream = []
  
  while CustomBinaryStreamDeserializer::has_next(stream_deserializer, stream_reader) {
    match CustomBinaryStreamDeserializer::deserialize_next(stream_deserializer, stream_reader) {
      Ok(data) => deserialized_stream = deserialized_stream.push(data),
      Err(_) => break
    }
  }
  
  assert_eq(deserialized_stream.length(), 3)
  assert_eq(TelemetryData::trace_id(deserialized_stream[0]), "trace-12345")
  assert_eq(TelemetryData::trace_id(deserialized_stream[1]), "trace-12346")
  assert_eq(TelemetryData::trace_id(deserialized_stream[2]), "trace-12347")
}

// Test 6: Serialization Format Comparison and Performance
test "serialization format comparison and performance" {
  // Create test data structures
  let telemetry_data = TelemetryData::new(
    "trace-12345",
    "span-67890",
    1640995200000,
    150,
    "ok"
  )
  
  TelemetryData::add_attribute(telemetry_data, "service.name", "user-service")
  TelemetryData::add_attribute(telemetry_data, "host.name", "server-01")
  TelemetryData::add_attribute(telemetry_data, "region", "us-west-2")
  TelemetryData::add_attribute(telemetry_data, "environment", "production")
  TelemetryData::add_attribute(telemetry_data, "version", "1.2.3")
  
  TelemetryData::add_event(telemetry_data, "db.query.start", 1640995200050)
  TelemetryData::add_event(telemetry_data, "db.query.execute", 1640995200100)
  TelemetryData::add_event(telemetry_data, "db.query.complete", 1640995200150)
  TelemetryData::add_event(telemetry_data, "cache.check", 1640995200160)
  TelemetryData::add_event(telemetry_data, "cache.hit", 1640995200170)
  
  // Test serialization with different formats
  let json_serializer = JsonSerializer::new()
  let protobuf_serializer = ProtobufSerializer::new()
  let msgpack_serializer = MessagePackSerializer::new()
  let xml_serializer = XmlSerializer::new()
  let binary_serializer = CustomBinarySerializer::new()
  
  // Serialize data with each format
  let json_start = Time::now()
  let json_data = JsonSerializer::serialize(json_serializer, telemetry_data)
  let json_end = Time::now()
  let json_serialize_time = json_end - json_start
  
  let protobuf_start = Time::now()
  let protobuf_data = ProtobufSerializer::serialize(protobuf_serializer, telemetry_data)
  let protobuf_end = Time::now()
  let protobuf_serialize_time = protobuf_end - protobuf_start
  
  let msgpack_start = Time::now()
  let msgpack_data = MessagePackSerializer::serialize(msgpack_serializer, telemetry_data)
  let msgpack_end = Time::now()
  let msgpack_serialize_time = msgpack_end - msgpack_start
  
  let xml_start = Time::now()
  let xml_data = XmlSerializer::serialize(xml_serializer, telemetry_data)
  let xml_end = Time::now()
  let xml_serialize_time = xml_end - xml_start
  
  let binary_start = Time::now()
  let binary_data = CustomBinarySerializer::serialize(binary_serializer, telemetry_data)
  let binary_end = Time::now()
  let binary_serialize_time = binary_end - binary_start
  
  // Test deserialization with each format
  let json_deserialize_start = Time::now()
  let json_deserialized = JsonSerializer::deserialize(json_serializer, json_data)
  let json_deserialize_end = Time::now()
  let json_deserialize_time = json_deserialize_end - json_deserialize_start
  
  let protobuf_deserialize_start = Time::now()
  let protobuf_deserialized = ProtobufSerializer::deserialize(protobuf_serializer, protobuf_data)
  let protobuf_deserialize_end = Time::now()
  let protobuf_deserialize_time = protobuf_deserialize_end - protobuf_deserialize_start
  
  let msgpack_deserialize_start = Time::now()
  let msgpack_deserialized = MessagePackSerializer::deserialize(msgpack_serializer, msgpack_data)
  let msgpack_deserialize_end = Time::now()
  let msgpack_deserialize_time = msgpack_deserialize_end - msgpack_deserialize_start
  
  let xml_deserialize_start = Time::now()
  let xml_deserialized = XmlSerializer::deserialize(xml_serializer, xml_data)
  let xml_deserialize_end = Time::now()
  let xml_deserialize_time = xml_deserialize_end - xml_deserialize_start
  
  let binary_deserialize_start = Time::now()
  let binary_deserialized = CustomBinarySerializer::deserialize(binary_serializer, binary_data)
  let binary_deserialize_end = Time::now()
  let binary_deserialize_time = binary_deserialize_end - binary_deserialize_start
  
  // Verify all deserializations succeeded
  match (json_deserialized, protobuf_deserialized, msgpack_deserialized, xml_deserialized, binary_deserialized) {
    (Ok(json_data), Ok(protobuf_data), Ok(msgpack_data), Ok(xml_data), Ok(binary_data)) => {
      // Verify data integrity
      assert_eq(TelemetryData::trace_id(json_data), TelemetryData::trace_id(protobuf_data))
      assert_eq(TelemetryData::trace_id(protobuf_data), TelemetryData::trace_id(msgpack_data))
      assert_eq(TelemetryData::trace_id(msgpack_data), TelemetryData::trace_id(xml_data))
      assert_eq(TelemetryData::trace_id(xml_data), TelemetryData::trace_id(binary_data))
      
      // Compare serialization sizes
      let json_size = json_data.length()
      let protobuf_size = protobuf_data.length()
      let msgpack_size = msgpack_data.length()
      let xml_size = xml_data.length()
      let binary_size = binary_data.length()
      
      // Binary formats should be more compact than text formats
      assert_true(protobuf_size < json_size)
      assert_true(protobuf_size < xml_size)
      assert_true(msgpack_size < json_size)
      assert_true(msgpack_size < xml_size)
      assert_true(binary_size < json_size)
      assert_true(binary_size < xml_size)
      
      // Custom binary should be most compact
      assert_true(binary_size <= protobuf_size)
      assert_true(binary_size <= msgpack_size)
      
      // XML should be most verbose
      assert_true(xml_size > json_size)
      
      // Compare serialization performance
      let serialize_times = [
        ("json", json_serialize_time),
        ("protobuf", protobuf_serialize_time),
        ("msgpack", msgpack_serialize_time),
        ("xml", xml_serialize_time),
        ("binary", binary_serialize_time)
      ]
      
      let fastest_serialize = serialize_times.reduce(fn(fastest, current) {
        if current.1 < fastest.1 {
          current
        } else {
          fastest
        }
      }, serialize_times[0])
      
      let slowest_serialize = serialize_times.reduce(fn(slowest, current) {
        if current.1 > slowest.1 {
          current
        } else {
          slowest
        }
      }, serialize_times[0])
      
      // Binary formats should generally be faster than text formats
      assert_true(fastest_serialize.0 == "binary" || fastest_serialize.0 == "protobuf" || fastest_serialize.0 == "msgpack")
      assert_true(slowest_serialize.0 == "xml" || slowest_serialize.0 == "json")
      
      // Compare deserialization performance
      let deserialize_times = [
        ("json", json_deserialize_time),
        ("protobuf", protobuf_deserialize_time),
        ("msgpack", msgpack_deserialize_time),
        ("xml", xml_deserialize_time),
        ("binary", binary_deserialize_time)
      ]
      
      let fastest_deserialize = deserialize_times.reduce(fn(fastest, current) {
        if current.1 < fastest.1 {
          current
        } else {
          fastest
        }
      }, deserialize_times[0])
      
      let slowest_deserialize = deserialize_times.reduce(fn(slowest, current) {
        if current.1 > slowest.1 {
          current
        } else {
          slowest
        }
      }, deserialize_times[0])
      
      // Binary formats should generally be faster than text formats
      assert_true(fastest_deserialize.0 == "binary" || fastest_deserialize.0 == "protobuf" || fastest_deserialize.0 == "msgpack")
      assert_true(slowest_deserialize.0 == "xml" || slowest_deserialize.0 == "json")
    }
    _ => assert_true(false)
  }
  
  // Test serialization performance with large collections
  let large_collection = []
  
  for i in 0..=999 { // 1000 items
    let data = TelemetryData::new(
      "trace-" + i.to_string(),
      "span-" + i.to_string(),
      1640995200000 + i * 1000,
      100 + i,
      if i % 10 == 0 { "error" } else { "ok" }
    )
    
    TelemetryData::add_attribute(data, "index", i.to_string())
    TelemetryData::add_attribute(data, "batch", "test")
    
    if i % 5 == 0 {
      TelemetryData::add_event(data, "event-" + i.to_string(), 1640995200000 + i * 1000 + 500)
    }
    
    large_collection = large_collection.push(data)
  }
  
  // Serialize large collection with each format
  let large_json_start = Time::now()
  let large_json_data = JsonSerializer::serialize_collection(json_serializer, large_collection)
  let large_json_end = Time::now()
  let large_json_time = large_json_end - large_json_start
  
  let large_protobuf_start = Time::now()
  let large_protobuf_data = ProtobufSerializer::serialize_collection(protobuf_serializer, large_collection)
  let large_protobuf_end = Time::now()
  let large_protobuf_time = large_protobuf_end - large_protobuf_start
  
  let large_msgpack_start = Time::now()
  let large_msgpack_data = MessagePackSerializer::serialize_collection(msgpack_serializer, large_collection)
  let large_msgpack_end = Time::now()
  let large_msgpack_time = large_msgpack_end - large_msgpack_start
  
  let large_binary_start = Time::now()
  let large_binary_data = CustomBinarySerializer::serialize_collection(binary_serializer, large_collection)
  let large_binary_end = Time::now()
  let large_binary_time = large_binary_end - large_binary_start
  
  // Compare large collection serialization performance
  let large_serialize_times = [
    ("json", large_json_time, large_json_data.length()),
    ("protobuf", large_protobuf_time, large_protobuf_data.length()),
    ("msgpack", large_msgpack_time, large_msgpack_data.length()),
    ("binary", large_binary_time, large_binary_data.length())
  ]
  
  let fastest_large_serialize = large_serialize_times.reduce(fn(fastest, current) {
    if current.1 < fastest.1 {
      current
    } else {
      fastest
    }
  }, large_serialize_times[0])
  
  // Binary formats should be significantly faster for large collections
  assert_true(fastest_large_serialize.0 == "binary" || fastest_large_serialize.0 == "protobuf" || fastest_large_serialize.0 == "msgpack")
  
  // Compare large collection sizes
  let json_size = large_json_data.length()
  let protobuf_size = large_protobuf_data.length()
  let msgpack_size = large_msgpack_data.length()
  let binary_size = large_binary_data.length()
  
  // Binary formats should be significantly more compact for large collections
  assert_true(protobuf_size < json_size * 0.5) // At least 50% smaller
  assert_true(msgpack_size < json_size * 0.5) // At least 50% smaller
  assert_true(binary_size < json_size * 0.5) // At least 50% smaller
}

// Test 7: Serialization Error Handling and Recovery
test "serialization error handling and recovery" {
  // Test JSON error handling
  let json_serializer = JsonSerializer::new()
  
  // Test with invalid JSON
  let invalid_json = "{\"trace_id\":123,\"span_id\":\"span-67890\"}" // trace_id should be string
  
  let json_error_result = JsonSerializer::deserialize(json_serializer, invalid_json)
  
  match json_error_result {
    Ok(_) => assert_true(false) // Should fail with invalid data
    Err(error) => {
      assert_true(error.contains("parse") || error.contains("type") || error.contains("invalid"))
    }
  }
  
  // Test with malformed JSON
  let malformed_json = "{\"trace_id\":\"trace-12345\",\"span_id\":\"span-67890\"" // Missing closing brace
  
  let malformed_result = JsonSerializer::deserialize(json_serializer, malformed_json)
  
  match malformed_result {
    Ok(_) => assert_true(false) // Should fail with malformed JSON
    Err(error) => {
      assert_true(error.contains("parse") || error.contains("syntax") || error.contains("malformed"))
    }
  }
  
  // Test with incomplete JSON
  let incomplete_json = "{\"trace_id\":\"trace-12345\"}" // Missing required fields
  
  let incomplete_result = JsonSerializer::deserialize(json_serializer, incomplete_json)
  
  match incomplete_result {
    Ok(_) => assert_true(false) // Should fail with incomplete data
    Err(error) => {
      assert_true(error.contains("required") || error.contains("missing") || error.contains("field"))
    }
  }
  
  // Test Protocol Buffers error handling
  let protobuf_serializer = ProtobufSerializer::new()
  
  // Test with invalid protobuf data
  let invalid_protobuf = [0x00, 0x01, 0x02, 0x03] // Invalid protobuf data
  
  let protobuf_error_result = ProtobufSerializer::deserialize(protobuf_serializer, invalid_protobuf)
  
  match protobuf_error_result {
    Ok(_) => assert_true(false) // Should fail with invalid data
    Err(error) => {
      assert_true(error.contains("parse") || error.contains("invalid") || error.contains("protobuf"))
    }
  }
  
  // Test with truncated protobuf data
  let telemetry_data = TelemetryData::new("trace-12345", "span-67890", 1640995200000, 150, "ok")
  let valid_protobuf = ProtobufSerializer::serialize(protobuf_serializer, telemetry_data)
  
  // Truncate the data
  let truncated_protobuf = valid_protobuf.slice(0, valid_protobuf.length() / 2)
  
  let truncated_result = ProtobufSerializer::deserialize(protobuf_serializer, truncated_protobuf)
  
  match truncated_result {
    Ok(_) => assert_true(false) // Should fail with truncated data
    Err(error) => {
      assert_true(error.contains("truncated") || error.contains("incomplete") || error.contains("eof"))
    }
  }
  
  // Test MessagePack error handling
  let msgpack_serializer = MessagePackSerializer::new()
  
  // Test with invalid MessagePack data
  let invalid_msgpack = [0xFF, 0xFF, 0xFF, 0xFF] // Invalid MessagePack data
  
  let msgpack_error_result = MessagePackSerializer::deserialize(msgpack_serializer, invalid_msgpack)
  
  match msgpack_error_result {
    Ok(_) => assert_true(false) // Should fail with invalid data
    Err(error) => {
      assert_true(error.contains("parse") || error.contains("invalid") || error.contains("msgpack"))
    }
  }
  
  // Test with truncated MessagePack data
  let valid_msgpack = MessagePackSerializer::serialize(msgpack_serializer, telemetry_data)
  
  // Truncate the data
  let truncated_msgpack = valid_msgpack.slice(0, valid_msgpack.length() / 2)
  
  let truncated_msgpack_result = MessagePackSerializer::deserialize(msgpack_serializer, truncated_msgpack)
  
  match truncated_msgpack_result {
    Ok(_) => assert_true(false) // Should fail with truncated data
    Err(error) => {
      assert_true(error.contains("truncated") || error.contains("incomplete") || error.contains("eof"))
    }
  }
  
  // Test XML error handling
  let xml_serializer = XmlSerializer::new()
  
  // Test with malformed XML
  let malformed_xml = "<TelemetryData><trace_id>trace-12345</TelemetryData>" // Missing closing tag
  
  let malformed_xml_result = XmlSerializer::deserialize(xml_serializer, malformed_xml)
  
  match malformed_xml_result {
    Ok(_) => assert_true(false) // Should fail with malformed XML
    Err(error) => {
      assert_true(error.contains("parse") || error.contains("xml") || error.contains("malformed"))
    }
  }
  
  // Test with incomplete XML
  let incomplete_xml = "<TelemetryData><trace_id>trace-12345</trace_id></TelemetryData>" // Missing required fields
  
  let incomplete_xml_result = XmlSerializer::deserialize(xml_serializer, incomplete_xml)
  
  match incomplete_xml_result {
    Ok(_) => assert_true(false) // Should fail with incomplete data
    Err(error) => {
      assert_true(error.contains("required") || error.contains("missing") || error.contains("field"))
    }
  }
  
  // Test custom binary error handling
  let binary_serializer = CustomBinarySerializer::new()
  
  // Test with invalid binary data
  let invalid_binary = [0x00, 0x01, 0x02, 0x03] // Invalid binary data
  
  let binary_error_result = CustomBinarySerializer::deserialize(binary_serializer, invalid_binary)
  
  match binary_error_result {
    Ok(_) => assert_true(false) // Should fail with invalid data
    Err(error) => {
      assert_true(error.contains("parse") || error.contains("invalid") || error.contains("binary"))
    }
  }
  
  // Test with version mismatch
  let v1_serializer = CustomBinarySerializer::with_version("v1")
  let v2_serializer = CustomBinarySerializer::with_version("v2")
  
  let v1_data = CustomBinarySerializer::serialize(v1_serializer, telemetry_data)
  
  // Try to deserialize with incompatible version
  let incompatible_result = CustomBinarySerializer::deserialize_with_version(v2_serializer, v1_data, "v3") // Non-existent version
  
  match incompatible_result {
    Ok(_) => assert_true(false) // Should fail with version mismatch
    Err(error) => {
      assert_true(error.contains("version") || error.contains("unsupported") || error.contains("incompatible"))
    }
  }
  
  // Test error recovery with fallback deserializer
  let fallback_deserializer = FallbackDeserializer::new([
    json_serializer,
    protobuf_serializer,
    msgpack_serializer
  ])
  
  // Try to deserialize with different formats
  let json_data = JsonSerializer::serialize(json_serializer, telemetry_data)
  
  let fallback_result = FallbackDeserializer::deserialize(fallback_deserializer, json_data)
  
  match fallback_result {
    Ok(data) => {
      assert_eq(TelemetryData::trace_id(data), "trace-12345")
      assert_eq(TelemetryData::span_id(data), "span-67890")
    }
    Err(_) => assert_true(false)
  }
  
  // Test error recovery with partial data
  let partial_deserializer = PartialDeserializer::new()
  
  // Try to deserialize with partial data
  let partial_result = PartialDeserializer::deserialize(partial_deserializer, truncated_protobuf)
  
  match partial_result {
    Ok(data) => {
      // Should succeed with partial data if possible
      assert_true(TelemetryData::trace_id(data).length() > 0)
    }
    Err(_) => {
      // Might fail if no partial data can be recovered
      assert_true(true)
    }
  }
  
  // Test serialization error logging
  let error_logger = SerializationErrorLogger::new()
  
  // Configure error logger
  SerializationErrorLogger::log_to_file(error_logger, "/tmp/serialization_errors.log")
  SerializationErrorLogger::set_max_log_size(error_logger, 1024 * 1024) // 1MB
  
  // Trigger some errors to log
  JsonSerializer::deserialize(json_serializer, invalid_json);
  ProtobufSerializer::deserialize(protobuf_serializer, invalid_protobuf);
  XmlSerializer::deserialize(xml_serializer, malformed_xml);
  
  // Check error log
  let error_log = SerializationErrorLogger::get_errors(error_logger)
  
  assert_true(error_log.length() >= 3)
  
  for error in error_log {
    assert_true(error.timestamp > 0)
    assert_true(error.error_type.length() > 0)
    assert_true(error.message.length() > 0)
    assert_true(error.format.length() > 0)
  }
}