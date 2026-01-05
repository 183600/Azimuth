// Azimuth Telemetry System - Serialization and Deserialization Quality Tests
// This file contains comprehensive test cases for data serialization, deserialization, and format conversion

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  let test_object = TestTelemetryData::new(
    "test_span",
    "123e4567-e89b-12d3-a456-426614174000",
    1234567890L,
    [
      ("cpu_usage", 75.5),
      ("memory_usage", 60.2),
      ("disk_io", 120.8)
    ],
    [
      ("service.name", "test_service"),
      ("service.version", "1.0.0")
    ]
  )
  
  // Test JSON serialization
  let json_data = JsonSerializer::serialize(test_object)
  assert_true(json_data.length() > 0)
  assert_true(json_data.contains("\"span_name\":\"test_span\""))
  assert_true(json_data.contains("\"trace_id\":\"123e4567-e89b-12d3-a456-426614174000\""))
  assert_true(json_data.contains("\"cpu_usage\":75.5"))
  
  // Test JSON deserialization
  let deserialized_object = JsonSerializer::deserialize(json_data)
  assert_true(TestTelemetryData::equals(test_object, deserialized_object))
  
  // Test with nested objects
  let nested_object = NestedTelemetryData::new(
    test_object,
    [
      TestTelemetryData::new("child_span1", "trace1", 1234567891L, [], []),
      TestTelemetryData::new("child_span2", "trace2", 1234567892L, [], [])
    ]
  )
  
  let nested_json = JsonSerializer::serialize(nested_object)
  let deserialized_nested = JsonSerializer::deserialize(nested_json)
  assert_true(NestedTelemetryData::equals(nested_object, deserialized_nested))
}

// Test 2: Binary Serialization for Performance
test "binary serialization for performance" {
  let test_object = TestTelemetryData::new(
    "performance_test",
    "456e7890-e12b-23d4-b567-537715185111",
    1234567890L,
    [
      ("response_time", 150.5),
      ("throughput", 1000.0),
      ("error_rate", 0.05)
    ],
    [
      ("host.name", "server1"),
      ("environment", "production")
    ]
  )
  
  // Test binary serialization
  let binary_data = BinarySerializer::serialize(test_object)
  assert_true(binary_data.length() > 0)
  
  // Test binary deserialization
  let deserialized_object = BinarySerializer::deserialize(binary_data)
  assert_true(TestTelemetryData::equals(test_object, deserialized_object))
  
  // Compare binary vs JSON size
  let json_data = JsonSerializer::serialize(test_object)
  assert_true(binary_data.length() < json_data.length())
  
  // Test binary serialization performance
  let start_time = PerformanceTimer::current_time()
  for i in 0..=999 {
    let temp_binary = BinarySerializer::serialize(test_object)
    let temp_deserialized = BinarySerializer::deserialize(temp_binary)
  }
  let binary_time = PerformanceTimer::current_time() - start_time
  
  start_time = PerformanceTimer::current_time()
  for i in 0..=999 {
    let temp_json = JsonSerializer::serialize(test_object)
    let temp_deserialized = JsonSerializer::deserialize(temp_json)
  }
  let json_time = PerformanceTimer::current_time() - start_time
  
  // Binary serialization should be faster
  assert_true(binary_time < json_time)
}

// Test 3: Protocol Buffers Serialization
test "protocol buffers serialization" {
  let test_object = TestTelemetryData::new(
    "protobuf_test",
    "789e0123-f34c-34e5-c678-648826296222",
    1234567890L,
    [
      ("latency", 25.3),
      ("packet_loss", 0.001)
    ],
    [
      ("datacenter", "us-east-1"),
      ("region", "east")
    ]
  )
  
  // Test Protocol Buffers serialization
  let proto_data = ProtobufSerializer::serialize(test_object)
  assert_true(proto_data.length() > 0)
  
  // Test Protocol Buffers deserialization
  let deserialized_object = ProtobufSerializer::deserialize(proto_data)
  assert_true(TestTelemetryData::equals(test_object, deserialized_object))
  
  // Test backward compatibility
  let versioned_object = VersionedTelemetryData::new(
    test_object,
    "1.2.3"
  )
  
  let versioned_proto = ProtobufSerializer::serialize(versioned_object)
  let deserialized_versioned = ProtobufSerializer::deserialize(versioned_proto)
  assert_true(VersionedTelemetryData::equals(versioned_object, deserialized_versioned))
  
  // Test forward compatibility with unknown fields
  let extended_proto_data = ProtobufSerializer::add_unknown_fields(proto_data, [
    ("future_field", "future_value")
  ])
  
  let compatible_object = ProtobufSerializer::deserialize_with_unknown_fields(extended_proto_data)
  assert_true(TestTelemetryData::equals(test_object, compatible_object.data))
}

// Test 4: XML Serialization and Deserialization
test "xml serialization and deserialization" {
  let test_object = TestTelemetryData::new(
    "xml_test_span",
    "012f3456-g45d-45f6-d789-759937307333",
    1234567890L,
    [
      ("cpu_temp", 65.5),
      ("fan_speed", 2500.0)
    ],
    [
      ("device.type", "server"),
      ("location", "rack-1")
    ]
  )
  
  // Test XML serialization
  let xml_data = XmlSerializer::serialize(test_object)
  assert_true(xml_data.length() > 0)
  assert_true(xml_data.contains("<TelemetryData>"))
  assert_true(xml_data.contains("<span_name>xml_test_span</span_name>"))
  assert_true(xml_data.contains("<metrics>"))
  assert_true(xml_data.contains("<cpu_temp>65.5</cpu_temp>"))
  assert_true(xml_data.contains("</TelemetryData>"))
  
  // Test XML deserialization
  let deserialized_object = XmlSerializer::deserialize(xml_data)
  assert_true(TestTelemetryData::equals(test_object, deserialized_object))
  
  // Test XML with namespaces
  let namespaced_object = NamespacedTelemetryData::new(
    test_object,
    "http://example.com/telemetry",
    "tel"
  )
  
  let namespaced_xml = XmlSerializer::serialize_with_namespace(namespaced_object)
  assert_true(namespaced_xml.contains("xmlns:tel=\"http://example.com/telemetry\""))
  
  let deserialized_namespaced = XmlSerializer::deserialize_with_namespace(namespaced_xml)
  assert_true(NamespacedTelemetryData::equals(namespaced_object, deserialized_namespaced))
}

// Test 5: Custom Serialization Format
test "custom serialization format" {
  let test_object = TestTelemetryData::new(
    "custom_format_test",
    "345g6789-h56e-56g7-e890-860148418444",
    1234567890L,
    [
      ("bandwidth", 1000.0),
      ("jitter", 2.5)
    ],
    [
      ("protocol", "tcp"),
      ("port", "8080")
    ]
  )
  
  // Test custom serialization
  let custom_serializer = CustomSerializer::new()
  let custom_data = CustomSerializer::serialize(custom_serializer, test_object)
  assert_true(custom_data.length() > 0)
  
  // Test custom deserialization
  let deserialized_object = CustomSerializer::deserialize(custom_serializer, custom_data)
  assert_true(TestTelemetryData::equals(test_object, deserialized_object))
  
  // Test custom format with compression
  let compressed_data = CustomSerializer::serialize_with_compression(custom_serializer, test_object)
  let decompressed_object = CustomSerializer::deserialize_with_compression(custom_serializer, compressed_data)
  assert_true(TestTelemetryData::equals(test_object, decompressed_object))
  
  // Custom format with compression should be smaller
  assert_true(compressed_data.length() < custom_data.length())
}

// Test 6: Serialization with Encryption
test "serialization with encryption" {
  let test_object = TestTelemetryData::new(
    "encrypted_test",
    "456h7890-i67f-67h8-f901-971259529555",
    1234567890L,
    [
      ("sensitive_metric", 42.0)
    ],
    [
      ("sensitive_attribute", "secret_value")
    ]
  )
  
  // Test encryption key
  let encryption_key = EncryptionKey::generate()
  assert_true(EncryptionKey::is_valid(encryption_key))
  
  // Test encrypted serialization
  let encrypted_serializer = EncryptedSerializer::new(encryption_key)
  let encrypted_data = EncryptedSerializer::serialize(encrypted_serializer, test_object)
  assert_true(encrypted_data.length() > 0)
  
  // Encrypted data should not contain plain text
  assert_false(encrypted_data.contains("encrypted_test"))
  assert_false(encrypted_data.contains("secret_value"))
  
  // Test encrypted deserialization
  let deserialized_object = EncryptedSerializer::deserialize(encrypted_serializer, encrypted_data)
  assert_true(TestTelemetryData::equals(test_object, deserialized_object))
  
  // Test with wrong key (should fail)
  let wrong_key = EncryptionKey::generate()
  let wrong_serializer = EncryptedSerializer::new(wrong_key)
  let decryption_result = EncryptedSerializer::try_deserialize(wrong_serializer, encrypted_data)
  assert_true(decryption_result.is_error)
}

// Test 7: Streaming Serialization for Large Data
test "streaming serialization for large data" {
  // Create large telemetry dataset
  let large_dataset = []
  for i in 0..=9999 {  // 10,000 records
    let telemetry_data = TestTelemetryData::new(
      "large_data_span_" + i.to_string(),
      "567i8901-j78g-78h9-g012-082360630666",
      1234567890L + i,
      [
        ("metric_" + i.to_string(), i.to_float())
      ],
      [
        ("attribute_" + i.to_string(), "value_" + i.to_string())
      ]
    )
    large_dataset.push(telemetry_data)
  }
  
  // Test streaming serialization
  let streaming_serializer = StreamingSerializer::new()
  let stream = StreamingSerializer::serialize_stream(streaming_serializer, large_dataset)
  
  // Test streaming deserialization
  let deserialized_dataset = StreamingSerializer::deserialize_stream(streaming_serializer, stream)
  assert_eq(deserialized_dataset.length(), large_dataset.length())
  
  // Verify first and last elements
  assert_true(TestTelemetryData::equals(large_dataset[0], deserialized_dataset[0]))
  assert_true(TestTelemetryData::equals(large_dataset[9999], deserialized_dataset[9999]))
  
  // Test memory efficiency of streaming
  let memory_before = MemoryProfiler::get_memory_usage()
  
  // Stream without loading all data into memory
  let stream_processor = StreamingSerializer::create_processor(streaming_serializer, stream, fn(item) {
    // Process each item individually
    return TestTelemetryData::get_span_name(item).length()
  })
  
  let processed_count = 0
  while StreamProcessor::has_next(stream_processor) {
    StreamProcessor::process_next(stream_processor)
    processed_count = processed_count + 1
  }
  
  assert_eq(processed_count, 10000)
  
  let memory_after = MemoryProfiler::get_memory_usage()
  // Memory usage should not increase significantly with streaming
  assert_true(memory_after.heap_used - memory_before.heap_used < 1000000)  // Less than 1MB increase
}

// Test 8: Schema Evolution and Versioning
test "schema evolution and versioning" {
  // Create version 1 telemetry data
  let v1_data = V1TelemetryData::new(
    "versioned_span",
    "678j9012-k89h-89i0-h123-193471731777",
    1234567890L,
    [
      ("legacy_metric", 100.0)
    ]
  )
  
  // Serialize with version 1 schema
  let schema_registry = SchemaRegistry::new()
  SchemaRegistry::register_schema(schema_registry, "TelemetryData", 1, V1Schema::new())
  
  let v1_serializer = VersionedSerializer::new(schema_registry)
  let v1_serialized = VersionedSerializer::serialize_with_version(v1_serializer, v1_data, 1)
  
  // Create version 2 telemetry data (with additional fields)
  let v2_data = V2TelemetryData::new(
    "versioned_span",
    "678j9012-k89h-89i0-h123-193471731777",
    1234567890L,
    [
      ("legacy_metric", 100.0),
      ("new_metric", 200.0)
    ],
    [
      ("new_attribute", "new_value")
    ]
  )
  
  // Register version 2 schema
  SchemaRegistry::register_schema(schema_registry, "TelemetryData", 2, V2Schema::new())
  
  // Test forward compatibility (reading v1 data with v2 schema)
  let v1_read_with_v2 = VersionedSerializer::deserialize_with_latest_compatible(v1_serializer, v1_serialized)
  match v1_read_with_v2 {
    V2Data(data) => {
      assert_eq(V2TelemetryData::get_span_name(data), "versioned_span")
      assert_eq(V2TelemetryData::get_metric(data, "legacy_metric"), Some(100.0))
      assert_eq(V2TelemetryData::get_metric(data, "new_metric"), None)  // Not in v1
    }
    _ => assert_true(false)
  }
  
  // Test backward compatibility (reading v2 data with v1 schema)
  let v2_serialized = VersionedSerializer::serialize_with_version(v1_serializer, v2_data, 2)
  let v2_read_with_v1 = VersionedSerializer::deserialize_with_version(v1_serializer, v2_serialized, 1)
  match v2_read_with_v1 {
    V1Data(data) => {
      assert_eq(V1TelemetryData::get_span_name(data), "versioned_span")
      assert_eq(V1TelemetryData::get_metric(data, "legacy_metric"), 100.0)
      // New fields should be ignored
    }
    _ => assert_true(false)
  }
}

// Test 9: Serialization Error Handling and Validation
test "serialization error handling and validation" {
  let validator = SerializationValidator::new()
  
  // Test with valid data
  let valid_object = TestTelemetryData::new(
    "valid_span",
    "789k0123-l89i-90j1-i234-204582832888",
    1234567890L,
    [("valid_metric", 50.0)],
    [("valid_attribute", "valid_value")]
  )
  
  let validation_result = SerializationValidator::validate(validator, valid_object)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // Test with invalid data (missing required fields)
  let invalid_object = TestTelemetryData::new(
    "",  // Empty span name (invalid)
    "",  // Empty trace ID (invalid)
    -1L,  // Invalid timestamp
    [("invalid_metric", -1.0)],  // Invalid metric value
    [("invalid_attribute", "")]  // Empty attribute value
  )
  
  let invalid_validation_result = SerializationValidator::validate(validator, invalid_object)
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length() > 0)
  
  // Test serialization with validation
  let validated_serializer = ValidatedSerializer::new(validator)
  let valid_serialization = ValidatedSerializer::try_serialize(validated_serializer, valid_object)
  assert_true(valid_serialization.is_success)
  
  let invalid_serialization = ValidatedSerializer::try_serialize(validated_serializer, invalid_object)
  assert_true(invalid_serialization.is_error)
  
  // Test deserialization with validation
  let valid_serialized_data = JsonSerializer::serialize(valid_object)
  let valid_deserialization = ValidatedSerializer::try_deserialize(validated_serializer, valid_serialized_data)
  assert_true(valid_deserialization.is_success)
  
  // Test with corrupted data
  let corrupted_data = "corrupted json data {"
  let corrupted_deserialization = ValidatedSerializer::try_deserialize(validated_serializer, corrupted_data)
  assert_true(corrupted_deserialization.is_error)
}

// Test 10: Cross-Format Conversion and Migration
test "cross-format conversion and migration" {
  let test_object = TestTelemetryData::new(
    "conversion_test",
    "890l1234-m90j-01k2-j345-315693933999",
    1234567890L,
    [
      ("conversion_metric", 75.0),
      ("another_metric", 150.0)
    ],
    [
      ("conversion_attribute", "conversion_value"),
      ("another_attribute", "another_value")
    ]
  )
  
  // Test JSON to XML conversion
  let json_data = JsonSerializer::serialize(test_object)
  let xml_data = FormatConverter::json_to_xml(json_data)
  assert_true(xml_data.contains("<TelemetryData>"))
  assert_true(xml_data.contains("<span_name>conversion_test</span_name>"))
  
  let xml_deserialized = XmlSerializer::deserialize(xml_data)
  assert_true(TestTelemetryData::equals(test_object, xml_deserialized))
  
  // Test XML to JSON conversion
  let back_to_json = FormatConverter::xml_to_json(xml_data)
  assert_true(back_to_json.contains("\"span_name\":\"conversion_test\""))
  
  let json_deserialized = JsonSerializer::deserialize(back_to_json)
  assert_true(TestTelemetryData::equals(test_object, json_deserialized))
  
  // Test JSON to Protocol Buffers conversion
  let proto_data = FormatConverter::json_to_protobuf(json_data)
  let proto_deserialized = ProtobufSerializer::deserialize(proto_data)
  assert_true(TestTelemetryData::equals(test_object, proto_deserialized))
  
  // Test Protocol Buffers to JSON conversion
  let json_from_proto = FormatConverter::protobuf_to_json(proto_data)
  let json_from_proto_deserialized = JsonSerializer::deserialize(json_from_proto)
  assert_true(TestTelemetryData::equals(test_object, json_from_proto_deserialized))
  
  // Test batch format migration
  let batch_data = [test_object, test_object, test_object]
  let json_batch = JsonSerializer::serialize_batch(batch_data)
  
  // Migrate from JSON to Protocol Buffers
  let proto_batch = FormatConverter::migrate_batch_format(json_batch, SerializationFormat::JSON, SerializationFormat::Protobuf)
  let proto_batch_deserialized = ProtobufSerializer::deserialize_batch(proto_batch)
  assert_eq(proto_batch_deserialized.length(), 3)
  
  for i in 0..=2 {
    assert_true(TestTelemetryData::equals(batch_data[i], proto_batch_deserialized[i]))
  }
  
  // Test format compatibility matrix
  let compatibility_matrix = FormatCompatibilityMatrix::new()
  assert_true(FormatCompatibilityMatrix::is_compatible(compatibility_matrix, SerializationFormat::JSON, SerializationFormat::XML))
  assert_true(FormatCompatibilityMatrix::is_compatible(compatibility_matrix, SerializationFormat::JSON, SerializationFormat::Protobuf))
  assert_true(FormatCompatibilityMatrix::is_compatible(compatibility_matrix, SerializationFormat::XML, SerializationFormat::JSON))
  assert_true(FormatCompatibilityMatrix::is_compatible(compatibility_matrix, SerializationFormat::Protobuf, SerializationFormat::JSON))
  
  // Test format conversion with data transformation
  let transformer = DataTransformer::new()
  DataTransformer::add_transformation(transformer, "metric_conversion", fn(value) {
    return value * 2.0  // Double all metrics
  })
  
  let transformed_xml = FormatConverter::json_to_xml_with_transform(json_data, transformer)
  let transformed_deserialized = XmlSerializer::deserialize(transformed_xml)
  let transformed_metrics = TestTelemetryData::get_metrics(transformed_deserialized)
  
  assert_eq(TestTelemetryData::get_metric_value(transformed_metrics, "conversion_metric"), Some(150.0))  // 75.0 * 2
  assert_eq(TestTelemetryData::get_metric_value(transformed_metrics, "another_metric"), Some(300.0))   // 150.0 * 2
}