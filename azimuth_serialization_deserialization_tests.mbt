// Azimuth Telemetry System - Serialization and Deserialization Tests
// This file contains test cases for serialization and deserialization functionality

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  let serializer = JsonSerializer::new()
  
  // Test basic data types
  let test_data = TestData::new(
    "test_string",
    42,
    3.14,
    true,
    [1, 2, 3, 4, 5],
    {"key1": "value1", "key2": "value2"}
  )
  
  // Serialize to JSON
  let json_result = serializer.serialize(test_data)
  assert_true(json_result.is_success)
  
  let json_string = json_result.data
  assert_true(json_string.contains("test_string"))
  assert_true(json_string.contains("42"))
  assert_true(json_string.contains("3.14"))
  assert_true(json_string.contains("true"))
  
  // Deserialize from JSON
  let deserialize_result = serializer.deserialize(json_string)
  assert_true(deserialize_result.is_success)
  
  let deserialized_data = deserialize_result.data
  assert_eq(deserialized_data.string_field, "test_string")
  assert_eq(deserialized_data.int_field, 42)
  assert_eq(deserialized_data.float_field, 3.14)
  assert_eq(deserialized_data.bool_field, true)
  assert_eq(deserialized_data.array_field, [1, 2, 3, 4, 5])
  assert_eq(deserialized_data.map_field, {"key1": "value1", "key2": "value2"})
  
  // Test nested objects
  let nested_data = NestedData::new(
    test_data,
    InnerData::new("inner_value", 100)
  )
  
  let nested_json_result = serializer.serialize(nested_data)
  assert_true(nested_json_result.is_success)
  
  let nested_deserialize_result = serializer.deserialize(nested_json_result.data)
  assert_true(nested_deserialize_result.is_success)
  
  let nested_deserialized = nested_deserialize_result.data
  assert_eq(nested_deserialized.inner_data.string_field, "inner_value")
  assert_eq(nested_deserialized.inner_data.int_field, 100)
  assert_eq(nested_deserialized.outer_data.string_field, "test_string")
  
  // Test optional fields
  let optional_data = OptionalData::new(
    Some("optional_value"),
    None,
    Some(42)
  )
  
  let optional_json_result = serializer.serialize(optional_data)
  assert_true(optional_json_result.is_success)
  
  let optional_deserialize_result = serializer.deserialize(optional_json_result.data)
  assert_true(optional_deserialize_result.is_success)
  
  let optional_deserialized = optional_deserialize_result.data
  match optional_deserialized.optional_string {
    Some(value) => assert_eq(value, "optional_value")
    None => assert_true(false)
  }
  match optional_deserialized.missing_field {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match optional_deserialized.optional_int {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
}

// Test 2: Protocol Buffers Serialization
test "protocol buffers serialization" {
  let protobuf_serializer = ProtobufSerializer::new()
  
  // Test telemetry data serialization
  let telemetry_data = TelemetryData::new(
    "trace-123",
    "span-456",
    1234567890L,
    "operation_name",
    [{"key": "service.name", "value": "test_service"}],
    [{"name": "event1", "timestamp": 1234567891L}]
  )
  
  // Serialize to protobuf
  let protobuf_result = protobuf_serializer.serialize(telemetry_data)
  assert_true(protobuf_result.is_success)
  
  let protobuf_bytes = protobuf_result.data
  assert_true(protobuf_bytes.length() > 0)
  
  // Deserialize from protobuf
  let deserialize_result = protobuf_serializer.deserialize(protobuf_bytes)
  assert_true(deserialize_result.is_success)
  
  let deserialized_data = deserialize_result.data
  assert_eq(deserialized_data.trace_id, "trace-123")
  assert_eq(deserialized_data.span_id, "span-456")
  assert_eq(deserialized_data.timestamp, 1234567890L)
  assert_eq(deserialized_data.operation_name, "operation_name")
  assert_eq(deserialized_data.attributes.length(), 1)
  assert_eq(deserialized_data.events.length(), 1)
  
  // Test batch serialization
  let batch_data = [
    TelemetryData::new("trace-1", "span-1", 1000L, "op1", [], []),
    TelemetryData::new("trace-2", "span-2", 2000L, "op2", [], []),
    TelemetryData::new("trace-3", "span-3", 3000L, "op3", [], [])
  ]
  
  let batch_result = protobuf_serializer.serialize_batch(batch_data)
  assert_true(batch_result.is_success)
  
  let batch_deserialize_result = protobuf_serializer.deserialize_batch(batch_result.data)
  assert_true(batch_deserialize_result.is_success)
  
  let deserialized_batch = batch_deserialize_result.data
  assert_eq(deserialized_batch.length(), 3)
  assert_eq(deserialized_batch[0].trace_id, "trace-1")
  assert_eq(deserialized_batch[1].trace_id, "trace-2")
  assert_eq(deserialized_batch[2].trace_id, "trace-3")
  
  // Test schema evolution compatibility
  let v1_data = TelemetryDataV1::new("trace-123", "span-456", 1234567890L)
  let v1_result = protobuf_serializer.serialize_v1(v1_data)
  assert_true(v1_result.is_success)
  
  // Deserialize V1 data with V2 schema (backward compatibility)
  let v2_deserialize_result = protobuf_serializer.deserialize_v2(v1_result.data)
  assert_true(v2_deserialize_result.is_success)
  
  let v2_data = v2_deserialize_result.data
  assert_eq(v2_data.trace_id, "trace-123")
  assert_eq(v2_data.span_id, "span-456")
  assert_eq(v2_data.timestamp, 1234567890L)
  assert_eq(v2_data.operation_name, "") // Default value for new field
}

// Test 3: MessagePack Serialization
test "messagepack serialization" {
  let msgpack_serializer = MessagePackSerializer::new()
  
  // Test complex data structure
  let complex_data = ComplexData::new(
    "test_id",
    12345,
    [1.1, 2.2, 3.3, 4.4, 5.5],
    {
      "nested_map": {
        "inner_key": "inner_value",
        "number": 42
      },
      "array_in_map": ["a", "b", "c"]
    },
    Some(OptionalField::new("optional_value"))
  )
  
  // Serialize to MessagePack
  let msgpack_result = msgpack_serializer.serialize(complex_data)
  assert_true(msgpack_result.is_success)
  
  let msgpack_bytes = msgpack_result.data
  assert_true(msgpack_bytes.length() > 0)
  
  // Verify MessagePack format
  assert_eq(msgpack_bytes[0], 0x82) // FixMap with 2 elements (example)
  
  // Deserialize from MessagePack
  let deserialize_result = msgpack_serializer.deserialize(msgpack_bytes)
  assert_true(deserialize_result.is_success)
  
  let deserialized_data = deserialize_result.data
  assert_eq(deserialized_data.id, "test_id")
  assert_eq(deserialized_data.number, 12345)
  assert_eq(deserialized_data.float_array, [1.1, 2.2, 3.3, 4.4, 5.5])
  assert_eq(deserialized_data.nested_map["nested_map"]["inner_key"], "inner_value")
  assert_eq(deserialized_data.nested_map["nested_map"]["number"], 42)
  assert_eq(deserialized_data.nested_map["array_in_map"], ["a", "b", "c"])
  
  match deserialized_data.optional_field {
    Some(field) => assert_eq(field.value, "optional_value")
    None => assert_true(false)
  }
  
  // Test streaming serialization
  let stream_data = [
    StreamData::new("item1", 1),
    StreamData::new("item2", 2),
    StreamData::new("item3", 3)
  ]
  
  let stream_result = msgpack_serializer.serialize_stream(stream_data)
  assert_true(stream_result.is_success)
  
  let stream_deserialize_result = msgpack_serializer.deserialize_stream(stream_result.data)
  assert_true(stream_deserialize_result.is_success)
  
  let deserialized_stream = stream_deserialize_result.data
  assert_eq(deserialized_stream.length(), 3)
  assert_eq(deserialized_stream[0].name, "item1")
  assert_eq(deserialized_stream[0].value, 1)
  assert_eq(deserialized_stream[1].name, "item2")
  assert_eq(deserialized_stream[1].value, 2)
  assert_eq(deserialized_stream[2].name, "item3")
  assert_eq(deserialized_stream[2].value, 3)
}

// Test 4: XML Serialization and Deserialization
test "xml serialization and deserialization" {
  let xml_serializer = XmlSerializer::new()
  
  // Test telemetry data with XML-specific considerations
  let xml_data = XmlTelemetryData::new(
    "trace-123",
    "span-456",
    1234567890L,
    "operation_name",
    [
      XmlAttribute::new("service.name", "test_service"),
      XmlAttribute::new("service.version", "1.0.0")
    ],
    [
      XmlEvent::new("start_event", 1234567891L, [
        XmlAttribute::new("event_type", "lifecycle")
      ])
    ]
  )
  
  // Serialize to XML
  let xml_result = xml_serializer.serialize(xml_data)
  assert_true(xml_result.is_success)
  
  let xml_string = xml_result.data
  assert_true(xml_string.contains("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))
  assert_true(xml_string.contains("<TelemetryData"))
  assert_true(xml_string.contains("<traceId>trace-123</traceId>"))
  assert_true(xml_string.contains("<spanId>span-456</spanId>"))
  assert_true(xml_string.contains("<operationName>operation_name</operationName>"))
  assert_true(xml_string.contains("<attributes>"))
  assert_true(xml_string.contains("<events>"))
  
  // Deserialize from XML
  let deserialize_result = xml_serializer.deserialize(xml_string)
  assert_true(deserialize_result.is_success)
  
  let deserialized_data = deserialize_result.data
  assert_eq(deserialized_data.trace_id, "trace-123")
  assert_eq(deserialized_data.span_id, "span-456")
  assert_eq(deserialized_data.operation_name, "operation_name")
  assert_eq(deserialized_data.attributes.length(), 2)
  assert_eq(deserialized_data.events.length(), 1)
  
  // Test XML with namespaces
  let namespaced_data = NamespacedData::new(
    "xmlns:telemetry=\"http://example.com/telemetry\"",
    "telemetry:span",
    [
      XmlNamespaceAttribute::new("telemetry:service", "test-service")
    ]
  )
  
  let namespaced_result = xml_serializer.serialize(namespaced_data)
  assert_true(namespaced_result.is_success)
  
  let namespaced_xml = namespaced_result.data
  assert_true(namespaced_xml.contains("xmlns:telemetry"))
  assert_true(namespaced_xml.contains("telemetry:span"))
  assert_true(namespaced_xml.contains("telemetry:service"))
  
  let namespaced_deserialize_result = xml_serializer.deserialize(namespaced_xml)
  assert_true(namespaced_deserialize_result.is_success)
  
  let namespaced_deserialized = namespaced_deserialize_result.data
  assert_eq(namespaced_deserialized.namespace, "xmlns:telemetry=\"http://example.com/telemetry\"")
  assert_eq(namespaced_deserialized.root_element, "telemetry:span")
  
  // Test XML schema validation
  let xml_schema = """
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:element name="TelemetryData">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="traceId" type="xs:string"/>
          <xs:element name="spanId" type="xs:string"/>
          <xs:element name="timestamp" type="xs:long"/>
          <xs:element name="operationName" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
  </xs:schema>
  """
  
  let validation_result = xml_serializer.validate_against_schema(xml_string, xml_schema)
  assert_true(validation_result.is_valid)
  
  // Test invalid XML
  let invalid_xml = "<TelemetryData><traceId>trace-123</spanId></TelemetryData>"
  let invalid_result = xml_serializer.deserialize(invalid_xml)
  assert_false(invalid_result.is_success)
  assert_true(invalid_result.error_message.contains("XML parsing error"))
}

// Test 5: Custom Serialization Formats
test "custom serialization formats" {
  let custom_serializer = CustomSerializer::new()
  
  // Register custom serializers for specific types
  custom_serializer.register_serializer("TelemetryData", CustomTelemetrySerializer::new())
  custom_serializer.register_serializer("MetricData", CustomMetricSerializer::new())
  
  // Test custom telemetry data serialization
  let telemetry_data = TelemetryData::new(
    "trace-123",
    "span-456",
    1234567890L,
    "operation_name",
    [],
    []
  )
  
  let custom_result = custom_serializer.serialize(telemetry_data)
  assert_true(custom_result.is_success)
  
  let custom_format = custom_result.data
  assert_true(custom_format.contains("TELEMETRY|"))
  assert_true(custom_format.contains("trace-123"))
  assert_true(custom_format.contains("span-456"))
  assert_true(custom_format.contains("1234567890"))
  
  // Deserialize using custom format
  let custom_deserialize_result = custom_serializer.deserialize(custom_format)
  assert_true(custom_deserialize_result.is_success)
  
  let custom_deserialized = custom_deserialize_result.data
  assert_eq(custom_deserialized.trace_id, "trace-123")
  assert_eq(custom_deserialized.span_id, "span-456")
  assert_eq(custom_deserialized.timestamp, 1234567890L)
  
  // Test compression in serialization
  let compression_serializer = CompressionSerializer::new()
  
  let large_data = LargeData::new("x".repeat(10000)) // 10KB string
  
  let compressed_result = compression_serializer.serialize(large_data)
  assert_true(compressed_result.is_success)
  
  let compressed_data = compressed_result.data
  assert_true(compressed_data.length() < 10000) // Should be compressed
  
  let decompress_result = compression_serializer.deserialize(compressed_data)
  assert_true(decompress_result.is_success)
  
  let decompressed_data = decompress_result.data
  assert_eq(decompressed_data.content, "x".repeat(10000))
  
  // Test encryption in serialization
  let encryption_serializer = EncryptionSerializer::new("secret_key_123")
  
  let sensitive_data = SensitiveData::new(
    "user123",
    "password456",
    "api_key_789"
  )
  
  let encrypted_result = encryption_serializer.serialize(sensitive_data)
  assert_true(encrypted_result.is_success)
  
  let encrypted_data = encrypted_result.data
  assert_false(encrypted_data.contains("user123"))
  assert_false(encrypted_data.contains("password456"))
  assert_false(encrypted_data.contains("api_key_789"))
  
  let decrypt_result = encryption_serializer.deserialize(encrypted_data)
  assert_true(decrypt_result.is_success)
  
  let decrypted_data = decrypt_result.data
  assert_eq(decrypted_data.user_id, "user123")
  assert_eq(decrypted_data.password, "password456")
  assert_eq(decrypted_data.api_key, "api_key_789")
}

// Test 6: Serialization Performance and Optimization
test "serialization performance and optimization" {
  let performance_tester = SerializationPerformanceTester::new()
  
  // Test large dataset serialization performance
  let large_dataset = generate_large_telemetry_dataset(10000) // 10,000 records
  
  let json_performance = performance_tester.test_json_serialization(large_dataset)
  assert_true(json_performance.serialization_time > 0)
  assert_true(json_performance.deserialization_time > 0)
  assert_true(json_performance.serialized_size > 0)
  
  let protobuf_performance = performance_tester.test_protobuf_serialization(large_dataset)
  assert_true(protobuf_performance.serialization_time > 0)
  assert_true(protobuf_performance.deserialization_time > 0)
  assert_true(protobuf_performance.serialized_size > 0)
  
  let msgpack_performance = performance_tester.test_msgpack_serialization(large_dataset)
  assert_true(msgpack_performance.serialization_time > 0)
  assert_true(msgpack_performance.deserialization_time > 0)
  assert_true(msgpack_performance.serialized_size > 0)
  
  // Compare performance
  assert_true(protobuf_performance.serialized_size < json_performance.serialized_size)
  assert_true(msgpack_performance.serialized_size < json_performance.serialized_size)
  
  // Test serialization with memory pooling
  let pooled_serializer = PooledSerializer::new(100) // 100 object pool
  
  let pooled_performance = performance_tester.test_pooled_serialization(pooled_serializer, large_dataset)
  assert_true(pooled_performance.serialization_time > 0)
  assert_true(pooled_performance.pool_hits > 0)
  assert_true(pooled_performance.pool_misses >= 0)
  
  // Test streaming serialization for large data
  let streaming_serializer = StreamingSerializer::new()
  
  let streaming_performance = performance_tester.test_streaming_serialization(streaming_serializer, large_dataset)
  assert_true(streaming_performance.serialization_time > 0)
  assert_true(streaming_performance.peak_memory_usage < json_performance.peak_memory_usage)
  
  // Test serialization optimization techniques
  let optimizer = SerializationOptimizer::new()
  
  let optimization_suggestions = optimizer.analyze_serialization_performance(large_dataset)
  assert_true(optimization_suggestions.length() > 0)
  
  let field_suggestion = optimization_suggestions.find(fn(s) { s.type == "field_optimization" })
  match field_suggestion {
    Some(suggestion) => {
      assert_true(suggestion.fields_to_optimize.length() > 0)
      assert_true(suggestion.estimated_size_reduction > 0)
    }
    None => assert_true(false)
  }
  
  // Apply optimizations
  let optimized_serializer = optimizer.create_optimized_serializer(large_dataset)
  let optimized_performance = performance_tester.test_optimized_serialization(optimized_serializer, large_dataset)
  
  assert_true(optimized_performance.serialized_size < json_performance.serialized_size)
  assert_true(optimized_performance.serialization_time <= json_performance.serialization_time * 1.1) // Allow 10% overhead
}

// Test 7: Serialization Error Handling and Recovery
test "serialization error handling and recovery" {
  let error_handler = SerializationErrorHandler::new()
  
  // Test malformed JSON handling
  let malformed_json = "{\"trace_id\": \"trace-123\", \"span_id\": \"span-456\", \"timestamp\": 1234567890"
  
  let json_error_result = error_handler.handle_json_deserialization_error(malformed_json)
  assert_true(json_error_result.is_error)
  assert_true(json_error_result.error_message.contains("JSON"))
  assert_true(json_error_result.recovery_suggestions.length() > 0)
  
  // Test corrupted protobuf handling
  let corrupted_protobuf = [0x08, 0x96, 0x01, 0x12, 0x04, 0x74] // Truncated protobuf
  
  let protobuf_error_result = error_handler.handle_protobuf_deserialization_error(corrupted_protobuf)
  assert_true(protobuf_error_result.is_error)
  assert_true(protobuf_error_result.error_message.contains("protobuf"))
  assert_true(protobuf_error_result.recovery_suggestions.length() > 0)
  
  // Test schema mismatch handling
  let v1_data = "{\"version\": 1, \"trace_id\": \"trace-123\"}"
  let v2_schema = "{\"required\": [\"version\", \"trace_id\", \"span_id\"]}"
  
  let schema_error_result = error_handler.handle_schema_mismatch_error(v1_data, v2_schema)
  assert_true(schema_error_result.is_error)
  assert_true(schema_error_result.error_message.contains("schema"))
  assert_true(schema_error_result.missing_fields.contains("span_id"))
  
  // Test data corruption detection
  let original_data = TelemetryData::new("trace-123", "span-456", 1234567890L, "operation", [], [])
  let serializer = JsonSerializer::new()
  
  let serialized_result = serializer.serialize(original_data)
  assert_true(serialized_result.is_success)
  
  // Corrupt the serialized data
  let corrupted_data = corrupt_serialized_data(serialized_result.data)
  
  let corruption_result = error_handler.detect_data_corruption(corrupted_data, "json")
  assert_true(corruption_result.is_corrupted)
  assert_true(corruption_result.corruption_score > 0.5)
  
  // Test data recovery strategies
  let recovery_strategies = error_handler.get_recovery_strategies(corruption_result)
  assert_true(recovery_strategies.length() > 0)
  
  let best_strategy = recovery_strategies[0]
  assert_true(best_strategy.success_rate > 0.0)
  assert_true(best_strategy.applicable_data_types.contains("json"))
  
  // Apply recovery strategy
  let recovery_result = error_handler.apply_recovery_strategy(best_strategy, corrupted_data)
  if recovery_result.success {
    let recovered_data = recovery_result.recovered_data
    assert_true(recovered_data.length() > 0)
  }
  
  // Test fallback serialization
  let fallback_serializer = FallbackSerializer::new()
  
  let fallback_result = fallback_serializer.serialize_with_fallback(original_data)
  assert_true(fallback_result.is_success)
  assert_true(fallback_result.primary_format_used || fallback_result.fallback_format_used)
  
  // Test partial data recovery
  let partial_data = "{\"trace_id\": \"trace-123\", \"span_id\": null, \"timestamp\": 1234567890}"
  let partial_result = error_handler.handle_partial_data(partial_data)
  assert_true(partial_result.is_partial)
  assert_true(partial_result.available_fields.contains("trace_id"))
  assert_true(partial_result.missing_fields.contains("span_id"))
}

// Test 8: Cross-Format Serialization Compatibility
test "cross-format serialization compatibility" {
  let format_converter = FormatConverter::new()
  
  // Test JSON to Protobuf conversion
  let json_data = """
  {
    "trace_id": "trace-123",
    "span_id": "span-456",
    "timestamp": 1234567890,
    "operation_name": "test_operation",
    "attributes": [
      {"key": "service.name", "value": "test_service"}
    ],
    "events": [
      {"name": "start_event", "timestamp": 1234567891}
    ]
  }
  """
  
  let json_to_protobuf_result = format_converter.convert(json_data, "json", "protobuf")
  assert_true(json_to_protobuf_result.is_success)
  
  let protobuf_data = json_to_protobuf_result.data
  assert_true(protobuf_data.length() > 0)
  
  // Verify converted data
  let protobuf_deserializer = ProtobufSerializer::new()
  let verify_result = protobuf_deserializer.deserialize(protobuf_data)
  assert_true(verify_result.is_success)
  
  let verified_data = verify_result.data
  assert_eq(verified_data.trace_id, "trace-123")
  assert_eq(verified_data.span_id, "span-456")
  
  // Test Protobuf to XML conversion
  let protobuf_to_xml_result = format_converter.convert(protobuf_data, "protobuf", "xml")
  assert_true(protobuf_to_xml_result.is_success)
  
  let xml_data = protobuf_to_xml_result.data
  assert_true(xml_data.contains("<?xml"))
  assert_true(xml_data.contains("<traceId>trace-123</traceId>"))
  assert_true(xml_data.contains("<spanId>span-456</spanId>"))
  
  // Test XML to MessagePack conversion
  let xml_to_msgpack_result = format_converter.convert(xml_data, "xml", "msgpack")
  assert_true(xml_to_msgpack_result.is_success)
  
  let msgpack_data = xml_to_msgpack_result.data
  assert_true(msgpack_data.length() > 0)
  
  // Verify round-trip conversion
  let msgpack_to_json_result = format_converter.convert(msgpack_data, "msgpack", "json")
  assert_true(msgpack_to_json_result.is_success)
  
  let final_json = msgpack_to_json_result.data
  let final_data = JsonSerializer::new().deserialize(final_json).data
  
  assert_eq(final_data.trace_id, "trace-123")
  assert_eq(final_data.span_id, "span-456")
  assert_eq(final_data.operation_name, "test_operation")
  
  // Test format compatibility matrix
  let compatibility_matrix = format_converter.get_compatibility_matrix()
  assert_true(compatibility_matrix.is_compatible("json", "protobuf"))
  assert_true(compatibility_matrix.is_compatible("protobuf", "xml"))
  assert_true(compatibility_matrix.is_compatible("xml", "msgpack"))
  assert_true(compatibility_matrix.is_compatible("msgpack", "json"))
  
  // Test data loss detection during conversion
  let rich_data = RichData::new(
    "trace-123",
    "span-456",
    1234567890L,
    "operation",
    {
      "complex_attribute": {
        "nested": {
          "deep": {
            "value": "deep_value"
          }
        }
      }
    },
    [
      Event::new("event1", 1234567891L, {
        "event_data": {
          "details": {
            "info": "detailed_info"
          }
        }
      })
    ]
  )
  
  let data_loss_detector = DataLossDetector::new()
  
  let json_to_protobuf_loss = data_loss_detector.detect_data_loss(rich_data, "json", "protobuf")
  assert_true(json_to_protobuf_loss.has_data_loss)
  assert_true(json_to_protobuf_loss.lost_fields.length() > 0)
  
  // Test format-specific optimizations
  let optimizer = FormatOptimizer::new()
  
  let json_optimizations = optimizer.get_optimizations_for_format("json")
  assert_true(json_optimizations.length() > 0)
  
  let protobuf_optimizations = optimizer.get_optimizations_for_format("protobuf")
  assert_true(protobuf_optimizations.length() > 0)
  
  // Apply optimizations
  let optimized_json = optimizer.optimize_for_format(rich_data, "json")
  assert_true(optimized_json.optimized)
  assert_true(optimized_json.size_reduction > 0)
  
  let optimized_protobuf = optimizer.optimize_for_format(rich_data, "protobuf")
  assert_true(optimized_protobuf.optimized)
  assert_true(optimized_protobuf.size_reduction > 0)
}

// Helper function to generate large telemetry dataset
fn generate_large_telemetry_dataset(count: Int) -> Array[TelemetryData] {
  let dataset = []
  for i in 0..count {
    let data = TelemetryData::new(
      "trace-" + i.to_string(),
      "span-" + i.to_string(),
      1234567890L + i.to_long(),
      "operation_" + i.to_string(),
      [],
      []
    )
    dataset = dataset.push(data)
  }
  dataset
}

// Helper function to corrupt serialized data
fn corrupt_serialized_data(data: String) -> String {
  if data.length() > 10 {
    data.substring(0, data.length() - 5) + "CORRUPT"
  } else {
    data + "CORRUPT"
  }
}