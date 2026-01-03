// Azimuth Serialization and Deserialization Tests
// This file contains comprehensive test cases for data serialization and deserialization

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  let json_serializer = JsonSerializer::new()
  
  // Test basic telemetry data serialization
  let telemetry_data = TelemetryData::with_attributes("test_span", [
    ("trace_id", StringValue("0af7651916cd43dd8448eb211c80319c")),
    ("span_id", StringValue("b7ad6b7169203331")),
    ("service_name", StringValue("payment-service")),
    ("operation_name", StringValue("process_payment")),
    ("duration_ms", IntValue(250)),
    ("status", StringValue("ok")),
    ("error", BoolValue(false))
  ])
  
  // Serialize to JSON
  let json_result = JsonSerializer::serialize(json_serializer, telemetry_data)
  match json_result {
    Ok(json_string) => {
      assert_true(json_string.contains("\"trace_id\":\"0af7651916cd43dd8448eb211c80319c\""))
      assert_true(json_string.contains("\"span_id\":\"b7ad6b7169203331\""))
      assert_true(json_string.contains("\"service_name\":\"payment-service\""))
      assert_true(json_string.contains("\"duration_ms\":250"))
    }
    Err(error) => assert_true(false)
  }
  
  // Deserialize from JSON
  match json_result {
    Ok(json_string) => {
      let deserialize_result = JsonSerializer::deserialize(json_serializer, json_string)
      match deserialize_result {
        Ok(deserialized_data) => {
          assert_eq(TelemetryData::get_attribute(deserialized_data, "trace_id"), Some(StringValue("0af7651916cd43dd8448eb211c80319c")))
          assert_eq(TelemetryData::get_attribute(deserialized_data, "span_id"), Some(StringValue("b7ad6b7169203331")))
          assert_eq(TelemetryData::get_attribute(deserialized_data, "service_name"), Some(StringValue("payment-service")))
          assert_eq(TelemetryData::get_attribute(deserialized_data, "duration_ms"), Some(IntValue(250)))
        }
        Err(error) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // Test complex nested structure serialization
  let nested_data = TelemetryData::with_nested_attributes("complex_span", [
    ("parent", NestedValue([
      ("child1", StringValue("value1")),
      ("child2", IntValue(42))
    ])),
    ("array", ArrayValue([
      StringValue("item1"),
      StringValue("item2"),
      IntValue(3)
    ]))
  ])
  
  let nested_json_result = JsonSerializer::serialize(json_serializer, nested_data)
  match nested_json_result {
    Ok(json_string) => {
      assert_true(json_string.contains("\"parent\""))
      assert_true(json_string.contains("\"child1\""))
      assert_true(json_string.contains("\"array\""))
    }
    Err(error) => assert_true(false)
  }
  
  // Test batch serialization
  let batch_data = [
    telemetry_data,
    nested_data,
    TelemetryData::with_attributes("simple_span", [
      ("trace_id", StringValue("trace2")),
      ("span_id", StringValue("span2"))
    ])
  ]
  
  let batch_json_result = JsonSerializer::serialize_batch(json_serializer, batch_data)
  match batch_json_result {
    Ok(json_string) => {
      assert_true(json_string.contains("["))
      assert_true(json_string.contains("]"))
      assert_true(json_string.contains("\"test_span\""))
      assert_true(json_string.contains("\"complex_span\""))
      assert_true(json_string.contains("\"simple_span\""))
    }
    Err(error) => assert_true(false)
  }
}

// Test 2: Binary Serialization and Deserialization
test "binary serialization and deserialization" {
  let binary_serializer = BinarySerializer::new()
  
  // Test basic telemetry data serialization
  let telemetry_data = TelemetryData::with_attributes("binary_test", [
    ("trace_id", StringValue("binary_trace_123")),
    ("span_id", StringValue("binary_span_456")),
    ("timestamp", IntValue(1640995200L)),
    ("duration_ms", IntValue(150)),
    ("status_code", IntValue(200)),
    ("success", BoolValue(true))
  ])
  
  // Serialize to binary
  let binary_result = BinarySerializer::serialize(binary_serializer, telemetry_data)
  match binary_result {
    Ok(binary_data) => {
      assert_true(binary_data.length() > 0)
    }
    Err(error) => assert_true(false)
  }
  
  // Deserialize from binary
  match binary_result {
    Ok(binary_data) => {
      let deserialize_result = BinarySerializer::deserialize(binary_serializer, binary_data)
      match deserialize_result {
        Ok(deserialized_data) => {
          assert_eq(TelemetryData::get_attribute(deserialized_data, "trace_id"), Some(StringValue("binary_trace_123")))
          assert_eq(TelemetryData::get_attribute(deserialized_data, "span_id"), Some(StringValue("binary_span_456")))
          assert_eq(TelemetryData::get_attribute(deserialized_data, "timestamp"), Some(IntValue(1640995200L)))
          assert_eq(TelemetryData::get_attribute(deserialized_data, "success"), Some(BoolValue(true)))
        }
        Err(error) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // Test binary serialization efficiency
  let large_dataset = []
  for i in 0..=1000 {
    large_dataset.push(TelemetryData::with_attributes("large_span_" + i.to_string(), [
      ("trace_id", StringValue("trace_" + i.to_string())),
      ("span_id", StringValue("span_" + i.to_string())),
      ("index", IntValue(i)),
      ("data", StringValue("large_data_payload_" + i.to_string()))
    ]))
  }
  
  let large_binary_result = BinarySerializer::serialize_batch(binary_serializer, large_dataset)
  match large_binary_result {
    Ok(binary_data) => {
      // Binary should be more compact than equivalent JSON
      let json_size = estimate_json_size(large_dataset)
      let binary_size = binary_data.length()
      assert_true(binary_size < json_size)
      
      // Test deserialization of large dataset
      let deserialize_result = BinarySerializer::deserialize_batch(binary_serializer, binary_data)
      match deserialize_result {
        Ok(deserialized_dataset) => {
          assert_eq(deserialized_dataset.length(), large_dataset.length())
        }
        Err(error) => assert_true(false)
      }
    }
    Err(error) => assert_true(false)
  }
}

// Test 3: Protocol Buffers Serialization
test "protocol buffers serialization" {
  let protobuf_serializer = ProtobufSerializer::new()
  
  // Test span serialization
  let span = Span::new("protobuf_test", Server, SpanContext::new("trace123", "span456", true, ""))
  Span::add_event(span, "start_event", Some([("key1", StringValue("value1"))]))
  Span::add_event(span, "end_event", Some([("key2", StringValue("value2"))]))
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Serialize to protobuf
  let protobuf_result = ProtobufSerializer::serialize_span(protobuf_serializer, span)
  match protobuf_result {
    Ok(protobuf_data) => {
      assert_true(protobuf_data.length() > 0)
    }
    Err(error) => assert_true(false)
  }
  
  // Deserialize from protobuf
  match protobuf_result {
    Ok(protobuf_data) => {
      let deserialize_result = ProtobufSerializer::deserialize_span(protobuf_serializer, protobuf_data)
      match deserialize_result {
        Ok(deserialized_span) => {
          assert_eq(Span::name(deserialized_span), "protobuf_test")
          assert_eq(Span::span_context(deserialized_span).trace_id, "trace123")
          assert_eq(Span::span_context(deserialized_span).span_id, "span456")
          assert_eq(Span::status(deserialized_span), Ok)
          
          let events = Span::events(deserialized_span)
          assert_eq(events.length(), 2)
        }
        Err(error) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // Test metric serialization
  let metric = Metric::with_labels("request_duration", Histogram, 150.5, [
    ("service", "api-gateway"),
    ("endpoint", "/api/users"),
    ("method", "GET")
  ])
  
  let metric_protobuf_result = ProtobufSerializer::serialize_metric(protobuf_serializer, metric)
  match metric_protobuf_result {
    Ok(protobuf_data) => {
      let deserialize_result = ProtobufSerializer::deserialize_metric(protobuf_serializer, protobuf_data)
      match deserialize_result {
        Ok(deserialized_metric) => {
          assert_eq(Metric::name(deserialized_metric), "request_duration")
          assert_eq(Metric::metric_type(deserialized_metric), Histogram)
          assert_eq(Metric::value(deserialized_metric), 150.5)
        }
        Err(error) => assert_true(false)
      }
    }
    Err(error) => assert_true(false)
  }
}

// Test 4: Cross-Format Serialization
test "cross-format serialization" {
  let format_converter = FormatConverter::new()
  
  // Create test data
  let original_data = TelemetryData::with_attributes("cross_format_test", [
    ("trace_id", StringValue("cross_format_trace")),
    ("span_id", StringValue("cross_format_span")),
    ("service_name", StringValue("format-converter-service")),
    ("operation", StringValue("convert_formats")),
    ("start_time", IntValue(1640995200L)),
    ("end_time", IntValue(1640995250L)),
    ("tags", ArrayValue(["tag1", "tag2", "tag3"]))
  ])
  
  // JSON to Binary conversion
  let json_serializer = JsonSerializer::new()
  let json_result = JsonSerializer::serialize(json_serializer, original_data)
  
  match json_result {
    Ok(json_string) => {
      let binary_result = FormatConverter::json_to_binary(format_converter, json_string)
      match binary_result {
        Ok(binary_data) => {
          // Verify binary data integrity
          let binary_deserializer = BinarySerializer::new()
          let deserialize_result = BinarySerializer::deserialize(binary_deserializer, binary_data)
          match deserialize_result {
            Ok(deserialized_data) => {
              assert_eq(TelemetryData::get_attribute(deserialized_data, "trace_id"), Some(StringValue("cross_format_trace")))
              assert_eq(TelemetryData::get_attribute(deserialized_data, "service_name"), Some(StringValue("format-converter-service")))
            }
            Err(error) => assert_true(false)
          }
        }
        Err(error) => assert_true(false)
      }
    }
    Err(error) => assert_true(false)
  }
  
  // Binary to JSON conversion
  let binary_serializer = BinarySerializer::new()
  let binary_result = BinarySerializer::serialize(binary_serializer, original_data)
  
  match binary_result {
    Ok(binary_data) => {
      let json_result = FormatConverter::binary_to_json(format_converter, binary_data)
      match json_result {
        Ok(json_string) => {
          // Verify JSON data integrity
          let json_deserializer = JsonSerializer::new()
          let deserialize_result = JsonSerializer::deserialize(json_deserializer, json_string)
          match deserialize_result {
            Ok(deserialized_data) => {
              assert_eq(TelemetryData::get_attribute(deserialized_data, "trace_id"), Some(StringValue("cross_format_trace")))
              assert_eq(TelemetryData::get_attribute(deserialized_data, "service_name"), Some(StringValue("format-converter-service")))
            }
            Err(error) => assert_true(false)
          }
        }
        Err(error) => assert_true(false)
      }
    }
    Err(error) => assert_true(false)
  }
  
  // JSON to Protobuf conversion
  let json_result = JsonSerializer::serialize(json_serializer, original_data)
  match json_result {
    Ok(json_string) => {
      let protobuf_result = FormatConverter::json_to_protobuf(format_converter, json_string)
      match protobuf_result {
        Ok(protobuf_data) => {
          assert_true(protobuf_data.length() > 0)
        }
        Err(error) => assert_true(false)
      }
    }
    Err(error) => assert_true(false)
  }
}

// Test 5: Schema Evolution and Compatibility
test "schema evolution and compatibility" {
  let schema_manager = SchemaManager::new()
  
  // Define version 1 schema
  let v1_schema = Schema::new("telemetry_data", "1.0.0", [
    Field::new("trace_id", FieldType::String, true),
    Field::new("span_id", FieldType::String, true),
    Field::new("service_name", FieldType::String, true),
    Field::new("duration_ms", FieldType::Int, false)
  ])
  
  SchemaManager::register_schema(schema_manager, v1_schema)
  
  // Create data with v1 schema
  let v1_data = TelemetryData::with_attributes("v1_data", [
    ("trace_id", StringValue("v1_trace")),
    ("span_id", StringValue("v1_span")),
    ("service_name", StringValue("v1_service")),
    ("duration_ms", IntValue(100))
  ])
  
  // Serialize with v1 schema
  let v1_serializer = SchemaAwareSerializer::new(schema_manager, "1.0.0")
  let v1_serialized = v1_serializer.serialize(v1_data)
  
  // Define version 2 schema (with additional fields)
  let v2_schema = Schema::new("telemetry_data", "2.0.0", [
    Field::new("trace_id", FieldType::String, true),
    Field::new("span_id", FieldType::String, true),
    Field::new("service_name", FieldType::String, true),
    Field::new("duration_ms", FieldType::Int, false),
    Field::new("status", FieldType::String, false), // New field
    Field::new("tags", FieldType::ArrayString, false) // New field
  ])
  
  SchemaManager::register_schema(schema_manager, v2_schema)
  
  // Test forward compatibility (v1 data read by v2 schema)
  let v2_deserializer = SchemaAwareDeserializer::new(schema_manager, "2.0.0")
  let forward_compatible_result = v2_deserializer.deserialize(v1_serialized)
  
  match forward_compatible_result {
    Ok(data) => {
      assert_eq(TelemetryData::get_attribute(data, "trace_id"), Some(StringValue("v1_trace")))
      assert_eq(TelemetryData::get_attribute(data, "duration_ms"), Some(IntValue(100)))
      // New fields should have default values
      assert_eq(TelemetryData::get_attribute(data, "status"), None)
      assert_eq(TelemetryData::get_attribute(data, "tags"), None)
    }
    Err(error) => assert_true(false)
  }
  
  // Create data with v2 schema
  let v2_data = TelemetryData::with_attributes("v2_data", [
    ("trace_id", StringValue("v2_trace")),
    ("span_id", StringValue("v2_span")),
    ("service_name", StringValue("v2_service")),
    ("duration_ms", IntValue(200)),
    ("status", StringValue("success")),
    ("tags", ArrayValue(["tag1", "tag2"]))
  ])
  
  // Serialize with v2 schema
  let v2_serializer = SchemaAwareSerializer::new(schema_manager, "2.0.0")
  let v2_serialized = v2_serializer.serialize(v2_data)
  
  // Test backward compatibility (v2 data read by v1 schema)
  let v1_deserializer = SchemaAwareDeserializer::new(schema_manager, "1.0.0")
  let backward_compatible_result = v1_deserializer.deserialize(v2_serialized)
  
  match backward_compatible_result {
    Ok(data) => {
      assert_eq(TelemetryData::get_attribute(data, "trace_id"), Some(StringValue("v2_trace")))
      assert_eq(TelemetryData::get_attribute(data, "duration_ms"), Some(IntValue(200)))
      // v1 schema should ignore v2-specific fields
    }
    Err(error) => assert_true(false)
  }
}

// Test 6: Compression and Serialization
test "compression and serialization" {
  let compression_serializer = CompressionSerializer::new(CompressionAlgorithm::Gzip)
  
  // Create large telemetry dataset
  let large_dataset = []
  for i in 0..=500 {
    large_dataset.push(TelemetryData::with_attributes("compression_test_" + i.to_string(), [
      ("trace_id", StringValue("compression_trace_" + i.to_string())),
      ("span_id", StringValue("compression_span_" + i.to_string())),
      ("service_name", StringValue("compression_service")),
      ("operation", StringValue("compression_operation")),
      ("payload", StringValue("large_payload_data_" + "x".repeat(1000))), // Large payload
      ("metadata", StringValue("metadata_" + "y".repeat(500)))
    ]))
  }
  
  // Serialize without compression
  let regular_serializer = JsonSerializer::new()
  let regular_result = JsonSerializer::serialize_batch(regular_serializer, large_dataset)
  
  // Serialize with compression
  let compressed_result = CompressionSerializer::serialize_batch(compression_serializer, large_dataset)
  
  match (regular_result, compressed_result) {
    (Ok(regular_data), Ok(compressed_data)) => {
      // Compressed data should be smaller
      assert_true(compressed_data.length() < regular_data.length())
      
      // Verify compression ratio
      let compression_ratio = compressed_data.length().to_float() / regular_data.length().to_float()
      assert_true(compression_ratio < 0.8) // At least 20% compression
      
      // Test decompression and deserialization
      let decompress_result = CompressionSerializer::deserialize_batch(compression_serializer, compressed_data)
      match decompress_result {
        Ok(decompressed_dataset) => {
          assert_eq(decompressed_dataset.length(), large_dataset.length())
          
          // Verify data integrity
          for i in 0..=large_dataset.length() - 1 {
            let original = large_dataset[i]
            let decompressed = decompressed_dataset[i]
            
            assert_eq(
              TelemetryData::get_attribute(original, "trace_id"),
              TelemetryData::get_attribute(decompressed, "trace_id")
            )
            
            assert_eq(
              TelemetryData::get_attribute(original, "payload"),
              TelemetryData::get_attribute(decompressed, "payload")
            )
          }
        }
        Err(error) => assert_true(false)
      }
    }
    (_, _) => assert_true(false)
  }
  
  // Test different compression algorithms
  let algorithms = [CompressionAlgorithm::Gzip, CompressionAlgorithm::Deflate, CompressionAlgorithm::LZ4]
  let compression_results = []
  
  for algorithm in algorithms {
    let algo_serializer = CompressionSerializer::new(algorithm)
    let result = CompressionSerializer::serialize_batch(algo_serializer, large_dataset)
    match result {
      Ok(data) => compression_results.push((algorithm, data.length())),
      Err(_) => assert_true(false)
    }
  }
  
  // Verify all algorithms produce valid compressed data
  assert_eq(compression_results.length(), algorithms.length())
  
  // Find most efficient algorithm
  let mut most_efficient = compression_results[0]
  for result in compression_results {
    if result.1 < most_efficient.1 {
      most_efficient = result
    }
  }
  
  // Most efficient algorithm should provide good compression
  match regular_result {
    Ok(regular_data) => {
      let best_ratio = most_efficient.1.to_float() / regular_data.length().to_float()
      assert_true(best_ratio < 0.7) // At least 30% compression
    }
    Err(_) => assert_true(false)
  }
}

// Test 7: Streaming Serialization
test "streaming serialization" {
  let streaming_serializer = StreamingSerializer::new()
  
  // Create large dataset that might not fit in memory
  let data_generator = DataGenerator::new(10000) // 10,000 items
  
  // Test streaming serialization
  let stream_result = StreamingSerializer::serialize_stream(streaming_serializer, data_generator, |item| {
    TelemetryData::with_attributes("stream_item_" + item.index.to_string(), [
      ("trace_id", StringValue("stream_trace_" + item.index.to_string())),
      ("span_id", StringValue("stream_span_" + item.index.to_string())),
      ("value", IntValue(item.index * 10))
    ])
  })
  
  match stream_result {
    Ok(stream_data) => {
      assert_true(stream_data.length() > 0)
      
      // Test streaming deserialization
      let deserializer = StreamingDeserializer::new()
      let deserialized_count = StreamingDeserializer::deserialize_stream(deserializer, stream_data, |data| {
        // Process each deserialized item
        assert_true(TelemetryData::get_attribute(data, "trace_id").is_some())
        assert_true(TelemetryData::get_attribute(data, "span_id").is_some())
        true // Continue processing
      })
      
      assert_eq(deserialized_count, 10000)
    }
    Err(error) => assert_true(false)
  }
  
  // Test partial stream processing
  let partial_stream = stream_result.unwrap().slice(0, 1000) // First 1000 bytes
  
  let partial_deserializer = StreamingDeserializer::new()
  let partial_count = StreamingDeserializer::deserialize_stream(partial_deserializer, partial_stream, |data| {
    true // Continue processing
  })
  
  // Should process fewer items due to partial data
  assert_true(partial_count < 10000)
}

// Test 8: Error Handling in Serialization
test "error handling in serialization" {
  let error_serializer = ErrorHandlingSerializer::new()
  
  // Test serialization with invalid data
  let invalid_data = TelemetryData::with_attributes("invalid", [
    ("valid_field", StringValue("valid_value")),
    ("invalid_field", StringValue("")) // Empty string might be invalid
  ])
  
  let result = ErrorHandlingSerializer::serialize_with_validation(error_serializer, invalid_data)
  match result {
    Ok(_) => assert_true(false), // Should fail validation
    Err(error) => assert_eq(error.code, "VALIDATION_FAILED")
  }
  
  // Test deserialization with corrupted data
  let corrupted_data = "{\"trace_id\": \"123\", \"span_id\":}".to_bytes() // Invalid JSON
  let deserialize_result = ErrorHandlingSerializer::deserialize_safe(error_serializer, corrupted_data)
  match deserialize_result {
    Ok(_) => assert_true(false), // Should fail with corrupted data
    Err(error) => assert_eq(error.code, "DESERIALIZATION_ERROR")
  }
  
  // Test graceful degradation
  let partial_data = TelemetryData::with_attributes("partial", [
    ("trace_id", StringValue("partial_trace")),
    ("span_id", StringValue("partial_span"))
    // Missing some required fields
  ])
  
  let graceful_result = ErrorHandlingSerializer::serialize_with_graceful_degradation(error_serializer, partial_data)
  match graceful_result {
    Ok(data) => {
      // Should succeed with default values for missing fields
      assert_true(data.length() > 0)
    }
    Err(error) => assert_true(false)
  }
  
  // Test recovery from serialization errors
  let recovery_serializer = ErrorHandlingSerializer::with_recovery()
  
  let problematic_data = TelemetryData::with_attributes("problematic", [
    ("trace_id", StringValue("problematic_trace")),
    ("circular_ref", CircularValue("problematic_trace")) // Circular reference
  ])
  
  let recovery_result = ErrorHandlingSerializer::serialize_with_recovery(recovery_serializer, problematic_data)
  match recovery_result {
    Ok(data) => {
      // Should recover by breaking circular references
      assert_true(data.contains("trace_id"))
      assert_false(data.contains("circular_ref")) // Circular reference should be removed
    }
    Err(error) => assert_true(false)
  }
}

// Test 9: Performance Comparison of Serialization Formats
test "performance comparison of serialization formats" {
  let performance_tester = SerializationPerformanceTester::new()
  
  // Create test dataset
  let test_data = []
  for i in 0..=1000 {
    test_data.push(TelemetryData::with_attributes("perf_test_" + i.to_string(), [
      ("trace_id", StringValue("perf_trace_" + i.to_string())),
      ("span_id", StringValue("perf_span_" + i.to_string())),
      ("service_name", StringValue("performance_test_service")),
      ("operation", StringValue("performance_test_operation")),
      ("timestamp", IntValue(1640995200L + i.to_int64())),
      ("duration_ms", IntValue(Random::int_range(50, 500))),
      ("status", StringValue(if i % 10 == 0 { "error" } else { "success" })),
      ("tags", ArrayValue(["tag1", "tag2", "tag3"]))
    ]))
  }
  
  // Test JSON serialization performance
  let json_perf = SerializationPerformanceTester::test_json(performance_tester, test_data)
  
  // Test binary serialization performance
  let binary_perf = SerializationPerformanceTester::test_binary(performance_tester, test_data)
  
  // Test protobuf serialization performance
  let protobuf_perf = SerializationPerformanceTester::test_protobuf(performance_tester, test_data)
  
  // Verify performance metrics
  assert_true(json_perf.serialization_time_ms > 0)
  assert_true(json_perf.deserialization_time_ms > 0)
  assert_true(json_perf.serialized_size > 0)
  
  assert_true(binary_perf.serialization_time_ms > 0)
  assert_true(binary_perf.deserialization_time_ms > 0)
  assert_true(binary_perf.serialized_size > 0)
  
  assert_true(protobuf_perf.serialization_time_ms > 0)
  assert_true(protobuf_perf.deserialization_time_ms > 0)
  assert_true(protobuf_perf.serialized_size > 0)
  
  // Binary should be more compact than JSON
  assert_true(binary_perf.serialized_size < json_perf.serialized_size)
  
  // Protobuf should also be more compact than JSON
  assert_true(protobuf_perf.serialized_size < json_perf.serialized_size)
  
  // Binary serialization should be faster than JSON
  assert_true(binary_perf.serialization_time_ms < json_perf.serialization_time_ms)
  
  // Test throughput
  let json_throughput = test_data.length().to_float() / (json_perf.serialization_time_ms / 1000.0)
  let binary_throughput = test_data.length().to_float() / (binary_perf.serialization_time_ms / 1000.0)
  let protobuf_throughput = test_data.length().to_float() / (protobuf_perf.serialization_time_ms / 1000.0)
  
  assert_true(json_throughput > 0)
  assert_true(binary_throughput > 0)
  assert_true(protobuf_throughput > 0)
  
  // Binary should have higher throughput than JSON
  assert_true(binary_throughput > json_throughput)
}

// Test 10: Custom Serialization Formats
test "custom serialization formats" {
  let custom_format_registry = CustomFormatRegistry::new()
  
  // Register custom format
  CustomFormatRegistry::register_format(custom_format_registry, "compact_telemetry", CompactTelemetryFormat::new())
  
  // Create test data
  let test_data = TelemetryData::with_attributes("custom_format_test", [
    ("trace_id", StringValue("custom_trace")),
    ("span_id", StringValue("custom_span")),
    ("service", StringValue("custom_service")),
    ("operation", StringValue("custom_operation")),
    ("timestamp", IntValue(1640995200L)),
    ("duration", IntValue(100))
  ])
  
  // Serialize with custom format
  let custom_serializer = CustomFormatSerializer::new(custom_format_registry, "compact_telemetry")
  let serialize_result = custom_serializer.serialize(test_data)
  
  match serialize_result {
    Ok(custom_data) => {
      assert_true(custom_data.length() > 0)
      
      // Verify custom format characteristics
      assert_true(custom_data.length() < 100) // Should be very compact
      
      // Deserialize with custom format
      let custom_deserializer = CustomFormatDeserializer::new(custom_format_registry, "compact_telemetry")
      let deserialize_result = custom_deserializer.deserialize(custom_data)
      
      match deserialize_result {
        Ok(deserialized_data) => {
          assert_eq(
            TelemetryData::get_attribute(deserialized_data, "trace_id"),
            Some(StringValue("custom_trace"))
          )
          
          assert_eq(
            TelemetryData::get_attribute(deserialized_data, "service"),
            Some(StringValue("custom_service"))
          )
        }
        Err(error) => assert_true(false)
      }
    }
    Err(error) => assert_true(false)
  }
  
  // Test format auto-detection
  let auto_detector = FormatAutoDetector::new()
  auto_detector.register_format_signature("compact_telemetry", b"CTF")
  
  let detected_format = FormatAutoDetector::detect_format(auto_detector, serialize_result.unwrap())
  match detected_format {
    Some(format_name) => assert_eq(format_name, "compact_telemetry"),
    None => assert_true(false)
  }
  
  // Test format conversion chain
  let conversion_chain = FormatConversionChain::new()
  conversion_chain.add_step("json_to_compact", JsonToCompactConverter::new())
  conversion_chain.add_step("compact_to_protobuf", CompactToProtobufConverter::new())
  
  let json_serializer = JsonSerializer::new()
  let json_data = json_serializer.serialize(test_data).unwrap()
  
  let chain_result = conversion_chain.convert(json_data, "compact_telemetry")
  match chain_result {
    Ok(converted_data) => {
      assert_true(converted_data.length() > 0)
      assert_true(converted_data.length() != json_data.length()) // Should be different format
    }
    Err(error) => assert_true(false)
  }
}

// Helper functions
fn estimate_json_size(data : Array[TelemetryData]) -> Int {
  let json_serializer = JsonSerializer::new()
  let result = JsonSerializer::serialize_batch(json_serializer, data)
  match result {
    Ok(json_string) => json_string.length(),
    Err(_) => 0
  }
}