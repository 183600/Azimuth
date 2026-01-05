// Azimuth Telemetry System - Serialization/Deserialization Quality Tests
// This file contains comprehensive test cases for serialization and deserialization functionality

// Test 1: JSON Serialization/Deserialization
test "json serialization deserialization" {
  let telemetry_data = ComplexTelemetryData::new()
  
  // Test JSON serialization
  let json_serializer = JsonSerializer::new()
  let json_result = JsonSerializer::serialize(json_serializer, telemetry_data)
  
  match json_result {
    Ok(json_string) => {
      // Verify JSON structure
      assert_true(String::contains(json_string, "\"metric_name\""))
      assert_true(String::contains(json_string, "\"value\""))
      assert_true(String::contains(json_string, "\"timestamp\""))
      assert_true(String::contains(json_string, "\"attributes\""))
      assert_true(String::contains(json_string, "\"metadata\""))
      
      // Test JSON deserialization
      let deserialize_result = JsonSerializer::deserialize(json_serializer, json_string)
      
      match deserialize_result {
        Ok(deserialized_data) => {
          // Verify deserialized data matches original
          assert_eq(deserialized_data.metric_name, telemetry_data.metric_name)
          assert_eq(deserialized_data.value, telemetry_data.value)
          assert_eq(deserialized_data.timestamp, telemetry_data.timestamp)
          assert_eq(deserialized_data.attributes.length(), telemetry_data.attributes.length())
          assert_eq(deserialized_data.metadata.length(), telemetry_data.metadata.length())
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test JSON serialization with special characters
  let special_data = ComplexTelemetryData::with_special_chars()
  let special_json_result = JsonSerializer::serialize(json_serializer, special_data)
  
  match special_json_result {
    Ok(json_string) => {
      // Verify special characters are properly escaped
      assert_true(String::contains(json_string, "\\n"))
      assert_true(String::contains(json_string, "\\t"))
      assert_true(String::contains(json_string, "\\\""))
      
      // Test deserialization of special characters
      let special_deserialize_result = JsonSerializer::deserialize(json_serializer, json_string)
      
      match special_deserialize_result {
        Ok(deserialized_data) => {
          assert_eq(deserialized_data.metric_name, special_data.metric_name)
          assert_eq(deserialized_data.value, special_data.value)
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
}

// Test 2: Binary Serialization/Deserialization
test "binary serialization deserialization" {
  let telemetry_data = ComplexTelemetryData::new()
  
  // Test binary serialization
  let binary_serializer = BinarySerializer::new()
  let binary_result = BinarySerializer::serialize(binary_serializer, telemetry_data)
  
  match binary_result {
    Ok(binary_data) => {
      // Verify binary data is not empty
      assert_true(binary_data.length() > 0)
      
      // Test binary deserialization
      let deserialize_result = BinarySerializer::deserialize(binary_serializer, binary_data)
      
      match deserialize_result {
        Ok(deserialized_data) => {
          // Verify deserialized data matches original
          assert_eq(deserialized_data.metric_name, telemetry_data.metric_name)
          assert_eq(deserialized_data.value, telemetry_data.value)
          assert_eq(deserialized_data.timestamp, telemetry_data.timestamp)
          assert_eq(deserialized_data.attributes.length(), telemetry_data.attributes.length())
          assert_eq(deserialized_data.metadata.length(), telemetry_data.metadata.length())
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test binary serialization with large data
  let large_data = ComplexTelemetryData::with_large_attributes()
  let large_binary_result = BinarySerializer::serialize(binary_serializer, large_data)
  
  match large_binary_result {
    Ok(binary_data) => {
      // Verify binary data size is reasonable
      assert_true(binary_data.length() > 1000)
      
      // Test deserialization of large data
      let large_deserialize_result = BinarySerializer::deserialize(binary_serializer, binary_data)
      
      match large_deserialize_result {
        Ok(deserialized_data) => {
          assert_eq(deserialized_data.metric_name, large_data.metric_name)
          assert_eq(deserialized_data.attributes.length(), large_data.attributes.length())
          assert_eq(deserialized_data.metadata.length(), large_data.metadata.length())
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
}

// Test 3: Protocol Buffer Serialization/Deserialization
test "protocol buffer serialization deserialization" {
  let telemetry_data = ComplexTelemetryData::new()
  
  // Test Protocol Buffer serialization
  let protobuf_serializer = ProtobufSerializer::new()
  let protobuf_result = ProtobufSerializer::serialize(protobuf_serializer, telemetry_data)
  
  match protobuf_result {
    Ok(protobuf_data) => {
      // Verify protobuf data is not empty
      assert_true(protobuf_data.length() > 0)
      
      // Test Protocol Buffer deserialization
      let deserialize_result = ProtobufSerializer::deserialize(protobuf_serializer, protobuf_data)
      
      match deserialize_result {
        Ok(deserialized_data) => {
          // Verify deserialized data matches original
          assert_eq(deserialized_data.metric_name, telemetry_data.metric_name)
          assert_eq(deserialized_data.value, telemetry_data.value)
          assert_eq(deserialized_data.timestamp, telemetry_data.timestamp)
          assert_eq(deserialized_data.attributes.length(), telemetry_data.attributes.length())
          assert_eq(deserialized_data.metadata.length(), telemetry_data.metadata.length())
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test Protocol Buffer serialization with nested structures
  let nested_data = ComplexTelemetryData::with_nested_structures()
  let nested_protobuf_result = ProtobufSerializer::serialize(protobuf_serializer, nested_data)
  
  match nested_protobuf_result {
    Ok(protobuf_data) => {
      // Test deserialization of nested structures
      let nested_deserialize_result = ProtobufSerializer::deserialize(protobuf_serializer, protobuf_data)
      
      match nested_deserialize_result {
        Ok(deserialized_data) => {
          assert_eq(deserialized_data.metric_name, nested_data.metric_name)
          assert_eq(deserialized_data.nested_structure.field1, nested_data.nested_structure.field1)
          assert_eq(deserialized_data.nested_structure.field2, nested_data.nested_structure.field2)
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
}

// Test 4: Serialization Format Comparison
test "serialization format comparison" {
  let telemetry_data = ComplexTelemetryData::with_large_attributes()
  
  // Test JSON serialization
  let json_serializer = JsonSerializer::new()
  let json_result = JsonSerializer::serialize(json_serializer, telemetry_data)
  
  // Test binary serialization
  let binary_serializer = BinarySerializer::new()
  let binary_result = BinarySerializer::serialize(binary_serializer, telemetry_data)
  
  // Test Protocol Buffer serialization
  let protobuf_serializer = ProtobufSerializer::new()
  let protobuf_result = ProtobufSerializer::serialize(protobuf_serializer, telemetry_data)
  
  // Compare serialization sizes
  match (json_result, binary_result, protobuf_result) {
    (Ok(json_data), Ok(binary_data), Ok(protobuf_data)) => {
      let json_size = json_data.length()
      let binary_size = binary_data.length()
      let protobuf_size = protobuf_data.length()
      
      // Binary formats should be more compact than JSON
      assert_true(binary_size < json_size)
      assert_true(protobuf_size < json_size)
      
      // Protocol Buffer should be competitive with binary
      assert_true(protobuf_size <= binary_size * 1.2) // Allow some overhead
      
      // Compare serialization times
      let json_start = Time::now()
      for i in 0..=100 {
        JsonSerializer::serialize(json_serializer, telemetry_data)
      }
      let json_end = Time::now()
      let json_duration = Time::duration(json_start, json_end)
      
      let binary_start = Time::now()
      for i in 0..=100 {
        BinarySerializer::serialize(binary_serializer, telemetry_data)
      }
      let binary_end = Time::now()
      let binary_duration = Time::duration(binary_start, binary_end)
      
      let protobuf_start = Time::now()
      for i in 0..=100 {
        ProtobufSerializer::serialize(protobuf_serializer, telemetry_data)
      }
      let protobuf_end = Time::now()
      let protobuf_duration = Time::duration(protobuf_start, protobuf_end)
      
      // Compare deserialization times
      let json_deserialize_start = Time::now()
      for i in 0..=100 {
        JsonSerializer::deserialize(json_serializer, json_data)
      }
      let json_deserialize_end = Time::now()
      let json_deserialize_duration = Time::duration(json_deserialize_start, json_deserialize_end)
      
      let binary_deserialize_start = Time::now()
      for i in 0..=100 {
        BinarySerializer::deserialize(binary_serializer, binary_data)
      }
      let binary_deserialize_end = Time::now()
      let binary_deserialize_duration = Time::duration(binary_deserialize_start, binary_deserialize_end)
      
      let protobuf_deserialize_start = Time::now()
      for i in 0..=100 {
        ProtobufSerializer::deserialize(protobuf_serializer, protobuf_data)
      }
      let protobuf_deserialize_end = Time::now()
      let protobuf_deserialize_duration = Time::duration(protobuf_deserialize_start, protobuf_deserialize_end)
      
      // Binary formats should be faster than JSON
      assert_true(binary_duration < json_duration)
      assert_true(protobuf_duration < json_duration)
      assert_true(binary_deserialize_duration < json_deserialize_duration)
      assert_true(protobuf_deserialize_duration < json_deserialize_duration)
    }
    _ => assert_true(false) // All should succeed
  }
}

// Test 5: Serialization Version Compatibility
test "serialization version compatibility" {
  let telemetry_data = ComplexTelemetryData::new()
  
  // Test versioned serialization
  let versioned_serializer = VersionedSerializer::new("1.0")
  let versioned_result = VersionedSerializer::serialize(versioned_serializer, telemetry_data)
  
  match versioned_result {
    Ok(versioned_data) => {
      // Verify version information is included
      assert_true(String::contains(versioned_data, "\"version\":\"1.0\""))
      
      // Test versioned deserialization with same version
      let same_version_result = VersionedSerializer::deserialize(versioned_serializer, versioned_data)
      
      match same_version_result {
        Ok(deserialized_data) => {
          assert_eq(deserialized_data.metric_name, telemetry_data.metric_name)
          assert_eq(deserialized_data.value, telemetry_data.value)
        }
        Error(_) => assert_true(false) // Should succeed
      }
      
      // Test versioned deserialization with different version
      let different_version_serializer = VersionedSerializer::new("2.0")
      let different_version_result = VersionedSerializer::deserialize(different_version_serializer, versioned_data)
      
      match different_version_result {
        Ok(_) => assert_true(false) // Should not succeed
        Error(err) => {
          match err {
            VersionMismatch => assert_true(true)
            IncompatibleVersion => assert_true(true)
            _ => assert_true(false)
          }
        }
      }
      
      // Test backward compatibility
      let backward_compatible_serializer = BackwardCompatibleSerializer::new("2.0")
      let backward_result = BackwardCompatibleSerializer::deserialize(backward_compatible_serializer, versioned_data)
      
      match backward_result {
        Ok(deserialized_data) => {
          // Should succeed with backward compatibility
          assert_eq(deserialized_data.metric_name, telemetry_data.metric_name)
          assert_eq(deserialized_data.value, telemetry_data.value)
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
}

// Test 6: Serialization Error Handling
test "serialization error handling" {
  // Test serialization of invalid data
  let invalid_data = InvalidTelemetryData::new()
  let json_serializer = JsonSerializer::new()
  let invalid_result = JsonSerializer::serialize(json_serializer, invalid_data)
  
  match invalid_result {
    Ok(_) => assert_true(false) // Should not succeed
    Error(err) => {
      match err {
        SerializationError => assert_true(true)
        InvalidDataError => assert_true(true)
        _ => assert_true(false)
      }
    }
  }
  
  // Test deserialization of malformed data
  let malformed_json = "{ invalid json structure"
  let malformed_result = JsonSerializer::deserialize(json_serializer, malformed_json)
  
  match malformed_result {
    Ok(_) => assert_true(false) // Should not succeed
    Error(err) => {
      match err {
        DeserializationError => assert_true(true)
        MalformedDataError => assert_true(true)
        _ => assert_true(false)
      }
    }
  }
  
  // Test deserialization of incomplete data
  let incomplete_binary = ByteArray::from([1, 2, 3, 4, 5]) // Too short
  let binary_serializer = BinarySerializer::new()
  let incomplete_result = BinarySerializer::deserialize(binary_serializer, incomplete_binary)
  
  match incomplete_result {
    Ok(_) => assert_true(false) // Should not succeed
    Error(err) => {
      match err {
        DeserializationError => assert_true(true)
        IncompleteDataError => assert_true(true)
        _ => assert_true(false)
      }
    }
  }
  
  // Test graceful degradation with partial data
  let partial_json = "{\"metric_name\":\"valid_metric\",\"invalid_field\":123}"
  let partial_result = JsonSerializer::deserialize_with_recovery(json_serializer, partial_json)
  
  match partial_result {
    Ok(data) => {
      assert_eq(data.metric_name, "valid_metric")
      // Invalid field should be ignored
    }
    Error(_) => assert_true(false) // Should recover
  }
}

// Test 7: Serialization Performance
test "serialization performance" {
  // Create large dataset
  let large_dataset = []
  for i in 0..=1000 {
    large_dataset.push(ComplexTelemetryData::with_large_attributes())
  }
  
  // Test batch serialization performance
  let json_serializer = JsonSerializer::new()
  let json_batch_start = Time::now()
  
  let json_results = []
  for data in large_dataset {
    let result = JsonSerializer::serialize(json_serializer, data)
    json_results.push(result)
  }
  
  let json_batch_end = Time::now()
  let json_batch_duration = Time::duration(json_batch_start, json_batch_end)
  
  // Test streaming serialization performance
  let json_stream_start = Time::now()
  let json_stream_result = JsonSerializer::serialize_stream(json_serializer, large_dataset)
  let json_stream_end = Time::now()
  let json_stream_duration = Time::duration(json_stream_start, json_stream_end)
  
  // Streaming should be faster than individual serialization
  assert_true(json_stream_duration < json_batch_duration)
  
  // Verify streaming result
  match json_stream_result {
    Ok(stream_data) => {
      assert_true(stream_data.length() > 0)
      
      // Test streaming deserialization
      let stream_deserialize_start = Time::now()
      let stream_deserialize_result = JsonSerializer::deserialize_stream(json_serializer, stream_data)
      let stream_deserialize_end = Time::now()
      let stream_deserialize_duration = Time::duration(stream_deserialize_start, stream_deserialize_end)
      
      match stream_deserialize_result {
        Ok(deserialized_data) => {
          assert_eq(deserialized_data.length(), large_dataset.length())
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test memory usage during serialization
  let initial_memory = MemoryStats::current_usage()
  
  let memory_intensive_serializer = MemoryIntensiveSerializer::new()
  let memory_result = MemoryIntensiveSerializer::serialize(memory_intensive_serializer, large_dataset)
  
  let peak_memory = MemoryStats::peak_usage()
  let final_memory = MemoryStats::current_usage()
  
  // Verify memory usage is reasonable
  let memory_overhead = peak_memory - initial_memory
  let memory_recovered = peak_memory - final_memory
  
  assert_true(memory_overhead < initial_memory * 2) // Less than 2x overhead
  assert_true(memory_recovered > memory_overhead * 0.8) // At least 80% recovered
}

// Test 8: Serialization Security
test "serialization security" {
  let telemetry_data = ComplexTelemetryData::new()
  
  // Test serialization with sensitive data
  let sensitive_data = ComplexTelemetryData::with_sensitive_attributes()
  let secure_serializer = SecureSerializer::new()
  let secure_result = SecureSerializer::serialize(secure_serializer, sensitive_data)
  
  match secure_result {
    Ok(secure_data) => {
      // Verify sensitive data is encrypted
      assert_false(String::contains(secure_data, "password"))
      assert_false(String::contains(secure_data, "secret_key"))
      assert_false(String::contains(secure_data, "token"))
      
      // Test secure deserialization
      let secure_deserialize_result = SecureSerializer::deserialize(secure_serializer, secure_data)
      
      match secure_deserialize_result {
        Ok(deserialized_data) => {
          // Verify sensitive data is properly restored
          assert_eq(deserialized_data.attributes["password"], sensitive_data.attributes["password"])
          assert_eq(deserialized_data.attributes["secret_key"], sensitive_data.attributes["secret_key"])
          assert_eq(deserialized_data.attributes["token"], sensitive_data.attributes["token"])
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test deserialization with malicious data
  let malicious_json = "{\"metric_name\":\"test\",\"value\":1,\"timestamp\":1,\"attributes\":{\"__proto__\":{\"admin\":true}}}"
  let secure_deserializer = SecureDeserializer::new()
  let malicious_result = SecureDeserializer::deserialize(secure_deserializer, malicious_json)
  
  match malicious_result {
    Ok(_) => assert_true(false) // Should not succeed
    Error(err) => {
      match err {
        SecurityError => assert_true(true)
        MaliciousDataError => assert_true(true)
        _ => assert_true(false)
      }
    }
  }
  
  // Test serialization with data integrity check
  let integrity_serializer = IntegritySerializer::new()
  let integrity_result = IntegritySerializer::serialize(integrity_serializer, telemetry_data)
  
  match integrity_result {
    Ok(integrity_data) => {
      // Test integrity verification
      let verify_result = IntegritySerializer::verify(integrity_serializer, integrity_data)
      match verify_result {
        Ok(_) => assert_true(true) // Should verify
        Error(_) => assert_true(false) // Should succeed
      }
      
      // Test tampering detection
      let tampered_data = IntegritySerializer::tamper_with(integrity_data)
      let tampered_result = IntegritySerializer::verify(integrity_serializer, tampered_data)
      
      match tampered_result {
        Ok(_) => assert_true(false) // Should not verify
        Error(err) => {
          match err {
            IntegrityError => assert_true(true)
            TamperedDataError => assert_true(true)
            _ => assert_true(false)
          }
        }
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
}

// Test 9: Serialization Compression
test "serialization compression" {
  let telemetry_data = ComplexTelemetryData::with_large_attributes()
  
  // Test serialization without compression
  let json_serializer = JsonSerializer::new()
  let uncompressed_result = JsonSerializer::serialize(json_serializer, telemetry_data)
  
  // Test serialization with compression
  let compressed_serializer = CompressedSerializer::new(json_serializer, GzipCompression)
  let compressed_result = CompressedSerializer::serialize(compressed_serializer, telemetry_data)
  
  match (uncompressed_result, compressed_result) {
    (Ok(uncompressed_data), Ok(compressed_data)) => {
      // Verify compression reduces size
      assert_true(compressed_data.length() < uncompressed_data.length())
      
      let compression_ratio = compressed_data.length().to_float() / uncompressed_data.length().to_float()
      assert_true(compression_ratio < 0.8) // At least 20% compression
      
      // Test decompression
      let decompressed_result = CompressedSerializer::deserialize(compressed_serializer, compressed_data)
      
      match decompressed_result {
        Ok(decompressed_data) => {
          // Verify decompressed data matches original
          assert_eq(decompressed_data.metric_name, telemetry_data.metric_name)
          assert_eq(decompressed_data.value, telemetry_data.value)
          assert_eq(decompressed_data.timestamp, telemetry_data.timestamp)
          assert_eq(decompressed_data.attributes.length(), telemetry_data.attributes.length())
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    _ => assert_true(false) // Both should succeed
  }
  
  // Test different compression algorithms
  let gzip_serializer = CompressedSerializer::new(json_serializer, GzipCompression)
  let lz4_serializer = CompressedSerializer::new(json_serializer, Lz4Compression)
  let zstd_serializer = CompressedSerializer::new(json_serializer, ZstdCompression)
  
  let gzip_result = CompressedSerializer::serialize(gzip_serializer, telemetry_data)
  let lz4_result = CompressedSerializer::serialize(lz4_serializer, telemetry_data)
  let zstd_result = CompressedSerializer::serialize(zstd_serializer, telemetry_data)
  
  match (gzip_result, lz4_result, zstd_result) {
    (Ok(gzip_data), Ok(lz4_data), Ok(zstd_data)) => {
      // Compare compression ratios
      let gzip_ratio = gzip_data.length().to_float() / uncompressed_data.length().to_float()
      let lz4_ratio = lz4_data.length().to_float() / uncompressed_data.length().to_float()
      let zstd_ratio = zstd_data.length().to_float() / uncompressed_data.length().to_float()
      
      // All should provide meaningful compression
      assert_true(gzip_ratio < 0.8)
      assert_true(lz4_ratio < 0.8)
      assert_true(zstd_ratio < 0.8)
      
      // Zstd should provide best compression
      assert_true(zstd_ratio <= gzip_ratio)
      assert_true(zstd_ratio <= lz4_ratio)
      
      // Compare compression speeds
      let gzip_start = Time::now()
      for i in 0..=10 {
        CompressedSerializer::serialize(gzip_serializer, telemetry_data)
      }
      let gzip_end = Time::now()
      let gzip_duration = Time::duration(gzip_start, gzip_end)
      
      let lz4_start = Time::now()
      for i in 0..=10 {
        CompressedSerializer::serialize(lz4_serializer, telemetry_data)
      }
      let lz4_end = Time::now()
      let lz4_duration = Time::duration(lz4_start, lz4_end)
      
      let zstd_start = Time::now()
      for i in 0..=10 {
        CompressedSerializer::serialize(zstd_serializer, telemetry_data)
      }
      let zstd_end = Time::now()
      let zstd_duration = Time::duration(zstd_start, zstd_end)
      
      // LZ4 should be fastest for compression
      assert_true(lz4_duration <= gzip_duration)
      assert_true(lz4_duration <= zstd_duration)
    }
    _ => assert_true(false) // All should succeed
  }
}

// Test 10: Serialization Customization
test "serialization customization" {
  let telemetry_data = ComplexTelemetryData::new()
  
  // Test custom field filtering
  let filter_config = FieldFilterConfig::new()
  FieldFilterConfig::exclude_field(filter_config, "metadata")
  FieldFilterConfig::exclude_field(filter_config, "internal_id")
  
  let filter_serializer = FilteredSerializer::new(filter_config)
  let filtered_result = FilteredSerializer::serialize(filter_serializer, telemetry_data)
  
  match filtered_result {
    Ok(filtered_data) => {
      // Verify excluded fields are not present
      assert_false(String::contains(filtered_data, "metadata"))
      assert_false(String::contains(filtered_data, "internal_id"))
      
      // Verify included fields are present
      assert_true(String::contains(filtered_data, "metric_name"))
      assert_true(String::contains(filtered_data, "value"))
      assert_true(String::contains(filtered_data, "timestamp"))
      assert_true(String::contains(filtered_data, "attributes"))
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test custom field transformation
  let transform_config = FieldTransformConfig::new()
  FieldTransformConfig::add_transform(transform_config, "value", func(v : Float) -> String {
    "value:" + v.to_string()
  })
  FieldTransformConfig::add_transform(transform_config, "timestamp", func(t : Long) -> String {
    "time:" + t.to_string()
  })
  
  let transform_serializer = TransformingSerializer::new(transform_config)
  let transformed_result = TransformingSerializer::serialize(transform_serializer, telemetry_data)
  
  match transformed_result {
    Ok(transformed_data) => {
      // Verify transformed fields
      assert_true(String::contains(transformed_data, "value:"))
      assert_true(String::contains(transformed_data, "time:"))
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test custom serialization format
  let custom_format = CustomFormat::new()
  CustomFormat::add_field(custom_format, "metric", "metric_name")
  CustomFormat::add_field(custom_format, "val", "value")
  CustomFormat::add_field(custom_format, "ts", "timestamp")
  
  let custom_serializer = CustomFormatSerializer::new(custom_format)
  let custom_result = CustomFormatSerializer::serialize(custom_serializer, telemetry_data)
  
  match custom_result {
    Ok(custom_data) => {
      // Verify custom format
      assert_true(String::contains(custom_data, "metric:"))
      assert_true(String::contains(custom_data, "val:"))
      assert_true(String::contains(custom_data, "ts:"))
      
      // Test custom deserialization
      let custom_deserialize_result = CustomFormatSerializer::deserialize(custom_serializer, custom_data)
      
      match custom_deserialize_result {
        Ok(deserialized_data) => {
          assert_eq(deserialized_data.metric_name, telemetry_data.metric_name)
          assert_eq(deserialized_data.value, telemetry_data.value)
          assert_eq(deserialized_data.timestamp, telemetry_data.timestamp)
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
}